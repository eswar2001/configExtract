{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}

module ConfigExtract.Plugin (plugin) where

#if __GLASGOW_HASKELL__ >= 900

import GHC.Types.Basic
import GHC
import GHC.Driver.Plugins (Plugin(..),CommandLineOption,defaultPlugin,PluginRecompile(..),purePlugin)
import GHC.Driver.Env
import GHC.Tc.Types
import GHC.Unit.Module.ModSummary
import GHC.Utils.Outputable (showSDocUnsafe,ppr)
import GHC.Data.Bag (bagToList)
import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import qualified Data.Aeson.KeyMap as HM
import qualified Data.IntMap.Internal as IntMap
import GHC as GhcPlugins
import GHC.Core.DataCon as GhcPlugins
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon as GhcPlugins
import GHC.Utils.Outputable (showSDocUnsafe,ppr,SDoc)
import GHC.Core.Opt.Monad
import GHC.Core
import GHC.Unit.Module.ModGuts
import GHC.Types.Name.Reader
import GHC.Types.Id
import GHC.Data.FastString
import GHC.Types.Literal

#else

import CoreMonad (CoreM, CoreToDo (CoreDoPluginPass), liftIO)
import CoreSyn
import DataCon
import GHC.Generics (Generic)
import GHC.Hs
import GHC.Hs.Decls
import GHC.IO (unsafePerformIO)
import GhcPlugins (
    Arg (..),
    CommandLineOption,
    DataCon,
    DynFlags,
    FunctionOrData (..),
    HsParsedModule (..),
    Hsc,
    LitNumType (..),
    Literal (..),
    ModGuts (mg_binds, mg_loc, mg_module),
    ModSummary (..),
    Module (moduleName),
    Name,
    NamedThing (getName),
    Outputable (..),
    Plugin (..),
    SDoc,
    TyCon,
    Var,
    coVarDetails,
    collectArgs,
    dataConFieldLabels,
    dataConName,
    dataConOrigArgTys,
    dataConRepType,
    defaultPlugin,
    flLabel,
    getDynFlags,
    idName,
    mkInternalName,
    mkLitString,
    mkLocalVar,
    mkVarOcc,
    moduleNameString,
    msHsFilePath,
    nameStableString,
    noCafIdInfo,
    purePlugin,
    showSDoc,
    showSDocUnsafe,
    tyConDataCons,
    tyConKind,
    tyConName,
    tyVarKind,
    typeEnvElts,
    unpackFS,
 )
import Id (Id, idType, isExportedId)
import Name (getSrcSpan)
import SrcLoc
import TcRnMonad
import TcRnTypes
import TyCoRep
import Unique (mkUnique)
import Var (isLocalId, varName, varType)
#endif

import Prelude
import ConfigExtract.Types
import Control.Concurrent (MVar, modifyMVar, newMVar)
import Control.Exception (evaluate)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as DBS
import Data.ByteString.Lazy (toStrict)
import Data.Data
import qualified Data.HashSet as HashSet
import Data.Int (Int64)
import Data.List (foldl', intercalate, nub, sortBy)
import Data.List.Extra (groupBy, intercalate, isSuffixOf, replace, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text, concat, isInfixOf, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Time
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Directory.Internal.Prelude hiding (mapM, mapM_)
import System.IO (writeFile)
-- import Streamly.Internal.Data.Stream (fromList,mapM_,mapM,toList)
-- import Streamly (parallely, serially)
-- import Streamly.Prelude hiding (concatMap, foldl', init, length, map, splitOn,intercalate)

plugin :: Plugin
plugin =
    defaultPlugin
        { installCoreToDos = install
        , pluginRecompile = purePlugin
        }
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args todos = return (CoreDoPluginPass "configExtract" (buildCfgPass args) : todos)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e

mkLLit :: Literal -> Text
mkLLit (LitChar char) = (pack [char])
#if __GLASGOW_HASKELL__ >= 900
mkLLit (LitNumber litNumType val) = (T.replace "\n" "" $ pack $ show val)
mkLLit (LitRubbish _) = pack "Rubbish"
#else
mkLLit (LitNumber litNumType val _) = (T.replace "\n" "" $ pack $ show val)
mkLLit LitRubbish = pack "Rubbish"
#endif
mkLLit (LitString bs) =
    case decodeUtf8' bs of
        Left _ -> T.replace "\n" "" $ pack $ show bs
        Right val -> val
mkLLit LitNullAddr = pack "Null"
mkLLit (LitFloat rational) = (T.replace "\n" "" $ pack $ show rational)
mkLLit (LitDouble rational) = (T.replace "\n" "" $ pack $ show rational)
mkLLit (LitLabel fs _ IsData) = (T.replace "\n" "" $ pack $ unpackFS fs)
mkLLit (LitLabel fs _ IsFunction) = (T.replace "\n" "" $ pack $ unpackFS fs)

buildCfgPass :: [CommandLineOption] -> ModGuts -> CoreM ModGuts
buildCfgPass opts guts = do
    let prefixPath = case opts of
            [] -> "./tmp/configExtract/"
            [local] -> local
            _ -> error "unexpected no of arguments"
    _ <- liftIO $ do
        let binds = mg_binds guts
            moduleN = moduleNameString $ GhcPlugins.moduleName $ mg_module guts
            moduleLoc = prefixPath Prelude.<> getFilePath (mg_loc guts)
        createDirectoryIfMissing True ((Data.List.Extra.intercalate "/" . init . splitOn "/") moduleLoc)
        removeIfExists (moduleLoc Prelude.<> ".configExtract.ast.show.jsonL")
        when (not $ checkIfExists (pack moduleN) filterModules) $ do
            let fBinds = flattenBinds binds
            coreFunctionsList <-
                        mapM
                            ( \(b, e) -> do
                                let nssName = (pack $ nameStableString $ idName b)
                                let funName = (pack $ showSDocUnsafe $ ppr $ idName b)
                                if shouldProcessBind funName nssName
                                    then do
                                        allVars <- pure $ map (pack . nameStableString . idName) $ collectAllVars e
                                        allLits <- pure $ nub $ map (mkLLit) $ collectAllLits e
                                        allTypes <- pure $ map (pack . showSDocUnsafe . ppr) $ collectAllTypes e
                                        allCaseRelations <- pure $ collectAllCaseAndRelations e
                                        pure ([(CoreFunction allVars allLits allTypes allCaseRelations nssName)])
                                    else mempty
                            )
                            (fBinds)
            -- l <- toList $ serially $ mapM (liftIO . toLBind) (fromList binds)
            mapM_   ( \x ->
                            (liftIO . DBS.appendFile (moduleLoc Prelude.<> ".configExtract.ast.show.jsonL") . (<> "\n") . toStrict . encode) x
                        )
                        (Prelude.concat coreFunctionsList)
        pure ()
    return guts

shouldProcessBind funName nssName =
    if ((checkIfExists funName filterList) || (checkIfExists nssName filterList) || ("Config.Options$$" `T.isInfixOf` funName) || ("$$tc" `T.isInfixOf` nssName) || ("$_in$$" `T.isPrefixOf` nssName))
        then False
        else True

getFilePath :: SrcSpan -> String
#if __GLASGOW_HASKELL__ >= 900
getFilePath (RealSrcSpan rSSpan _) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) = showSDocUnsafe $ ppr $ fs
#else
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) = unpackFS fs
#endif

filterList =
    [ "show"
    , "showsPrec"
    , "from"
    , "to"
    , "toConstr"
    , "toDomResAcc"
    , "toEncoding"
    , "toEncodingList"
    , "toEnum"
    , "toForm"
    , "toHaskellString"
    , "toInt"
    , "toJSON"
    , "toJSONList"
    , "toJSONWithOptions"
    , "gfoldl"
    , "ghmParser"
    , "gmapM"
    , "gmapMo"
    , "gmapMp"
    , "gmapQ"
    , "gmapQi"
    , "gmapQl"
    , "gmapQr"
    , "gmapT"
    , "parseField"
    , "parseJSON"
    , "parseJSONList"
    , "parseJSONWithOptions"
    , "hasField"
    , "gunfold"
    , "getField"
    , "_mapObjectDeep'"
    , "_mapObjectDeep"
    , "_mapObjectDeepForSnakeCase"
    , "!!"
    , "/="
    , "<"
    , "<="
    , "<>"
    , "<$"
    , "=="
    , ">"
    , ">="
    , "readsPrec"
    , "readPrec"
    , "toDyn"
    , "fromDyn"
    , "fromDynamic"
    , "compare"
    , "readListPrec"
    , "toXml"
    , "fromXml"
    , "d~"
    , "dMonad"
    , "dApplicative"
    , "dFunctor"
    , "dMonadState"
    , "$_sys$$dIP"
    , "$_sys$$d~"
    , "$_sys$$dMonadFlow"
    , "$_sys$$"
    , "$_sys$$krep"
    ]

checkIfExists :: Text -> [Text] -> Bool
checkIfExists query [] = False
checkIfExists query (x : xs) = if query `T.isInfixOf` x then True else checkIfExists query xs

toLBind :: CoreBind -> IO [Node]
toLBind x@(NonRec binder expr) = do
    let nssName = (pack $ nameStableString $ idName binder)
    let funName = (pack $ showSDocUnsafe $ ppr $ idName binder)
    if ((checkIfExists funName filterList) || (checkIfExists nssName filterList) || ("Config.Options$$" `T.isPrefixOf` funName) || (("$_in$$" `T.isPrefixOf` nssName) && (not $ "$_in$$sel:" `T.isPrefixOf` nssName)))
        then pure mempty
        else do
            print nssName
            allVars <- pure $ collectAllVars expr
            allLits <- pure $ collectAllLits expr
            allTypes <- pure $ collectAllTypes expr
            allCaseRelations <- pure $ collectAllCaseAndRelations expr
            print $ (\x -> ("allVars", x)) $ map (pack . nameStableString . idName) allVars
            print $ (\x -> ("allLits", x)) $ map (showSDocUnsafe . ppr) allLits
            print $ (\x -> ("allTypes", x)) $ map (showSDocUnsafe . ppr) allTypes
            print $ (\x -> ("allCaseRelations", x)) $ encode allCaseRelations
            res <- traverseExpr mempty (pack $ nameStableString $ idName binder) expr
            pure $ [NFunction (pack $ nameStableString $ idName binder) res]
toLBind (Rec binds) = do
    r <-
        mapM
            ( \x@(b, e) -> do
                let funName = (pack $ showSDocUnsafe $ ppr $ idName b)
                let nssName = (pack $ nameStableString $ idName b)
                if ((checkIfExists funName filterList) || (checkIfExists nssName filterList) || (("$_in$$" `T.isPrefixOf` nssName) && (not $ "$_in$$sel:" `T.isPrefixOf` nssName)))
                    then pure mempty
                    else do
                        print nssName
                        allVars <- pure $ collectAllVars e
                        allLits <- pure $ collectAllLits e
                        allTypes <- pure $ collectAllTypes e
                        allCaseRelations <- pure $ collectAllCaseAndRelations e
                        print $ (\x -> ("allVars", x)) $ map (pack . nameStableString . idName) allVars
                        print $ (\x -> ("allLits", x)) $ map (showSDocUnsafe . ppr) allLits
                        print $ (\x -> ("allTypes", x)) $ map (showSDocUnsafe . ppr) allTypes
                        print $ (\x -> ("allCaseRelations", x)) $ encode allCaseRelations
                        res <- traverseExpr mempty (pack $ nameStableString (idName b)) e
                        pure $ [NFunction (pack $ nameStableString $ idName b) res]
            )   (binds)
    pure $ Prelude.concat r

typeOfNumber :: LitNumType -> String
typeOfNumber LitNumInteger = "LitNumInteger"
typeOfNumber LitNumNatural = "LitNumNatural"
typeOfNumber LitNumInt = "LitNumInt"
typeOfNumber LitNumInt64 = "LitNumInt64"
typeOfNumber LitNumWord = "LitNumWord"
typeOfNumber LitNumWord64 = "LitNumWord64"

mkConstant :: Literal -> Node
mkConstant (LitChar char) = NConstant "LitChar" (pack [char]) False
#if __GLASGOW_HASKELL__ >= 900
mkConstant (LitNumber litNumType val) = NConstant (pack $ typeOfNumber litNumType) (pack $ show val) False
mkConstant (LitRubbish _) = NConstant "LitRubbish" "" False
#else
mkConstant (LitNumber litNumType val _) = NConstant (pack $ typeOfNumber litNumType) (pack $ show val) False
mkConstant LitRubbish = NConstant "LitRubbish" "" False
#endif
mkConstant (LitString bs) = NConstant "LitString" (pack $ show bs) False
mkConstant LitNullAddr = NConstant "LitNullAddr" "" False
mkConstant (LitFloat rational) = NConstant "LitFloat" (pack $ show rational) False
mkConstant (LitDouble rational) = NConstant "LitDouble" (pack $ show rational) False
mkConstant (LitLabel fs _ IsData) = NConstant "LitLabel" (pack $ unpackFS fs) False
mkConstant (LitLabel fs _ IsFunction) = NConstant "LitLabel" (pack $ unpackFS fs) True

mkCFunction x = CalledFunction (pack $ nameStableString $ idName x) (pack $ showSDocUnsafe $ ppr $ tyVarKind x) (pack $ showSDocUnsafe $ ppr $ getSrcSpan $ getName x)
mkType x = NType $ nub $ map pack $ extractVarFromType x

extractVarFromType :: Type -> [String]
extractVarFromType = go
  where
    go :: Type -> [String]
    go (TyVarTy v) = [(nameStableString $ varName v)]
    go (TyConApp haskellTypeT z) = [(nameStableString $ GhcPlugins.tyConName haskellTypeT)] <> concatMap go z
    go (AppTy a b) = go a <> go b
    go (ForAllTy _ t) = go t
#if __GLASGOW_HASKELL__ >= 900
    go (FunTy _ arg res _) = go arg <> go res
#else
    go (FunTy _ arg res) = go arg <> go res
#endif
    go (LitTy x) = [showSDocUnsafe $ ppr x]
    go _ = mempty

filterModules = (["euler-events-hs", "Utils.PrestoBackend.Shims", "Euler.Utils.Utils.Shims", "Euler.Events.", "Euler.WebService.Logging", "Euler.Analytics.Tracker", "Euler.Utils.Utils.Shims", "Config.Options", "Euler.Events.MetricApi.MetricApi", "Engineering.Flow", "Utils.Metric.Shims", "Utils.Log.Shims", ".Types", "Utils.Metric.Shims", "EulerHS.Framework.Language", "Euler.Analytics.Tracker"] :: [Text])
filterFunctions = ["$$fShow"]

shouldExpand (App f a) = shouldExpand f || shouldExpand a
shouldExpand (Lam _ _) = True
shouldExpand (Let _ _) = True
shouldExpand (Case _ _ _ _) = True
shouldExpand (Tick _ _) = True
shouldExpand (Cast _ _) = True
shouldExpand _ = False

traverseExpr :: HashSet.HashSet Text -> Text -> Expr Var -> IO [Node]
traverseExpr addedList functionName (Var x) = do
    if checkIfExists (pack $ nameStableString $ idName x) filterList
        then pure $ mempty
        else do
            if ("findByName" `T.isInfixOf` (pack $ nameStableString $ idName x)) then pure $ [mkCFunction x] else pure $ mempty
traverseExpr addedList functionName (Lit x) = pure $ [mkConstant x]
traverseExpr addedList functionName (Type x) = pure $ [mkType x]
traverseExpr addedList functionName x@(App func args) = do
    if shouldExpand x
        then do
            f <- traverseExpr (addedList) functionName func
            a <- traverseExpr (addedList) functionName args
            pure $ a <> f
        else do
            (fExpr, argsExpr) <- pure $ collectArgs x
            case fExpr of
                (Var _id) -> do
                    let name = pack $ nameStableString $ idName _id
                        _type = pack $ showSDocUnsafe $ ppr $ tyVarKind _id
                    -- when True $ --(not $ checkIfExists name (filterModules <> filterFunctions)) $ do
                    --     print (name, T.replace "  " " " $ T.replace "\n" "" $ pack $ showSDocUnsafe $ ppr x,map (T.replace "\n" "" . pack . showSDocUnsafe . ppr) argsExpr)
                    pure $ [DFunction name (T.replace "\n" "" $ pack $ showSDocUnsafe $ ppr x)]
                _ -> pure mempty
traverseExpr addedList functionName (Lam func args) = do
    res <- traverseExpr addedList functionName args
    pure $ [NLambda (pack $ nameStableString $ idName func) res]
traverseExpr addedList functionName (Let func args) = do
    a <- traverseExpr addedList functionName args
    f <- toLBind func
    pure $ a <> f
traverseExpr addedList functionName (Case condition bind _type alts) = do
    c <- traverseExpr addedList functionName condition
    a <- mapM (toLAlt functionName) (alts)
    pure $ [NCase (pack $ showSDocUnsafe $ ppr condition, c) a]
  where
#if __GLASGOW_HASKELL__ >= 900
    toLAlt :: Text -> Alt Var -> IO ((FRelation, [Node], [Node]))
    toLAlt x (Alt a b c) = toLAlt' x (a,b,c)
        where
            toLAlt' :: Text -> (AltCon, [Var], CoreExpr) -> IO ((FRelation, [Node], [Node]))
            toLAlt' functionName (y@(DataAlt dataCon), val, e) = do
                res <- traverseExpr addedList functionName e
                pure $ (pack $ showSDocUnsafe $ ppr $ y, Prelude.map (mkCFunction) val, res)
            toLAlt' functionName (y@(LitAlt lit), val, e) = do
                res <- traverseExpr addedList functionName e
                pure $ (pack $ showSDocUnsafe $ ppr $ y, Prelude.map (mkCFunction) val, res)
            toLAlt' functionName (y@DEFAULT, val, e) = do
                res <- traverseExpr addedList functionName e
                pure $ (pack $ showSDocUnsafe $ ppr $ y, Prelude.map (mkCFunction) val, res)
#else
    toLAlt :: Text -> (AltCon, [Var], CoreExpr) -> IO ((FRelation, [Node], [Node]))
    toLAlt functionName (y@(DataAlt dataCon), val, e) = do
        res <- traverseExpr addedList functionName e
        pure $ (pack $ showSDocUnsafe $ ppr $ y, Prelude.map (mkCFunction) val, res)
    toLAlt functionName (y@(LitAlt lit), val, e) = do
        res <- traverseExpr addedList functionName e
        pure $ (pack $ showSDocUnsafe $ ppr $ y, Prelude.map (mkCFunction) val, res)
    toLAlt functionName (y@DEFAULT, val, e) = do
        res <- traverseExpr addedList functionName e
        pure $ (pack $ showSDocUnsafe $ ppr $ y, Prelude.map (mkCFunction) val, res)
#endif
traverseExpr addedList functionName (Tick _ expr) = traverseExpr addedList functionName expr
traverseExpr addedList functionName (Cast expr _) = traverseExpr addedList functionName expr
traverseExpr addedList functionName _ = pure mempty

collectAllVars :: CoreExpr -> [Id]
collectAllVars d =
    ok d
  where
    ok :: CoreExpr -> [Id]
    ok e = go e

    go (Var v) = [v]
    go (Cast e _) = go e

    go (Case scrut _ _ alts) =
#if __GLASGOW_HASKELL__ >= 900
        ok scrut <> concatMap (\((Alt zz lv rhs)) -> getDataAltId zz <> (lv) <> go rhs) alts
#else
        ok scrut <> concatMap (\((zz,lv,rhs) :: (AltCon, [Var], CoreExpr)) -> getDataAltId zz <> (lv) <> go rhs) alts
#endif
    go (Tick t e) = go e
    go (Lam x e) = go e
    go (App f e) = go f <> go e
    go (Let (NonRec _ r) e) = go e <> ok r
    go (Let (Rec prs) e) = go e <> concatMap (\(b, e) -> [b] <> go e) prs
    go (Lit{}) = []
    go (Type{}) = []
    go (Coercion{}) = []

getLitAlt (LitAlt x) = [x]
getLitAlt _ = []

getDataAlt (DataAlt x) = [tyConKind $ dataConTyCon x]
getDataAlt _ = []

getDataAltId (DataAlt x) = [dataConWorkId x]
getDataAltId _ = []

collectAllLits :: CoreExpr -> [Literal]
collectAllLits d =
    ok d
  where
    ok :: CoreExpr -> [Literal]
    ok e = go e

    go (Var v) = []
    go (Cast e _) = go e
    go (Case scrut _ _ alts) =
#if __GLASGOW_HASKELL__ >= 900
        ok scrut <> concatMap (\((Alt zz lv rhs)) -> getLitAlt zz <> go rhs) alts
#else
        ok scrut <> concatMap (\((zz,lv,rhs) :: (AltCon, [Var], CoreExpr)) -> getLitAlt zz <> go rhs) alts
#endif
    go (Tick t e)= go e
    go (Lam x e) = go e
    go (App f e) = go f <> go e
    go (Let (NonRec _ r) e) = go e <> ok r
    go (Let (Rec prs) e) = go e <> concatMap (\(b, e) -> go e) prs
    go (Lit literal) = [literal]
    go (Type{}) = []
    go (Coercion{}) = []

collectAllTypes :: CoreExpr -> [Type]
collectAllTypes d =
    ok d
  where
    ok :: CoreExpr -> [Type]
    ok e = go e

    go (Var v) = []
    go (Cast e _) = go e
    go (Case scrut _ _ alts) =
#if __GLASGOW_HASKELL__ >= 900
        ok scrut <> concatMap (\((Alt zz lv rhs)) -> getDataAlt zz <> go rhs) alts
#else
        ok scrut <> concatMap (\((zz,lv,rhs) :: (AltCon, [Var], CoreExpr)) -> getDataAlt zz <> go rhs) alts
#endif
    go (Tick t e) = go e
    go (Lam x e) = go e
    go (App f e) = go f <> go e
    go (Let (NonRec _ r) e) = go e <> ok r
    go (Let (Rec prs) e) = go e <> concatMap (\(b, e) -> go e) prs
    go (Lit literal) = []
    go (Type _type) = [_type]
    go (Coercion{}) = []

collectAllCaseAndRelations :: CoreExpr -> [CaseCollate]
collectAllCaseAndRelations d =
    ok d
  where
    ok :: CoreExpr -> [CaseCollate]
    ok e = go e

    go (Var v) = []
    go (Cast e _) = go e
    go (Case scrut _ _ alts) =
#if __GLASGOW_HASKELL__ >= 900
        [ CaseCollate
            (pack $ showSDocUnsafe $ ppr scrut)
            ( map
                ( \(Alt zz _ rhs) ->
                    let inSideCase = go rhs
                     in (getEitherTextTypeFromAlt zz, inSideCase)
                )
                alts
            )
        ]
#else
        [ CaseCollate
            (pack $ showSDocUnsafe $ ppr scrut)
            ( map
                ( \(zz, _, rhs) ->
                    let inSideCase = go rhs
                     in (getEitherTextTypeFromAlt zz, inSideCase)
                )
                alts
            )
        ]
#endif
    go (Tick t e) = go e
    go (Lam x e) = go e
    go (App f e) = go f <> go e
    go (Let (NonRec _ r) e) = go e <> ok r
    go (Let (Rec prs) e) = go e <> concatMap (\(b, e) -> go e) prs
    go (Lit literal) = []
    go (Type _type) = []
    go (Coercion{}) = []

collectAllNestesPerCaseRelation :: CoreExpr -> [CaseCollate]
collectAllNestesPerCaseRelation d =
    ok d
  where
    ok :: CoreExpr -> [CaseCollate]
    ok e = go e

    go (Var v) = []
    go (Cast e _) = go e
    go (Case scrut _ _ alts) =
#if __GLASGOW_HASKELL__ >= 900
        [ CaseCollate
            (pack $ showSDocUnsafe $ ppr scrut)
            ( map
                ( \(Alt zz _ rhs) ->
                    let inSideCase = go rhs
                     in (getEitherTextTypeFromAlt zz, inSideCase)
                )
                alts
            )
        ]
#else
        [ CaseCollate
            (pack $ showSDocUnsafe $ ppr scrut)
            ( map
                ( \(zz, _, rhs) ->
                    let inSideCase = go rhs
                     in (getEitherTextTypeFromAlt zz, inSideCase)
                )
                alts
            )
        ]
#endif
    go (Tick t e) = go e
    go (Lam x e) = go e
    go (App f e) = go f <> go e
    go (Let (NonRec _ r) e) = go e <> ok r
    go (Let (Rec prs) e) = go e <> concatMap (\(b, e) -> go e) prs
    go (Lit literal) = []
    go (Type _type) = []
    go (Coercion{}) = []

getEitherTextTypeFromAlt (DataAlt x) = pack $ showSDocUnsafe $ ppr x
getEitherTextTypeFromAlt (LitAlt x) = pack $ showSDocUnsafe $ ppr x
getEitherTextTypeFromAlt _ = "DEFAULT"
