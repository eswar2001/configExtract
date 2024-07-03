
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, DeriveDataTypeable, DeriveAnyClass,RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ConfigExtract.Plugin (plugin) where

import Control.Concurrent (MVar, modifyMVar, newMVar)
import CoreMonad (CoreM, CoreToDo (CoreDoPluginPass), liftIO)
import CoreSyn
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as DBS
import Data.ByteString.Lazy (toStrict)
import Data.Int (Int64)
import Data.List.Extra (intercalate, isSuffixOf, replace, splitOn,groupBy)
import Data.List ( sortBy, intercalate ,foldl',nub)
import qualified Data.Map as Map
import Data.Text (Text, concat, isInfixOf, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8,decodeUtf8')
import Data.Time
import TyCoRep
import GHC.IO (unsafePerformIO)
import GHC.Hs
import Data.Map (Map)
import Data.Data
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class (liftIO)
import System.IO (writeFile)
import GHC.Hs.Decls
import GhcPlugins (
    CommandLineOption,Arg (..),
    HsParsedModule(..),collectArgs,
    Hsc,
    Name,SDoc,DataCon,DynFlags,ModSummary(..),TyCon,
    Literal (..),typeEnvElts,
    ModGuts (mg_binds, mg_loc, mg_module),showSDoc,
    Module (moduleName),tyConKind,
    NamedThing (getName),getDynFlags,tyConDataCons,dataConOrigArgTys,dataConName,
    Outputable (..),dataConFieldLabels,
    Plugin (..),
    Var,flLabel,dataConRepType,
    coVarDetails,
    defaultPlugin,
    idName,
    mkInternalName,
    mkLitString,
    mkLocalVar,
    mkVarOcc,
    moduleNameString,
    nameStableString,
    noCafIdInfo,
    purePlugin,
    showSDocUnsafe,
    tyVarKind,
    unpackFS,
    tyConName
 )
import GhcPlugins
    ( Plugin(installCoreToDos, pluginRecompile),
      unpackFS,
      idName,
      coVarDetails,
      noCafIdInfo,
      mkLitString,
      moduleNameString,
      mkInternalName,
      nameStableString,
      mkVarOcc,
      showSDocUnsafe,
      defaultPlugin,
      purePlugin,
      mkLocalVar,
      tyVarKind,
      ModGuts(mg_binds, mg_module, mg_loc),
      Module(moduleName),
      Outputable(ppr),
      CommandLineOption, Var, NamedThing (getName), Literal (..), FunctionOrData (..), LitNumType (..) )
import qualified Data.HashSet as HashSet
import Id (isExportedId,idType,Id)
import Name (getSrcSpan)
import Control.Monad (forM)
import SrcLoc
import Streamly (parallely, serially)
import Streamly.Prelude hiding (concatMap, init, length, map, splitOn,foldl')
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Directory.Internal.Prelude hiding (mapM, mapM_)
import Unique (mkUnique)
import Var (isLocalId,varType,varName)
import Prelude hiding (id, mapM, mapM_)
import ConfigExtract.Types
import TcRnTypes
import TcRnMonad
import DataCon
import Control.Exception (evaluate)
import GHC.Generics (Generic)

plugin :: Plugin
plugin =
    defaultPlugin
        { installCoreToDos = install
        , pluginRecompile = GhcPlugins.purePlugin
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
mkLLit (LitChar   char) = (pack [char]) 
mkLLit (LitNumber litNumType val _) = (T.replace "\n" "" $ pack $ show val) 
mkLLit (LitString  bs) = 
    case decodeUtf8' bs of
        Left _ -> T.replace "\n" "" $ pack $ show bs
        Right val -> val
mkLLit LitNullAddr = pack "Null" 
mkLLit LitRubbish =  pack "Rubbish" 
mkLLit (LitFloat   rational) = (T.replace "\n" "" $ pack $ show rational) 
mkLLit (LitDouble  rational) = (T.replace "\n" "" $ pack $ show rational) 
mkLLit (LitLabel   fs _ IsData) = (T.replace "\n" "" $ pack $ unpackFS fs) 
mkLLit (LitLabel   fs _ IsFunction) = (T.replace "\n" "" $ pack $ unpackFS fs) 

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
        createDirectoryIfMissing True ((intercalate "/" . init . splitOn "/") moduleLoc)
        removeIfExists (moduleLoc Prelude.<> ".configExtract.ast.show.jsonL")
        when (not $ checkIfExists (pack moduleN) filterModules) $ do
            let fBinds = flattenBinds binds
            coreFunctionsList <- toList $ serially $ mapM (\(b,e) -> do
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
                ) (fromList fBinds)
            -- l <- toList $ serially $ mapM (liftIO . toLBind) (fromList binds)
            drain $ serially $
                mapM_ (\x -> 
                    (liftIO . DBS.appendFile (moduleLoc Prelude.<> ".configExtract.ast.show.jsonL") . (<> "\n") . toStrict . encode) x
                ) (fromList $ Prelude.concat coreFunctionsList)
        pure ()    
    return guts

shouldProcessBind funName nssName = 
    if ((checkIfExists funName filterList) || (checkIfExists nssName filterList) || ("Config.Options$$" `T.isInfixOf` funName) || ("$$tc" `T.isInfixOf` nssName) || ("$_in$$" `T.isPrefixOf` nssName))
        then False
        else True

getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) = unpackFS fs

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
    ,  "dMonad"
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
checkIfExists query (x:xs) = if query `T.isInfixOf` x then True else checkIfExists query xs

toLBind :: CoreBind -> IO [Node]
toLBind x@(NonRec binder expr) = do
    let nssName = (pack $ nameStableString $ idName binder)
    let funName = (pack $ showSDocUnsafe $ ppr $ idName binder)
    if ((checkIfExists funName filterList) || (checkIfExists nssName filterList) || ("Config.Options$$" `T.isPrefixOf` funName) || (("$_in$$" `T.isPrefixOf` nssName) && (not $ "$_in$$sel:" `T.isPrefixOf` nssName )))
        then pure mempty
        else do
            print nssName
            allVars <- pure $ collectAllVars expr
            allLits <- pure $ collectAllLits expr
            allTypes <- pure $ collectAllTypes expr
            allCaseRelations <- pure $ collectAllCaseAndRelations expr
            print $ (\x -> ("allVars",x)) $ map (pack . nameStableString . idName) allVars
            print $ (\x -> ("allLits",x)) $ map (showSDocUnsafe . ppr) allLits
            print $ (\x -> ("allTypes",x)) $ map (showSDocUnsafe . ppr) allTypes
            print $ (\x -> ("allCaseRelations",x)) $ encode allCaseRelations
            res <- traverseExpr mempty (pack $ nameStableString $ idName binder) expr
            pure $ [NFunction (pack $ nameStableString $ idName binder) res]
toLBind (Rec binds) = do
    r <-
        toList $
            serially $
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
                                print $ (\x -> ("allVars",x)) $ map (pack . nameStableString . idName) allVars
                                print $ (\x -> ("allLits",x)) $ map (showSDocUnsafe . ppr) allLits
                                print $ (\x -> ("allTypes",x)) $ map (showSDocUnsafe . ppr) allTypes
                                print $ (\x -> ("allCaseRelations",x)) $ encode allCaseRelations
                                res <- traverseExpr mempty (pack $ nameStableString (idName b)) e
                                pure $ [NFunction (pack $ nameStableString $ idName b) res]
                    )
                    (fromList binds)
    pure $ Prelude.concat r

typeOfNumber :: LitNumType -> String
typeOfNumber LitNumInteger = "LitNumInteger"
typeOfNumber LitNumNatural = "LitNumNatural"
typeOfNumber LitNumInt     = "LitNumInt"
typeOfNumber LitNumInt64   = "LitNumInt64"
typeOfNumber LitNumWord    = "LitNumWord"
typeOfNumber LitNumWord64  = "LitNumWord64"

mkConstant :: Literal -> Node
mkConstant (LitChar   char) = NConstant "LitChar" (pack [char]) False
mkConstant (LitNumber litNumType val _) = NConstant (pack $ typeOfNumber litNumType)  (pack $ show val) False
mkConstant (LitString  bs) = NConstant "LitString" (pack $ show bs) False
mkConstant LitNullAddr = NConstant "LitNullAddr" "" False
mkConstant LitRubbish = NConstant "LitRubbish" "" False
mkConstant (LitFloat   rational) = NConstant "LitFloat" (pack $ show rational) False
mkConstant (LitDouble  rational) = NConstant "LitDouble" (pack $ show rational) False
mkConstant (LitLabel   fs _ IsData) = NConstant "LitLabel" (pack $ unpackFS fs) False
mkConstant (LitLabel   fs _ IsFunction) = NConstant "LitLabel" (pack $ unpackFS fs) True

mkCFunction x = CalledFunction (pack $ nameStableString $ idName x) (pack $ showSDocUnsafe $ ppr $ tyVarKind x) (pack $ showSDocUnsafe $ ppr $ getSrcSpan $ getName x)
mkType x = NType $ nub $ map pack $ extractVarFromType x

extractVarFromType :: Type -> [String]
extractVarFromType = go
    where
        go :: Type -> [String]
        go (TyVarTy v) = [(nameStableString $ varName v)]
        go (TyConApp haskellTypeT z) = [(nameStableString $ GhcPlugins.tyConName haskellTypeT)] <> concatMap go z
        go (AppTy a b) = go a <>  go b
        go (ForAllTy _ t) = go t
        go (FunTy _ arg res) = go arg <> go res
        go (LitTy x) = [showSDocUnsafe $ ppr x]
        go _ = mempty


filterModules = (["euler-events-hs","Utils.PrestoBackend.Shims","Euler.Utils.Utils.Shims","Euler.Events.","Euler.WebService.Logging","Euler.Analytics.Tracker","Euler.Utils.Utils.Shims","Config.Options","Euler.Events.MetricApi.MetricApi","Engineering.Flow","Utils.Metric.Shims","Utils.Log.Shims",".Types","Utils.Metric.Shims","EulerHS.Framework.Language","Euler.Analytics.Tracker"] :: [Text])
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
            (fExpr,argsExpr) <- pure $ collectArgs x
            case fExpr of
                (Var _id) -> do 
                    let name = pack $ nameStableString $ idName _id
                        _type =  pack $ showSDocUnsafe $ ppr $ tyVarKind _id
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
    a <- toList $ serially $ mapM (toLAlt functionName) (fromList alts)
    pure $ [NCase (pack $ showSDocUnsafe $ ppr condition,c) a]
    where
        toLAlt :: Text -> (AltCon, [Var], CoreExpr) -> IO ((FRelation,[Node],[Node]))
        toLAlt functionName (y@(DataAlt dataCon), val, e) = do
            res <- traverseExpr addedList functionName e
            pure $ (pack $ showSDocUnsafe $ ppr $ y,Prelude.map (mkCFunction) val,res)
        toLAlt functionName (y@(LitAlt lit), val, e) = do
            res <- traverseExpr addedList functionName e
            pure $ (pack $ showSDocUnsafe $ ppr $ y,Prelude.map (mkCFunction) val,res)
        toLAlt functionName (y@DEFAULT, val, e) = do
            res <- traverseExpr addedList functionName e
            pure $ (pack $ showSDocUnsafe $ ppr $ y,Prelude.map (mkCFunction) val,res)
traverseExpr addedList functionName (Tick _ expr) = traverseExpr addedList functionName expr
traverseExpr addedList functionName (Cast expr _) = traverseExpr addedList functionName expr
traverseExpr addedList functionName _ = pure mempty

collectAllVars :: CoreExpr -> [Id]
collectAllVars d
  = ok d
  where
    ok :: CoreExpr -> [Id]
    ok e = go e

    go (Var v)                      = [v]
    go (Cast e _)                   = go e
    go (Case scrut _ _ alts)        = ok scrut <> concatMap (\((zz,lv,rhs) :: (AltCon, [Var], CoreExpr)) -> getDataAltId zz <> (lv) <> go rhs) alts
    go (Tick t e) | tickishCounts t = []
                    | otherwise     = go e
    go (Lam x e)                    = go e
    go (App f e)                    = go f <> go e
    go (Let (NonRec _ r) e)         = go e <> ok r
    go (Let (Rec prs) e)            = go e <> concatMap (\(b,e) -> [b] <> go e) prs
    go (Lit {})                     = []
    go (Type {})                    = []
    go (Coercion {})                = []

getLitAlt (LitAlt x) = [x]
getLitAlt _ = []

getDataAlt (DataAlt x) = [tyConKind $ dataConTyCon x]
getDataAlt _ = []

getDataAltId (DataAlt x) = [dataConWorkId x]
getDataAltId _ = []

collectAllLits :: CoreExpr -> [Literal]
collectAllLits d
  = ok d
  where
    ok :: CoreExpr -> [Literal]
    ok e = go e

    go (Var v)                      = []
    go (Cast e _)                   = go e
    go (Case scrut _ _ alts)        = ok scrut <> concatMap (\(zz,_,rhs) -> getLitAlt zz <> go rhs) alts
    go (Tick t e) | tickishCounts t = []
                    | otherwise     = go e
    go (Lam x e)                    = go e
    go (App f e)                    = go f <> go e
    go (Let (NonRec _ r) e)         = go e <> ok r
    go (Let (Rec prs) e)            = go e <> concatMap (\(b,e) -> go e) prs
    go (Lit literal)                = [literal]
    go (Type {})                    = []
    go (Coercion {})                = []

collectAllTypes :: CoreExpr -> [Type]
collectAllTypes d
  = ok d
  where
    ok :: CoreExpr -> [Type]
    ok e = go e

    go (Var v)                      = []
    go (Cast e _)                   = go e
    go (Case scrut _ _ alts)        = ok scrut <> concatMap (\(zz,_,rhs) -> getDataAlt zz <> go rhs) alts
    go (Tick t e) | tickishCounts t = []
                    | otherwise     = go e
    go (Lam x e)                    = go e
    go (App f e)                    = go f <> go e
    go (Let (NonRec _ r) e)         = go e <> ok r
    go (Let (Rec prs) e)            = go e <> concatMap (\(b,e) -> go e) prs
    go (Lit literal)                = []
    go (Type _type)                 = [_type]
    go (Coercion {})                = []

collectAllCaseAndRelations :: CoreExpr -> [CaseCollate]
collectAllCaseAndRelations d
  = ok d
  where
    ok :: CoreExpr -> [CaseCollate]
    ok e = go e

    go (Var v)                      = []
    go (Cast e _)                   = go e
    go (Case scrut _ _ alts)        = 
        [CaseCollate (pack $ showSDocUnsafe $ ppr scrut) (map (\(zz,_,rhs) -> 
                let inSideCase = go rhs 
                in (getEitherTextTypeFromAlt zz,inSideCase)
            ) alts)]
    go (Tick t e) | tickishCounts t = []
                    | otherwise     = go e
    go (Lam x e)                    = go e
    go (App f e)                    = go f <> go e
    go (Let (NonRec _ r) e)         = go e <> ok r
    go (Let (Rec prs) e)            = go e <> concatMap (\(b,e) -> go e) prs
    go (Lit literal)                = []
    go (Type _type)                 = []
    go (Coercion {})                = []

getEitherTextTypeFromAlt (DataAlt x) = pack $ showSDocUnsafe $ ppr x
getEitherTextTypeFromAlt (LitAlt x) = pack $ showSDocUnsafe $ ppr x
getEitherTextTypeFromAlt _ = "DEFAULT"
