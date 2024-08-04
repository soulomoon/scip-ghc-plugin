{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SCIPGHCPlugin (plugin) where

import GHC.Plugins hiding ((<>))
import GHC.Tc.Types
import GHC.Hs
import Ext.Ast
import Ext.Types
import Proto.Scip (Document, PositionEncoding (UTF32CodeUnitOffsetFromLineStart), Occurrence, Symbol, Package, SymbolInformation, Descriptor, Diagnostic)
import qualified Proto.Scip_Fields
import Data.ProtoLens (Message(..))
import Lens.Family2 ((&), (.~), (^.))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Version (showVersion)
import GHC.Tc.Utils.Monad (getTopEnv)
import GHC.Unit.Env (ue_units)
import qualified Data.Map.Strict as Map
import Ext.Utils (getNodeIds, recoverFullType, renderHieType, generateReferencesMap, RefMap)
import EntityInfoKind (entityInfosKind)
import Control.Arrow (Arrow (..))
import Data.Functor.Identity (Identity(..))
import Data.ProtoLens.Encoding (encodeMessage)
import System.FilePath ((</>), takeDirectory)
import qualified Data.ByteString as BS
import Control.Monad (forM_)
import qualified System.Directory as BS
import GHC.Utils.Logger (LogAction, pushLogHook, LogFlags (log_default_dump_context))
import GHC.Utils.Json (json, renderJSON)
import GHC.Utils.Error (MessageClass (..))
import qualified GHC.Types.Error as GHC
import qualified Proto.Scip as SCIP
import qualified Data.ByteString.Char8 as BC

-- This is the entry point for the plugin
plugin :: Plugin
plugin = defaultPlugin {
    driverPlugin = myDriverPlugin,
    typeCheckResultAction = typechecked,
    renamedResultAction = renamedResult,
    parsedResultAction = myParsedResultAction
}
-- create file
createNewFile :: FilePath -> BS.ByteString -> IO ()
createNewFile path encoded = do
        liftIO $ BS.createDirectoryIfMissing True $ takeDirectory path
        liftIO $ BS.writeFile path encoded

diagnosticDataFile :: FilePath
diagnosticDataFile = ".data/diagnosticDataFile.diag"

myDriverPlugin :: [CommandLineOption] -> HscEnv -> IO HscEnv
myDriverPlugin _opts env = do
    let logger = hsc_logger env
    let newLogger = pushLogHook loggerHook logger
    -- trunk the diagnostic data file if existed
    createNewFile diagnosticDataFile ""
    -- putStrLn "Hello from the driver plugin!"
    return env { hsc_logger = newLogger }

loggerHook :: LogAction -> LogAction
loggerHook log_action dflags severity srcSpan msg | not (isDiagnostic severity) = log_action dflags severity srcSpan msg
loggerHook log_action dflags severity srcSpan msg = do
    let diag = mkDiagOcc dflags severity srcSpan msg
    let encoded = encodeMessage diag
    BS.appendFile diagnosticDataFile $ BC.pack ("\n"++show (countLines encoded) ++ "\n")
    BS.appendFile diagnosticDataFile encoded
    putStrLn $ ['=' | _ <- [1..80]] ++ "error" ++ ['=' | _ <- [1..80]]
    putStrLn $ "severity: " ++ renderWithContext (log_default_dump_context dflags) (renderJSON $ json severity)
    putStrLn $ "msg: " ++ renderWithContext (log_default_dump_context dflags) msg
    putStrLn $ "srcSpan: " ++ show srcSpan
    putStrLn $ ['=' | _ <- [1..80]] ++ "\n"
    log_action dflags severity srcSpan msg

isDiagnostic :: MessageClass -> Bool
isDiagnostic (MCDiagnostic {}) = True
isDiagnostic _ = False

messageClassCode :: LogFlags -> MessageClass -> Maybe Text
messageClassCode dflags (MCDiagnostic _ _ code) = fmap (T.pack . renderWithContext (log_default_dump_context dflags) . ppr) code
messageClassCode _ _ = Nothing

mkDiagOcc :: LogFlags -> MessageClass -> SrcSpan -> SDoc -> Occurrence
mkDiagOcc lf mc srcSpan msg =
    defMessage
    & Proto.Scip_Fields.range .~ srcSpanRange srcSpan
    & Proto.Scip_Fields.diagnostics .~ [mkDiagnostic lf mc srcSpan msg]

srcSpanRange :: Num a => SrcSpan -> [a]
srcSpanRange (RealSrcSpan span _) = spanRange span
srcSpanRange _ = []

mkDiagnostic :: LogFlags -> MessageClass -> SrcSpan -> SDoc -> Diagnostic
mkDiagnostic lf mc srcSpan msg =
    defMessage
    & Proto.Scip_Fields.severity .~ mcSv mc
    & Proto.Scip_Fields.message .~ T.pack (showSDocUnsafe msg)
    & Proto.Scip_Fields.source .~ T.pack (show srcSpan)
    & Proto.Scip_Fields.code .~ fromMaybe "no code" (messageClassCode lf mc)
    where
        mcSv :: MessageClass -> SCIP.Severity
        mcSv MCFatal = SCIP.Error
        mcSv (MCDiagnostic sv _ _) = toSV sv
        mcSv _ = SCIP.UnspecifiedSeverity

        toSV :: GHC.Severity -> SCIP.Severity
        toSV GHC.SevWarning = SCIP.Warning
        toSV GHC.SevError = SCIP.Error
        toSV GHC.SevIgnore = SCIP.UnspecifiedSeverity


-- Define the parsedResultAction function
myParsedResultAction :: [CommandLineOption] -> ModSummary -> ParsedResult -> Hsc ParsedResult
myParsedResultAction _opts _modSummary parsedModule = do
  -- Wrap the parsing action in an exception handler
  let result = parsedResultMessages parsedModule
  -- print result
  liftIO $ putStrLn "Parsed result error:"
  liftIO $ print $ showSDocUnsafe $  ppr $ psWarnings result
  liftIO $ print $ showSDocUnsafe $  ppr $ psErrors result
  liftIO $ putStrLn "Parsed result end"
  return parsedModule


renamedResult :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
renamedResult _ env hsGroup = do
    -- bind the renamed declarations to the environment
    -- so we can access them in the typechecked phase
    return (env{tcg_rn_decls=Just hsGroup}, hsGroup)

countLines :: BS.ByteString -> Int
countLines = length . BC.lines

typechecked :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typechecked _ ms env = do
    hscEnv <- getTopEnv
    let unit_state = ue_units $ hsc_unit_env hscEnv
    let renamed = fromMaybe (error "tcg_rn_decls is nothing in TcGblEnv") (tcg_rn_decls env)
        ex = tcg_rn_imports env
        ix = tcg_rn_exports env
        h = tcg_doc_hdr env
        dy = hsc_dflags hscEnv
    hf <- mkHieFile ms env (renamed, ex, ix, h)
    let kindMap = hie_entity_infos hf
    let docs = hieFileScip dy kindMap unit_state hf
    forM_ docs $ \doc -> do
        let encoded = encodeMessage doc
            path = "./data" </> T.unpack (doc ^. Proto.Scip_Fields.relativePath)
        -- -- create a file
        liftIO $ BS.createDirectoryIfMissing True $ takeDirectory path
        liftIO $ BS.writeFile path encoded
        -- liftIO $ putStrLn $ "wrote: " ++ path
        -- -- lush
        -- fc <- liftIO $ BS.readFile path
        -- let decoded :: Either String Document = decodeMessage fc
        -- case decoded of
        --     Left e -> liftIO $ putStrLn $ "decode error (" ++ e ++ "):" ++ show path
        --     Right d -> liftIO $ putStrLn $ "decoded: " ++ show path
    liftIO $ putStrLn $ unlines $ map show docs
    liftIO $ putStrLn $ unlines $ map (unlines . map show . toSymbols) docs
    return env

fastStringToText :: FastString -> Text
fastStringToText = decodeUtf8 . bytesFS

hieFileScip :: DynFlags -> NameEntityInfo -> UnitState -> HieFile -> [Document]
hieFileScip dy ni uns hf = map (uncurry $ mkScip dy ni uns hf) asts
    where astsMap = getAsts $ hie_asts hf
          asts = Map.toList astsMap

toSymbols :: Document -> [SymbolInformation]
toSymbols doc = doc ^. Proto.Scip_Fields.symbols

mkScip :: DynFlags -> NameEntityInfo -> UnitState -> HieFile -> HiePath -> HieAST TypeIndex -> Document
mkScip dy ni uns hf (HiePath path) hs = defMessage
    & Proto.Scip_Fields.language .~ "Haskell"
    & Proto.Scip_Fields.relativePath .~ fastStringToText path
    & Proto.Scip_Fields.occurrences .~ concatMap (mapOccurrences refs . fst) names
    & Proto.Scip_Fields.symbols .~ symbols
    & Proto.Scip_Fields.positionEncoding .~ UTF32CodeUnitOffsetFromLineStart
    where
        nodes =  getNodeIds hs
        names :: [(Identifier, Maybe Text)]
        names =  map (second (fmap (T.pack . renderHieType dy . (`recoverFullType` hie_types hf)) . identType)) $ Map.toList nodes
        refs = generateReferencesMap (Identity hs)
        symbols = map (nameToSymbolInfo ni uns curPa ) names
        curPa = fromMaybe (defMessage & Proto.Scip_Fields.name .~ fastStringToText path) (packageNameWithVersion uns (hie_module hf))

        mapOccurrences ::  RefMap TypeIndex -> Identifier -> [Proto.Scip.Occurrence]
        mapOccurrences _refs (Left _) = []
        mapOccurrences _refs iden@(Right name) = toOcc <$> spansOcc
            where spansOcc = fst <$> refs Map.! iden
                  toOcc :: Span -> Proto.Scip.Occurrence
                  toOcc spanOcc = defMessage
                    & Proto.Scip_Fields.range .~ spanRange spanOcc
                    & Proto.Scip_Fields.symbol .~ T.pack (show $ nameToSymbol uns curPa name)

spanRange :: Num a => RealSrcSpan -> [a]
spanRange sp = [fromIntegral $ srcSpanStartLine sp, fromIntegral $ srcSpanEndLine sp, fromIntegral $ srcSpanStartCol sp, fromIntegral $ srcSpanEndCol sp]

nameToSymbolInfo :: NameEntityInfo -> UnitState -> Package -> (Identifier, Maybe Text) -> SymbolInformation
nameToSymbolInfo _ni _uns _pa (Left _, _ty) = defMessage
nameToSymbolInfo ni uns pa (Right name, ty) = defMessage
    & Proto.Scip_Fields.symbol .~ T.pack (show (nameToSymbol uns pa name))
    & Proto.Scip_Fields.kind .~ entityInfosKind (ni Map.! name)
    & Proto.Scip_Fields.displayName .~ T.pack (occNameString $ occName name)
    & Proto.Scip_Fields.signatureDocumentation .~ sigDoc ty
    where
        sigDoc :: Maybe Text -> Document
        sigDoc (Just t) = defMessage & Proto.Scip_Fields.text .~ t & Proto.Scip_Fields.language .~ "Haskell"
        sigDoc Nothing = defMessage & Proto.Scip_Fields.language .~ "Haskell"

nameToSymbol :: UnitState -> Package -> Name -> Symbol
nameToSymbol uns pa name = defMessage
    & Proto.Scip_Fields.scheme .~ "ghc"
    & Proto.Scip_Fields.package .~ fromMaybe pa (namePackage uns name)
    & Proto.Scip_Fields.descriptors .~ [desc]
    where
        desc :: Descriptor
        desc = defMessage
                    & Proto.Scip_Fields.name .~ T.pack (occNameString $ occName name)
                    & Proto.Scip_Fields.disambiguator .~ T.pack (nameStableString name)

namePackage :: UnitState -> Name -> Maybe Package
namePackage uns name =
    case nameSrcLoc name of
        UnhelpfulLoc {} | isInternalName name || isSystemName name -> Nothing
        _ -> packageNameWithVersion uns (nameModule name)

packageNameWithVersion :: UnitState -> Module -> Maybe Package
packageNameWithVersion env  m = do
    let pid = moduleUnit m
    conf <- lookupUnit env pid
    let pkgName = T.pack $ unitPackageNameString conf
        version = T.pack $ showVersion (unitPackageVersion conf)
    return $ defMessage
        & Proto.Scip_Fields.manager .~ "cabal"
        & Proto.Scip_Fields.name .~ pkgName
        & Proto.Scip_Fields.version .~ version