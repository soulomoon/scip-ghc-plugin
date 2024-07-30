{-# LANGUAGE OverloadedStrings #-}

module MyPlugin (plugin) where

import GHC.Plugins hiding ((<>))
import GHC.Tc.Types
import GHC.Hs
import Ext.Ast
import Ext.Types
import Proto.Scip (Document, PositionEncoding (UTF32CodeUnitOffsetFromLineStart), Occurrence, Symbol, Package, SymbolInformation, SyntaxKind (Identifier), Descriptor'Suffix (Descriptor'Suffix'Unrecognized), Descriptor)
import qualified Proto.Scip_Fields
import Data.ProtoLens (Message(..))
import Lens.Family2 ((&), (.~), (^.))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Text as T
import Data.Version (showVersion)
import GHC.Tc.Utils.Monad (getTopEnv)
import GHC.Unit.Env (ue_units)
import qualified Data.Map.Strict as Map
import Ext.Utils (emptyNodeInfo, flattenAst, getNodeIds, recoverFullType, renderHieType, generateReferencesMap, RefMap)
import qualified Data.Set as S
import Data.Either (lefts, rights, fromRight, fromLeft)
import qualified Data.Vector as V
import Debug.Trace (traceShow)
import EntityInfoKind (entityInfoKind, entityInfosKind)
import Control.Arrow ((***), Arrow (..))
import Data.Functor.Identity (Identity(..))
import qualified Data.Vector.Unboxed
import qualified Data.Int

-- This is the entry point for the plugin
plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install,
    typeCheckResultAction = typechecked,
    renamedResultAction = renamedResult
}

-- This function modifies the compilation pipeline
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
    putMsgS "Hello from the plugin!"
    return (CoreDoPluginPass "MyPluginPass" pass : todos)

-- This is the core-to-core pass
pass :: ModGuts -> CoreM ModGuts
pass guts = do
    -- Do something with the Core here
    putMsgS "Running my pass"
    return guts

renamedResult :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
renamedResult _ env hsGroup = do
    return (env{tcg_rn_decls=Just hsGroup}, hsGroup)

-- Document

typechecked :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typechecked _ ms env = do
    hscEnv <- getTopEnv
    let unit_state = ue_units $ hsc_unit_env hscEnv
    let (Just renamed) = tcg_rn_decls env
        ex = tcg_rn_imports env
        ix = tcg_rn_exports env
        h = tcg_doc_hdr env
        dy = hsc_dflags hscEnv
    hf <- mkHieFile ms env (renamed, ex, ix, h)
    let kindMap = hie_entity_infos hf
    let docs = hieFileScip dy kindMap unit_state hf
    liftIO $ putStrLn $ unlines $ map show docs
    liftIO $ putStrLn $ unlines $ map (unlines . map show . toSymbols) docs
    return env

fastStringToText :: FastString -> Text
fastStringToText = decodeUtf8 . bytesFS

hieFileScip :: DynFlags -> NameEntityInfo -> UnitState -> HieFile -> [Document]
hieFileScip dy ni uns hf = traceShow ("asts:", length asts) $ map (uncurry $ mkScip dy ni uns hf) asts
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
        mapOccurrences refs (Left _) = []
        mapOccurrences refs iden@(Right name) = toOcc <$> spans
            where spans = fst <$> refs Map.! iden
                  spanRange sp = [fromIntegral $ srcSpanStartLine sp, fromIntegral $ srcSpanEndLine sp, fromIntegral $ srcSpanStartCol sp, fromIntegral $ srcSpanEndCol sp]
                  toOcc :: Span -> Proto.Scip.Occurrence
                  toOcc span = defMessage
                    & Proto.Scip_Fields.range .~ spanRange span
                    & Proto.Scip_Fields.symbol .~ T.pack (show $ nameToSymbol uns curPa name)
                    -- todo add Relationship



-- srcSpanStartLine :: RealSrcSpan -> Int
-- srcSpanEndLine :: RealSrcSpan -> Int
-- srcSpanStartCol :: RealSrcSpan -> Int
-- srcSpanEndCol :: RealSrcSpan -> Int



nameToSymbolInfo :: NameEntityInfo -> UnitState -> Package -> (Identifier, Maybe Text) -> SymbolInformation
nameToSymbolInfo ni uns pa (Left _, ty) = defMessage
nameToSymbolInfo ni uns pa (Right name, ty) = defMessage
    & Proto.Scip_Fields.symbol .~ T.pack (show (nameToSymbol uns pa name))
    & Proto.Scip_Fields.kind .~ entityInfosKind (ni Map.! name)
    & Proto.Scip_Fields.displayName .~ T.pack (occNameString $ occName name)
    & Proto.Scip_Fields.signatureDocumentation .~ sigDoc ty
        --  * 'Proto.Scip_Fields.relationships' @:: Lens' SymbolInformation [Relationship]@
        --  * 'Proto.Scip_Fields.vec'relationships' @:: Lens' SymbolInformation (Data.Vector.Vector Relationship)@
        --  * 'Proto.Scip_Fields.signatureDocumentation' @:: Lens' SymbolInformation Document@
        --  * 'Proto.Scip_Fields.maybe'signatureDocumentation' @:: Lens' SymbolInformation (Prelude.Maybe Document)@
        --  * 'Proto.Scip_Fields.enclosingSymbol' @:: Lens' SymbolInformation Data.Text.Text@ -}
    where
        sigDoc :: Maybe Text -> Document
        sigDoc (Just t) = defMessage & Proto.Scip_Fields.text .~ t & Proto.Scip_Fields.language .~ "Haskell"
        sigDoc Nothing = defMessage & Proto.Scip_Fields.language .~ "Haskell"

nameToSymbol :: UnitState -> Package -> Name -> Symbol
nameToSymbol uns pa name = defMessage
    & Proto.Scip_Fields.scheme .~ "ghc"
    -- _Symbol'package :: !(Prelude.Maybe Package),
    & Proto.Scip_Fields.package .~ fromMaybe pa (namePackage uns name)
    --  _Symbol'descriptors :: !(Data.Vector.Vector Descriptor),
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

            -- & Proto.Scip_Fields.version .~ "
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

definedAt :: Name -> Maybe T.Text
definedAt name =
    -- do not show "at <no location info>" and similar messages
    -- see the code of 'pprNameDefnLoc' for more information
    case nameSrcLoc name of
    UnhelpfulLoc {} | isInternalName name || isSystemName name -> Nothing
    _ -> Just $ "*Defined " <> fromString (renderWithContext defaultSDocContext (pprNameDefnLoc name)) <> "*"