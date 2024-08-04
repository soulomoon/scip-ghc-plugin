module EntityInfoKind where
import Ext.Types (EntityInfo (..))
import Proto.Scip (SymbolInformation'Kind(..))
import qualified Data.Set as S


entityInfosKind :: S.Set EntityInfo -> SymbolInformation'Kind
entityInfosKind = entityInfoKind . maximum

entityInfoKind :: EntityInfo -> SymbolInformation'Kind
entityInfoKind e = case e of
   EntityVariable -> SymbolInformation'Variable
   EntityFunction -> SymbolInformation'Function
   EntityDataConstructor -> SymbolInformation'Constructor
   EntityTypeVariable -> SymbolInformation'TypeParameter
   EntityClassMethod -> SymbolInformation'Method
   EntityPatternSynonym -> SymbolInformation'Pattern
   EntityTypeConstructor -> SymbolInformation'Type
   EntityTypeClass -> SymbolInformation'Class
   EntityTypeSynonym -> SymbolInformation'TypeAlias
   EntityTypeFamily -> SymbolInformation'TypeFamily
   EntityRecordField -> SymbolInformation'Field



