module Bli.Error where
import qualified Data.Text as T
import Data.Text (Text)
import Text.Megaparsec (ShowErrorComponent (..))

data BliException 
  = ErrorMsg Text
  | Goto
  deriving (Eq, Ord, Show)

instance ShowErrorComponent BliException where
    showErrorComponent = show
    errorComponentLen (ErrorMsg msg) = T.length msg
    errorComponentLen Goto = 0
    

mkErrorMsg :: Show a => a -> a -> BliException
mkErrorMsg found expected =
  ErrorMsg . T.pack $ "Found " <> show found <> ", but expected " <> show expected <> "."