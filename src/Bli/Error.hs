module Bli.Error where
import qualified Data.Text as T
import Data.Text (Text)
import Text.Megaparsec (ShowErrorComponent (..))

newtype ErrorMsg = ErrorMsg Text
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ErrorMsg where
    showErrorComponent = show
    errorComponentLen (ErrorMsg msg) = T.length msg
    

mkErrorMsg :: Show a => a -> a -> ErrorMsg
mkErrorMsg found expected =
  ErrorMsg . T.pack $ "Found " <> show found <> ", but expected " <> show expected <> "."