import Text.Liquoh

import qualified Data.Aeson as Aeson
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  source <- Text.getContents
  case parseAndInterpret (Aeson.object []) source of
    Right t -> Text.putStr t
    Left err -> Text.putStrLn err
