import           Tokenization (testTokenize)
import           PreProcessing (testPreProcessing)
import           TokenParsing (testParsing)
import Encoding (testEncoding)

main :: IO ()
main = do
  testTokenize
  testPreProcessing
  testParsing
  testEncoding
