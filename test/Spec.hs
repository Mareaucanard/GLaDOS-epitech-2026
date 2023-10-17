import           Tokenization (testTokenize)
import           PreProcessing (testPreProcessing)
import           TokenParsing (testParsing)
import Encoding (testEncoding)
import LISP.Spec (lispSpec)

main :: IO ()
main = do
  testTokenize
  testPreProcessing
  testParsing
  testEncoding
  lispSpec
