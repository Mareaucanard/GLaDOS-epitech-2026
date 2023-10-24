module ParseArguments (Args(..), Mode(..), parseArguments) where

import           System.FilePath (splitExtension)
import           Data.Maybe (fromJust)

data Mode = Comp
          | Human
            -- | Run
          | Exec
          | Lisp
          | Decode

data Args = Args { inputFile :: Maybe String
                 , outputFile :: Maybe String
                 , help :: Bool
                 , mode :: Maybe Mode
                 , version :: Bool
                 }

defaultArgs :: Args
defaultArgs = Args { outputFile = Nothing
                   , help = False
                   , mode = Nothing
                   , version = False
                   , inputFile = Nothing
                   }

argsLogic :: [String] -> Args -> Either Args String
argsLogic [] args = Left args
argsLogic ("--output":x:xs) args = argsLogic xs (args { outputFile = Just x })
argsLogic ("-o":x:xs) args = argsLogic xs (args { outputFile = Just x })
argsLogic ("--help":_) args = Left $ args { help = True }
argsLogic ("-h":_) args = Left $ args { help = True }
argsLogic ("--version":_) args = Left $ args { version = True }
argsLogic ("-v":_) args = Left $ args { version = True }
argsLogic ("--compile":xs) args = argsLogic xs $ args { mode = Just Comp }
argsLogic ("--decode":xs) args = argsLogic xs $ args { mode = Just Decode }
-- argsLogic ("--run":xs) args = argsLogic xs $ args { mode = Just Run }
argsLogic ("--human":xs) args = argsLogic xs $ args { mode = Just Human }
argsLogic ("--exec":xs) args = argsLogic xs $ args { mode = Just Exec }
argsLogic ("--lisp":xs) args = argsLogic xs $ args { mode = Just Lisp }
argsLogic (('-':x):_) _ = Right $ "unknown option: -" ++ show x
argsLogic (x:xs) args = case args of
  Args { inputFile = Nothing } -> argsLogic xs (args { inputFile = Just x })
  Args { inputFile = Just _ }  -> Right "Multiple files specified"

verifyArgs :: Args -> Either Args String
verifyArgs Args { inputFile = Nothing } = Right "No input file specified"
verifyArgs Args { mode = Nothing } = Right "No mode specified"
verifyArgs l = case outputFile l of
  Nothing -> Left
    $ handleDefaultOutput l (fromJust $ mode l) (fromJust $ inputFile l)
  Just _  -> Left l

handleDefaultOutput :: Args -> Mode -> String -> Args
handleDefaultOutput args Comp file =
  args { outputFile = Just (fst (splitExtension file) ++ ".gvm") }
handleDefaultOutput args _ _ = args { outputFile = Just "stdout" }

parseArguments :: [String] -> Either Args String
parseArguments str = case argsLogic str defaultArgs of
  Left (Args { help = True }) -> Left (defaultArgs { help = True })
  Left (Args { version = True }) -> Left (defaultArgs { version = True })
  Left args -> verifyArgs args
  Right err -> Right err
