{-# LANGUAGE TemplateHaskell #-}
module LookupBuildEnv
    (
    -- * Compile-time configuration
      lookupCompileEnv
    , lookupCompileEnvExp
    , getCompileEnv
    , getCompileEnvExp
    , fileAsString
    ) where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import System.Environment (getEnvironment)

lookupCompileEnv :: String -> Q (Maybe String)
lookupCompileEnv key = lookup key `liftM` runIO getEnvironment

lookupCompileEnvExp :: String -> Q Exp
lookupCompileEnvExp = (`sigE` [t| Maybe String |]) . lift <=< lookupCompileEnv

getCompileEnv :: String -> Q String
getCompileEnv key =
  lookupCompileEnv key >>=
  maybe (fail $ "Environment variable " ++ key ++ " not defined") return

getCompileEnvExp :: String -> Q Exp
getCompileEnvExp = lift <=< getCompileEnv

fileAsString :: FilePath -> Q Exp
fileAsString = do
  stringE . T.unpack . T.strip <=< runIO . T.readFile
