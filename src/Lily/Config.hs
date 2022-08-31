module Lily.Config (Config(..), getConfig, updateConfig) where

import Lily.Prelude

import System.IO.Unsafe

data Config = Config {
    verboseNames :: Bool
}

defaultConfig :: Config
defaultConfig = Config {
    verboseNames = False
}

configRef :: IORef Config
configRef = unsafePerformIO $ newIORef defaultConfig
{-# NOINLINE configRef #-}

{- | WARNING: This is technically an unsafe operation and not referentially transparent!
     The value returned by @getConfig@ might depend on modifications performed by
     @updateConfig@ and will therefore not necessarily be the same every time.
     
     The intended way to use this is to update the configuration at the beginning of the program
     and treat @getConfig@ as effectively pure after that.
-}
getConfig :: () -> Config
getConfig () = unsafePerformIO $ readIORef configRef
{-# NOINLINE getConfig #-}

{- | Set the configuration value returned by @getConfig@. 

     Doing so after @getConfig@ has been called is unsafe,
     but will probably be fine in practice. See @getConfig@ for more info. 
-}
updateConfig :: (Config -> Config) -> IO ()
updateConfig = modifyIORef' configRef
