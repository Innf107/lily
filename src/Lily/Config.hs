module Lily.Config (
    Config (..),
    getConfig,
    updateConfig,
    trace,
    traceM,
    TraceCategory (..),
) where

import Lily.Prelude

import System.IO.Unsafe

import Debug.Trace qualified

data Config = Config
    { verboseNames :: Bool
    , enabledTraces :: Set TraceCategory
    , printClosures :: Bool
    , printDebruijn :: Bool
    }

defaultConfig :: Config
defaultConfig =
    Config
        { verboseNames = False
        , enabledTraces = mempty
        , printClosures = False
        , printDebruijn = False
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

trace :: TraceCategory -> Text -> a -> a
trace cat message x =
    let Config{enabledTraces} = getConfig ()
     in if cat `member` enabledTraces
            then Debug.Trace.trace (toString message) x
            else x

traceM :: Applicative f => TraceCategory -> Text -> f ()
traceM cat message = do
    let Config{enabledTraces} = getConfig ()
    if cat `member` enabledTraces
        then Debug.Trace.traceM (toString message)
        else pure ()

data TraceCategory = TC | Eval deriving (Show, Eq, Ord, Read, Enum, Bounded)