module Main (main) where

import Lily.Prelude

import Lily.Effect.Fresh
import Lily.Config qualified as Config

import Data.Text qualified as Text

import Lily.Lexer qualified as Lexer
import Lily.Parser qualified as Parser
import Lily.Rename qualified as Rename
import Lily.Types qualified as Types

data Options = Options {
    verboseNames :: Bool
}

defaultOptions :: Options
defaultOptions = Options {
    verboseNames = False
}

failUsage :: Text -> IO a
failUsage message = do
    putTextLn $ "ERROR: " <> message <> "\n\n" <> unlines [
            "usage: lily [OPTIONS] <FILE>"
        ,   ""
        ,   "Options:"
        ,   "    --verbose-names        Disambiguate names in debug output by appending a unique number"
        ]
    exitFailure

main :: IO ()
main = do
    (Options { verboseNames }, args) <- parseArgs =<< getArgs
    Config.updateConfig (\cfg -> cfg { Config.verboseNames = verboseNames })
    case args of
        [file] -> do
            content <- readFileText file
            tokens <- case runPureEff (runError @Lexer.LexError (Lexer.lex content)) of
                Left err -> putStrLn ("Lexical error: " <> show err) >> exitFailure
                Right tokens -> pure tokens
            putStrLn ("TOKENS: " <> show tokens)

            let parsed = Parser.parse tokens
            putStrLn ("\nPARSED: " <> show parsed)
            
            renamed <- runEff (runError @Rename.RenameError (runFreshUnique (Rename.rename parsed))) >>= \case
                Left err -> putStrLn ("Lexical error: " <> show err) >> exitFailure
                Right renamed -> pure renamed
            
            putStrLn ("\nRENAMED: " <> show renamed)

            (coreExpr, ty) <- runEff (runError @Types.TypeError (Types.infer Types.emptyTCEnv renamed)) >>= \case
                Left err -> putStrLn ("Type error: " <> show err) >> exitFailure
                Right res -> pure res

            putStrLn ("\nCORE: " <> show coreExpr)
            putStrLn ("TYPE: " <> show ty)

            let resultValue = Types.eval Types.emptyEvalEnv coreExpr

            putStrLn ("RESULT VALUE: " <> show resultValue)
        [] -> failUsage "Missing required FILE argument"
        _ -> failUsage "Too many arguments"
    where
        parseArgs :: [String] -> IO (Options, [String])
        parseArgs [] = pure (defaultOptions, [])
        parseArgs ("--verbose-names" : args) = do
            (opts, args) <- parseArgs args
            pure (opts { verboseNames = True }, args)
        parseArgs (arg : args)
            | "-" `Text.isPrefixOf` toText arg = failUsage ("Invalid flag '" <> toText arg <> "'")
            | otherwise = do
                (opts, args) <- parseArgs args
                pure (opts, arg : args)
