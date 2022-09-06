module Main (main) where

import Lily.Prelude

import Lily.Config qualified as Config
import Lily.Effect.Fresh

import Data.Text qualified as Text

import Lily.Lexer qualified as Lexer
import Lily.Parser qualified as Parser
import Lily.Rename qualified as Rename
import Lily.Types qualified as Types

import Data.Set qualified as Set

data Options = Options
    {
    }

defaultOptions :: Options
defaultOptions =
    Options
        {
        }

failUsage :: Text -> IO a
failUsage message = do
    putTextLn $
        "ERROR: " <> message <> "\n\n"
            <> unlines
                [ "usage: lily [OPTIONS] <FILE>"
                , ""
                , "Options:"
                , "    --verbose-names        Disambiguate names in debug output by appending a unique number"
                , "    --print-closures       Explicitly print closures in partially evaluated λ and Π expressions"
                , "    --trace <CATEGORY>     Enable traces for CATEGORY. Possible values: " <> intercalate ", " (map show (universe @Config.TraceCategory))
                ]
    exitFailure

prettyTypeError :: Types.TypeError -> Text
prettyTypeError (Types.ConversionError expected actual fullExpected fullActual) =
    unlines
        ([ "\ESC[1mConversion Error:"
        , "Unable to match expected type\ESC[0m"
        , "    \ESC[1m\ESC[32m" <> show expected <> "\ESC[0m"
        , "\ESC[1mwith actual type\ESC[0m"
        , "    \ESC[1m\ESC[31m" <> show actual <> "\ESC[0m"
        ] <> if expected /= fullExpected || actual /= fullActual then
        [ "while comparing types"
        , "    \ESC[32m" <> show expected <> "\ESC[0m"
        , "and"
        , "    \ESC[31m" <> show actual <> "\ESC[0m"
        ] else [])
prettyTypeError err = show err -- TODO

main :: IO ()
main = do
    (Options{}, args) <- parseArgs =<< getArgs
    case args of
        [file] -> do
            content <- readFileText file
            tokens <- case runPureEff (runErrorNoCallStack @Lexer.LexError (Lexer.lex content)) of
                Left err -> putStrLn ("Lexical error: " <> show err) >> exitFailure
                Right tokens -> pure tokens
            putStrLn ("TOKENS: " <> show tokens)

            let parsed = Parser.parse tokens
            putStrLn ("\nPARSED: " <> show parsed)

            renamed <-
                runEff (runErrorNoCallStack @Rename.RenameError (runFreshUnique (Rename.rename parsed))) >>= \case
                    Left err -> putStrLn ("Name resolution error: " <> show err) >> exitFailure
                    Right renamed -> pure renamed

            putStrLn ("\nRENAMED: " <> show renamed)

            (coreExpr, ty) <-
                runEff (runErrorNoCallStack @Types.TypeError (Types.infer Types.emptyTCEnv renamed)) >>= \case
                    Left err -> putTextLn (prettyTypeError err) >> exitFailure
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
        Config.updateConfig (\cfg -> cfg{Config.verboseNames = True})
        parseArgs args
    parseArgs ("--print-closures" : args) = do
        Config.updateConfig (\cfg -> cfg{Config.printClosures = True})
        parseArgs args
    parseArgs ["--trace"] = failUsage ("'--trace' expects an argument")
    parseArgs ("--trace" : category : args) =
        case readMaybe category of
            Nothing ->
                failUsage $
                    "Invalid trace category '" <> show category <> "'. Possible values: "
                        <> intercalate ", " (map show (universe @Config.TraceCategory))
            Just cat -> do
                Config.updateConfig
                    (\cfg@Config.Config{enabledTraces} -> cfg{Config.enabledTraces = Set.insert cat enabledTraces})
                parseArgs args
    parseArgs (arg : args)
        | "-" `Text.isPrefixOf` toText arg = failUsage ("Invalid flag '" <> toText arg <> "'")
        | otherwise = do
            (opts, args) <- parseArgs args
            pure (opts, arg : args)
