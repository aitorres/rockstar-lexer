import System.Environment
import System.Exit
import qualified Lexer as L

-- | The 'showArgumentError' function handles a missing argument error and quits.
showArgumentError :: IO ()
showArgumentError = do
    name <- getProgName
    putStrLn ("Fatal error: missing file argument.")
    putStrLn("\tUsage: " ++ name ++ "<filename>")
    exitWith (ExitFailure 1)

-- | The 'showHelpText' function shows a help text and quits.
showHelpText :: IO ()
showHelpText = do
    name <- getProgName
    putStrLn (name ++ ": missing file argument.\n\tUsage: " ++ name ++ "<filename>")
    exitWith ExitSuccess

-- | The 'scanFile' function scans a file for tokens and prints them on screen, 
--   then quits.
scanFile :: FilePath -> IO ()
scanFile file = do
    content <- readFile file
    print (L.alexScanTokens content)
    exitWith ExitSuccess

-- | The 'main' procedure tries to read a file passed as a first argument, 
--   show the tokens in the file, and quits.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> showArgumentError
        firstArg:_ -> do
            if firstArg == "--help" 
                then showHelpText
            else scanFile firstArg