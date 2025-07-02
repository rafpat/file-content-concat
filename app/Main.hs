import Options.Applicative
import System.IO.Error (tryIOError)

data Options = Options
  { files :: [FilePath]
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> many (argument str (metavar "FilePath..."))

main :: IO ()
main = do
  options <- execParser optionsInfo
  case files options of
    [] -> putStrLn "Give me some files!"
    fs -> mapM_ showFile fs

showFile :: FilePath -> IO ()
showFile path = do
  result <- tryIOError (readFile path)
  case result of
    Right content -> putStr content
    Left _        -> putStrLn $ "Could not open file: " ++ path

optionsInfo :: ParserInfo Options
optionsInfo = info (optionsParser <**> helper) fullDesc
