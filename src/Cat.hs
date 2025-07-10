module Cat where

import Options.Applicative
import System.IO.Error (tryIOError)

-- | Options for the cat command
data Options = Options
  { files :: [FilePath]
  }

-- | Show the contents of a single file
showFile :: FilePath -> IO ()
showFile path = do
  result <- tryIOError (readFile path)
  case result of
    Right content -> putStr content
    Left err -> putStrLn $ "Could not open file: " ++ path ++ " - Error: " ++ show err

-- | Process multiple files and return their concatenated contents
catFiles :: [FilePath] -> IO String
catFiles [] = return "Give me some files!"
catFiles fs = do
  contents <- mapM readFileSafe fs
  return $ concat contents

-- | Read a file safely, returning empty string if file doesn't exist
readFileSafe :: FilePath -> IO String
readFileSafe path = do
  result <- tryIOError (readFile path)
  case result of
    Right content -> return content
    Left _        -> return $ "Could not open file: " ++ path

-- | Process multiple files and show their contents
processFiles :: [FilePath] -> IO ()
processFiles [] = putStrLn "Give me some files!"
processFiles fs = do
  mapM_ showFile fs

runCat :: Options -> IO ()
runCat options = processFiles (files options)

optionsParser :: Parser Options
optionsParser = Options
  <$> many (argument str (metavar "FilePath..."))

optionsInfo :: ParserInfo Options
optionsInfo = info (optionsParser <**> helper) fullDesc

-- | Main application entry point for Main.hs to call
runCatMain :: IO ()
runCatMain = do
  options <- execParser optionsInfo
  runCat options 