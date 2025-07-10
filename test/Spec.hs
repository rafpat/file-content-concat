{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck (elements, listOf1, suchThat, forAll, property, arbitrary, ioProperty)
import System.IO.Temp
import System.FilePath
import Control.Monad (forM)
import Cat (catFiles)

main :: IO ()
main = hspec $ do
  describe "Cat program tests" $ do
    
    it "concatenates A.txt and B.txt to produce HelloWorld" $ do
      result <- catFiles ["test-files/A.txt", "test-files/B.txt"]
      result `shouldBe` "HelloWorld"
    
    it "concatenates arbitrary file contents correctly" $ 
      property $ \content1 content2 -> do
        withSystemTempDirectory "cat-test" $ \tempDir -> do
          let file1 = tempDir </> "file1.txt"
              file2 = tempDir </> "file2.txt"
          
          writeFile file1 content1
          writeFile file2 content2
          
          result <- catFiles [file1, file2]
          result `shouldBe` (content1 ++ content2)
    
    it "works with arbitrary filenames" $ 
      property $ forAll safeFileName $ \filename1 ->
        forAll (safeFileName `suchThat` (/= filename1)) $ \filename2 ->
          forAll arbitrary $ \content1 ->
            forAll arbitrary $ \content2 ->
              ioProperty $ withSystemTempDirectory "cat-test" $ \tempDir -> do
                let file1 = tempDir </> filename1
                    file2 = tempDir </> filename2
                writeFile file1 content1
                writeFile file2 content2
                result <- catFiles [file1, file2]
                return (result == content1 ++ content2)
    
    it "works with arbitrary number of files" $ 
      property $ forAll (listOf1 arbitrary) $ \fileContents ->
        ioProperty $ withSystemTempDirectory "cat-test" $ \tempDir -> do
          filePaths <- forM (zip [0..] fileContents) $ \(i, content) -> do
            let filePath = tempDir </> ("file" ++ show i ++ ".txt")
            writeFile filePath content
            return filePath
          result <- catFiles filePaths
          let expected = concat fileContents
          return (result == expected)
    
    it "returns 'Give me some files!' when no files are provided" $ do
      result <- catFiles []
      result `shouldBe` "Give me some files!"
    
    it "handles non-existent files correctly" $ do
      result <- catFiles ["nonexistent.txt"]
      result `shouldContain` "Could not open file:"
      result `shouldContain` "nonexistent.txt"

  where
    safeFileName = do
      name <- listOf1 (elements ['a'..'z'])
      ext <- elements [".txt", ".dat", ".log"]
      return (name ++ ext) 
      