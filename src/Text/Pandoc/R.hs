

module Text.Pandoc.R where

import           Control.Applicative
import           Control.Monad
import           Data.List           (delete)
import           Data.List.Split
import           Data.Maybe
import           System.Directory    (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import           System.Exit         (ExitCode (..))
import           System.FilePath     ((<.>), (</>), pathSeparator)
import           System.IO           (hPutStrLn, stderr)
import           System.Process
import           Text.Pandoc.Generic
import           Text.Pandoc.JSON

rClass, defFile, defRplot, tmpRFile :: String
rClass = "Rplot"
defFile = "Rplot.png"
defRplot = "Rplots.pdf"
tmpRFile = "plot.R"

data Echo = Above | Below
   deriving (Read, Show)

renderRPandoc :: FilePath -> Bool -> Pandoc -> IO Pandoc
renderRPandoc f absolutePath p = bottomUpM (insertRplots f absolutePath) p

insertRplots :: FilePath -> Bool -> Block -> IO Block
insertRplots outDir absolutePath block@(CodeBlock (ident, classes, attrs) code) | rClass `elem` classes = do
   let imgFiles = readImgFiles attrs
   d <- renderRPlot code
   when (imgFiles == [defFile]) $ void $ convertDefault
   moveFiles imgFiles outDir
   let imgFiles' = map ((if absolutePath then pathSeparator : outDir else outDir) </>) imgFiles
   let imgBlock = Plain (map insertImage imgFiles')
   let codeBlock = CodeBlock (ident, "r":delete "Rplot" classes, attrs) code
   let block' = case readEcho attrs of
        (Just Above) -> Table [] [AlignLeft] [] [] [[[codeBlock]], [[imgBlock]]]
        (Just Below) -> Table [] [AlignLeft] [] [] [[[imgBlock]], [[codeBlock]]]
        Nothing -> imgBlock
   return $ if d then block' else block
insertRplots _ _ block = return block

readEcho :: [(String, String)] -> Maybe Echo
readEcho attrs = case lookup "echo" attrs of
   Just e  -> Just (read e)
   Nothing -> Nothing

readImgFiles :: [(String, String)] -> [FilePath]
readImgFiles attrs = case lookup "files" attrs of
   Just is -> splitOn "," is
   Nothing -> [defFile]

insertImage :: FilePath -> Inline
insertImage file = Image [] (file,"")

--plot the R graph
--the files created will be the one specified in the R code with commands such as:
--png(filename="fig1.png")
--if no filename is specified, a file "Rplots.pdf" is generated.
renderRPlot :: String -> IO Bool
renderRPlot rcode = do
   writeFile tmpRFile rcode
   (code,stdout,stderr) <- readProcessWithExitCode "R" ["CMD", "BATCH", "--no-save", "--quiet", tmpRFile] ""
   when (code /= ExitSuccess) $ do
      putStrLnErr $ "R exited with: " ++ (show code)
      putStrLnErr stdout
      putStrLnErr stderr
   whenM (doesFileExist tmpRFile) $ removeFile tmpRFile
   whenM (doesFileExist "plot.Rout") $ removeFile "plot.Rout"
   return $ (code==ExitSuccess)

moveFiles :: [FilePath] -> FilePath -> IO ()
moveFiles files outDir = do
   createDirectoryIfMissing False outDir
   mapM_ (\a -> whenM (doesFileExist a) $ renameFile a (outDir </> a)) files


convertDefault :: IO Bool
convertDefault = do
   (code,_,_) <- readProcessWithExitCode "convert" [defRplot, defFile] ""
   whenM (doesFileExist defRplot) $ void $ removeFile defRplot
   return $ (code==ExitSuccess)

putStrLnErr = hPutStrLn stderr

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond a = do
  c <- cond
  if c then a else (return ())
