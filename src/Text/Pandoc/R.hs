

module Text.Pandoc.R where

import           Control.Applicative
import           Control.Monad
import           Data.List           (delete)
import           Data.List.Split
import           Data.Maybe
import           Options.Applicative
import           System.Directory    (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import           System.Exit         (ExitCode (..))
import           System.FilePath     ((<.>), (</>), pathSeparator)
import           System.IO           (hPutStrLn, stderr)
import           System.Process
import           Text.Pandoc.Generic
import           Text.Pandoc.JSON

rClass, fileAttr, defFile, defRplot, tmpRFile :: String
rClass = "Rplot"
fileAttr = "files"
defFile = "Rplot.png"
defRplot = "Rplots.pdf"
tmpRFile = "plot.R"

renderRPandoc :: FilePath -> Pandoc -> IO Pandoc
renderRPandoc f p = bottomUpM (insertRplots f) p

insertRplots :: FilePath -> Block -> IO Block
insertRplots outDir block@(CodeBlock (ident, classes, attrs) code) = do
   hPutStrLn stderr $ "insertRplots classes: " ++ (show classes) ++ " attrs: " ++ (show attrs) ++ " ident: " ++ (show ident)
   if rClass `elem` classes then do
      let imgFiles = case lookup fileAttr attrs of
           Just is -> splitOn "," is
           Nothing   -> [defFile]
      d <- renderRPlot code
      when (imgFiles == [defFile]) $ void $ convertDefault
      imgFiles' <- moveFiles imgFiles outDir
      return $ if d then Plain (map insertImage imgFiles') else block
   else return block
insertRplots _ block = return block

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
   putStrLnErr $ "R exited with: " ++ (show code)
   putStrLnErr stdout
   putStrLnErr stderr
   whenM (doesFileExist tmpRFile) $ removeFile tmpRFile
   whenM (doesFileExist "plot.Rout") $ removeFile "plot.Rout"
   return $ (code==ExitSuccess)

moveFiles :: [FilePath] -> FilePath -> IO [FilePath]
moveFiles files outDir = do
   createDirectoryIfMissing False outDir
   mapM_ (\a -> whenM (doesFileExist a) $ renameFile a (outDir </> a)) files
   return $ map ((pathSeparator : outDir) </>) files

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
