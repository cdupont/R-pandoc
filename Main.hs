import           Control.Applicative
import           Control.Monad       (when)
import           Control.Monad.Loops
import           Data.List           (delete)
import           Options.Applicative
import           System.Directory    (createDirectoryIfMissing, doesFileExist)
import           System.Exit         (ExitCode (..))
import           System.FilePath     ((<.>), (</>))
import           System.IO           (hPutStrLn, stderr)
import           System.Process
import           Text.Pandoc.Generic
import           Text.Pandoc.JSON

-- TODO choose output format based on pandoc target
backendExt :: String
backendExt = "png"

main :: IO ()
main = toJSONFilter insertRplots

renderRPandoc :: Pandoc -> IO Pandoc
renderRPandoc = bottomUpM insertRplots

insertRplots :: Block -> IO Block
insertRplots block@(CodeBlock (ident, classes, attrs) code) = do
   hPutStrLn stderr $ "insertRplots: " ++ (show classes)
   if "R" `elem` classes then do
      d <- renderRPlot code
      return $ case d of
         Nothing     -> block
         Just imgName -> Plain [Image [] (imgName,"")] -- no alt text, no title
   else return block
insertRplots block = return block

--plot the R graph
renderRPlot :: String -> IO (Maybe FilePath)
renderRPlot rcode = do
   createDirectoryIfMissing True "Rtmp"
   writeFile "Rtmp/plot.R" rcode
   (e1,_,_) <- readProcessWithExitCode "R" ["CMD", "BATCH", "--no-save", "--quiet", "Rtmp/plot.R"] ""
   if (e1==ExitSuccess) then do
      untilM_ (return ()) (doesFileExist "Rplots.pdf")
      hPutStrLn stderr $  "Rplots.pdf created"
      (e2,a,b) <- readProcessWithExitCode "convert" ["Rplots.pdf", "Rplot.png"] "" --"-density 150", "-quality 90",
      hPutStrLn stderr $ "e2: " ++ (show e2)  ++ "a: " ++ (show a)  ++ "b: " ++ (show b)
      return $ if (e2==ExitSuccess) then (Just "Rplot.png") else Nothing
   else return Nothing
   --hPutStrLn stderr $ "e1:" ++ (show e1)
   --hPutStrLn stderr $ "e2:" ++ (show e2)
   --return $ if (e1==ExitSuccess && e2==ExitSuccess) then (Just "Rtmp/Rplots.png") else Nothing
