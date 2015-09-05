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
   hPutStrLn stderr $ "insertRplots classes: " ++ (show classes) ++ " attrs: " ++ (show attrs) ++ " ident: " ++ (show ident)
   if "Rplot" `elem` classes then do
      let imgFiles = case (lookup "imgs" attrs) of
           Just imgs -> [imgs]
           Nothing   -> ["Rplots.pdf"]
      d <- renderRPlot code imgFiles
      return $ if d then Plain (map image imgFiles) else block
   else return block
insertRplots block = return block

image file = Image [] (file,"")

--plot the R graph
renderRPlot :: String -> [FilePath] -> IO Bool
renderRPlot rcode imgs = do
   createDirectoryIfMissing True "Rtmp"
   writeFile "Rtmp/plot.R" rcode
   (code,stdout,stderr) <- readProcessWithExitCode "R" ["CMD", "BATCH", "--no-save", "--quiet", "Rtmp/plot.R"] ""
   putStrLnErr $ "R exited with: " ++ (show code)
   putStrLnErr stdout
   putStrLnErr stderr
   return $ (code==ExitSuccess)

putStrLnErr = hPutStrLn stderr
