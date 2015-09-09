import           Control.Applicative
import           Control.Monad
import           Data.List           (delete)
import           Data.List.Split
import           Data.Maybe
import           Options.Applicative
import           System.Directory    (createDirectoryIfMissing, doesFileExist)
import           System.Exit         (ExitCode (..))
import           System.FilePath     ((<.>), (</>))
import           System.IO           (hPutStrLn, stderr)
import           System.Process
import           Text.Pandoc.Generic
import           Text.Pandoc.JSON
import           Text.Pandoc.R


main :: IO ()
main = toJSONFilter (insertRplots "plots")
