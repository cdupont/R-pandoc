import           Text.Pandoc.JSON
import           Text.Pandoc.R

main :: IO ()
main = toJSONFilter (insertRplots "plots")
