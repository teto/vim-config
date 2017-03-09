import System.IO
import Data.Maybe (catMaybes)

data OptionRecord = OptionRecord { name :: String, abbr :: String, desc :: String } deriving (Show)

getFirstWord :: String -> String
getFirstWord line
  | null line = []
  | null $ words line = []
  | otherwise = head $ words line

-- here we could do filter (/='\'') str
stripTicks :: String -> String
stripTicks w
  | null w = []
  | otherwise = init $ tail w

dropLinesTill :: String -> [String] -> ([String], Bool)
dropLinesTill firstWord rlines
  | null rlines = ([], False)
  | header == firstWord = (rlines, True)
  | otherwise = dropLinesTill firstWord $ tail rlines
  where header = getFirstWord $ head rlines

parseLine :: String -> Maybe OptionRecord
parseLine line
    | null settings = Nothing
    | length settings < 3 = Nothing
    | otherwise = Just $ OptionRecord (stripTicks $ head settings) (stripTicks $ head $ tail settings) (unwords $ tail $ tail settings)
    where settings = words line

stringifyRecord :: OptionRecord -> String
stringifyRecord OptionRecord { name =n , abbr=b, desc=d} = n ++ "," ++ b ++ "," ++ d

saveRecords :: Handle -> [OptionRecord] -> IO ()
saveRecords out records
  | null records = return ()
  | otherwise = do
    hPutStrLn out (stringifyRecord $ head records)
    saveRecords out ( tail records )

main :: IO ()
main = do
  fd <- Prelude.readFile "quickref.txt"
  let start = dropLinesTill "'aleph'" ( lines fd )
  let res = dropLinesTill "'writedelay'" $ reverse $ fst start
  let relevantLines = fst res
  -- create entries 
  print $ show relevantLines
  let records = map parseLine relevantLines
  -- filter out nothing values
  -- http://stackoverflow.com/questions/40327699/filtering-nothing-and-unpack-just
  let cleanRecords = catMaybes records
  -- let records = Just ( OptionRecord "name" "abbr" "desc" ++  records
  outh <- openFile "out.txt" WriteMode
  -- do
  saveRecords outh cleanRecords
  -- print records
  print "hello world"
  -- print $ "start=" ++ unlines start
