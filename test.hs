-- import Text.Parsec
-- import Debug.Trace
-- import Text.Parsec.String
-- import Text.Parsec.String.Char
-- import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
-- import Data.Char
-- import Data.List.Split
-- import Text.Parsec.String.Combinator (many1)

import System.IO
import Data.Maybe (catMaybes)

-- data OptionName = String
-- data OptionType = String
-- data OptionShortDesc = String
-- data OptionScope = String

data OptionRecord = OptionRecord { name :: String, abbr :: String, desc :: String } deriving (Show)
-- anyChar :: Parser Char

-- data Entry = Entry {
--  name :: String,
--    typeOption :: String,
--  desc :: Just String,
--    scope :: String 
-- }

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

-- TODO return a tuple
dropLinesTill :: String -> [String] -> ([String], Bool)
dropLinesTill firstWord rlines
  | null rlines = ([], False)
  -- print "header=" ++ header
  -- trace("calling f with x = " ++ show header) header
  -- | null header = dropLinesTill $ tail rlines
  | header == firstWord = (rlines, True)
  | otherwise = dropLinesTill firstWord $ tail rlines
  where header = getFirstWord $ head rlines

parseLine :: String -> Maybe OptionRecord
parseLine line
    -- = OptionRecord "toto" "to"  "test desc"
    | null settings = Nothing
    | length settings < 3 = Nothing
    | otherwise = Just $ OptionRecord (stripTicks $ head settings) (stripTicks $ head $ tail settings) (unwords $ tail $ tail settings)
    where settings = words line
    -- if header == "aleph"
    -- then rlines
    -- else dropLinesTill $ tail rlines
    --   -- where header =  head $ words $ head rlines
  -- where let header = case
-- filter (/='\'')

-- saveRecords :: IO -> 
-- saveRecords fd records 
--   | null records = 

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

-- parseEntry = do
-- -- {{{
-- main = do
--  file <- Prelude.readFile "options.csv"
--  -- let h = head file
--  let l = lines file
--  let header = head l
--  let fields = split (dropDelims $ oneOf ",") header
--  -- print $ head fields
--  case fields!!1 of
--    "full_name" -> print "ok"
--    -- how different is it from otherwise ?
--    _ -> print "not ok"
--  putStrLn $ "hello world: " ++ head fields
--  -- return
-- -- }}}

-- parse parseEntry "aleph,number,the ASCII code for the first letter of the Hebrew alphabet,global"

-- import Text.ParserCombinators.Parsec
-- -- import Data.String.CSV
-- import Text.CSV
-- import qualified Data.Map as M
-- -- let r = do result <- parseFromFile csvFile "options.csv"
-- -- type CSV = [Record]
-- -- type Record = [Field] and Field = String

-- main = do
--  res <- parseCSVFromFile "options.csv" 
--  -- "toto,4,231,esalut"
--    -- "toto,4,231,esalut"
--  -- res <- parseCSVTest "toto,4,231,esalut"
--    case res of
--    Left errmsg -> putStrLn "an error happened"
--    Right v -> map_header_records v

-- map_header_records :: CSV -> IO ()
-- map_header_records [] = return ()
-- map_header_records (x:xs) = process x xs

-- process :: [String] -> CSV -> IO ()
-- process x [] = return ()
-- process x (y:ys) = do
--     let tuple = zip x y
--     let hash = M.fromList tuple
--     putStrLn (show (M.lookup "full_name" hash))
--     process x ys
--  -- | lefts res == False putStrLn "an error happened"
--  -- then putStrLn "an error happened"
--     -- else putStrLn "decoding failure"
