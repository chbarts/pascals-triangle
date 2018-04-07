import System.IO
import System.Exit
import Text.Printf
import Numeric.Natural
import System.Environment

choose :: Integral a => a -> a -> a
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

numDig :: Integral a => a -> a
numDig n = floor ((logBase 10 (fromIntegral n)) + 1)

sizeAtLevel :: Integral a => a -> a
sizeAtLevel n = foldr max 0 (map (\x -> numDig (choose n x)) [0..n])

makeLevel :: Integral a => a -> [a]
makeLevel n = map (\x -> choose n x) [0..n]

-- In characters, n is how tall triangle is in total
baseWidth :: Integral a => a -> a
baseWidth n = (2*n + 1) * (sizeAtLevel n)

printInCell :: (PrintfArg a, Integral a) => a -> a -> String
printInCell width num = printf "%*s%*d%*s" pad "" res num pad ""
  where
    nlen = numDig num
    pad = (width - nlen) `div` 2
    res = width - 2*pad

interleave :: a -> [a] -> [a]
interleave _ []       = []
interleave _ (x:[])   = [x]
interleave z (x:y:xs) = [x,z] ++ (interleave z (y:xs))

levelString :: (PrintfArg a, Integral a) => a -> a -> String
levelString totlev lev = padstr ++ levstr ++ padstr
  where
    cellsize = sizeAtLevel totlev
    padcell = take (fromIntegral cellsize) (repeat ' ')
    basecells = 2*totlev + 1
    levelcells = 2*lev + 1
    padstr = foldr (++) "" (take (fromIntegral ((basecells - levelcells) `div` 2)) (repeat padcell))
    levstr = foldr (++) "" (interleave padcell (map (printInCell (sizeAtLevel totlev)) (makeLevel lev)))

printTriangle :: (PrintfArg a, Integral a) => a -> IO ()
printTriangle totlev = mapM_ putStrLn (map (levelString totlev) [0..totlev])

doit :: String -> IO ()
doit str = case ((reads str)::[(Natural,String)]) of
  [(val, _)] -> printTriangle val
  _          -> hPutStrLn stderr (str ++ " not a valid natural number")

help :: IO ()
help = do
  putStrLn "pascals-triangle nums..."
  putStrLn "Print Pascal's Triangles with num levels"

version :: IO ()
version = do
  putStrLn "pascals-triangle version 1.0"
  putStrLn "Chris Barts <chbarts@gmail.com> 2018"

parse :: [String] -> IO ()
parse ["-h"]        = help
parse ["--help"]    = help
parse ["-v"]        = version
parse ["--version"] = version
parse []            = help >> exitFailure
parse lns           = mapM_ doit lns

main :: IO ()
main = getArgs >>= parse >> (exitWith ExitSuccess)
