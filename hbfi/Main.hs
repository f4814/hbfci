module Main where

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BC
import           System.IO
import           System.Environment
import           Data.Word
import           Data.Char

data Machine = Machine B.ByteString Word8 B.ByteString

instance Show Machine where
    show (Machine p a n) = (show . map ord . BC.unpack . B.take 10 $ p) ++
        show a ++ (show . map ord . BC.unpack . B.take 10 $ p)

empty :: Machine
empty = Machine (B.repeat 0) 0 (B.repeat 0)

incPtr :: Machine -> Machine
incPtr (Machine p a n) = Machine (a `B.cons` p) (B.head n) (B.tail n)

decPtr :: Machine -> Machine
decPtr (Machine p a n) = Machine (B.tail p) (B.head p) (a `B.cons` n)

incVal :: Machine -> Machine
incVal (Machine p a n) = Machine p (a + 1) n

decVal :: Machine -> Machine
decVal (Machine p a n) = Machine p (a - 1) n

out :: Machine -> IO ()
out (Machine _ a _) = putStrLn . show . chr . fromEnum $ a

inp :: Machine -> IO Machine
inp (Machine p _ n) = do
    a <- getLine
    return $ Machine p (fromIntegral . ord . head $ a) n

parens :: String -> (String, String)
parens x = (body 0 x, after 0 x)
    where body c (x:xs)
            | c == 0 && x == ']' = ""
            | c /= 0 && x == ']' = x : body (c - 1) xs
            | x == '['           = x : body (c + 1) xs
            | otherwise          = x : body c xs
          after c (x:xs)
            | c == 0 && x == ']' = xs
            | c /= 0 && x == ']' = after (c - 1) xs
            | x == '['           = after (c + 1) xs
            | otherwise          = after c xs

loop :: String -> Machine -> IO Machine
loop s m@(Machine p a n)
  | a == 0 = return m
  | otherwise = do
      n <- brf s m
      loop s n

brf :: String -> Machine -> IO Machine
brf [] m = return m
brf (x:xs) m =
    case x of
        '>' -> brf xs (incPtr m)
        '<' -> brf xs (decPtr m)
        '+' -> brf xs (incVal m)
        '-' -> brf xs (decVal m)
        '.' -> do
            out m
            brf xs m
        ',' -> do
            n <- inp m
            brf xs n
        '[' -> do
            let p = parens xs
            l <- loop (fst p) m
            brf (snd p) l
        _   -> brf xs m


main :: IO ()
main = do
    args <- getArgs
    h <- openFile (args !! 0) ReadMode
    t <- hGetContents h
    brf t empty
    return ()

