module Main where

import qualified Data.ByteString.Lazy as B
import           System.IO
import           System.Environment
import           Data.Word
import           Data.Char

data Machine = Machine B.ByteString Word8 B.ByteString

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
    return $ Machine p (ord . fromEnum $ a) n

parens :: String -> (String, String)
parens x = (body 0 x, after 0 x)
    where body c (x:xs)
            | c == 0 && x == ']' = []
            | c /= 0 && x == ']' = x : body (c - 1) xs
            | c == '['           = x : body (c + 1) xs
            | otherwise          = x : body x xs
          after c (x:xs)
            | c == 0 && x == ']' = xs
            | c /= 0 && x == ']' = after (c - 1) xs
            | c == '['           = after (c + 1) xs
            | otherwise          = after c xs

loop :: String -> Machine -> IO Machine
loop s m@(Machine p a n)
  | s == 0 = m
  | otherwise = do
      n <- brf s m
      loop s m

brf :: String -> Machine -> IO Machine
brf [] _ = []
brf (x:xs) m =
    case x of
        '>' -> brf s (incPtr m)
        '<' -> brf s (decPtr m)
        '+' -> brf s (incVal m)
        '-' -> brf s (devVal m)
        '.' -> do
            out
            brf s m
        ',' -> do
            n <- inp m
            brf s n
        '[' -> do
            let p = parens xs
            brf (snd parens) (loop (fst parens) m)
        _   -> brf xs m


main :: IO ()
main = do
    args <- getArgs
    h <- openFile (args !! 0) ReadMode
    t <- hGetContents h
    brf t
    return ()

