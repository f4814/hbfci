module Main where

import System.Environment
import System.IO
import System.Process

header :: Int -> String
header i = "#include <stdio.h>\nchar array[" ++ show i ++ "]= {0}; char *ptr=array; void main() {"

footer :: String
footer = "}"

compile :: Char -> String
compile c =
    case c of
        '>' -> "++ptr;"
        '<' -> "--ptr;"
        '+' -> "++*ptr;"
        '-' -> "--*ptr;"
        '.' -> "putchar(*ptr);"
        ',' -> "*ptr=getchar();"
        '[' -> "while (*ptr) {"
        ']' -> "}"
        _   -> ""

source :: String -> String
source inp = concat $ map compile inp

code :: Int -> String -> String
code len inp = header len ++ source inp ++ footer

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
        then putStrLn "USAGE:\nhbfc [stack size] [brainfuck file]\n Compiles Brainfuck to C code."
        else do
            inp <- readFile (args !! 1)
            writeFile (args !! 1 ++ ".c") (code (read $ args !! 0) inp )
