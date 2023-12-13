module Main where

import Lexer
import Parser
import TypeChecker
import Interpreter

--passa pelo lexer, parser, typecheck e eval

main = getContents >>= print . eval . typecheck . parser .lexer 
--runghc Main
-- cat example.mylang | runghc Main.hs

-- echo "if true then 2 else 5" | runghc Main.hs

--Testando no dia 01/12
--parser $ lexer $ "(\\x -> x + 1) 2"