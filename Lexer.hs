module Lexer where

import Data.Char

data Expr = BTrue
          | BFalse
          | Num Int
          | Add Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Menor Expr Expr
          | Maior Expr Expr
          | Iguais Expr Expr
          | MenorIgual Expr Expr
          | MaiorIgual Expr Expr
          | Diferente Expr Expr
          --Adicionando a expressao if
          | If Expr Expr Expr
          --Adicionando subtração e multiplicação
          | Sub Expr Expr
          | Mult Expr Expr
          | Var String
          | Lam String Ty Expr
          --Lam "x" (Add (Var "x") (Num 2))
          --(lambdax -> x + 2) 3
          | App Expr Expr
          | Paren Expr
          | Let String Expr Expr
          -- Adicionando o while para o trabalho final
          --O funcionamento é o seguinte:
          -- 1. Verifica se o int é maior do que 0;
          -- 2. Executa o que está "dentro"
          | While Int Expr
    deriving Show

--Tipos da linguagem
data Ty = TBool
        | TNum
        | TFun Ty Ty
        deriving (Show, Eq)

data Token = TokenTrue
           | TokenFalse
           | TokenNum Int
           | TokenAdd
           | TokenAnd
           | TokenMenor
           | TokenMaior
           | TokenIguais
           | TokenMenorIgual
           | TokenMaiorIgual
           | TokenDiferente
           | TokenOr
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenSub
           | TokenMult
           | TokenVar String
           | TokenLam -- \\
           | TokenArrow
           | TokenLParen
           | TokenRParen
           | TokenLet
           | TokenEq
           | TokenIn
           | TokenColon --Representa o ":"
           | TokenBoolean
           | TokenNumber
           | TokenWhile
           -- parser $ lexer "\\x : Bool -> x"
           deriving (Show, Eq)

isSymb :: Char -> Bool
isSymb c = c `elem` "+&-*|\\->()=:<>!"


-- Le letrinha por letrinha para ver se faz parte da linguagem
lexer :: String -> [Token]
lexer [] = []
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer (c:cs) | isSpace c = lexer cs -- Ignora o espaço
             | isDigit c = lexNum (c:cs) -- Verifica se é um número, passa a lista inteira, pois o nº pode ter + de 1 caractere
             | isSymb c = lexSymbol (c:cs) -- Verifica se é um símbolo (pode ter 2 símbolos juntos)
             | isAlpha c = lexKW (c:cs)
lexer _ = error "Lexical error!"


lexNum :: String -> [Token]
-- span = meio que separa os dígitos
-- 123 + 456
-- O span vai colocar no num = "123"
-- e o rest vai ser = "+456"
lexNum cs = case span isDigit cs of
              -- read converte de string em int
              -- Aí coloca na lista [TokenNum 123, lexer rest]
              (num, rest) -> TokenNum (read num) : lexer rest

lexSymbol :: String -> [Token]
--Fica conferindo até que seja um símbolo
lexSymbol cs = case span isSymb cs of
                 ("+", rest) -> TokenAdd : lexer rest -- reconhece o +
                 ("&&", rest) -> TokenAnd : lexer rest -- reconhece o "and"
                 ("||", rest) -> TokenOr : lexer rest -- reconhece o "or"
                 ("<", rest) -> TokenMenor : lexer rest -- reconhece o menor
                 (">", rest) -> TokenMaior : lexer rest -- reconhece o maior
                 ("==", rest) -> TokenIguais : lexer rest -- reconhce o ==
                 ("<=", rest) -> TokenMenorIgual : lexer rest -- reconhece o menor ou igual
                 (">=", rest) -> TokenMaiorIgual : lexer rest -- reconhce o maior ou igual
                 ("!=", rest) -> TokenDiferente : lexer rest -- reconhce o diferente
                 ("-", rest) -> TokenSub : lexer rest -- reconhece o -
                 ("*", rest) -> TokenMult : lexer rest -- reconhece o *
                 ("\\", rest) -> TokenLam : lexer rest -- reconhece o almbda
                 ("->", rest) -> TokenArrow : lexer rest -- reconhece a flecha
                 ("(", rest) -> TokenLParen : lexer rest -- reconhe o abre parenteses
                 (")", rest) -> TokenRParen : lexer rest -- reconhe o fecha parenteses
                 ("=", rest) -> TokenEq : lexer rest
                 (":", rest) -> TokenColon : lexer rest
                 _ -> error "Lexical error: invalid symbol!" -- caso não encontre

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of
              ("true", rest) -> TokenTrue : lexer rest -- reconhece o true
              ("false", rest) -> TokenFalse : lexer rest -- reconhece o false
              ("if", rest) -> TokenIf : lexer rest -- reconhece o  if
              ("then", rest) -> TokenThen : lexer rest -- reconhece o then
              ("else", rest) -> TokenElse : lexer rest -- reconhece o else
              ("let", rest) -> TokenLet : lexer rest
              ("in", rest) -> TokenIn : lexer rest
              --Lexer> lexer "let x = 1 in x + 1"
              ("Num", rest) -> TokenNumber : lexer rest
              ("Bool", rest) -> TokenBoolean : lexer rest
              ("while", rest) -> TokenWhile : lexer rest
              (var, rest) -> TokenVar var : lexer rest
              -- lexer "\\x -> x"

-- lexer "if true then 2 + 5 else 4 + 6"