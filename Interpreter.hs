module Interpreter where

import Lexer
import Parser

isValue :: Expr -> Bool
isValue BTrue = True
isValue BFalse =  True
isValue (Num _) = True
isValue (Lam _ _ _) = True
isValue _ = False

--Função para substituir x por n na função lambda
subst :: String -> Expr -> Expr -> Expr
--subst "a" (Num 1) (Var "b")
subst x n (Var v) = if (x == v) then
                     n
                    else
                     (Var v)
-- v == \\x -> ; e o b == função que vem em seguida
subst x n (Lam v t b) = Lam v t (subst x n b)
--SUbstiituição da operação lambda
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)
-- subst "a" (Num 1) (Add (Var "a") (Num 3))
-- subst "a" (Num 1) (Add (Var "a") (Var "a"))
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
--Substitui no and
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2)
--Substitui no or
subst x n (Or e1 e2) = Or (subst x n e1) (subst x n e2)
--Substitui os op relacionais
subst x n (Menor e1 e2) = Menor (subst x n e1) (subst x n e2)
subst x n (Maior e1 e2) = Maior (subst x n e1) (subst x n e2)
subst x n (Iguais e1 e2) = Iguais (subst x n e1) (subst x n e2)
subst x n (MenorIgual e1 e2) = MenorIgual (subst x n e1) (subst x n e2)
subst x n (MaiorIgual e1 e2) = Maior (subst x n e1) (subst x n e2)
subst x n (Diferente e1 e2) = Diferente (subst x n e1) (subst x n e2)
--Substitui no if
subst x n (If e1 e2 e3) = If (subst x n e1) (subst x n e2) (subst x n e3)
subst x n (Mult e1 e2) = Mult (subst x n e1) (subst x n e2)
subst x n (Sub e1 e2) = Sub (subst x n e1) (subst x n e2)
subst x n (Paren e) = Paren (subst x n e)
subst x n (Let v e1 e2) = Let v (subst x n e1) (subst x n e2)
--Substituindo no while
subst x n (While cont e2) = While cont (subst x n e2)
--Se não cair em nenhum caso acima
subst x n e = e

--Passo: recebe uma expressão e retorna uma expressão
step :: Expr -> Expr
--O caso base é a soma de dois números
step (Add (Num n1) (Num n2)) = Num (n1 + n2) -- step (Add (Num 2) (Num 3))
--Primeiro caso recursivo para ser tratado: 
--quando o primeiro elemento for um número e o segundo uma expressão
step (Add (Num n) e) = Add (Num n) (step e) -- step (Add (Num 1) (Add (Num 1) (Num 2)))
--Segundo caso recursivo para ser tratado:
--quando os dois dados são expressões
step (Add e1 e2) = Add (step e1) e2 -- step (Add (Add (Num 1) (Num 2)) (Num 3))
-- let e' = step (Add (Add (Num 1) (Num 2)) (Num 3))
-- step e'
--Subtração
step (Sub (Num n1) (Num n2)) = Num (n1 - n2)
step (Sub (Num n) e) = Sub (Num n) (step e)
step (Sub e1 e2) = Sub (step e1) e2
-- step (Sub (Sub (Num 1) (Num 2)) (Num 3))
--Multiplicação
step (Mult (Num n1) (Num n2)) = Num (n1 * n2)
step (Mult (Num n) e) = Mult (Num n) (step e)
step (Mult e1 e2) = Mult (step e1) e2
-- step (Mult (Mult (Num 1) (Num 2)) (Num 3))
--Tratando
step (And BFalse _) = BFalse
step (And BTrue e) = e
step (And e1 e2) = And (step e1) e2
--step (And BTrue BTrue)
--step (And (And BTrue BFalse) BFalse)

--Adicionando a operação OR
step (Or BFalse BFalse) = BFalse
step (Or BFalse BTrue) = BTrue
step (Or BTrue _) = BTrue
step (Or e1 e2) = Or (step e1) e2

--Adicionando as operações relacionais
--Menor
step (Menor (Num n1) (Num n2)) = if n1 < n2 then BTrue else BFalse
step (Menor (Num n) e) = Menor (Num n) (step e)
step (Menor e1 e2) = Menor (step e1) e2
--Maior
step (Maior (Num n1) (Num n2)) = if n1 > n2 then BTrue else BFalse
step (Maior (Num n) e) = Maior (Num n) (step e)
step (Maior e1 e2) = Maior (step e1) e2
--Iguais
step (Iguais (Num n1) (Num n2)) = if n1 == n2 then BTrue else BFalse
step (Iguais (Num n) e) = Iguais (Num n) (step e)
step (Iguais e1 e2) = Iguais (step e1) e2
--Menor ou igual
step (MenorIgual (Num n1) (Num n2)) = if n1 <= n2 then BTrue else BFalse
step (MenorIgual (Num n) e) = MenorIgual (Num n) (step e)
step (MenorIgual e1 e2) = MenorIgual (step e1) e2
--Maior ou igual
step (MaiorIgual (Num n1) (Num n2)) = if n1 >= n2 then BTrue else BFalse
step (MaiorIgual (Num n) e) = MaiorIgual (Num n) (step e)
step (MaiorIgual e1 e2) = MaiorIgual (step e1) e2
--Diferentes
step (Diferente (Num n1) (Num n2)) = if n1 == n2 then BFalse else BTrue
step (Diferente (Num n) e) = Diferente (Num n) (step e)
step (Diferente e1 e2) = Diferente (step e1) e2

-- if false e1 e2 -> e2
step (If BFalse e1 e2) = e2
-- if true e1 e2 -> e1
step (If BTrue e1 e2) = e1
-- if e e1 e2 -> if e' e1 e2
step (If e e1 e2) = If (step e) e1 e2
-- step (If (BTrue) (Add (Num 1) (Num 2)) (Num 3))

--Tirando a expressão de dentro dos parentes
step (Paren e) = e

--Quando tiver uma aplicação
step (App (Lam x t b) e2) | isValue e2 = subst x e2 b
                          | otherwise = (App (Lam x t b) (step e2))
step (App e1 e2) = App (step e1) e2

--Para o let
step (Let v e1 e2) | isValue e1 = subst v e1 e2
                   | otherwise = Let v (step e1) e2

-- Para o while
step (While cont e2) = case cont of
                        0 -> e2
                        _ -> While (cont - 1) (step e2)
                     -- | (const == 0) = Num 0
                     -- | otherwise = While (const - 1) (step e2)

--Só para não dar erro
step e = e

eval :: Expr -> Expr
eval e | isValue e = e
       | otherwise = eval (step e)

-- eval $ parser $ lexer "if true && false then 4 + 5 else 2 + 4"