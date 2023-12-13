--Começou +/- com 1:48 no dia 17/11
{
module Parser where

import Lexer
}

%name parser
%tokentype { Token }
%error { parserError }

%left '*'
%left '+' '-'

%token
    num         { TokenNum $$ }
    "+"         { TokenAdd }
    "-"         { TokenSub }
    "*"         { TokenMult }
    "&&"        { TokenAnd }
    "||"        { TokenOr }
    "<"         { TokenMenor }
    ">"         { TokenMaior }
    "=="        { TokenIguais }
    "<="        { TokenMenorIgual }
    ">="        { TokenMaiorIgual }
    "!="        { TokenDiferente }
    true        { TokenTrue }
    false       { TokenFalse }
    then        { TokenThen }
    if          { TokenIf }
    else        { TokenElse }
    while       { TokenWhile }
    var         { TokenVar $$ }
    '\\'        { TokenLam }
    "->"        { TokenArrow }
    '('         { TokenLParen }
    ')'         { TokenRParen }
    '='         { TokenEq }
    let         { TokenLet }
    in          { TokenIn }
    Bool        { TokenBoolean }
    Num         { TokenNumber }
    ':'         { TokenColon }

%%

Exp         : num                           { Num $1 }
            | true                          { BTrue }
            | false                         { BFalse }
            | Exp "+" Exp                   { Add $1 $3}
            | Exp "-" Exp                   { Sub $1 $3}
            | Exp "*" Exp                   { Mult $1 $3}
            | Exp "&&" Exp                  { And $1 $3}
            | Exp "||" Exp                  { Or $1 $3}
            | Exp "<" Exp                   { Menor $1 $3}
            | Exp ">" Exp                   { Maior $1 $3}
            | Exp "==" Exp                  { Iguais $1 $3}
            | Exp "<=" Exp                  { MenorIgual $1 $3}
            | Exp ">=" Exp                  { MaiorIgual $1 $3}
            | Exp "!=" Exp                  { Diferente $1 $3}
            | if Exp then Exp else Exp      { If $2 $4 $6 }
            | while num Exp                 { While $2 $3 }
            | var                           { Var $1 }
            | '\\' var ':' Type "->" Exp    { Lam $2 $4 $6 }
            | Exp Exp                       { App $1 $2 }
            | '(' Exp ')'                   { Paren $2 }
            | let var '=' Exp in Exp        { Let $2 $4 $6 }

Type    : Bool                              { TBool }
        | Num                               { TNum }
        | '(' Type "->" Type ')'            { TFun $2 $4 }
{
parserError :: [Token] -> a
parserError _ = error "Syntaz error!"
}

--happy Parser.y