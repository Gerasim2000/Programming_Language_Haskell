{ 
module Lexer where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
  $white+       ; 
  "--".*        ; 
  show          { \p s -> TokenShow p } 
  where         { \p s -> TokenWhere p }
  if            { \p s -> TokenIf p }
  then          { \p s -> TokenThen p }
  else          { \p s -> TokenElse p }
  "<-"          { \p s -> TokenAssign p }
  \&            { \p s -> TokenAnd p }
  \_            { \p s -> TokenSkip p }
  \=            { \p s -> TokenEq p }
  "!="          { \p s -> TokenNEq p }
  \(            { \p s -> TokenLParen p }
  \)            { \p s -> TokenRParen p }
  \,            { \p s -> TokenSeparator p }
  \"            { \p s -> TokenQuote p }
  $alpha [$alpha $digit \_ \’]*               { \p s -> TokenVar p s } 
  \" [$alpha $digit $white \_ \’]* \"                { \p s -> TokenString p s }
  $digit+                                     { \p s -> TokenInt p (read s) } 

{ 
-- Each action has type :: AlexPosn -> String -> Token 

-- The token type: 
data Token = 
  TokenShow AlexPosn          | 
  TokenWhere  AlexPosn        | 
  TokenIf AlexPosn            |
  TokenThen AlexPosn          |
  TokenElse AlexPosn          |
  TokenAssign AlexPosn        |
  TokenAnd AlexPosn           |
  TokenSkip AlexPosn          |
  TokenEq AlexPosn            |
  TokenNEq AlexPosn           |
  TokenLParen AlexPosn        |
  TokenRParen AlexPosn        |
  TokenSeparator AlexPosn     |
  TokenQuote AlexPosn         |
  TokenVar AlexPosn String    |
  TokenString AlexPosn String |
  TokenInt AlexPosn Int       
  deriving (Eq,Show) 

tokenPosn :: Token -> String
tokenPosn (TokenInt  (AlexPn a l c) n) = show(l) ++ ":" ++ show(c) ++ " With int " ++ show n
tokenPosn (TokenVar  (AlexPn a l c) x) = show(l) ++ ":" ++ show(c) ++ " With Variable " ++ x
tokenPosn (TokenString (AlexPn a l c) x) = show(l) ++ ":" ++ show(c) ++ " With String " ++ x
tokenPosn (TokenShow (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ "show"
tokenPosn (TokenWhere (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ "where"
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ "if"
tokenPosn (TokenThen (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ "then"
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ "else"
tokenPosn (TokenAssign (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ "<-"
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ "&"
tokenPosn (TokenSkip (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ "_"
tokenPosn (TokenEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ "="
tokenPosn (TokenNEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ "!="
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ "("
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ ")"
tokenPosn (TokenSeparator (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ ","
tokenPosn (TokenQuote (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " With " ++ "\""

}