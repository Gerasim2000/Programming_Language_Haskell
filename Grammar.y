{ 
module Grammar where 
import Lexer 
}

%name queryLang 
%tokentype { Token } 
%error { parseError }
%token 
    show   { TokenShow _ } 
    where  { TokenWhere _ } 
    if     { TokenIf _ }
    then   { TokenThen _ }
    else   { TokenElse _ }
    "<-"   { TokenAssign _ }
    '&'    { TokenAnd _ }
    '_'    { TokenSkip _ }
    '='    { TokenEq _ }
    "!="   { TokenNEq _ }
    '('    { TokenLParen _ } 
    ')'    { TokenRParen _ } 
    ','    { TokenSeparator _ }
--    '"'    { TokenQuote _ }
    var    { TokenVar _ $$ }
    str    { TokenString _ $$ }

%right for 
%left '&' 

--haskell happy documentation 2.5. Monadic Parsers
--%monad { E } { thenE } { returnE }

%% 

Exp : show '(' BoundVars ')' where RequirementList  { FinalExp  $3 $6 }

BoundVars : var ',' BoundVars    { $1:$3 }
     | var                       { [$1] }

ColumnList : Column ',' ColumnList     { $1:$3 }
     | Column                          { [$1] }

Column : var                   { Var $1 }
     | '_'                     { SkipVar }

RequirementList : Requirement '&' RequirementList { $1:$3 }
     | Requirement { [$1] }

Requirement : str '(' ColumnList ')'           { Table $1 $3 }
     | var '=' var                             { Eq $1 $3 }
     | var "!=" var                            { NEq $1 $3 } 
     | var '=' str                             { EqConst $1 $3 }
     | var "!=" str                            { NEqConst $1 $3 }
     | var "<-" var                            { AsignVarVar $1 $3 }
     | var "<-" str                            { AsignVarStr $1 $3 }
     | if '(' RequirementList ')' then '(' RequirementList ')' else '(' RequirementList ')'  { IfTF $3 $7 $11 }
     | if '(' RequirementList ')' then '(' RequirementList ')'                               { IfT $3 $7 }


{ 

---------------------DATA TYPES---------------------------

data Exp = FinalExp [String] RequirementList
     deriving (Eq,Show) 

data Column = Var String
      | SkipVar
     deriving (Eq,Show) 

data Requirement = Table String ColumnList
      | Eq String String
      | NEq String String
      | EqConst String String
      | NEqConst String String
      | AsignVarVar String String
      | AsignVarStr String String
      | IfTF RequirementList RequirementList RequirementList
      | IfT RequirementList RequirementList
     deriving (Eq,Show)      

type RequirementList = [Requirement]
type ColumnList = [Column]



parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))
parseError tokens = error "Parse error"

} 