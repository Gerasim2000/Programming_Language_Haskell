# use command "make" to compile the interpreter
# a csvql.exe will be generated upon compilation
# csvql.exe takes an argument input file "example.cql"
# Usage: .\csvql.exe example.cql

csvql: Lexer.hs Grammar.hs Interpreter.hs
	ghc -o csvql main.hs

Lexer.hs: Lexer.x
	alex -o Lexer.hs Lexer.x

Grammar.hs: Grammar.y
	happy -o Grammar.hs Grammar.y