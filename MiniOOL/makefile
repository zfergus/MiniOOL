all: delete
	@echo "Creating lexer"
	ocamllex MiniOOL_lex.mll
	@echo "Creating parser"
	menhir MiniOOL_parser.mly
	@echo "Compiling the lexer and parser:"
	ocamlc -c MiniOOL_parser.mli
	ocamlc -c MiniOOL_lex.ml
	ocamlc -c MiniOOL_parser.ml
	@echo "Compiling the interpreter"
	ocamlc -c MiniOOL.ml
	@echo "Linking the lexer, parser, and interpreter"
	ocamlc -o MiniOOL MiniOOL_lex.cmo MiniOOL_parser.cmo MiniOOL.cmo
	# @echo "# using the calculator:"
	# @echo "X:=1; Y:=2; Z:= 3; (X+Y)*Z+4" | ./calculator
	# @echo "X:=1; Y:=2; *X; X" | ./calculator

delete:
	/bin/rm -f MiniOOL MiniOOL.cmi MiniOOL.cmo MiniOOL_lex.cmi MiniOOL_lex.cmo MiniOOL_lex.ml MiniOOL_parser.cmi MiniOOL_parser.cmo MiniOOL_parser.ml MiniOOL_parser.mli makefile~
