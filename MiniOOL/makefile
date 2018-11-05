all: delete
	@echo "\nCreating lexer"
	ocamllex MiniOOL_lex.mll
	@echo "\nCreating parser"
	menhir -v --explain MiniOOL_parser.mly
	@echo "\nCompiling the lexer and parser:"
	ocamlc -g -c MiniOOL_parser.mli
	ocamlc -g -c MiniOOL_lex.ml
	ocamlc -g -c MiniOOL_parser.ml
	@echo "\nCompiling the interpreter"
	ocamlc -g -c MiniOOL.ml
	@echo "\nLinking the lexer, parser, and interpreter"
	ocamlc -g -o MiniOOL_raw MiniOOL_lex.cmo MiniOOL_parser.cmo MiniOOL.cmo
	echo "#!/bin/bash\nrlwrap ./MiniOOL_raw" >> MiniOOL
	chmod +x MiniOOL

delete:
	/bin/rm -f MiniOOL MiniOOL.cmi MiniOOL.cmo MiniOOL_lex.cmi MiniOOL_lex.cmo MiniOOL_lex.ml MiniOOL_parser.cmi MiniOOL_parser.cmo MiniOOL_parser.ml MiniOOL_parser.mli makefile~
