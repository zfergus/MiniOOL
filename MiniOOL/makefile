all: clean
	@echo "\nCreating lexer"
	ocamllex lexer.mll
	@echo "\nCreating parser"
	menhir -v --explain parser.mly
	@echo "\nCompiling the lexer and parser:"
	ocamlc -g -c abstractSyntaxTree.ml
	ocamlc -g -c to_string.ml
	ocamlc -g -c to_tree_string.ml
	ocamlc -g -c staticSemantics.ml
	ocamlc -g -c parser.mli
	ocamlc -g -c lexer.ml
	ocamlc -g -c parser.ml
	@echo "\nCompiling the interpreter"
	ocamlc -g -c MiniOOL.ml
	@echo "\nLinking the lexer, parser, and interpreter"
	ocamlc -g -o MiniOOL_raw abstractSyntaxTree.cmo to_string.cmo \
		to_tree_string.cmo staticSemantics.cmo lexer.cmo \
		parser.cmo MiniOOL.cmo
	echo "#!/bin/bash\nrlwrap ./MiniOOL_raw" >> MiniOOL
	chmod +x MiniOOL

clean:
	/bin/rm -f *.cmo *.cmi *.cmx *.o MiniOOL MiniOOL_raw lexer.ml \
		parser.ml parser.mli *.automaton *.conflicts makefile~
