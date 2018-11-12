# Compile MiniOOL
# Written by Zachary Ferguson

# Compiler flags
CFLAGS = -g

SDIR = src
DDIR = doc
ODIR = bin
_OBJ_PARSER = flags.cmx abstractSyntaxTree.cmx programString.cmx \
abstractSyntaxTreeString.cmx staticSemantics.cmx lexer.cmx
_OBJ = $(_OBJ_PARSER) parser.cmx MiniOOL.cmx
OBJ_PARSER = $(patsubst %,$(ODIR)/%,$(_OBJ_PARSER))
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

_DEPS = flags.ml abstractSyntaxTree.ml programString.ml \
abstractSyntaxTreeString.ml staticSemantics.ml lexer.ml
DEPS = $(patsubst %,$(SDIR)/%,$(_DEPS))

# Print some helpful usage information
.PHONY: all
all: $(ODIR) MiniOOL
	@echo "\033[1;32mSuccessfully compiled MiniOOL!\033[0m"
	@echo "To use MiniOOL run '\033[1;36m./MiniOOL\033[0m'."
	@echo "Optionally run '\033[1;36m./MiniOOL --verbose\033[0m' to print the\n\
	abstract syntax tree and additional information."

$(ODIR):
	@echo "\033[1;32mMaking a bin directory to store object files\033[0m"
	mkdir -p $(ODIR)
	@echo ""

# Wrap MiniOOL_raw with rlwrap so it has a history and arrow key usage
MiniOOL: $(ODIR)/MiniOOL_raw
	@echo "\033[1;32mCreating a script to wrap MiniOOL with rlwrap\033[0m"
	printf "#!/bin/bash\nrlwrap ./$(ODIR)/MiniOOL_raw \"$$%s\"" "@" > MiniOOL
	chmod +x MiniOOL
	@echo ""

# Link all compiled files together
$(ODIR)/MiniOOL_raw: $(OBJ)
	@echo "\033[1;32mLinking the lexer, parser, and interpreter\033[0m"
	ocamlopt -o $@ $^ $(CFLAGS)
	@echo ""

# Lexer
$(ODIR)/lexer.ml: $(SDIR)/lexer.mll
	@echo "\033[1;32mCreating lexer\033[0m"
	ocamllex -o $@ $<
	@echo ""

$(ODIR)/lexer.cmx: $(ODIR)/lexer.ml $(ODIR)/parser.cmi
	@echo "\033[1;32m\nCompiling the lexer\033[0m"
	ocamlopt -c -o $@ -I $(ODIR) $(ODIR)/lexer.ml $(CFLAGS)
	@echo ""

# Parser
$(ODIR)/parser.ml: $(SDIR)/parser.mly
	@echo "\033[1;32mCreating parser\033[0m"
	menhir -v --explain -b $(ODIR)/parser $(SDIR)/parser.mly

$(ODIR)/parser.cmi: $(ODIR)/parser.ml
	ocamlopt -c -o $@ -I $(ODIR) $(ODIR)/parser.mli $(CFLAGS)

$(ODIR)/parser.cmx: $(ODIR)/parser.ml $(OBJ_PARSER)
	@echo "\033[1;32mCompiling the parser\033[0m"
	ocamlopt -c -o $@ -I $(ODIR) $< $(CFLAGS)
	@echo ""

$(ODIR)/MiniOOL.cmx: $(SDIR)/MiniOOL.ml $(SDIR)/flags.ml $(ODIR)/parser.cmx
	@echo "\033[1;32mCompiling the MiniOOL.ml\033[0m"
	ocamlopt -c -o $@ -I $(ODIR) $< $(CFLAGS)
	@echo ""

$(ODIR)/%.cmx: $(SDIR)/%.ml
	@echo "\033[1;32mCompiling $<\033[0m"
	ocamlopt -c -o $@ -I $(ODIR) $< $(CFLAGS)
	@echo ""

.PHONY: docs
docs:
	@echo "\033[1;32mCreating documentation files\033[0m"
	mkdir -p $(DDIR)
	ocamldoc -html -charset "utf-8" -I $(ODIR) -d $(DDIR) $(SDIR)/*.ml $(ODIR)/*.ml

# Clean up build files
.PHONY: clean
clean:
	@echo "\033[1;32mCleaning up build files\033[0m"
	/bin/rm -rf $(ODIR) MiniOOL makefile~
