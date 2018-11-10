# Compile MiniOOL
# Created by Zachary Ferguson

# Compiler flags
CFLAGS = -g

SDIR = src

ODIR = bin
_OBJ = flags.cmx abstractSyntaxTree.cmx programString.cmx \
abstractSyntaxTreeString.cmx staticSemantics.cmx lexer.cmx parser.cmx \
MiniOOL.cmx
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))


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

$(ODIR)/parser.cmx: $(ODIR)/parser.ml
	@echo "\033[1;32mCompiling the parser\033[0m"
	ocamlopt -c -o $@ -I $(ODIR) $< $(CFLAGS)
	@echo ""

$(ODIR)/%.cmx: $(SDIR)/%.ml
	@echo "\033[1;32mCompiling $<\033[0m"
	ocamlopt -c -o $@ -I $(ODIR) $< $(CFLAGS)
	@echo ""

# Clean up build files
.PHONY: clean
clean:
	@echo "\033[1;32mCleaning up build files\033[0m"
	/bin/rm -rf $(ODIR) MiniOOL makefile~
