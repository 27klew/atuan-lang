## File generated by the BNF Converter (bnfc 2.9.4.1).

# Makefile for building the parser and test program.

GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : Atuan/Interpreter

# Rules for building the parser.

Atuan/Abs.hs Atuan/Lex.x Atuan/Par.y Atuan/Print.hs Atuan/Test.hs : Atuan.cf
	bnfc --haskell -d --functor Atuan.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

Atuan/Interpreter : Atuan/Abs.hs Atuan/Lex.hs Atuan/Par.hs Atuan/Print.hs Atuan/Test.hs Atuan/CollectTypes.hs Atuan/AlgorithmW.hs Atuan/Translate.hs Atuan/Interpreter.hs Atuan/Types.hs
	${GHC} ${GHC_OPTS} $@

# Rules for cleaning generated files.

clean :
	-rm -f Atuan/*.hi Atuan/*.o Atuan/*.log Atuan/*.aux Atuan/*.dvi
	-rm -f Atuan/Interpreter Atuan/Test

distclean : clean
	-rm -f Atuan/Abs.hs Atuan/Abs.hs.bak Atuan/ComposOp.hs Atuan/ComposOp.hs.bak Atuan/Doc.txt Atuan/Doc.txt.bak Atuan/ErrM.hs Atuan/ErrM.hs.bak Atuan/Layout.hs Atuan/Layout.hs.bak Atuan/Lex.x Atuan/Lex.x.bak Atuan/Par.y Atuan/Par.y.bak Atuan/Print.hs Atuan/Print.hs.bak Atuan/Skel.hs Atuan/Skel.hs.bak Atuan/Test.hs Atuan/Test.hs.bak Atuan/XML.hs Atuan/XML.hs.bak Atuan/AST.agda Atuan/AST.agda.bak Atuan/Parser.agda Atuan/Parser.agda.bak Atuan/IOLib.agda Atuan/IOLib.agda.bak Atuan/Main.agda Atuan/Main.agda.bak Atuan/Atuan.dtd Atuan/Atuan.dtd.bak Atuan/Test Atuan/Lex.hs Atuan/Par.hs Atuan/Par.info Atuan/ParData.hs Makefile
	-rmdir -p Atuan/

# EOF
