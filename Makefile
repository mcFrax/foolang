Langname := foo
ParserHsPrefixes := Abs Layout Lex Par Print Skel
ParserHs := $(addsuffix $(Langname).hs,$(ParserHsPrefixes)) ErrM.hs
ParserObjects := $(patsubst %.hs,%.o,$(ParserHs))
SourceHs := $(filter-out $(ParserHs) StdLib.hs Test$(Langname).hs,$(wildcard *.hs))
BnfcFiles := $(subst __,$(Langname),Lex__.x Par__.y Doc__.tex Doc__.txt Abs__.hs Print__.hs Layout__.hs Test__.hs Skel__.hs ErrM.hs)
GoodExamples := $(wildcard good/*.$(Langname))
BadExamples := $(wildcard bad/*.$(Langname))
Examples := $(GoodExamples) $(BadExamples)
TestBuild :=
TestBuildMarker := ./testbuild
OptBuildMarker := ./optbuild
BuildMarker := $(if $(TestBuild),$(TestBuildMarker),$(OptBuildMarker))
CommonGHCFlags := -cpp $(if $(TestBuild),-fhpc -prof -auto-all -caf-all,-O2) -DUSE_HASKELINE

all: Test$(Langname) interpreter

%: %.hs $(BuildMarker)
	ghc $(CommonGHCFlags) -Wall -Werror --make "$<" -o "$@"

interpreter: $(ParserObjects) $(filter-out parselib.hs,$(SourceHs)) #StdLib.hs
parselib: $(ParserObjects)

Test$(Langname): $(ParserObjects) Test$(Langname).hs
	# special rule without -Wall and -Werror
	ghc $(CommonGHCFlags) --make Test$(Langname).hs -o Test$(Langname)

StdLib.hs: stdlib.$(Langname) parselib
	./parselib stdlib.$(Langname) StdLib.hs stdlib

$(ParserObjects): %.o: %.hs $(ParserHs) $(BuildMarker)
	@# bnfc generated files  - compiled without -Wall and -Werror
	ghc $(CommonGHCFlags) -w --make "$<"

Lex$(Langname).hs: Lex$(Langname).x
	alex -g Lex$(Langname).x

Par$(Langname).hs: Par$(Langname).y
	happy -gcai Par$(Langname).y

Doc$(Langname).pdf: %.pdf: %.tex
	- pdflatex "$<"

readme.pdf: %.pdf: %.html
	wkhtmltopdf --title 'Adora language' "$<" "$@"

readme.html: readme.md github.css
	pandoc -s -S "$<" -o "$@" --css=github.css --self-contained --highlight-style=kate

$(BnfcFiles): $(Langname).cf
	bnfc -haskell $(Langname).cf >/dev/null || bnfc -haskell $(Langname).cf 1>&2
	touch $(BnfcFiles)  # tell `make` these files are up to date

GoodTestCases := $(addprefix test-case-,$(GoodExamples))
BadTestCases := $(addprefix test-case-,$(BadExamples))
TestCases := $(GoodTestCases) $(BadTestCases)
.PHONY: test $(TestCases) test-coverage

test: interpreter
	@./run-test.py $(GoodExamples) $(BadExamples)

define CollectCoverage
mkdir -p coverage;
hpc markup interpreter.tix --destdir=coverage $(patsubst %.hs,--exclude=%,$(ParserHs))
endef

test-coverage: $(if $(TestBuild),tix-clean test,)
	$(if $(TestBuild),$(CollectCoverage),$(MAKE) TestBuild=1 test-coverage)

$(GoodTestCases): test-case-%: % interpreter
	@./run-test.py "$<"

$(BadTestCases): test-case-%: % interpreter
	@./run-test.py "$<"


$(TestBuildMarker):
	if [ -f "$(OptBuildMarker)" ]; then $(MAKE) clean; fi
	touch $(TestBuildMarker)

$(OptBuildMarker):
	if [ -f "$(TestBuildMarker)" ]; then $(MAKE) clean; fi
	touch $(OptBuildMarker)

tix-clean:
	-rm -f *.tix

clean: tix-clean
	-rm -f *.log *.aux *.hi *.o *.ps *.dvi *.x *.y
	-rm -f readme.html readme.pdf Doc$(Langname).pdf
	-rm -f parselib StdLib.hs
	-rm -f $(TestBuildMarker) $(OptBuildMarker)

distclean: clean
	-rm -f Doc$(Langname).* Lex$(Langname).* Par$(Langname).* Layout$(Langname).* Skel$(Langname).* Print$(Langname).* Test$(Langname).* Abs$(Langname).* ErrM.* SharedString.* $(Langname).dtd XML$(Langname).*
	-rm -f coverage/*
	-rm -f interpreter Test$(Langname)
