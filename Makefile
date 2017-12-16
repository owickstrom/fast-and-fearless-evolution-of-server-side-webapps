TEX=pdflatex

MASTER=src/master.tex
TEX_OPTIONS=options.tex
SRCS=$(shell find src -name '*.tex') \
     $(shell find src -name '*.bib') \
		 $(shell find src/listings) \
		 src/slides.tex
IMGS=$(shell find src -name '*.png')

HS_SRCS=$(shell find src/listings/scotty-demo -name '*.hs') \
				src/listings/scotty-demo/scotty-demo.cabal \
				src/listings/scotty-demo/stack.yaml

PANDOC_FLAGS= -t beamer \
		-f markdown+multiline_tables \
						 -s \
						 -H src/customizations.tex \
						 -Vurlcolor=linkcolor \
						 --highlight-style=haddock \
						 --slide-level=2 \
						 --filter pandoc-include-code \
						 -fmarkdown-implicit_figures \

SLIDES_DIR=target/slides
SLIDES=$(SLIDES_DIR)/slides.pdf

SLIDES_NO_NOTES_DIR=target/slides-no-notes
SLIDES_NO_NOTES=$(SLIDES_NO_NOTES_DIR)/slides-no-notes.pdf

.PHONY: all
all: slides programs

.PHONY: slides
slides: $(SLIDES) $(SLIDES_NO_NOTES)

target/slides.tex: src/slides.md src/customizations.tex src/notes.tex $(HS_SRCS) $(IMGS)
	mkdir -p target
	pandoc $(PANDOC_FLAGS) \
		-H src/notes.tex \
		$< \
		-o $@

target/slides-no-notes.tex: src/slides.md src/customizations.tex
	mkdir -p target
	pandoc $(PANDOC_FLAGS) -V classoption=handout $< -o $@

$(SLIDES): target/slides.tex
	rm -rf $(SLIDES_DIR)
	mkdir -p $(SLIDES_DIR)
	cp target/slides.tex $(SLIDES_DIR)/slides.tex
	cd $(SLIDES_DIR) && \
		$(TEX) \
		-jobname slides \
		-halt-on-error \
		slides.tex

$(SLIDES_NO_NOTES): target/slides-no-notes.tex
	rm -rf $(SLIDES_NO_NOTES_DIR)
	mkdir -p $(SLIDES_NO_NOTES_DIR)
	cp target/slides-no-notes.tex $(SLIDES_NO_NOTES_DIR)/slides.tex
	cd $(SLIDES_NO_NOTES_DIR) && \
		$(TEX) \
		-jobname slides-no-notes \
		-halt-on-error \
		slides.tex

programs:
	cd src/listings/scotty-demo && cabal new-build
	cd src/listings/yesod-demo && stack build
	cd src/listings/airship-demo && cabal new-build

clean:
	rm -rf target
