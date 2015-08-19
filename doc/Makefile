
.PHONY: default

PDFs := $(patsubst %.md,%.pdf,$(wildcard *.md))

#default: $(PDFs)
default: pores.pdf

%.tex: %.md
	multimarkdown -b -t latex $<

%.pdf: %.tex
	xelatex -shell-escape $<

clean:
	rm -f *.{aux,out,glo,idx,ist,log,tex}

clean-pdf: clean
	rm -f *.pdf

.DELETE_ON_ERROR:

