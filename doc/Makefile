.PHONY: clean html pdf info

all: html pdf info

clean:
	rm -rf include
	rm -f  *.pdf *.html *.info
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr
	rm -f include-stamp

include-stamp: ../*.lisp ../*.asd
	sbcl \
	--eval '(require :sb-texinfo)' \
	--eval '(let ((asdf:*central-registry* (cons #p"../" asdf:*central-registry*))) (require :screamer))' \
	--eval '(sb-texinfo:generate-includes "include/" (list :screamer) :base-package :screamer)' \
	--eval '(quit)'
	touch include-stamp

%.html: %.texinfo style.css include-stamp
	makeinfo --html --no-split --css-include=style.css $<

%.pdf: %.texinfo include-stamp
	texi2dvi -p $<

%.info: %.texinfo include-stamp
	makeinfo --no-split $<

html: screamer.html

pdf: screamer.pdf

info: screamer.info
