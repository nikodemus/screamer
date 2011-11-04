.PHONY: clean html pdf info examples

EXAMPLES=$(foreach lisp, $(wildcard ../examples/*.lisp), $(notdir $(addsuffix .html, $(lisp))))

all: html pdf info

clean:
	rm -f *~ \#*
	rm -rf include
	rm -f  *.pdf *.html *.info
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr *.vrs
	rm -f include-stamp
	make -C examples clean

include-stamp: ../*.lisp ../*.asd
	sbcl \
	--eval '(require :sb-texinfo)' \
	--eval '(let ((asdf:*central-registry* (cons #p"../" asdf:*central-registry*))) (require :screamer))' \
	--eval '(sb-texinfo:generate-includes "include/" (list :screamer) :base-package :screamer)' \
	--eval '(quit)'
	touch include-stamp

examples:
	make -C examples html

%.html: %.texinfo style.css include-stamp examples
	makeinfo --html --no-split --css-include=style.css $<

%.pdf: %.texinfo include-stamp examples
	texi2dvi -p $<

%.info: %.texinfo include-stamp examples
	makeinfo --no-split $<

html: screamer.html

pdf: screamer.pdf

info: screamer.info
