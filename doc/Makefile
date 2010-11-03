.PHONY: clean html include doc

doc: html

clean:
	rm -rf include
	rm -f  *.pdf *.html *.info
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr

include:
	sbcl \
	--eval '(let ((asdf:*central-registry* (cons #p"../" asdf:*central-registry*))) (require :screamer))' \
        --load docstrings.lisp \
	--eval '(sb-texinfo:generate-includes "include/" (list :screamer) :base-package :screamer)' \
	--eval '(quit)'

html: include
	makeinfo --html --no-split --css=style.css screamer.texinfo
