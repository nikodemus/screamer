.PHONY: doc webdocs wc clean all test examples

all:
	echo "Targets: clean, wc, doc, test, pages"

clean:
	rm -f *.fasl *~ \#*
	make -C doc clean

wc:
	wc -l *.lisp

test: clean
	sbcl --eval '(let ((asdf:*central-registry* (cons #p"./" asdf:*central-registry*))) (asdf:test-system :screamer) (quit))'

doc:
	make -C doc all

pages:
	rm -rf web-tmp
	mkdir web-tmp

	make -C doc html pdf
	cp doc/*.pdf doc/examples/*.lisp web-tmp/
	sh -c 'for f in doc/*.html doc/examples/*.html; \
          do sbcl --script tools/splice-to-head.lisp tools/analytics.script \
               < $$f > web-tmp/`basename $$f`; done'
	cp web-tmp/screamer.html web-tmp/index.html

	git checkout gh-pages
	mv web-tmp/* .
	rm -rf web-tmp
	git commit -a -c master
	git checkout -f master
