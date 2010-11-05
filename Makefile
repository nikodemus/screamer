.PHONY: doc web wc clean all test

all:
	echo "Targets: clean, wc, doc, test, web"

clean:
	rm -f *.fasl *~
	make -C doc clean

wc:
	wc -l *.lisp

test: clean
	sbcl --eval '(let ((asdf:*central-registry* (cons #p"./" asdf:*central-registry*))) (asdf:test-system :screamer) (quit))'

doc: test
	make -C doc

web: doc
	sbcl --script doc/splice-analytics.lisp < doc/screamer.html > tmp.html
	git checkout gh-pages
	mv tmp.html index.html
	git commit -a -c master
	git checkout -f master
