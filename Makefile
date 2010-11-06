.PHONY: doc web wc clean all test

all:
	echo "Targets: clean, wc, doc, test, web"

clean:
	rm -f *.fasl *~ examples/*.fasl examples/*~
	make -C doc clean
	make -C web clean

wc:
	wc -l *.lisp

test: clean
	sbcl --eval '(let ((asdf:*central-registry* (cons #p"./" asdf:*central-registry*))) (asdf:test-system :screamer) (quit))'

doc:
	make -C doc

web:
	make -C web

gh-pages: web
	rm -rf web-tmp
	mv web web-tmp
	git checkout gh-pages
	cp web-tmp/* .
	git commit -a -c master
	mv web-tmp web
	git checkout -f master
