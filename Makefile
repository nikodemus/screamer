.PHONY: doc web wc clean all

all:
	echo "Targets: clean, wc, doc, web"

clean:
	rm -f *.fasl *~
	make -C doc clean

wc:
	wc -l *.lisp

doc:
	make -C doc

web: doc
	sbcl --script doc/splice-analytics.lisp < doc/screamer.html > tmp.html
	git checkout gh-pages
	mv tmp.html index.html
	git commit -a -c master
	git checkout -f master
