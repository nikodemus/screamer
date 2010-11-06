.PHONY: clean

EXAMPLES:=$(foreach lisp, $(wildcard ../examples/*.lisp), $(notdir $(addsuffix .html, $(lisp))))

all: $(EXAMPLES) index.html

%.lisp.html: ../examples/%.lisp
	cp $< .
	sh ../tools/htmlize-source.sh $< ../web/$@

index.html: ../doc/screamer.html ../tools/splice-analytics.lisp
	sbcl --script ../tools/splice-analytics.lisp < ../doc/screamer.html > index.html

../doc/screamer.html:
	make -C ../doc

clean:
	rm -f *~ *.lisp *.lisp.html \#*
