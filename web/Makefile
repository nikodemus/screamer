.PHONY: clean

EXAMPLES:=$(foreach lisp, $(wildcard ../examples/*.lisp), $(notdir $(addsuffix .html, $(lisp))))

all: $(EXAMPLES) index.html

%.lisp.html: ../examples/%.lisp
	cp $< .
	sh ../tools/htmlize-source.sh $< ../web/$@ "Screamer Example: $(notdir $<)"

index.html: ../doc/screamer.html ../tools/analytics.script
	sbcl --script ../tools/splice-to-head.lisp ../tools/analytics.script \
	  < ../doc/screamer.html > index.html

../doc/screamer.html:
	make -C ../doc

clean:
	rm -f *~ *.lisp *.lisp.html \#*
