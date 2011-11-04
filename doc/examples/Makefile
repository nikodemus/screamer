.PHONY: clean

EXAMPLES:=$(foreach lisp, $(wildcard *.lisp), $(notdir $(addsuffix .html, $(lisp))))

html: $(EXAMPLES)

%.lisp.html: %.lisp
	sh ../../tools/htmlize-source.sh $< ../examples/$@ "Screamer Example: $(notdir $<)"

clean:
	rm -f *~ *.lisp.html \#* *.fasl
