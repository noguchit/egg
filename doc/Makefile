all: egg.html egg.pdf

egg.html : egg.texi
	makeinfo -o $@ --no-split --html $<

egg.pdf : egg.texi
	texi2pdf --tidy --pdf -o $@ $<

clean:
	-rm egg.html egg.pdf
	-rm -rf egg.t2d
