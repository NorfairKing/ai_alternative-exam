INTERACTION=nonstopmode
all:
	make alternative-exam.pdf >/dev/null 2>/dev/null
	make clean >/dev/null 2>/dev/null
%.pdf: %.tex
	bash makepdf.sh $(INTERACTION) $<
clean:
	rm -f *.aux *.log *.bbl *.bak *.blg *.toc *.out *.glg *.glo *.gls *.ist *~ *.*~* *.backup *.synctex.gz *.dvi
