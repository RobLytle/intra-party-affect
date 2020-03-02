
# define the `doc` and `cleandoc` phonies that allow you
# to run`make doc` and `make cleandoc` to build and 
# delete **only the pdf**, respectively.
all: dag doc

doc: doc/intra-party-affect.pdf
	rm -f rplots.pdf
	
dag: makefile-dag.png

cleandoc:
	rm -f doc/intra-party-affect.pdf
	cd fig; rm -f *.png 
	cd data; rm -f *.csv *.rds
	#cd data/raw; rm -f *.csv *.rds #comment out this line to prevent Makefile from running anes-cdf-trim.R every time you clean and rebuild. Should only need to be done if anes-cdf-trim.R is changed
	
makefile-dag.png: makefile-dag.R Makefile
	Rscript $<
	
# define the target, prereqs, and recipe to create the pdf

#

doc/fyp-rob-lytle.pdf: fig/ext-dag.png  doc/fyp-rob-lytle.tex
  # compile -> run bibLaTeX -> compile -> compile
	cd doc; pdflatex fyp-rob-lytle.tex
	cd doc; bibtex fyp-rob-lytle.tex
	cd doc; pdflatex fyp-rob-lytle.tex
	cd doc; pdflatex fyp-rob-lytle.tex
	# remove the junk files leftover
	cd doc; rm -f *.log *.synctex.gz *.out *.blg *.toc *.bak *.bbl *.aux
	
#fig/table-2016.png: R/perceived-ideology.R R/wrangle-2016.R
	#Rscript R/wrangle-2016
#	Rscript R/perceived-ideology

fig/ext-dag.png: dag.R
	Rscript dag.R
# define the `clean` phony so that you can run `make clean` 
# (or click *More* > *Clean All* in the build tab in RStudio) 
# and clean the entire project
clean: cleandoc