
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


doc/intra-party-affect.pdf: doc/intra-party-affect.tex fig/isl-model.tex fig/table-2016.tex doc/references.bib data/tidy-cdf.rds data/tidy-primaries.rds fig/cdf-scatter-dem.png fig/cdf-ideo-line.png
  # compile -> run bibLaTeX -> compile -> compile
	cd doc; pdflatex intra-party-affect.tex
	cd doc; bibtex intra-party-affect.tex
	cd doc; pdflatex intra-party-affect.tex
	cd doc; pdflatex intra-party-affect.tex
	# remove the junk files leftover
	cd doc; rm -f *.log *.synctex.gz *.out *.blg *.toc *.bak *.bbl *.aux
	
#fig/table-2016.png: R/perceived-ideology.R R/wrangle-2016.R
	#Rscript R/wrangle-2016
#	Rscript R/perceived-ideology

fig/isl-model.tex: R/isl-replication.R
	Rscript R/isl-replication.R
	
fig/cdf-sd.png: R/plots-cdf

fig/table-levene.tex: R/variance-tests.R
	Rscript R/variance-tests.R

fig/table-2016.tex: R/primary-tables.R data/tidy-primaries.rds
	Rscript R/primary-tables.R

fig/cdf-ideo-line.png: R/plots-cdf.R data/tidy-cdf.rds
	Rscript R/plots-cdf.R
	
fig/cdf-scatter-dem.png: R/plots-cdf.R data/tidy-cdf.rds
	Rscript R/plots-cdf.R
	
#fig/therm-hist-2016.png: R/plots-2016.R data/tidy-2016.rds 
#	Rscript R/plots-2016-deprecated.R
	
fig/therm-ideo-scatter-2016.png: R/plots-primaries.R data/tidy-2016.rds
	Rscript R/plots-primaries.R
	
data/tidy-primaries.rds: R/wrangle-primary.R data/tidy-2016.rds
	Rscript R/wrangle-primary.R
	
data/tidy-primaries.csv: R/wrangle-primary.R
	Rscript R/wrangle-primary.R

data/tidy-cdf.csv: R/wrangle-cdf.R data/raw/cdf-raw-trim.csv
	Rscript R/wrangle-cdf.R
	
data/tidy-cdf.rds: R/wrangle-cdf.R data/raw/cdf-raw-trim.csv
	Rscript R/wrangle-cdf.R
	
data/tidy-2016.csv: R/wrangle-2016.R
	Rscript R/wrangle-2016.R

data/tidy-2016.rds: R/wrangle-2016.R
	Rscript R/wrangle-2016.R
	
data/raw/cdf-raw-trim.csv: R/anes-cdf-trim.R
	Rscript R/anes-cdf-trim.R
	
	
# define the `clean` phony so that you can run `make clean` 
# (or click *More* > *Clean All* in the build tab in RStudio) 
# and clean the entire project
clean: cleandoc
	
#fig/table-2016.png: R/perceived-ideology.R R/wrangle-2016.R
	#Rscript R/wrangle-2016
#	Rscript R/perceived-ideology

fig/ext-dag.png: dag.R
	Rscript dag.R
# define the `clean` phony so that you can run `make clean` 
# (or click *More* > *Clean All* in the build tab in RStudio) 
# and clean the entire project
clean: cleandoc