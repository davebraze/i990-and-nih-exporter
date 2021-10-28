R_OPTS := --vanilla
fname := 80-haskins-financials
rmd := .Rmd
source := $(fname)$(rmd)
outname := haskins-financials
date := $(shell date "+%Y%m%d")

.phoney: help html pdf slides nocache tidy noreports clean
## Few targets correspond to files, so, list them here to ensure they always run.

## TODO:

help:
## Print Constants and Targets defined in this Makefile
	@echo Constants::
	@echo R.OPTS: $(R.OPTS)
	@echo fname: $(fname)
	@echo rmd: $(rmd)
	@echo source: $(source)
	@echo outname: $(outname)
	@echo date: $(date)
	@echo
	@echo Targets::
	@echo --
	@grep -E -A 1 "^[^\# ]+:" Makefile
	@echo --
	@echo 

### building reports

html:
# render html document
	R $(R_OPTS) -e "rmarkdown::render('"$(source)"', output_format='bookdown::html_document2', output_file='$(outname).html')"

pdf:
# render pdf document
	R $(R_OPTS) -e "rmarkdown::render('"$(source)"', output_format='bookdown::pdf_document2', output_file='$(outname).pdf')"

slides:
# render slides
	R $(R_OPTS) -e "knitr::purl('$(source)')" ## pull source from Rmd file
	R $(R_OPTS) -e "rmarkdown::render('"03-slides.Rmd"')"
	rm -f "02-cohort1-dibels-pssa.R"

### cleaning up

## fix what needs fixing below to catch $(outfile) based names instead of $(fname)  

nocache:
# delete cache files
	rm -rf $(fname)_cache $(fname)_files

tidy:
# delete intermediate files creating by knitting to pdf
	rm -f $(fname).aux $(outname).log $(fname).out
	rm -f $(fname).tex $(fname).toc
	rm -f $(fname).md $(fname).knit.md $(fname).utf8.md

noreports:
# delete formatted reports
	rm -f $(outname).html $(outname).pdf $(outname).docx

clean:
# nocache tidy noreports
	make nocache
	make tidy
	make noreports
