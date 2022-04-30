R.OPTS := --vanilla
fname := 80-haskins-financials
rmd := .Rmd
source := $(fname)$(rmd)
outname := irs990-NIHgrant-case
date := $(shell date "+%Y%m%d")

.phoney: help html pdf slides nocache nopartials noreports clean
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
	R $(R_OPTS) -e "rmarkdown::render('"$(source)"', output_format='rmdformats::readthedown', output_file='$(outname).html')"

pdf:
# render pdf document
	R $(R_OPTS) -e "rmarkdown::render('"$(source)"', output_format='bookdown::pdf_document2', output_file='$(outname).pdf')"

slides:
# render slides
	R $(R_OPTS) -e "knitr::purl('$(source)')" ## pull source from Rmd file
	R $(R_OPTS) -e "rmarkdown::render('"03-slides.Rmd"')"
	rm -f "02-cohort1-dibels-pssa.R"


### deploy to web

html2web: 
# make html page suitable for web deployment & put it in '/docs/' folder
	sed '/CT Achievement Gap: NAEP 4th Grade Reading Scores/ r gtag.js' < $(outname).html > tmp0.html ## insert google analytics tag below page title
	sed '/CT Achievement Gap: NAEP 4th Grade Reading Scores/ r html-meta.txt' < tmp0.html > index.html ## insert meta tags below page title
	mv --backup index.html ./docs/.
	rm -f tmp0.html

publish: html html2web
# stage and commit webpage changes, then push changes to github
	git add --verbose 'docs/*'
	git commit --verbose -m "update webpage"
	git push --verbose --all

### cleaning up

## fix what needs fixing below to catch $(outfile) based names instead of $(fname)  

nocache:
# delete cache files
	rm -rf $(fname)_cache $(fname)_files

nopartials:
# delete partial products of knitting to pdf
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
