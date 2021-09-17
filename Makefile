R_OPTS = --vanilla
fname = 80-haskins-financials
rmd = .Rmd
source = $(fname)$(rmd)
outname = haskins-financials

date:
	date.exe "+%Y%m%d"

### building reports

html:
	R $(R_OPTS) -e "rmarkdown::render('"$(source)"', output_format='bookdown::html_document2')"

pdf:
	R $(R_OPTS) -e "rmarkdown::render('"$(source)"', output_format='bookdown::pdf_document2', output_file='$(outname).pdf')"

slides:
	R $(R_OPTS) -e "knitr::purl('$(source)')" ## pull source from Rmd file
	R $(R_OPTS) -e "rmarkdown::render('"03-slides.Rmd"')"
	rm -f "02-cohort1-dibels-pssa.R"

### cleaning up

## fix what needs fixing below to catch $(outfile) based names instead of $(fname)  

nocache:
	# delete cache files
	rm -rf $(fname)_cache $(fname)_files

tidy:
	# delete intermediate files
	rm -f $(fname).aux $(outname).log $(fname).out
	rm -f $(fname).tex $(fname).toc
	rm -f $(fname).md $(fname).knit.md $(fname).utf8.md

noreports:
	# delete formatted reports
	rm -f $(fname).html $(fname).pdf $(fname).docx

clean:
	make nocache
	make tidy
	make noreports
