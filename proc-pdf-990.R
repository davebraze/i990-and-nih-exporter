library(fs)
library(here)

## library(tm)
## library(tesseract) ## ocr
library(pdftools)
library(magick)
library(stringr)

## Look at the ParseIRS990:: package at (https://github.com/p3lab/ParseIRS990)
## It's good for pulling xml formatted 990 info from the AWS

## Parts cribbed from https://www.charlesbordet.com/en/extract-pdf/ .

## All of the following assumes that PDFs of form 990s have been OCRed, as
## with Adobe Acrobat or similar tool.

## pdftools

## pdf_info("131628174_201612_990.pdf")
form990s <- dir_ls(".", regexp="990\\.pdf")

txt <- pdf_text(form990s[2])
p1 <-
    str_split(txt[[1]], "\r\n", simplify=TRUE) %>%
    str_trim()

fname <- form990s[1]

parse.990 <- function(fname){

    txt <- pdf_text(fname)
    p1 <-
        str_split(txt[[1]], "\r\n", simplify=TRUE) %>%
        str_trim()

    year <-
        str_subset(p1, "^A.*calendar year.*") %>%
        str_extract("[12][0-9]{3}") %>%
        as.integer()

    revenue <-
        str_subset(p1, ".*Total revenue.*") %>%
        str_extract("[0-9,-]+$") %>%
        str_remove_all(",") %>%
        as.integer()

    expenses <-
        str_subset(p1, ".*Total expenses.*") %>%
        str_extract("[0-9,-]+$") %>%
        str_remove_all(",") %>%
        as.integer()

    return(list(year=year,
                revenue=revenue,
                expenses=expenses))

}


## pdftools::pdf_text() returns a character vector with length equal to the
## number of pages in the PDF document. But content of vector elements is
## not exactly the aligned with page content. Regardless, these strings
## will need to be parsed further (e.g., into lines) before info can be
## easily extracted.


## tm
read <- readPDF(control = list(text="-layout"))
f990_2016_tm <- Corpus(URISource("./131628174_201612_990.pdf"), readerControl=list(reader=read))
f990_2016_tm_txt <- content(f990_2016_tm[[1]])

## This too yields a character vector with as many elements as document
## pages. The tm package relies on the external tool "pdftotext" to extract
## text from pdf files. It may be that fiddling with the control parameters
## for that tool can generate better results than the defaults used here.
