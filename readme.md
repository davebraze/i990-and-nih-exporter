# README

The code in this repository is an exercise in working with public data from two main sources. The first source is IRS form 990, "Return of Organization Exempt from Income Tax." The 990 is an informational form that non-profit organizations (NPO) must file with the Internal Revenue Service (IRS) each year. The completed forms are publicly available records as a matter of federal law. The second source of information used here is details of individual grants from National Institutes of Health (NIH). The NIH Exporter gives access to information about all individual grants made by the NIH. That information includes the direct and indirect moneys paid to grantees each year for each grant. 

NIH grant data can be downloaded from the [NIH Exporter web tool](https://exporter.nih.gov/). The data files are available for each fiscal year since 1985 in either CSV or XML format. I've chosen to do bulk downloads of NIH "Project" data files (in CSV format). See [01-load-data.R](https://github.com/davebraze/i990-and-nih-exporter/blob/d8682694ee34b491cda8d6943d6ab84179799929/01-load-data.R#L74) for details. Another option would have been to use the NIH REST API documented in a PDF entitled [Reporter API Data Elements](https://api.reporter.nih.gov/documents/Data%20Elements%20for%20RePORTER%20Project%20API%20v2.pdf). But, I consider the bulk download approach I've used here to be simpler and more efficient. With the bulk data on my machine, I then do filtering and variable selection locally using regular `dplyr` operations. If you're more partial to API-based downloads, you might check out  [repoRter.nih](https://github.com/bikeactuary/repoRter.nih), an R package by [Michael Barr](http://bikeactuary.com/) that was released in Feb. 2022. It's at an early stage of development and looks fairly bare-bones, but it will almost certainly save you some headaches.

A convenient place to get IRS 990s is from the [ProPublica Nonprofit Explorer](https://projects.propublica.org/nonprofits/). It is necessary to search for the particular NPOs you are interested in, and individually download form 990s for each tax year of interest. The form for a specific NPO/tax-year will be available either as a scanned PDF, if it was filed as a paper document, or as an XML file, if it was filed electronically. All NPOs are required to file electronically from 2019 forward. Another option would be to use the [IRS search and bulk download](https://www.irs.gov/charities-non-profits/tax-exempt-organization-search) tools.

Possibly useful resources are IRS 990
 +   https://appliednonprofitresearch.com/posts/2018/06/the-irs-990-e-file-dataset-getting-to-the-chocolatey-center-of-data-deliciousness/
 +  https://github.com/CharityNavigator/990_long
 +  http://irs-990-explorer.chrisgherbert.com/
 +  https://www.grantmakers.io/
 +  https://lecy.github.io/Open-Data-for-Nonprofit-Research/
 +  http://nonprofitlight.com/
 +  https://x4i.org/nonprofit-ntee-code-finder
 +  https://github.com/ma-ji/npo_classifier
 +  https://www.open990.com/
 +  https://github.com/jsfenfen/990-xml-reader
 
# Notes

Note that this repo does NOT contain the NIH or IRS data files themselves, since they are a bit large, and can be obtained elsewhere. Other public data referenced here includes:

  * [Connecticut SotS database of businesses](https://service.ct.gov/business/s/onlinebusinesssearch?businessName=haskins%20laboratories)
  * Public Employee salaries at  [Open Payrolls](https://openpayrolls.com/)
  * [US Bureau of Labor Statistics consumer price index data](https://download.bls.gov/pub/time.series/cu/cu.series)

