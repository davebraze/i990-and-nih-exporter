# README

The code in this repository is an exercise in working with public data from two main sources. The first source is IRS form 990, "Return of Organization Exempt from Income Tax." The 990 is an informational form that non-profit organizations (NPO) must file with the Internal Revenue Service (IRS) each year. The completed forms are publicly available records as a matter of federal law. The second source of information used here is details of individual grants from National Institutes of Health (NIH). The NIH Exporter gives access to information about all individual grants made by the NIH. That information includes the direct and indirect moneys paid to grantees each year for each grant. 

NIH grant data can be downloaded from the [NIH Exporter web tool](https://exporter.nih.gov/). The data files are available for each fiscal year since 1985 in either CSV or XML format. A convenient place to get IRS 990s is from the [ProPublica Nonprofit Explorer](https://projects.propublica.org/nonprofits/). It is necessary to search for the particular NPOs you are interested in, and individually download form 990s for each tax year of interest. The form for a specific NPO/tax-year will be available either as a scanned PDF, if it was filed as a paper document, or as an XML file, if it was filed electronically. All NPOs are required to file electronically from 2019 forward. 

Note that this repo does NOT contain the NIH or IRS data files themselves, since they are a bit large, and can be obtained elsewhere. Other public data referenced here includes:

  * [Connecticut SotS database of businesses](https://service.ct.gov/business/s/onlinebusinesssearch?businessName=haskins%20laboratories)
  * Public Employee salaries at  [Open Payrolls](https://openpayrolls.com/)
  * [US Bureau of Labor Statistics consumer price index data](https://download.bls.gov/pub/time.series/cu/cu.series)
