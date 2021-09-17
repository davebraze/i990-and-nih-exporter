######### List of Schedules for form 990

## o Schedule A (https://www.irs.gov/instructions/i990sa) is used "to provide the required information about public charity status and public support." The XML node label "IRS990ScheduleA".

## o Schedule B () "Schedule of Contributors."  The XML node label "IRS990ScheduleB".

## o Schedule C (https://www.irs.gov/instructions/i990sc) is used to "furnish additional information on political campaign activities or lobbying activities"

## o Schedule D (https://www.irs.gov/instructions/i990sd) provides info on "reporting for donor advised funds, conservation easements, certain art and museum collections, escrow or custodial accounts or arrangements, endowment funds, and supplemental financial information."  The XML node label "IRS990ScheduleD".

## o Schedule F (https://www.irs.gov/instructions/i990sf) is used "to provide information on its activities conducted outside the United States by the organization at any time during the tax year."  The XML node label "IRS990ScheduleF".

## o Schedule G (https://www.irs.gov/instructions/i990sg) is used "to report professional fundraising services, fundraising events, and gaming."

## o Schedule H (https://www.irs.gov/instructions/i990sh) is used by hospitals "to provide information on the activities and policies of, and community benefit provided by, its hospital facilities and other non-hospital health care facilities "

## o Schedule I () "Grants and Other Assistance to Organizations, Governments and Individuals in the United States."  The XML node label "IRS990ScheduleI".

## o Schedule J (https://www.irs.gov/instructions/i990sj) "to report compensation information for certain officers, directors, individual trustees, key employees, and highest compensated employees, and information on certain compensation practices of the organization."  The XML node label "IRS990ScheduleJ".

## o Schedule K (https://www.irs.gov/instructions/i990sk) "to provide certain information on its outstanding liabilities associated with tax-exempt bond issues. "

## o Schedule L (https://www.irs.gov/instructions/i990sl) "is used by an organization that files Form 990 or 990-EZ to provide information on certain financial transactions or arrangements between the organization and disqualified person(s) under section 4958 or other interested persons."

## o Schedule O () "Supplemental Information to Form 990 or 990-EZ."  The XML node label "IRS990ScheduleO".

## o Schedule R (https://www.irs.gov/instructions/i990sr) "is used by an organization that files Form 990 to provide information on related organizations, on certain transactions with related organizations, and on certain unrelated partnerships through which the organization conducts significant activities."  The XML node label "IRS990ScheduleR".

##### Priorities

## Equivalent of ParseIRS990::get_all_financial_data()

## Extract employee data from 990 Part VII (what's the difference between this and Schedule J?) "Form990PartVIISectionAGrp"

######### xml2::
## Play with the xml2 package to parse 990 xml files downloaded from the Propublica nonprofit explorer.
## ParseIRS990:: is too limited in its functionality, and uses the older XML:: package, and so is incompatible with xml2::

library(xml2)

## read Haskins' 2019 form 990
if (FALSE){
    infile <- here("irs990", "xml-files", "202032449349300723_public.xml")
    xml <- read_xml(infile)
}

##### Get info on grantees contained in Schedule I

## A function to extract info on US-based grantees from Schedule I
get_us_grantees <- function(xml.doc) {
    ## xml.doc is an xml document (IRS 990) sourced from xml2::read_xml()
    x <- xml_find_all(xml.doc, "//d1:RecipientTable")
    names <- x %>% xml_children() %>% xml_name()
    values <- x %>% xml_children() %>% xml_text()
    record.num <- x %>% xml_length() %>% rep(seq_along(.), .)

    xx <- xml_find_all(xml.doc, ".//d1:RecipientTable/d1:USAddress")
    addr.names <- xx %>% xml_children() %>% xml_name()
    addr.values <- xx %>% xml_children() %>% xml_text()
    addr.num <- xx %>% xml_length() %>% rep(seq_along(.), .)

    tax.year <- xml_find_all(xml.doc, "//d1:TaxYr") %>% xml_text() %>% as.integer()

    tibble(tax.year = tax.year, record.num = c(record.num, addr.num),
           names = c(names, addr.names),
           values = c(values, addr.values)) %>%
        pivot_wider(names_from=names, values_from=values) %>%
        mutate(CashGrantAmt = as.integer(CashGrantAmt)) %>%
        select(-USAddress)
}

if (FALSE){
    grantees <-
        xml %>%
        get_us_grantees()
}

##### Get info on Key employees contained in Schedule J

## A function to extract info on US-based grantees from Schedule I
get_key_employees <- function(xml.doc) {
    ## xml.doc is an xml document (IRS 990) sourced from xml2::read_xml()
    x <- xml_find_all(xml.doc, "//d1:RltdOrgOfficerTrstKeyEmplGrp")
    names <- x %>% xml_children() %>% xml_name()
    values <- x %>% xml_children() %>% xml_text()
    record.num <- x %>% xml_length() %>% rep(seq_along(.), .)

    tax.year <- xml_find_all(xml.doc, "//d1:TaxYr") %>% xml_text() %>% as.integer()

    tibble(tax.year = tax.year, record.num = c(record.num),
           names = c(names),
           values = c(values)) %>%
        pivot_wider(names_from=names, values_from=values) %>%
        mutate_at(vars(matches("(Amt|Amount)$")), as.integer)
}

if (FALSE) {
    key.employees <-
        xml %>%
        get_key_employees()
}

##### Get info on activities outside the US, contained in Schedule F
## For now, pull the content of sections tagged "GrantsToOrgOutsideUSGrp"

## A function to extract info on international grant-making from Schedule F
get_intl_grants <- function(xml.doc) {
    ## xml.doc is an xml document (IRS 990) sourced from xml2::read_xml()
    x <- xml_find_all(xml.doc, "//d1:GrantsToOrgOutsideUSGrp")
    names <- x %>% xml_children() %>% xml_name()
    values <- x %>% xml_children() %>% xml_text()
    record.num <- x %>% xml_length() %>% rep(seq_along(.), .)

    tax.year <- xml_find_all(xml.doc, "//d1:TaxYr") %>% xml_text() %>% as.integer()

    tibble(tax.year = tax.year,
           record.num = c(record.num),
           names = c(names),
           values = c(values)) %>%
        pivot_wider(names_from=names, values_from=values) %>%
        mutate(CashGrantAmt = as.integer(CashGrantAmt))
}

if (FALSE) {
    intl.grants <-
        xml %>%
        get_intl_grants()
}

##### Get form 990 Part VII

## A function to extract Form 990 Part VII: employee information

get_990_partvii <- function(xml.doc) {
    ## xml.doc is an xml document (IRS 990) sourced from xml2::read_xml()
    x <- xml_find_all(xml.doc, "//d1:Form990PartVIISectionAGrp")
    names <- x %>% xml_children() %>% xml_name()
    values <- x %>% xml_children() %>% xml_text()
    record.num <- x %>% xml_length() %>% rep(seq_along(.), .)

    tax.year <- xml_find_all(xml.doc, "//d1:TaxYr") %>% xml_text() %>% as.integer()

    tibble(tax.year = tax.year,
           record.num = c(record.num),
           names = c(names),
           values = c(values)) %>%
        pivot_wider(names_from=names, values_from=values) %>%
        mutate_at(vars(matches("(Amt|Amount)$")), as.integer) %>%
        mutate_at(vars(matches("Ind$")), ~if_else(!is.na(.x), TRUE, FALSE)) %>%
        mutate_at(vars(matches("Rt$")), as.numeric)
}

if (FALSE) {
    partiiv <-
        xml[[1]] %>%
        get_990_partiiv()
}

## ##### get nodelist for form 990 and all attached schedules
## xml_structure(xml) ## show the overall structure of the document and node names
## xml_ns(xml) ## get all namespaces in a document; d1 is a default namespace

## ##### Helpful links

## https://lecy.github.io/Open-Data-for-Nonprofit-Research/Quick_Guide_to_XML_in_R.html

## ##### xml2 notes

## xml_parent(xml, "//d1:RecipientTable")
## xml_siblings(xml)
## xml_root(xml)

## xml_name(xml) ## name of the current node
## xml_length(xml) ## number of child elements
## xml_children(xml) ## details about child elements

## xml_child(xml, 1) ## get a specific child element
## xml_child(xml, 2)
## xml_name(xml_child(xml, 1))
## xml_name(xml_child(xml, 2))

## xml_text(xml) ## get text from specified node
## xml_text(xml_child(xml, 1))
## xml_text(xml_child(xml, 2))

## xml_structure(xml) ## show the overall structure of the document and node names
## xml_ns(xml) ## get all namespaces in a document; d1 is a default namespace
## xml_ns_strip(xml) ## remove namespaces from a document

