## revenue, expenses, and assets by year

rea.cap <- "Data is taken from IRS form 990s. 'Revenue' is the total revenue line from the 990; 'Expenses is' total expenses; 'Net Assets' is total assets minus total liabilities."

labels.rea <-
    D_financials %>%
    filter(year == 2019) %>%
    select(year, revenue_total, expenses_total, net_assets) %>%
    rename(`Revenue` = revenue_total, `Expenses` = expenses_total, `Net Assets` = net_assets) %>%
    pivot_longer(cols = -1)

rea.fig <-
    D_financials %>%
    select(year, revenue_total, expenses_total, net_assets) %>%
    pivot_longer(cols=c(revenue_total, expenses_total, net_assets)) %>%
    mutate(name = fct_relevel(name, "revenue_total", "expenses_total", "net_assets")) %>%
    filter(year %in% 2014:2019) %>%
    ggplot() +
    geom_line(aes(x = year, y = value, color = name),
              alpha=2/3,
              show.legend=FALSE,
              size=size*.1) +
    scale_color_manual(values=c("dark green", "dark red", "dark orange", "blue")) +
    scale_y_continuous(limits = c(0,NA),
                       labels = ~.x/10^6) +
    scale_x_continuous(breaks = ~seq(ceiling(.x[1]), floor(.x[2]))) +
    labs(title = "Haskins Laboratories",
         subtitle = "Expenses, Revenue, & Net Assets",
         y = "Dollars (millions)",
         x = "Tax Year") +
    geom_text_repel(data = labels.rea,
                    aes(label = name, x = year, y = value-150000),
                    segment.size = .25,
                    size = size*.2,
                    hjust = -0,
                    direction = "y",
                    force = 1/4,
                    nudge_x = 5,
                    nudge_y = 5) +
    theme_fdbplot() +
    background_grid(major = c("y"),
                    minor = c("y"))

## revenue, expenses, and assets: adjusted for CPI and unadjusted in one figure

reacpi.cap <- "Data is taken from IRS form 990s. Solid lines reflect reported dollar values. Dashed lines are adjusted for inflation to equivalent 2014 purchasing power (using CPI data). 'Revenue' is the total revenue line from the 990; 'Expenses' is total expenses; 'Net Assets' is total assets minus total liabilities."

labels.reacpi <-
    D_financials %>%
    filter(year == 2019) %>%
    select(year, revenue_total, expenses_total, net_assets) %>%
    rename(`Revenue` = revenue_total, `Expenses` = expenses_total, `Net Assets` = net_assets) %>%
    pivot_longer(cols = -1)

reacpi.fig <-
    D_financials %>%
    select(year,
           revenue_total, expenses_total, net_assets,
            revenue_total_adj, expenses_total_adj, net_assets_adj) %>%
    pivot_longer(cols=c(revenue_total, expenses_total, net_assets,
                        revenue_total_adj, expenses_total_adj, net_assets_adj)) %>%
    mutate(cpi_adj = str_detect(name, "_adj"),
           name = fct_relevel(name, "revenue_total", "expenses_total", "net_assets"),
           name = fct_recode(name,
                             revenue_total = "revenue_total_adj",
                             expenses_total = "expenses_total_adj",
                             net_assets = "net_assets_adj")) %>%
    filter(year %in% 2014:2019) %>%
    ggplot() +
    geom_line(aes(x = year, y = value, color = name, linetype=cpi_adj),
              alpha=2/3,
              show.legend=FALSE,
              size=size*.1) +
    scale_color_manual(values=c("dark green", "dark red", "dark orange", "blue")) +
    scale_y_continuous(limits = c(0,NA),
                       labels = ~.x/10^6) +
    scale_x_continuous(breaks = ~seq(ceiling(.x[1]), floor(.x[2]))) +
    labs(title = "Haskins Laboratories",
         subtitle = "Expenses, Revenue, & Net Assets with and without adjustment for inflation",
         y = "Dollars (millions)",
         x = "Tax Year") +
    geom_text_repel(data = labels.reacpi,
                    aes(label = name, x = year, y = value-150000),
                    segment.size = .25,
                    size = size*.2,
                    hjust = -0,
                    direction = "y",
                    force = 1/4,
                    nudge_x = 5,
                    nudge_y = 5) +
    theme_fdbplot() +
    background_grid(major = c("y"),
                    minor = c("y"),
                    size.major = size*.075)


## revenue, expenses, and assets plus NIH total costs

reanih.cap <- "Revenue, expense, and asset figures are taken from IRS form 990s. NIH Total Costs comes from NIH Exporter. 'Revenue' is the total revenue line from the 990; 'Expenses is total expenses; 'Net Assets' is total assets minus total liabilities. 'NIH Total Costs' is direct + indirect costs for all NIH grants where Haskins is the primary grantee (subawards to HL are not included)."

label990 <-
    D_financials %>%
    filter(year == 2019) %>%
    select(year, revenue_total, expenses_total, net_assets) %>%
    rename(`Revenue` = revenue_total, `Expenses` = expenses_total, `Net Assets` = net_assets) %>%
    pivot_longer(cols = -1)

labelnih <-
    D_financials %>%
    filter(year == 2020) %>%
    select(year, nih_total_cost) %>%
    rename(`NIH Total Costs` = nih_total_cost) %>%
    pivot_longer(cols = -1)

reanih.fig <-
    D_financials %>%
    select(year, revenue_total, expenses_total, net_assets, nih_total_cost) %>%
    pivot_longer(cols=c(revenue_total, expenses_total, net_assets, nih_total_cost)) %>%
    mutate(name = fct_relevel(name, "revenue_total", "expenses_total", "net_assets", "nih_total_cost")) %>%
    filter(year %in% 2013:2020) %>%
    ggplot() +
    geom_line(aes(x = year, y = value, color = name),
              alpha=2/3,
              show.legend=FALSE,
              size=size*.1) +
    scale_color_manual(values=c("dark green", "dark red", "dark orange", "blue")) +
    scale_y_continuous(limits = c(0,NA),
                       labels = ~.x/10^6) +
    scale_x_continuous(breaks = ~seq(ceiling(.x[1]), floor(.x[2]))) +
    labs(title = "Haskins Laboratories",
         subtitle = "Expenses, Revenue, Net Assets, & NIH Total Costs",
         y = "Dollars (millions)",
         x = "Tax Year") +
    geom_text_repel(data = label990,
                    aes(label = name, x = year, y = value),
                    segment.size = .25,
                    size = size*.2,
                    hjust = 0,
                    nudge_x = .1,
                    nudge_y = 5) +
    geom_text_repel(data=labelnih,
                    aes(label = name, x = year, y = value-150000),
                    segment.size = .25,
                    size = size*.2,
                    hjust = 0,
                    nudge_x = .1,
                    nudge_y = 5) +
    theme_fdbplot() +
    background_grid(major = c("y"),
                    minor = c("y"),
                    size.major = size*.075)

## Revenue breakdown by category

revenue.cap <- "'NIH Total Cost' is is direct + indirect funds for all NIH grants where Haskins is the primary grantee (subawards to HL are not included); 'Other Gov. Grants' is income from other government grants (including, I think, NIH subawards); other categories are self explanatory."

D_revenue <-
    D_financials %>%
    select(starts_with("revenue_"),
           nih_total_cost,
           year,
           -revenue_total_adj,
           -revenue_contributions_and_grants) %>%
    filter(year %in% 2014:2019) %>%
    rename(Contributions = revenue_contributions_cash_other,
           gov_grants = revenue_contributions_government_grants,
           Investments = revenue_investment_income,
           Other = revenue_other,
           `NIH Total Cost` = nih_total_cost) %>%
    mutate_all(~na_if(.,0)) %>%
    mutate(`Other Gov. Grants` = gov_grants - `NIH Total Cost`) %>%
    janitor::remove_empty() %>%
    pivot_longer(!c(gov_grants, revenue_total, year), names_to = "Revenue Type", values_to ="Dollars") %>%
    mutate(`Revenue Type` = fct_relevel(`Revenue Type`, "Other", "Investments", "Contributions", "Other Gov. Grants", "NIH Total Cost"))

revenue.fig <-
    D_revenue %>%
    ggplot(aes(x = year)) +
    geom_bar(aes(fill=`Revenue Type`, y=Dollars),
             position="stack", stat="identity",
             alpha=2/3) +
    scale_y_continuous(limits = c(0,NA),
                       labels = ~.x/10^6) +
    scale_x_continuous(breaks = ~seq(ceiling(.x[1]), floor(.x[2]))) +
    scale_fill_viridis_d(option="H") +
    labs(title = "Haskins Laboratories",
         subtitle = "Revenue Categories by Year",
         y = "Dollars (millions)",
         x = "Tax Year") +
    theme_fdbplot() +
    background_grid(major = c("y"),
                    minor = c("y"),
                    size.major = size*.075)

## Expense breakdown by category

expense.cap <- "Four expense categories are in use by Haskins Laboratories over this six year period: Grants to Organizations, Other Grants, Other Expenses, and Salaries."

D_expenses <-
    D_financials %>%
    select(starts_with("expenses_"),
           -expenses_total_adj,
           year) %>%
    filter(year %in% 2014:2019) %>%
    mutate_all(~na_if(.,0)) %>%
    janitor::remove_empty() %>%
    mutate(`Other Grants` = expenses_grants_total - expenses_grants_to_orgs) %>%
    select(-expenses_grants_total) %>%
    rename(`Grants to Orgs` = expenses_grants_to_orgs,
           Salaries = expenses_salaries,
           `Other Expenses` = expenses_other) %>%
    pivot_longer(!c(expenses_total, year), names_to = "Expense Type", values_to ="Dollars") %>%
    mutate(`Expense Type` = fct_relevel(`Expense Type`, "Grants to Orgs", "Other Grants", "Other Expenses", "Salaries"))

expense.fig <-
    D_expenses %>%
    ggplot(aes(x = year)) +
    geom_bar(aes(fill=`Expense Type`, y=Dollars),
             position="stack", stat="identity",
             alpha=2/3) +
    scale_y_continuous(limits = c(0,NA),
                       labels = ~.x/10^6) +
    scale_x_continuous(breaks = ~seq(ceiling(.x[1]), floor(.x[2]))) +
    scale_fill_viridis_d(option="H") +
    labs(title = "Haskins Laboratories",
         subtitle = "Expense Categories by Year",
         y = "Dollars (millions)",
         x = "Tax Year") +
    theme_fdbplot() +
    background_grid(major = c("y"),
                    minor = c("y"),
                    size.major = size*.075)

######### i990 part VII

## Up to this point, I've been pulling i990 data with the ParseIRS990 package. But, I've discovered it has significant limitations and so, from this point on, I am using some hand-rolled code to pull information from xml formatted i990s. See functions in the file '99-xml2-convenience.R'

## USA-based grantees
us_grantees <- map_dfr(xml.990, get_us_grantees)
grant.count <- table(us_grantees$RecipientBusinessName)

us.grantees.fig <-
    us_grantees %>%
    mutate(RecipientBusinessName = recode_factor(RecipientBusinessName,
                                                 `YALE UNIVERSITY` = "YALE",
                                                 `UNIVERSITY OF CALIFORNIA - SAN FRANCISCO` = "UCSF",
                                                 `RESEARCH FOUNDATION OF CUNY` = "CUNY",
                                                 `NEW YORK UNIVERSITY` = "NYU",
                                                 `UNIVERSITY OF SOUTHERN CALIFORNIA` = "USC",
                                                 `UNIVERSITY OF CONNECTICUT` = "UCONN",
                                                 `THE ENDANGERED LANGUAGE FUND` = "ELF",
                                                 `DUPONT HOSPITAL FOR CHILDREN` = "DUPONT HOSP",
                                                 `TEACHERS COLLEGE COLUMBIA UNIVERSITY` = "COLUMBIA U",
                                                 `UNIVERSITY OF IOWA` = "U OF IOWA")) %>%
    mutate(RecipientBusinessName = fct_rev(fct_infreq(RecipientBusinessName))) %>%
    ggplot(aes(x = tax.year)) +
    geom_bar(aes(fill=`RecipientBusinessName`, y=CashGrantAmt),
             position="stack", stat="identity",
             alpha=2/3) +
    scale_y_continuous(limits = c(0,NA),
                       labels = ~.x/10^6) +
    scale_x_continuous(breaks = ~seq(ceiling(.x[1]), floor(.x[2]))) +
    scale_fill_viridis_d(option="H") +
    theme_fdbplot() +
    background_grid(major = c("y"),
                    minor = c("y"),
                    size.major = size*.075) +
    labs(title = "Haskins Laboratories",
         subtitle = "US Based Grantees by Year",
         y = "Dollars (millions)",
         x = "Tax Year",
         fill = "Grant Recipient")

## International grants

intl_grants <- map_dfr(xml.990, get_intl_grants)

intl.grants.fig <-
    intl_grants %>%
    mutate(RegionTxt = fct_relabel(RegionTxt, ~gsub("NORTH AMERICA.*", "NO. AMERICA (NOT USA)", .x))) %>%
    mutate(RegionTxt = fct_relabel(RegionTxt, ~gsub("EUROPE.*", "EUROPE", .x))) %>%
    mutate(RegionTxt = fct_relabel(RegionTxt, ~gsub("MIDDLE EAST.*", "MIDDLE EAST", .x))) %>%
    mutate(RegionTxt = fct_rev(fct_infreq(RegionTxt))) %>%
    ggplot(aes(x = tax.year)) +
    geom_bar(aes(fill=`RegionTxt`, y=CashGrantAmt),
             position="stack", stat="identity",
             alpha=2/3) +
    scale_y_continuous(limits = c(0,NA),
                       labels = ~.x/10^6) +
    scale_x_continuous(breaks = ~seq(ceiling(.x[1]), floor(.x[2]))) +
    scale_fill_viridis_d(option="H") +
    labs(title = "Haskins Laboratories",
         subtitle = "International Grants by Year and Region",
         y = "Dollars (millions)",
         x = "Tax Year",
         fill = "Grant Region") +
    theme_fdbplot() +
    background_grid(major = c("y"),
                    minor = c("y"),
                    size.major = size*.075)


## table of directors over time
key.employees <- map_dfr(xml.990, get_key_employees)
partvii.personnel <- map_dfr(xml.990, get_990_partvii)

director.tbl <-
    partvii.personnel %>%
    filter(IndividualTrusteeOrDirectorInd) %>%
    select(PersonNm, tax.year, TitleTxt)  %>%
    mutate(TitleTxt = fct_recode(TitleTxt,
                                 DIR = "PRESIDENT", ## KP is a director
                                 SEC = "SECRETARY OF THE BOARD",
                                 DIR = "DIRECTOR",
                                 TRE = "TREASURER",
                                 SEC = "SECRETARY",
                                 PCH = "PAST CHAIR",
                                 )) %>%
    group_by(PersonNm) %>%
    mutate(last_year_of_service = max(tax.year)) %>%
    ungroup() %>%
    mutate(PersonNm = fct_reorder(PersonNm, last_year_of_service, .desc=FALSE)) %>%
    arrange(last_year_of_service) %>%
    select(-last_year_of_service) %>%
    pivot_wider(names_from=tax.year, values_from=TitleTxt) %>%
    rename(` ` = PersonNm)

## table of corporate officers (not officers of the board) over time
## default sort order is not ideal
officers.tbl <-
    partvii.personnel %>%
    filter(OfficerInd,
           ## special handling for KRP, who is a director & corp officer, but not an officer of the BOD
           IndividualTrusteeOrDirectorInd == FALSE | TitleTxt == "PRESIDENT") %>%
    select(PersonNm, tax.year, TitleTxt)  %>%
    mutate(TitleTxt = fct_recode(TitleTxt,
                                 PRES = "PRESIDENT",
                                 VPR = "VICE PRESIDENT OF RESEARCH",
                                 VPSO = "VICE PRESIDENT OF SCIENTIFIC OPERATIONS",
                                 VPSO = "VICE PRESIDENT OF SCIENTIFIC OPS",
                                 VPSO = "VICE PRESIDENT OF SCIENTIF",
                                 VPF = "VICE PRESIDENT OF FINANCE",
                                 SEC = "PAST SECRETARY/CORP SECRETARY",
                                 SEC = "PAST SECRETARY/CORP SECRET",
                                 SEC = "CORPORATE SECRETARY",
                                 `CEO,VP` = "CEO AND VICE PRESIDENT",
                                 VP = "PAST CEO AND VICE PRESIDENT"
                                 )) %>%
    group_by(PersonNm) %>%
    mutate(last_year_of_service = max(tax.year)) %>%
    ungroup() %>%
    mutate(PersonNm = fct_reorder(PersonNm, last_year_of_service, .desc=FALSE)) %>%
    arrange(last_year_of_service) %>%
    select(-last_year_of_service) %>%
    pivot_wider(names_from=tax.year, values_from=TitleTxt) %>%
    rename(` ` = PersonNm)


## table of highly compensated employees who are not officers
## default sort order is not ideal
hce.tbl <-
    partvii.personnel %>%  ## glimpse
    filter(HighestCompensatedEmployeeInd) %>%
    mutate(PersonNm = recode_factor(PersonNm,
                                    `EINAR MENCIL` = "EINAR MENCL",
                                    `FORREST D BRAZE` = "DAVID BRAZE")) %>%
    select(PersonNm, tax.year) %>%
    group_by(PersonNm) %>%
    mutate(last_year_of_service = max(tax.year)) %>%
    ungroup() %>%
    mutate(PersonNm = fct_reorder(PersonNm, last_year_of_service, .desc=FALSE)) %>%
    select(-last_year_of_service) %>%
    table()


######### moving back to NIH data

## Lifespans of Grants, sorted by final year of activity within depicted span
grant.activity <-
    D0_nih %>%
    group_by(grant) %>%
    mutate(last_year_to_date = max(fy)) %>%
    ungroup() %>%
    mutate(grant = fct_reorder(grant, last_year_to_date, .desc=TRUE)) %>%
    ggplot(aes(x=fy, y=grant)) +
##    geom_line(stat="identity", size=1, color='black', alpha=1/3) +
    geom_point(stat="identity", size=3, aes(shape=activity, color=activity)) +
    scale_shape_manual(values=LETTERS) +
    scale_x_continuous(breaks = ~seq(ceiling(.x[1])+1, floor(.x[2]), by=5)) +
    scale_color_viridis_d(option="H") +
    labs(title = "Haskins Laboratories",
         subtitle = "Grant Duration and Activity Code",
         y = "Grant ID",
         x = "NIH Fiscal Year") +
    theme_fdbplot() +
    background_grid(major = c("xy"),
                    size.major = size*.05)

## PI grant activity: years in which each primary investigator has grant income
pi.activity <-
    D0_nih %>%
    group_by(pi_names) %>%
    mutate(last_year_active = max(fy)) %>%
    ungroup() %>%
    mutate(pi_names = fct_reorder(pi_names, last_year_active, .desc=TRUE)) %>%
    ggplot(aes(x=fy, y=pi_names)) +
    geom_point(stat="identity", size=2.5, alpha=2/3) +
    scale_x_continuous(breaks = ~seq(ceiling(.x[1])+1, floor(.x[2]), by=5)) +
    scale_color_viridis_d(option="H") +
    labs(title = "Haskins Laboratories",
         subtitle = "Primary Investigator Activity",
         y = "Primary Investigator",
         x = "NIH Fiscal Year") +
    theme_fdbplot() +
    background_grid(major = c("xy"),
                    size.major = size*.05)

## NIH Total Costs per year since 2000 in blue. NIH Direct Costs per year are in grey, which are only provided as disaggregated from total costs since 2012

D2_nih <-
    D0_nih %>%
    filter(fy>1999) %>%
    group_by(fy) %>%
    mutate(total_cost_fy = sum(total_cost),
           direct_cost_fy = sum(direct_cost_amt)) %>%
    ungroup() %>%
    select(fy, total_cost_fy, direct_cost_fy) %>%
    pivot_longer(cols = -1) %>%
    distinct()

labeltc <-
    D2_nih %>%
    filter(fy == 2020) %>%
    mutate(name = fct_recode(name,
                              `Total Costs` = "total_cost_fy",
                              `Direct Costs` = "direct_cost_fy"))

totalcosts.fig <-
    D2_nih %>%
    ggplot() +
    geom_line(aes(x = fy, y = value, color = name),
              alpha=2/3,
              show.legend=FALSE,
              size=size*.1) +
    scale_color_manual(values=c("grey50", "blue")) +
    scale_y_continuous(limits = c(0,NA),
                       labels = ~.x/10^6) +
    scale_x_continuous(breaks = ~seq(ceiling(.x[1])+1, floor(.x[2])-1, by=5)) +
    labs(title = "Haskins Laboratories",
         subtitle = "NIH Total Costs since 2000 & Direct Costs since 2012",
         y = "Dollars (millions)",
         x = "NIH Fiscal Year") +
    geom_text_repel(data=labeltc,
                    aes(label = name, x = fy, y = value-150000),
                    segment.size = .25,
                    size = size*.2,
                    hjust = 0,
                    nudge_x = .1,
                    nudge_y = 5) +
    theme_fdbplot() +
    background_grid(major = c("y"),
                    minor = c("y"),
                    size.major = size*.075)
