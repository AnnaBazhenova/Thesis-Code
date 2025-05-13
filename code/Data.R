install.packages("openxlsx")
install.packages("mice")
install.packages("fixest")
install.packages("plm")
install.packages("lmtest")
install.packages("lme4")
install.packages("ordinal")
install.packages("dotwhisker")
install.packages("interactions")
install.packages("ggeffects")
install.packages("modelsummary")
install.packages("stargazer")
install.packages("psych")
install.packages("car")
install.packages("corrplot")
install.packages("factoextra")
install.packages("e1071")
install.packages("gt")
install.packages("ggcorrplot")
library(ggcorrplot)  
library(gt)
library(e1071)
library(factoextra)
library(corrplot)
library(psych)
library(car)
library(stargazer)
library(modelsummary)
library(dotwhisker)
library(ggeffects)
library(interactions)
library(readr)
library(sandwich)
library(dplyr)
library(vroom)
library(tidyr)
library(readxl)
library(mice)
library(fixest)   
library(plm)      
library(lmtest)  
library(lme4)     
library(ordinal)
library(stringr)
library(broom)
library(ggplot2)
library(purrr)


## WWBI ##

#this is a dataset on gov. consumption and wage expenditure growth
data_WWBI <- read_csv("/Users/bazenovaanna/Desktop/Thesis/Data/WWBI_CSV/Government consumption or wage expenditure growth.csv")

#keep only countries and years I need
countries_WWBI <- c("Ukraine", "Hungary", "Romania", "Bulgaria", "Serbia", "Czech Republic", "Slovak Republic", "Poland", "Croatia", "Albania", "Greece", "Moldova")
year_WWBI <- c ("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
filtered_WWBI <- data_WWBI %>%
  filter(`Country Name` %in% countries_WWBI) %>%
  select(`Country Name`, `Indicator Name`, all_of(year_WWBI))

#keep only indicators I need
keep_WWBI <- c(
  "Wage bill as a percentage of GDP",
  "Public sector employment, as a share of total employment",
  "Public sector employment, as a share of formal employment",
  "Public sector wage premium (compared to all private employees)",
  "Public sector wage premium (compared to all formal employees)"
  )
filtered_WWBI <- filtered_WWBI %>%
  filter(`Indicator Name` %in% keep_WWBI)

#long format
long_WWBI <- filtered_WWBI %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Value")

#recoding
long_WWBI2 <- long_WWBI %>%
  mutate(`Country Name` = recode(`Country Name`,
                                 "Czech Republic" = "Czechia",
                                 "Slovak Republic" = "Slovakia"))
wide_WWBI <- long_WWBI2 %>%
  pivot_wider(names_from = `Indicator Name`, values_from = Value)
wide_WWBI <- wide_WWBI %>%
  rename(year = Year,
         country = `Country Name`)
wide_WWBI <- wide_WWBI %>%
  mutate(year = as.numeric(year))


## Clientelism ##

#this is a dataset on clientelism indicators 
data_client <- readRDS("/Users/bazenovaanna/Desktop/Thesis/Data/client/V-Dem-CY-Full+Others-v15.rds")
unique(data_client$country_name)

#keep only countries and years I need
countries_cl <- c("Ukraine", "Hungary", "Romania", "Bulgaria", "Serbia", "Czechia", "Slovakia", "Poland", "Croatia", "Albania", "Greece", "Moldova")
year_cl <- c ("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
filtered_client <- data_client %>%
  filter(country_name %in% countries_cl) %>%
  filter(year %in% year_cl)

#keep only indicators I need
keep_client <- c(
  "country_name", "country_id", "year",
  "v2dlencmps",      # particularistic vs public goods
  "v2psprbrib",      # party linkages - bribery
  "v2psprlnks",      # general clientelism index
  "v2x_corr",
  "v2xnp_client"
)

filtered_client <- filtered_client %>%
  select(any_of(keep_client)) 

#recoding
filtered_client <- filtered_client %>%
  rename(country = country_name)
filtered_client <- filtered_client %>%
  mutate(year = as.numeric(year))

#дать пояснение в чем это выражается  - в кодбуке - 0 - 1 (super clientelistic)


## Unemployment ##

#this is a dataset on unemployment - % - already filtered
data_un <- read_csv("/Users/bazenovaanna/Desktop/Thesis/Data/dataset_2025-04-09T15_31_56.808593646Z_DEFAULT_INTEGRATION_IMF.STA_LS_9.0.0.csv")

#add Moldova - I did not include it at the beginning
moldova_un <- data.frame(
  COUNTRY  = "Moldova",
  INDICATOR = "Unemployment rate",
  `2006`= 7.4,
  `2007`= 5.1,
  `2008`= 4,
  `2009`= 6.4,
  `2010`= 7.4,
  `2011`= 6.7,
  `2012`= 5.6,
  `2013`= 5.1,
  `2014`= 3.9,
  `2015`= 5,
  `2016`= 4.2) 

data_un2 <- bind_rows(data_un, moldova_un)

data_un2 <- data_un2 %>%
  mutate(
    `2006` = coalesce(`2006`, X2006),
    `2007` = coalesce(`2007`, X2007),
    `2008` = coalesce(`2008`, X2008),
    `2009` = coalesce(`2009`, X2009),
    `2010` = coalesce(`2010`, X2010),
    `2011` = coalesce(`2011`, X2011),
    `2012` = coalesce(`2012`, X2012),
    `2013` = coalesce(`2013`, X2013),
    `2014` = coalesce(`2014`, X2014),
    `2015` = coalesce(`2015`, X2015),
    `2016` = coalesce(`2016`, X2016),
    
  ) %>%
  select(-starts_with("X")) 

#long format
filtered_un <- data_un2 %>%
  pivot_longer(
    cols = starts_with("20"),  
    names_to = "year",         
    values_to = "value"     
  )

#recoding
filtered_un2 <- filtered_un %>%
  mutate(COUNTRY = recode(COUNTRY,
                                 "Czech Republic" = "Czechia",
                                 "Slovak Republic" = "Slovakia",
                                 "Croatia, Republic of" = "Croatia",
                                 "Poland, Republic of" = "Poland",
                                 "Serbia, Republic of" = "Serbia"))
wide_un <- filtered_un2 %>%
  pivot_wider(names_from = INDICATOR, values_from = value)
wide_un <- wide_un %>%
  rename(country = COUNTRY)
wide_un <- wide_un %>%
  mutate(year = as.numeric(year))

wide_un <- wide_un %>%
  filter(country != "Montenegro")

#Unemployment rate (%)


## WGI ##

#this is a dataset on world government indicators
data_wgi <- read_excel("/Users/bazenovaanna/Desktop/Thesis/Data/wgi/wgidataset_with_sourcedata.xlsx")

#keep countries and years I need
countries_wgi <- c("Ukraine", "Hungary", "Romania", "Bulgaria", "Serbia", "Czech Republic", "Slovak Republic", "Poland", "Croatia", "Albania", "Greece", "Moldova")
year_wgi <- c ("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
filtered_wgi <- data_wgi %>%
  filter(countryname %in% countries_wgi) %>%
  filter(year %in% year_wgi) %>%
  select(countryname, year, indicator, estimate)

#keep variables I need  
filtered_wgi2 <- filtered_wgi[filtered_wgi$indicator == "ge", ]

#long format
long_wgi <- filtered_wgi2 %>%
  mutate(countryname = recode(countryname,
                                 "Czech Republic" = "Czechia",
                                 "Slovak Republic" = "Slovakia"))
#wide format + recoding
wide_wgi <- long_wgi %>%
  pivot_wider(names_from = indicator, values_from = estimate)
wide_wgi <- wide_wgi %>%
  rename(country = countryname)
wide_wgi <- wide_wgi %>%
  mutate(year = as.numeric(year))

#effectiveness gov  -2.5 (weak) to 2.5 (strong)


## Gov. debt ##

#this is a dataset on general government debt - I created addirional sheet with countries and years I need
data_debt <- read_excel("/Users/bazenovaanna/Desktop/Thesis/Data/GlobalDebtDatabase.xlsx", sheet = "GenGovDebt")
data_debt <- data_debt %>%
  filter(country != "Montenegro")


## IMF help ##

#this is a data set on whether IMF lended resources to coutries expressed as a dummy variable - source IMF - history of lending commitments
data_imf <- read_excel("/Users/bazenovaanna/Desktop/Thesis/Data/IMF_help.xlsx")
data_imf <- data_imf %>%
  filter(country != "Montenegro")


## Expenditure, deficit ##

#this is a dataset on expenditure, deficit 
data_wiiw <- read_csv("/Users/bazenovaanna/Desktop/Thesis/Data/wiiw-annual-database.csv")

#keep years and countries I need
filtered_wiiw <- data_wiiw %>%
  select(Country, Indicator, "_2006", "_2007", "_2008", "_2009", "_2010", "_2011", "_2012", "_2013", "_2014", "_2015", "_2016" )

countries_wiiw <- c("Albania", "Bulgaria", "Croatia", "Czechia",
                  "Hungary", "Serbia", "Ukraine", "Poland", "Romania", 
                  "Slovakia", "Moldova")
filtered_wiiw2 <- filtered_wiiw %>%
  filter(Country %in% countries_wiiw)

#keep variables I need
ind_wiiw <- c ("General government expenditures, total", "General government budget, deficit (-) / surplus (+)")
filtered_wiiw3 <- filtered_wiiw2 %>%
  filter (Indicator %in% ind_wiiw)

#long format + recoding
long_wiiw <- filtered_wiiw3 %>%
  pivot_longer(cols = starts_with("_20"),
               names_to = "Year",
               values_to = "Value")

long_wiiw2 <- long_wiiw %>%
  mutate(Year = recode(Year,
                       "_2006" = "2006",
                       "_2007" = "2007",
                       "_2008" = "2008",
                       "_2009" = "2009",
                       "_2010" = "2010",
                       "_2011" = "2011",
                       "_2012" = "2012",
                       "_2013" = "2013",
                       "_2014" = "2014",
                       "_2015" = "2015",
                       "_2016" = "2016"))

#wide format and recoding
wide_wiiw <- long_wiiw2 %>%
  pivot_wider(names_from = Indicator, values_from = Value)

wide_wiiw <- wide_wiiw %>%
  rename(country = Country,
         year = Year)

wide_wiiw <- wide_wiiw %>%
  mutate(year = as.numeric(year))

#add Greece - data was taken form IMF and European Central Bank data
#there wan no data in wiiw dataset
wide_greece  <- tibble(
  country = rep("Greece",11),
  year = 2006:2016,                
  "General government expenditures, total" = c(45.1167163,	47.06942061, 50.84540544,	54.08442237,	52.9572915,	55.12025585,	54.1483571,	52.46035787,	51.16229209,	51.55840967,	50.27922845),
  "General government budget, deficit (-) / surplus (+)" = c(-1.554, -2.241, -5.434, -10.314, -5.316, -2.833, -3.892, -9.433, 0.243, -2.324, 3.428))

wide_wiiw <- wide_wiiw %>%
  mutate(`General government expenditures, total` =
           as.numeric(`General government expenditures, total`))

wide_wiiw <- wide_wiiw %>%
  mutate(`General government budget, deficit (-) / surplus (+)` =
           as.numeric(`General government budget, deficit (-) / surplus (+)`))

wide_wiiw2 <- bind_rows(wide_wiiw, wide_greece)

#% of GDP


## GDP Growth ##

#gdp growth - GDP per capita growth (annual %)
data_gdp <- read_csv("/Users/bazenovaanna/Desktop/Thesis/Data/2752cf33-3f5f-4904-a4d9-f7b4cda8fe7e_Data.csv")

#long format + recoding
long_gdp <- data_gdp %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "year",
               values_to = "GDP per capita growth (%)")
long_gdp <- long_gdp %>%
  rename(country = "Country Name")

long_gdp <- long_gdp %>%
  mutate(country = recode(country,
                              "Slovak Republic" = "Slovakia"))

long_gdp <- long_gdp %>%
  mutate(year = recode(year,
                       "2006 [YR2006]" = "2006",
                       "2007 [YR2007]" = "2007",
                       "2008 [YR2008]" = "2008",
                       "2009 [YR2009]" = "2009",
                       "2010 [YR2010]" = "2010",
                       "2011 [YR2011]" = "2011",
                       "2012 [YR2012]" = "2012",
                       "2013 [YR2013]" = "2013",
                       "2014 [YR2014]" = "2014",
                       "2015 [YR2015]" = "2015",
                       "2016 [YR2016]" = "2016"))

long_gdp <- long_gdp %>%
  mutate(year = as.numeric(year))
long_gdp <- long_gdp %>%
  filter(country != "Montenegro")

## Subsidies ##

#this is a dataset on subsidies and other transfers (% of expense)
data_sub <- read_csv("/Users/bazenovaanna/Desktop/Thesis/Data/API_GC.XPN.TRFT.ZS_DS2_en_csv_v2_17989.csv",skip = 4)

#keep countries and years I need
countries_sub <- c("Albania", "Bulgaria", "Croatia", "Czechia",
                   "Hungary", "Serbia", "Ukraine", "Poland", "Romania", 
                   "Slovak Republic", "Greece", "Moldova")
filtered_sub <- data_sub %>%
  filter(`Country Name` %in% countries_sub) %>%
  select(`Country Name`, `Country Code`, `Indicator Name`, `Indicator Code`, 
         `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`)

#long format + recoding
long_sub <- filtered_sub %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Value")

long_sub2 <- long_sub %>%
  mutate(`Country Name` = recode(`Country Name`,
                              "Slovak Republic" = "Slovakia"))

#wide format + recoding
wide_sub <- long_sub2 %>%
  pivot_wider(names_from = `Indicator Name`, values_from = Value)

wide_sub <- wide_sub %>%
  rename(country = `Country Name`,
         year = Year)

wide_sub <- wide_sub %>%
  mutate(year = as.numeric(year))

## Elections ##

#this is a dataset on elections
data_el <- read_csv("/Users/bazenovaanna/Desktop/Thesis/Data/DPI2020.csv")

#keep only countries and years I need
countries_el <- c("Ukraine", "Hungary", "Romania", "Bulgaria", "Czech Rep.", "Slovakia", "Poland", "Croatia", "Albania", "Greece", "Moldova")
year_el <- 2006:2016
data_el <- data_el %>%
  filter(countryname %in% countries_el) %>%
  filter(year %in% year_el) %>%
  select(countryname, exelec, legelec, year)

#add Serbia and rename countries
data_el <- data_el %>%
  mutate(countryname = recode(countryname,
                          "Czech Rep." = "Czechia"))
data_el <- data_el %>%
  mutate(election_y = ifelse(exelec == 1 | legelec == 1, 1, 0))

serbia_elections <- data.frame(
  countryname = "Serbia",
  year = c(2007, 2008, 2012, 2014, 2016),
  election_y = 1
)

serbia_years <- data.frame(
  countryname = "Serbia",
  year = 2006:2016
)

serbia_election_panel <- serbia_years %>%
  left_join(serbia_elections, by = c("countryname", "year")) %>%
  mutate(election_y = ifelse(is.na(election_y), 0, election_y))

data_el2 <- bind_rows(data_el, serbia_election_panel)

data_el2 <- data_el2 %>%
  select(-exelec, -legelec)

data_el2 <- data_el2 %>%
  rename(country = countryname)

## Inflation ## IMF
data_infl <- read_excel("/Users/bazenovaanna/Desktop/Thesis/Data/imf-dm-export-20250506.xls")

#keep countries and years I need
long_infl <- data_infl %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"),
               names_to = "year",
               values_to = "inflation") 
long_infl <- long_infl %>%
  rename(country = `Inflation rate, average consumer prices (Annual percent change)`)  
countries_infl <- c("Ukraine", "Hungary", "Romania", "Bulgaria", "Serbia", "Czech Republic", "Slovak Republic", "Poland", "Croatia", "Albania", "Greece", "Moldova")
year_infl <- c ("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")

filtered_infl <- long_infl %>%
  filter( country %in% countries_infl, year %in% year_infl)

#recoding names 
filtered_infl <- filtered_infl %>%
  mutate(country = recode(country,
                                 "Czech Republic" = "Czechia",
                                 "Slovak Republic" = "Slovakia"))

filtered_infl$year <- as.numeric(filtered_infl$year)


## Merging ##

#merging datasets
whole_panel <- reduce(
  list(wide_WWBI, filtered_client, wide_un, wide_wgi, wide_wiiw2, wide_sub, data_debt, data_imf, long_gdp, data_el2, filtered_infl), 
  full_join, 
  by = c("country", "year")
)

#keep columns I need
panel_selected <- whole_panel %>%
  select(country, year, 
         "Wage bill as a percentage of GDP", v2dlencmps,
          v2psprlnks, v2x_corr, "Unemployment rate",
         ge, "General government expenditures, total", "General government budget, deficit (-) / surplus (+)", "Subsidies and other transfers (% of expense)",
         "gengovdebt (% of GDP)", "imf", "GDP per capita growth (%)", election_y, v2xnp_client, inflation)


# imputation for Albania subsidies 
albania_data <- subset(panel_selected, country == "Albania")
md.pattern(albania_data)
imputed <- mice(albania_data, m = 5, method = 'pmm', seed = 123)
summary(imputed)
albania_complete <- complete(imputed, 1)
no_albania <- subset(panel_selected, country != "Albania")
panel_updated <- rbind(no_albania, albania_complete)
panel_updated <- panel_updated[order(panel_updated$country,panel_updated$year), ]

#imputation for Ukraine wage bill
ukraine_data <- subset(panel_updated, country == "Ukraine")
md.pattern(ukraine_data)
imputed_ukraine <- mice(ukraine_data, m = 5, method = "pmm", seed = 123)
ukraine_complete <- complete(imputed_ukraine, 1)
no_ukraine <- subset(panel_updated, country != "Ukraine")
panel_updated2 <- rbind(no_ukraine, ukraine_complete)
panel_updated2 <- panel_updated2[order(panel_updated2$country,panel_updated2$year), ]

#numeric variable
panel_updated2$ge <- as.numeric(panel_updated2$ge)


#Renaming columns for convenience
names(panel_updated2)[names(panel_updated2) %in% c("Wage bill as a percentage of GDP", "Unemployment rate",
                                                   "General government expenditures, total", "General government budget, deficit (-) / surplus (+)",
                                                   "Subsidies and other transfers (% of expense)", "gengovdebt (% of GDP)", "GDP per capita growth (%)")] <- c("wage_gdp", "unemp",
                                                                                                                                                               "expend", "deficit", "subsidy", "debt", "gdpgrowth")
panel_updated2 <- panel_updated2 %>%
  mutate(inflation = as.numeric(inflation),
         inflation = round(inflation, 3))

#now I need to transform and create some variables for following regressions 
#crisis dummy
panel_updated2$crisisperiod <- ifelse(panel_updated2$year >= 2009 & panel_updated2$year <= 2010, 1, 0)


#scale and center
#scaling to a 0–1 range
panel_updated2$v2psprlnks_scaled <- (panel_updated2$v2psprlnks - min(panel_updated2$v2psprlnks, na.rm = TRUE)) /
  (max(panel_updated2$v2psprlnks, na.rm = TRUE) - min(panel_updated2$v2psprlnks, na.rm = TRUE))
#standardizing (mean = 0, SD = 1)
panel_updated2$v2psprlnks_standardized <- scale(panel_updated2$v2psprlnks, center = TRUE, scale = TRUE)
#centering - create a deviation variable (subtract the mean)
panel_updated2$clientelism_deviation <- panel_updated2$v2psprlnks - mean(panel_updated2$v2psprlnks, na.rm = TRUE)

#scaling to a 0–1 range
panel_updated2$v2dlencmps_scaled <- (panel_updated2$v2dlencmps - min(panel_updated2$v2dlencmps, na.rm = TRUE)) /
  (max(panel_updated2$v2dlencmps, na.rm = TRUE) - min(panel_updated2$v2dlencmps, na.rm = TRUE))
#standardizing (mean = 0, SD = 1)
panel_updated2$v2dlencmps_standardized <- scale(panel_updated2$v2dlencmps, center = TRUE, scale = TRUE)
#centering - create a deviation variable (subtract the mean)
panel_updated2$v2dlencmps_deviation <- panel_updated2$v2dlencmps - mean(panel_updated2$v2dlencmps, na.rm = TRUE)

panel_updated2 <- panel_updated2 %>%
  rename(
    clientelism_raw = v2psprlnks,
    clientelism_alt = v2xnp_client,
    corruption = v2x_corr,
    particularism = v2dlencmps,
    crisis = crisisperiod
  )

#collinearity check of control variables
vars_to_check <- panel_updated2 %>%
  select(clientelism_alt, crisis, election_y, gdpgrowth, unemp, inflation, corruption, ge, imf)
cor_matrix <- cor(vars_to_check, use = "complete.obs")
round(cor_matrix, 2)
corrplot(cor_matrix, method = "circle", type = "upper",
         tl.col = "black", tl.srt = 45)

## Models and Visualizations ##


#Arellano–Bond Dynamic Panel (GMM) (lagged wage)
gmm_diff_3 <- pgmm(
  wage_gdp ~ clientelism_alt + crisis |
    lag(wage_gdp, 3:4),               
  data = panel_updated2,
  index = c("country", "year"),
  effect = "individual",
  model = "onestep",
  transformation = "d",            
  collapse = TRUE
)

summary (gmm_diff_3)

#robustness check
gmm_diff_4 <- pgmm(
  wage_gdp ~ clientelism_alt + crisis |
    lag(wage_gdp, 4:5),               
  data = panel_updated2,
  index = c("country", "year"),
  effect = "individual",
  model = "onestep",
  transformation = "d",            
  collapse = TRUE
)

summary (gmm_diff_4)
# report with 3:4 - and add another one as a robustness check


#Arellano–Bond Dynamic Panel (GMM) (lagged subsidies)
gmm_subsidy_3 <- pgmm(
  subsidy ~ clientelism_alt + crisis |
    lag(subsidy, 3:4),
  data = panel_updated2,
  index = c("country", "year"),
  effect = "individual",
  model = "onestep",
  transformation = "d",
  collapse = TRUE
)

summary(gmm_subsidy_3)

#robustness check
gmm_subsidy_4 <- pgmm(
  subsidy ~ clientelism_alt + crisis |
    lag(subsidy, 4:5),
  data = panel_updated2,
  index = c("country", "year"),
  effect = "individual",
  model = "onestep",
  transformation = "d",
  collapse = TRUE
)

summary(gmm_subsidy_4)
# report with 3:4 - and add another one as a robustness check

#Arellano–Bond Dynamic Panel (GMM) (lagged deficit)
gmm_deficit_3 <- pgmm(
  deficit ~ clientelism_alt + crisis |
    lag(deficit, 3:4),
  data = panel_updated2,
  index = c("country", "year"),
  effect = "individual",
  model = "onestep",
  transformation = "d",
  collapse = TRUE
)

summary(gmm_deficit_3)

#robustness check
gmm_deficit_4 <- pgmm(
  deficit ~ clientelism_alt + crisis |
    lag(deficit, 4:5),
  data = panel_updated2,
  index = c("country", "year"),
  effect = "individual",
  model = "onestep",
  transformation = "d",
  collapse = TRUE
)

summary(gmm_deficit_4)
# report with 3:4 - and add another one as a robustness check

#interaction wages
gmm_diff_3_int <- pgmm(
  wage_gdp ~ clientelism_alt * crisis |      
    lag(wage_gdp, 3:4),
  data           = panel_updated2,
  index          = c("country", "year"),
  effect         = "individual",
  model          = "onestep",
  transformation = "d",
  collapse       = TRUE
)
summary(gmm_diff_3_int)

#interaction subsidy
gmm_subsidy_3_int <- pgmm(
  subsidy ~ clientelism_alt * crisis |
    lag(subsidy, 3:4),
  data           = panel_updated2,
  index          = c("country", "year"),
  effect         = "individual",
  model          = "onestep",
  transformation = "d",
  collapse       = TRUE
)
summary(gmm_subsidy_3_int)


#interaction deficit
gmm_deficit_3_int <- pgmm(
  deficit ~ clientelism_alt * crisis |
    lag(deficit, 3:4),
  data           = panel_updated2,
  index          = c("country", "year"),
  effect         = "individual",
  model          = "onestep",
  transformation = "d",
  collapse       = TRUE
)
summary(gmm_deficit_3_int)

#visualization of GMM
gmm_list <- list(
  Wage_Main  = gmm_diff_3,
  Wage_Int   = gmm_diff_3_int,
  Subsidy_Main  = gmm_subsidy_3,
  Subsidy_Int   = gmm_subsidy_3_int,
  Deficit_Main  = gmm_deficit_3,
  Deficit_Int   = gmm_deficit_3_int
)

tidy_gmm <- function(model, model_name) {
  V    <- vcov(model)                  # exact covariance matrix
  beta <- coef(model)                  # named vector of estimates
  se   <- sqrt(diag(V))                # exact SEs
  
  df <- data.frame(
    model    = model_name,
    term     = names(beta),
    estimate = as.numeric(beta),
    conf.low = beta - 1.96*se,
    conf.high= beta + 1.96*se,
    row.names = NULL
  )

  df[df$term %in% c("clientelism_alt", "crisis", "clientelism_alt:crisis"), ]
}

all_coefs <- imap_dfr(gmm_list, ~ tidy_gmm(.x, .y))

all_coefs <- transform(all_coefs,
                       term = factor(term,
                                     levels = c("clientelism_alt", "crisis", "clientelism_alt:crisis"),
                                     labels = c("Clientelism","Crisis","Clientelism × Crisis")),
                       outcome = sub("_.*","", model),            # "Wage", "Subsidy", "Deficit"
                       spec    = ifelse(grepl("_Int$",model),"WithInt","Main")
)

gmm_plot<- ggplot(all_coefs, aes(x=estimate, y=term, color=interaction(outcome,spec), shape=spec)) +
  geom_vline(xintercept=0, linetype="dashed", color="grey40") +
  geom_point(position=position_dodge(width=0.6), size=3) +
  geom_errorbarh(aes(xmin=conf.low,xmax=conf.high),
                 height=0.2, position=position_dodge(width=0.6)) +
  scale_color_manual(values=c(
    "Wage.Main"       = "#d35400", "Wage.WithInt"    = "#e67e22",
    "Subsidy.Main"    = "#2ca02c", "Subsidy.WithInt" = "#27ae60",
    "Deficit.Main"    = "#f1c40f", "Deficit.WithInt" = "#f39c12"
  )) +
  scale_shape_manual(values=c("Main"=16,"WithInt"=17)) +
  labs(
    title="GMM: Main vs. Interaction Specs",
    subtitle="95% CIs from one-step Difference GMM (exact SEs)",
    x="Coefficient", y=NULL, color="Outcome & Spec", shape="Model"
  ) +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom", plot.title=element_text(face="bold"))

print(gmm_plot)


ggsave("gmm_results_plot.png", width = 10, height = 6, dpi = 300)


#Short-termism 
pca_data <- panel_updated2 %>%
  select(wage_gdp, subsidy, deficit) %>%
  scale()
pca_result <- principal(pca_data, nfactors = 1, rotate = "none")
panel_updated2$short_termism_index <- as.numeric(pca_result$scores)

model_fe_short <- plm(
  short_termism_index ~ clientelism_alt * crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "within"
)
summary(model_fe_short)

coeftest(model_fe_short, vcov = vcovSCC(model_fe_short, type = "HC1", maxlag = 2))

#visualization of PCA 
se_model_fe_short <- coeftest(model_fe_short, vcov = vcovSCC(model_fe_short, type = "HC1", maxlag = 2))[, 2]
stargazer(model_fe_short,
          type = "html",
          out = "short_termism_model.html",
          title = "Regression Results: Short-Termism Index",
          dep.var.labels = "Short-Termism Index (PCA)",
          covariate.labels = c("Clientelism", "Crisis", "GDP Growth", "Unemployment", "Gov. Effectiveness",
                               "Clientelism × Crisis"),
          se = list(se_model_fe_short),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.",
                    "* p < 0.1, ** p < 0.05, *** p < 0.01"))


# Create new prediction data
clientelism_seq <- seq(min(panel_updated2$clientelism_alt, na.rm = TRUE),
                       max(panel_updated2$clientelism_alt, na.rm = TRUE),
                       length.out = 100)

newdata <- data.frame(
  clientelism_alt = rep(clientelism_seq, 2),
  crisis = rep(c(0, 1), each = 100),
  gdpgrowth = mean(panel_updated2$gdpgrowth, na.rm = TRUE),
  unemp = mean(panel_updated2$unemp, na.rm = TRUE),
  ge = mean(panel_updated2$ge, na.rm = TRUE)
)

# Predict from fixed-effects model
X <- model.matrix(~ clientelism_alt * crisis + gdpgrowth + unemp + ge - 1, data = newdata)
beta <- coef(model_fe_short)
newdata$fit <- as.vector(X %*% beta)

png("short_termism_visualization.png", width = 1800, height = 1200, res = 180)

ggplot(newdata, aes(x = clientelism_alt, y = fit, color = factor(crisis))) +
  geom_line(size = 1.6) +
  labs(title = "Predicted Short-Termism by Clientelism and Crisis",
       x = "Clientelism Index",
       y = "Predicted Short-Termism",
       color = "Crisis") +
  scale_color_manual(labels = c("No Crisis", "Crisis"),
                     values = c("#196f3d", "#b03a2e")) +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right")

dev.off()



#classifying clientelism
panel_updated2 <- panel_updated2 %>%
  mutate(clientelism_group = cut(clientelism_alt,
                                 breaks = quantile(clientelism_alt, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                                 labels = c("low", "medium", "high"),
                                 include.lowest = TRUE))

panel_updated2$clientelism_group <- factor(panel_updated2$clientelism_group,
                                           levels = c("low", "medium", "high"))




# baseline model without interaction - wages
model_baseline_A <- plm(
  wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp,
  data = panel_updated2,
  index = c("country", "year"),
  model = "within",
  effect = "twoways"
)
summary(model_baseline_A)

coeftest(model_baseline_A, vcov = vcovSCC(model_baseline_A, type = "HC1", maxlag = 2))
bptest(model_baseline_A) #bad - using Kraay
pwartest(model_baseline_A) #bad - using Kraay
pcdtest(model_baseline_A, test = "cd") #on the edge - using Kraay
lm_baseline_A <- lm(wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp, data = panel_updated2)
vif(lm_baseline_A) #all good

model_re_A <- plm(wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp,
                data = panel_updated2, index = c("country", "year"), model = "random")
summary(model_re_A)

phtest(model_baseline_A, model_re_A) #RE is also good
#will keep RE model because of the fact that the client as a factor almost do not change across years in a country and FE will drop it 
coeftest(model_re_A, vcov = vcovSCC(model_re_A, type = "HC1", maxlag = 2))


# adding additional deficit to model
model_re_B<- plm(
  wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp + deficit,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_B)
coeftest(model_re_B, vcov = vcovSCC(model_re_B, type = "HC1", maxlag = 2))

# adding additional ge to model
model_re_C<- plm(
  wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp + deficit + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_C)
coeftest(model_re_C, vcov = vcovSCC(model_re_C, type = "HC1", maxlag = 2))

# adding interaction
model_re_D <- plm(
  wage_gdp ~ clientelism_group * crisis + gdpgrowth + unemp + deficit + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_D)

coeftest(model_re_D, vcov = vcovSCC(model_re_D, type = "HC1", maxlag = 2))


# visualization 
se_model_re_A <- sqrt(diag(vcovSCC(model_re_A, type = "HC1", maxlag = 2)))
se_model_re_B <- sqrt(diag(vcovSCC(model_re_B, type = "HC1", maxlag = 2)))
se_model_re_C <- sqrt(diag(vcovSCC(model_re_C, type = "HC1", maxlag = 2)))
se_model_re_D <- sqrt(diag(vcovSCC(model_re_D, type = "HC1", maxlag = 2)))

stargazer(model_re_A, model_re_B, model_re_C, model_re_D,
          type = "html",
          out = "Regression_Wage.html",
          title = "Regression Results: Wage Spending and Clientelism",
          dep.var.labels = "Wage Bill (% of GDP)",
          column.labels = c("Baseline", "+ Deficit", "+ GE", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", "Deficit", 
                               "Government Effectiveness", 
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_A, se_model_re_B, se_model_re_C, se_model_re_D),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))



# baseline model without interaction - subsidies
model_re_E<- plm(
  subsidy ~ clientelism_group + crisis + gdpgrowth + unemp,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_E)
coeftest(model_re_E, vcov = vcovSCC(model_re_E, type = "HC1", maxlag = 2))

# adding additional deficit to model
model_re_F<- plm(
  subsidy ~ clientelism_group + crisis + gdpgrowth + unemp + deficit,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_F)
coeftest(model_re_F, vcov = vcovSCC(model_re_F, type = "HC1", maxlag = 2))

# adding additional ge to model
model_re_G<- plm(
  subsidy ~ clientelism_group + crisis + gdpgrowth + unemp + deficit + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_G)
coeftest(model_re_G, vcov = vcovSCC(model_re_G, type = "HC1", maxlag = 2))

# adding interaction
model_re_H <- plm(
  subsidy ~ clientelism_group * crisis + gdpgrowth + unemp + deficit + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_H)

coeftest(model_re_H, vcov = vcovSCC(model_re_H, type = "HC1", maxlag = 2))

# visualization
se_model_re_E <- sqrt(diag(vcovSCC(model_re_E, type = "HC1", maxlag = 2)))
se_model_re_F <- sqrt(diag(vcovSCC(model_re_F, type = "HC1", maxlag = 2)))
se_model_re_G <- sqrt(diag(vcovSCC(model_re_G, type = "HC1", maxlag = 2)))
se_model_re_H <- sqrt(diag(vcovSCC(model_re_H, type = "HC1", maxlag = 2)))

stargazer(model_re_E, model_re_F, model_re_G, model_re_H,
          type = "html",
          out = "Regression_Subsidy.html",
          title = "Regression Results: Subsidy Spending and Clientelism",
          dep.var.labels = "Subsidy Spending (% of GDP)",
          column.labels = c("Baseline", "+ Deficit", "+ GE", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", "Deficit", 
                               "Government Effectiveness", 
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_E, se_model_re_F, se_model_re_G, se_model_re_H),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))


# baseline model without interaction - deficit
model_re_I<- plm(
  deficit ~ clientelism_group + crisis + gdpgrowth + unemp,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_I)
coeftest(model_re_I, vcov = vcovSCC(model_re_I, type = "HC1", maxlag = 2))

# adding additional ge to model
model_re_J<- plm(
  deficit ~ clientelism_group + crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_J)
coeftest(model_re_J, vcov = vcovSCC(model_re_J, type = "HC1", maxlag = 2))

# adding interaction + added election year
model_re_K <- plm(
  deficit ~ clientelism_group * crisis + gdpgrowth + unemp + ge + election_y,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_K)

coeftest(model_re_K, vcov = vcovSCC(model_re_K, type = "HC1", maxlag = 2))

# visualization
se_model_re_I <- sqrt(diag(vcovSCC(model_re_I, type = "HC1", maxlag = 2)))
se_model_re_J <- sqrt(diag(vcovSCC(model_re_J, type = "HC1", maxlag = 2)))
se_model_re_K <- sqrt(diag(vcovSCC(model_re_K, type = "HC1", maxlag = 2)))


stargazer(model_re_I, model_re_J, model_re_K, 
          type = "html",
          out = "Regression_Deficit.html",
          title = "Regression Results: Budget Deficit and Clientelism",
          dep.var.labels = "Budget Deficit (% of GDP)",
          column.labels = c("Baseline", "+ GE", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", 
                               "Government Effectiveness",
                               "Election Year",
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_I, se_model_re_J, se_model_re_K),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))


# Check and visualize skewness for subsidy
skew_subsidy <- skewness(panel_updated2$subsidy, na.rm = TRUE)
hist(panel_updated2$subsidy, main = paste("Subsidy (skewness:", round(skew_subsidy, 2), ")"), xlab = "Subsidy (% GDP)")
boxplot(panel_updated2$subsidy, main = "Subsidy Distribution")

# Check and visualize skewness for wage_gdp
skew_wage <- skewness(panel_updated2$wage_gdp, na.rm = TRUE)
hist(panel_updated2$wage_gdp, main = paste("Wage Bill (skewness:", round(skew_wage, 2), ")"), xlab = "Wage Bill (% GDP)")
boxplot(panel_updated2$wage_gdp, main = "Wage Bill Distribution")

# Check and visualize skewness for deficit
skew_deficit <- skewness(panel_updated2$deficit, na.rm = TRUE)
hist(panel_updated2$deficit, main = paste("Deficit (skewness:", round(skew_deficit, 2), ")"), xlab = "Deficit (% GDP)")
boxplot(panel_updated2$deficit, main = "Deficit Distribution")

#how to save the dataset
library(openxlsx)
write.xlsx(panel_updated2, file = "panel_updated2.xlsx")




# apppendices for the presentation 
#classification of the countri and their clientelism index
country_table <- panel_updated2 %>% 
  group_by(country) %>% 
  summarise(
    avg_client = round(mean(clientelism_alt, na.rm = TRUE), 2),
    tier       = first(clientelism_group),
    eu_member  = ifelse(country %in% c("Ukraine","Serbia","Albania","Moldova"), "No", "Yes")
  ) %>% 
  arrange(desc(avg_client))

gt(country_table) |>
  tab_header(title = "Sample countries and clientelism levels")
gtsave("A1_country_table.png")


library(reshape2); library(ggplot2)

vars <- panel_updated2 %>%                       
  select(
    clientelism_alt,    # main regressor
    gdpgrowth,          # macro control
    unemp,              # "
    inflation,          # "
    corruption,         # inst. quality control
    ge,                 # government effectiveness
    debt                # public‑debt control
  ) %>% 
  na.omit()             
M <- cor(vars, use = "pairwise.complete.obs")
   
p <- ggcorrplot(
  M,
  hc.order   = TRUE,                    
  type       = "lower",                  
  lab        = TRUE,                     
  lab_size   = 3,                      
  colors     = c("#B2182B", "#FFFFFF", "#2166AC"),  
  outline.col = "black"                 
) +
  theme_minimal(base_size = 14) +
  labs(title = "Correlation heat-map") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)

dev.off()       
dev.new(width = 6, height = 6)   
print(p)       
ggsave("A3_corr_heatmap.png", p, width = 6, height = 6, dpi = 300)



load_df <- data.frame(variable = rownames(pca_result$loadings),
                      loading  = as.numeric(pca_result$loadings[,1]))
library(ggplot2)
ggplot(load_df, aes(variable, loading, fill = variable)) +
  geom_col(show.legend = FALSE) +
  labs(title = "PCA loadings on Short-Termism Index", y = "Loading on PC1") +
  coord_flip()
ggsave("A10_PCA_loadings.png", width = 5, height = 3)






panel_no_greece <- subset(panel_updated2, country != "Greece")
#classifying clientelism
panel_no_greece <- panel_no_greece %>%
  mutate(panel_no_greece = cut(clientelism_alt,
                                 breaks = quantile(clientelism_alt, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                                 labels = c("low", "medium", "high"),
                                 include.lowest = TRUE))

panel_no_greece$clientelism_group <- factor(panel_no_greece$clientelism_group,
                                           levels = c("low", "medium", "high"))




# baseline model without interaction - wages
model_baseline_1 <- plm(
  wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp,
  data = panel_no_greece,
  index = c("country", "year"),
  model = "within",
  effect = "twoways"
)
summary(model_baseline_1)

coeftest(model_baseline_1, vcov = vcovSCC(model_baseline_1, type = "HC1", maxlag = 2))
bptest(model_baseline_A) #bad - using Kraay
pwartest(model_baseline_A) #bad - using Kraay
pcdtest(model_baseline_A, test = "cd") #on the edge - using Kraay
lm_baseline_A <- lm(wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp, data = panel_updated2)
vif(lm_baseline_A) #all good

model_re_A <- plm(wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp,
                  data = panel_updated2, index = c("country", "year"), model = "random")
summary(model_re_A)

phtest(model_baseline_A, model_re_A) #RE is also good
#will keep RE model because of the fact that the client as a factor almost do not change across years in a country and FE will drop it 
coeftest(model_re_A, vcov = vcovSCC(model_re_A, type = "HC1", maxlag = 2))


# adding additional deficit to model
model_re_B<- plm(
  wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp + deficit,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_B)
coeftest(model_re_B, vcov = vcovSCC(model_re_B, type = "HC1", maxlag = 2))

# adding additional ge to model
model_re_C<- plm(
  wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp + deficit + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_C)
coeftest(model_re_C, vcov = vcovSCC(model_re_C, type = "HC1", maxlag = 2))

# adding interaction
model_re_D <- plm(
  wage_gdp ~ clientelism_group * crisis + gdpgrowth + unemp + deficit + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_D)

coeftest(model_re_D, vcov = vcovSCC(model_re_D, type = "HC1", maxlag = 2))


# visualization 
se_model_re_A <- sqrt(diag(vcovSCC(model_re_A, type = "HC1", maxlag = 2)))
se_model_re_B <- sqrt(diag(vcovSCC(model_re_B, type = "HC1", maxlag = 2)))
se_model_re_C <- sqrt(diag(vcovSCC(model_re_C, type = "HC1", maxlag = 2)))
se_model_re_D <- sqrt(diag(vcovSCC(model_re_D, type = "HC1", maxlag = 2)))

stargazer(model_re_A, model_re_B, model_re_C, model_re_D,
          type = "html",
          out = "Regression_Wage.html",
          title = "Regression Results: Wage Spending and Clientelism",
          dep.var.labels = "Wage Bill (% of GDP)",
          column.labels = c("Baseline", "+ Deficit", "+ GE", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", "Deficit", 
                               "Government Effectiveness", 
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_A, se_model_re_B, se_model_re_C, se_model_re_D),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))



# baseline model without interaction - subsidies
model_re_E<- plm(
  subsidy ~ clientelism_group + crisis + gdpgrowth + unemp,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_E)
coeftest(model_re_E, vcov = vcovSCC(model_re_E, type = "HC1", maxlag = 2))

# adding additional deficit to model
model_re_F<- plm(
  subsidy ~ clientelism_group + crisis + gdpgrowth + unemp + deficit,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_F)
coeftest(model_re_F, vcov = vcovSCC(model_re_F, type = "HC1", maxlag = 2))

# adding additional ge to model
model_re_G<- plm(
  subsidy ~ clientelism_group + crisis + gdpgrowth + unemp + deficit + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_G)
coeftest(model_re_G, vcov = vcovSCC(model_re_G, type = "HC1", maxlag = 2))

# adding interaction
model_re_H <- plm(
  subsidy ~ clientelism_group * crisis + gdpgrowth + unemp + deficit + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_H)

coeftest(model_re_H, vcov = vcovSCC(model_re_H, type = "HC1", maxlag = 2))

# visualization
se_model_re_E <- sqrt(diag(vcovSCC(model_re_E, type = "HC1", maxlag = 2)))
se_model_re_F <- sqrt(diag(vcovSCC(model_re_F, type = "HC1", maxlag = 2)))
se_model_re_G <- sqrt(diag(vcovSCC(model_re_G, type = "HC1", maxlag = 2)))
se_model_re_H <- sqrt(diag(vcovSCC(model_re_H, type = "HC1", maxlag = 2)))

stargazer(model_re_E, model_re_F, model_re_G, model_re_H,
          type = "html",
          out = "Regression_Subsidy.html",
          title = "Regression Results: Subsidy Spending and Clientelism",
          dep.var.labels = "Subsidy Spending (% of GDP)",
          column.labels = c("Baseline", "+ Deficit", "+ GE", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", "Deficit", 
                               "Government Effectiveness", 
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_E, se_model_re_F, se_model_re_G, se_model_re_H),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))


# baseline model without interaction - deficit
model_re_I<- plm(
  deficit ~ clientelism_group + crisis + gdpgrowth + unemp,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_I)
coeftest(model_re_I, vcov = vcovSCC(model_re_I, type = "HC1", maxlag = 2))

# adding additional ge to model
model_re_J<- plm(
  deficit ~ clientelism_group + crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_J)
coeftest(model_re_J, vcov = vcovSCC(model_re_J, type = "HC1", maxlag = 2))

# adding interaction + added election year
model_re_K <- plm(
  deficit ~ clientelism_group * crisis + gdpgrowth + unemp + ge + election_y,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_K)

coeftest(model_re_K, vcov = vcovSCC(model_re_K, type = "HC1", maxlag = 2))

# visualization
se_model_re_I <- sqrt(diag(vcovSCC(model_re_I, type = "HC1", maxlag = 2)))
se_model_re_J <- sqrt(diag(vcovSCC(model_re_J, type = "HC1", maxlag = 2)))
se_model_re_K <- sqrt(diag(vcovSCC(model_re_K, type = "HC1", maxlag = 2)))


stargazer(model_re_I, model_re_J, model_re_K, 
          type = "html",
          out = "Regression_Deficit.html",
          title = "Regression Results: Budget Deficit and Clientelism",
          dep.var.labels = "Budget Deficit (% of GDP)",
          column.labels = c("Baseline", "+ GE", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", 
                               "Government Effectiveness",
                               "Election Year",
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_I, se_model_re_J, se_model_re_K),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))


