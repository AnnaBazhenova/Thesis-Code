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

### This is an example of script without one country (Greece in this case) ###

panel_check <- panel_no_greece %>%
  select(country, year, wage_gdp, clientelism_alt, crisis) %>%
  na.omit()
str(panel_check)

panel_no_greece <- subset(panel_updated2, country != "Greece")


## 1.Short-termism ##
pca_data_G <- panel_no_greece %>%
  select(wage_gdp, subsidy, positive_deficit) %>%
  scale()
pca_result_G <- principal(pca_data_G, nfactors = 1, rotate = "none")
panel_no_greece$short_termism_index <- as.numeric(pca_result_G$scores)

model_fe_short_G <- plm(
  short_termism_index ~ clientelism_alt * crisis + gdpgrowth + unemp + ge,
  data = panel_no_greece,
  index = c("country", "year"),
  model = "within"
)
summary(model_fe_short_G)

coeftest(model_fe_short_G, vcov = vcovSCC(model_fe_short_G, type = "HC1", maxlag = 2))


## Visualization of PCA ##
se_model_fe_short_G <- coeftest(model_fe_short_G, vcov = vcovSCC(model_fe_short_G, type = "HC1", maxlag = 2))[, 2]
stargazer(model_fe_short_G,
          type = "html",
          out = "short_termism_model_withoutG.html",
          title = "Regression Results: Short-Termism Index",
          dep.var.labels = "Short-Termism Index (PCA)",
          covariate.labels = c("Clientelism", "Crisis", "GDP Growth", "Unemployment", "Gov. Effectiveness",
                               "Clientelism × Crisis"),
          se = list(se_model_fe_short_G),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.",
                    "* p < 0.1, ** p < 0.05, *** p < 0.01"))



#classifying clientelism
panel_no_greece <- panel_no_greece %>%
  mutate(clientelism_group = cut(clientelism_alt,
                                 breaks = quantile(clientelism_alt, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                                 labels = c("low", "medium", "high"),
                                 include.lowest = TRUE))

panel_no_greece$clientelism_group <- factor(panel_no_greece$clientelism_group,
                                           levels = c("low", "medium", "high"))


## 2.Baseline model without interaction - wages ##
model_re_A_1 <- plm(wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp,
                  data = panel_no_greece, index = c("country", "year"), model = "random")
summary(model_re_A_1)

phtest(model_baseline_A_1, model_re_A_1) #RE is also good
#will keep RE model because of the fact that the client as a factor almost do not change across years in a country and FE will drop it 
coeftest(model_re_A_1, vcov = vcovSCC(model_re_A_1, type = "HC1", maxlag = 2))


## 3.Adding additional ge to model ##
model_re_C_1<- plm(
  wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp + ge,
  data = panel_no_greece,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_C_1)
coeftest(model_re_C_1, vcov = vcovSCC(model_re_C_1, type = "HC1", maxlag = 2))


## 4.Adding interaction ##
model_re_D_1 <- plm(
  wage_gdp ~ clientelism_group * crisis + gdpgrowth + unemp + ge,
  data = panel_no_greece,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_D_1)

coeftest(model_re_D_1, vcov = vcovSCC(model_re_D_1, type = "HC1", maxlag = 2))


## Visualization - wages ##
se_model_re_A_1 <- sqrt(diag(vcovSCC(model_re_A_1, type = "HC1", maxlag = 2)))
se_model_re_C_1 <- sqrt(diag(vcovSCC(model_re_C_1, type = "HC1", maxlag = 2)))
se_model_re_D_1 <- sqrt(diag(vcovSCC(model_re_D_1, type = "HC1", maxlag = 2)))

stargazer(model_re_A_1, model_re_C_1, model_re_D_1,
          type = "latex",
          float = FALSE,
          out = "Regression_Wage_withoutG.tex",
          title = "Regression Results: Wage Spending and Clientelism",
          dep.var.labels = "Wage Bill (% of GDP)",
          column.labels = c("Baseline", "+ GE", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment",
                               "Government Effectiveness", 
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_A_1, se_model_re_C_1, se_model_re_D_1),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))


## 5.Baseline model without interaction - subsidies ##
model_re_E_1<- plm(
  subsidy ~ clientelism_group + crisis + gdpgrowth + unemp,
  data = panel_no_greece,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_E_1)
coeftest(model_re_E_1, vcov = vcovSCC(model_re_E_1, type = "HC1", maxlag = 2))


## 6.Adding additional ge to model ##
model_re_G_1<- plm(
  subsidy ~ clientelism_group + crisis + gdpgrowth + unemp + ge,
  data = panel_no_greece,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_G_1)
coeftest(model_re_G_1, vcov = vcovSCC(model_re_G_1, type = "HC1", maxlag = 2))


## 7.Adding interaction ##
model_re_H_1 <- plm(
  subsidy ~ clientelism_group * crisis + gdpgrowth + unemp + ge,
  data = panel_no_greece,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_H_1)

coeftest(model_re_H_1, vcov = vcovSCC(model_re_H_1, type = "HC1", maxlag = 2))


## Visualization - subsidies ##
se_model_re_E_1 <- sqrt(diag(vcovSCC(model_re_E_1, type = "HC1", maxlag = 2)))
se_model_re_G_1 <- sqrt(diag(vcovSCC(model_re_G_1, type = "HC1", maxlag = 2)))
se_model_re_H_1 <- sqrt(diag(vcovSCC(model_re_H_1, type = "HC1", maxlag = 2)))

stargazer(model_re_E_1, model_re_G_1, model_re_H_1,
          type = "latex",
          float = FALSE,
          out = "Regression_Subsidy_withoutG.tex",
          title = "Regression Results: Subsidy Spending and Clientelism",
          dep.var.labels = "Subsidy Spending (% of GDP)",
          column.labels = c("Baseline", "+ GE", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment",
                               "Government Effectiveness", 
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_E_1, se_model_re_G_1, se_model_re_H_1),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))


## 8.Baseline model without interaction - deficit ##
model_re_I_1<- plm(
  positive_deficit ~ clientelism_group + crisis + gdpgrowth + unemp,
  data = panel_no_greece,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_I_1)
coeftest(model_re_I_1, vcov = vcovSCC(model_re_I_1, type = "HC1", maxlag = 2))


## 9.Adding additional ge to model ##
model_re_J_1<- plm(
  positive_deficit ~ clientelism_group + crisis + gdpgrowth + unemp + ge,
  data = panel_no_greece,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_J_1)
coeftest(model_re_J_1, vcov = vcovSCC(model_re_J_1, type = "HC1", maxlag = 2))


## 10.Adding interaction + added election year ##
model_re_K_1 <- plm(
  positive_deficit ~ clientelism_group * crisis + gdpgrowth + unemp + ge + election_y,
  data = panel_no_greece,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_K_1)

coeftest(model_re_K_1, vcov = vcovSCC(model_re_K_1, type = "HC1", maxlag = 2))


## Visualization - deficit ##
se_model_re_I_1 <- sqrt(diag(vcovSCC(model_re_I_1, type = "HC1", maxlag = 2)))
se_model_re_J_1 <- sqrt(diag(vcovSCC(model_re_J_1, type = "HC1", maxlag = 2)))
se_model_re_K_1 <- sqrt(diag(vcovSCC(model_re_K_1, type = "HC1", maxlag = 2)))


stargazer(model_re_I_1, model_re_J_1, model_re_K_1, 
          type = "latex",
          float = FALSE,
          out = "Regression_Deficit_WithoutG.tex",
          title = "Regression Results: Budget Deficit and Clientelism",
          dep.var.labels = "Budget Deficit (% of GDP)",
          column.labels = c("Baseline", "+ GE", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", 
                               "Government Effectiveness",
                               "Election Year",
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_I_1, se_model_re_J_1, se_model_re_K_1),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))


## Checks ##

# Skewness for subsidy #
skew_subsidy_G <- skewness(panel_no_greece$subsidy, na.rm = TRUE)
hist(panel_no_greece$subsidy, main = paste("Subsidy (skewness:", round(skew_subsidy_G, 2), ")"), xlab = "Subsidy (% GDP)")
boxplot(panel_no_greece$subsidy, main = "Subsidy Distribution")

# Skewness for wages #
skew_wage_G <- skewness(panel_no_greece$wage_gdp, na.rm = TRUE)
hist(panel_no_greece$wage_gdp, main = paste("Wage Bill (skewness:", round(skew_wage_G, 2), ")"), xlab = "Wage Bill (% GDP)")
boxplot(panel_no_greece$wage_gdp, main = "Wage Bill Distribution")

# Skewness for deficit #
skew_deficit_G <- skewness(panel_no_greece$deficit, na.rm = TRUE)
hist(panel_no_greece$deficit, main = paste("Deficit (skewness:", round(skew_deficit_G, 2), ")"), xlab = "Deficit (% GDP)")
boxplot(panel_no_greece$deficit, main = "Deficit Distribution")