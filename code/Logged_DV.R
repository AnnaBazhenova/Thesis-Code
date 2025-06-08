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

### This is a script with log transformation of DVs ###


panel_updated2 <- panel_updated2 %>%
  mutate(
    log_wage_gdp  = if_else(wage_gdp > 0, log(wage_gdp), NA_real_),
    log_subsidy   = if_else(subsidy > 0, log(subsidy), NA_real_)
  )
panel_updated2 <- panel_updated2 %>%
  mutate(
    positive_deficit = -deficit,
    log_deficit = if_else(positive_deficit > 0, log(positive_deficit), NA_real_)
  )


## 1.Baseline model without interaction - wages ##
model_re_A_2 <- plm(log_wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp,
                  data = panel_updated2, index = c("country", "year"), model = "random")
summary(model_re_A_2)
coeftest(model_re_A_2, vcov = vcovSCC(model_re_A_2, type = "HC1", maxlag = 2))


## 2.Adding additional ge to model ##
model_re_C_2<- plm(
  log_wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_C_2)
coeftest(model_re_C_2, vcov = vcovSCC(model_re_C_2, type = "HC1", maxlag = 2))


## 3.Adding additional expenditure to model ##
model_re_C_e2<- plm(
  log_wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp + ge + expend,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_C_e2)
coeftest(model_re_C_e2, vcov = vcovSCC(model_re_C_e2, type = "HC1", maxlag = 2))


## 4.adding interaction ##
model_re_D_2 <- plm(
  log_wage_gdp ~ clientelism_group * crisis + gdpgrowth + unemp + ge + expend,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_D_2)

coeftest(model_re_D_2, vcov = vcovSCC(model_re_D_2, type = "HC1", maxlag = 2))


## Visualization - wages ##
se_model_re_A_2 <- sqrt(diag(vcovSCC(model_re_A_2, type = "HC1", maxlag = 2)))
se_model_re_C_2 <- sqrt(diag(vcovSCC(model_re_C_2, type = "HC1", maxlag = 2)))
se_model_re_C_e2 <- sqrt(diag(vcovSCC(model_re_C_e2, type = "HC1", maxlag = 2)))
se_model_re_D_2 <- sqrt(diag(vcovSCC(model_re_D_2, type = "HC1", maxlag = 2)))

stargazer(model_re_A_2, model_re_C_2, model_re_C_e2, model_re_D_2,
          type = "latex",
          float = FALSE, 
          out = "Regression_Wage_logged.tex",
          title = "Regression Results: Wage Spending and Clientelism",
          dep.var.labels = "Logged Wage Bill",
          column.labels = c("Baseline", "+ GE", "+ Expend", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", 
                               "Government Effectiveness", "Expenditure",
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_A_2, se_model_re_C_2, se_model_re_D_2),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))


## 5.Baseline model without interaction - subsidies ##
model_re_E_2<- plm(
  log_subsidy ~ clientelism_group + crisis + gdpgrowth + unemp,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_E_2)
coeftest(model_re_E_2, vcov = vcovSCC(model_re_E_2, type = "HC1", maxlag = 2))


## 6.Adding additional ge to model ##
model_re_G_2<- plm(
  log_subsidy ~ clientelism_group + crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_G_2)
coeftest(model_re_G_2, vcov = vcovSCC(model_re_G_2, type = "HC1", maxlag = 2))


## 7.Adding interaction ##
model_re_H_2 <- plm(
  log_subsidy ~ clientelism_group * crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_H_2)

coeftest(model_re_H_2, vcov = vcovSCC(model_re_H_2, type = "HC1", maxlag = 2))


## Visualization - subsidies ##
se_model_re_E_2 <- sqrt(diag(vcovSCC(model_re_E_2, type = "HC1", maxlag = 2)))
se_model_re_G_2 <- sqrt(diag(vcovSCC(model_re_G_2, type = "HC1", maxlag = 2)))
se_model_re_H_2 <- sqrt(diag(vcovSCC(model_re_H_2, type = "HC1", maxlag = 2)))

stargazer(model_re_E_2, model_re_G_2, model_re_H_2,
          type = "latex",
          float = FALSE,
          out = "Regression_Subsidy_logged.tex",
          title = "Regression Results: Subsidy Spending and Clientelism",
          dep.var.labels = "Subsidy Spending (% of GDP)",
          column.labels = c("Baseline", "+ GE", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment",
                               "Government Effectiveness", 
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_E_2, se_model_re_G_2, se_model_re_H_2),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))


## 8.Baseline model without interaction - deficit ##
model_re_I_2<- plm(
  log_deficit ~ clientelism_group + crisis + gdpgrowth + unemp,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_I_2)
coeftest(model_re_I_2, vcov = vcovSCC(model_re_I_2, type = "HC1", maxlag = 2))


## 9.Adding additional ge to model ##
model_re_J_2<- plm(
  log_deficit ~ clientelism_group + crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_J_2)
coeftest(model_re_J_2, vcov = vcovSCC(model_re_J_2, type = "HC1", maxlag = 2))


## 10.Ading interaction + added election year ##
model_re_K_2 <- plm(
  log_deficit ~ clientelism_group * crisis + gdpgrowth + unemp + ge + election_y,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_K_2)

coeftest(model_re_K_2, vcov = vcovSCC(model_re_K_2, type = "HC1", maxlag = 2))


## Visualization - deficit ##
se_model_re_I_2 <- sqrt(diag(vcovSCC(model_re_I_2, type = "HC1", maxlag = 2)))
se_model_re_J_2 <- sqrt(diag(vcovSCC(model_re_J_2, type = "HC1", maxlag = 2)))
se_model_re_K_2 <- sqrt(diag(vcovSCC(model_re_K_2, type = "HC1", maxlag = 2)))


stargazer(model_re_I_2, model_re_J_2, model_re_K_2, 
          type = "latex",
          float = FALSE,
          out = "Regression_Deficit_logged.tex",
          title = "Regression Results: Budget Deficit and Clientelism",
          dep.var.labels = "Budget Deficit (% of GDP)",
          column.labels = c("Baseline", "+ GE", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", 
                               "Government Effectiveness",
                               "Election Year",
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_I_2, se_model_re_J_2, se_model_re_K_2),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))

