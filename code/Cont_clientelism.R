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

### This is a script with continuous clientelism used as the main DV ###

## 1.Baseline model without interaction - wages ##
model_re_A_3 <- plm(wage_gdp ~ clientelism_alt + crisis + gdpgrowth + unemp,
                  data = panel_updated2, index = c("country", "year"), model = "random")
summary(model_re_A_3)
coeftest(model_re_A_3, vcov = vcovSCC(model_re_A_3, type = "HC1", maxlag = 2))


## 2.Adding additional ge to model ##
model_re_C_3<- plm(
  wage_gdp ~ clientelism_alt + crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_C_3)
coeftest(model_re_C_3, vcov = vcovSCC(model_re_C_3, type = "HC1", maxlag = 2))


## 3.Adding interaction ##
model_re_D_3 <- plm(
  wage_gdp ~ clientelism_alt * crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_D_3)

coeftest(model_re_D_3, vcov = vcovSCC(model_re_D_3, type = "HC1", maxlag = 2))


## Visualization - wages ##
se_model_re_A_3 <- sqrt(diag(vcovSCC(model_re_A_3, type = "HC1", maxlag = 2)))
se_model_re_C_3 <- sqrt(diag(vcovSCC(model_re_C_3, type = "HC1", maxlag = 2)))
se_model_re_D_3 <- sqrt(diag(vcovSCC(model_re_D_3, type = "HC1", maxlag = 2)))

stargazer(model_re_A_3, model_re_C_3, model_re_D_3,
          type = "latex",
          float = FALSE,
          out = "Regression_Wage_alt.tex",
          title = "Regression Results: Wage Spending and Clientelism",
          dep.var.labels = "Wage Bill (% of GDP)",
          column.labels = c("Baseline", "+ GE", "Interaction"),
          covariate.labels = c("Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment",
                               "Government Effectiveness", 
                               "Clientelism × Crisis"),
          se = list(se_model_re_A_3, se_model_re_C_3, se_model_re_D_3),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))


## 4.Baseline model without interaction - subsidies ##
model_re_E_3<- plm(
  subsidy ~ clientelism_alt + crisis + gdpgrowth + unemp,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_E_3)
coeftest(model_re_E_3, vcov = vcovSCC(model_re_E_3, type = "HC1", maxlag = 2))


## 5.Adding additional ge to model ##
model_re_G_3<- plm(
  subsidy ~ clientelism_alt + crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_G_3)
coeftest(model_re_G_3, vcov = vcovSCC(model_re_G_3, type = "HC1", maxlag = 2))


## 6.Adding interaction ##
model_re_H_3 <- plm(
  subsidy ~ clientelism_alt * crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_H_3)

coeftest(model_re_H_3, vcov = vcovSCC(model_re_H_3, type = "HC1", maxlag = 2))


## Visualization - subsidies ##
se_model_re_E_3 <- sqrt(diag(vcovSCC(model_re_E_3, type = "HC1", maxlag = 2)))
se_model_re_G_3 <- sqrt(diag(vcovSCC(model_re_G_3, type = "HC1", maxlag = 2)))
se_model_re_H_3 <- sqrt(diag(vcovSCC(model_re_H_3, type = "HC1", maxlag = 2)))

stargazer(model_re_E_3, model_re_G_3, model_re_H_3,
          type = "latex",
          float = FALSE,
          out = "Regression_Subsidy_alt.tex",
          title = "Regression Results: Subsidy Spending and Clientelism",
          dep.var.labels = "Subsidy Spending (% of GDP)",
          column.labels = c("Baseline", "+ GE", "Interaction"),
          covariate.labels = c("Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment",
                               "Government Effectiveness", 
                               "Clientelism × Crisis"),
          se = list(se_model_re_E_3, se_model_re_G_3, se_model_re_H_3),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))


## 7.Baseline model without interaction - deficit ##
model_re_I_3<- plm(
  positive_deficit ~ clientelism_alt + crisis + gdpgrowth + unemp,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_I_3)
coeftest(model_re_I_3, vcov = vcovSCC(model_re_I_3, type = "HC1", maxlag = 2))


## 8.Adding additional ge to model ##
model_re_J_3<- plm(
  positive_deficit ~ clientelism_alt + crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_J_3)
coeftest(model_re_J_3, vcov = vcovSCC(model_re_J_3, type = "HC1", maxlag = 2))


## 9.Adding interaction + added election year ##
model_re_K_3 <- plm(
  positive_deficit ~ clientelism_alt * crisis + gdpgrowth + unemp + ge + election_y,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_K_3)

coeftest(model_re_K_3, vcov = vcovSCC(model_re_K_3, type = "HC1", maxlag = 2))


## 10.Visualization - deficit ##
se_model_re_I_3 <- sqrt(diag(vcovSCC(model_re_I_3, type = "HC1", maxlag = 2)))
se_model_re_J_3 <- sqrt(diag(vcovSCC(model_re_J_3, type = "HC1", maxlag = 2)))
se_model_re_K_3 <- sqrt(diag(vcovSCC(model_re_K_3, type = "HC1", maxlag = 2)))


stargazer(model_re_I_3, model_re_J_3, model_re_K_3, 
          type = "latex",
          float = FALSE,
          out = "Regression_Deficit_alt.tex",
          title = "Regression Results: Budget Deficit and Clientelism",
          dep.var.labels = "Budget Deficit (% of GDP)",
          column.labels = c("Baseline", "+ GE", "Interaction"),
          covariate.labels = c("Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", 
                               "Government Effectiveness",
                               "Election Year",
                               "Clientelism x Crisis"),
          se = list(se_model_re_I_3, se_model_re_J_3, se_model_re_K_3),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))

