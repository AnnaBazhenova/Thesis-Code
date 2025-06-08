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


### This is a script with additional controls for PCA short-termism ###


model_fe_short_4 <- plm(
  short_termism_index ~ clientelism_alt * crisis + gdpgrowth + unemp + ge + debt + expend,
  data = panel_updated2,
  index = c("country", "year"),
  model = "within"
)
summary(model_fe_short_4)

coeftest(model_fe_short_4, vcov = vcovSCC(model_fe_short_4, type = "HC1", maxlag = 2))


## Visualization of PCA ##
se_model_fe_short_4 <- coeftest(model_fe_short_4, vcov = vcovSCC(model_fe_short_4, type = "HC1", maxlag = 2))[, 2]
stargazer(model_fe_short_4,
          type = "latex",
          float = FALSE,
          out = "short_termism_model_debt.tex",
          title = "Regression Results: Short-Termism Index",
          dep.var.labels = "Short-Termism Index (PCA)",
          covariate.labels = c("Clientelism", "Crisis", "GDP Growth", "Unemployment", "Gov. Effectiveness",
                               "Debt", "Expenditure",
                               "Clientelism × Crisis"),
          se = list(se_model_fe_short_4),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.",
                    "* p < 0.1, ** p < 0.05, *** p < 0.01"))



