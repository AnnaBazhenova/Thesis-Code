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


### This is a script for VIF checks ###


## Subsidy ##

lm_main_s <- lm(
  subsidy ~ clientelism_group + crisis + gdpgrowth + unemp + ge + expend,
  data = panel_updated2
)

vif_values <- car::vif(lm_main_s)
vif_vec <- sort(if (is.matrix(vif_values)) vif_values[, 1] else vif_values)
vif_tbl <- tibble(
  Term = names(vif_vec),
  VIF  = as.numeric(vif_vec),
  Flag = case_when(
    VIF < 5 ~ "<5",
    VIF < 10 ~ "5–10",
    TRUE ~ ">10"
  )
)
vif_tbl$Flag <- factor(vif_tbl$Flag, levels = c("<5", "5–10", ">10"))


ggplot(vif_tbl, aes(x = Term, y = VIF, fill = Flag, shape = Flag)) +
  geom_point(size = 5, color = "black") +
  scale_shape_manual(values = c("<5" = 21, "5–10" = 22, ">10" = 23)) +
  scale_fill_manual(values = c("<5" = "lightgreen", "5–10" = "gold", ">10" = "salmon")) +
  labs(
    title = "Variance Inflation Factors for the Main Effects Model (Subsidies)",
    x = "Term", y = "VIF", fill = "Flag", shape = "Flag"
  ) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "orange") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  theme_minimal(base_size = 14)
ggsave("vif_plot_subsidy.png", width = 8, height = 6, dpi = 300)


## Wage ##
lm_main <- lm(
  wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp + ge + expend,
  data = panel_updated2
)

vif_values <- car::vif(lm_main)
vif_vec <- sort(if (is.matrix(vif_values)) vif_values[, 1] else vif_values)
vif_tbl <- tibble(
  Term = names(vif_vec),
  VIF  = as.numeric(vif_vec),
  Flag = case_when(
    VIF < 5 ~ "<5",
    VIF < 10 ~ "5–10",
    TRUE ~ ">10"
  )
)
vif_tbl$Flag <- factor(vif_tbl$Flag, levels = c("<5", "5–10", ">10"))


ggplot(vif_tbl, aes(x = Term, y = VIF, fill = Flag, shape = Flag)) +
  geom_point(size = 5, color = "black") +
  scale_shape_manual(values = c("<5" = 21, "5–10" = 22, ">10" = 23)) +
  scale_fill_manual(values = c("<5" = "lightgreen", "5–10" = "gold", ">10" = "salmon")) +
  labs(
    title = "Variance Inflation Factors (Main Effects Model)",
    x = "Term", y = "VIF", fill = "Flag", shape = "Flag"
  ) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "orange") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  theme_minimal(base_size = 14)


ggsave("vif_plot_wages.png", width = 8, height = 6, dpi = 300)


## Deficit ##
lm_main_d <- lm(
  positive_deficit ~ clientelism_group + crisis + gdpgrowth + unemp + ge + debt,
  data = panel_updated2
)

vif_values <- car::vif(lm_main_d)
vif_vec <- sort(if (is.matrix(vif_values)) vif_values[, 1] else vif_values)
vif_tbl <- tibble(
  Term = names(vif_vec),
  VIF  = as.numeric(vif_vec),
  Flag = case_when(
    VIF < 5 ~ "<5",
    VIF < 10 ~ "5–10",
    TRUE ~ ">10"
  )
)
vif_tbl$Flag <- factor(vif_tbl$Flag, levels = c("<5", "5–10", ">10"))


ggplot(vif_tbl, aes(x = Term, y = VIF, fill = Flag, shape = Flag)) +
  geom_point(size = 5, color = "black") +
  scale_shape_manual(values = c("<5" = 21, "5–10" = 22, ">10" = 23)) +
  scale_fill_manual(values = c("<5" = "lightgreen", "5–10" = "gold", ">10" = "salmon")) +
  labs(
    title = "Variance Inflation Factors for the Main Effects Model (Deficit)",
    x = "Term", y = "VIF", fill = "Flag", shape = "Flag"
  ) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "orange") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  theme_minimal(base_size = 14)

ggsave("vif_plot_deficit.png", width = 8, height = 6, dpi = 300)