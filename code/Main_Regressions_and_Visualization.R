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

### Models and Visualizations ###


## 1.GMM - lagged wage ##
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

## 2.GMM - lagged subsidies ##
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


## 3.GMM - lagged deficit ##
gmm_deficit_3 <- pgmm(
  positive_deficit ~ clientelism_alt + crisis |
    lag(positive_deficit, 3:4),
  data = panel_updated2,
  index = c("country", "year"),
  effect = "individual",
  model = "onestep",
  transformation = "d",
  collapse = TRUE
)

summary(gmm_deficit_3)


## 4.GMM - lagged wage with interaction ##
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

## 5.GMM - lagged subsidies with interaction ##
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


## 6.GMM - lagged dificit with interaction ##
gmm_deficit_3_int <- pgmm(
  positive_deficit ~ clientelism_alt * crisis |
    lag(positive_deficit, 3:4),
  data           = panel_updated2,
  index          = c("country", "year"),
  effect         = "individual",
  model          = "onestep",
  transformation = "d",
  collapse       = TRUE
)
summary(gmm_deficit_3_int)


## visualization of GMM ##
gmm_lst <- list(
  Wage_Main     = gmm_diff_3,
  Wage_WithInt  = gmm_diff_3_int,
  Subsidy_Main  = gmm_subsidy_3,
  Subsidy_WithInt = gmm_subsidy_3_int,
  Deficit_Main  = gmm_deficit_3,
  Deficit_WithInt = gmm_deficit_3_int
)

tidy_gmm <- function(mod, name){
  beta <- coef(mod)
  se   <- sqrt(diag(vcov(mod)))      
  
  tibble::tibble(
    model = name,
    term  = names(beta),
    estimate = beta,
    conf.low  = beta - 1.96*se,
    conf.high = beta + 1.96*se
  )
}

plot_df <- imap_dfr(gmm_lst, tidy_gmm) |>
  filter(term %in% c("clientelism_alt","crisis","clientelism_alt:crisis")) |>
  
  mutate(
    Outcome = sub("_.*","", model),            
    Spec    = ifelse(grepl("WithInt", model),"WithInt","Main"),
    Pretty  = recode(term,
                     clientelism_alt       = "Clientelism",
                     crisis                = "Crisis",
                     `clientelism_alt:crisis` = "Clientelism × Crisis"),
    has_int = term == "clientelism_alt:crisis"  
  )

plot_df <- plot_df |>
  filter(!(Spec == "Main" & has_int))

graphics.off()
dev.new(width = 7, height = 5) 

new <-ggplot(plot_df,
             aes(x = estimate, y = Pretty,
                 colour = Spec, shape = Spec)) +
  
  geom_vline(xintercept = 0, lty = 2, colour = "grey60") +
  geom_point(size = 2.8, position = position_dodge(width = 0.35)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, position = position_dodge(width = 0.35)) +
  
  scale_colour_manual(values = c(Main = "#636363", WithInt = "#cc4c02")) +
  scale_shape_manual(values  = c(Main = 16, WithInt = 17)) +
  
  facet_wrap(~ Outcome, scales = "free_x") +
  labs(title    = "One-step Difference-GMM (exact SEs)",
       subtitle = "Black = main  Orange = interaction",
       x = "Coefficient (95 % CI)", y = NULL,
       colour = "Specification", shape = "Specification") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

ggsave("gmm_plot_new_d.png", plot = new, width = 10, height = 6, dpi = 300)


## 7.Short-termism ##
pca_data <- panel_updated2 %>%
  select(wage_gdp, subsidy, positive_deficit) %>%
  scale()
pca_result <- principal(pca_data, nfactors = 1, rotate = "none")
panel_updated2$short_termism_index <- as.numeric(pca_result$scores)
print(pca_result$loadings)

model_fe_short_f <- plm(
  short_termism_index ~ clientelism_alt * crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "within"
)
summary(model_fe_short_f)

coeftest(model_fe_short_f, vcov = vcovSCC(model_fe_short_f, type = "HC1", maxlag = 2))

## visualization PCA ##
se_fe_short_f <- sqrt(diag(vcovSCC(model_fe_short_f, type = "HC1", maxlag = 2)))

stargazer(model_fe_short_f,
          type = "latex",
          float = FALSE,
          out = "short_termism_regression.tex",
          title = "Regression Results: Short-Termism and Clientelism",
          dep.var.labels = "Short-Termism Index (PCA)",
          covariate.labels = c("Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", "Government Effectiveness",
                               "Clientelism × Crisis"),
          se = list(se_fe_short_f),
          notes = c("Driscoll–Kraay SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"),
          no.space = TRUE)

## visualization PCA ## 
med_val <- median(panel_updated2$clientelism_alt, na.rm = TRUE)

#the mean of all observations
low_val <- panel_updated2 %>%
  filter(!is.na(clientelism_alt) & clientelism_alt <= med_val) %>%
  summarise(mean_val = mean(clientelism_alt)) %>%
  pull(mean_val)

high_val <- panel_updated2 %>%
  filter(!is.na(clientelism_alt) & clientelism_alt > med_val) %>%
  summarise(mean_val = mean(clientelism_alt)) %>%
  pull(mean_val)

#means of other control variables 
mean_gdp   <- mean(panel_updated2$gdpgrowth, na.rm = TRUE)
mean_unemp <- mean(panel_updated2$unemp,     na.rm = TRUE)
mean_ge    <- mean(panel_updated2$ge,        na.rm = TRUE)

pred_grid <- expand.grid(
  clientelism_alt = c(low_val, high_val),
  crisis          = c(0, 1)
) %>%
  mutate(
    gdpgrowth = mean_gdp,
    unemp     = mean_unemp,
    ge        = mean_ge,
    clientelism_cat = factor(
      clientelism_alt,
      levels = c(low_val, high_val),
      labels = c("Low", "High")
    )
  )

beta_hat <- coef(model_fe_short_f)
V_hat    <- vcovSCC(model_fe_short_f, type = "HC1", maxlag = 2)

X <- model.matrix(
  ~ clientelism_alt * crisis + gdpgrowth + unemp + ge - 1,
  data = pred_grid
)

pred_grid <- pred_grid %>%
  mutate(
    fit    = as.vector(X %*% beta_hat),
    se_fit = sqrt(diag(X %*% V_hat %*% t(X)))
  )

plot_short_termism <- ggplot(
  pred_grid,
  aes(x = clientelism_cat, y = fit, fill = factor(crisis))
) +
  geom_col(
    position = position_dodge(width = 0.6),
    width    = 0.5,
    color    = NA
  ) +
  geom_text(
    aes(label = sprintf("%.2f", fit)),
    position = position_dodge(width = 0.6),
    vjust    = -0.7,
    fontface = "bold",
    size     = 4
  ) +
  scale_fill_manual(
    values = c("0" = "#2C7BB6", 
               "1" = "#D7191C"),
    labels = c("No Crisis", "Crisis"),
    name   = ""
  ) +
  labs(
    title    = "Predicted Short-Termism (PCA) by Clientelism Group",
    subtitle = "Low vs. High clientelism, with and without 2008 Crisis",
    x        = "Clientelism Group",
    y        = "Predicted Short-Termism Index"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title         = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle      = element_text(size = 14, hjust = 0.5, margin = margin(b = 10)),
    axis.title.x       = element_text(face = "bold", size = 14, margin = margin(t = 8)),
    axis.title.y       = element_text(face = "bold", size = 14, margin = margin(r = 8)),
    axis.text.x        = element_text(size = 12),
    axis.text.y        = element_text(size = 12),
    legend.position    = "top",
    legend.text        = element_text(size = 12),
    panel.grid.major.x = element_blank(),                
    panel.grid.minor   = element_blank(),               
    panel.grid.major.y = element_line(color = "grey80")  
  )

print(plot_short_termism)

ggsave(
  filename = "ShortTermism_LowHigh_HorizGrid_NoErrBars.png",
  plot     = plot_short_termism,
  width    = 10,
  height   = 6,
  dpi      = 300
)

## 8.Baseline model without interaction - wages ##
model_re_A <- plm(wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp,
                  data = panel_updated2, index = c("country", "year"), model = "random")
summary(model_re_A)
coeftest(model_re_A, vcov = vcovSCC(model_re_A, type = "HC1", maxlag = 2))


## 9.Adding additional ge to model ##
model_re_C<- plm(
  wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_C)
coeftest(model_re_C, vcov = vcovSCC(model_re_C, type = "HC1", maxlag = 2))


## 10.Adding additional expenditure to model ##
model_re_C_e<- plm(
  wage_gdp ~ clientelism_group + crisis + gdpgrowth + unemp + ge + expend,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_C_e)
coeftest(model_re_C_e, vcov = vcovSCC(model_re_C_e, type = "HC1", maxlag = 2))


# 11.Adding interaction ##
model_re_D <- plm(
  wage_gdp ~ clientelism_group * crisis + gdpgrowth + unemp + ge + expend,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_D)

coeftest(model_re_D, vcov = vcovSCC(model_re_D, type = "HC1", maxlag = 2))


# Visualization - wages ##
se_model_re_A <- sqrt(diag(vcovSCC(model_re_A, type = "HC1", maxlag = 2)))
se_model_re_C <- sqrt(diag(vcovSCC(model_re_C, type = "HC1", maxlag = 2)))
se_model_re_C_e <- sqrt(diag(vcovSCC(model_re_C_e, type = "HC1", maxlag = 2)))
se_model_re_D <- sqrt(diag(vcovSCC(model_re_D, type = "HC1", maxlag = 2)))

stargazer(model_re_A, model_re_C, model_re_C_e, model_re_D,
          type = "latex",
          float = FALSE,
          out = "Regression_Wage_full.tex",
          title = "Regression Results: Wage Spending and Clientelism",
          dep.var.labels = "Wage Bill (% of GDP)",
          column.labels = c("Baseline", "+ GE", "+ Expend", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", 
                               "Government Effectiveness", "Expenditure", 
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_A, se_model_re_C, se_model_re_C_e, se_model_re_D),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))

## Another visualization - key coefficients ##
se_A <- sqrt(diag(vcovSCC(model_re_A, type = "HC1", maxlag = 2)))
se_D <- sqrt(diag(vcovSCC(model_re_D, type = "HC1", maxlag = 2)))
stargazer(
  model_re_A,
  model_re_D,
  type              = "html",              
  out               = "Wage_KeyVars_Table.html",
  title             = "Wage Spending (% of GDP): Key Coefficients Only",
  dep.var.labels    = "Wage Bill (% of GDP)",
  column.labels     = c("Baseline", "Interaction"),
  keep              = c(
    "clientelism_groupmedium", 
    "clientelism_grouphigh", 
    "^crisis$", 
    "clientelism_groupmedium:crisis",
    "clientelism_grouphigh:crisis"
  ),
  covariate.labels  = c(
    "Medium Clientelism", 
    "High Clientelism", 
    "Crisis", 
    "Medium × Crisis", 
    "High × Crisis"
  ),
  se                = list(se_A, se_D),
  add.lines         = list(
    c("Other controls included", "Yes", "Yes")
  ),
  digits            = 3,
  star.char         = c("*", "**", "***"),
  star.cutoffs      = c(0.10, 0.05, 0.01),
  
  notes             = c("Driscoll–Kraay SEs (HC1), maxlag = 2;.", 
                        "* p < 0.10; ** p < 0.05; *** p < 0.01")
)


## 12.Baseline model without interaction - subsidies ##
model_re_E<- plm(
  subsidy ~ clientelism_group + crisis + gdpgrowth + unemp,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_E)
coeftest(model_re_E, vcov = vcovSCC(model_re_E, type = "HC1", maxlag = 2))


## 13.Adding additional ge to model ##
model_re_G<- plm(
  subsidy ~ clientelism_group + crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_G)
coeftest(model_re_G, vcov = vcovSCC(model_re_G, type = "HC1", maxlag = 2))


## 14.Adding additional expend to model ##
model_re_G_e<- plm(
  subsidy ~ clientelism_group + crisis + gdpgrowth + unemp + ge + expend,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_G_e)
coeftest(model_re_G_e, vcov = vcovSCC(model_re_G_e, type = "HC1", maxlag = 2))


## 15.Adding interaction ##
model_re_H <- plm(
  subsidy ~ clientelism_group * crisis + gdpgrowth + unemp + ge + expend,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_H)

coeftest(model_re_H, vcov = vcovSCC(model_re_H, type = "HC1", maxlag = 2))


## Visualization - subsidies ##
se_model_re_E <- sqrt(diag(vcovSCC(model_re_E, type = "HC1", maxlag = 2)))
se_model_re_G <- sqrt(diag(vcovSCC(model_re_G, type = "HC1", maxlag = 2)))
se_model_re_G_e <- sqrt(diag(vcovSCC(model_re_G_e, type = "HC1", maxlag = 2)))
se_model_re_H <- sqrt(diag(vcovSCC(model_re_H, type = "HC1", maxlag = 2)))

stargazer(model_re_E, model_re_G, model_re_G_e, model_re_H,
          type = "latex",
          out = "Regression_Subsidy_full.tex",
          float = FALSE,
          title = "Regression Results: Subsidy Spending and Clientelism",
          dep.var.labels = "Subsidy Spending (% of expense)",
          column.labels = c("Baseline", "+ GE", "+ Expend", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", 
                               "Government Effectiveness", "Expenditure", 
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_E, se_model_re_G, se_model_re_G_e, se_model_re_H),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))


## Another visualization - main coefficients ##
se_E <- sqrt(diag(vcovSCC(model_re_E, type = "HC1", maxlag = 2)))
se_H <- sqrt(diag(vcovSCC(model_re_H, type = "HC1", maxlag = 2)))

stargazer(
  model_re_E,  
  model_re_H,    
  type              = "html",                            
  out               = "Subsidy_KeyVars_Table.html",
  title             = "Subsidy Spending (% of expense): Key Coefficients Only",
  dep.var.labels    = "Subsidy Spending (% of expense)",
  column.labels     = c("Baseline", "Interaction"),
  keep              = c(
    "clientelism_groupmedium",      
    "clientelism_grouphigh",        
    "^crisis$",                     
    "clientelism_groupmedium:crisis",  
    "clientelism_grouphigh:crisis"     
  ),
  covariate.labels  = c(
    "Medium Clientelism", 
    "High Clientelism", 
    "Crisis", 
    "Medium × Crisis", 
    "High × Crisis"
  ),
  se                = list(se_E, se_H),
  add.lines         = list(
    c("Other controls included", "Yes", "Yes")
  ),
  digits            = 3,
  star.char         = c("*", "**", "***"),
  star.cutoffs      = c(0.10, 0.05, 0.01),
  
  notes             = c("Driscoll–Kraay SEs (HC1), maxlag = 2.",
                        "* p < 0.10; ** p < 0.05; *** p < 0.01")
)


## 16.Baseline model without interaction - deficit ##
model_re_I<- plm(
  positive_deficit ~ clientelism_group + crisis + gdpgrowth + unemp,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_I)
coeftest(model_re_I, vcov = vcovSCC(model_re_I, type = "HC1", maxlag = 2))


## 17.Adding additional ge to model ##
model_re_J<- plm(
  positive_deficit ~ clientelism_group + crisis + gdpgrowth + unemp + ge,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_J)
coeftest(model_re_J, vcov = vcovSCC(model_re_J, type = "HC1", maxlag = 2))


## 18.Adding additional debt to model ##
model_re_J_d<- plm(
  positive_deficit ~ clientelism_group + crisis + gdpgrowth + unemp + ge + debt,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)

summary(model_re_J_d)
coeftest(model_re_J_d, vcov = vcovSCC(model_re_J_d, type = "HC1", maxlag = 2))


## 19.Adding interaction + added election year ##
model_re_K <- plm(
  positive_deficit ~ clientelism_group * crisis + gdpgrowth + unemp + ge + debt + election_y,
  data = panel_updated2,
  index = c("country", "year"),
  model = "random"
)
summary(model_re_K)

coeftest(model_re_K, vcov = vcovSCC(model_re_K, type = "HC1", maxlag = 2))


## Visualization - deficit ##
se_model_re_I <- sqrt(diag(vcovSCC(model_re_I, type = "HC1", maxlag = 2)))
se_model_re_J <- sqrt(diag(vcovSCC(model_re_J, type = "HC1", maxlag = 2)))
se_model_re_J_d <- sqrt(diag(vcovSCC(model_re_J_d, type = "HC1", maxlag = 2)))
se_model_re_K <- sqrt(diag(vcovSCC(model_re_K, type = "HC1", maxlag = 2)))


stargazer(model_re_I, model_re_J, model_re_J_d, model_re_K, 
          type = "latex",
          float = FALSE,
          out = "Regression_Deficit_full.tex",
          title = "Regression Results: Budget Deficit and Clientelism",
          dep.var.labels = "Budget Deficit (% of GDP)",
          column.labels = c("Baseline", "+ GE", "+ Debt", "Interaction"),
          covariate.labels = c("Medium Clientelism", "High Clientelism", "Crisis", 
                               "GDP Growth", "Unemployment", 
                               "Government Effectiveness", "Gov. Debt",
                               "Election Year",
                               "Medium x Crisis", "High x Crisis"),
          se = list(se_model_re_I, se_model_re_J, se_model_re_J_d, se_model_re_K),
          notes = c("Driscoll–Kraay robust SEs (HC1), maxlag = 2.", "* p < 0.1, ** p < 0.05, *** p < 0.01"))