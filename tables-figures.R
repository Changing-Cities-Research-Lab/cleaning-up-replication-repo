# This script generates all tables and figures in "Cleaning Up the Neighborhood: White Influx and Differential Requests for Services"

library(tidycensus)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(stargazer)
library(tigris)
library(patchwork)
library(scales)
library(ggmap)

# Load Data ----

df_full <- read_csv(here::here("df_full.csv")) 
df_gent <- read_csv(here::here("df_gent.csv"))
df_map <- st_read(here::here("df_map.shp"))

# Table 1: Descriptive Statistics of Trash Prevalence by Block Group-Year and City ----

df_full %>% 
  group_by(city) %>% 
  summarize(n_blkgrps = n(), mean = mean(trash_pred), var = var(trash_pred, na.rm=TRUE))

# Table 2: Descriptive Statistics for Tract-Level Independent Variables by City ----

## Create summary data frame:
### Calculate summary statistics for various independent variables grouped by city
df_summary <- df_full %>%
  group_by(city) %>%
  summarise(across(c('pop_trt', # Population
                     'pnhwht_trt', # Percentage Non-Hispanic White
                     'pnhblk_trt', # Percentage Non-Hispanic Black
                     'phisp_trt', # Percentage Hispanic 
                     'pasian_trt', # Percentage Asian
                     'prenter_trt', # Percentage Renters
                     'income_trt', # Median Household Income
                     'pfgnbn_trt'), # Percentage Foreign Born
                   list(mean = ~sprintf("%.2f", mean(.)),
                        sd = ~sprintf("%.2f", sd(.))),
                   .names = "{.col}_{.fn}"),
            .groups = 'drop')

## Display the summary table in a nicely formatted HTML table
knitr::kable(df_summary, col.names = c("City"), format = "html") %>%
  kableExtra::kable_styling()

# Table 3: Coefficients for Poisson Models Predicting Logged 311 Requests per Capita ----

## List of various models of interest
models <- list(
  glm(trash_311 ~ offset(log(pop_blkgrp)) + trash_pred*pnhblk_trt + trash_pred*phisp_trt + trash_pred*pasian_trt + trash_pred*factor(year) + trash_pred*factor(city) + trash_pred*residential, family=quasipoisson(link="log"), data=df_full, weights=weight),
  glm(trash_311 ~ offset(log(pop_blkgrp)) + trash_pred*log_income_trt + trash_pred*factor(year) + trash_pred*factor(city) + trash_pred*residential, family=quasipoisson(link="log"), data=df_full, weights=weight),
  glm(trash_311 ~ offset(log(pop_blkgrp)) + trash_pred*pfgnbn_trt + trash_pred*factor(year) + trash_pred*factor(city) + trash_pred*residential, family=quasipoisson(link="log"), data=df_full, weights=weight),
  glm(trash_311 ~ offset(log(pop_blkgrp)) + trash_pred*prenter_trt + trash_pred*factor(year) + trash_pred*factor(city) + trash_pred*residential, family=quasipoisson(link="log"), data=df_full, weights=weight)
)

## Print the models using stargazer
stargazer(models, 
          type = "text",
          keep = c("trash_pred:pnhblk_trt", "trash_pred:phisp_trt", "trash_pred:pasian_trt", "trash_pred:log_income_trt", "trash_pred:pfgnbn_trt", "trash_pred:prenter_trt"),
          covariate.labels = c("% Black × trash prevalence", "% Hispanic × trash prevalence", "% Asian × trash prevalence", "Log median income × trash prevalence", "% Foreign × trash prevalence", "% Renter × trash prevalence"),
          apply.coef = function(x) x*100, # Multiply coefficients by 100
          apply.se = function(x) x*100, # Multiply standard errors by 100
          star.cutoffs = c(0.05, 0.01, 0.001),
          add.lines=list(c('Zoning', 'Yes', 'Yes','Yes', 'Yes'),
                         c('Year', 'Yes', 'Yes','Yes', 'Yes'),
                         c('City', 'Yes', 'Yes','Yes', 'Yes')
          ))

# Table 4: Coefficients for Poisson Models for Neighborhood Change Predicting Logged 311 Requests per Capita ----

## Relevel gentrification: 

df_full$gentri_stage_trt <- relevel(factor(df_full$gentri_stage_trt), ref="nogent")

## List of models
models <- list(
  glm(trash_311 ~ offset(log(pop_blkgrp)) + trash_pred*gentri_stage_trt + trash_pred*factor(year) + trash_pred*factor(city) + trash_pred*residential, family=quasipoisson(link="log"), data=df_full, weights=weight),
  glm(trash_311 ~ offset(log(pop_blkgrp)) + trash_pred*wht_change0a9a_blkgrp + trash_pred*factor(year) + trash_pred*factor(city) + trash_pred*residential, family=quasipoisson(link="log"), data=df_full, weights=weight),
  glm(trash_311 ~ offset(log(pop_blkgrp)) + trash_pred*ses_change0a9a_blkgrp + trash_pred*factor(year) + trash_pred*factor(city) + trash_pred*residential, family=quasipoisson(link="log"), data=df_full, weights=weight)
)

## Print the models using stargazer
stargazer(models, 
          type = "text",
          keep = c("trash_pred:gentri_stage_trt", "trash_pred:wht_change0a9a_blkgrp", "trash_pred:ses_change0a9a_blkgrp"),
          covariate.labels = c("Early-stage gentrification × trash prevalence", "White change × trash prevalence", "SES change × trash prevalence"),
          apply.coef = function(x) x*100, # Multiply coefficients by 100
          apply.se = function(x) x*100, # Multiply standard errors by 100
          star.cutoffs = c(0.05, 0.01, 0.001),
          add.lines=list(c('Zoning', 'Yes', 'Yes','Yes', 'Yes'),
                         c('Year', 'Yes', 'Yes','Yes', 'Yes'),
                         c('City', 'Yes', 'Yes','Yes', 'Yes')
          ))


# FIGURES: ----

# Figure 1: Maps of Block Group-Level Trash Prevalence by City ----

## Make a separate dataframe for each city
df_bos <- df_map %>% filter(city=="Boston") 
df_aus <- df_map %>% filter(city=="Austin")
df_la <- df_map %>% filter(city=="LA")
df_det <- df_map %>% filter(city=="Detroit")
df_philly <- df_map %>% filter(city=="Philly")

## Make maps
bos <- ggplot() + 
  
  # Add a spatial feature layer using the 'df_bos' data frame
  geom_sf(data = df_bos, aes(fill = trash_pred), lwd = 0) +
  
  # Set the fill color scale using the 'trash_pred' variable
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", 
                       breaks=pretty_breaks(n=5)) +
  
  # Apply a custom theme with no axes and a legend
  theme_nothing(legend=TRUE) +
  
  # Set the label for the legend
  labs(fill="Predicted Trash Share")

aus <- ggplot() + 
  geom_sf(data = df_aus, aes(fill = trash_pred), lwd = 0) +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", 
                       breaks=pretty_breaks(n=5)) +
  theme_nothing(legend=TRUE) +
  labs(fill="Predicted Trash Share")

la <- ggplot() + 
  geom_sf(data = df_la, aes(fill = trash_pred), lwd = 0) +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", 
                       breaks=pretty_breaks(n=5)) +
  theme_nothing(legend=TRUE) +
  labs(fill="Predicted Trash Share")

det <- ggplot() + 
  geom_sf(data = df_det, aes(fill = trash_pred), lwd = 0) +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", 
                       breaks=pretty_breaks(n=5)) +
  theme_nothing(legend=TRUE) +
  labs(fill="Predicted Trash Share")

philly <- ggplot() + 
  geom_sf(data = df_philly, aes(fill = trash_pred), lwd = 0) +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", 
                       breaks=pretty_breaks(n=5)) +
  theme_nothing(legend=TRUE) +
  labs(fill="Predicted Trash Share")

combined <- ggpubr::ggarrange(aus, bos, det, la, philly, ncol=3, nrow=2, common.legend = TRUE, legend = "right",
                              labels = c("Austin",
                                         "Boston",
                                         "Detroit",
                                         "Los Angeles",
                                         "Philadelphia"
                              ))


# Figure 2: Distributions of Block Group-Level Per Capita 311 Reports ----

df_full %>% 
  mutate(trash_311_per = trash_311/pop_blkgrp) %>% # Create per capita 311 reports
  ggplot(aes(x = trash_311_per, group=city)) +
  geom_density() +
  theme_bw() + 
  theme(
    axis.title.y = element_text(hjust=0.5, size=20),
    axis.title.x = element_text(size=20),
    strip.text = element_text(size=12)
  ) +
  ylab("Density") +
  xlab("Per Capita 311 Reports") +
  facet_wrap(~city, scales = "free") +
  theme(
    legend.position="none",
    axis.ticks.x=element_blank()
  ) 


# Figure 3: Gentrification Stage Distribution by City ----

df_gent %>% 
  mutate(gentri_stage_trt = factor(gentri_stage_trt, levels = c("nongentrifiable","late", "middle", "early", "nogent"))) %>% 
  ggplot +
  geom_bar(aes(x = factor(city), fill = factor(gentri_stage_trt)), position = "fill") +
  theme_bw() + 
  theme(
    axis.title.y = element_text(hjust=0.5, size=15),
    axis.title.x = element_text(size=15),
    axis.text.x = element_text(size=12),
    legend.text = element_text(size=11)
  ) +
  labs(fill='Gentrification Stage',
       x = "City",
       y = "Proportion") +
  scale_fill_grey(labels = labels) 



# Figure 4: Predicted Reporting Propensities on Demographic Changes by Neighborhood Gentrifiability and Racial Composition ----

# Fit models of interest:
poisson1 <- glm(trash_311 ~ offset(log(pop_blkgrp)) + trash_pred*gentrifiable*ses_change0a9a_blkgrp + trash_pred*factor(year) + trash_pred*city + trash_pred*residential, family=quasipoisson(link="log"), data=df_full, weights=weight)
poisson2 <- glm(trash_311 ~ offset(log(pop_blkgrp)) + trash_pred*gentrifiable*wht_change0a9a_blkgrp + trash_pred*factor(year) + trash_pred*city + trash_pred*residential, family=quasipoisson(link="log"), data=df_full, weights=weight)
poisson3 <- glm(trash_311 ~ offset(log(pop_blkgrp)) + trash_pred*majority_minority*ses_change0a9a_blkgrp + trash_pred*factor(year) + trash_pred*city + trash_pred*residential, family=quasipoisson(link="log"), data=df_full, weights=weight)
poisson4 <- glm(trash_311 ~ offset(log(pop_blkgrp)) + trash_pred*majority_minority*wht_change0a9a_blkgrp + trash_pred*factor(year) + trash_pred*city + trash_pred*residential, family=quasipoisson(link="log"), data=df_full, weights=weight)

# Plot Predicted Values for Model 1:
# Define x values for the plot
x_values <- seq(from = quantile(df_full$ses_change0a9a_blkgrp, 0.05), to = quantile(df_full$ses_change0a9a_blkgrp, 0.95), by = 0.05)

# Extract coefficients for Model 1
line1_intercept <- coef(poisson1)["trash_pred"]
line1_slope <- coef(poisson1)["trash_pred:ses_change0a9a_blkgrp"]
line2_intercept <- coef(poisson1)["trash_pred"] + coef(poisson1)["trash_pred:gentrifiable"]
line2_slope <- coef(poisson1)["trash_pred:ses_change0a9a_blkgrp"] + coef(poisson1)["trash_pred:gentrifiable:ses_change0a9a_blkgrp"]

# Calculate y values for Model 1
line1_y_values <- line1_intercept + line1_slope * x_values
line2_y_values <- line2_intercept + line2_slope * x_values

# Create a data frame for the plot
df <- data.frame(x = c(x_values, x_values),
                 y = c(line1_y_values, line2_y_values),
                 Neighborhood = factor(c(rep("non-gentrifiable", length(x_values)),
                                         rep("gentrifiable", length(x_values)))))

# Create and customize the plot for Model 1
p1 <- ggplot(data = df, aes(x = x, y = y, color = Neighborhood, group = Neighborhood)) +
  geom_line(linewidth=1) +
  coord_cartesian(ylim=c(-0.5,1.5), expand=FALSE) +
  labs(x = "SES change",
       y = "Reporting  propensity") +
  theme_minimal()

# Repeat the process for Models 2, 3, and 4, creating p2, p3, and p4 plots:

## Plot Predicted Values for Model 2: 
x_values <- seq(from = quantile(df_full$wht_change0a9a_blkgrp, 0.05), to = quantile(df_full$wht_change0a9a_blkgrp, 0.95), by = 0.05)

line1_intercept <- coef(poisson2)["trash_pred"]
line1_slope <- coef(poisson2)["trash_pred:wht_change0a9a_blkgrp"]
line2_intercept <- coef(poisson2)["trash_pred"] + coef(poisson2)["trash_pred:gentrifiable"]
line2_slope <- coef(poisson2)["trash_pred:wht_change0a9a_blkgrp"] + coef(poisson2)["trash_pred:gentrifiable:wht_change0a9a_blkgrp"]

line1_y_values <- line1_intercept + line1_slope * x_values
line2_y_values <- line2_intercept + line2_slope * x_values

df <- data.frame(x = c(x_values, x_values),
                 y = c(line1_y_values, line2_y_values),
                 Neighborhood = factor(c(rep("non-gentrifiable", length(x_values)),
                                         rep("gentrifiable", length(x_values)))))

p2 <- ggplot(data = df, aes(x = x, y = y, color = Neighborhood, group = Neighborhood)) +
  geom_line(linewidth=1) +
  coord_cartesian(ylim=c(-0.5,1.5), expand=FALSE) +
  labs(x = "White change",
       y = "Reporting  propensity") +
  theme_minimal()

## Plot Predicted Values for Model 3: 
x_values <- seq(from = quantile(df_full$ses_change0a9a_blkgrp, 0.05), to = quantile(df_full$ses_change0a9a_blkgrp, 0.95), by = 0.05)

line1_intercept <- coef(poisson3)["trash_pred"]
line1_slope <- coef(poisson3)["trash_pred:ses_change0a9a_blkgrp"]
line2_intercept <- coef(poisson3)["trash_pred"] + coef(poisson3)["trash_pred:majority_minority"]
line2_slope <- coef(poisson3)["trash_pred:ses_change0a9a_blkgrp"] + coef(poisson3)["trash_pred:majority_minority:ses_change0a9a_blkgrp"]

line1_y_values <- line1_intercept + line1_slope * x_values
line2_y_values <- line2_intercept + line2_slope * x_values

df <- data.frame(x = c(x_values, x_values),
                 y = c(line1_y_values, line2_y_values),
                 Neighborhood = factor(c(rep("majority-minority", length(x_values)),
                                         rep("majority-white", length(x_values)))))

p3 <- ggplot(data = df, aes(x = x, y = y, color = Neighborhood, group = Neighborhood)) +
  geom_line(linewidth=1) +
  coord_cartesian(ylim=c(-0.5,1.5), expand=FALSE) +
  labs(x = "SES change",
       y = "Reporting  propensity") +
  theme_minimal()

## Plot Predicted Values for Model 4: 
x_values <- seq(from = quantile(df_full$wht_change0a9a_blkgrp, 0.05), to = quantile(df_full$wht_change0a9a_blkgrp, 0.95), by = 0.05)

line1_intercept <- coef(poisson4)["trash_pred"]
line1_slope <- coef(poisson4)["trash_pred:wht_change0a9a_blkgrp"]
line2_intercept <- coef(poisson4)["trash_pred"] + coef(poisson4)["trash_pred:majority_minority"]
line2_slope <- coef(poisson4)["trash_pred:wht_change0a9a_blkgrp"] + coef(poisson4)["trash_pred:majority_minority:wht_change0a9a_blkgrp"]

line1_y_values <- line1_intercept + line1_slope * x_values
line2_y_values <- line2_intercept + line2_slope * x_values

df <- data.frame(x = c(x_values, x_values),
                 y = c(line1_y_values, line2_y_values),
                 Neighborhood = factor(c(rep("majority-minority", length(x_values)),
                                         rep("majority-white", length(x_values)))))

p4 <- ggplot(data = df, aes(x = x, y = y, color = Neighborhood, group = Neighborhood)) +
  geom_line(linewidth=1) +
  coord_cartesian(ylim=c(-0.5,1.5), expand=FALSE) +
  labs(x = "White change",
       y = "Reporting  propensity") +
  theme_minimal()

# Combine all plots into one grid
p_all <- grid.arrange(p2, p1, p4, p3, nrow = 2, ncol = 2)
