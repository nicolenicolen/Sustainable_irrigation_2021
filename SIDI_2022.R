library(ggplot2)
library(tidyverse)
library(plm)
library(broom)
library(WDI)
library(countrycode)
library(data.table)
library(readstata13)
library(viridis)
library(ggthemes)
library(sandwich)
library(lmtest)
library(stargazer)
library(dplyr)
library(readxl)
library(gridExtra)
library(betareg)
library(viridis)
library(ggthemes)
library(maps)
library(rworldmap)
library(mapproj)
library(writexl)
library(scales)
library(rlang)
library(car)
library(caret)
library(glm.predict)
library(sjPlot)

# Load data from Rosa et al (2018) and build SIDI 

SIDI <- read_xlsx('/Users/nicole/Desktop/SIDI_2022/Data/SIDI_2000.xlsx') %>% 
  mutate(countrycode = countrycode(country, 'country.name', 'iso3c'),
         scenario = 'Baseline', 
         year = 2000,
         SIDI = (c_irrigation - c_unsustainable)/(ygc_irrigation - ygc_intensification - ygc_expansion - c_unsustainable),
         c.sustainable = c_irrigation - c_unsustainable,
         ygc.sustainable = ygc_irrigation - ygc_intensification - ygc_expansion - c_unsustainable, 
         c.share.rain = c_rainfed / (c_irrigation + c_rainfed)) %>%
  mutate(rain_s = scale(c.share.rain))

# Load regions 

regions <- read.csv("/Users/nicole/Desktop/SIDI_2022/Data/Regions_WB.csv")
#regions_g <- read_xlsx('/Users/nicole/Desktop/SIDI_2022/Data/Regions_WB_global.xlsx')

# c.share.rain = c_rainfed / (c_irrigation + c_rainfed - c_unsustainable), 

# Load all available socioeconomic variables (SSPs) + standardize them 

# Population 

pop_hist <- read_xlsx('/Users/nicole/Desktop/SIDI_2022/Data/Socioeconomic_variables/Population_historical_WPP.xlsx', skip = 1) %>% 
  rename_all(tolower) %>% 
  select(-model, -variable, -unit) %>% 
  gather(year, population, -'scenario (history)', -region) %>%
  rename('countrycode' = 'region') %>% 
  mutate(year = str_sub(year, end = -2) %>% as.integer())

# GDP  

gdp_hist <- read_xlsx('/Users/nicole/Desktop/SIDI_2022/Data/Socioeconomic_variables/GDP_historical_WDI.xlsx', skip = 1) %>% 
  rename_all(tolower) %>% 
  select(-model, -variable, -unit) %>% 
  gather(year, gdp, -'scenario (history)', -region) %>%
  rename(countrycode = region) %>% 
  mutate(year = str_sub(year, end = -2) %>% as.integer()) %>% 
  left_join(pop_hist, by = c('countrycode', 'year')) %>% 
  mutate(gdppc = gdp/population * 1000) %>% 
  filter(year %in% 1995:2005) %>% 
  group_by(countrycode) %>% 
  mutate(lngdppc = mean(log(gdppc))) %>% 
  ungroup() %>% 
  filter(year == 2000) %>%
  select(-population) %>%
  mutate(gdppc_s = scale(gdppc)) %>%
  select(-`scenario (history).x`) %>%
  select(-`scenario (history).y`)

# Urbanization 

urb_hist <- read_xls('/Users/nicole/Desktop/SIDI_2022/Data/Socioeconomic_variables/Urbanization_historical_WB.xls') %>%
  rename_all(tolower) %>%
  rename('countrycode' = 'country code') %>%
  select(-'indicator name', -'country name') %>%
  group_by(countrycode) %>% 
  mutate(urbanization = mean(urbanization)) %>%
  ungroup() %>%
  filter(year == 2000) %>%
  mutate(urb_s = scale(urbanization))

# Governance 

gov_hist <- read.csv('/Users/nicole/Desktop/SIDI_2022/Data/Socioeconomic_variables/Governance_obs_proj_Andijevic.csv') %>% 
  select(-X) %>% 
  filter(year %in% c("2000", "2005", "2010", "2015")) %>% 
  filter(scenario == "Observed") %>%
  group_by(countrycode) %>% 
  mutate(governance = mean(governance)) %>% 
  select(-scenario) %>%
  ungroup() %>%
  filter(year == 2000) %>%
  mutate(gov_s = scale(governance))

# Gender Inequality 

gii_hist <- read.csv('/Users/nicole/Desktop/SIDI_2022/Data/Socioeconomic_variables/Gender_obs_proj_Andrijevic.csv') %>% 
  select(-X) %>% 
  filter(year %in% 1996:2006) %>% 
  group_by(countrycode) %>% 
  mutate(gii = mean(gii)) %>% 
  ungroup() %>% 
  filter(year == 2000) %>%
  mutate(gii_s = scale(gii))

# Population (average 2000) 

pop_hist <- pop_hist %>%
  filter(year %in% 1996:2006) %>% 
  group_by(countrycode) %>% 
  mutate(population = mean(population)) %>% 
  ungroup() %>% 
  filter(year == 2000) %>%
  mutate(pop_s = scale(population)) %>%
  select(-`scenario (history)`)

# Merge all 

master <- SIDI %>% 
  left_join(pop_hist, countrycode, by=c('countrycode', 'year')) %>% 
  left_join(gdp_hist, countrycode, by = c('countrycode', 'year')) %>% 
  left_join(urb_hist, countrycode, by = c('countrycode', 'year')) %>% 
  left_join(gov_hist, countrycode, by = c('countrycode', 'year')) %>%
  left_join(gii_hist, countrycode, by = c('countrycode', 'year')) %>%
  select(-scenario.x) %>%
  rename(scenario = scenario.y)

# Plot SIDI against socioeconomic variables 

plot(c.share.rain ~ SIDI, pch = 19, col = "black", data = master)
plot(governance ~ SIDI, pch = 19, col = "black", data = master)
plot(gdppc ~ SIDI, pch = 19, col = "black", data = master)
plot(population ~ SIDI, pch = 19, col = "black", data = master)
plot(urbanization ~ SIDI, pch = 19, col = "black", data = master)
plot(gii ~ SIDI, pch = 19, col = "black", data = master)

# Regression analysis (SIDI as dependent and socioeconomic variables ad independent variables)

# Find all possible combinations (called: dredging)

master_dredging <- master %>%
  select(c.share.rain, governance, gdppc, population, urbanization, gii)
  # select(rain_s, gov_s, gdppc_s, pop_s, urb_s, gii_s)           # for standardized 
X = names(master_dredging[,-1])
print(X)
out <- unlist(lapply(1:length(X), function(n) combn(X, n, FUN=function(row) paste0("SIDI ~ ", paste0(row, collapse = "+")))))
print(out)

# Apply all models 

mods = lapply(out, function(frml) lm(frml, data=master))
print(mods)

# Write out models separately (standardized or non-standardized)

m1 <- glm(SIDI ~ c.share.rain, data = master)
summary(m1)
m2 <- glm(SIDI ~ population, data = master)
summary(m2)
m3 <- glm(SIDI ~ gdppc, data = master)   
summary(m3)
m4 <- glm(SIDI ~ urbanization, data = master)
summary(m4)
m5 <- glm(SIDI ~ governance, data = master)
summary(m5) 
m6 <- glm(SIDI ~ gii, data = master)
summary(m6)
m7 <- glm(SIDI ~ c.share.rain+population, data = master)
summary(m7)
m8 <- glm(SIDI ~ c.share.rain+gdppc, data = master)
summary(m8)
m9 <- glm(SIDI ~ c.share.rain+urbanization, data = master)
summary(m9)

# our model of choice 

m10 <- glm(SIDI ~ c.share.rain+governance, data = master)
summary(m10) 

# Assess R^2 
# R2 = 1 - (Residual Deviance/Null Deviance)
# R2 = 1 - (2.3826/9.1084)
# print(R2)

tab_model(m10)
m11 <- glm(SIDI ~ c.share.rain+gii, data = master)
summary(m11)
m12 <- glm(SIDI ~ population+gdppc, data = master)
summary(m12)
m13 <- glm(SIDI ~ population+urbanization, data = master)
summary(m13)
m14 <- glm(SIDI ~ population+governance, data = master)
summary(m14)
m15 <- glm(SIDI ~ population+gii, data = master)
summary(m15)
m16 <- glm(SIDI ~ gdppc+urbanization, data = master)
summary(m16)
m17 <- glm(SIDI ~ gdppc+governance, data = master)
summary(m17)
m18 <- glm(SIDI ~ gdppc+gii, data = master)
summary(m18)
m19 <- glm(SIDI ~ urbanization+governance, data = master)
summary(m19)
m20 <- glm(SIDI ~ urbanization+gii, data = master)
summary(m20)
m21 <- glm(SIDI ~ governance+gii, data = master)
summary(m21)
m22 <- glm(SIDI ~ c.share.rain+population+gdppc, data = master)
summary(m22)
m23 <- glm(SIDI ~ c.share.rain+population+urbanization, data = master)
summary(m23)
m24 <- glm(SIDI ~ c.share.rain+population+governance, data = master)
summary(m24)
m25 <- glm(SIDI ~ c.share.rain+population+gii, data = master)
summary(m25)
m26 <- glm(SIDI ~ c.share.rain+gdppc+urbanization, data = master)
summary(m26)
m27 <- glm(SIDI ~ c.share.rain + gdppc + governance, data = master)
summary(m27)
m28 <- glm(SIDI ~ c.share.rain+gdppc+gii, data = master)
summary(m28)
m29 <- glm(SIDI ~ c.share.rain+urbanization+governance, data = master)
summary(m29)
m30 <- glm(SIDI ~ c.share.rain+urbanization+gii, data = master)
summary(m30)
m31 <- glm(SIDI ~ c.share.rain+governance+gii, data = master)
summary(m31)
m32 <- glm(SIDI ~ population+gdppc+urbanization, data = master)
summary(m32)
m33 <- glm(SIDI ~ population+gdppc+governance, data = master)
summary(m33)
m34 <- glm(SIDI ~ population+gdppc+gii, data = master)
summary(m34)
m35 <- glm(SIDI ~ population+urbanization+governance, data = master)
summary(m35)
m36 <- glm(SIDI ~ population+urbanization+gii, data = master)
summary(m36)
m37 <- glm(SIDI ~ population+governance+gii, data = master)
summary(m37)
m38 <- glm(SIDI ~ gdppc+urbanization+governance, data = master)
summary(m38)
m39 <- glm(SIDI ~ gdppc+urbanization+gii, data = master)
summary(m39)
m40 <- glm(SIDI ~ gdppc+governance+gii, data = master)
summary(m40)
m41 <- glm(SIDI ~ urbanization+governance+gii, data = master)
summary(m41)
m42 <- glm(SIDI ~ c.share.rain+population+gdppc+urbanization, data = master)
summary(m42)
m43 <- glm(SIDI ~ c.share.rain+population+gdppc+governance, data = master)
summary(m43)
m44 <- glm(SIDI ~ c.share.rain+population+gdppc+gii, data = master)
summary(m44)
m45 <- glm(SIDI ~ c.share.rain+population+urbanization+governance, data = master)
summary(m45)
m46 <- glm(SIDI ~ c.share.rain+population+urbanization+gii, data = master)
summary(m46)
m47 <- glm(SIDI ~ c.share.rain+population+governance+gii, data = master)
summary(m47)
m48 <- glm(SIDI ~ c.share.rain+gdppc+urbanization+governance, data = master)
summary(m48)
m49 <- glm(SIDI ~ c.share.rain+gdppc+urbanization+gii, data = master)
summary(m49)
m50 <- glm(SIDI ~ c.share.rain+gdppc+governance+gii, data = master)
summary(m50)
m51 <- glm(SIDI ~ c.share.rain+urbanization+governance+gii, data = master)
summary(m51)
m52 <- glm(SIDI ~ population+gdppc+urbanization+governance, data = master)
summary(m52)
m53 <- glm(SIDI ~ population+gdppc+urbanization+gii, data = master)
summary(m53)
m54 <- glm(SIDI ~ population+gdppc+governance+gii, data = master)
summary(m54)
m55 <- glm(SIDI ~ population+urbanization+governance+gii, data = master)
summary(m55)
m56 <- glm(SIDI ~ gdppc+urbanization+governance+gii, data = master)
summary(m56)
m57 <- glm(SIDI ~ c.share.rain+population+gdppc+urbanization+governance, data = master)
summary(m57)
m58 <- glm(SIDI ~ c.share.rain+population+gdppc+urbanization+gii, data = master)
summary(m58)
m59 <- glm(SIDI ~ c.share.rain+population+gdppc+governance+gii, data = master)
summary(m59)
m60 <- glm(SIDI ~ c.share.rain+population+urbanization+governance+gii, data = master)
summary(m60)
m61 <- glm(SIDI ~ c.share.rain+gdppc+urbanization+governance+gii, data = master)
summary(m61)
m62 <- glm(SIDI ~ population+gdppc+urbanization+governance+gii, data = master)
summary(m62)
m63 <- glm(SIDI ~ c.share.rain+population+gdppc+urbanization+governance+gii, data = master)
summary(m63)

# Assess R^2 for all models 
#with(summary(m63), 1 - deviance/null.deviance)

# Save regression table on Desktop

stargazer(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30,m31,m32,m33,m34,m35,m36,m37,m38,m39,m40,m41,m42,m43,m44,m45,m46,m47,m48,m49,m50,m51,m52,m53,m54,m55,m56,m57,m58,m59,m60,m61,m62,m63, type = "text", title = 'Regression Results', no.space=TRUE, ci = TRUE, out="/Users/nicole/Desktop/regression_table.html")

# Choose model with lowest AIC -> M10 (-176)

# Load projected socioeconomic variables (in this case: governance and c.share.rain)

# Governance 

gov_proj <- read.csv('/Users/nicole/Desktop/SIDI_2022/Data/Socioeconomic_variables/Governance_obs_proj_Andijevic.csv') %>% 
  select(-X) %>% 
  mutate(gov_s = scale(governance)) %>%
  filter(year > 2014)

# Share in rainfed agriculture (manually computed in Excel)

rain_proj <- read_xlsx('/Users/nicole/Desktop/SIDI_2022/Data/SIDI_manual_share_rainfed.xlsx') 

# Merge Data for projections 

proj_data <- gov_proj %>% 
  left_join(rain_proj %>% select(c.share.rain, countrycode, year, scenario), by =c('countrycode','year','scenario'))%>%
  select(-gov_s) %>%
  filter(year %in% seq(2020, 2100, 5)) # we selected only some years for projections 

# Predict SIDI 

Proj = predict.lm(m10, newdata = proj_data, interval = "confidence", level = 0.95)

# Safe confidence intervals in separate dataset

SIDI_proj <- data.frame(proj_data,y=NA,Proj)
#write_xlsx(dat,"/Users/nicole/Desktop/SIDI_confidence_intervals_final.xlsx")

#regions <- read_xlsx("/Users/nicole/Desktop/SIDI_2022/Data/Regions_WB_global.xlsx")

# Average over regions 

Summary <- SIDI_proj %>% 
  full_join(SIDI %>% select(countrycode, country, year, scenario, SIDI, c.share.rain), by = c('countrycode', 'year', 'scenario')) %>%
  full_join(SIDI %>% select(ygc.sustainable, countrycode), by = c('countrycode')) %>%
  full_join(regions, by="countrycode") %>%
  group_by(region, scenario, year) %>% 
  mutate(SIDI_projected_fit = mean(fit, na.rm = T),
         SIDI_projected_upr = mean(upr, na.rm = T), 
         SIDI_projected_lwr = mean(lwr, na.rm = T),
         SIDI_baseline = mean(SIDI, na.rm = T)) %>%
  ungroup() %>% 
  drop_na(region) %>% 
  arrange(year, country)
print(Summary)
#write_xlsx(Summary,"/Users/nicole/Desktop/SIDI_2022/Data/SIDI_projections_region.xlsx")

### FAO analysis ###

#FAO <- SIDI %>%
  #left_join(Summary %>% filter(year == '2020', scenario == 'SSP1') %>% select(countrycode, country,fit), by = c('countrycode')) %>%
  #mutate(change = (fit - SIDI)*100/SIDI)
#write_xlsx(FAO,"/Users/nicole/Desktop/FAO_test.xlsx")

# Plot prediction (Figure 3a) 

f <- function(x){
  format(round(x, 2), nsmall=2)
}

# Change order for plot 

ggplot() +
  geom_point(data = Summary %>% filter(year == 2000), aes(year, SIDI_baseline, color = scenario)) +
  geom_ribbon(data = Summary %>% filter(year > 2019), aes(x = year, y=SIDI_projected_fit, ymin = SIDI_projected_lwr, ymax= SIDI_projected_upr, color = scenario, fill = scenario), alpha = .1, linetype = 0) +
  geom_line(data = Summary %>% filter(year > 2019), aes(year, SIDI_projected_fit, color = scenario), size = 0.8) +
  facet_wrap(~ region, ncol = 3) +
  #facet_grid(~factor(region, levels=c("Global", "East Asia & Pacific", "South Asia", "Central Asia", "North America", "Sub-Saharan Africa", "Europe", "Middle East & North Africa", "Latin America & Caribbean"))) +
  #facet_grid(~fct_relevel(region, "Global", "East Asia & Pacific", "South Asia", "Central Asia", "North America", "Sub-Saharan Africa", "Europe", "Middle East & North Africa", "Latin America & Caribbean")) +
  scale_x_continuous(breaks = c(2000, 2020, 2040, 2060, 2080, 2095),
                     labels = c("2000", "2020", "2040", "2060", "2080", "2100"))+
  #ylim(0.0, 1.0) +
  theme(panel.spacing = unit(0.9, "lines"), strip.text = element_text(size=18)) +
  theme(panel.background = element_blank()) +
  labs(x = 'Year', y = 'SIDI', title = '') +
  scale_color_manual(values=c("Baseline" = "#FF0000", "SSP1" = "darkcyan", "SSP2" = 'saddlebrown', 
                              'SSP3' = 'moccasin', 'SSP4' = "sandybrown", 'SSP5' = 'darkolivegreen')) +
  scale_fill_manual(values=c("Baseline" = "#FF0000", "SSP1" = "darkcyan", "SSP2" = 'saddlebrown', 
                              'SSP3' = 'moccasin', 'SSP4' = "sandybrown", 'SSP5' = 'darkolivegreen'))+
  theme(strip.text = element_text(size=13)) +
  theme(axis.text.x = element_text(angle = 90, size = 13), axis.text.y = element_text(size = 13) ) +
  theme(legend.position= 'right', text = element_text(size=13), plot.title = element_text(hjust = 0.5)) 
  ggsave(file="/Users/nicole/Desktop/Fig3_SIDI_projections_CI_0_1_3rows.pdf", width=10, height=10)

# Plot prediction (Figure 3b)
  
ggplot() +
  geom_point(data = Summary %>% filter(year == 2000), aes(year, SIDI_baseline, color = region)) +
  geom_ribbon(data = Summary %>% filter(scenario == "SSP2"), aes(x = year, y=SIDI_projected_fit, ymin = SIDI_projected_lwr, ymax= SIDI_projected_upr, color = region, fill = region), alpha = .2, linetype = 0) +
  geom_line(data = Summary  %>% filter(scenario == "SSP2"), aes(year, SIDI_projected_fit, color = region), size = 0.8) +
  #facet_wrap(~region, scales = "free") +
  #scale_y_continuous(labels = f) +
  scale_x_continuous(breaks = c(2000, 2020, 2040, 2060, 2080, 2095),
                      labels = c("2000", "2020", "2040", "2060", "2080", "2100"))+
  #scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  ylim(0.0, 1) +
  theme(panel.spacing = unit(0.9, "lines"), strip.text = element_text(size=13)) +
  theme(panel.background = element_blank()) +
  labs(x = 'Year', y = 'SIDI', title = '') +
  theme(axis.text.x = element_text(angle = 90, size = 13), axis.text.y = element_text(size = 13) ) +
  theme(legend.position= 'right', text = element_text(size=13), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("East Asia & Pacific" = "violetred", "South Asia" = "red4", "Central Asia" = 'darkseagreen3', 
                              'North America' = 'sandybrown', 'Sub-Saharan Africa' = "firebrick1", 'Europe' = 'gold2', 
                              'Middle East & North Africa' = 'skyblue4', 'Latin America & Caribbean' = 'mediumpurple4')) +
  scale_fill_manual(values=c("East Asia & Pacific" = "violetred", "South Asia" = "red4", "Central Asia" = 'darkseagreen3', 
                              'North America' = 'sandybrown', 'Sub-Saharan Africa' = "firebrick1", 'Europe' = 'gold2', 
                              'Middle East & North Africa' = 'skyblue4', 'Latin America & Caribbean' = 'mediumpurple4'))
  ggsave(file="/Users/nicole/Desktop/Fig3b_SSP2_projections_CI_0_1.pdf", width=10, height=4)
  
# Global average 

Summary_global <- SIDI_proj %>% 
  full_join(SIDI %>% select(countrycode, country, year, scenario, SIDI, c.share.rain), by = c('countrycode', 'year', 'scenario')) %>%
  full_join(SIDI %>% select(ygc.sustainable, countrycode), by = c('countrycode')) %>%
  full_join(regions, by="countrycode") %>%
  group_by(scenario, year) %>% 
  mutate(SIDI_projected_fit = mean(fit, na.rm = T),
         SIDI_projected_upr = mean(upr, na.rm = T), 
         SIDI_projected_lwr = mean(lwr, na.rm = T),
         SIDI_baseline = mean(SIDI, na.rm = T)) %>%
  ungroup() %>% 
  drop_na(region) %>% 
  arrange(year)
print(Summary_global)

# Plot global (SM)

ggplot() +
  geom_point(data = Summary_global %>% filter(year == 2000), aes(year, SIDI_baseline, color = scenario)) +
  geom_ribbon(data = Summary_global %>% filter(year > 2019), aes(x = year, y=SIDI_projected_fit, ymin = SIDI_projected_lwr, ymax= SIDI_projected_upr, color = scenario, fill = scenario), alpha = .1, linetype = 0) +
  geom_line(data = Summary_global %>% filter(year > 2019), aes(year, SIDI_projected_fit, color = scenario), size = 0.8) +
  scale_x_continuous(breaks = c(2000, 2020, 2040, 2060, 2080, 2095),
                     labels = c("2000", "2020", "2040", "2060", "2080", "2100"))+
  ylim(0.2, 0.7) +
  theme(panel.spacing = unit(0.9, "lines"), strip.text = element_text(size=11)) +
  theme(panel.background = element_blank()) +
  labs(x = 'Year', y = 'SIDI', title = '') +
  scale_color_manual(values=c("Baseline" = "#FF0000", "SSP1" = "darkcyan", "SSP2" = 'saddlebrown', 
                              'SSP3' = 'moccasin', 'SSP4' = "sandybrown", 'SSP5' = 'darkolivegreen')) +
  scale_fill_manual(values=c("Baseline" = "#FF0000", "SSP1" = "darkcyan", "SSP2" = 'saddlebrown', 
                             'SSP3' = 'moccasin', 'SSP4' = "sandybrown", 'SSP5' = 'darkolivegreen'))+
  theme(strip.text = element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 90, size = 9), axis.text.y = element_text(size = 9) ) +
  theme(legend.position= 'bottom', text = element_text(size=12), plot.title = element_text(hjust = 0.5)) 
  #ggsave(file="/Users/nicole/Desktop/SIDI_2022/Figures /Global_SIDI_projections_CI_0_1.pdf")

#### Calories ####

# Analysis (SIDI*ygc_sustainable)

Calories <- Summary %>%
  group_by(region, scenario, year) %>% 
  mutate(calories_baseline = SIDI*ygc.sustainable, 
         calories_projected_fit = fit*ygc.sustainable, 
         calories_projected_lwr = lwr*ygc.sustainable, 
         calories_projected_upr = upr*ygc.sustainable) %>%
  ungroup() %>% 
  drop_na(region) %>% 
  arrange(year, country)
#write_xlsx(Calories,"/Users/nicole/Desktop/SIDI_projected_calories.xlsx")

# Figure 4 projections (percentage change in people fed via sustainable irrigation)

proj.calories.baseline <- Calories %>%
  rename(sustainable_2020 = calories_projected_fit) %>%
  filter(year == '2020') 

proj.calories.future <- Calories %>%
  rename(calories_projected = calories_projected_fit) %>%
  filter(year > '2015')

percentage.calories <- proj.calories.future %>%
  full_join(proj.calories.baseline %>% select(countrycode, scenario, sustainable_2020), by = c('countrycode', 'scenario')) %>%
  mutate(percentage = ((100*(calories_projected - sustainable_2020))/sustainable_2020), na.rm = T)

regional_calories <- percentage.calories %>%
  group_by(region, scenario, year) %>% 
  mutate(avg.cals.percentage = mean(percentage, na.rm = T),
         avg.cals = mean(calories_projected, na.rm = T)) %>%
  ungroup() %>% 
  drop_na(region) %>% 
  arrange(year)

ggplot() +
  geom_line(data = regional_calories, aes(year, avg.cals.percentage, color = scenario), size = 1.6) +
  facet_wrap(~region, scales="free") +
  theme(panel.spacing = unit(0.7, "lines"), strip.text = element_text(size=11)) +
  theme(panel.background = element_blank()) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(x = 'Year', y = 'percentage change (%)') +
  scale_x_continuous(breaks = c(2020, 2040, 2060, 2080, 2100))+
  theme(axis.text.x = element_text(angle = 90, size = 14), axis.text.y = element_text(size = 14) ) +
  theme(legend.position= 'right', text = element_text(size=12), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("Baseline" = "#FF0000", "SSP1" = "darkcyan", "SSP2" = 'saddlebrown', 
                              'SSP3' = 'moccasin', 'SSP4' = "sandybrown", 'SSP5' = 'darkolivegreen'))
#ggsave(file="/Users/nicole/Desktop/Figure_4_calories_projection.pdf", width=9, height=10)

# SSP1 analysis for Figure 4 (people fed per region in 2020, 2050, 2100)

# fit 

SSP1_people_fed_2020 <- Calories %>%
  filter(scenario == 'SSP1', year == '2020') %>%
  group_by(region) %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals_region = sum*10^15, 
         day_region = kcals_region/365, 
         pp_region = day_region/3343) 
#write_xlsx(SSP1_people_fed_2020,"/Users/nicole/Desktop/test.xlsx")
#print(SSP1_people_fed_2020)

SSP1_people_fed_2050 <- Calories %>%
  filter(scenario == 'SSP1', year == '2050') %>%
  group_by(region) %>%
  mutate(sum_region = sum(calories_projected_fit, na.rm = TRUE), 
         kcals_region = sum_region*10^15, 
         day_region = kcals_region/365, 
         pp_region = day_region/3343)
  
SSP1_people_fed_2100 <- Calories %>%
  filter(scenario == 'SSP3', year == '2095') %>%
  group_by(region) %>%
  mutate(sum_region = sum(calories_projected_fit, na.rm = TRUE), 
         kcals_region = sum_region*10^15, 
         day_region = kcals_region/365, 
         pp_region = day_region/3343)

# upper (confidence interval)

SSP1_people_fed_2020_upr <- Calories %>%
  filter(scenario == 'SSP1', year == '2020') %>%
  group_by(region) %>%
  mutate(sum_region = sum(calories_projected_upr, na.rm = TRUE), 
         kcals_region = sum_region*10^15, 
         day_region = kcals_region/365, 
         pp_region = day_region/3343)

SSP1_people_fed_2050_upr <- Calories %>%
  filter(scenario == 'SSP1', year == '2050') %>%
  group_by(region) %>%
  mutate(sum_region = sum(calories_projected_upr, na.rm = TRUE), 
         kcals_region = sum_region*10^15, 
         day_region = kcals_region/365, 
         pp_region = day_region/3343)

SSP1_people_fed_2100_upr <- Calories %>%
  filter(scenario == 'SSP1', year == '2095') %>%
  group_by(region) %>%
  mutate(sum_region = sum(calories_projected_upr, na.rm = TRUE), 
         kcals_region = sum_region*10^15, 
         day_region = kcals_region/365, 
         pp_region = day_region/3343)


# lower (confidence interval)

SSP1_people_fed_2020_lwr <- Calories %>%
  filter(scenario == 'SSP1', year == '2020') %>%
  group_by(region) %>%
  mutate(sum_region = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals_region = sum_region*10^15, 
         day_region = kcals_region/365, 
         pp_region = day_region/3343)

SSP1_people_fed_2050_lwr <- Calories %>%
  filter(scenario == 'SSP1', year == '2050') %>%
  group_by(region) %>%
  mutate(sum_region = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals_region = sum_region*10^15, 
         day_region = kcals_region/365, 
         pp_region = day_region/3343)

SSP1_people_fed_2100_lwr <- Calories %>%
  filter(scenario == 'SSP1', year == '2095') %>%
  group_by(region) %>%
  mutate(sum_region = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals_region = sum_region*10^15, 
         day_region = kcals_region/365, 
         pp_region = day_region/3343)


####################################################

# Table 2 (Sum of people fed globally per SSP)

# SSP1 

SSP1_2020_fit <- Calories %>%
  filter(scenario == 'SSP1', year == "2020") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343) 


SSP1_2050_fit <- Calories %>%
  filter(scenario == 'SSP1', year == "2050") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP1_2100_fit <- Calories %>%
  filter(scenario == 'SSP1', year == "2095") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP1_2020_upr <- Calories %>%
  filter(scenario == 'SSP1', year == "2020") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP1_2050_upr <- Calories %>%
  filter(scenario == 'SSP1', year == "2050") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP1_2100_upr <- Calories %>%
  filter(scenario == 'SSP1', year == "2095") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP1_2020_lwr <- Calories %>%
  filter(scenario == 'SSP1', year == "2020") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP1_2050_lwr <- Calories %>%
  filter(scenario == 'SSP1', year == "2050") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP1_2100_lwr <- Calories %>%
  filter(scenario == 'SSP1', year == "2095") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

# SSP2 

SSP2_2020_fit <- Calories %>%
  filter(scenario == 'SSP2', year == "2020") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP2_2050_fit <- Calories %>%
  filter(scenario == 'SSP2', year == "2050") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP2_2100_fit <- Calories %>%
  filter(scenario == 'SSP2', year == "2095") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP2_2020_upr <- Calories %>%
  filter(scenario == 'SSP2', year == "2020") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP2_2050_upr <- Calories %>%
  filter(scenario == 'SSP2', year == "2050") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP2_2100_upr <- Calories %>%
  filter(scenario == 'SSP2', year == "2095") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP2_2020_lwr <- Calories %>%
  filter(scenario == 'SSP2', year == "2020") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP2_2050_lwr <- Calories %>%
  filter(scenario == 'SSP2', year == "2050") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP2_2100_lwr <- Calories %>%
  filter(scenario == 'SSP2', year == "2095") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

# SSP3 

SSP3_2020_fit <- Calories %>%
  filter(scenario == 'SSP3', year == "2020") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP3_2050_fit <- Calories %>%
  filter(scenario == 'SSP3', year == "2050") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP3_2100_fit <- Calories %>%
  filter(scenario == 'SSP3', year == "2095") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP3_2020_upr <- Calories %>%
  filter(scenario == 'SSP3', year == "2020") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP3_2050_upr <- Calories %>%
  filter(scenario == 'SSP3', year == "2050") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP3_2100_upr <- Calories %>%
  filter(scenario == 'SSP3', year == "2095") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP3_2020_lwr <- Calories %>%
  filter(scenario == 'SSP3', year == "2020") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP3_2050_lwr <- Calories %>%
  filter(scenario == 'SSP3', year == "2050") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP3_2100_lwr <- Calories %>%
  filter(scenario == 'SSP3', year == "2095") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

# SSP4 

SSP4_2020_fit <- Calories %>%
  filter(scenario == 'SSP4', year == "2020") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP4_2050_fit <- Calories %>%
  filter(scenario == 'SSP4', year == "2050") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP4_2100_fit <- Calories %>%
  filter(scenario == 'SSP4', year == "2095") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP4_2020_upr <- Calories %>%
  filter(scenario == 'SSP4', year == "2020") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP4_2050_upr <- Calories %>%
  filter(scenario == 'SSP4', year == "2050") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP4_2100_upr <- Calories %>%
  filter(scenario == 'SSP4', year == "2095") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP4_2020_lwr <- Calories %>%
  filter(scenario == 'SSP4', year == "2020") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP4_2050_lwr <- Calories %>%
  filter(scenario == 'SSP4', year == "2050") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP4_2100_lwr <- Calories %>%
  filter(scenario == 'SSP4', year == "2095") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

# SSP5 

SSP5_2020_fit <- Calories %>%
  filter(scenario == 'SSP5', year == "2020") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP5_2050_fit <- Calories %>%
  filter(scenario == 'SSP5', year == "2050") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP5_2100_fit <- Calories %>%
  filter(scenario == 'SSP5', year == "2095") %>%
  mutate(sum = sum(calories_projected_fit, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP5_2020_upr <- Calories %>%
  filter(scenario == 'SSP5', year == "2020") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP5_2050_upr <- Calories %>%
  filter(scenario == 'SSP5', year == "2050") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP5_2100_upr <- Calories %>%
  filter(scenario == 'SSP5', year == "2095") %>%
  mutate(sum = sum(calories_projected_upr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP5_2020_lwr <- Calories %>%
  filter(scenario == 'SSP5', year == "2020") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP5_2050_lwr <- Calories %>%
  filter(scenario == 'SSP5', year == "2050") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

SSP5_2100_lwr <- Calories %>%
  filter(scenario == 'SSP5', year == "2095") %>%
  mutate(sum = sum(calories_projected_lwr, na.rm = TRUE), 
         kcals = sum*10^15, 
         day = kcals/365, 
         pp = day/3343)

###################################################


