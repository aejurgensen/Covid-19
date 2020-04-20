# try dropping NYC (consolidated under New York County)
abbrev <- read.csv("Documents/bioinformatics/covid/data/abbrev.csv", header = TRUE)
abbrev <- abbrev[abbrev$count != "New York County",]

summary(abbrev)
abbrev <- replace(abbrev, abbrev == 0, 0.0000001)
summary(abbrev)

fit_main = glm(deaths ~ household_size + empl_agriculture + empl_professional + empl_social + 
                empl_services + empl_manufacturing + empl_retail + prc_fam_poverty + 
                avg_income + prc_public_transp + population + pop_65_plus + health_ins + 
                domestic_passengers + intl_passengers + ten_plus + order + density, 
               family=Gamma(link=log), data = abbrev)
summary(fit_main) # adj. r2= 0.456, rse=63.47
plot(fit_main) 
# residuals vs. leverage looks better than linear model, anyway
# residuals vs. fitted still wonky

fit = glm(deaths ~ (household_size + empl_agriculture + empl_professional + empl_social + 
                      empl_services + empl_manufacturing + empl_retail + prc_fam_poverty + 
                      avg_income + prc_public_transp + population + pop_65_plus + health_ins + 
                      domestic_passengers + intl_passengers + ten_plus + order + density)^2, 
          family=Gamma(link=log), data=abbrev)
summary(fit) #adj. r2 = 0.831, rse=35.32
plot(fit) # looks about the same as the linear model... 

summary(fitted.values(fit))

library(ggplot2)
fitted = data.frame(fitted.values(fit))
names(fitted) = c("values")
residuals = data.frame(residuals(fit))
names(residuals) = c("resid")
ggplot(fitted, aes(values)) + geom_density()
ggplot(abbrev, aes(deaths)) + geom_density()
ggplot(residuals, aes(resid)) + geom_density()  
# residuals look pretty normally distributed (but long tails, as seen in QQplot?)

ggplot() + geom_density(data=fitted, aes(values, color="red")) + 
  geom_density(data=abbrev, aes(deaths)) 
# if the model only made the neg. predicted values 0, these would match pretty well
# SAME as the plot for linear model in r_models_noNyc.R



