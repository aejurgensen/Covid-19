# try dropping NYC (consolidated under New York County)
abbrev <- read.csv("Documents/bioinformatics/covid/data/abbrev.csv", header = TRUE)
abbrev <- abbrev[abbrev$count != "New York County",]

fit_main = lm(deaths ~ household_size + empl_agriculture + empl_professional + empl_social + 
                empl_services + empl_manufacturing + empl_retail + prc_fam_poverty + 
                avg_income + prc_public_transp + population + pop_65_plus + health_ins + 
                domestic_passengers + intl_passengers + ten_plus + order + density, data = abbrev)
summary(fit_main) # adj. r2= 0.456, rse=63.47
plot(fit_main)
# generally looks okay? residuals vs. fitted seems a little too structured, but better than had been

fit = lm(deaths ~ (household_size + empl_agriculture + empl_professional + empl_social + 
                     empl_services + empl_manufacturing + empl_retail + prc_fam_poverty + 
                     avg_income + prc_public_transp + population + pop_65_plus + health_ins + 
                     domestic_passengers + intl_passengers + ten_plus + order + density)^2, data = abbrev)
summary(fit) # adj. r2==0.831, rse=35.32
plot(fit)
# residuals vs. fitted even more structured than main effects model
# not so sure about residuals vs. leverage at the high end of leverage...

# apply backward step function
step <- step(fit, direction=c("backward"))
summary(step)
plot(step) # same issues as fit_main and fit

# copied and pasted step into an excel/odf file for easier searching outside of the notebook
# used "text to columns" function to break into separate columns
# from step, the highest p-value is for 'heath_ins' by itself (p=0.983)
lm_reduced = lm(deaths ~ household_size + empl_agriculture + empl_professional + empl_social + empl_services + empl_manufacturing + empl_retail + 
                  prc_fam_poverty + avg_income + prc_public_transp + population + pop_65_plus + domestic_passengers + intl_passengers + ten_plus + 
                  order + density + household_size:empl_professional + household_size:empl_services + household_size:prc_fam_poverty + 
                  household_size:avg_income + household_size:prc_public_transp + household_size:intl_passengers + household_size:ten_plus +
                  household_size:density + empl_agriculture:empl_professional + empl_agriculture:population + empl_agriculture:pop_65_plus + 
                  empl_agriculture:intl_passengers + empl_agriculture:density + empl_professional:empl_social + empl_professional:empl_manufacturing + 
                  empl_professional:prc_public_transp + empl_professional:population + empl_professional:pop_65_plus + empl_professional:domestic_passengers + 
                  empl_professional:ten_plus + empl_social:prc_fam_poverty + empl_social:population + empl_social:domestic_passengers + 
                  empl_social:intl_passengers + empl_services:prc_public_transp + empl_services:population + empl_services:pop_65_plus + 
                  empl_services:domestic_passengers + empl_services:intl_passengers + empl_manufacturing:prc_public_transp + empl_manufacturing:population + 
                  empl_manufacturing:intl_passengers + empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + 
                  empl_retail:domestic_passengers + empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:population + avg_income:prc_public_transp + avg_income:population + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:density + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:domestic_passengers + prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + 
                  population:ten_plus + population:order + population:density + pop_65_plus:domestic_passengers + pop_65_plus:intl_passengers + 
                  pop_65_plus:ten_plus + pop_65_plus:order + pop_65_plus:density + health_ins:domestic_passengers + domestic_passengers:intl_passengers + 
                  domestic_passengers:ten_plus + domestic_passengers:order + intl_passengers:ten_plus + intl_passengers:order + intl_passengers:density + 
                  ten_plus:density + order:density, data = abbrev)
summary(lm_reduced) # adj. r2= 0.843, rse=34.09

# drop household_size (p=0.625)
lm_reduced = lm(deaths ~ empl_agriculture + empl_professional + empl_social + empl_services + empl_manufacturing + empl_retail + 
                  prc_fam_poverty + avg_income + prc_public_transp + population + pop_65_plus + domestic_passengers + intl_passengers + ten_plus + 
                  order + density + household_size:empl_professional + household_size:empl_services + household_size:prc_fam_poverty + 
                  household_size:avg_income + household_size:prc_public_transp + household_size:intl_passengers + household_size:ten_plus +
                  household_size:density + empl_agriculture:empl_professional + empl_agriculture:population + empl_agriculture:pop_65_plus + 
                  empl_agriculture:intl_passengers + empl_agriculture:density + empl_professional:empl_social + empl_professional:empl_manufacturing + 
                  empl_professional:prc_public_transp + empl_professional:population + empl_professional:pop_65_plus + empl_professional:domestic_passengers + 
                  empl_professional:ten_plus + empl_social:prc_fam_poverty + empl_social:population + empl_social:domestic_passengers + 
                  empl_social:intl_passengers + empl_services:prc_public_transp + empl_services:population + empl_services:pop_65_plus + 
                  empl_services:domestic_passengers + empl_services:intl_passengers + empl_manufacturing:prc_public_transp + empl_manufacturing:population + 
                  empl_manufacturing:intl_passengers + empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + 
                  empl_retail:domestic_passengers + empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:population + avg_income:prc_public_transp + avg_income:population + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:density + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:domestic_passengers + prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + 
                  population:ten_plus + population:order + population:density + pop_65_plus:domestic_passengers + pop_65_plus:intl_passengers + 
                  pop_65_plus:ten_plus + pop_65_plus:order + pop_65_plus:density + health_ins:domestic_passengers + domestic_passengers:intl_passengers + 
                  domestic_passengers:ten_plus + domestic_passengers:order + intl_passengers:ten_plus + intl_passengers:order + intl_passengers:density + 
                  ten_plus:density + order:density, data = abbrev)
summary(lm_reduced) # adj.r2 = 0.843, rse=34.07
plot(lm_reduced) # same issues... don't think these will go away while still using a linear model


final = lm_reduced
summary(final)

pred = fitted.values(final)
summary(pred)

resid = residuals(final)
summary(resid)

compare = abbrev
compare["pred"] <- fitted.values(final)
compare["resid"] <- residuals(final)
head(compare[,c("county", "state", "deaths", "pred", "resid")])

compare[compare$resid > 200,] # Suffolk County, NY and Wayne County, MI both many more deaths than predicted

length(resid[resid > 100]) # only 12 counties
length(resid[resid > 50]) # only 40 counties
compare[compare$resid > 50, c("county", "state", "pred", "deaths", "resid")]

length(resid[resid < -200])
compare[compare$resid < -200,] # Cuyahoga County, OH over-estimated by 254

length(resid[resid < -100]) #9
length(resid[resid< -50]) # 45
compare[compare$resid < -50, c("county", "state", "pred", "deaths", "resid")]

summary(compare[compare$deaths < 100, "resid"])
summary(compare[compare$deaths < 50, "resid"])
summary(compare[compare$deaths < 20, "resid"])
summary(compare[compare$deaths < 10, "resid"])
summary(compare[compare$deaths < 5, "resid"])
# residuals don't get bigger in magnitude with smaller number of deaths in a county, so maybe not more difficult to model than 
# counties with many more deaths?

plot(final)
# residuals vs. leverage looks just fine, BUT
# residuals vs. fitted does not look good and
# from QQ plot see that the data (deaths) is over-dispersed (lots of extreme values at both high and low end of deaths)
# http://www.ucd.ie/ecomodel/Resources/QQplots_WebVersion.html

compare[c(238, 490, 573), c("county", "state", "pred", "deaths", "resid")]
# orange county, ca; suffolk county, ny; wayne county, mi

# look at distribution of deaths --> is it actually normal?
library(ggplot2)
ggplot() + geom_density(data=compare, aes(pred, color="red")) + 
  geom_density(data=compare, aes(deaths)) 


ggplot(abbrev,  aes(x=deaths)) + geom_density()
ggplot(abbrev,  aes(x=log(deaths))) + geom_density()


pred2 = fitted.values(final)
summary(pred2)
pred2[pred2 < 0] <- 0
summary(pred2)
compare = abbrev
compare["pred"] <- pred2
compare["resid"] = compare$deaths - compare$pred  # residual = actual - predicted
summary(compare[,c("deaths", "pred", "resid")])
ggplot() + geom_density(data=compare, aes(pred, color="red")) + 
  geom_density(data=compare, aes(deaths)) # looks better than when negative deaths are predicted...


# counties with significantly lower deaths than predicted
compare[compare$resid < -200, c("county", "state", "pred", "deaths")]
compare[compare$resid < -100, c("county", "state", "pred", "deaths")]
compare[compare$resid < -50, c("county", "state", "pred", "deaths")]
  
# counties with significantly higher deaths than predicted
compare[compare$resid > 400, c("county", "state", "pred", "deaths")]
compare[compare$resid > 200, c("county", "state", "pred", "deaths")]
compare[compare$resid > 100, c("county", "state", "pred", "deaths")]
compare[compare$resid > 50, c("county", "state", "pred", "deaths")]

library(caret)
#vect1 <- c(1, 2, 3)
#vect2 <- c(3, 2, 2)
#res <- caret::postResample(vect1, vect2)
#rsq <- res[2]
caret::postResample(compare$deaths, compare$pred)
# R2=0.843, RMSE = 34.5

summary(final) # adj. r2 = 0.843, rse=34.1
