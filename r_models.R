abbrev <- read.csv("Documents/bioinformatics/covid/data/abbrev.csv", header = TRUE)

fit_main = lm(deaths ~ household_size + empl_agriculture + empl_professional + empl_social + 
                empl_services + empl_manufacturing + empl_retail + prc_fam_poverty + 
                avg_income + prc_public_transp + population + pop_65_plus + health_ins + 
                domestic_passengers + intl_passengers + ten_plus + order + density, data = abbrev)
summary(fit_main)


fit = lm(deaths ~ (household_size + empl_agriculture + empl_professional + empl_social + 
                     empl_services + empl_manufacturing + empl_retail + prc_fam_poverty + 
                     avg_income + prc_public_transp + population + pop_65_plus + health_ins + 
                     domestic_passengers + intl_passengers + ten_plus + order + density)^2, data = abbrev)
summary(fit)

# apply backward step function
step <- step(fit, direction=c("backward"))

summary(step)

# copied and pasted step into an excel/odf file for easier searching outside of the notebook
# used "text to columns" function to break into separate columns
# from step, the highest p-value is for 'order' by itself (p=0.945)
lm_reduced = lm(deaths ~ household_size + empl_agriculture + empl_professional + empl_social + empl_services + empl_manufacturing + 
                  empl_retail + prc_fam_poverty + avg_income + prc_public_transp + population + pop_65_plus + health_ins + 
                  domestic_passengers + intl_passengers + ten_plus + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:avg_income + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:empl_services + empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty + empl_manufacturing:avg_income + 
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + empl_retail:population + 
                  empl_retail:intl_passengers + empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:intl_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# next removed ten_plus (p=.907)
lm_reduced = lm(deaths ~ household_size + empl_agriculture + empl_professional + empl_social + empl_services + empl_manufacturing + 
                  empl_retail + prc_fam_poverty + avg_income + prc_public_transp + population + pop_65_plus + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:avg_income + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:empl_services + empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty + empl_manufacturing:avg_income + 
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + empl_retail:population + 
                  empl_retail:intl_passengers + empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:intl_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# remove pop_65_plus (p=0.312)
lm_reduced = lm(deaths ~ household_size + empl_agriculture + empl_professional + empl_social + empl_services + empl_manufacturing + 
                  empl_retail + prc_fam_poverty + avg_income + prc_public_transp + population + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:avg_income + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:empl_services + empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty + empl_manufacturing:avg_income + 
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + empl_retail:population + 
                  empl_retail:intl_passengers + empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:intl_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop prc_public_transp (p=0.250)
lm_reduced = lm(deaths ~ household_size + empl_agriculture + empl_professional + empl_social + empl_services + empl_manufacturing + 
                  empl_retail + prc_fam_poverty + avg_income + population + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:avg_income + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:empl_services + empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty + empl_manufacturing:avg_income + 
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + empl_retail:population + 
                  empl_retail:intl_passengers + empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:intl_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop empl_manufacturing (p=.203)
lm_reduced = lm(deaths ~ household_size + empl_agriculture + empl_professional + empl_social + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + population + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:avg_income + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:empl_services + empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty + empl_manufacturing:avg_income + 
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + empl_retail:population + 
                  empl_retail:intl_passengers + empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:intl_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop avg_income:empl_manufacturing (p=0.252)
lm_reduced = lm(deaths ~ household_size + empl_agriculture + empl_professional + empl_social + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + population + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:avg_income + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:empl_services + empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + empl_retail:population + 
                  empl_retail:intl_passengers + empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:intl_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop empl_agriculture (p=.227)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_social + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + population + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:avg_income + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:empl_services + empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + empl_retail:population + 
                  empl_retail:intl_passengers + empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:intl_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop avg_income:empl_agriculture (p=0.559)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_social + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + population + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:empl_services + empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + empl_retail:population + 
                  empl_retail:intl_passengers + empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:intl_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop empl_social:empl_services (p=0.183)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_social + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + population + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + empl_retail:population + 
                  empl_retail:intl_passengers + empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:intl_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop empl_social (p=0.184)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + population + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + empl_retail:population + 
                  empl_retail:intl_passengers + empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:intl_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop intl_passengers:pop_65_plus (p=0.162)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + population + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + empl_retail:population + 
                  empl_retail:intl_passengers + empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop empl_retail:intl_passengers (p=0.185)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + population + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp + empl_retail:population + 
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop empl_retail:population (p=0.200)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + population + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop population (p=0.328)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:domestic_passengers + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop domestic_passgners:pop_65_plus (p=.258)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:empl_manufacturing + empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop empl_professional:empl_manufacturing (p=0.260)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:prc_fam_poverty + empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop empl_services:prc_fam_poverty (p=0.275)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + pop_65_plus:density + 
                  domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop density:pop_65_plus (p=0.202)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_services + 
                  empl_retail + prc_fam_poverty + avg_income + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop prc_fam_poverty (p=0.123)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_services + 
                  empl_retail + avg_income + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:pop_65_plus + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop household_size:pop_65_plus (p=0.128)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_services + 
                  empl_retail + avg_income + health_ins + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop health_ins (p=0.151)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:health_ins + household_size:domestic_passengers + 
                  household_size:intl_passengers + household_size:density + empl_agriculture:prc_fam_poverty + 
                  empl_agriculture:population + empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop household_size:health_ins (p=0.899)
lm_reduced = lm(deaths ~ household_size + empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop household_size (p=0.146)
lm_reduced = lm(deaths ~ empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density, data = abbrev)
summary(lm_reduced)

# drop the intercept (p=0.675)
lm_reduced = lm(deaths ~ empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:pop_65_plus + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density - 1, data = abbrev)
summary(lm_reduced)

# drop empl_services:pop_65_plus (p=0.186)
lm_reduced = lm(deaths ~ empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_services:density + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density - 1, data = abbrev)
summary(lm_reduced)

# this model has all variables' coefficients p<=0.1
final_p10 = lm_reduced
summary(final_p10)


# get to all variables' coefficients p<=0.5
# drop empl_services:density (p=0.100)
lm_reduced = lm(deaths ~ empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:prc_fam_poverty + empl_professional:population + 
                  empl_professional:pop_65_plus + empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density - 1, data = abbrev)
summary(lm_reduced)

# drop empl_professional:prc_fam_poverty (p=0.109)
lm_reduced = lm(deaths ~ empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:population + empl_professional:pop_65_plus + 
                  empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + empl_retail:prc_public_transp +  
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density - 1, data = abbrev)
summary(lm_reduced)

# drop empl_retail:prc_public_trans (p=0.130)
lm_reduced = lm(deaths ~ empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:population + empl_professional:pop_65_plus + 
                  empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:domestic_passengers + 
                  empl_services:intl_passengers + empl_manufacturing:prc_fam_poverty +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + 
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density - 1, data = abbrev)
summary(lm_reduced)

# drop prc_fam_poverty:empl_manufacturing
lm_reduced = lm(deaths ~ empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:population + empl_professional:pop_65_plus + 
                  empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:domestic_passengers + empl_services:intl_passengers +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + 
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + prc_fam_poverty:ten_plus + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density - 1, data = abbrev)
summary(lm_reduced)

# drop prc_family_poverty:ten_plus (p=0.151)
lm_reduced = lm(deaths ~ empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  household_size:density + empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:population + empl_professional:pop_65_plus + 
                  empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:domestic_passengers + empl_services:intl_passengers +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + 
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density - 1, data = abbrev)
summary(lm_reduced)

# drop density:household_size (p=0.084)
lm_reduced = lm(deaths ~ empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:population + empl_professional:pop_65_plus + 
                  empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:domestic_passengers + empl_services:intl_passengers +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + 
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers + prc_public_transp:intl_passengers + 
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density - 1, data = abbrev)
summary(lm_reduced)

#drop intl_passengers:prc_public_transp (p=0.066)
lm_reduced = lm(deaths ~ empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:population + empl_professional:pop_65_plus + 
                  empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:domestic_passengers + empl_services:intl_passengers +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + 
                  empl_retail:density + prc_fam_poverty:avg_income + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers +  
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density - 1, data = abbrev)
summary(lm_reduced)

# drop avg_income:prc_fam_poverty (p=0.541)
lm_reduced = lm(deaths ~ empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + empl_professional:empl_social + 
                  empl_professional:population + empl_professional:pop_65_plus + 
                  empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:domestic_passengers + empl_services:intl_passengers +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + 
                  empl_retail:density + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers +  
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density - 1, data = abbrev)
summary(lm_reduced)

# drop empl_professional:empl_social (p=0.058)
lm_reduced = lm(deaths ~ empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + 
                  empl_professional:population + empl_professional:pop_65_plus + 
                  empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:health_ins + empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:domestic_passengers + empl_services:intl_passengers +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + 
                  empl_retail:density + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers +  
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density - 1, data = abbrev)
summary(lm_reduced)

# drop empl_social:health_ins (p=0.0830) (didn't need to copy into spreadsheet: only variable coeff with sig. '.')
lm_reduced = lm(deaths ~ empl_professional + empl_services + empl_retail + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + 
                  empl_professional:population + empl_professional:pop_65_plus + 
                  empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:domestic_passengers + empl_services:intl_passengers +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + 
                  empl_retail:density + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers +  
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density - 1, data = abbrev)
summary(lm_reduced)

# drop empl_retail (p=0.102) (didn't need to paste into spreadsheet: only variable coeff with p>0.05)
lm_reduced = lm(deaths ~ empl_professional + empl_services + avg_income + 
                  domestic_passengers + intl_passengers + density + household_size:empl_professional + 
                  household_size:empl_services + household_size:avg_income + household_size:prc_public_transp + 
                  household_size:population + household_size:domestic_passengers + household_size:intl_passengers + 
                  empl_agriculture:prc_fam_poverty + empl_agriculture:population + 
                  empl_agriculture:domestic_passengers + empl_agriculture:density + 
                  empl_professional:population + empl_professional:pop_65_plus + 
                  empl_professional:domestic_passengers + empl_professional:density + 
                  empl_social:domestic_passengers + empl_social:intl_passengers + 
                  empl_services:population + empl_services:domestic_passengers + empl_services:intl_passengers +  
                  empl_manufacturing:prc_public_transp + empl_manufacturing:population + empl_manufacturing:intl_passengers + 
                  empl_manufacturing:ten_plus + empl_retail:avg_income + 
                  empl_retail:density + prc_fam_poverty:prc_public_transp + 
                  prc_fam_poverty:domestic_passengers + avg_income:prc_public_transp + avg_income:pop_65_plus + 
                  avg_income:domestic_passengers + avg_income:intl_passengers + prc_public_transp:population + prc_public_transp:pop_65_plus + 
                  prc_public_transp:health_ins + prc_public_transp:domestic_passengers +  
                  prc_public_transp:density + population:pop_65_plus + population:domestic_passengers + population:intl_passengers + 
                  population:order + domestic_passengers:intl_passengers + domestic_passengers:ten_plus + intl_passengers:ten_plus + 
                  intl_passengers:density + ten_plus:density + order:density - 1, data = abbrev)
summary(lm_reduced)

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
