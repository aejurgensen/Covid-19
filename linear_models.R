# trying linear models using log-transformed target 

df <- read.csv("Documents/bioinformatics/covid/abbrev.csv", header = TRUE)

summary(df)
df <- df[, !(names(df) %in% c("X"))]
summary(df)
names(df)

library(ggplot2)

ggplot(data=df, aes(death_prc)) + geom_density()
ggplot(data=df, aes(log( nh_nurse_hours))) + geom_density()
ggplot(df, aes(cases_may11/population)) + geom_density()

df["household_size", "population", "density", "cases_march24", "prc_public_transp",
   "ten_plus", "order", "uninsured", "prc_family_poverty", "prc_obese", "pop_65_plus", 
   "incarcerated", "intl_passengers", "domestic_passengers", "death_prc_1K"]

lm <- lm(death_prc ~ household_size + density + empl_agriculture +
           empl_professional + empl_social + empl_services + empl_manufacturing +
           empl_retail + empl_transp_utilities + prc_public_transp + ten_plus + order + 
           uninsured + prc_fam_poverty + prc_obese + pop_65_plus + population + 
           intl_passengers + domestic_passengers + cases_prc, prc_incarcerated +
           immigrant + latino + aa + nh_weighted_health_score + nh_overall_rating +
           nh_prc_occupied + nh_num_beds + nh_nurse_hours + nh_total_fines, 
         data=df)
summary(lm)
plot(lm)
# heteroskedastic? definitely leverage issue


# try some log transformations
df2 <- read.csv("Documents/bioinformatics/covid/logged.csv", header = TRUE)
summary(df2)
lm2 <- lm(death_prc ~ household_size + density + empl_agriculture +
           empl_professional + empl_social + empl_services + empl_manufacturing +
           empl_retail + empl_transp_utilities + prc_public_transp + ten_plus + order + 
           uninsured + prc_fam_poverty + prc_obese + pop_65_plus + population + 
           intl_passengers + domestic_passengers + cases_prc, prc_incarcerated +
           immigrant + latino + aa + nh_weighted_health_score + nh_overall_rating +
           nh_prc_occupied + nh_num_beds + nh_nurse_hours + nh_total_fines, 
         data=df2)
summary(lm2)
plot(lm2)


library(boot)
library(MASS)

#quasipoisson without log-transformed select predictors
mod = glm(formula = deaths ~ household_size + empl_agriculture + empl_professional + empl_social + 
            empl_services + empl_manufacturing + empl_retail + empl_transp_utilities + 
            prc_fam_poverty + avg_income + prc_public_transp + population + pop_65_plus + uninsured +
            area + incarcerated + domestic_passengers + intl_passengers + prc_obese + ten_plus + 
            order + density, family = quasipoisson, data = low)
cv_glm <- cv.glm(low, mod, K = 10)
print(cv_glm$delta)
glm.diag.plots(mod, glm.diag(mod))
print(summary(mod))
plot(mod)

mod = glm(formula = deaths ~ household_size + empl_agriculture + empl_professional + empl_social + 
            empl_services + empl_manufacturing + empl_retail + empl_transp_utilities + 
            prc_fam_poverty + avg_income + prc_public_transp + population + pop_65_plus + uninsured +
            area + incarcerated + domestic_passengers + intl_passengers + prc_obese + ten_plus + 
            order + density, family = quasipoisson, data = high)
cv_glm <- cv.glm(high, mod, K = 10)
print(cv_glm$delta)
glm.diag.plots(mod, glm.diag(mod))
print(summary(mod))
plot(mod)


mod = glm(formula = deaths ~ household_size + prc_fam_poverty + 
            prc_public_transp + population + pop_65_plus + uninsured +
            area + domestic_passengers + intl_passengers + prc_obese + ten_plus + 
            order + density, family = quasipoisson, data = high)
cv_glm <- cv.glm(high, mod, K = 10)
print(cv_glm$delta)
glm.diag.plots(mod, glm.diag(mod))
print(summary(mod))
plot(mod)


# high dispersion since dispersion parameter 
# https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf

#actually respects assumption of linearity for poisson??
# https://bookdown.org/egarpor/PM-UC3M/glm-diagnostics.html



## try GAM
library(mgcv)

#without smoothing
gam1 <- gam(deaths ~ household_size + prc_fam_poverty +  prc_public_transp + 
              population + pop_65_plus + uninsured + domestic_passengers + 
              intl_passengers + prc_obese + ten_plus + order + density, data=high)
gam.check(gam1)
summary(gam1)

# with smoothing
# https://statistique.cuso.ch/fileadmin/statistique/part-3.pdf
gam2 <- gam(deaths ~ s(household_size) + s(prc_fam_poverty) +  s(prc_public_transp) + 
              s(population) + s(pop_65_plus) + s(uninsured) + 
              s(domestic_passengers) + s(intl_passengers) + s(prc_obese) + 
              s(ten_plus) + s(order) + s(density), data=high)
gam.check(gam2)
summary(gam2)

gam3 <- gam(deaths ~ s(household_size) + s(prc_fam_poverty) +  s(prc_public_transp) + 
              s(population) + s(pop_65_plus) + s(uninsured) + 
              s(domestic_passengers) + s(intl_passengers) + s(prc_obese) + 
              s(ten_plus) + s(order) + s(density), data=high, family=poisson)
gam.check(gam3)
summary(gam3)
anova.gam(gam3)




# glm vs gam http://ecology.msu.montana.edu/labdsv/R/labs/lab5/lab5.html

# https://m-clark.github.io/generalized-additive-models/technical.html
anova(gam5a)
AIC(gam1, gam4, gam5a)
df$pred <- predict(gam5a, df)
summary(df[c("deaths", "pred")])

gam0 <- gam(deaths ~ s(household_size, k=4) + s(empl_agriculture, k=3) + s(empl_professional, k=3) + s(empl_social, k=3) + s(prc_fam_poverty, k=3) + 
              s(empl_services, k=3) + s(empl_manufacturing, k=3) + s(empl_retail, k=11) + s(empl_transp_utilities, k=3) + 
              s(avg_income, k=6) + s(prc_public_transp, k=3) + s(population, k=4) + s(pop_65_plus, k=3) +
              s(uninsured, k=3) + s(incarcerated, k=3) + s(domestic_passengers, k=15) + s(intl_passengers, k=6) + 
              s(prc_obese, k=3) + s(ten_plus, k=3) + s(order, k=19) + s(density, k=4) + ti(order, ten_plus) +
              ti(order, density) + ti(order, population), 
            data=df, family=gaussian, gamma=2, method = "GCV.Cp", select=TRUE)
gam.check(gam0)
summary(gam0)
gam0

gam0a <- gam(deaths ~ s(household_size, k=4) + s(empl_agriculture, k=3) + s(empl_professional, k=3) + s(empl_social, k=3) + s(prc_fam_poverty, k=3) + 
               s(empl_services, k=3) + s(empl_manufacturing, k=3) + s(empl_retail, k=11) + s(empl_transp_utilities, k=3) + 
               s(avg_income, k=6) + s(prc_public_transp, k=3) + s(population, k=4) + s(pop_65_plus, k=3) +
               s(uninsured, k=3) + s(incarcerated, k=3) + s(domestic_passengers, k=15) + s(intl_passengers, k=6) + 
               s(prc_obese, k=3) + s(ten_plus, k=3) + s(order, k=19) + s(density, k=4) + ti(order, ten_plus) +
               ti(order, density) + ti(order, population), 
             data=df2, family=gaussian, gamma=2, method = "GCV.Cp", select=TRUE)
gam.check(gam0a)
summary(gam0a)
gam0a


gam0b <- gam(log_deaths ~ s(household_size, k=4) + s(empl_agriculture, k=10) + s(empl_professional, k=3) + s(empl_social, k=3) + s(prc_fam_poverty, k=10) + 
               s(empl_services, k=10) + s(empl_manufacturing, k=3) + s(empl_retail, k=11) + s(empl_transp_utilities, k=3) + 
               s(avg_income, k=10) + s(prc_public_transp, k=3) + s(population, k=4) + s(pop_65_plus, k=3) +
               s(uninsured, k=3) + s(incarcerated, k=20) + s(domestic_passengers, k=20) + s(intl_passengers, k=10) + 
               s(prc_obese, k=120) + s(ten_plus, k=15) + s(order, k=10) + s(density, k=4), #+ ti(order, ten_plus) +
             #ti(order, density) + ti(order, population), 
             data=df2, family=gaussian, gamma=2, method = "GCV.Cp", select=TRUE)
gam.check(gam0b)
summary(gam0b)
gam0b



