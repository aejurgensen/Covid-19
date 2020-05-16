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
summary(lm) # adj. R2 = 0.9297
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
summary(lm2) # adj.r2 = 0.927
plot(lm2)

# remove non-significant variables one by one
lm2 <- lm(death_prc ~ household_size + density + empl_agriculture +
            empl_professional + empl_social + empl_services + empl_manufacturing +
            empl_retail + prc_public_transp + ten_plus + order + 
            uninsured + prc_fam_poverty + prc_obese + pop_65_plus + population + 
            intl_passengers + domestic_passengers + cases_prc, prc_incarcerated +
            immigrant + latino + aa + nh_weighted_health_score + nh_overall_rating +
            nh_prc_occupied + nh_num_beds + nh_nurse_hours + nh_total_fines, 
          data=df2)
summary(lm2) # adj.r2 = 0.927
plot(lm2)
# leverage problem eliminated


step <- step(lm2, direction=c("backward"))
summary(step)

stepped <- lm(death_prc ~ household_size + density + empl_agriculture + empl_professional +
                empl_social + empl_services + empl_manufacturing + empl_retail + prc_public_transp + 
                ten_plus + order + uninsured + prc_fam_poverty + prc_obese + pop_65_plus + population +
                intl_passengers + domestic_passengers + cases_prc, data=df2)
summary(stepped)
plot(stepped) # adj. r2 = 0.7813
# more heteroskedastic that lm2?

# reduce to only significant variables
stepped <- lm(death_prc ~ density + empl_professional +
                empl_services + prc_public_transp + 
                order + pop_65_plus +
                cases_prc, data=df2)
summary(stepped) # adj. R2 == 0.7828
plot(stepped)
# at least no leverage issues (barely)


# add interaction terms
lm3 <- lm(death_prc ~ household_size + density + empl_agriculture +
            empl_professional + empl_social + empl_services + empl_manufacturing +
            empl_retail + prc_public_transp + order + 
            uninsured + prc_fam_poverty + prc_obese + pop_65_plus + population + 
            intl_passengers + domestic_passengers + prc_incarcerated +
            immigrant + latino + aa + nh_weighted_health_score + nh_overall_rating +
            nh_prc_occupied + nh_num_beds + nh_nurse_hours + nh_total_fines + 
            (cases_prc + ten_plus)^2, 
          data=df2)
summary(lm3) # adj R2 = 0.8052
plot(lm3)

lm3 <- lm(death_prc ~ (household_size + density + empl_agriculture +
            empl_professional + empl_social + empl_services + empl_manufacturing +
            empl_retail + prc_public_transp + order + 
            uninsured + prc_fam_poverty + prc_obese + pop_65_plus + population + 
            intl_passengers + domestic_passengers + prc_incarcerated +
            immigrant + latino + aa + nh_weighted_health_score + nh_overall_rating +
            nh_prc_occupied + nh_num_beds + nh_nurse_hours + nh_total_fines + 
            cases_prc + ten_plus)^2, 
          data=df2)
summary(lm3) # adj R2 = 0.9017
plot(lm3)

step2 <- step(lm3, direction=c("backward"))
summary(step2) #adj r2 = 0.9276 # an increase in adjusted R@, but SO MANY variables. will opt for nearly as good, but more parsimonious model?
plot(step2)
coef(summary(step2))

# because of the very large number of variables, iteratively dropped all with p-value >= 0.1 with each round
# reduced to variables with p<0.1
stepped2 <- lm(death_prc ~ density + prc_fam_poverty + intl_passengers + immigrant + latino + nh_total_fines + domestic_passengers +
                 prc_incarcerated + nh_weighted_health_score + pop_65_plus + nh_overall_rating + ten_plus + household_size*cases_prc +
                 density*empl_services + density*prc_obese + prc_public_transp*empl_professional +
                 empl_professional*uninsured + nh_total_fines*empl_professional + immigrant*empl_social + latino*empl_social +
                 nh_total_fines*empl_social + empl_services*empl_retail + prc_fam_poverty*empl_services +
                 empl_manufacturing*population + empl_manufacturing*nh_num_beds + pop_65_plus*prc_public_transp +
                 immigrant*prc_public_transp + prc_public_transp*aa + nh_nurse_hours*prc_public_transp + population*order +
                 nh_num_beds*order + cases_prc*order + prc_fam_poverty*pop_65_plus + prc_fam_poverty*immigrant +
                 prc_obese*population + nh_weighted_health_score*prc_obese + prc_obese*nh_num_beds + nh_total_fines*pop_65_plus +
                 pop_65_plus*cases_prc + prc_incarcerated*population + cases_prc*population + intl_passengers*domestic_passengers +
                 intl_passengers*prc_incarcerated + intl_passengers*latino + 
                 intl_passengers*nh_nurse_hours + domestic_passengers*cases_prc + prc_incarcerated*aa + prc_incarcerated*cases_prc +
                 immigrant*cases_prc + immigrant*ten_plus + latino*cases_prc + latino*ten_plus + cases_prc*nh_num_beds   -
                 (nh_num_beds + household_size + empl_services + empl_retail + prc_obese + empl_agriculture + prc_public_transp +
                    empl_professional + uninsured + empl_social + empl_manufacturing + population + aa + order +
                    nh_prc_occupied + nh_nurse_hours), 
               data=df2)
coef(summary(stepped2))
summary(stepped2) # adj R2 on 635 dof = 0.9243->adj. R2 on 668 dof->0.9224, adj.R2 on 693 dof->0.9214, 
                  # adj.R2 on 701 dof: 0.8883, adjR2 on 724 dof->0.8707, adj.R2 on 739 dof->0.8688. 
                  # adj.R2 on 753 dof->0.8688, adj.R2 on 762 def->0.866, adjR2 on 770 dof->0.8658
                  # adj.R2 on 771 dof->0.8655, adjR2 on 772 dof->0.8652
plot(stepped2)
# leverage issue?


#reduced further to all variables with p<0.05
stepped3 <- lm(death_prc ~ density + prc_fam_poverty + intl_passengers + immigrant + latino + nh_total_fines + domestic_passengers +
                 nh_weighted_health_score + pop_65_plus + ten_plus + household_size*cases_prc + density*empl_services + 
                 density*prc_obese + prc_public_transp*empl_professional + empl_professional*uninsured + 
                 nh_total_fines*empl_professional + immigrant*empl_social + latino*empl_social + nh_total_fines*empl_social +
                 empl_services*empl_retail + prc_fam_poverty*empl_services + empl_manufacturing*population + 
                 empl_manufacturing*nh_num_beds + immigrant*prc_public_transp +
                 prc_public_transp*aa + population*order + nh_num_beds*order + 
                 cases_prc*order + prc_fam_poverty*pop_65_plus + prc_fam_poverty*immigrant + prc_obese*population +
                 nh_weighted_health_score*prc_obese + prc_obese*nh_num_beds + nh_total_fines*pop_65_plus + 
                 pop_65_plus*cases_prc + cases_prc*population + intl_passengers*domestic_passengers + 
                 intl_passengers*prc_incarcerated + intl_passengers*latino + 
                 domestic_passengers*cases_prc + prc_incarcerated*cases_prc + immigrant*cases_prc + immigrant*ten_plus + 
                 latino*cases_prc + latino*ten_plus + cases_prc*nh_num_beds  -
                 (nh_num_beds + household_size + empl_services + empl_retail + prc_obese + empl_agriculture + prc_public_transp +
                    empl_professional + uninsured + empl_social + empl_manufacturing + population + aa + order +
                    nh_prc_occupied + nh_nurse_hours + prc_incarcerated + nh_overall_rating + ten_plus), 
               data=df2)
summary(stepped3)  # Adj.R2 on 776 dof->0.8641, AdjR2 with 777 dof->0.8636, AdjR2 with 780 dof->0.8629
plot(stepped3)
# fit isn't any better, and also has leverage issues; NOT PARSIMONIOUS and difficult to interpret



