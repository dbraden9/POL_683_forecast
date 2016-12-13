## Sample logit and probit code ##

## Generate data ##
X<-rnorm(1000,0,1)
y_latent<-1+1*X+rnorm(1000,0,1)
y_observed<-rep(NA, 1000)
for(i in 1:1000)
  y_observed[i]<-rbinom(1, 1, plogis(y_latent[i]))
Y<-y_observed

## estimate a logit model using above data ##
logit_test<-glm(Y~X,family=binomial(link="logit"))
probit_test<-glm(Y~X,family=binomial(link="probit"))

## summary of logit model ##
summary(logit_test)

## summary of probit model ##
summary(probit_test)

## predicted probability for logit_test at level 0 ##
cat("Predicted probability:", plogis(logit_test$coef[1]+logit_test$coef[2]*0))

## predicted probability for a logit_test at level 1 ##
cat("Predicted probability:", plogis(logit_test$coef[1]+logit_test$coef[2]*1))

## generate a plot of predicted probabilities at varying levels of y latent ##
x_sim<-seq(min(X), max(X), by=0.01)
plot(x_sim, plogis(logit_test$coef[1]+logit_test$coef[2]*x_sim),
     xlab="X values", ylab="Predicted Probabilities")

## Odds ratios for logit_test model (these are hard to interpret probably dont use) ##
exp(coef(logit_test))

## Discrete changes in predicted probabilities ##
cat("Change in Predicted probability:", plogis(logit_test$coef[1]+logit_test$coef[2]*1)-plogis(logit_test$coef[1]+logit_test$coef[2]*0))

###################################################### Terrorism as a logit ########################################################################
## load in GTD data, and COW systems list ##
setwd("~/Desktop/Volgy Project Materials/")
load("terror_years.Rdata")
require(readstata13)
require(MASS)
require(pscl)
require(lattice)
require(aod)
require(ggplot2)
require("effects")
require(VIM)
require(mice)
require(Hmisc)
require(dplyr)
require(missForest)
require(stargazer)
gtd_70_91<-read.dta13("gtd_70to91_0616dist.dta")
gtd_92_11<-read.dta13("gtd_92to11_0616dist.dta")
gtd_93<-read.dta13("gtd1993_0616dist.dta")
gtd_12_15<-read.dta13("gtd_12to15_0616dist.dta")
final_systems_list<-read.csv("systems_list_revised.csv")
polity_IV<-read.csv("polity_IV_2015.csv")
gdp_capita<-read.csv("final_gdp_capita.csv")
world_pop<-read.csv("final_world_bank_pop.csv")
aid_3.0<-read.dta13("AidData3.0DyadicRelationships.dta")
ucdp<-read.csv("ucdp_civil_war.csv")
aid_raw<-read.dta13("DyadicRelationships.dta") ## older aid data (2.1)

## some simple histograms of non-logged vs. logged GDP per capita ##
hist(gdp_capita$gdppc, breaks=50, xlim=c(0,65000), main="Histogram of Non-Logged GDP per Capita", xlab="GDP per Capita", ylab="Frequency")

hist(log(gdp_capita$gdppc), main="Histogram of Logged GDP per Capita", xlab="Log GDP per Capita", ylab="Frequency")

## some simple histograms of non-logged vs. logged world population ##
hist(rev_world_pop$population, breaks=50, xlim=c(0,315000000), main="Histogram of Non-Logged World Population", xlab="World Population", ylab="Frequency")

hist(log(rev_world_pop$population), main="Histogram of Logged World Population", xlab="World Population", ylab="Frequency")

## truncate final_systems_list years from 1970 to 2013, there is no terrorism data before 1970 and aid data stops at 2013 ##
terror_years <- final_systems_list[which(final_systems_list$year %in% c(1970:2013)),]

## create empty vector to populate with num_attacks ##
num_attacks <- NULL

## for loop counting the number of attacks in a country in a given year. Will need to do this for all 4 GTD data sets ##
for(i in 1:nrow(terror_years))
{
  attacks <- nrow(gtd_70_91[which(gtd_70_91$ccode == terror_years$ccode[i] & gtd_70_91$iyear == 
                                    terror_years$year[i] ), ] )
  
  attacks_1 <- nrow(gtd_92_11[which(gtd_92_11$ccode == terror_years$ccode[i] & gtd_92_11$iyear == 
                                      terror_years$year[i] ), ] )
  
  attacks_2 <- nrow(gtd_93[which(gtd_93$ccode == terror_years$ccode[i] & gtd_93$iyear == 
                                   terror_years$year[i] ), ] )
  
  attacks_3 <- nrow(gtd_12_15[which(gtd_12_15$ccode == terror_years$ccode[i] & gtd_12_15$iyear == 
                                      terror_years$year[i] ), ] )
  
  num_attacks[i]<-attacks+attacks_1+attacks_2+attacks_3
}
## end for

## attach num_attacks to terror_years ##
terror_years$num_attacks <- num_attacks

## if else to populate terror_dummy with 1 for positive incident of terrorism and 0 for no incident of terrorism ##
terror_years$terror_dummy<-ifelse(terror_years$num_attacks>0,1,0)

## if else to create a post cold war dummy variable, less than or equal to 1989 gets a 0, greater than 1989 gets a 1 ##
terror_years$post_cold_war<-ifelse(terror_years$year<=1989,0,1)

## if else to create a civil war dummy variable, greater than or equal to 3 gets a 1, otherwise a 0 ##
terror_years$civil_war<-ifelse(terror_years$civil_intensity<3,0,1)

## assign 0's to NA's in terror_year civil_war because we only coded for 3's and 4's from ucdp whicg represent civil wars ##
terror_years$civil_war[which(is.na(terror_years$civil_war) == T) ] <- 0

## removing civil_intensity from terror_years as it was simply a column needed to create a dummy for civil_war ##
drops<-c("civil_intensity")
terror_years<-terror_years[, !(names(terror_years) %in% drops)]

## for loop to attach polity2 from polity IV to terror_years ##
terror_years$polity_score <- NA

for(i in 1:nrow(terror_years)) # go through every row of what I'd like to add on to
{
  temp<-polity_IV$polity2[which(polity_IV$ccode==terror_years$ccode[i] & polity_IV$year==
                                  terror_years$year[i] ) ] # looking through polity annual data set and pulling out polity 2 scores
  # where country code equals terror_years ccode, and polity annual year equals terror_years year
  if(length(temp)==1) # since pre populated with "NA's" we need to only pull data with a value
  {
    terror_years$polity_score[i] <- temp # attach/assign temp to major_power
  }
}
# end for

## for loop to attach civil war conflict intensity ##
terror_years$civil_intensity <- NA

## A for loop adding in population for each country ##
for(i in 1:nrow(terror_years)) # go through every row of what I'd like to add on to
{
  temp_13<-ucdp$TypeOfConflict[which(ucdp$COW==terror_years$ccode[i] & ucdp$Year==
                                              terror_years$year[i] ) ] # looking through ucdp data set and pulling out conflict intensity
  # where ccode equals target ccode in terror_years, and ucdp year equals terror_year year
  if(length(temp_13)==1) # since pre populated with "NA's" we need to only pull data with a value
  {
    terror_years$civil_intensity[i] <- temp_13 # attach/assign temp7 to terror_years
  }
  if(length(temp_13) > 1)
  {
    terror_years$civil_intensity[i] <- max(temp_13)
  } #if
} # close for

## eliminate 2016, 2015, 2014 from world population as my data ends at 2013 ##
rev_world_pop <- world_pop[which(world_pop$year %in% c(1970:2013)),]

## take the natural log of population to correct for skew ##
range(rev_world_pop$population,na.rm=TRUE)
min(rev_world_pop$population,na.rm=TRUE)
log_rev_world_pop<-NULL
log_world_pop<-log(rev_world_pop$population)
rev_world_pop$log_world_pop<-log_world_pop

## for loop to attach world population ##
terror_years$log_world_pop <- NA

## A for loop adding in population for each country ##
for(i in 1:nrow(terror_years)) # go through every row of what I'd like to add on to
{
  temp_7<-rev_world_pop$log_world_pop[which(rev_world_pop$ccode==terror_years$ccode[i] & rev_world_pop$year==
                                   terror_years$year[i] ) ] # looking through world_pop data set and pulling out log population
  # where ccode equals target ccode in terror_years, and world_pop year equals terror_year year
  if(length(temp_7)==1) # since pre populated with "NA's" we need to only pull data with a value
  {
    terror_years$log_world_pop[i] <- temp_7 # attach/assign temp7 to terror_years
  }
  if(length(temp_7) > 1)
  {
    terror_years$log_world_pop[i] <- max(temp_7)
  } #if
} ## close for

## take the natural log of aid_3.0 commit value to correct for skew ##
range(aid_3.0$commitment_amount_usd_constant_s,na.rm=TRUE)
min(aid_3.0$commitment_amount_usd_constant_s,na.rm=TRUE)
log_aid_received<-NULL
log_aid_received<-log(aid_3.0$commitment_amount_usd_constant_s)
aid_3.0$log_aid_received<-log_aid_received

## histogram of non-logged aid received ##
hist(aid_3.0$commitment_amount_usd_constant_s, breaks=75, main="Histogram of Non-Logged Foreign Aid Received", xlab="Foreign Aid Received", ylab="Frequency")

hist(log(aid_3.0$commitment_amount_usd_constant_s), breaks=50, main="Histogram of Logged Foreign Aid Received", xlab="Logged Foreign Aid Received", ylab="Frequency")

## for loop to attach log_aid received ##
terror_years$log_aid_received <- NA

## A for loop adding in aid received for each target country ##
for(i in 1:nrow(terror_years)) # go through every row of what I'd like to add on to
{
  temp_6<-aid_3.0$log_aid_received[which(aid_3.0$recipient_cc==terror_years$ccode[i] & aid_3.0$year==
                                       terror_years$year[i] ) ] # looking through aid_3.0 data set and pulling out log commitment amount
  # where recepient ccode equals target ccode in terror_years, and aid_3.0 year equals terror_year year
  if(length(temp_6)==1) # since pre populated with "NA's" we need to only pull data with a value
  {
    terror_years$log_aid_received[i] <- temp_6 # attach/assign temp6 to terror_years
  }
  if(length(temp_6) > 1)
  {
    terror_years$log_aid_received[i] <- max(temp_6)
  } #if
} ## close for

## replacing NA values in terror_years log aid received with a zero because aid 3.0 only reports positive values and not zeros
## AND because NOT all countries on systems list receieve aid each year ##

## replace NA's with 0 ##
terror_years$log_aid_received[which(is.na(terror_years$log_aid_received) == T) ] <- 0

## confirm no NA's ##
sum(is.na(aid_3.0$commitment_amount_usd_constant_s))

## decade code to add in regions, which I will not use because the 1980's are entirely missing ##
terror_years$decade <- NA
for(i in 1:nrow(terror_years))
{
  if(terror_years$year[i] %in%  c(1970:1979))
  {
    dec <- 1970
  } else if(terror_years$year[i] %in% c(1980:1989))
  {
    dec <- 1980
  } else if(terror_years$year[i] %in% c(1990:1999))
  {
    dec <- 1990
  } else if(terror_years$year[i] %in% c(2000:2010))
  {
    dec <- 2000
  } else if(terror_years$year[i] %in% c(2010:2020))
  {
    dec <- 2010
  }
  #else if
  terror_years$decade[i]<-dec
}

## taking log of gdp_capita to correct for skew ##
range(gdp_capita$gdppc,na.rm=TRUE) ## notice there is a zero
## add +1 before logging ##
gdp_capita$gdp_capita_corrected<-gdp_capita$gdppc+1
log_gdppc<-NULL
log_gdppc<-log(gdp_capita$gdp_capita_corrected)
gdp_capita$log_gdppc<-log_gdppc

## Creating a GDP per capita variable in terror_years
terror_years$log_gdp_capita <- NA

## A for loop adding in gdp per capita for each country ##
for(i in 1:nrow(terror_years)) # go through every row of what I'd like to add on to
{
  temp_9<-gdp_capita$log_gdppc[which(gdp_capita$ccode==terror_years$ccode[i] & gdp_capita$year==
                                        terror_years$year[i] ) ] # looking through gdp_capita data set and pulling out log gdp per capita
  # where recipient cc equals recipient cc in terror_years, and gdp_capita year equals terror_year year
  if(length(temp_9)==1) # since pre populated with "NA's" we need to only pull data with a value
  {
    terror_years$log_gdp_capita[i] <- temp_9 # attach/assign temp9 to terror_years
  }
  if(length(temp_9) > 1)
  {
    terror_years$log_gdp_capita[i] <- max(temp_9)
  } #if
} ## close for

############################# imputing the missing values in terror_years ################################################################################

## mice pattern of missing data ##
md.pattern(terror_years)

## histogram of missing data ##
aggr_plot<-aggr(terror_years, col=c("navyblue", "red"),
                numbers=TRUE, sortVars=TRUE, labels=names(terror_years),
                cex.axis=.7, gap=3, ylab=c("Histogram of Missing Data", "Pattern"))

## subset out cccode and polity_score which I do NOT want to impute ##
temp_impute<-terror_years[c(1,3,4,5,6,7,9,10,11)]
temp_ccode_polity<-terror_years[c(2,8)]

## run imputation using "cart" method to avoid error message from not invertable matrices ##
imp_temp_impute<-mice(temp_impute, m=5, method="cart", seed=500)
summary(imp_temp_impute)
imp_temp_impute$imp$log_world_pop
imp_temp_impute$imp$log_gdp_capita
nrow(imp_temp_impute$imp$log_world_pop)
nrow(imp_temp_impute$imp$log_gdp_capita)

## take third observation of imputed data and put them into terror_years ##
final_temp_impute<-complete(imp_temp_impute,2)

## cbind ccode and polity to imputed data set ##
imputed_terror_years<-cbind(final_temp_impute,temp_ccode_polity)

## check to see that log_pop and log_gdp_capita weere actually imputed ##
sum(is.na(imputed_terror_years$log_world_pop)) ## equals 0
sum(is.na(imputed_terror_years$log_gdp_capita)) ## equals 0

## a density plot of observed and imputed data. Magenta=imputed and blue=observed ##
densityplot(imp_temp_impute)

## adding in lagged DV to terror_years ##

imputed_terror_years$lag_attacks <- NA

## lag code for num_attacks ##
for (i in 1:nrow(imputed_terror_years))
{
  lag <-imputed_terror_years$num_attacks[which(imputed_terror_years$ccode==imputed_terror_years$ccode[i] & imputed_terror_years$year==imputed_terror_years$year[i]-1) ]
  if(length(lag)==1)
  {
    imputed_terror_years$lag_attacks[i] <-lag
  } ## close if
} ## close for

## adding in a lagged aid variable to terror_years ##

imputed_terror_years$lag_aid <- NA

## lag code for num_attacks ##
for (i in 1:nrow(imputed_terror_years))
{
  lag1 <-imputed_terror_years$log_aid_received[which(imputed_terror_years$ccode==imputed_terror_years$ccode[i] & imputed_terror_years$year==imputed_terror_years$year[i]-1) ]
  if(length(lag1)==1)
  {
    imputed_terror_years$lag_aid[i] <-lag1
  } ## close if
}

## trying a logit with terror_dummy as DV and polity score as IV ##
m1<-glm(terror_dummy~polity_score,data=imputed_terror_years,family=binomial(link="logit"))
summary(m1)
vcov(m1)

## table of polity scores ##
table(terror_years$polity_score)

## looking to see what the median of polity_score is ##
median(terror_years$polity_score,na.rm=TRUE)

## holding polity_score at median (3) and seeing predicted probability ##
cat("Predicted Probability:",plogis(m1$coef[1]+m1$coef[2]*3))

## taking polity_score to -7 (2nd most occuring value) and looking at effect on predicted probability ##
cat("Predicted Probability:",plogis(m1$coef[1]+m1$coef[2]*-7))

## taking polity_score to 10 (which is mode) and looking at effect on predicted probability ##
cat("Predicted Probability:",plogis(m1$coef[1]+m1$coef[2]*10))

## trying another logit with only population as IV ##
m1b<-glm(terror_dummy~log_world_pop, data=imputed_terror_years,family=binomial(link="logit"))
summary(m1b)

## hold world_pop at its median and see the predicted probability ##
cat("Predicted Probability:",plogis(m1b$coef[1]+m1b$coef[2]*15.65))

## hold worl_pop at its maximum (India) in 2013 (20.98563) and see probability of attack ##
cat("Predicted Probability:",plogis(m1b$coef[1]+m1b$coef[2]*20.98563))

## trying another logit with only GDP per capita as the IV ##
m1c<-glm(terror_dummy~log_gdp_capita, data=imputed_terror_years,family=binomial(link="logit"))
summary(m1c)

## hold gdp_capita at its maximum (Monaco) in 2011 ($163,353 or 12.00 logged) and see probability of an attack ##
cat("Predicted Probability:",plogis(m1c$coef[1]+m1c$coef[2]*12.00))

## trying another logit with terror_dummy as DV and both polity_score and gdp_capita as IV's ##
m2<-glm(terror_dummy~polity_score+log_gdp_capita,data=imputed_terror_years,family=binomial(link="logit"))
summary(m2)

## median of gdp_capita ##
median(imputed_terror_years$log_gdp_capita)

## holding polity_score at median (3) and log gdp_capita at median and seeing predicted probability ##
cat("Predicted Probability:",plogis(m2$coef[1]+m2$coef[2]*3+m2$coef[3]*7.56))

## hold polity_score at median (3) and log_gdp_capita at 9.21 (2015 world average) and seeing the predicted probability ##
cat("Predicted Probability:",plogis(m2$coef[1]+m2$coef[2]*3+m2$coef[3]*9.21))

## trying another logit with terror_dummy as DV and polity_score, log_gdp_capita, and lag_attacks as IV's ##
m3<-glm(terror_dummy~polity_score+log_gdp_capita+lag_attacks,data=imputed_terror_years,family=binomial(link="logit"))
summary(m3)

## mean of lag_attacks ##
median(imputed_terror_years$lag_attacks,na.rm=TRUE)
table(imputed_terror_years$lag_attacks)

## holding polity_score at median (3) and gdp_capita at median (7.647), and lag_attacks at median (0) and seeing predicted probability ##
cat("Predicted Probability:",plogis(m3$coef[1]+m3$coef[2]*3+m3$coef[3]*7.56+m3$coef[4]*0))

#################################################################Code I may not need ######################################################
## creating a new data frame specifying the values we want our predictor variables to take ##
pred_prob_1<-with(terror_years,data.frame(polity_score=median(polity_score,na.rm=TRUE), log_gdp_capita=mean(log_gdp_capita), lag_attacks=mean(lag_attacks,na.rm=TRUE)))

## generating predicted probabilities to put into pred_prob_1 ##
pred_prob_1$pred_probs_mean<-predict(m3, newdata=pred_prob_1, type="response")

## creating a new data frame varying the values of polity_score and lag_attacks but holding gdp_capita constant ##
pred_prob_2<-with(terror_years, data.frame(polity_score=rep(seq(from=-10, to=10,length.out=100),1), gdp_capita=mean(gdp_capita,na.rm=TRUE),
                                           lag_attacks=mean(lag_attacks,na.rm=TRUE)))
###############################################################################################################################################################

## run a logit with just my IV of interest (log_aid_received)
m4<-glm(terror_dummy~log_aid_received, data=imputed_terror_years, family="binomial"(link="logit"))
summary(m4)

## median of log_aid_received ##
median(imputed_terror_years$log_aid_received) ## 18.66

## generate a predicted probability when holding log_aid_received at median (18.66) ##
cat("Predicted Probability:", plogis(m4$coef[1]+m4$coef[2]*18.66))

## increase the median value to 25 ##
cat("Predicted Probability:", plogis(m4$coef[1]+m4$coef[2]*25))

## various median values ##
median(imputed_terror_years$log_world_pop) ## 15.65
median(imputed_terror_years$log_gdp_capita) ## 7.46
range(imputed_terror_years$lag_attacks,na.rm=TRUE)
table(imputed_terror_years$lag_attacks)
median(imputed_terror_years$lag_attacks,na.rm=TRUE) ## 0
median(imputed_terror_years$lag_aid,na.rm=TRUE) ## 18.95
median(imputed_terror_years$log_aid_received) ## 18.66
median(imputed_terror_years$polity_score,na.rm=TRUE) ## 3
range(aid_3.0$commitment_amount_usd_constant_s)

## trying the total model withOUT my main IV of interest(log_aid_received) and  all my controls (log world pop, log aid, log gdp, polity, lag attacks, lag aid, civl war dummy, and post cold war dummy) 
m5<-glm(terror_dummy~log_world_pop+log_gdp_capita+polity_score+lag_attacks+lag_aid+civil_war+post_cold_war, data=imputed_terror_years, family="binomial"(link="logit"))
summary(m5)

## predicted probability when all values held at median and dummies at 0 ##
cat("Predicted Probability:", plogis(m5$coef[1]+m5$coef[2]*15.67+m5$coef[3]*7.56+m5$coef[4]*3+m5$coef[5]*0+m5$coef[6]*18.81+m5$coef[7]*0+m5$coef[8]*0))

## predicted probability when all values held at median and civil war dummy at 1 and post cold war dummy at 1 ##
cat("Predicted Probability:", plogis(m5$coef[1]+m5$coef[2]*15.67+m5$coef[3]*7.56+m5$coef[4]*3+m5$coef[5]*0+m5$coef[6]*18.81+m5$coef[7]*1+m5$coef[8]*1))

## predicted probability when all values held at median and civil war dummy at 1 and post cold war dummy at 0 ##
cat("Predicted Probability:", plogis(m5$coef[1]+m5$coef[2]*15.67+m5$coef[3]*7.56+m5$coef[4]*3+m5$coef[5]*0+m5$coef[6]*18.81+m5$coef[7]*1+m5$coef[8]*0))

## trying out total model WITH my main IV of interest (log_aid_received) and  all my controls (log world pop, log gdp, polity, lag attacks, lag aid, civl war dummy, and post cold war dummy)
m6<-glm(terror_dummy~log_aid_received+log_world_pop+log_gdp_capita+polity_score+lag_attacks+lag_aid+civil_war+post_cold_war, data=imputed_terror_years, family="binomial"(link="logit"))
summary(m6)

## predicted probabilities when all values held at median and dummies at 0 ##
cat("Predicted Probability:", plogis(m6$coef[1]+m6$coef[2]*18.66+m6$coef[3]*15.66+m6$coef[4]*7.46+m6$coef[5]*3+m6$coef[6]*0+m6$coef[7]*18.95+m6$coef[8]*0+m6$coef[9]*0))

## predicted probabilities when all values held at median EXCEPT log aid received (hold at 0) and dummies at 0 ##
cat("Predicted Probability:", plogis(m6$coef[1]+m6$coef[2]*0+m6$coef[3]*15.65+m6$coef[4]*7.46+m6$coef[5]*3+m6$coef[6]*0+m6$coef[7]*18.95+m6$coef[8]*0+m6$coef[9]*0))

## predicted probabilities when all values held at median EXCEPT log aid received (hold at 28.05915 logged) and dummies at 0 ##
cat("Predicted Probability:", plogis(m6$coef[1]+m6$coef[2]*28.05915+m6$coef[3]*15.65+m6$coef[4]*7.46+m6$coef[5]*3+m6$coef[6]*0+m6$coef[7]*18.95+m6$coef[8]*0+m6$coef[9]*0))

## predicted probabilities when all values held at median and civil war dummy at 1 and post cold war dummy at 1 ##
cat("Predicted Probability:", plogis(m6$coef[1]+m6$coef[2]*18.66+m6$coef[3]*15.66+m6$coef[4]*7.46+m6$coef[5]*3+m6$coef[6]*0+m6$coef[7]*18.95+m6$coef[8]*1+m6$coef[9]*1))

## predicted probabilities when all values held at median EXCEPT log aid received (hold at 0) and civil war dummy at 1 and post cold war dummy at 1 ##
cat("Predicted Probability:", plogis(m6$coef[1]+m6$coef[2]*0+m6$coef[3]*15.66+m6$coef[4]*7.46+m6$coef[5]*3+m6$coef[6]*0+m6$coef[7]*18.95+m6$coef[8]*1+m6$coef[9]*1))

## predicted probabilities when all values held at median EXCEPT log aid received (hold at 28.05915 logged) and civil war dummy at 1 and post cold war dummy at 0 ##
cat("Predicted Probability:", plogis(m6$coef[1]+m6$coef[2]*28.05915+m6$coef[3]*15.65+m6$coef[4]*7.46+m6$coef[5]*3+m6$coef[6]*0+m6$coef[7]*18.95+m6$coef[8]*1+m6$coef[9]*0))

## predicted probabilities when all values held at median and civil war dummy at 1 and post cold war dummy at 0 ##
cat("Predicted Probability:", plogis(m6$coef[1]+m6$coef[2]*18.66+m6$coef[3]*15.67+m6$coef[4]*7.46+m6$coef[5]*3+m6$coef[6]*0+m6$coef[7]*18.95+m6$coef[8]*1+m6$coef[9]*0))

## predicted probabilities when all values held at median EXCEPT log aid received (hold at 0) and civil war dummy at 1 and post cold war dummy at 0 ##
cat("Predicted Probability:", plogis(m6$coef[1]+m6$coef[2]*0+m6$coef[3]*15.67+m6$coef[4]*7.46+m6$coef[5]*3+m6$coef[6]*0+m6$coef[7]*18.95+m6$coef[8]*1+m6$coef[9]*0))

## predicted probabilities when all values held at median EXCEPT log aid received (hold at 28.05915) and civil war dummy at 1 and post cold war dummy at 0 ##
cat("Predicted Probability:", plogis(m6$coef[1]+m6$coef[2]*28.05915+m6$coef[3]*15.67+m6$coef[4]*7.46+m6$coef[5]*3+m6$coef[6]*0+m6$coef[7]*18.95+m6$coef[8]*1+m6$coef[9]*0))


## Combining both m5 and m6 into one stargazer table ##
stargazer(m5, m6, type="html", title = "Table 1: Base Model and Base Model With Aid", dep.var.labels=c("Probability of Attack"),
          covariate.labels=c("Log Aid Received","Log Population","Log GDP per Capita","Polity Score","Lag Number of Attacks",
                             "Lag Aid Received", "Civil War", "Post Cold-War"), align=TRUE, no.space=TRUE, out="Model_5+6_combined.htm")

######################################### Weber Plot Version ###########################################################################################
## generate predicted values from a multivariate normal distribution of model 6 coefficients
pred<-mvrnorm(1000, coef(m1), vcov(m1))

## take standard deviation of pred column 2 (the main IV)
sd(pred[,2])

## generate the 95% confidence interval for column 2 (main IV)
quantile(pred[,2],c(0.025,0.975))

## plot coefficients ##
plot(seq(0:1000)/100,
plogis(m1$coef[1]+m1$coef[2]*seq(0:1000)/100),
      type="l", xlab="Polity", ylab="Probability of Attack", ylim=c(0,1), main="Aid and Terror")

lines(seq(0:1000)/100,
      plogis(m1$coef[1]+quantile(pred[,2], 0.025)*seq(0:1000)/100), col="red")

lines(seq(0:1000)/100,
      plogis(m1$coef[1]+quantile(pred[,2], 0.975)*seq(0:1000)/100), col="red")

####################################################################################GGplot shit#################################################

## plot predicted probabilities of polity on terror attacks ##
polity.temp.data<-data.frame(polity_score=seq(from=-10, to=10, length.out=10000))

polity.predicted.data<-as.data.frame(predict(m1, newdata=polity.temp.data, type="link", se=TRUE))

polity.new.data<-cbind(polity.temp.data, polity.predicted.data)

polity.new.data$ymin<-m1$family$linkinv(polity.new.data$fit-(1.96 * polity.new.data$se.fit))
polity.new.data$ymax<-m1$family$linkinv(polity.new.data$fit+(1.96 * polity.new.data$se.fit))
polity.new.data$fit<-m1$family$linkinv(polity.new.data$fit)

p1<-ggplot(imputed_terror_years, aes(x=polity_score, y=terror_dummy))
p1 + geom_point() +
  geom_ribbon(data=polity.new.data, aes(y=fit, ymin=ymin, ymax=ymax), fill="orange", alpha=0.5) +
  labs(x="Polity Score", y="Probability") +
  geom_line(data=polity.new.data,aes(y=fit)) +
  ggtitle("Polity Score vs. Probability of an Attack")

## plot predicted probabilities of world population on terror attacks ##
range(imputed_terror_years$log_world_pop)

pop.temp.data<-data.frame(log_world_pop=seq(from=9, to=21, length.out=1000))

pop.predicted.data<-as.data.frame(predict(m1b, newdata=pop.temp.data, type="link", se=TRUE))

pop.new.data<-cbind(pop.temp.data, pop.predicted.data)

pop.new.data$ymin<-m1b$family$linkinv(pop.new.data$fit-(1.96 * pop.new.data$se.fit))
pop.new.data$ymax<-m1b$family$linkinv(pop.new.data$fit+(1.96 * pop.new.data$se.fit))
pop.new.data$fit<-m1b$family$linkinv(pop.new.data$fit)

p2<-ggplot(imputed_terror_years, aes(x=log_world_pop, y=terror_dummy))
p2 + geom_point() +
  geom_ribbon(data=pop.new.data, aes(y=fit, ymin=ymin, ymax=ymax), fill="orange", alpha=0.5) +
  labs(x="Country Population", y="Probability") +
  geom_line(data=pop.new.data,aes(y=fit)) +
  ggtitle("Country Population vs. Probability of an Attack")

## plot predicted probabilities of GDP per capita on terror attacks ##
range(imputed_terror_years$log_gdp_capita)

gdp.temp.data<-data.frame(log_gdp_capita=seq(from=0, to=12, length.out=1000))

gdp.predicted.data<-as.data.frame(predict(m1c, newdata=gdp.temp.data, type="link", se=TRUE))

gdp.new.data<-cbind(gdp.temp.data, gdp.predicted.data)

gdp.new.data$ymin<-m1c$family$linkinv(gdp.new.data$fit-(1.96 * gdp.new.data$se.fit))
gdp.new.data$ymax<-m1c$family$linkinv(gdp.new.data$fit+(1.96 * gdp.new.data$se.fit))
gdp.new.data$fit<-m1c$family$linkinv(gdp.new.data$fit)

p3<-ggplot(imputed_terror_years, aes(x=log_gdp_capita, y=terror_dummy))
p3 + geom_point() +
  geom_ribbon(data=gdp.new.data, aes(y=fit, ymin=ymin, ymax=ymax), fill="orange", alpha=0.5) +
  labs(x="GDP per Capita", y="Probability") +
  geom_line(data=gdp.new.data,aes(y=fit)) +
  ggtitle("GDP per Capita vs. Probability of an Attack")

## predicted probabilities plot for m2 ##
m2.temp.data<-data.frame(polity_score=seq(from=-10, to=10, length.out=1000),log_gdp_capita=rep(median(imputed_terror_years$log_gdp_capita), 1000))

m2.predicted.data<-as.data.frame(predict(m2, newdata=m2.temp.data, type="link", se=TRUE))

m2.new.data<-cbind(m2.temp.data,m2.predicted.data)

m2.new.data$ymin<-m2$family$linkinv(m2.new.data$fit-(1.96 * polity.new.data$se.fit))
m2.new.data$ymax<-m2$family$linkinv(m2.new.data$fit+(1.96 * polity.new.data$se.fit))
m2.new.data$fit<-m2$family$linkinv(m2.new.data$fit)

p4<-ggplot(imputed_terror_years, aes(x=polity_score, y=terror_dummy))
p4 + geom_point() +
  geom_ribbon(data=m2.new.data, aes(y=fit, ymin=ymin, ymax=ymax), fill="red", alpha=0.5) +
  labs(x="Polity Score", y="Terror Attacks") +
  geom_line(data=m2.new.data,aes(y=fit)) +
  ggtitle("Polity Score vs. Probability of a Terrorist Attack")

## predicted probabilities plot for m5 (too many 1's so I don't think I'll graph) ############################################################

## range lag_attacks ##
range(imputed_terror_years$lag_attacks,na.rm=TRUE)

attacks.temp.data<-data.frame(lag_attacks=seq(from=0, to=1652, length.out=5000), log_world_pop=rep(median(imputed_terror_years$log_world_pop), 5000), 
                      log_gdp_capita=rep(median(imputed_terror_years$log_gdp_capita), 5000), polity_score=rep(median(imputed_terror_years$polity_score,na.rm=TRUE), 5000), 
                      lag_aid=rep(median(imputed_terror_years$lag_aid,na.rm=TRUE), 5000), civil_war=rep(0,5000),
                      post_cold_war=rep(0,5000))

attacks.predicted.data<-as.data.frame(predict(m5, newdata=attacks.temp.data, type="link", se=TRUE))

attacks.new.data<-cbind(attacks.temp.data, attacks.predicted.data)

attacks.new.data$ymin<-m5$family$linkinv(attacks.new.data$fit-(1.96 * attacks.new.data$se.fit))
attacks.new.data$ymax<-m5$family$linkinv(attacks.new.data$fit+(1.96 * attacks.new.data$se.fit))
attacks.new.data$fit<-m5$family$linkinv(attacks.new.data$fit)

p5<-ggplot(imputed_terror_years, aes(x=lag_attacks, y=terror_dummy))
p5 + geom_point() +
  geom_ribbon(data=attacks.new.data, aes(y=fit, ymin=ymin, ymax=ymax), fill="orange", alpha=0.5) +
  labs(x="Lagged Attacks", y="Probability") +
  geom_line(data=attacks.new.data,aes(y=fit)) +
  ggtitle("Lagged Attacks vs. Probability of an Attack")

#######################################################################################################################################
## predicted probabilities plot for m6 ##

## range log_aid_received ##
range(imputed_terror_years$log_aid_received)

aid.temp.data<-data.frame(log_aid_received=seq(from=0, to=30, length.out=1000), log_world_pop=rep(median(imputed_terror_years$log_world_pop,na.rm=TRUE), 1000), 
                      log_gdp_capita=rep(median(imputed_terror_years$log_gdp_capita), 1000), polity_score=rep(median(imputed_terror_years$polity_score,na.rm=TRUE), 1000), 
                      lag_attacks=rep(median(imputed_terror_years$lag_attacks,na.rm=TRUE), 1000), lag_aid=rep(median(imputed_terror_years$lag_aid,na.rm=TRUE), 1000), civil_war=rep(0,1000),
                      post_cold_war=rep(0,1000))

aid.predicted.data<-as.data.frame(predict(m6, newdata=aid.temp.data, type="link", se=TRUE))

aid.new.data<-cbind(aid.temp.data, aid.predicted.data)

aid.new.data$ymin<-m6$family$linkinv(aid.new.data$fit-(1.96 * aid.new.data$se.fit))
aid.new.data$ymax<-m6$family$linkinv(aid.new.data$fit+(1.96 * aid.new.data$se.fit))
aid.new.data$fit<-m6$family$linkinv(aid.new.data$fit)

p6<-ggplot(imputed_terror_years, aes(x=log_aid_received, y=terror_dummy))
p6 + geom_point() +
  geom_ribbon(data=aid.new.data, aes(y=fit, ymin=ymin, ymax=ymax), fill="orange", alpha=0.5) +
  labs(x="Aid Received", y="Probability") +
  geom_line(data=aid.new.data,aes(y=fit)) +
  ggtitle("Aid Received vs. Probability of a Terrorist Attack")

## predicted probabilities plot for m6 when taking both civil war and cold war dummies to 1 ##

n.aid.temp.data<-data.frame(log_aid_received=seq(from=0, to=30, length.out=1000), log_world_pop=rep(median(imputed_terror_years$log_world_pop,na.rm=TRUE), 1000), 
                          log_gdp_capita=rep(median(imputed_terror_years$log_gdp_capita), 1000), polity_score=rep(median(imputed_terror_years$polity_score,na.rm=TRUE), 1000), 
                          lag_attacks=rep(median(imputed_terror_years$lag_attacks,na.rm=TRUE), 1000), lag_aid=rep(median(imputed_terror_years$lag_aid,na.rm=TRUE), 1000), civil_war=rep(1,1000),
                          post_cold_war=rep(1,1000))

n.aid.predicted.data<-as.data.frame(predict(m6, newdata=n.aid.temp.data, type="link", se=TRUE))

n.aid.new.data<-cbind(n.aid.temp.data, n.aid.predicted.data)

n.aid.new.data$ymin<-m6$family$linkinv(n.aid.new.data$fit-(1.96 * n.aid.new.data$se.fit))
n.aid.new.data$ymax<-m6$family$linkinv(n.aid.new.data$fit+(1.96 * n.aid.new.data$se.fit))
n.aid.new.data$fit<-m6$family$linkinv(n.aid.new.data$fit)

p7<-ggplot(imputed_terror_years, aes(x=log_aid_received, y=terror_dummy))
p7 + geom_point() +
  geom_ribbon(data=n.aid.new.data, aes(y=fit, ymin=ymin, ymax=ymax), fill="orange", alpha=0.5) +
  labs(x="Aid Received", y="Probability") +
  geom_line(data=n.aid.new.data,aes(y=fit)) +
  ggtitle("Aid Received vs. Probability of a Terrorist Attack")

## predicted probabilities plot for m6 when taking civil war to 1 and cold war to 0 ##

f.aid.temp.data<-data.frame(log_aid_received=seq(from=0, to=30, length.out=1000), log_world_pop=rep(median(imputed_terror_years$log_world_pop,na.rm=TRUE), 1000), 
                            log_gdp_capita=rep(median(imputed_terror_years$log_gdp_capita), 1000), polity_score=rep(median(imputed_terror_years$polity_score,na.rm=TRUE), 1000), 
                            lag_attacks=rep(median(imputed_terror_years$lag_attacks,na.rm=TRUE), 1000), lag_aid=rep(median(imputed_terror_years$lag_aid,na.rm=TRUE), 1000), civil_war=rep(1,1000),
                            post_cold_war=rep(0,1000))

f.aid.predicted.data<-as.data.frame(predict(m6, newdata=f.aid.temp.data, type="link", se=TRUE))

f.aid.new.data<-cbind(f.aid.temp.data, f.aid.predicted.data)

f.aid.new.data$ymin<-m6$family$linkinv(f.aid.new.data$fit-(1.96 * f.aid.new.data$se.fit))
f.aid.new.data$ymax<-m6$family$linkinv(f.aid.new.data$fit+(1.96 * f.aid.new.data$se.fit))
f.aid.new.data$fit<-m6$family$linkinv(f.aid.new.data$fit)

p7<-ggplot(imputed_terror_years, aes(x=log_aid_received, y=terror_dummy))
p7 + geom_point() +
  geom_ribbon(data=f.aid.new.data, aes(y=fit, ymin=ymin, ymax=ymax), fill="orange", alpha=0.5) +
  labs(x="Aid Received", y="Probability") +
  geom_line(data=f.aid.new.data,aes(y=fit)) +
  ggtitle("Aid Received vs. Probability of a Terrorist Attack")

####################################################################################################################################################
## discern counts correctly predicted for m6 ##
hitmiss(m6)

mean(terror_years$terror_dummy [!is.na(log_aid_received)],na.rm=TRUE)

##############################################################################Extra Code#######################################################################################################################






