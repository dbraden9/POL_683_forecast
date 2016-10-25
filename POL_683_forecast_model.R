setwd("G:/Fall 2016/POL 683_Advanced Regression Analysis/Forecasting Project/Data")
require(foreign)
require(car)
require(ggplot2)
require(plyr)
require(stargazer)
ElectionData<-read.csv("ElectionForecastData.csv")
View(ElectionData)
ElectionData2<-read.csv("ElectionData2.csv")
View(ElectionData2)


## graph for Democrat vote share ##
ggplot(ElectionData, aes(y=dem_vote_share, x=election_yr)) +
  geom_line(col="blue")+ geom_point(col="blue") +
  geom_smooth(color = "blue") + scale_y_continuous("Democratic Vote Share") +
  ggtitle("Democratic Vote Share 1968-2012") + labs(x="Election Year")

## graph for Republican vote share ##
ggplot(ElectionData, aes(y=rep_vote_share, x=election_yr)) +
  geom_line(col="red")+ geom_point(col="red") +
  geom_smooth(color = "red") + scale_y_continuous("Republican Vote Share") +
  ggtitle("Republican Vote Share 1968-2012") + labs(y="Republican Vote Share", x="Election Year")

## Getting all the plots of the effects of our IV into one plot ##
ggplot(ElectionData, aes(y=dem_vote_share, x=dem_popularity)) +
  geom_line(col="blue")+ geom_point(col="blue") +
  geom_smooth(color = "blue") + scale_y_continuous("Democratic Vote Share") +
  ggtitle("Democratic Candidate Popularity vs. Democratic Vote Share") + labs(x="Democratic Candidate Poularity")

ggplot(ElectionData, aes(y=dem_vote_share, x=incumb_job_app)) +
  geom_line(col="blue")+ geom_point(col="blue") +
  geom_smooth(color = "blue") + scale_y_continuous("Democratic Vote Share") +
  ggtitle("Incumbant Popularity vs. Democratic Vote Share") + labs(x="Incumbant Popularity", y="Democratic Vote Share")+ylim(20,65)

ggplot(ElectionData, aes(y=dem_vote_share, x=dem_incumb)) +
  geom_line(col="blue")+ geom_point(col="blue") +
  geom_smooth(color = "blue") + scale_y_continuous("Democratic Vote Share") +
  ggtitle("Democratic Incumbant Dummy vs. Democratic Vote Share") + labs(x="Democratic Incumbant Dummy")

ggplot(ElectionData, aes(y=dem_vote_share, x=GDP_Q2)) +
  geom_line(col="blue")+ geom_point(col="blue") +
  geom_smooth(color = "blue") + scale_y_continuous("Democratic Vote Share") +
  ggtitle("2nd Quarter GDP Growth vs. Democratic Vote Share") + labs(x="2nd Quarter GDP Growth")+xlim(1,10)

ggplot(ElectionData, aes(y=dem_vote_share, x=incumb_terms)) +
  geom_line(col="blue")+ geom_point(col="blue") +
  geom_smooth(color = "blue") + scale_y_continuous("Democratic Vote Share") +
  ggtitle("2 Incumbant Terms Dummy vs. Democratic Vote Share") + labs(x="2 Incumbant Terms Dummy")

ggplot(ElectionData, aes(y=dem_vote_share, x=nonwhite_voters)) +
  geom_line(col="blue")+ geom_point(col="blue") +
  geom_smooth(color = "blue") + scale_y_continuous("Democratic Vote Share") +
  ggtitle("Percent Non-White Voters vs. Democratic Vote Share") + labs(x="Percent Non-White Voters")+xlim(10,30)

## Full Unrestricted model ##
model1b<-lm(dem_vote_share~incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb, data=ElectionData)
summary(model1b)

##Results table for full unrestricted model ##
stargazer(model1b, type="text",dep.var.labels=c("Democratic Vote Share"),covariate.labels=c("Incumbent Job Approval","Economic Growth Rate","Democratic Candidate Favorability","Incumbent Party Terms","Non-White Voters", "Democratic Incumbent"))

## Subset data into all years minus 1968 ##
R_predict_1968<-ElectionData[which(ElectionData$election_yr %in% c(1972,1976,1980,1984,1988,1992,1996,2000,2004,2008,2012)),]

## Subset data into all years minus 1972 ##
R_predict_1972<-ElectionData[which(ElectionData$election_yr %in% c(1968,1976,1980,1984,1988,1992,1996,2000,2004,2008,2012)),]

## Subset data into all years minus 1976 ##
R_predict_1976<-ElectionData[which(ElectionData$election_yr %in% c(1968,1972,1980,1984,1988,1992,1996,2000,2004,2008,2012)),]

## Subset data into all years minus 1980 ##
R_predict_1980<-ElectionData[which(ElectionData$election_yr %in% c(1968,1972,1976,1984,1988,1992,1996,2000,2004,2008,2012)),]

## Subset data into all years minus 1984 ##
R_predict_1984<-ElectionData[which(ElectionData$election_yr %in% c(1968,1972,1976,1980,1988,1992,1996,2000,2004,2008,2012)),]

## Subset data into all years minus 1988 ##
R_predict_1988<-ElectionData[which(ElectionData$election_yr %in% c(1968,1972,1976,1980,1984,1992,1996,2000,2004,2008,2012)),]

## Subset data into all years minus 1992 ##
R_predict_1992<-ElectionData[which(ElectionData$election_yr %in% c(1968,1972,1976,1980,1984,1988,1996,2000,2004,2008,2012)),]

## Subset data into all years minus 1996 ##
R_predict_1996<-ElectionData[which(ElectionData$election_yr %in% c(1968,1972,1976,1980,1984,1988,1992,2000,2004,2008,2012)),]

## Subset data into all years minus 2000 ##
R_predict_2000<-ElectionData[which(ElectionData$election_yr %in% c(1968,1972,1976,1980,1984,1988,1992,1996,2004,2008,2012)),]

## Subset data into all years minus 2004 ##
R_predict_2004<-ElectionData[which(ElectionData$election_yr %in% c(1968,1972,1976,1980,1984,1988,1992,1996,2000,2008,2012)),]

## Subset data into all years minus 2008 ##
R_predict_2008<-ElectionData[which(ElectionData$election_yr %in% c(1968,1972,1976,1980,1984,1988,1992,1996,2000,2004,2012)),]

## Subset data into all years minus 2012 ##
R_predict_2012<-ElectionData[which(ElectionData$election_yr %in% c(1968,1972,1976,1980,1984,1988,1992,1996,2000,2004,2008)),]

## Model to predict vote share of 1968 election with full unrestricted model ##
R_lm_predict_1968<-lm(dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1968)
summary(R_lm_predict_1968)

## Model to predict vote share of 1972 election with full unrestricted model ##
R_lm_predict_1972<-lm(dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1972)
summary(R_lm_predict_1972)

## Model to predict vote share of 1976 election with full unrestricted model ##
R_lm_predict_1976<-lm(dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1976)
summary(R_lm_predict_1976)

## Model to predict vote share of 1980 election with full unrestricted model ##
R_lm_predict_1980<-lm(dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1980)
summary(R_lm_predict_1980)

## Model to predict vote share of 1984 election with full unrestricted model ##
R_lm_predict_1984<-lm(dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1984)
summary(R_lm_predict_1984)

## Model to predict vote share of 1988 election with full unrestricted model ##
R_lm_predict_1988<-lm(dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1988)
summary(R_lm_predict_1988)

## Model to predict vote share of 1992 election with full unrestricted model ##
R_lm_predict_1992<-lm(dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1992)
summary(R_lm_predict_1992)

## Model to predict vote share of 1996 election with full unrestricted model ##
R_lm_predict_1996<-lm(dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1996)
summary(R_lm_predict_1996)

## Model to predict vote share of 2000 election with full unrestricted model ##
R_lm_predict_2000<-lm(dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_2000)
summary(R_lm_predict_2000)

## Model to predict vote share of 2004 election with full unrestricted model ##
R_lm_predict_2004<-lm(dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_2004)
summary(R_lm_predict_2004)

## Model to predict vote share of 2008 election with full unrestricted model ##
R_lm_predict_2008<-lm(dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_2008)
summary(R_lm_predict_2008)

## Model to predict vote share of 2012 election with full unrestricted model ##
R_lm_predict_2012<-lm(dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_2012)
summary(R_lm_predict_2012)

z<-ElectionData2$dem_vote_share

q<-ElectionData2$predicted_dem_vote

v<-ElectionData2$election_yr

## Scatter plot comparing actual dem vote share to election year ##
plot(v,z, col="blue", pch=16, xlab="Election Year", ylab="Vote Share", main="Predicted vs. Actual Vote Share", axes = FALSE)
points(v,q, col="red", pch=16)
axis(side=1, at=c(1968,1972,1976,1980,1984,1988,1992,1996,2000,2004,2008,2012,2016))
axis(side=2, at=c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65))
box()
legend(2005, 42, legend=c("Predicted", "Actual"),col=c("red","blue"),pch=16,cex=0.8)

## Robustness check -- Predicted vote share regressed on actual ##
robustness<-lm(dem_vote_share~predicted_dem_vote, data = ElectionData2)
summary(robustness)