setwd("~/Desktop/POL 683/")
require(foreign)
require(car)
require(ggplot2)
require(plyr)
install.packages("stargazer")
require(stargazer)
ElectionData<-read.csv("ElectionForecastData_5.csv")
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
model1b<-lm(percent_dem_vote_share~incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb, data=ElectionData)
summary(model1b)


## Modifying unrestricted model ##
model1a<-lm(percent_dem_vote_share~GDP_Q2+dem_popularity+incumb_terms+dem_incumb, data=ElectionData)
summary(model1a)

##Results table for full unrestricted model ##
stargazer(model1b, type="text",dep.var.labels=c("Democratic Vote Share"),covariate.labels=c("Incumbent Job Approval","Economic Growth Rate","Democratic Candidate Favorability","Incumbent Party Terms","Non-White Voters", "Democratic Incumbent"), out="Democratic Vote Share")

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
R_lm_predict_1968<-lm(percent_dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1968)
summary(R_lm_predict_1968)

## Model to predict vote share of 1972 election with full unrestricted model ##
R_lm_predict_1972<-lm(percent_dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1972)
summary(R_lm_predict_1972)

## Model to predict vote share of 1976 election with full unrestricted model ##
R_lm_predict_1976<-lm(percent_dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1976)
summary(R_lm_predict_1976)

## Model to predict vote share of 1980 election with full unrestricted model ##
R_lm_predict_1980<-lm(percent_dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1980)
summary(R_lm_predict_1980)

## Model to predict vote share of 1984 election with full unrestricted model ##
R_lm_predict_1984<-lm(percent_dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1984)
summary(R_lm_predict_1984)

## Model to predict vote share of 1988 election with full unrestricted model ##
R_lm_predict_1988<-lm(percent_dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1988)
summary(R_lm_predict_1988)

## Model to predict vote share of 1992 election with full unrestricted model ##
R_lm_predict_1992<-lm(percent_dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1992)
summary(R_lm_predict_1992)

## Model to predict vote share of 1996 election with full unrestricted model ##
R_lm_predict_1996<-lm(percent_dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_1996)
summary(R_lm_predict_1996)

## Model to predict vote share of 2000 election with full unrestricted model ##
R_lm_predict_2000<-lm(percent_dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_2000)
summary(R_lm_predict_2000)

## Model to predict vote share of 2004 election with full unrestricted model ##
R_lm_predict_2004<-lm(percent_dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_2004)
summary(R_lm_predict_2004)

## Model to predict vote share of 2008 election with full unrestricted model ##
R_lm_predict_2008<-lm(percent_dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_2008)
summary(R_lm_predict_2008)

## Model to predict vote share of 2012 election with full unrestricted model ##
R_lm_predict_2012<-lm(percent_dem_vote_share~+incumb_job_app+GDP_Q2+dem_popularity+incumb_terms+nonwhite_voters+dem_incumb,data=R_predict_2012)
summary(R_lm_predict_2012)

## Arriving at prediction of two party vote share for 2016 ##
y<-(1.28889)+(-.01525*54)+(.18240*1.4)+(.99556*41)+(2.40435*1)+(-.08815*30.9)+(3.45363*1)

## Arriving at prediction of two party vote share for 2012 ##
a<-(-.059392)+(-.02811*54)+(.23326*1.9)+(1.08790*49)+(2.38410*0)+(-.20742*28.9)+(3.14326*1)

## Arriving at prediction of two party vote share for 2008 ##
b<-(4.227958)+(-.142698*25)+(.298455*2.0)+(1.033716*55)+(2.110555*1)+(.008605*26.6)+(2.971814*0)

## Arriving at prediction of two party vote share for 2004 ##
d<-(1.09126)+(-.01906*49)+(.19915*3.0)+(1.00549*49)+(2.63900*0)+(-.12021*24.8)+(3.83813*0)

## Arriving at prediction of two party vote share for 2000 ##
e<-(1.449109)+(.030017*36)+(.148707*7.8)+(.900115*46)+(3.806373*1)+(.002825*22.3)+(3.986478*1)

## Arriving at prediction of two party vote share for 1996 ##
g<-(-12.885729)+(.004332*58)+(.330565*7.2)+(1.351374*52)+(.999580*0)+(-.270045*20.8)+(4.436732*1)

## Arriving at prediction of two party vote share for 1992 ##
h<-(-1.13296)+(.03155*58)+(.12309*4.5)+(1.00110*49)+(2.39083*0)+(-.09644*18.6)+(3.64717*1)

## Arriving at prediction of two party vote share for 1988 ##
j<-(.94961)+(-.00250*53)+(.16033*5.4)+(.99109*44)+(2.64303*1)+(-.08684*14.3)+(3.31832*0)

## Arriving at prediction of two party vote share for 1984 ##
k<-(1.66092)+(-.012132*58)+(.18836*7.2)+(.99141*44)+(2.24421*0)+(-.09508*12.5)+(3.30625*0)

## Arriving at prediction of two party vote share for 1980 ##
m<-(10.51676)+(-.03028*37)+(-.27615*-7.9)+(.90817*44)+(1.65152*0)+(-.19237*12.4)+(4.58858*1)

## Arriving at prediction of two party vote share for 1976 ##
n<-(5.41293)+(-.04656*45)+(-.23026*3.1)+(.90791*48)+(2.03642*1)+(-.02470*11.9)+(3.75858*0)

## Arriving at prediction of two party vote share for 1972 ##
p<-(5.89339)+(-.04010*56)+(.24973*9.6)+(.92092*38)+(1.86668*0)+(-.07426*11.4)+(3.29746*0)

## Arriving at prediction of two party vote share for 1968 ##
q<-(-4.08927)+(.04682*41)+(.06034*6.9)+(1.05385*42)+(2.4051*1)+(-.06146*10.4)+(2.31818*1)

actual_dem_vote_share<-ElectionData$percent_dem_vote_share

ElectionData$pred_vote_share<-c(46.59,40.19,47.92,53.74,44.74,46.69,52.15,58.83,52.95,47.04,60.45,49.32,44.67)

pred_vote_share<-ElectionData$pred_vote_share

election_year<-ElectionData$election_yr



## Scatter plot comparing actual dem vote share to election year ##
plot(election_year,actual_dem_vote_share, col="blue", pch=16, ylim = c(35,65), xlab="Election Year", ylab="Vote Share", main="Predicted vs. Actual Vote Share", axes = FALSE)
points(election_year,pred_vote_share, col="red", pch=16)
axis(side=1, at=c(1968,1972,1976,1980,1984,1988,1992,1996,2000,2004,2008,2012,2016))
axis(side=2, at=c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75))
box()
legend(2005, 42, legend=c("Predicted", "Actual"),col=c("red","blue"),pch=16,cex=0.8)

## Robustness check -- Predicted vote share regressed on actual ##
robustness<-lm(dem_vote_share~predicted_dem_vote, data = ElectionData2)
summary(robustness)

## Re-operationalize the DV to be two party vote share ##

## First Add up percentages of both republicans and democrats to use as a denominator ##

ElectionData$denominator<-ElectionData$dem_vote_share+ElectionData$rep_vote_share

## Then use dem vote share as numerator and our new denominator of both republican and democractic vote share and divide ##

ElectionData$revised_dem_vote_share<-ElectionData$dem_vote_share/ElectionData$denominator

## Multiply the two party vote share variable by 100 to arrive at percentages ##

ElectionData$percent_dem_vote_share<-ElectionData$revised_dem_vote_share*100
