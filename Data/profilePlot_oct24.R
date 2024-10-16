### ukoss profile plot


### Setting working directory
setwd("/Users/richardjackson/Documents/github/UKOSS_CA/")


### Loading data

setwd("Data")
load("ukossData_dates.R")
update <- read.csv("dataUpdate.csv")[1:368,]
update2 <- read.csv("update2.csv")


data2$GestAtAnyTypeofBirthBaby1[which(data2$TOPyes==1)]

which(data2$GestAtAnyTypeofBirthBaby1>200)

data2 <- data2[order(data2$BestguessEPPROMdate),]

data2[1:3,]

table(data2$TOPyes)


ppromDt <- as.Date(data2$BestguessEPPROMoccur,"%d/%m/%Y")
sepsisDt <- as.Date(data2$M_SEPSIS_DATE_E1_C1,"%d/%m/%Y")
dischDt <- as.Date(data2$EPPROM_DISCH_E1_C1,"%d/%m/%Y")
csterDt <- as.Date(data2$CSTER_DATE_END_E1_C1_1,"%d/%m/%Y")
eddDt <- as.Date(data2$EDD_E1_C1,"%d/%m/%Y")
inf1Dt <- as.Date(data2$INF1_DOB_E1_C1,"%d/%m/%Y")
antiDt <- as.Date(data2$ANTIB_DATE_START_E1_C1_1,"%d/%m/%Y");antiDt
#endTm <- as.Date(data2$GestAtAnyTypeofBirthBaby1)



start <- data2$BestguessEPPROMdate

septm <- (sepsisDt-ppromDt)/7 + start
distm <- (dischDt-ppromDt)/7 + start
cstm <- (csterDt-ppromDt)/7 + start
#eddtm <- (eddDt-ppromDt)/7 + start- 40 weeks for everyone!
inf1tm <- (inf1Dt-ppromDt)/7 + start
antitm <- (antiDt-ppromDt)/7 + start





###############################
###############################

update <- read.csv("dataUpdate.csv")[1:368,]


update <- update[-which(update$Gestation.at.PPROM<0),]
update <- merge(update,update2)
update <- update[order(update$Gestation.at.PPROM),]


update[1:3,]

start <- update$Gestation.at.PPROM
septm <- pmax(0,as.Date(update$M_SEPSIS_DATE_E1_C1,"%d/%m/%Y")-as.Date(update$Date.of.PPROM,"%d/%m/%Y"));
septm2 <- pmax(0,as.Date(update$bs_s_dt,"%d/%m/%Y")-as.Date(update$Date.of.PPROM,"%d/%m/%Y"));
septm <- septm+start
septm2 <- septm2+start
antitm <- update$Gest.at.prophylactic.antibiotics
cstm <- update$Gest.at.steroids
inf1tm <- as.numeric(update$gest2)
lb.cl <- as.character(factor(update$Simplified.outcome,labels=c("green","darkorchid","royalblue","black","gray")))

table(lb.cl,update$Simplified.outcome)

par(mfrow=c(1,1))
anti.id <- which(update$ProphylacticAntibiotics==1)
plot(0,0,xlim=c(15,42),ylim=c(0,368),bty="l",yaxt="n",xlab="Time (Weeks)",ylab="Patient ID")
segments(start[anti.id],anti.id,rep(40,368),anti.id,lty=3,lwd=0.5,col="gray")
segments(start[anti.id],anti.id,inf1tm[anti.id],anti.id,lwd=4,col=lb.cl[anti.id])
#points(septm[anti.id],anti.id,pch=20,cex=2,font=2,col=2)
points(septm2[anti.id],anti.id,pch=20,cex=1.5,font=2,col=6)
points(cstm[anti.id],anti.id,pch="*",cex=2,font=2,col=5)

legend(15,350,c("Live Birth","Spon. Loss","Still Birth","TFMR","Sepsis","Steroid"),
       col=c("green","darkorchid","royalblue","black",2,5),pch=c(rep("-",4),"o","*"),bty="n")
dev.copy2pdf(file="antiProfile_june24.pdf",height=10,width=6)





anti.id <- which(is.na(update$ProphylacticAntibiotics))
plot(0,0,xlim=c(15,42),ylim=c(0,368),bty="l",yaxt="n",xlab="Time (Weeks)",ylab="Patient ID")
segments(start[anti.id],anti.id,rep(40,368),anti.id,lty=3,lwd=0.5,col="gray")
segments(start[anti.id],anti.id,inf1tm[anti.id],anti.id,lwd=2,col=lb.cl[anti.id])
#points(septm[anti.id],anti.id,pch=20,cex=2,font=2,col=2)
points(septm2[anti.id],anti.id,pch=20,cex=1.5,font=2,col=6)
points(cstm[anti.id],anti.id,pch="*",cex=2,font=2,col=5)

legend(15,350,c("Live Birth","Spon. Loss","Still Birth","TFMR","Sepsis","Steroid"),
       col=c("green","darkorchid","royalblue","black",2,5),pch=c(rep("-",4),"o","*"),bty="n")
dev.copy2pdf(file="no_antiProfile_june24.pdf",height=10,width=6)



##########
####
##########

library(cmprsk)
update$status <- factor(update$Simplified.outcome,labels=c("Livebirth","Spont. loss/TFMR","Stillbirth","Spont. loss/TFMR",NA))

table(update$Simplified.outcome,update$status)

antib <- as.numeric(!is.na(update$ProphylacticAntibiotics))

anti.id <- which(!is.na(update$ProphylacticAntibiotics))
par(mfrow=c(1,2))
plot(cuminc(inf1tm[anti.id],update$status[anti.id]),col=c("green","darkorchid","royalblue","black"),lwd=3,ylim=c(0,0.8),main="Antibiotics")
plot(cuminc(inf1tm[-anti.id],update$status[-anti.id]),col=c("green","darkorchid","royalblue","black"),lwd=3,ylim=c(0,0.80),main="No Antibiotics")
dev.copy2pdf(file="Cumulative Incidence.pdf")


##########
### Table 1
##########

dat.id <- which(data2$id%in%update$ParticipantID)
anti.id <- unique(update$ParticipantID[update$ProphylacticAntibiotics==1])


cov1 <- data2[,which(names(data2)%in%c("Gestation.at.PPROM","MULT_PREG_E1_C1",
                               "APHpriortoEPPROM","AgeatEDD","eth","weight","BMI",
                               "height","smoke","AnyCerclage"))]
by1 <- as.numeric(data2$id%in%anti.id)

cov1$MULT_PREG_E1_C1 <- as.character(cov1$MULT_PREG_E1_C1)

stab1 <- summaryTable(cov1,by1)

by2 <- update$ProphylacticAntibiotics

by2[which(is.na(by2))] <- 0
cov2 <- update[,which(names(update)%in%c("Gestation.at.PPROM","gest2","Gestation.at.Steroids","Gest.at.prophylactic.antibiotic","Simplified.outcome","bs_s","Maternal.sepsis.yes.no"))]
  

cov2$Gestation.at.PPROM <- as.numeric(as.character(cov2$Gestation.at.PPROM))
cov2$gest2 <- as.numeric(as.character(cov2$gest2))

stab2 <- summaryTable(cov2,by2)

stab2


write.csv(stab1,"stab1.csv")
write.csv(stab2,"stab2.csv")




cov1$eth <-factor(cov1$eth,labels=c(1,2,2,2,2,2,2,2,2,2,2,2,2,2,2))
cov1$smoke2 <- factor(cov1$smoke,labels=c(1,2,3,3,3))

### Propensity score analysis
## Grouping together 'rare' ethnicities
anal.data$eth <- factor(anal.data$eth,labels=c(1,6,2,6,6,6,3,44,6,6,6,5,6,6,6))


anal.data$eth[which(is.na(anal.data$eth))] <- 6
anal.data$smoke2[which(is.na(anal.data$smoke2))] <- 1

null.mod <- glm(by1~eth+smoke2+Gestation.at.PPROM,data=anal.data,family="binomial")
summary(null.mod)
anova(null.mod,test="Chisq")


### adding ps score to database
library(boot)

anal.data$ps <- inv.logit(fitted(null.mod))

par(mfrow=c(1,2))
hist(anal.data$ps,col=5,border=4,xlab="Prop. score")
boxplot(anal.data$ps~by1,col=c(4,5),names=c("no anti.","anti"))



## estimating weight
anal.data$w <- anal.data$ps*by1 + (1-anal.data$ps)*(1-by1)


anal.data$antib <- by1

bs_mod <- glm(bs_s~antib,data=anal.data,family="binomial",weights = w)
summLogistic(bs_mod)

bs_mod <- glm(bs_s~antib,data=anal.data,family="binomial",weights = w)
summLogistic(bs_mod)



### Adding weights to data


### assessing outcomes






