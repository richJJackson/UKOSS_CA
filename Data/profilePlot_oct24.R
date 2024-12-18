### ukoss profile plot


### Setting working directory
setwd("/Users/richardjackson/Documents/github/UKOSS_CA/")


### Loading data

setwd("Data")
load("ukossData_dates.R")
update <- read.csv("dataUpdate.csv")[1:368,]
update2 <- read.csv("update2.csv")

data2[1:3,]


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
update <- merge(update,update2)

### multiple preg
update$mult.id <- as.numeric(update$MULT_PREG_E1_C1..also.exlcude.at.the.moment.==1)
update$postNat <- as.numeric(update$ParticipantID=="EPR_377")
update$priorSep <- as.numeric(update$ParticipantID%in%c("EPR_242","EPR_452","EPR_180"))
update$unkOut <- as.numeric(update$Simplified.outcome=="Unknown outcome")



### 6 patients with unknown pprom date - removed
update <- update[-which(update$Gestation.at.PPROM<0),]


### ordering gestation
update <- update[order(update$Gestation.at.PPROM),]


### multiple preg
multDat <- update[which(update$mult.id==1),]
save(multDat,file="multDat.R")


### filtering dataset
update <- update[-which(update$mult.id==1|update$unkOut==1),]

### id for patients with a sepsis date of 0 or 1 days
septm <- pmax(0,as.Date(update$M_SEPSIS_DATE_E1_C1,"%d/%m/%Y")-as.Date(update$Date.of.PPROM,"%d/%m/%Y"));septm
update$postSep <- as.numeric(septm<2);update$postSep



which(update$priorSep==1|update$postSep==1)



update$M_SEPSIS_DATE_E1_C1


update$start <- update$Gestation.at.PPROM
update$septm <- pmax(0,as.Date(update$M_SEPSIS_DATE_E1_C1,"%d/%m/%Y")-as.Date(update$Date.of.PPROM,"%d/%m/%Y"));
update$septm2 <- pmax(0,as.Date(update$bs_s_dt,"%d/%m/%Y")-as.Date(update$Date.of.PPROM,"%d/%m/%Y"));
update$septm <- update$septm+update$start
update$septm2 <- update$septm2+update$start
update$antitm <- update$Gest.at.prophylactic.antibiotics
update$cstm <- update$Gest.at.steroids
update$inf1tm <- as.numeric(update$gest2)
update$lb.cl <- as.character(factor(update$Simplified.outcome,labels=c("forestgreen","darkorchid","royalblue","navyblue")))


### ordering gestation
update <- update[order(update$Gestation.at.PPROM),];update$septm
#update <- update[order(update$priorSep),];update$septm
#update <- update[order(update$postSep),];update$septm


anti.id <- which(update$ProphylacticAntibiotics==1&update$priorSep!=1&(is.na(update$postSep)|update$postSep==0))
d <- update[anti.id,]

par(mfrow=c(1,1))
plot(0,0,xlim=c(15,42),ylim=c(0,nrow(d)),bty="l",yaxt="n",xlab="Time (Weeks)",ylab="Patient ID")
segments(d$start,1:nrow(d),rep(40,nrow(d)),1:nrow(d),lty=3,lwd=0.5,col="gray")
segments(d$start,1:nrow(d),d$inf1tm,1:nrow(d),lwd=4,col=d$lb.cl)
#points(septm[anti.id],anti.id,pch=20,cex=2,font=2,col=2)
points(d$septm,1:nrow(d),pch=20,cex=1.5,font=2,col=6)
points(d$cstm,1:nrow(d),pch="*",cex=2,font=2,col=5)

legend(15,180,c("Live Birth","Spon. Loss","Still Birth","TFMR","Sepsis","Steroid"),
       col=c("green","darkorchid","royalblue","navyblue",2,5),pch=c(rep("-",4),"o","*"),bty="n",lwd=3)
dev.copy2pdf(file="antiProfile_june24.pdf",height=10,width=6)




no.anti.id <- which(is.na(update$ProphylacticAntibiotics)&update$priorSep!=1&(is.na(update$postSep)|update$postSep==0))
d <- update[no.anti.id,]

par(mfrow=c(1,1))
plot(0,0,xlim=c(15,42),ylim=c(0,nrow(d)),bty="l",yaxt="n",xlab="Time (Weeks)",ylab="Patient ID")
segments(d$start,1:nrow(d),rep(40,nrow(d)),1:nrow(d),lty=3,lwd=0.5,col="gray")
segments(d$start,1:nrow(d),d$inf1tm,1:nrow(d),lwd=4,col=d$lb.cl)
#points(septm[anti.id],anti.id,pch=20,cex=2,font=2,col=2)
points(d$septm,1:nrow(d),pch=20,cex=1.5,font=2,col=6)
points(d$cstm,1:nrow(d),pch="*",cex=2,font=2,col=5)

legend(15,100,c("Live Birth","Spon. Loss","Still Birth","TFMR","Sepsis","Steroid"),
       col=c("green","darkorchid","royalblue","navyblue",2,5),pch=c(rep("-",4),"o","*"),bty="n",lwd=3)
dev.copy2pdf(file="NOantiProfile_dev24.pdf",height=10,width=6)



sep.id <- which(update$priorSep==1|update$postSep==1)
d <- update[sep.id,]

par(mfrow=c(1,1))
plot(0,0,xlim=c(15,42),ylim=c(0,nrow(d)),bty="l",yaxt="n",xlab="Time (Weeks)",ylab="Patient ID")
segments(d$start,1:nrow(d),rep(40,nrow(d)),1:nrow(d),lty=3,lwd=0.5,col="gray")
segments(d$start,1:nrow(d),d$inf1tm,1:nrow(d),lwd=4,col=d$lb.cl)
#points(septm[anti.id],anti.id,pch=20,cex=2,font=2,col=2)
points(d$septm,1:nrow(d),pch=20,cex=1.5,font=2,col=6)
points(d$cstm,1:nrow(d),pch="*",cex=2,font=2,col=5)

legend(15,100,c("Live Birth","Spon. Loss","Still Birth","TFMR","Sepsis","Steroid"),
       col=c("green","darkorchid","royalblue","navyblue",2,5),pch=c(rep("-",4),"o","*"),bty="n",lwd=3)
dev.copy2pdf(file="NOantiProfile_dev24.pdf",height=10,width=6)



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






