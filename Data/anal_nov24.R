### Analysis - Nov24

setwd("Data")

### Reading in datasets
data125 <- read.csv("ukoss_nov24_125.csv")[1:309,]
data3 <- read.csv("ukoss_nov24_3.csv")
data4 <- read.csv("ukoss_nov24_4.csv")
data67 <- read.csv("ukoss_nov24_67.csv")


### Outcome 1


nrow(data125)


data125[1:3,1:20]

names(data125)[13] <- "pa"
data125$pa <- factor(data125$pa,labels=c("None","Eryth.","IV","Alternative"))
data125$pa2 <- factor(data125$pa,labels=c("None","Anti","Anti","Anti"))
data125$pa3 <- factor(data125$pa,labels=c(0,1,1,1))

table(data125$pa)

table(data125$Woman.sick,data125$pa)

prop.cov.nm <- c("Gestation.at.PPROM",
                 "W_WEIGHT_E1_C1","W_HEIGHT_E1_C1","NewSmoking",
                 "SimpleUSSAfterEPPROM","AnyCerclage",
                 "Maternal.age.at.PPROM..years.","simpleEthnicity",
                 "APHpriortoEPPROM","SteroidsWithin24hoursOfDiagnosis","newAPH",
                 "SteroidsWithin7daysOfBirth","OUT_MANAGE_E1_C1")

prop.cov <- data125[,which(names(data125)%in%prop.cov.nm)]

prop.cov[which(is.na(prop.cov),arr.ind=T)] <- 0
prop.cov$SimpleUSSAfterEPPROM[which(prop.cov$SimpleUSSAfterEPPROM=="")] <- "None"
prop.cov$simpleEthnicity[which(prop.cov$simpleEthnicity=="")] <- "Other"
prop.cov$simpleEthnicity <- factor(prop.cov$simpleEthnicity,labels=c("Other","Other","Other","Other","White"))

table(prop.cov$simpleEthnicity,data125$pa2)




names(data125)

### propensity socre
ps.mod <- glm(data125$pa3~.,data=prop.cov,family="binomial")


step.mod <- step(ps.mod,direction="backward",k=6)
summary(step.mod)


ps <- fitted(step.mod)
mod.data <- data.frame("out"=data125$Woman.sick,"pa3"=data125$pa3,"ps"=ps)


mod.data$ps2 <- 1- mod.data$ps
mod.data$w <- mod.data$ps*(as.numeric(mod.data$pa3)-1) + mod.data$ps2*(2-as.numeric(mod.data$pa3))

  
mod.data[1:3,]

null.mod <- glm(out~pa3,data=mod.data,family="binomial")
psa.mod <- glm(out~pa3,data=mod.data,family="binomial",weight=w)


summary(null.mod)
summary(psa.mod)


mod.data[1:3,]

summary(ps.mod)


