### Analysis - Nov24

setwd("Data")



#### Function for summarising binomal glm
summLogistic <- function(mod){
  
  co.tab <- summary(mod)$coef
  co.tab
  est <- round(exp(co.tab[,1]),2)
  low <- round(exp(co.tab[,1]-1.96*co.tab[,2]),2)
  upp <- round(exp(co.tab[,1]+1.96*co.tab[,2]),2)
  
  c1 <- paste(round(co.tab[,1],2)," (",round(co.tab[,2],3),")",sep="")
  c2 <- paste(est," (",low,", ",upp,")",sep="")
  c3 <- round(co.tab[,4],3)
  
  zero.id <- which(c3==0)
  if(length(zero.id)>0) c3[zero.id] <- "<0.001"
  
  res <- data.frame(cbind(c1,c2,c3))
  names(res) <- c("est (se)","OR (95%CI)","Pval")
  res
}

### Summary with means
summ.mean <- function(x,se=TRUE,na.rm=TRUE){
  
  me <- round(mean(x,na.rm=na.rm),1)
  st.dev <- sd(x,na.rm=na.rm)
  if(se) {
    id <- which(is.na(x))
    if(length(id)>0) st.dev <- st.dev/sqrt(length(x[-id])) else st.dev <- st.dev/sqrt(length(x)) }
  st.dev <- round(st.dev,2)
  paste(me," (",st.dev,")",sep="")
}

percTable<-function(r,c,total=T,row=T){
  
  r <- as.factor(r);r.lev <- levels(r)
  c <- as.factor(c);c.lev <- levels(c)
  
  ### creating basic table
  tab <- table(r,c)
  
  ### Adding in Totals (if required)
  if(total){
    r.lev <- c(r.lev,"Total")
    c.lev <- c(c.lev,"Total")
    tab <- rbind(tab,colSums(tab))
    tab <- cbind(tab,rowSums(tab))
  }
  
  ### Adding in percentages (if required)
  p.tab <- tab[,-ncol(tab)]
  perc <- round(100*p.tab/rowSums(p.tab),0)
  #perc <- cbind(perc,"")
  
  if(!row){
    p.tab <- tab[-nrow(tab),]
    perc <- round(100*t(t(p.tab)/colSums(p.tab)),0)
    #perc <- rbind(perc,"")
  }
  
  ### replacing "NaN"
  miss.id <- which(is.na(perc)|perc=="NaN"|perc=="NA",arr.ind=T)
  if(length(miss.id)!=0){
    perc[miss.id] <- "0"
  }
  
  
  ### pasting
  
  tab2 <- paste(p.tab," (",perc,"%)",sep="")
  dim(tab2) <- dim(p.tab)
  
  if(row)	tab2 <- cbind(tab2,tab[,ncol(tab)])
  if(!row) tab2 <- rbind(tab2,tab[nrow(tab),])
  
  
  ### Adding in row/column names
  rownames(tab2) <- r.lev
  colnames(tab2) <- c.lev
  
  ### writing out results
  tab2
}




####


### Reading in datasets
data125 <- read.csv("ukoss_nov24_125.csv")[1:309,]
data3 <- read.csv("ukoss_nov24_3.csv")[1:289,]
data4 <- read.csv("ukoss_nov24_4.csv")[1:213,]
data67 <- read.csv("ukoss_nov24_67.csv")[1:309,]


data67[,1:3]

### Outcome 1


nrow(data125)


data3[1:3,1:20]
names(data67)[14]

names(data125)[13] <- "pa"
names(data3)[12] <- "pa"
names(data4)[13] <- "pa"
names(data67)[14] <- "pa"

data125$pa <- factor(data125$pa,labels=c("None","Eryth.","IV","Alternative"))
data125$pa2 <- factor(data125$pa,labels=c("None","Anti","Anti","Anti"))
data125$pa3 <- factor(data125$pa,labels=c(0,1,1,1))
data125$pa4 <- factor(data125$pa,labels=c(0,1,NA,NA))

data3$pa <- factor(data3$pa,labels=c("None","Eryth.","IV","Alternative"))
data3$pa2 <- factor(data3$pa,labels=c("None","Anti","Anti","Anti"))
data3$pa3 <- factor(data3$pa,labels=c(0,1,1,1))
data3$pa4 <- factor(data3$pa,labels=c(0,1,NA,NA))

data4$pa <- factor(data4$pa,labels=c("None","Eryth.","IV","Alternative"))
data4$pa2 <- factor(data4$pa,labels=c("None","Anti","Anti","Anti"))
data4$pa3 <- factor(data4$pa,labels=c(0,1,1,1))
data4$pa4 <- factor(data4$pa,labels=c(0,1,NA,NA))

data67$pa <- factor(data67$pa,labels=c("None","Eryth.","IV","Alternative"))
data67$pa2 <- factor(data67$pa,labels=c("None","Anti","Anti","Anti"))
data67$pa3 <- factor(data67$pa,labels=c(0,1,1,1))
data67$pa4 <- factor(data67$pa,labels=c(0,1,NA,NA))



### Propensity score datasets
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


prop.cov.3 <- data3[,which(names(data3)%in%prop.cov.nm)]

prop.cov.3[which(is.na(prop.cov.3),arr.ind=T)] <- 0
prop.cov.3$SimpleUSSAfterEPPROM[which(prop.cov.3$SimpleUSSAfterEPPROM=="")] <- "None"
prop.cov.3$simpleEthnicity[which(prop.cov.3$simpleEthnicity=="")] <- "Other"
prop.cov.3$simpleEthnicity <- factor(prop.cov.3$simpleEthnicity,labels=c("Other","Other","Other","Other","White"))


prop.cov.4 <- data4[,which(names(data4)%in%prop.cov.nm)]

prop.cov.4[which(is.na(prop.cov.4),arr.ind=T)] <- 0
prop.cov.4$SimpleUSSAfterEPPROM[which(prop.cov.4$SimpleUSSAfterEPPROM=="")] <- "None"
prop.cov.4$simpleEthnicity[which(prop.cov.4$simpleEthnicity=="")] <- "Other"
prop.cov.4$simpleEthnicity <- factor(prop.cov.4$simpleEthnicity,labels=c("Other","Other","Other","Other","White"))


prop.cov.67 <- data67[,which(names(data67)%in%prop.cov.nm)]

prop.cov.67[which(is.na(prop.cov.67),arr.ind=T)] <- 0
prop.cov.67$simpleEthnicity[which(prop.cov.67$simpleEthnicity=="")] <- "Other"
prop.cov.67$simpleEthnicity <- factor(prop.cov.67$simpleEthnicity,labels=c("Other","Other","Other","Other","White"))





####################################################################
####################################################################
######### Question 1

### propensity socre
ps.mod <- glm(data125$pa3~.,data=prop.cov,family="binomial")

step.mod <- step(ps.mod,direction="backward",k=6)
summary(step.mod)

ps <- fitted(step.mod)
mod.data <- data.frame("out"=data125$Woman.sick,"pa3"=data125$pa3,"pa4"=data125$pa4,"ps"=ps)

mod.data$ps2 <- 1- mod.data$ps
mod.data$w <- mod.data$ps*(as.numeric(mod.data$pa3)-1) + mod.data$ps2*(2-as.numeric(mod.data$pa3))

null.mod <- glm(out~pa3,data=mod.data,family="binomial")
psa.mod <- glm(out~pa3,data=mod.data,family="binomial",weight=1/w)
sen.mod.1 <- glm(out~pa4,data=mod.data,family="binomial",weight=1/w)


### plot results
tb1 <- percTable(data125$Woman.sick,data125$pa2,row=F)
tb2.1 <- tapply(mod.data$ps,mod.data$pa3,summ.mean,se=F)
tb2.2 <- summ.mean(mod.data$ps)

tb.res <- rbind(tb1,c(tb2.1,tb2.2))

r1 <- summLogistic(step.mod)
r2 <- summLogistic(null.mod)
r3 <- summLogistic(psa.mod)

mod.res <- rbind(r1,"",r2,"",r3)

nmt <- colnames(tb.res)
nmm <- names(mod.res)

colnames(tb.res) <- c("c1","c2","c3")
colnames(mod.res) <-c("c1","c2","c3")

res_1 <- rbind(nmt,tb.res,"",nmm,mod.res)

res_1



####################################################################
####################################################################
######### Question 2

data125[1:3,]

mod.data <- data.frame("out"=data125$Liverbirth.or.not..1.livebirth.0.not.,"pa3"=data125$pa3,"pa4"=data125$pa4,"ps"=ps)
mod.data$ps2 <- 1- mod.data$ps
mod.data$w <- mod.data$ps*(as.numeric(mod.data$pa3)-1) + mod.data$ps2*(2-as.numeric(mod.data$pa3))

null.mod <- glm(out~pa3,data=mod.data,family="binomial")
psa.mod <- glm(out~pa3,data=mod.data,family="binomial",weight=1/w)
sen.mod.2 <- glm(out~pa4,data=mod.data,family="binomial",weight=1/w)

### plot results
tb1 <- percTable(data125$Liverbirth.or.not..1.livebirth.0.not.,data125$pa2,row=F)
tb2.1 <- tapply(mod.data$ps,mod.data$pa3,summ.mean,se=F)
tb2.2 <- summ.mean(mod.data$ps)

tb.res <- rbind(tb1,c(tb2.1,tb2.2))

r1 <- summLogistic(step.mod)
r2 <- summLogistic(null.mod)
r3 <- summLogistic(psa.mod)

mod.res <- rbind(r1,"",r2,"",r3)

nmt <- colnames(tb.res)
nmm <- names(mod.res)

colnames(tb.res) <- c("c1","c2","c3")
colnames(mod.res) <-c("c1","c2","c3")

res_2 <- rbind(nmt,tb.res,"",nmm,mod.res)


res_2

####################################################################
####################################################################
######### Question 3

### propensity socre
ps.mod <- glm(data3$pa3~.,data=prop.cov.3,family="binomial")
step.mod <- step(ps.mod,direction="backward",k=6)

ps <- fitted(step.mod)
mod.data <- data.frame("out"=data3$Woman.sick,"pa3"=data3$pa3,"pa4"=data3$pa4,"ps"=ps)

mod.data$ps2 <- 1- mod.data$ps
mod.data$w <- mod.data$ps*(as.numeric(mod.data$pa3)-1) + mod.data$ps2*(2-as.numeric(mod.data$pa3))

null.mod <- glm(out~pa3,data=mod.data,family="binomial")
psa.mod <- glm(out~pa3,data=mod.data,family="binomial",weight=1/w)
sen.mod.3 <- glm(out~pa4,data=mod.data,family="binomial",weight=1/w)

### plot results
tb1 <- percTable(data3$Woman.sick,data3$pa2,row=F)
tb2.1 <- tapply(mod.data$ps,mod.data$pa3,summ.mean,se=F)
tb2.2 <- summ.mean(mod.data$ps)

tb.res <- rbind(tb1,c(tb2.1,tb2.2))

r1 <- summLogistic(step.mod)
r2 <- summLogistic(null.mod)
r3 <- summLogistic(psa.mod)

mod.res <- rbind(r1,"",r2,"",r3)

nmt <- colnames(tb.res)
nmm <- names(mod.res)

colnames(tb.res) <- c("c1","c2","c3")
colnames(mod.res) <-c("c1","c2","c3")

res_3 <- rbind(nmt,tb.res,"",nmm,mod.res)




####################################################################
####################################################################
######### Question 4

### propensity socre
ps.mod <- glm(data4$pa3~.,data=prop.cov.4,family="binomial")
step.mod <- step(ps.mod,direction="backward",k=6)

ps <- fitted(step.mod)
mod.data <- data.frame("out"=data4$Liverbirth.or.not..1.livebirth.0.not.,"pa3"=data4$pa3,"pa4"=data4$pa4,"ps"=ps)

mod.data$ps2 <- 1- mod.data$ps
mod.data$w <- mod.data$ps*(as.numeric(mod.data$pa3)-1) + mod.data$ps2*(2-as.numeric(mod.data$pa3))

null.mod <- glm(out~pa3,data=mod.data,family="binomial")
psa.mod <- glm(out~pa3,data=mod.data,family="binomial",weight=1/w)
sen.mod.4 <- glm(out~pa4,data=mod.data,family="binomial",weight=1/w)

### plot results
tb1 <- percTable(data4$Liverbirth.or.not..1.livebirth.0.not.,data4$pa2,row=F)
tb2.1 <- tapply(mod.data$ps,mod.data$pa3,summ.mean,se=F)
tb2.2 <- summ.mean(mod.data$ps)

tb.res <- rbind(tb1,c(tb2.1,tb2.2))

r1 <- summLogistic(step.mod)
r2 <- summLogistic(null.mod)
r3 <- summLogistic(psa.mod)

mod.res <- rbind(r1,"",r2,"",r3)

nmt <- colnames(tb.res)
nmm <- names(mod.res)

colnames(tb.res) <- c("c1","c2","c3")
colnames(mod.res) <-c("c1","c2","c3")

res_4 <- rbind(nmt,tb.res,"",nmm,mod.res)
res_4



####################################################################
####################################################################
######### Question 4.2
data4[1:3,1:10]

data4$out2 <- data4$Woman.sick==0&data4$Liverbirth.or.not..1.livebirth.0.not.==1

mod.data <- data.frame("out"=data4$out2,"pa3"=data4$pa3,"pa4"=data4$pa4,"ps"=ps)

mod.data$ps2 <- 1- mod.data$ps
mod.data$w <- mod.data$ps*(as.numeric(mod.data$pa3)-1) + mod.data$ps2*(2-as.numeric(mod.data$pa3))

null.mod <- glm(out~pa3,data=mod.data,family="binomial")
psa.mod <- glm(out~pa3,data=mod.data,family="binomial",weight=1/w)
sen.mod.4.2 <- glm(out~pa4,data=mod.data,family="binomial",weight=1/w)

### plot results
tb1 <- percTable(data4$out2,data4$pa2,row=F)
tb2.1 <- tapply(mod.data$ps,mod.data$pa3,summ.mean,se=F)
tb2.2 <- summ.mean(mod.data$ps)

tb.res <- rbind(tb1,c(tb2.1,tb2.2))

r1 <- summLogistic(step.mod)
r2 <- summLogistic(null.mod)
r3 <- summLogistic(psa.mod)

mod.res <- rbind(r1,"",r2,"",r3)

nmt <- colnames(tb.res)
nmm <- names(mod.res)

colnames(tb.res) <- c("c1","c2","c3")
colnames(mod.res) <-c("c1","c2","c3")

res_4.2<- rbind(nmt,tb.res,"",nmm,mod.res)
res_4.2


####################################################################
####################################################################
######### Question 5


### propensity socre
ps.mod <- glm(data125$pa3~.,data=prop.cov,family="binomial")

step.mod <- step(ps.mod,direction="backward",k=6)
summary(step.mod)

ps <- fitted(step.mod)

mod.data <- data.frame("out"=data125$Whole.pregnancy.outcome..mum.sepsis.free.and.livebirth.,
                       "pa3"=data125$pa3,"pa4"=data125$pa4,"ps"=ps)
mod.data$ps2 <- 1- mod.data$ps
mod.data$w <- mod.data$ps*(as.numeric(mod.data$pa3)-1) + mod.data$ps2*(2-as.numeric(mod.data$pa3))

null.mod <- glm(out~pa3,data=mod.data,family="binomial")
psa.mod <- glm(out~pa3,data=mod.data,family="binomial",weight=1/w)
sen.mod.5 <- glm(out~pa4,data=mod.data,family="binomial",weight=1/w)

### plot results
tb1 <- percTable(data125$Woman.sick,data125$pa2,row=F)
tb2.1 <- tapply(mod.data$ps,mod.data$pa3,summ.mean,se=F)
tb2.2 <- summ.mean(mod.data$ps)

tb.res <- rbind(tb1,c(tb2.1,tb2.2))

r1 <- summLogistic(step.mod)
r2 <- summLogistic(null.mod)
r3 <- summLogistic(psa.mod)

mod.res <- rbind(r1,"",r2,"",r3)

nmt <- colnames(tb.res)
nmm <- names(mod.res)

colnames(tb.res) <- c("c1","c2","c3")
colnames(mod.res) <-c("c1","c2","c3")

res_5 <- rbind(nmt,tb.res,"",nmm,mod.res)





####################################################################
####################################################################
######### Question 6

### propensity socre
ps.mod <- glm(data67$pa3~.,data=prop.cov.67,family="binomial")
step.mod <- step(ps.mod,direction="backward",k=6)

data67[1:3,1:10]

ps <- fitted(step.mod)
mod.data <- data.frame("out"=data67$Mum.well.and.baby.survive.to.discharge,"pa3"=data67$pa3,"pa4"=data67$pa4,"ps"=ps)

mod.data$ps2 <- 1- mod.data$ps
mod.data$w <- mod.data$ps*(as.numeric(mod.data$pa3)-1) + mod.data$ps2*(2-as.numeric(mod.data$pa3))

null.mod <- glm(out~pa3,data=mod.data,family="binomial")
psa.mod <- glm(out~pa3,data=mod.data,family="binomial",weight=1/w)
sen.mod.6 <- glm(out~pa4,data=mod.data,family="binomial",weight=1/w)

### plot results
tb1 <- percTable(data67$Mum.well.and.baby.survive.to.discharge,data67$pa2,row=F)
tb2.1 <- tapply(mod.data$ps,mod.data$pa3,summ.mean,se=F)
tb2.2 <- summ.mean(mod.data$ps)

tb.res <- rbind(tb1,c(tb2.1,tb2.2))

r1 <- summLogistic(step.mod)
r2 <- summLogistic(null.mod)
r3 <- summLogistic(psa.mod)

mod.res <- rbind(r1,"",r2,"",r3)

nmt <- colnames(tb.res)
nmm <- names(mod.res)

colnames(tb.res) <- c("c1","c2","c3")
colnames(mod.res) <-c("c1","c2","c3")

res_6 <- rbind(nmt,tb.res,"",nmm,mod.res)
res_6


####################################################################
####################################################################
######### Question 7

mod.data <- data.frame("out"=data67$Mum.well.and.baby.without.major.morbidity.at.discharge,
                       "pa3"=data67$pa3,"pa4"=data67$pa4,"ps"=ps)

mod.data$ps2 <- 1- mod.data$ps
mod.data$w <- mod.data$ps*(as.numeric(mod.data$pa3)-1) + mod.data$ps2*(2-as.numeric(mod.data$pa3))

null.mod <- glm(out~pa3,data=mod.data,family="binomial")
psa.mod <- glm(out~pa3,data=mod.data,family="binomial",weight=1/w)
sen.mod.7 <- glm(out~pa4,data=mod.data,family="binomial",weight=1/w)


### plot results
tb1 <- percTable(data67$Mum.well.and.baby.without.major.morbidity.at.discharge,data67$pa2,row=F)
tb2.1 <- tapply(mod.data$ps,mod.data$pa3,summ.mean,se=F)
tb2.2 <- summ.mean(mod.data$ps)

tb.res <- rbind(tb1,c(tb2.1,tb2.2))

r1 <- summLogistic(step.mod)
r2 <- summLogistic(null.mod)
r3 <- summLogistic(psa.mod)

mod.res <- rbind(r1,"",r2,"",r3)

nmt <- colnames(tb.res)
nmm <- names(mod.res)

colnames(tb.res) <- c("c1","c2","c3")
colnames(mod.res) <-c("c1","c2","c3")

res_7 <- rbind(nmt,tb.res,"",nmm,mod.res)




######## sensitivity analysis (erythromycin only)

sen.res <- rbind(summLogistic(sen.mod.1)[2,],summLogistic(sen.mod.2)[2,],
                 summLogistic(sen.mod.3)[2,],summLogistic(sen.mod.4)[2,],
                 summLogistic(sen.mod.4.2)[2,],summLogistic(sen.mod.5)[2,],
                 summLogistic(sen.mod.6)[2,],summLogistic(sen.mod.7)[2,])

kable(sen.res)


############ pro
