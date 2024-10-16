### Personalised SYnthetic Controls approach on UKOSS data

### Loading packages
library(psc)

### Setting working directory
setwd("/Users/richardjackson/Documents/local/UKOSS_CA//Data")


getwd()
### Loading data
load("ukossData.R")

###
class(data$OUT_MANAGE_E1_C1) <- "character"
data$OUT_MANAGE_E1_C1[which(is.na(data$OUT_MANAGE_E1_C1))] <- 3
data$OUT_MANAGE_E1_C1 <- factor(data$OUT_MANAGE_E1_C1)

### Creating model for control group
id <- which(data$exposure=="No")
cdata <- data[id,]


## model for outcome (based on glm)
null <- glm(out~BestguessEPPROMdate+AgeatEDD+AnyCerclage,family="binomial",data=cdata)
step <- step(null)

ukoss.mod <- glm(out~BestguessEPPROMdate,family="binomial",data=cdata)

## saving model
setwd("~/Documents/local/UKOSS_CA/Personalised Synthetic Controls")
save(ukoss.mod,file="ukoss.mod.R")

edata <- data[-id,]


save(edata,file="bin.data.R")

edata[1:3,]

#### Personalised Synthetic Controls
remove.packages("psc")
library(devtools)
install_github("RichJJackson/psc",ref="2024_05_15")
library(psc)


table(data$out,data$exposure)

psc <- pscfit(ukoss.mod,edata)


lapply(psc$posterior,mean)


plot(psc$posterior$DIC,psc$posterior$beta)


plot(psc$posterior$beta,typ="s")
plot(psc$posterior$OUT_MANAGE_E1_C11,typ="s")
psc$posterior[1:3,]
plot.psc(psc)
plot(psc)

### Plotting 









