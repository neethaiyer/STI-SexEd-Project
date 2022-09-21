####Import Libraries ----

library(plyr)
library(tidyverse)
library(maps)
library(ggplot2)
library(dplyr)
library(lme4)
library(nlme)

#### Import Data ----

datTEST<-read.csv("/Users/evebanas/Desktop/SEXEDUPROJECT/DATATABLES/STI/CG0019.csv")

Gdata<-subset(datTEST, Indicator=="Gonorrhea") # table for gonorrhea
Cdata<-subset(datTEST, Indicator=="Chlamydia") # table for chlamydia


#### Basic Data Exploration ----

hist(Gdata$Rate.per.100000)
hist(Cdata$Rate.per.100000)

### Plots Rate vs Year ----

#Alabama

GAla<- subset(Gdata, Geography=="Alabama")
CAla<- subset(Cdata, Geography=="Alabama")

plot (CAla$Year, CAla$Rate.per.100000, 
      main="Rate vs Year Alabama", type = "l",
      bty="l", 
      xlab="Year",
      ylab="Rate per 100000", ylim=c(0,4000))
lines(GAla$Year, GAla$Rate.per.100000, col="blue")
legend("topright", c("Chlamydia", "Gonorrhea"), fill=c("black", "blue"))

#Alaska

GAlaska<- subset(Gdata, Geography=="Alaska")
CAlaska<- subset(Cdata, Geography=="Alaska")

plot (CAlaska$Year, CAlaska$Rate.per.100000, 
      main="Rate vs Year Alaska", type = "l",
      bty="l", 
      xlab="Year",
      ylab="Rate per 100000", ylim=c(0,4000))
lines(GAlaska$Year, GAlaska$Rate.per.100000, col="blue")
legend("topright", c("Chlamydia", "Gonorrhea"), fill=c("black", "blue"))

#Arizona
GAri<- subset(Gdata, Geography=="Arizona")
CAri<- subset(Cdata, Geography=="Arizona")

plot (CAri$Year, CAri$Rate.per.100000, 
      main="Rate vs Year Arizona", type = "l",
      bty="l", 
      xlab="Year",
      ylab="Rate per 100000", ylim=c(0,4000))
lines(GAri$Year, GAri$Rate.per.100000, col="blue")
legend("topright", c("Chlamydia", "Gonorrhea"), fill=c("black", "blue"))
