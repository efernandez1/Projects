##@@ Copyright @ 2015 by Eddie Fernandez
##@@ R v3.1.1
##@@ Attrition Project September 2015

setwd("~/Desktop/R Files")
.libPaths("~/Desktop/R_Packages/")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,tidyr,dplyr,magrittr, lubridate, treemap, plyr, car,
               caret, rpart, e1071, Hmisc, gridExtra, nnet, RCurl, Metrics,
               treemap, data.table)

csat <- read.csv("csat.csv", header = TRUE)
travel1 <- read.csv("travel1.csv", header = TRUE)
travel2 <- read.csv("travel2.csv", header = TRUE)
travel3 <- read.csv("travel3.csv", header = TRUE)
dcr <- read.csv("dcr.csv", header = TRUE)
pph <- read.csv("pph.csv", header = TRUE)
r12 <- read.csv("r12.csv", header = TRUE)
clockout <- read.csv("clockout.csv", header = TRUE)
termed <- read.csv("termed.csv", header = TRUE)
active <- read.csv("active.csv", header = FALSE)

new_csat <- read.csv("new_csat.csv", header = TRUE)
new_travel <- read.csv("new_travel.csv", header = TRUE)
new_dcr <- read.csv("new_dcr.csv", header = TRUE)
new_pph <- read.csv("new_pph.csv", header = TRUE)
new_r12 <- read.csv("new_r12.csv", header = TRUE)
new_termed <- read.csv("new_termed.csv", header = TRUE)
new_clockout <- read.csv("new_clockout.csv", header = TRUE)

csat <- rbind(new_csat, csat)
travel1 <- rbind(new_travel, travel1)
dcr <- rbind(new_dcr, dcr)
pph <- rbind(new_pph, pph)
r12 <- rbind(new_r12, r12)
termed <- rbind(new_termed, termed)
termed$Termination.Date <- as.Date(termed$Termination.Date, format = "%m/%d/%Y")
clockout <- rbind(new_clockout, clockout)
##SAVE DATA HERE

active = active[-c(1,2),]
active <- sapply(active, as.character)
colnames(active) <- active[1, ]
active <- as.data.frame(active[-1,], header = TRUE)
active$"Latest Hire Date" <- as.Date(active$"Latest Hire Date", format = "%m/%d/%Y")

# active <- active[active$"Latest Hire Date" < as.Date("2015-01-01"),] ## Change for testing
# termed <- termed[termed$Termination.Date > as.Date("2015-06-30"),] ## Change for testing

travel <- rbind(travel1, travel2, travel3)
travel$Queue.Date <- as.Date(travel$Queue.Date, format = "%m/%d/%Y")
travel$Travel <- as.numeric(travel$Travel)

## Only keep those who had working hours
pph$pph <- pph$POINTS/pph$WORKING_HOURS
pph <- pph[pph$WORKING_HOURS >0,]

clockout$ClockInHR <- clockout$ClockIn * 24
clockout$ClockOutHR <- clockout$ClockOut * 24
clockout$ClockInMIN <- clockout$ClockInHR * 60
clockout$ClockOutMIN <- clockout$ClockOutHR * 60
clockout$working_mins <- clockout$ClockOutMIN - clockout$ClockInMIN

## Rename to EID consistent
r12$EID <- as.factor(r12$EMPLOYEE_NUMBER)
r12$EMPLOYEE_NUMBER <- NULL
csat$EID <- as.factor(csat$EMPLOYEE_ID)
csat$EMPLOYEE_ID <- NULL
clockout$EID <- as.factor(clockout$Employee.Number)
clockout$Employee.Number <- NULL
pph$EID <- as.factor(pph$EMPLOYEE_NUMBER)
pph$EMPLOYEE_NUMBER <- NULL
dcr$EID <- as.factor(dcr$EMPLOYEE_NUMBER)
dcr$EMPLOYEE_NUMBER <- NULL
active$EID <- as.factor(active$"Employee Number")
active$"Employee Number" <- NULL
termed$EID <- as.factor(termed$Employee.Number)
termed$Employee.Number <- NULL
travel$EID <- as.factor(travel$Technician.Employee.Id)
travel$Technician.Employee.Id <- NULL

setDT(travel)
travel1 <- travel[, list(Travel = sum(Travel)), by = c("EID","Queue.Date")]
    
## Format dates
pph$WORK_DATE <- as.Date(pph$WORK_DATE, format = "%m/%d/%Y")
csat$LAST_CHANGED_DATE <- as.Date(csat$LAST_CHANGED_DATE, format = "%m/%d/%Y")
r12$le_comp_date <- as.Date(r12$le_comp_date, format = "%m/%d/%Y")
dcr$QUEUE_DATE <- as.Date(dcr$QUEUE_DATE, format = "%m/%d/%Y")
clockout$Punch.Date <- as.Date(clockout$Punch.Date, format = "%m/%d/%Y")
dcr$QUEUE_DATE <- as.Date(dcr$QUEUE_DATE, format = "%m/%d/%Y")

## CONSECUTIVE WORK DAYS
dcr <- dcr[order(dcr$EID, dcr$QUEUE_DATE), ]

dcr$consec_days <- 1

qd <- as.integer(dcr$QUEUE_DATE)
cd <- dcr$consec_days

for(i in 2:nrow(dcr)){
    if(qd[i] - qd[i - 1] == 1){
        cd[i] <- cd[i - 1] + 1      
    }
}

dcr$consec_days <- cd

# Both Weekend days
dcr$dow <- wday(dcr$QUEUE_DATE)
dcr$both_wknd <- 0
dcr$both_wknd[dcr$dow == 1 & dcr$consec_days > 1] <- 1

csat1 <- csat[csat$LAST_CHANGED_DATE >= as.Date("2015-07-01") & csat$overall_score > 9,]
csat1$overall_score <- csat1$overall_score * 0.9
csat2 <- csat[csat$LAST_CHANGED_DATE < as.Date("2015-07-01"),]
csat2$overall_score <- csat2$overall_score * 0.9
csat3 <- csat[csat$LAST_CHANGED_DATE >= as.Date("2015-07-01") & csat$overall_score <= 9,]
csat <- rbind(csat1, csat2, csat3)

#### SD, Z-Scores, Means, Lags ####

#Travel
setDT(travel)[,Queue.Date:=as.Date(Queue.Date, format="%m/%d/%Y")]
setkey(travel,EID,Queue.Date)     # ensures that Dates are ascending within EID
n      <- c(5,10,15,20)
avg_travel <- travel[,c(mean(Travel),lapply(n,function(i){if(.N < i) as.numeric(NA) else mean(tail(Travel,i))})),by=EID]
setnames(avg_travel,c("EID","avg_travel",paste0("travel",n)))
travel_sd <- travel[, list(sd = sd(Travel)), by = "EID"]
travel_z <- travel[, z:= scale(Travel), by = "EID"]
travel_stats <- merge(travel_z, travel_sd, all.x = TRUE)

travel_stats <- travel_stats[,c(mean(sd),lapply(n,function(i){if(.N < i) as.numeric(NA) else mean(tail(z,i))})),by=EID]
setnames(travel_stats,c("EID","Travel_SD",paste0("Travel_Z_",n)))

#PPH
setDT(pph)[,WORK_DATE:=as.Date(WORK_DATE, format="%m/%d/%Y")]
setkey(pph,EID,WORK_DATE)     # ensures that Dates are ascending within EID
n      <- c(5,10,15,20)
avg_pph <- pph[,c(mean(pph),lapply(n,function(i){if(.N < i) as.numeric(NA) else mean(tail(pph,i))})),by=EID]
setnames(avg_pph,c("EID","avg_pph",paste0("pph",n)))
pph_sd <- pph[, list(sd = sd(pph)), by = "EID"]
pph_z <- pph[, z:= scale(pph), by = "EID"]
pph_stats <- merge(pph_z, pph_sd, all.x = TRUE)

pph_stats <- pph_stats[,c(mean(sd),lapply(n,function(i){if(.N < i) as.numeric(NA) else mean(tail(z,i))})),by=EID]
setnames(pph_stats,c("EID","PPH_SD",paste0("PPH_Z_",n)))

#CSAT
setDT(csat)[,LAST_CHANGED_DATE:=as.Date(LAST_CHANGED_DATE, format="%m/%d/%Y")]
setkey(csat,EID,LAST_CHANGED_DATE)     # ensures that Dates are ascending within EID
n      <- c(5,10,15,20)
avg_csat <- csat[,c(mean(overall_score),lapply(n,function(i){if(.N < i) as.numeric(NA) else mean(tail(overall_score,i))})),by=EID]
setnames(avg_csat,c("EID","avg_csat",paste0("csat",n)))
csat_sd <- csat[, list(sd = sd(overall_score)), by = "EID"]
csat_z <- csat[, z:= scale(overall_score), by = "EID"]
csat_stats <- merge(csat_z, csat_sd, all.x = TRUE)

csat_stats <- csat_stats[,c(mean(sd),lapply(n,function(i){if(.N < i) as.numeric(NA) else mean(tail(z,i))})),by=EID]
setnames(csat_stats,c("EID","CSAT_SD",paste0("CSAT_Z_",n)))

#R12
setDT(r12)[,le_comp_date:=as.Date(le_comp_date, format="%m/%d/%Y")]
setkey(r12,EID,le_comp_date)     # ensures that Dates are ascending within EID
n      <- c(5,10,15,20)
avg_r12 <- r12[,c(mean(R12_percent),lapply(n,function(i){if(.N < i) as.numeric(NA) else mean(tail(R12_percent,i))})),by=EID]
setnames(avg_r12,c("EID","avg_r12",paste0("r12.",n)))
r12_sd <- r12[, list(sd = sd(R12_percent)), by = "EID"]
r12_z <- r12[, z:= scale(R12_percent), by = "EID"]
r12_stats <- merge(r12_z, r12_sd, all.x = TRUE)

r12_stats <- r12_stats[,c(mean(sd),lapply(n,function(i){if(.N < i) as.numeric(NA) else mean(tail(z,i))})),by=EID]
setnames(r12_stats,c("EID","r12_SD",paste0("r12_Z_",n)))


#Clockout
setDT(clockout)[,Punch.Date:=as.Date(Punch.Date, format="%m/%d/%Y")]
setkey(clockout,EID,Punch.Date)     # ensures that Dates are ascending within EID
n      <- c(5,10,15,20)
avg_clockout <- clockout[,c(mean(ClockOutMIN),lapply(n,function(i){if(.N < i) as.numeric(NA) else mean(tail(ClockOutMIN,i))})),by=EID]
setnames(avg_clockout,c("EID","avg_clockout",paste0("clockout",n)))
clockout_sd <- clockout[, list(sd = sd(ClockOutMIN)), by = "EID"]
clockout_z <- clockout[, z:= scale(ClockOutMIN), by = "EID"]
clockout_stats <- merge(clockout_z, clockout_sd, all.x = TRUE)

clockout_stats <- clockout_stats[,c(mean(sd),lapply(n,function(i){if(.N < i) as.numeric(NA) else mean(tail(z,i))})),by=EID]
setnames(clockout_stats,c("EID","Clockout_SD",paste0("Clockout_Z_",n)))

#DCR
setDT(dcr)[,QUEUE_DATE:=as.Date(QUEUE_DATE, format="%m/%d/%Y")]
setkey(dcr,EID,QUEUE_DATE)     # ensures that Dates are ascending within EID
n      <- c(5,10,15,20)
avg_dcr <- dcr[,c(mean(dcr),lapply(n,function(i){if(.N < i) as.numeric(NA) else mean(tail(dcr,i))})),by=EID]
setnames(avg_dcr,c("EID","avg_dcr",paste0("dcr",n)))
dcr_sd <- dcr[, list(sd = sd(dcr)), by = "EID"]
dcr_z <- dcr[, z:= scale(dcr), by = "EID"]
dcr_stats <- merge(dcr_z, dcr_sd, all.x = TRUE)

dcr_stats <- dcr_stats[,c(mean(sd),lapply(n,function(i){if(.N < i) as.numeric(NA) else mean(tail(z,i))})),by=EID]
setnames(dcr_stats,c("EID","DCR_SD",paste0("DCR_Z_",n)))


dcr2 <- dcr[, list(consec_days = mean(consec_days),
                   dow_avg = mean(dow),
                   both_wknd = mean(both_wknd)), by = "EID"]

pph2 <- pph[, list(points = mean(POINTS)),
            by = "EID"]

clockout$woy <- week(clockout$Punch.Date)
clockout$dummy <- 1
clockout2 <- clockout[, list(days_per_week = sum(dummy)), by = c("EID", "woy")]
clockout3 <- clockout2[, list(avg_days_week = mean(days_per_week)), by = "EID"]

## Merge

active$flag <- active$"Remote Tech Flag"
active$"Remote Tech Flag" <- NULL
termed$flag <- termed$Remote.Tech.Flag
termed$Remote.Tech.Flag <- NULL
active$region <- active$"Location DNS Region"
active$"Location DNS Region" <- NULL
termed$region <- termed$Location.DNS.Region
termed$Location.DNS.Region <- NULL
active$hire_date <- active$"Latest Hire Date"
active$"Latest Hire Date" <- NULL
termed$hire_date <- termed$Latest.Hire.Date
termed$Latest.Hire.Date <- NULL
active$level <- active$"Position Name"
active$"Position Name" <- NULL
termed$level <- termed$Full.Position.Combo
termed$Full.Position.Combo <- NULL
termed$choice <- termed$Voluntary...Involuntary
termed$Voluntary...Involuntary <- NULL
active$choice <- "active"
active$termed <- "active"
termed$termed <- "termed"

termed <- termed[which(termed$Department == 'DNS' | 
                           termed$Department == 'DNSC RESIDENTIAL' | 
                           termed$Department == 'DNS RESIDENTIAL - CA'),]

active$Termination.Date <- as.Date("1900-01-01")
termed$Termination.Date <- as.Date(termed$Termination.Date, format = "%m/%d/%Y")
active_df <- active[c(9:16)]
termed_df <- termed[c(11,15:21)]

termed_df$termed <- as.factor(termed_df$termed)
termed_df$hire_date <- as.Date(termed_df$hire_date, format = "%m/%d/%Y")
active_df$choice <- as.factor(active_df$choice)
active_df$termed <- as.factor(active_df$termed)

techs <- rbind(active_df, termed_df)

techs$level <- recode(techs$level,"c('Dish Tech 4',
    'OP3013.DNSC Residential.Dish Tech 4',
    'OP3013.DNS Residential - CA.Dish Tech 4',
    'Field Service Specialist IV') = 4")

techs$level <- recode(techs$level,"c('Dish Tech 1',
    'OP3010.DNSC Residential.Dish Tech 1',
    'OP3010.DNS Residential - CA.Dish Tech 1',
    'Field Service Specialist I') = 1")

techs$level <- recode(techs$level,"c('Dish Tech 2',
    'OP3011.DNSC Residential.Dish Tech 2',
    'OP3011.DNS Residential - CA.Dish Tech 2',
    'Field Service Specialist II') = 2")

techs$level <- recode(techs$level,"c('Dish Tech 3',
    'OP3012.DNSC Residential.Dish Tech 3',
    'OP3012.DNS Residential - CA.Dish Tech 3',
    'Field Service Specialist IV') = 3")

techs$level <- as.numeric(techs$level)
techs <- subset(techs, level >= 1 & level < 5)
techs$level <- as.factor(techs$level)

active_techs <- active[, 'EID', drop = FALSE]
termed_techs <- termed[, 'EID', drop = FALSE]

master <- techs

master <- merge(master, avg_csat, all.x = T)
master <- merge(master, avg_pph, all.x = T)
master <- merge(master, pph2, all.x = T)
master <- merge(master, avg_r12, all.x = T)
master <- merge(master, avg_dcr, all.x = T)
master <- merge(master, dcr2, all.x = T)
master <- merge(master, avg_clockout, all.x = T)
master <- merge(master, avg_travel, all.x = T)
master <- merge(master, travel_stats, all.x = T)
master <- merge(master, pph_stats, all.x = T)
master <- merge(master, csat_stats, all.x = T)
master <- merge(master, r12_stats, all.x = T)
master <- merge(master, dcr_stats, all.x = T)
master <- merge(master, clockout_stats, all.x = T)
master <- merge(master, clockout3, all.x = T)

master <- master[!duplicated(master$EID),]
row.names(master) <- master$EID
master$EID <- NULL

master$termed <- as.factor(master$termed)
master$hire_date <- as.Date(master$hire_date, format = "%m/%d/%Y")
# pph z10, r12_Z_10, csat 20 z, dcr sd, clockout20z,
## avg_pph, clockout20, travel40, avg_r12, csat20, avg_dcr, daily minutes
mastertest <- master[c(1,2,4,5,6,12,13,18,19,24,29,30,31,36,41,49,56,59,62,71,72)]
mastertest <- mastertest[complete.cases(master),]

mastertest <- subset(mastertest, choice == 'active' | choice == 'Voluntary Termination')
mastertest$choice <- NULL

ls()[!(ls() %in% c('mastertest'))]

rm(list= ls()[!(ls() %in% c('mastertest'))])