## Costbook 
## report_100_100T_macro
## Nicole Jarvie - 19/07/2019

## Install packages
library(RODBC)      #Uses functions odbcConnect, sqlQuery, and odbcClose.
library(tidyverse)
library(haven)
library(stringr)
library(xlsx)
library(ggplot2)

## This is the section for creating what would have been the output from SPSS directly from the database using an ODBC connection.

#Get user id and password
.uid <- .rs.askForPassword("What is your user id?")
.pwd <- .rs.askForPassword("What is your LDAP password?")

###Connect to data
connect <- odbcConnect("APEXP", uid = .uid, pwd=.pwd, rows_at_time=50)

#erase user id and password
rm(.pwd, .uid)

#This line gathering data from the costbook takes a few minutes, the financial year needs to be updated with the appropriate years

data <- sqlQuery(connect,paste( "SELECT T0.O_UNIFIED_HB AS provider, T0.O_LOCATION AS Location, T1.SF_FINANCIAL_YEAR AS 
    YEARID, T2.CB_FORMNAME AS FORMNAME, T2.CB_LINENO AS LINENO, T2.CB_COL1 AS COL1, T2.CB_COL2 AS COL2, 
    T2.CB_COL3 AS COL3, T2.CB_COL4 AS COL4, T2.CB_COL5 AS COL5, T2.CB_COL6 AS COL6, T2.CB_COL7 AS COL7, 
    T2.CB_COL8 AS COL8, T2.CB_COL9 AS COL9, T2.CB_COL10 AS COL10, T2.CB_COL11 AS COL11, T2.CB_COL12 AS COL12,
    T2.CB_COL13 AS COL13, T2.CB_COL14 AS COL14, T2.CB_COL15 AS COL15, T2.CB_COL16 AS COL16, T2.CB_COL17 AS COL17, 
    T2.CB_COL18 AS COL18, T2.CB_COL19 AS COL19 FROM CBDCS.ORGANISATION T0, CBDCS.SUBMITTED_FORMS T1, CBDCS.COSTBOOK_DATA T2 
    WHERE SF_FINANCIAL_YEAR = 2014 and T1.SF_ID = T2.CB_SF_ID and T0.O_ID = T1.SF_O_ID and T1.SF_ID = T2.CB_SF_ID and T0.O_ID = T1.SF_O_ID"))

## Close ODBC connection
odbcClose(connect)

## Edit provider codes

data$PROVIDER <- as.character(data$PROVIDER)

data$PROVIDER[str_sub(data$PROVIDER,2,2) == 'A']<- "SAA20"
data$PROVIDER[str_sub(data$PROVIDER,2,2) == 'C'] <- "SCA20"
data$PROVIDER[str_sub(data$PROVIDER,2,2) == 'F'] <- "SFA20"
data$PROVIDER[str_sub(data$PROVIDER,2,2) == 'G'] <- "SGA20"
data$PROVIDER[str_sub(data$PROVIDER,2,2) == 'H'] <- "SHA20"
data$PROVIDER[str_sub(data$PROVIDER,2,2) == 'L'] <- "SLA20"
data$PROVIDER[str_sub(data$PROVIDER,2,2) == 'N'] <- "SNA20"
data$PROVIDER[str_sub(data$PROVIDER,2,2) == 'S'] <- "SSA20"
data$PROVIDER[str_sub(data$PROVIDER,2,2) == 'T'] <- "STA20"
data$PROVIDER[str_sub(data$PROVIDER,2,2) == 'V'] <- "SVA20"
data$PROVIDER[str_sub(data$PROVIDER,2,2) == 'Y'] <- "SYA20"

data$COL1[is.na(data$COL1)] <- 0
data$COL2[is.na(data$COL2)] <- 0
data$COL3[is.na(data$COL3)] <- 0

## TO DO - CHANGE TO MAKE GENERIC 
## LOAD IN POPULATIONS

populations <- read.xlsx(file="/../conf/costbook_rserv/Populations.xlsx", sheetIndex = 1, header = TRUE,stringsAsFactors = FALSE )

populations17 = data_frame(populations[,1],populations[,2])
colnames(populations17) <- c("PROVIDER","POPULATION")
populations17$PROVIDER <- as.character(populations17$PROVIDER)
populations17$POPULATION <- as.numeric(populations17$POPULATION)

femalepopulations <- read.xlsx(file="/../conf/costbook_rserv/Populations.xlsx", sheetIndex = 2, header = TRUE,stringsAsFactors = FALSE )
femalepopulations17 = data_frame(femalepopulations [,1],femalepopulations [,2])
colnames(femalepopulations17) <- c("PROVIDER","POPULATION")
femalepopulations17$PROVIDER <- as.character(femalepopulations17$PROVIDER)
femalepopulations17$POPULATION <- as.numeric(femalepopulations17$POPULATION)


## TOTAL OPERATING COSTS ## 

###################################
## HOSPITAL COSTS - rep line 20##
###################################

## Total Gross - Hospital Running Costs

hospital_running_costs= data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 640) %>%
  ##separate(unitcode,c("board","location"), sep = 5) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm=TRUE))

## Health Services purchases from sub-contractors

health_services_subcontractors= data[,1:8] %>%
  filter(FORMNAME == "SFR 24.0" & LINENO %in% c(120,130,140,150,180,190,200,230,240,250,280,290,300,330,340,350,380,390,420,430)) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm=TRUE))

temp1 <- rbind(hospital_running_costs,health_services_subcontractors)

temp1 = temp1 %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost)) 
temp1 = add_column(temp1, repline = "020") 

scottemp1 = temp1 %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost))
scottemp1 = add_column(scottemp1,repline ="020")


## MAKE FILE NAME GENERIC 
exsumc <- rbind(temp1, scottemp1)

##Keep hospital sector figures for % calculation 
hospital_sector <- exsumc

## Remove unneccessary tables 
rm(health_services_subcontractors, hospital_running_costs,temp1, scottemp1)

###################################
## COMMUNITY SECTOR - repline 30 ## 
###################################

## Gross Community Service

## Board figures 
gross_community_service= data[,1:8] %>%
  filter(FORMNAME == "SFR 8.2" & LINENO == 870) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm=TRUE))

community_services_subcontractors=  data[,1:8] %>%
  filter(FORMNAME == "SFR 24.0" & LINENO %in% c(310,360,460,471,472)) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

temp2 <- rbind(gross_community_service,community_services_subcontractors)

temp2 = temp2%>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

temp2 = add_column(temp2, repline = "030")

## Scotland figure
scottemp2 = temp2 %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))
scottemp2 = add_column(scottemp2,repline ="030")

community_sector <- rbind(temp2, scottemp2)

exsumc <- rbind(exsumc,community_sector)

## Remove unneccessary tables 
rm(community_services_subcontractors, gross_community_service,temp2, scottemp2)

########################################
## FAMILY HEALTH SECTOR - rep line 40 ##
########################################

family_health_services= data[,1:8] %>%
  filter(FORMNAME == "SFR 8.4"& LINENO == 340) %>%
  mutate(cost = COL3) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

family_health_services <- add_column(family_health_services,repline = "040")

scottemp3 = family_health_services %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))
scottemp3 = add_column(scottemp3,repline ="040")


exsumc <- rbind(exsumc, family_health_services, scottemp3)
rm(family_health_services, scottemp3)

######################################
## RESOURCE TRANSFER - repl line 45 ##
######################################

resource_transfer= data[,1:8] %>%
  filter(FORMNAME == "SFR 24.0" & LINENO %in% c(311,361,391,431,470)) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))
resource_transfer = add_column(resource_transfer,repline ="045")

scottemp4 <- resource_transfer %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost, na.rm = TRUE))
scottemp4 = add_column(scottemp4,repline ="045")


exsumc <- rbind(exsumc, resource_transfer, scottemp4)
rm(resource_transfer, scottemp4)

################################
## Total costs - rep line 10 ###
################################

totalcosts = exsumc[,1:2] %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost, na.rm =  TRUE))
totalcosts = add_column(totalcosts,repline ="010") 


exsumc <- rbind(exsumc, totalcosts)
rm(totalcosts)

## At this point exsumsc should have 79 observaitions containing:
## Total costs, hospital sector, community sector, family health sector and resource transfer figures for each board 
## Rows 1-5 of R100

###################################
## HOSPITAL SECTOR - EXPENDITURE ##
###################################

## Total staff costs - repline 50 

totalstaffcosts = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.1B" & LINENO == 540) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm =  TRUE))
totalstaffcosts = add_column(totalstaffcosts,repline ="050")

scottemp5 <- totalstaffcosts %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost, na.rm = TRUE))
scottemp5 = add_column(scottemp5,repline ="050")

totalstaffcosts <- rbind(totalstaffcosts, scottemp5)
exsumc <- rbind(exsumc, totalstaffcosts)
rm(scottemp5)

## Total staff costs - as % of hospital running costs - repline 60

totalstaffcostspercent <- left_join(totalstaffcosts,hospital_sector,by = "PROVIDER")

totalstaffcostspercent <- totalstaffcostspercent[,c(1,2,4)]%>%
  mutate(totalcost = (totalcost.x/totalcost.y)*100)

totalstaffcostspercent <- totalstaffcostspercent [,c(1,4)]
totalstaffcostspercent = add_column(totalstaffcostspercent, repline = "060")

exsumc <- rbind(exsumc,totalstaffcostspercent)

rm(hospital_sector,totalstaffcosts, totalstaffcostspercent)

## Medical staff costs - rep line 70

medical = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.1B" & LINENO == 410) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))
medical <- add_column(medical, repline = "070")

scottemp6 <- medical %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost, na.rm = TRUE))
scottemp6 = add_column(scottemp6,repline ="070")

exsumc <- rbind(exsumc, medical, scottemp6)
rm(medical,scottemp6)

## Nursing staff costs - rep line 80

nursing = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.1B" & LINENO == 430) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
nursing<- add_column(nursing, repline = "080")

scottemp7 <- nursing %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost, na.rm = TRUE))
scottemp7 = add_column(scottemp7,repline ="080")

exsumc <- rbind(exsumc, nursing, scottemp7)
rm(nursing,scottemp7)

## AHP - repl line 85 

ahp = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO %in% c(160,250,260,270,280,290)) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
ahp<- add_column(ahp, repline = "085")

scottemp8 <- ahp %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost, na.rm =TRUE))
scottemp8 = add_column(scottemp8,repline ="085")

exsumc <- rbind(exsumc, ahp, scottemp8)
rm(ahp,scottemp8)

## Total staff numbers (wtes) - repl line 90

wte = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.1B" & LINENO == 540) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
wte <- add_column(wte,repline = "090")

scottemp9 <- wte %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost, na.rm = TRUE))
scottemp9 = add_column(scottemp9,repline ="090")

exsumc <- rbind(exsumc, wte, scottemp9)
rm(wte,scottemp9)

## Pharamcy services - rep line 100

pharmacy = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO %in% c(200,210,220,230,240)) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
pharmacy <- add_column(pharmacy, repline = "100")

scottemp10 <- pharmacy %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost, na.rm = TRUE))
scottemp10 = add_column(scottemp10,repline ="100")

exsumc <- rbind(exsumc, pharmacy, scottemp10)
rm(pharmacy, scottemp10)

# Drug expenditure - rep line 110
drug = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 210) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
drug <- add_column(drug, repline = "110")

scottemp11 <- drug %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost, na.rm = TRUE))
scottemp11 = add_column(scottemp11,repline ="110")

exsumc <- rbind(exsumc,drug,scottemp11)
rm(drug,scottemp11)

## Admin expenditure - rep line 120

admin = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 410) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
admin <- add_column(admin, repline = "120")

scottemp12 <- admin %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost, na.rm = TRUE))
scottemp12 = add_column(scottemp12,repline ="120")

exsumc <- rbind(exsumc,admin,scottemp12)
rm(admin, scottemp12)

## Admin staff - repl line 121

adminstaff = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 410) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
adminstaff <- add_column(adminstaff, repline = "121")

scottemp13 <- adminstaff%>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost, na.rm = TRUE))
scottemp13 = add_column(scottemp13,repline ="121")

exsumc <- rbind(exsumc,adminstaff,scottemp13)
rm(adminstaff,scottemp13)

## Admin supplies - repl line 122

adminsupplies = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 410) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost))
adminsupplies <- add_column(adminsupplies, repline = "122")

scottemp14 <- adminsupplies%>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost, na.rm = TRUE))
scottemp14 = add_column(scottemp14,repline ="122")

exsumc <- rbind(exsumc,adminsupplies,scottemp14)
rm(adminsupplies,scottemp14)

## Catering - rep line 150 

catering = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 330) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
catering <- add_column(catering, repline = "150")

scottemp15 <- catering%>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))
scottemp15 = add_column(scottemp15,repline ="150")

catering <- rbind(catering,scottemp15)
exsumc <- rbind(exsumc,catering)
rm(scottemp15)

## Catering - cost per inpatient week (£) 

cateringcost = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 330) %>%
  mutate(cost = COL3) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

scottemp16 <- cateringcost%>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

cateringcost <- rbind(cateringcost,scottemp16)
cateringcostfinal <- left_join(catering,cateringcost,by ="PROVIDER")

cateringcostfinal = cateringcostfinal %>% 
  mutate(totalcost = totalcost.x/totalcost.y)
cateringcostfinal = cateringcostfinal[,c(1,5)]
cateringcostfinal = add_column(cateringcostfinal,repline ="160")

exsumc <- rbind(exsumc,cateringcostfinal)

rm(scottemp16,catering,cateringcost,cateringcostfinal)

## Laundry - rep line 170 

laundry = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 380) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
laundry <- add_column(laundry, repline = "170")

scottemp17 <- laundry%>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))
scottemp17 = add_column(scottemp17,repline ="170")

exsumc <- rbind(exsumc,laundry,scottemp17)
rm(laundry,scottemp17)

## Cleaning - rep line 180 

cleaning = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 480) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
cleaning <- add_column(cleaning, repline = "180")

scottemp18 <- cleaning%>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))
scottemp18 = add_column(scottemp18,repline ="180")

cleaning <- rbind(cleaning,scottemp18)

exsumc <- rbind(exsumc,cleaning)

## Cleaning cost per square metre - rep line 190

cleaningcost = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 480) %>%
  mutate(cost = COL3) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

scottemp19 <- cleaningcost%>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

cleaningcost <- rbind(cleaningcost,scottemp19)
cleaningcostfinal <- left_join(cleaning,cleaningcost,by ="PROVIDER")

cleaningcostfinal = cleaningcostfinal %>% 
  mutate(totalcost = totalcost.x/totalcost.y) 
cleaningcostfinal = cleaningcostfinal[,c(1,5)]
cleaningcostfinal = add_column(cleaningcostfinal,repline ="190")

exsumc <- rbind(exsumc,cleaningcostfinal)

rm(scottemp18,scottemp19,cleaning,cleaningcost,cleaningcostfinal)

## Portering - rep line  200

portering = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 390) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
portering  <- add_column(portering , repline = "200")

scottemp20 <- portering%>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))
scottemp20 = add_column(scottemp20,repline ="200")


exsumc <- rbind(exsumc,portering,scottemp20)
rm(portering,scottemp20)

## Property maintenance - rep line  210

property = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 450) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
property   <- add_column(property , repline = "210")

scottemp21 <- property%>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))
scottemp21 = add_column(scottemp21,repline ="210")


exsumc <- rbind(exsumc,property,scottemp21)
rm(property,scottemp21)

## Energy - rep line  220

energy = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 530) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
energy   <- add_column(energy , repline = "220")

scottemp22 <- energy%>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))
scottemp22 = add_column(scottemp22,repline ="220")


exsumc <- rbind(exsumc,energy,scottemp22)
rm(energy,scottemp22)


## Teaching & Research - rep line  240
## Note added in a negative value here as values are showing as negative in file 

teaching = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 655) %>%
  mutate(cost = -COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
teaching <- add_column(teaching  , repline = "240")

scottemp23 <- teaching%>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))
scottemp23 = add_column(scottemp23,repline ="240")


exsumc <- rbind(exsumc,teaching,scottemp23)
rm(teaching,scottemp23)

#####################################################
## exsumc should have 419 observations at this point 
## Covering up to row 34 of R100 
####################################################


#######################################
## Hospital sector - patient statistics
#######################################

## Number of beds - repline 270

nobeds = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.3" & LINENO == 550) %>%
  mutate(cost = COL1/365) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
nobeds <- add_column(nobeds  , repline = "270")

scottemp24 <- nobeds %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))
scottemp24 = add_column(scottemp24,repline ="270")

exsumc <- rbind(exsumc,nobeds,scottemp24)
rm(scottemp24,nobeds)

## Inpatient activity - rep line 280

inpatient1 = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.3" & LINENO %in% c(330,340,360,370,380,390,400)) %>%
  mutate(cost = COL3) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

scottemp25 <- inpatient1 %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))


inpatient1 <- rbind(inpatient1,scottemp25)

inpatient2 = data[,1:8] %>%
  filter(FORMNAME == "SFR 5.3" & LINENO %in% c(550)) %>%
  mutate(cost = COL3) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

scottemp26 <- inpatient2 %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

inpatient2 <- rbind(inpatient2,scottemp26)

inpatient <- left_join(inpatient1,inpatient2,"PROVIDER")
inpatient = inpatient %>%
  mutate(cost = totalcost.y - totalcost.x) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
inpatient <- add_column(inpatient,repline ="280")

exsumc <- rbind(exsumc,inpatient)
rm(inpatient1,inpatient2,scottemp25,scottemp26)

## Inpatient activity cost per case - rep line 290

casecost1 = data[,1:24] %>%
  filter(FORMNAME == "SFR 5.3" & LINENO %in% c(330,340,360,370,380,390,400)) %>%
  mutate(cost = COL19) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

scottemp27 <- casecost1 %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))


casecost1 <- rbind(casecost1,scottemp27)

casecost2 = data[,1:24] %>%
  filter(FORMNAME == "SFR 5.3" & LINENO %in% c(550)) %>%
  mutate(cost = COL19) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

scottemp28 <- casecost2 %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

casecost2 <- rbind(casecost2,scottemp28)

casecost<- left_join(casecost1,casecost2,"PROVIDER")
casecost = casecost %>%
  mutate(cost = totalcost.y - totalcost.x) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))


casecost <- left_join(inpatient,casecost, by="PROVIDER")
casecost = casecost[,c(1,2,4)] %>%
  mutate(totalcost = totalcost.y/totalcost.x)
casecost = casecost[,c(1,4)]
casecost = add_column(casecost,repline ="290")

exsumc <- rbind(exsumc,casecost)
rm(casecost1,casecost2,casecost,scottemp27,scottemp28, inpatient)


## Cost of inpatient surgery - rep line 291 
##### NEED TO VERIFY #############

surgery = data[,1:24] %>%
  filter( ( LINENO >= 121 & LINENO <= 200 & FORMNAME == "SFR 5.3") | (LINENO %in% c(420,460,465,470,495,500,505)) & FORMNAME == "SFR 5.3") %>%
  mutate(cost1 = COL19, cost2 = COL3) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = (totalcost1 = sum(cost1, na.rm = TRUE))/(tolcost2 =sum(cost2,na.rm=TRUE)))
surgery <- add_column(surgery,repline ="291")

scottemp28 <- data[,1:24] %>%
  filter( ( LINENO >= 121 & LINENO <= 200 & FORMNAME == "SFR 5.3") | (LINENO %in% c(420,460,465,470,495,500,505)) & FORMNAME == "SFR 5.3") %>%
  mutate(cost1 = COL19, cost2 = COL3, PROVIDER= "AAAAA") %>%
  summarise(totalcost = (totalcost1 = sum(cost1, na.rm = TRUE))/(tolcost2 =sum(cost2,na.rm=TRUE)))
scottemp28 <- add_column(scottemp28, PROVIDER = "AAAAA")
scottemp28 <- add_column(scottemp28,repline ="291")

exsumc <- rbind(exsumc,surgery, scottemp28)
rm(surgery,scottemp28)

## Cost of intensive care - rep line 292

intensive = data[,1:24] %>%
  filter(LINENO == 500  & FORMNAME == "SFR 5.3") %>%
  mutate(cost1 = COL19, cost2 = COL3) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = (totalcost1 = sum(cost1, na.rm = TRUE))/(tolcost2 =sum(cost2,na.rm=TRUE)))
intensive <- add_column(intensive,repline ="292")

scottemp29 <- data[,1:24] %>%
  filter( LINENO == 500 & FORMNAME == "SFR 5.3") %>%
  mutate(cost1 = COL19, cost2 = COL3, PROVIDER= "AAAAA") %>%
  summarise(totalcost = (totalcost1 = sum(cost1, na.rm = TRUE))/(tolcost2 =sum(cost2,na.rm=TRUE)))
scottemp29 <- add_column(scottemp29, PROVIDER = "AAAAA")
scottemp29 <- add_column(scottemp29,repline ="292")

exsumc <- rbind(exsumc,intensive, scottemp29)
rm(intensive, scottemp29)

## Long Stay - rep line 300

longstay = data[,1:24] %>%
  filter(LINENO %in% c(330,340,360,370,380,390,400)  & FORMNAME == "SFR 5.3") %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = (totalcost= sum(cost, na.rm = TRUE))/7)
longstay  <- add_column(longstay,repline ="300")

scottemp30 <- longstay %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))
scottemp30 <- add_column(scottemp30, repline = "300")

exsumc <- rbind(exsumc,longstay, scottemp30)
rm(longstay, scottemp30)

## Cost per week - rep line 310 
weekcost = data[,1:24] %>%
  filter(LINENO %in% c(330,340,360,370,380,390,400)  & FORMNAME == "SFR 5.3") %>%
  mutate(cost1 = COL19, cost2 = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost1,na.rm = TRUE)/ (sum(cost2, na.rm = TRUE)/7))
weekcost  <- add_column(weekcost,repline ="310")

scottemp31 <- data[,1:24] %>%
  filter(LINENO %in% c(330,340,360,370,380,390,400)  & FORMNAME == "SFR 5.3") %>%
  mutate(cost1 = COL19, cost2 = COL2) %>%
  summarise(totalcost = sum(cost1,na.rm = TRUE)/ (sum(cost2, na.rm = TRUE)/7))
scottemp31 <- add_column(scottemp31, PROVIDER = "AAAAA")
scottemp31 <- add_column(scottemp31,repline ="310")

exsumc <- rbind(exsumc,weekcost, scottemp31)
rm(weekcost, scottemp31)

## Daycases - repline 320

daycase = data[,1:24] %>%
  filter(LINENO == 550 & FORMNAME == "SFR 5.5") %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))
daycase <- add_column(daycase,repline ="320")

scottemp32 <- daycase %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))
scottemp32 <- add_column(scottemp32, repline = "320")

daycase <- rbind(daycase,scottemp32)

exsumc <- rbind(exsumc,daycase)
rm(scottemp32)

## Daycase cost per case - repline 330

daycasecost = data[,1:24] %>%
  filter(LINENO == 550 & FORMNAME == "SFR 5.5") %>%
  mutate(cost = COL17) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

scottemp33 <- daycasecost %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

daycasecost <- rbind(daycasecost,scottemp33)

daycasecost<- left_join(daycasecost,daycase,"PROVIDER")
daycasecost = daycasecost %>%
  mutate(cost = totalcost.x / totalcost.y) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
daycasecost <- add_column(daycasecost,repline ="330")

exsumc <- rbind(exsumc,daycasecost)
rm(scottemp33,daycasecost,daycase)


## Day patients - repline 340

daypatients = data[,1:24] %>%
  filter(LINENO == 310 & FORMNAME == "SFR 5.9") %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))
daypatients <- add_column(daypatients,repline ="340")

scottemp34 <- daypatients %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))
scottemp34 <- add_column(scottemp34, repline = "340")

daypatients <- rbind(daypatients,scottemp34)

exsumc <- rbind(exsumc,daypatients)
rm(scottemp34)

## Day patients cost per case - repline 350

daypatientscost = data[,1:24] %>%
  filter(LINENO == 310 & FORMNAME == "SFR 5.9") %>%
  mutate(cost = COL15) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

scottemp35 <- daypatientscost %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

daypatientscost <- rbind(daypatientscost,scottemp35)

daypatientscost<- left_join(daypatientscost,daypatients,"PROVIDER")
daypatientscost = daypatientscost %>%
  mutate(cost = totalcost.x / totalcost.y) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))
daypatientscost <- add_column(daypatientscost,repline ="350")

exsumc <- rbind(exsumc,daypatientscost)
rm(scottemp35,daypatientscost,daypatients)

## Attendances at outpatient clinics - repline 360

attendancesop1 = data[,1:24] %>%
  filter(LINENO %in% c(550) & FORMNAME %in% c("SFR 5.7","SFR 5.7N")) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))


attendancesop2 = data[,1:24] %>%
  filter(LINENO %in% c(210) & FORMNAME %in% c("SFR 5.7","SFR 5.7N")) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

attendancesop <- left_join(attendancesop1,attendancesop2,"PROVIDER")
attendancesop  = attendancesop  %>%
  mutate(cost = totalcost.x - totalcost.y) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

scottemp36 <- attendancesop %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

attendancesop <- rbind(scottemp36,attendancesop)
attendancesop <- add_column(attendancesop,repline ="360")

exsumc <- rbind(exsumc,attendancesop)
rm(attendancesop1,attendancesop2,scottemp36)

## Attendances at outpatient clinics - cost per attendance - repline 370

attendancesopcost1a = data[,1:24] %>%
  filter(LINENO %in% c(550) & FORMNAME %in% c("SFR 5.7")) %>%
  mutate(cost = COL18) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

attendancesopcost1b = data[,1:24] %>%
  filter(LINENO %in% c(210) & FORMNAME %in% c("SFR 5.7")) %>%
  mutate(cost = COL18) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

attendancesopcost1 <- left_join(attendancesopcost1a,attendancesopcost1b, "PROVIDER")

attendancesopcost1  = attendancesopcost1  %>%
  mutate(cost = totalcost.x - totalcost.y) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

attendancesopcost2a = data[,1:24] %>%
  filter(LINENO %in% c(550) & FORMNAME %in% c("SFR 5.7N")) %>%
  mutate(cost = COL14) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

attendancesopcost2b = data[,1:24] %>%
  filter(LINENO %in% c(210) & FORMNAME %in% c("SFR 5.7N")) %>%
  mutate(cost = COL14) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

attendancesopcost2 <- left_join(attendancesopcost2a,attendancesopcost2b, "PROVIDER")

attendancesopcost2  = attendancesopcost2  %>%
  mutate(cost = totalcost.x - totalcost.y) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))


attendancesopcost <- left_join(attendancesopcost1,attendancesopcost2, "PROVIDER")

attendancesopcost  = attendancesopcost  %>%
  mutate(cost = totalcost.x + totalcost.y) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

scottemp37 <- attendancesopcost %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))


attendancesopcost <- rbind(scottemp37,attendancesopcost)

attendancesopcost <- left_join(attendancesop,attendancesopcost, "PROVIDER")

attendancesopcost  = attendancesopcost  %>%
  mutate(cost = totalcost.y / totalcost.x) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

attendancesopcost <- add_column(attendancesopcost,repline ="370")

exsumc <- rbind(exsumc,attendancesopcost)

rm(attendancesopcost1a,attendancesopcost1b,attendancesopcost1,attendancesopcost2a,attendancesopcost2b,attendancesopcost2,attendancesop,attendancesopcost,scottemp37)

## Attendances at A&E - repline 371

attendancesae = data[,1:24] %>%
  filter(LINENO %in% c(210) & FORMNAME %in% c("SFR 5.7","SFR 5.7N")) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

scottemp38 <- attendancesae %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

attendancesae <- rbind(attendancesae,scottemp38)
attendancesae <- add_column(attendancesae,repline ="371")
exsumc <- rbind(exsumc,attendancesae)
rm(scottemp38)

## Attendances at A&E - cost per attendance - repline 372

attendancesaecost1 = data[,1:24] %>%
  filter(LINENO %in% c(210) & FORMNAME %in% c("SFR 5.7")) %>%
  mutate(cost = COL18) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

attendancesaecost2 = data[,1:24] %>%
  filter(LINENO %in% c(210) & FORMNAME %in% c("SFR 5.7N")) %>%
  mutate(cost = COL14) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

attendancesaecost <- left_join(attendancesaecost1,attendancesaecost2,"PROVIDER")
attendancesaecost  = attendancesaecost  %>%
  mutate(cost = totalcost.x + totalcost.y) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

scottemp39 <- attendancesaecost %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

attendancesaecost <- rbind(attendancesaecost,scottemp39)

rm(attendancesaecost1,attendancesaecost2)

attendancesaecost <- left_join(attendancesaecost,attendancesae,"PROVIDER")
attendancesaecost  = attendancesaecost  %>%
  mutate(cost = totalcost.x / totalcost.y) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

scottemp40 <- attendancesaecost %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

attendancesaecost <- add_column(attendancesaecost,repline ="372")

exsumc <- rbind(exsumc,attendancesaecost)
rm(attendancesae,attendancesaecost,scottemp39,scottemp40)


## Covers up to line 40 of R100 publication - should be 664 observations in exsumc at this point


###################################
### Hospital Sector - Services ###
###################################


## Theatres ##
## Number of theatres - repline - 380

theatres <- data[,1:24] %>%
  filter(LINENO %in% c(150) & FORMNAME %in% c("SFR 5.10")) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

scottemp41 <- theatres %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

theatres <- rbind(theatres,scottemp41)
theatres <- add_column(theatres,repline ="380")

exsumc <- rbind(exsumc,theatres)
rm(scottemp41)

## Theatre costs - repline - 390

theatrecost <- data[,1:24] %>%
  filter(LINENO %in% c(170) & FORMNAME %in% c("SFR 5.10")) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

scottemp42 <- theatrecost %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

theatrecost <- rbind(theatrecost,scottemp42)
rm(scottemp42)

theatrecost <- left_join(theatrecost,theatres, by ="PROVIDER")

theatrecost  = theatrecost  %>%
  mutate(cost = totalcost.x / totalcost.y) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

theatrecost <- add_column(theatrecost,repline ="390")

exsumc <- rbind(exsumc,theatrecost)
rm(theatrecost)

## Theatre usage - repline - 400

theatreusage <- data[,1:24] %>%
  filter(LINENO %in% c(160) & FORMNAME %in% c("SFR 5.10")) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

scottemp43 <- theatreusage %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

theatreusage <- rbind(theatreusage,scottemp43)

theatreusage <- left_join(theatreusage,theatres, by ="PROVIDER")

theatreusage  = theatreusage  %>%
  mutate(cost = totalcost.x / totalcost.y /52) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost, na.rm = TRUE))

theatreusage <- add_column(theatreusage,repline ="400")

exsumc <- rbind(exsumc,theatreusage)
rm(theatreusage,scottemp43,theatres)

########## Radiology ############

## Total expenditure - repline - 410

radio <- data[,1:24] %>%
  filter(LINENO %in% c(180) & FORMNAME %in% c("SFR 5.11")) %>%
  mutate(cost = COL7) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

scottemp44 <- radio  %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

radio <- rbind(radio,scottemp44)
radio <- add_column(radio,repline ="410")

exsumc <- rbind(exsumc,radio)
rm(radio,scottemp44)

## Total expenditure - repline - 420

radioexam <- data[,1:24] %>%
  filter(LINENO %in% c(120,130,140,150,160) & FORMNAME %in% c("SFR 5.11")) %>%
  mutate(cost = COL8) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

scottemp45 <- radioexam   %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

radioexam <- rbind(radioexam,scottemp45)
radioexam <- add_column(radioexam,repline ="420")

exsumc <- rbind(exsumc,radioexam)
rm(radioexam,scottemp45)

## CT Scan - repline - 430

ctscan <- data[,1:24] %>%
  filter(LINENO %in% c(120) & FORMNAME %in% c("SFR 5.11")) %>%
  mutate(cost1 = COL7, cost2 =COL8) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost1 = sum(cost1,na.rm = TRUE), totalcost2 = sum(cost2,na.rm = TRUE))

scottemp46 <- ctscan  %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost1 = sum(totalcost1,na.rm = TRUE),totalcost2 = sum(totalcost2,na.rm = TRUE))

ctscan <- rbind(ctscan,scottemp46)

ctscan <- ctscan %>%
  mutate(totalcost = totalcost1/totalcost2) %>%
  group_by(PROVIDER) 


ctscan <- ctscan[,c(1,4)]
ctscan <- add_column(ctscan,repline ="430")

exsumc <- bind_rows(exsumc,ctscan)
rm(ctscan,scottemp46)

## MRI Scan - repline - 440

mriscan <- data[,1:24] %>%
  filter(LINENO %in% c(140) & FORMNAME %in% c("SFR 5.11")) %>%
  mutate(cost1 = COL7, cost2 =COL8) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost1 = sum(cost1,na.rm = TRUE), totalcost2 = sum(cost2,na.rm = TRUE))

scottemp47 <- mriscan  %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost1 = sum(totalcost1,na.rm = TRUE),totalcost2 = sum(totalcost2,na.rm = TRUE))

mriscan <- rbind(mriscan,scottemp47)

mriscan<- mriscan %>%
  mutate(totalcost = totalcost1/totalcost2) %>%
  group_by(PROVIDER) 

mriscan <- mriscan[,c(1,4)]
mriscan <- add_column(mriscan,repline ="440")

exsumc <- bind_rows(exsumc,mriscan)
rm(mriscan,scottemp47)

##### Covers up to line 78 or R100 - should be 776 observations in exsumc at this point 

####################################
## Community Expenditure - (£000) ##
####################################

## Total staff costs - repline 490

commstaff <- data[,1:24] %>%
  filter(LINENO %in% c(980) & FORMNAME %in% c("SFR 8.1")) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp48 <- commstaff  %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

commstaff <- rbind(commstaff,scottemp48)
commstaff <- add_column(commstaff,repline ="490")

exsumc <- bind_rows(exsumc,commstaff)
rm(scottemp48)


## as % gross community expenditure - repline 500

grosscommstaff <- left_join(commstaff,community_sector,by = "PROVIDER")

grosscommstaff <- grosscommstaff[,c(1,2,4)]  %>%
  mutate(cost = totalcost.x / totalcost.y * 100) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost=cost)

grosscommstaff <- add_column(grosscommstaff,repline ="500")

exsumc <- bind_rows(exsumc,grosscommstaff)
rm(grosscommstaff,commstaff,community_sector)

## Total staff numbers2 (wtes) - repline 501

wte <- data[,1:24] %>%
  filter(LINENO %in% c(980) & FORMNAME %in% c("SFR 8.1")) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp49 <- wte  %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

wte <- rbind(wte,scottemp49)
wte <- add_column(wte,repline ="501")

exsumc <- bind_rows(exsumc,wte)
rm(wte,scottemp49)

## Medical staff costs - repline 510

medwte <- data[,1:24] %>%
  filter(LINENO %in% c(150) & FORMNAME %in% c("SFR 8.1")) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp50 <- medwte   %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

medwte <- rbind(medwte,scottemp50)
medwte <- add_column(medwte,repline ="510")

exsumc <- bind_rows(exsumc,medwte)
rm(medwte,scottemp50)


## Nursing staff costs - repline 520

nursewte <- data[,1:24] %>%
  filter(LINENO %in% c(200) & FORMNAME %in% c("SFR 8.1")) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp51 <- nursewte   %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

nursewte <- rbind(nursewte,scottemp51)
nursewte <- add_column(nursewte,repline ="520")

exsumc <- bind_rows(exsumc,nursewte)
rm(nursewte,scottemp51)

## AHP staff costs - repline 530

ahpwte <- data[,1:24] %>%
  filter(LINENO %in% c(210, 220, 230, 240, 250) & FORMNAME %in% c("SFR 8.2")) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp52 <- ahpwte   %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

ahpwte <- rbind(ahpwte,scottemp52)
ahpwte <- add_column(ahpwte,repline ="530")

exsumc <- bind_rows(exsumc,ahpwte)
rm(ahpwte,scottemp52)

## Pharmacy services - repline 540

pharmwte <- data[,1:24] %>%
  filter(LINENO %in% c(310, 320, 330, 340) & FORMNAME %in% c("SFR 8.2")) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp53 <- pharmwte   %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

pharmwte <- rbind(pharmwte,scottemp53)
pharmwte <- add_column(pharmwte,repline ="540")

exsumc <- bind_rows(exsumc,pharmwte)
rm(pharmwte,scottemp53)

## Drug expenditure - repline 550

drugwte <- data[,1:24] %>%
  filter(LINENO %in% c(320) & FORMNAME %in% c("SFR 8.2")) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp54 <- drugwte   %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

drugwte <- rbind(drugwte,scottemp54)
drugwte <- add_column(drugwte,repline ="550")

exsumc <- bind_rows(exsumc,drugwte)
rm(drugwte,scottemp54)


## Admin expenditure - repline 560

adminwte <- data[,1:24] %>%
  filter(LINENO %in% c(710) & FORMNAME %in% c("SFR 8.2")) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp55 <- adminwte %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

adminwte <- rbind(adminwte,scottemp55)
adminwte <- add_column(adminwte,repline ="560")

exsumc <- bind_rows(exsumc,adminwte)
rm(adminwte,scottemp55)


## Admin expenditure - repline 561

adminstaff <- data[,1:24] %>%
  filter(LINENO %in% c(710) & FORMNAME %in% c("SFR 8.2")) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp56 <- adminstaff %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

adminstaff <- rbind(adminstaff,scottemp56)
adminstaff <- add_column(adminstaff,repline ="561")

exsumc <- bind_rows(exsumc,adminstaff)
rm(adminstaff,scottemp56)


## Admin supplies - repline 562

adminsupplies <- data[,1:24] %>%
  filter(LINENO %in% c(710) & FORMNAME %in% c("SFR 8.2")) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp57 <- adminsupplies %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

adminsupplies <- rbind(adminsupplies,scottemp57)
adminsupplies <- add_column(adminsupplies,repline ="562")

exsumc <- bind_rows(exsumc,adminsupplies)
rm(adminsupplies,scottemp57)

## Facilities ##
## Laundry- repline 570

laundry <- data[,1:24] %>%
  filter(LINENO %in% c(450) & FORMNAME %in% c("SFR 8.2")) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp58 <- laundry %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

laundry <- rbind(laundry,scottemp58)
laundry <- add_column(laundry,repline ="570")

exsumc <- bind_rows(exsumc,laundry)
rm(laundry,scottemp58)

## Cleaning - repline 575

cleaning <- data[,1:24] %>%
  filter(LINENO %in% c(620) & FORMNAME %in% c("SFR 8.2")) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp59 <- cleaning %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

cleaning <- rbind(cleaning,scottemp59)
cleaning <- add_column(cleaning,repline ="575")

exsumc <- bind_rows(exsumc,cleaning)
rm(cleaning,scottemp59)

## Property maintenance - repline 580

pm <- data[,1:24] %>%
  filter(LINENO %in% c(610) & FORMNAME %in% c("SFR 8.2")) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp60 <- pm %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

pm <- rbind(pm,scottemp60)
pm <- add_column(pm,repline ="580")

exsumc <- bind_rows(exsumc,pm)
rm(pm,scottemp60)

## Energy & Utilities - repline 585

eu <- data[,1:24] %>%
  filter(LINENO %in% c(640) & FORMNAME %in% c("SFR 8.2")) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp61 <- eu %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

eu <- rbind(eu,scottemp61)
eu <- add_column(eu,repline ="585")

exsumc <- bind_rows(exsumc,eu)
rm(eu,scottemp61)

##########################
## Community - services ##
##########################

## District nursing - repline 591 

dn <- data[,1:24] %>%
  filter(LINENO %in% c(110) & FORMNAME %in% c("SFR 8.3")) %>%
  mutate(cost = COL7) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp62 <- dn %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

dn <- rbind(dn,scottemp62)
dn <- add_column(dn,repline ="591")

exsumc <- bind_rows(exsumc,dn)
rm(scottemp62)

## District nursing cost per head of population  - repline 592

dncost <- merge(dn,populations17)

dncost <- dncost[,-3] %>%
  mutate(totalcost = totalcost/POPULATION) %>%
  group_by(PROVIDER)

dncost <- dncost[1:2]
dncost <- add_column(dncost,repline ="592")

exsumc <- bind_rows(exsumc,dncost)
rm(dncost,dn)

## Communtiy Midwifery - repline 593

cm <- data[,1:24] %>%
  filter(LINENO %in% c(130) & FORMNAME %in% c("SFR 8.3")) %>%
  mutate(cost = COL7) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp63 <- cm %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

cm <- rbind(cm,scottemp63)
cm <- add_column(cm,repline ="593")

exsumc <- bind_rows(exsumc,cm)
rm(scottemp63)

## Community midwifery cost per head of population  - repline 594
## CN / female population (15-44)
## To update 

cmcost <- merge(cm,femalepopulations17)

cmcost <- cmcost[,-3] %>%
  mutate(totalcost = totalcost/POPULATION) %>%
  group_by(PROVIDER)

cmcost <- cmcost[1:2]
cmcost <- add_column(cmcost,repline ="594")

exsumc <- bind_rows(exsumc,cmcost)
rm(cm,cmcost)

## Community Denistry 

cd <- data[,1:24] %>%
  filter(LINENO %in% c(180) & FORMNAME %in% c("SFR 8.3")) %>%
  mutate(cost = COL7) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp64 <- cd %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

cd <- rbind(cd,scottemp64)
cd <- add_column(cd,repline ="595")

exsumc <- bind_rows(exsumc,cd)
rm(scottemp64)

## CD per population - repline 596 

cdcost <- merge(cd,populations17)

cdcost <- cdcost[,-3] %>%
  mutate(totalcost = totalcost/POPULATION) %>%
  group_by(PROVIDER)

cdcost <- cdcost[1:2]
cdcost <- add_column(cdcost,repline ="596")

exsumc <- bind_rows(exsumc,cdcost)
rm(cd,cdcost)

## Child Health - repline 597 

ch <- data[,1:24] %>%
  filter(LINENO %in% c(140) & FORMNAME %in% c("SFR 8.3")) %>%
  mutate(cost = COL7) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp65 <- ch %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

ch <- rbind(ch,scottemp65)
ch <- add_column(ch,repline ="597")

exsumc <- bind_rows(exsumc,ch)
rm(scottemp65,ch)

## Home dialysis - repline 598 

hd<- data[,1:24] %>%
  filter(LINENO %in% c(190) & FORMNAME %in% c("SFR 8.3")) %>%
  mutate(cost = COL7) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp66 <- hd %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

hd <- rbind(hd,scottemp66)
hd <- add_column(hd,repline ="598")

exsumc <- bind_rows(exsumc,hd)
rm(scottemp66,hd)

## GP OOH - repline 599 

gpooh<- data[,1:24] %>%
  filter(LINENO %in% c(176) & FORMNAME %in% c("SFR 8.3")) %>%
  mutate(cost = COL7) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp67 <- gpooh %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

gpooh <- rbind(gpooh,scottemp67)
gpooh <- add_column(gpooh,repline ="599")

exsumc <- bind_rows(exsumc,gpooh)
rm(scottemp67,gpooh)

############################
## Family Health Services ##
############################ 


## Primary Medical Services - rep line 600 

pms<- data[,1:24] %>%
  filter(LINENO %in% c(100) & FORMNAME %in% c("SFR 8.4")) %>%
  mutate(cost = COL3) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp68 <- pms %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

pms <- rbind(pms,scottemp68)
pms <- add_column(pms,repline ="600")

exsumc <- bind_rows(exsumc,pms)
rm(scottemp68,pms)

## Practice list - rep line 605 

plist <- data[,1:24] %>%
  filter(LINENO %in% c(110) & FORMNAME %in% c("SFR 8.4")) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp69 <- plist %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

plist <- rbind(plist,scottemp69)
plist <- add_column(plist,repline ="605")

exsumc <- bind_rows(exsumc,plist)
rm(scottemp69,plist)

## GP practices - rep line 610 

gplist <- data[,1:24] %>%
  filter(LINENO %in% c(120) & FORMNAME %in% c("SFR 8.4")) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp70 <- gplist %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

gplist <- rbind(gplist,scottemp70)
gplist <- add_column(gplist,repline ="610")

exsumc <- bind_rows(exsumc,gplist)
rm(scottemp70,gplist)


## GDS practices - rep line 620 

gdslist <- data[,1:24] %>%
  filter(LINENO %in% c(200) & FORMNAME %in% c("SFR 8.4")) %>%
  mutate(cost = COL3) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp71 <- gdslist %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

gdslist <- rbind(gdslist,scottemp71)
gdslist <- add_column(gdslist,repline ="620")

exsumc <- bind_rows(exsumc,gdslist)
rm(scottemp71,gdslist)

## Registrations - rep line 630 

reg <- data[,1:24] %>%
  filter(LINENO %in% c(220, 230, 240) & FORMNAME %in% c("SFR 8.4")) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp72 <- reg %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

reg <- rbind(reg,scottemp72)
reg <- add_column(reg,repline ="630")

exsumc <- bind_rows(exsumc,reg)
rm(scottemp72,reg)


## General Ophthalmic Services - rep line 640 

gos <- data[,1:24] %>%
  filter(LINENO %in% c(250) & FORMNAME %in% c("SFR 8.4")) %>%
  mutate(cost = COL3) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp73 <- gos %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

gos <- rbind(gos,scottemp73)
gos <- add_column(gos,repline ="640")

exsumc <- bind_rows(exsumc,gos)
rm(scottemp73,gos)

## Sight tests - rep line 650 

sight <- data[,1:24] %>%
  filter(LINENO %in% c(270) & FORMNAME %in% c("SFR 8.4")) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp74 <- sight %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

sight <- rbind(sight,scottemp74)
sight <- add_column(sight,repline ="650")

exsumc <- bind_rows(exsumc,sight)
rm(scottemp74,sight)

## Pharmaceutical Services - rep line 660 

pharm <- data[,1:24] %>%
  filter(LINENO %in% c(290) & FORMNAME %in% c("SFR 8.4")) %>%
  mutate(cost = COL3) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp75 <- pharm %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

pharm <- rbind(pharm,scottemp75)
pharm <- add_column(pharm,repline ="660")

exsumc <- bind_rows(exsumc,pharm)
rm(scottemp75,pharm)

## No. of pharmacies - rep line 670 

nopharm <- data[,1:24] %>%
  filter(LINENO %in% c(300) & FORMNAME %in% c("SFR 8.4")) %>%
  mutate(cost = COL2) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp76 <- nopharm  %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

nopharm  <- rbind(nopharm ,scottemp76)
nopharm  <- add_column(nopharm ,repline ="670")

exsumc <- bind_rows(exsumc,nopharm)
rm(nopharm ,scottemp76)

## Drug expenditure - rep line 680 

drugcost <- data[,1:24] %>%
  filter(LINENO %in% c(335) & FORMNAME %in% c("SFR 8.4")) %>%
  mutate(cost = COL3) %>%
  group_by(PROVIDER) %>%
  summarise(totalcost= sum(cost,na.rm = TRUE))

scottemp77 <- drugcost  %>%
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

drugcost  <- rbind(drugcost ,scottemp77)
drugcost  <- add_column(drugcost ,repline ="680")

exsumc <- bind_rows(exsumc,drugcost)
rm(drugcost ,scottemp77)

exsumc$Year <- 2014
exsumc2014 <- exsumc

exsumc <- rbind(exsumc2014,exsumc2015,exsumc2016,exsumc2017)

write.xlsx(exsumc, file="/../conf/costbook_rserv/exsumc.xlsx")

