###########################################################
# Name of script: Dashboard - Costbook Overview.R
# Written by: Nicole Jarvie - Costbook team 
#             within Resources 
#
# Type of script: Setup environment
# Written/run on: R Studio Server 
# Version of R: 3.6.1
#
# Description: Data Prep
###########################################################

### 1 - Load packages ----

library(odbc)
library(tidyverse)
library(haven)
library(stringr)
library(readxl)
library(scales)
library(ggplot2)
library(plotly)
library(tidyverse)
library(reshape2)
library(data.table)
library(DT)
library(formattable)
library(tidyr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)

### 2 - Load data ---

## This is the section for creating what would have been the output from SPSS directly from the database using an ODBC connection.

APEXP_connection <- dbConnect(odbc::odbc(), 
                 dsn = "APEXP", 
                 uid = rstudioapi::askForPassword("Database user"), 
                 pwd = rstudioapi::askForPassword("Database password"))

#This line gathering data from the costbook takes a few minutes, the financial year needs to be updated with the appropriate years

data <- dbGetQuery(APEXP_connection,paste( "SELECT T0.O_UNIFIED_HB AS provider, T0.O_LOCATION AS Location, T1.SF_FINANCIAL_YEAR AS 
    YEARID, T2.CB_FORMNAME AS FORMNAME, T2.CB_LINENO AS LINENO, T2.CB_COL1 AS COL1, T2.CB_COL2 AS COL2, 
    T2.CB_COL3 AS COL3, T2.CB_COL4 AS COL4, T2.CB_COL5 AS COL5, T2.CB_COL6 AS COL6, T2.CB_COL7 AS COL7, 
    T2.CB_COL8 AS COL8, T2.CB_COL9 AS COL9, T2.CB_COL10 AS COL10, T2.CB_COL11 AS COL11, T2.CB_COL12 AS COL12,
    T2.CB_COL13 AS COL13, T2.CB_COL14 AS COL14, T2.CB_COL15 AS COL15, T2.CB_COL16 AS COL16, T2.CB_COL17 AS COL17, 
    T2.CB_COL18 AS COL18, T2.CB_COL19 AS COL19 FROM CBDCS.ORGANISATION T0, CBDCS.SUBMITTED_FORMS T1, CBDCS.COSTBOOK_DATA T2 
    WHERE T1.SF_ID = T2.CB_SF_ID and T0.O_ID = T1.SF_O_ID and T1.SF_ID = T2.CB_SF_ID and T0.O_ID = T1.SF_O_ID"))

## Remove connection
rm(APEXP_connection)

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


## TOTAL OPERATING COSTS ## 

###################################
## HOSPITAL COSTS - rep line 20##
###################################

## Total Gross - Hospital Running Costs

hospital_running_costs= data[,1:8] %>%
  filter(FORMNAME == "SFR 5.2" & LINENO == 640) %>%
  ##separate(unitcode,c("board","location"), sep = 5) %>%
  mutate(cost = COL1 + COL2) %>%
  group_by(PROVIDER,YEARID) %>%
  summarise(totalcost = sum(cost, na.rm=TRUE))

## Health Services purchases from sub-contractors

health_services_subcontractors= data[,1:8] %>%
  filter(FORMNAME == "SFR 24.0" & LINENO %in% c(120,130,140,150,180,190,200,230,240,250,280,290,300,330,340,350,380,390,420,430)) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER,YEARID) %>%
  summarise(totalcost = sum(cost, na.rm=TRUE))

temp1 <- rbind(hospital_running_costs,health_services_subcontractors)

temp1 = temp1 %>%
  group_by(PROVIDER,YEARID) %>%
  summarise(totalcost = sum(totalcost)) 
temp1 = add_column(temp1, repline = "020") 

scottemp1 = temp1 %>%
  ungroup(PROVIDER) %>% 
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER,YEARID) %>%
  summarise(totalcost = sum(totalcost))
scottemp1 = add_column(scottemp1,repline ="020")

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
  group_by(PROVIDER,YEARID) %>%
  summarise(totalcost = sum(cost,na.rm=TRUE))

community_services_subcontractors=  data[,1:8] %>%
  filter(FORMNAME == "SFR 24.0" & LINENO %in% c(310,360,460,471,472)) %>%
  mutate(cost = COL1) %>%
  group_by(PROVIDER,YEARID) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

temp2 <- rbind(gross_community_service,community_services_subcontractors)

temp2 = temp2%>%
  group_by(PROVIDER,YEARID) %>%
  summarise(totalcost = sum(totalcost,na.rm = TRUE))

temp2 = add_column(temp2, repline = "030")

## Scotland figure
scottemp2 = temp2 %>%
  ungroup(PROVIDER) %>% 
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER,YEARID) %>%
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
  group_by(PROVIDER,YEARID) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))

family_health_services <- add_column(family_health_services,repline = "040")

scottemp3 = family_health_services %>%
  ungroup(PROVIDER) %>% 
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER,YEARID) %>%
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
  group_by(PROVIDER,YEARID) %>%
  summarise(totalcost = sum(cost,na.rm = TRUE))
resource_transfer = add_column(resource_transfer,repline ="045")

scottemp4 <- resource_transfer %>%
  ungroup(PROVIDER) %>% 
  mutate(PROVIDER = "AAAAA") %>%
  group_by(PROVIDER,YEARID) %>%
  summarise(totalcost = sum(totalcost, na.rm = TRUE))
scottemp4 = add_column(scottemp4,repline ="045")


exsumc <- rbind(exsumc, resource_transfer, scottemp4)
rm(resource_transfer, scottemp4)

################################
## Total costs - rep line 10 ###
################################

totalcosts = exsumc[,1:3] %>%
  group_by(PROVIDER,YEARID) %>%
  summarise(totalcost = sum(totalcost, na.rm =  TRUE))
totalcosts = add_column(totalcosts,repline ="010") 


exsumc <- rbind(exsumc, totalcosts)
rm(totalcosts)

######################

exsumc$Percentage <- 0 

i<-1
j<-1 

for (i in 1:nrow(exsumc)){
  if (exsumc[i,4] == "010") {
    for (j in 1:nrow(exsumc)){
      if(exsumc[i,1] == exsumc[j,1] & exsumc[i,2] == exsumc[j,2]){
        exsumc[i,5] = 100 
        exsumc[j,5] = round((exsumc[j,3]/exsumc[i,3])*100,2)
      }
    }  
  }
} 


definition <- read_excel("/../conf/costbook_rserv/Reference Files/Definitions.xlsx", sheet = 1)##,stringsAsFactors = FALSE)
definition$Definition[definition$Definition == "Total operating costs1 (£000)"] <- "Total operating costs"

boardcodes <- read_excel("/../conf/costbook_rserv/Reference Files/Board Codes.xlsx", sheet = 1)##,stringsAsFactors = FALSE)
colnames(boardcodes) <- c("Board","Name")

exsumc <- merge(exsumc,definition,by = "repline")
colnames(exsumc) <- c("repline","Board","Year","Cost","Percentage", "Defn")

exsumc <- merge(exsumc,boardcodes)

exsumc$CostType[exsumc$repline %in% c("010","020","030","040","045")] <- "Board Operating Costs"



##CAMHs Data 
Year06 <- read_excel("/../conf/costbook_rserv/Reference Files/CAMHSExpenditureDetailed.xlsx", sheet = 1)
colnames(Year06) <- c("Board","ChildPsychiatryR04LSX","AdolescentPsychiatryR04LSX","CommunityMentalHealthTeamsSFR8","ChildPsychiatrySFR5.9","AdolescentPsychiatrySFR5.9","CAMHTotal","NHSTotal","PercentNHSTotal","MHTotal","PercentCAMHSMH")
Year06 <- Year06[-1, ]  
Year06$Year <- 2006
Year06 <- Year06[1:17,]


Year07 <- read_excel("/../conf/costbook_rserv/Reference Files/CAMHSExpenditureDetailed.xlsx", sheet = 2)
colnames(Year07) <- c("Board","ChildPsychiatryR04LSX","AdolescentPsychiatryR04LSX","CommunityMentalHealthTeamsSFR8","ChildPsychiatrySFR5.9","AdolescentPsychiatrySFR5.9","CAMHTotal","NHSTotal","PercentNHSTotal","MHTotal","PercentCAMHSMH")
Year07 <- Year07[-1, ]  
Year07$Year <- 2007

Year08 <- read_excel("/../conf/costbook_rserv/Reference Files/CAMHSExpenditureDetailed.xlsx", sheet = 3)
colnames(Year08) <- c("Board","ChildPsychiatryR04LSX","AdolescentPsychiatryR04LSX","CommunityMentalHealthTeamsSFR8","ChildPsychiatrySFR5.9","AdolescentPsychiatrySFR5.9","CAMHTotal","NHSTotal","PercentNHSTotal","MHTotal","PercentCAMHSMH")
Year08 <- Year08[-1, ]  
Year08$Year <- 2008

Year09 <- read_excel("/../conf/costbook_rserv/Reference Files/CAMHSExpenditureDetailed.xlsx", sheet = 4)
colnames(Year09) <- c("Board","ChildPsychiatryR04LSX","AdolescentPsychiatryR04LSX","CommunityMentalHealthTeamsSFR8","ChildPsychiatrySFR5.9","AdolescentPsychiatrySFR5.9","CAMHTotal","NHSTotal","PercentNHSTotal","MHTotal","PercentCAMHSMH")
Year09 <- Year09[-1, ]  
Year09$Year <- 2009

Year10 <- read_excel("/../conf/costbook_rserv/Reference Files/CAMHSExpenditureDetailed.xlsx", sheet = 5)
colnames(Year10) <- c("Board","ChildPsychiatryR04LSX","AdolescentPsychiatryR04LSX","CommunityMentalHealthTeamsSFR8","ChildPsychiatrySFR5.9","AdolescentPsychiatrySFR5.9","CAMHTotal","NHSTotal","PercentNHSTotal","MHTotal","PercentCAMHSMH")
Year10 <- Year10[-1, ]  
Year10$Year <- 2010

Year11 <- read_excel("/../conf/costbook_rserv/Reference Files/CAMHSExpenditureDetailed.xlsx", sheet = 6)
colnames(Year11) <- c("Board","ChildPsychiatryR04LSX","AdolescentPsychiatryR04LSX","CommunityMentalHealthTeamsSFR8","ChildPsychiatrySFR5.9","AdolescentPsychiatrySFR5.9","CAMHTotal","NHSTotal","PercentNHSTotal","MHTotal","PercentCAMHSMH")
Year11 <- Year11[-1, ]  
Year11$Year <- 2011

Year12 <- read_excel("/../conf/costbook_rserv/Reference Files/CAMHSExpenditureDetailed.xlsx", sheet = 7)
colnames(Year12) <- c("Board","ChildPsychiatryR04LSX","AdolescentPsychiatryR04LSX","CommunityMentalHealthTeamsSFR8","ChildPsychiatrySFR5.9","AdolescentPsychiatrySFR5.9","CAMHTotal","NHSTotal","PercentNHSTotal","MHTotal","PercentCAMHSMH")
Year12 <- Year12[-1, ]  
Year12$Year <- 2012
Year12 <- Year12[1:17,]

Year13 <- read_excel("/../conf/costbook_rserv/Reference Files/CAMHSExpenditureDetailed.xlsx", sheet = 8)
colnames(Year13) <- c("Board","ChildPsychiatryR04LSX","AdolescentPsychiatryR04LSX","CommunityMentalHealthTeamsSFR8","ChildPsychiatrySFR5.9","AdolescentPsychiatrySFR5.9","CAMHTotal","NHSTotal","PercentNHSTotal","MHTotal","PercentCAMHSMH")
Year13 <- Year13[-1, ]  
Year13$Year <- 2013
Year13 <- Year13[1:17,]

Year14 <- read_excel("/../conf/costbook_rserv/Reference Files/CAMHSExpenditureDetailed.xlsx", sheet = 9)
colnames(Year14) <- c("Board","ChildPsychiatryR04LSX","AdolescentPsychiatryR04LSX","CommunityMentalHealthTeamsSFR8","ChildPsychiatrySFR5.9","AdolescentPsychiatrySFR5.9","CAMHTotal","NHSTotal","PercentNHSTotal","MHTotal","PercentCAMHSMH")
Year14 <- Year14[-1, ]  
Year14$Year <- 2014
Year14 <- Year14[1:17,]

Year15 <- read_excel("/../conf/costbook_rserv/Reference Files/CAMHSExpenditureDetailed.xlsx", sheet = 10)
colnames(Year15) <- c("Board","ChildPsychiatryR04LSX","AdolescentPsychiatryR04LSX","CommunityMentalHealthTeamsSFR8","ChildPsychiatrySFR5.9","AdolescentPsychiatrySFR5.9","CAMHTotal","NHSTotal","PercentNHSTotal","MHTotal","PercentCAMHSMH")
Year15 <- Year15[-1, ]  
Year15$Year <- 2015
Year15 <- Year15[1:17,]

Year16 <- read_excel("/../conf/costbook_rserv/Reference Files/CAMHSExpenditureDetailed.xlsx", sheet = 11)
colnames(Year16) <- c("Board","ChildPsychiatryR04LSX","AdolescentPsychiatryR04LSX","CommunityMentalHealthTeamsSFR8","ChildPsychiatrySFR5.9","AdolescentPsychiatrySFR5.9","CAMHTotal","NHSTotal","PercentNHSTotal","MHTotal","PercentCAMHSMH")
Year16 <- Year16[-1, ]  
Year16$Year <- 2016
Year16 <- Year16[1:17,]

Year17 <- read_excel("/../conf/costbook_rserv/Reference Files/CAMHSExpenditureDetailed.xlsx", sheet = 12)
colnames(Year17) <- c("Board","ChildPsychiatryR04LSX","AdolescentPsychiatryR04LSX","CommunityMentalHealthTeamsSFR8","ChildPsychiatrySFR5.9","AdolescentPsychiatrySFR5.9","CAMHTotal","NHSTotal","PercentNHSTotal","MHTotal","PercentCAMHSMH")
Year17 <- Year17[-1, ]  
Year17$Year <- 2017
Year17 <- Year17[1:17,]

Year18 <- read_excel("/../conf/costbook_rserv/Reference Files/CAMHSExpenditureDetailed.xlsx", sheet = 13)
colnames(Year18) <- c("Board","ChildPsychiatryR04LSX","AdolescentPsychiatryR04LSX","CommunityMentalHealthTeamsSFR8","ChildPsychiatrySFR5.9","AdolescentPsychiatrySFR5.9","CAMHTotal","NHSTotal","PercentNHSTotal","MHTotal","PercentCAMHSMH")
Year18 <- Year18[-1, ]  
Year18$Year <- 2018
Year18 <- Year18[1:17,]

CompleteYear <- rbind(Year06,Year07,Year08,Year09,Year10,Year11,Year12,Year13,Year14,Year15,Year16,Year17,Year18)


CompleteYear$ChildPsychiatryR04LSX <- as.numeric(CompleteYear$ChildPsychiatryR04LSX) * 1000
CompleteYear$AdolescentPsychiatryR04LSX <- as.numeric(CompleteYear$AdolescentPsychiatryR04LSX) * 1000
CompleteYear$CommunityMentalHealthTeamsSFR8 <- as.numeric(CompleteYear$CommunityMentalHealthTeamsSFR8) * 1000
CompleteYear$ChildPsychiatrySFR5.9 <- as.numeric(CompleteYear$ChildPsychiatrySFR5.9) * 1000
CompleteYear$AdolescentPsychiatrySFR5.9 <- as.numeric(CompleteYear$AdolescentPsychiatrySFR5.9) * 1000
CompleteYear$CAMHTotal <- as.numeric(CompleteYear$CAMHTotal) * 1000
CompleteYear$NHSTotal <- as.numeric(CompleteYear$NHSTotal) * 1000
CompleteYear$MHTotal <- as.numeric(CompleteYear$MHTotal) * 1000

CAMHComplete <- CompleteYear[,c(1:6,12)]
CAMHComplete <- CAMHComplete[,c(1,7,2:6)]


CompleteYear$Board<- gsub("*", "", CompleteYear$Board, fixed=TRUE)





##Dashboard Theme

customGreen = "#71CA97"
customRed = "#ff7f7f"

improvement_formatter <- formatter("span", 
                                   style = x ~ style(font.weight = "bold", 
                                                     color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
                                   x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )


