library(tidyverse)
library(readxl)
library(compare)
library(rmarkdown)
library (dplyr)

#Calculate Electric Costs of Res,NR Heat Pumps (done)
#Add in Infra Costs. It is a 1 time cost applied to install Qtr. of each Claim. No Discounting needed.
#exclude Incentive increases. Incentives cancel out in the TRC (done)


#****Data and Vars

#Selections
MeasAppType <- 'NR'
BldgType <- 'SFm'
Sctr <- 'res'
ClimateZone <- 12
HeatPumpHVAC_res <- 'SWHC045'  
HPWH_res <- 'SWWH025'
EU_HP_HVAC<-'DEER:HVAC_Eff_HP'
EU_HPWH<-'DEER:Res_ClothesDishWasher'
EUL_Claims_HeatPumpHVAC_res <- 15
EUL_Claims_HPWH_res <- 10
#EUL_New_HeatPumpHVAC_res <- 23
#EUL_New_HPWH_res <- 20
#Constants
PGE_WACC_A <- 0.0734 #Source:ACC, discount Rate
PGE_WACC_Q <- PGE_WACC_A/4
Divider_discount <- PGE_WACC_Q + 1

#Read in Fuel Sub Claims 2023 Q1-Q4
FS23 <- read_excel("C:\\Users\\mmh\\R  Programming\\subtests\\FS_NewBens\\23_FS_Claims_PGE.xlsx") #Normal Replacement Only (No New Construction)
FS23 <- as.data.frame(FS23)
#Read in Load Shapes (accessible at https://file.ac/l1-GqhWF8OU/)
kWh_LS_QRT <- read_excel("C:\\Users\\mmh\\R  Programming\\subtests\\FS_NewBens\\E_LS_QRT_22.xlsx") #Quarterly LS

#Read in Claim Yr Quarters
Quarters <- read_excel("C:\\Users\\mmh\\R  Programming\\subtests\\FS_NewBens\\Qrt.xlsx")

#Read in Data from Travis
Utility_Bill_Ben_CARE <- read_excel("C:\\Users\\mmh\\R  Programming\\subtests\\FS_NewBens\\data_TH\\res_bill_impacts_PGE_Monthly_CARE.xlsx")
Utility_Bill_Ben_Market <- read_excel("C:\\Users\\mmh\\R  Programming\\subtests\\FS_NewBens\\data_TH\\res_bill_impacts_PGE_Monthly_Market.xlsx")
Infra_Costs <- read_excel("C:\\Users\\mmh\\R  Programming\\subtests\\FS_NewBens\\data_TH\\FS_InfraCosts.xlsx") #input as full cost to install Qtr. No Discounting Needed


#Total Energy: by Measure & CZ
df_HeatPumpHVAC_res<- filter(FS23, CZ == ClimateZone, MeasureID == HeatPumpHVAC_res, `Measure Application Type` == MeasAppType, `Building Type` == BldgType) 
df_HeatPumpHVAC_res  <- df_HeatPumpHVAC_res %>% mutate(kWh_TotalAnnual=df_HeatPumpHVAC_res$`Unit kWh First Baseline` * df_HeatPumpHVAC_res$`Number of Units`)
HeatPumpHVAC_res_kWh<- c(sum(df_HeatPumpHVAC_res$`kWh_TotalAnnual`))

df_HPWH_res<- filter(FS23, CZ == ClimateZone, MeasureID == HPWH_res,`Measure Application Type` == MeasAppType, `Building Type` == BldgType)
df_HPWH_res<-df_HPWH_res %>% mutate(kWh_TotalAnnual=df_HPWH_res$`Unit kWh First Baseline` * df_HPWH_res$`Number of Units`)
HPWH_res_kWh<- c(sum(df_HPWH_res$`kWh_TotalAnnual`))

#Claimed Electric Costs and Gas Benefits
#df_HeatPumpHVAC_res <- lapply(df_HeatPumpHVAC_res,as.numeric)
#df_HPWH_res <- lapply(df_HPWH_res,as.numeric)


Claims_ElecCost_HeatPumpHVAC_res<- c(sum(df_HeatPumpHVAC_res$`Electric Supply Cost Gross`))
Claims_ElecCost_HPWH_res<- c(sum(df_HPWH_res$`Electric Supply Cost Gross`))

#Calculated Quarterly Electric ACs (energy x LS) over EUL_Claims
#Filter for CZ, End Use, CLaimYearQrt
EUL_Claims_HeatPumpHVAC_res <-  list(Quarters$EUL_15) 
EUL_Claims_HeatPumpHVAC_res<- as.data.frame(EUL_Claims_HeatPumpHVAC_res)
colnames(EUL_Claims_HeatPumpHVAC_res) <- c('CYQtr')
LS_HeatPumpHVAC_res <- filter(kWh_LS_QRT, CZ == ClimateZone, EU == EU_HP_HVAC, Qtr %in% EUL_Claims_HeatPumpHVAC_res$CYQt) 
LS_HeatPumpHVAC_res <-  LS_HeatPumpHVAC_res %>% mutate(Qtr_step = 1:n())  
LS_HeatPumpHVAC_res <- LS_HeatPumpHVAC_res %>% mutate(Qtr_ID = LS_HeatPumpHVAC_res$'Qtr_step'-1) #this is the exponent
EUL_Claims_HPWH_res <-  list(Quarters$EUL_10) 
EUL_Claims_HPWH_res<- as.data.frame(EUL_Claims_HPWH_res)
colnames(EUL_Claims_HPWH_res) <- c('CYQtr')
LS_HPWH_res <- filter(kWh_LS_QRT, CZ == ClimateZone, EU == EU_HPWH, Qtr %in% EUL_Claims_HPWH_res$CYQt) 
LS_HPWH_res <-  LS_HPWH_res %>% mutate(Qtr_step = 1:n())  
LS_HPWH_res <- LS_HPWH_res %>% mutate(Qtr_ID = LS_HPWH_res$'Qtr_step'-1)

kWh_TotalAnnual_HVAC <- c(sum(df_HeatPumpHVAC_res$`kWh_TotalAnnual`))
kWh_TotalAnnual_HPWH <- c(sum(df_HPWH_res$`kWh_TotalAnnual`))

#Calculate Electric Costs, Discount the Qrt Costs, Multiply & Sum 

LS_HeatPumpHVAC_res <- LS_HeatPumpHVAC_res %>% mutate(Gen_Discounted = (LS_HeatPumpHVAC_res$'Gen'/(Divider_discount^LS_HeatPumpHVAC_res$'Qtr_ID')))
LS_HeatPumpHVAC_res <- LS_HeatPumpHVAC_res %>% mutate(TD_Discounted = (LS_HeatPumpHVAC_res$'TD'/(Divider_discount^LS_HeatPumpHVAC_res$'Qtr_ID')))
LS_HeatPumpHVAC_res <- LS_HeatPumpHVAC_res %>% mutate(CO2_Discounted = (LS_HeatPumpHVAC_res$'CO2'/(Divider_discount^LS_HeatPumpHVAC_res$'Qtr_ID')))
AC_Sum_Gen_E_HeatPumpHVAC_res <- c(sum(LS_HeatPumpHVAC_res$'Gen_Discounted'*kWh_TotalAnnual_HVAC))
AC_Sum_TD_E_HeatPumpHVAC_res <- c(sum(LS_HeatPumpHVAC_res$'TD_Discounted'*kWh_TotalAnnual_HVAC))
AC_SUM_CO2_E_HeatPumpHVAC_res <- c(sum(LS_HeatPumpHVAC_res$'CO2_Discounted'*kWh_TotalAnnual_HVAC))
Calc_ElecCost_HeatPumpHVAC_res <- (AC_Sum_Gen_E_HeatPumpHVAC_res + AC_Sum_TD_E_HeatPumpHVAC_res + AC_SUM_CO2_E_HeatPumpHVAC_res)*(-1)

LS_HPWH_res <- LS_HPWH_res %>% mutate(Gen_Discounted = (LS_HPWH_res$'Gen'/(Divider_discount^LS_HPWH_res$'Qtr_ID')))
LS_HPWH_res <- LS_HPWH_res %>% mutate(TD_Discounted = (LS_HPWH_res$'TD'/(Divider_discount^LS_HPWH_res$'Qtr_ID')))
LS_HPWH_res <- LS_HPWH_res %>% mutate(CO2_Discounted = (LS_HPWH_res$'CO2'/(Divider_discount^LS_HPWH_res$'Qtr_ID')))
AC_Sum_Gen_E_HPWH_res <- c(sum(LS_HPWH_res$'Gen_Discounted'*kWh_TotalAnnual_HPWH))
AC_Sum_TD_E_HPWH_res <- c(sum(LS_HPWH_res$'TD_Discounted'*kWh_TotalAnnual_HPWH))
AC_SUM_CO2_E_HPWH_res <- c(sum(LS_HPWH_res$'CO2_Discounted'*kWh_TotalAnnual_HPWH))
Calc_ElecCost_HPWH_res <- (AC_Sum_Gen_E_HPWH_res + AC_Sum_TD_E_HPWH_res + AC_SUM_CO2_E_HPWH_res)*(-1)

#Add Infra Costs
Num_Measures <- FS23 %>% count(MeasureID)
Num_HPWH <- filter(Num_Measures, MeasureID == HPWH_res ) 
Num_HVAC <- filter(Num_Measures, MeasureID == HeatPumpHVAC_res ) 


InfraCosts <- filter(Infra_Costs, Sector == 'res')
HP_HVAC_InfraCosts <- InfraCosts$HP_HVAC*Num_HVAC$n
HPWH_InfraCosts <- InfraCosts$HPWH*Num_HPWH$n


print("HVAC_HP")
print (Calc_ElecCost_HeatPumpHVAC_res/Claims_ElecCost_HeatPumpHVAC_res) 
print("HPWH")
print (Calc_ElecCost_HPWH_res/Claims_ElecCost_HPWH_res)
