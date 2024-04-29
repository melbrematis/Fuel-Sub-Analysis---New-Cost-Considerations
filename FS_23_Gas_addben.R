library(tidyverse)
library(readxl)
library(compare)
library(rmarkdown)
library (dplyr)

#Calculate Gas Benefits of Res,NR Heat Pumps (done)
#Add in Utility Bill Benefits. Data is Monthly. 
#use elec CO2 to calc Gas GHG Adder - but x 2 (done)
#add asthma ben by HH

#****Data and Vars

#Selections
MeasAppType <- 'NR'
BldgType <- 'SFm'
SZ <- 'med'
ClimateZone <- 12
HeatPumpHVAC_res <- 'SWHC045'  
HPWH_res <- 'SWWH025'
EU_HP_HVAC<-'DEER:HVAC_Eff_HP'
EU_HPWH<-'DEER:Res_ClothesDishWasher'
EUL_Claims_HeatPumpHVAC_res <- 15
EUL_Claims_HPWH_res <- 10
#EUL_New_HeatPumpHVAC_res <- 20
#EUL_New_HPWH_res <- 20
#Constants
PGE_WACC_A <- 0.0734 #Source:ACC, discount Rate
PGE_WACC_Q <- PGE_WACC_A/4
Divider_discount <- PGE_WACC_Q + 1

#Read in Fuel Sub Claims 2023 Q1-Q4
FS23 <- read_excel("C:\\Users\\mmh\\R  Programming\\subtests\\FS_NewBens\\23_FS_Claims_PGE.xlsx") #Normal Replacement Only (No New Construction)
FS23 <- as.data.frame(FS23)
#Read in Load Shapes (accessible at https://file.ac/l1-GqhWF8OU/)
Gas_LS_QRT <- read_excel("C:\\Users\\mmh\\R  Programming\\subtests\\FS_NewBens\\G_LS_QRT.xlsx") #Quarterly LS

#Read in Claim Yr Quarters
Quarters <- read_excel("C:\\Users\\mmh\\R  Programming\\subtests\\FS_NewBens\\Qrt.xlsx")

#Read in Data from Travis
Utility_Bill_Ben_CARE <- read_excel("C:\\Users\\mmh\\R  Programming\\subtests\\FS_NewBens\\data_TH\\res_bill_impacts_PGE_Monthly_CARE.xlsx")
Utility_Bill_Ben_Market <- read_excel("C:\\Users\\mmh\\R  Programming\\subtests\\FS_NewBens\\data_TH\\res_bill_impacts_PGE_Monthly_Market.xlsx")



#Total Energy: by Measure & CZ
df_HeatPumpHVAC_res<- filter(FS23, CZ == ClimateZone, MeasureID == HeatPumpHVAC_res,`Measure Application Type` == MeasAppType, `Building Type` == BldgType )
df_HeatPumpHVAC_res  <- df_HeatPumpHVAC_res %>% mutate(Therm_TotalAnnual=df_HeatPumpHVAC_res$`Unit Therm First Baseline` * df_HeatPumpHVAC_res$`Number of Units`)
HeatPumpHVAC_res_therm<- c(sum(df_HeatPumpHVAC_res$`Therm_TotalAnnual`))

df_HPWH_res<- filter(FS23, CZ == ClimateZone, MeasureID == HPWH_res, `Measure Application Type` == MeasAppType, `Building Type` == BldgType)
df_HPWH_res<-df_HPWH_res%>% mutate(Therm_TotalAnnual=df_HPWH_res$`Unit Therm First Baseline` * df_HPWH_res$`Number of Units`)
HPWH_res_therm<- c(sum(df_HPWH_res$`Therm_TotalAnnual`))

#Add Bill Savings Bens
#filter, Size, CZ
#x3 to get quarterly
Num_Measures <- FS23 %>% count(MeasureID)
Num_HPWH <- filter(Num_Measures, MeasureID == HPWH_res ) 
Num_HVAC <- filter(Num_Measures, MeasureID == HeatPumpHVAC_res ) 

Utility_Bill_M <- filter(Utility_Bill_Ben_Market, `CZ` == ClimateZone, `Building Type` == BldgType, `Size` == SZ)

Utility_Bill_HVAC_M <- (Utility_Bill_M$HP_HVAC*12*EUL_Claims_HeatPumpHVAC_res)/(1+PGE_WACC_A)^EUL_Claims_HeatPumpHVAC_res
Utility_Bill_HVAC_M_Total <- (Num_HVAC$n*Utility_Bill_HVAC_M)

Utility_Bill_HPWH_M <- (Utility_Bill_M$HPWH*12*EUL_Claims_HPWH_res)/(1+PGE_WACC_A)^EUL_Claims_HPWH_res
Utility_Bill_HPWH_M_Total <- (Num_HPWH$n*Utility_Bill_HPWH_M)


Utility_Bill_C <- filter(Utility_Bill_Ben_CARE, `CZ` == ClimateZone, `Building Type` == BldgType, `Size` == SZ)

Utility_Bill_HVAC_C <- (Utility_Bill_C$HP_HVAC*12*EUL_Claims_HeatPumpHVAC_res)/(1+PGE_WACC_A)^EUL_Claims_HeatPumpHVAC_res
Utility_Bill_HVAC_C_Total <- (Num_HVAC$n*Utility_Bill_HVAC_C)

Utility_Bill_HPWH_C <- (Utility_Bill_C$HPWH*12*EUL_Claims_HPWH_res)/(1+PGE_WACC_A)^EUL_Claims_HPWH_res
Utility_Bill_HPWH_C_Total <- (Num_HPWH$n*Utility_Bill_HPWH_C)




Claims_GasBen_HeatPumpHVAC_res<- c(sum(df_HeatPumpHVAC_res$`Gas Benefits`))
Claims_GasBen_HPWH_res<- c(sum(df_HPWH_res$`Gas Benefits`))

#Calculated Quarterly Electric ACs (energy x LS) over EUL_Claims
#Filter for CZ, End Use, CLaimYearQrt
EUL_Claims_HeatPumpHVAC_res <-  list(Quarters$EUL_15) 
EUL_Claims_HeatPumpHVAC_res<- as.data.frame(EUL_Claims_HeatPumpHVAC_res)
colnames(EUL_Claims_HeatPumpHVAC_res) <- c('CYQtr')
LS_HeatPumpHVAC_res <- filter(Gas_LS_QRT, Qtr %in% EUL_Claims_HeatPumpHVAC_res$CYQt) 
LS_HeatPumpHVAC_res <-  LS_HeatPumpHVAC_res %>% mutate(Qtr_step = 1:n())  
LS_HeatPumpHVAC_res <- LS_HeatPumpHVAC_res %>% mutate(Qtr_ID = LS_HeatPumpHVAC_res$'Qtr_step'-1) #this is the exponent
EUL_Claims_HPWH_res <-  list(Quarters$EUL_10) 
EUL_Claims_HPWH_res<- as.data.frame(EUL_Claims_HPWH_res)
colnames(EUL_Claims_HPWH_res) <- c('CYQtr')
LS_HPWH_res <- filter(Gas_LS_QRT, Qtr %in% EUL_Claims_HPWH_res$CYQt) 
LS_HPWH_res <-  LS_HPWH_res %>% mutate(Qtr_step = 1:n())  
LS_HPWH_res <- LS_HPWH_res %>% mutate(Qtr_ID = LS_HPWH_res$'Qtr_step'-1) #this is the exponent

Therm_TotalAnnual_HVAC <- c(sum(df_HeatPumpHVAC_res$`Therm_TotalAnnual`))
Therm_TotalAnnual_HPWH <- c(sum(df_HPWH_res$`Therm_TotalAnnual`))

#Calculate Gas Bens, Discount the Qrt Costs, Multiply & Sum 

LS_HeatPumpHVAC_res <- LS_HeatPumpHVAC_res %>% mutate(Total_Discounted = (LS_HeatPumpHVAC_res$'Total'/(Divider_discount^LS_HeatPumpHVAC_res$'Qtr_ID')))


LS_HPWH_res <- LS_HPWH_res %>% mutate(Total_Discounted = (LS_HPWH_res$'Total'/(Divider_discount^LS_HPWH_res$'Qtr_ID')))


#Gas Therms to kWh, then x by CO2 elec x 2
Gas_kwhe_HPWH <- Therm_TotalAnnual_HPWH*29.3
Gas_kwhe_HVAC <- Therm_TotalAnnual_HVAC*29.3

#Calculate Electric Costs, Discount the Qrt Costs, Multiply & Sum 
kWh_LS_QRT <- read_excel("C:\\Users\\mmh\\R  Programming\\subtests\\FS_NewBens\\E_LS_QRT_22.xlsx") #Quarterly LS

generate_qtrly_npv <- function(load_shape, climate_zone, end_use, 
                               considered_qtrs, discount_rate, gas_kwhe) {
  filtered_load_shape <- filter(load_shape, CZ == climate_zone, EU == end_use, 
                                Qtr %in% considered_qtrs$CYQt) %>% 
    mutate(Qtr_step = 1:n()) %>% 
    mutate(Qtr_ID = Qtr_step-1) %>% 
    mutate(CO2_Discounted = (CO2/(discount_rate^Qtr_ID)))
  
  result <- 2*c(sum(filtered_load_shape$'CO2_Discounted'*gas_kwhe))
  return (result)
}

GasCO2_HeatPumpHVAC_res <- 
  generate_qtrly_npv(kWh_LS_QRT, ClimateZone, EU_HP_HVAC, 
                     EUL_Claims_HeatPumpHVAC_res, Divider_discount,
                     Gas_kwhe_HVAC)

GasCO2_HPWH_res <- 
  generate_qtrly_npv(kWh_LS_QRT, ClimateZone, EU_HPWH, EUL_Claims_HPWH_res,
                     Divider_discount, Gas_kwhe_HPWH)

Calc_GasBen_HeatPumpHVAC_res <-  GasCO2_HeatPumpHVAC_res+ c(sum(LS_HeatPumpHVAC_res$'Total_Discounted'*Therm_TotalAnnual_HVAC))
Calc_GasBen_HPWH_res <- GasCO2_HPWH_res + c(sum(LS_HPWH_res$'Total_Discounted'*Therm_TotalAnnual_HPWH))

print("HVAC_HP")
print (Calc_GasBen_HeatPumpHVAC_res/Claims_GasBen_HeatPumpHVAC_res) 
print("HPWH")
print ( Calc_GasBen_HPWH_res/Claims_GasBen_HPWH_res)
