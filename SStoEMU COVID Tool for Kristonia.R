# Kristonia National Covid Analysis SStoEMU
# This code produces Covid Analysis for a fake nation's data

library(tidyverse)
library(viridis)
setwd("C:/Users/KristinBietsch/files/Track20/SS to EMU/Creating Fake Subnational SStoEMU")

options(scipen = 999)
##################################################################3
# Read in data
inputs <- read.csv("Kristonia Annual National Service Statistics Input Data.csv") 
pop <- read.csv("Kristonia National Population Data.csv") %>% rename(year=Year)
FPSource <- read.csv("Kristonia National FPSource.csv") 

fpet <- read.csv("FPET Results for National Kristonia.csv")
method_mix <- read.csv("Kristonia National Method Mix Data.csv")
dhs_mcpr <- read.csv("Fake DHS Results for National Kristonia.csv")
pop_long <- read.csv("Population 2000 2030 for National Kristonia.csv")

Sectors_reporting <- read.csv("Kristonia Sectors Reporting.csv") 
prior_dist <- read.csv("Kristonia Prior Distribution.csv") 

cyp <- read.csv("CYP_Data 2022.csv")
continuation <- read.csv("Continuation_data 2022.csv")

graph_names <- read.csv("GraphNames.csv")
graph_num <- read.csv("GraphNum.csv")
##############################################################

fpet_growth <- fpet %>%  filter(year>=2019 & year<=2021) %>% mutate(growth=mCPR_MW-lag(mCPR_MW)) %>% rename(Year=year) %>% select(Year, growth)

pop_growth <- pop_long %>%  filter(year>=2019 & year<=2021) %>% mutate(r= log(Pop/lag(Pop)))  %>% rename(Year=year) %>% select(Year, r)
##############################################################

# CYP for sterilization is 10 in this example
continuation_long <- continuation %>% gather(Year, Value, Yr0:yr10) %>% 
  mutate(Year=as.numeric(substr(Year, 3, 4))) %>% 
  arrange(Method_Det, Year) %>%
  group_by(Method_Det) %>%
  mutate(Continuation=case_when( Year==10  ~ 0,
                                 Year==9 & Method=="Sterilization" ~ 1,
                                 Year<10 ~ (Value+lead(Value))/2))

# Creating the percent of acceptors who will still be using each year
continuation_wide <- continuation_long %>% mutate(Year=paste("Year", Year, sep="_")) %>% select(-Value) %>% spread(Year, Continuation) %>% select(Method_Det, Year_0, Year_1 , Year_2, Year_3, Year_4, Year_5, Year_6, Year_7, Year_8, Year_9, Year_10)

##############################################################

# Editing names, transforming from proportion to percentage, transforming to long format
FPSource_clean <- FPSource %>% rename( Private_Clinic=PrivateClinic, Shop_Church_Friend=ShopChurchFriend) %>%
  mutate(Public=Public*100,
         Private_Clinic=Private_Clinic*100,
         Pharmacy=Pharmacy*100,
         Shop_Church_Friend=Shop_Church_Friend*100,
         NGO=NGO*100,
         Other=Other*100)  %>% 
  gather(Source, Value, NGO:Shop_Church_Friend) %>%
  mutate(Value=case_when(is.na(Value) ~ 0, ! is.na(Value) ~ Value))


############
# Editing names, transforming to long format
Sectors_reporting_clean <- Sectors_reporting  %>% rename(     "NGO"  = "Private_NGO"  ,   "Private_Clinic"= "Private_HospClin"   , "Pharmacy" ="Private_Pharm"   ) %>%
  gather(Source, Reporting, Public:Other) %>%
  mutate(Reporting=case_when(is.na(Reporting) ~ 0, ! is.na(Reporting) ~ Reporting)) 

##################
# want to make sure we have adjustment factors for all methods even if not in DHS
Method <- c("Female Sterilization",  "Male Sterilization", "IUD", "Implant",  "Injectable",   "OC Pills", "Male Condom",   "Female Condom",  "Other Modern Methods",  "EC"  )
Method_Num <- c(1,2,3,4,5,6,7,8,9,10)
method.df <- data.frame(Method, Method_Num)

#########################################################
# selecting just the needed data
prior_dist_clean <- prior_dist %>% select(Method_Det, Prior_Dist)

##################
# Creating the adjustment factors
country_sector_rep <- full_join(FPSource_clean, Sectors_reporting_clean, by="Source") %>%
  mutate(Method=case_when(Method=="Other Modern Method" ~ "Other Modern Methods",
                          Method!="Other Modern Method" ~ Method)) %>% 
  mutate(Reported=Value*Reporting) %>% 
  group_by( Method, Type) %>% 
  summarise(Reported=sum(Reported)) %>%
  mutate(Reported=case_when(Reported==0 ~ 100,
                            Reported!=0 ~ Reported),
         Adjustment_Factor=100/Reported) %>% 
  ungroup() %>%
  select(-Reported, -Type) %>%
  full_join(method.df, by="Method") %>% 
  mutate(Adjustment_Factor= case_when(is.na(Adjustment_Factor)~ 1, !is.na(Adjustment_Factor)~ Adjustment_Factor)) %>%
  filter(!is.na(Method_Num))

###########################################################################
#If you want to have unadjusted male condom data run this code:
country_sector_rep <- country_sector_rep %>% mutate(Adjustment_Factor=case_when(Method=="Male Condom" ~ 1, Method!="Male Condom" ~ Adjustment_Factor))

###########################################################################

# We are now ready to construct the SStoEMU

#####################################################################################
# add in a blank 2021
data <- inputs  %>% rename(Method_Det=Method) %>% mutate(Year= paste("X", Year, sep="")) %>% spread(Year, Value) %>%
  filter(!is.na(X2020)) %>% 
  mutate(X2021=NA) %>% # Creating an empty spot for 2021 data
  gather(Year, Value, X2015:X2021) %>% mutate(Year =as.numeric(as.character(str_sub(Year, 2, -1))), Value=as.numeric(as.character(Value))) %>% 
  mutate(Value=case_when(is.na(Value) & Year!=2021 ~ 0, !is.na(Value) ~ Value)) 


########################################################################################

# Creating a list of the years we have data for, and identifying the first and last year of data
Year <- seq(min(data$Year), max(data$Year), 1)
year_list.df <- data.frame(Year)
year_min <- as.numeric(min(data$Year))
year_max <- as.numeric(max(data$Year))

# We need to pull out the long acting methods to estimate the number of historical users before HMIS data was recorded
lam_first <- data %>%   full_join( cyp, by="Method_Det") %>% 
  filter(Year== as.numeric(min(data$Year))) %>% filter(Method_Num<=4) %>% 
  select(Method_Det,  Value) %>% rename(LAPM_First=Value)

# Creating a dataset of the first year of long acting methods and the cummulative sum multiplier from continuation_long
continuation_lapm <- continuation_long %>% 
  mutate(Year=Year+as.numeric(min(data$Year))) %>% 
  select(Method_Det, Year, Continuation) %>% 
  spread(Method_Det, Continuation) %>% 
  full_join(year_list.df, by="Year") %>% 
  gather(Method_Det, Continuation, "Copper- T 380-A IUD":"Vasectomy (M)" ) %>% 
  mutate(Continuation=case_when(is.na(Continuation) ~ 0, !is.na(Continuation) ~ Continuation)) %>%
  arrange(Method_Det , -Year) %>%
  group_by(Method_Det) %>% 
  mutate(Cont_Sum1=cumsum(Continuation),
         Cont_Sum=lag(Cont_Sum1)) %>%
  mutate(Cont_Sum=case_when(is.na(Cont_Sum) ~ 0, !is.na(Cont_Sum) ~ Cont_Sum)) %>%
  select(Year, Method_Det, Cont_Sum) %>% 
  filter(Year<=year_max) %>%
  full_join(lam_first,  by=c("Method_Det"))

# This code creates the EMUs
clients <-  data %>% 
  full_join( cyp, by="Method_Det") %>% # Bringing in the CYPs for each method
  select(-VisitCYP, -Method) %>%
  full_join( country_sector_rep, by=c( "Method_Num")) %>%
  full_join(continuation_lapm, by=c("Method_Det", "Year")) %>%
  full_join( continuation_wide, by=c("Method_Det")) %>% 
  full_join( prior_dist_clean, by=c("Method_Det")) %>%
  filter(!is.na(Method_Num)) %>%
  mutate(historical= Cont_Sum * LAPM_First* Prior_Dist,
         Value_0=Value*Year_0,
         Value_1=Value*Year_1,
         Value_2=Value*Year_2,
         Value_3=Value*Year_3,
         Value_4=Value*Year_4,
         Value_5=Value*Year_5,
         Value_6=Value*Year_6,
         Value_7=Value*Year_7,
         Value_8=Value*Year_8,
         Value_9=Value*Year_9,
         Value_10=Value*Year_10) %>%
  arrange( Method_Det, Year) %>%
  group_by(Method_Det) %>% 
  mutate( lag_1 = case_when(Year== year_min ~ 0, Year>year_min~ lag(Value_1)) ,
          lag_2=case_when(Year<= year_min+1 ~ 0, Year>year_min+1~ lag(Value_2, 2)) ,
          lag_3=case_when(Year<= year_min+2 ~ 0, Year>year_min+2~ lag(Value_3, 3)) ,
          lag_4=case_when(Year<= year_min+3 ~ 0, Year>year_min+3~ lag(Value_4, 4)) ,
          lag_5=case_when(Year<= year_min+4 ~ 0, Year>year_min+4~ lag(Value_5, 5)) ,
          lag_6=case_when(Year<= year_min+5 ~ 0, Year>year_min+5~ lag(Value_6, 6)) ,
          lag_7=case_when(Year<= year_min+6 ~ 0, Year>year_min+6~ lag(Value_7, 7)) ,
          lag_8=case_when(Year<= year_min+7 ~ 0, Year>year_min+7~ lag(Value_8, 8)) ,
          lag_9=case_when(Year<= year_min+8 ~ 0, Year>year_min+8~ lag(Value_9, 9)) ,
          lag_10=case_when(Year<= year_min+9 ~ 0, Year>year_min+9~ lag(Value_10, 10)) ) %>%
  mutate(lag_data =  lag_1  + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + lag_7 + lag_8 + lag_9  + lag_10, 
         unadj_users= historical + Value_0 + lag_data) %>% 
  mutate(users=case_when(Method_Num>=5 ~ Value*CYP*Adjustment_Factor,
                         Method_Num<=4 ~ unadj_users*Adjustment_Factor),
         PreHMIS_users=case_when(Method_Num>=5 ~ 0,
                                 Method_Num<=4 ~ historical*Adjustment_Factor),
         ThisYear_users=case_when(Method_Num>=5 ~ Value*CYP*Adjustment_Factor,
                                  Method_Num<=4 ~ Value_0*Adjustment_Factor),
         HMIS_His_users=case_when(Method_Num>=5 ~ 0,
                                  Method_Num<=4 ~ lag_data*Adjustment_Factor)) %>%
  select(Method_Det, Year, Method, Method_Num, Adjustment_Factor, CYP, Year_0, users, unadj_users, Value, historical, Value_0, lag_data, PreHMIS_users, ThisYear_users, HMIS_His_users) %>% filter(!is.na(Year))


clients.small <- clients %>% select(Method_Det, Year, Method_Num, PreHMIS_users, ThisYear_users, HMIS_His_users) %>% 
  rename(PreHMIS_Actual=PreHMIS_users, ThisYear_Actual=ThisYear_users, HMISHis_Actual=HMIS_His_users)



##################################################
# Fpet and pop growth

data_expectedfpet <- clients   %>% filter(Year>=2019) %>% full_join(pop_growth, by="Year")  %>% full_join(fpet_growth, by="Year") %>%
  group_by(Method_Det) %>%
  mutate(users_exp_fpet =case_when(Year==2020 ~ lag(users)* exp(r) * (1+growth),
                                   Year==2021 ~ lag(users, 2)* exp(lag(r)) * (1+lag(growth)) * exp(r) * (1+growth))) %>%
  mutate(PreHMIS_users_exp_fpet= PreHMIS_users,
         HMIS_His_users_exp_fpet=case_when(Method_Num>=5 ~ HMIS_His_users,
                                           Year==2020 ~ HMIS_His_users),
         ThisYear_users_exp_fpet=users_exp_fpet - PreHMIS_users_exp_fpet - HMIS_His_users_exp_fpet) %>%
  mutate(Value_0_exp_fpet = case_when(Method_Num<5 ~ (ThisYear_users_exp_fpet / Adjustment_Factor)),
         value_exp_fpet = (case_when(Method_Num<5 ~ Value_0_exp_fpet/Year_0,
                                     Method_Num >=5 ~ ThisYear_users_exp_fpet / (CYP*Adjustment_Factor) )))

data_expectedfpet2020 <-data_expectedfpet  %>%
  filter(Year==2020) %>% select(Method_Det, Year, value_exp_fpet) %>% rename(Value=value_exp_fpet)

users_expectedfpet2021  <-data_expectedfpet  %>%
  filter(Year==2021)  %>% select(Method_Det, Year, users_exp_fpet)


clients_fpet <- data %>% filter(Year!=2020) %>% bind_rows(data_expectedfpet2020) %>% arrange(Method_Det, Year)  %>% 
  full_join( cyp, by="Method_Det") %>% select(-VisitCYP, -Method) %>%
  full_join( country_sector_rep, by=c( "Method_Num")) %>%
  full_join(continuation_lapm, by=c("Method_Det", "Year")) %>%
  full_join( continuation_wide, by=c("Method_Det")) %>% 
  full_join( prior_dist_clean, by=c("Method_Det")) %>%
  filter(!is.na(Method_Num)) %>%
  mutate(historical= Cont_Sum * LAPM_First* Prior_Dist,
         Value_0=Value*Year_0,
         Value_1=Value*Year_1,
         Value_2=Value*Year_2,
         Value_3=Value*Year_3,
         Value_4=Value*Year_4,
         Value_5=Value*Year_5,
         Value_6=Value*Year_6,
         Value_7=Value*Year_7,
         Value_8=Value*Year_8,
         Value_9=Value*Year_9,
         Value_10=Value*Year_10) %>%
  arrange( Method_Det, Year) %>%
  group_by(Method_Det) %>% 
  mutate( lag_1 = case_when(Year== year_min ~ 0, Year>year_min~ lag(Value_1)) ,
          lag_2=case_when(Year<= year_min+1 ~ 0, Year>year_min+1~ lag(Value_2, 2)) ,
          lag_3=case_when(Year<= year_min+2 ~ 0, Year>year_min+2~ lag(Value_3, 3)) ,
          lag_4=case_when(Year<= year_min+3 ~ 0, Year>year_min+3~ lag(Value_4, 4)) ,
          lag_5=case_when(Year<= year_min+4 ~ 0, Year>year_min+4~ lag(Value_5, 5)) ,
          lag_6=case_when(Year<= year_min+5 ~ 0, Year>year_min+5~ lag(Value_6, 6)) ,
          lag_7=case_when(Year<= year_min+6 ~ 0, Year>year_min+6~ lag(Value_7, 7)) ,
          lag_8=case_when(Year<= year_min+7 ~ 0, Year>year_min+7~ lag(Value_8, 8)) ,
          lag_9=case_when(Year<= year_min+8 ~ 0, Year>year_min+8~ lag(Value_9, 9)) ,
          lag_10=case_when(Year<= year_min+9 ~ 0, Year>year_min+9~ lag(Value_10, 10)) ) %>%
  mutate(lag_data =  lag_1  + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + lag_7 + lag_8 + lag_9  + lag_10, 
         unadj_users= historical + Value_0 + lag_data) %>% 
  mutate(users=case_when(Method_Num>=5 ~ Value*CYP*Adjustment_Factor,
                         Method_Num<=4 ~ unadj_users*Adjustment_Factor),
         PreHMIS_users=case_when(Method_Num>=5 ~ 0,
                                 Method_Num<=4 ~ historical*Adjustment_Factor),
         ThisYear_users=case_when(Method_Num>=5 ~ Value*CYP*Adjustment_Factor,
                                  Method_Num<=4 ~ Value_0*Adjustment_Factor),
         HMIS_His_users=case_when(Method_Num>=5 ~ 0,
                                  Method_Num<=4 ~ lag_data*Adjustment_Factor)) %>%
  select(Method_Det, Year, Method, Method_Num, Adjustment_Factor, CYP, Year_0, users, unadj_users, Value, historical, Value_0, lag_data, PreHMIS_users, ThisYear_users, HMIS_His_users) %>%
  full_join(users_expectedfpet2021,  by=c("Method_Det", "Year")) %>%
  mutate(ThisYear_users=case_when(Year==2021 ~ users_exp_fpet - PreHMIS_users - HMIS_His_users,
                                  Year!=2021 ~ ThisYear_users)) %>%
  rename(PreHMIS_users_fpet=PreHMIS_users, ThisYear_users_fpet=ThisYear_users, HMIS_His_users_fpet=HMIS_His_users) %>%
  select(Method_Det, Year, PreHMIS_users_fpet, ThisYear_users_fpet, HMIS_His_users_fpet) %>% filter(!is.na(Year)) %>% 
  rename(PreHMIS_FPET=PreHMIS_users_fpet, ThisYear_FPET=ThisYear_users_fpet, HMISHis_FPET=HMIS_His_users_fpet)


##################################################################


all_data <- full_join(clients.small,clients_fpet, by=c("Method_Det", "Year")) %>%
  gather(Variable, Value, PreHMIS_Actual:HMISHis_FPET) %>%
  separate(Variable, c("UserGroup","Expected"), "_")

total_users <- all_data %>% group_by( Year, UserGroup, Expected) %>% summarise(Users=sum(Value)) %>% ungroup()

dif2020 <- total_users %>% filter(Year==2020) %>% group_by(Year, Expected) %>% summarise(total_users=sum(Users)) %>% 
  spread(Expected, total_users)  %>% mutate(difference=FPET-Actual)

# by method type
tot_user_meth <- all_data %>% mutate(LTM=case_when(Method_Num<=4 ~ 1,
                                                   Method_Num>4 ~ 0)) %>%
  group_by( Year, UserGroup, Expected, LTM) %>% summarise(Users=sum(Value)) %>% ungroup()


makeup2021_thisyear <- total_users %>% filter(Year==2021) %>% filter(Expected!="Actual") %>% 
  filter(UserGroup=="ThisYear") %>% select(Expected, Users) %>% rename(ThisYear_users=Users)

makeup2021 <- total_users %>% filter(Year==2021) %>% filter(UserGroup=="HMISHis") %>% 
  select(Year, Expected, Users) %>% spread(Expected, Users)   %>%
  mutate(Hisdifference=FPET-Actual) %>% rename(HMIS_His_Actual=Actual) %>%
  mutate(Expected="FPET") %>%
  full_join(makeup2021_thisyear, by="Expected") %>% select(Year, Expected, ThisYear_users, FPET, HMIS_His_Actual, Hisdifference)


dif2020_table <- total_users %>% filter(Year==2020) %>% group_by(Year, Expected) %>% summarise(total_users=sum(Users)) 

hist_2021table <- total_users %>% filter(Year==2021) %>% filter(UserGroup!="ThisYear") %>% group_by(Expected) %>% summarise(Users=sum(Users))  
##################################################
# Box data
box1 <- all_data %>% filter(Expected=="Actual") %>% filter(Year==2019 | Year==2020) %>% group_by(Year) %>% summarise(total=sum(Value)) %>% mutate(change=(total-lag(total))/lag(total))
box2 <- all_data %>% filter(Expected=="Actual") %>% filter(Year==2019 | Year==2020) %>% 
  mutate(LTM=case_when(Method_Num<=4 ~ "Long Term", Method_Num>4 ~ "Short Term"))  %>% group_by(Year, LTM) %>% 
  summarise(total=sum(Value)) %>% ungroup() %>% arrange(LTM, Year) %>% group_by(Year) %>% mutate(share=total/sum(total)) %>%
  ungroup() %>% group_by(LTM) %>% mutate(change=(total-lag(total))/lag(total))
box3 <-  all_data %>% filter(Expected=="Actual") %>% filter(Year==2019 | Year==2020) %>% group_by(Year, Method_Det) %>% summarise(total=sum(Value)) %>% arrange(Year, -total)
box4 <- all_data %>% filter(Expected=="Actual") %>% filter(Year==2019 | Year==2020) %>% filter(Method_Num==4) %>%
  group_by(Year, Method_Det) %>%  summarise(total=sum(Value)) %>% ungroup() %>% group_by(Year) %>% mutate(total_imp=sum(total)) %>%
  ungroup() %>% arrange(Method_Det, Year) %>% mutate(change=(total_imp-lag(total_imp))/lag(total_imp))
box5 <- all_data %>% filter(Expected=="Actual") %>% filter(Year==2019 | Year==2020) %>% filter(Method_Num==5) %>%
  group_by(Year, Method_Det) %>%  summarise(total=sum(Value)) %>% ungroup() %>% group_by(Year) %>% mutate(total_inj=sum(total)) %>%
  ungroup() %>% arrange(Method_Det, Year) %>% mutate(change=(total_inj-lag(total_inj))/lag(total_inj))
box6 <-  all_data %>% filter(Year==2020) %>% group_by(Expected) %>%  summarise(total=sum(Value)) %>% mutate(dif=(total-lead(total))/lead(total))
box7 <-  all_data %>% filter(Year==2021) %>% filter(UserGroup!="ThisYear") %>% group_by(Expected) %>%  summarise(total=sum(Value)) %>% mutate(dif=(total-lead(total))/lead(total))

######################################################
# Graphics

# Trends: Total
trend_total <- all_data %>% filter(Expected=="Actual") %>% filter(Year!=2021) %>% group_by(Year) %>% summarise(total=sum(Value))
trend_total_growth <- trend_total %>% mutate(change=(total-lag(total))/lag(total))
ggplot(trend_total, aes(x=Year, y=total)) +
  geom_bar(stat="identity", fill="blue") +
  labs(title="",
       x ="", y = "Estimated Modern Users") +
  theme_bw()

# trends: long and short term methods
trends_ltm_stm <- all_data %>% filter(Expected=="Actual") %>% filter(Year!=2021) %>% 
  mutate(LTM=case_when(Method_Num<=4 ~ "Long Term", Method_Num>4 ~ "Short Term"))  %>% group_by(Year, LTM) %>% 
  summarise(total=sum(Value))

ggplot(trends_ltm_stm, aes(x=Year, y=total, fill=LTM)) +
  geom_bar(stat="identity") +
  labs(title="",
       x ="", y = "Estimated Modern Users",
       fill="") +
  theme_bw() +
  theme(legend.position = "none")

# method specific trends
trend_method <- all_data %>% filter(Expected=="Actual") %>% filter(Year!=2021) %>% group_by(Method_Det, Year) %>% 
  summarise(total=sum(Value))

ggplot(trend_method, aes(x=Year, y=total, fill=Method_Det)) +
  geom_bar(stat="identity") +
  facet_wrap( ~ Method_Det) +
  labs(title="Estimated Modern Users",
       x ="", y = "Estimated Modern Users",
       fill="") +
  theme_bw() +
  theme(legend.position = "none")

# 2020 expected versus actual
data2020 <-  all_data %>% filter(Year==2020) %>% group_by(Expected) %>%  summarise(total=sum(Value))

ggplot(data2020, aes(x=Expected, y=total, fill=Expected)) +
  geom_bar(stat="identity") +
  labs(title="Estimated Modern Users 2020",
       x ="", y = "Estimated Modern Users",
       fill="") +
  theme_bw() +
  theme(legend.position = "none")

# 2021 Historical Users
data2021 <-  all_data %>% filter(Year==2021) %>% filter(UserGroup!="ThisYear") %>% group_by(Expected) %>%  summarise(total=sum(Value))

ggplot(data2021, aes(x=Expected, y=total, fill=Expected)) +
  geom_bar(stat="identity") +
  labs(title="Historical Estimated Modern Users 2021",
       x ="", y = "Estimated Modern Users",
       fill="") +
  theme_bw() +
  theme(legend.position = "none")

# Injectable trends
injectable <- all_data %>% filter(Expected=="Actual") %>% filter(Year!=2021) %>% filter(Method_Num==5) %>%
  group_by(Year, Method_Det) %>%  summarise(total=sum(Value)) 

cols.injectable <- c( "Depo Provera (DMPA)"=  "#433E85FF"  , "Lunelle"="#51C56AFF"  ,    "Noristerat (NET-En)"= "#2D708EFF" , "Sayana Press" = "#FDE725FF" )

ggplot(injectable, aes(x=Year, y=total, fill=Method_Det)) +
  geom_bar(stat="identity") +
  labs(title="Injectable Estimated Modern Users",
       x ="", y = "Estimated Modern Users",
       fill="") +
  scale_fill_manual(values = cols.injectable) +
  theme_bw() 

# Implant trends
implant <- all_data %>% filter(Expected=="Actual") %>% filter(Year!=2021) %>% filter(Method_Num==4) %>%
  group_by(Year, Method_Det) %>%  summarise(total=sum(Value)) 

cols.implant <- c("Implanon"=   "#2BB07FFF",   "Jadelle" =   "#440154FF"  ,  "Sino-Implant" ="#C2DF23FF")

ggplot(implant, aes(x=Year, y=total, fill=Method_Det)) +
  geom_bar(stat="identity") +
  labs(title="Implant Estimated Modern Users",
       x ="", y = "Estimated Modern Users",
       fill="") +
  scale_fill_manual(values = cols.implant) +
  theme_bw() 

