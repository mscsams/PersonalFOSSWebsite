# CHSTRONG - R Notebook
## Cardiac and Other  Health Care Utilization to adherence to cardiac care among adults with congenital heart disease 2016-2018
##### What are the healthcare utilization patterns, barriers to care, and health concerns among individuals with CHD? Do they differ by CHD severity? How do they compare to the general population?

20190429_Andrews_HealthcareUtililization

CH STRONG Data release folder is November 2019; Version 11319

Notebook Version Date August 03, 2020 JGA

## Data Prepararation
### Create Initial File
#### Obtain data
```{r, Get File & Set Dir}
CHSTRONG<-read.csv("/Users/Jen/Box/ARID_Arizona_Research_for_Improving_Diagnosis/CHD_Main/3_Operations/3_SOPsandManuals_CHDMain/CH_STRONG/20191118_SurveyData/CHSTRONG_11319.csv",header=T, sep=",") #Get File

setwd("/Users/Jen/Box/ARID_Arizona_Research_for_Improving_Diagnosis/CHD_Main/8_AnalyticProjects/20190429_Andrews_CHSTRONG_HealthcareUtililization") #Set working directory to your CHSTRONG Folder
```
#### Restrict Data to Desired Respondents
This analysis will restrict the dataset to participants who completed the CH STRONG survey where surveydata=1 and excluding individuals reported as deceased via Question 3 (other_reason_f not = 3) (n=1,656).
```{r, subset cases}
library(dplyr)
CHSTRONG_RESP<-CHSTRONG %>% filter(surveydata == 1, (other_reason_f != 3 | is.na(other_reason_f)))
```
#### Select Variables for analytic dataset
Provide list of variables required to produce data requested:

* Number/Percent by last seen: *Cardio_last_f*, *Cardio_visit_f* 
* Number/Percent by provider type last seen: *Cardio_type*
* Reasons not seen cardio: *NocardioXX_f (01, 04-08,999)*
* Gender: *SEX* 
* Race/Ethnicity: *RaceX_f (1-5)*
* Age in years: *age_surveycomplete*, *ages* 
* Works Full/Part Time: *anyworkX_f (1-3)*
* Completed High School: *education*
* Site: *Site*
* Insurance: *instypeXX_f (01-10,888,999)*
* Down syndrome: *DS_yn* 
* CHD: *Severity5*
* Cardiac Comorbidities: *ConditionsXX_f (04,05, 07,08,12)*
* Other Comorbidities:  *ConditionsXX_f (01-03,06,09-11,999)*
* Told current doctors about CHD: *place_inform*
* Not told about need to follow-up: *Discuss_need, NocardioXX_f (02-03)*
* Number of Cardiac Admissions in Past 12 Months: *Hosp12_chd_f*
* Health Care Utilization in Past 12 months: *Health12_visit*
* Number of Emergency Department Visits: *ER12visit*
* Number of Hospital Admissions: *Hosp12_admit*

### Create dataset
```{r, create dataset}
HCU <- CHSTRONG_RESP[,c("id","cardio_last_f","cardio_type","sex", "age_surveycomplete","cardio_visit_f","ages","nocardio01_f","nocardio02_f","nocardio03_f","nocardio04_f","nocardio05_f","nocardio06_f","nocardio07_f","nocardio08_f","nocardio999_f","race1_f","race2_f","race3_f","race4_f","race5_f","hispanic","anywork1_f","anywork2_f","anywork3_f","education","site","instype01_f","instype02_f","instype03_f","instype04_f","instype05_f","instype06_f","instype07_f","instype08_f","instype09_f","instype10_f","instype888_f","instype999_f","ins_any_f","ds_yn","severity5","conditions01_f","conditions02_f","conditions03_f","conditions04_f","conditions05_f","conditions06_f","conditions07_f","conditions08_f","conditions09_f","conditions10_f","conditions11_f","conditions12_f","conditions13_f","conditions999_f","place_inform","discuss_need","hosp12_chd_f","health12_visit","hosp12_admit","er12visit")] # Create database with variables required
```
### Demographics
#### Race-Ethnicity
Race and Ethnicity are combined in hierarchichal approach:
- Hispanic, Non Hispanic Black, Non Hispanic Other, Non Hispanic White
- Non Hispanic with Blank race is considered as Non Hispanic Other
```{r,RaceEth Combined}
# CHECK DISPERSION
ftable(HCU$hispanic) #check dispersion = 173 Hispanic
ftable(HCU$race3_f) #check dispersion = 245 BLACK
table(HCU$race3_f,HCU$race1_f,useNA = "ifany") #check dispersion = 76 AIAN not BLACK
table(HCU$race3_f,HCU$race2_f,useNA = "ifany") #check dispersion = 33 Asian not BLACK
table(HCU$race3_f,HCU$race4_f,useNA = "ifany") #check dispersion = 5 NHPI not BLACK
table(HCU$race1_f,HCU$race2_f,useNA = "ifany") #check dispersion = 0 AIAN Asian overlap
table(HCU$race1_f,HCU$race4_f,useNA = "ifany") #check dispersion = 1 AIAN NHPI overlap
table(HCU$race2_f,HCU$race4_f,useNA = "ifany") #check dispersion = 2 Asian NHPI overlap = 111 OTHER

#HISPANIC
HCU$raceeth[HCU$hispanic==1] <- 3 #Transfer into new variable
mytable1 <- table(HCU$raceeth,HCU$hispanic) #Verify accuracy of transformation
ftable(mytable1)

#BLACK
HCU$raceeth[HCU$race3_f==1&is.na(HCU$raceeth)] <- 2 #Transfer into new Race variable
table(HCU$raceeth) #Verify accuracy of transformation

#OTHER
HCU$raceeth[HCU$race1_f==1 & is.na(HCU$raceeth)] <- 4 #Transfer into new Race variable
HCU$raceeth[HCU$race2_f==1 & is.na(HCU$raceeth)] <- 4 #Transfer into new Race variable
HCU$raceeth[HCU$race4_f==1 & is.na(HCU$raceeth)] <- 4 #Transfer into new Race variable
table(HCU$raceeth) #Verify accuracy of transformation

#NH WHITE
HCU$raceeth[HCU$race5_f==1 & is.na(HCU$raceeth)] <- 1
table(HCU$raceeth) #Verify accuracy of transformation

#OTHER
HCU$raceeth[is.na(HCU$raceeth)] <- 4 #Change NA to OTHER

# Add value labels to variables
HCU$raceeth <- factor(HCU$raceeth, levels = c(1,2,3,4),
labels = c("NH White", "NH Black", "Hispanic", "NH Other/Unknown"))
table(HCU$raceeth, useNA = "ifany")

#SEX INTO NUMERIC
HCU$male[HCU$sex=='M'] <- 1 #Change M to 1
HCU$male[HCU$sex=='F'] <- 0 #Change F to 0
HCU$sex <- factor(HCU$sex, levels = c(0,1),
labels = c("No", "Yes"))

```
#### Works at least part-time
Variable combines full time (anywork1_f) and part time (anywork2_f) as "Yes" and includes does not work (anywork3_f) as "No".  Any missing answers are assumed to be "No".
```{r,Works Full/Part Time}
# Create Factor
HCU$works[(HCU$anywork1_f==1|HCU$anywork2_f== 1) & HCU$anywork3_f!= 1]<- 1
HCU$works[HCU$anywork3_f== 1] <- 0

#Verify accuracy of transformation
table(HCU$anywork2_f)
table(HCU$anywork1_f)
mytable1 <- table(HCU$works,HCU$anywork3_f)
ftable(mytable1)

HCU$works[is.na(HCU$works)] <- 0 #Change NA to No
table(HCU$works, useNA = "ifany")
# Add value labels to variables
HCU$works <- factor(HCU$works, levels = c(0,1),
          labels = c("No", "Yes"))
```
#### HS Education
Variable groups education response 1 - 3 as "No", 4 - 8 as "Yes".  Any missing answers are assumed to be "No".
```{r,HS Education}
# Create Factor 
HCU$hs[HCU$education<4]<-0
HCU$hs[HCU$education<9 & is.na(HCU$hs)]<-1
table(HCU$hs, useNA = "ifany") #Verify accuracy of transformation
HCU$hs[is.na(HCU$hs)] <- 0 #Change NA to No
table(HCU$hs, useNA = "ifany") #Verify accuracy of transformation

# Add value labels to variables
HCU$hs <- factor(HCU$hs, levels = c(0,1),
          labels = c("No", "Yes"))
```
#### Insurance
Insurance grouped into private (Private & Military), public (
Medicare, Medi-gap,Medicaid, SCHIP, Indian Health Service, state-sponsored, Other government), other (Single service plan  & Other  & any insruance Yes) and None(Any insurance No)
```{r,Insurance type}
# CHECK DISPERSION
ftable(HCU$instype01_f,HCU$instype06_f) # 918 Private
table(HCU$ins_any_f)

# Create Factor
HCU$insurance[HCU$instype01_f==1|HCU$instype06_f==1]<-1
HCU$insurance[(HCU$instype02_f==1|HCU$instype03_f==1|HCU$instype04_f==1|HCU$instype05_f==1|HCU$instype07_f==1|HCU$instype08_f==1|HCU$instype09_f==1) & is.na(HCU$insurance)]<-2
HCU$insurance[(HCU$instype10_f==1|HCU$instype999_f==1)& is.na(HCU$insurance)]<-3

HCU$insurance[HCU$ins_any_f==0 & is.na(HCU$insurance)]<-4
HCU$insurance[HCU$ins_any_f==1 & is.na(HCU$insurance)]<-3

#Verify accuracy of transformation
table(HCU$insurance, useNA = "ifany")

# Add value labels to variables
HCU$insurance <- factor(HCU$insurance, levels = c(1,2,3,4),labels = c("Private", "Public","Other","None"))
```
### Comorbidities
#### Cardiac Comorbidities
Cardiac comorbidities include: 
Congestive heart failure, Cardiac dysrhythmias or irregular heart beat, heart attack, stroke,  Hypertension.
Any missing answers are assumed to be "No".

```{r,Cardiac Comorbidities}
# Create Factor
HCU$comorbid_Cardiac[HCU$conditions04_f==1|HCU$conditions05_f==1|HCU$conditions07_f==1|HCU$conditions08_f==1|HCU$conditions12_f==1]<-1 #Any cardiac comorbidity - CHF, ARRH, MI, Stroke, HT

HCU$comorbid_Cardiac[is.na(HCU$comorbid_Cardiac)]<-0

#Verify accuracy of transformation
table(HCU$comorbid_Cardiac, useNA = "ifany")

# Add value labels to variables
HCU$comorbid_Cardiac <- factor(HCU$comorbid_Cardiac, levels = c(0,1),labels = c("No", "Yes"))
```
#### Other Comorbidities
Other comorbidities include: 
Diabetes or sugar diabetes, Obstructive sleep apnea, Cancer or a malignancy of any kind, mood disorder or depression, Asthma,  ulcer, Arthritis, gout, lupus, fibromyalgia, Other
 Any missing answers are assumed to be "No".
```{r,Other Comorbidities}
# Create Factor
HCU$comorbid_other[HCU$conditions01_f==1|HCU$conditions02_f==1|HCU$conditions03_f==1|HCU$conditions06_f==1|HCU$conditions09_f==1|HCU$conditions10_f==1|HCU$conditions11_f==1|HCU$conditions999_f==1]<-1 #Any other comorbidity

HCU$comorbid_other[is.na(HCU$comorbid_other)]<-0

#Verify accuracy of transformation
table(HCU$comorbid_other, useNA = "ifany")

# Add value labels to variables
HCU$comorbid_other <- factor(HCU$comorbid_other, levels = c(0,1),labels = c("No", "Yes"))
```
#### Total Comorbidities
Sum of all Yes responses for cardiac and other comorbidities. No Value = 0.
```{r,Total Comorbidities}
HCU$comorbid_total<-rowSums(HCU[,c("conditions01_f", "conditions02_f","conditions03_f","conditions04_f","conditions05_f","conditions06_f","conditions07_f","conditions08_f","conditions09_f","conditions10_f","conditions11_f","conditions12_f","conditions999_f")])
HCU$comorbid_total[is.na(HCU$comorbid_total)]<-0

#Verify accuracy of transformation
table(HCU$comorbid_total, useNA = "ifany")

as.numeric(HCU$comorbid_total)
```
### Variables for DAP 
#### Last Seen by Cardiologist
Last seen by cardiologist is combination of categorical variable (Last_seen) and number of visits in last year (cardio_last_f)
```{r,Last Seen by cardiologist }
# Create Factor
HCU$last_seen<-HCU$cardio_last_f
table(HCU$last_seen, useNA = "ifany") #Verify accuracy of transformation
HCU$last_seen[is.na(HCU$last_seen)] <- 5 #Change NA to Never
HCU$last_seen <- factor(HCU$last_seen, levels = c(1,2,3,4,5),
labels = c("<1y", "1-2y", "3-5y", ">5y","Never")) #Add Value Labels
table(HCU$last_seen, useNA = "ifany")#Verify accuracy of transformation
HCU$cardio_type[is.na(HCU$cardio_type)] <- 4 #Change NA to None/Unknown
HCU$cardio_type <- factor(HCU$cardio_type, levels =     c(1,2,3,4),labels = c("Pediatric","ACHD", "Adult","Unknown"))
table(HCU$cardio_type, useNA = "ifany")#Verify accuracy of transformation
```
#### Not told about need to follow-up
Discuss need is "no" and reason for no cardio is "not needed" or "doctor said not needed" 
```{r,No Follow Up}
# Create Factor
HCU$noneed[HCU$discuss_need==0|HCU$nocardio02_f==1|HCU$nocardio03_f==1]<- "Yes"
HCU$noneed[HCU$discuss_need==1]<-"No"

#Verify accuracy of transformation
table(HCU$noneed, useNA = "ifany")
```
#### Categorical Healthcare Utilization 
For ER visits, admissions, and health visits recode as None, one and two or more.
```{r,Healthcare Utilization}
# Create Factor
HCU$Admits[HCU$hosp12_admit==0]<-0
HCU$Admits[HCU$hosp12_admit==1]<-1
HCU$Admits[HCU$hosp12_admit<8 & is.na(HCU$Admits)]<-2

HCU$ER[HCU$er12visit==0]<-0
HCU$ER[HCU$er12visit==1]<-1
HCU$ER[HCU$er12visit<8 & is.na(HCU$ER)]<-2

HCU$Visits[HCU$health12_visit==0]<-0
HCU$Visits[HCU$health12_visit==1]<-1
HCU$Visits[HCU$health12_visit<8 & is.na(HCU$Visits)]<-2

# Add value labels to variables
HCU$Admits <- factor(HCU$Admits, levels = c(0,1,2),labels = c("None", "One", "Two or more"))
HCU$Visits <- factor(HCU$Visits, levels = c(0,1,2),labels = c("None", "One", "Two or more"))
HCU$ER <- factor(HCU$ER, levels = c(0,1,2),labels = c("None", "One", "Two or more"))

#Verify accuracy of transformation
table(HCU$Visits, useNA = "ifany")
table(HCU$ER, useNA = "ifany")
table(HCU$Admits, useNA = "ifany")
```
### Value Labels
For any variables that will remain as is - add descriptive variable labels for analysis.
```{r, Value Labels}
# Add value labels to variables with no change
HCU$ages <- factor(HCU$ages, levels = c(1,2,3),labels = c("<25 years", "25-30 years", ">30 years"))
HCU$ds_yn <- factor(HCU$ds_yn, levels = c(1,0),labels = c("Yes", "No"))
HCU$severity5 <- factor(HCU$severity5, levels =   c(1,2,3,4,5),labels = c("Other CHD","Valve CHD", "Shunt CHD","Shunt + Valve CHD", "Severe CHD"))
HCU$place_inform <- factor(HCU$place_inform, levels =  c(1,0),labels = c("Yes", "No"))
HCU$nocardio04_f <- factor(HCU$nocardio04_f, levels =  c(1),labels = c("Parents stopped taking me"))
HCU$nocardio01_f <- factor(HCU$nocardio01_f, levels = c(1),labels = c("Felt well"))
HCU$nocardio05_f <- factor(HCU$nocardio05_f, levels = c(1),labels = c("Insurance"))
HCU$nocardio06_f <- factor(HCU$nocardio06_f, levels = c(1),labels = c("Moved"))
HCU$nocardio07_f <- factor(HCU$nocardio07_f, levels = c(1),labels = c("Bad Doctor"))
HCU$nocardio08_f <- factor(HCU$nocardio08_f, levels = c(1),labels = c("No Doctor"))
HCU$nocardio02_f <- factor(HCU$nocardio02_f, levels = c(1),labels = c("No Need"))
HCU$nocardio03_f <- factor(HCU$nocardio03_f, levels = c(1),labels = c("Doctor said no need"))
```
### Delete Variables
Delete unused variables.
```{r,Delete vars}
library(dplyr)

HCU %>%
  select("male","raceeth","ages","age_surveycomplete","works","hs","site","insurance","ds_yn","severity5","comorbid_Cardiac","comorbid_other","comorbid_total","last_seen","Admits","ER","Visits","noneed","cardio_type","place_inform","nocardio01_f","nocardio02_f","nocardio03_f","nocardio04_f","nocardio05_f","nocardio06_f","nocardio07_f","nocardio08_f")
```
### Analysis
#### Table 1
Descriptive characteristics of sample by last seen (N, weighted %)
```{r,Table 1}
# Frequency count
tabyl(HCU, male, last_seen)
tabyl(HCU, male)
tabyl(HCU, raceeth, last_seen)
tabyl(HCU, raceeth)
tabyl(HCU, ages, last_seen)
tabyl(HCU, ages)
tabyl(HCU, works, last_seen)
tabyl(HCU, works)
tabyl(HCU, hs, last_seen)
tabyl(HCU, hs)
tabyl(HCU, site, last_seen)
tabyl(HCU, site)
tabyl(HCU, insurance, last_seen)
tabyl(HCU, insurance)
tabyl(HCU, ds_yn, last_seen)
tabyl(HCU, ds_yn)
tabyl(HCU, severity5, last_seen)
tabyl(HCU, severity5)
tabyl(HCU, comorbid_Cardiac, last_seen)
tabyl(HCU, comorbid_Cardiac)
tabyl(HCU, comorbid_other, last_seen)
tabyl(HCU, comorbid_other)
tabyl(HCU, Admits, last_seen)
tabyl(HCU, Admits)
tabyl(HCU, ER, last_seen)
tabyl(HCU, ER)
tabyl(HCU, Visits, last_seen)
tabyl(HCU, Visits)
tabyl(HCU, noneed, last_seen)
tabyl(HCU, noneed)
tabyl(HCU, place_inform, last_seen)
tabyl(HCU, place_inform)
```
#### Figure 1
Line chart describing last receipt of cardiac care by age and gender
```{r,Figure 1}
# Create Line Chart
# Libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
# Summarize data
mean_data <- group_by(HCU, sex, last_seen) %>%
             summarise(age_surveycomplete = mean(age_surveycomplete, na.rm = TRUE))

# Plot 1
p1 <- ggplot(data=mean_data, aes(x=last_seen, y=age_surveycomplete, group=sex, shape=sex, color=sex)) + geom_line() + geom_point() + labs(x="Time since Last Seen", y="Age in Years") + scale_x_discrete(labels=c("< 1 Year","1-2 Years","3-5 Years",">5 Years", "Never")) + theme_bw()
p1
```
```{r,Figure 1}
#Create frequencies for last seen by cardio type
d2 <- HCU %>%
    filter(last_seen != ">5y") %>%
    filter(last_seen != "Never") %>%        
    count(last_seen, cardio_type) %>%
    group_by(last_seen) %>%
    mutate(percent = n / sum(n) * 100) %>%
    ungroup()
d2$percent<- round(d2$percent, digits = 0)
g2<-ggplot(d2, aes(x = last_seen, y = percent, fill = cardio_type)) + geom_bar(stat = "identity", position = "dodge") # Basic barplot
g2 <- g2 + scale_x_discrete(labels=c("< 1 Year","1-2 Years","3-5 Years")) #Revise x-axis labels
g2 <- g2  +  theme(legend.position = "bottom",axis.title.x = element_blank()) +labs(fill = "Cardiologist") #Remove x-axis title and change legend title
g2
```
