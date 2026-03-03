#Read in Packages 
library(tidyverse)
library(epiR)
library(epitools)
library(survival)
library(survMisc)
library(survival)
library(survminer)
library(readxl)
library(Hmisc)
library(car)
library(DescTools)

#load Youth Risk Behavior Surveilance 2023 Dataset
yrbs <- read_rds("~/Downloads/Spring 26/EP850 (Applications of Intermediate Epidemiology)/yrbs2023_final.rds")
dim(yrbs) #dimesions 
unique(yrbs) #variable names 

#Group Question:
#How is sexual identity associated with suicidal ideation?
#Does this association differ by race/ethnicity? 

#frequency table sexual identity and SI, inital visualizations 
tab1 <-table(yrbs$sexual_identity, yrbs$suicideideation_binary)
pct_tab1 <- round(prop.table(tab1, 2) * 100, 2)
tab1
pct_tab1
table(yrbs$raceeth, yrbs$suicideideation_binary) #freq table race and SI
table(yrbs$age, yrbs$suicideideation_binary) #freq table age and SI 

#Data Cleaning/Modifying
#restrict data to >13 years of age, create new sexual identity dich. var.
yrbs_clean <- yrbs %>% 
  select(sexual_identity, suicideideation_binary, raceeth, age, 
         bulliedschool_binary, sex, lowgrades_binary) %>% 
  filter(age %in% c(3, 4, 5, 6, 7)) %>% 
  mutate(sexual_identity_cat = case_when(
    sexual_identity == 1 ~ 0,
    sexual_identity %in% c(2, 3, 4, 5) ~ 1,
    TRUE ~ NA
    ))

#Prevalence of SI amoung sample 
table(yrbs_clean$suicideideation_binary)
4173/(15334+4173) # = 0.214

#manipulate variables
vector_n_pct_cc <- function(vector_exp, vector_dis) {
  tab <- table(vector_exp, vector_dis)
  prop_tab <- round(prop.table(tab, margin = 2) * 100, 2)
  dis_levels <- colnames(tab)
  
  out <- cbind(
    case_n = tab[, dis_levels[2]],
    case_pct = prop_tab[, dis_levels[2]],
    control_n = tab[, dis_levels[1]],
    control_pct = prop_tab[, dis_levels[1]]
  )
  #write.csv(out, paste0("Vector_n_pct_cc"), row.names = F)
  out <- as.data.frame(out)
  print(out)
}

#freq table of all covariates
vector_n_pct_cc(yrbs_clean$sexual_identity_cat, yrbs_clean$suicideideation_binary)
vector_n_pct_cc(yrbs_clean$raceeth, yrbs_clean$suicideideation_binary)
vector_n_pct_cc(yrbs_clean$age, yrbs_clean$suicideideation_binary)
vector_n_pct_cc(yrbs_clean$bulliedschool_binary, yrbs_clean$suicideideation_binary)
vector_n_pct_cc(yrbs_clean$sex, yrbs_clean$suicideideation_binary)
vector_n_pct_cc(yrbs_clean$lowgrades_binary, yrbs_clean$suicideideation_binary)


#Assocation Testing 
#Crude 
crude_tab <- table(desc(yrbs_clean$sexual_identity_cat), desc(yrbs_clean$suicideideation_binary))
prop.table(crude_tab)

#OR 95%CI, Chisq, p
epi.2by2(crude_tab, method = 'cross.sectional')
#Crude prev OR = 4.39 (4.05, 4.73), chisq = 1393, p<0.001


#MH adjusted OR
tab_adjust <- table(desc(yrbs_clean$sexual_identity_cat), desc(yrbs_clean$suicideideation_binary), desc(yrbs_clean$raceeth))
epi.2by2(tab_adjust)


#Simple Logistic Regression Model 
simple.model.out <- glm(suicideideation_binary ~ sexual_identity_cat, 
                        data = yrbs_clean, family = binomial(link=logit))
summary(simple.model.out) 
exp(coef(simple.model.out)) #ORs
exp(confint(simple.model.out)) #Cis
Cstat(simple.model.out) #C stat

#Adjusted Logistic Regression Model 
model.out <- glm(suicideideation_binary ~ sexual_identity_cat + 
                   relevel(factor(raceeth), ref = '5') + 
                   relevel(factor(age), ref = '5') + sex + bulliedschool_binary + 
                   lowgrades_binary,data = yrbs_clean, 
                 family = binomial(link=logit))
summary(model.out)
exp(coef(model.out)) #ORs
exp(confint(model.out)) #Cis
Cstat(model.out) #C stat


#Adjusted Model Stratified by Race/Ethnicity 
strat.model.out <- glm(suicideideation_binary ~ sexual_identity_cat * relevel(factor(raceeth), ref = '5') + 
                    relevel(factor(age), ref = '5') + sex + bulliedschool_binary + 
                    lowgrades_binary,data = yrbs_clean, family = binomial(link=logit))
summary(strat.model.out)
exp(coef(strat.model.out)) #ORs
exp(confint(strat.model.out)) #Cis
Cstat(strat.model.out) #C stat









