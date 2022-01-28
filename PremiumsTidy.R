library(tidyverse)
library(haven)
library(readxl)

## Loading in the data------

setwd('~/source-data')

noniles<- read_dta('clean version (by cz).dta')%>% # The authors were able to release to us average CZ effect values for CZs split into noniles (ie. nine groups)
  dplyr::filter(., grepl('quantile', sample))
outliers <- read_excel('official table.xlsx', sheet=3)%>%  # We also got access to 10 top and bottom outliers and their average CZ effect. This allows us to create 11 groups 
  mutate(., CZname=gsub(".*: ","",Names), CZnum=as.numeric(gsub(".:.*","",Names))*10)
meanWages<- read_dta('czearningscounts_dan.dta')%>% # This is the mean wage data for each CZ from ACS
  rename(., CZ=cz)%>%
  dplyr::select(., -sample)
housing <- read_excel('newhranks.xlsx')%>% # This is info on housing costs for each CZ
  rename(., CZ=cz)

## Merging and transforming-----

# y= average wage in the nonile, phi = CZ effect, pe =person effect (average of nonile), xb= covariates
# ln_cbp15_earnq1 = average (logged) quarterly earnings
# acs1018_wcount = the number of workers in each CZ


noniles_merge <- noniles%>%
  left_join(.,outliers)%>% # Taking the noniles data and merging with the outliers
  rename(., phi=fe)%>%
  mutate(y=ifelse(is.na(y_c), y, y_c), # Replacing the values from the noniles data if the CZ is an outlier
         pe=ifelse(is.na(pe_c), pe, pe_c),
         phi=ifelse(is.na(phi_c), phi, phi_c),
         xb=ifelse(is.na(xb_c), xb, xb_c),
         sample=ifelse(is.na(Group), sample, Group))%>%
  left_join(., meanWages)%>% # Merging other data sets
  left_join(., housing)%>%
  mutate(., CZname=ifelse(CZ==38601,'Spokane city W, WA', 
                          ifelse(CZ==38602,'Spokane city E, WA', CZname)))%>% # Fixing a naming issue
  mutate(., wagesWeighted=ln_cbp15_earnq1*acs1018_wcount)%>%
  mutate(., meanWages= mean(wagesWeighted)/mean(acs1018_wcount))%>%
  mutate(., baseWage=min(ln_cbp15_earnq1))%>% ## Step 1, Make a baseline wage 
  mutate(., rebasedCZ=phi-min(phi))%>% ## Step 2, Make CZ score fluctuate above 0
  mutate(., rebasedSkill=ln_cbp15_earnq1-baseWage-rebasedCZ-xb)%>%  ## Step 3, Calculate worker skill effect (relative to lowest wage location)
  mutate(., rebasedcz_rent=cz_rent-cz_rent[which.min(logrent)], ## Step 4: Make baseline wage
         rebasedcz_rent_test=(exp(logrent)-exp(min(logrent)))/exp(baseWage))%>%
  mutate(., rent_multiplier=-(exp(logrent)*3/exp(baseWage))*100, ## Step 5: Transform everything wrt baseline wage 
         CZ_multiplier= exp(rebasedCZ)*100-100, 
         rent_multiplier_dif=rent_multiplier-max(rent_multiplier), 
         person_multiplier=exp(rebasedSkill)*100-100)%>%
  mutate(., CZname=fct_reorder(factor(CZname), CZ_multiplier+ person_multiplier))%>%
  mutate(., NetPos=ifelse(CZ_multiplier > -rent_multiplier_dif, 1,0))%>%
  mutate(., CZname=fct_reorder(factor(CZname), CZ_multiplier+ person_multiplier), 
         order=rank(CZ_multiplier+ person_multiplier))%>%
  mutate(., test=CZ_multiplier+person_multiplier)

# Plotting -----
PlotData <- noniles_merge%>%
  dplyr::select(., CZname, rent_multiplier_dif,  person_multiplier ,CZ_multiplier)%>%
  pivot_longer(., cols=c(-'CZname'), values_to='values', names_to='variables')%>%
  left_join(.,noniles_merge)%>%
  dplyr::select(., CZname, values,  variables ,NetPos)

ggplot(PlotData, aes(y=CZname, x=values, colour=NetPos,
                     fill=factor(variables,levels=c( "person_multiplier","CZ_multiplier", "rent_multiplier_dif"))))+
  geom_bar(stat='identity')+
  labs(y='',x='Multiplicative effect on baseline wages',fill='',
       title='All effects relative to wage baseline')


