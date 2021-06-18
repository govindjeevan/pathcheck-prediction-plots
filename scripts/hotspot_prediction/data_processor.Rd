####### load libraries #################
library(deSolve)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(lubridate)
library(plotly)
library(zoo)
library (readr)
library(cronR)
library(RCurl)
library(slider)
####### download from git #################
setwd("/Users/christinglorioso/Projects/covid_predictive_analytics/Emerging_hotspots/Christin")
urlfile= getURL("https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/estimates-symptom-survey/region-v11/IN.csv")
IN_UMD_cases<-read.csv(text=urlfile)
urlfile= getURL("https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/jhu/region/IN.csv")
IN_JHU_cases<-read.csv(text=urlfile)
urlfile= getURL("https://raw.githubusercontent.com/govindjeevan/PathCheck-Covid-Plots/master/PathCheck%20Predictive/population.txt")
IN_population<-read.csv(text=urlfile)
########## combine into one dataframe ##########
IN_JHU_cases$Country_region<-paste(IN_JHU_cases$country, IN_JHU_cases$region, sep=" ") #creates column to merge data by
IN_UMD_cases$Country_region<-paste(IN_UMD_cases$country_agg, IN_UMD_cases$region_agg, sep=" ") #creates column to merge data by
IN_UMD_cases$Country_region<-sub("India NCT of Delhi", "India Delhi", IN_UMD_cases$Country_region)
IN_population$Country_region<-paste("India",IN_population$region, sep=" " ) #creates column to merge data by
IN_UMD_JHU<-merge(IN_UMD_cases, IN_JHU_cases, by=c("Country_region", "date")) #merges data
IN_UMD_JHU_pop<-merge(IN_UMD_JHU, IN_population, by="Country_region") #merges data
########## create new columns smoothing, slope, categories###########################
IN_UMD_JHU_pop$active_perpop<-IN_UMD_JHU_pop$Active/IN_UMD_JHU_pop$population
regions_ls<-unique(IN_UMD_JHU_pop$Country_region) #list of regions
IN_UMD_JHU_pop_ksmooth<-data.frame()
for (i in regions_ls) {
cases <- plot_ly(data = IN_UMD_JHU_pop)
IN_UMD_JHU_pop_s<-IN_UMD_JHU_pop[IN_UMD_JHU_pop$"Country_region"==i,]
IN_UMD_JHU_pop_s$date<-as.Date(IN_UMD_JHU_pop_s$date)
ks1<- ksmooth(IN_UMD_JHU_pop_s$date, IN_UMD_JHU_pop_s$p_cli, "normal",80, x.points=IN_UMD_JHU_pop_s$date)
IN_UMD_JHU_pop_s$p_cli_ksmooth<-ks1$y
ks2<-ksmooth(IN_UMD_JHU_pop_s$date, IN_UMD_JHU_pop_s$active_perpop, "normal",80, x.points=IN_UMD_JHU_pop_s$date)
IN_UMD_JHU_pop_s$active_perpop_ksmooth<-ks2$y
IN_UMD_JHU_pop_ksmooth<-rbind(IN_UMD_JHU_pop_ksmooth, IN_UMD_JHU_pop_s)
}
IN_UMD_JHU_pop_ksmooth<- IN_UMD_JHU_pop_ksmooth %>%
    dplyr::arrange(desc(Country_region)) %>% 
    dplyr::group_by(Country_region) %>% 
   dplyr::mutate(p_cli_ksmooth_slope= p_cli_ksmooth- lag(p_cli_ksmooth, default = first(p_cli_ksmooth), order_by = date)) %>% 
dplyr::ungroup()
write.csv(IN_UMD_JHU_pop_ksmooth, "Data/IN_UMD_JHU_pop_ksmooth.csv")
##### predict df ip ############
predict_ip<-data.frame()
for (i in regions_ls) {
IN_UMD_JHU_pop_s<-IN_UMD_JHU_pop_ksmooth[IN_UMD_JHU_pop_ksmooth$"Country_region"==i,]
IN_UMD_JHU_pop_s$date<-as.Date(IN_UMD_JHU_pop_s$date)
max_date<-max(IN_UMD_JHU_pop_s$date)
max_date_lag1<-max_date-1
IN_UMD_JHU_pop_s<-IN_UMD_JHU_pop_s[IN_UMD_JHU_pop_s$date<=max_date & IN_UMD_JHU_pop_s$date>=max_date_lag1,]
p_cli_ksmooth_slope<-IN_UMD_JHU_pop_s$p_cli_ksmooth_slope
indicies<-which(p_cli_ksmooth_slope>0)
if(length(indicies)>0){
lead_60_pos<-IN_UMD_JHU_pop_s[indicies,]
pos_lead<-lead_60_pos[2:nrow(lead_60_pos),]
pos_lag<-lead_60_pos[1:nrow(lead_60_pos)-1,]
sub<-pos_lead$p_cli_ksmooth_slope- pos_lag$p_cli_ksmooth_slope
Indicies_2<-which(sub>0)
if(length(Indicies_2)>0){
lead_60_pos_inc<-lead_60_pos[Indicies_2,]
p_cli_ksmooth_slope<-lead_60_pos_inc$p_cli_ksmooth_slope
date<-as.Date(lead_60_pos_inc$"date")
date<-as.data.frame(date)
vars<-cbind(i, date, p_cli_ksmooth_slope)
vars<-as.data.frame(vars)
colnames(vars)<-c("State", "date", "p_cli_ksmooth_slope")
predict_ip<-rbind(predict_ip, vars)
}
}
}
df_p<-predict_ip
df_p$slope_log<-log((as.numeric(df_p$p_cli_ksmooth_slope)*1000))+10
load("scripts/model_ip.rda")
predict_ip_out<-predict(model_ip, newdata=df_p)
predict_ip_out<-cbind(predict_ip, predict_ip_out)
colnames(predict_ip_out)<-c("State", "date", "p_cli_ksmooth_slope", "predicted")
########### predict dp ##################
predict_dp<-data.frame()
for (i in regions_ls) {
IN_UMD_JHU_pop_s<-IN_UMD_JHU_pop_ksmooth[IN_UMD_JHU_pop_ksmooth$"Country_region"==i,]
IN_UMD_JHU_pop_s$date<-as.Date(IN_UMD_JHU_pop_s$date)
max_date<-max(IN_UMD_JHU_pop_s$date)
max_date_lag1<-max_date-1
#IN_UMD_JHU_pop_s<-IN_UMD_JHU_pop_s[IN_UMD_JHU_pop_s$date>"2021-05-31",]
IN_UMD_JHU_pop_s<-IN_UMD_JHU_pop_s[IN_UMD_JHU_pop_s$date<=max_date & IN_UMD_JHU_pop_s$date>=max_date_lag1,]
p_cli_ksmooth_slope<-IN_UMD_JHU_pop_s$p_cli_ksmooth_slope
indicies<-which(p_cli_ksmooth_slope>0)
if(length(indicies)>0){
lead_60_pos<-IN_UMD_JHU_pop_s[indicies,]
pos_lead<-lead_60_pos[2:nrow(lead_60_pos),]
pos_lag<-lead_60_pos[1:nrow(lead_60_pos)-1,]
sub<-pos_lead$p_cli_ksmooth_slope- pos_lag$p_cli_ksmooth_slope
Indicies_2<-which(sub<0)
if(length(Indicies_2)>0){
lead_60_pos_inc<-lead_60_pos[Indicies_2,]
p_cli_ksmooth_slope<-lead_60_pos_inc$p_cli_ksmooth_slope
date<-as.Date(lead_60_pos_inc$"date")
date<-as.data.frame(date)
vars<-cbind(i, date, p_cli_ksmooth_slope)
vars<-as.data.frame(vars)
colnames(vars)<-c("State", "date", "p_cli_ksmooth_slope")
predict_dp<-rbind(predict_dp, vars)
}
}
}
df_p<-predict_dp
df_p$slope_log<-log((as.numeric(df_p$p_cli_ksmooth_slope)*1000))+10
#save(model_dp, file = "scripts/model_dp.rda")
load("scripts/model_dp.rda")
predict_dp_out<-predict(model_ip, newdata=df_p)
predict_dp_out<-cbind(predict_dp, predict_dp_out)
colnames(predict_dp_out)<-c("State", "date", "p_cli_ksmooth_slope", "predicted")
############ pred nd #################
predict_nd<-data.frame()
for (i in regions_ls) {
IN_UMD_JHU_pop_s<-IN_UMD_JHU_pop_ksmooth[IN_UMD_JHU_pop_ksmooth$"Country_region"==i,]
IN_UMD_JHU_pop_s$date<-as.Date(IN_UMD_JHU_pop_s$date)
max_date<-max(IN_UMD_JHU_pop_s$date)
max_date_lag1<-max_date-1
#IN_UMD_JHU_pop_s<-IN_UMD_JHU_pop_s[IN_UMD_JHU_pop_s$date>"2021-05-31",]
IN_UMD_JHU_pop_s<-IN_UMD_JHU_pop_s[IN_UMD_JHU_pop_s$date<=max_date & IN_UMD_JHU_pop_s$date>=max_date_lag1,]
p_cli_ksmooth_slope<-IN_UMD_JHU_pop_s$p_cli_ksmooth_slope
indicies<-which(p_cli_ksmooth_slope<0)
if(length(indicies)>0){
lead_60_pos<-IN_UMD_JHU_pop_s[indicies,]
pos_lead<-lead_60_pos[2:nrow(lead_60_pos),]
pos_lag<-lead_60_pos[1:nrow(lead_60_pos)-1,]
sub<-pos_lead$p_cli_ksmooth_slope- pos_lag$p_cli_ksmooth_slope
Indicies_2<-which(sub<0)
if(length(Indicies_2)>0){
lead_60_pos_inc<-lead_60_pos[Indicies_2,]
p_cli_ksmooth_slope<-lead_60_pos_inc$p_cli_ksmooth_slope
date<-as.Date(lead_60_pos_inc$"date")
date<-as.data.frame(date)
vars<-cbind(i, date, p_cli_ksmooth_slope)
vars<-as.data.frame(vars)
colnames(vars)<-c("State", "date", "p_cli_ksmooth_slope")
predict_nd<-rbind(predict_nd, vars)
}
}
}
df_p<-predict_nd
df_p$slope_log<-as.numeric(df_p$p_cli_ksmooth_slope)+20
#save(model_nd, file = "scripts/model_nd.rda")
load("scripts/model_nd.rda")
predict_nd_out<-predict(model_nd, newdata=df_p)
predict_nd_out<-cbind(predict_nd, predict_nd_out)
colnames(predict_nd_out)<-c("State", "date", "p_cli_ksmooth_slope", "predicted")
############ pred ni #################
predict_ni<-data.frame()
for (i in regions_ls) {
IN_UMD_JHU_pop_s<-IN_UMD_JHU_pop_ksmooth[IN_UMD_JHU_pop_ksmooth$"Country_region"==i,]
IN_UMD_JHU_pop_s$date<-as.Date(IN_UMD_JHU_pop_s$date)
max_date<-max(IN_UMD_JHU_pop_s$date)
max_date_lag1<-max_date-1
#IN_UMD_JHU_pop_s<-IN_UMD_JHU_pop_s[IN_UMD_JHU_pop_s$date>"2021-05-31",]
IN_UMD_JHU_pop_s<-IN_UMD_JHU_pop_s[IN_UMD_JHU_pop_s$date<=max_date & IN_UMD_JHU_pop_s$date>=max_date_lag1,]
p_cli_ksmooth_slope<-IN_UMD_JHU_pop_s$p_cli_ksmooth_slope
indicies<-which(p_cli_ksmooth_slope<0)
if(length(indicies)>0){
lead_60_pos<-IN_UMD_JHU_pop_s[indicies,]
pos_lead<-lead_60_pos[2:nrow(lead_60_pos),]
pos_lag<-lead_60_pos[1:nrow(lead_60_pos)-1,]
sub<-pos_lead$p_cli_ksmooth_slope- pos_lag$p_cli_ksmooth_slope
Indicies_2<-which(sub>0)
if(length(Indicies_2)>0){
lead_60_pos_inc<-lead_60_pos[Indicies_2,]
p_cli_ksmooth_slope<-lead_60_pos_inc$p_cli_ksmooth_slope
date<-as.Date(lead_60_pos_inc$"date")
date<-as.data.frame(date)
vars<-cbind(i, date, p_cli_ksmooth_slope)
vars<-as.data.frame(vars)
colnames(vars)<-c("State", "date", "p_cli_ksmooth_slope")
predict_ni<-rbind(predict_ni, vars)
}
}
}
df_p<-predict_ni
df_p$slope_log<-as.numeric(df_p$p_cli_ksmooth_slope)+20
#save(model_ni, file = "scripts/model_ni.rda")
load("scripts/model_ni.rda")
predict_ni_out<-predict(model_ni, newdata=df_p)
predict_ni_out<-cbind(predict_ni, predict_ni_out)
colnames(predict_ni_out)<-c("State", "date", "p_cli_ksmooth_slope", "predicted")
################### bind together current predictions ###############
Six_week_predictions<-rbind(predict_ip_out, predict_dp_out, predict_nd_out, predict_ni_out)
Six_week_predictions$predicted[Six_week_predictions$predicted<0]<-0
   
write.csv(Six_week_predictions, "Data/Six_week_predictions.csv")