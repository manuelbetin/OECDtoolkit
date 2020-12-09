struc_stat=function(ctry_code, struc){

# 1 home-ownership stats: min max and below above or close t average
house_tenure_data=struc %>% select(ISO3_code, period, Own_outright, Owner_with_mortgage, Private_rent, Subsidized_rent,Other )%>%   filter(year(period)==2018)
house_tenure_data<- house_tenure_data %>% mutate(temp=rowSums(house_tenure_data[,c("Own_outright", "Owner_with_mortgage", "Private_rent", "Subsidized_rent","Other")], na.rm=TRUE)) %>%
  filter(temp>0) %>% select(ISO3_code, period, Own_outright, Owner_with_mortgage, Private_rent, Subsidized_rent,Other )
house_tenure_data<- house_tenure_data %>% gather(period, value, -c(ISO3_code, period))
colnames(house_tenure_data)<-c("ISO3_code", "Tenure_cat", 	"Tenure_share")

# by category: summarize and add
avg_ht<- house_tenure_data  %>% group_by(Tenure_cat)  %>%
  summarize(Tenure_share = mean(Tenure_share, na.rm=T)) %>%
  mutate(ISO3_code="OECD")%>%
  select(ISO3_code, Tenure_cat, Tenure_share)
# add OECD to house_tenure_data
house_tenure_data<- rbind(house_tenure_data, avg_ht)

stats_ht<- house_tenure_data  %>%
  filter( Tenure_cat == "Own_outright"| Tenure_cat == "Owner_with_mortgage") %>%
  group_by(ISO3_code)  %>%
  summarize(tot_own = sum(Tenure_share, na.rm=T))

stats_ht<- stats_ht %>% mutate(country_name=countrycode(ISO3_code,origin="iso3c",destination="country.name"))
stats_ht<-stats_ht %>% mutate(country_name=ifelse(ISO3_code=="OECD", "OECD", country_name))

min_ht<-min(stats_ht$tot_own, na.rm=T)
max_ht<-max(stats_ht$tot_own, na.rm=T)

max_ctry_ht<-stats_ht$country_name[which.max(stats_ht$tot_own)]
min_ctry_ht<-stats_ht$country_name[which.min(stats_ht$tot_own)]

avg_OECD=mean(stats_ht$tot_own, na.rm=T)
myctry_hh=stats_ht$tot_own[which(stats_ht$ISO3_code==ctry_code)]

ht_avg_OECD=mean(stats_ht$tot_own, na.rm=T)
myctry_hh=stats_ht$tot_own[which(stats_ht$ISO3_code==ctry_code)]
#################################################################################
# 2 house prices
#################################################################################
# show latest year in bar: 2019
# index to minimum year I can use for all: 2010
# show in a symbol 1995 [for countries for which we have it]
house_price_data=struc %>% select(ISO3_code, period, rhp) %>% filter(year(period)>=1990)
house_price_data<-na.omit(house_price_data)

stats_hp<-house_price_data %>%arrange(ISO3_code,period) %>%group_by(ISO3_code)  %>%
  arrange(ISO3_code,period)%>% group_by(ISO3_code)
#%>%
 # mutate(rhp_2010= ifelse(year(period)==2010, rhp,NA ))%>%
  #fill(rhp_2010) %>% fill(rhp_2010, .direction = "up")%>%
  #mutate(rhp_ind2010=rhp/rhp_2010*100)
  stats_hp<-stats_hp %>%rename(rhp_ind2010=rhp)

#keep 2020 and 2000 for who has it
stats_hp_bar<-stats_hp %>% group_by(ISO3_code) %>%
  filter( (year(period))>=2000)  %>%
  filter( year(period)==first(year(period)) |year(period)==2008 | year(period)==last(year(period) )) %>%
  select(ISO3_code, period,  rhp_ind2010) %>%
  filter(ISO3_code!="OECD")


gr_hp<-stats_hp_bar %>%arrange(ISO3_code,period) %>% group_by(ISO3_code) %>%
  mutate(before_GFC=ifelse(year(period)<=2008, 1, 0 )) %>%
  mutate(rate=((rhp_ind2010-lag(rhp_ind2010))/lag(rhp_ind2010))*100)%>%
  ungroup()%>% filter(rate!=is.na(rate)) %>% group_by(before_GFC)%>%
  mutate(OECD_av=mean(rate, na.rm =T))%>%ungroup()%>%
  filter(ISO3_code==ctry_code)
#################################################################################
# 3 rent prices
################################################################################
rent_price_data=struc %>% select(ISO3_code, period, rpi)%>% filter(year(period)>=1990)

stats_rentp<- rent_price_data %>%arrange(ISO3_code,period) %>%group_by(ISO3_code)  %>%
  arrange(ISO3_code,period)%>% group_by(ISO3_code)
#%>%
 # mutate(rpi_2010= ifelse(year(period)==2010, rpi,NA ))%>%
  #fill(rpi_2010) %>% fill(rpi_2010, .direction = "up")%>%
  #mutate(rentp_ind2010=rpi/rpi_2010*100)
stats_rentp<-stats_rentp %>%rename(rentp_ind2010=rpi)

#keep 2019 and 2000 for who has it
stats_rentp_bar<-stats_rentp %>% group_by(ISO3_code) %>%
  filter( (year(period))>=2000)  %>%
  filter( year(period)==first(year(period)) | year(period)==last(year(period) )) %>%
  select(ISO3_code, period, rentp_ind2010) %>%
  filter(ISO3_code!="OECD")

gr_rentp<-stats_rentp_bar %>%arrange(ISO3_code,period) %>% group_by(ISO3_code) %>%
  mutate(rate=((rentp_ind2010-lag(rentp_ind2010))/lag(rentp_ind2010))*100)%>%
  ungroup()%>%
  mutate(OECD_av=mean(rate, na.rm =T))%>%
  filter(ISO3_code==ctry_code)%>%
  slice(tail(row_number(), 1))
################################################################################################
# 4 mortgage claims
########################################################################################
mortgage_data <- struc %>% select("ISO3_code","period", "ECO_resilience_LMHQ")
mortgage_data <- na.omit(mortgage_data  )

mortgage_data<-mortgage_data %>% group_by(ISO3_code)  %>%
  filter( year(period)==last(year(period)))

avg_mortg<-mean(mortgage_data$ECO_resilience_LMHQ, na.rm=T)
mortgage_data_myctr<-mortgage_data%>%   filter(ISO3_code==ctry_code)
mortgage_data_myctr<-mortgage_data_myctr$ECO_resilience_LMHQ

# by category: summarize and add OECD
OECD_mrg_avg<- mortgage_data  %>% ungroup()%>%
  summarize(ECO_resilience_LMHQ = mean(ECO_resilience_LMHQ, na.rm=T)) %>%
  mutate(ISO3_code="OECD")%>%
  select(ISO3_code, ECO_resilience_LMHQ)
# add OECD to house_tenure_data
mortgage_data<- mortgage_data%>%   select(ISO3_code, ECO_resilience_LMHQ)
mortgage_data<-data.frame(mortgage_data)
OECD_mrg_avg<- data.frame(OECD_mrg_avg)

mortgage_data_fin<-rbind(mortgage_data, OECD_mrg_avg)
return(list(house_tenure_data, stats_ht, ht_avg_OECD, myctry_hh,
            stats_hp, gr_hp,
            stats_rentp,
            avg_mortg,
            mortgage_data_fin,
            mortgage_data_myctr))
}


