#libraries
library(dplyr)
library(ggplot2)
library(countrycode)
library(ggflags)
library(forcats)

#read data
# tuesdata <- tidytuesdayR::tt_load(2021, week = 32)
athletes_raw <- tuesdata$athletes

athletes_raw%>%
  ggplot()+
  geom_bar(aes(year))

athletes<-athletes_raw%>%
  mutate(abb=case_when(abb=="DEN"~"DNK",
                       abb=="NED"~"NLD",
                       abb=="GER"~"DEU",
                       abb=="RSA"~"ZAF",
                       abb=="CRO"~"HRV",
                       abb=="IRI"~"IRN",
                       abb=="SUI"~"CHE",
                       abb=="FRG"~"DEU",
                       abb=="ALG"~"DZA",
                       abb=="POR"~"PRT",
                       abb=="BUL"~"BGR",
                       abb=="GRE"~"GRC",
                       abb=="UAE"~"ARE",
                       abb=="MAS"~"MYS",
                       abb=="KSA"~"SAU",
                       abb=="LAT"~"LVA",
                       abb=="NGR"~"NGA",
                       TRUE~abb))%>%
  mutate(iso=countrycode(abb, 'iso3c', 'country.name'))%>%
  mutate(continent=countrycode(abb, 'iso3c', 'continent'))%>%
  filter(!is.na(abb))%>%
  filter(!is.na(iso))


athletes%>%
  select(gender,type,event,year)%>%
  distinct()%>%
  ggplot()+
  geom_bar(aes(year))+
  facet_wrap(~type)
  

#number of distinct events by gender, type, event and year
athletes%>%
  group_by(gender,type,event,year)%>%
  count()%>%
  ggplot()+
  geom_bar(aes(year))+
  facet_wrap(~type)

#count total number of medals by type
total_medals<-athletes%>%
  filter(gender=="Men")%>%
  select(type)%>%
  group_by(type)%>%
  summarise(total_medals=n())%>%
  arrange(-total_medals)%>%
  mutate(rank=row_number())
  

athletes%>%
  filter(gender=="Men")%>%
  group_by(year,type)%>%
  count(abb)%>%
  filter(n==max(n))%>%
  rename(number_of_medals=n)%>%
  mutate(year=as.factor(year))%>%
  left_join(.,total_medals,by="type")%>%
  ungroup()%>%
  mutate(type = fct_reorder(type, -rank))%>%
  ggplot(aes(x=year,y=type))+
  geom_point(aes(size = number_of_medals, fill = abb), alpha = 0.75, shape = 21)+
  scale_size_continuous(range = c(4,10))


