#libraries
library(dplyr)
library(ggplot2)
library(countrycode)
library(ggflags)
library(forcats)
library(png)
library(cowplot)

#set up parameters and read in images
font<-"Ink Free"
text_colour<-"black"
font_size<-12
logo <- readPNG("images\\paralympics_logo.png",native=TRUE)

#read data
tuesdata <- tidytuesdayR::tt_load(2021, week = 32)
athletes_raw <- tuesdata$athletes

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

#plot to show how many medals have been awarded for each type
iso_want<-athletes%>%
  select(year,medal,iso)%>%
  group_by(iso)%>%
  summarise(total_medals=n())%>%
  arrange(-total_medals)%>%
  slice(1:12)%>%
  mutate(rank=row_number(),
         iso = fct_reorder(iso, rank))%>%
  ungroup()%>%
  select(iso,rank)

p1<-athletes%>%
  left_join(.,iso_want,by="iso")%>%
  filter(!is.na(rank))%>%
  mutate(iso = fct_reorder(iso, rank),
         medal = factor(medal, levels = c("Gold","Silver","Bronze")))%>%
  select(gender,year,medal,iso)%>%
  group_by(gender,year,medal,iso)%>%
  summarise(number_of_medals=n())%>%
  ggplot()+
  geom_bar(aes(x=year,y=number_of_medals,fill=medal),position="stack", stat="identity")+
  facet_wrap(~iso)+
  scale_x_continuous(breaks = athletes %>%filter(year %% 8 == 0)%>%pull(year)%>%unique()) +
  scale_y_continuous(name = "Number of medals won") +
  scale_fill_manual(values=c("Gold"="#D4AF37","Silver"="#C0C0C0","Bronze"="#CD7F32"))+
  theme(
    text = element_text(family = font, color = text_colour, size = font_size),
    plot.title = element_text(family = font, lineheight = 0.9, hjust=0, size = font_size+2),
    plot.caption=element_text(family = font),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = "white", colour = "white"),
    legend.position = "bottom",
    axis.text.x =  element_text (angle = 45, hjust=1),
    axis.ticks.x = element_blank(),
    axis.text.y =  element_text (margin = unit(c(5, 5, 5, 5), "mm")),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("Number of medals")+
  labs(title = 'The countries with the most number of medals',
       subtitle= 'at the paralympic games from 1980 to 2016*',
       caption="*for the 12 countries that have won the most medals")


#count total number of medals by type
total_medals<-athletes%>%
  select(type)%>%
  group_by(type)%>%
  summarise(total_medals=n())%>%
  arrange(-total_medals)%>%
  mutate(rank=row_number())

p2<-athletes%>%
  group_by(year,type)%>%
  count(abb)%>%
  filter(n==max(n))%>%
  rename(number_of_medals=n)%>%
  mutate(year=as.factor(year))%>%
  left_join(.,total_medals,by="type")%>%
  ungroup()%>%
  mutate(type = fct_reorder(type, -rank),
         iso2=tolower(countrycode(abb, "iso3c", "iso2c")))%>%
  ggplot(aes(x=year,y=type))+
  geom_point(shape=21,colour="black",size=11.5)+
  geom_flag(aes(country = iso2,size=9.5)) +
  scale_size_continuous(range = c(9.5,9.5))+
  theme(
    text = element_text(family = font, color = text_colour, size = font_size),
    plot.title = element_text(family = font, lineheight = 0.9, hjust=0, size = font_size+2),
    plot.caption=element_text(family = font),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = "white", colour = "white"),
    legend.position = "none",
    axis.text.x =  element_text (angle = 45, hjust=1),
    axis.ticks.x = element_blank(),
    axis.text.y =  element_text (margin = unit(c(5, 5, 5, 5), "mm")),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")+
  labs(title = 'The country with the most number of medals',
       subtitle= 'per event at the paralympic games from 1980 to 2016**',
       caption="**events shown in order from most medals awarded to least | Laura Cole | #TidyTuesday")

final_plot<-plot_grid(p1, p2)+
  draw_image(logo,x = 0.0, y = 0.13, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)

save_plot("wk_32_paralympics\\paralympics_medals.png", final_plot, base_height = 7, base_width = 12.5)
