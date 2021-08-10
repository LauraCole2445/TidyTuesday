#setup----
library(tidyverse)
library(janitor)
library(dslabs)
library(png)
library(cowplot)
library(extrafont)
library(ggflags)
library(countrycode)
library(ggimage)

logo <- readPNG("images\\olympics_logo.png",native=TRUE)
start_year<-1992
font<-"Scooby Doo"
text_colour<-"black"

#read in data----
# tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
olympics_raw <- tuesdata$olympics
regions<-tuesdata$regions

data(gapminder)

regions <- regions %>% 
  clean_names()

olympics <- olympics_raw %>% 
  left_join(regions)%>%
  filter(year>=start_year,
         season=="Summer")

medals <- olympics %>%
  count(year, region, season, medal) %>%
  filter(!is.na(medal)) %>%
  rename("number_of_medals" = n) %>% #this gives breakdown by gold, silver, bronze
  group_by(year, season, region) %>%
  mutate(medal_total = sum(number_of_medals)) %>% #this gives total medals
  ungroup()

entries <- olympics %>% 
  count(year, season, region) %>% 
  rename("number_of_entries" = n)

medals<-left_join(entries,medals,by=c("year","season","region"))%>%
  rename("country"=region)%>%
  mutate(iso2=tolower(countrycode(country, "country.name", "iso2c")))

#get gdp data...
countries_want<-gapminder%>%
  select(country,continent,year,gdp)%>%
  mutate(country=as.character(country))%>%
  mutate(country=case_when(country=="United States"~"USA",
                           country=="United Kingdom"~"UK",
                           TRUE~country))%>%
  filter(!is.na(gdp))%>%
  arrange(-gdp)%>%
  filter(year==max(year))%>%
  slice(1:12)%>%
  select(country)%>%
  mutate(gdp_rank=row_number())

country_list<-countries_want%>%
  select(country)%>%
  pull()

data_for_plot<-left_join(medals,countries_want,by="country")%>%
  mutate(ratio=medal_total/number_of_entries)%>%
  filter(!is.na(gdp_rank))%>%
  mutate(country = fct_reorder(country, gdp_rank,median))%>%
  mutate(medal = factor(medal, levels = c("Bronze","Silver","Gold")))

plot<-data_for_plot%>%
  ggplot(aes(country=iso2))+
  geom_bar(aes(x=year,y=number_of_medals,fill=medal),position="stack", stat="identity") +
  geom_line(aes(x = year, y = ratio*300), size = 0.5)+
  ggflags::geom_flag(aes(x=start_year,y=290), size=9)+
  scale_y_continuous(name = "Number of medals won",
                     sec.axis = sec_axis(~./300, name = "Ratio of medals to number of entries")) +
  scale_x_continuous(breaks=seq(start_year,2016,by=4))+
  scale_fill_manual(values=c("Bronze"="#CD7F32", "Silver"="#C0C0C0", "Gold"="#D4AF37"))+
  theme(
    text = element_text(family = font, color = text_colour, size = 14),
    plot.title = element_text(family = font, lineheight = 0.9, hjust=0, size = 20),
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
  facet_wrap(~country)+
  labs(title = 'Number of medals won at the summer olympic games between 1992 and 2016* (bars)',
       subtitle = "and the ratio of medals won vs the number of events entered (lines)",
       caption="*For the coutries with the 12 highest GDP in 2011 | Laura Cole | #TidyTuesday")

plot<-ggdraw(plot) +
  draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)

save_plot("wk_31_olympics\\olympics_gdp_v_medals.png", plot, base_height = 7, base_width = 12.5)

