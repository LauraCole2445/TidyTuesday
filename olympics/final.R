library(tidyverse)
library(janitor)
library(dslabs)
# library(gapminder)

data(gapminder)

tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
olympics_raw <- tuesdata$olympics
regions<-tuesdata$regions

regions <- regions %>% 
  clean_names()

olympics <- olympics_raw %>% 
  left_join(regions)%>%
  filter(year>=1998,
         season=="Summer")

medals <- olympics %>%
  count(year, region, season, medal) %>%
  filter(!is.na(medal)) %>%
  rename("number_of_medals" = n) %>% #this gives breakdown by gold, silver, bronze
  group_by(year, season, region) %>%
  mutate(medal_total = sum(number_of_medals)) %>% #this gives total medals
  ungroup()

entries <- olympics %>% 
  count(year, season, region, sex, age, name) %>% 
  count(year, season, region) %>% 
  rename("number_of_entries" = n)

medals<-left_join(entries,medals,by=c("year","season","region"))%>%
  rename("country"=region)

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

font<-"Ink Free"
text_colour<-"black"#E8AA57"
point_colour<-"red"

country_list<-countries_want%>%select(country)%>%pull()

# Helper to insert whitespace
spacer <- function(size = 1, n = 1) glue::glue("<span style = 'font-size: {size}pt'>{strrep(' ', n)}</span>")

data_for_plot<-left_join(medals,countries_want,by="country")%>%
  mutate(ratio=medal_total/number_of_entries)%>%
  filter(!is.na(gdp_rank))%>%
  mutate(country = fct_reorder(country, gdp_rank,median))%>%
  mutate(medal = factor(medal, levels = c("Bronze","Silver","Gold")))%>%
  mutate(label = fct_inorder(glue::glue(
    "<span style = 'font-size: 11pt; font-family: \"Apple Color Emoji\"'></span>",
    "{spacer(3)}**{country}**{spacer(3)}",
    "<span style = 'font-size: 11pt; font-family: \"Apple Color Emoji\"'></span>"
  )))

small<-data_for_plot%>%select(year,country,ratio)%>%distinct()

ggplot()+
  geom_bar(data=data_for_plot,aes(x=year,y=number_of_medals,fill=medal),position="stack", stat="identity") +
  geom_line(data=small,mapping = aes(x = year, y = ratio*300), size = 2)+
  facet_wrap(~country)+
  scale_y_continuous(name = "Number of medals won",
                     sec.axis = sec_axis(~./300, name = "Ratio of wins to entries")) +
  # theme(
  #   axis.title.y = element_text(color = "grey"),
  #   axis.title.y.right = element_text(color = "blue"))+
  scale_fill_manual(values=c("Bronze"="#CD7F32", "Silver"="#C0C0C0", "Gold"="#D4AF37"))+
  theme(
    plot.caption=element_text(face="bold"),
    plot.title=element_text(face="bold"),
    text = element_text(family = font,color = text_colour, size = 14),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = "white", colour = "white"),
    axis.text.x =  element_text (angle = 45),
    axis.ticks.x = element_blank(),
    # axis.text.y= element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()#,
    # legend.position = "none"
  )+
  xlab("Country")+
  ylab("Number of medals")+
  facet_wrap(~country)
