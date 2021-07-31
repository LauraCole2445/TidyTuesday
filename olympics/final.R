library(tidyverse)
library(janitor)
library(dslabs)
library(png)
library(cowplot)
# library(gapminder)

data(gapminder)

# tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
# olympics_raw <- tuesdata$olympics
# regions<-tuesdata$regions

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
  mutate(medal = factor(medal, levels = c("Bronze","Silver","Gold")))#%>%
  # mutate(year=as.factor(year))

small<-data_for_plot%>%select(year,country,ratio)%>%distinct()

logo <- readPNG("..\\images\\olympics_logo.png",native=TRUE)

plot<-ggplot()+
  geom_bar(data=data_for_plot,aes(x=year,y=number_of_medals,fill=medal),position="stack", stat="identity") +
  geom_line(data=small,mapping = aes(x = year, y = ratio*300), size = 0.5)+
  facet_wrap(~country)+
  scale_y_continuous(name = "Number of medals won",
                     sec.axis = sec_axis(~./300, name = "Ratio of medals to number of entries")) +
  scale_x_continuous(breaks=seq(2000,2016,by=4))+
  scale_fill_manual(values=c("Bronze"="#CD7F32", "Silver"="#C0C0C0", "Gold"="#D4AF37"))+
  theme(
    text = element_text(family = font, color = text_colour, size = 16),
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
  # inset_element(logo, 0.95, 0.95, 1.0, 1.0)+
  labs(title = 'Number of medals won at the summer olympic games between 2000 and 2016*',
       subtitle = "and the ratio of medals won vs the number of events entered",
       caption="*For the coutries with the top 12 highest GDP in 2008 | Laura Cole | #TidyTuesday")

#this is a test


cow_final<-cowplot::ggdraw(plot) +
  cowplot::draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)+
  cowplot::draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)
  cowplot::draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)
  cowplot::draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)
  cowplot::draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)
  cowplot::draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)
  cowplot::draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)
  cowplot::draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)
  cowplot::draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)
  cowplot::draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)
  cowplot::draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)
  cowplot::draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)
  cowplot::draw_image(logo,x = 0.1, y = 0.1, width = 0.1,hjust = 0, vjust = 1, halign = 1, valign = 1)


save_plot("test.png", cow_final, base_height = 7, base_width = 12.5)

