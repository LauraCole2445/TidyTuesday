#load in libraries
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
library(countrycode)
library(maps)
library(stringr)
library(gganimate)
library(ragg)
library(systemfonts)
library(textshaping)
library(showtext)
library(RColorBrewer)
library(extrafont)
extrafont::loadfonts(device = "win")
library(ggplot2)
library(magick)


#load in data
drought_raw<-read.csv("2021\\wk_30_droughts\\input_data.csv")

state_dict <- "https://bit.ly/2ToSrFv"
state_dict <- read.csv(state_dict)

#create dataset for map
drought<-drought_raw%>%
  clean_names()%>%
  filter(state!="PR")%>%
  mutate(valid_start=ymd(valid_start),
         none=as.numeric(as.character(gsub(",","",none))),
         d0=as.numeric(as.character(gsub(",","",d0))),
         d1=as.numeric(as.character(gsub(",","",d1))),
         d2=as.numeric(as.character(gsub(",","",d2))),
         d3=as.numeric(as.character(gsub(",","",d3))),
         d4=as.numeric(as.character(gsub(",","",d4))),
         county=as.character(county))%>%
  pivot_longer(!c(map_date,fips,county,state,valid_start,valid_end,statistic_format_id),
               names_to="drought_level",
               values_to = "total_population")%>%
  select(-county,-state,-valid_end,-statistic_format_id)%>%
  group_by(map_date,fips,valid_start)%>%
  mutate(max_cat=max(total_population))%>%
  mutate(metric = drought_level[which(total_population == max(total_population))],
         metric=case_when(metric=="none"~"None",
                          metric=="d0"~"Abnormally Dry",
                          metric=="d1"~"Moderate",
                          metric=="d2"~"Severe",
                          metric=="d3"~"Extreme",
                          metric=="d4"~"Exceptional"),
         metric = factor(metric, levels = c("None","Abnormally Dry","Moderate","Severe","Extreme","Exceptional")))%>%
  select(fips,map_date,valid_start,metric)%>%
  distinct()

drought<-drought%>%
  filter(fips==46007)%>%
  mutate(fips=46113)%>%
  rbind(drought)

#shannon fips = 46113
#bennet fips = 46007

data("county.fips")

county.fips <-county.fips %>%
  mutate(region = word(polyname, 1, sep = ","),
         subregion = word(polyname, 2, sep = ",")) %>%
  mutate(subregion = word(subregion, 1, sep = ":")) %>%
  mutate(fips = str_pad(as.character(fips), side = "left", width = 5, "0"))

map_data<-map_data("county")%>%
  left_join(county.fips)%>%
  mutate(fips=as.numeric(fips))%>%
  left_join(drought)

#create dataset for time series
drought<-drought_raw%>%
  clean_names()%>%
  filter(state!="PR")%>%
  mutate(valid_start=ymd(valid_start),
         none=as.numeric(as.character(gsub(",","",none))),
         d0=as.numeric(as.character(gsub(",","",d0))),
         d1=as.numeric(as.character(gsub(",","",d1))),
         d2=as.numeric(as.character(gsub(",","",d2))),
         d3=as.numeric(as.character(gsub(",","",d3))),
         d4=as.numeric(as.character(gsub(",","",d4))),
         county=as.character(county))%>%
  select(valid_start,none,d0,d1,d2,d3,d4)%>%
  pivot_longer(!valid_start,
               names_to="drought_level",
               values_to = "population")%>%
  group_by(valid_start)%>%
  mutate(total_population=sum(population),
         pop_per=100*population/total_population)%>%
  mutate(drought_level=case_when(drought_level=="none"~"None",
                                 drought_level=="d0"~"Abnormally Dry",
                                 drought_level=="d1"~"Moderate",
                                 drought_level=="d2"~"Severe",
                                 drought_level=="d3"~"Extreme",
                                 drought_level=="d4"~"Exceptional"),
         rank=case_when(drought_level=="None"~1,
                        drought_level=="Abnormally Dry"~2,
                        drought_level=="Moderate"~3,
                        drought_level=="Severe"~4,
                        drought_level=="Extreme"~5,
                        drought_level=="Exceptional"~6))%>%#,
  mutate(drought_level = factor(drought_level, levels = c("Exceptional","Extreme","Severe","Moderate","Abnormally Dry","None")))%>%
  group_by(valid_start,drought_level)%>%
  summarise(pop_per=sum(pop_per))

font<-"Ink Free"
text_colour<-"black"
font_size=20
n_frames<-100

map_plot<-map_data%>%
  filter(valid_start>=unique(map_data$valid_start)[n_frames])%>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=metric)) +
  borders("state")+
  coord_map() +
  scale_fill_brewer(palette = "YlOrRd")+
  theme_minimal()+
  theme(
    plot.caption=element_text(family = font,color = text_colour,size = 14),
    text = element_text(family = font,color = text_colour, size = font_size),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y= element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title=element_blank(),
    legend.text=element_text(colour=text_colour,size=16)
  )+
  xlab("")+
  ylab("")+
  labs(title = 'Drought levels in the US from August 2019 to August 2021')+
  transition_manual(valid_start)

time_series_plot<-drought%>%  
  filter(valid_start>=unique(drought$valid_start)[length(unique(drought$valid_start))-n_frames][1])%>%
  filter(drought_level!="None")%>%
  mutate(year=year(valid_start),
         month=month(valid_start))%>%
  ggplot(aes(x=valid_start, y=pop_per, fill=drought_level)) +
  geom_area()+
  scale_x_date(date_labels = "%b %y",
               date_breaks = "3 months")+
  scale_fill_brewer(palette = "YlOrRd",direction=-1)+
  theme(
    #legend
    legend.title=element_blank(),
    legend.text=element_text(colour=text_colour,size=14),
    legend.position = "none",
    
    #title and caption
    plot.caption=element_text(family = font,color = text_colour,size = 16),
    
    #text
    text = element_text(family = font,color = text_colour, size = font_size),
    
    #panel
    panel.grid.major = element_line(colour="grey"),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    
    #axes
    axis.text.x = element_text(family = font,color = text_colour,size = 14),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(family = font,color = text_colour,size = 14)
    
  )+
  xlab("")+
  ylab("")+
  labs(title = 'Percentage of US population experiencing drought levels of at least Abnormally Dry',
       caption = "Data from the US Drought Monitor website | Laura Cole | #TidyTuesday")+
  transition_reveal(valid_start)

map_animation<-animate(map_plot,
                       duration=n_frames/20,
                       fps=20,
                       height=675,
                       width=1200,
                       units="px",
                       res=90,
                       device='ragg_png',
                       end_pause=round(n_frames/10,0),
                       renderer = magick_renderer())

time_series_animation<-animate(time_series_plot,
                               duration=n_frames/20,
                               fps=20,
                               height=250,
                               width=1200,
                               units="px",
                               res=90,
                               device='ragg_png',
                               end_pause=round(n_frames/10,0),
                               renderer = magick_renderer())

i=1
final_gif <- image_append(c(map_animation[i], time_series_animation[i]),stack=TRUE)

for(i in 2:100){
  combined <- image_append(c(map_animation[i], time_series_animation[i]),stack=TRUE)
  final_gif <- c(final_gif, combined)
}

rm(map_animation,time_series_animation,combined)

image_write_gif(
  image = final_gif, 
  loop = 0,
  path = here::here("2021\\wk_30_droughts\\final_animation.gif"))
