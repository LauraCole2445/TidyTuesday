#load in libraries
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
library(countrycode)
extrafont::loadfonts(device = "win")
library(ggplot2)
library(maps)
library(stringr)
library(gganimate)
library(ragg)

library(systemfonts)
library(textshaping)
library(showtext)

#load in data
drought_raw<-read.csv("2021\\wk_30_droughts\\input_data.csv")

state_dict <- "https://bit.ly/2ToSrFv"
state_dict <- read.csv(state_dict)

#generate percentage of area with at least severe drought
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

data("county.fips")

county.fips <-county.fips %>% 
  mutate(region = word(polyname, 1, sep = ","),
         subregion = word(polyname, 2, sep = ",")) %>% 
  mutate(subregion = word(subregion, 1, sep = ":")) %>% 
  mutate(fips = str_pad(as.character(fips), side = "left", width = 5, "0"))

#shannon fips = 46113
#bennet fips = 46007

drought<-drought%>%
  filter(fips==46007)%>%
  mutate(fips=46113)%>%
  rbind(drought)

map_data<-map_data("county")%>% 
  left_join(county.fips)%>%
  mutate(fips=as.numeric(fips))%>%
  left_join(drought)

font<-"Ink Free"
text_colour<-"black"
font_size=20

n_frames<-60

a<-map_data%>%
  filter(valid_start>=unique(map_data$valid_start)[n_frames])%>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=metric)) +
  borders("state")+
  coord_map() +
  viridis::scale_fill_viridis(discrete = TRUE,option = "B")+
  theme_minimal()+
  geom_text(aes(x=-72,y=27,label=year(valid_start)),family=font,size=7,colour=text_colour)+
  geom_text(aes(x=-105,y=51,label="Drought levels in the US from 2019-2021"),family=font,size=7,colour=text_colour)+
  theme(
    plot.caption=element_text(family = font,color = text_colour, size = 10),
    plot.title=element_blank(),
    text = element_text(family = font,color = text_colour, size = font_size),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y= element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title=element_blank(),
    legend.text=element_text(colour=text_colour,size=10)
  )+
  xlab("")+
  ylab("")+
  labs(caption = "Data from the US Drought Monitor website | Laura Cole | #TidyTuesday")+
  gganimate::transition_manual(valid_start)

# animate(a,
#         duration=n_frames/10,
#         fps=5,
#         height=675,
#         width=1200,
#         units="px",
#         res=120,
#         device='ragg_png')\

animate(a,
        duration=n_frames/4,
        fps=25,
        height=675,
        width=1200,
        units="px",
        res=120,
        device='ragg_png')

anim_save("\\2021\\wk_30_droughts\\animation_1.gif",
          animation = last_animation(),
          path = getwd(),
          renderer = gifski_renderer(loop = FALSE))

