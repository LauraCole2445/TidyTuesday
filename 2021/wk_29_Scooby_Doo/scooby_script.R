#who caught the most monsters...

# Setup----
library(dplyr)
library(ggplot2)
library(gganimate)
library(tidyr)
library(extrafont)
library(directlabels)
library(png)

#source functions script
source("functions.R")

#load in images
mystery_machine<-readPNG("images\\mystery_machine.png")
scooby_doo_title<-readPNG("images\\scooby_doo_title.png")

#set font and text colour
font<-"Scooby Doo"
text_colour<-"#E8AA57"

# Read data----
tuesdata <- tidytuesdayR::tt_load(2021, week = 29)
scoobydoo <- tuesdata$scoobydoo

scooby_gather_unmasked<-scoobydoo %>%
  mutate(date_aired=lubridate::ymd(date_aired))%>%
  arrange(date_aired)%>%
  separate("date_aired", into = c("year", "month", "day"), remove = FALSE)%>%
  mutate(year=as.numeric(year))%>%
  mutate(Fred=cumsum(case_when(unmask_fred==TRUE~1,TRUE~0)),
         Daphnie=cumsum(case_when(unmask_daphnie==TRUE~1,TRUE~0)),
         Velma=cumsum(case_when(unmask_velma==TRUE~1,TRUE~0)),
         Shaggy=cumsum(case_when(unmask_shaggy==TRUE~1,TRUE~0)),
         Scooby=cumsum(case_when(unmask_scooby==TRUE~1,TRUE~0)))%>%
  select(year,Fred,Daphnie,Velma,Shaggy,Scooby)%>%
  group_by(year)%>%
  filter(row_number()==n())%>%
  arrange(year)%>%
  gather("character","cumulative_sum",2:6)%>%
  mutate(class="unmasked")

scooby_gather_caught<-scoobydoo %>%
  mutate(date_aired=lubridate::ymd(date_aired))%>%
  arrange(date_aired)%>%
  separate("date_aired", into = c("year", "month", "day"), remove = FALSE)%>%
  mutate(year=as.numeric(year))%>%
  mutate(Fred=cumsum(case_when(caught_fred==TRUE~1,TRUE~0)),
         Daphnie=cumsum(case_when(caught_daphnie==TRUE~1,TRUE~0)),
         Velma=cumsum(case_when(caught_velma==TRUE~1,TRUE~0)),
         Shaggy=cumsum(case_when(caught_shaggy==TRUE~1,TRUE~0)),
         Scooby=cumsum(case_when(caught_scooby==TRUE~1,TRUE~0)))%>%
  select(year,Fred,Daphnie,Velma,Shaggy,Scooby)%>%
  group_by(year)%>%
  filter(row_number()==n())%>%
  arrange(year)%>%
  gather("character","cumulative_sum",2:6)%>%
  mutate(class="caught")

scooby_gathered<-rbind(scooby_gather_caught,scooby_gather_unmasked)

factor<-0.75

scooby_gathered<-scooby_gathered%>%
  arrange(year)%>%
  mutate(scooby_colour=case_when(
                          character=="Velma" & class=="caught"~"#E8AA57",
                          character=="Scooby" & class=="caught" ~"#856842",
                          character=="Shaggy"& class=="caught" ~"#98BF6D",
                          character=="Fred"& class=="caught" ~"#2d96d7",
                          character=="Daphnie"& class=="caught" ~"#6C4287",
                          character=="Velma" & class=="unmasked"~lighten("#E8AA57",factor),
                          character=="Scooby" & class=="unmasked" ~lighten("#856842",factor),
                          character=="Shaggy"& class=="unmasked" ~lighten("#98BF6D",factor),
                          character=="Fred"& class=="unmasked" ~lighten("#2d96d7",factor),
                          character=="Daphnie"& class=="unmasked" ~lighten("#6C4287",factor)))%>%
  ungroup()




p<-scooby_gathered%>%
  ggplot(mapping = aes(x = ifelse(test = class == "caught", yes = -cumulative_sum, no = cumulative_sum),
                     y = character, fill = class)) +
  geom_col(aes(group = class),fill=scooby_gathered$scooby_colour)+
  annotation_raster(mystery_machine, ymin = 0.7, ymax=4.7, xmin = 200, xmax=500) +
  annotation_raster(scooby_doo_title, ymin = 4.0, ymax=5.5, xmin = 200, xmax=500) +
  theme(
        plot.caption=element_text(face="bold"),
        plot.title=element_text(face="bold"),
        text = element_text(family = font,color = text_colour, size = 20),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black", colour = "black"),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "black", colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y= element_text(face="bold",color=text_colour,size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        )+
  xlim(c(-200,500))+
  labs(x="",y="")+
  annotate("text",label="Caught by",x = -120,y = 0.9,family = font,size = 7,colour= text_colour)+
  annotate("text",label="the monster",x = -120,y = 0.6,family = font,size = 7,colour= text_colour)+
  annotate("text",label="Unmasked",x = 120,y = 0.9,family = font,size = 7,colour= text_colour)+
  annotate("text",label="the monster",x = 120,y = 0.6,family = font,size = 7,colour= text_colour)+
  labs(title = "Who's the super sleuth and who gets caught?",caption = "Laura Cole| #TidyTuesday")+
  geom_text(aes(x=350,y=1,label=as.character(round(year,0))),family=font,size=14,colour= text_colour)+
  gganimate::transition_time(as.integer(year))+
  ease_aes("cubic-in-out")+
  enter_grow() +
  exit_fade()

n_frames<-1*length(unique(scooby_gathered$year))

gganimate::animate(p,
                   nframes=n_frames,
                   fps=n_frames/18,
                   height=675,
                   width=1200,
                   units="px",
                   res=120,
                   end_pause=round(n_frames/2,0)
                   )

anim_save("\\2021\\wk_29_Scooby_Doo\\scooby_caught_v_unmasked_test.gif",
          animation = last_animation(),
          path = getwd(),
          renderer = gifski_renderer(loop = FALSE))
