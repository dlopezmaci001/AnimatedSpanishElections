library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)


gdp_tidy1 <- read.csv2("YOUR PATH",dec=".")

gdp_formatted3 <- gdp_tidy1 %>%
  group_by(mes) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-Escaños)) %>% 
  group_by(Partido) %>% 
  ungroup()


staticplot2 = ggplot(gdp_formatted3, aes(rank, group = Partido, 
                                        fill = as.factor(Partido)
                                        , color = as.factor(Partido))) +
  geom_tile(aes(y = Escaños/2,
                height = Escaños
                ,width = 0.5
                )
                , alpha = 1, color = NA) +
  geom_text(aes(y = 0, label = paste(Partido, " ")),size=14, vjust = 0.5, hjust = 1) +
  geom_text(aes(y=Escaños,label = Escaños, hjust=0),size=14) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=34, hjust=0, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=30, hjust=0, color="dark grey"),
        plot.caption =element_text(size=28, hjust=0, color="dark blue"),
        plot.background=element_blank()
        ,plot.margin = margin(2.5,1.5, 2.5, 8, "cm")
)

d <- staticplot2 +  
  scale_fill_manual(values=c("darkorange1", "darkorchid","blue1","red","chartreuse4")) +
  scale_color_manual(values=c("darkorange1", "darkorchid","blue1","red","chartreuse4"))

anim = d + transition_states(mes, transition_length = 1, state_length = 1) +
  view_follow(fixed_x = TRUE)   +
  labs(title = 'Porcentaje de intención voto por partido : {closest_state}',  
       subtitle  =  "Principales partidos en España",
       caption  = "© A Nimerya Data Science Business Case")

animate(anim, 200, fps = 8,  width = 1800, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))
