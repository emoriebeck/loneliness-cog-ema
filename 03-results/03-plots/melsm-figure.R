
# library(graphicalVAR)
# library(mgm)
# library(qgraph)
library(patchwork)
library(knitr)
library(kableExtra)
library(cowplot)
library(plyr)
library(tidyverse)

# Example time series -----------------------------------------------------

pomp <- function(x, mini = 1, maxi = 5) (x - mini)/(maxi - mini)*100

# POMP based on observed max & min
pomp_obs <- function(x) {
  y <- (x - min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))*100
  y[is.nan(y)] <- NA
  return(y)
}

my_theme <- function(){
  theme_bw() +
    theme(
      legend.position = "bottom"
      , legend.title = element_text(face = "bold", size = rel(1))
      , legend.text = element_text(face = "italic", size = rel(1))
      , axis.text = element_text(face = "bold", size = rel(1.1), color = "black")
      , axis.title = element_text(face = "bold", size = rel(1.2))
      , plot.title = element_text(face = "bold", size = rel(1.2), hjust = .5)
      , plot.subtitle = element_text(face = "italic", size = rel(1.2), hjust = .5)
      , strip.text = element_text(face = "bold", size = rel(1.1), color = "white")
      , strip.background = element_rect(fill = "black")
    )
}

set.seed(6)
ts4 <- tibble(
  ID = 4
  , stability = "High Instability"
  , loneliness = "High Trait-Like Loneliness"
  , t = 1:100
  # , value = pomp(as.numeric(ceiling(arima.sim(n = 100, list(order = c(1,0,0), ar = 0.00001),
  #                                        sd = sqrt(.2)) + 4)))
  , value = pomp_obs(as.numeric(arima.sim(n = 100, list(order = c(1,0,0), ar = 0.00001),
                                          sd = sqrt(.2))))
) 
p1 <- ts4 %>% 
  ggplot(aes(x = t, y = value)) + 
  geom_line(color = "goldenrod3", size = .75) + 
  geom_point(size = .5) +
  geom_hline(aes(yintercept = mean(value)), linetype = "dashed") + 
  annotate(
    "label"
    , label = "Average Cognitive\nFunction (Location)"
    , x = 80, y = mean(ts4$value), hjust = 0
  ) +
  annotate(
    "segment"
    , arrow = arrow(type = "closed", length=unit(2, "mm"))
    , size = 1, x = 34, xend = 26.5, y = 90, yend = 95
  ) +
  annotate(
    "segment"
    , arrow = arrow(type = "closed", length=unit(2, "mm"))
    , size = 1, x = 40, xend = 36.5, y = 86, yend = 79
  ) +
  annotate(
    "label"
    , label = "Cognitive\nFluctuations"
    , x = 40, y = 90, hjust = .5
  ) +
  scale_x_continuous(limits = c(0, 100)) + 
  # scale_y_continuous(limits = c(1,5), breaks = seq(1,5,1)) + 
  labs(x = "EMA Beep"
       , y = "Value"
  ) + 
  my_theme() + 
  # theme(
  #   axis.text = element_text(face = "bold", size = rel(1.1))
  #   , axis.title = element_text(face = "bold", size = rel(1.1))
  #   , plot.title = element_text(face = "bold", size = rel(1.2), hjust = .5)
  # )
  theme(
    axis.text = element_blank()
    , axis.title.y = element_blank()
    , axis.title.x = element_text(face = "bold")
    , axis.ticks = element_blank()
    , axis.line = element_blank()
    # , axis.line = element_line(linewidth = 1.2)
    , legend.position = "none"
    , panel.background = element_rect(color = "black",linewidth = 1.2)
    , strip.background = element_rect(color = NA)
    , strip.text = element_text(face = "bold", size = rel(1.2))
  )
p1

p2 <- ts4 %>% 
  ggplot(aes(y = value)) + 
  geom_density(fill = "goldenrod3", alpha = .6) + 
  annotate(
    "label"
    , label = "Cognitive Variability (Scale)",
    y = mean(ts4$value), x = .003
    , angle = 270
    , fill = NA
  ) + 
  theme_void() 

p1 + p2  + 
  plot_layout(widths = c(8,2)) + 
  plot_annotation(
    title = "Dynamic Assessments of Cognitive Function in Daily Life"
    , theme = my_theme()
  )
ggsave(file = "~/Downloads/fig-2-cog-var.png", width = 9, height = 5)
