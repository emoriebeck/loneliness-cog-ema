
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

# high loneliness, high stability
set.seed(7)
ts1 <- tibble(
  ID = 1 
  , t = 1:100
  , stability = "Low Instability"
  , loneliness = "High Trait-Like Loneliness"
  , value = pomp(round(arima.sim(n = 100, list(ar = c(1, -0.99), ma = c(-0.99, 0.99)),
                              sd = sqrt(.1))))
  # , value = pomp(arima.sim(n = 100, list(ar = c(1, -0.99), ma = c(-0.99, 0.99)),
  #                                sd = sqrt(.1)))
) 
ts1 %>%
  ggplot(aes(x = t, y = value)) + 
  geom_line(color = "seagreen4", size = .75) + 
  geom_point(size = .5) +
  # scale_y_continuous(limits = c(1,5), breaks = seq(1,5,1)) + 
  labs(x = "Time Point (1-100)"
       , y = "Value"
       , title = "Sample Time Series Plot") + 
  theme_classic() + 
  theme(
    axis.text = element_text(face = "bold", size = rel(1.1))
    , axis.title = element_text(face = "bold", size = rel(1.1))
    , plot.title = element_text(face = "bold", size = rel(1.2), hjust = .5)
  )

# low loneliness, high stability
set.seed(6)
ts2 <- tibble(
  ID = 2
  , t = 1:100
  , stability = "Low Instability"
  , loneliness = "Low Trait-Like Loneliness"
  , value = pomp(round(arima.sim(n = 100, list(ar = c(1, -.99), ma = c(-0.9, .9)),
                              sd = sqrt(.1)) + 1))
) 
ts2 %>%
  ggplot(aes(x = t, y = value)) + 
  geom_line(color = "seagreen4", size = .75) + 
  geom_point(size = .5) +
  scale_y_continuous(limits = c(0,100), breaks = seq(1,5,1)) +
  labs(x = "Time Point (1-100)"
       , y = "Value"
       , title = "Sample Time Series Plot") + 
  theme_classic() + 
  theme(
    axis.text = element_text(face = "bold", size = rel(1.1))
    , axis.title = element_text(face = "bold", size = rel(1.1))
    , plot.title = element_text(face = "bold", size = rel(1.2), hjust = .5)
  )

# low loneliness, low stability
set.seed(6)
ts3 <- tibble(
  ID = 3
  , t = 1:100
  , stability = "High Instability"
  , loneliness = "Low Trait-Like Loneliness"
  , value = pomp(as.numeric(ceiling(arima.sim(n = 100, list(order = c(1,0,0), ar = 0.00001),
                              sd = sqrt(.1)) + 1)))
) 
ts3 %>%
  ggplot(aes(x = t, y = value)) + 
  geom_line(color = "seagreen4", size = .75) + 
  geom_point(size = .5) +
  # scale_y_continuous(limits = c(1,5), breaks = seq(1,5,1)) + 
  labs(x = "Time Point (1-100)"
       , y = "Value"
       , title = "Sample Time Series Plot") + 
  theme_classic() + 
  theme(
    axis.text = element_text(face = "bold", size = rel(1.1))
    , axis.title = element_text(face = "bold", size = rel(1.1))
    , plot.title = element_text(face = "bold", size = rel(1.2), hjust = .5)
  )

# high loneliness, low stability
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
  labs(x = "EMA Beeps Across Time"
       , y = "Cognitive Scores/RTs"
  ) + 
  my_theme() + 
  # theme(
  #   axis.text = element_text(face = "bold", size = rel(1.1))
  #   , axis.title = element_text(face = "bold", size = rel(1.1))
  #   , plot.title = element_text(face = "bold", size = rel(1.2), hjust = .5)
  # )
  theme(
    axis.text = element_blank()
    # , axis.title.y = element_blank()
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
ggsave(file = "~/Downloads/cog-var.png", width = 9, height = 5)

ts1 %>%
  full_join(ts2) %>%
  full_join(ts3) %>%
  full_join(ts4) %>%
  mutate(comb = paste(loneliness, stability, sep = "\n")) %>%
  ggplot(aes(x = t, y = value)) + 
    geom_line(
      aes(color = loneliness)
      , size = .5
      ) + 
  geom_point(size = .5) + 
  scale_color_manual(values = c("black", "gray60")) + 
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,25)) + 
  scale_x_continuous(limits = c(1,20)) + 
  labs(
    x = "Time Point (1-100)"
    , y = "Momentary Loneliness"
  ) +
  facet_wrap(~comb) + 
  my_theme() + 
  theme(
    legend.position = "none"
    , strip.background = element_rect(fill = "grey90")
    , strip.text = element_text(color = "black")
    )
ggsave(file = "~/Downloads/ex_ts_rq1.png", width = 5, height = 3)

ts1 %>%
  full_join(ts2) %>%
  full_join(ts3) %>%
  full_join(ts4) %>%
  mutate(comb = paste(loneliness, stability, sep = "\n")) %>%
  filter(loneliness == "High Trait-Like Loneliness") %>%
  ggplot(aes(x = t, y = value)) + 
  geom_line(
    color = "#9AA0F9"
    , size = .5
  ) + 
  geom_point(size = .5) + 
  # scale_color_manual(values = c("black", "gray60")) + 
  # scale_y_continuous(limits = c(0,100), breaks = seq(0,100,25)) + 
  scale_x_continuous(limits = c(1,20)) + 
  labs(
    x = "EMA Beeps"
    , y = "Momentary Loneliness"
  ) +
  facet_wrap(~stability, nrow = 2) + 
  theme_classic() + 
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
  # my_theme() + 
  # theme(
  #   legend.position = "none"
  #   , strip.background = element_rect(fill = "grey90")
  #   , strip.text = element_text(color = "black")
  # )
ggsave(file = "~/Downloads/ex_ts_rq1.png", width = 2.5, height = 3)

set.seed(61)
tibble(
  var = c("A", "B", "C", "cog")
  , m = c(1.5, 3.3, 7, 5)
  , sd = c(1.2, .3, 1.5, .8)
  , y = map2(m, sd, ~tibble(time = 1:10, y = rnorm(10, mean = .x, sd = .y)))
) %>%
  unnest(y) %>%
  filter(var %in% c("A", "cog")) %>%
  ggplot(aes(x = time, y = y, group = var)) + 
    scale_color_manual(values = c("#9AA0F9", "goldenrod3")) + 
    scale_y_continuous(limits = c(-1, 10)) + 
    geom_line(aes(color = var), size = 1) + 
    geom_line(aes(color = var)) + 
    labs(x = "Time") + 
    theme_classic() + 
    theme(axis.text = element_blank()
          , axis.title.y = element_blank()
          , axis.title.x = element_text(face = "bold", size = rel(1.2))
          , legend.position = "none"
          , axis.ticks = element_blank()
          , axis.line = element_blank()# element_line(linewidth = 1.2)
          , panel.background = element_rect(color = "black",linewidth = 1.2)
          # , axis.line = element_blank()
          )
ggsave(file = "~/Downloads/ex_ts_3.png", width = 2.7, height = 2.7)

tibble(
  A = c(2, 2, 3, 3, 2, 3, 5, 5, 2, 2)
  , C = c(2, 1, 3, 2, 2, 2, 0, 0, 2, 2) + 7
  , time = 1:10
) %>%
  pivot_longer(
    cols = -time
    , names_to = "var"
    , values_to = "y"
  ) %>%
  ggplot(aes(x = time, y = y, group = var)) + 
  scale_color_manual(
    values = c("#9AA0F9", "goldenrod3")
    ) + 
  scale_y_continuous(limits = c(-1, 10)) + 
  annotate(
    "rect"
    , xmin = 6, xmax = 9
    , ymin = 1, ymax = Inf
    , fill = "goldenrod1"
    , alpha = .1, color = "goldenrod3"
    ) + 
  geom_line(aes(color = var), size = 1) + 
  geom_line(aes(color = var)) + 
  labs(x = "EMA Beeps") + 
  theme_classic() + 
  theme(axis.text = element_blank()
        , axis.title.y = element_blank()
        , axis.title.x = element_text(face = "bold", size = rel(1.2))
        , legend.position = "none"
        , axis.ticks = element_blank()
        , panel.background = element_rect(color = "black",linewidth = 1.2)
        # , axis.line = element_line(linewidth = 1.2)
        , axis.line = element_blank()
  )
ggsave(file = "~/Downloads/ex_ts_4.png", width = 2.7, height = 2.7)

# set.seed(78)
# tibble(
#   var = c("A", "B", "C")
#   , m = c(1.5, 3.3, 7)
#   , sd = c(.8, .3, 2)
#   , y = map2(m, sd, ~tibble(time = 1:10, y = rnorm(10, mean = .x, sd = .y)))
# ) %>%
#   unnest(y) %>%
#   ggplot(aes(x = time, y = y, group = var)) + 
#   scale_color_manual(values = c("blue", "red3", "goldenrod2")) + 
#   scale_y_continuous(limits = c(-1, 11)) + 
#   geom_line(aes(color = var), size = 1) + 
#   theme_classic() + 
#   theme(axis.text = element_blank()
#         , axis.title = element_blank()
#         , legend.position = "none"
#         , axis.ticks = element_blank()
#         # , axis.line.x = element_line(linewidth = 1.5)
#         , axis.line = element_blank())
# ggsave(file = "~/Downloads/ex_ts.png", width = 2.7, height = 2)

# 
# # Aim 3 -------------------------------------------------------------------
# 
# gvar_sim_fun <- function(pk, pb){
#   Mod <- randomGVARmodel(7,probKappaEdge = pk,probBetaEdge = pb)
#   Data <- graphicalVARsim(150, Mod$beta, Mod$kappa)
#   Data[,4:7] <- apply(Data[,4:7], 2, function(x) ifelse(x > 0, 1, 0))
#   data_train <- Data[1:100,]
#   data_test <- Data[101:150,]
#   Res <- mvar(data = data_train
#               , type = c(rep("g", 3), rep("c", 4))
#               , level = c(rep(1, 3), rep(2,4))
#               , lags = 1
#               , k = 2
#               , lambdaSel = "CV"
#               , ruleReg = "AND"
#               , pbar = T)
#   pred <- predict(Res, data_test)
#   res_small <- Res$signs
#   res_small[is.na(res_small) | res_small == 0] <- 1
#   res_small <- res_small*Res$wadj
#   return(list(res = Res, pred = pred, small = res_small))
# }
# 
# 
# set.seed(6)
# gvar_ms <- tibble(
#   P = c(1,2)
#   , pk = c(.1,.3)
#   , pb = c(.1, .7)
#   , m = map2(pk, pb, gvar_sim_fun)
# )
# 
# 
# net_plot_fun <- function(m, p){
#   groups <- list(
#     A = 1:2
#     , B = 3:4
#     , C = 5:6
#     , Cog = 7
#   )
#   
#   img <- paste0(wd, "/images/", c("A", "B", "C", "cog"), ".png")
#   
#   colors <- c("#dee0ff", "#f8dedb", "#ffefcb", "#aae6c8")#c(color, color, "black")
#   
#   plot <- qgraph(
#     m$small[,,1]
#     , layout = "spring"
#     , loop = .7
#     # , title = sprintf("Wave %s: %s for S%s", wave, type, subject)
#     , node.width = 1.85
#     , repulsion = .8
#     , label.font = 2
#     , label.fill.vertical = 1
#     , label.fill.horizontal = 1
#     , esize = 7
#     , edge.width = 1
#     , edge.color = "black"
#     , threshold = .2
#     # , edge.labels = TRUE
#     # , edge.label.color = "black"
#     , negDashed = T
#     , border.width = 3
#     , asize = 5
#     # , shape = c(rep("circle", 6), "square")
#     # , groups = groups
#     # , color = colors
#     , images = img
#     , legend = F
#     , DoNotPlot = TRUE
#     , mar = c(6,4,8,4)
#   )
#   #change line colors
#   # plot$graphAttributes$Edges$color <-
#   #   ifelse(abs(plot$Edgelist$weight) <.1, edge_colors[1],
#   #          ifelse(abs(plot$Edgelist$weight) <.2, edge_colors[2], edge_colors[3]))
#   dark_colors <- c("#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D")
#   plot$graphAttributes$Nodes$label.color[plot$graphAttributes$Nodes$color %in% dark_colors] <- "white"
#   vars <- c(paste0(rep(c("A", "B", "C"), each = 2), rep(1:2, times = 3)), "Cog")#str_replace(colnames(data), "_", "\n")
#   #change variable names
#   plot$graphAttributes$Nodes$labels <- vars
#   plot(plot)
#   title(sprintf("Person %i", p), cex.main = 5)
#   # title(sprintf("Accuracy = %.2f", m$pred$errors$CC[9]), font.main = 1, cex.main = 3,line = -2)
# }
# 
# 
# wd <- "~/edbeck@ucdavis.edu - Google Drive/My Drive/Emorie & Zoe Grants/RS/figures"
# 
# 
# png(file = "~/Downloads/aim-3-nets-8png", width = 600, height = 1200)
# par(mfrow = c(2,1))
# gvar_ms %>%
#   mutate(net = pmap(list(m, P), net_plot_fun))
# dev.off()
# 
# 
# 
# gvar_sim_fun <- function(pk, pb){
#   Mod <- randomGVARmodel(5, probKappaEdge = pk,probBetaEdge = pb)
#   Data <- graphicalVARsim(150, Mod$beta, Mod$kappa)
#   Data[,1:2] <- apply(Data[,1:2], 2, function(x) ifelse(x > 0, 1, 0))
#   data_train <- Data[1:100,]
#   data_test <- Data[101:150,]
#   Res <- mvar(data = data_train
#               , type = c(rep("c", 2), rep("g", 3))
#               , level = c(rep(2, 2), rep(1, 3))
#               , lags = 1
#               , k = 2
#               , lambdaSel = "CV"
#               , ruleReg = "AND"
#               , pbar = T)
#   pred <- predict(Res, data_test)
#   res_small <- Res$signs
#   res_small[is.na(res_small) | res_small == 0] <- 1
#   res_small <- res_small*Res$wadj
#   return(list(res = Res, pred = pred, small = res_small))
# }
# 
# set.seed(6)
# gvar_ms <- tibble(
#   P = c(1,2)
#   , pk = c(.1, .3)
#   , pb = c(.1, .6)
#   , m = map2(pk, pb, gvar_sim_fun)
# )
# 
# 
# net_plot_fun <- function(m, p){
#   groups <- list(
#     A = 1
#     , B = 2
#     , C = 3
#     , Cog = 4
#     , HB = 5
#   )
#   
#   colors <- c("#dee0ff", "#f8dedb", "#ffefcb", "#aae6c8", "#dcc0fc")#c(color, color, "black")
#   
#   plot <- qgraph(
#     m$small[,,1]
#     , layout = "spring"
#     , loop = .7
#     # , title = sprintf("Wave %s: %s for S%s", wave, type, subject)
#     , node.width = 1.85
#     , repulsion = .8
#     , label.font = 2
#     , label.fill.vertical = 1
#     , label.fill.horizontal = 1
#     , esize = 7
#     , edge.width = 1
#     , edge.color = "black"
#     # , edge.labels = TRUE
#     # , edge.label.color = "black"
#     , negDashed = T
#     , border.width = 3
#     , asize = 5
#     , shape = c(rep("circle", 6), "square")
#     , groups = groups
#     , color = colors
#     , legend = F
#     , DoNotPlot = TRUE
#     , mar = c(6,4,8,4)
#   )
#   #change line colors
#   # plot$graphAttributes$Edges$color <-
#   #   ifelse(abs(plot$Edgelist$weight) <.1, edge_colors[1],
#   #          ifelse(abs(plot$Edgelist$weight) <.2, edge_colors[2], edge_colors[3]))
#   dark_colors <- c("#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D")
#   plot$graphAttributes$Nodes$label.color[plot$graphAttributes$Nodes$color %in% dark_colors] <- "white"
#   vars <- c("A", "B", "C", "Cog", "HB")#str_replace(colnames(data), "_", "\n")
#   #change variable names
#   plot$graphAttributes$Nodes$labels <- vars
#   plot(plot)
#   title(sprintf("Person %i", p), cex.main = 5)
#   # title(sprintf("Accuracy = %.2f", m$pred$errors$CC[9]), font.main = 1, cex.main = 3,line = -2)
# }


# 
# # 
# # png(file = "~/Downloads/aim-3-nets.png", width = 600, height = 1200)
# # par(mfrow = c(2,1))
# # gvar_ms %>%
# #   mutate(net = pmap(list(m, P), net_plot_fun))
# # dev.off()
# 
# net_plot_fun <- function(m, p){
#   groups <- list(
#     A = 1
#     , B = 2
#     , C = 3
#     , Cog = 4
#   )
#   
#   colors <- c("#dee0ff", "#f8dedb", "#ffefcb", "#aae6c8", "#dcc0fc")#c(color, color, "black")
#   img <- paste0(wd, "/images/", c("A", "B", "C", "cog", "HB"), ".png")
#   
#   plot <- qgraph(
#     m$small[,,1]
#     , layout = "spring"
#     , loop = .7
#     # , title = sprintf("Wave %s: %s for S%s", wave, type, subject)
#     , node.width = 1.85
#     , repulsion = .8
#     , labels = F
#     # , label.font = 2
#     # , label.fill.vertical = 1
#     # , label.fill.horizontal = 1
#     , esize = 7
#     , edge.width = 1
#     , edge.color = "black"
#     # , edge.labels = TRUE
#     # , edge.label.color = "black"
#     , negDashed = T
#     , border.width = 3
#     , asize = 5
#     , shape = c(rep("circle", 6), "square")
#     , groups = groups
#     , color = colors
#     , images = img
#     , legend = F
#     , DoNotPlot = TRUE
#     , mar = c(6,4,8,4)
#   )
#   #change line colors
#   # plot$graphAttributes$Edges$color <-
#   #   ifelse(abs(plot$Edgelist$weight) <.1, edge_colors[1],
#   #          ifelse(abs(plot$Edgelist$weight) <.2, edge_colors[2], edge_colors[3]))
#   dark_colors <- c("#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D")
#   plot$graphAttributes$Nodes$label.color[plot$graphAttributes$Nodes$color %in% dark_colors] <- "white"
#   # vars <- c("A", "B", "C", "Cog")#str_replace(colnames(data), "_", "\n")
#   #change variable names
#   # plot$graphAttributes$Nodes$labels <- vars
#   plot(plot)
#   title(sprintf("Person %i", p), cex.main = 5)
#   # title(sprintf("Accuracy = %.2f", m$pred$errors$CC[9]), font.main = 1, cex.main = 3,line = -2)
# }
# 
# 
# 
# 
# png(file = "~/Downloads/aim-3-nets-img-ex.png", width = 600, height = 600)
# par(mfrow = c(1,1))
# gvar_ms %>%
#   filter(row_number() == 1) %>%
#   mutate(net = pmap(list(m, P), net_plot_fun))
# dev.off()
# 
# png(file = "~/Downloads/aim-3-nets-img-2.png", width = 600, height = 600)
# par(mfrow = c(1,1))
# gvar_ms %>%
#   filter(row_number() == 2) %>%
#   mutate(net = pmap(list(m, P), net_plot_fun))
# dev.off()
# 
# net_plot_fun <- function(m, p){
#   groups <- list(
#     A = 1
#     , B = 2
#     , C = 3
#     , Cog = 4
#   )
#   
#   colors <- c("#dee0ff", "#f8dedb", "#ffefcb", "#aae6c8")#c(color, color, "black")
#   img <- paste0(wd, "/images/", c("A", "B", "C", "cog"), "-col.png")
#   
#   plot <- qgraph(
#     m$small[,,1]
#     , layout = "spring"
#     , loop = .7
#     # , title = sprintf("Wave %s: %s for S%s", wave, type, subject)
#     , node.width = 1.85
#     , repulsion = .8
#     , labels = F
#     # , label.font = 2
#     # , label.fill.vertical = 1
#     # , label.fill.horizontal = 1
#     , esize = 7
#     , edge.width = 1
#     , edge.color = "black"
#     # , edge.labels = TRUE
#     # , edge.label.color = "black"
#     , negDashed = T
#     , border.width = 3
#     , asize = 5
#     , shape = c(rep("circle", 6), "square")
#     , groups = groups
#     , color = colors
#     , images = img
#     , legend = F
#     , DoNotPlot = TRUE
#     , mar = c(6,4,8,4)
#   )
#   #change line colors
#   # plot$graphAttributes$Edges$color <-
#   #   ifelse(abs(plot$Edgelist$weight) <.1, edge_colors[1],
#   #          ifelse(abs(plot$Edgelist$weight) <.2, edge_colors[2], edge_colors[3]))
#   dark_colors <- c("#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D")
#   plot$graphAttributes$Nodes$label.color[plot$graphAttributes$Nodes$color %in% dark_colors] <- "white"
#   # vars <- c("A", "B", "C", "Cog")#str_replace(colnames(data), "_", "\n")
#   #change variable names
#   # plot$graphAttributes$Nodes$labels <- vars
#   plot(plot)
#   title(sprintf("Person %i", p), cex.main = 5)
#   # title(sprintf("Accuracy = %.2f", m$pred$errors$CC[9]), font.main = 1, cex.main = 3,line = -2)
# }
# 
# 
# 
# 
# png(file = "~/Downloads/aim-3-nets-img-ex-col.png", width = 600, height = 600)
# par(mfrow = c(1,1))
# gvar_ms %>%
#   filter(row_number() == 1) %>%
#   mutate(net = pmap(list(m, P), net_plot_fun))
# dev.off()
# 
# png(file = "~/Downloads/aim-3-nets-img-2-col.png", width = 600, height = 600)
# par(mfrow = c(1,1))
# gvar_ms %>%
#   filter(row_number() == 2) %>%
#   mutate(net = pmap(list(m, P), net_plot_fun))
# dev.off()
# 
