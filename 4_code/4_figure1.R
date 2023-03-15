library("ICED")
library("tidyverse")
library("patchwork")
library("Cairo")

# generate syntax given set between-subjects and error variances
struc <- data.frame(time = c("T1", "T2"))

syn <- iced_syntax(struc)

# equal between-subjects (true) variance and error variance
hight_highe <- sim_ICED(structure = struc,
                        means = c(1, 10),
                        variances = list(time = 5,
                                         e = 5),
                        n = 30)$data %>%
  mutate(ppid = 1:nrow(.)) %>%
  pivot_longer(cols = c(T1, T2)) %>%
  ggplot(aes(x = name, y = value, group = ppid)) +
  geom_line(size = .3, alpha = .6) +
  geom_point() +
  theme(axis.text.y = element_blank()) +
    ggtitle("Equal between-subjects variance, \nand error variance, ICC = .5")

# high between-subjects (true) variance and low error variance - high reliability
hight_lowe <- sim_ICED(structure = struc,
                        means = c(1, 10),
                        variances = list(time = 9,
                                         e = 1),
                        n = 30)$data %>%
  mutate(ppid = 1:nrow(.)) %>%
  pivot_longer(cols = c(T1, T2)) %>%
  ggplot(aes(x = name, y = value, group = ppid)) +
  geom_line(size = .3, alpha = .6) +
  geom_point() +  
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("High between-subjects variance, \nlow error variance, ICC = .9")


# low between-subjects (true) variance and high error variance - low reliability

lowt_highe <- sim_ICED(structure = struc,
                        means = c(1, 10),
                        variances = list(time = 1,
                                         e = 9),
                        n = 30)$data %>%
  mutate(ppid = 1:nrow(.)) %>%
  pivot_longer(cols = c(T1, T2)) %>%
  ggplot(aes(x = name, y = value, group = ppid)) +
  geom_line(size = .3, alpha = .6) +
  geom_point() +
  theme(axis.text.y = element_blank()) +
  ggtitle("Low between-subjects variance, \nhigh error variance, ICC = .1")

# equal but low variances

lowt_lowe <- sim_ICED(structure = struc,
                        means = c(1, 10),
                        variances = list(time = 1,
                                         e = 1),
                        n = 30)$data %>%
  mutate(ppid = 1:nrow(.)) %>%
  pivot_longer(cols = c(T1, T2)) %>%
  ggplot(aes(x = name, y = value, group = ppid)) +
  geom_line(size = .3, alpha = .6) +
  geom_point() +
  theme(axis.text.y = element_blank()) +
  ggtitle("Low between-subjects variance, \nlow error variance, ICC = .5")


# perfect reliability
hight_zeroe <- sim_ICED(structure = struc,
                        means = c(1, 10),
                        variances = list(time = 10,
                                         e = 0),
                        n = 30)$data %>%
  mutate(ppid = 1:nrow(.)) %>%
  pivot_longer(cols = c(T1, T2)) %>%
  ggplot(aes(x = name, y = value, group = ppid)) +
  geom_line(size = .3, alpha = .6) +
  geom_point() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("High between-subjects variance, \nno error variance, ICC = 1, (perfect reliability)")

# save figure
##
Cairo(file = "6_figures/Figure1_spaghetti.png",
      type = "png",
      units="in", 
      width=6, 
      height=6, 
      pointsize=12, 
      dpi=600)

(hight_zeroe + hight_lowe) / (lowt_highe + hight_highe) & theme(axis.title = element_blank(), plot.title = element_text(size=10), panel.grid.minor = element_blank()) & coord_cartesian(ylim=c(-11, 20))

dev.off()




