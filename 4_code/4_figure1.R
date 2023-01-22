library("ICED")
library("tidyverse")
library("patchwork")
library("Cairo")


struc <- data.frame(time = c("T1", "T2"))

syn <- iced_syntax(struc)

# need to make the right ones of each of these to make a 2x2 panel

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
    ggtitle("Equal Between-subjects variance, \nand Error variance, ICC = .5")

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
  ggtitle("High Between-subjects variance, \nLow Error variance, ICC = .9")

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
  ggtitle("Low Between-subjects variance, \nHigh Error variance, ICC = .1")

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
  ggtitle("Low Between-subjects variance, \nLow Error variance, ICC = .5")

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
  ggtitle("High Between-subjects variance, \nNo Error variance, ICC = 1, (Perfect reliability)")

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




