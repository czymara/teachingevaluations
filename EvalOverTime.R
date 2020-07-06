

### plot time trend of course evaluations

evals <- data.frame("semester" = c("2016", "2016/17", "2017", "2017/18",
                                   # frankfurt
                                   "2018", "2018/19",
                                   "2019", "2019/20",
                                   "2020"),
                    "mean" = c(2, 1.6, NA, 1.4,
                               # frankfurt
                               1.7,
                               1.6, 1.4,
                               1.5, 1.4
                               ),
                    "sd" = c(0, 0.7, NA, 0.6,
                             # frankfurt
                             0.5,
                             0.5, 0.5,
                             0.7, 1.2
                             ),
                    "n" = c(5, 21, NA, 18,
                            # frankfurt
                            6,
                            10, 8,
                            12, 19
                            ),
                    "University" = c("CGN", "CGN", "CGN", "CGN",
                              "FRA", "FRA", "FRA", "FRA", "FRA"),
                    "worth" = c (NA, NA, NA, NA, NA,
                                 5.8, 5.3, 5.5, 5.9),
                    "worthSD" = c (NA, NA, NA, NA, NA,
                                 0.4, 0.8, 0.7, 0.3)
                    )

evals$grandmean <- mean(evals$mean, na.rm = T)

evals$worthgrandmean <- mean(evals$worth, na.rm = T)

library(ggplot2); theme_set(theme_bw() +
                              theme(axis.line = element_line(colour = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_blank(),
                                    panel.background = element_blank(),
                                    axis.text = element_text(color="black"),
                                    axis.ticks = element_line(colour = "black")))

# win.metafile("C:/Users/czymara.local/PowerFolders/teaching/teachingevaluations/out/evalovertime.wmf")
dev.copy(png, "C:/Users/czymara.local/PowerFolders/teaching/teachingevaluations/out/evalovertime.png",
         units="px", width=1600, height=1600, res=300)
ggplot(data = evals, aes(y = semester,
                         x = mean,
                         colour = University)) +
  geom_errorbarh(aes(xmin = mean - sd,
                     xmax = mean + sd),
                 colour = "grey") +
  xlab("Overall grade (1: best)") + ylab("") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Teaching evaluation of C. Czymara",
       caption = "Means and standard deviations
       Overall grade (left y-axis): Until 2018 scale 1-5,
       from 2018/19 scale 1-6") +
  geom_path(aes(group = 1), colour = "grey") +
  geom_vline(xintercept = evals$grandmean ,
             colour = "black") +
  geom_point() +
  geom_errorbarh(data = evals, aes(xmin = worth - worthSD,
                     xmax = worth + worthSD),
                 colour = "grey") +
  scale_x_continuous(limits = c(0, 6.5), breaks = c(1:6),
                     sec.axis = sec_axis(~. , breaks = c(1:6),
                                         name = "Course worth\nattending (6: agree)")
                     ) +
  geom_path(data = evals, aes(x = worth, group = 1), colour = "grey") +  # add the second axis
  geom_point(data = evals, aes(y = semester,
                               x = worth,
                               colour = University)) +
coord_flip()
dev.off()

