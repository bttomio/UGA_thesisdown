library(BGVAR)
library(ggplot2)
library(tidyverse)
library(cowplot)

#CH ####

# MODEL 1 ####

yfit <- fitted(model1t.ch.ssvs, global = FALSE) %>%
  as.data.frame()
fit.CH <- which(grepl("CH.", dimnames(yfit)[[2]]))
own.CH <- select(yfit, all_of(fit.CH)) %>%
  add_row(.before = 1) %>% # ONLY ONE LAG
  as.data.frame()

realvalues.CH <-
  MODEL1T_CH$CH  %>%
  as.data.frame()

names(own.CH)
# [1] "CH.GDP" "CH.IR"  "CH.ER"  "CH.EQ"  "CH.NP"  "CH.RES"

g.CH.GDP.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = GDP)) +
  geom_line(aes(y = CH.GDP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("GDP"))))

g.CH.IR.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IR)) +
  geom_line(aes(y = CH.IR), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("IR"))))

g.CH.ER.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = ER)) +
  geom_line(aes(y = CH.ER), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("ER"))))

g.CH.EQ.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = EQ)) +
  geom_line(aes(y = CH.EQ), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("EQ"))))

g.CH.NP.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = NP)) +
  geom_line(aes(y = CH.NP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("NP"))))

g.CH.RES.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = RES)) +
  geom_line(aes(y = CH.RES), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("RES"))))

g.model1t.ch <- plot_grid(g.CH.GDP.fitted, g.CH.IR.fitted, g.CH.ER.fitted, 
                          g.CH.EQ.fitted, g.CH.NP.fitted, g.CH.RES.fitted,
                          ncol = 3, nrow = 2, align = "hv")

ggsave(filename = "g.model1t.ch.png", g.model1t.ch,
       width = 9, height = 4, dpi = 300, units = "in", device='png', bg = "white")

# MODEL 2 ####

yfit <- fitted(model2t.ch.ssvs, global = FALSE) %>%
  as.data.frame()
fit.CH <- which(grepl("CH.", dimnames(yfit)[[2]]))
own.CH <- select(yfit, all_of(fit.CH)) %>%
  add_row(.before = 1) %>% # ONLY ONE LAG
  as.data.frame()

realvalues.CH <-
  MODEL2T_CH$CH  %>%
  as.data.frame()

names(own.CH)
# [1] "CH.GDP" "CH.IR"  "CH.ER"  "CH.EQ"  "CH.NP"  "CH.RES"

g.CH.GDP.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = GDP)) +
  geom_line(aes(y = CH.GDP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("GDP"))))

g.CH.IR.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IR)) +
  geom_line(aes(y = CH.IR), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("IR"))))

g.CH.ER.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = ER)) +
  geom_line(aes(y = CH.ER), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("ER"))))

g.CH.EQ.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = EQ)) +
  geom_line(aes(y = CH.EQ), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("EQ"))))

g.CH.NP.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = NP)) +
  geom_line(aes(y = CH.NP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("NP"))))

g.CH.RES.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = RES)) +
  geom_line(aes(y = CH.RES), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("RES"))))

g.model2t.ch <- plot_grid(g.CH.GDP.fitted, g.CH.IR.fitted, g.CH.ER.fitted, 
                          g.CH.EQ.fitted, g.CH.NP.fitted, g.CH.RES.fitted,
                          ncol = 3, nrow = 2, align = "hv")

ggsave(filename = "g.model2t.ch.png", g.model2t.ch,
       width = 9, height = 4, dpi = 300, units = "in", device='png', bg = "white")

# MODEL 3 ####

yfit <- fitted(model3t.ch.ssvs, global = FALSE) %>%
  as.data.frame()
fit.CH <- which(grepl("CH.", dimnames(yfit)[[2]]))
own.CH <- select(yfit, all_of(fit.CH)) %>%
  add_row(.before = 1) %>% # ONLY ONE LAG
  as.data.frame()

realvalues.CH <-
  MODEL3T_CH$CH  %>%
  as.data.frame()

names(own.CH)
# [1] "CH.C"    "CH.GFCF" "CH.M"    "CH.X"    "CH.IR"   "CH.ER"   "CH.EQ"   "CH.NP"   "CH.RES"

g.CH.C.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = C)) +
  geom_line(aes(y = CH.C), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("C"))))

g.CH.GFCF.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = GFCF)) +
  geom_line(aes(y = CH.GFCF), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("GFCF"))))

g.CH.M.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = M)) +
  geom_line(aes(y = CH.M), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("M"))))

g.CH.X.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = X)) +
  geom_line(aes(y = CH.X), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("X"))))

g.CH.IR.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IR)) +
  geom_line(aes(y = CH.IR), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("IR"))))

g.CH.ER.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = ER)) +
  geom_line(aes(y = CH.ER), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("ER"))))

g.CH.EQ.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = EQ)) +
  geom_line(aes(y = CH.EQ), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("EQ"))))

g.CH.NP.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = NP)) +
  geom_line(aes(y = CH.NP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("NP"))))

g.CH.RES.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 61)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = RES)) +
  geom_line(aes(y = CH.RES), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("RES"))))

g.model3t.ch <- plot_grid(g.CH.C.fitted, g.CH.GFCF.fitted, g.CH.M.fitted,
                          g.CH.X.fitted, g.CH.IR.fitted, g.CH.ER.fitted, 
                          g.CH.EQ.fitted, g.CH.NP.fitted, g.CH.RES.fitted,
                          ncol = 3, nrow = 3, align = "hv")

ggsave(filename = "g.model3t.ch.png", g.model3t.ch,
       width = 9, height = 6, dpi = 300, units = "in", device='png', bg = "white")

# MODEL 4 ####

yfit <- fitted(model4t.ch.ssvs, global = FALSE) %>%
  as.data.frame()
fit.CH <- which(grepl("CH.", dimnames(yfit)[[2]]))
own.CH <- select(yfit, all_of(fit.CH)) %>%
  add_row(.before = 1) %>% # ONLY ONE LAG
  as.data.frame()

realvalues.CH <-
  MODEL4T_CH$CH  %>%
  as.data.frame()

names(own.CH)
# [1] "CH.C"    "CH.GFCF" "CH.M"    "CH.X"    "CH.IR"   "CH.ER"   "CH.EQ"   "CH.NP"   "CH.RES"

g.CH.C.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = C)) +
  geom_line(aes(y = CH.C), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("C"))))

g.CH.GFCF.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = GFCF)) +
  geom_line(aes(y = CH.GFCF), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5, size = 6),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("GFCF"))))

g.CH.M.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = M)) +
  geom_line(aes(y = CH.M), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("M"))))

g.CH.X.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = X)) +
  geom_line(aes(y = CH.X), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("X"))))

g.CH.IR.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IR)) +
  geom_line(aes(y = CH.IR), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("IR"))))

g.CH.ER.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = ER)) +
  geom_line(aes(y = CH.ER), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("ER"))))

g.CH.EQ.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = EQ)) +
  geom_line(aes(y = CH.EQ), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("EQ"))))

g.CH.NP.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = NP)) +
  geom_line(aes(y = CH.NP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("NP"))))

g.CH.RES.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "quarter", length.out = 52)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = RES)) +
  geom_line(aes(y = CH.RES), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q", n = 3) +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("RES"))))

g.model4t.ch <- plot_grid(g.CH.C.fitted, g.CH.GFCF.fitted, g.CH.M.fitted,
                          g.CH.X.fitted, g.CH.IR.fitted, g.CH.ER.fitted, 
                          g.CH.EQ.fitted, g.CH.NP.fitted, g.CH.RES.fitted,
                          ncol = 3, nrow = 3, align = "hv")

ggsave(filename = "g.model4t.ch.png", g.model4t.ch,
       width = 9, height = 6, dpi = 300, units = "in", device='png', bg = "white")

#BR ####

# MODEL 1 ####

yfit <- fitted(model1t.br.ssvs, global = FALSE) %>%
  as.data.frame()
fit.BR <- which(grepl("BR.", dimnames(yfit)[[2]]))
own.BR <- select(yfit, all_of(fit.BR)) %>%
  add_row(.before = 1) %>% # ONLY ONE LAG
  as.data.frame()

realvalues.BR <-
  MODEL1T_BR$BR  %>%
  as.data.frame()

names(own.BR)
# [1] "BR.GDP" "BR.IR"  "BR.ER"  "BR.EQ"  "BR.NP"  "BR.RES"

g.BR.GDP.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = GDP)) +
  geom_line(aes(y = BR.GDP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("GDP"))))

g.BR.IR.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IR)) +
  geom_line(aes(y = BR.IR), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("IR"))))

g.BR.ER.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = ER)) +
  geom_line(aes(y = BR.ER), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("ER"))))

g.BR.EQ.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = EQ)) +
  geom_line(aes(y = BR.EQ), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("EQ"))))

g.BR.NP.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = NP)) +
  geom_line(aes(y = BR.NP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("NP"))))

g.BR.RES.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = RES)) +
  geom_line(aes(y = BR.RES), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5, size = 6),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("RES"))))

g.model1t.br <- plot_grid(g.BR.GDP.fitted, g.BR.IR.fitted, g.BR.ER.fitted, 
                          g.BR.EQ.fitted, g.BR.NP.fitted, g.BR.RES.fitted,
                          ncol = 3, nrow = 2, align = "hv")

ggsave(filename = "g.model1t.br.png", g.model1t.br,
       width = 9, height = 4, dpi = 300, units = "in", device='png', bg = "white")


# MODEL 3 ####

yfit <- fitted(model3t.br.ssvs, global = FALSE) %>%
  as.data.frame()
fit.BR <- which(grepl("BR.", dimnames(yfit)[[2]]))
own.BR <- select(yfit, all_of(fit.BR)) %>%
  add_row(.before = 1) %>% # ONLY ONE LAG
  as.data.frame()

realvalues.BR <-
  MODEL3T_BR$BR  %>%
  as.data.frame()

names(own.BR)
# [1] "BR.C"    "BR.GFCF" "BR.M"    "BR.X"    "BR.IR"   "BR.ER"   "BR.EQ"   "BR.NP"   "BR.RES" 

g.BR.C.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = C)) +
  geom_line(aes(y = BR.C), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("C"))))

g.BR.GFCF.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = GFCF)) +
  geom_line(aes(y = BR.GFCF), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("GFCF"))))

g.BR.M.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = M)) +
  geom_line(aes(y = BR.M), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("M"))))

g.BR.X.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = X)) +
  geom_line(aes(y = BR.X), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("X"))))

g.BR.IR.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IR)) +
  geom_line(aes(y = BR.IR), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("IR"))))

g.BR.ER.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = ER)) +
  geom_line(aes(y = BR.ER), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("ER"))))

g.BR.EQ.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = EQ)) +
  geom_line(aes(y = BR.EQ), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("EQ"))))

g.BR.NP.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = NP)) +
  geom_line(aes(y = BR.NP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("NP"))))

g.BR.RES.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2012/6/1"), by = "quarter", length.out = 37)) %>%
  mutate(steps = zoo::as.yearqtr(steps)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = RES)) +
  geom_line(aes(y = BR.RES), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  zoo::scale_x_yearqtr(format = "%Y-Q%q") +
  #scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5, size = 6),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("RES"))))

g.model3t.br <- plot_grid(g.BR.C.fitted, g.BR.GFCF.fitted, g.BR.M.fitted, 
                          g.BR.X.fitted, g.BR.IR.fitted, g.BR.ER.fitted, 
                          g.BR.EQ.fitted, g.BR.NP.fitted, g.BR.RES.fitted,
                          ncol = 3, nrow = 3, align = "hv")

ggsave(filename = "g.model3t.br.png", g.model3t.br,
       width = 9, height = 6, dpi = 300, units = "in", device='png', bg = "white")

