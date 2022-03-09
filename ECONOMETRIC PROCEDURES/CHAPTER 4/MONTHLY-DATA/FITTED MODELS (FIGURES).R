library(BGVAR)
library(ggplot2)
library(tidyverse)
library(cowplot)

#CH ####

# MODEL 5 ####

yfit <- fitted(model5t.ch.ssvs, global = FALSE) %>%
  as.data.frame()
fit.CH <- which(grepl("CH.", dimnames(yfit)[[2]]))
own.CH <- select(yfit, all_of(fit.CH)) %>%
  add_row(.before = 1) %>% # ONLY ONE LAG
  as.data.frame()

realvalues.CH <-
  MODEL5T_CH$CH  %>%
  as.data.frame()

names(own.CH)
# [1] "CH.X"   "CH.M"   "CH.IR"  "CH.ER"  "CH.EQ"  "CH.NP"  "CH.RES"

g.CH.X.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 182)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = X)) +
  geom_line(aes(y = CH.X), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("X"))))

g.CH.M.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 182)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = M)) +
  geom_line(aes(y = CH.M), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("M"))))

g.CH.IR.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 182)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IR)) +
  geom_line(aes(y = CH.IR), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 182)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = ER)) +
  geom_line(aes(y = CH.ER), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 182)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = EQ)) +
  geom_line(aes(y = CH.EQ), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 182)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = NP)) +
  geom_line(aes(y = CH.NP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 182)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = RES)) +
  geom_line(aes(y = CH.RES), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("RES"))))

g.model5t.CH <- plot_grid(g.CH.X.fitted, g.CH.M.fitted, g.CH.IR.fitted,
                          g.CH.ER.fitted, g.CH.EQ.fitted, g.CH.NP.fitted,
                          NULL, g.CH.RES.fitted, NULL,
                          ncol = 3, nrow = 3, align = "hv")

ggsave(filename = "g.model5t.CH.png", g.model5t.CH,
       width = 9, height = 6, dpi = 300, units = "in", device='png', bg = "white")

# MODEL 6 ####

yfit <- fitted(model6t.ch.ssvs, global = FALSE) %>%
  as.data.frame()
fit.CH <- which(grepl("CH.", dimnames(yfit)[[2]]))
own.CH <- select(yfit, all_of(fit.CH)) %>%
  add_row(.before = 1) %>% # ONLY ONE LAG
  as.data.frame()

realvalues.CH <-
  MODEL6T_CH$CH  %>%
  as.data.frame()

names(own.CH)
# [1] "CH.X"   "CH.M"   "CH.IR"  "CH.ER"  "CH.EQ"  "CH.NP"  "CH.RES"

g.CH.X.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 155)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = X)) +
  geom_line(aes(y = CH.X), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("X"))))

g.CH.M.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 155)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = M)) +
  geom_line(aes(y = CH.M), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("M"))))

g.CH.IR.fitted <-
  cbind(own.CH, realvalues.CH) %>%
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 155)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IR)) +
  geom_line(aes(y = CH.IR), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 155)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = ER)) +
  geom_line(aes(y = CH.ER), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 155)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = EQ)) +
  geom_line(aes(y = CH.EQ), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 155)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = NP)) +
  geom_line(aes(y = CH.NP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2006/6/1"), by = "month", length.out = 155)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = RES)) +
  geom_line(aes(y = CH.RES), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("RES"))))

g.model6t.CH <- plot_grid(g.CH.X.fitted, g.CH.M.fitted, g.CH.IR.fitted,
                          g.CH.ER.fitted, g.CH.EQ.fitted, g.CH.NP.fitted,
                          NULL, g.CH.RES.fitted, NULL,
                          ncol = 3, nrow = 3, align = "hv")

ggsave(filename = "g.model6t.CH.png", g.model6t.CH,
       width = 9, height = 6, dpi = 300, units = "in", device='png', bg = "white")


#BR ####

# MODEL 5 ####

yfit <- fitted(model5t.br.ssvs, global = FALSE) %>%
  as.data.frame()
fit.BR <- which(grepl("BR.", dimnames(yfit)[[2]]))
own.BR <- select(yfit, all_of(fit.BR)) %>%
  add_row(.before = 1) %>% # ONLY ONE LAG
  as.data.frame()

realvalues.BR <-
  MODEL5T_BR$BR  %>%
  as.data.frame()

names(own.BR)
# [1] "BR.X"   "BR.M"   "BR.IR"  "BR.ER"  "BR.EQ"  "BR.NP"  "BR.RES"

g.BR.X.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = X)) +
  geom_line(aes(y = BR.X), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("X"))))

g.BR.M.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = M)) +
  geom_line(aes(y = BR.M), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("M"))))

g.BR.IR.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IR)) +
  geom_line(aes(y = BR.IR), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = ER)) +
  geom_line(aes(y = BR.ER), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = EQ)) +
  geom_line(aes(y = BR.EQ), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = NP)) +
  geom_line(aes(y = BR.NP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = RES)) +
  geom_line(aes(y = BR.RES), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("RES"))))

g.model5t.BR <- plot_grid(g.BR.X.fitted, g.BR.M.fitted, g.BR.IR.fitted,
                          g.BR.ER.fitted, g.BR.EQ.fitted, g.BR.NP.fitted,
                          NULL, g.BR.RES.fitted, NULL,
                          ncol = 3, nrow = 3, align = "hv")

ggsave(filename = "g.model5t.BR.png", g.model5t.BR,
       width = 9, height = 6, dpi = 300, units = "in", device='png', bg = "white")

# MODEL 6 ####

yfit <- fitted(model6t.br.ssvs, global = FALSE) %>%
  as.data.frame()
fit.BR <- which(grepl("BR.", dimnames(yfit)[[2]]))
own.BR <- select(yfit, all_of(fit.BR)) %>%
  add_row(.before = 1) %>% # ONLY ONE LAG
  as.data.frame()

realvalues.BR <-
  MODEL6T_BR$BR  %>%
  as.data.frame()

names(own.BR)
# [1] "BR.X"   "BR.M"   "BR.IR"  "BR.ER"  "BR.EQ"  "BR.NP"  "BR.RES"

g.BR.X.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = X)) +
  geom_line(aes(y = BR.X), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("X"))))

g.BR.M.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = M)) +
  geom_line(aes(y = BR.M), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("M"))))

g.BR.IR.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IR)) +
  geom_line(aes(y = BR.IR), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = ER)) +
  geom_line(aes(y = BR.ER), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = EQ)) +
  geom_line(aes(y = BR.EQ), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = NP)) +
  geom_line(aes(y = BR.NP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = RES)) +
  geom_line(aes(y = BR.RES), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("RES"))))

g.model6t.BR <- plot_grid(g.BR.X.fitted, g.BR.M.fitted, g.BR.IR.fitted,
                          g.BR.ER.fitted, g.BR.EQ.fitted, g.BR.NP.fitted,
                          NULL, g.BR.RES.fitted, NULL,
                          ncol = 3, nrow = 3, align = "hv")

ggsave(filename = "g.model6t.BR.png", g.model6t.BR,
       width = 9, height = 6, dpi = 300, units = "in", device='png', bg = "white")


# MODEL 7 ####

yfit <- fitted(model7t.br.ssvs, global = FALSE) %>%
  as.data.frame()
fit.BR <- which(grepl("BR.", dimnames(yfit)[[2]]))
own.BR <- select(yfit, all_of(fit.BR)) %>%
  add_row(.before = 1) %>% # ONLY ONE LAG
  as.data.frame()

realvalues.BR <-
  MODEL7T_BR$BR  %>%
  as.data.frame()

names(own.BR)
# [1] "BR.IP"  "BR.X"   "BR.M"   "BR.IR"  "BR.ER"  "BR.EQ"  "BR.NP"  "BR.RES"

g.BR.IP.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IP)) +
  geom_line(aes(y = BR.IP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("IP"))))

g.BR.X.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = X)) +
  geom_line(aes(y = BR.X), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("X"))))

g.BR.M.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = M)) +
  geom_line(aes(y = BR.M), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("M"))))

g.BR.IR.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IR)) +
  geom_line(aes(y = BR.IR), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = ER)) +
  geom_line(aes(y = BR.ER), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = EQ)) +
  geom_line(aes(y = BR.EQ), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = NP)) +
  geom_line(aes(y = BR.NP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 91)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = RES)) +
  geom_line(aes(y = BR.RES), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("RES"))))

g.model7t.BR.1 <- plot_grid(g.BR.IP.fitted, g.BR.X.fitted, g.BR.M.fitted, 
                          g.BR.IR.fitted, g.BR.ER.fitted, g.BR.EQ.fitted,
                          ncol = 3, nrow = 2, align = "hv")

g.model7t.BR.2 <- plot_grid(NULL, g.BR.NP.fitted, g.BR.RES.fitted, NULL,
                            ncol = 4, nrow = 1, align = "hv",
                            rel_widths = c(0.165, 0.33, 0.33, 0.165))

g.model7t.BR <- plot_grid(g.model7t.BR.1,
                            g.model7t.BR.2,
                            ncol = 1, nrow = 2, align = "hv",
                          rel_heights = c(1, 0.5))

ggsave(filename = "g.model7t.BR.png", g.model7t.BR,
       width = 9, height = 6, dpi = 300, units = "in", device='png', bg = "white")


# MODEL 8 ####

yfit <- fitted(model8t.br.ssvs, global = FALSE) %>%
  as.data.frame()
fit.BR <- which(grepl("BR.", dimnames(yfit)[[2]]))
own.BR <- select(yfit, all_of(fit.BR)) %>%
  add_row(.before = 1) %>% # ONLY ONE LAG
  as.data.frame()

realvalues.BR <-
  MODEL8T_BR$BR  %>%
  as.data.frame()

names(own.BR)
# [1] "BR.IP"  "BR.X"   "BR.M"   "BR.IR"  "BR.ER"  "BR.EQ"  "BR.NP"  "BR.RES"

g.BR.IP.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IP)) +
  geom_line(aes(y = BR.IP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("IP"))))

g.BR.X.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = X)) +
  geom_line(aes(y = BR.X), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("X"))))

g.BR.M.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = M)) +
  geom_line(aes(y = BR.M), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("M"))))

g.BR.IR.fitted <-
  cbind(own.BR, realvalues.BR) %>%
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = IR)) +
  geom_line(aes(y = BR.IR), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = ER)) +
  geom_line(aes(y = BR.ER), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = EQ)) +
  geom_line(aes(y = BR.EQ), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = NP)) +
  geom_line(aes(y = BR.NP), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
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
  mutate(steps = seq(as.Date("2014/1/1"), by = "month", length.out = 64)) %>%
  ggplot(aes(x = steps)) +
  geom_line(aes(y = RES)) +
  geom_line(aes(y = BR.RES), linetype="dotted", color = "red") +
  #geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y-%m") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("RES"))))

g.model8t.BR.1 <- plot_grid(g.BR.IP.fitted, g.BR.X.fitted, g.BR.M.fitted, 
                            g.BR.IR.fitted, g.BR.ER.fitted, g.BR.EQ.fitted,
                            ncol = 3, nrow = 2, align = "hv")

g.model8t.BR.2 <- plot_grid(NULL, g.BR.NP.fitted, g.BR.RES.fitted, NULL,
                            ncol = 4, nrow = 1, align = "hv",
                            rel_widths = c(0.165, 0.33, 0.33, 0.165))

g.model8t.BR <- plot_grid(g.model8t.BR.1,
                          g.model8t.BR.2,
                          ncol = 1, nrow = 2, align = "hv",
                          rel_heights = c(1, 0.5))

ggsave(filename = "g.model8t.BR.png", g.model8t.BR,
       width = 9, height = 6, dpi = 300, units = "in", device='png', bg = "white")



