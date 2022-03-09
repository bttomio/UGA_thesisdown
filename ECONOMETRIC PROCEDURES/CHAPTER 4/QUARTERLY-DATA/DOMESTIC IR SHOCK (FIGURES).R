RNGkind("L'Ecuyer-CMRG")
set.seed(123)
library(BGVAR)
options(scipen=999)
library(tidyverse)
library(cowplot)

## SHOCKS
## DOM MON. TIGHTENING ####
# FOR TARGET CURRENCIES (BR, MX, RU) ####
# BR ####
## MODEL 1 (MODEL 2 - BOTH - GDP, RES, VIX (DATA).R -> model1t.br.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"BR.IR"
shockinfo_girf$scale<- 1

irf.girf.model1t.br.ssvs.DOMSHOCK.BR <- 
  irf(model1t.br.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 12,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# NP
MODEL1.DOMSHOCK.NP.BR <- 
  plot(irf.girf.model1t.br.ssvs.DOMSHOCK.BR, resp="BR.NP", shock = "BR.IR")

g.MODEL1.DOMSHOCK.NP.BR <-
  MODEL1.DOMSHOCK.NP.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL1.DOMSHOCK.NP.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (1) | "), italic("VIX"))))

## MODEL 3 (MODEL 1 - BOTH - GDPCOMP, RES, VIX (DATA).R -> model3t.br.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"BR.IR"
shockinfo_girf$scale<- 1

irf.girf.model3t.br.ssvs.DOMSHOCK.BR <- 
  irf(model3t.br.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 12,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# NP
MODEL3.DOMSHOCK.NP.BR <- 
  plot(irf.girf.model3t.br.ssvs.DOMSHOCK.BR, resp="BR.NP", shock = "BR.IR")

g.MODEL3.DOMSHOCK.NP.BR <-
  MODEL3.DOMSHOCK.NP.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.DOMSHOCK.NP.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (3) | "), italic("VIX"))))

# PLOT THEM TOGETHER (MODEL 1 +3) ####

label <- substitute(paste(bold("Brazil")))

title <- ggdraw() + 
  draw_label(
    label,
    fontface = 'bold',
    fontfamily = "LM Roman 10",
    size = 10,
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 180)
  )

g.MODEL13.DOMSHOCK.BR <- 
  plot_grid(NULL,
            g.MODEL1.DOMSHOCK.NP.BR,
            g.MODEL3.DOMSHOCK.NP.BR,
            NULL,
            ncol = 4, nrow = 1, align = "hv",
            rel_widths = c(0.25, 0.25, 0.25, 0.25))

g.MODEL13.DOMSHOCK.BR <- 
  plot_grid(
    title, g.MODEL13.DOMSHOCK.BR,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

ggsave(filename = "g.MODEL13.DOMSHOCK.BR.png", g.MODEL13.DOMSHOCK.BR,
       width = 9, height = 2.5, dpi = 300, units = "in", device='png', bg = "white")

# FOR FUNDING CURRENCIES (CA, CH, GB, JP, US, U2) ####
# CH ####
## MODEL 1 (MODEL 4 - CH - GDP, RES, VIX (DATA).R -> model1t.ch.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"CH.IR"
shockinfo_girf$scale<- 0.5

irf.girf.model1t.ch.ssvs.DOMSHOCK.CH <- 
  irf(model1t.ch.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 12,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# NP
MODEL1.DOMSHOCK.NP.CH <- 
  plot(irf.girf.model1t.ch.ssvs.DOMSHOCK.CH, resp="CH.NP", shock = "CH.IR")

g.MODEL1.DOMSHOCK.NP.CH <-
  MODEL1.DOMSHOCK.NP.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL1.DOMSHOCK.NP.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (1) | "), italic("VIX"))))

## MODEL 2 (MODEL 6 - CH - GDP, RES, GFC (DATA).R -> model2t.ch.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"CH.IR"
shockinfo_girf$scale<- 0.5

irf.girf.model2t.ch.ssvs.DOMSHOCK.CH <- 
  irf(model2t.ch.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 12,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# NP
MODEL2.DOMSHOCK.NP.CH <- 
  plot(irf.girf.model2t.ch.ssvs.DOMSHOCK.CH, resp="CH.NP", shock = "CH.IR")

g.MODEL2.DOMSHOCK.NP.CH <-
  MODEL2.DOMSHOCK.NP.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL2.DOMSHOCK.NP.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (2) | "), italic("GCF"))))

## MODEL 3 (MODEL 3 - BOTH - GDPCOMP, RES, VIX (DATA).R -> model3t.ch.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"CH.IR"
shockinfo_girf$scale<- 0.5

irf.girf.model3t.ch.ssvs.DOMSHOCK.CH <- 
  irf(model3t.ch.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 12,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# NP
MODEL3.DOMSHOCK.NP.CH <- 
  plot(irf.girf.model3t.ch.ssvs.DOMSHOCK.CH, resp="CH.NP", shock = "CH.IR")

g.MODEL3.DOMSHOCK.NP.CH <-
  MODEL3.DOMSHOCK.NP.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.DOMSHOCK.NP.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (3) | "), italic("VIX"))))

## MODEL 4 (MODEL 5 - BOTH - GDPCOMP, RES, GFC (DATA).R -> model4t.ch.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"CH.IR"
shockinfo_girf$scale<- 0.5

irf.girf.model4t.ch.ssvs.DOMSHOCK.CH <- 
  irf(model4t.ch.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 12,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# NP
MODEL4.DOMSHOCK.NP.CH <- 
  plot(irf.girf.model4t.ch.ssvs.DOMSHOCK.CH, resp="CH.NP", shock = "CH.IR")

g.MODEL4.DOMSHOCK.NP.CH <-
  MODEL4.DOMSHOCK.NP.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL4.DOMSHOCK.NP.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(size = 6, angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (4) | "), italic("GCF"))))

# PLOT THEM TOGETHER (MODEL 1 + 2 + 3 + 4) ####

label <- substitute(paste(bold("Switzerland")))

title <- ggdraw() + 
  draw_label(
    label,
    fontface = 'bold',
    fontfamily = "LM Roman 10",
    size = 10,
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

g.MODEL1234.DOMSHOCK.CH <- 
  plot_grid(g.MODEL1.DOMSHOCK.NP.CH,
            g.MODEL2.DOMSHOCK.NP.CH,
            g.MODEL3.DOMSHOCK.NP.CH,
            g.MODEL4.DOMSHOCK.NP.CH,
            ncol = 4, nrow = 1, align = "hv",
            rel_widths = c(0.25, 0.25, 0.25, 0.25))

g.MODEL1234.DOMSHOCK.CH <- 
  plot_grid(
    title, g.MODEL1234.DOMSHOCK.CH,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

ggsave(filename = "g.MODEL1234.DOMSHOCK.CH.png", g.MODEL1234.DOMSHOCK.CH,
       width = 9, height = 2.5, dpi = 300, units = "in", device='png', bg = "white")

#### PLOT THEM TOGETHER (CH + BR) ####

g.DOMSHOCK <- 
  plot_grid(g.MODEL1234.DOMSHOCK.CH, 
            g.MODEL13.DOMSHOCK.BR, 
            nrow = 2, align = "hv")

ggsave(filename = "g.DOMSHOCK.png", g.DOMSHOCK,
       width = 9, height = 5, dpi = 300, units = "in", device='png', bg = "white")
