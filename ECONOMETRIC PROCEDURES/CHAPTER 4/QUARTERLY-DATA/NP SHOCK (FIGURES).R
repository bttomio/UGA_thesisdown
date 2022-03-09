RNGkind("L'Ecuyer-CMRG")
set.seed(123)
library(BGVAR)
options(scipen=999)
library(tidyverse)
library(cowplot)

## SHOCKS
## NP SHOCK - IMPACT ON DOMESTIC ECONOMY ####
# FOR TARGET CURRENCIES (BR, MX, RU) -> +NP (LONG>SHORT) ####
# Name = NPHIGHER.TARGET
# Higher NP shock - GIRF

# BR ####
## MODEL 1 (model1t.br.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"BR.NP"
shockinfo_girf$scale<- 0.5

irf.girf.model1t.br.ssvs.NPHIGHER.TARGET.BR <- 
  irf(model1t.br.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 12,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# GDP
MODEL1.NPHIGHER.TARGET.GDP.BR <- 
  plot(irf.girf.model1t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.GDP", shock = "BR.NP")

g.MODEL1.NPHIGHER.TARGET.GDP.BR <-
  MODEL1.NPHIGHER.TARGET.GDP.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL1.NPHIGHER.TARGET.GDP.BR[-1,]))) %>%
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
  labs(title = expression(paste(italic("GDP"))))

# RES
MODEL1.NPHIGHER.TARGET.RES.BR <- 
  plot(irf.girf.model1t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.RES", shock = "BR.NP")

g.MODEL1.NPHIGHER.TARGET.RES.BR <-
  MODEL1.NPHIGHER.TARGET.RES.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL1.NPHIGHER.TARGET.RES.BR[-1,]))) %>%
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
  labs(title = expression(paste(italic("RES"))))

# ER
MODEL1.NPHIGHER.TARGET.ER.BR <- 
  plot(irf.girf.model1t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.ER", shock = "BR.NP")

g.MODEL1.NPHIGHER.TARGET.ER.BR <-
  MODEL1.NPHIGHER.TARGET.ER.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL1.NPHIGHER.TARGET.ER.BR[-1,]))) %>%
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
  labs(title = expression(paste(italic("ER"))))

# EQ
MODEL1.NPHIGHER.TARGET.EQ.BR <- 
  plot(irf.girf.model1t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.EQ", shock = "BR.NP")

g.MODEL1.NPHIGHER.TARGET.EQ.BR <-
  MODEL1.NPHIGHER.TARGET.EQ.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL1.NPHIGHER.TARGET.EQ.BR[-1,]))) %>%
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
  labs(title = expression(paste(italic("EQ"))))

# IR
MODEL1.NPHIGHER.TARGET.IR.BR <- 
  plot(irf.girf.model1t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.IR", shock = "BR.NP")

g.MODEL1.NPHIGHER.TARGET.IR.BR <-
  MODEL1.NPHIGHER.TARGET.IR.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL1.NPHIGHER.TARGET.IR.BR[-1,]))) %>%
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
  labs(title = expression(paste(bold("Model (1) | "), bold(italic("VIX")))))

# PLOT THEM TOGETHER

label <- substitute(paste(bold("Model (1) | "), bold(italic("VIX"))))

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

g.MODEL1.NPHIGHER.TARGET.BR <- 
  plot_grid(g.MODEL1.NPHIGHER.TARGET.GDP.BR,
            g.MODEL1.NPHIGHER.TARGET.RES.BR,
            g.MODEL1.NPHIGHER.TARGET.ER.BR,
            g.MODEL1.NPHIGHER.TARGET.EQ.BR,
            ncol = 4, nrow = 1, align = "hv")

g.MODEL1.NPHIGHER.TARGET.BR <- 
  plot_grid(
    title, g.MODEL1.NPHIGHER.TARGET.BR,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

ggsave(filename = "g.MODEL1.NPHIGHER.TARGET.BR.png", g.MODEL1.NPHIGHER.TARGET.BR,
       width = 9, height = 2.5, dpi = 300, units = "in", device='png', bg = "white")

## MODEL 3 (model3t.br.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"BR.NP"
shockinfo_girf$scale<- 0.5

irf.girf.model3t.br.ssvs.NPHIGHER.TARGET.BR <- 
  irf(model3t.br.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 12,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# C
MODEL3.NPHIGHER.TARGET.C.BR <- 
  plot(irf.girf.model3t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.C", shock = "BR.NP")

g.MODEL3.NPHIGHER.TARGET.C.BR <-
  MODEL3.NPHIGHER.TARGET.C.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPHIGHER.TARGET.C.BR[-1,]))) %>%
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
  labs(title = expression(paste(italic("C"))))

# GFCF
MODEL3.NPHIGHER.TARGET.GFCF.BR <- 
  plot(irf.girf.model3t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.GFCF", shock = "BR.NP")

g.MODEL3.NPHIGHER.TARGET.GFCF.BR <-
  MODEL3.NPHIGHER.TARGET.GFCF.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPHIGHER.TARGET.GFCF.BR[-1,]))) %>%
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
  labs(title = expression(paste(italic("GFCF"))))

# X
MODEL3.NPHIGHER.TARGET.X.BR <- 
  plot(irf.girf.model3t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.X", shock = "BR.NP")

g.MODEL3.NPHIGHER.TARGET.X.BR <-
  MODEL3.NPHIGHER.TARGET.X.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPHIGHER.TARGET.X.BR[-1,]))) %>%
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
  labs(title = expression(paste(italic("X"))))

# M
MODEL3.NPHIGHER.TARGET.M.BR <- 
  plot(irf.girf.model3t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.M", shock = "BR.NP")

g.MODEL3.NPHIGHER.TARGET.M.BR <-
  MODEL3.NPHIGHER.TARGET.M.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPHIGHER.TARGET.M.BR[-1,]))) %>%
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
  labs(title = expression(paste(italic("M"))))

# RES
MODEL3.NPHIGHER.TARGET.RES.BR <- 
  plot(irf.girf.model3t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.RES", shock = "BR.NP")

g.MODEL3.NPHIGHER.TARGET.RES.BR <-
  MODEL3.NPHIGHER.TARGET.RES.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPHIGHER.TARGET.RES.BR[-1,]))) %>%
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
  labs(title = expression(paste(italic("RES"))))

# ER
MODEL3.NPHIGHER.TARGET.ER.BR <- 
  plot(irf.girf.model3t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.ER", shock = "BR.NP")

g.MODEL3.NPHIGHER.TARGET.ER.BR <-
  MODEL3.NPHIGHER.TARGET.ER.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPHIGHER.TARGET.ER.BR[-1,]))) %>%
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
  labs(title = expression(paste(italic("ER"))))

# EQ
MODEL3.NPHIGHER.TARGET.EQ.BR <- 
  plot(irf.girf.model3t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.EQ", shock = "BR.NP")

g.MODEL3.NPHIGHER.TARGET.EQ.BR <-
  MODEL3.NPHIGHER.TARGET.EQ.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPHIGHER.TARGET.EQ.BR[-1,]))) %>%
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
  labs(title = expression(paste(italic("EQ"))))

# IR
MODEL3.NPHIGHER.TARGET.IR.BR <- 
  plot(irf.girf.model3t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.IR", shock = "BR.NP")

g.MODEL3.NPHIGHER.TARGET.IR.BR <-
  MODEL3.NPHIGHER.TARGET.IR.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPHIGHER.TARGET.IR.BR[-1,]))) %>%
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
  labs(title = expression(paste(bold("Model (3) | "), bold(italic("VIX")))))

# PLOT THEM TOGETHER

label <- substitute(paste(bold("Model (3) | "), bold(italic("VIX"))))

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

g.MODEL3.NPHIGHER.TARGET.BR.1 <- 
  plot_grid(g.MODEL3.NPHIGHER.TARGET.C.BR,
            g.MODEL3.NPHIGHER.TARGET.GFCF.BR,
            g.MODEL3.NPHIGHER.TARGET.X.BR,
            g.MODEL3.NPHIGHER.TARGET.M.BR,
            ncol = 4, nrow = 1, align = "hv")

g.MODEL3.NPHIGHER.TARGET.BR.2 <- 
  plot_grid(NULL,
            g.MODEL3.NPHIGHER.TARGET.RES.BR,
            g.MODEL3.NPHIGHER.TARGET.ER.BR,
            g.MODEL3.NPHIGHER.TARGET.EQ.BR,
            NULL,
            ncol = 5, nrow = 1, align = "hv",
            rel_widths = c(0.125, 0.25, 0.25, 0.25, 0.125))

g.MODEL3.NPHIGHER.TARGET.BR <- 
  plot_grid(g.MODEL3.NPHIGHER.TARGET.BR.1,
            g.MODEL3.NPHIGHER.TARGET.BR.2,
            nrow = 2, align = "hv")

g.MODEL3.NPHIGHER.TARGET.BR <- 
  plot_grid(
    title, g.MODEL3.NPHIGHER.TARGET.BR,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

ggsave(filename = "g.MODEL3.NPHIGHER.TARGET.BR.png", g.MODEL3.NPHIGHER.TARGET.BR,
       width = 9, height = 5, dpi = 300, units = "in", device='png', bg = "white")

### PLOT THEM TOGETHER (MODEL 1 + 3) ####

g.MODEL13.NPHIGHER.TARGET.BR <- 
  plot_grid(g.MODEL1.NPHIGHER.TARGET.BR, 
            g.MODEL3.NPHIGHER.TARGET.BR, 
            nrow = 2, align = "hv",
            rel_heights = c(0.5,1))

ggsave(filename = "g.MODEL13.NPHIGHER.TARGET.BR.png", g.MODEL13.NPHIGHER.TARGET.BR,
       width = 9, height = 8, dpi = 300, units = "in", device='png', bg = "white")

### IR - PLOT THEM TOGETHER (MODEL 1 + 3) ####

g.MODEL13.NPHIGHER.TARGET.IR.BR <- 
  plot_grid(NULL, g.MODEL1.NPHIGHER.TARGET.IR.BR, g.MODEL3.NPHIGHER.TARGET.IR.BR, NULL,
            nrow = 1, align = "hv", rel_widths = c(0.25, 0.25, 0.25, 0.25))

ggsave(filename = "g.MODEL13.NPHIGHER.TARGET.IR.BR.png", g.MODEL13.NPHIGHER.TARGET.IR.BR,
       width = 9, height = 2.5, dpi = 300, units = "in", device='png', bg = "white")

# FOR FUNDING CURRENCIES (CA, CH, GB, JP, US, U2) -> -NP (LONG<SHORT) ####
# Name = NPLOWER.FUND
# Lower NP shock - GIRF

# CH ####
## MODEL 1 (model1t.ch.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"CH.NP"
shockinfo_girf$scale<- -0.5

irf.girf.model1t.ch.ssvs.NPLOWER.FUND.CH <- 
  irf(model1t.ch.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 12,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# GDP
MODEL1.NPLOWER.FUND.GDP.CH <- 
  plot(irf.girf.model1t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.GDP", shock = "CH.NP")

g.MODEL1.NPLOWER.FUND.GDP.CH <-
  MODEL1.NPLOWER.FUND.GDP.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL1.NPLOWER.FUND.GDP.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("GDP"))))

# RES
MODEL1.NPLOWER.FUND.RES.CH <- 
  plot(irf.girf.model1t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.RES", shock = "CH.NP")

g.MODEL1.NPLOWER.FUND.RES.CH <-
  MODEL1.NPLOWER.FUND.RES.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL1.NPLOWER.FUND.RES.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("RES"))))

# ER
MODEL1.NPLOWER.FUND.ER.CH <- 
  plot(irf.girf.model1t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.ER", shock = "CH.NP")

g.MODEL1.NPLOWER.FUND.ER.CH <-
  MODEL1.NPLOWER.FUND.ER.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL1.NPLOWER.FUND.ER.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("ER"))))

# EQ
MODEL1.NPLOWER.FUND.EQ.CH <- 
  plot(irf.girf.model1t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.EQ", shock = "CH.NP")

g.MODEL1.NPLOWER.FUND.EQ.CH <-
  MODEL1.NPLOWER.FUND.EQ.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL1.NPLOWER.FUND.EQ.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("EQ"))))

# IR
MODEL1.NPLOWER.FUND.IR.CH <- 
  plot(irf.girf.model1t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.IR", shock = "CH.NP")

g.MODEL1.NPLOWER.FUND.IR.CH <-
  MODEL1.NPLOWER.FUND.IR.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL1.NPLOWER.FUND.IR.CH[-1,]))) %>%
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
        axis.text.y.left = element_text(size = 4, angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (1) | "), bold(italic("VIX")))))

# PLOT THEM TOGETHER

label <- substitute(paste(bold("Model (1) | "), bold(italic("VIX"))))

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

g.MODEL1.NPLOWER.FUND.CH <- 
  plot_grid(g.MODEL1.NPLOWER.FUND.GDP.CH,
            g.MODEL1.NPLOWER.FUND.RES.CH,
            g.MODEL1.NPLOWER.FUND.ER.CH,
            g.MODEL1.NPLOWER.FUND.EQ.CH,
            ncol = 4, nrow = 1, align = "hv")

g.MODEL1.NPLOWER.FUND.CH <- 
  plot_grid(
    title, g.MODEL1.NPLOWER.FUND.CH,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
)

## MODEL 2 (model2t.ch.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"CH.NP"
shockinfo_girf$scale<- -0.5

irf.girf.model2t.ch.ssvs.NPLOWER.FUND.CH <- 
  irf(model2t.ch.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 12,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# GDP
MODEL2.NPLOWER.FUND.GDP.CH <- 
  plot(irf.girf.model2t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.GDP", shock = "CH.NP")

g.MODEL2.NPLOWER.FUND.GDP.CH <-
  MODEL2.NPLOWER.FUND.GDP.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL2.NPLOWER.FUND.GDP.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("GDP"))))

# RES
MODEL2.NPLOWER.FUND.RES.CH <- 
  plot(irf.girf.model2t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.RES", shock = "CH.NP")

g.MODEL2.NPLOWER.FUND.RES.CH <-
  MODEL2.NPLOWER.FUND.RES.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL2.NPLOWER.FUND.RES.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("RES"))))

# ER
MODEL2.NPLOWER.FUND.ER.CH <- 
  plot(irf.girf.model2t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.ER", shock = "CH.NP")

g.MODEL2.NPLOWER.FUND.ER.CH <-
  MODEL2.NPLOWER.FUND.ER.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL2.NPLOWER.FUND.ER.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("ER"))))

# EQ
MODEL2.NPLOWER.FUND.EQ.CH <- 
  plot(irf.girf.model2t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.EQ", shock = "CH.NP")

g.MODEL2.NPLOWER.FUND.EQ.CH <-
  MODEL2.NPLOWER.FUND.EQ.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL2.NPLOWER.FUND.EQ.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("EQ"))))

# IR
MODEL2.NPLOWER.FUND.IR.CH <- 
  plot(irf.girf.model2t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.IR", shock = "CH.NP")

g.MODEL2.NPLOWER.FUND.IR.CH <-
  MODEL2.NPLOWER.FUND.IR.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL2.NPLOWER.FUND.IR.CH[-1,]))) %>%
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
  labs(title = expression(paste(bold("Model (2) | "), bold(italic("GCF")))))

# PLOT THEM TOGETHER

label <- substitute(paste(bold("Model (2) | "), bold(italic("GCF"))))

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

g.MODEL2.NPLOWER.FUND.CH <- 
  plot_grid(g.MODEL2.NPLOWER.FUND.GDP.CH,
            g.MODEL2.NPLOWER.FUND.RES.CH,
            g.MODEL2.NPLOWER.FUND.ER.CH,
            g.MODEL2.NPLOWER.FUND.EQ.CH,
            ncol = 4, nrow = 1, align = "hv")

g.MODEL2.NPLOWER.FUND.CH <- 
  plot_grid(
    title, g.MODEL2.NPLOWER.FUND.CH,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

### PLOT THEM TOGETHER (MODEL 1 + 2) ####

g.MODEL12.NPLOWER.FUND.CH <- 
  plot_grid(g.MODEL1.NPLOWER.FUND.CH, 
            g.MODEL2.NPLOWER.FUND.CH, 
            nrow = 2, align = "hv")

ggsave(filename = "g.MODEL12.NPLOWER.FUND.CH.png", g.MODEL12.NPLOWER.FUND.CH,
       width = 9, height = 5, dpi = 300, units = "in", device='png', bg = "white")

## MODEL 3 (model3t.ch.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"CH.NP"
shockinfo_girf$scale<- -0.5

irf.girf.model3t.ch.ssvs.NPLOWER.FUND.CH <- 
  irf(model3t.ch.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 12,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# C
MODEL3.NPLOWER.FUND.C.CH <- 
  plot(irf.girf.model3t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.C", shock = "CH.NP")

g.MODEL3.NPLOWER.FUND.C.CH <-
  MODEL3.NPLOWER.FUND.C.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPLOWER.FUND.C.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("C"))))

# GFCF
MODEL3.NPLOWER.FUND.GFCF.CH <- 
  plot(irf.girf.model3t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.GFCF", shock = "CH.NP")

g.MODEL3.NPLOWER.FUND.GFCF.CH <-
  MODEL3.NPLOWER.FUND.GFCF.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPLOWER.FUND.GFCF.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("GFCF"))))

# X
MODEL3.NPLOWER.FUND.X.CH <- 
  plot(irf.girf.model3t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.X", shock = "CH.NP")

g.MODEL3.NPLOWER.FUND.X.CH <-
  MODEL3.NPLOWER.FUND.X.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPLOWER.FUND.X.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("X"))))

# M
MODEL3.NPLOWER.FUND.M.CH <- 
  plot(irf.girf.model3t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.M", shock = "CH.NP")

g.MODEL3.NPLOWER.FUND.M.CH <-
  MODEL3.NPLOWER.FUND.M.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPLOWER.FUND.M.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("M"))))

# RES
MODEL3.NPLOWER.FUND.RES.CH <- 
  plot(irf.girf.model3t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.RES", shock = "CH.NP")

g.MODEL3.NPLOWER.FUND.RES.CH <-
  MODEL3.NPLOWER.FUND.RES.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPLOWER.FUND.RES.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("RES"))))

# ER
MODEL3.NPLOWER.FUND.ER.CH <- 
  plot(irf.girf.model3t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.ER", shock = "CH.NP")

g.MODEL3.NPLOWER.FUND.ER.CH <-
  MODEL3.NPLOWER.FUND.ER.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPLOWER.FUND.ER.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("ER"))))

# EQ
MODEL3.NPLOWER.FUND.EQ.CH <- 
  plot(irf.girf.model3t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.EQ", shock = "CH.NP")

g.MODEL3.NPLOWER.FUND.EQ.CH <-
  MODEL3.NPLOWER.FUND.EQ.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPLOWER.FUND.EQ.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("EQ"))))

# IR
MODEL3.NPLOWER.FUND.IR.CH <- 
  plot(irf.girf.model3t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.IR", shock = "CH.NP")

g.MODEL3.NPLOWER.FUND.IR.CH <-
  MODEL3.NPLOWER.FUND.IR.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL3.NPLOWER.FUND.IR.CH[-1,]))) %>%
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
        axis.text.y.left = element_text(angle = 90, hjust = 0.5, size = 6),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (3) | "), bold(italic("VIX")))))

# PLOT THEM TOGETHER

label <- substitute(paste(bold("Model (3) | "), bold(italic("VIX"))))

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

g.MODEL3.NPLOWER.FUND.CH.1 <- 
  plot_grid(g.MODEL3.NPLOWER.FUND.C.CH,
            g.MODEL3.NPLOWER.FUND.GFCF.CH,
            g.MODEL3.NPLOWER.FUND.X.CH,
            g.MODEL3.NPLOWER.FUND.M.CH,
            ncol = 4, nrow = 1, align = "hv")

g.MODEL3.NPLOWER.FUND.CH.2 <- 
  plot_grid(NULL,
            g.MODEL3.NPLOWER.FUND.RES.CH,
            g.MODEL3.NPLOWER.FUND.ER.CH,
            g.MODEL3.NPLOWER.FUND.EQ.CH,
            NULL,
            ncol = 5, nrow = 1, align = "hv",
            rel_widths = c(0.125, 0.25, 0.25, 0.25, 0.125))

g.MODEL3.NPLOWER.FUND.CH <- 
  plot_grid(g.MODEL3.NPLOWER.FUND.CH.1,
            g.MODEL3.NPLOWER.FUND.CH.2,
            nrow = 2, align = "hv")

g.MODEL3.NPLOWER.FUND.CH <- 
  plot_grid(
    title, g.MODEL3.NPLOWER.FUND.CH,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

ggsave(filename = "g.MODEL3.NPLOWER.FUND.CH.png", g.MODEL3.NPLOWER.FUND.CH,
       width = 9, height = 5, dpi = 300, units = "in", device='png', bg = "white")


## MODEL 4 (model4t.ch.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"CH.NP"
shockinfo_girf$scale<- -0.5

irf.girf.model4t.ch.ssvs.NPLOWER.FUND.CH <- 
  irf(model4t.ch.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 12,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# C
MODEL4.NPLOWER.FUND.C.CH <- 
  plot(irf.girf.model4t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.C", shock = "CH.NP")

g.MODEL4.NPLOWER.FUND.C.CH <-
  MODEL4.NPLOWER.FUND.C.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL4.NPLOWER.FUND.C.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("C"))))

# GFCF
MODEL4.NPLOWER.FUND.GFCF.CH <- 
  plot(irf.girf.model4t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.GFCF", shock = "CH.NP")

g.MODEL4.NPLOWER.FUND.GFCF.CH <-
  MODEL4.NPLOWER.FUND.GFCF.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL4.NPLOWER.FUND.GFCF.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("GFCF"))))

# X
MODEL4.NPLOWER.FUND.X.CH <- 
  plot(irf.girf.model4t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.X", shock = "CH.NP")

g.MODEL4.NPLOWER.FUND.X.CH <-
  MODEL4.NPLOWER.FUND.X.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL4.NPLOWER.FUND.X.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("X"))))

# M
MODEL4.NPLOWER.FUND.M.CH <- 
  plot(irf.girf.model4t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.M", shock = "CH.NP")

g.MODEL4.NPLOWER.FUND.M.CH <-
  MODEL4.NPLOWER.FUND.M.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL4.NPLOWER.FUND.M.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("M"))))

# RES
MODEL4.NPLOWER.FUND.RES.CH <- 
  plot(irf.girf.model4t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.RES", shock = "CH.NP")

g.MODEL4.NPLOWER.FUND.RES.CH <-
  MODEL4.NPLOWER.FUND.RES.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL4.NPLOWER.FUND.RES.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("RES"))))

# ER
MODEL4.NPLOWER.FUND.ER.CH <- 
  plot(irf.girf.model4t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.ER", shock = "CH.NP")

g.MODEL4.NPLOWER.FUND.ER.CH <-
  MODEL4.NPLOWER.FUND.ER.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL4.NPLOWER.FUND.ER.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("ER"))))

# EQ
MODEL4.NPLOWER.FUND.EQ.CH <- 
  plot(irf.girf.model4t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.EQ", shock = "CH.NP")

g.MODEL4.NPLOWER.FUND.EQ.CH <-
  MODEL4.NPLOWER.FUND.EQ.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL4.NPLOWER.FUND.EQ.CH[-1,]))) %>%
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
  labs(title = expression(paste(italic("EQ"))))

# IR
MODEL4.NPLOWER.FUND.IR.CH <- 
  plot(irf.girf.model4t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.IR", shock = "CH.NP")

g.MODEL4.NPLOWER.FUND.IR.CH <-
  MODEL4.NPLOWER.FUND.IR.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL4.NPLOWER.FUND.IR.CH[-1,]))) %>%
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
  labs(title = expression(paste(bold("Model (4) | "), bold(italic("GCF")))))

# PLOT THEM TOGETHER

label <- substitute(paste(bold("Model (4) | "), bold(italic("GCF"))))

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

g.MODEL4.NPLOWER.FUND.CH.1 <- 
  plot_grid(g.MODEL4.NPLOWER.FUND.C.CH,
            g.MODEL4.NPLOWER.FUND.GFCF.CH,
            g.MODEL4.NPLOWER.FUND.X.CH,
            g.MODEL4.NPLOWER.FUND.M.CH,
            ncol = 4, nrow = 1, align = "hv")

g.MODEL4.NPLOWER.FUND.CH.2 <- 
  plot_grid(NULL,
            g.MODEL4.NPLOWER.FUND.RES.CH,
            g.MODEL4.NPLOWER.FUND.ER.CH,
            g.MODEL4.NPLOWER.FUND.EQ.CH,
            NULL,
            ncol = 5, nrow = 1, align = "hv",
            rel_widths = c(0.125, 0.25, 0.25, 0.25, 0.125))

g.MODEL4.NPLOWER.FUND.CH <- 
  plot_grid(g.MODEL4.NPLOWER.FUND.CH.1,
            g.MODEL4.NPLOWER.FUND.CH.2,
            nrow = 2, align = "hv")

g.MODEL4.NPLOWER.FUND.CH <- 
  plot_grid(
    title, g.MODEL4.NPLOWER.FUND.CH,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

ggsave(filename = "g.MODEL4.NPLOWER.FUND.CH.png", g.MODEL4.NPLOWER.FUND.CH,
       width = 9, height = 5, dpi = 300, units = "in", device='png', bg = "white")


### PLOT THEM TOGETHER (MODEL 3 + 4) ####

g.MODEL34.NPLOWER.FUND.CH <- 
  plot_grid(g.MODEL3.NPLOWER.FUND.CH, 
            g.MODEL4.NPLOWER.FUND.CH, 
            nrow = 2, align = "hv")

ggsave(filename = "g.MODEL34.NPLOWER.FUND.CH.png", g.MODEL34.NPLOWER.FUND.CH,
       width = 9, height = 10, dpi = 300, units = "in", device='png', bg = "white")

### IR - PLOT THEM TOGETHER (MODEL 1 + 2 + 3 + 4) ####

g.MODEL1234.NPLOWER.FUND.IR.CH <- 
  plot_grid(g.MODEL1.NPLOWER.FUND.IR.CH, g.MODEL2.NPLOWER.FUND.IR.CH, 
            g.MODEL3.NPLOWER.FUND.IR.CH, g.MODEL4.NPLOWER.FUND.IR.CH,
            nrow = 1, align = "hv", rel_widths = c(0.25, 0.25, 0.25, 0.25))

ggsave(filename = "g.MODEL1234.NPLOWER.FUND.IR.CH.png", g.MODEL1234.NPLOWER.FUND.IR.CH,
       width = 9, height = 2.5, dpi = 300, units = "in", device='png', bg = "white")

