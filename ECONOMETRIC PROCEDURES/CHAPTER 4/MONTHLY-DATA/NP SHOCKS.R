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
## MODEL 5 (MODEL 5 - BOTH - XM, RES, VIX (DATA).R -> model5t.br.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"BR.NP"
shockinfo_girf$scale<- 0.5

irf.girf.model5t.br.ssvs.NPHIGHER.TARGET.BR <- 
  irf(model5t.br.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 24,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# X
MODEL5.NPHIGHER.TARGET.X.BR <- 
  plot(irf.girf.model5t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.X", shock = "BR.NP")

g.MODEL5.NPHIGHER.TARGET.X.BR <-
  MODEL5.NPHIGHER.TARGET.X.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL5.NPHIGHER.TARGET.X.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL5.NPHIGHER.TARGET.M.BR <- 
  plot(irf.girf.model5t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.M", shock = "BR.NP")

g.MODEL5.NPHIGHER.TARGET.M.BR <-
  MODEL5.NPHIGHER.TARGET.M.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL5.NPHIGHER.TARGET.M.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL5.NPHIGHER.TARGET.RES.BR <- 
  plot(irf.girf.model5t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.RES", shock = "BR.NP")

g.MODEL5.NPHIGHER.TARGET.RES.BR <-
  MODEL5.NPHIGHER.TARGET.RES.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL5.NPHIGHER.TARGET.RES.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL5.NPHIGHER.TARGET.ER.BR <- 
  plot(irf.girf.model5t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.ER", shock = "BR.NP")

g.MODEL5.NPHIGHER.TARGET.ER.BR <-
  MODEL5.NPHIGHER.TARGET.ER.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL5.NPHIGHER.TARGET.ER.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL5.NPHIGHER.TARGET.EQ.BR <- 
  plot(irf.girf.model5t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.EQ", shock = "BR.NP")

g.MODEL5.NPHIGHER.TARGET.EQ.BR <-
  MODEL5.NPHIGHER.TARGET.EQ.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL5.NPHIGHER.TARGET.EQ.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL5.NPHIGHER.TARGET.IR.BR <- 
  plot(irf.girf.model5t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.IR", shock = "BR.NP")

g.MODEL5.NPHIGHER.TARGET.IR.BR <-
  MODEL5.NPHIGHER.TARGET.IR.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL5.NPHIGHER.TARGET.IR.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (5) | "), bold(italic("VIX")))))

# PLOT THEM TOGETHER

label <- substitute(paste(bold("Model (5) | "), bold(italic("VIX"))))

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
    plot.margin = margin(0, 0, 0, 90)
  )

g.MODEL5.NPHIGHER.TARGET.BR.1 <- 
  plot_grid(NULL,
            g.MODEL5.NPHIGHER.TARGET.X.BR,
            g.MODEL5.NPHIGHER.TARGET.M.BR,
            g.MODEL5.NPHIGHER.TARGET.RES.BR,
            NULL,
            ncol = 5, nrow = 1, align = "hv",
            rel_widths = c(0.125, 0.25, 0.25, 0.25, 0.125))

g.MODEL5.NPHIGHER.TARGET.BR.2 <- 
  plot_grid(NULL,
            g.MODEL5.NPHIGHER.TARGET.ER.BR,
            g.MODEL5.NPHIGHER.TARGET.EQ.BR,
            NULL,
            ncol = 4, nrow = 1, align = "hv",
            rel_widths = c(0.25, 0.25, 0.25, 0.25))

g.MODEL5.NPHIGHER.TARGET.BR <- 
  plot_grid(g.MODEL5.NPHIGHER.TARGET.BR.1,
            g.MODEL5.NPHIGHER.TARGET.BR.2,
            nrow = 2, align = "hv")

g.MODEL5.NPHIGHER.TARGET.BR <- 
  plot_grid(
    title, g.MODEL5.NPHIGHER.TARGET.BR,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

ggsave(filename = "g.MODEL5.NPHIGHER.TARGET.BR.png", g.MODEL5.NPHIGHER.TARGET.BR,
       width = 9, height = 5, dpi = 300, units = "in", device='png', bg = "white")

## MODEL 6 (MODEL 6 - BOTH - XM, RES, GFC (DATA).R -> model6t.br.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"BR.NP"
shockinfo_girf$scale<- 0.5

irf.girf.model6t.br.ssvs.NPHIGHER.TARGET.BR <- 
  irf(model6t.br.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 24,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# X
MODEL6.NPHIGHER.TARGET.X.BR <- 
  plot(irf.girf.model6t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.X", shock = "BR.NP")

g.MODEL6.NPHIGHER.TARGET.X.BR <-
  MODEL6.NPHIGHER.TARGET.X.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL6.NPHIGHER.TARGET.X.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL6.NPHIGHER.TARGET.M.BR <- 
  plot(irf.girf.model6t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.M", shock = "BR.NP")

g.MODEL6.NPHIGHER.TARGET.M.BR <-
  MODEL6.NPHIGHER.TARGET.M.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL6.NPHIGHER.TARGET.M.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL6.NPHIGHER.TARGET.RES.BR <- 
  plot(irf.girf.model6t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.RES", shock = "BR.NP")

g.MODEL6.NPHIGHER.TARGET.RES.BR <-
  MODEL6.NPHIGHER.TARGET.RES.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL6.NPHIGHER.TARGET.RES.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL6.NPHIGHER.TARGET.ER.BR <- 
  plot(irf.girf.model6t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.ER", shock = "BR.NP")

g.MODEL6.NPHIGHER.TARGET.ER.BR <-
  MODEL6.NPHIGHER.TARGET.ER.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL6.NPHIGHER.TARGET.ER.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL6.NPHIGHER.TARGET.EQ.BR <- 
  plot(irf.girf.model6t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.EQ", shock = "BR.NP")

g.MODEL6.NPHIGHER.TARGET.EQ.BR <-
  MODEL6.NPHIGHER.TARGET.EQ.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL6.NPHIGHER.TARGET.EQ.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL6.NPHIGHER.TARGET.IR.BR <- 
  plot(irf.girf.model6t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.IR", shock = "BR.NP")

g.MODEL6.NPHIGHER.TARGET.IR.BR <-
  MODEL6.NPHIGHER.TARGET.IR.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL6.NPHIGHER.TARGET.IR.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (6) | "), bold(italic("GCF")))))

# PLOT THEM TOGETHER

label <- substitute(paste(bold("Model (6) | "), bold(italic("GCF"))))

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
    plot.margin = margin(0, 0, 0, 90)
  )

g.MODEL6.NPHIGHER.TARGET.BR.1 <- 
  plot_grid(NULL,
            g.MODEL6.NPHIGHER.TARGET.X.BR,
            g.MODEL6.NPHIGHER.TARGET.M.BR,
            g.MODEL6.NPHIGHER.TARGET.RES.BR,
            NULL,
            ncol = 5, nrow = 1, align = "hv",
            rel_widths = c(0.125, 0.25, 0.25, 0.25, 0.125))

g.MODEL6.NPHIGHER.TARGET.BR.2 <- 
  plot_grid(NULL,
            g.MODEL6.NPHIGHER.TARGET.ER.BR,
            g.MODEL6.NPHIGHER.TARGET.EQ.BR,
            NULL,
            ncol = 4, nrow = 1, align = "hv",
            rel_widths = c(0.25, 0.25, 0.25, 0.25))

g.MODEL6.NPHIGHER.TARGET.BR <- 
  plot_grid(g.MODEL6.NPHIGHER.TARGET.BR.1,
            g.MODEL6.NPHIGHER.TARGET.BR.2,
            nrow = 2, align = "hv")

g.MODEL6.NPHIGHER.TARGET.BR <- 
  plot_grid(
    title, g.MODEL6.NPHIGHER.TARGET.BR,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

ggsave(filename = "g.MODEL6.NPHIGHER.TARGET.BR.png", g.MODEL6.NPHIGHER.TARGET.BR,
       width = 9, height = 5, dpi = 300, units = "in", device='png', bg = "white")

### PLOT THEM TOGETHER (MODEL 5 + 6) ####

g.MODEL56.NPHIGHER.TARGET.BR <- 
  plot_grid(g.MODEL5.NPHIGHER.TARGET.BR, 
            g.MODEL6.NPHIGHER.TARGET.BR, 
            nrow = 2, align = "hv")

ggsave(filename = "g.MODEL56.NPHIGHER.TARGET.BR.png", g.MODEL56.NPHIGHER.TARGET.BR,
       width = 9, height = 10, dpi = 300, units = "in", device='png', bg = "white")

## MODEL 7 (MODEL 1 - BR - IP, RES, VIX (DATA).R -> model7t.br.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"BR.NP"
shockinfo_girf$scale<- 0.5

irf.girf.model7t.br.ssvs.NPHIGHER.TARGET.BR <- 
  irf(model7t.br.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 24,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# X
MODEL7.NPHIGHER.TARGET.X.BR <- 
  plot(irf.girf.model7t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.X", shock = "BR.NP")

g.MODEL7.NPHIGHER.TARGET.X.BR <-
  MODEL7.NPHIGHER.TARGET.X.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL7.NPHIGHER.TARGET.X.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL7.NPHIGHER.TARGET.M.BR <- 
  plot(irf.girf.model7t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.M", shock = "BR.NP")

g.MODEL7.NPHIGHER.TARGET.M.BR <-
  MODEL7.NPHIGHER.TARGET.M.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL7.NPHIGHER.TARGET.M.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL7.NPHIGHER.TARGET.RES.BR <- 
  plot(irf.girf.model7t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.RES", shock = "BR.NP")

g.MODEL7.NPHIGHER.TARGET.RES.BR <-
  MODEL7.NPHIGHER.TARGET.RES.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL7.NPHIGHER.TARGET.RES.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL7.NPHIGHER.TARGET.ER.BR <- 
  plot(irf.girf.model7t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.ER", shock = "BR.NP")

g.MODEL7.NPHIGHER.TARGET.ER.BR <-
  MODEL7.NPHIGHER.TARGET.ER.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL7.NPHIGHER.TARGET.ER.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL7.NPHIGHER.TARGET.EQ.BR <- 
  plot(irf.girf.model7t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.EQ", shock = "BR.NP")

g.MODEL7.NPHIGHER.TARGET.EQ.BR <-
  MODEL7.NPHIGHER.TARGET.EQ.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL7.NPHIGHER.TARGET.EQ.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("EQ"))))

# IP
MODEL7.NPHIGHER.TARGET.IP.BR <- 
  plot(irf.girf.model7t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.IP", shock = "BR.NP")

g.MODEL7.NPHIGHER.TARGET.IP.BR <-
  MODEL7.NPHIGHER.TARGET.IP.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL7.NPHIGHER.TARGET.IP.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("IP"))))

# IR
MODEL7.NPHIGHER.TARGET.IR.BR <- 
  plot(irf.girf.model7t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.IR", shock = "BR.NP")

g.MODEL7.NPHIGHER.TARGET.IR.BR <-
  MODEL7.NPHIGHER.TARGET.IR.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL7.NPHIGHER.TARGET.IR.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (7) | "), bold(italic("VIX")))))

# PLOT THEM TOGETHER

label <- substitute(paste(bold("Model (7) | "), bold(italic("VIX"))))

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
    plot.margin = margin(0, 0, 0, 90)
  )

g.MODEL7.NPHIGHER.TARGET.BR.1 <- 
  plot_grid(NULL,
            g.MODEL7.NPHIGHER.TARGET.X.BR,
            g.MODEL7.NPHIGHER.TARGET.M.BR,
            g.MODEL7.NPHIGHER.TARGET.RES.BR,
            NULL,
            ncol = 5, nrow = 1, align = "hv",
            rel_widths = c(0.125, 0.25, 0.25, 0.25, 0.125))

g.MODEL7.NPHIGHER.TARGET.BR.2 <- 
  plot_grid(NULL,
            g.MODEL7.NPHIGHER.TARGET.ER.BR,
            g.MODEL7.NPHIGHER.TARGET.EQ.BR,
            g.MODEL7.NPHIGHER.TARGET.IP.BR,
            NULL,
            ncol = 5, nrow = 1, align = "hv",
            rel_widths = c(0.125, 0.25, 0.25, 0.25, 0.125))

g.MODEL7.NPHIGHER.TARGET.BR <- 
  plot_grid(g.MODEL7.NPHIGHER.TARGET.BR.1,
            g.MODEL7.NPHIGHER.TARGET.BR.2,
            nrow = 2, align = "hv")

g.MODEL7.NPHIGHER.TARGET.BR <- 
  plot_grid(
    title, g.MODEL7.NPHIGHER.TARGET.BR,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

ggsave(filename = "g.MODEL7.NPHIGHER.TARGET.BR.png", g.MODEL7.NPHIGHER.TARGET.BR,
       width = 9, height = 5, dpi = 300, units = "in", device='png', bg = "white")

## MODEL 8 (MODEL 2 - BR - IP, RES, GFC (DATA).R -> model8t.br.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"BR.NP"
shockinfo_girf$scale<- 0.5

irf.girf.model8t.br.ssvs.NPHIGHER.TARGET.BR <- 
  irf(model8t.br.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 24,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# X
MODEL8.NPHIGHER.TARGET.X.BR <- 
  plot(irf.girf.model8t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.X", shock = "BR.NP")

g.MODEL8.NPHIGHER.TARGET.X.BR <-
  MODEL8.NPHIGHER.TARGET.X.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL8.NPHIGHER.TARGET.X.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL8.NPHIGHER.TARGET.M.BR <- 
  plot(irf.girf.model8t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.M", shock = "BR.NP")

g.MODEL8.NPHIGHER.TARGET.M.BR <-
  MODEL8.NPHIGHER.TARGET.M.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL8.NPHIGHER.TARGET.M.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL8.NPHIGHER.TARGET.RES.BR <- 
  plot(irf.girf.model8t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.RES", shock = "BR.NP")

g.MODEL8.NPHIGHER.TARGET.RES.BR <-
  MODEL8.NPHIGHER.TARGET.RES.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL8.NPHIGHER.TARGET.RES.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL8.NPHIGHER.TARGET.ER.BR <- 
  plot(irf.girf.model8t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.ER", shock = "BR.NP")

g.MODEL8.NPHIGHER.TARGET.ER.BR <-
  MODEL8.NPHIGHER.TARGET.ER.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL8.NPHIGHER.TARGET.ER.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL8.NPHIGHER.TARGET.EQ.BR <- 
  plot(irf.girf.model8t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.EQ", shock = "BR.NP")

g.MODEL8.NPHIGHER.TARGET.EQ.BR <-
  MODEL8.NPHIGHER.TARGET.EQ.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL8.NPHIGHER.TARGET.EQ.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("EQ"))))

# IP
MODEL8.NPHIGHER.TARGET.IP.BR <- 
  plot(irf.girf.model8t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.IP", shock = "BR.NP")

g.MODEL8.NPHIGHER.TARGET.IP.BR <-
  MODEL8.NPHIGHER.TARGET.IP.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL8.NPHIGHER.TARGET.IP.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("IP"))))

# IR
MODEL8.NPHIGHER.TARGET.IR.BR <- 
  plot(irf.girf.model8t.br.ssvs.NPHIGHER.TARGET.BR, resp="BR.IR", shock = "BR.NP")

g.MODEL8.NPHIGHER.TARGET.IR.BR <-
  MODEL8.NPHIGHER.TARGET.IR.BR %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL8.NPHIGHER.TARGET.IR.BR[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (8) | "), bold(italic("GCF")))))

# PLOT THEM TOGETHER

label <- substitute(paste(bold("Model (8) | "), bold(italic("GCF"))))

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
    plot.margin = margin(0, 0, 0, 90)
  )

g.MODEL8.NPHIGHER.TARGET.BR.1 <- 
  plot_grid(NULL,
            g.MODEL8.NPHIGHER.TARGET.X.BR,
            g.MODEL8.NPHIGHER.TARGET.M.BR,
            g.MODEL8.NPHIGHER.TARGET.RES.BR,
            NULL,
            ncol = 5, nrow = 1, align = "hv",
            rel_widths = c(0.125, 0.25, 0.25, 0.25, 0.125))

g.MODEL8.NPHIGHER.TARGET.BR.2 <- 
  plot_grid(NULL,
            g.MODEL8.NPHIGHER.TARGET.ER.BR,
            g.MODEL8.NPHIGHER.TARGET.EQ.BR,
            g.MODEL8.NPHIGHER.TARGET.IP.BR,
            NULL,
            ncol = 5, nrow = 1, align = "hv",
            rel_widths = c(0.125, 0.25, 0.25, 0.25, 0.125))

g.MODEL8.NPHIGHER.TARGET.BR <- 
  plot_grid(g.MODEL8.NPHIGHER.TARGET.BR.1,
            g.MODEL8.NPHIGHER.TARGET.BR.2,
            nrow = 2, align = "hv")

g.MODEL8.NPHIGHER.TARGET.BR <- 
  plot_grid(
    title, g.MODEL8.NPHIGHER.TARGET.BR,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

ggsave(filename = "g.MODEL8.NPHIGHER.TARGET.BR.png", g.MODEL8.NPHIGHER.TARGET.BR,
       width = 9, height = 5, dpi = 300, units = "in", device='png', bg = "white")

### PLOT THEM TOGETHER (MODEL 7 + 8) ####

g.MODEL78.NPHIGHER.TARGET.BR <- 
  plot_grid(g.MODEL7.NPHIGHER.TARGET.BR, 
            g.MODEL8.NPHIGHER.TARGET.BR, 
            nrow = 2, align = "hv")

ggsave(filename = "g.MODEL78.NPHIGHER.TARGET.BR.png", g.MODEL78.NPHIGHER.TARGET.BR,
       width = 9, height = 10, dpi = 300, units = "in", device='png', bg = "white")

### IR PLOT THEM TOGETHER (MODEL 5 + 6 + 7 + 8) ####

g.MODEL5678.NPHIGHER.TARGET.IR.BR <- 
  plot_grid(g.MODEL5.NPHIGHER.TARGET.IR.BR, 
            g.MODEL6.NPHIGHER.TARGET.IR.BR, 
            g.MODEL7.NPHIGHER.TARGET.IR.BR,
            g.MODEL8.NPHIGHER.TARGET.IR.BR,
            nrow = 1, align = "hv")

ggsave(filename = "g.MODEL5678.NPHIGHER.TARGET.IR.BR.png", g.MODEL5678.NPHIGHER.TARGET.IR.BR,
       width = 9, height = 2.5, dpi = 300, units = "in", device='png', bg = "white")

# FOR FUNDING CURRENCIES (CA, CH, GB, JP, US, U2) -> -NP (LONG<SHORT) ####
# Name = NPLOWER.FUND
# Lower NP shock - GIRF

# CH ####
## MODEL 5 (MODEL 3 - CH - XM, RES, VIX (DATA).R -> model5t.ch.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"CH.NP"
shockinfo_girf$scale<- -0.5

irf.girf.model5t.ch.ssvs.NPLOWER.FUND.CH <- 
  irf(model5t.ch.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 24,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# X
MODEL5.NPLOWER.FUND.X.CH <- 
  plot(irf.girf.model5t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.X", shock = "CH.NP")

g.MODEL5.NPLOWER.FUND.X.CH <-
  MODEL5.NPLOWER.FUND.X.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL5.NPLOWER.FUND.X.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL5.NPLOWER.FUND.M.CH <- 
  plot(irf.girf.model5t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.M", shock = "CH.NP")

g.MODEL5.NPLOWER.FUND.M.CH <-
  MODEL5.NPLOWER.FUND.M.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL5.NPLOWER.FUND.M.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL5.NPLOWER.FUND.RES.CH <- 
  plot(irf.girf.model5t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.RES", shock = "CH.NP")

g.MODEL5.NPLOWER.FUND.RES.CH <-
  MODEL5.NPLOWER.FUND.RES.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL5.NPLOWER.FUND.RES.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL5.NPLOWER.FUND.ER.CH <- 
  plot(irf.girf.model5t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.ER", shock = "CH.NP")

g.MODEL5.NPLOWER.FUND.ER.CH <-
  MODEL5.NPLOWER.FUND.ER.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL5.NPLOWER.FUND.ER.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL5.NPLOWER.FUND.EQ.CH <- 
  plot(irf.girf.model5t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.EQ", shock = "CH.NP")

g.MODEL5.NPLOWER.FUND.EQ.CH <-
  MODEL5.NPLOWER.FUND.EQ.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL5.NPLOWER.FUND.EQ.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5, size = 6),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(italic("EQ"))))

# IR
MODEL5.NPLOWER.FUND.IR.CH <- 
  plot(irf.girf.model5t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.IR", shock = "CH.NP")

g.MODEL5.NPLOWER.FUND.IR.CH <-
  MODEL5.NPLOWER.FUND.IR.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL5.NPLOWER.FUND.IR.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5, size = 5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (5) | "), bold(italic("VIX")))))

# PLOT THEM TOGETHER

label <- substitute(paste(bold("Model (5) | "), bold(italic("VIX"))))

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
    plot.margin = margin(0, 0, 0, 90)
  )

g.MODEL5.NPLOWER.FUND.CH.1 <- 
  plot_grid(NULL,
            g.MODEL5.NPLOWER.FUND.X.CH,
            g.MODEL5.NPLOWER.FUND.M.CH,
            g.MODEL5.NPLOWER.FUND.RES.CH,
            NULL,
            ncol = 5, nrow = 1, align = "hv",
            rel_widths = c(0.125, 0.25, 0.25, 0.25, 0.125))

g.MODEL5.NPLOWER.FUND.CH.2 <- 
  plot_grid(NULL,
            g.MODEL5.NPLOWER.FUND.ER.CH,
            g.MODEL5.NPLOWER.FUND.EQ.CH,
            NULL,
            ncol = 4, nrow = 1, align = "hv",
            rel_widths = c(0.25, 0.25, 0.25, 0.25))

g.MODEL5.NPLOWER.FUND.CH <- 
  plot_grid(g.MODEL5.NPLOWER.FUND.CH.1,
            g.MODEL5.NPLOWER.FUND.CH.2,
            nrow = 2, align = "hv")

g.MODEL5.NPLOWER.FUND.CH <- 
  plot_grid(
    title, g.MODEL5.NPLOWER.FUND.CH,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

ggsave(filename = "g.MODEL5.NPLOWER.FUND.CH.png", g.MODEL5.NPLOWER.FUND.CH,
       width = 9, height = 5, dpi = 300, units = "in", device='png', bg = "white")

## MODEL 6 (MODEL 4 - CH - XM, RES, VIX (DATA).R -> model6t.ch.ssvs) ####
shockinfo_girf<-get_shockinfo(ident = "girf")
shockinfo_girf$shock<-"CH.NP"
shockinfo_girf$scale<- -0.5

irf.girf.model6t.ch.ssvs.NPLOWER.FUND.CH <- 
  irf(model6t.ch.ssvs,
      shockinfo = shockinfo_girf,
      n.ahead = 24,
      expert = list(
        save.store = TRUE,
        cores = 7
      )
  )

# X
MODEL6.NPLOWER.FUND.X.CH <- 
  plot(irf.girf.model6t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.X", shock = "CH.NP")

g.MODEL6.NPLOWER.FUND.X.CH <-
  MODEL6.NPLOWER.FUND.X.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL6.NPLOWER.FUND.X.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL6.NPLOWER.FUND.M.CH <- 
  plot(irf.girf.model6t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.M", shock = "CH.NP")

g.MODEL6.NPLOWER.FUND.M.CH <-
  MODEL6.NPLOWER.FUND.M.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL6.NPLOWER.FUND.M.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL6.NPLOWER.FUND.RES.CH <- 
  plot(irf.girf.model6t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.RES", shock = "CH.NP")

g.MODEL6.NPLOWER.FUND.RES.CH <-
  MODEL6.NPLOWER.FUND.RES.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL6.NPLOWER.FUND.RES.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL6.NPLOWER.FUND.ER.CH <- 
  plot(irf.girf.model6t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.ER", shock = "CH.NP")

g.MODEL6.NPLOWER.FUND.ER.CH <-
  MODEL6.NPLOWER.FUND.ER.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL6.NPLOWER.FUND.ER.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL6.NPLOWER.FUND.EQ.CH <- 
  plot(irf.girf.model6t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.EQ", shock = "CH.NP")

g.MODEL6.NPLOWER.FUND.EQ.CH <-
  MODEL6.NPLOWER.FUND.EQ.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL6.NPLOWER.FUND.EQ.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
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
MODEL6.NPLOWER.FUND.IR.CH <- 
  plot(irf.girf.model6t.ch.ssvs.NPLOWER.FUND.CH, resp="CH.IR", shock = "CH.NP")

g.MODEL6.NPLOWER.FUND.IR.CH <-
  MODEL6.NPLOWER.FUND.IR.CH %>%
  as.data.frame() %>%
  mutate(steps = seq(from = 0, to = NROW(MODEL6.NPLOWER.FUND.IR.CH[-1,]))) %>%
  rowwise() %>%
  mutate(median = median(c(Q10, Q16, Q50, Q84, Q90), na.rm = TRUE)) %>%
  ggplot(aes(x = steps)) +
  geom_ribbon(aes(ymin=Q10, ymax=Q90), fill='grey80') +
  geom_ribbon(aes(ymin=Q16, ymax=Q84), fill='grey50') +
  geom_line(aes(y = median)) +
  geom_hline(yintercept = 0, linetype='dotted', col = 'red') +
  #scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5, size = 3),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5), 
        panel.grid.minor = element_blank()) +
  labs(title = expression(paste(bold("Model (6) | "), bold(italic("GCF")))))

# PLOT THEM TOGETHER

label <- substitute(paste(bold("Model (6) | "), bold(italic("GCF"))))

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
    plot.margin = margin(0, 0, 0, 90)
  )

g.MODEL6.NPLOWER.FUND.CH.1 <- 
  plot_grid(NULL,
            g.MODEL6.NPLOWER.FUND.X.CH,
            g.MODEL6.NPLOWER.FUND.M.CH,
            g.MODEL6.NPLOWER.FUND.RES.CH,
            NULL,
            ncol = 5, nrow = 1, align = "hv",
            rel_widths = c(0.125, 0.25, 0.25, 0.25, 0.125))

g.MODEL6.NPLOWER.FUND.CH.2 <- 
  plot_grid(NULL,
            g.MODEL6.NPLOWER.FUND.ER.CH,
            g.MODEL6.NPLOWER.FUND.EQ.CH,
            NULL,
            ncol = 4, nrow = 1, align = "hv",
            rel_widths = c(0.25, 0.25, 0.25, 0.25))

g.MODEL6.NPLOWER.FUND.CH <- 
  plot_grid(g.MODEL6.NPLOWER.FUND.CH.1,
            g.MODEL6.NPLOWER.FUND.CH.2,
            nrow = 2, align = "hv")

g.MODEL6.NPLOWER.FUND.CH <- 
  plot_grid(
    title, g.MODEL6.NPLOWER.FUND.CH,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )

ggsave(filename = "g.MODEL6.NPLOWER.FUND.CH.png", g.MODEL6.NPLOWER.FUND.CH,
       width = 9, height = 5, dpi = 300, units = "in", device='png', bg = "white")

### PLOT THEM TOGETHER (MODEL 5 + 6) ####

g.MODEL56.NPLOWER.FUND.CH <- 
  plot_grid(g.MODEL5.NPLOWER.FUND.CH, 
            g.MODEL6.NPLOWER.FUND.CH, 
            nrow = 2, align = "hv")

ggsave(filename = "g.MODEL56.NPLOWER.FUND.CH.png", g.MODEL56.NPLOWER.FUND.CH,
       width = 9, height = 10, dpi = 300, units = "in", device='png', bg = "white")


### IR PLOT THEM TOGETHER (MODEL 5 + 6) ####

g.MODEL56.NPLOWER.FUND.IR.CH <- 
  plot_grid(NULL, g.MODEL5.NPLOWER.FUND.IR.CH, g.MODEL6.NPLOWER.FUND.IR.CH, NULL,
            nrow = 1, align = "hv")

ggsave(filename = "g.MODEL56.NPLOWER.FUND.IR.CH.png", g.MODEL56.NPLOWER.FUND.IR.CH,
       width = 9, height = 2.5, dpi = 300, units = "in", device='png', bg = "white")

