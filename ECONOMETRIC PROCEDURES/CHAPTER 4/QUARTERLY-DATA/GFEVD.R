#calculates the LN GFEVD
RNGkind("L'Ecuyer-CMRG")
set.seed(123)
library(BGVAR)
options(scipen=999)
library(tidyverse)
library(cowplot)
library(reshape2)

# MODEL 1 (CH) ####
# LEGEND
gfevd <- gfevd(model1t.ch.ssvs,
               n.ahead = 12,
               running = TRUE,
               cores = 7)$FEVD

idx.CH <- which(grepl("CH.", dimnames(gfevd)[[2]]))
own.CH <- gfevd["CH.NP", idx.CH,] %>%
  t() %>%
  as.data.frame()

colnames(own.CH) <- lapply(colnames(own.CH), function(x) {gsub("Decomp. of CH.", "", x)})

GFEVD.CH <-
  own.CH  %>%
  rename_all( ~ paste0(.x, "   ")) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 4), nsmall = 4))) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate("Foreign " = 1 - rowSums(across(where(is.numeric)))) %>%
  mutate(steps = as.numeric(seq(from = 0, to = NROW(own.CH[-1,]))))

GFEVD.CH <- melt(GFEVD.CH, id.vars = "steps",
                 measure.vars=c("Foreign ", "GDP   ", "RES   ", "IR   ", 
                                "ER   ", "EQ   ", "NP   "))

g.GFEVD.CH <- 
  GFEVD.CH %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8, face = "italic")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(title = expression(paste(bold("Switzerland (CH)"))))

legend.GFEVD <- get_legend(g.GFEVD.CH)

# PLOT

idx.CH <- which(grepl("CH.", dimnames(gfevd)[[2]]))
own.CH <- gfevd["CH.NP", idx.CH,] %>%
  t() %>%
  as.data.frame()

colnames(own.CH) <- lapply(colnames(own.CH), function(x) {gsub("Decomp. of CH.", "", x)})

GFEVD.CH <-
  own.CH %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 4), nsmall = 4))) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate("Foreign" = 1 - rowSums(across(where(is.numeric)))) %>%
  mutate(steps = as.numeric(seq(from = 0, to = NROW(own.CH[-1,]))))

GFEVD.CH <- melt(GFEVD.CH, id.vars = "steps",
                 measure.vars=c("Foreign", "GDP", "RES", "IR", "ER", "EQ", "NP"))

g.GFEVD.model1.CH <- 
  GFEVD.CH %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 12, by = 4)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = expression(paste(bold("Model (1) | "), italic("VIX"))))

# MODEL 2 (CH) ####
# LEGEND
gfevd <- gfevd(model2t.ch.ssvs,
               n.ahead = 12,
               running = TRUE,
               cores = 7)$FEVD

idx.CH <- which(grepl("CH.", dimnames(gfevd)[[2]]))
own.CH <- gfevd["CH.NP", idx.CH,] %>%
  t() %>%
  as.data.frame()

colnames(own.CH) <- lapply(colnames(own.CH), function(x) {gsub("Decomp. of CH.", "", x)})

GFEVD.CH <-
  own.CH  %>%
  rename_all( ~ paste0(.x, "   ")) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 4), nsmall = 4))) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate("Foreign " = 1 - rowSums(across(where(is.numeric)))) %>%
  mutate(steps = as.numeric(seq(from = 0, to = NROW(own.CH[-1,]))))

GFEVD.CH <- melt(GFEVD.CH, id.vars = "steps",
                 measure.vars=c("Foreign ", "GDP   ", "RES   ", "IR   ", 
                                "ER   ", "EQ   ", "NP   "))

g.GFEVD.CH <- 
  GFEVD.CH %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8, face = "italic")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(title = expression(paste(bold("Switzerland (CH)"))))

legend.GFEVD <- get_legend(g.GFEVD.CH)

# PLOT

idx.CH <- which(grepl("CH.", dimnames(gfevd)[[2]]))
own.CH <- gfevd["CH.NP", idx.CH,] %>%
  t() %>%
  as.data.frame()

colnames(own.CH) <- lapply(colnames(own.CH), function(x) {gsub("Decomp. of CH.", "", x)})

GFEVD.CH <-
  own.CH %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 4), nsmall = 4))) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate("Foreign" = 1 - rowSums(across(where(is.numeric)))) %>%
  mutate(steps = as.numeric(seq(from = 0, to = NROW(own.CH[-1,]))))

GFEVD.CH <- melt(GFEVD.CH, id.vars = "steps",
                 measure.vars=c("Foreign", "GDP", "RES", "IR", "ER", "EQ", "NP"))

g.GFEVD.model2.CH <- 
  GFEVD.CH %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 12, by = 4)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = expression(paste(bold("Model (2) | "), italic("GCF"))))

# MODEL 1 (BR) ####
# LEGEND
gfevd <- gfevd(model1t.br.ssvs,
               n.ahead = 12,
               running = TRUE,
               cores = 7)$FEVD

idx.BR <- which(grepl("BR.", dimnames(gfevd)[[2]]))
own.BR <- gfevd["BR.NP", idx.BR,] %>%
  t() %>%
  as.data.frame()

colnames(own.BR) <- lapply(colnames(own.BR), function(x) {gsub("Decomp. of BR.", "", x)})

GFEVD.BR <-
  own.BR  %>%
  rename_all( ~ paste0(.x, "   ")) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 4), nsmall = 4))) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate("Foreign " = 1 - rowSums(across(where(is.numeric)))) %>%
  mutate(steps = as.numeric(seq(from = 0, to = NROW(own.BR[-1,]))))

GFEVD.BR <- melt(GFEVD.BR, id.vars = "steps",
                 measure.vars=c("Foreign ", "GDP   ", "RES   ", "IR   ", 
                                "ER   ", "EQ   ", "NP   "))

g.GFEVD.BR <- 
  GFEVD.BR %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8, face = "italic")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(title = expression(paste(bold("Switzerland (BR)"))))

legend.GFEVD <- get_legend(g.GFEVD.BR)

# PLOT

idx.BR <- which(grepl("BR.", dimnames(gfevd)[[2]]))
own.BR <- gfevd["BR.NP", idx.BR,] %>%
  t() %>%
  as.data.frame()

colnames(own.BR) <- lapply(colnames(own.BR), function(x) {gsub("Decomp. of BR.", "", x)})

GFEVD.BR <-
  own.BR %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 4), nsmall = 4))) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate("Foreign" = 1 - rowSums(across(where(is.numeric)))) %>%
  mutate(steps = as.numeric(seq(from = 0, to = NROW(own.BR[-1,]))))

GFEVD.BR <- melt(GFEVD.BR, id.vars = "steps",
                 measure.vars=c("Foreign", "GDP", "RES", "IR", "ER", "EQ", "NP"))

g.GFEVD.model1.BR <- 
  GFEVD.BR %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 12, by = 4)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = expression(paste(bold("Model (1) | "), italic("VIX"))))

# PLOT ALL TOGETHER ####

label <- substitute(paste(bold("Switzerland")))

title <- ggdraw() + 
  draw_label(
    label,
    fontface = 'bold',
    fontfamily = "LM Roman 10",
    size = 10
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )

g.GFEVD.CH.1 <- plot_grid(g.GFEVD.model1.CH,
                          g.GFEVD.model2.CH,
                          ncol = 2, nrow = 1, align = "hv")

g.GFEVD.CH.1 <- plot_grid(title,
                          g.GFEVD.CH.1,
                          ncol = 1, 
                          rel_heights = c(0.1, 1), 
                          align = "hv")

label <- substitute(paste(bold("Brazil")))

title <- ggdraw() + 
  draw_label(
    label,
    fontface = 'bold',
    fontfamily = "LM Roman 10",
    size = 10
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )

g.GFEVD.BR.1 <- plot_grid(title,
                          g.GFEVD.model1.BR,
                          ncol = 1, 
                          rel_heights = c(0.1, 1), 
                          align = "hv")

g.GFEVD.BR.1 <- plot_grid(NULL, g.GFEVD.BR.1, NULL,
                          ncol = 3, 
                          rel_widths = c(0.25, 0.5, 0.25), 
                          align = "hv")

g.GFEVD.CHBR.12 <- plot_grid(g.GFEVD.CH.1,
                             g.GFEVD.BR.1,
                             ncol = 1, nrow = 2, align = "hv")

g.GFEVD.CHBR.12 <- plot_grid(g.GFEVD.CHBR.12,
                             legend.GFEVD,
                             ncol = 1, 
                             rel_heights = c(1, 0.1))

ggsave(filename = "g.GFEVD.MODEL12.CHBR.png", g.GFEVD.CHBR.12,
       width = 7, height = 6, dpi = 300, units = "in", device='png', bg = "white")


# MODEL 3 (CH) ####
# LEGEND
gfevd <- gfevd(model3t.ch.ssvs,
               n.ahead = 12,
               running = TRUE,
               cores = 7)$FEVD

idx.CH <- which(grepl("CH.", dimnames(gfevd)[[2]]))
own.CH <- gfevd["CH.NP", idx.CH,] %>%
  t() %>%
  as.data.frame()

colnames(own.CH) <- lapply(colnames(own.CH), function(x) {gsub("Decomp. of CH.", "", x)})

GFEVD.CH <-
  own.CH  %>%
  rename_all( ~ paste0(.x, "   ")) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 4), nsmall = 4))) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate("Foreign " = 1 - rowSums(across(where(is.numeric)))) %>%
  mutate(steps = as.numeric(seq(from = 0, to = NROW(own.CH[-1,]))))

GFEVD.CH <- melt(GFEVD.CH, id.vars = "steps",
                 measure.vars=c("Foreign ", "C   ",  "GFCF   ",  "X   ",  "M   ", 
                                "RES   ", "IR   ", "ER   ", "EQ   ", "NP   "))

g.GFEVD.CH <- 
  GFEVD.CH %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8, face = "italic")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(title = expression(paste(bold("Switzerland (CH)"))))

legend.GFEVD <- get_legend(g.GFEVD.CH)

# PLOT

idx.CH <- which(grepl("CH.", dimnames(gfevd)[[2]]))
own.CH <- gfevd["CH.NP", idx.CH,] %>%
  t() %>%
  as.data.frame()

colnames(own.CH) <- lapply(colnames(own.CH), function(x) {gsub("Decomp. of CH.", "", x)})

GFEVD.CH <-
  own.CH %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 4), nsmall = 4))) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate("Foreign" = 1 - rowSums(across(where(is.numeric)))) %>%
  mutate(steps = as.numeric(seq(from = 0, to = NROW(own.CH[-1,]))))

GFEVD.CH <- melt(GFEVD.CH, id.vars = "steps",
                 measure.vars=c("Foreign", "C", "GFCF", "X", "M", 
                                "RES", "IR", "ER", "EQ", "NP"))

g.GFEVD.model3.CH <- 
  GFEVD.CH %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 12, by = 4)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = expression(paste(bold("Model (3) | "), italic("VIX"))))

# MODEL 4 (CH) ####
# LEGEND
gfevd <- gfevd(model4t.ch.ssvs,
               n.ahead = 12,
               running = TRUE,
               cores = 7)$FEVD

idx.CH <- which(grepl("CH.", dimnames(gfevd)[[2]]))
own.CH <- gfevd["CH.NP", idx.CH,] %>%
  t() %>%
  as.data.frame()

colnames(own.CH) <- lapply(colnames(own.CH), function(x) {gsub("Decomp. of CH.", "", x)})

GFEVD.CH <-
  own.CH  %>%
  rename_all( ~ paste0(.x, "   ")) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 4), nsmall = 4))) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate("Foreign " = 1 - rowSums(across(where(is.numeric)))) %>%
  mutate(steps = as.numeric(seq(from = 0, to = NROW(own.CH[-1,]))))

GFEVD.CH <- melt(GFEVD.CH, id.vars = "steps",
                 measure.vars=c("Foreign ", "C   ",  "GFCF   ",  "X   ",  "M   ", 
                                "RES   ", "IR   ", "ER   ", "EQ   ", "NP   "))

g.GFEVD.CH <- 
  GFEVD.CH %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8, face = "italic")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(title = expression(paste(bold("Switzerland (CH)"))))

legend.GFEVD <- get_legend(g.GFEVD.CH)

# PLOT

idx.CH <- which(grepl("CH.", dimnames(gfevd)[[2]]))
own.CH <- gfevd["CH.NP", idx.CH,] %>%
  t() %>%
  as.data.frame()

colnames(own.CH) <- lapply(colnames(own.CH), function(x) {gsub("Decomp. of CH.", "", x)})

GFEVD.CH <-
  own.CH %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 4), nsmall = 4))) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate("Foreign" = 1 - rowSums(across(where(is.numeric)))) %>%
  mutate(steps = as.numeric(seq(from = 0, to = NROW(own.CH[-1,]))))

GFEVD.CH <- melt(GFEVD.CH, id.vars = "steps",
                 measure.vars=c("Foreign", "C", "GFCF", "X", "M", 
                                "RES", "IR", "ER", "EQ", "NP"))

g.GFEVD.model4.CH <- 
  GFEVD.CH %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 12, by = 4)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = expression(paste(bold("Model (4) | "), italic("GCF"))))

# MODEL 3 (BR) ####
# LEGEND
gfevd <- gfevd(model3t.br.ssvs,
               n.ahead = 12,
               running = TRUE,
               cores = 7)$FEVD

idx.BR <- which(grepl("BR.", dimnames(gfevd)[[2]]))
own.BR <- gfevd["BR.NP", idx.BR,] %>%
  t() %>%
  as.data.frame()

colnames(own.BR) <- lapply(colnames(own.BR), function(x) {gsub("Decomp. of BR.", "", x)})

GFEVD.BR <-
  own.BR  %>%
  rename_all( ~ paste0(.x, "   ")) %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 4), nsmall = 4))) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate("Foreign " = 1 - rowSums(across(where(is.numeric)))) %>%
  mutate(steps = as.numeric(seq(from = 0, to = NROW(own.BR[-1,]))))

GFEVD.BR <- melt(GFEVD.BR, id.vars = "steps",
                 measure.vars=c("Foreign ", "C   ",  "GFCF   ",  "X   ",  "M   ", 
                                "RES   ", "IR   ", "ER   ", "EQ   ", "NP   "))

g.GFEVD.BR <- 
  GFEVD.BR %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8, face = "italic")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(title = expression(paste(bold("Switzerland (BR)"))))

legend.GFEVD <- get_legend(g.GFEVD.BR)

# PLOT

idx.BR <- which(grepl("BR.", dimnames(gfevd)[[2]]))
own.BR <- gfevd["BR.NP", idx.BR,] %>%
  t() %>%
  as.data.frame()

colnames(own.BR) <- lapply(colnames(own.BR), function(x) {gsub("Decomp. of BR.", "", x)})

GFEVD.BR <-
  own.BR %>%
  mutate(across(where(~ is.numeric(.)), ~ format(round(., 4), nsmall = 4))) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate("Foreign" = 1 - rowSums(across(where(is.numeric)))) %>%
  mutate(steps = as.numeric(seq(from = 0, to = NROW(own.BR[-1,]))))

GFEVD.BR <- melt(GFEVD.BR, id.vars = "steps",
                 measure.vars=c("Foreign", "C", "GFCF", "X", "M", 
                                "RES", "IR", "ER", "EQ", "NP"))

g.GFEVD.model3.BR <- 
  GFEVD.BR %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 12, by = 4)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = expression(paste(bold("Model (3) | "), italic("VIX"))))

# PLOT ALL TOGETHER ####

label <- substitute(paste(bold("Switzerland")))

title <- ggdraw() + 
  draw_label(
    label,
    fontface = 'bold',
    fontfamily = "LM Roman 10",
    size = 10
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )

g.GFEVD.CH.1 <- plot_grid(g.GFEVD.model3.CH,
                          g.GFEVD.model4.CH,
                          ncol = 2, nrow = 1, align = "hv")

g.GFEVD.CH.1 <- plot_grid(title,
                          g.GFEVD.CH.1,
                          ncol = 1, 
                          rel_heights = c(0.1, 1), 
                          align = "hv")

label <- substitute(paste(bold("Brazil")))

title <- ggdraw() + 
  draw_label(
    label,
    fontface = 'bold',
    fontfamily = "LM Roman 10",
    size = 10
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )

g.GFEVD.BR.1 <- plot_grid(title,
                          g.GFEVD.model3.BR,
                          ncol = 1, 
                          rel_heights = c(0.1, 1), 
                          align = "hv")

g.GFEVD.BR.1 <- plot_grid(NULL, g.GFEVD.BR.1, NULL,
                          ncol = 3, 
                          rel_widths = c(0.25, 0.5, 0.25), 
                          align = "hv")

g.GFEVD.CHBR.34 <- plot_grid(g.GFEVD.CH.1,
                             g.GFEVD.BR.1,
                             ncol = 1, nrow = 2, align = "hv")

g.GFEVD.CHBR.34 <- plot_grid(g.GFEVD.CHBR.34,
                             legend.GFEVD,
                             ncol = 1, 
                             rel_heights = c(1, 0.1))

ggsave(filename = "g.GFEVD.MODEL34.CHBR.png", g.GFEVD.CHBR.34,
       width = 7, height = 6, dpi = 300, units = "in", device='png', bg = "white")
