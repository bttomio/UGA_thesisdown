#calculates the LN GFEVD
RNGkind("L'Ecuyer-CMRG")
set.seed(123)
library(BGVAR)
options(scipen=999)
library(tidyverse)
library(cowplot)
library(reshape2)

# MODEL 5 (CH) ####
# LEGEND
gfevd <- gfevd(model5t.ch.ssvs,
               n.ahead = 24,
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
                 measure.vars=c("Foreign ", "X   ", "M   ", "RES   ", "IR   ", 
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
                 measure.vars=c("Foreign", "X", "M", "RES", "IR", "ER", "EQ", "NP"))

g.GFEVD.model5.CH <- 
  GFEVD.CH %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 24, by = 4)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = expression(paste(bold("Model (5) | "), italic("VIX"))))

# MODEL 6 (CH) ####
# LEGEND
gfevd <- gfevd(model6t.ch.ssvs,
               n.ahead = 24,
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
                 measure.vars=c("Foreign ", "X   ", "M   ", "RES   ", "IR   ", 
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
                 measure.vars=c("Foreign", "X", "M", "RES", "IR", "ER", "EQ", "NP"))

g.GFEVD.model6.CH <- 
  GFEVD.CH %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 24, by = 4)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = expression(paste(bold("Model (6) | "), italic("GCF"))))

# MODEL 5 (BR) ####
# LEGEND
gfevd <- gfevd(model5t.br.ssvs,
               n.ahead = 24,
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
                 measure.vars=c("Foreign ", "X   ", "M   ", "RES   ", "IR   ", 
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
                 measure.vars=c("Foreign", "X", "M", "RES", "IR", "ER", "EQ", "NP"))

g.GFEVD.model5.BR <- 
  GFEVD.BR %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 24, by = 4)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = expression(paste(bold("Model (5) | "), italic("VIX"))))

# MODEL 6 (BR) ####
# LEGEND
gfevd <- gfevd(model6t.br.ssvs,
               n.ahead = 24,
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
                 measure.vars=c("Foreign ", "X   ", "M   ", "RES   ", "IR   ", 
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
                 measure.vars=c("Foreign", "X", "M", "RES", "IR", "ER", "EQ", "NP"))

g.GFEVD.model6.BR <- 
  GFEVD.BR %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 24, by = 4)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = expression(paste(bold("Model (6) | "), italic("GCF"))))

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

g.GFEVD.CH.1 <- plot_grid(g.GFEVD.model5.CH,
                          g.GFEVD.model6.CH,
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

g.GFEVD.BR.1 <- plot_grid(g.GFEVD.model5.BR,
                          g.GFEVD.model6.BR,
                          ncol = 2, nrow = 1, align = "hv")

g.GFEVD.BR.1 <- plot_grid(title,
                          g.GFEVD.BR.1,
                          ncol = 1, 
                          rel_heights = c(0.1, 1), 
                          align = "hv")

g.GFEVD.CHBR.56 <- plot_grid(g.GFEVD.CH.1,
                             g.GFEVD.BR.1,
                             ncol = 1, nrow = 2, align = "hv")

g.GFEVD.CHBR.56 <- plot_grid(g.GFEVD.CHBR.56,
                             legend.GFEVD,
                             ncol = 1, 
                             rel_heights = c(1, 0.1))

ggsave(filename = "g.GFEVD.MODEL56.CHBR.png", g.GFEVD.CHBR.56,
       width = 7, height = 6, dpi = 300, units = "in", device='png', bg = "white")

# MODEL 7 (BR) ####
# LEGEND
gfevd <- gfevd(model7t.br.ssvs,
               n.ahead = 24,
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
                 measure.vars=c("Foreign ", "IP   ", "X   ", "M   ", "RES   ", "IR   ", 
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
                 measure.vars=c("Foreign", "X", "M", "RES", "IR", "ER", "EQ", "NP"))

g.GFEVD.model7.BR <- 
  GFEVD.BR %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 24, by = 4)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = expression(paste(bold("Model (7) | "), italic("VIX"))))


# MODEL 8 (BR) ####
# LEGEND
gfevd <- gfevd(model8t.br.ssvs,
               n.ahead = 24,
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
                 measure.vars=c("Foreign ", "IP   ", "X   ", "M   ", "RES   ", "IR   ", 
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
                 measure.vars=c("Foreign", "X", "M", "RES", "IR", "ER", "EQ", "NP"))

g.GFEVD.model8.BR <- 
  GFEVD.BR %>%
  ggplot(aes(x=steps, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 24, by = 4)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        axis.text = element_text(size = 8),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5), 
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = expression(paste(bold("Model (8) | "), italic("GCF"))))

# PLOT ALL TOGETHER ####

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

g.GFEVD.BR.1 <- plot_grid(g.GFEVD.model7.BR,
                          g.GFEVD.model8.BR,
                          ncol = 2, nrow = 1, align = "hv")

g.GFEVD.BR.78 <- plot_grid(g.GFEVD.BR.1,
                             legend.GFEVD,
                             ncol = 1, 
                             rel_heights = c(1, 0.2))

ggsave(filename = "g.GFEVD.MODEL78.BR.png", g.GFEVD.BR.78,
       width = 7, height = 3, dpi = 300, units = "in", device='png', bg = "white")

