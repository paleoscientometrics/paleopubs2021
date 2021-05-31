# Load packages -----------------------------------------------------------
library(ggplot2)
library(RColorBrewer)
library(patchwork)

# Plotting parameters -----------------------------------------------------
pal <- brewer.pal(8, "Dark2")[c(3,6)]
theme_set(ggthemes::theme_hc())

# Load data ---------------------------------------------------------------
topjournals <- read.csv(file.path("data","topjournals.csv"))
topjournals$value2 <- NA
topjournals$value2[topjournals$name=="jif"] <- topjournals$value[topjournals$name=="records"]


# Plots -------------------------------------------------------------------

# a. Percentage publications ----------------------------------------------
p1 <- ggplot(topjournals[topjournals$name == "records",], aes(x=reorder(abbrev, value), y=value)) +
  geom_bar(stat="identity", fill=pal[1]) +
  ylim(0, 35)+
  coord_flip(expand = F) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_text(face="bold"),
        axis.ticks.length.y.left = unit("0", "cm"),
        axis.text.y = element_text(hjust=0.5)
  ) +
  labs(y="Percentage of publications")


# b. Impact factor --------------------------------------------------------
p2 <- ggplot(topjournals[topjournals$name == "jif",], 
             aes(x=reorder(abbrev, value2), y=value)) +
  geom_bar(stat="identity", fill=pal[2]) +
  scale_y_continuous(breaks = -seq(0,44, 10), labels=seq(0,44, 10))+
  coord_flip(expand = F) +
  theme(axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(face="bold"),
        axis.ticks.length.y.left = unit("0", "cm")
  ) +
  labs(y="Impact factor") 


# c. Save plot ------------------------------------------------------------
svg(file.path("figs", "Fig01_journals.svg"), w=8, h=6)
p2 + p1
dev.off()
