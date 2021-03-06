# Load libraries ----------------------------------------------------------
library(tidyverse)

# Plot parameters ---------------------------------------------------------
theme_set(theme_minimal(base_size=14) %+replace%
            theme(legend.title = element_text(
              face="bold"),
              axis.text.x= element_text(face="bold", colour="black", size=10),
              axis.title.x = element_blank(),
              legend.position="bottom",
              plot.background = element_rect(fill="white", colour=NA),
              plot.title = element_text(face="bold", hjust=0, size=16))
)
pal <- brewer.pal(8, "Dark2")[c(3,6)]


# Data --------------------------------------------------------------------
fnames <- list.files(path="data", "alt") #get filenames

#categories
cats <- gsub("alt_|\\.csv|_paleo", "", fnames)
cats <- do.call(rbind,strsplit(cats, "_"))

alt.df <- list()

for(i in seq_along(fnames)){
  alt.df[[i]] <- read.csv(file.path("data", fnames[i]))
  alt.df[[i]]$journal <-cats[i,1]
  alt.df[[i]]$topic <-cats[i,2]
}

alt.df <- data.table::rbindlist(alt.df, fill=TRUE) # all alt metric data

# Classify and summarise data  ------------------------------------------------

# Dinosaur in title?
alt.df$dino <- "no"
alt.df$dino[grep("dino", alt.df$title, ignore.case = TRUE)] <- "yes"

# Altmetric summary
alt.summary <- alt.df %>%group_by(topic, dino) %>% 
  summarise(score = mean(score), 
            tweets = mean(cited_by_tweeters_count, na.rm = TRUE),
            posts = mean(cited_by_posts_count, na.rm = TRUE),
            fb=mean(cited_by_fbwalls_count, na.rm=TRUE))%>% 
  mutate(none=0) %>% 
  pivot_longer(cols=c("score", "posts", "tweets", "fb", "none"),
                             names_to="metric")

alt.summary$x <- rep(c(8.5,9.5,0.5,1.5), each = 5)
alt.summary$metric <- factor(alt.summary$metric, levels=rev(c("score", "posts", "tweets", "fb", "none")))


# Plot and save figure ----------------------------------------------------
p <- ggplot(alt.summary, aes(x=x, y=metric)) + 
  geom_point(aes(size=value, col=dino), alpha=0.2) +
  scale_x_continuous(breaks=c(9,2), 
                     labels = c("Palaeontology \nin general","Myanmar amber"),
                     limits = c(0,10)
                     ) +
  scale_y_discrete(labels=rev(c("Altmetric Score", "Posts", "Tweets", "Facebook Walls", "None"))) +
  scale_size_continuous(range=c(5,20), guide=FALSE) +
  labs(x="", y="", col='Includes "dinosaur" in title')+
  scale_color_manual(values=pal) +
  geom_vline(xintercept = 0, linetype="dashed", col="darkgrey")+
  geom_text(aes(label=round(value), col=dino)) +
  coord_polar()

ggsave(file.path("figs", "Fig03_altmetrics.svg"), p, width=8, height=8) #edit in inskcape afterwards
