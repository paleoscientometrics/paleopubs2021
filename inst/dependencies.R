# Attachments ----
to_install <- c("ggplot2", "ggthemes", "patchwork", "RColorBrewer", "dplyr", "readxl", "tm", "wordcloud", "data.table", "tidyverse")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }
  }
