library(ggplot2)
library(ggridges)
library(reshape)
library(dplyr)
library(viridis)

setwd("~/Projects/gisaid_uploads")
seqs <- read.csv('data/data_reduced_mar_2022.csv', header = TRUE)

seqs$region <- factor(seqs$region)
seqs$region <- relevel(seqs$region, "Canada")

seqsmelt <- melt(seqs)
seqsmelt$date <- as.Date(seqsmelt$date)

names(seqsmelt)[4] <- 'sequences'

ggplot(seqsmelt, aes(x=date, y=sequences, group=variable)) +
  geom_area(aes(fill=variable), position = "identity") +
  scale_fill_viridis(discrete = TRUE) +
  facet_wrap(~region, ncol = 2, scales = "free_y") +
  guides(x = guide_axis(angle = 90)) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y") +
  labs(fill='Sequence Type') +
  ggtitle("Cumulative number of sequences generated (passing regional QC standards),\nuploaded to GISAID and uploaded to the Data Portal by region")

ggsave("plots/seqs_gisaid_portal_by_prov_date.png", width = 25, height = 25, units = 'cm')