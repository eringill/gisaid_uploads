# A script to plot weekly fraction of COVID-19 cases sequenced by province 

library(ggplot2)
library(ggridges)
library(reshape)
library(dplyr)
library(viridis)

setwd("~/Projects/gisaid_uploads")
seqs <- read.csv('data.csv', header = TRUE)
seqs <- subset(seqs, select=-c(proportion))

seqs$region <- factor(seqs$region)
seqs$region <- relevel(seqs$region, "Canada")


ggplot(seqs, aes(x = date, y = GISAID_uploads)) + 
  geom_col(show.legend = FALSE, aes(fill = region)) +
  facet_wrap(~region, ncol = 1, scales = "free_y") +
  guides(x = guide_axis(angle = 90)) +
  xlab("date") + 
  ylab("number of sequences") +
  ggtitle("Cumulative number of sequences uploaded to GISAID by region and date") +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) 

ggsave("gisaid_prov_date.png", width = 25, height = 25, units = 'cm')



#-----------French graphs
seqs3 <- read.csv('data_fr.csv', header = TRUE)
seqs3 <- subset(seqs3, select=-c(proportion))

seqs3$région <- factor(seqs3$région)
seqs3$région <- relevel(seqs3$région, "Canada")


ggplot(seqs3, aes(x = date, y = téléchargements_à_GISAID)) + 
  geom_col(show.legend = FALSE, aes(fill = région)) +
  facet_wrap(~région, ncol = 1, scales = "free_y") +
  guides(x = guide_axis(angle = 90)) +
  xlab("date") + 
  ylab("nombre de séquences") +
  ggtitle("nombre cumulatif de séquences téleverseés dans GISAID par région et date") +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) 

#ggsave("gisaid_prov_date_fr.png", width = 25, height = 25, units = 'cm')


