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

seqs_can <- seqs[seqs$region == 'Canada',]
names(seqs_can)[3] <- 'GISAID'
names(seqs_can)[2] <- 'Sequences'
names(seqs_can)[4] <- 'Date'
seqs_can <- data.frame(seqs_can)
seqs_can$Date <- as.Date(seqs_can$Date)

ggplot(seqs_can, aes(x = Date, y = Sequences, fill = 'Sequences')) +
  geom_col() +
  geom_col(aes(x = Date, y = GISAID, fill = 'GISAID')) +
  ylab("Number of sequences") +
  ggtitle("Cumulative number of sequences passing national QC standards and sequences uploaded to GISAID") +
  guides(x = guide_axis(angle = 90)) +
  geom_text(size=2.5, aes(y = GISAID, label = GISAID), vjust = 1.5, colour = "black") +
  labs(fill='') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_manual(values = c("#9ADADC", "#457B9D"))

ggsave("gisaid_seqs_running_canada.png", width = 25, height = 25, units = 'cm')

seqs2 <- seqs %>%
  group_by(region, GISAID_uploads) %>%
  summarise(max = max(sequences,na.rm=TRUE))

seqs2<- seqs %>%
  group_by(region) %>%
  summarise(GISAID = max(GISAID_uploads,na.rm=TRUE), sequences = max(sequences))

seqs2$region <- factor(seqs2$region)
seqs2$region <- relevel(seqs2$region, "Canada")

#-----------French graphs

seqs3 <- read.csv('data_fr.csv', header = TRUE)
seqs3 <- subset(seqs3, select=-c(proportion))

seqs3$région <- factor(seqs3$région)
seqs3$région <- relevel(seqs3$région, "Canada")

names(seqs_can)[2] <- 'Séquences'

ggplot(seqs_can, aes(x = Date, y = Séquences, fill = 'Séquences')) +
  geom_col() +
  geom_col(aes(x = Date, y = GISAID, fill = 'GISAID')) +
  ylab("Nombre de séquences") +
  ggtitle("Nombre cumulatif de séquences conformes aux normes nationales de contrôle de la qualité et séquences \ntélechargées dans GISAID") +
  guides(x = guide_axis(angle = 90)) +
  geom_text(size=2.5, aes(y = GISAID, label = GISAID), vjust = 1.5, colour = "black") +
  labs(fill='') +
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_manual(values = c("#9ADADC", "#457B9D"))

ggsave("gisaid_seqs_running_canada_fr.png", width = 25, height = 25, units = 'cm')


seqs4 <- seqs3 %>%
  group_by(région, téléchargements_à_GISAID) %>%
  summarise(max = max(séquences,na.rm=TRUE))

seqs4<- seqs3 %>%
  group_by(région) %>%
  summarise(GISAID = max(téléchargements_à_GISAID,na.rm=TRUE), séquences = max(séquences))

seqs4$region <- factor(seqs4$région)
seqs4$region <- relevel(seqs4$région, "Canada")

ggplot(seqs4, aes(x = région, y = séquences, fill = 'séquences')) +
  geom_col() +
  geom_col(aes(x = région, y = GISAID, fill = 'GISAID')) +
  ylab("nombre de séquences") +
  ggtitle("nombre cumulatif de séquences conformes aux normes nationales de contrôle de la qualité et séquences \ntéleversées dans GISAID par région") +
  guides(x = guide_axis(angle = 90)) +
  geom_text(aes(y = GISAID, label = GISAID), vjust = 1.5, colour = "black") +
  labs(fill='') +
  scale_fill_manual(values = c("#9ADADC", "#457B9D"))

ggsave("gisaid_seqs_totals_fr.png", width = 25, height = 25, units = 'cm')



ggplot(seqs2, aes(x = region, y = sequences, fill = 'sequences')) +
  geom_col() +
  geom_col(aes(x = region, y = GISAID, fill = 'GISAID')) +
  ylab("number of sequences") +
  ggtitle("Cumulative number of sequences passing national QC standards and sequences uploaded to GISAID \nby region") +
  guides(x = guide_axis(angle = 90)) +
  geom_text(aes(y = GISAID, label = GISAID), vjust = 1.5, colour = "black") +
  labs(fill='') +
  scale_fill_manual(values = c("#9ADADC", "#457B9D"))

#ggsave("gisaid_seqs_totals.png", width = 25, height = 25, units = 'cm')
