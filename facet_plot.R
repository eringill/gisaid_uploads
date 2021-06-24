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
  ggtitle("Cumulative number of sequences uploaded to GISAID by province and date") +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) 

ggsave("gisaid_prov_date.png", width = 25, height = 25, units = 'cm')


seqs2 <- seqs %>%
  group_by(region, GISAID_uploads) %>%
  summarise(max = max(sequences,na.rm=TRUE))

seqs2<- seqs %>%
  group_by(region) %>%
  summarise(GISAID = max(GISAID_uploads,na.rm=TRUE), sequences = max(sequences))

seqs2$region <- factor(seqs2$region)
seqs2$region <- relevel(seqs2$region, "Canada")

ggplot(seqs2, aes(x = region, y = sequences, fill = 'sequences')) +
  geom_col() +
  geom_col(aes(x = region, y = GISAID, fill = 'GISAID')) +
  ylab("number of sequences") +
  ggtitle("Cumulative number of sequences passing national QC standards and sequences uploaded to GISAID \nby region") +
  guides(x = guide_axis(angle = 90)) +
  geom_text(aes(y = GISAID, label = GISAID), vjust = 1.5, colour = "black") +
  labs(fill='') +
  scale_fill_manual(values = c("#9ADADC", "#457B9D"))

ggsave("gisaid_seqs_totals.png", width = 25, height = 25, units = 'cm')


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
  ggtitle("nombre cumulé de séquences télechargés à GISAID per région et date") +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) 

ggsave("gisaid_prov_date_fr.png", width = 25, height = 25, units = 'cm')


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
  ggtitle("nombre cumulé de séquences qui passent les normes nationales cq et séquences \ntélechargés à GISAID per région") +
  guides(x = guide_axis(angle = 90)) +
  geom_text(aes(y = GISAID, label = GISAID), vjust = 1.5, colour = "black") +
  labs(fill='') +
  scale_fill_manual(values = c("#9ADADC", "#457B9D"))

ggsave("gisaid_seqs_totals_fr.png", width = 25, height = 25, units = 'cm')


'''
seqs3 <- read.csv("stats_grouped_by_month.csv", header = TRUE)
seqs3$month <- factor(seqs3$month, levels = c("June", "July", "August", "September", "October", "November", "December"))

ggplot(seqs3, aes(x = month, y = seqs)) + 
  geom_col(show.legend = FALSE, aes(fill = month)) +
  guides(x = guide_axis(angle = 90)) +
  xlab("month") + 
  ylab("number of sequences") +
  ggtitle("Number of sequences by month")



seqs4 <- read.csv("stats_grouped_by_provs.csv", header = TRUE)
seqs4$month <- factor(seqs4$month, levels = c("June", "July", "August", "September", "October", "November", "December"))

ggplot(seqs4, aes(x = prname, y = seqs)) + 
  geom_col(show.legend = FALSE, aes(fill = prname)) +
  guides(x = guide_axis(angle = 90)) +
  xlab("province") + 
  ylab("number of sequences") +
  ggtitle("Number of sequences by province")



ggplot(seqs4, aes(x = prname)) + 
  geom_col(show.legend = FALSE, aes(y = caseload, fill = prname, alpha = 0.5)) +
  geom_col(show.legend = FALSE, aes(y = seqs, fill = prname)) +
  guides(x = guide_axis(angle = 90)) +
  #scale_fill_identity(name = 'opacity', guide = 'legend',labels = c('cases', 'sequences')) +
  scale_y_log10() +
  geom_text(aes(label = proportion, y = seqs), size = 3) +
  xlab("province") + 
  ylab("number") +
  ggtitle("Number of cases and sequences by province")

ggsave("sequences_cases_proportions_prov.png", width = 25, height = 25, units = 'cm')

seqs8 <- read.csv("Dec18nums.csv", header = TRUE)
seqs8$GISAIDproportion <- seqs8$GISAID / seqs8$seqs

ggplot(seqs8, aes(x = prname, y = GISAIDproportion)) + 
  geom_col(show.legend = FALSE, aes(fill = prname)) +
  guides(x = guide_axis(angle = 90)) +
  xlab("province") + 
  ylab("proportion of sequences uploaded") +
  ggtitle("Proportion of sequences uploaded to GISAID by province")

#ggsave("proportion_uploaded_to_GISAID_revised.png", width = 25, height = 25, units = 'cm')

seqs9 <- read.csv("stats_GISAID.csv", header = TRUE)


#ggsave("sequences_cases_proportions_prov_revised.png", width = 25, height = 25, units = 'cm')

ggplot(seqs9, aes(x = prname, y = GISAIDproportion)) + 
  geom_col(show.legend = FALSE, aes(fill = prname)) +
  guides(x = guide_axis(angle = 90)) +
  xlab("province") + 
  ylab("proportion of sequences uploaded") +
  ggtitle("Proportion of sequences uploaded to GISAID by province")

#ggsave("proportion_uploaded_to_GISAID_revised2.png", width = 25, height = 25, units = 'cm')

