# A script to plot weekly fraction of COVID-19 cases sequenced by province 

library(ggplot2)
library(ggridges)

setwd("~/Projects/gisaid_uploads")
seqs <- read.csv('data.csv', header = TRUE)


ggplot(seqs, aes(x = date, y = GISAID_uploads)) + 
  geom_col(show.legend = FALSE, aes(fill = region)) +
  facet_wrap(~region, ncol = 1, scales = "free_y") +
  guides(x = guide_axis(angle = 90)) +
  xlab("date") + 
  ylab("number of sequences") +
  scale_fill_hue(l=40) +
  ggtitle("Cumulative number of sequences uploaded to GISAID by province and month")

ggsave("gisaid_prov_date.png", width = 25, height = 25, units = 'cm')

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

