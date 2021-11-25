library(ggplot2)
library(ggridges)
library(reshape)
library(dplyr)
library(viridis)
library(anytime)

setwd("~/Projects/gisaid_uploads")

seqs <- read.csv('data/data_portal_proportions_month.csv', header = TRUE)

seqs$year_month <- anydate(seqs$year_month)

#Sampling purpose
ggplot(seqs, aes(x = year_month, y = sampling_purpose, group = province)) +
  geom_line(aes(color=province)) +
  ylab("Proportion") +
  xlab("Collection date") +
  ggtitle("Proportion of sequences in the VirusSeq data portal by collection month and province\nthat have sampling purpose information") +
  guides(x = guide_axis(angle = 90)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")

ggsave("data_portal_sampling_purpose_month_prov.png", width = 20, height = 20, units = 'cm')


#age bin
ggplot(seqs, aes(x = year_month, y = age_bin, group = province)) +
  geom_line(aes(color=province)) +
  ylab("Proportion") +
  xlab("Collection date") +
  ggtitle("Proportion of sequences in the VirusSeq data portal by collection month and province\nthat have age bin information") +
  guides(x = guide_axis(angle = 90)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")

ggsave("data_portal_age_bin_month_prov.png", width = 20, height = 20, units = 'cm')

#gender
ggplot(seqs, aes(x = year_month, y = gender, group = province)) +
  geom_line(aes(color=province)) +
  ylab("Proportion") +
  xlab("Collection date") +
  ggtitle("Proportion of sequences in the VirusSeq data portal by collection month and province\nthat have gender information") +
  guides(x = guide_axis(angle = 90)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")

ggsave("data_portal_gender_month_prov.png", width = 20, height = 20, units = 'cm')

#sequencing purpose
ggplot(seqs, aes(x = year_month, y = sequencing_purpose, group = province)) +
  geom_line(aes(color=province)) +
  ylab("Proportion") +
  xlab("Collection date") +
  ggtitle("Proportion of sequences in the VirusSeq data portal by collection month and province\nthat have sequencing purpose information") +
  guides(x = guide_axis(angle = 90)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")

ggsave("data_portal_sequencing_purpose_month_prov.png", width = 20, height = 20, units = 'cm')

#Ct
ggplot(seqs, aes(x = year_month, y = Ct, group = province)) +
  geom_line(aes(color=province)) +
  ylab("Proportion") +
  xlab("Collection date") +
  ggtitle("Proportion of sequences in the VirusSeq data portal by collection month and province\nthat have Ct information") +
  guides(x = guide_axis(angle = 90)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y")

ggsave("data_portal_Ct_month_prov.png", width = 20, height = 20, units = 'cm')

