library(ggplot2)
library(ggridges)
library(reshape)
library(dplyr)
library(viridis)

setwd("~/Projects/gisaid_uploads")
seqs <- read.csv('days_to_deposition_90.csv', header = TRUE)

seqs$country <- factor(seqs$country)
seqs$country <- relevel(seqs$country, "Canada")

names(seqs)[2] <- 'Aug_2021'
names(seqs)[5] <- 'Nov_2021'

#plot of median days to deposition to GISAID over last 90 days
ggplot(seqs, aes(x = country, y = Aug_2021, fill = 'Aug_2021', alpha = 0.5)) +
  geom_col() +
  scale_alpha(guide='none') +
  geom_col(aes(x = country, y = Nov_2021, fill = 'Nov_2021', alpha = 0.5)) +
  ylab("Median number of days to deposition\nover 90 day period") +
  ggtitle("Median number of days from collection to deposition in GISAID by country over\nlast 90 days in Aug and Nov 2021") +
  guides(x = guide_axis(angle = 90)) +
  geom_text(size=2.5, aes(y = Aug_2021, label = Aug_2021), vjust = 1.5, colour = "black") +
  geom_text(size=2.5, aes(y = Nov_2021, label = Nov_2021), vjust = 1.5, colour = "black") +
  labs(fill='Month') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_manual(values = c("red", "blue"))

ggsave("gisaid_seqs_by_country_90_days_nov_2021.png", width = 20, height = 20, units = 'cm')

#plot of median days to deposition to GISAID since start of pandemic
seqs <- read.csv('days_to_deposition.csv', header = TRUE)

seqs$country <- factor(seqs$country)
seqs$country <- relevel(seqs$country, "Canada")

names(seqs)[2] <- 'Aug_2021'
names(seqs)[3] <- 'Nov_2021'

ggplot(seqs, aes(x = country, y = Aug_2021, fill = 'Aug_2021', alpha = 0.5)) +
  geom_col() +
  scale_alpha(guide='none') +
  geom_col(aes(x = country, y = Nov_2021, fill = 'Nov_2021', alpha = 0.5)) +
  ylab("Median number of days to deposition\nsince start of pandemic") +
  ggtitle("Median number of days from collection to deposition in GISAID by country since\nthe start of the pandemic in Aug and Nov 2021") +
  guides(x = guide_axis(angle = 90)) +
  geom_text(size=2.5, aes(y = Aug_2021, label = Aug_2021), vjust = 1.5, colour = "black") +
  geom_text(size=2.5, aes(y = Nov_2021, label = Nov_2021), vjust = 1.5, colour = "black") +
  labs(fill='Month') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_manual(values = c("red", "blue"))

ggsave("gisaid_seqs_by_country_pandemic_start_nov_2021.png", width = 20, height = 20, units = 'cm')
