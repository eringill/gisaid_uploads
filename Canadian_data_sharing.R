library(ggplot2)
library(viridis)
library(ggrepel)
library(scales)

setwd("~/Projects/gisaid_uploads")
seqs <- read.csv('seqs2021.csv', header = TRUE)

seqs$region <- factor(seqs$region)
seqs$region <- relevel(seqs$region, "Canada")

seqs$type <- factor(seqs$type)
seqs$type <-relevel(seqs$type, "not in gisiad")

ggplot(seqs, aes(x = region, y = sequences, fill = type)) + 
  geom_bar(position = "fill", stat = "identity") +
  guides(x = guide_axis(angle = 90)) +
  xlab("Region") + 
  ylab("Proportion of sequences") +
  ggtitle("Proportion of samples sequenced and uploaded to GISAID in\n2021 by region") +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  labs(fill = "")

ggsave("samples_gisiad_2021.png", width = 15, height = 15, units = 'cm')

deposition <- read.csv('days_to_deposition.csv', header=TRUE)

deposition$country <- factor(deposition$country)
deposition$country <- relevel(deposition$country, "Canada")

ggplot(deposition, aes(x = country, y = median_days_to_deposition)) + 
  geom_col(show.legend = FALSE, aes(fill = country)) +
  guides(x = guide_axis(angle = 90)) +
  xlab("Country") + 
  ylab("Median days to deposition") +
  ggtitle("Median days to deposition in GISAID from sample collection by\ncountry") +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  geom_hline(yintercept=21)

ggsave("days_to_deposition.png", width = 15, height = 15, units = 'cm')

shared <- read.csv('cases_sequenced_and_shared.csv', header=TRUE)

shared$country <- factor(shared$country)
shared$country <- relevel(shared$country, "Canada")

ggplot(shared, aes(x = country, y = percent_cases_sequenced_and_shared)) + 
  geom_col(show.legend = FALSE, aes(fill = country)) +
  guides(x = guide_axis(angle = 90)) +
  xlab("Country") + 
  ylab("Percent cases sequenced and shared") +
  ggtitle("Percent cases sequenced and shared in GISAID over last 90 days") +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  geom_hline(yintercept=4.94)

ggsave("sequenced_and_shared.png", width = 15, height = 15, units = 'cm')

total <- read.csv("total_data_jul16.csv",header=TRUE)

total$region <- factor(total$region)
total$region <- relevel(total$region, "Canada")

ggplot(total, aes(x = region, y = sequences_total, fill = 'Total sequences')) +
  geom_col() +
  geom_col(aes(x = region, y = sequences_qc, fill = 'QC sequences')) +
  geom_col(aes(x = region, y = gisaid, fill = 'GISAID')) +
  geom_col(aes(x = region, y = data_portal, fill = 'Data Portal')) +
  xlab("Region") + 
  ylab("Number of sequences") +
  ggtitle("Total number of samples sequenced and shared by region") +
  guides(x = guide_axis(angle = 90)) +
  #scale_y_log10() +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  labs(fill = "")

ggsave("sequenced_and_shared_canada.png", width = 15, height = 15, units = 'cm')

shared_c <- read.csv('cases_seq_shared_canada_90_days.csv', header=TRUE)

shared_c$region <- factor(shared_c$region)
shared_c$region <- relevel(shared_c$region, "Canada")

ggplot(shared_c, aes(x = region, y = percent)) + 
  geom_col(show.legend = FALSE, aes(fill = region)) +
  guides(x = guide_axis(angle = 90)) +
  xlab("Region") + 
  ylab("Percent cases sequenced and shared") +
  ggtitle("Percent cases sequenced and shared in GISAID\nover last 90 days") +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  geom_hline(yintercept=1.5)

ggsave("sequenced_and_shared_canada_90_days.png", width = 15, height = 15, units = 'cm')

shared_global <- read.csv('days_to_deposition_90.csv', header=TRUE)

shared_global$country <- factor(shared_global$country)
shared_global$country <- relevel(shared_global$country, "Canada")

ggplot(shared_global, aes(x = country, y = median_days_to_deposition)) + 
  geom_col(show.legend = FALSE, aes(fill = country)) +
  guides(x = guide_axis(angle = 90)) +
  xlab("Country") + 
  ylab("Median days to deposition") +
  ggtitle("Median days to deposition in GISAID from sample collection\nby country over last 90 days ") +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  geom_hline(yintercept=18)

ggsave("days_to_deposition_90_days.png", width = 15, height = 15, units = 'cm')

ggplot(shared_global, aes(x = country, y = percent_cases_sequenced_and_shared)) + 
  geom_col(show.legend = FALSE, aes(fill = country)) +
  guides(x = guide_axis(angle = 90)) +
  xlab("Country") + 
  ylab("Percent cases sequenced and shared") +
  ggtitle("Percent cases sequenced and shared in GISAID over last 90 days") +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  geom_hline(yintercept=5.16)+
  geom_text(size=2.5, aes(y = percent_cases_sequenced_and_shared, label = cases), vjust = -1.5, colour = "black")

ggsave("sequenced_and_shared_90.png", width = 15, height = 15, units = 'cm')

can <- subset(shared_global, country == "Canada")

ggplot(shared_global, aes(x = cases, y = percent_cases_sequenced_and_shared)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_point(data = can, x = can$cases, y = can$percent_cases_sequenced_and_shared, color = "red") +
  ylab("Percent cases sequenced and shared") +
  xlab("Cases") +
  ggtitle("Number of cases over last 90 days vs. percent cases\nsequenced and shared in GISAID") +
  geom_text_repel(label=shared_global$country) +
  scale_x_continuous(labels = comma)

ggsave("cases_vs_shared.png", width = 15, height = 15, units = 'cm')

