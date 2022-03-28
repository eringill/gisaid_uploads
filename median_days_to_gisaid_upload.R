library(ggplot2)
library(ggridges)
library(reshape)
library(plyr)
library(dplyr)
library(viridis)


setwd("~/Projects/gisaid_uploads")
seqs <- read.csv('data/days_to_deposition_90.csv', header = TRUE)

seqs$country <- factor(seqs$country)
seqs$country <- relevel(seqs$country, "Canada")

names(seqs)[2] <- 'Aug_2021'
names(seqs)[5] <- 'Nov_2021'
names(seqs)[6] <- 'Mar_2022'

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
seqs <- read.csv('data/days_to_deposition.csv', header = TRUE)

seqs$country <- factor(seqs$country)
seqs$country <- relevel(seqs$country, "Canada")

names(seqs)[2] <- 'Aug_2021'
names(seqs)[3] <- 'Nov_2021'

# bar plot with dates overlapping (red and blue)
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


seqs <- subset(seqs, select = -c(percent_cases_sequenced_and_shared, cases))
seqsmelt <- melt(seqs)

names(seqsmelt)[2] <- 'date'
names(seqsmelt)[3] <- 'days_to_deposition'

# bar plot colored by month (each country has dates stacked together)
ggplot(seqsmelt, aes(x = country, y = days_to_deposition, fill = date)) +
  geom_bar(position ='dodge', stat = 'identity') +
  ylab("Median number of days to deposition\nover 90 day period") +
  xlab("Country") +
  ggtitle("Median number of days from collection to deposition in GISAID by country over\nlast 90 days in Aug 2021, Nov 2021 and Mar 2022") +
  guides(x = guide_axis(angle = 90)) +
  labs(fill='Month') +
  scale_fill_viridis(discrete = TRUE) 

ggsave("plots/gisaid_seqs_by_country_90_days_mar_2022.png", width = 20, height = 20, units = 'cm')

seqsmelt$date <- mapvalues(seqsmelt$date, from=c("Aug_2021","Nov_2021","Mar_2022"), to=c("2021-08-01", "2021-11-01", "2022-03-01"))
seqsmelt$date <- as.Date(seqsmelt$date)

# line plot
ggplot(seqsmelt, aes(x = date, y = days_to_deposition)) +
  geom_line(aes(color = country, linetype = country)) +
  scale_color_viridis(discrete = TRUE) +
  ylab("Median number of days to deposition\nover 90 day period") +
  xlab("Country") +
  ggtitle("Median number of days from collection to deposition in GISAID by country over\nlast 90 days in Aug 2021, Nov 2021 and Mar 2022")

# facet wrap line plot
gplot(seqsmelt, aes(x = date, y = days_to_deposition)) +
  geom_line(aes(color = country, linetype = country)) +
  facet_wrap(~country) +
  scale_color_viridis(discrete = TRUE) +
  ylab("Median number of days to deposition\nover 90 day period") +
  xlab("Country") +
  ggtitle("Median number of days from collection to deposition in GISAID by country over\nlast 90 days in Aug 2021, Nov 2021 and Mar 2022")

# facet wrap bar plot
ggplot(seqsmelt, aes(x = date, y = days_to_deposition, fill = country)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~country, ncol = 2) +
  scale_fill_viridis(discrete = TRUE) +
  ylab("Median number of days to deposition\nover 90 day period") +
  xlab("Country") +
  ggtitle("Median number of days from collection to deposition in GISAID by country over\nlast 90 days in Aug 2021, Nov 2021 and Mar 2022")
