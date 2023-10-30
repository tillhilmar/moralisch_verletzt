library("academictwitteR")
library("purrr")
library("tibble")
library("tidyverse")
library("plyr")
library("dplyr")
library("data.table")
library("lubridate")
library("readxl")
library("writexl")
library("ggthemes")
library("kableExtra")
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda)

# Twitter data downloaded via Academic API
ALL_moralischverletzt <- readRDS("Twitterdata/ALL_moralischverletzt.rds")
ALL_moralischverletzt_2022 <- readRDS("ALL_moralischverletzt_2022.rds")
ALL_moralischverletzt_final <- rbind(ALL_moralischverletzt, ALL_moralischverletzt_2022)
#saveRDS(ALL_moralischverletzt_final, "ALL_moralischverletzt_final.rds")
write_xlsx(ALL_moralischverletzt_final,"ALL_moralischverletzt_final.xlsx")

# ordered by date, without conversations
ALL_moralischverletzt_final_ord <- arrange(ALL_moralischverletzt_final, ALL_moralischverletzt_final$created_at)
write_xlsx(ALL_moralischverletzt_final_ord,"ALL_moralischverletzt_final_ord.xlsx")

# analysis ----

# tokenization
ger_stopwords <- read_lines("https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt")
custom_stopwords <- setdiff(ger_stopwords, stopwords("german"))
custom_stopwords <- c(custom_stopwords, "amp", "rt", "+", "via", "de", "<U+304C>", "<U+0001F926><U+200D><U+2642>", "<U+0001F926><U+200D><U+2640>", "<U+0001F1E9><U+0001F1EA>", "<U+306E>", "aldigi")
corp_ALL_moralischverletzt <- quanteda::corpus(ALL_moralischverletzt_final_noRT)
dfm_ALL_moralischverletzt <- dfm(corp_ALL_moralischverletzt, remove = c(stopwords("german"), stopwords("english"), custom_stopwords), remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, tolower = FALSE, verbose = FALSE)

freq <- textstat_frequency(dfm_ALL_moralischverletzt)
head(freq, 200)
freq[1:40, 1:4] %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "top_words.html", self_contained = T)

#identifying top hashtags
hashtags_dfm <- dfm_select(dfm_ALL_moralischverletzt, ('#*'), selection = "keep")
tstat_freq <- textstat_frequency(hashtags_dfm, n = 40)
head(tstat_freq, 40)
tstat_freq[1:20, 1:4] %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "top_hashtags.html", self_contained = T)

#identifying top mentions
mentions_dfm <- dfm_select(dfm_ALL_moralischverletzt, ('@*'), selection = "keep")
tstat_freq <- textstat_frequency(mentions_dfm, n = 40)
head(tstat_freq, 20)
tstat_freq[1:40, 1:4] %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "top_mentions.html", self_contained = T)

# analyzing user descriptions ----

library("quanteda.textplots")
library("stopwords")
stopwords_english <- stopwords("english")
stopwords_german <- stopwords ("german")
custom_stopwords <- c("🏳‍🌈", "👩‍🎓", "🇩🇪", "🇪🇺")

user_descriptions <- ALL_moralischverletzt_final$user_description
user_descriptions <- unique(user_descriptions)

corp_user_descriptions <- quanteda::corpus(user_descriptions)

tokens_user_descriptions <- tokens(corp_user_descriptions,remove_punct = TRUE,remove_symbols = TRUE,remove_numbers = TRUE,remove_url = TRUE, remove_separators = TRUE, split_hyphens = FALSE, split_tags = FALSE, include_docvars = TRUE,padding = FALSE,verbose = FALSE)
tokens_user_descriptions <- tokens_remove(tokens_user_descriptions, pattern = c(stopwords_english, stopwords_german, custom_stopwords))

freq <- textstat_frequency(dfm_user_descriptions)
head(freq, 200)

fcmat <- fcm(tokens_user_descriptions, context = "window", tri = FALSE)
feat <- names(topfeatures(fcmat, 60))
fcm_select(fcmat, pattern = feat) %>%
  quanteda.textplots::textplot_network(min_freq = 0.8)

# adding conversations -----

# determining the conversation ids from main dataframe
conversations_moralischverletzt_list <- ALL_moralischverletzt_final$conversation_id
conversations_moralischverletzt_list <- unique(conversations_moralischverletzt_list)
conversations_moralischverletzt <- as.numeric(conversations_moralischverletzt_list)

#command used to fetch the conversations
fetched_conversations <- map_dfr(conversations_moralischverletzt_list, ~get_all_tweets(start_tweets = "2007-01-01T00:00:00Z", end_tweets = "2023-05-01T00:00:00Z", bearer_token = "...", n = 90000, conversation_id = ., data_path = "data1/"))

# plots ----

ALL_moralischverletzt_simplifieddate <- ALL_moralischverletzt_final
ALL_moralischverletzt_simplifieddate$created_at <- as_date(ALL_moralischverletzt_final$created_at)
ALL_moralischverletzt_simplifieddate_NoRT <- subset(ALL_moralischverletzt_simplifieddate, !(in_reference == 'retweeted'))


counts <- ALL_moralischverletzt_simplifieddate %>%
  group_by(created_at) %>%
  tally

wsjPal <- c('#1C366B',
            '#C4CFD0',
            '#1DACE8',
            '#F24D29',
            '#76A08A',
            '#9A872D')

#Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "English")
#Sys.setlocale("LC_TIME", "German")

#PLOT
gb <- counts %>% 
  ggplot(aes(x = created_at, y = n)) +
  geom_line(size = 0.5)+
  labs(x = "Date", y = "Keyword", title = "")+
  scale_color_manual(values = wsjPal) +
  #scale_x_date(date_labels = "%b-%d-%Y")+
  scale_x_date(breaks = "1 month", date_labels= "%b-%Y") +
  #scale_color_wsj(palette = "colors6")+
  #scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_manual(values = wes.palette(n=3, name="Zissou"))+
  theme_wsj()+
  theme(plot.title = element_text(size = 13, family="sans"),
        plot.subtitle = element_text(size = 11, family="sans"),
        plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
gb


#PLOT without retweets
counts <- ALL_moralischverletzt_simplifieddate_NoRT %>%
  group_by(created_at) %>%
  tally

wsjPal <- c('#1C366B',
            '#C4CFD0',
            '#1DACE8',
            '#F24D29',
            '#76A08A',
            '#9A872D')

#Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "English")
#Sys.setlocale("LC_TIME", "German")


start_date <- as.Date("2021-05-04")
end_date <- as.Date("2022-12-29")

gb <- counts %>% 
  ggplot(aes(x = created_at, y = n)) +
  geom_line(size = 0.5) +
  labs(x = "Date", y = "Keyword", title = "04. Mai 2021 - 29. Dezember 2022, n = 1137 Tweets (ohne Retweets)") +
  scale_color_manual(values = wsjPal) +
  scale_x_date(limits = c(start_date, end_date), breaks = "1 month", date_labels= "%b-%Y") +
  theme_wsj() +
  theme(plot.title = element_text(size = 13, family="sans"),
        plot.subtitle = element_text(size = 11, family="sans"),
        plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom") +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

print(gb)


gb <- counts %>% 
  ggplot(aes(x = created_at, y = n)) +
  geom_line(size = 0.5)+
  labs(x = "Date", y = "Keyword", title = "n = 1137 Tweets (ohne Retweets), 04. Mai 2021 - 29. Dezember 2022")+
  scale_color_manual(values = wsjPal) +
  #scale_x_date(date_labels = "%b-%d-%Y")+
  scale_x_date(breaks = "1 month", date_labels= "%b-%Y") +
  #scale_color_wsj(palette = "colors6")+
  #scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_manual(values = wes.palette(n=3, name="Zissou"))+
  theme_wsj()+
  theme(plot.title = element_text(size = 13, family="sans"),
        plot.subtitle = element_text(size = 11, family="sans"),
        plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
gb

# not used: Plot just the first couple of months

ALL_moralischverletzt_simplifieddate_firstmonths <- subset(ALL_moralischverletzt_simplifieddate, created_at <= '2021-09-30 23:59:59')
counts <- ALL_moralischverletzt_simplifieddate_firstmonths %>%
  group_by(created_at) %>%
  tally

wsjPal <- c('#1C366B',
            '#C4CFD0',
            '#1DACE8',
            '#F24D29',
            '#76A08A',
            '#9A872D')


gb <- counts %>% 
  ggplot(aes(x = created_at, y = n)) +
  geom_line(size = 0.5)+
  labs(x = "Date", y = "Keyword", title = "")+
  scale_color_manual(values = wsjPal) +
  #scale_x_date(date_labels = "%b-%d-%Y")+
  scale_x_date(breaks = "2 month", date_labels= "%b-%Y") +
  #scale_color_wsj(palette = "colors6")+
  #scale_color_viridis(discrete = TRUE, option = "D")+
  #scale_color_manual(values = wes.palette(n=3, name="Zissou"))+
  theme_wsj()+
  theme(plot.title = element_text(size = 13, family="sans"),
        plot.subtitle = element_text(size = 11, family="sans"),
        plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
gb

# No retweets
# Plot just the first hours

ALL_moralischverletzt_final_noRT <- subset(ALL_moralischverletzt_final, !(in_reference == 'retweeted'))
ALL_moralischverletzt_final_firsthours_noRT <- subset(ALL_moralischverletzt_final_noRT, created_at <= '2021-05-20T11:50:52.000Z')

ALL_moralischverletzt_final_firsthours_noRT$created_at <- ymd_hms(ALL_moralischverletzt_final_firsthours_noRT$created_at)

counts <- ALL_moralischverletzt_final_firsthours_noRT %>%
  group_by(created_at) %>%
  tally

library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)

# Aggregate data by hour
counts_hourly <- counts %>%
  mutate(hour = floor_date(created_at, unit = "hour")) %>%
  group_by(hour) %>%
  dplyr::summarize(n = sum(n))

# Plot
start_date <- as.POSIXct("2021-05-04 09:00:00", tz = "UTC")

gb <- ggplot(counts_hourly, aes(x = hour, y = n)) +
  geom_line(size = 0.5) +
  labs(x = "Date", y = "Keyword", title = "04. Mai 2021 - 20. Mai 2021, n = 798 Tweets (ohne Retweets)") +
  scale_color_manual(values = wsjPal) +
  scale_x_datetime(limits = c(start_date, NA), breaks = "24 hour", date_labels= "%d-%b %H:%M") +
  theme_wsj() +
  theme(plot.title = element_text(size = 13, family="sans"),
        plot.subtitle = element_text(size = 11, family="sans"),
        plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "bottom") +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

print(gb)

