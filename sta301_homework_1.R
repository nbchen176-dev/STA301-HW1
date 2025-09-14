library(tidyverse)
library(mosaic)

#1. Taylor Swift

#Danceability
ggplot(TS) +
  geom_histogram(aes(x = danceability), color = "white") +
  labs(title = "Dance to Taylor Swift",
       x = "Danceability on a Scale of 0 to 1",
       y = "Number of Songs") +
  theme_minimal()

TS %>%
  summarize(mean_dance = mean(danceability),
            sd_dance = sd(danceability))

#Valence
TS = TS %>%
  mutate(album = factor(album, levels = c("Taylor Swift", "Fearless", "Speak Now",
                                          "Red", "1989", "reputation", "Lover", "folklore", "evermore", "Midnights")))
ggplot(TS) +
  geom_boxplot(aes(x = album, y = valence)) +
  labs(title = "1989 is the Happiest TS Album",
       y = "Valence on a Scale of 0 to 1",
       x = "TS Albums (ordered chronologically)") +
  theme_minimal() +
  coord_flip()

#Energy and Loudness
TS = TS %>%
  mutate(version = ifelse(album == "Red" | album == "Fearless",
                        yes="Taylor's Version", no="original version"))

ggplot(TS) +
  geom_point(aes(x = loudness, y = energy)) +
  facet_wrap(~version) +
  labs(title = "Louder TS Tracks Tend to be Energetic",
       x = "Loudness of TS Tracks (dBu)",
       y = "Energy of TS Track on Scale of 0 to 1") +
  theme_minimal() 
  

#Song Duration
TS %>%
  summarize(max_length = max(duration),
            mean_length = mean(duration),
            sd_length = sd(duration))
  #Song name: All Too Well (10 Minute Version) [Taylor's Version] [From The Vault]



#2. The Avengers

xtabs(~death, data = avengers) %>%
  prop.table() %>%
  round(2)

xtabs(~death + return, data = avengers) %>%
  prop.table() %>%
  addmargins() %>%
  round(2)

xtabs(~death + return, data = avengers) %>%
  prop.table(margin = 1) %>%
  round(2)

xtabs(~death + return, data = avengers) %>%
  prop.table(margin = 2) %>%
  round(2)


#3. Super Bowl Commercials

#Part A
ads = ads %>%
  group_by(year) %>%
  mutate(year_count = n())

ggplot(ads) +
  geom_line(aes(x = year, y = year_count)) +
  labs(title = "Number of Super Bowl Ads Varies Greatly Each Year",
     x = "Year (2000-2020)",
     y = "Number of Super Bowl Ads") +
  theme_minimal()


#Part B
#1.
xtabs(~animals, data = ads) %>%
  prop.table() %>%
  round(2)

#2.
ads %>%
  group_by(animals) %>%
  summarize(median_views = median(views))

#3.
ads %>%
  group_by(animals) %>%
  summarize(sd_dislikes = sd(dislikes))

#4.
ads %>%
  group_by(animals) %>%
  summarize(ninety_perc = quantile(likes, 0.90))

#Part C
ggplot(ads) +
  geom_bar(aes(x = brand, fill = animals)) +
  labs(title = "Most Brands Don't Have Animals in their Super Bowl Ads",
       x = "Brand",
       y = "Number of Super Bowl Ads") +
  scale_fill_discrete(name = "Animals Present",
                      labels = c("No", "Yes")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 8))
  

#Part D
#P(B | A) = P(B) and P(A | B) = P(A)
xtabs(~celebrity, data = ads) %>%
  prop.table() %>%
  round(2)

xtabs(~patriotic, data = ads) %>%
  prop.table() %>%
  round(2)

xtabs(~celebrity + patriotic, data = ads) %>%
  prop.table(margin = 2) %>%
  round(2)

xtabs(~celebrity + patriotic, data = ads) %>%
  prop.table(margin = 1) %>%
  round(2)

#Celebrity and Patriotic look nearly independent

ggplot(ads) +
  geom_bar(aes(x = patriotic, fill = celebrity)) +
  labs(title = "Most Celebrities Don't Show Up in SB Ads, Patriotic or Not",
     x = "Whether SB Ad was Patriotic",
     y = "Number of Super Bowl Ads") +
  scale_fill_discrete(name = "Celebrities Present",
                      labels = c("No", "Yes")) +
  theme_minimal() 
  
#Part E
xtabs(~funny, data = ads) %>%
  prop.table() %>%
  round(2)

xtabs(~danger, data = ads) %>%
  prop.table() %>%
  round(2)

xtabs(~danger + funny, data = ads) %>%
  prop.table(margin = 1) %>%
  round(2)

xtabs(~danger + funny, data = ads) %>%
  prop.table(margin = 2) %>%
  round(2)

#Danger and Funny don't seem to be independent

funny_column = ads %>%
  mutate(funny = ifelse(funny == "FALSE", "Unfunny", "Funny"))

ggplot(funny_column) +
  geom_bar(aes(x = danger)) +
  facet_wrap(~funny) +
  labs(title = "Most Funny Ads Are Not Dangerous",
       x = "Whether SB Ad was Dangerous",
       y = "Number of Super Bowl Ads") +
  theme_minimal()

#4 Storytelling with Plots

#https://www.kaggle.com/datasets/nyagami/ea-sports-fc-25-database-ratings-and-stats

eight5ovr_players = all_players %>%
  filter(OVR >= 85) %>%
  select(OVR, Finishing, DEF, Preferred.foot)

ggplot(eight5ovr_players) +
  geom_boxplot(aes(x=factor(OVR), y=DEF)) + 
  labs(title = "Higher OVR Doesn't Equate to Good Defense in EA FC 25",
       x = "Overall Rating from 85-91 (OVR) ",
       y = "Defensive Ability (DEF)") +
  coord_flip() +
  theme_minimal()

ggplot(eight5ovr_players) +
  geom_boxplot(aes(x=Preferred.foot, y= OVR)) +
  coord_flip() +
  theme_minimal()


