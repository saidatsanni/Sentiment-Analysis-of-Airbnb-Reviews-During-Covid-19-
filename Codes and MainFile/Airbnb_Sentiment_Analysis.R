library(readxl)
library(tm)
library(NLP)
library(wordcloud)
library(RColorBrewer)
library(slamlibrary(topicmodels))
library(SnowballC)
library(httr)
library(dplyr)
library(openxlsx)
library(plotly)
library(ggplot2)
library(fastDummies)
library(PerformanceAnalytics)
library(tidytext)
library(MASS) 
library(wordcloud)
library(tm)
library(RColorBrewer) 
library(SnowballC) 
library(NLP)
library(syuzhet)
library(stringi)
library(topicmodels)
library(ROAuth)
library(stringr)
library(sqldf)
library(biclust)
library(cluster)
library(igraph)
library(fpc)
library(lubridate)
library(tidyr)
library(ggsci)

AirbnbData <- read_excel("ProjectData_AirbnbReviews.xlsx") #96,630 reviews
AirbnbData <- AirbnbData[!duplicated(AirbnbData[ , c("comments")]),] # Now get rid of duplicates

reviews <- AirbnbData$comments
reviews<- tolower(reviews) #Convert to lower case
reviews <- gsub(",", " ", reviews)  # Remove !
reviews <- gsub("[[:punct:]]", " ", reviews)  # Remove punctuation
reviews <- gsub("/", " ", reviews)
reviews <- gsub("[ |\t]{2,}", " ", reviews)  # Remove tabs
reviews <- gsub("amp", " ", reviews)  # "&" is "&amp" in HTML, so after punctuation removed ...
reviews <- gsub("^ ", "", reviews)  # Leading blanks # Remove blank spaces at the beginning
reviews <- gsub(" $", "", reviews)  # Lagging blanks # Remove blank spaces at the end
reviews <- gsub(" +", " ", reviews) # General spaces (should just do all whitespaces no?)
reviews <- gsub("[[:digit:]]", " ", reviews)


clean_reviews <- reviews #89,645 reviews
#add clean reviews back into the column in the cleanreviews page
AirbnbData$clean_reviews <- clean_reviews

###################################################
AirbnbData$year <- format(as.Date(AirbnbData$date, format = "%Y-%m-%d"),"%Y")
AirbnbData$month <- format(as.Date(AirbnbData$date, format = "%Y-%m-%d"),"%m")
AirbnbData$yrmonth <- format(as.Date(AirbnbData$date, format = "%Y-%m-%d"), "%Y-%m")
AirbnbData$weekday <- weekdays(AirbnbData$date)


##plot number of reviews by year and month
library(ggplot2)
freq <- as.data.frame(table(AirbnbData$yrmonth))
plot_freq <- ggplot(freq, aes(x=Var1, y=Freq))+
  geom_bar(stat = "identity", fill="darkgreen")+theme_classic()
plot_freq + xlab("Year and Month") +ylab("Total number of Reviews")


##plot Total reviews per day
freq <- as.data.frame(table(AirbnbData$weekday))
plot_freq <- ggplot(freq, aes(x=Var1, y= Freq))+
  geom_bar(stat = "identity", fill="black")+theme_classic()
plot_freq + xlab("Days of the Week") +ylab("Total number of Reviews per Day")

##plot Avg. reviews per day
n <- dim(AirbnbData)[1]

Weekday_mean <- AirbnbData %>%
  group_by(weekday)%>%
  summarise(mean_day = table(weekday)/n)


ggplot(Weekday_mean, aes(x =weekday , y = mean_day)) + geom_line(aes(group=1), size = 0.5) + geom_point()  
+ xlab("Days of the Week") +ylab("Average Reviews per Day")

#WorldCloud
#install.packages("wordcloud")
library(wordcloud)

wordcloud(AirbnbData$clean_reviews, min.freq=1500, scale=c(3.5, .7), random.order=FALSE, rot.per=0.5, 
          colors=brewer.pal(8, "Dark2"))


# Tokenizing Text ---------------------------------------------------------
tidyreviews <- AirbnbData %>%
  unnest_tokens(word, clean_reviews) %>%
  anti_join(stop_words)

aAll <- tidyreviews %>%
  count(word) %>%
  arrange(desc(n))

# Word Frequency of all Years Bar Chart ------------------------------------------------------
ggplot(subset(aAll[-3,], n>10000), aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + coord_flip()

#bigrams
clean_reviews <- data.frame(text = clean_reviews,stringsAsFactors = FALSE)

#Bigrams
separatedBigrams <- clean_reviews %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

unitedBigrams <- separatedBigrams %>%
  unite(bigram, word1, word2, sep = " ")
unitedBigrams <- unitedBigrams[-c(1,3,6,7,10,12,15,18,28),]


unitedBigrams %>%
  arrange(desc(n)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  top_n(20) %>%
  ungroup() %>%
  ggplot(aes(bigram, n)) +
  geom_col(show.legend = FALSE, fill = "#52854C", color = "#52854C") +
  labs(x = NULL, y = "n") +
  coord_flip() +
  ggtitle("Bigrams for the entire data period") +
  scale_color_manual(values=c("#E69F00"))

#Trigrams
clean_reviews <- data.frame(text = clean_reviews,stringsAsFactors = FALSE)
clean_reviewsTrigrams <- clean_reviews %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE) %>%
  unite(trigram, word1, word2, word3, sep = " ")
clean_reviewsTrigrams <- clean_reviewsTrigrams[-c(1:16,18:27,29:31,34:50,53:54,56:64,67:78,81:86,88:89,91:95),]


clean_reviewsTrigrams %>%
  arrange(desc(n)) %>%
  mutate(trigram = factor(trigram, levels = rev(unique(trigram)))) %>%
  top_n(15) %>%
  ungroup() %>%
  ggplot(aes(trigram, n)) +
  geom_col(show.legend = FALSE, fill = "#52854C", color = "#52854C") +
  labs(x = NULL, y = "n") +
  coord_flip() +
  ggtitle("Trigram for the entire data period") +
  scale_color_manual(values=c("#E69F00"))


# PositiveNegative Sentiment -----------------------------------------------
#create new datasets with each of the lexicons
reviews_bing <- tidyreviews %>%
  inner_join(get_sentiments("bing"))

#bing lexicon
reviews_bing %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) +
  ggtitle("All reviews - Sentiment") +
  labs(x = NULL, y = "Word Count") +
  coord_flip()




# polarity by yrmonth --------------------------------------------------------
reviews_polarity_yrmonth <- reviews_bing %>%
  count(sentiment, yrmonth) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative) * 100,
         percent_negative = negative / (positive + negative) * 100)

# Plot the bar chart.
par(mfrow=c(1,2))
plot(reviews_polarity_yrmonth$percent_positive,type = "o",col = "red", xlab = "Yrmonth", ylab = "Positivity Value",
     main = "Percentage Sentiment of Positivity chart")

plot(reviews_polarity_yrmonth$percent_negative,type = "o",col = "blue", xlab = "Yrmonth", ylab = "Negativity Value",
     main = "Percentage Sentiment of Negativity chart")
dev.off()

tidyreviews %>%
  count(word, sort = TRUE) %>%
  filter(n > 10000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



#Sentiment Analysis

#getting emotions using in-built function
textSentiment<-get_nrc_sentiment((reviews))

#calculating total score for each sentiment
sentimentScores<-data.frame(colSums(textSentiment[,]))

names(sentimentScores)<-"Score"
sentimentScores<-cbind("sentiment"=rownames(sentimentScores),sentimentScores)
rownames(sentimentScores)<-NULL
sentimentScores


#plotting the sentiments with scores
ggplot(data=sentimentScores,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),
                                                               stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of Airbnb Reviews in New York City")


# Extracting Yearmonth ---------------------------------------------------------
cleanreviews2020_03 <- AirbnbData[AirbnbData$yrmonth == "2020-03",]$clean_reviews
cleanreviews2020_04 <- AirbnbData[AirbnbData$yrmonth == "2020-04",]$clean_reviews 
cleanreviews2020_05 <- AirbnbData[AirbnbData$yrmonth == "2020-05",]$clean_reviews
cleanreviews2020_06 <- AirbnbData[AirbnbData$yrmonth == "2020-06",]$clean_reviews
cleanreviews2020_07 <- AirbnbData[AirbnbData$yrmonth == "2020-07",]$clean_reviews
cleanreviews2020_08 <- AirbnbData[AirbnbData$yrmonth == "2020-08",]$clean_reviews
cleanreviews2020_09 <- AirbnbData[AirbnbData$yrmonth == "2020-09",]$clean_reviews
cleanreviews2020_10 <- AirbnbData[AirbnbData$yrmonth == "2020-10",]$clean_reviews
cleanreviews2020_11 <- AirbnbData[AirbnbData$yrmonth == "2020-11",]$clean_reviews
cleanreviews2020_12 <- AirbnbData[AirbnbData$yrmonth == "2020-12",]$clean_reviews
cleanreviews2021_01 <- AirbnbData[AirbnbData$yrmonth == "2021-01",]$clean_reviews
cleanreviews2021_02 <- AirbnbData[AirbnbData$yrmonth == "2021-02",]$clean_reviews
cleanreviews2021_03 <- AirbnbData[AirbnbData$yrmonth == "2021-03",]$clean_reviews


#Sentiment Analysis 2020-03---------------------------------------------------

#getting emotions using in-built function
textSentiment2020_03<-get_nrc_sentiment((cleanreviews2020_03))

#calculating total score for each sentiment
sentimentScores2020_03<-data.frame(colSums(textSentiment2020_03[,]))
names(sentimentScores2020_03)<-"2020_03"
sentimentScores2020_03<-cbind("sentiment"=rownames(sentimentScores2020_03),sentimentScores2020_03)
rownames(sentimentScores2020_03)<-NULL
sentimentScores2020_03

#plotting the sentiments with scores
ggplot(data=sentimentScores2020_03,aes(x=sentiment,y=`2020_03`))+geom_bar(aes(fill=sentiment),
                                                                          stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("2020_03")+ggtitle("Sentiments of Airbnb Reviews in New York City in 2020_03")

sentimentPercentage2020_03 <-colSums(prop.table(textSentiment2020_03[, 1:8]))




#Sentiment Analysis 2020-04---------------------------------------------------

#getting emotions using in-built function
textSentiment2020_04<-get_nrc_sentiment((cleanreviews2020_04))

#calculating total score for each sentiment
sentimentScores2020_04<-data.frame(colSums(textSentiment2020_04[,]))
names(sentimentScores2020_04)<-"2020_04"
sentimentScores2020_04<-cbind("sentiment"=rownames(sentimentScores2020_04),sentimentScores2020_04)
rownames(sentimentScores2020_04)<-NULL
sentimentScores2020_04

#plotting the sentiments with scores
ggplot(data=sentimentScores2020_04,aes(x=sentiment,y=`2020_04`))+geom_bar(aes(fill=sentiment),
                                                                          stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("2020_04")+ggtitle("Sentiments of Airbnb Reviews in New York City in 2020_04")

sentimentPercentage2020_04 <-colSums(prop.table(textSentiment2020_04[, 1:8]))





#Sentiment Analysis 2020-05---------------------------------------------------

#getting emotions using in-built function
textSentiment2020_05<-get_nrc_sentiment((cleanreviews2020_05))

#calculating total score for each sentiment
sentimentScores2020_05<-data.frame(colSums(textSentiment2020_05[,]))
names(sentimentScores2020_05)<-"2020_05"
sentimentScores2020_05<-cbind("sentiment"=rownames(sentimentScores2020_05),sentimentScores2020_05)
rownames(sentimentScores2020_05)<-NULL
sentimentScores2020_05

#plotting the sentiments with scores
ggplot(data=sentimentScores2020_05,aes(x=sentiment,y=`2020_05`))+geom_bar(aes(fill=sentiment),
                                                                          stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("2020_05")+ggtitle("Sentiments of Airbnb Reviews in New York City in 2020_05")

sentimentPercentage2020_05 <-colSums(prop.table(textSentiment2020_05[, 1:8]))




#Sentiment Analysis 2020-06---------------------------------------------------

#getting emotions using in-built function
textSentiment2020_06<-get_nrc_sentiment((cleanreviews2020_06))

#calculating total score for each sentiment
sentimentScores2020_06<-data.frame(colSums(textSentiment2020_06[,]))
names(sentimentScores2020_06)<-"2020_06"
sentimentScores2020_06<-cbind("sentiment"=rownames(sentimentScores2020_06),sentimentScores2020_06)
rownames(sentimentScores2020_06)<-NULL
sentimentScores2020_06

#plotting the sentiments with scores
ggplot(data=sentimentScores2020_06,aes(x=sentiment,y=`2020_06`))+geom_bar(aes(fill=sentiment),
                                                                          stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("2020_06")+ggtitle("Sentiments of Airbnb Reviews in New York City in 2020_06")

sentimentPercentage2020_06 <-colSums(prop.table(textSentiment2020_06[, 1:8]))



#Sentiment Analysis 2020-07---------------------------------------------------

#getting emotions using in-built function
textSentiment2020_07<-get_nrc_sentiment((cleanreviews2020_07))

#calculating total score for each sentiment
sentimentScores2020_07<-data.frame(colSums(textSentiment2020_07[,]))
names(sentimentScores2020_07)<-"2020_07"
sentimentScores2020_07<-cbind("sentiment"=rownames(sentimentScores2020_07),sentimentScores2020_07)
rownames(sentimentScores2020_07)<-NULL
sentimentScores2020_07

#plotting the sentiments with scores
ggplot(data=sentimentScores2020_07,aes(x=sentiment,y=`2020_07`))+geom_bar(aes(fill=sentiment),
                                                                          stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("2020_07")+ggtitle("Sentiments of Airbnb Reviews in New York City in 2020_07")

sentimentPercentage2020_07 <-colSums(prop.table(textSentiment2020_07[, 1:8]))




#Sentiment Analysis 2020-08---------------------------------------------------

#getting emotions using in-built function
textSentiment2020_08<-get_nrc_sentiment((cleanreviews2020_08))

#calculating total score for each sentiment
sentimentScores2020_08<-data.frame(colSums(textSentiment2020_08[,]))
names(sentimentScores2020_08)<-"2020_08"
sentimentScores2020_08<-cbind("sentiment"=rownames(sentimentScores2020_08),sentimentScores2020_08)
rownames(sentimentScores2020_08)<-NULL
sentimentScores2020_08

#plotting the sentiments with scores
ggplot(data=sentimentScores2020_08,aes(x=sentiment,y=`2020_08`))+geom_bar(aes(fill=sentiment),
                                                                          stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("2020_08")+ggtitle("Sentiments of Airbnb Reviews in New York City in 2020_08")

sentimentPercentage2020_08 <-colSums(prop.table(textSentiment2020_08[, 1:8]))



#Sentiment Analysis 2020-09---------------------------------------------------

#getting emotions using in-built function
textSentiment2020_09<-get_nrc_sentiment((cleanreviews2020_09))

#calculating total score for each sentiment
sentimentScores2020_09<-data.frame(colSums(textSentiment2020_09[,]))
names(sentimentScores2020_09)<-"2020_09"
sentimentScores2020_09<-cbind("sentiment"=rownames(sentimentScores2020_09),sentimentScores2020_09)
rownames(sentimentScores2020_09)<-NULL
sentimentScores2020_09

#plotting the sentiments with scores
ggplot(data=sentimentScores2020_09,aes(x=sentiment,y=`2020_09`))+geom_bar(aes(fill=sentiment),
                                                                          stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("2020_09")+ggtitle("Sentiments of Airbnb Reviews in New York City in 2020_09")

sentimentPercentage2020_09 <-colSums(prop.table(textSentiment2020_09[, 1:8]))







#Sentiment Analysis 2020-10---------------------------------------------------

#getting emotions using in-built function
textSentiment2020_10<-get_nrc_sentiment((cleanreviews2020_10))

#calculating total score for each sentiment
sentimentScores2020_10<-data.frame(colSums(textSentiment2020_10[,]))
names(sentimentScores2020_10)<-"2020_10"
sentimentScores2020_10<-cbind("sentiment"=rownames(sentimentScores2020_10),sentimentScores2020_10)
rownames(sentimentScores2020_10)<-NULL
sentimentScores2020_10

#plotting the sentiments with scores
ggplot(data=sentimentScores2020_10,aes(x=sentiment,y=`2020_10`))+geom_bar(aes(fill=sentiment),
                                                                          stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("2020_10")+ggtitle("Sentiments of Airbnb Reviews in New York City in 2020_10")

sentimentPercentage2020_10 <-colSums(prop.table(textSentiment2020_10[, 1:8]))




#Sentiment Analysis 2020-11---------------------------------------------------

#getting emotions using in-built function
textSentiment2020_11<-get_nrc_sentiment((cleanreviews2020_11))

#calculating total score for each sentiment
sentimentScores2020_11<-data.frame(colSums(textSentiment2020_11[,]))
names(sentimentScores2020_11)<-"2020_11"
sentimentScores2020_11<-cbind("sentiment"=rownames(sentimentScores2020_11),sentimentScores2020_11)
rownames(sentimentScores2020_11)<-NULL
sentimentScores2020_11

#plotting the sentiments with scores
ggplot(data=sentimentScores2020_11,aes(x=sentiment,y=`2020_11`))+geom_bar(aes(fill=sentiment),
                                                                          stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("2020_11")+ggtitle("Sentiments of Airbnb Reviews in New York City in 2020_11")

sentimentPercentage2020_11 <-colSums(prop.table(textSentiment2020_11[, 1:8]))




#Sentiment Analysis 2020-12---------------------------------------------------

#getting emotions using in-built function
textSentiment2020_12<-get_nrc_sentiment((cleanreviews2020_12))

#calculating total score for each sentiment
sentimentScores2020_12<-data.frame(colSums(textSentiment2020_12[,]))
names(sentimentScores2020_12)<-"2020_12"
sentimentScores2020_12<-cbind("sentiment"=rownames(sentimentScores2020_12),sentimentScores2020_12)
rownames(sentimentScores2020_12)<-NULL
sentimentScores2020_12

#plotting the sentiments with scores
ggplot(data=sentimentScores2020_12,aes(x=sentiment,y=`2020_12`))+geom_bar(aes(fill=sentiment),
                                                                          stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("2020_12")+ggtitle("Sentiments of Airbnb Reviews in New York City in 2020_12")

sentimentPercentage2020_12 <-colSums(prop.table(textSentiment2020_11[, 1:8]))




#Sentiment Analysis 2021-01---------------------------------------------------

#getting emotions using in-built function
textSentiment2021_01<-get_nrc_sentiment((cleanreviews2021_01))

#calculating total score for each sentiment
sentimentScores2021_01<-data.frame(colSums(textSentiment2021_01[,]))
names(sentimentScores2021_01)<-"2021_01"
sentimentScores2021_01<-cbind("sentiment"=rownames(sentimentScores2021_01),sentimentScores2021_01)
rownames(sentimentScores2021_01)<-NULL
sentimentScores2021_01

#plotting the sentiments with scores
ggplot(data=sentimentScores2021_01,aes(x=sentiment,y=`2021_01`))+geom_bar(aes(fill=sentiment),
                                                                          stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("2021_01")+ggtitle("Sentiments of Airbnb Reviews in New York City in 2021_01")

sentimentPercentage2021_01 <-colSums(prop.table(textSentiment2021_01[, 1:8]))





#Sentiment Analysis 2021-02---------------------------------------------------

#getting emotions using in-built function
textSentiment2021_02<-get_nrc_sentiment((cleanreviews2021_02))

#calculating total score for each sentiment
sentimentScores2021_02<-data.frame(colSums(textSentiment2021_02[,]))
names(sentimentScores2021_02)<-"2021_02"
sentimentScores2021_02<-cbind("sentiment"=rownames(sentimentScores2021_02),sentimentScores2021_02)
rownames(sentimentScores2021_02)<-NULL
sentimentScores2021_02

#plotting the sentiments with scores
ggplot(data=sentimentScores2021_02,aes(x=sentiment,y=`2021_02`))+geom_bar(aes(fill=sentiment),
                                                                          stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("2021_02")+ggtitle("Sentiments of Airbnb Reviews in New York City in 2021_02")

sentimentPercentage2021_02 <-colSums(prop.table(textSentiment2021_02[, 1:8]))




#Sentiment Analysis 2021-03---------------------------------------------------

#getting emotions using in-built function
textSentiment2021_03<-get_nrc_sentiment((cleanreviews2021_03))

#calculating total score for each sentiment
sentimentScores2021_03<-data.frame(colSums(textSentiment2021_03[,]))
names(sentimentScores2021_03)<-"2021_03"
sentimentScores2021_03<-cbind("sentiment"=rownames(sentimentScores2021_03),sentimentScores2021_03)
rownames(sentimentScores2021_03)<-NULL
sentimentScores2021_03

#plotting the sentiments with scores
ggplot(data=sentimentScores2021_03,aes(x=sentiment,y=`2021_03`))+geom_bar(aes(fill=sentiment),
                                                                          stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("2021_03")+ggtitle("Sentiments of Airbnb Reviews in New York City in 2021_03")

sentimentPercentage2021_03 <-colSums(prop.table(textSentiment2021_03[, 1:8]))

