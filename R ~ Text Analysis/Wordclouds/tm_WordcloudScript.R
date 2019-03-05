# tm Wordcloud
# Author: Joseph O'Malley (JO054429)
# Date: 3/7/2018
# Script ID: 0050
# Tags: ngrams, custom stopwords, wordcloud, Cerner colors
# packages: tm, wordcloud, plyr, class, ggplot2, SnowballC, RWeka, rJava, RWekajars

MLK <- read.csv("C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/MLK.csv",
                                quote = "", 
                                row.names = NULL, 
                                stringsAsFactors = FALSE,
                                header = FALSE)

# Load Packages
require(tm)
require(wordcloud)
require(plyr)
require(class)
require(psych)
require(ggplot2)
require(SnowballC)
require(RWeka)
require(rJava)
require(RWekajars)

# Analysis - UN Transcript
custom_stopwords <- (c(stopwords("english"), "can"))
# create corpus (for tm)

po.corpus <- Corpus(DataframeSource(data.frame(doc_id=row.names(MLK), text=MLK$V1)))
po.corpus <- tm_map(po.corpus, removePunctuation)
po.corpus <- tm_map(po.corpus, tolower)
po.corpus <- tm_map(po.corpus, function(x) removeWords(x, custom_stopwords))

###create tdm
tdm <- TermDocumentMatrix(po.corpus)

# wordcloud
two.m <- as.matrix(tdm)
two.v <- sort(rowSums(two.m),decreasing=TRUE)
two.d <- data.frame(word = names(two.v),freq=two.v)
table(two.d$freq)
pal2 <- brewer.pal(8,"Dark2")

wordcloud(two.d$word,two.d$freq, scale=c(2,1),min.freq=2,
          fmax.words=Inf, random.order=FALSE, rot.per=.25, colors=pal2)


# Analysis - MLK speech Stemmed
# create corpus
po.corpus2 <- Corpus(DataframeSource(data.frame(doc_id=row.names(MLK), text=MLK$V1)))
po.corpus2 <- tm_map(po.corpus2, removePunctuation)
po.corpus2 <- tm_map(po.corpus2, content_transformer(tolower))
po.corpus2 <- tm_map(po.corpus2, stemDocument, language= "english")
po.corpus2 <- tm_map(po.corpus2, function(x) removeWords(x, custom_stopwords))

# create Term Document Matrix
tdm2 <- TermDocumentMatrix(po.corpus2)

# tm wordcloud
three.m <- as.matrix(tdm2)
three.v <- sort(rowSums(three.m),decreasing=TRUE)
three.d <- data.frame(word = names(three.v),freq=three.v)
table(three.d$freq)
pal2 <- brewer.pal(8,"Dark2")

## tm wordcloud 
wordcloud(three.d$word,three.d$freq, scale=c(2,1),min.freq=2,
          fmax.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)


CernerBlue <- rgb(13,148,210, maxColorValue = 255)
CernerGreen <- rgb(123,193,67, maxColorValue = 255)
CernerOrange <- rgb(245,128,37, maxColorValue = 255)
CernerPurple <- rgb(124,43,131, maxColorValue = 255)
CernerYellow <- rgb(253,185,19, maxColorValue = 255)
CernerGrey <- rgb(106,115,123, maxColorValue = 255)

CernerPalette <- c(CernerBlue,CernerGreen,CernerOrange,CernerPurple,CernerYellow,CernerGrey)

##tm wordcloud
wordcloud(three.d$word,three.d$freq, scale=c(3,.5),min.freq=2,
          fmax.words=Inf, random.order=FALSE, rot.per=.15, colors=CernerPalette)


CernerPalette2 <- c(CernerGrey ,CernerYellow, CernerPurple, CernerOrange, CernerGreen, CernerBlue)

##tm wordcloud
wordcloud(three.d$word,three.d$freq, scale=c(3,.5),min.freq=2,
          fmax.words=Inf, random.order=FALSE, rot.per=.15, colors=CernerPalette2)

##texplot dfm wordcloud
require(ggplot2)
textNgram <- textplot_wordcloud(FreeText_tokens_dfm, comparison = TRUE,
                                min_size = 0.5, max_size = 4, min_count = 40,
                                max_words = 500, font = NULL, adjust = 0, color = c("green", "red", "blue"),
                                rotation = 0.1, random_order = TRUE, random_color = TRUE,
                                ordered_color = FALSE, labelcolor = "gray20", labelsize = 1.5,
                                labeloffset = 0, fixed_aspect = TRUE)


#, color = "darkblue"


