# TA Lightning Talk: Inaugural Speeches -- Part 2
# Author: Joseph O'Malley (JO054429)
# Date: 7/11/2018
# Script ID: 0040
# Tags: custom stopwords, ngram frequency, barplot, comparison cloud, commonality cloud, frequency counts 
# packages: RWeka, quanteda, tm, data.table, ggplot2, wordcloud 


##############################################################################################
## Table of Contents
##############################################################################################

##5) N-gram frequency
##6) Comparison (Cloud)


##import cleaned data (from pt 1)
inaug_speeches_clean <- read.csv("C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks/inaug_speeches_clean.csv", encoding = "ASCII")

##install needed packages
##install.packages(c("RWeka", "quanteda", "tm","wordcloud"))

####################################################################################################
#################################### N-Grams Frequency #############################################
####################################################################################################
require(RWeka)
require(quanteda)
require(tm)
require(data.table)


##check structure of the data
str(inaug_speeches_clean)

###create data table for conversion of free text to charachters
inaug_speeches_clean <- as.data.table(inaug_speeches_clean)
inaug_speeches_clean <- inaug_speeches_clean[, text_final:=as.character(text_final)]

###specify the text column to be used 
text <- inaug_speeches_clean$text_final

##convert row names
#inaug_speeches_clean <- as.data.frame(inaug_speeches_clean)
#row.names(inaug_speeches_clean) <- inaug_speeches_clean$Speech

###custom stopwords list
custom_stopwords <- (c(stopwords("english"), "otherwords"))

################################# tokenize using quanteda ##########################################
##tokenization and text cleaning
toks <- tokens(text, remove_punct = TRUE,
               remove_symbols = TRUE)
toks <- tokens_tolower(toks)
#toks <- tokens_wordstem(toks, language = quanteda_options("language_stemmer"))
toks <- tokens_remove(toks, custom_stopwords)
###create n-grams, specify size (sparsity/computing power are a consideration here)
toksNgrams <- tokens_ngrams(toks, n = c(2, 3), concatenator = "_")


###convert to data frame matrix
dfm_toksNgrams <- as.matrix(toksNgrams)
dfm_toksNgrams <- dfm(toksNgrams)
dfm_toksNgrams <- dfm_remove(dfm_toksNgrams, c(stopwords("english"), "high_school"))
###remove leading and trailing stopwords from n-grams
dfm_toksNgrams <- dfm_remove(dfm_toksNgrams, 
                             pattern = c(paste0("^", stopwords("english"), "_"), 
                                         paste0("_", stopwords("english"), "$")), 
                             valuetype = "regex")

##frequency dist
term_frequency <- colSums(dfm_toksNgrams)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency, decreasing = TRUE)
term_frequency <- as.data.frame(term_frequency)

# extract column names (words/ngrams) to new column in r
setDT(term_frequency, keep.rownames = TRUE)[]
# alternative method of extract column names (words/ngrams) to new column in r
# term_frequency$names <- rownames(term_frequency)


################################## Frequency Visualizations #######################################
require(ggplot2)
library(wordcloud)

## Plot a barchart of the most frequent words/phrases 
##subset top 40 rows
term_frequency2 <- term_frequency %>% 
  arrange(desc(term_frequency)) %>%
  head(40)

## Plot a barchart of the most frequent words/phrases 
ggplot(term_frequency2, aes(x=reorder(rn, -term_frequency),y=(term_frequency))) + 
  geom_bar(stat = "identity",width=0.5, 
           aes(fill = term_frequency2$term_frequency)) +
  theme(axis.text.x = element_text(vjust=1,angle=90)) + theme(legend.position="none") +
  geom_text(aes(label=term_frequency), vjust=0,angle=90,size=2.5,hjust=0)+
  labs(title="Most Common Phrases", caption="United States: Inauguration Speeches")
############################## write term frequency into new file (optional) #######################

#setwd('C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks')
#write.csv(term_frequency, "inaug_term_frequency.csv")



###################################################################################################
################################### Comparison Cloud ##############################################
###################################################################################################
library(wordcloud)
####convert from quanteda to tm (see https://rdrr.io/cran/quanteda/man/convert.html)
##check structure of the data
#str(inaug_speeches_clean)

###create data table for conversion of free text to charachters
inaug_speeches_clean <- as.data.table(inaug_speeches_clean)

###specify the text column to be used 
text <- inaug_speeches_clean$text_final

###custom stopwords list
custom_stopwords <- (c(stopwords("english"), "otherwords"))

################################# tokenize using quanteda ##########################################
##tokenization and text cleaning
toks <- tokens(text, remove_punct = TRUE,
               remove_symbols = TRUE)
toks <- tokens_tolower(toks)
#toks <- tokens_wordstem(toks, language = quanteda_options("language_stemmer"))
toks <- tokens_remove(toks, custom_stopwords)
###create n-grams, specify size (sparsity/computing power are a consideration here)
toksNgrams <- tokens_ngrams(toks, n = c(1), concatenator = "_")


###convert to data frame matrix
dfm_toksNgrams <- as.matrix(toksNgrams)
dfm_toksNgrams <- dfm(toksNgrams)
dfm_toksNgrams <- dfm_remove(dfm_toksNgrams, c(stopwords("english"), "high_school"))
###remove leading and trailing stopwords from n-grams
dfm_toksNgrams <- dfm_remove(dfm_toksNgrams, 
                             pattern = c(paste0("^", stopwords("english"), "_"), 
                                         paste0("_", stopwords("english"), "$")), 
                             valuetype = "regex")

##rename rows of matrix to presidents names
row.names(dfm_toksNgrams) <- inaug_speeches_clean$Speech

##frequency dist
term_frequency <- colSums(dfm_toksNgrams)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency, decreasing = TRUE)
term_frequency <- as.data.frame(term_frequency)

# extract column names (words/ngrams) to new column in r
setDT(term_frequency, keep.rownames = TRUE)[]
# alternative method of extract column names (words/ngrams) to new column in r
# term_frequency$names <- rownames(term_frequency)

########################################## create Commonality Cloud ################################
####convert from quanteda to tm (see https://rdrr.io/cran/quanteda/man/convert.html)

##can compare 2 or more texts (president numbers)
dat <- dfm_toksNgrams[c(57, 58),]
##convert from quanteda dfm to tm DTM
dat <- convert(dat, to = "tm")
##convert from DTM to TDM
dat <- as.TermDocumentMatrix(dat)
dat <- as.matrix(dat)


comparison.cloud(dat,max.words=80,random.order=FALSE,colors=c("#1F497D","#C0504D", "light blue"),
                 main="Differences Between Inauguration Speeches")
commonality.cloud(dat,random.order=FALSE,max.words=80, color="#1F497D",main="Commonalities in Inauguration Speeches")

##convert row names
inaug_speeches_clean <- as.data.frame(inaug_speeches_clean)
row.names(inaug_speeches_clean) <- inaug_speeches_clean$Speech


#############################################################
