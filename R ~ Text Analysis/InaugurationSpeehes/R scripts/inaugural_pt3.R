# TA Lightning Talk: Inaugural Speeches -- Part 3
# Author: Joseph O'Malley (JO054429)
# Date: 7/11/2018
# Script ID: 0041
# Tags: custom stopwords, lexical diversity, cosine similarity, time-series plot, scatter plot, unique counts, Unique IDs, correlation plot
# packages: lsa, ggplot2, RWeka, quanteda, tm, data.table, ggcorrplot, ggplot2, plotly   


###################################################################################################
################################## Table of Contents ##############################################
###################################################################################################


##7) Cosine Similarity
##8) Lexical Diversity


############################### import cleaned data (from pt 1) ####################################
inaug_speeches_clean <- read.csv("C:/Users/JO054429/OneDrive - Cerner Corporation/Documents/Github_Portfolio/InaugurationSpeehes/data/inaug_speeches_clean.csv")

##install needed packages
#install.packages(c("lsa", "ggcorrplot", "plotly"))


####################################################################################################
####################################### Cosine Similarity ##########################################
####################################################################################################
library(lsa)
require(ggplot2)
require(RWeka)
require(quanteda)
require(tm)
require(data.table)

#################################### Create tokens for cosine similarity ###########################

##check structure of the data
str(inaug_speeches_clean)

###create data table for conversion of free text to charachters
inaug_speeches_clean <- as.data.table(inaug_speeches_clean)
inaug_speeches_clean <- inaug_speeches_clean[, text_final:=as.character(text_final)]

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


#################################### Transform Matrix ##############################################


## (see https://www.youtube.com/watch?v=7cwBhWYHgsA)
##can compare 2 or more texts
CosineSim_dfm <- dfm_toksNgrams[c(1:nrow(dfm_toksNgrams)),]
##convert from quanteda dfm to tm DTM
CosineSim_dfm <- convert(CosineSim_dfm, to = "tm")

##check dimensions of dfm
dim(CosineSim_dfm)

## get Cosine similarity in transposed Matrix format
CosineSim_Matrix <- cosine(t(as.matrix(CosineSim_dfm)))
##check dimensions and transpose
dim(CosineSim_Matrix)
##convert to data frame
CosineSim_Matrix <- as.data.frame(CosineSim_Matrix)

###################### join to primary dataset (optional) #########################################
##add cosine similarities to original text
## create unique ID to join Presdident Names to Cosine similarity text
CosineSim_Matrix$UniqueResp <- seq.int(nrow(CosineSim_Matrix))
inaug_speeches_clean$UniqueResp <- seq.int(nrow(inaug_speeches_clean))
##cartesian join
#inaug_speeches_wCosine <- merge(x = inaug_speeches_clean, y = CosineSim_Matrix, by = "UniqueResp", all.x = TRUE)



#################################### visualize cosine similarity matrix ############################
require(ggcorrplot)

##Convert RowNames
##convert row and column names

CosineSim_Matrix <- as.data.frame(CosineSim_Matrix)
row.names(CosineSim_Matrix) <- inaug_speeches_clean$Speech
colnames(CosineSim_Matrix) <- t(inaug_speeches_clean$Speech)


##subset matrix of presidents
CosineSim_Matrix_1900 <- CosineSim_Matrix[c(30:32, 34:37, 41:42, 44:46, 48:49, 51, 52, 54, 56, 58), 
                                          c(30:32, 34:37, 41:42, 44:46, 48:49, 51, 52, 54, 56, 58)]

##correlation matrix plot
ggcorrplot(CosineSim_Matrix, type = "lower", lab = TRUE, legend.title = "Cos Sim", 
           title = "Cosine Similarity of Inauguration Speeches",
           show.diag = TRUE, outline.color = "black", lab_size = 2)

##correlation matrix plot for presidents 1st Inauguration speeches since 1900

ggcorrplot(CosineSim_Matrix_1900, type = "lower", lab = TRUE, legend.title = "Cos Sim", 
           title = "Cosine Similarity of Inauguration Speeches",
           show.diag = TRUE, outline.color = "black", lab_size = 2)

###################################################################################################











###################################################################################################
################################# Lexical Diversity ###############################################
###################################################################################################

## (see https://rdrr.io/github/kbenoit/quanteda/man/textstat_lexdiv.html)
require(RWeka)
require(quanteda)
require(tm)

##Import Data For pt. 2

##check structure of the data
str(inaug_speeches_clean)

###create data table for conversion of free text to charachters
inaug_speeches_clean <- as.data.table(inaug_speeches_clean)

###specify the text column to be used 
text <- inaug_speeches_clean$text_final

###custom stopwords list
custom_stopwords <- (c(stopwords("english"), "otherwords"))
###tokenize using quanteda

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

##calculate lexical diversity
textstat_lexdiv(dfm_toksNgrams, measure = c("all", "TTR", "C", "R", "CTTR", "U", "S",
                                            "Maas"), log.base = 10)

##calculate create new column with chosen lexical diversity  
inaug_speeches_clean$LexDiv_r <- textstat_lexdiv(dfm_toksNgrams, measure = c("CTTR"), log.base = 10)


##################################### Visualize Lexical Diversity ##################################
require(ggplot2)
require(plotly)
##convert strings we will visualize to numeric
inaug_speeches_clean <- inaug_speeches_clean[, Year:=as.numeric(Year)]

## look at number of Inaugurations by Address Number
inaug_speeches_clean[, .(number_of_distinct_records = uniqueN(X)), by = Inaugural.Address]

################################## Time Series Plot ################################################
##subset
first_inaug <- 
  inaug_speeches_clean %>%
  filter(Inaugural.Address == "First Inaugural Address")
##create plot
LexDiv_plot <- ggplot(data=first_inaug, aes(x=Year, y=LexDiv_r)) +
  geom_line(colour = "blue", linejoin = "mitre")+
  geom_point(colour = "Black") +  
  theme(axis.text.x = element_text(vjust=1,angle=90)) +
  geom_text(aes(label=Name), vjust=0,angle=90,size=2.5,hjust=0) +
  scale_x_continuous(breaks = seq(1789,2017,4))
##show plot
LexDiv_plot
##convert and show in plotly
LexDiv_plotly <- ggplotly(LexDiv_plot)
LexDiv_plotly

##################### scatter plot
##scatter plot of number of words vs Lexical Diversity Measure
scat <- ggplot(inaug_speeches_clean, aes(x = num_words, y = LexDiv_r)) +
  geom_point(aes(color = Year))

##convert to plotly and visualize
scat <- ggplotly(scat)
scat



###################################################################################################

##################################### Export Cleaned Data ##########################################
setwd('C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks')
write.csv(inaug_speeches_clean, "inaug_speeches_clean_plus.csv")



