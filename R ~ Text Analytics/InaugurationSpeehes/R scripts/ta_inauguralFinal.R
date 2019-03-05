# TA Lightning Talk: Inaugural Speeches -- Final
# Author: Joseph O'Malley (JO054429)
# Date: 7/11/2018
# Script ID: 0042
# Tags: conditional formatting, unique counts, increase memory limit, NER, NER Visualization, Dictionary Creation, data export, remove duplicates, rename columns, gsub, merge, tableau transformation, custom stopwords, lexical diversity, cosine similarity, time-series plot, scatter plot, Unique IDs, correlation plot, ngram frequency, barplot, comparison cloud, commonality cloud, frequency counts, kable tables, recode factors, date formatting, nchar, nwords, subsetting    
# packages: data.table, tidytext, dplyr, ggplot2, stringr, lubridate, tidyr, RWeka, quanteda, lsa, tm, ggcorrplot, plotly, rJava, NLP, openNLP, wordcloud, reshape, tibble, magrittr, openNLPmodels.en


##import data (from kaggle) see https://www.kaggle.com/adhok93/presidentialaddress/data
inaug_speeches <- read.csv("C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks/inaug_speeches.csv", encoding = "ASCII")

##############################################################################################
## Table of Contents
##############################################################################################

##1) EDA
##2) Text Cleaning/Manipulation
##3) Convert Dates
##4) Extract Number of words & characters
##5) N-gram frequency
##6) Comparison (Cloud)
##7) Cosine Similarity
##8) Lexical Diversity
##9) Named Entity Recognition (NER)
##10) Dictionary Creation
##11) Transformation for Tableau


##############################################################################################
## EDA - Exploratory Data Analysis
##############################################################################################
require(data.table)
require(kableExtra)
##initial exploration
str(inaug_speeches)
inaug_speeches_dt <- as.data.table(inaug_speeches)
names(inaug_speeches)


##count unique values in each column
apply(inaug_speeches[, c(1:ncol(inaug_speeches))], 2, function(x) length(unique(x)))

##counts of each unique president
dt <- inaug_speeches_dt[, .(number_of_distinct_records = uniqueN(text)), by = Name]

##convert to kable tables
kable(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "center")



##############################################################################################
## Text Cleaning/Manipulation
##############################################################################################

##convert strings we will analyze to charachters
inaug_speeches_dt <- inaug_speeches_dt[, text:=as.character(text)]
inaug_speeches_dt <- inaug_speeches_dt[, text2:=as.character(text2)]
##combine text columns in r (only one that couldn't fit into one excel cell)
inaug_speeches_dt$text <- apply(inaug_speeches_dt[ , c("text", "text2") ] ,1 , paste , collapse = "" )


##create duplicate text column (for manipulation)
##trim whitespace and covert text to lowercase
inaug_speeches_dt$text_final <- trimws(inaug_speeches_dt$text)


##convert to regex and add additional text cleanin using stringr
##remove encoding errors using stringr (i.e. <U+AO97>) see <https://stackoverflow.com/questions/39993715/how-to-remove-unicode-u00a6-from-string>
inaug_speeches_dt$text_final <- gsub("\\s*<u\\+\\w+>\\s*", " ", inaug_speeches_dt$text_final)
inaug_speeches_dt$text_final <- gsub("\\s*<U\\+\\w+>\\s*", " ", inaug_speeches_dt$text_final)
##convert whitespace to single space
inaug_speeches_dt$text_final <- gsub("\\s", " ", inaug_speeches_dt$text_final)
##remove non-reg charachters
inaug_speeches_dt$text_final <- gsub("[^[A-Za-z0-9 ][:punct:]]", "", inaug_speeches_dt$text_final)


##############################################################################################
## Convert date formats
##############################################################################################

##convert dates using lubridate (see https://www.rstudio.com/resources/cheatsheets/)
##date conventions (see https://www.statmethods.net/input/dates.html)

##load require packages
require(stringr)
require(lubridate)
require(tidyr)
require(dplyr)

##check unique dates
unique(inaug_speeches_dt$Date)
##make copy of original date format for manipulation
inaug_speeches_dt$DateOriginal <- inaug_speeches_dt$Date
##edit missmatched date format (Clinton 1997)
inaug_speeches_dt$Date <- gsub("20-Jan-97", "Monday, January 20, 1997", inaug_speeches_dt$Date)

## split current date formats
inaug_speeches_dt <- separate(inaug_speeches_dt, "Date", c('DayOfWeek', 'MonthDay', 'Year'), sep = ",") 

inaug_speeches_dt$MonthDay <- trimws(inaug_speeches_dt$MonthDay)
inaug_speeches_dt$Year <- trimws(inaug_speeches_dt$Year)

##replace NA Values in main date column with 
#inaug_speeches_dt$MonthDay <- ifelse(is.na(inaug_speeches_dt$MonthDay), inaug_speeches_dt$DayOfWeek, inaug_speeches_dt$MonthDay)

##recode factors for First Inaugural address
inaug_speeches_dt$Inaugural.Address <- recode_factor(inaug_speeches_dt$Inaugural.Address, "Inaugural Address" = "First Inaugural Address")

                
##replace non-day of the week with "unknown"
inaug_speeches_dt$DayOfWeek <- 
  ifelse(inaug_speeches_dt$DayOfWeek == "Monday", "Monday", 
         ifelse(inaug_speeches_dt$DayOfWeek == "Tuesday", "Tuesday", 
                ifelse(inaug_speeches_dt$DayOfWeek == "Wednesday", "Wednesday", 
                       ifelse(inaug_speeches_dt$DayOfWeek == "Thursday", "Thursday", 
                              ifelse(inaug_speeches_dt$DayOfWeek == "Friday", "Friday", 
                                     ifelse(inaug_speeches_dt$DayOfWeek == "Saturday", "Saturday", 
                                            ifelse(inaug_speeches_dt$DayOfWeek == "Sunday", "Sunday", "Unknown")))))))
##check unique days of week
inaug_speeches_dt[, .(number_of_distinct_questions = uniqueN(text_final)), by = DayOfWeek]                                

##concatenate DayMonth and Year
inaug_speeches_dt$Date <- apply(inaug_speeches_dt[, c('MonthDay', 'Year')], 1, paste, collapse = ",")
inaug_speeches_dt$Date <- gsub(",NA", "", inaug_speeches_dt$Date)

##check unique month/day
unique(inaug_speeches_dt$MonthDay)

##multidate function in r
multidate <- function(data, formats){
  data <- gsub("nd", "", data, perl = TRUE)
  a<-list()
  for(i in 1:length(formats)){
    a[[i]]<- as.Date(data,format=formats[i])
    a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
  }
  a[[1]]
}

###converts disparate date types (list) to common format
inaug_speeches_dt$Date <- multidate(inaug_speeches_dt$Date, 
                                     c("%m/%d/%Y","%d.%m.%Y","%d %b %y", "%d %b %y", "%B %d, %Y",
                                       "%B %d %Y", "%d-%b-%y", "%m.%d.%Y", "%d %b, %Y", "%A, %m/%d/%y",
                                       "%A, %B %d, %Y", "%d of %B %Y", "%m%d%y", "%d%m%Y", "%d-%m-%Y",
                                       "%A %B %d %Y", "%d %B, %Y", "%d %B %Y", "%d %B %Y", "%B %d %Y",
                                       "%B %d, %Y", "%A %B %d, %Y", "%h %d %Y", "%m/%d%y", "%d/%m/%Y",
                                       "%d/%m/%Y", "%m%d%y", "%d %B %Y", "%B %d,%Y", "%b. %d,%Y",
                                       "%b-%d-%Y", "%m/%d.%Y"))
unique(inaug_speeches_dt$Date)
###change format to m/d/Y
inaug_speeches_dt$Date <- format(inaug_speeches_dt$Date, "%m/%d/%Y")






####################################################################################################
############################## Number of Charachters and Number of Words ###########################
####################################################################################################


##calculate number of charachters
inaug_speeches_dt$nchar <- nchar(inaug_speeches_dt$text_final)

##calculate number of words
nwords <- nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}

##calculate number of words using nwords() function
inaug_speeches_dt$num_words <- nwords(inaug_speeches_dt$text_final)


##create new column for column labels for President
inaug_speeches_dt$PresidentNumber <- seq.int(nrow(inaug_speeches_dt))
inaug_speeches_dt$Speech <- apply(inaug_speeches_dt[, c('PresidentNumber', 'Name')], 1, paste, collapse = " ")

############### Visualizing speech length
require(tidytext)
require(dplyr)
require(ggplot2)

##create barchart in ggplot

inaug_speeches_dt %>%
  unnest_tokens(word,text_final) %>%
  group_by(Speech) %>%
  summarise(num_words=n()) %>%
  mutate(mean_words=mean(num_words)) %>%
  ggplot(aes(x=Speech,y=(num_words)))+geom_bar(stat = "identity",width=0.5, 
                                               aes(fill = inaug_speeches_dt$Inaugural.Address)) +
  scale_fill_manual(values = c("red", "goldenrod", "blue", "light blue", "black")) +
  theme(axis.text.x = element_text(vjust=1,angle=90)) + theme(legend.position="bottom") +
  geom_text(aes(label=inaug_speeches_dt$Year), vjust=0,angle=90,size=2.5,hjust=0)+ylim(c(0,11500)) +
  labs(title="Speech Length",
       caption="United States: Inauguration Speeches")


##checkaverage speech length by inaigural address number
inaug_groups <-inaug_speeches_dt %>%
  group_by(Inaugural.Address) %>%
  summarise_at(vars(num_words), funs(mean(., na.rm=TRUE))) %>% 
  arrange(desc(num_words)) %>% 
  mutate(num_words = round(num_words, 2))


inaug_groups <- as.data.table(inaug_groups)

##convert to kable tables
kable(inaug_groups) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "center")

## subset columns
inaug_speeches_clean <- subset(inaug_speeches_dt, select = c("PresidentNumber", "Name","Inaugural.Address", 
                                                             "DayOfWeek", "Year", "Date", "DateOriginal", 
                                                             "text_final","nchar", "num_words", "Speech"))

##################################### Export Cleaned Data ##########################################
#setwd('C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks')
#write.csv(inaug_speeches_clean, "inaug_speeches_clean.csv")







####################################################################################################
#################################### N-Grams Frequency #############################################
####################################################################################################
require(RWeka)
require(quanteda)
require(tm)
require(data.table)

##Import Data For pt. 2

##check structure of the data
str(inaug_speeches_clean)

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
toksNgrams <- tokens_ngrams(toks, n = c(1, 2), concatenator = "_")


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
library(data.table)
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

####################################################################################################
#################################### Comparison Cloud ##############################################
####################################################################################################

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

################### Time Series Plot 
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





###################################################################################################
################################# NER #############################################################
###################################################################################################
require(rJava)
require(NLP)
require(openNLP)
require(data.table)
require(dplyr)

##load in data
#inaug_speeches_clean <- read.csv("C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks/inaug_speeches_clean.csv")

###convert raw data to data.table 
data <- as.data.table(inaug_speeches_clean)

###convert column(s) with text to be anaalyzed to charachter
data <- data[, text_final:=as.character(text_final)]

###to subset data with less than certain number of charachters (optional)
#data$NumChar <- nchar(data$TextColumn)
#data <- data[NumChar > 3, ]

###check for unique responses to subset by (optional)
#unique(data$ColumnWithFactors)
#data_subset <- data[which(data$ColumnWithFactors == 'Particular Response'),]

##remove NAs
data <- data[!(is.na(data$text_final) | data$text_final==""), ]

#select only text column for analysis
names(inaug_speeches_clean)
unique(inaug_speeches_clean$Inaugural.Address)


###check for unique responses to subset by (optional)
inaug_speeches_clean[, .(number_of_distinct_records = uniqueN(PresidentNumber)), by = Inaugural.Address]
inaug_speeches_first <- inaug_speeches_clean[which(inaug_speeches_clean$Inaugural.Address == 'First Inaugural Address'),]

text <- inaug_speeches_first$text_final
###for tagging
text <- as.String(text)

###############################################################################
##################### NER (Named Entity Extraction) ###########################
###############################################################################
require(magrittr)
require(openNLPmodels.en)
require(rJava)
require(NLP)
require(openNLP)
require(data.table)

##increase memory limit ~ 7.2gb
memory.limit(size=56500)
##extract words
word_ann <- Maxent_Word_Token_Annotator()  
##extract dentences
sent_ann <- Maxent_Sent_Token_Annotator()  
pos_ann <- Maxent_POS_Tag_Annotator()

pos_annotations <- annotate(text, list(sent_ann, word_ann, pos_ann))
text_annotations <- annotate(text, list(sent_ann, word_ann))
head(text_annotations)

text_doc <- AnnotatedPlainTextDocument(text, text_annotations)
words(text_doc) %>% head(10)

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")
date_ann <- Maxent_Entity_Annotator(kind = "date")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann,
                 date_ann)

text_annotations <- annotate(text_doc, pipeline)
text_doc <- AnnotatedPlainTextDocument(text, text_annotations)


entities <- function(doc, kind){
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, '[[', "kind")
    s[a[k == kind]]
  } else {
    a[s[a$tpe == "entity"]]
  }
}

person <- as.data.table(entities(text_doc, kind = "person"))
location <- as.data.table(entities(text_doc, kind = "location"))
organization <- as.data.table(entities(text_doc, kind = "organization"))
date <- as.data.table(entities(text_doc, kind = "date"))            



##Convert NER terms to new data frame with frequency 
library(dplyr)

##################################### location  ############################################
location$X <- seq.int(nrow(location))
location$NER_type <- "location"
location_tibble <- location %>% 
  group_by(V1) %>% 
  mutate(Count=n_distinct(X)) %>% 
  arrange(desc(Count))
##remove duplicates (specify columns that create make a response unique)
location_final <- location_tibble[!duplicated(location_tibble$V1), c(1,3:4)]


#################################### person  ############################################
person$X <- seq.int(nrow(person))
person$NER_type <- "person"
person_tibble <- person %>% 
  group_by(V1) %>% 
  mutate(Count=n_distinct(X)) %>% 
  arrange(desc(Count))
##remove duplicates (specify columns that create make a response unique)
person_final <- person_tibble[!duplicated(person_tibble$V1), c(1, 3:4)]


################################## organization #########################################
organization$X <- seq.int(nrow(organization))
organization$NER_type <- "organization"
organization_tibble <- organization %>% 
  group_by(V1) %>% 
  mutate(Count=n_distinct(X)) %>% 
  arrange(desc(Count))
##remove duplicates (specify columns that create make a response unique)
organization_final <- organization_tibble[!duplicated(organization_tibble$V1), c(1, 3:4)]


################################# date ##################################################
date$X <- seq.int(nrow(date))
date$NER_type <- "date"
date_tibble <- date %>% 
  group_by(V1) %>% 
  mutate(Count=n_distinct(X)) %>% 
  arrange(desc(Count))
##remove duplicates (specify columns that create make a response unique)
date_final <- date_tibble[!duplicated(date_tibble$V1), c(1, 3:4)]

##rbind columns
NER_combined <- rbind(organization_final, date_final, person_final, location_final)



################################## Frequency Visualizations #######################################
require(ggplot2)
require(wordcloud)

##subset top 40 most frequent terms using dplyr pipes 
NER_combined2 <- NER_combined %>% 
  arrange(desc(Count)) %>%
  head(40) 

ggplot(NER_combined2, aes(x=reorder(V1, -Count),y=(Count))) + 
  geom_bar(stat = "identity",width=0.5, 
           aes(fill = NER_combined2$NER_type)) +
  theme(axis.text.x = element_text(vjust=1,angle=90)) + theme(legend.position="bottom") +
  scale_fill_manual(values = c("red", "goldenrod", "blue", "light blue", "black")) +
  geom_text(aes(label=Count), vjust=0,angle=90,size=2.5,hjust=0)+
  labs(title="NER: Frequencies", caption="United States: Inauguration Speeches")




################################## Dictionary Creation ############################################
###export NER terms for further analysis
setwd("C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks")
write.csv(NER_combined, 'NER_combined.CSV')

###################################### Bad Words List #############################################
##returns lists of custom bad words dictionary in r
NER_combined <- read.csv("C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks/NER_combined.CSV")
NER_combined <- as.data.table(NER_combined)

##convert needed text columns to charachters
str(NER_combined)
##rename column with words
colnames(NER_combined)[colnames(NER_combined) == 'V1'] <- 'NER_terms'

##convert NER terms from factors to charachters
NER_combined <- NER_combined[, NER_terms:=as.character(NER_terms)]
##trim whitespace and convert to lowercase
NER_combined$NER_terms <- trimws(tolower(NER_combined$NER_terms))
##replace spaces with underscores
NER_combined$NER_terms <- gsub(" ", "_", NER_combined$NER_terms)
NER_terms <- as.data.table(NER_combined$NER_terms)

### transform list of bad words to custome dictionary format ###
NER_terms <- as.data.table(NER_combined$NER_terms)
NER_dictionary <- dictionary(as.list(NER_terms))



####################################################################################################
######################### Tokenize, convert to DFM, select N-Grams, and clean ######################
############ apply custome dictionary and output words occuring in new column ######################
####################################################################################################

###tokenization packages 
require(tibble)
require(quanteda)
require(tm)
require(stringr)
library(data.table)


##increase memory limit (~7.2GB)
memory.limit(size=56500)

################################## Text Preprocessing Pipeline #####################################

###create custome stopwords list
custom_stopwords <- (c(stopwords("english"), "additional", "stopwords"))

##tokenization and dfm (data frame matrix) transformation function
tokenize_dfm <- function(x) {tokenize <- tokens(x, remove_punct = TRUE,
                                                remove_symbols = TRUE)
tok_lower <- tokens_tolower(tokenize)
#tok_stem <- tokens_wordstem(tok_lower, language = quanteda_options("language_stemmer"))
toksNgrams <- tokens_ngrams(tok_lower, n = c(1, 2, 3), concatenator = "_") ###specify n-grams
dfm_toksNgrams <- dfm(toksNgrams)
#dfm_toksNgrams <- dfm_remove(dfm_toksNgrams, custom_stopwords)
## keeps only words occuring in "bad words" dictionary
dfm_toksNgrams <- dfm_select(dfm_toksNgrams, NER_dictionary, selection = "keep")
dfm_toksNgrams <- dfm_remove(dfm_toksNgrams, 
                             pattern = c(paste0("^", stopwords("english"), "_"), 
                                         paste0("_", stopwords("english"), "$")), 
                             valuetype = "regex")
rowTotals <- apply(dfm_toksNgrams, 1, sum) ###remove rows with no words
tokens_dfm_new   <- dfm_toksNgrams[rowTotals> 0, ]}

###run select data through tokenization/cleaning function
FreeText_tokens_dfm <- tokenize_dfm(inaug_speeches_clean$text_final)

###returns matrix dimensions and sparsity
FreeText_tokens_dfm
###view top terms 
head(featnames(FreeText_tokens_dfm))


##returns overall counts of word occurnences
textstat_frequency(FreeText_tokens_dfm)

####transform dfm to data frame
FreeText_tokens_df <- convert(FreeText_tokens_dfm, "data.frame")

FreeText_tokens_df[FreeText_tokens_df == 0] <- FALSE
FreeText_tokens_df[FreeText_tokens_df != 0] <- TRUE

#wc <- droplevels(col(FreeText_tokens_df, as.factor=TRUE)[which(FreeText_tokens_df != NA)])
#FreeText_tokens_df[levels(wc)] <- Map(factor, FreeText_tokens_df[levels(wc)], labels = levels(wc))
#FreeText_tokens_df

##change True/False to column names (words/phrases)
w <- which(FreeText_tokens_df==TRUE,arr.ind=TRUE)
FreeText_tokens_df[w] <- names(FreeText_tokens_df)[w[,"col"]]

##create new column that concatenates all columns (with occurences of words) by individual response
FreeText_tokens_df$NER_terms <- apply(FreeText_tokens_df[, 1:ncol(FreeText_tokens_df)], 1, paste, collapse = ",")

##remove whitespace
#trimws(FreeText_tokens_df$NER_terms)

##Remove "0s" (that represent blanks) in concatenated column
FreeText_tokens_df$NER_terms <- gsub("0,", "", FreeText_tokens_df$NER_terms)
FreeText_tokens_df$NER_terms <- gsub("0", "", FreeText_tokens_df$NER_terms)

##Remove last character "," at end of string
FreeText_tokens_df$NER_terms <- substr(FreeText_tokens_df$NER_terms,1,nchar(FreeText_tokens_df$NER_terms)-1)
dim(FreeText_tokens_df)

##rearrange columns to have common delimited list first
FreeText_tokens_df <- FreeText_tokens_df[c(ncol(FreeText_tokens_df), 1:(ncol(FreeText_tokens_df)-1))]
## check to make necessary columns remain
names(FreeText_tokens_df)

## create new df with list of NER terms
text_NERterms <- FreeText_tokens_df[c(1)]

### count number of bad words occuring in new column
text_NERterms$count_NERterms <- str_count(text_NERterms$NER_terms, ",") + 1

##join to feebackCombined
setDT(text_NERterms, keep.rownames = TRUE)[]
text_NERterms$rn <- gsub("[A-Za-z]", "", text_NERterms$rn)

text_NERterms <- text_NERterms[, rn:=as.integer(rn)]

################################ merge bad words to complete set ##################################
merge_doc <- function(x, y) {merge(x, y, 
                                   by.x="PresidentNumber", by.y="rn", all.x = TRUE)}

feedbackCombined_NERterms <- merge_doc(inaug_speeches_clean, text_NERterms)
str(feedbackCombined_NERterms)

##duplicate list to use for tooltip
feedbackCombined_NERterms$NER_terms_list <- feedbackCombined_NERterms$NER_terms

### count number of bad words occuring in new column
feedbackCombined_NERterms$count_NERterms <- str_count(feedbackCombined_NERterms$NER_terms, ",") + 1

##create placeholder for responses without at least 1 bad word
feedbackCombined_NERterms$NER_terms[which(is.na(feedbackCombined_NERterms$NER_terms))] <- "none"
feedbackCombined_NERterms$NER_terms[which(feedbackCombined_NERterms$NER_terms == c(""))] <- "none"


####################################################################################################
##################### Transform list of bad words (to feed into Tableau Dashboard) #################
####################################################################################################
require(reshape)
require(tidyr)

##seperate NER terms into multiple new columns
feedbackCombined_NERterms <- separate(feedbackCombined_NERterms, "NER_terms", c(paste0("NER_terms",1:60)), 
                                      sep = ",") 

##convert new split columns to data.table
feedbackCombined_NERterms <- as.data.table(feedbackCombined_NERterms)
names(feedbackCombined_NERterms)


feedbackCombined_NERterms <- melt(feedbackCombined_NERterms, measure.vars = c(paste0("NER_terms",1:60)), 
                                  value.name = "NER_term")

feedbackCombined_NERterms <- feedbackCombined_NERterms[!is.na(feedbackCombined_NERterms$NER_term)]

##
names(feedbackCombined_NERterms)
##check for unique counts
feedbackCombined_NERterms[, .(number_of_distinct = uniqueN(PresidentNumber)), by = NER_term]
##subset columns
#feedbackCombined_badWords <- feedbackCombined_badWords[, -c("X.1", "X", "Hire.Date", 
                                                            #"Original.Hire.Date", "Continuous.Service.Date")]

########################### Final data export ##################################################### 
setwd('C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks')
write.csv(feedbackCombined_NERterms, "feedbackCombined_NERterms.csv")





