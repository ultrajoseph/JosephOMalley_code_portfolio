# TA Lightning Talk: Inaugural Speeches -- Part 4
# Author: Joseph O'Malley (JO054429)
# Date: 7/11/2018
# Script ID: 0042
# Tags: conditional formatting, unique counts, increase memory limit, NER, NER Visualization, Dictionary Creation, data export, remove duplicates, rename columns, gsub, export data, conditional formatting, merge, tableau transformation   
# packages: rJava, NLP, openNLP, data.table, dplyr, ggplot2, wordcloud, reshape, tidyr, tibble, quanteda, tm, stringr, magrittr, openNLPmodels.en 


###################################################################################################
################################## Table of Contents ##############################################
###################################################################################################

##9) Named Entity Recognition (NER)
##10) Dictionary Creation
##11) Transformation for Tableau

############################### import cleaned data (from pt 3) ####################################
inaug_speeches_clean <- read.csv("C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks/inaug_speeches_clean_plus.csv")

##install needed packages
#install.packages(c("rJava", "NLP", "openNLP","openNLPmodels.en", "reshape", "tibble",
                                                #"googleVis", "magrittr", "RColorBrewer"))


###################################################################################################
################################# NER #############################################################
###################################################################################################
require(rJava)
require(NLP)
require(openNLP)
require(data.table)
require(dplyr)

## see https://cran.r-project.org/web/packages/NLP/NLP.pdf


###convert raw data to data.table 
data <- as.data.table(inaug_speeches_clean)

###convert column(s) with text to be anaalyzed to charachter
data <- data[, text_final:=as.character(text_final)]

###to subset data with less than certain number of charachters (optional)
#data$NumChar <- nchar(data$TextColumn)
#data <- data[NumChar > 3, ]

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

####################################################################################################
############################## NER (Named Entity Extraction) #######################################
####################################################################################################
require(magrittr)
require(openNLPmodels.en)
require(rJava)
require(NLP)
require(openNLP)
require(data.table)
library(dplyr)


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


##visualize with googleVis (optional)
#library(googleVis)
#dfl <- data.frame(table(entities(text_doc, kind = "location")))
#Barp <- gvisColumnChart(dfl)
#plot(Barp)



##Convert NER terms to new data frame with frequency 

########################################## location  ###############################################
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



###################################################################################################
################################## Dictionary Creation ############################################
###################################################################################################

###export NER terms for further analysis
setwd("C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks")
write.csv(NER_combined, 'NER_combined.CSV')

###################################### NER term List #############################################
##returns lists of custom NER term dictionary in r
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

###create data table for conversion of free text to charachters
inaug_speeches_clean <- as.data.table(inaug_speeches_clean)
inaug_speeches_clean <- inaug_speeches_clean[, text_final:=as.character(text_final)]
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

################################ merge NER words to complete set ##################################
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
##################### Transform list of NER words (to feed into Tableau Dashboard) #################
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


########################### Final data export ##################################################### 
setwd('C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks')
write.csv(feedbackCombined_NERterms, "inaug_combined.csv")
