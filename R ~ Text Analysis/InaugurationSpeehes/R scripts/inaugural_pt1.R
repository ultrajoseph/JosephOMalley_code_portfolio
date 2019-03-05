# TA Lightning Talk: Inaugural Speeches -- Part 1
# Author: Joseph O'Malley (JO054429)
# Date: 7/11/2018
# Script ID: 0039
# Tags: count unique, kable tables, gsub, recode factors, date formatting, nchar, nwords, barchart, data export, subsetting
# packages: data.table, tidytext, dplyr, ggplot2, stringr, lubridate, tidyr 


##import data (from kaggle) see https://www.kaggle.com/adhok93/presidentialaddress/data
inaug_speeches <- read.csv("C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks/inaug_speeches.csv", encoding = "ASCII")

##############################################################################################
## Table of Contents
##############################################################################################

##1) EDA
##2) Text Cleaning/Manipulation
##3) Convert Dates
##4) Extract Number of words & characters

##install needed packages
#install.packages(c("data.table", "stringr", "lubridate","tidyr", "dplyr", "tidytext", "ggplot2"))

##############################################################################################
## EDA - Exploratory Data Analysis
##############################################################################################
require(data.table)

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


####################################################################################################
########################################## Text Cleaning/Manipulation ##############################
####################################################################################################

##convert strings we will analyze to charachters
inaug_speeches_dt <- inaug_speeches_dt[, text:=as.character(text)]
inaug_speeches_dt <- inaug_speeches_dt[, text2:=as.character(text2)]
##combine text columns in r (only one that couldn't fit into one excel cell)
inaug_speeches_dt$text <- apply(inaug_speeches_dt[ , c("text", "text2") ] ,1 , paste , collapse = "" )



##create duplicate text column (for manipulation)
##trim whitespace and covert text to lowercase
inaug_speeches_dt$text_final <- trimws(inaug_speeches_dt$text)



##convert to regex and add additional text cleanin using stringr (see https://www.rstudio.com/resources/cheatsheets/)
##remove encoding errors using stringr (i.e. <U+AO97>) see https://stackoverflow.com/questions/39993715/how-to-remove-unicode-u00a6-from-string
inaug_speeches_dt$text_final <- gsub("\\s*<u\\+\\w+>\\s*", " ", inaug_speeches_dt$text_final)
inaug_speeches_dt$text_final <- gsub("\\s*<U\\+\\w+>\\s*", " ", inaug_speeches_dt$text_final)
##convert whitespace to single space
inaug_speeches_dt$text_final <- gsub("\\s", " ", inaug_speeches_dt$text_final)
##remove non-reg charachters
inaug_speeches_dt$text_final <- gsub("[^[A-Za-z0-9 ][:punct:]]", "", inaug_speeches_dt$text_final)




###################################################################################################
############################# Convert date formats ################################################
###################################################################################################

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

##trim whitespace
inaug_speeches_dt$MonthDay <- trimws(inaug_speeches_dt$MonthDay)
inaug_speeches_dt$Year <- trimws(inaug_speeches_dt$Year)


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
##check counts for each "days of week"DayOfWeek"
inaug_speeches_dt[, .(number_of_distinct = uniqueN(text_final)), by = DayOfWeek]                                

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
unique(inaug_speeches_dt$Date)





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

####################################### Visualizing speech length ##################################
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

##subset barchart to first inauguration speeches
first_inaug <- 
  inaug_speeches_dt %>%
  filter(Inaugural.Address == "First Inaugural Address")

##create barchart in ggplot
first_inaug %>%
  unnest_tokens(word,text_final) %>%
  group_by(Speech) %>%
  summarise(num_words=n()) %>%
  mutate(mean_words=mean(num_words)) %>%
  ggplot(aes(x=Speech,y=(num_words)))+geom_bar(stat = "identity",width=0.5, 
                                               aes(fill = first_inaug$DayOfWeek)) +
  theme(axis.text.x = element_text(vjust=1,angle=90)) + theme(legend.position="bottom") +
  geom_text(aes(label=first_inaug$Year), vjust=0,angle=90,size=2.5,hjust=0)+ylim(c(0,11500)) +
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

######################################## Subset Columns ############################################
#### subset columns from dataset
inaug_speeches_clean <- subset(inaug_speeches_dt, select = c("PresidentNumber", "Name","Inaugural.Address", 
                                                             "DayOfWeek", "Year", "Date", "DateOriginal", 
                                                             "text_final","nchar", "num_words", "Speech"))

##################################### Export Cleaned Data ##########################################
setwd('C:/Users/JO054429/Documents/Consulting_ClientSurveysPractice/Analytics Training Center/Text Analysis Template Scripts/TA Lightning Talks')
write.csv(inaug_speeches_clean, "inaug_speeches_clean.csv")








