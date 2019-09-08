# ---
# title: "Analysis"
# author: "Ryan Marinelli"
# date: "9/7/2019"
# output: html_document
# ---



pkg <- c("shiny", "dplyr", "knitr", "plyr", "tidyverse", "ggplot2", "shinyWidgets", "gridExtra", "rmarkdown", "kableExtra",
         "markdown", "tm", "lubridiate", "MASS", "twitteR", "devtools")


new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg,dependencies=T)


library(twitteR)
api_key <- "fOP5NTozqjGsbTAMsGR0nWYq0"
api_secret <- "7NpNb1TTBF0VGeDCoEhFq2fhvuqjb0seYnP3d58BFO4nHQMeha"
token <- "1161997375336062977-zmC4VqMM0TMHiRONMwOmLMYSTc2u3g"
token_secret <- "MRlSdoIKYg81YiP0s5kN7jsi6s2DIIfMqMFYz3klrdqFS"


setup_twitter_oauth(api_key, api_secret, token, token_secret)



account <- readline(prompt = "Please type the twitter handle of the person you wish to review")
account.timeline <- userTimeline(account, n = 1000, includeRts = TRUE)


test.DF <- twListToDF(account.timeline)





text <- test.DF$text
retweetCount <-test.DF$retweetCount
created <- test.DF$created
fav <- test.DF$favoriteCount




library(tm)

corp <- Corpus(VectorSource(text))
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removePunctuation) 
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp, stemDocument)

tdm <- TermDocumentMatrix(corp)
tdm <- findFreqTerms(tdm, 20)




library(lsa) 
tfidf <- weightTfIdf(tdm)
lsa.tfidf <- lsa(tfidf)
words.df <- as.data.frame(as.matrix(lsa.tfidf$dk))

}

second <- function(created)
{

library(lubridate)

created <- ymd_hms(created)
created <- diff.POSIXt(created) 
created <- abs(created)/60
}

Third <- function(NewData){

newData <- cbind(test.DF$retweetCount, test.DF$favoriteCount) 
newData <- cbind(newData, created)
newData <-  cbind(newData, words.df[1])
newData <- cbind(newData, words.df[2]) 
newData <- cbind(newData, words.df[3])


colnames(newData) <- c("retweets", "favorite Count", "time", "LSA One","LSA Two", "LSA Three")

        


model.3 <- lm(retweetCount ~ time + time*`favorite Count`+ `LSA One`+ `LSA Two` + `LSA Three` , data = newData) 
model.3
}


library(ggplot2)

jpeg("Linear Model 2.jpeg")
plot(model.3)

# par(mfrow=c(2,2)


# Step 3: Run dev.off() to create the file!
# dev.off()

Twitter(hoangvo1312)



