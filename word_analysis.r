library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(scales)
library(cetcolor)
library(tm)
library(wordcloud)
source('plotting_functions.r')

survey = read.csv('data/clean_survey.csv') %>% fix_factors() %>%
  mutate(description=as.character(description))

#functions to transform words
map_words <- function(text){
  text %>% 
    VectorSource %>%
    Corpus %>%
    tm_map(tolower) %>%
    tm_map(removeWords, stopwords('english') )
}

comp.wordcloud <- function(data, scale=c(10,1), ...){
  wordcloud(data$description %>% map_words, colors=c(cet_pal(12), '#F00101'),
            scale=scale, random.order=FALSE, ...)
}

ntitle <- function(text, cex=1.5, n_newline=4){
  title(paste0(paste(rep('\n', n_newline), collapse=''), text), cex.main=cex)
}


comp.wordcloud(survey)
ntitle('How People Describe Christmas')

comp.wordcloud(survey %>% filter(celebrates_christmas=="No"), scale=c(4,2),min.freq=1)
ntitle('How people who don\'t celebrate Christmas describe it')

comp.wordcloud(survey %>% filter(religion=='Atheist/agnostic'))
ntitle('How atheist/agnostic persons describe Christmas')

comp.wordcloud(survey %>% filter(grepl('Christ',religion)))
ntitle('How Christains describe Christmas')

comp.wordcloud(survey %>% filter(religion %in% c('Judaism',"Other")), min.freq=1, scale=c(6,1))
ntitle('How followers of non-Christian religions describe Christmas')


