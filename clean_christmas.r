#always load plyr before dplyr, since they share some function names, but
# you will almost always want the dplyr one
library(plyr)
library(dplyr)
#this is going to be used to split apart checkbox columns
# (food, drinks, and desserts questions in this survey)
library(qdapTools)

survey = read.csv('data/christmasSurvey.csv', stringsAsFactors=FALSE)


#first, remove duplicate rows
#in this case someone submitted 11 consecutive responses 
#no two responses should really be exactly the same
survey = survey[!duplicated(survey),]

#combine duplicate columns, an artifact of having two different
#branches on the Google Forms survey
duplicate_columns = names(survey)[names(survey) %in% paste0(names(survey),'.1')]

is_na_or_blank <- function(x){
  is.na(x) | x==''
}

for (column in duplicate_columns){
  original_column = substr(column, 1, nchar(column)-2)
  survey[,original_column] = ifelse(is_na_or_blank(survey[,original_column]), survey[,column], survey[,original_column])
  survey[,column] = NULL
}

#give each row in the survey a unique ID
survey$id = 1:nrow(survey)

#let's rename columns
#currently rename() doesn't support these inner functions, so
# I am using select()
survey = survey %>% select(id=id,
                           celebrates_christmas = Do.you.celebrate.Christmas.,
                           activities=starts_with('What.do.you.typically'),
                           celebration_date=starts_with('When.you..celebrate.'),
                           christmas_religious_service=starts_with('Do.you.attend.religious.service.'),
                           parent_gifts=starts_with('When.you.were.under.18..did.you.receive'),
                           santa_not_real_age=starts_with('If.you.answered...Yes..and.they.'),
                           desserts=contains('desserts'),
                           beverages=contains('beverages'),
                           foods=contains('foods'),
                           description=starts_with('Describe.Christmas'),
                           gender=contains('gender'),
                           age_group=contains('age.group'),
                           religion=ends_with('your.religion.'),
                           religious_service_attendance=contains('religious.services.regularly.'),
                           religion_pre18=contains('religion.before.you.were.18'),
                           religious_service_attendance_pre18=contains('religious.services.before.'),
                           region=contains('Which.region.of.')
                           )

#make beverages, foods, desserts, and description lowercase
for (column in c('foods','desserts','beverages','description')){
  survey[,column] = tolower(survey[,column])
}


#copy over before-18 columns to their original columns if
# responder is under 18 age group
for (column in c('religion_pre18', 'religious_service_attendance_pre18')){
  survey[,column] = ifelse(survey$age_group=='Under 18', 
                           survey[,substr(column,1,nchar(column)-6)],
                           survey[,column])
}

#(for fun) create a column indicating if survey was  what revealed Santa not being real
# (i.e., censoring); technically these are joke responses, but it should be fun just as
# an example; do not assume for those under 18 currently
survey$censored = 0
survey = survey %>% 
  mutate(censored = ifelse(age_group=='18-24' & between(santa_not_real_age, 18, 24),1, censored)) %>%
  mutate(censored = ifelse(age_group=='25-34' & between(santa_not_real_age, 25, 34),1, censored)) %>%
  mutate(censored = ifelse(age_group=='35-44' & between(santa_not_real_age, 35, 44),1, censored)) %>%
  mutate(censored = ifelse(age_group=='54-54' & between(santa_not_real_age, 45, 54),1, censored)) %>%
  mutate(censored = ifelse(age_group=='55+' & santa_not_real_age >= 55, 1, censored))

#let's break apart the dessert, beverage, and food columns
split_columns <- function(data, column, sep=';'){
  new_columns = mtabulate(lapply( strsplit(data[,column], sep),
                                  function(x) paste0(column,'_',x))
                                )
  return(cbind(data, new_columns))
}

survey_categories = survey %>% 
  split_columns('desserts') %>%
  split_columns('beverages') %>%
  split_columns('foods') %>% 
  split_columns('activities')


#write data back to folder
write.csv(survey, file='data/clean_survey.csv', row.names=FALSE)
write.csv(survey_categories, file='data/survey_categories.csv', row.names=FALSE)
