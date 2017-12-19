#this file just reads in the raw data, combines the surveys into one, removes the timestamps,
# fixes the categories of two questions that were changed after a while (so they would be 
# indicative of response time if kept in)

data_fb = read.csv('data_raw/survey_reddit_fb.csv', stringsAsFactors=FALSE)
data_promo = read.csv('data_raw/survey_promoted.csv', stringsAsFactors=FALSE)
data_fbshare = read.csv('data_raw/survey_fb_sharing.csv', stringsAsFactors=FALSE)

combined_data = rbind(data_fb, data_promo, data_fbshare)

#remove timestamps
combined_data$Timestamp = NULL

#recategorize early responses to religious service questions
religious_freq_cols = c('Do.you.attend.religious.services.regularly.','How.frequently.did.you.attend.religious.services.before.you.were.18.')
for (col in religious_freq_cols){
 combined_data[,col] = ifelse(combined_data[,col]=='3-4 times a month', '3+ times a month', combined_data[,col])
}

combined_data = combined_data[sample(nrow(combined_data)),]

write.csv(combined_data, file='data/christmasSurvey.csv', row.names=FALSE)
