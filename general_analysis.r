library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(scales)
library(cetcolor)
library(knitr)
source('plotting_functions.r')

survey = read.csv('data/clean_survey.csv') %>% fix_factors()
survey_categories = read.csv('data/survey_categories.csv') %>% fix_factors()

#what activities do you do on christmas?
activities = gather_category(survey_categories, 'activities_')
activities = activities %>% mutate(activities = mapvalues(activities, from='', to='None of the Above'),
                    celebrates_christmas = mapvalues(celebrates_christmas, from=c('Yes','No'), 
              to = c('Celebrates Christmas','Doesn\'t Celebrate Christmas')))



#separate data for questions that only apply to those who celebrate christmas
survey_celebrates = survey %>% filter(celebrates_christmas=='Yes')
survey_categories_celebrates = survey %>% filter(celebrates_christmas=='Yes')

#what does the demographics look like
#gender

ggplot(survey, aes(x=gender,  group=1)) + stat_count(aes(y=(..prop..)),fill='#EE3333') + 
  geom_text(aes(y=..prop.., label=utils::getAnywhere('smart_percent')$objs[[1]](..prop.., ..count..)), 
            stat='count', vjust='inward', size=6) + 
  ylab('') + xlab('') + 
  scale_y_continuous(label=percent, breaks = seq(0,0.6,0.1)) + 
  ggtitle('Gender of Christmas Survey Responses') + 
  theme_bw() + better_text_size
  
  

#religion
ggplot(survey, aes(x=religion,  group=1)) + stat_count(aes(y=(..prop..)),fill='#33EE33') + 
  geom_text(aes(y=..prop.., label=utils::getAnywhere('smart_percent')$objs[[1]](..prop.., ..count..)), 
            stat='count', hjust='inward', size=6) + 
  ylab('') + xlab('') + 
  scale_y_continuous(label=percent, breaks = seq(0,0.6,0.1), sec.axis=dup_axis()) + 
  ggtitle('Religion of Christmas Survey Responses') + 
  theme_bw() + better_text_size_manylabs + 
  coord_flip() + scale_x_discrete(limits=rev(levels(survey$religion)))

#region
ggplot(survey, aes(x=region,  group=1)) + stat_count(aes(y=(..prop..)),fill='#EE3333') + 
  geom_text(aes(y=..prop.., label=utils::getAnywhere('smart_percent')$objs[[1]](..prop.., ..count..)), 
            stat='count', hjust='inward', size=6) + 
  ylab('') + xlab('') + 
  scale_y_continuous(label=percent, breaks = seq(0,0.6,0.1), sec.axis=dup_axis()) + 
  ggtitle('Region of Christmas Survey Responses') + 
  theme_bw() + better_text_size_manylabs + 
  coord_flip() + scale_x_discrete(limits=rev(levels(survey$region)))



#who celebrates christmas?
#overall
ggplot(survey, aes(x=celebrates_christmas,  group=1)) + stat_count(aes(y=(..prop..)),fill='#33EE33') + 
  geom_text(aes(y=..prop.., label=utils::getAnywhere('smart_percent')$objs[[1]](..prop.., ..count..)), 
            stat='count', vjust='inward', size=6) + 
  ylab('') + xlab('response') + 
  scale_y_continuous(label=percent, breaks = seq(0,0.6,0.1)) + 
  ggtitle('Question: Do you Celebrate Christmas?') + 
  theme_bw() + better_text_size

#by gender
ggplot(survey, aes(x=celebrates_christmas,  group=1)) + stat_count(aes(y=(..prop..)),fill='#EE3333') + 
  geom_text(aes(y=..prop.., label=utils::getAnywhere('smart_percent')$objs[[1]](..prop.., ..count..)), 
            stat='count', vjust='inward', size=4.5) + 
  ylab('') + xlab('response') + 
  scale_y_continuous(label=percent, breaks = seq(0,1,0.1)) + 
  ggtitle('Question: Do you Celebrate Christmas?',subtitle='grouped by gender') + 
  theme_bw() + better_text_size + 
  facet_grid(~gender)

#by religion
ggplot(survey %>% split_factor_levels_with_newlines('religion'), 
       aes(x=celebrates_christmas,  group=1)) + stat_count(aes(y=(..prop..)),fill='#33EE33') + 
  geom_text(aes(y=..prop.., label=utils::getAnywhere('smart_percent')$objs[[1]](..prop.., ..count..)), 
            stat='count', hjust='outward', size=6, angle=90) + 
  ylab('') + xlab('') + 
  scale_y_continuous(label=percent, breaks = seq(0,0.6,0.1), sec.axis=dup_axis()) + 
  ggtitle('Question: Do you celebrate Christmas?', subtitle='grouped by religion') + 
  theme_bw() + better_text_size_manylabs + 
  facet_grid(~religion)


#by region
ggplot(survey %>% split_factor_levels_with_newlines('region'), 
       aes(x=celebrates_christmas,  group=1)) + stat_count(aes(y=(..prop..)),fill='#EE3333') + 
  geom_text(aes(y=..prop.., label=utils::getAnywhere('smart_percent')$objs[[1]](..prop.., ..count..)), 
            stat='count', hjust='outward', size=6, angle=90) + 
  ylab('') + xlab('') + 
  scale_y_continuous(label=percent, breaks = seq(0,0.6,0.1), sec.axis=dup_axis()) + 
  ggtitle('Question: Do you celebrate Christmas?', subtitle='grouped by region') + 
  theme_bw() + better_text_size_manylabs + 
  facet_grid(~region)

#what day do they celebrate?
ggplot(survey %>% 
         filter(celebrates_christmas=="Yes" & celebration_date != ''))+
  stat_count(aes(x=celebration_date, y=..prop.., group=1), fill='#33DD33') + 
  scale_y_continuous(label=percent) + xlab('') + ylab('') + 
  geom_text(aes_(group=1, x=~celebration_date, y=~..prop.., label=bquote(.(smart_percent)(..prop.., ..count..))), 
            vjust='inward', size=6, stat='count') + 
  ggtitle('Question: What activities do you do on Christmas?') + 
  better_text_size_manylabs 

ggplot(survey %>% 
         filter(celebrates_christmas=="Yes" & celebration_date != ''))+
  stat_count(aes(x=celebration_date, y=..prop.., group=1), fill='#EE3333') + 
  scale_y_continuous(label=percent) + xlab('') + ylab('') + 
  geom_text(aes_(group=1, x=~celebration_date, y=~..prop.., label=bquote(.(smart_percent)(..prop.., ..count..))), 
            vjust='inward', size=6, stat='count') + 
  ggtitle('Question: What activities do you do on Christmas?', subtitle='Grouped by religion') + 
  better_text_size_manylabs +
  facet_grid(religion~.)
  

#activities
#overall
ggplot(activities %>% filter(activities_value==1), 
       aes(x=activities, group=1))  + stat_count(aes(y=(..prop..)),fill='#33EE33') + 
  geom_text(aes(y=..prop.., 
                label=utils::getAnywhere('smart_percent')$objs[[1]](..prop.., ..count..)), 
            stat='count', hjust='inward', size=6) + 
  ylab('') + xlab('') + 
  scale_y_continuous(label=percent, breaks = seq(0,0.6,0.1), sec.axis=dup_axis()) + 
  ggtitle('Question: What activities do you do on Christmas?') + 
  theme_bw() + better_text_size_manylabs + 
  coord_flip()
  
#by celebration of Christmas
ggplot(activities %>% 
         calculate_group_stats('activities', 
                               group_variables='celebrates_christmas')) +
  geom_bar(aes(x=activities, y=prop), stat='identity', fill='#EE3333') + 
  scale_y_continuous(label=percent) + xlab('') + ylab('') + 
  geom_text(aes_(x=~activities, y=~prop, label=bquote(.(smart_percent)(prop, count))), hjust='inward', size=6) + 
  ggtitle('Question: What activities do you do on Christmas?', subtitle='Grouped by celebration of Christmas') + 
  coord_flip() + better_text_size_manylabs + 
  facet_grid(~celebrates_christmas)
  

       
#by gender
ggplot(activities %>% 
         calculate_group_stats('activities', 
                               group_variables='gender')) +
  geom_bar(aes(x=activities, y=prop), stat='identity', fill='#33EE33') + 
  scale_y_continuous(label=percent) + xlab('') + ylab('') + 
  geom_text(aes_(x=~activities, y=~prop, label=bquote(.(smart_percent)(prop, count))), hjust='inward', size=6) + 
  ggtitle('Question: What activities do you do on Christmas?', subtitle='Grouped by gender') + 
  coord_flip() + better_text_size_manylabs + 
  facet_grid(~gender)

#by religion
ggplot(activities %>% 
         calculate_group_stats('activities', 
                               group_variables='religion')) +
  geom_tile(aes(x=religion, y=activities, fill=prop)) + 
  geom_text(aes_(x=~religion, y=~activities, 
                 label=bquote(.(smart_percent)(prop, count))), size=3) + 
  ggtitle('Question: What activities do you do on Christmas?', subtitle='Grouped by religion') + 
   better_text_size_tiled + 
  scale_fill_gradientn('', colors=cet_pal(9, 'diverging_gwr_55-95_c38_n256'), label=percent) + 
  theme(axis.text.x = element_text(angle=20))
  

#by region
ggplot(activities %>% 
         calculate_group_stats('activities', 
                               group_variables='region')) +
  geom_tile(aes(x=region, y=activities, fill=prop)) + 
  geom_text(aes_(x=~region, y=~activities, 
                 label=bquote(.(smart_percent)(prop, count))), size=3) + 
  ggtitle('Question: What activities do you do on Christmas?', subtitle='Grouped by region of United States') + 
  better_text_size_tiled + 
  scale_fill_gradientn('', colors=cet_pal(9, 'diverging_gwr_55-95_c38_n256'), label=percent) + 
  theme(axis.text.x = element_text(angle=20))

#hmmm, so what makes someone more likely to actually spend time with friends/attend religious service?

service_model = step(glm(activities_Attend.religious.service ~ religion + region + celebrates_christmas + gender, 
                         family=binomial,
                         data=survey_categories), 
             k=log(nrow(survey)),
             direction='both')

friends_model = step(glm(activities_Spend.time.with.friends ~ religion + region + celebrates_christmas + gender, 
                         family=binomial,
                         data=survey_categories), 
                     k=log(nrow(survey)),
                     direction='both')

summary(friends_model)
