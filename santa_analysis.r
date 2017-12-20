library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(scales)
library(cetcolor)
source('plotting_functions.r')
library(survival)

survey = read.csv('data/clean_survey.csv') %>% fix_factors()
santa = survey %>% filter(!is.na(santa_not_real_age), santa_not_real_age > 0) %>% mutate(santa_not_real_age = as.numeric(santa_not_real_age))
santa_survival = rbind(santa, santa %>% mutate(censored=1, santa_not_real_age=santa_not_real_age-1))

#look at overall responses to celebrating christmas and believing in santa
santa_summary = survey %>% group_by(parent_gifts) %>% summarise(prop=n()/nrow(survey), cnt=n())
ggplot(santa_summary, aes(x="")) + 
  geom_bar(aes(y=prop,  fill=parent_gifts), 
             width=1, position='stack', stat='identity') + 
  
  scale_fill_manual('Question:\nDid your parents give you gifts on Christmas?',
                    values=c('#EE3333','#228b22','#222222')) + 
  xlab('') + ylab('') + theme_void() + 
  ggtitle('Question: Did your parents give you gifts as a child for Christmas?') +
  geom_label(aes(y=1-(prop/2 + c(0, cumsum((prop))[-3])), label=percent(prop)), size=6) + 
  coord_polar('y', start=0, direction=-1) +
  theme(legend.text=element_text(size=rel(1.5)),
        legend.title=element_blank(),
        plot.title=element_text(size=rel(2)))

ggplot(santa) + geom_density(aes(x=santa_not_real_age), fill='#228b22', alpha=0.35) + 
  scale_x_continuous(breaks = c(seq(0,15, 3), seq(20,60,10))) + theme_bw() + 
  better_text_size + 
  xlab('Age stopped believing in Santa Claus')  + 
  ggtitle("Age Kids Stop Believing in Santa", 
          subtitle='(including silly ages)')

ggplot(santa %>% filter(santa_not_real_age < 18)) + geom_density(aes(x=santa_not_real_age), fill='#228b22', alpha=0.35) + 
  scale_x_continuous(breaks = c(seq(0,15, 3), seq(20,60,10))) + theme_bw() + 
  better_text_size + 
  xlab('Age stopped believing in Santa Claus') + 
  ggtitle("Age Kids Stop Believing in Santa")

ggplot(santa %>% filter(santa_not_real_age < 18)) + geom_density(aes(x=santa_not_real_age, fill=gender), alpha=0.35)  + 
  scale_x_continuous(breaks = c(seq(0,15, 3), seq(20,60,10))) + theme_bw() + 
  better_text_size + 
  xlab('Age stopped believing in Santa Claus') + 
  ggtitle("Age Kids Stop Believing in Santa", 
          subtitle='Grouped by gender (at time of response)')

survival_obj = Surv(time=santa$santa_not_real_age ,
                    event=1-santa$censored)

fit = survfit(survival_obj ~ 1, data=santa) 

fit_data = with(summary(fit),data.frame(believes=surv, age=time, upper=upper, lower=lower))
#fill missing rows
i=2
while (i <= nrow(fit_data)){
  if (fit_data[i,'age'] != fit_data[i-1, 'age'] + 1){
    fit_data = rbind(fit_data[1:(i-1),], fit_data[i-1,], fit_data[i:nrow(fit_data),])
    fit_data[i, 'age'] = fit_data[i-1, 'age'] + 1
  }
  else{
    i = i + 1
  }
}

#survival curve
ggplot(fit_data) + geom_bar(aes(x=age, y=believes),stat='identity', width=1, fill='#228b22') + 
  geom_rect(aes(xmin=age-0.5, xmax=age+0.5, ymin=lower,ymax=upper), 
            color='#33FF0000', fill='red', alpha=0.3) + 
  scale_x_continuous(breaks=seq(4, 26, 2)) + theme_bw() + 
  better_text_size_manylabs + 
  ylab('probability of believing in Santa') + 
  scale_y_continuous(label=percent) + 
  ggtitle('Probability of believing in Santa vs. age',subtitle='95% error bars as red rectangles') + 
  theme(plot.subtitle=element_text(size=rel(1)))

#hazard ratio
fit_data$failure = c(1,head(fit_data$believes, -1)) - fit_data$believes 
fit_data$hazard = fit_data$failure/(1-cumsum(fit_data$failure))

ggplot(fit_data) + geom_bar(aes(x=age, y=hazard),stat='identity', width=1, fill='#EE3333') + 
  scale_x_continuous(breaks=seq(4, 26, 2)) + theme_bw() + 
  better_text_size_manylabs + 
  ylab('probability of no longer believing in Santa if still believing') + 
  scale_y_continuous(label=percent) + 
  ggtitle('Probability of no longer believing in Santa vs. Age', 
          subtitle='if person still believes in Santa; aka "hazard function"') + 
  theme(plot.subtitle=element_text(size=rel(1))) + 
  christmas_grids

cox_model = step(coxph(survival_obj ~ age_group + religion_pre18 + religious_service_attendance_pre18 + region + gender, 
                       data=santa, 
                       method='efron', singular.ok=TRUE),
                 direction='both', trace=0)
summary(cox_model)

ggplot(santa %>% filter(santa_not_real_age < 18) %>% split_factor_levels_with_newlines('religion_pre18')) + 
  geom_boxplot(aes(x=religion_pre18, y=santa_not_real_age, fill=religion_pre18)) + 
  xlab('Religion Before Turning 18') + ylab('Age when stopped believing in Santa') + 
  theme_bw() + better_text_size_manylabs + 
  scale_fill_discrete('') + 
  ggtitle('Age When Kids Stop Believing in Santa vs. Religion') + 
  theme(legend.key.height=unit(4.5, 'lines')) + 
  scale_y_continuous(breaks=3:18)
