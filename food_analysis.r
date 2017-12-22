library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(scales)
library(cetcolor)
library(ape)
source('plotting_functions.r')


survey = read.csv('data/clean_survey.csv') %>% fix_factors()
survey_categories = read.csv('data/survey_categories.csv') %>% fix_factors() %>%
  filter(celebrates_christmas=="Yes") %>% 
  mutate(desserts_pie..any. = desserts_pie..any. | desserts_pie..pecan. | desserts_pie..apple. | desserts_pie..pumpkin.)

#gather data for visualization
foods = gather_category(survey_categories, 'foods_')
foods = foods %>% 
  mutate(foods = mapvalues(foods, from=c('', 'stuffing dressing'), to=c('None of the Above','stuffing/dressing')),
                                celebrates_christmas = mapvalues(celebrates_christmas, 
                                                                 from=c('Yes','No'), 
                                to = c('Celebrates Christmas','Doesn\'t Celebrate Christmas')))


#beverages
beverages = gather_category(survey_categories, 'beverages_')
beverages = beverages %>% mutate(beverages = mapvalues(beverages, from='', to='None of the Above'),
                                   celebrates_christmas = mapvalues(celebrates_christmas, from=c('Yes','No'), 
                                   to = c('Celebrates Christmas','Doesn\'t Celebrate Christmas')))

desserts = gather_category(survey_categories, 'desserts_')
desserts = desserts %>% mutate(desserts = mapvalues(desserts, from='', to='None of the Above'),
                                 celebrates_christmas = mapvalues(celebrates_christmas, from=c('Yes','No'), 
                                 to = c('Celebrates Christmas','Doesn\'t Celebrate Christmas'))) %>%
  mutate(desserts = mapvalues(desserts, from=levels(desserts), to=gsub('pie (\\w+)','pie (\\1)', levels(desserts))))


select.not_na <- function(...){
 dots = list(...)
 result = character(length(dots[[1]]))
 for (i in 1:length(dots)){
   result = ifelse(is.na(dots[[i]]), result, as.character(dots[[i]]))
 }
 result
}

#similarity functions and their matrix-row wrappers (with an extra _ at the end)
cosine_similarity <- function(x, y){
  as.numeric(x %*% y /sqrt(sum(x^2)*sum(y^2)))
}

cosine_similarity_ <- function(i, j, m){
  cosine_similarity(m[i,], m[j,])
}

jaccard_similarity <- function(x, y){
  sum(x & y)/sum(x | y)
}

jaccard_similarity_ <- function(i, j, m){
  jaccard_similarity(m[i,],m[j,])
}

##let's do some regional clustering :-)
#we can do regional clustering by using calculated group stats and using cosine similarity
#between the percentages
region_food_stats = rbind(
  foods %>% calculate_group_stats('foods',group_variables='region') %>% 
    mutate(desserts=mapvalues(foods, from="None of the Above", to="None of the Above (foods)")),
  beverages %>% calculate_group_stats('beverages',group_variables='region') %>% 
    mutate(desserts=mapvalues(beverages, from="None of the Above", to="None of the Above (beverages)")),
  desserts %>% calculate_group_stats('desserts',group_variables='region') %>% 
    mutate(desserts=mapvalues(desserts, from="None of the Above", to="None of the Above (desserts)"))
) %>% mutate(item = factor(select.not_na(foods, beverages, desserts)), beverages=NULL, foods=NULL, desserts=NULL)

general_food_stats = rbind(
  foods %>% calculate_group_stats('foods',group_variables='id') %>% 
    mutate(desserts=mapvalues(foods, from="None of the Above", to="None of the Above (foods)")),
  beverages %>% calculate_group_stats('beverages',group_variables='id') %>% 
    mutate(desserts=mapvalues(beverages, from="None of the Above", to="None of the Above (beverages)")),
  desserts %>% calculate_group_stats('desserts',group_variables='id') %>% 
    mutate(desserts=mapvalues(desserts, from="None of the Above", to="None of the Above (desserts)"))
) %>% mutate(item = factor(select.not_na(foods, beverages, desserts)), beverages=NULL, foods=NULL, desserts=NULL)

calculate_distance_matrix <- function(data, formula, value.var, sim.function=cosine_similarity_){
  wide_matrix = acast(data, formula, value.var = value.var)
  distmat = 1 - outer(1:nrow(wide_matrix), 1:nrow(wide_matrix), 
                      FUN=Vectorize(cosine_similarity_, vectorize.args=c('i','j')), 
                      m=wide_matrix)
  rownames(distmat) = colnames(distmat) = rownames(wide_matrix)
  return(as.dist(distmat))
}

#cosine distance
region_distance_matrix = calculate_distance_matrix(region_food_stats, region~item, value.var="prop")

item_distance_matrix = calculate_distance_matrix(region_food_stats, item~region, value.var="prop")

#use jaccard with items...
item_jaccard_matrix = calculate_distance_matrix(general_food_stats, item~id, value.var="prop", jaccard_similarity_)

##let's cluster foods, beverages, and desserts together :P
region_clust = hclust(region_distance_matrix, method='ward.D2')
item_clust_cosine = hclust(item_distance_matrix, method='ward.D2')
item_clust_jaccard = hclust(item_jaccard_matrix, method='ward.D2')


#look at foods
#overall
ggplot(foods %>% 
         calculate_group_stats('foods', 
                               group_variables=NULL)) +
  geom_bar(aes(x=foods, y=prop), stat='identity', fill='#228b22') + 
  scale_y_continuous(label=percent) + xlab('') + ylab('') + 
  geom_text(aes_(x=~foods, y=~prop, label=bquote(.(smart_percent)(prop, count))), hjust='inward', size=6) + 
  ggtitle('Question: What foods do you eat on Christmas?') + 
  coord_flip() + better_text_size

#by region
ggplot(foods %>% 
         calculate_group_stats('foods', 
                               group_variables='region')) +
  geom_tile(aes(x=region, y=foods, fill=prop)) + 
  geom_text(aes_(x=~region, y=~foods, 
                 label=bquote(.(smart_percent)(prop, count))), size=3.5) + 
  ggtitle('Question: What foods do you eat on Christmas?', subtitle='Grouped by region') + 
  better_text_size_tiled + 
  scale_fill_gradientn('', colors=cet_pal(9, 'diverging_gwr_55-95_c38_n256'), label=percent) + 
  theme(axis.text.x = element_text(angle=0))


#look at beverages
#overall
ggplot(beverages %>% 
         calculate_group_stats('beverages', 
                               group_variables=NULL)) +
  geom_bar(aes(x=beverages, y=prop), stat='identity', fill='#228b22') + 
  scale_y_continuous(label=percent) + xlab('') + ylab('') + 
  geom_text(aes_(x=~beverages, y=~prop, label=bquote(.(smart_percent)(prop, count))), hjust='inward', size=6) + 
  ggtitle('Question: What beverages do you drink on Christmas?') + 
  coord_flip() + better_text_size

#by region
ggplot(beverages %>% 
         calculate_group_stats('beverages', 
                               group_variables='region')) +
  geom_tile(aes(x=region, y=beverages, fill=prop)) + 
  geom_text(aes_(x=~region, y=~beverages, 
                 label=bquote(.(smart_percent)(prop, count))), size=3.5) + 
  ggtitle('Question: What beverages do you drink on Christmas?', subtitle='Grouped by region') + 
  better_text_size_tiled + 
  scale_fill_gradientn('', colors=cet_pal(9, 'diverging_gwr_55-95_c38_n256'), label=percent) + 
  theme(axis.text.x = element_text(angle=0))


#look at desserts
#overall
ggplot(desserts %>% 
         calculate_group_stats('desserts', 
                               group_variables=NULL)) +
  geom_bar(aes(x=desserts, y=prop), stat='identity', fill='#228b22') + 
  scale_y_continuous(label=percent) + xlab('') + ylab('') + 
  geom_text(aes_(x=~desserts, y=~prop, label=bquote(.(smart_percent)(prop, count))), hjust='inward', size=6) + 
  ggtitle('Question: What desserts do you eat on Christmas?') + 
  coord_flip() + better_text_size


#by region
ggplot(desserts %>% 
         calculate_group_stats('desserts', 
                               group_variables='region')) +
  geom_tile(aes(x=region, y=desserts, fill=prop)) + 
  geom_text(aes_(x=~region, y=~desserts, 
                 label=bquote(.(smart_percent)(prop, count))), size=3.5) + 
  ggtitle('Question: What desserts do you eat on Christmas?', subtitle='Grouped by region') + 
  better_text_size_tiled + 
  scale_fill_gradientn('', colors=cet_pal(9, 'diverging_gwr_55-95_c38_n256'), label=percent) + 
  theme(axis.text.x = element_text(angle=0))

#time to plot CLUSTERING

refactor_by_cluster <- function(data, cluster){
  data %>% mutate(Var1 = factor(Var1, levels = levels(Var1)[cluster$order]),
                  Var2 = factor(Var2, levels = levels(Var2)[cluster$order])
  )
}


#region clust
ggplot(melt(as.matrix(1-as.matrix(region_distance_matrix))) %>%
         refactor_by_cluster(region_clust)
       ) + 
  geom_tile(aes(x=Var1, y=Var2, fill=value)) + 
  scale_fill_gradientn('similarity\n', colors=cet_pal(9, 'linear_blue_5-95_c73_n256'),
                       limits = 0:1) + 
  xlab('') + ylab('') + 
  ggtitle('Similarity of Regions by Food, Drink, and Desserts\nConsumed on Christmas')


plot(as.phylo(region_clust), 
     main="Clusters of US Regions by Similarity of Food, Drink, and Desserts\nConsumed on Christmas")

#item clust

ggplot(melt(as.matrix(1-as.matrix(item_distance_matrix))) %>% 
         refactor_by_cluster(item_clust_cosine)) + 
  geom_tile(aes(x=Var1, y=Var2, fill=value)) + 
  scale_fill_gradientn('similarity\n', colors=cet_pal(9, 'linear_blue_5-95_c73_n256'),
                       limits = 0:1) + 
  xlab('') + ylab('') + 
  ggtitle('Similarity of  Food, Drink, and Desserts\nConsumed on Christmas by Correlation within Regions') + 
  theme(axis.text.x = element_text(angle=90, hjust=1))
plot(as.phylo(item_clust_cosine), 
     main="Clusters of Food, Drink, and Desserts\nConsumed on Christmas by Correlation within Regions")

#item clust jaccard

ggplot(melt(as.matrix(1-as.matrix(item_jaccard_matrix))) %>%
         refactor_by_cluster(item_clust_jaccard)) + 
  geom_tile(aes(x=Var1, y=Var2, fill=value)) + 
  scale_fill_gradientn('similarity\n', colors=cet_pal(9, 'linear_blue_5-95_c73_n256'),
                       limits = 0:1) + 
  xlab('') + ylab('') + 
  ggtitle('Similarity of  Food, Drink, and Desserts\nConsumed on Christmas by Consumption of Individuals') + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

plot(as.phylo(item_clust_jaccard), 
     main="Clusters of Food, Drink, and Desserts\nConsumed on Christmas by Consumption of Individuals")