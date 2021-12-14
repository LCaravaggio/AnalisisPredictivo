setwd("~/prog/R/predict")

library(dplyr)
library(funModeling)
library(ggplot2)

df <- read.csv('data/resultados_original.csv')


df_status(df)
df %>% filter(startYear>2000) %>% ggplot(aes(y=startYear,x=log(budget)))+geom_boxplot()

zq <- df %>% group_by(production_companies) %>% summarise(avg_rating=mean(averageRating),movies=n()) %>% filter(movies>4) %>% arrange(desc(movies))
View(zq)

zq <- zq[3:158,'production_companies']

df$prod_comp_monster <- ifelse(df$production_companies %in% zq$production_companies,1,0)

sum(df$prod_comp_monster)

df$disney <- ifelse(grepl('Disney',df$production_companies, fixed = TRUE),1,0)
df$warner <- ifelse(grepl('Warner',df$production_companies, fixed = TRUE),1,0)
df$columbia <- ifelse(grepl('Columbia',df$production_companies, fixed = TRUE),1,0)
df$bbc <- ifelse(grepl('BBC',df$production_companies, fixed = TRUE),1,0)
df$fox <- ifelse(grepl('Fox',df$production_companies, fixed = TRUE),1,0)
df$mgm <- ifelse(grepl('MGM',df$production_companies, fixed = TRUE),1,0)
df$paramount <- ifelse(grepl('Paramount',df$production_companies, fixed = TRUE),1,0)
df$universal <- ifelse(grepl('Universal',df$production_companies, fixed = TRUE),1,0)
df$weinstein <- ifelse(grepl('Weinstein',df$production_companies, fixed = TRUE),1,0)
df$marvel <- ifelse(grepl('Marvel',df$production_companies, fixed = TRUE),1,0)
sum(df$disney)

median(df$budget[df$startYear==2019],na.rm=T)


df %>% filter(startYear>2000) %>% ggplot(aes(x=(budget)))+geom_histogram()+facet_wrap(startYear~.)

df$budget <- ifelse(df$budget==0,NaN,df$budget)


budget <- df %>% group_by(startYear) %>% summarise(movies=n(),mean_budget=mean(budget,na.rm=T),median_budget=median(budget,na.rm=T)) %>% arrange(desc(movies))
View(budget)

library(corrplot)
cor(df$budget,df$averageRating)

df <- df %>% left_join(budget,by='startYear')


df$budget <- ifelse(is.na(df$budget),df$median_budget,df$budget)

mean(df$budget[df$startYear==2019],na.rm=T)/mean(df$budget[df$startYear==2018],na.rm=T)

mean(df$budget[df$startYear==2017],na.rm=T)/mean(df$budget[df$startYear==2016],na.rm=T)

mean(df$budget[df$startYear==2016],na.rm=T)/mean(df$budget[df$startYear==2015],na.rm=T)

df$budget

df$budget[df$startYear==2020] <- 1.55*budget$median_budget[budget$startYear==2019]

df$budget <- ifelse(is.na(df$budget),0,df$budget)

df_status(df)

df %>% ggplot(aes(x=budget))+geom_histogram()

df$mean_budget <- ifelse(is.na(df$mean_budget),0,df$mean_budget)

df$budget_dif <- (df$budget-df$mean_budget)

df$budget_dif <- as.numeric(df$budget_dif)
df$budget_dif

df$budget_dif_scaled <- scale(as.numeric(df$budget_dif))
df$budget_dif_scaled
df_status(df)

df$budget[df$startYear==2020]
df_status(df)
# View(df)

glimpse(df)

# variables_con_nulos <- df %>%
#     select(revenue,runtime,isOriginalTitle,ordering,budget,popularity) %>% 
#     filter(!is.na(revenue),!is.na(runtime),!is.na(isOriginalTitle),!is.na(ordering),!is.na(budget),!is.na(popularity))
# 
# directors> asignar a frecuencia, onehot con ~20 mas populares, cross validation?
# summary(variables_con_nulos)


# variables_con_nulos %>% ggplot(aes(y=log(runtime)))+geom_boxplot()

#tratamos missings, atípicos, etc.

df$isAdult <- ifelse(is.na(df$isAdult),0,df$isAdult)

#directors
df %>%
    group_by(directors) %>%
    summarise(movies=n(),avg_score=mean(averageRating)) %>%
    filter(movies>10) %>% 
    arrange(desc(avg_score))

p <- df %>% group_by(directors) %>%
    summarise(avg_score=mean(averageRating),movies=n(), votes=sum(numVotes)) %>%
    arrange(desc(avg_score)) %>% 
    filter(votes>5000, avg_score>8)

q <- df %>% group_by(directors) %>%
    summarise(avg_score=mean(averageRating),movies=n(), votes=sum(numVotes)) %>%
    arrange(desc(avg_score)) %>% 
    filter(votes>5000, avg_score<=4)

nrow(p)
nrow(q)

#genre
generos <- df %>%
    group_by(genres_x) %>%
    summarise(movies=n(),avg_score=mean(averageRating)) %>%
    filter(movies>10000) %>% 
    arrange(desc(avg_score)) %>% 
    select(genres_x)

generos[2:19,]

# titleType

df$titleType <- as.factor(df$titleType)

df %>% group_by(titleType) %>%
    summarise(rating=mean(averageRating),count=n()) %>% 
    arrange(desc(rating))

# df <- df %>% filter(titleType %in% c('tvEpisode','video','movie','tvMovie','short','tvSeries'))
glimpse(df)
#variables
df$tvEpisode <- ifelse(df$titleType=='tvEpisode',1,0)
df$movie <- ifelse(df$titleType=='movie',1,0)
df$video <- ifelse(df$titleType=='video',1,0)
df$tvMovie <- ifelse(df$titleType=='tvMovie',1,0)
df$short <- ifelse(df$titleType=='short',1,0)
df$tvSeries <- ifelse(df$titleType=='tvSeries',1,0)

glimpse(df)
df$Action <- ifelse(grepl('Action',df$genres_x, fixed = TRUE),1,0)
df$Adventure <- ifelse(grepl('Adventure',df$genres_x, fixed = TRUE),1,0)
df$Animation <- ifelse(grepl('Animation',df$genres_x, fixed = TRUE),1,0)
df$Crime <- ifelse(grepl('Crime',df$genres_x, fixed = TRUE),1,0)
df$Drama <- ifelse(grepl('Drama',df$genres_x, fixed = TRUE),1,0)
df$Adult <- ifelse(grepl('Adult',df$genres_x, fixed = TRUE),1,0)
df$Comedy <- ifelse(grepl('Comedy',df$genres_x, fixed = TRUE),1,0)
df$Romance <- ifelse(grepl('Romance',df$genres_x, fixed = TRUE),1,0)
# df$Short <- ifelse(grepl('Short',df$genres_x, fixed = TRUE),1,0) coincide conn title type
df$Mystery <- ifelse(grepl('Mystery',df$genres_x, fixed = TRUE),1,0)
df$Documentary <- ifelse(grepl('Documentary',df$genres_x, fixed = TRUE),1,0)
df$Music <- ifelse(grepl('Music',df$genres_x, fixed = TRUE),1,0)
df$Reality_TV <- ifelse(grepl('Reality-TV',df$genres_x, fixed = TRUE),1,0)
df$Thriller <- ifelse(grepl('Thriller',df$genres_x, fixed = TRUE),1,0)
df$Horror <- ifelse(grepl('Horror',df$genres_x, fixed = TRUE),1,0)


df$es_director_piola <- ifelse(df$directors %in% p$directors,1,0)
df$es_director_malo <- ifelse(df$directors %in% q$directors,1,0)
sum(df$es_director_malo)
sum(df$es_director_piola)
df$episodeNumber <- ifelse(df$episodeNumber=="",0,df$episodeNumber)
df$episodeNumber <- as.numeric(df$episodeNumber)
df$episodeNumber <- ifelse(is.na(df$episodeNumber),0,df$episodeNumber)

glimpse(df)

df$titleType <- as.factor(df$titleType)
df$startYear <- as.numeric(df$startYear)
df$endYear <- as.numeric(df$endYear)
df$endYear <- ifelse(is.na(df$endYear),2021,df$endYear)
df$seasonNumber <- as.numeric(df$seasonNumber)
df$seasonNumber <- ifelse(is.na(df$seasonNumber),0,df$seasonNumber)

# df$duracion <- ifelse(df$tvEpisode==1 || df$tvSeries==1,df$endYear-df$startYear,0)

df_status(df)

inflacion <- read.csv('CPIAUCSL.csv')
inflacion <- inflacion %>% filter(mes==12) %>% select(CPIAUCSL,año)
inflacion

ipcbase <- inflacion$CPIAUCSL[inflacion$año==2020]
ipcbase <- 261.56
ipcbase

df <- df %>% left_join(inflacion, by=c('startYear'='año'))


df$budget_real <- (df$budget*ipcbase)/df$CPIAUCSL

df$budget_real <- ifelse(is.na(df$budget_real),0,df$budget_real)
summary(df$budget_real)
df
j <- df %>% select(disney,warner,columbia,bbc,fox,mgm,paramount,universal,weinstein,marvel,
                   budget,budget_dif,budget_dif_scaled,budget_real,numVotes,video,tvMovie,tvSeries,tvEpisode,movie,Action,Adventure,Animation,Crime,Drama,Adult,Comedy,
                   Romance,short,Mystery,Documentary,Music,Reality_TV,Thriller,Horror,es_director_malo,es_director_piola)
dim(j)

df_status(j)

# escritura y vamos a python

# library(caTools)
# set.seed(123)
# split = sample.split(j, SplitRatio = 0.8)
# j_training = subset(j, split == TRUE)
# j_testing = subset(j, split == FALSE)
# 
# write.csv(j_training,'j_training_FINAL.csv')
# write.csv(j_testing,'j_testing_FINAL.csv')

write.csv(j,'TESTEAR_FINAL.csv')

#vamos a python
