library(ggplot2)
library(gridExtra)

#Load Data

base <- read.csv("data/database.csv")

db <- subset(base, 
             select = c("full_name","sex", "birth_year", "city", "country", 
                        "continent", "occupation", "industry", "domain", 
                        "article_languages", "page_views", "average_views",
                        "historical_popularity_index"))


db$birth <- as.numeric(paste(db$birth_year))
db <- subset(db, !is.na(birth))

#Exploration

#Sex

ggplot(db, aes(x = sex)) + 
  geom_bar()

#Birth

ggplot(db, aes(x = birth))+
  geom_histogram(binwidth = 100) +
  scale_x_continuous(breaks = c(-5000,2000,250))

str(db)
