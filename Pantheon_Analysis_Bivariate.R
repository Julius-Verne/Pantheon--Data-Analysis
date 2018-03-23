
##Import Libraries
library(ggplot2)
library(dplyr)
library(scales)
library(GGally)
library(memisc)
library(gridExtra)
library(forcats)

##Import Data
base <-read.csv("data/database.csv", stringsAsFactors = FALSE)
db <- subset(base,
             select = c(full_name, birth_year, sex, country, continent, 
                        occupation, industry, domain, article_languages,
                        page_views,average_views,
                        historical_popularity_index))



db$birth_year <- as.numeric(db$birth_year)
db$hpi <- db$historical_popularity_index
db$sex <- as.factor(db$sex)
db$country <- as.factor(db$country)
db$continent <- as.factor(db$continent)
db$occupation <- as.factor(db$occupation)
db$industry <- as.factor(db$industry)
db$domain <- as.factor(db$domain)


summary(db)
str(base)

if(db$continent == "", db$continent <- "Unknown")
  
str(db)

??fig.height
  
##Univariate Exploration

## Birth Year  #########################################

ggplot(db, aes(x=birth_year))+
  geom_histogram(binwidth = 25)+ 
  scale_x_continuous(breaks = seq(-5000,2000,500))+
  ggtitle("Year of Birth")+ 
  labs(colour = "Cylinders", x = "Years", y = "# of People")+
  theme_minimal()+ 
  theme_update(plot.title = element_text(size = rel(2)))

ggplot(db, aes(x=birth_year))+
  geom_histogram()+ 
  scale_x_log10()+
  ggtitle("Year of Birth")+ 
  labs(colour = "Cylinders", x = "Years", y = "# of People")+
  theme_minimal()+ 
  theme_update(plot.title = element_text(size = rel(2)))


## Sex  #########################################

ggplot(db, aes(x= sex))+
  geom_bar(aes(fill = sex))+ 
  ggtitle("# of People By Sex", 
          subtitle = "Data Obtained from the Pantheon Dataset") + 
  labs(y = "# of People", x = "")+
  theme_minimal()+ 
  theme_update(plot.title = element_text(size = rel(2)))

p1 <- ggplot(db, aes(x = sex, y = page_views)) + 
  geom_boxplot()+
  ylim(0, quantile(db$page_views, probs = 0.9)) +
  ggtitle("Sex & Total views", subtitle = "Up to the 0.9 quantile")+ 
  labs(colour = "Cylinders", x = "Sex", y = "Page Views")

p2 <- ggplot(db, aes(x = sex, y = average_views)) + 
  geom_boxplot()+
  ylim(0, quantile(db$average_views, probs = 0.9)) +
  ggtitle("Sex and Average Views", subtitle = "Up to the 0.9 quantile")+ 
  labs(colour = "Cylinders", x = "Sex", y = "Average Views")

p3 <- ggplot(db, aes(x = sex, y = historical_popularity_index)) + 
  geom_boxplot()+
  ggtitle("Sex and HPI", subtitle = "Up to the 0.9 quantile")+ 
  labs(colour = "Cylinders", x = "Sex", y = "HPI")

grid.arrange(p1,p2,p3, ncol = 3)

str(db)

ggplot(db, aes(x=birth_year))+
  geom_histogram(binwidth = 25)+ 
  scale_x_continuous(breaks = seq(-5000,2000,500))+
  ggtitle("Year of Birth")+ 
  labs(colour = "Cylinders", x = "Years", y = "# of People")+
  facet_wrap(~ sex)


ggplot(subset(db, continent != ""), 
       aes(x=reorder(country , country,
                     function(x)-length(x))))+
  geom_bar()+ 
  ggtitle("# of People By Country", 
          subtitle = "Data Obtained from the Pantheon Dataset") + 
  labs(y = "# of People", x = "Country")+
  theme_minimal()+ 
  theme_update(plot.title = element_text(size = rel(2)))+
  coord_flip()+
  facet_wrap(~ sex)

########################################
## Total 

ggplot(db, aes(x = hpi, y = page_views)) + 
  geom_point() +
  ylim(0, quantile(db$page_views, probs = 0.95)) + 
  ggtitle("Initial Test Between Page Views & HPI", 
          subtitle = "Page Views & Historical Popularity Index") + 
  labs(y = "Page Views", x = "Historical Popularity Index")

ggplot(db, aes(x = hpi, y = log10(page_views) )) + 
  geom_point() +
  ylim(4, quantile(log10(db$page_views), probs = 0.95)) + 
  ggtitle("Initial Test Between Page Views & HPI", 
          subtitle = "Page Views & Historical Popularity Index") + 
  labs(y = "Page Views", x = "Historical Popularity Index")

cor.test(db$hpi, log10(db$page_views))

## Avg Views

ggplot(db, aes(x = hpi, y = average_views)) + 
  geom_point() +
  ylim(0, quantile(db$average_views, probs = 0.95))

ggplot(db, aes(x = hpi, y = log10(average_views) )) + 
  geom_point() +
  ylim(4, quantile(log10(db$average_views), probs = 0.95))

cor.test(db$hpi, db$page_views)
cor.test(db$hpi, log10(db$average_views))
        
## Lang

ggplot(db, aes(x = hpi, y = article_languages)) + 
  geom_jitter() +
  ylim(25, quantile(db$article_languages, probs = 0.95))+ 
  ggtitle("There is a vague positive relationship", 
          subtitle = "# of Languages & Historical Popularity Index") + 
  labs(y = "# of Languages", x = "Historical Popularity Index")

cor.test(db$hpi, db$article_languages)


## Year

ggplot(db, aes(x = hpi, y = birth_year)) + 
  geom_jitter(alpha = 0.5)+
  scale_y_continuous( breaks = seq(-3000, 2000, 500) ) + 
  ggtitle("Older Notable figures tend to have a higher HPI", 
          subtitle = "Birth Year & Historical Popularity Index") + 
  labs(y = "Year", x = "Historical Popularity Index")

cor.test(db$hpi, db$birth_year)

## Domain

ggplot(db, aes(x=reorder( domain , domain,
                          function(x)-length(x)),
               y = hpi))+
  geom_boxplot()

## Industry

ggplot(db, aes(x=reorder( industry , industry,
                          function(x)-length(x)),
               y = hpi))+
  geom_boxplot()+
  geom_hline( yintercept= median(db$hpi), linetype="dashed", color = "red")

## Continent

ggplot(subset(db, continent!="" ), aes(x=reorder( continent , continent,
                          function(x)-length(x)),
               y = hpi))+
  geom_boxplot()+
  geom_hline( yintercept= median(db$hpi), linetype="dashed", color = "red")

## Sex

ggplot(subset(db, continent!="" ), aes(x=reorder( sex , sex,
                                                  function(x)-length(x)),
                                       y = hpi))+
  geom_boxplot()+
  geom_hline( yintercept= median(db$hpi), linetype="dashed", color = "red")
