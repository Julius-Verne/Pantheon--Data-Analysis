
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

ggplot(db, aes(x=birth_year))+
  geom_histogram(binwidth = 5)+ 
  ggtitle("Year of Birth", subtitle = "5% Quantile and Up")+ 
  labs( x = "Years", y = "# of People")+
  xlim(1800, 2010)

??break

## Country  #########################################

ggplot(db, aes(x = fct_infreq(country)))+
  geom_bar()+ 
  ggtitle("# of People By Country", 
          subtitle = "Data Obtained from the Pantheon Dataset") + 
  labs(y = "# of People", x = "Countries")+
  theme_minimal()+ 
  theme_update(plot.title = element_text(size = rel(2)))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.6))



## Region  #########################################

ggplot( subset(db, continent != ""), aes(x = continent)) +
  geom_bar()+ 
  ggtitle("# of People By Continent", 
          subtitle = "Data Obtained from the Pantheon Dataset") + 
  labs(y = "# of People", x = "Continent")+
  theme_minimal()+ 
  theme_update(plot.title = element_text(size = rel(2)))

size <- subset(db, !is.na(continent))

## Occupation  #########################################

ggplot(db, aes(x = occupation))+
  geom_bar()+ 
  ggtitle("# of People By Industry", 
          subtitle = "Data Obtained from the Pantheon Dataset") + 
  labs(y = "# of People", x = "Industry")+
  theme_minimal()+ 
  theme_update(plot.title = element_text(size = rel(2)))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.6))

## Industry  #########################################

ggplot(db, aes(x = industry))+
  geom_bar()+ 
  ggtitle("# of People By Industry", 
          subtitle = "Data Obtained from the Pantheon Dataset") + 
  labs(y = "# of People", x = "Industry")+
  theme_minimal()+ 
  theme_update(plot.title = element_text(size = rel(2)))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.6))
 
## Domain  #########################################

ggplot(db, aes(x = domain))+
  geom_bar()+ 
  ggtitle("# of People By Domain", 
          subtitle = "Data Obtained from the Pantheon Dataset") + 
  labs(y = "# of People", x = "Field of Expertise")+
  theme_minimal()+ 
  theme_update(plot.title = element_text(size = rel(2)))

## Article Languages  #########################################

ggplot(db, aes(x = article_languages))+
  geom_histogram(binwidth = 5)+ 
  scale_x_continuous(breaks = seq(25,200,5))+
  ggtitle("# of People By Article Language", 
          subtitle = "Data Obtained from the Pantheon Dataset") + 
  labs(y = "# of People", x = "Field of Expertise")+
  theme_minimal()+ 
  theme_update(plot.title = element_text(size = rel(2)))

ggplot(db, aes(x = article_languages))+
  geom_histogram(binwidth = 0.05)+ 
  ggtitle("# of People By Domain", 
          subtitle = "Data Obtained from the Pantheon Dataset") + 
  labs(y = "# of People", x = "Field of Expertise")+
  theme_minimal()+ 
  theme_update(plot.title = element_text(size = rel(2)))+
  scale_x_log10()

## Page Views  #########################################

ggplot(db, aes(x = page_views))+
  geom_histogram()+
  scale_x_log10() +
  xlim(quantile(db$page_views, probs = 0.05),
       quantile(db$page_views, probs = 0.95))

ggplot(db, aes(x = page_views))+
  geom_histogram()+
  scale_x_log10()

## Average Views  #########################################

ggplot(db, aes(x = average_views))+
  geom_histogram()+
  xlim(0,
       quantile(db$average_views, probs = 0.95))

ggplot(db, aes(x = average_views))+
  geom_histogram()+
  scale_x_log10()

## Historical Popularity Index  #########################################

ggplot(db, aes(x = hpi))+
  geom_histogram(binwidth = 0.5)


?arrange()
