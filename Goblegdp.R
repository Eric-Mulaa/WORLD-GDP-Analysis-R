mainfolder <- "C:/Users/Admin/OneDrive/Desktop/MULAA/"
  setwd(mainfolder)
gdp <- read.table(file = "GDPdata.csv", header = TRUE, sep = ",")
summary(gdp)
str(gdp)

# I wll need to convert Agriculture columns to numeric
gdp$Agriculture.GDP <- gsub(",", "", gdp$Agriculture.GDP)
gdp$Agriculture.GDP <- as.numeric(gdp$Agriculture.GDP)
gdp$AGR.Rank <- gsub(",", "", gdp$AGR.Rank)
gdp$AGR.Rank <- as.numeric(gdp$AGR.Rank)

#let me get rid of incomplete entries if any
sum(is.na(gdp))
gdp <- gdp[complete.cases(gdp), ]

# lets check for duplicates
which(duplicated(gdp))

head(gdp)
#lets see what % every sector contributes to overall gdp
x <- sum(gdp$GDP..millions.of...)
perc <- round(c(100*sum(gdp$Agriculture.GDP/x), 100*sum(gdp$Industry)/x, 100*sum(gdp$Services)/x))
lbl <- c("Agriculture", "Industry", "Service")
lbl1 <- paste(lbl, perc, "%", sep = " ")
pie(perc, labels = lbl1, main = "Sector cotribution to overall GDP", col = rainbow(length(perc)))

# let see the top 3 economies in every sector
# I will need to load some necessary packages
library(tidyverse)
library(tidyr)
library(ggplot2)

top <- gdp %>%
  filter(AGR.Rank <= 3| IND.Rank <= 3| SER.Rank <= 3) %>%
  select(Country.Economy, Agriculture.GDP, Industry, Services)
top <- gather(top, sectors, output, -1)
ggplot(top, aes(x = sectors, y = output, fill = Country.Economy))+ 
  geom_bar(stat = "identity", position = "dodge")+ 
  labs(title = "Top 3 Economies In All Sectors", caption = "Kaggle Dataset")

## We see that in Agriculture sector, China is leading, followed by India and the USA,
## In the Industry sector, China is the leading economy, followed by USA and the Japan
## In Service sector, USA comes first, then China and Japan at the third place.

# let see the bottom economies in each sector using slice function

agri_min <- gdp %>%
  slice_min(Agriculture.GDP, n = 3)
ggplot(agri_min, aes(reorder(Country.Economy, Agriculture.GDP), Agriculture.GDP))+ 
  geom_bar(stat = "identity", fill = "green")+ 
  labs(title = "Bottom Economies", subtitle = "Agricultural Sector", 
       caption = "Kaggle Dataset", x = "Economies")

ind_min <- gdp %>%
  slice_min(Industry, n = 3)
ggplot(ind_min, aes(reorder(Country.Economy, Industry), Industry))+ 
  geom_bar(stat = "identity", fill = "blue")+ 
  labs(title = "Bottom Economies", subtitle = "Industry Sector", 
       caption = "Kaggle Dataset", x = "Economies")

ser_min <- gdp %>%
  slice_min(Services, n = 3)
ggplot(ser_min, aes(reorder(Country.Economy, Services), Services))+ 
  geom_bar(stat = "identity", fill = "orange")+ 
  labs(title = "Bottom Economies", subtitle = "Service Sector", 
       caption = "Kaggle Dataset", x = "Economies")
  
# lets have an analysis of the G20 countries
G20 <- c("Argentina", "Australia", "Brazil", "Canada", "China", 
         "India", "Indonesia", "Japan", "South Korea", "Mexico", 
         "Russia", "Saudi Arabia", "South Africa", "Turkey", "United Kingdom", 
         "United States", "Austria", "Belgium", "Bulgaria", "Croatia", 
         "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", 
         "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", 
         "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", 
         "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

g20 <- gdp %>%
  filter(Country.Economy %in% G20, Rank <= 10)
ggplot(g20, aes(reorder(Country.Economy,-GDP..millions.of...), GDP..millions.of...))+ 
  geom_bar(stat = "identity", fill = "blue")+ 
  labs(title = "G20 Economies", caption = "Kaggle Dataset", x = "Economies") 

rename(gdp, proportion_agr = X..of.GDP, proportion_ind = X..of.GDP.1, proportion_ser = SER.Rank)
gdp$X..of.GDP

#leading proportion among the top 10 econoies
leading_prop <- gdp%>%
  filter(Rank <= 10) %>%
 gather(sector_prop, prop, X..of.GDP, X..of.GDP.1, X..of.GDP.2)

ggplot(leading_prop, aes(x = Country.Economy, y = prop, fill = sector_prop))+ 
  geom_bar(stat = "identity", position = "dodge")+ 
  labs(title = "Top Ten Economies' Sectors", x = "Economies")+ 
  scale_color_discrete(labels = c("Agriculture", "Industry", "Service"))
## we can see that Service sector is leading in all econimies. All these Economies relay most on srvice sector

# lets see about BRICS
brics <- c("Brazil", "Russia", "India", "China", "South Africa")
gdp_brics <- gdp %>%
  filter(Country.Economy %in% brics)
ggplot(gdp_brics, aes(reorder(Country.Economy,-GDP..millions.of...), GDP..millions.of...))+ 
  geom_bar(stat = "identity", fill = "blue")+ 
  labs(title = "BRICS Economies 2017")

## China is the largest Economy of all, followed by India
gdp_brics2 <- gdp_brics %>%
  gather(sectors, outputs, Agriculture.GDP, Industry, Services)
ggplot(gdp_brics2, aes(x = Country.Economy, y = outputs, fill = sectors))+ 
  geom_bar(stat = "identity", position = "dodge")+ 
  labs(title = "BRICS Economies 2017", subtitle = "Sectors Output")
  
# I will now check for correlation in the data
install.packages("GGally")  
library(GGally)
head(gdp)
ggpairs(gdp, columns = c(4, 5, 8, 11))
## Conclusion: There is a positive correlation between overall GDP and Agriculture,
## Industry, and Services. The correlation is strong for Industry and Services
## There is a strong correlation between Agriculture ouput and Service, 
## and between Industry and Services, 
## However the correlation between Agriculture and Services is weak.

# I will create a model to predict this relationship.
gdpmodel <- lm(gdp$GDP..millions.of... ~ gdp$Agriculture.GDP + 
     gdp$Agriculture.GDP + gdp$Industry + gdp$Services)
summary(gdpmodel)


