#Installing Packages

install.packages("ggplot2") #For Visualizations
library("ggplot2")

install.packages("dplyr") # For Data maipulation
library("dplyr")

install.packages("RColorBrewer") #Color pallete
library("RColorBrewer")

install.packages("ggthemes") #Extra themes for ggplot2
library("ggthemes")

install.packages("ggrepel") #Provides text and label for ggplot2
library("ggrepel")

install.packages("corrplot") # For corelation matrix
library("corrplot")

# Loading data 

data = read.csv("HappinessAlcoholConsumption.csv")
head(data)
tail(data)
summary(data) #Summary of data - Min, max etc
str(data) #Shows structure of data

#Analysis

#Find out Happiness Score
ggplot(data, aes(x = "", y = HappinessScore)) +
geom_boxplot(color = "red", fill = "blue") +
stat_summary(fun.y=mean, geom="point", shape=20, size=15, color="red", fill="red") +
xlab("Happiness Score") +
ylab("Happiness Scores") +
theme(legend.position ='none',axis.title.y = element_text(size=15), axis.text.y = element_text(size = 25),
      axis.title.x = element_text(size=25),axis.text.x = element_text(angle = 90, size = 50), 
      plot.title = element_text(size=25, hjust = 0.5)) +
ggtitle("Boxplot of the Happiness Score") 


#Happiness score by Region

data %>%
  group_by(Region) %>%
  summarise(avg_HappinessScore = mean(HappinessScore)) %>%
  ggplot(aes(x=reorder(factor(Region), avg_HappinessScore), y=avg_HappinessScore, fill = avg_HappinessScore)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Region") + 
  ylab("Mean Happiness Score") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10), 
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_HappinessScore), hjust = -0.3, size = 3) +
  ggtitle("Happiness Score by Region") + 
coord_flip() #transpose


#Top 25 Countries by Happiness Score

data %>%
  group_by(Country) %>%
  summarise(avg_HappinessScore = mean(HappinessScore)) %>%
  top_n(25) %>%
  ggplot(aes(x=reorder(factor(Country), avg_HappinessScore), y=avg_HappinessScore, fill = avg_HappinessScore)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Country") + 
  ylab("Mean Happiness Score") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
  axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10), 
  plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_HappinessScore), hjust = -0.3, size = 3) +
  ggtitle("Top 25 Countries by Happiness Score") + 
  coord_flip()


#Calculate Human Development Index

ggplot(data, aes(x = "", y = HDI)) +
  geom_boxplot(color = "red", fill = "blue") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=15, color="red", fill="red") +
  xlab("Human Development Index") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(angle = 90, size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  ggtitle("Boxplot of the Human Development Index") 

#Density plot of HDI

data %>% 
  ggplot(aes(x=HDI)) + 
  geom_density(fill = "red") + 
  geom_vline(aes(xintercept=mean(HDI)), color="blue", linetype="dashed", size=2) +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10), 
        plot.title = element_text(size=10, hjust = 0.5)) +
  ggtitle('Density Plot of the Human Development Index')


#HDI by Region

data %>%
  group_by(Region) %>%
  summarise(avg_HDI = mean(HDI)) %>%
  ggplot(aes(x=reorder(factor(Region), avg_HDI), y=avg_HDI, fill = avg_HDI)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Region") + 
  ylab("Mean Human Development Index") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_HDI), hjust = -0.3, size = 3) +
  ggtitle("Human Development Index by Region") + 
  coord_flip()


#Top 25 Countries by HDI

data %>%
  group_by(Country) %>%
  summarise(avg_HDI = mean(HDI)) %>%
  top_n(25) %>%
  ggplot(aes(x=reorder(factor(Country), avg_HDI), y=avg_HDI, fill = avg_HDI)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Country") + 
  ylab("Mean Human Development Index") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_HDI), hjust = -0.3, size = 3) +
  ggtitle("Top 25 Countries by Human Development Index") + 
  coord_flip()


#Bottom 25 Countries by HDI

data %>%
  group_by(Country) %>%
  summarise(avg_HDI = mean(HDI)) %>%
  top_n(-25) %>%
  ggplot(aes(x=reorder(factor(Country), avg_HDI), y=avg_HDI, fill = avg_HDI)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Country") + 
  ylab("Mean Human Development Index") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_HDI), hjust = -0.3, size = 3) +
  ggtitle("Bottom 25 Countries by Human Development Index") + 
  coord_flip()


#GDP per Capita

ggplot(data, aes(x = "", y = GDP_PerCapita)) +
  geom_boxplot(color = "red", fill = "blue") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=15, color="red", fill="red") +
  xlab("GDP Per Capita") +
  ylab("GDP Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), 
        axis.text.y = element_text(size = 10), axis.title.x = element_text(size=10),
        axis.text.x = element_text(angle = 90, size = 10), plot.title = element_text(size=10, hjust = 0.5)) +
  ggtitle("Boxplot of the GDP Per Capita") 


#GDP per Capita by Region

data %>%
  group_by(Region) %>%
  summarise(avg_GDP_PerCapita = mean(GDP_PerCapita)) %>%
  ggplot(aes(x=reorder(factor(Region), avg_GDP_PerCapita), y=avg_GDP_PerCapita, fill = avg_GDP_PerCapita)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Region") + 
  ylab("Mean GDP Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_GDP_PerCapita), hjust = -0.1, size = 3) +
  ggtitle("GDP Per Capita by Region") + 
  coord_flip()


#Top 25 Countries by GDPper capita

data %>%
  group_by(Country) %>%
  summarise(avg_GDP_PerCapita = mean(GDP_PerCapita)) %>%
  top_n(25) %>%
  ggplot(aes(x=reorder(factor(Country), avg_GDP_PerCapita), y=avg_GDP_PerCapita, fill = avg_GDP_PerCapita)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Country") + 
  ylab("Mean GDP Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_GDP_PerCapita), hjust = -0.3, size = 3) +
  ggtitle("Top 25 Countries by GDP Per Capita") + 
  coord_flip()

#Beer per Capita

ggplot(data, aes(x = "", y = Beer_PerCapita)) +
  geom_boxplot(color = "red", fill = "blue") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=15, color="red", fill="red") +
  xlab("Beer Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(angle = 90, size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  ggtitle("Boxplot of the Beer Per Capita") 


#Density plot of Beer Per Capita

data %>% 
  ggplot(aes(x=Beer_PerCapita)) + 
  geom_density(fill = "red") + 
  geom_vline(aes(xintercept=mean(Beer_PerCapita)), color="blue", linetype="dashed", size=2) +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  ggtitle('Density Plot of the Beer Per Capita')


#Beer per Capita by Region

data %>%
  group_by(Region) %>%
  summarise(avg_Beer_PerCapita = mean(Beer_PerCapita)) %>%
  ggplot(aes(x=reorder(factor(Region), avg_Beer_PerCapita), y=avg_Beer_PerCapita, fill = avg_Beer_PerCapita)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Region") + 
  ylab("Mean Beer Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_Beer_PerCapita), hjust = -0.3, size = 3) +
  ggtitle("Beer Per Capita by Region") + 
  coord_flip()

#Top 25 Countries by Beer Per Capita

data %>%
  group_by(Country) %>%
  summarise(avg_Beer_PerCapita = mean(Beer_PerCapita)) %>%
  top_n(25) %>%
  ggplot(aes(x=reorder(factor(Country), avg_Beer_PerCapita), y=avg_Beer_PerCapita, fill = avg_Beer_PerCapita)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Country") + 
  ylab("Mean Beer Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_Beer_PerCapita), hjust = -0.3, size = 3) +
  ggtitle("Top 25 Countries by Beer Per Capita") + 
  coord_flip()

#Bottom 25 Countries by Beer Per Capita

data %>%
  group_by(Country) %>%
  summarise(avg_Beer_PerCapita = mean(Beer_PerCapita)) %>%
  top_n(-25) %>%
  ggplot(aes(x=reorder(factor(Country), avg_Beer_PerCapita), y=avg_Beer_PerCapita, fill = avg_Beer_PerCapita)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Country") + 
  ylab("Mean Beer Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_Beer_PerCapita), hjust = -0.3, size = 3) +
  ggtitle("Bottom 25 Countries by Beer Per Capita") + 
  coord_flip()

# Boxplot of Spirit per Capita

ggplot(data, aes(x = "", y = Spirit_PerCapita)) +
  geom_boxplot(color = "red", fill = "blue") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=15, color="red", fill="red") +
  xlab("Spirit Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(angle = 90, size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  ggtitle("Boxplot of the Spirit Per Capita") 

#Spirit per capita by Region

data %>%
  group_by(Region) %>%
  summarise(avg_Spirit_PerCapita = mean(Spirit_PerCapita)) %>%
  ggplot(aes(x=reorder(factor(Region), avg_Spirit_PerCapita), y=avg_Spirit_PerCapita, fill = avg_Spirit_PerCapita)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Region") + 
  ylab("Mean Spirit Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_Spirit_PerCapita), hjust = -0.1, size = 3) +
  ggtitle("Spirit Per Capita by Region") + 
  coord_flip()


#Top 25 countries by SPC

data %>%
  group_by(Country) %>%
  summarise(avg_Spirit_PerCapita = mean(Spirit_PerCapita)) %>%
  top_n(25) %>%
  ggplot(aes(x=reorder(factor(Country), avg_Spirit_PerCapita), y=avg_Spirit_PerCapita, fill = avg_Spirit_PerCapita)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Country") + 
  ylab("Mean Spirit Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_Spirit_PerCapita), hjust = -0.3, size = 3) +
  ggtitle("Top 25 Countries by Spirit Per Capita") + 
  coord_flip()


#Bottom 25 countries by SPC

data %>%
  group_by(Country) %>%
  summarise(avg_Spirit_PerCapita = mean(Spirit_PerCapita)) %>%
  top_n(-25) %>%
  ggplot(aes(x=reorder(factor(Country), avg_Spirit_PerCapita), y=avg_Spirit_PerCapita, fill = avg_Spirit_PerCapita)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Country") + 
  ylab("Mean Spirit Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_Spirit_PerCapita), hjust = -0.3, size = 3) +
  ggtitle("Bottom 25 Countries by Spirit Per Capita") + 
  coord_flip()


#Boxplot of Wine per capita

ggplot(data, aes(x = "", y = Wine_PerCapita)) +
  geom_boxplot(color = "red", fill = "blue") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=15, color="red", fill="red") +
  xlab("Wine Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size=10),axis.text.x = element_text(angle = 90, size = 10), 
        plot.title = element_text(size=10, hjust = 0.5)) +
  ggtitle("Boxplot of the Wine Per Capita") 


#Density plot oof wine per capita

data %>% 
  ggplot(aes(x=Wine_PerCapita)) + 
  geom_density(fill = "red") + 
  geom_vline(aes(xintercept=mean(Wine_PerCapita)), color="blue", linetype="dashed", size=2) +
  theme(legend.position ='none',axis.title.y = element_text(size= 10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  ggtitle('Density Plot of the Wine Per Capita')


#Wine per capita by Region

data %>%
  group_by(Region) %>%
  summarise(avg_Wine_PerCapita = mean(Wine_PerCapita)) %>%
  ggplot(aes(x=reorder(factor(Region), avg_Wine_PerCapita), y=avg_Wine_PerCapita, fill = avg_Wine_PerCapita)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Region") + 
  ylab("Mean Wine Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10), 
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_Wine_PerCapita), hjust = -0.1, size = 3) +
  ggtitle("Wine Per Capita by Region") + 
  coord_flip()

#Top 25 Countries by Wine per capita

data %>%
  group_by(Country) %>%
  summarise(avg_Wine_PerCapita = mean(Wine_PerCapita)) %>%
  top_n(25) %>%
  ggplot(aes(x=reorder(factor(Country), avg_Wine_PerCapita), y=avg_Wine_PerCapita, fill = avg_Wine_PerCapita)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Country") + 
  ylab("Mean Wine Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_Wine_PerCapita), hjust = -0.3, size = 3) +
  ggtitle("Top 25 Countries by Wine Per Capita") + 
  coord_flip()

#Bottom 25 Countries By Wine pwe capita

data %>%
  group_by(Country) %>%
  summarise(avg_Wine_PerCapita = mean(Wine_PerCapita)) %>%
  top_n(-25) %>%
  ggplot(aes(x=reorder(factor(Country), avg_Wine_PerCapita), y=avg_Wine_PerCapita, fill = avg_Wine_PerCapita)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Country") + 
  ylab("Mean Wine Per Capita") +
  theme(legend.position ='none',axis.title.y = element_text(size=10), axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size=10),axis.text.x = element_text(size = 10),
        plot.title = element_text(size=10, hjust = 0.5)) +
  scale_fill_gradientn(name = '',colours = rev(brewer.pal(10,'Spectral'))) +
  geom_text(aes(label = avg_Wine_PerCapita), hjust = -0.3, size = 3) +
  ggtitle("Bottom 25 Countries by Wine Per Capita") + 
  coord_flip()

#Co relation

corrplot.mixed(cor(
  data %>% select_if(is.numeric)), lower = "number", upper = "shade", tl.pos = "lt", diag = "n",
  tl.cex = 1, number.cex = 1, cl.cex = 1)
