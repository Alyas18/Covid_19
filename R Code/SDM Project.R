library(tidyverse)
library(dplyr)
library(ggplot)
covid_data=read.csv("covid-19-data-csv.csv")
png(file ="Scatter Plot.png")
covid_data %>% 
  filter(as.Date(ObservationDate) == as.Date('2020-05-01') &
           as.Date(ObservationDate) <= as.Date('2020-05-30')) %>% 
  select(Country, Confirmed, Deaths, Recovered) %>% 
  group_by(Country) %>% 
  summarise(Deaths = sum(Deaths),Confirmed = sum(Confirmed)) %>% 
  ggplot(aes(x = Confirmed, y = Deaths)) + 
  ggtitle("Scatter for Deaths and Confirmed") +
  geom_point(color="black",size=4,alpha=0.8)
dev.off()

png(file ="Bar Plot Confirmed.png")
covid_data %>% 
  filter(as.Date(ObservationDate) == as.Date('2020-05-01')) %>% 
  select(Country, Confirmed) %>% 
  group_by(Country) %>% 
  summarise(Confirmed = sum(Confirmed)) %>% 
  arrange(desc(Confirmed)) %>% 
  top_n(15) %>% 
  ggplot(aes(y = fct_reorder(Country, Confirmed), x = Confirmed)) +
  ggtitle("Bar Plot for Confirmed state in Different countries") +
  geom_bar(stat = 'identity', fill = 'mediumvioletred',col='gray') +
  labs(y = '')
dev.off()

png(file ="Bar Plot Confirmed2.png")
covid_data %>% 
  filter(as.Date(ObservationDate) == as.Date('2020-05-01')) %>% 
  select(Country, Confirmed) %>% 
  group_by(Country) %>% 
  summarise(Confirmed = sum(Confirmed)) %>% 
  top_n(9) %>%  
  ggplot(aes(x = Country, y = Confirmed)) +
  ggtitle("Bar Plot for Confirmed state in Different countries") +
  geom_bar(stat = 'identity', fill = 'midnightblue')
dev.off()
  
png(file ="Bar Plot Confirmed2.1.png")
covid_data %>% 
  filter(as.Date(ObservationDate) == as.Date('2020-03-01') &
           as.Date(ObservationDate) <= as.Date('2020-12-30')) %>% 
  select(Country, Confirmed) %>% 
  group_by(Country) %>% 
  summarise(Confirmed = sum(Confirmed)) %>% 
  top_n(5) %>%  
  ggplot(aes(x = Country, y = Confirmed)) +
  ggtitle("Bar Plot for Confirmed state in Different countries") +
  geom_bar(stat = 'identity', fill = 'midnightblue')
dev.off()

png(file ="Bar Plot Deaths.png")
covid_data %>% 
  filter(as.Date(ObservationDate) == as.Date('2020-05-01')) %>% 
  select(Country, Deaths) %>% 
  group_by(Country) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  arrange(desc(Deaths)) %>% 
  top_n(15) %>% 
  ggplot(aes(y = fct_reorder(Country, Deaths), x = Deaths)) +
  ggtitle("Bar Plot for Deaths state in Different countries") +
  geom_bar(stat = 'identity', fill = 'mediumvioletred',col='gray') +
  labs(y = '')
dev.off()

png(file ="Bar Plot Deaths2.png")
covid_data %>% 
  filter(as.Date(ObservationDate) == as.Date('2020-05-01')) %>% 
  select(Country, Deaths) %>% 
  group_by(Country) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  top_n(9) %>%  
  ggplot(aes(x = Country, y = Deaths)) +
  ggtitle("Bar Plot for Deaths state in Different countries") +
  geom_bar(stat = 'identity', fill = 'midnightblue')
dev.off()

png(file ="Bar Plot Deaths2.1.png")
covid_data %>% 
  filter(as.Date(ObservationDate) == as.Date('2020-03-01') &
           as.Date(ObservationDate) <= as.Date('2020-12-30')) %>% 
  select(Country, Deaths) %>% 
  group_by(Country) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  top_n(5) %>%  
  ggplot(aes(x = Country, y = Deaths)) +
  ggtitle("Bar Plot for Deaths state in Different countries") +
  geom_bar(stat = 'identity', fill = 'mediumvioletred')
dev.off()

png(file ="Bar Plot Recovered.png")
covid_data %>% 
  filter(as.Date(ObservationDate) == as.Date('2020-05-01')) %>% 
  select(Country, Recovered) %>% 
  group_by(Country) %>% 
  summarise(Recovered = sum(Recovered)) %>% 
  arrange(desc(Recovered)) %>% 
  top_n(15) %>% 
  ggplot(aes(y = fct_reorder(Country, Recovered), x = Recovered)) +
  ggtitle("Bar Plot for Recovered state in Different countries") +
  geom_bar(stat = 'identity', fill = 'mediumvioletred',col='gray') +
  labs(y = '')
dev.off()

png(file ="Bar Plot Recovered2.png")
covid_data %>% 
  filter(as.Date(ObservationDate) == as.Date('2020-05-01')) %>% 
  select(Country, Recovered) %>% 
  group_by(Country) %>% 
  summarise(Recovered = sum(Recovered)) %>% 
  top_n(9) %>%  
  ggplot(aes(x = Country, y = Recovered)) +
  ggtitle("Bar Plot for Recovered state in Different countries") +
  geom_bar(stat = 'identity', fill = 'midnightblue')
dev.off()


png(file ="Line Plot Egypt.png")
covid_data %>% 
  filter(Country == 'Egypt') %>% 
  group_by(ObservationDate) %>% 
  summarise(Recovered = sum(Recovered)) %>% 
  ggplot(aes(x = as.Date(ObservationDate), y = Recovered)) +
  ggtitle("Line Plot for Recovered state in Egypt") +
  geom_line(color = 'red') + xlab("Date") + ylab("Recovered") 
dev.off()


png(file ="Line Plot.png")
covid_data %>% 
  filter(Country %in% c("Us","Italy","Spain","France")) %>% 
  group_by(Country, ObservationDate) %>% 
  summarise(Confirmed = sum(Confirmed)) %>% 
  ggplot(aes(x =as.Date(ObservationDate), y = Confirmed)) +
  geom_line(aes(color = Country)) +
  xlab("Date") + ylab("Confirmed") +
  ggtitle("Line Plot for Confirmed in Different countries") +
  scale_color_manual(values = c('navy','green', 'purple', 'red'))
dev.off()

png(file ="Line Plot Deaths.png")
covid_data %>% 
  filter(Country %in% c("Us","Italy","Spain","France")) %>% 
  group_by(Country, ObservationDate) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ggplot(aes(x =as.Date(ObservationDate), y = Deaths)) +
  geom_line(aes(color = Country)) +
  xlab("Date") + ylab("Deaths") +
  ggtitle("Line Plot for Deaths in Different countries") +
  scale_color_manual(values = c('navy','green', 'purple', 'red'))
dev.off()

t.test (covid_data$Deaths, covid_data$Recovered)


labels <- c("Us","Italy","Spain","France")
png(file ="pie Plot de.png")
covid_data %>% 
  filter(Country %in% c("Us","Italy","Spain","France")) %>% 
  group_by(Country) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  mutate(mlabel = paste0(round(Deaths / sum(Deaths) * 100, 1), "%"))%>% 
  arrange(desc(Deaths)) %>% 
  ggplot(aes(x="", y=Deaths,fill=Country)) + geom_bar(width = 1, stat = "identity", color = "black") +
  ggtitle("Pie Plot for Deaths in Different countries") +
    coord_polar("y", start=0) + geom_text(aes(x=1,y = cumsum(Deaths) - Deaths/2, label = mlabel), color = "black")  + theme_void()
dev.off()




png(file ="pie Plot co.png")
covid_data %>% 
  filter(Country %in% c("Us","Italy","Spain","France")) %>% 
  group_by(Country) %>% 
  summarise(Confirmed = sum(Confirmed)) %>% 
  mutate(mlabel = paste0(round(Confirmed / sum(Confirmed) * 100, 1), "%"))%>% 
  arrange(desc(Confirmed)) %>% 
  ggplot(aes(x="", y=Confirmed,fill=Country)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  ggtitle("Pie Plot for Confirmed in Different countries") +
  coord_polar("y", start=0) + 
  geom_text(aes(x=1,y = cumsum(Confirmed) - Confirmed/2, label = mlabel), color = "black") +
  theme_void()
dev.off()

