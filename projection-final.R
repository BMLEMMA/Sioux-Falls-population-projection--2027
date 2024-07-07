library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
getwd()
setwd("C:/Users/blenl/OneDrive/Desktop/work/beacom reserch fellow/projection")
getwd()

#main functions:
NAs <- function(my_data) {
  my_data[my_data == "N"] <- NA
  return(my_data)
}
spaces<- function(my_data) {
  my_data<- my_data[my_data==""]<- NA
}

Xs<- function(my_data) {
  my_data<- my_data[my_data=="(X)"]<- NA
}

stars<- function(my_data) {
  my_data<- my_data[my_data=="***"]<- NA
}

commas_estimate<- function (my_data) {
  my_data<- gsub(",","",my_data$Estimate)
}


##demographics data:
setwd("C:/Users/blenl/OneDrive/Desktop/work/beacom reserch fellow/projection/demographic")
demographics_2022<- read.csv("demographic 2022.csv")
demographics_2021<- read.csv("demographic 2021.csv")
demographics_2019<- read.csv("demographic 2019.csv")
demographics_2018<- read.csv("demographic 2018.csv")
demographics_2017<- read.csv("demographic 2017.csv")
demographics_2016<- read.csv("demographic 2016.csv")
demographics_2015<- read.csv("demographic 2015.csv")
demographics_2014<- read.csv("demographic 2014.csv")
demographics_2013<- read.csv("demographic 2013.csv")
demographics_2012<- read.csv("demographic 2012.csv")
demographics_2011<- read.csv("demographic 2011.csv")
demographics_2010<- read.csv("demographic 2010.csv")

demographics_age_2022<- demographics_2022[6:18,]
demographics_age_2022_estimate<- commas_estimate(demographics_age_2022)
demographics_age_2022_estimate<- as.numeric(demographics_age_2022_estimate)

demographics_age_2021<- demographics_2021[6:18,]
demographics_age_2021_estimate<- commas_estimate(demographics_age_2021)
demographics_age_2021_estimate<- as.numeric(demographics_age_2021_estimate)

demographics_age_2019<- demographics_2019[6:18,]
demographics_age_2019_estimate<- commas_estimate(demographics_age_2019)
demographics_age_2019_estimate<- as.numeric(demographics_age_2019_estimate)

demographics_age_2018<- demographics_2018[6:18,]
demographics_age_2018_estimate<- commas_estimate(demographics_age_2018)
demographics_age_2018_estimate<- as.numeric(demographics_age_2018_estimate)

demographics_age_2017<- demographics_2017[6:18,]
demographics_age_2017_estimate<- commas_estimate(demographics_age_2017)
demographics_age_2017_estimate<- as.numeric(demographics_age_2017_estimate)

demographics_age_2016<- demographics_2016[6:18,]
demographics_age_2016_estimate<- commas_estimate(demographics_age_2016)
demographics_age_2016_estimate<- as.numeric(demographics_age_2016_estimate)

demographics_age_2015<- demographics_2015[6:18,]
demographics_age_2015_estimate<- commas_estimate(demographics_age_2015)
demographics_age_2015_estimate<- as.numeric(demographics_age_2015_estimate)

demographics_age_2014<- demographics_2014[6:18,]
demographics_age_2014_estimate<- commas_estimate(demographics_age_2014)
demographics_age_2014_estimate<- as.numeric(demographics_age_2014_estimate)

demographics_age_2013<- demographics_2013[6:18,]
demographics_age_2013_estimate<- commas_estimate(demographics_age_2013)
demographics_age_2013_estimate<- as.numeric(demographics_age_2013_estimate)

demographics_age_2012<- demographics_2012[6:18,]
demographics_age_2012_estimate<- commas_estimate(demographics_age_2012)
demographics_age_2012_estimate<- as.numeric(demographics_age_2012_estimate)

demographics_age_2011<- demographics_2011[6:18,]
demographics_age_2011_estimate<- commas_estimate(demographics_age_2011)
demographics_age_2011_estimate<- as.numeric(demographics_age_2011_estimate)

demographics_age_2010<- demographics_2010[6:18,]
demographics_age_2010_estimate<- commas_estimate(demographics_age_2010)
demographics_age_2010_estimate<- as.numeric(demographics_age_2010_estimate)
#DONE CLEANING

##visualizing:
order_labels<- c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", "20 to 24 years", 
                 "25 to 34 years","35 to 44 years", "45 to 54 years", "55 to 59 years", "60 to 64 years",
                 "65 to 74 years", "75 to 84 years", "85 years and over")
demographics_age_df_2022<- data.frame(Label=order_labels, Estimate=demographics_age_2022_estimate)
demographics_age_df_2021<- data.frame(Label=order_labels, Estimate=demographics_age_2021_estimate)
demographics_age_df_2019<- data.frame(Label=order_labels, Estimate=demographics_age_2019_estimate)
demographics_age_df_2018<- data.frame(Label=order_labels, Estimate=demographics_age_2018_estimate)
demographics_age_df_2017<- data.frame(Label=order_labels, Estimate=demographics_age_2017_estimate)
demographics_age_df_2016<- data.frame(Label=order_labels, Estimate=demographics_age_2016_estimate)
demographics_age_df_2015<- data.frame(Label=order_labels, Estimate=demographics_age_2015_estimate)
demographics_age_df_2014<- data.frame(Label=order_labels, Estimate=demographics_age_2014_estimate)
demographics_age_df_2013<- data.frame(Label=order_labels, Estimate=demographics_age_2013_estimate)
demographics_age_df_2012<- data.frame(Label=order_labels, Estimate=demographics_age_2012_estimate)
demographics_age_df_2011<- data.frame(Label=order_labels, Estimate=demographics_age_2011_estimate)
demographics_age_df_2010<- data.frame(Label=order_labels, Estimate=demographics_age_2010_estimate)
##########
demographics_age_graph <- function(my_data, year) {
  ggplot(my_data, aes(x = order_labels, y = Estimate)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = paste("Demographics by Age Group in", year), x = "Age Groups", y = "Total Population") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
demographics_age_graph(demographics_age_df_2022, 2022)
demographics_age_graph(demographics_age_df_2021, 2021)
demographics_age_graph(demographics_age_df_2019, 2019)
demographics_age_graph(demographics_age_df_2018, 2018)
demographics_age_graph(demographics_age_df_2017, 2017)
demographics_age_graph(demographics_age_df_2016, 2016)
demographics_age_graph(demographics_age_df_2015, 2015)
demographics_age_graph(demographics_age_df_2014, 2014)
demographics_age_graph(demographics_age_df_2013, 2013)
demographics_age_graph(demographics_age_df_2012, 2012)
demographics_age_graph(demographics_age_df_2011, 2011)
demographics_age_graph(demographics_age_df_2010, 2010)

demographics_age_df<- data.frame(years=c(2010:2019, 2022:2023), 
                                 Estimate= c(demographics_age_df_2010[1,2], 
                                             demographics_age_df_2011[1,2],
                                             demographics_age_df_2012[1,2],
                                             demographics_age_df_2013[1,2],
                                             demographics_age_df_2014[1,2],
                                             demographics_age_df_2015[1,2],
                                             demographics_age_df_2016[1,2],
                                             demographics_age_df_2017[1,2],
                                             demographics_age_df_2018[1,2],
                                             demographics_age_df_2019[1,2],
                                             demographics_age_df_2021[1,2], 
                                             demographics_age_df_2022[1,2]))

ggplot(demographics_age_df, aes(x = factor(years), y = Estimate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Population of children under five in Sioux Falls", x = "Year", y = "Estimate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(demographics_age_df$years, demographics_age_df$Estimate,type="o", xlab="Years", 
     ylab="Total estimate", main="Graph of the number of children aged 1 to 4 in the Sioux Falls MSA")
#DONE CLEANING
#NO FURTHER VISUALIZATION NEEDED. MIGRATION
#FIRST ONLY TAKE THE 0-5 AGES BECAUSE THOSE ARE THE ONES WE ARE INTERSTED IN BUT THERE'S NOT MUCH DIFFERENCE
#ACROSS THE YEARS.
#ALSO USE THE REST AFTERWARDS TO INCREASE ACCURACY EVEN THOUGH THERE'S NOT MUCH DIFFERENCE ACROSS THE YEARS. 

#demographics sex data
demographics_sex_2022<- demographics_2022[3:4,]
demographics_sex_2022_estimate<- commas_estimate(demographics_sex_2022)
demographics_sex_2022_estimate<- as.numeric(demographics_sex_2022_estimate)

demographics_sex_2021<- demographics_2021[3:4,]
demographics_sex_2021_estimate<- commas_estimate(demographics_sex_2021)
demographics_sex_2021_estimate<- as.numeric(demographics_sex_2021_estimate)

demographics_sex_2019<- demographics_2019[3:4,]
demographics_sex_2019_estimate<- commas_estimate(demographics_sex_2019)
demographics_sex_2019_estimate<- as.numeric(demographics_sex_2019_estimate)

demographics_sex_2018<- demographics_2018[3:4,]
demographics_sex_2018_estimate<- commas_estimate(demographics_sex_2018)
demographics_sex_2018_estimate<- as.numeric(demographics_sex_2018_estimate)

demographics_sex_2017<- demographics_2017[3:4,]
demographics_sex_2017_estimate<- commas_estimate(demographics_sex_2017)
demographics_sex_2017_estimate<- as.numeric(demographics_sex_2017_estimate)

demographics_sex_2016<- demographics_2016[3:4,]
demographics_sex_2016_estimate<- commas_estimate(demographics_sex_2016)
demographics_sex_2016_estimate<- as.numeric(demographics_sex_2016_estimate)

demographics_sex_2015<- demographics_2015[3:4,]
demographics_sex_2015_estimate<- commas_estimate(demographics_sex_2015)
demographics_sex_2015_estimate<- as.numeric(demographics_sex_2015_estimate)

demographics_sex_2014<- demographics_2014[3:4,]
demographics_sex_2014_estimate<- commas_estimate(demographics_sex_2014)
demographics_sex_2014_estimate<- as.numeric(demographics_sex_2014_estimate)

demographics_sex_2013<- demographics_2013[3:4,]
demographics_sex_2013_estimate<- commas_estimate(demographics_sex_2013)
demographics_sex_2013_estimate<- as.numeric(demographics_sex_2013_estimate)

demographics_sex_2012<- demographics_2012[3:4,]
demographics_sex_2012_estimate<- commas_estimate(demographics_sex_2012)
demographics_sex_2012_estimate<- as.numeric(demographics_sex_2012_estimate)

demographics_sex_2011<- demographics_2011[3:4,]
demographics_sex_2011_estimate<- commas_estimate(demographics_sex_2011)
demographics_sex_2011_estimate<- as.numeric(demographics_sex_2011_estimate)

demographics_sex_2010<- demographics_2010[3:4,]
demographics_sex_2010_estimate<- commas_estimate(demographics_sex_2010)
demographics_sex_2010_estimate<- as.numeric(demographics_sex_2010_estimate)
#DONE CLEANING

#visualizing:
demographics_sex_df<- data.frame(Year=rep(c(2010:2019, 2021:2022), each=2),
                                 Sex=rep(c("Male", "Female"), times=12),
                                 Estimate=c(demographics_sex_2010_estimate[1],
                                            demographics_sex_2010_estimate[2],
                                            demographics_sex_2011_estimate[1],
                                            demographics_sex_2011_estimate[2],
                                            demographics_sex_2012_estimate[1],
                                            demographics_sex_2012_estimate[2],
                                            demographics_sex_2013_estimate[1],
                                            demographics_sex_2013_estimate[2],
                                            demographics_sex_2014_estimate[1],
                                            demographics_sex_2014_estimate[2],
                                            demographics_sex_2015_estimate[1],
                                            demographics_sex_2015_estimate[2],
                                            demographics_sex_2016_estimate[1],
                                            demographics_sex_2016_estimate[2],
                                            demographics_sex_2017_estimate[1],
                                            demographics_sex_2017_estimate[2],
                                            demographics_sex_2018_estimate[1],
                                            demographics_sex_2018_estimate[2],
                                            demographics_sex_2019_estimate[1],
                                            demographics_sex_2019_estimate[2],
                                            demographics_sex_2021_estimate[1],
                                            demographics_sex_2021_estimate[2],
                                            demographics_sex_2022_estimate[1],
                                            demographics_sex_2022_estimate[2]))
plot_population_bars <- function(data) {
  ggplot(data, aes(x = as.factor(Year), y = Estimate, fill = Sex)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = "Population Estimates by Sex Over Years",
         x = "Year",
         y = "Population Estimate") +
    scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
    theme_minimal() +
    theme(legend.position = "bottom")
}
plot_population_bars(demographics_sex_df)
#INCREASES LINEARLY (ALTHOUGH VERY LITTLE)
#SHOULD BE CONSIDERED A VARIABLE. 

#demographics of people with one race:
demographics_onerace_2022<- demographics_2022[39:59,]
demographics_onerace_2022<- NAs(demographics_onerace_2022)
demographics_onerace_2022_estimate<- commas_estimate(demographics_onerace_2022)
demographics_onerace_2022_estimate<- as.numeric(demographics_onerace_2022_estimate)
demographics_onerace_2022 <- data.frame(Label = demographics_onerace_2022$Label, Estimate = demographics_onerace_2022_estimate)
demographics_onerace_2022 <- demographics_onerace_2022 %>% filter(!is.na(Estimate))
demographics_onerace_2022 <- demographics_onerace_2022 %>%
  slice(-c(4, 5, 6, 7,9,10,11,12,13,14,15))

demographics_onerace_2021<- demographics_2021[39:59,]
demographics_onerace_2021<- NAs(demographics_onerace_2021)
demographics_onerace_2021_estimate<- commas_estimate(demographics_onerace_2021)
demographics_onerace_2021_estimate<- as.numeric(demographics_onerace_2021_estimate)
demographics_onerace_2021 <- data.frame(Label = demographics_onerace_2021$Label, Estimate = demographics_onerace_2021_estimate)
demographics_onerace_2021 <- demographics_onerace_2021 %>% filter(!is.na(Estimate))

demographics_onerace_2019<- demographics_2019[39:59,]
demographics_onerace_2019<- NAs(demographics_onerace_2019)
demographics_onerace_2019_estimate<- commas_estimate(demographics_onerace_2019)
demographics_onerace_2019_estimate<- as.numeric(demographics_onerace_2019_estimate)
demographics_onerace_2019 <- data.frame(Label = demographics_onerace_2019$Label, Estimate = demographics_onerace_2019_estimate)
demographics_onerace_2019 <- demographics_onerace_2019 %>% filter(!is.na(Estimate))
demographics_onerace_2019 <- demographics_onerace_2019 %>%
  slice(-c(5:11))

demographics_onerace_2018<- demographics_2018[39:59,]
demographics_onerace_2018<- NAs(demographics_onerace_2018)
demographics_onerace_2018_estimate<- commas_estimate(demographics_onerace_2018)
demographics_onerace_2018_estimate<- as.numeric(demographics_onerace_2018_estimate)
demographics_onerace_2018 <- data.frame(Label = demographics_onerace_2018$Label, Estimate = demographics_onerace_2018_estimate)
demographics_onerace_2018 <- demographics_onerace_2018 %>% filter(!is.na(Estimate))
demographics_onerace_2018 <- demographics_onerace_2018 %>%
  slice(-c(5,6,7,8,9,10,11))

demographics_onerace_2017<- demographics_2017[39:59,]
demographics_onerace_2017<- NAs(demographics_onerace_2017)
demographics_onerace_2017_estimate<- commas_estimate(demographics_onerace_2017)
demographics_onerace_2017_estimate<- as.numeric(demographics_onerace_2017_estimate)
demographics_onerace_2017 <- data.frame(Label = demographics_onerace_2017$Label, Estimate = demographics_onerace_2017_estimate)
demographics_onerace_2017 <- demographics_onerace_2017 %>% filter(!is.na(Estimate))
demographics_onerace_2017 <- demographics_onerace_2017 %>%
  slice(-c(4, 5, 6, 7))

demographics_onerace_2016<- demographics_2016[34:54,]
demographics_onerace_2016 <- demographics_onerace_2016 %>%
  slice(-c(4, 5, 6, 7,9,10,11,12,13,14,15, 17,18,19,20))
demographics_onerace_2016<- NAs(demographics_onerace_2016)
demographics_onerace_2016_estimate<- commas_estimate(demographics_onerace_2016)
demographics_onerace_2016_estimate<- as.numeric(demographics_onerace_2016_estimate)
demographics_onerace_2016 <- data.frame(Label = demographics_onerace_2016$Label, Estimate = demographics_onerace_2016_estimate)
demographics_onerace_2016 <- demographics_onerace_2016 %>% filter(!is.na(Estimate))

demographics_onerace_2015<- demographics_2015[34:54,]
demographics_onerace_2015 <- demographics_onerace_2015 %>%
  slice(-c(4, 5, 6, 7,9,10,11,12,13,14,15, 17,18,19,20))
demographics_onerace_2015<- NAs(demographics_onerace_2015)
demographics_onerace_2015_estimate<- commas_estimate(demographics_onerace_2015)
demographics_onerace_2015_estimate<- as.numeric(demographics_onerace_2015_estimate)
demographics_onerace_2015 <- data.frame(Label = demographics_onerace_2015$Label, Estimate = demographics_onerace_2015_estimate)
demographics_onerace_2015 <- demographics_onerace_2015 %>% filter(!is.na(Estimate))

demographics_onerace_2014<- demographics_2014[34:54,]
demographics_onerace_2014 <- demographics_onerace_2014 %>%
  slice(-c(4, 5, 6, 7,9,10,11,12,13,14,15, 17,18,19,20))
demographics_onerace_2014<- NAs(demographics_onerace_2014)
demographics_onerace_2014_estimate<- commas_estimate(demographics_onerace_2014)
demographics_onerace_2014_estimate<- as.numeric(demographics_onerace_2014_estimate)
demographics_onerace_2014 <- data.frame(Label = demographics_onerace_2014$Label, Estimate = demographics_onerace_2014_estimate)
demographics_onerace_2014 <- demographics_onerace_2014 %>% filter(!is.na(Estimate))


demographics_onerace_2013<- demographics_2013[34:54,]
demographics_onerace_2013 <- demographics_onerace_2013 %>%
  slice(-c(4, 5, 6, 7,9,10,11,12,13,14,15, 17,18,19,20))
demographics_onerace_2013<- NAs(demographics_onerace_2013)
demographics_onerace_2013_estimate<- commas_estimate(demographics_onerace_2013)
demographics_onerace_2013_estimate<- as.numeric(demographics_onerace_2013_estimate)
demographics_onerace_2013 <- data.frame(Label = demographics_onerace_2013$Label, Estimate = demographics_onerace_2013_estimate)
demographics_onerace_2013 <- demographics_onerace_2013 %>% filter(!is.na(Estimate))


demographics_onerace_2012<- demographics_2012[34:54,]
demographics_onerace_2012 <- demographics_onerace_2012 %>%
  slice(-c(4, 5, 6, 7,9,10,11,12,13,14,15, 17,18,19,20))
demographics_onerace_2012<- NAs(demographics_onerace_2012)
demographics_onerace_2012_estimate<- commas_estimate(demographics_onerace_2012)
demographics_onerace_2012_estimate<- as.numeric(demographics_onerace_2012_estimate)
demographics_onerace_2012 <- data.frame(Label = demographics_onerace_2012$Label, Estimate = demographics_onerace_2012_estimate)
demographics_onerace_2012 <- demographics_onerace_2012 %>% filter(!is.na(Estimate))


demographics_onerace_2011<- demographics_2011[34:54,]
demographics_onerace_2011 <- demographics_onerace_2011 %>%
  slice(-c(4, 5, 6, 7,9,10,11,12,13,14,15, 17,18,19,20))
demographics_onerace_2011<- NAs(demographics_onerace_2011)
demographics_onerace_2011_estimate<- commas_estimate(demographics_onerace_2011)
demographics_onerace_2011_estimate<- as.numeric(demographics_onerace_2011_estimate)
demographics_onerace_2011 <- data.frame(Label = demographics_onerace_2011$Label, Estimate = demographics_onerace_2011_estimate)
demographics_onerace_2011 <- demographics_onerace_2011 %>% filter(!is.na(Estimate))


demographics_onerace_2010<- demographics_2010[34:54,]
demographics_onerace_2010 <- demographics_onerace_2010 %>%
  slice(-c(4, 5, 6, 7,9,10,11,12,13,14,15, 17,18,19,20))
demographics_onerace_2010<- NAs(demographics_onerace_2010)
demographics_onerace_2010_estimate<- commas_estimate(demographics_onerace_2010)
demographics_onerace_2010_estimate<- as.numeric(demographics_onerace_2010_estimate)
demographics_onerace_2010 <- data.frame(Label = demographics_onerace_2010$Label, Estimate = demographics_onerace_2010_estimate)
demographics_onerace_2010 <- demographics_onerace_2010 %>% filter(!is.na(Estimate))
#DONE CLEANING

#visualizing:
demographics_onerace_graph<- function(my_data, year) {
  ggplot(my_data, aes(x =Estimate, y = Label)) +
    geom_point(size = 3, color = "blue") +
    labs(title = paste("Dot Plot of Demographics by Age Group in", year), x = "Total Population", y = "Age Group") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  
}
demographics_onerace_graph(demographics_onerace_2022, 2022)
demographics_onerace_graph(demographics_onerace_2021, 2021)
demographics_onerace_graph(demographics_onerace_2019, 2019)
demographics_onerace_graph(demographics_onerace_2018, 2018)
demographics_onerace_graph(demographics_onerace_2017, 2017)
demographics_onerace_graph(demographics_onerace_2016, 2016)
demographics_onerace_graph(demographics_onerace_2015, 2015)
demographics_onerace_graph(demographics_onerace_2014, 2014)
demographics_onerace_graph(demographics_onerace_2013, 2013)
demographics_onerace_graph(demographics_onerace_2012, 2012)
demographics_onerace_graph(demographics_onerace_2011, 2011)
demographics_onerace_graph(demographics_onerace_2010, 2010)
#VERY LITTLE CHANGE ACROSS THE years


#how do races change over time in the Sioux Falls MSA?
w_list<- list()
extract_w_estimate <- function(df, year) {
  df$Label <- str_trim(df$Label)
  w_estimate <- df %>%
    filter(Label == "White") %>%
    pull(Estimate)
  print(w_estimate)
  w_list<<- append(w_list, w_estimate)
}
extract_w_estimate(demographics_onerace_2010, 2010)
extract_w_estimate(demographics_onerace_2011, 2011)
extract_w_estimate(demographics_onerace_2012, 2012)
extract_w_estimate(demographics_onerace_2013, 2013)
extract_w_estimate(demographics_onerace_2014, 2014)
extract_w_estimate(demographics_onerace_2015, 2015)
extract_w_estimate(demographics_onerace_2016, 2016)
extract_w_estimate(demographics_onerace_2017, 2017)
extract_w_estimate(demographics_onerace_2018, 2018)
extract_w_estimate(demographics_onerace_2019, 2019)
extract_w_estimate(demographics_onerace_2021, 2021)
extract_w_estimate(demographics_onerace_2022, 2022)

w_list<- sapply(w_list, unlist)
years<- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022)
w_df<- data.frame(w_list, years)


ggplot(w_df, aes(x = years, y = wh_list)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 3) +
  labs(
    title = "Population Estimates Over Time",
    x = "Year",
    y = "Population Estimate"
  ) +
  theme_minimal()

##################################################################

b_list<- list()
extract_b_estimate <- function(df, year) {
  df$Label <- str_trim(df$Label)
  b_estimate <- df %>%
    filter(Label == "Black or African American") %>%
    pull(Estimate)
  print(b_estimate)
  b_list<<- append(b_list, b_estimate)
}


extract_b_estimate(demographics_onerace_2010, 2010)
extract_b_estimate(demographics_onerace_2011, 2011)
extract_b_estimate(demographics_onerace_2012, 2012)
extract_b_estimate(demographics_onerace_2013, 2013)
extract_b_estimate(demographics_onerace_2014, 2014)
extract_b_estimate(demographics_onerace_2015, 2015)
extract_b_estimate(demographics_onerace_2016, 2016)
extract_b_estimate(demographics_onerace_2017, 2017)
extract_b_estimate(demographics_onerace_2018, 2018)
extract_b_estimate(demographics_onerace_2019, 2019)
extract_b_estimate(demographics_onerace_2021, 2021)
extract_b_estimate(demographics_onerace_2022, 2022)


b_list<- sapply(b_list, unlist)
years<- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022)
b_df<- data.frame(b_list, years)

ggplot(b_df, aes(x = years, y = b_list)) +
  geom_line(color = "blue") +
  geom_point(color = "black", size = 3) +
  labs(
    title = "Black or African Population Estimates Over Time",
    x = "Year",
    y = "Population Estimate"
  ) +
  theme_minimal()
#################################################################


alaskan_list<- list()
extract_alaskan_estimate <- function(df, year) {
  df$Label <- str_trim(df$Label)
  alaskan_estimate <- df %>%
    filter(Label == "American Indian and Alaska Native") %>%
    pull(Estimate)
  print(alaskan_estimate)
  alaskan_list<<- append(alaskan_list, alaskan_estimate)
}


extract_alaskan_estimate(demographics_onerace_2010, 2010)
extract_alaskan_estimate(demographics_onerace_2011, 2011)
extract_alaskan_estimate(demographics_onerace_2012, 2012)
extract_alaskan_estimate(demographics_onerace_2013, 2013)
extract_alaskan_estimate(demographics_onerace_2014, 2014)
extract_alaskan_estimate(demographics_onerace_2015, 2015)
extract_alaskan_estimate(demographics_onerace_2016, 2016)
extract_alaskan_estimate(demographics_onerace_2017, 2017)
extract_alaskan_estimate(demographics_onerace_2018, 2018)
extract_alaskan_estimate(demographics_onerace_2019, 2019)
extract_alaskan_estimate(demographics_onerace_2021, 2021)
extract_alaskan_estimate(demographics_onerace_2022, 2022)


alaskan_list<- sapply(alaskan_list, unlist)
years<- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022)
alaskan_df<- data.frame(alaskan_list, years)

ggplot(alaskan_df, aes(x = years, y = alaskan_list)) +
  geom_line(color = "blue") +
  geom_point(color = "black", size = 3) +
  labs(
    title = "American Indian and Alaska Native Population Estimates Over Time",
    x = "Year",
    y = "Population Estimate"
  ) +
  theme_minimal()

############################

a_list<- list()
extract_a_estimate <- function(df, year) {
  df$Label <- str_trim(df$Label)
  a_estimate <- df %>%
    filter(Label == "Asian") %>%
    pull(Estimate)
  print(a_estimate)
  a_list<<- append(a_list, a_estimate)
}

extract_a_estimate(demographics_onerace_2010, 2010)
extract_a_estimate(demographics_onerace_2011, 2011)
extract_a_estimate(demographics_onerace_2012, 2012)
extract_a_estimate(demographics_onerace_2013, 2013)
extract_a_estimate(demographics_onerace_2014, 2014)
extract_a_estimate(demographics_onerace_2015, 2015)
extract_a_estimate(demographics_onerace_2016, 2016)
extract_a_estimate(demographics_onerace_2017, 2017)
extract_a_estimate(demographics_onerace_2018, 2018)
extract_a_estimate(demographics_onerace_2019, 2019)
extract_a_estimate(demographics_onerace_2021, 2021)
extract_a_estimate(demographics_onerace_2022, 2022)

a_list<- sapply(a_list, unlist)
years<- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022)
a_df<- data.frame(a_list, years)

ggplot(a_df, aes(x = years, y = a_list)) +
  geom_line(color = "blue") +
  geom_point(color = "black", size = 3) +
  labs(
    title = "Asian Population Estimates Over Time",
    x = "Year",
    y = "Population Estimate"
  ) +
  theme_minimal()

########################################

nativehawaiian_list<- list()
extract_nativehawaiian_estimate <- function(df, year) {
  df$Label <- str_trim(df$Label)
  nativehawaiian_estimate <- df %>%
    filter(Label == "Native Hawaiian and Other Pacific Islander") %>%
    pull(Estimate)
  print(nativehawaiian_estimate)
  nativehawaiian_list<<- append(nativehawaiian_list, nativehawaiian_estimate)
}

extract_nativehawaiian_estimate(demographics_onerace_2010, 2010)
extract_nativehawaiian_estimate(demographics_onerace_2011, 2011)
extract_nativehawaiian_estimate(demographics_onerace_2012, 2012)
extract_nativehawaiian_estimate(demographics_onerace_2013, 2013)
extract_nativehawaiian_estimate(demographics_onerace_2014, 2014)
extract_nativehawaiian_estimate(demographics_onerace_2015, 2015)
extract_nativehawaiian_estimate(demographics_onerace_2016, 2016)
extract_nativehawaiian_estimate(demographics_onerace_2017, 2017)
extract_nativehawaiian_estimate(demographics_onerace_2018, 2018)
extract_nativehawaiian_estimate(demographics_onerace_2019, 2019)
extract_nativehawaiian_estimate(demographics_onerace_2021, 2021)
extract_nativehawaiian_estimate(demographics_onerace_2022, 2022)

nativehawaiian_list<- sapply(nativehawaiian_list, unlist)
years<- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022)
nativehawaiian_df<- data.frame(nativehawaiian_list, years)

ggplot(nativehawaiian_df, aes(x = years, y = nativehawaiian_list)) +
  geom_line(color = "blue") +
  geom_point(color = "black", size = 3) +
  labs(
    title = "Native Hawaiian and Other Pacific Islander Population Estimates Over Time",
    x = "Year",
    y = "Population Estimate"
  ) +
  theme_minimal()

##############
other_list<- list()
extract_other_estimate <- function(df, year) {
  df$Label <- str_trim(df$Label)
  other_estimate <- df %>%
    filter(Label == "Some other race") %>%
    pull(Estimate)
  print(other_estimate)
  other_list<<- append(other_list, other_estimate)
}
extract_other_estimate(demographics_onerace_2010, 2010)
extract_other_estimate(demographics_onerace_2011, 2011)
extract_other_estimate(demographics_onerace_2012, 2012)
extract_other_estimate(demographics_onerace_2013, 2013)
extract_other_estimate(demographics_onerace_2014, 2014)
extract_other_estimate(demographics_onerace_2015, 2015)
extract_other_estimate(demographics_onerace_2016, 2016)
extract_other_estimate(demographics_onerace_2017, 2017)
extract_other_estimate(demographics_onerace_2018, 2018)
extract_other_estimate(demographics_onerace_2019, 2019)
extract_other_estimate(demographics_onerace_2021, 2021)
extract_other_estimate(demographics_onerace_2022, 2022)

other_list<-append(other_list,demographics_onerace_2022[6,2])

other_list<- sapply(other_list, unlist)
years<- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022)
other_df<- data.frame(other_list, years)

ggplot(other_df, aes(x = years, y = other_list)) +
  geom_line(color = "blue") +
  geom_point(color = "black", size = 3) +
  labs(
    title = "Other races Population Estimates Over Time",
    x = "Year",
    y = "Population Estimate"
  ) +
  theme_minimal()
#THERE'S NOT MUCH CHANGE IN RACE ACROSS THE YEARS. 
#BUT SINCE IT IS AN IMPORTANT ASPECT OF THE COMMUNITY, IT SHOULD BE TAKEN INTO CONSIDERATION


#####migration data functions:
setwd("C:/Users/blenl/OneDrive/Desktop/work/beacom reserch fellow/projection/migration")
#main migration functions:
migration_deselect <- function(migration_data) {
  migration_data <- migration_data %>%
    select(-c(Moved.within.the.same.county.Estimate, 
              Moved.within.the.same.county.Margin.of.Error,
              Moved.from.different.county.same.state.Estimate,
              Moved.from.different.county.same.state.margin.of.error,
              Total.Estimate, Total.Margin.of.Error))
  
  return(migration_data)
}
migration_percent <- function(migration_data, population_total) {
  migration_data <- migration_data %>%
    mutate(
      Moved.from.different.state.Estimate = (as.numeric(sub("%", "", Moved.from.different.state.Estimate)) / 100) * population_total,
      Moved.from.abroad.Estimate = (as.numeric(sub("%", "", Moved.from.abroad.Estimate)) / 100) * population_total
    )
  
  return(migration_data)
}
migration_add <- function(migration_data) {
  migration_data[, 2] <- as.numeric(gsub(",", "", migration_data[, 2]))  
  migration_data[, 4] <- as.numeric(gsub(",", "", migration_data[, 4]))  
  migration_data$total_sum <- migration_data[, 2] + migration_data[, 4]
  return(migration_data)
}


migration_2022<-read.csv("migration 2022.csv")
migration_2022<- migration_deselect(migration_2022)
migration_2022<- migration_percent(migration_2022, 285081)
migration_2022<- migration_add(migration_2022)
migration_2022<- na.omit(migration_2022)

migration_2021<-read.csv("migration 2021.csv")
migration_2021<- migration_deselect(migration_2021)
migration_2021<- migration_percent(migration_2021, 279113)
migration_2021<- migration_add(migration_2021)
migration_2021<- na.omit(migration_2021)

migration_2019<-read.csv("migration 2019.csv")
migration_2019<- migration_deselect(migration_2019)
migration_2019<- migration_percent(migration_2019, 264718)
migration_2019<- migration_add(migration_2019)
migration_2019<- na.omit(migration_2019)

migration_2018<-read.csv("migration 2018.csv")
migration_2018<- migration_deselect(migration_2018)
migration_2018<- migration_percent(migration_2018, 263241)
migration_2018<- migration_add(migration_2018)
migration_2018<- na.omit(migration_2018)

migration_2017<-read.csv("migration 2017.csv")
migration_2017<- migration_deselect(migration_2017)
migration_2017<- migration_percent(migration_2017, 255294)
migration_2017<- migration_add(migration_2017)
migration_2017<-na.omit(migration_2017)

migration_2016<-read.csv("migration 2016.csv")
migration_2016<- migration_deselect(migration_2016)
migration_2016<- migration_percent(migration_2016, 253156)
migration_2016<- migration_add(migration_2016)
migration_2016<- na.omit(migration_2016)

migration_2015<-read.csv("migration 2015.csv")
migration_2015<- migration_deselect(migration_2015)
migration_2015<- migration_percent(migration_2015, 248983)
migration_2015<- migration_add(migration_2015)
migration_2015<- na.omit(migration_2015)

migration_2014<-read.csv("migration 2014.csv")
migration_2014<- migration_deselect(migration_2014)
migration_2014<- migration_percent(migration_2014, 244648)
migration_2014<- migration_add(migration_2014)
migration_2014<- na.omit(migration_2014)

migration_2013<-read.csv("migration 2013.csv")
migration_2013<- migration_deselect(migration_2013)
migration_2013<- migration_percent(migration_2013, 239916)
migration_2013<- migration_add(migration_2013)
migration_2013<- na.omit(migration_2013)

migration_2012<-read.csv("migration 2012.csv")
migration_2012<- migration_deselect(migration_2012)
migration_2012<- migration_percent(migration_2012, 233871)
migration_2012<- migration_add(migration_2012)
migration_2012<- na.omit(migration_2012)

migration_2011<-read.csv("migration 2011.csv")
migration_2011<- migration_deselect(migration_2011)
migration_2011<- migration_percent(migration_2011, 229969)
migration_2011<- migration_add(migration_2011)
migration_2011<- na.omit(migration_2011)

migration_2010<-read.csv("migration 2010.csv")
migration_2010<- migration_deselect(migration_2010)
migration_2010<- migration_percent(migration_2010, 225924)
migration_2010<- migration_add(migration_2010)
migration_2010<- na.omit(migration_2010)

#migration by age groups:
migration_age_2022<- migration_2022[2:10,]
migration_age_2021<- migration_2021[2:10,]
migration_age_2019<- migration_2019[2:10,]
migration_age_2018<- migration_2018[2:10,]
migration_age_2017<- migration_2017[2:10,]
migration_age_2016<- migration_2016[2:10,]
migration_age_2015<- migration_2015[2:10,]
migration_age_2014<- migration_2014[2:10,]
migration_age_2013<- migration_2013[2:10,]
migration_age_2012<- migration_2012[2:10,]
migration_age_2011<- migration_2011[2:10,]
migration_age_2010<- migration_2010[2:10,]

#CLEANED
#visualizing migration:
migration_age_graphs<- function(my_data,year){
  ggplot(my_data, aes(x = Label, y =total_sum)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = paste("Migration by Age Group in", year), x = "Age Group", y = "Total Migration") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(0,40000)
}

migration_age_graphs(migration_age_2022, 2022)
migration_age_graphs(migration_age_2021, 2021)
migration_age_graphs(migration_age_2019, 2019)
migration_age_graphs(migration_age_2018, 2018)
migration_age_graphs(migration_age_2017, 2017)
migration_age_graphs(migration_age_2016, 2016)
migration_age_graphs(migration_age_2015, 2015)
migration_age_graphs(migration_age_2014, 2014)
migration_age_graphs(migration_age_2013, 2013)
migration_age_graphs(migration_age_2012, 2012)
migration_age_graphs(migration_age_2011, 2011)
migration_age_graphs(migration_age_2010, 2010)

migration_preschool<- data.frame(
  years= c(2010, 2011,2012,2013,2014,2015,2016,2017,2018,2019,2021,2022),
  total_estimate=c(migration_age_2010[1,6],
                   migration_age_2011[1,6],
                   migration_age_2012[1,6],
                   migration_age_2013[1,6],
                   migration_age_2014[1,6],
                   migration_age_2015[1,6],
                   migration_age_2016[1,6],
                   migration_age_2017[1,6],
                   migration_age_2018[1,6],
                   migration_age_2019[1,6],
                   migration_age_2021[1,6],
                   migration_age_2022[1,6])
)
plot(migration_preschool$years, migration_preschool$total_estimate,type="o", xlab="Years", 
     ylab="Total estimate", main="Graph of the number of children aged 1 to 4 who moved to Sioux Falls ")


##migration by sex:
migration_sex_2022<- migration_2022[12:13,]
migration_sex_2021<- migration_2021[12:13,]
migration_sex_2019<- migration_2019[12:13,]
migration_sex_2018<- migration_2018[12:13,]
migration_sex_2017<- migration_2017[12:13,]
migration_sex_2016<- migration_2016[12:13,]
migration_sex_2015<- migration_2015[12:13,]
migration_sex_2014<- migration_2014[12:13,]
migration_sex_2013<- migration_2013[12:13,]
migration_sex_2012<- migration_2012[12:13,]
migration_sex_2011<- migration_2011[12:13,]
migration_sex_2010<- migration_2010[12:13,]
#CLEANED
#visualizing:

#migration by sex
migration_sex_graphs<- function(my_data, year) {
  ggplot(my_data,aes(x = Label, y = total_sum)) +
    geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
    labs(title = paste("Migration by sex of", year), x ="Sex", y = "Total Migration") +
    theme_minimal() + ylim(0,15000)
}
#PROBLEM: SAME THING WITH DEMOGRAPHICS SEX. 
migration_sex_graphs(migration_sex_2022, 2022)
migration_sex_graphs(migration_sex_2021, 2021)
migration_sex_graphs(migration_sex_2019, 2019)
migration_sex_graphs(migration_sex_2018, 2018)
migration_sex_graphs(migration_sex_2017, 2017)
migration_sex_graphs(migration_sex_2016, 2016)
migration_sex_graphs(migration_sex_2015, 2015)
migration_sex_graphs(migration_sex_2014, 2014)
migration_sex_graphs(migration_sex_2013, 2013)
migration_sex_graphs(migration_sex_2012, 2012)
migration_sex_graphs(migration_sex_2011, 2011)
migration_sex_graphs(migration_sex_2010, 2010)


migration_sex_df<- data.frame(Year=rep(c(2010:2019, 2021:2022), each=2),
                                 Sex=rep(c("Male", "Female"), times=12),
                                 Estimate=c(migration_sex_2010[1,6],
                                            migration_sex_2010[2,6],
                                            migration_sex_2011[1,6],
                                            migration_sex_2011[2,6],
                                            migration_sex_2012[1,6],
                                            migration_sex_2012[2,6],
                                            migration_sex_2013[1,6],
                                            migration_sex_2013[2,6],
                                            migration_sex_2014[1,6],
                                            migration_sex_2014[2,6],
                                            migration_sex_2015[1,6],
                                            migration_sex_2015[2,6],
                                            migration_sex_2016[1,6],
                                            migration_sex_2016[2,6],
                                            migration_sex_2017[1,6],
                                            migration_sex_2017[2,6],
                                            migration_sex_2018[1,6],
                                            migration_sex_2018[2,6],
                                            migration_sex_2019[1,6],
                                            migration_sex_2019[2,6],
                                            migration_sex_2021[1,6],
                                            migration_sex_2021[2,6],
                                            migration_sex_2022[1,6],
                                            migration_sex_2022[2,6]))

plot_population_bars(migration_sex_df)

#NOT MUCH DIFFERENCE ACTOSS THE YEARS KEEPNG IN MIND THAT IT;S CHANGING BY THOUSANDS AND THE ENTIRE 
#POPULATION IS AROUND 300,000.
#STILL SOMETHING THAT SHOULD PROBABLY BE INCLUDED TO INCREASE ACCURACY. 


#demographics by people with one race only. 
migration_onerace_graph<- function(my_data, year) {
  ggplot(my_data, aes(x =Estimate, y = Label)) +
    geom_point(size = 3, color = "blue") +
    labs(title = paste("Dot Plot of Migration by race in", year), x = "Total Population", y = "race") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
}
migration_onerace_2022<- migration_2022[18,]
migration_onerace_2021<- migration_2021[18,]
migration_onerace_2019<- migration_2019[18,]
migration_onerace_2018<- migration_2018[18,]
migration_onerace_2017<- migration_2017[18,]
migration_onerace_2016<- migration_2016[18,]
migration_onerace_2015<- migration_2015[18,]
migration_onerace_2014<- migration_2014[18,]
migration_onerace_2013<- migration_2013[18,]
migration_onerace_2012<- migration_2012[18,]
migration_onerace_2011<- migration_2011[18,]
migration_onerace_2010<- migration_2010[18,]

##MIGRATION BY RACE IS PRETTY MESSY AND ONLY HAS LIKE 3 ROWS TO WORK WITH. 
#IT WILL NOT BE USED IN THE FINAL PROJECTION.
#migration with citizenship:

migration_citizenship_2022<- migration_2022[20:23,]
migration_citizenship_2021<- migration_2021[20:23,]
migration_citizenship_2019<- migration_2019[18:21,]
migration_citizenship_2018<- migration_2018[19:22,]
migration_citizenship_2017<- migration_2017[19:22,]
migration_citizenship_2016<- migration_2016[20:23,]
migration_citizenship_2015<- migration_2015[18:21,]
migration_citizenship_2014<- migration_2014[20:23,]
migration_citizenship_2013<- migration_2013[18:21,]
migration_citizenship_2012<- migration_2012[17:20,]
migration_citizenship_2011<- migration_2011[17:20,]
migration_citizenship_2010<- migration_2010[17:20,]
migration_native_df<- data.frame (years=c(2010:2019, 2021:2022),
                                  total=c(migration_citizenship_2010[1,6],
                                          migration_citizenship_2011[1,6],
                                          migration_citizenship_2012[1,6],
                                          migration_citizenship_2013[1,6],
                                          migration_citizenship_2014[1,6],
                                          migration_citizenship_2015[1,6],
                                          migration_citizenship_2016[1,6],
                                          migration_citizenship_2017[1,6],
                                          migration_citizenship_2018[1,6],
                                          migration_citizenship_2019[1,6],
                                          migration_citizenship_2021[1,6],
                                          migration_citizenship_2022[1,6]))
migration_foreign_born_df<- data.frame (years=c(2010:2019, 2021:2022),
                                  total=c(migration_citizenship_2010[2,6],
                                  migration_citizenship_2011[2,6],
                                  migration_citizenship_2012[2,6],
                                  migration_citizenship_2013[2,6],
                                  migration_citizenship_2014[2,6],
                                  migration_citizenship_2015[2,6],
                                  migration_citizenship_2016[2,6],
                                  migration_citizenship_2017[2,6],
                                  migration_citizenship_2018[2,6],
                                  migration_citizenship_2019[2,6],
                                  migration_citizenship_2021[2,6],
                                  migration_citizenship_2022[2,6]))

migration_naturalized_df<- data.frame (years=c(2010:2019, 2021:2022),
                                  total=c(migration_citizenship_2010[3,6],
                                          migration_citizenship_2011[3,6],
                                          migration_citizenship_2012[3,6],
                                          migration_citizenship_2013[3,6],
                                          migration_citizenship_2014[3,6],
                                          migration_citizenship_2015[3,6],
                                          migration_citizenship_2016[3,6],
                                          migration_citizenship_2017[3,6],
                                          migration_citizenship_2018[3,6],
                                          migration_citizenship_2019[3,6],
                                          migration_citizenship_2021[3,6],
                                          migration_citizenship_2022[3,6]))
migration_non_us_df<- data.frame (years=c(2010:2019, 2021:2022),
                                  total=c(migration_citizenship_2010[4,6],
                                          migration_citizenship_2011[4,6],
                                          migration_citizenship_2012[4,6],
                                          migration_citizenship_2013[4,6],
                                          migration_citizenship_2014[4,6],
                                          migration_citizenship_2015[4,6],
                                          migration_citizenship_2016[4,6],
                                          migration_citizenship_2017[4,6],
                                          migration_citizenship_2018[4,6],
                                          migration_citizenship_2019[4,6],
                                          migration_citizenship_2021[4,6],
                                          migration_citizenship_2022[4,6]))
#CLEANED

#visualizing:

#migration by citizenship
migration_citizenship_graph<- function(my_data, year) {
  ggplot(my_data, aes(x = Label, y = total_sum)) +
    geom_bar(stat = "identity", fill = "red", alpha = 0.7) +
    labs(title = paste("Migration by citizenship of", year), x ="Citizenship status", y = "Total Migration") +
    theme_minimal() + ylim(0,50000)
}
migration_citizenship_graph(migration_citizenship_2022, 2022)
migration_citizenship_graph(migration_citizenship_2021, 2021)
migration_citizenship_graph(migration_citizenship_2019, 2019)
migration_citizenship_graph(migration_citizenship_2018, 2018)
migration_citizenship_graph(migration_citizenship_2017, 2017)
migration_citizenship_graph(migration_citizenship_2016, 2016)
migration_citizenship_graph(migration_citizenship_2015, 2015)
migration_citizenship_graph(migration_citizenship_2014, 2014)
migration_citizenship_graph(migration_citizenship_2013, 2013)
migration_citizenship_graph(migration_citizenship_2012, 2012)
migration_citizenship_graph(migration_citizenship_2011, 2011)
migration_citizenship_graph(migration_citizenship_2010, 2010)
#DONE
#MIGRATION BY CITIZENSHIP DOESN'T CHANGE THAT MUCH CONSIDERING IT CHANGES BY THOUSANDS AND THE ENTIRE 
#POPULATION IS AROUND 300,000. BUT IT MIGHT MAKE THE PREDICTION MORE ACCURATE.

#THEREFORE IT SHOULD BE USED IN THE PREDICTION.
migration_marital_2022<- migration_2022[25:28,]
migration_marital_2021<- migration_2021[25:28,]
migration_marital_2019<- migration_2019[23:26,]
migration_marital_2018<- migration_2018[24:27,]
migration_marital_2017<- migration_2017[24:27,]
migration_marital_2016<- migration_2016[25:28,]
migration_marital_2015<- migration_2015[23:26,]
migration_marital_2014<- migration_2014[25:28,]
migration_marital_2013<- migration_2013[23:26,]
migration_marital_2012<- migration_2012[22:25,]
migration_marital_2011<- migration_2011[22:25,]
migration_marital_2010<- migration_2010[22:25,]
#CLEANED

#visualizing:

migration_marital_graph<- function(my_data, year) {
  ggplot(my_data, aes(x = Label, y = total_sum)) +
    geom_bar(stat = "identity", fill = "purple", alpha = 0.7) +
    labs(title = paste("Migration by marital status of", year), x ="Marital status", y = "Total Migration") +
    theme_minimal() + ylim(0,25000)
}
migration_marital_graph(migration_marital_2022, 2022)
migration_marital_graph(migration_marital_2021, 2021)
migration_marital_graph(migration_marital_2019, 2019)
migration_marital_graph(migration_marital_2018, 2018)
migration_marital_graph(migration_marital_2017, 2017)
migration_marital_graph(migration_marital_2016, 2016)
migration_marital_graph(migration_marital_2015, 2015)
migration_marital_graph(migration_marital_2014, 2014)
migration_marital_graph(migration_marital_2013, 2013)
migration_marital_graph(migration_marital_2012, 2012)
migration_marital_graph(migration_marital_2011, 2011)
migration_marital_graph(migration_marital_2010, 2010)
#DONE
#MIGRATION IS ALSO AFFECTED BY MARITAL STATUS SO IT SHOULD BE CONSIDERED A VARIABLE. 

migration_education_2022<- migration_2022[30:34,]
migration_education_2021<- migration_2021[30:34,]
migration_education_2019<- migration_2019[28:32,]
migration_education_2018<- migration_2018[29:33,]
migration_education_2017<- migration_2017[29:33,]
migration_education_2016<- migration_2016[30:34,]
migration_education_2015<- migration_2015[28:32,]
migration_education_2014<- migration_2014[30:34,]
migration_education_2013<- migration_2013[28:32,]
migration_education_2012<- migration_2012[28:31,]
migration_education_2011<- migration_2011[28:31,]
migration_education_2010<- migration_2010[28:31,]
#CLEANED
#visualizing:

#migration by education

migration_education_graph<- function(my_data, year) {
  ggplot(my_data, aes(x = Label, y = total_sum)) +
    geom_bar(stat = "identity", fill = "orange", alpha = 0.7) +
    labs(title = paste("Migration by Education level of", year), x ="Education Level", y = "Total Migration") +
    theme_minimal() + ylim (0,30000)
}
migration_education_graph(migration_education_2022, 2022)
migration_education_graph(migration_education_2021, 2021)
migration_education_graph(migration_education_2019, 2019)
migration_education_graph(migration_education_2018, 2018)
migration_education_graph(migration_education_2017, 2017)
migration_education_graph(migration_education_2016, 2016)
migration_education_graph(migration_education_2015, 2015)
migration_education_graph(migration_education_2014, 2014)
migration_education_graph(migration_education_2013, 2013)
migration_education_graph(migration_education_2012, 2012)
migration_education_graph(migration_education_2011, 2011)
migration_education_graph(migration_education_2010, 2010)
#DONE
#SSAME WITH MARITAL AND CITIZENSHIP STATUS. 
#DOESN'T CHANGE THAT MUCH CONSIDERING THE ENTIRE POPULATION BUT SHOULD BE CONSIDERED A VARIABLE BECAUSE
#IT MIGHT PROVIDE MORE ACCURATE PREDICTIONS. 
#migration by income levels:
migration_income_2022<- migration_2022[36:43,]
migration_income_2021<- migration_2021[36:43,]
migration_income_2019<- migration_2019[34:41,]
migration_income_2018<- migration_2018[35:42,]
migration_income_2017<- migration_2017[35:42,]
migration_income_2016<- migration_2016[36:43,]
migration_income_2015<- migration_2015[34:41,]
migration_income_2014<- migration_2014[36:43,]
migration_income_2013<- migration_2013[34:41,]
migration_income_2012<- migration_2012[33:40,]
migration_income_2011<- migration_2011[33:40,]
migration_income_2010<- migration_2010[33:40,]
#CLEANED
#visualizing:

#migration by income 
migration_income_graph<- function(my_data, year) {
  ggplot(my_data, aes(x = Label, y = total_sum)) +
    geom_bar(stat = "identity", fill = "brown", alpha = 0.7) +
    labs(title = paste("Migration by income of", year), x ="Income", y = "Total Migration") +
    theme_minimal() +ylim(0,30000)
}
migration_income_graph(migration_income_2022, 2022)
migration_income_graph(migration_income_2021, 2021)
migration_income_graph(migration_income_2019, 2019)
migration_income_graph(migration_income_2018, 2018)
migration_income_graph(migration_income_2017, 2017)
migration_income_graph(migration_income_2016, 2016)

migration_income_graph(migration_income_2015, 2015)
migration_income_graph(migration_income_2014, 2014)
migration_income_graph(migration_income_2013, 2013)
migration_income_graph(migration_income_2012, 2012)
migration_income_graph(migration_income_2011, 2011)
migration_income_graph(migration_income_2010, 2010)
#DONE
#MIGHT NOT CHANGE THAT MUCH CONSIDERING THE ENTIRE POPULATION BUT HAS ENOUGH OF AN EFFECT 
#TO BE CONSIDERED A VARIABLE. 


#DIDN'T CLEAN TENURE YET BECAUSE I DON'T THINK ITS THAT IMPORTANT. I'LL COME BACK TO IT IF I NEED IT TO 
#INCREASE ACCURACY
migration_tenure_2022<- migration_2022[62:64,]
migration_tenure_2021<- migration_2021[62:64,]
migration_tenure_2019<- migration_2019[62:64,]
migration_tenure_2018<- migration_2018[62:64,]
migration_tenure_2017<- migration_2017[62:64,]
migration_tenure_2016<- migration_2016[62:64,]
migration_tenure_2015<- migration_2015[62:64,]
migration_tenure_2014<- migration_2014[62:64,]
migration_tenure_2013<- migration_2013[62:64,]
migration_tenure_2012<- migration_2012[62:64,]
migration_tenure_2011<- migration_2011[62:64,]
migration_tenure_2010<- migration_2010[62:64,]
#visualizing:

#migration by tenure
migration_tenure_graph<- function(my_data, year) {
  ggplot(my_data, aes(x = Label, y = total_sum)) +
    geom_bar(stat = "identity", fill = "grey", alpha = 0.7) +
    labs(title = paste("Migration by tenure of", year), x ="Tenure", y = "Total Migration") +
    theme_minimal() + ylim(0,40000)
}
migration_tenure_graph(migration_tenure_2022, 2022)
migration_tenure_graph(migration_tenure_2021, 2021)
migration_tenure_graph(migration_tenure_2019, 2019)
migration_tenure_graph(migration_tenure_2018, 2018)
migration_tenure_graph(migration_tenure_2017, 2017)


migration_tenure_graph(migration_tenure_2016, 2016)
migration_tenure_graph(migration_tenure_2015, 2015)
migration_tenure_graph(migration_tenure_2014, 2014)
migration_tenure_graph(migration_tenure_2013, 2013)
migration_tenure_graph(migration_tenure_2012, 2012)
migration_tenure_graph(migration_tenure_2011, 2011)
migration_tenure_graph(migration_tenure_2010, 2010)
#DONE
#DOESN'T CHANGE MUCH OVER THE YEARS, SHOULD NOT BE CONSIDERED A VARIABLE. 
#ALSO MIGHT NOT BE THAT IMPORTANT. 

setwd("C:/Users/blenl/OneDrive/Desktop/work/beacom reserch fellow/projection/income")
###income data

convert_number_estimate <- function(data) {
  if ("number.estimate" %in% colnames(data)) {
    data <- data %>%
      mutate(number.estimate = as.numeric(gsub(",", "", number.estimate)))
    return(data)
  } else {
    stop("The column 'number.estimate' does not exist in the data frame.")
  }
}

income_2022<- read.csv("income 2022.csv")
income_2022<- convert_number_estimate(income_2022)
income_2022<- NAs(income_2022)
income_2022<- na.omit(income_2022)

income_2021<- read.csv("income 2021.csv")
income_2021<- convert_number_estimate(income_2021)
income_2021<- NAs(income_2021)
income_2021<- na.omit(income_2021)

income_2019<- read.csv("income 2019.csv")
income_2019<- convert_number_estimate(income_2019)
income_2019<- NAs(income_2019)
income_2019<- na.omit(income_2019)

income_2018<- read.csv("income 2018.csv")
income_2018<- convert_number_estimate(income_2018)
income_2018<- NAs(income_2018)
income_2018<- na.omit(income_2018)

income_2017<- read.csv("income 2017.csv")
income_2017<- convert_number_estimate(income_2017)
income_2017<- NAs(income_2017)
income_2017<- na.omit(income_2017)

convert_total_estimate<- function(data) {
  if ("total.estimate" %in% colnames(data)) {
    data <- data %>%
      mutate(total.estimate = as.numeric(gsub(",", "", total.estimate)))
    return(data)
  } else {
    stop("The column 'number.estimate' does not exist in the data frame.")
  }
}


income_percent <- function(my_data,population_total) {
  print("Original data:")
  print(my_data)
  my_data <- my_data %>%
    mutate(
      total.estimate = if_else(
        str_detect(total.estimate, "%"),
        as.numeric(str_replace(total.estimate, "%", "")) / 100 * population_total,
        as.numeric(total.estimate)
      )
    )
  print("Updated data:")
  print(my_data)
  return(my_data)
}

income_2016<- read.csv("income 2016.csv")
income_2016<- income_2016[3:28,]
income_2016<- income_percent(income_2016, 253156)
income_2016<- na.omit(income_2016)

income_2015<- read.csv("income 2015.csv")
income_2015<- income_2015[3:28,]
income_2015<- income_percent(income_2015, 248983)
income_2015<- na.omit(income_2015)

income_2014<- read.csv("income 2014.csv")
income_2014<- income_2014[3:28,]
income_2014<- income_percent(income_2014, 244648)
income_2014<- na.omit(income_2014)

income_2013<- read.csv("income 2013.csv")
income_2013<- income_2013[3:28,]
income_2013<- income_percent(income_2013, 239916)
income_2013<- na.omit(income_2013)

income_2012<- read.csv("income 2012.csv")
income_2012<- income_2012[3:28,]
income_2012<- income_percent(income_2012, 233871)
income_2012<- na.omit(income_2012)

income_2011<- read.csv("income 2011.csv")
income_2011<- income_2011[3:28,]
income_2011<- income_percent(income_2011, 229969)
income_2011<- na.omit(income_2011)

income_2010<- read.csv("income 2010.csv")
income_2010<- income_2010[3:28,]
income_2010<- income_percent(income_2010, 225924)
income_2010<- na.omit(income_2010)

#income by household
income_houshold_2022<- income_2022[2:11,]
income_houshold_2021<- income_2021[2:11,]
income_houshold_2019<- income_2019[2:11,]
income_houshold_2018<- income_2018[2:11,]
income_houshold_2017<- income_2017[2:11,]
income_houshold_2016<- income_2016[1:6,]
income_houshold_2015<- income_2015[1:6,]
income_houshold_2014<- income_2014[1:6,]
income_houshold_2013<- income_2013[1:6,]
income_houshold_2012<- income_2012[1:6,]
income_houshold_2011<- income_2011[1:6,]
income_houshold_2010<- income_2010[1:6,]
#CLEANED
#visualizing:
income_houshold_graph_no<- function(my_data, year){
  ggplot(my_data, aes(x = Label, y = number.estimate)) +
    geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
    labs(title = paste("Household income of", year), x ="Forms of Income", y = "Total income") +
    theme_minimal() 
}
income_houshold_graph_no(income_houshold_2022, 2022)
income_houshold_graph_no(income_houshold_2021, 2021)
income_houshold_graph_no(income_houshold_2019, 2019)
income_houshold_graph_no(income_houshold_2018, 2018)
income_houshold_graph_no(income_houshold_2017, 2017)


income_houshold_graph_tot<- function(my_data, year){
  ggplot(my_data, aes(x = Label, y = total.estimate)) +
    geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
    labs(title = paste("Household income of", year), x ="Forms of Income", y = "Total income") +
    theme_minimal() + ylim(0,100000)
}


income_houshold_graph_tot(income_houshold_2016, 2016)
income_houshold_graph_tot(income_houshold_2015, 2015)
income_houshold_graph_tot(income_houshold_2014, 2014)
income_houshold_graph_tot(income_houshold_2013, 2013)
income_houshold_graph_tot(income_houshold_2012, 2012)
income_houshold_graph_tot(income_houshold_2011, 2011)
income_houshold_graph_tot(income_houshold_2010, 2010)
##WILL NEED FURTHER STASTICS. 
#BUT IT'S SUCH AN IMPORTANT ASPECT THAT IT SHOULD BE CONSIDERED A VARAIBLE. 

##DID NOT CLEAN IT BECAUSE I DIDN'T THINK IT WAS THAT BIG OF A VARIABLE. 
#WILL COME BACK IF I NEED IT TO INCREASE ACCURACY
income_workers_2022<- income_2022[14:20,]
income_workers_2022_estimate<- commas_estimate(income_workers_2022$number.estimate)
income_workers_2021<- income_2021[14:20,]
income_workers_2019<- income_2019[14:20,]
income_workers_2018<- income_2018[14:20,]
income_workers_2017<- income_2017[14:20,]
income_workers_2016<- income_2016[14:20,]
income_workers_2015<- income_2015[14:20,]
income_workers_2014<- income_2014[14:20,]
income_workers_2013<- income_2013[14:20,]
income_workers_2012<- income_2012[14:20,]
income_workers_2011<- income_2011[14:20,]
income_workers_2010<- income_2010[14:20,]

#income by race

convert_number_estimate<- function(data) {
  if ("number.estimate" %in% colnames(data)) {
    data <- data %>%
      mutate(number.estimate = as.numeric(gsub(",", "", number.estimate)))
    return(data)
  } else {
    stop("The column 'number.estimate' does not exist in the data frame.")
  }
}
income_onerace_2022<- income_2022[20:23,]
income_onerace_2022<- convert_number_estimate(income_onerace_2022)
income_onerace_2021<- income_2021[20:23,]
income_onerace_2021<- convert_number_estimate(income_onerace_2021)
income_onerace_2019<- income_2019[20:23,]
income_onerace_2018<- income_2018[20:23,]
income_onerace_2017<- income_2017[20:23,]
income_onerace_2016<- income_2016[13:16,]
income_onerace_2015<- income_2015[13:16,]
income_onerace_2014<- income_2014[13:16,]
income_onerace_2013<- income_2013[13:16,]
income_onerace_2012<- income_2012[13:16,]
income_onerace_2011<- income_2011[13:16,]
income_onerace_2010<- income_2010[13:16,]
#CLEANED
#visualizing:

#income by one race
income_onerace_graph_no<- function(my_data, year){
  ggplot(my_data, aes(x = Label, y = number.estimate)) +
    geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
    labs(title = paste("income based on race of", year), x ="Forms of Income", y = "Total income") +
    theme_minimal()
}

income_onerace_graph_no(income_onerace_2022, 2022)
income_onerace_graph_no(income_onerace_2021, 2021)
income_onerace_graph_no(income_onerace_2019, 2019)
income_onerace_graph_no(income_onerace_2018, 2018)
income_onerace_graph_no(income_onerace_2017, 2017)

income_onerace_graph_tot<- function(my_data, year){
  ggplot(my_data, aes(x = Label, y = total.estimate)) +
    geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
    labs(title = paste("income based on race of", year), x ="Forms of Income", y = "Total income") +
    theme_minimal()  +ylim(0,15000)
}
income_onerace_graph_tot(income_percent(income_onerace_2016, 253156), 2016)
income_onerace_graph_tot(income_percent(income_onerace_2015, 248983), 2015)
income_onerace_graph_tot(income_percent(income_onerace_2014, 244648), 2014)
income_onerace_graph_tot(income_percent(income_onerace_2013, 239966), 2013)
income_onerace_graph_tot(income_percent(income_onerace_2012, 233871), 2012)
income_onerace_graph_tot(income_percent(income_onerace_2011, 229969), 2011)
income_onerace_graph_tot(income_percent(income_onerace_2010, 225924), 2010)
#will need further statistics
#INCOME LEVELS BY RACE DO NOT SHOW CHANGE OVER THE YEARS SO THEY SHOULD NOT BE CONSIDERED A VARIABLE. 

###age and sex data
setwd("C:/Users/blenl/OneDrive/Desktop/work/beacom reserch fellow/projection/age and sex data")
age_sex_2010<- read.csv("age and sex data 2010.csv")
age_sex_2010<- income_percent(age_sex_2010, 225924)
age_sex_2010<- na.omit(age_sex_2010)
age_sex_age_2010<- age_sex_2010[1:18,]

age_sex_2011<- read.csv("age and sex data 2011.csv")
age_sex_2011<- income_percent(age_sex_2011, 229969)
age_sex_2011<- na.omit(age_sex_2011)
age_sex_age_2011<- age_sex_2011[1:18,]

age_sex_2012<- read.csv("age and sex data 2012.csv")
age_sex_2012<- income_percent(age_sex_2012, 233871)
age_sex_2012<- na.omit(age_sex_2012)
age_sex_age_2012<- age_sex_2012[1:18,]

age_sex_2013<- read.csv("age and sex data 2013.csv")
age_sex_2013<- income_percent(age_sex_2013, 233316)
age_sex_2013<- na.omit(age_sex_2013)
age_sex_age_2013<- age_sex_2013[1:18,]

age_sex_2014<- read.csv("age and sex data 2014.csv")
age_sex_2014<- income_percent(age_sex_2014, 244648)
age_sex_2014<- na.omit(age_sex_2014)
age_sex_age_2014<- age_sex_2014[1:18,]

age_sex_2015<- read.csv("age and sex data 2015.csv")
age_sex_2015<- income_percent(age_sex_2015, 248983)
age_sex_2015<- na.omit(age_sex_2015)
age_sex_age_2015<- age_sex_2015[1:18,]

age_sex_2016<- read.csv("age and sex data 2016.csv")
age_sex_2016<- income_percent(age_sex_2016, 253156)
age_sex_2016<- na.omit(age_sex_2016)
age_sex_age_2016<- age_sex_2016[1:18,]

age_sex_2017<- read.csv("age and sex data 2017.csv")
age_sex_2017<- convert_total_estimate(age_sex_2017)
age_sex_2017<- na.omit(age_sex_2017)
age_sex_age_2017<- age_sex_2017[2:19,]

age_sex_2018<- read.csv("age and sex data 2018.csv")
age_sex_2018<- convert_total_estimate(age_sex_2018)
age_sex_2018<- na.omit(age_sex_2018)
age_sex_age_2018<- age_sex_2018[2:19,]

age_sex_2019<- read.csv("age and sex data 2019.csv")
age_sex_2019<- convert_total_estimate(age_sex_2019)
age_sex_2019<- na.omit(age_sex_2019)
age_sex_age_2019<- age_sex_2019[2:19,]

age_sex_2021<- read.csv("age and sex data 2021.csv")
age_sex_2021<- convert_total_estimate(age_sex_2021)
age_sex_2021<- na.omit(age_sex_2021)
age_sex_age_2021<- age_sex_2021[2:19,]

age_sex_2022<- read.csv("age and sex data 2022.csv")
age_sex_2022<- convert_total_estimate(age_sex_2022)
age_sex_2022<- na.omit(age_sex_2022)
age_sex_age_2022<- age_sex_2022[2:19,]

under_five<- data.frame(years=c(2010:2019, 2021:2022),
                        total=c(age_sex_age_2010[1,2],
                                age_sex_age_2011[1,2],
                                age_sex_age_2012[1,2],
                                age_sex_age_2013[1,2],
                                age_sex_age_2014[1,2],
                                age_sex_age_2015[1,2],
                                age_sex_age_2016[1,2],
                                age_sex_age_2017[1,2],
                                age_sex_age_2018[1,2],
                                age_sex_age_2019[1,2],
                                age_sex_age_2021[1,2],
                                age_sex_age_2022[1,2]))
#DONE CLEANING

#visualizing:

order_labels=c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", 
               "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years",
               "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years",
               "60 to 64 years", "65 to 69 years", "70 to 74 years", "75 to 79 years", 
               "80 to 84 years", "85 years and over")

Estimate_2022<- age_sex_age_2022$total.estimate
Estimate_2021<- age_sex_age_2021$total.estimate
Estimate_2019<- age_sex_age_2019$total.estimate
Estimate_2018<- age_sex_age_2018$total.estimate
Estimate_2017<- age_sex_age_2017$total.estimate
Estimate_2016<- age_sex_age_2016$total.estimate
Estimate_2015<- age_sex_age_2015$total.estimate
Estimate_2014<- age_sex_age_2014$total.estimate
Estimate_2013<- age_sex_age_2013$total.estimate
Estimate_2012<- age_sex_age_2012$total.estimate
Estimate_2011<- age_sex_age_2011$total.estimate
Estimate_2010<- age_sex_age_2010$total.estimate

age_sex_2022_df<- data.frame(Label=order_labels, 
                             Estimate=Estimate_2022)
age_sex_2021_df<- data.frame(Label=order_labels, 
                             Estimate=Estimate_2021)
age_sex_2019_df<- data.frame(Label=order_labels, 
                             Estimate=Estimate_2019)
age_sex_2018_df<- data.frame(Label=order_labels, 
                             Estimate=Estimate_2018)
age_sex_2017_df<- data.frame(Label=order_labels, 
                             Estimate=Estimate_2017)
age_sex_2016_df<- data.frame(Label=order_labels, 
                             Estimate=Estimate_2016)
age_sex_2015_df<- data.frame(Label=order_labels, 
                             Estimate=Estimate_2015)
age_sex_2014_df<- data.frame(Label=order_labels, 
                             Estimate=Estimate_2014)
age_sex_2013_df<- data.frame(Label=order_labels, 
                             Estimate=Estimate_2013)
age_sex_2012_df<- data.frame(Label=order_labels, 
                             Estimate=Estimate_2012)
age_sex_2011_df<- data.frame(Label=order_labels, 
                             Estimate=Estimate_2011)
age_sex_2010_df<- data.frame(Label=order_labels, 
                             Estimate=Estimate_2010)

age_sex_graphs<- function(my_data, year) {
  ggplot(my_data, aes(x = order_labels, y = Estimate)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = paste("Population of children under five in Sioux Falls in", year), x = "Label", y = "Estimate") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(0,25000)
  
}

age_sex_graphs(age_sex_2022_df, 2022)
age_sex_graphs(age_sex_2022_df, 2021)
age_sex_graphs(age_sex_2022_df, 2019)
age_sex_graphs(age_sex_2022_df, 2018)
age_sex_graphs(age_sex_2022_df, 2017)
age_sex_graphs(age_sex_2022_df, 2016)
age_sex_graphs(age_sex_2022_df, 2015)
age_sex_graphs(age_sex_2022_df, 2014)
age_sex_graphs(age_sex_2022_df, 2013)
age_sex_graphs(age_sex_2022_df, 2012)
age_sex_graphs(age_sex_2022_df, 2011)
age_sex_graphs(age_sex_2022_df, 2010)
#NOT THAT MUCH CHANGE BUT WILL HELP GET MORE ACCURATE RESULTS. 

setwd("C:/Users/blenl/OneDrive/Desktop/work/beacom reserch fellow/projection/employment")
employment_commas<- function(my_data){
  if ("Estimate" %in% colnames(my_data)) {
    my_data <- my_data %>%
      mutate(Estimate = as.numeric(gsub(",", "", Estimate)))
    return(my_data)
  } else {
    stop("The column 'Estimate' does not exist in the data frame.")
  }
}
employment_2010<- read.csv("employment 2010.csv")
employment_2010<- employment_commas(employment_2010)
employment_2010<- na.omit(employment_2010)

employment_2011<- read.csv("employment 2011.csv")
employment_2011<- employment_commas(employment_2011)
employment_2011<- na.omit(employment_2011)

employment_2012<- read.csv("employment 2012.csv")
employment_2012<- employment_commas(employment_2012)
employment_2012<- na.omit(employment_2012)

employment_2013<- read.csv("employment 2013.csv")
employment_2013<- employment_commas(employment_2013)
employment_2013<- na.omit(employment_2013)

employment_2014<- read.csv("employment 2014.csv")
employment_2014<- employment_commas(employment_2014)
employment_2014<- na.omit(employment_2014)

employment_2015<- read.csv("employment 2015.csv")
employment_2015<- employment_commas(employment_2015)
employment_2015<- na.omit(employment_2015)

employment_2016<- read.csv("employment 2016.csv")
employment_2016<- employment_commas(employment_2016)
employment_2016<- na.omit(employment_2016)

employment_2017<- read.csv("employment 2017.csv")
employment_2017<- employment_commas(employment_2017)
employment_2017<- na.omit(employment_2017)

employment_2018<- read.csv("employment 2018.csv")
employment_2018<- employment_commas(employment_2018)
employment_2018<- na.omit(employment_2018)

employment_2019<- read.csv("employment 2019.csv")
employment_2019<- employment_commas(employment_2019)
employment_2019<- na.omit(employment_2019)

employment_2021<- read.csv("employment 2021.csv")
employment_2021<- employment_commas(employment_2021)
employment_2021<- na.omit(employment_2021)

employment_2022<- read.csv("employment 2022.csv")
employment_2022<- employment_commas(employment_2022)
employment_2022<- na.omit(employment_2022)

# DON'T THINK EMPLOYMENT WILL HAVE THAT MUCH OF AN EFFECT. WILL COME BACK IF NECESSARY TO INCREASE 
#ACCURACY.
employment_labor_2022<- employment_2022[3:6,]
employment_labor_2021<- employment_2021[3:6,]
employment_labor_2019<- employment_2019[3:6,]
employment_labor_2018<- employment_2018[3:6,]
employment_labor_2017<- employment_2017[3:6,]
employment_labor_2016<- employment_2016[3:6,]
employment_labor_2015<- employment_2015[3:6,]
employment_labor_2014<- employment_2014[3:6,]
employment_labor_2013<- employment_2013[3:6,]
employment_labor_2012<- employment_2012[3:6,]
employment_labor_2011<- employment_2011[3:6,]
employment_labor_2010<- employment_2010[3:6,]



years_total_migration<- data.frame(
  years= c(2010, 2011,2012,2013,2014,2015,2016,2017,2018,2019,2021,2022),
  total_estimate=c(migration_age_2010[1,6],
                   migration_age_2011[1,6],
                   migration_age_2012[1,6],
                   migration_age_2013[1,6],
                   migration_age_2014[1,6],
                   migration_age_2015[1,6],
                   migration_age_2016[1,6],
                   migration_age_2017[1,6],
                   migration_age_2018[1,6],
                   migration_age_2019[1,6],
                   migration_age_2021[1,6],
                   migration_age_2022[1,6])
)
plot(years_total_migration$years, years_total_migration$total_estimate,type="o", xlab="Years", 
     ylab="Total estimate", main="Graph of the number of children aged 1 to 4 who moved to Sioux Falls ")
#DONE CLEANING
#NO FURTHER VISUALIZATION NEEDED. MIGRATION OF KIDS FROM 1 TO 4 SHOULD BE A VARIABLE. 

###preschool enrollment data
preschool_commas<- function(my_data){
  if ("Estimate" %in% colnames(my_data)) {
    my_data <- my_data %>%
      mutate(Estimate = as.numeric(gsub(",", "", Estimate)))
    return(my_data)
  } else {
    stop("The column 'Estimate' does not exist in the data frame.")
  }
}

setwd("C:/Users/blenl/OneDrive/Desktop/work/beacom reserch fellow/projection/preschool enrollment")
preschool_2022<- read.csv("preschool 2022.csv")
preschool_2022<- preschool_commas(preschool_2022)
preschool_2022<- data.frame(Male=preschool_2022[4,2], 
                            Female=preschool_2022[28,2])

preschool_2021<- read.csv("preschool 2021.csv")
preschool_2021<- preschool_commas(preschool_2021)
preschool_2021<- data.frame(Male=preschool_2021[4,2], 
                            Female=preschool_2021[28,2])

preschool_2019<- read.csv("preschool 2019.csv")
preschool_2019<- preschool_commas(preschool_2019)
preschool_2019<- data.frame(Male=preschool_2019[4,2], 
                            Female=preschool_2019[28,2])

preschool_2018<- read.csv("preschool 2018.csv")
preschool_2018<- preschool_commas(preschool_2018)
preschool_2018<- data.frame(Male=preschool_2018[4,2], 
                            Female=preschool_2018[28,2])

preschool_2017<- read.csv("preschool 2017.csv")
preschool_2017<- preschool_commas(preschool_2017)
preschool_2017<- data.frame(Male=preschool_2017[4,2], 
                            Female=preschool_2017[28,2])

preschool_2016<- read.csv("preschool 2016.csv")
preschool_2016<- preschool_commas(preschool_2016)
preschool_2016<- data.frame(Male=preschool_2016[4,2], 
                            Female=preschool_2016[28,2])

preschool_2015<- read.csv("preschool 2015.csv")
preschool_2015<- preschool_commas(preschool_2015)
preschool_2015<- data.frame(Male=preschool_2015[4,2], 
                            Female=preschool_2015[28,2])

preschool_2014<- read.csv("preschool 2014.csv")
preschool_2014<- preschool_commas(preschool_2014)
preschool_2014<- data.frame(Male=preschool_2014[4,2], 
                            Female=preschool_2014[28,2])

preschool_2013<- read.csv("preschool 2013.csv")
preschool_2013<- preschool_commas(preschool_2013)
preschool_2013<- data.frame(Male=preschool_2013[4,2], 
                            Female=preschool_2013[28,2])

preschool_2012<- read.csv("preschool 2012.csv")
preschool_2012<- preschool_commas(preschool_2012)
preschool_2012<- data.frame(Male=preschool_2012[4,2], 
                            Female=preschool_2012[28,2])

preschool_2011<- read.csv("preschool 2011.csv")
preschool_2011<- preschool_commas(preschool_2011)
preschool_2011<- data.frame(Male=preschool_2011[4,2], 
                            Female=preschool_2011[28,2])

preschool_2010<- read.csv("preschool 2010.csv")
preschool_2010<- preschool_commas(preschool_2010)
preschool_2010<- data.frame(Male=preschool_2010[4,2], 
                            Female=preschool_2010[28,2])

male_values <- c(
  preschool_2010$Male, preschool_2011$Male, preschool_2012$Male,
  preschool_2013$Male, preschool_2014$Male, preschool_2015$Male,
  preschool_2016$Male, preschool_2017$Male, preschool_2018$Male,
  preschool_2019$Male, preschool_2021$Male, preschool_2022$Male
)

female_values <- c(
  preschool_2010$Female, preschool_2011$Female, preschool_2012$Female,
  preschool_2013$Female, preschool_2014$Female, preschool_2015$Female,
  preschool_2016$Female, preschool_2017$Female, preschool_2018$Female,
  preschool_2019$Female, preschool_2021$Female, preschool_2022$Female
)

preschool_df <- data.frame(
  Year = c(2010:2019, 2021:2022),
  Male = male_values,
  Female = female_values,
  total= male_values + female_values
)

print(preschool_df)

ggplot(preschool_df, aes(x = Year)) +
  geom_line(aes(y = Male, color = "Male"), size = 1) +
  geom_line(aes(y = Female, color = "Female"), size = 1) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "pink")) +
  labs(
    title = "Trend of Preschool Enrollment by Gender",
    x = "Year",
    y = "Number of Enrollments",
    color = "Gender"
  ) +
  theme_minimal()



# PROJECTION:
final_df <- data.frame(
  years = c(2010:2019, 2021:2022),
  demographics_preschool = demographics_age_df$Estimate,
  migration_preschool = migration_preschool$total_estimate
)

lm_model <- lm(demographics_preschool ~  migration_preschool, data = final_df)

years <- c(2010:2019, 2021:2022)
preschool_estimates <- final_df$demographics_preschool
migration_preschool_estimates <- final_df$migration_preschool

preschool_growth_rate <- (preschool_estimates[length(preschool_estimates)] / preschool_estimates[1])^(1/(length(years)-1)) - 1
migration_growth_rate <- (migration_preschool_estimates[length(migration_preschool_estimates)] / migration_preschool_estimates[1])^(1/(length(years)-1)) - 1

last_preschool_estimate <- preschool_estimates[length(preschool_estimates)]
last_migration_estimate <- migration_preschool_estimates[length(migration_preschool_estimates)]

new_years <- 2023:2032
new_data <- data.frame(
  years = new_years,
  demographics_preschool = numeric(length(new_years)),
  migration_preschool = numeric(length(new_years))
)

new_data$demographics_preschool[1] <- last_preschool_estimate * (1 + preschool_growth_rate)
new_data$migration_preschool[1] <- last_migration_estimate * (1 + migration_growth_rate)

for (i in 2:length(new_years)) {
  new_data$demographics_male[i] <- new_data$demographics_male[i-1] * (1 + male_growth_rate)
  new_data$demographics_female[i] <- new_data$demographics_female[i-1] * (1 + female_growth_rate)
  new_data$demographics_preschool[i] <- new_data$demographics_preschool[i-1] * (1 + preschool_growth_rate)
  new_data$migration_preschool[i] <- new_data$migration_preschool[i-1] * (1 + migration_growth_rate)
}

predictions <- predict(lm_model, newdata = new_data)
new_data$predictions <- predictions
print(new_data)







###TESTING ACCURACY BY USING ONLY MIGRATION AS THE INDEPENDENT VARIABLE. 
training_df<- final_df[1:6,]
testing_df<- final_df[7:12,]

model_demographics<- lm(training_df$demographics_preschool~training_df$migration_preschool)
#assuming that demographics is going to be the dependent variable and migration is the only independent 
#variable. 
predictions_demographics<-predict(model_demographics, newdata=training_df) 
new_data1<- data.frame(years=c(2016:2019, 2021:2022),
                      demographics_testing=training_df$demographics_preschool,#the actual demographics data
                      prediction_demographics= predictions_demographics, 
                      percent_error=abs((predictions_demographics-testing_df$demographics_preschool)/testing_df$demographics_preschool)) *100#the predicted demographics data

print (new_data1)






###TESTING ACCURACY BY USING MIGRATION AND PRESCHOOL ENROLLMENT AS INDEPENDENT VARAIBLES. 
final_df$enrollment<- abs((preschool_df$total))
training_df<- final_df[1:6,]
testing_df<- final_df[7:12,]

model_demographics<- lm(training_df$demographics_preschool~training_df$migration_preschool+ training_df$enrollment)
#assuming that demographics is going to be the dependent variable and migration is the only independent 
#variable. 
predictions_demographics<-predict(model_demographics, newdata=training_df) 
new_data2<- data.frame(years=c(2016:2019, 2021:2022),
                      demographics_testing=training_df$demographics_preschool,#the actual demographics data
                      prediction_demographics= predictions_demographics, 
                      percent_error=abs((predictions_demographics-testing_df$demographics_preschool)/testing_df$demographics_preschool)) *100#the predicted demographics data

print (new_data2)




###TESTING ACCURACY INCLUDING  A SECOND SET OF DEMOGRAPHICS(AGE) DATA.

final_df$age<- abs((under_five$total))
training_df<- final_df[1:6,]
testing_df<- final_df[7:12,]

model_demographics<- lm(training_df$demographics_preschool~training_df$migration_preschool+ training_df$enrollment+training_df$age)
#assuming that demographics is going to be the dependent variable and migration is the only independent 
#variable. 
predictions_demographics<-predict(model_demographics, newdata=training_df) 
new_data3<- data.frame(years=c(2016:2019, 2021:2022),
                      demographics_testing=training_df$demographics_preschool,#the actual demographics data
                      prediction_demographics= predictions_demographics, 
                      percent_error=abs((predictions_demographics-testing_df$demographics_preschool)/testing_df$demographics_preschool)) *100#the predicted demographics data

print (new_data3)




##INCLUDING MIGRATION PATTERNS BASED ON CITIZENSHIP.
final_df$native<- abs((migration_native_df$total))
final_df$naturalized<- abs((migration_naturalized_df$total))
final_df$non_us_df<- abs((migration_non_us_df$total))
final_df$migration_foreign_born_df<- abs((migration_foreign_born_df$total))
training_df<- final_df[1:6,]
testing_df<- final_df[7:12,]

model_demographics<- lm(training_df$demographics_preschool~training_df$migration_preschool+ training_df$enrollment + training_df$age +training_df$native+
                          training_df$naturalized+ training_df$non_us_df +training_df$migration_foreign_born_df)
#assuming that demographics is going to be the dependent variable and migration is the only independent 
#variable. 
predictions_demographics<-predict(model_demographics, newdata=training_df) 
new_data4<- data.frame(years=c(2016:2019, 2021:2022),
                      demographics_testing=training_df$demographics_preschool,#the actual demographics data
                      prediction_demographics= predictions_demographics, 
                      percent_error=abs((predictions_demographics-testing_df$demographics_preschool)/testing_df$demographics_preschool)) *100#the predicted demographics data

print (new_data4)



##DETERMINING WHICH MODEL TO USE
percent_error<- data.frame(new_data1[,4], new_data2[,4], new_data3[,4], new_data4[,4])
colnames(percent_error)<- c("Model1", "Model2", "Model3", "Model4")
rownames(percent_error)<- c("Year1", "year2", "year3", "year4", "year5", "year6")

mean(percent_error[,1])
mean(percent_error[,2])
mean(percent_error[,3])
mean(percent_error[,4])

#therefore the fourth model is the best one to use. 
