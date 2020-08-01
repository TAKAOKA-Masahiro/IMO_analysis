## ----------------------------------------------------------------------------------------------------------------
library(readr)
library(XML)
library(dplyr)
library(ggplot2)


## ----------------------------------------------------------------------------------------------------------------
data2000 <- xmlParse("IMO_Team_2000.xml")
data2000 <- XML::xmlToDataFrame(data2000)
data2001 <- xmlParse("IMO_Team_2001.xml")
data2001 <- XML::xmlToDataFrame(data2001)
data2002 <- xmlParse("IMO_Team_2002.xml")
data2002 <- XML::xmlToDataFrame(data2002)
data2003 <- xmlParse("IMO_Team_2003.xml")
data2003 <- XML::xmlToDataFrame(data2003)
data2004 <- xmlParse("IMO_Team_2004.xml")
data2004 <- XML::xmlToDataFrame(data2004)
data2005 <- xmlParse("IMO_Team_2005.xml")
data2005 <- XML::xmlToDataFrame(data2005)
data2006 <- xmlParse("IMO_Team_2006.xml")
data2006 <- XML::xmlToDataFrame(data2006)
data2007 <- xmlParse("IMO_Team_2007.xml")
data2007 <- XML::xmlToDataFrame(data2007)
data2008 <- xmlParse("IMO_Team_2008.xml")
data2008 <- XML::xmlToDataFrame(data2008)
data2009 <- xmlParse("IMO_Team_2009.xml")
data2009 <- XML::xmlToDataFrame(data2009)
data2010 <- xmlParse("IMO_Team_2010.xml")
data2010 <- XML::xmlToDataFrame(data2010)
data2011 <- xmlParse("IMO_Team_2011.xml")
data2011 <- XML::xmlToDataFrame(data2011)
data2012 <- xmlParse("IMO_Team_2012.xml")
data2012 <- XML::xmlToDataFrame(data2012)
data2013 <- xmlParse("IMO_Team_2013.xml")
data2013 <- XML::xmlToDataFrame(data2013)
data2014 <- xmlParse("IMO_Team_2014.xml")
data2014 <- XML::xmlToDataFrame(data2014)
data2015 <- xmlParse("IMO_Team_2015.xml")
data2015 <- XML::xmlToDataFrame(data2015)
data2016 <- xmlParse("IMO_Team_2016.xml")
data2016 <- XML::xmlToDataFrame(data2016)
data2017 <- xmlParse("IMO_Team_2017.xml")
data2017 <- XML::xmlToDataFrame(data2017)
data2018 <- xmlParse("IMO_Team_2018.xml")
data2018 <- XML::xmlToDataFrame(data2018)
data2019 <- xmlParse("IMO_Team_2019.xml")
data2019 <- XML::xmlToDataFrame(data2019)


## ----------------------------------------------------------------------------------------------------------------
data2000 <- transform(data2000, year =  2000)
data2001 <- transform(data2001, year =  2001)
data2002 <- transform(data2002, year =  2002)
data2003 <- transform(data2003, year =  2003)
data2004 <- transform(data2004, year =  2004)
data2005 <- transform(data2005, year =  2005)
data2006 <- transform(data2006, year =  2006)
data2007 <- transform(data2007, year =  2007)
data2008 <- transform(data2008, year =  2008)
data2009 <- transform(data2009, year =  2009)
data2010 <- transform(data2010, year =  2010)
data2011 <- transform(data2011, year =  2011)
data2012 <- transform(data2012, year =  2012)
data2013 <- transform(data2013, year =  2013)
data2014 <- transform(data2014, year =  2014)
data2015 <- transform(data2015, year =  2015)
data2016 <- transform(data2016, year =  2016)
data2017 <- transform(data2017, year =  2017)
data2018 <- transform(data2018, year =  2018)
data2019 <- transform(data2019, year =  2019)


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data <- rbind(data2000,data2001,data2002,data2003,data2004,data2005,data2006,data2007,data2008,data2009,data2010,data2011,data2012,data2013,data2014,data2015,data2016,data2017,data2018,data2019)


## ----------------------------------------------------------------------------------------------------------------
str(Math_Oly_data)


## ----------------------------------------------------------------------------------------------------------------
write.csv(Math_Oly_data, "Math_Oly_data.csv", row.names = FALSE) 


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data <- read.csv("~/R/20202Q_sat_9/Math_Oly_data.csv")


## ----------------------------------------------------------------------------------------------------------------
str(Math_Oly_data)


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data %>% 
  group_by(name) %>%
  summarize(entry_cnt = n(),
            rank_avg = mean(rank, na.rm = TRUE),
            rank_high = min(rank, na.rm = TRUE),
            rank_low = max(rank, na.rm = TRUE),
            max_diff = max(max(rank, na.rm = TRUE) - min(rank, na.rm = TRUE))) %>% arrange(rank_avg)


## ----------------------------------------------------------------------------------------------------------------
CountryCode <- read.csv("~/R/20202Q_sat_9/CountryCode.csv")
names(CountryCode)[1] <- "name"


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data <- Math_Oly_data %>%
  left_join(CountryCode, by='name')


## ----------------------------------------------------------------------------------------------------------------
sum(is.na(Math_Oly_data$Country_Code))


## ----------------------------------------------------------------------------------------------------------------
temperature <- read.csv("~/R/20202Q_sat_9/temperature.csv")


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data <- Math_Oly_data %>%
  left_join(temperature, by='Country_Code')


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data %>% 
  group_by(name) %>%
  summarize(entry_cnt = n(),
            rank_avg = mean(rank, na.rm = TRUE),
            rank_high = min(rank, na.rm = TRUE),
            rank_low = max(rank, na.rm = TRUE),
            max_diff = max(max(rank, na.rm = TRUE) - min(rank, na.rm = TRUE)),
            temperature = mean(Average_yearly_temperature)) %>% arrange(rank_avg)


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data %>% filter(year == 2019) %>% filter(!is.na(Average_yearly_temperature)) %>% ggplot(., aes(x = Average_yearly_temperature, y = rank)) + geom_point() + geom_smooth(method = "lm")


## ----------------------------------------------------------------------------------------------------------------
temp_lm <- Math_Oly_data %>% filter(year == 2019) %>% filter(!is.na(Average_yearly_temperature)) %>% lm(rank ~ Average_yearly_temperature, data = .)
summary(temp_lm)


## ----------------------------------------------------------------------------------------------------------------
GDP_per_capita <- read_csv("GDP_per_capita.csv")


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data <- Math_Oly_data %>%
  left_join(GDP_per_capita, by='Country_Code')


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data %>% 
  group_by(name) %>%
  summarize(entry_cnt = n(),
            rank_avg = mean(rank, na.rm = TRUE),
            rank_high = min(rank, na.rm = TRUE),
            rank_low = max(rank, na.rm = TRUE),
            max_diff = max(max(rank, na.rm = TRUE) - min(rank, na.rm = TRUE)),
            temperature = mean(Average_yearly_temperature),
            GDP_per_capita = mean(`2019`)) %>% arrange(rank_avg)


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data %>% filter(year == 2019) %>% filter(!is.na(Average_yearly_temperature)) %>% filter(!is.na(`2019`)) %>% ggplot(., aes(x = `2019`, y = rank)) + geom_point() + geom_smooth(method = "lm")


## ----------------------------------------------------------------------------------------------------------------
temp_lm <- Math_Oly_data %>% filter(year == 2019) %>% filter(!is.na(Average_yearly_temperature)) %>% filter(!is.na(`2019`)) %>% lm(rank ~ Average_yearly_temperature + `2019`, data = .)
summary(temp_lm)


## ----------------------------------------------------------------------------------------------------------------
GDP_per_capita_v2 <- read_csv("GDP_per_capita_v2.csv")
Gov_expenditure_on_educa <- read_csv("Gov_expenditure_on_educa.csv")


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data2 <- Math_Oly_data[1:19]


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data2 <- Math_Oly_data2 %>%
  left_join(GDP_per_capita_v2, by='Country_Code')


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data2 %>% 
  group_by(name) %>%
  summarize(entry_cnt = n(),
            rank_avg = mean(rank, na.rm = TRUE),
            rank_high = min(rank, na.rm = TRUE),
            rank_low = max(rank, na.rm = TRUE),
            max_diff = max(max(rank, na.rm = TRUE) - min(rank, na.rm = TRUE)),
            temperature = mean(Average_yearly_temperature),
            GDP_per_capita = mean(GDP_per_capita)) %>% arrange(rank_avg)


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data2 %>% filter(year == 2019) %>% filter(!is.na(GDP_per_capita)) %>% filter(!is.na(Average_yearly_temperature)) %>% ggplot(., aes(x = GDP_per_capita, y = rank)) + geom_point() + geom_smooth(method = "lm")


## ----------------------------------------------------------------------------------------------------------------
temp_lm2 <- Math_Oly_data2 %>% filter(year == 2019) %>% filter(!is.na(Average_yearly_temperature)) %>% filter(!is.na(GDP_per_capita)) %>% lm(rank ~ Average_yearly_temperature + GDP_per_capita, data = .)
summary(temp_lm2)


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data2 <- Math_Oly_data2 %>%
  left_join(Gov_expenditure_on_educa, by='Country_Code')


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data2 %>% 
  group_by(name) %>%
  summarize(entry_cnt = n(),
            rank_avg = mean(rank, na.rm = TRUE),
            rank_high = min(rank, na.rm = TRUE),
            rank_low = max(rank, na.rm = TRUE),
            max_diff = max(max(rank, na.rm = TRUE) - min(rank, na.rm = TRUE)),
            temperature = mean(Average_yearly_temperature),
            GDP_per_capita = mean(GDP_per_capita),
            Gov_expenditure_on_educa = mean(Gov_expenditure_on_educa)) %>% arrange(rank_avg)


## ----------------------------------------------------------------------------------------------------------------
Math_Oly_data2 %>% filter(year == 2019) %>% filter(!is.na(GDP_per_capita)) %>% filter(!is.na(Average_yearly_temperature)) %>% filter(!is.na(Gov_expenditure_on_educa)) %>% ggplot(., aes(x = Gov_expenditure_on_educa, y = rank)) + geom_point() + geom_smooth(method = "lm")


## ----------------------------------------------------------------------------------------------------------------
temp_lm2 <- Math_Oly_data2 %>% filter(year == 2019) %>% filter(!is.na(Average_yearly_temperature)) %>% filter(!is.na(GDP_per_capita)) %>% filter(!is.na(Gov_expenditure_on_educa)) %>% lm(rank ~ Average_yearly_temperature + GDP_per_capita + Gov_expenditure_on_educa, data = .)
summary(temp_lm2)


## ----------------------------------------------------------------------------------------------------------------
library(psych)
Math_Oly_data2_2019 <- Math_Oly_data2 %>% filter(year == 2019) %>% filter(!is.na(Average_yearly_temperature)) %>% filter(!is.na(GDP_per_capita)) %>% filter(!is.na(Gov_expenditure_on_educa))
pairs.panels(Math_Oly_data2[c( "rank", "Average_yearly_temperature", "GDP_per_capita", "Gov_expenditure_on_educa")])


## ----------------------------------------------------------------------------------------------------------------
knitr::purl("Math_Oly.Rmd")

