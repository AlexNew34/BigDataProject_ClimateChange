#Load necessary packages for cleaning.
pacman::p_load(tidyverse,stringr)

#######################################
#Cleaning Climate Change Disaster Frequency Yearly taken from 'https://climatedata.imf.org/pages/climatechange-data'.
#Read named file.
df <- read.csv('CCDF.csv')
#Filter dataframe such that only total frequencies are taken.
df <- df %>% 
  filter(df$Indicator == "Climate related disasters frequency, Number of Disasters: TOTAL")
#Remove other columns within the dataframe.
df <- df %>% 
  select(-ObjectId,-Indicator,-ISO2,-ISO3,-Unit,-Source,-CTS.Code,-CTS.Name,-CTS.Full.Descriptor)
#Rename columns.
df <- df %>%
  rename(
    "country" = "Country",
    "1980" = "X1980",
    "1981" = "X1981",
    "1982" = "X1982",
    "1983" = "X1983",
    "1984" = "X1984",
    "1985" = "X1985",
    "1986" = "X1986",
    "1987" = "X1987",
    "1988" = "X1988",
    "1989" = "X1989",
    "1990" = "X1990",
    "1991" = "X1991",
    "1992" = "X1992",
    "1993" = "X1993",
    "1994" = "X1994",
    "1995" = "X1995",
    "1996" = "X1996",
    "1997" = "X1997",
    "1998" = "X1998",
    "1999" = "X1999",
    "2000" = "X2000",
    "2001" = "X2001",
    "2002" = "X2002",
    "2003" = "X2003",
    "2004" = "X2004",
    "2005" = "X2005",
    "2006" = "X2006",
    "2007" = "X2007",
    "2008" = "X2008",
    "2009" = "X2009",
    "2010" = "X2010",
    "2011" = "X2011",
    "2012" = "X2012",
    "2013" = "X2013",
    "2014" = "X2014",
    "2015" = "X2015",
    "2016" = "X2016",
    "2017" = "X2017",
    "2018" = "X2018",
    "2019" = "X2019",
    "2020" = "X2020",
    "2021" = "X2021",
    "2022" = "X2022",
    "2023" = "X2023",
    "2024" = "X2024",
  )
#Change dataframe style.
df <- gather(df, key = "year", value = "freq", '1980':'2024')
#Filter countries.
df_na <- df %>% 
  filter(df$country != "Australia")
df_na <- df %>% 
  filter(df$country != "World")
df_a <- df %>% 
  filter(df$country == "Australia")
#Save dataframe as a csv file.
write.csv(df_na,"./Cleaned_CCDF_na.csv", row.names = FALSE)
write.csv(df_a,"./Cleaned_CCDF_a.csv", row.names = FALSE)

#######################################
#Cleaning Carbon Dioxide Yearly from 'https://climatedata.imf.org/pages/climatechange-data'.
df <- read.csv('CCCD.csv')
df <- df %>%
  select(-ObjectId,-Country,-ISO2,-ISO3,-Source,-CTS.Code,-CTS.Name,-CTS.Full.Descriptor)
#Select certain rows from dataframe.
df <- df %>%
  slice(513:1592)
#Filter for parts per million.
df <- df %>% 
  filter(df$Unit == "Parts Per Million")
last_col_name <- names(df)[ncol(df)]
#Average for over the 12 months.
df <- df %>%
  mutate(group = ceiling(row_number() / 12)) %>%
  group_by(group) %>%
  summarise(avg_ce = mean(.data[[last_col_name]], na.rm = TRUE))
df$year <- 1980:2024 
df <- df %>% 
  select(year,avg_ce)
#Save dataframe as a csv file.
write.csv(df,"./Cleaned_CCCD.csv", row.names = FALSE)

#######################################
#Cleaning Annual Temperature from 'https://climatedata.imf.org/pages/climatechange-data'.
df <- read.csv('CCAT.csv')
df <- df %>% 
  select(-ObjectId,-Indicator,-ISO2,-ISO3,-Unit,-Source,-CTS.Code,-CTS.Name,
         -CTS.Full.Descriptor,-X1961,-X1962,-X1963,-X1964,-X1965,-X1966,-X1967,
         -X1968,-X1969,-X1970,-X1971,-X1972,-X1973,-X1974,-X1975,-X1976,-X1977,
         -X1978,-X1979)
df <- df %>%
  rename(
    "country" = "Country",
    "1980" = "X1980",
    "1981" = "X1981",
    "1982" = "X1982",
    "1983" = "X1983",
    "1984" = "X1984",
    "1985" = "X1985",
    "1986" = "X1986",
    "1987" = "X1987",
    "1988" = "X1988",
    "1989" = "X1989",
    "1990" = "X1990",
    "1991" = "X1991",
    "1992" = "X1992",
    "1993" = "X1993",
    "1994" = "X1994",
    "1995" = "X1995",
    "1996" = "X1996",
    "1997" = "X1997",
    "1998" = "X1998",
    "1999" = "X1999",
    "2000" = "X2000",
    "2001" = "X2001",
    "2002" = "X2002",
    "2003" = "X2003",
    "2004" = "X2004",
    "2005" = "X2005",
    "2006" = "X2006",
    "2007" = "X2007",
    "2008" = "X2008",
    "2009" = "X2009",
    "2010" = "X2010",
    "2011" = "X2011",
    "2012" = "X2012",
    "2013" = "X2013",
    "2014" = "X2014",
    "2015" = "X2015",
    "2016" = "X2016",
    "2017" = "X2017",
    "2018" = "X2018",
    "2019" = "X2019",
    "2020" = "X2020",
    "2021" = "X2021",
    "2022" = "X2022",
    "2023" = "X2023",
    "2024" = "X2024",
  )
df <- gather(df, key = "year", value = "avg_temp", '1980':'2024')
df_na <- df %>% 
  filter(df$country != "Australia")
df_na <- df %>% 
  filter(df$country != "World")
df_a <- df %>% 
  filter(df$country == "Australia")
#Save dataframe as a csv file.
write.csv(df_na,"./Cleaned_CCAT_na.csv", row.names = FALSE)
write.csv(df_a,"./Cleaned_CCAT_a.csv", row.names = FALSE)
#######################################
#Cleaning Adelaide Rainfall from ' http://www.bom.gov.au/climate/data/'.
df1 <- read.csv('./Rainfall/AR.csv')
df1 <- df1 %>%
  select(Year, Annual)
df1$Annual<-as.numeric(df$Annual)
df1 <- df1 %>%
  slice(98:142)

#Cleaning Brisbane Rainfall from ' http://www.bom.gov.au/climate/data/'.
df2 <- read.csv('./Rainfall/BR.csv')
df2 <- df2 %>%
  select(Year, Annual)
df2$Annual<-as.numeric(df$Annual)
df2 <- df2 %>%
  slice(82:126)

#Cleaning Canberra Rainfall from ' http://www.bom.gov.au/climate/data/'.
df3 <- read.csv('./Rainfall/CR.csv')
df3 <- df3 %>%
  select(Year, Annual)
df3$Annual<-as.numeric(df$Annual)
df3 <- df3 %>%
  slice(13:57)

#Cleaning Melbourne Rainfall from ' http://www.bom.gov.au/climate/data/'.
df4 <- read.csv('./Rainfall/MR.csv')
df4 <- df4 %>%
  select(Year, Annual)
df4$Annual<-as.numeric(df$Annual)
df4 <- df4 %>%
  slice(9:52)
newdata<-data.frame(Year = 1985, Annual = NA)
df4 <- rbind(df4, newdata)
df4 <- df4[order(df4$Year, decreasing = FALSE),]

#Cleaning Perth Rainfall from ' http://www.bom.gov.au/climate/data/'.
df5 <- read.csv('./Rainfall/PR.csv')
df5 <- df5 %>%
  select(Year, Annual)
df5$Annual<-as.numeric(df5$Annual)
df5 <- df5 %>%
  slice(37:81)

#Cleaning Sydney Rainfall from ' http://www.bom.gov.au/climate/data/'.
df6 <- read.csv('./Rainfall/SR.csv')
df6 <- df6 %>%
  select(Year, Annual)
df6$Annual<-as.numeric(df$Annual)
df6 <- df6 %>%
  slice(96:140)

#Annual Australian Rainfall using the previous dataframes.
df_rainfall <- cbind(df1,df2$Annual,df3$Annual,df4$Annual,df5$Annual,df6$Annual)
df_rainfall <- df_rainfall %>% 
  select(-Year)
df_rainfall$Annual <- as.numeric(df_rainfall$Annual)
df_rainfall$`df2$Annual` <- as.numeric(df_rainfall$`df2$Annual`)
df_rainfall$`df3$Annual` <- as.numeric(df_rainfall$`df3$Annual`)
df_rainfall$`df4$Annual` <- as.numeric(df_rainfall$`df4$Annual`)
df_rainfall$`df5$Annual` <- as.numeric(df_rainfall$`df5$Annual`)
df_rainfall$`df6$Annual` <- as.numeric(df_rainfall$`df6$Annual`)

#Make it a dataframe.
df <- data.frame(cbind(year = 1980:2024, annual_rain = rowMeans(df_rainfall,na.rm = TRUE)))
#Save dataframe as a csv file.
write.csv(df,"./Cleaned_CCAR_a.csv", row.names = FALSE)

#######################################
#Annual Global Rainfall from 'https://ourworldindata.org/grapher/average-precipitation-per-year'.
df <- read.csv('./Rainfall/GAR.csv')
df <- df %>% 
  select(-Code)
df <- df %>%
  rename(
    "country" = "Entity",
    "year" = "Year",
    "ann_rain" = "Annual.precipitation")
df_na <- df %>% 
  filter(df$country != "Australia")
df_na <- df %>% 
  filter(df$country != "World")
df_na <- df_na %>% 
  filter(df_na$year >= 1980)
df_na <- df_na[order(df_na$year, decreasing = FALSE),]
df_a <- df %>% 
  filter(df$country == "Australia")
df_a <- df_a %>% 
  filter(df_a$year >= 1980)
write.csv(df_a,"./Cleaned_CCAR_a_diff.csv", row.names = FALSE)
write.csv(df_na,"./Cleaned_CCAR_na.csv", row.names = FALSE)

#######################################
#Cleaning Land Cover Data from 'https://climatedata.imf.org/pages/climatechange-data'.
df <- read.csv('./CCLC.csv')
df <- df %>% 
  select(-ObjectId,-Indicator,-ISO2,-ISO3,-Unit,-Source,-CTS_Code,-CTS_Name,-CTS_Full_Descriptor,-Climate_Influence)
df[,"F1980"] <- NA
df[,"F1981"] <- NA
df[,"F1982"] <- NA
df[,"F1983"] <- NA
df[,"F1984"] <- NA
df[,"F1985"] <- NA
df[,"F1986"] <- NA
df[,"F1987"] <- NA
df[,"F1988"] <- NA
df[,"F1989"] <- NA
df[,"F1990"] <- NA
df[,"F1991"] <- NA
df[,"F2023"] <- NA
df[,"F2024"] <- NA
df <- df %>%
  rename(
    "country" = "Country",
    "1980" = "F1980",
    "1981" = "F1981",
    "1982" = "F1982",
    "1983" = "F1983",
    "1984" = "F1984",
    "1985" = "F1985",
    "1986" = "F1986",
    "1987" = "F1987",
    "1988" = "F1988",
    "1989" = "F1989",
    "1990" = "F1990",
    "1991" = "F1991",
    "1992" = "F1992",
    "1993" = "F1993",
    "1994" = "F1994",
    "1995" = "F1995",
    "1996" = "F1996",
    "1997" = "F1997",
    "1998" = "F1998",
    "1999" = "F1999",
    "2000" = "F2000",
    "2001" = "F2001",
    "2002" = "F2002",
    "2003" = "F2003",
    "2004" = "F2004",
    "2005" = "F2005",
    "2006" = "F2006",
    "2007" = "F2007",
    "2008" = "F2008",
    "2009" = "F2009",
    "2010" = "F2010",
    "2011" = "F2011",
    "2012" = "F2012",
    "2013" = "F2013",
    "2014" = "F2014",
    "2015" = "F2015",
    "2016" = "F2016",
    "2017" = "F2017",
    "2018" = "F2018",
    "2019" = "F2019",
    "2020" = "F2020",
    "2021" = "F2021",
    "2022" = "F2022",
    "2023" = "F2023",
    "2024" = "F2024",
  )
df <- df[, sort(names(df))]
df <- df %>% relocate(country)
df_na <- df[14,]
df_a <- df[26,]
df_na <- gather(df_na, key = "year", value = "gl_landc", '1980':'2024')
df_a <- gather(df_a, key = "year", value = "gl_landc", '1980':'2024')
#Save dataframe as a csv file.
write.csv(df_na,"./Cleaned_CCLC_na.csv", row.names = FALSE)
write.csv(df_a,"./Cleaned_CCLC_a.csv", row.names = FALSE)
#######################################
#Cleaned Sea Level from 'https://climatedata.imf.org/pages/climatechange-data'.
df <- read.csv('./CCSL.csv')
df <- df %>% 
  select(-Country,-ObjectId,-Indicator,-ISO2,-ISO3,-Unit,-Source,-CTS.Code,-CTS.Name,-CTS.Full.Descriptor,-Measure)
df$Date <- str_sub(df$Date, start=-4)
newdata<-data.frame(Date = (1980:1991), Value = NA)
df <- rbind(df, newdata)
df <- df[order(df$Date, decreasing = FALSE),]
df <- df %>%
  rename(
    "year" = "Date",
    "gl_sea" = "Value",
  )
df <- df %>% 
  filter(df$year < 2025)
#Save dataframe as a csv file.
write.csv(df,"./Cleaned_CCSL.csv", row.names = FALSE)
