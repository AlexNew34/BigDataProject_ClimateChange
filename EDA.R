#Load necessary packages for cleaning and visualisation.
pacman::p_load(tidyverse,corrplot,vip,randomForest, ggplot2, reshape2, car)

#Creating the dataframe for global data.
df1_na <- read.csv('./Cleaned_CCAT_na.csv')
df2_na <- read.csv('./Cleaned_CCCD.csv')
df3_na <- read.csv('./Cleaned_CCDF_na.csv')
df4_na <- read.csv('./Cleaned_CCAR_na.csv')
df5_na <- read.csv('./Cleaned_CCLC_na.csv')
df6_na <- read.csv('./Cleaned_CCSL.csv')
df1_na <- aggregate(avg_temp ~ year,df1_na, mean)
df3_na <- aggregate(freq ~ year,df3_na, mean)
df4_na <- aggregate(ann_rain ~ year,df4_na, mean)
df5_na <- aggregate(gl_landc ~ year,df5_na, mean)
newdata<-data.frame(year = (1980:1991), gl_landc = NA)
df5_na <- rbind(df5_na, newdata)
newdata<-data.frame(year = (2023:2024), gl_landc = NA)
df5_na <- rbind(df5_na, newdata)
df5_na <- df5_na[order(df5_na$year, decreasing = FALSE),]
df6_na <- aggregate(gl_sea ~ year,df6_na, mean)
newdata<-data.frame(year = (1980:1991), gl_sea = NA)
df6_na <- rbind(df6_na, newdata)
df6_na <- df6_na[order(df6_na$year, decreasing = FALSE),]
df_na <- cbind(df1_na,df2_na$avg_ce,df3_na$freq,df4_na$ann_rain, df5_na$gl_landc,df6_na$gl_sea)
df_na <- df_na %>%
  rename(
    "gl_avg_carbon" = "df2_na$avg_ce",
    "gl_freq" = "df3_na$freq",
    "gl_avg_prec" = "df4_na$ann_rain",
    "gl_avg_temp" = "avg_temp",
    "gl_land" = "df5_na$gl_landc",
    "gl_avg_sea" = "df6_na$gl_sea")
df_na$gl_freq <- ceiling(df_na$gl_freq)

#Creating the dataframe for Australian Data
df1_a <- read.csv('./Cleaned_CCAT_a.csv')
df2_a <- read.csv('./Cleaned_CCDF_a.csv')
df3_a <- read.csv('./Cleaned_CCAR_a_diff.csv')
df4_a <- read.csv('./Cleaned_CCCD.csv')
df5_a <- read.csv('./Cleaned_CCLC_a.csv')
df5_a <- aggregate(gl_landc ~ year,df5_a, mean)
newdata<-data.frame(year = (1980:1991), gl_landc = NA)
df5_a <- rbind(df5_a, newdata)
newdata<-data.frame(year = (2023:2024), gl_landc = NA)
df5_a <- rbind(df5_a, newdata)
df5_a <- df5_a[order(df5_a$year, decreasing = FALSE),]
df_a <- cbind(df1_a,df2_a$freq,df3_a$ann_rain,df4_a$avg_ce,df5_a$gl_landc)
df_a <- df_a %>%
  rename(
    "aus_avg_carbon" = "df4_a$avg_ce",
    "aus_freq" = "df2_a$freq",
    "aus_avg_prec" = "df3_a$ann_rain",
    "aus_avg_temp" = "avg_temp",
    "aus_land" = "df5_a$gl_landc")
df_a <- df_a %>% 
  select(-country)

#Join Global and Australian data together.
df <- cbind(df_na,df_a)
df <- subset(df, select = -8)
df <- subset(df, select = -11)
df <- df[, sort(names(df))]
df <- df %>% relocate(year)
df$aus_avg_prec <- round(df$aus_avg_prec, 4)
df$aus_avg_temp <- round(df$aus_avg_temp, 4)
df$aus_land <- round(df$aus_land, 4)
df$gl_avg_prec <- round(df$gl_avg_prec, 4)
df$gl_avg_carbon <- round(df$gl_avg_carbon, 4)
df$gl_avg_temp <- round(df$gl_avg_temp, 4)
df$gl_freq <- as.integer(df$gl_freq)
df$gl_land <- round(df$gl_land, 4)
df$gl_avg_sea <- round(df$gl_avg_sea, 4)
sapply(df,class)
#Save as the final csv file to use as a dataframe.
write.csv(df,"./Final_Cleaned_CC.csv", row.names = FALSE)

#######################################
#Visualisation of data.
df <- read.csv("./Final_Cleaned_CC.csv")
#Skim data.
skimr::skim(df)
#Correlogram.
cormat <-cor(df, use="complete.obs")
corrplot(cormat, method="circle")
#Scatterplot.
scatterplotMatrix(df[1:5])
scatterplotMatrix(select(df,year, starts_with("gl")))
#VIP and VIF analysis.
model <- lm(aus_freq ~ ., data = df)
vip(model)
vif_values <- vif(model)
print(vif_values)

df_long <- df %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value")

#Linear regression plots
ggplot(df_long, aes(x = year, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = " ",
       x = "Years",
       y = " ")

df_long <- df %>%
  pivot_longer(cols = -aus_freq, names_to = "variable", values_to = "value")

ggplot(df_long, aes(x = value, y = aus_freq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = " ",
       x = " ",
       y = "Climate disaster frequency in Australia")

df_long <- df %>%
  pivot_longer(cols = c(-year,-aus_freq), names_to = "variable", values_to = "value")

ggplot(df_long, aes(x = value, y = aus_freq, colour = year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = 'red') +
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = " ",
       x = " ",
       y = "Climate disaster frequency in Australia")
