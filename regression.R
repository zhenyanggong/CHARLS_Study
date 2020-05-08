# start code
#install.packages("plm")
library(plm)
library(lmtest)
library(multiwayvcov)
library(foreign)
library(dplyr)
#install.packages("multiwayvcov")
setwd("/Users/zhenyanggong/Desktop/CHARLS")
# reading data from IPUMs extract
# Change these filepaths to the filepaths of your downloaded extract

df_2011 <- read.csv("2011.csv") 
df_2013 <- read.csv("2013.csv") 
df_2015 <- read.csv("2015.csv") 
df_gender <- read.csv("gender.csv")

id_list_11 <- select(df_2011, 'ID')
id_list_13 <- select(df_2013, 'ID')
id_list_15 <- select(df_2015, 'ID')

id_list <- merge(x = id_list_11, y = id_list_13, by = "ID")
id_list <- merge(x = id_list, y = id_list_15, by = "ID")
id_list <- merge(x = id_list, y = df_gender, by = "ID")
id_list <- select(id_list, 'ID', 'gender')

df_2011 = merge(x = id_list, y = df_2011, by = "ID")
df_2013 = merge(x = id_list, y = df_2013, by = "ID")
df_2015 = merge(x = id_list, y = df_2015, by = "ID")

df1 <- subset(df_2011, gender == 1)
df2 <- subset(df_2011, gender == 2)
df_2011 = merge(x = df1, y = df2, by = "householdID")

df1 <- subset(df_2013, gender == 1)
df2 <- subset(df_2013, gender == 2)
df_2013 = merge(x = df1, y = df2, by = "householdID")

df1 <- subset(df_2015, gender == 1)
df2 <- subset(df_2015, gender == 2)
df_2015 = merge(x = df1, y = df2, by = "householdID")

df <- rbind(df_2011, df_2013, df_2015)



df_temp = df[df[, "year.x"] == 2013,]
df_temp2 = df[df[, "year.x"] == 2011,]
df_temp3 <- rbind(df_temp, df_temp2)
df_temp3$year.x <- df_temp3$year.x + 2
df_temp3 <- select(df_temp3, ID.x, saving.x, year.x)
colnames(df_temp3)[2] <- "previos_saving"

df_saving = merge(x = df, y = df_temp3, by = c("ID.x", "year.x") )
df_saving$delta_saving = df_saving$saving.x - df_saving$previos_saving

fit1 <- lm(saving.x ~ nrsp.x + nrsp.y + factor(ID.x) + factor(year.x), data = df)
summary(fit1)

fit2 <- lm(delta_saving ~ nrsp.x + nrsp.y + factor(ID.x) + factor(year.x), data = df_saving)
summary(fit2)
