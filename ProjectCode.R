
# Importing libraries

library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)

# Importing the csv file
dp <- read_csv("Food_production_dataset.csv")

# Checking summary of columns
summary(dp)

# Checking for missing values in Columns
colSums(is.na(dp))

#Plotting for Outliers
dp1  <- boxplot.stats(dp$Y2000)$out
boxplot(dp$Y2000, main="For the Year 2000", horizontal = T)

dp1  <- boxplot.stats(dp$Y2001)$out
boxplot(dp$Y2001, main="For the Year 2001", horizontal = T)

dp1  <- boxplot.stats(dp$Y2002)$out
boxplot(dp$Y2002, main="For the Year 2002", horizontal = T)

dp1  <- boxplot.stats(dp$Y2003)$out
boxplot(dp$Y2003, main="For the Year 2003", horizontal = T)

dp1  <- boxplot.stats(dp$Y2004)$out
boxplot(dp$Y2004, main="For the Year 2004", horizontal = T)

dp1  <- boxplot.stats(dp$Y2005)$out
boxplot(dp$Y2005, main="For the Year 2005", horizontal = T)

dp1  <- boxplot.stats(dp$Y2006)$out
boxplot(dp$Y2006, main="For the Year 2006", horizontal = T)

dp1  <- boxplot.stats(dp$Y2007)$out
boxplot(dp$Y2007, main="For the Year 2007", horizontal = T)

dp1  <- boxplot.stats(dp$Y2008)$out
boxplot(dp$Y2008, main="For the Year 2008", horizontal = T)

dp1  <- boxplot.stats(dp$Y2009)$out
boxplot(dp$Y2009, main="For the Year 2009", horizontal = T)

dp1  <- boxplot.stats(dp$Y2010)$out
boxplot(dp$Y2010, main="For the Year 2010", horizontal = T)

dp1  <- boxplot.stats(dp$Y2011)$out
boxplot(dp$Y2011, main="For the Year 2011", horizontal = T)

dp1  <- boxplot.stats(dp$Y2012)$out
boxplot(dp$Y2012, main="For the Year 2012", horizontal = T)

dp1  <- boxplot.stats(dp$Y2013)$out
boxplot(dp$Y2013, main="For the Year 2013", horizontal = T)

dp1  <- boxplot.stats(dp$Y2014)$out
boxplot(dp$Y2014, main="For the Year 2014", horizontal = T)

dp1  <- boxplot.stats(dp$Y2015)$out
boxplot(dp$Y2015, main="For the Year 2015", horizontal = T)

dp1  <- boxplot.stats(dp$Y2016)$out
boxplot(dp$Y2016, main="For the Year 2016", horizontal = T)

dp1  <- boxplot.stats(dp$Y2017)$out
boxplot(dp$Y2017, main="For the Year 2017", horizontal = T)

dp1  <- boxplot.stats(dp$Y2018)$out
boxplot(dp$Y2018, main="For the Year 2018", horizontal = T)

dp1  <- boxplot.stats(dp$Y2019)$out
boxplot(dp$Y2019, main="For the Year 2019", horizontal = T)

dp1  <- boxplot.stats(dp$Y2020)$out
boxplot(dp$Y2020, main="For the Year 2020", horizontal = T)

# Plotting Charts for Univariate analysis

par(mfrow=c(2,3))

hist(dp$Y2000,xlab = "Y2000", col=brewer.pal(8,"Greens"),main="Y2000")
hist(dp$Y2005,xlab = "Y2005", col=brewer.pal(8,"Greens"),main="Y2005")
hist(dp$Y2010,xlab = "Y2010", col=brewer.pal(8,"Greens"),main="Y2010")
hist(dp$Y2015,xlab = "Y2015", col=brewer.pal(8,"Greens"),main="Y2015")
hist(dp$Y2019,xlab = "Y2019", col=brewer.pal(8,"Greens"),main="Y2019")
hist(dp$Y2020,xlab = "Y2020", col=brewer.pal(8,"Greens"),main="Y2020")

# Bivariate analysis

# Performing Bivariate analysis

par(mfrow=c(3,3))
ggplot(dp, aes(x = Item, y = Y2000)) + geom_bar(stat = "identity") + coord_flip()
ggplot(dp, aes(x = Item, y = Y2005)) + geom_bar(stat = "identity") + coord_flip()
ggplot(dp, aes(x = Item, y = Y2010)) + geom_bar(stat = "identity") + coord_flip()
ggplot(dp, aes(x = Item, y = Y2015)) + geom_bar(stat = "identity") + coord_flip()
ggplot(dp, aes(x = Item, y = Y2019)) + geom_bar(stat = "identity") + coord_flip()
ggplot(dp, aes(x = Item, y = Y2020)) + geom_bar(stat = "identity") + coord_flip()

# Multivariate Analysis

# Performing Multivariate analyses

par(mfrow=c(2,2))
ggplot(dp, aes(x = Y2000, y = Y2005, color = Item)) + geom_point() +  labs(title = "Individual Items in the years 2000 and 2005")
ggplot(dp, aes(x = Y2010, y = Y2015, color = Item)) + geom_point() +  labs(title = "Individual Items in the years 2010 and 2015")
ggplot(dp, aes(x = Y2015, y = Y2019, color = Item)) + geom_point() +  labs(title = "Individual Items in the years 2015 and 2019")
ggplot(dp, aes(x = Y2019, y = Y2020, color = Item)) + geom_point() +  labs(title = "Individual Items in the years 2019 and 2020")

################################################################################

# Reading CSV file 

ds <- read_csv("CovidAndFood.csv")

ds1 <- ds %>% select(-`Food_exp_perc`) %>% select(- `Time_through_customs`) %>% select(-`Agri_exp_perc`) %>% select(-`Food_imp_perc`) %>% select(-`Food_Production`) %>% select(-`Food_bev_vat`) %>% select(-`...1`)

# Imputing missing values with median values of respective columns.

ds2 <- ds1 %>% mutate(Cases_100k = ifelse(is.na(Cases_100k),median(Cases_100k, na.rm = T),Cases_100k))

ds3 <- ds2 %>% mutate(Deaths_100k = ifelse(is.na(Deaths_100k), median(Deaths_100k, na.rm = T),Deaths_100k))


# Plotting for Outliers
ds4  <- boxplot.stats(ds3$Cases_100k)$out
boxplot(ds3$Cases_100k, main="Detection of Cases", horizontal = T)

ds4  <- boxplot.stats(ds3$Deaths_100k)$out
boxplot(ds3$Deaths_100k, main="Detection of Deaths", horizontal = T)

ds4 <- boxplot.stats(ds3$Cumulative_deaths)$out
boxplot(ds3$Cumulative_deaths, main = "Cumulative Deaths", horizontal = T)

ds4 <- boxplot.stats(ds3$Cumulative_cases)$out
boxplot(ds3$Cumulative_cases, main = "Cumulative Cases", horizontal = T)

ds4 <- boxplot.stats(ds3$New_deaths)$out
boxplot(ds3$New_deaths, main = "Deaths Recorded", horizontal = T)

ds4 <- boxplot.stats(ds3$New_cases)$out
boxplot(ds3$New_cases, main = "Cases Recorded", horizontal = T)



# Removing the Outliers

rem_outliers <- boxplot(ds3$Cases_100k, plot = TRUE)$out 
ds3[-which(ds3$Cases_100k %in% rem_outliers) ,]
boxplot(rem_outliers)

rem_outliers <- boxplot(ds3$Deaths_100k, plot = TRUE)$out 
ds3[-which(ds3$Deaths_100k %in% rem_outliers) ,]
boxplot(rem_outliers)

rem_outliers <- boxplot(ds3$Cumulative_deaths, plot = TRUE)$out 
ds3[-which(ds3$Cumulative_deaths %in% rem_outliers) ,]
boxplot(rem_outliers)

rem_outliers <- boxplot(ds3$Cumulative_cases, plot = TRUE)$out 
ds3[-which(ds3$Cumulative_cases %in% rem_outliers) ,]
boxplot(rem_outliers, horizontal = TRUE)

rem_outliers <- boxplot(ds3$New_deaths, plot = TRUE)$out 
ds3[-which(ds3$New_deaths %in% rem_outliers) ,]
boxplot(rem_outliers, horizontal = TRUE)

rem_outliers <- boxplot(ds3$New_cases, plot = TRUE)$out 
ds3[-which(ds3$New_cases %in% rem_outliers) ,]
boxplot(rem_outliers, horizontal = TRUE)

#rem_outliers

# Plotting Charts for Univariate analysis

# Histogram plots for Numerical columns

par(mfrow=c(2,3))
hist(ds3$New_cases,xlab = "New_cases", col=brewer.pal(8,"Greens"),main="New_cases")
hist(ds3$New_deaths,xlab = "New_deaths", col=brewer.pal(8,"Greens"),main="New_deaths")
hist(ds3$Cumulative_cases,xlab = "Cumulative_cases", col=brewer.pal(8,"Greens"),main="Cumulative_cases")
hist(ds3$Cumulative_deaths,xlab = "Cumulative_deaths", col=brewer.pal(8,"Greens"),main="Cumulative_deaths")

hist(ds3$Cases_100k,xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k")

hist(ds3$Deaths_100k,xlab = "Deaths_100k", col=brewer.pal(8,"Greens"),main="Deaths_100k")

# Performing Multivariate analysis on columns

par(mfrow=c(2,2))
ggplot(ds3, aes(x = New_cases, y = New_deaths, color = Year)) + geom_point()
ggplot(ds3, aes(x = New_cases, y = Cumulative_cases, color = Year)) + geom_point()
ggplot(ds3, aes(x = Cumulative_deaths, y = New_deaths, color = Year)) + geom_point()
ggplot(ds3, aes(x = Cumulative_cases, y = Cumulative_deaths, color = Year)) + geom_point()

par(mfrow = c(2,2))
ggplot(df, aes(x=Cases_100k)) + geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 40)+geom_density(alpha=.2, fill="#FF6666")

ggplot(df, aes(x=Cases_100k)) + geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 25) +geom_density(alpha=.2, fill="#FF6666")

ggplot(df, aes(x=Cases_100k)) + geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 50) +geom_density(alpha=.2, fill="#FF6666")

ggplot(df, aes(x=Cases_100k)) + geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 100) +geom_density(alpha=.2, fill="#FF6666")

par(mfrow = c(2,2))
hist(ds3$Cases_100k,xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k", breaks = 5)
hist(ds3$Cases_100k,xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k", breaks = 25)
hist(ds3$Cases_100k,xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k", breaks = 40)
hist(ds3$Cases_100k,xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k", breaks = 50)

hist(log(ds3$Cases_100k),xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k", breaks = 50)

################################################################################

###################################################### Part 1 #############################################################

# Reading CSV file

df <- read.csv("COVID19_data_cleaned.csv")

df

# Selecting column cases_100k


Mean <- mean(df$Cases_100k)
Mean

Median <- median(df$Cases_100k)
Median

Mode <- mode(df$Cases_100k)
Mode

Range <- range(df$Cases_100k)
Range

SD <- sd(df$Cases_100k)
SD

Var <- var(df$Cases_100k)
Var

Summary <- summary(df$Cases_100k)
Summary


# Histograms for variable Cases_100k using different bin sizes

par(mfrow = c(2,2))

hist(df$Cases_100k,xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k", breaks = 5)

hist(df$Cases_100k,xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k", breaks = 25)

hist(df$Cases_100k,xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k", breaks = 40)

hist(df$Cases_100k,xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k", breaks = 50)


# Histogram using logarithmic scale for variable Cases_100k

par(mfrow = c(2,2))

hist(log(df$Cases_100k),xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k", breaks = 5)

hist(log(df$Cases_100k),xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k", breaks = 25)

hist(log(df$Cases_100k),xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k", breaks = 40)

hist(log(df$Cases_100k),xlab = "Cases_100k", col=brewer.pal(8,"Greens"),main="Cases_100k", breaks = 50)


# Plotting boxplot for variable Cases_100k

boxplot(df$Cases_100k, axes = TRUE, staplewex = 1, col = "lightblue", border = "black", ylab = "Measure")
text(y = boxplot.stats(df$Cases_100k)$stats, labels = boxplot.stats(df$Cases_100k)$stats, x = 1)

# Violin plot for variable Cases_100k
ggplot(df, aes(x="", y = Cases_100k), xName = 'Cases_100k') + 
  geom_violin(adjust=.75, 
              scale='count', 
              draw_quantiles = c(0,0.25,0.5,0.75,1))  + 
  labs(title = "Violin Plots of Cases 100k", 
       y = "Count", 
       x = "Cases 100k")

# Kernel Density functions for variable Cases_100k

ggplot() +
  aes(df$Cases_100k) +
  geom_histogram(aes(y=..density..), bins = 25) +
  stat_density(kernel = "gaussian", bw = 500, fill = NA, col = "red") +
  labs(title = "ggplot: KDF for Cases 100k (kernel : gaussian)")

ggplot() +
  aes(df$Cases_100k) +
  geom_histogram(aes(y=..density..), bins = 25) +
  stat_density(kernel = "rectangular", bw = 500, fill = NA, col = "red") +
  labs(title = "ggplot: KDF for Cases 100k (kernel : Rectangular)")

# Q-Q Plot for variable Cases_100k

qqnorm(df$Cases_100k, pch = 1, frame = FALSE, main = 'Q-Q Plot for Cases 100k')
qqline(df$Cases_100k, col = "red", lwd = 2)

###################################################### Part 2 #############################################################

# Reading CSV file 

AnnualTradeAnnouncementsTally <- read.csv("AnnualTradeAnnouncementsTally.csv")

AnnualTradeAnnouncementsTally


# Statistical summary of # of **new Trade Policies** announced by **country and year** (2020-2022):

newTradePolicies <- AnnualTradeAnnouncementsTally %>%
  mutate(total = rowSums(across(matches(".*total.*"))),
         new = rowSums(across(matches(".*new.*")))) %>%
  select(Year,Country,new) %>%
  filter(Year >= 2015)

new <- newTradePolicies$new
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mean <- mean(new)
mean

median <- median(new)
median

mode <- getmode(new)
mode

range <- range(new)
range

sd <- sd(new)
sd

var <- var(new)
var

Summary <- summary(newTradePolicies %>% select(new))
Summary

Quantile <- quantile(new)
Quantile

IQR <- IQR(new)
IQR

# Histogram

ggplot(newTradePolicies, aes(new)) +
  facet_grid(rows= vars(Year)) +
  geom_histogram(bins = 15) +
  labs(title = "Histogram(New Trade Policies)",
       y = "New Trade Policies",
       x = "Country Count")

ggplot(newTradePolicies, aes(new)) +
  facet_grid(rows= vars(Year)) +
  geom_histogram(bins = 15) +
  scale_x_log10() +
  labs(title = "Histogram(New Trade Policies), Logorithmic Scale",
       y = "New Trade Policies",
       x = "Country Count")

# Violin plot
newTradePolicies %>%
  ggplot(aes(factor(Year), new, fill=Year)) +
  geom_violin(draw_quantiles = c(0,0.25,0.5,0.75,1)) +
  labs(title = "Violin Plots of New Trade Policies by Year",
       y = "New Trade Policies by Country",
       x = "Year")

newTradePolicies %>%
  ggplot(aes(factor(Year), new, fill=Year)) +
  geom_violin(adjust=.75, scale='count', draw_quantiles = c(0,0.25,0.5,0.75,1)) +
  scale_y_log10() +
  labs(title = "Violin Plots of New Trade Policies by Year, Logorithmic Scale",
       y = "New Trade Policies by Country",
       x = "Year")

newTradePolicies %>%
  ggplot(aes(factor(Year), new, fill=Year)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 1,
               size = 2, color = "blue", show.legend = TRUE) +
  scale_y_log10() + labs(title = "Box Plots of New Trade Policies by Year, Logorithmic Scale",
                         y = "New Trade Policies by Country",
                         x = "Year")

# KDF Plots

newTradePolicies %>%
  ggplot(aes(new, fill=Year)) +
  geom_density(kernel="triangular", position='stack') +
  facet_grid(rows=vars(Year)) +
  scale_x_log10() +
  labs(title = "KDF of New Trade Policies by Year, Logorithmic Scale, Triangular KDF")


newTradePolicies %>%
  ggplot(aes(new, fill=Year)) +
  geom_density(kernel="epanechnikov") +
  facet_grid(rows=vars(Year)) +
  scale_x_log10() +
  labs(title = "KDF of New Trade Policies by Year, Logorithmic Scale, epanechnikov KDF")

# Q-Q plot

newTradePolicies %>%
  ggplot(aes(sample = new, group = Year, color=Year)) +
  stat_qq() + stat_qq_line(distribution='geom') +
  labs( x= "Expected", y="Actual", title = "Q-Q Plot of New Trade Policies Per Country, grouped by Year.")


###################################################### Part 3 #############################################################

df <- read.csv("COVID19_data_cleaned.csv")

df

# Selecting column New_cases

Mean <- mean(df$New_cases)
Mean

Median <- median(df$New_cases)
Median

Mode <- mode(df$New_cases)
Mode

Range <- range(df$New_cases)
Range

SD <- sd(df$New_cases)
SD

Var <- var(df$New_cases)
Var

Summary <- summary(df$New_cases)
Summary


# Histograms for variable New_cases using different bin sizes

par(mfrow = c(2,2))

hist(df$New_cases,xlab = "New_cases", col=brewer.pal(8,"Red"),main="New_cases", breaks = 5)

hist(df$New_cases,xlab = "New_cases", col=brewer.pal(8,"blue"),main="New_cases", breaks = 25)

hist(df$New_cases,xlab = "New_cases", col=brewer.pal(8,"blue"),main="New_cases", breaks = 40)

hist(df$New_cases,xlab = "New_cases", col=brewer.pal(8,"blue"),main="New_cases", breaks = 50)


# Histogram using logarithmic scale for variable New_cases

par(mfrow = c(2,2))

hist(log(df$New_cases),xlab = "New_cases", col=brewer.pal(8,"Greens"),main="New_cases", breaks = 5)

hist(log(df$New_cases),xlab = "New_cases", col=brewer.pal(8,"Greens"),main="New_cases", breaks = 25)

hist(log(df$New_cases),xlab = "New_cases", col=brewer.pal(8,"Greens"),main="New_cases", breaks = 40)

hist(log(df$New_cases),xlab = "New_cases", col=brewer.pal(8,"Greens"),main="New_cases", breaks = 50)


# Plotting boxplot for variable New_cases

boxplot(df$New_cases, axes = TRUE, staplewex = 1, col = "lightblue", border = "black", ylab = "Measure")
text(y = boxplot.stats(df$New_cases)$stats, labels = boxplot.stats(df$New_cases)$stats, x = 1)

# Violin plot for variable New_cases
ggplot(df, aes(x="", y = New_cases), xName = 'New_cases') + 
  geom_violin(adjust=.75, 
              scale='count', 
              draw_quantiles = c(0,0.25,0.5,0.75,1))  + 
  labs(title = "Violin Plots of New Cases", 
       y = "Count", 
       x = "New Cases")

# Kernel Density functions for variable New_cases

ggplot() +
  aes(df$New_cases) +
  geom_histogram(aes(y=..density..), bins = 100) +
  stat_density(kernel = "gaussian", bw = 50000, fill = NA, col = "red") +
  labs(title = "ggplot: KDF for New Cases (kernel : gaussian)")

ggplot() +
  aes(df$New_cases) +
  geom_histogram(aes(y=..density..), bins = 100) +
  stat_density(kernel = "rectangular", bw = 50000, fill = NA, col = "red") +
  labs(title = "ggplot: KDF for New Cases (kernel : Rectangular)")

# Q-Q Plot for variable New_cases

qqnorm(df$New_cases, pch = 1, frame = FALSE, main = 'Q-Q Plot for New Cases')
qqline(df$New_cases, col = "red", lwd = 2)


###################################################### Part 4 #############################################################

# Reading CSV file

df <- read.csv("Food_production_dataset.csv")

# Checking summary of columns
summary(df)

# Checking for missing values in Columns
colSums(is.na(df))

df2 <- read.csv("CovidAndFood.csv")

df2

# summaries on 2 datasets. 
# One on meat produced in 2020, the other on new covid cases
# Data is split it up by year, so  summary for 2020, 2021, and 2022

summary(df$Y2020[df$Item=='Meat, Total'])

# there isn't a mode, which makes sense with no two 

# countries making the same amount

which.max(tabulate(df$Y2020[df$Item=='Meat, Total']))
fivenum(df$Y2020[df$Item=='Meat, Total'])
var(df$Y2020[df$Item=='Meat, Total'])
sd(df$Y2020[df$Item=='Meat, Total'])
hist(df$Y2020[df$Item=='Meat, Total'],breaks=25,main="Meat Produced by Country in 2020")
boxplot(df$Y2020[df$Item=='Meat, Total'], main="Box plot of meat produced by country")

#Here is the covid cases data

summary(df2$Cases_100k[df2$Year=='2020'])
which.max(tabulate(df2$Cases_100k[df2$Year=='2020']))
var(df2$Cases_100k[df2$Year=='2020'], na.rm =TRUE)
sd(df2$Cases_100k[df2$Year=='2020'], na.rm =TRUE)
hist(df2$Cases_100k[df2$Year=='2020'],breaks=25,main="Covid cases of 2020 by country")
boxplot(df2$Cases_100k[df2$Year=='2020'], main="Boxplot for Covid cases in 2020")

summary(df2$Cases_100k[df2$Year=='2021'])
which.max(tabulate(df2$Cases_100k[df2$Year=='2021']))
var(df2$Cases_100k[df2$Year=='2021'], na.rm =TRUE)
sd(df2$Cases_100k[df2$Year=='2021'], na.rm =TRUE)
hist(df2$Cases_100k[df2$Year=='2021'],breaks=25,main="Covid cases of 2021 by country")
boxplot(df2$Cases_100k[df2$Year=='2021'], main="Boxplot for Covid cases in 2021")

summary(df2$Cases_100k[df2$Year=='2022'])
which.max(tabulate(df2$Cases_100k[df2$Year=='2022']))
var(df2$Cases_100k[df2$Year=='2022'], na.rm =TRUE)
sd(df2$Cases_100k[df2$Year=='2022'], na.rm =TRUE)
hist(df2$Cases_100k[df2$Year=='2022'],breaks=25,main="Covid cases of 2022 by country")
boxplot(df2$Cases_100k[df2$Year=='2022'], main="Boxplot for Covid cases in 2022")


#Here are KDEs and a Q-Q norm plot

data_no_na <- df2$Cases_100k[df2$Year=='2020']
data_no_na <- data_no_na[!is.na(data_no_na)]
density(data_no_na, kernel='triangular')
plot(density(data_no_na, kernel='gaussian'), main="KDE of 2020 cases")
plot(density(data_no_na, kernel='rectangular'), main="KDE of 2020 cases", col='blue')
qqnorm(data_no_na, main='QQ plot of covid cases in 2020')
qqline(data_no_na, col='red')

###################################################### Part 5 #############################################################

# Reading CSV file

df <- read.csv("COVID19_data_cleaned.csv")

df

# Selecting column Deaths_100k


Mean <- mean(df$Deaths_100k)
Mean

Median <- median(df$Deaths_100k)
Median

Mode <- mode(df$Deaths_100k)
Mode

Range <- range(df$Deaths_100k)
Range

SD <- sd(df$Deaths_100k)
SD

Var <- var(df$Deaths_100k)
Var

Summary <- summary(df$Deaths_100k)
Summary


# Histograms for variable Deaths_100k using different bin sizes

par(mfrow = c(2,2))

hist(df$Deaths_100k,xlab = "Deaths_100k", col=brewer.pal(8,"Greens"),main="Deaths_100k", breaks = 5)

hist(df$Deaths_100k,xlab = "Deaths_100k", col=brewer.pal(8,"Greens"),main="Deaths_100k", breaks = 25)

hist(df$Deaths_100k,xlab = "Deaths_100k", col=brewer.pal(8,"Greens"),main="Deaths_100k", breaks = 40)

hist(df$Deaths_100k,xlab = "Deaths_100k", col=brewer.pal(8,"Greens"),main="Deaths_100k", breaks = 50)


# Histogram using logarithmic scale for variable Deaths_100k

par(mfrow = c(2,2))

hist(log(df$Deaths_100k),xlab = "Deaths_100k", col=brewer.pal(8,"Greens"),main="Deaths_100k", breaks = 5)

hist(log(df$Deaths_100k),xlab = "Deaths_100k", col=brewer.pal(8,"Greens"),main="Deaths_100k", breaks = 25)

hist(log(df$Deaths_100k),xlab = "Deaths_100k", col=brewer.pal(8,"Greens"),main="Deaths_100k", breaks = 40)

hist(log(df$Deaths_100k),xlab = "Deaths_100k", col=brewer.pal(8,"Greens"),main="Deaths_100k", breaks = 50)


# Plotting boxplot for variable Deaths_100k

boxplot(df$Deaths_100k, axes = TRUE, staplewex = 1, col = "lightblue", border = "black", ylab = "Measure")
text(y = boxplot.stats(df$Deaths_100k)$stats, labels = boxplot.stats(df$Deaths_100k)$stats, x = 1)

# Violin plot for variable Deaths_100k
ggplot(df, aes(x="", y = Deaths_100k), xName = 'Deaths_100k') + 
  geom_violin(adjust=.75, 
              scale='count', 
              draw_quantiles = c(0,0.25,0.5,0.75,1))  + 
  labs(title = "Violin Plots of Deaths 100k", 
       y = "Count", 
       x = "Deaths 100k")

# Kernel Density functions for variable Deaths_100k

ggplot() +
  aes(df$Deaths_100k) +
  geom_histogram(aes(y=..density..), bins = 40) +
  stat_density(kernel = "gaussian", bw = 50, fill = NA, col = "red") +
  labs(title = "ggplot: KDF for Deaths 100k (kernel : gaussian)")

ggplot() +
  aes(df$Deaths_100k) +
  geom_histogram(aes(y=..density..), bins = 40) +
  stat_density(kernel = "rectangular", bw = 50, fill = NA, col = "red") +
  labs(title = "ggplot: KDF for Deaths 100k (kernel : Rectangular)")

# Q-Q Plot for variable Deaths_100k

qqnorm(df$Deaths_100k, pch = 1, frame = FALSE, main = 'Q-Q Plot for Deaths 100k')
qqline(df$Deaths_100k, col = "red", lwd = 2)

################################################################################

# Reading data sets

df = read.csv("COVID19_data_cleaned.csv")

head(df)

# Finding the minimum and maximum value of the column Cases_100K for creating appropriate bins 

cases_100k = df$Cases_100k

# Printing maximum and minimum values from column Cases_100k
print(max(cases_100k))
print(min(cases_100k))

# Plotting histogram with bin width as 15
hist_info <- hist(cases_100k, xlab = "Cases_100k",main="Cases_100k", xlim = c(0, 50000), breaks = 15)

hist_info

# Computing density values
hist_info$density <- (hist_info$counts / sum(hist_info$counts))*100

# Plotting density vales keeping freq as False
plot(hist_info, freq = FALSE)

# Defining function to model the data 
fx <- function(x) { # the pdf
  y <- 10*exp(-x*10) # exponential distribution with lambda value = 10
  y[x < 0] <- 0
  return(y)
}
par(new=T)
plot(fx, axes=FALSE, ylab= "", xlab = "") # Plotting pdf on histogram

axis=4 # Plotting axis on the right of pdf

####################################################################################################################################################

#reading data in.
pop_data <- read.csv('population_by_country_2020.csv')
df <- read.csv('country_food_data.csv')

#selecting the data we want to use, merging the two datasets and mutating a new column
meat_produced <- df[df$Item=='Meat, Total',]

new_data <- merge(meat_produced, pop_data, by="Country")

new_data$meat_per_person <- new_data$Y2020 / new_data$Population

# Make a histogram of the food data
hist_info <- hist(new_data$meat_per_person,breaks=50, main = 'Meat Production of Countries per Population %', 
                  xlab="Meat produced in tonnes per person")         # Store output of hist function

hist_info$density <- hist_info$counts /    # Compute density values
  sum(hist_info$counts) * 100

plot(hist_info, freq = FALSE)              # Plot histogram with percentages
# this makes the histogram look like a pdf.

# Here is the function I chose to model the data
fx <- function(x) {   ## The pdf
  y <- 8*exp(-x*8) #exponential distribution, lambda = 8.
  y[x < 0] <- 0 #This is needed to make it a pdf I believe
  return(y) }
par(new=T)
plot(fx, axes=FALSE, ylab="", xlab="") #plots the pdf on top of the histogram
axis(4) #puts the axis for the pdf on the right.
#There are a couple notable outliers, but overall a good fit in my opinion.

#########################################################################################################################################################

# Reading data sets

df = read.csv("Food_production_Dataset_cleaned.csv")

head(df)

# Finding the minimum and maximum value of the column Y2020 for creating appropriate bins 

Y2020 = df$Y2020

# Printing maximum and minimum values from column Y2020
print(max(Y2020))
print(min(Y2020))

# Plotting histogram with bin width as 15
hist_info <- hist(Y2020, xlab = "Year 2020",main="Year 2020", breaks = 15)

hist_info

# Computing density values
hist_info$density <- (hist_info$counts / sum(hist_info$counts))*100

# Plotting density vales keeping freq as False
plot(hist_info, freq = FALSE)

# Defining function to model the data 
fx <- function(x) { # the pdf
  y <- 10*exp(-x*10) # exponential distribution with lambda value = 10
  y[x < 0] <- 0
  return(y)
}
par(new=T)
plot(fx, axes=FALSE, ylab= "", xlab = "") # Plotting pdf on histogram

axis=4 # Plotting axis on the right of pdf

############################################################################################################################################################

AnnualTradeAnnouncementsTally <- read.csv("AnnualTradeAnnouncementsTally.csv")

head(AnnualTradeAnnouncementsTally,5)

totalTradePolicyDistribution <- AnnualTradeAnnouncementsTally %>%
  filter(Year>2019) %>%
  select(Year,Country,Num_imp_total,Num_exp_total,Num_vacc_total)

totalTradePolicyByCountryDistribution <- totalTradePolicyDistribution %>%
  mutate(`Import Policies` = cut(Num_imp_total,breaks=c(-Inf, 1, 10, 100, Inf), 
                                 labels=c("1 or Less","1 to 10","10 to 100", "100+")),
         `Export Policies` = cut(Num_exp_total,breaks=c(-Inf, 1, 10, 100, Inf), 
                                 labels=c("1 or Less","1 to 10","10 to 100", "100+")),
         `Vaccine Policies` = cut(Num_vacc_total,breaks=c(-Inf, 1, 10, 100, Inf), 
                                  labels=c("1 or Less","1 to 10","10 to 100", "100+"))
  ) %>%
  select(Country,Year,`Import Policies`,`Export Policies`,`Vaccine Policies`) %>%
  pivot_longer(cols=c(`Import Policies`,`Export Policies`,`Vaccine Policies`), names_to="Type") %>%
  group_by(`Year`, `Type`, `value`) %>%
  tally() %>%
  rename(Size=value,Value=n)
head(totalTradePolicyByCountryDistribution)


totalTradePolicyByCountryDistribution %>%
  ggplot(aes(x="", y=Value, fill=Size)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y",start=0) +
  facet_grid(rows=vars(Type), cols = vars(Year)) +
  theme_void() +
  scale_fill_discrete(name = "Number of Trade policies per country") +
  theme(strip.text.y = element_text(angle = 0))

totalTradePolicyDistribution %>%
  ggplot(aes(x=Num_imp_total, fill="Number of New Import Policies implemented by country.")) +
  geom_density(bw=2,outline.type="full")

totalTradePolicyDistribution %>%
  ggplot(aes(x=Num_exp_total, fill="Number of New Export Policies implemented by country.")) +
  geom_density(bw=2,outline.type="full")

totalTradePolicyDistribution %>%
  ggplot(aes(x=Num_vacc_total, fill="Number of New vaccine Policies implemented by country.")) +
  geom_density(bw=2,outline.type="full")

###################################################################################################################################################################

# Reading data sets

df = read.csv("AnnualTradeAnnouncementsTally.csv")

head(df)

# Finding the minimum and maximum value of the column Num_exp_total  for creating appropriate bins 

Num_exp_total  = df$Num_exp_total 

# Printing maximum and minimum values from column Num_exp_total 
print(max(Num_exp_total ))
print(min(Num_exp_total ))

# Plotting histogram with bin width as 15
hist_info <- hist(Num_exp_total , xlab = "Num_exp_total ",main="Num_exp_total", breaks = 15)

hist_info

# Computing density values
hist_info$density <- (hist_info$counts / sum(hist_info$counts))*100

# Plotting density vales keeping freq as False
plot(hist_info, freq = FALSE)

# Defining function to model the data 
fx <- function(x) { # the pdf
  y <- 10*exp(-x*10) # exponential distribution with lambda value = 10
  y[x < 0] <- 0
  return(y)
}
par(new=T)
plot(fx, axes=FALSE, ylab= "", xlab = "") # Plotting pdf on histogram

axis=4 # Plotting axis on the right of pdf

################################################################################

# Hypothesis test 1: Food Security Environment Scores 2017 & 2021

# 𝐻0: The null hypothesis for all these tests is going to be that the mean food security score
# of the selected group is equal to the mean food security score of the population of given
# countries.

# 𝐻𝑎: The alternative hypothesis is that the higher income countries have a higher mean than
# the population mean, and the low-income countries are lower. 

study <- read.csv(file = 'FoodSecurityIndex.csv')
df2 <- read.csv('CovidAndFood.csv')
library(tidyverse)

#this one is for different labeled countries in the dataset
low_income_country = c('Mali', 'Rwanda', 'Burkina Faso', 'Malawi', 'Uganda', 'Mozambique', 
                       'Niger', 'Togo', 'Guinea', 'Ethiopia', 'Zambia', 'Chad', 'Congo (Dem. Rep.)',
                       'Sudan', 'Burundi', 'Madagascar', 'Sierra Leone', 'Yemen', 'Syria')

#These are low income countries according to the data by Economist Impact
low_income_country_covid = c('Mali', 'Rwanda', 'Burkina Faso', 'Malawi', 'Uganda', 'Mozambique', 
                             'Niger', 'Togo', 'Guinea', 'Ethiopia', 'Zambia', 'Chad', 'Democratic Republic of the Congo',
                             'Sudan', 'Burundi', 'Madagascar', 'Sierra Leone', 'Yemen', 'Syrian Arab Republic')
#Select the countries I want
lic_foodsecurity = study[study$Country %in% low_income_country,]
#Select the years I want
lic_foodsecurity = select(lic_foodsecurity, X2017, X2021)

#lic_Covid$Country[lic_Covid$Year==2021]

#These are high income countries according to the data by Economist Impact
high_income_country = c('Finland', 'Ireland', 'Norway', 'France', 'Netherlands', 
                        'Japan', 'Canada', 'Sweden', 'United Kingdom', 'Portugal', 'Switzerland', 'Austria', 
                        'United States', 'Denmark', 'New Zealand', 'Czech Republic', 'Belgium', 'Germany', 
                        'Spain', 'Poland', 'Australia', 'United Arab Emirates', 'Israel', 'Chile', 
                        'Italy', 'Singapore', 'Qatar', 'Greece', 'Uruguay', 'Hungary', 'Oman', 
                        'Slovakia', 'Bahrain', 'South Korea', 'Panama', 'Saudi Arabia', 'Romania', 'Kuwait')
#Select the countries I want
hic_foodsecurity = study[study$Country %in% high_income_country,]
#Select the years I want
hic_foodsecurity = select(hic_foodsecurity, X2017, X2021)

#Select data from the covid dataset
hic_Covid <-df2[df2$Country %in% high_income_country,]
hic_Covid = hic_Covid[hic_Covid$Year==2020,]
lic_Covid <- df2[df2$Country %in% low_income_country_covid,]
lic_Covid = lic_Covid[lic_Covid$Year==2020,]


#Here are summaries of all the data used.
study = select(study, X2017, X2021)
summary(study)
summary(hic_foodsecurity)
summary(lic_foodsecurity)
sd(study$X2017)
sd(study$X2021)
sd(lic_foodsecurity$X2017)
sd(lic_foodsecurity$X2021)

summary(df2$Cases_100k[df2$Year=='2020'])
summary(hic_Covid$Cases_100k)
summary(lic_Covid$Cases_100k)
sd(df2$Cases_100k[df2$Year=='2020'], na.rm =TRUE)
sd(lic_Covid$Cases_100k, na.rm =TRUE)

#Here are the Hypothesis tests
z_2017 <- (mean(hic_foodsecurity$X2017) - mean(study$X2017))/
  (sd(study$X2017)/sqrt(nrow(hic_foodsecurity)))
z_2021 <- (mean(hic_foodsecurity$X2021) - mean(study$X2021))/
  (sd(study$X2021)/sqrt(nrow(hic_foodsecurity)))

t_2017 <- t.test(x=lic_foodsecurity$X2017,
                 alternative = 'less',
                 mu = mean(study$X2017),
                 conf.level = 0.99)
t_2021 <- t.test(x=lic_foodsecurity$X2021,
                 alternative = 'less',
                 mu = mean(study$X2021),
                 conf.level = 0.99)

covid_test_high <- (mean(hic_Covid$Cases_100k, na.rm = TRUE) - 
                      mean(df2$Cases_100k[df2$Year=='2020'], na.rm=TRUE))/
  (sd(df2$Cases_100k[df2$Year=='2020'], na.rm = TRUE)/sqrt(nrow(hic_Covid)))
covid_test_low <- t.test(x=lic_Covid$Cases_100k,
                         alternative = 'less',
                         mu = mean(df2$Cases_100k[df2$Year=='2020'], na.rm = TRUE),
                         conf.level = 0.99)
#Here are the results
z_2017
z_2021
t_2017
t_2021
covid_test_high
covid_test_low


#Creating the plots
all_hist <- hist(study$X2021, breaks=20, plot=FALSE)
low_hist <- hist(lic_foodsecurity$X2021, breaks=10, plot=FALSE)
high_hist <- hist(hic_foodsecurity$X2021, breaks=10, plot=FALSE)
plot(all_hist,main="Food Security Environment Scores 2021", xlab="Score", xlim = c(20,100), ylim = c(0,10))
plot(low_hist, col='red',add=TRUE)
plot(high_hist, col='blue', add=TRUE)

all_hist <- hist(study$X2017, breaks=20, plot=FALSE)
low_hist <- hist(lic_foodsecurity$X2017, breaks=10, plot=FALSE)
high_hist <- hist(hic_foodsecurity$X2017, breaks=10, plot=FALSE)
plot(all_hist,main="Food Security Environment Scores 2017", xlab="Score", xlim = c(20,100), ylim = c(0,10))
plot(low_hist, col='red',add=TRUE)
plot(high_hist, col='blue', add=TRUE)

# Hypothesis test 2:  Trade Policies before and after COVID-19

# Chi-squared test

# Ha: The number of trade policies issued globally each year was significantly altered by the outbreak of Covid in the beginning of 2020.
# Ho: The number of trade policies issued globally each year was not significantly altered by the outbreak of Covid in the beginning of 2020

library(tidyverse)
TradePolicies <- read.csv("AnnualTradeAnnouncementsTally.csv")
CovidAndFood <- read.csv("CovidAndFood.csv")

TotalTrade <- TradePolicies %>%
  group_by(`Year`) %>%
  summarize(
    Trade = sum(`Num_exp_total`) + sum(`Num_imp_total`)
  )

# Running chisq test.

tradefreq <- TotalTrade$Trade

chi_2 <- chisq.test(tradefreq)
sprintf('Chi-Squared(%d,N=%d) = %5.2f, p = %5.4f', chi_2$parameter, sum(tradefreq), chi_2$statistic, chi_2$p.value)

# By the Chi Squared test, the distribution of trade policies is clearly not symmetric, with P<.00001. 
# Thus, the number of trade policies issued varies significantly.Hence, we reject null hypothesis.

# Hypothesis test 3:  Trade Policies before and after COVID-19

#	Ho: The annual number of trade policies announced globally was not significantly different before / after the outbreak of Covid.
#	Ha: The annual number of trade policies announced globally was significantly different before / after the outbreak of Covid.


# running a 2-factor t test to estimate how COVID modulates the trade policy distribution.

tradePre <- tradefreq[1:5]
tradePost <- tradefreq[6:8]

test <- t.test(tradePre, tradePost)
sprintf('T-test(%f,N=%d) = %5.2f, p = %5.4f', test$parameter, 9, test$statistic, test$p.value)

# The number of trade policies issued globally by year was significantly affected by the outbreak of Covid by a t-test, p<.05.

TotalTrade <- TotalTrade %>%
  mutate(
    postCovid = Year > 2019
  )

ggplot(TotalTrade, aes(x=Year, y=Trade,fill=postCovid)) +
  geom_col()

