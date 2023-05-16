#Loading csv file to R
df <- read.csv("https://public.tableau.com/app/sample-data/HollywoodsMostProfitableStories.csv")

#Viewing the data as a table
View(df)

#Installing tidyverse to R Studio
install.packages("tidyverse")

#Loading tidyverse to R Studio
library(tidyverse)

#Checking data types
str(df)

#checking for missing values
colSums(is.na(df))

#removing missing values
df <- na.omit(df)

#checking if the missing values were removed
colSums(is.na(df))

#checking for duplicates
dim(df[duplicated(df$Film),])[1]

#rounding off values to 2 decimal places
df$Profitability <- round(df$Profitability ,digit=2)
df$Worldwide.Gross <- round(df$Worldwide.Gross ,digit=2)

#viewing the table if values rounded off
View(df)

#checking column and rows count
dim(df)

#loading ggplot2 package
library(ggplot2)

#Create a boxplot that highlights the outliers   
ggplot(df, aes(x=Profitability, y=Worldwide.Gross))+ geom_boxplot(outlier.colour = "red", outlier.shape = 1)+ scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 1000))

#Remove outliers in 'Profitability' 
Q1 <- quantile(df$Profitability, .25)
Q3 <- quantile(df$Profitability, .75)
IQR <- IQR(df$Profitability)
no_outliers <- subset(df, df$Profitability> (Q1 - 1.5*IQR) & df$Profitability< (Q3 + 1.5*IQR))

#checking column and rows count in no_outliers
dim(no_outliers)

# Remove outliers in 'Worldwide.Gross' 
Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)
IQR <- IQR(no_outliers$Worldwide.Gross)
df1 <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))

#checking column and rows count in df1
dim(df1)

#Summary Statistics/Univariate Analysis:
summary(df1)

#bivariate analysis
#with a scatterplot
ggplot(df1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

# and a bar chart
ggplot(df1, aes(x=Year)) + geom_bar()

#Export clean data 

write.csv(df1, "clean_df.csv")



































































"hello world"

install.packages("tidyverse")

library(tidyverse)

df <- read.csv("C:/Users/shyme/Downloads/Data Set- Inc5000 Company List_2014.csv")

na.omit(year)
mean(year)

mean(na.omit(year))

dim(dftb)

str(df)

ggplot(no_outliers, aes(x=Country.or.territory.name, y=Estimated.prevalence.of.TB..all.forms..per.100.000.population)) + geom_point()+ scale_y_continuous(labels=scales::comma)

death <- c(dftb$Estimated.mortality.of.TB.cases..all.forms..excluding.HIV..per.100.000.population)
TB.Prevalence <- c(dftb$Estimated.prevalence.of.TB..all.forms..per.100.000.population)
Country <- c(dftb$Country.or.territory.name)
value <- abs(rnorm(3030, 2, 12))
cause <- rep(c("death", "TB.Prevalence"))
data <- data.frame(Country, cause, value)

ggplot(data, aes(fill=cause, y=value, x=Country))+ scale_x_discrete(guide = guide_axis(angle = 90)) + geom_col(width = 0.5, position = position_dodge(0.7))

ggplot(no_outliers, aes(x=yrs_on_list)) + geom_bar()+coord_cartesian(ylim = c(0, 1500), xlim=c(0,15))

#removing outliers:

#Remove outliers in 'prevalence.all.low.bound'
Q1 <- quantile(dftb$prevalence.all.low.bound, .25)
Q3 <- quantile(dftb$prevalence.all.low.bound, .75)
#inter quartile range
IQR <- IQR(dftb$prevalence.all.low.bound)

no_outliers <- subset(dftb, dftb$prevalence.all.low.bound> (Q1 - 1.5*IQR) & dftb$prevalence.all.low.bound< (Q3 + 1.5*IQR))

dim(no_outliers)
 

# Remove outliers in 'revenue'
Q1 <- quantile(no_outliers$revenue, .25)
Q3 <- quantile(no_outliers$revenue, .75)
IQR <- IQR(no_outliers$revenue)

no_outliers <- subset(no_outliers, no_outliers$revenue> (Q1 - 1.5*IQR) & no_outliers$revenue< (Q3 + 1.5*IQR))

dim(no_outliers) 

#Summary Statistics:
summary(no_outliers)

#bivariate analysis

ggplot(no_outliers, aes(x=rank, y=workers)) + geom_point()+ scale_y_continuous(labels=scales::comma)+coord_cartesian(ylim = c(0, 300))

ggplot(no_outliers, aes(x=yrs_on_list)) + geom_bar()+coord_cartesian(ylim = c(0, 1500), xlim=c(0,15))

#Export clean data
write.csv(no_outliers, "clean_df.csv")


cor(no_outliers$growth,no_outliers$revenue)

#shows how many missing values are there in each column
colSums(is.na(dftb))


#removing rows with missing values
dftb <- na.omit(dftb)

#shows column names
colnames(dftb)

view(dftb)

dftb%>%
  rename(
    country = Country.or.territory.name,
    iso_2 = ISO.2.character.country.territory.code,
    iso_3 = ISO.3.character.country.territory.code,
    iso_numeric = ISO.numeric.country.territory.code
  )

colnames(dftb)[13] = "prevalence.all.low.bound"

colnames(dftb)
[1] = "Country"                                                                     
[2] = "Country.Code"                                                        
[3] = "Country.Code2"                                                        
[4] = "Country.Code3"                                                            
[7] = "Population"                                                             
[8] = "TB.Prevalence.per.100.000"                                 
[9] = "TB.Prevalence.per.100.000.low.bound"                      
[10] = "TB.Prevalence.of.TB.per.100.000.high.bound"                     
[11] = "TB.Prevalence"                                                        
[12] = "TB.Prevalence.low.bound"                                             
[13] = "Prevalence.all.low.bound"                                                                      
[14] = "Method.to.derive.prevalence.estimates"                                                         
[15] = "TB.Mortality.per.100.000"             
[16] = "TB.Mortality.per.100.000.low.bound" 
[17] = "TB.Mortality.per.100.000.high.bound"
[18] = "TB.number.of.deaths"                                 
[19] = "TB.number.of.deaths.low.bound"                      
[20] = "TB.number.of.deaths.high.bound"                     
[21] = "TB.mortality.who.are.HIV.positive.per.100.000"                  
[22] = "TB.mortality.who.are.HIV.positive.per.100.000.low.bound"       
[23] = "TB.mortality.who.are.HIV.positive.per.100.000.high.bound"      
[24] = "TB.deaths.in.people.who.are.HIV.positive"                             
[25] = "TB.deaths.in.people.who.are.HIV.positive.low.bound"                  
[26] = "TB.deaths.in.people.who.are.HIV.positive.high.bound"                 
[27] = "Method.to.derive.mortality.estimates"                                                          
[28] = "Incidence.all.forms.per.100.000"                                        
[29] = "Incidence.all.forms.per.100.000.low.bound"                             
[30] = "Incidence.all.forms.per.100.000.high.bound"                            
[31] = "Number.of.incident.cases.all.forms"                                                
[32] = "Number.of.incident.cases.all.forms.low.bound"                                     
[33] = "Number.of.incident.cases.all.forms.high.bound"                                    
[34] = "Method.to.derive.incidence.estimates"                                                          
[35] = "HIV.in.incident.TB..percent."                                                        
[36] = "HIV.in.incident.TB..percent...low.bound"                                             
[37] = "HIV.in.incident.TB..percent...high.bound"                                            
[38] = "incidence.of.TB.who.are.HIV.positive.per.100.000"                   
[39] = "incidence.of.TB.who.are.HIV.positive.per.100.000.low.bound"        
[40] = "incidence.of.TB.who.are.HIV.positive.per.100.000.high.bound"       
[41] = "incidence.of.TB.who.are.HIV.positive"                                          
[42] = "incidence.of.TB.who.are.HIV.positive..low.bound"                               
[43] = "incidence.of.TB.who.are.HIV.positive..high.bound"                              
[44] = "Method.to.derive.TBHIV.estimates"                                                              
[45] = "detection.rate.all.forms.percent"                                                      
[46] = "detection.rate.all.forms.percent.low.bound"                                           
[47] = "detection.rate.all.forms.percent.high.bound" 

#checking for missing values within dataframe 
colSums(is.na(dftb))

dim(dftb)

# Check data types:

str(df)

dim(df) 

drops <- c("Method.to.derive.TBHIV.estimates")
dftb <- dftb[ ,!(names(dftb) %in% drops)]

# Check for missing values
colSums(is.na(df))



#Drop missing values (creating a subset drops)
drops <- c('X_input')

df <- df[ ,!(names(df) %in% drops)]

# check to make sure that the column has been removed
head(df)

#Remove missing values in rows
df <- na.omit(df)

#Check for duplicates


# Remove duplicated rows based on a given column
df <-df %>% distinct(id, .keep_all = TRUE)
dim(df)

#round off values to 2 places

df$growth <- round(df$growth ,digit=2)

head(df)

#Check for outliers using a boxplot

library(ggplot2)

#remove outlier

#Create a boxplot that labels the outliers  
ggplot(df, aes(x=revenue, y=growth)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1)+ scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 1000))

#Remove outliers in 'growth'
Q1 <- quantile(df$growth, .25)
Q3 <- quantile(df$growth, .75)
#inter quartile range
IQR <- IQR(df$growth)

no_outliers <- subset(df, df$growth> (Q1 - 1.5*IQR) & df$growth< (Q3 + 1.5*IQR))

dim(no_outliers) 

# Remove outliers in 'revenue'
Q1 <- quantile(no_outliers$revenue, .25)
Q3 <- quantile(no_outliers$revenue, .75)
IQR <- IQR(no_outliers$revenue)

no_outliers <- subset(no_outliers, no_outliers$revenue> (Q1 - 1.5*IQR) & no_outliers$revenue< (Q3 + 1.5*IQR))

dim(no_outliers) 

#Summary Statistics:
summary(no_outliers)

#bivariate analysis

ggplot(no_outliers, aes(x=rank, y=workers)) + geom_point()+ scale_y_continuous(labels=scales::comma)+coord_cartesian(ylim = c(0, 300))

ggplot(no_outliers, aes(x=yrs_on_list)) + geom_bar()+coord_cartesian(ylim = c(0, 1500), xlim=c(0,15))

#Export clean data
write.csv(no_outliers, "clean_df.csv")


cor(no_outliers$growth,no_outliers$revenue)

#Filtering is a vast topic because there are many different types of filters and variations of these filters

order(no_outliers$growth)

library(tidyverse)




p_data(datasets)
if (!require("pacman")) install.packages("pacman")
p_data(base)
p_data("pacman")
pacman::p_load(pacman, rio, tidyverse)
library(datasets)

data()

iris
titanic
Titanic

view(Titanic)

p_data(ggplot2)

x <- c(5, 15, 25, 36, 18, 50)
barplot(x)

barplot(x, col ="skyblue") #color names
barplot(x, col = rgb(.98, .94, .90)) # rgb triplets (0.00-1.00)
barplot(x, col=rgb(135, 150, 200, max=250)) #rgb triplets (0-255)
barplot(x, col="#FAF0E6") #rgb hexcodes
barplot(x, col=colors() [600]) #index numbers for colors
barplot(x, col=c("skyblue", "linen")) #seversl colours in a vector to cycle

?palette #info on built in color palettes
palette() # see current palette

barplot(x, col=1:6) #use of current palette.
barplot(x, col=rainbow(6)) #rainbow colors
barplot(x, col=heat.colors(6)) #yellow through red 
barplot(x, col=terrain.colors(6)) #grey through green
barplot(x, col=topo.colors(6)) # purpure through tan
barplot(x, col=cm.colors(6)) #pinks and blues

p_load(RColorBrewer) #loading color brewer from colorbrewer.org
?RColorBrewer
display.brewer.all()

palette1 <- c("lightcyan", "orange2", "salmon", "tan", "red") #defining own palette. 
barplot(x, col=palette1)

HairEyeColor
df <- HairEyeColor %>%
  as_tibble()%>%
  print() 

df <- read.csv("https://public.tableau.com/app/sample-data/HollywoodsMostProfitableStories.csv")
View(df)
install.packages("tidyverse")
library(tidyverse)
str(df)
colSums(is.na(df))
df <- na.omit(df)
colSums(is.na(df))
dim(df[duplicated(df$Film),])[1]
df$Profitability <- round(df$Profitability ,digit=2)
df$Worldwide.Gross <- round(df$Worldwide.Gross ,digit=2)
dim(df)
library(ggplot2)
ggplot(df, aes(x=Profitability, y=Worldwide.Gross))+ geom_boxplot(outlier.colour = "red", outlier.shape = 1)+ scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 1000))
Q1 <- quantile(df$Profitability, .25)
Q3 <- quantile(df$Profitability, .75)
IQR <- IQR(df$Profitability)
no_outliers <- subset(df, df$Profitability> (Q1 - 1.5*IQR) & df$Profitability< (Q3 + 1.5*IQR))
dim(no_outliers)
Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)
IQR <- IQR(no_outliers$Worldwide.Gross)
df1 <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))
dim(df1)
summary(df1)
ggplot(df1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))
ggplot(df1, aes(x=Year)) + geom_bar()
write.csv(df1, "clean_df.csv")























