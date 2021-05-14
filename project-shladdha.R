#Author: Shripad Laddha
#net id: shladdha
#SUID: 711417912
#IST 719 spring 2021 Smoking Poster project script

rm(list=ls())
my.dir<-"C:/Users/Shripad/Desktop/MS/IST719/PROJECT/data final/deaths/"
library(maps)
library(ggplot2)
library("dplyr")

#####################################################################################
###What is the percent share of deaths caused due to smoking worldwide?
deaths<-read.csv(paste0(my.dir, "share-deaths-smoking.csv"), header = TRUE, 
                stringsAsFactors = FALSE, na.strings=c("","NA"))
View(deaths)
str(deaths)
summary(deaths)
colSums(is.na(deaths))

#subsetting to take only the year 2015 for each country
deaths.2015<-deaths[which(deaths$Year== 2015),]
# Rename column where names is "Entity" to region
names(deaths.2015)[names(deaths.2015) == "Entity"] <- "region"
deaths.2015 %>% slice_max(Smoking..IHME..2019., n = 10)#top 10 countries by death

world_map <- map_data("world")
death.map <- left_join(deaths.2015, world_map, by = "region")

p<-ggplot(death.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Smoking..IHME..2019.), color = "#404040")
p+scale_fill_gradient(low='yellow', high='red')+theme_dark()

########################################################
###What is the age group of people belonging to top 10 countries by death in 2015

my.dir2<-"C:/Users/Shripad/Desktop/MS/IST719/PROJECT/data final/deathage/"
deathage<-read.csv(paste0(my.dir2, "death-rates-smoking-age.csv"), header = TRUE, 
                 stringsAsFactors = FALSE, na.strings=c("","NA"))
View(deathage)
str(deathage)
summary(deathage)
colSums(is.na(deathage))

deathage.2015<-deathage[which(deathage$Year== 2015),]
names(deathage.2015)[names(deathage.2015) == "Entity"] <- "country"
names(deathage.2015)[names(deathage.2015) == "Deaths...Smoking...Sex..Both...Age..Under.5..Rate."] <- "Below_5_years"
names(deathage.2015)[names(deathage.2015) == "Deaths...Smoking...Sex..Both...Age..5.14.years..Rate."] <- "5_to_14_years"
names(deathage.2015)[names(deathage.2015) == "Deaths...Smoking...Sex..Both...Age..15.49.years..Rate."] <- "15_to_49_years"
names(deathage.2015)[names(deathage.2015) == "Deaths...Smoking...Sex..Both...Age..50.69.years..Rate."] <- "50_to_69_years"
names(deathage.2015)[names(deathage.2015) == "Deaths...Smoking...Sex..Both...Age..70..years..Rate."] <- "70_and_above_years"

deathage.2015.filtered<-subset(deathage.2015 , country == "Montenegro" | 
                                 country == "Greenland" |
                                 country == "Denmark" |
                                 country == "North Macedonia" |
                                 country == "Bosnia and Herzegovina" |
                                 country == "Greece" |
                                 country == "China" |
                                 country == "East Asia" |
                                 country == "Netherlands" |
                                 country == "Hungary" )
View(deathage.2015.filtered)
deathage.2015.filtered <- deathage.2015.filtered[c(1,2,3,4,5,6,8,7,9)] #reordering columns
yy<-c()
yy$yy<-c(17.2950, 338.7848, 1238.2727)
xx<-barplot((colSums(deathage.2015.filtered[,7:9]))/10,
            col = "#d9992b", main="Average Deaths by age for top 10 countries",
            xlab="Age Groups",
            ylab="Deaths per 100000",ylim = c(0,1400),
            border = NA)
#dividing by 10 because its the total of 10 countries
text(x = xx, y = yy$yy, label = yy$yy, pos = 3, cex = 0.8, col = "black")


#################################################################
###How does death vary with respect to gender for top 10 countries in 2015

my.dir3<-"C:/Users/Shripad/Desktop/MS/IST719/PROJECT/data final/gender/"
deathgender<-read.csv(paste0(my.dir3, "comparing-the-share-of-men-and-women-who-are-smoking.csv"), header = TRUE, 
                   stringsAsFactors = FALSE, na.strings=c("","NA"))
View(deathgender)
str(deathgender)
summary(deathgender)
colSums(is.na(deathgender))

deathgender.2015<-deathgender[which(deathgender$Year== 2015),]
names(deathgender.2015)[names(deathgender.2015) == "Entity"] <- "country"
View(deathgender.2015)
names(deathgender.2015)[names(deathgender.2015) == "Smoking.prevalence..females....of.adults."] <- "Smoking_prevalence_females"
names(deathgender.2015)[names(deathgender.2015) == "Smoking.prevalence..males....of.adults."] <- "Smoking_prevalence_males"

deathgender.2015.filtered<-subset(deathgender.2015 , country == "Montenegro" | 
                                 country == "Denmark" |
                                 country == "Bosnia and Herzegovina" |
                                 country == "Greece" |
                                 country == "China" |
                                 country == "East Asia & Pacific" |
                                 country == "Netherlands" |
                                 country == "Hungary"|
                                 country == "Serbia"|
                                 country == "Turkey" )
View(deathgender.2015.filtered)
barplot(deathgender.2015.filtered$Smoking_prevalence_males, 
        names=deathgender.2015.filtered$country, col="#d9992b",
        horiz=T, las=1, 
        main="Male Smoking prevalence for Top 10 countries by death",
        border=NA)

barplot(deathgender.2015.filtered$Smoking_prevalence_females, 
        names=deathgender.2015.filtered$country, col="#d9992b",
        horiz=T, las=1, 
        main="Female Smoking prevalence for Top 10 countries by death",
        border=NA)

##############################################################
###How do insurance vary according to smoking status


my.dir4<-"C:/Users/Shripad/Desktop/MS/IST719/PROJECT/data final/"
insurance<-read.csv(paste0(my.dir4, "insurance.csv"), header = TRUE, 
                      stringsAsFactors = FALSE, na.strings=c("","NA"))
View(insurance)
str(insurance)
summary(insurance)
colSums(is.na(insurance))
dim(insurance)

barplot(table(insurance$children))
barplot(table(insurance$age))
barplot(table(insurance$sex))
boxplot(insurance$bmi)
boxplot(insurance$charges)
barplot(table(insurance$region))
pie(table(insurance$smoker))

boxplot(insurance$charges ~insurance$children)
boxplot(insurance$charges ~insurance$sex)
boxplot(insurance$charges ~insurance$smoker)

ggplot(insurance, aes(x = smoker, y = charges, 
                 fill = sex)) + geom_boxplot()

d<-ggplot(data = insurance, aes(x = charges, fill = smoker)) + 
  geom_density(alpha = 0.5, color=FALSE) + 
  ggtitle("Distribution of Charges per Smoking Category")
d+scale_fill_manual(values=c("#A9A9A9", "#E69F00", "#56B4E9"))+theme_classic()

######################################################################
###how are charges affected by bmi and smoker status

p <- ggplot(data = insurance, aes_string(x = 'bmi', y = 'charges', group = 'smoker', fill = 'smoker', col = 'smoker')) + 
    geom_point() + 
    geom_smooth(method = 'lm',se=FALSE) +
    ggtitle("Charges vs BMI for smoker and non smoker") 
p+scale_color_manual(values = c("no" = "#A9A9A9", "yes" = "#d9992b"))+theme_classic()


########################################################################
##taxes on top 10 coutries by death

my.dir5<-"C:/Users/Shripad/Desktop/MS/IST719/PROJECT/data final/taxes/"
tax<-read.csv(paste0(my.dir5, "taxes-as-share-of-cigarette-price.csv"), header = TRUE, 
                    stringsAsFactors = FALSE, na.strings=c("","NA"))
str(tax)
summary(tax)
colSums(is.na(tax))
dim(tax)

tax.2014<-tax[which(tax$Year== 2014),]
names(tax.2014)[names(tax.2014) == "Entity"] <- "country"
names(tax.2014)[names(tax.2014) == "Indicator.Average...taxes.as.a...of.cigarette.price...total.tax"] <- "tax_as_percent_of_cig_price"

tax.2014.filtered<-subset(tax.2014 , country == "North Macedonia" | 
                                    country == "Denmark" |
                                    country == "Bosnia and Herzegovina" |
                                    country == "Greece" |
                                    country == "China" |
                                    country == "Poland" |
                                    country == "Netherlands" |
                                    country == "Hungary"|
                                    country == "Serbia"|
                                    country == "Turkey" )
View(tax.2014.filtered)

xx<-barplot(sort(tax.2014.filtered$tax_as_percent_of_cig_price, decreasing = TRUE), 
        names=tax.2014.filtered$country, col="#d9992b",
        main="Tax as % of cigarette price in top 10 countries by death",
        border=NA, ylim = c(0,100))

yy<-c()
yy$yy<-c(86.0,82.6,82.2,81.6,77.8,77.4,74.8,73.8,69.8,47.8)
text(x = xx, y = yy$yy, label = yy$yy, pos = 3, cex = 0.8, col = "black")
