
# To make sure script run,add this folder as your working path

# Check packages and install them
if(!require("openxlsx")) install.packages("openxlsx")
library(openxlsx)

if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if(!require("here")) install.packages("here")
library(here)

if(!require("grid")) install.packages("grid")
library(grid)

if(!require("gridExtra")) install.packages("gridExtra")
library(gridExtra)

if(!require("ggpubr")) install.packages("ggpubr")
library(ggpubr)

if(!require("ggmap")) install.packages("ggmap")
library(ggmap)


#load the data set and the exactly sheet will use later.
data1<- read.xlsx(here("Raw_Data/publishedweek522021.xlsx"), sheet = 9)
head(data1)


####################The First Figure  #########################################
#

# Data pre-processing#

# Selects the data for the specified row.

Weekly_Death <- slice(data1,2L,4L)

# Convert this data into a data set that ggPlot can use.

# make the first row the name of the column, 
rownames(Weekly_Death)=Weekly_Death[,1] 
Weekly_Death=Weekly_Death[,-1]
# transpose the data, 
Weekly_Death <-t(Weekly_Death)
# convert the matrix to the data set, 
Weekly_Death <-as.data.frame(Weekly_Death)
# remove missing values
Weekly_Death<- na.omit(Weekly_Death)

# Rename column
colnames(Weekly_Death)
names(Weekly_Death)[names(Weekly_Death) =="Week number"] <-"Week"

names(Weekly_Death)[names(Weekly_Death) =="UK deaths involving COVID-19 7"] <-"Deaths"
# convert data into a numerical format
Weekly_Death = as.data.frame(lapply(Weekly_Death,as.numeric))


head(Weekly_Death)



# Use ggplot2 to draw a bar graph with weeks on the horizontal and deaths on the vertical.
# Use "fill" to control the fill color and "color" to control the border color
p1 <- ggplot(Weekly_Death,aes(x=Week, y=Deaths))+
  
  geom_bar(stat='identity',fill="red", colour="black")

#add a title for the figure.
p1 <- p1 +
  
  ggtitle("UK weekly deaths involving COVID-19 in 2021") +
  
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

p1

# save the first figure
ggsave('Weekly_deaths_in_general.png', 
       plot = p1, path = here("Figure/"),
       width = 260,
       height = 109,
       units = "mm",
       dpi = 300)



####################The Second Figure  #########################################

# Data pre-processing #
# (Same process as in the last part)

AgeGroup_Deaths <- slice(data1,2L,13L,14L,15L,16L,17L,18L,19L)
AgeGroup_Deaths <- AgeGroup_Deaths[,-1]
AgeGroup_Deaths[is.na(AgeGroup_Deaths)]<-"Week"
rownames(AgeGroup_Deaths)=AgeGroup_Deaths[,1] 
AgeGroup_Deaths <- AgeGroup_Deaths[,-1]
AgeGroup_Deaths <- t(AgeGroup_Deaths)
AgeGroup_Deaths <-as.data.frame(AgeGroup_Deaths)

AgeGroup_Deaths = as.data.frame(lapply(AgeGroup_Deaths,as.numeric))


colnames(AgeGroup_Deaths)
names(AgeGroup_Deaths)[names(AgeGroup_Deaths) =="Under.1.year"] <-"< 1"
names(AgeGroup_Deaths)[names(AgeGroup_Deaths) =="X01.14"] <-"1-14"
names(AgeGroup_Deaths)[names(AgeGroup_Deaths) =="X15.44"] <-"15-44"
names(AgeGroup_Deaths)[names(AgeGroup_Deaths) =="X45.64"] <-"45-64"
names(AgeGroup_Deaths)[names(AgeGroup_Deaths) =="X65.74"] <-"65-74"
names(AgeGroup_Deaths)[names(AgeGroup_Deaths) =="X75.84"] <-"75-84"
names(AgeGroup_Deaths)[names(AgeGroup_Deaths) =="X85."] <-"> 85"

AgeGroup_Deaths$"0-14" <- rowSums(AgeGroup_Deaths[,c(2,3)])
AgeGroup_Deaths$"> 65" <- rowSums(AgeGroup_Deaths[,c(6:8)])

AgeGroup_Deaths <- AgeGroup_Deaths[,c(-2:-3,-6:-8)]

AgeGroup_Deaths <- AgeGroup_Deaths[,c("Week","0-14","15-44","45-64","> 65")]


# I did this in Excel because of the hassle of using R to consolidate multiple columns of data. 

write.csv(AgeGroup_Deaths,here("Raw_data/AgeGroup_Deaths.csv"))

# This might be done using the "paste" function.
AgeGroup_Deaths<- read.xlsx(here("Raw_Data/AgeGroup_Deaths.xlsx"), sheet = 1)
AgeGroup_Deaths <- AgeGroup_Deaths[,-1]



# Plotting #
# To achieve the stacking effect, adjust the bar chart using the "fill" and "stack" parameters.
p2 <- ggplot(data = AgeGroup_Deaths, 
             mapping = aes(
               x =Week, 
               y= Deaths, 
               fill = Age.Group))+
  geom_bar(stat = 'identity', position = 'stack')

p2 <- p2 +
  
  ggtitle("Deaths Involving COVID-19 Grouping by Age") +
  
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

p2

ggsave('Deaths_involving_COVID19_by_age_Group.png',
       plot = p2, path = here("Figure/"),
       width = 260,
       height = 109,
       units = "mm",
       dpi = 300)



#####################################The Third Figure########################################################

# Data Pre-processing #

# Import data from another sheet
data2<- read.xlsx(here("Raw_Data/publishedweek522021.xlsx"), sheet = 14)

head(data2)

Excess_Deaths <-  slice(data2,3L,47L:98L)
colnames(Excess_Deaths)=Excess_Deaths[1,]
Excess_Deaths <- Excess_Deaths[-1,]
colnames(Excess_Deaths)
names(Excess_Deaths)[6] <-"Home"
names(Excess_Deaths)[10] <-"Hospital"
names(Excess_Deaths)[14] <-"CareHome"
names(Excess_Deaths)[18] <-"Other"
Excess_Deaths$"Week" <- (1:52)
Excess_Deaths <- Excess_Deaths[,c(-1:-5,-7:-9,-11:-13,-15:-17)]
Excess_Deaths = as.data.frame(lapply(Excess_Deaths,as.numeric))

# sum, will used in the forth figure
Excess_Deaths$"InTotal" <- rowSums(Excess_Deaths[,c(1:4)])

# Plotting 4 figures for each place, and a figure for total

# Using "xlab" and "ylab" to define the axes, 
p3_1 <- ggplot(data = Excess_Deaths, mapping = aes(x = Week, y = Home))+
  xlab("Week Number") + theme(axis.title.x = element_text(size = 11, face = "bold"))+
  ylab("Excess Deaths in Home")+ theme(axis.title.y = element_text(size = 11, face = "bold", vjust = 0.5, hjust = 0.5))+
  #and "scale_ _continuous" to control the scope of the axes 
  scale_y_continuous(limits = c(-1500,3500))+
  scale_x_continuous(limits = c(0,52))+
  #Color" and "fill" define the color of the line and point.
  geom_line(size = 2, color = "#0066CC", alpha = 0.2)+
  geom_point(shape = 20, size = 2, color = "#0066CC",fill = "#0066CC", stroke = 1 )
  

p3_2 <- ggplot(data = Excess_Deaths, mapping = aes(x = Week, y = Hospital))+
  xlab("Week Number") + theme(axis.title.x = element_text(size = 11, face = "bold"))+
  ylab("Excess Deaths in Hospital")+ theme(axis.title.y = element_text(size = 11, face = "bold", vjust = 0.5, hjust = 0.5))+
  scale_y_continuous(limits = c(-1500,3500))+
  scale_x_continuous(limits = c(0,52))+
  geom_line(size = 2, color = "#CC0000", alpha = 0.2)+
  geom_point(shape = 20, size = 2, color = "#CC0000",fill = "#CC0000", stroke = 1 )


p3_3 <- ggplot(data = Excess_Deaths, mapping = aes(x = Week, y = CareHome))+
  xlab("Week Number") + theme(axis.title.x = element_text(size = 11, face = "bold"))+
  ylab("Excess Deaths in Care Home")+ theme(axis.title.y = element_text(size = 11, face = "bold", vjust = 0.5, hjust = 0.5))+
  scale_y_continuous(limits = c(-1500,3500))+
  scale_x_continuous(limits = c(0,52))+
  geom_line(size = 2, color = "#006600", alpha = 0.2)+
  geom_point(shape = 20, size = 2, color = "#006600",fill = "#006600", stroke = 1 )


p3_4 <- ggplot(data = Excess_Deaths, mapping = aes(x = Week, y = Other))+
  xlab("Week Number") + theme(axis.title.x = element_text(size = 11, face = "bold"))+
  ylab("Excess Deaths in Other Place")+ theme(axis.title.y = element_text(size = 11, face = "bold", vjust = 0.5, hjust = 0.5))+
  scale_y_continuous(limits = c(-1500,3500))+
  scale_x_continuous(limits = c(0,52))+
  geom_line(size = 2, color = "#660099", alpha = 0.2)+
  geom_point(shape = 20, size = 2, color = "#660099",fill = "#660099", stroke = 1 )

# Use ggarrange package to combine 4 figures.
p3 <- ggarrange(p3_1,p3_2,p3_3,p3_4, ncol= 2, nrow = 2)

# Add a title 
p3 <- p3 +
  
  ggtitle("The number of excess deaths at different places in the UK in 2021") +
  
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
p3

# Saving as a png. pic.
ggsave('2021 Excess Deaths in UK.png',
       plot = p3, path = here("Figure/"),
       width = 520,
       height = 240,
       units = "mm",
       dpi = 300)

#############################The Forth Figure#################################

# Plotting a total figure by using the "sum" data

p4 <- ggplot(data = Excess_Deaths, mapping = aes(x = Week, y = InTotal))+
  xlab("Week Number") + theme(axis.title.x = element_text(size = 11, face = "bold"))+
  ylab("Excess Deaths in Total")+ theme(axis.title.y = element_text(size = 11, face = "bold", vjust = 0.5, hjust = 0.5))+
  scale_x_continuous(limits = c(0,52))+
  geom_line(size = 2, color = "#333333", alpha = 0.4)+
  geom_point(shape = 20, size = 2, color = "#000000",fill = "#000000", stroke = 1 )

p4 <- p4 +
  ggtitle("The number of excess deaths per week in the UK in 2021") +
  
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

p4

# save the figure

ggsave('The_number_of_excess_deaths_per_week.png', 
       plot = p4, path = here("Figure/"),
       width = 260,
       height = 109,
       units = "mm",
       dpi = 300)

###############################The Fifth Figure###################################

# Data Pre-processing#

Age_Group_sum <- 
  aggregate(AgeGroup_Deaths$Deaths, by=list(Age.Group=AgeGroup_Deaths$Age.Group),sum)

colnames(Age_Group_sum)
names(Age_Group_sum)[names(Age_Group_sum) =="Age.Group"] <-"Group"
names(Age_Group_sum)[names(Age_Group_sum) =="x"] <-"Deaths"

type <- c('15-44','45-64','> 65')
nums <- c(1588,11234,71185)
Pie_Data_Set <- data.frame(Age_Group = type, nums = nums)

# Get the percentages of each group and edit the labels
label_value <- paste('(', round(Pie_Data_Set$nums/sum(Pie_Data_Set$nums) * 100, 1), '%)', sep = '')
label <- c("> 65: 84.7%","15-44: 1.9%","45-64: 13.4%")


# Through the bar chart and polar transformation to achieve the pie chart drawing

# Draw a vertical stacked bar chart.
# All three groups of data are stacked on the same blank independent variable.
p4_1 <- ggplot(data = Pie_Data_Set, mapping = aes(x = ' ', y = nums, fill = type)) + 
  geom_bar(stat = 'identity', position = 'stack', width = 1)

# Polar transformation to transform a bar chart into a pie chart.

p4_1 <- p4_1 +
  coord_polar(theta = 'y') +
  labs(x = '', y = '', title = '')+
  theme(axis.text = element_blank())+ #Remove unnecessary elements and add labels.
  theme(axis.ticks = element_blank())+
  scale_fill_discrete(labels = label)+
  ggtitle("A Pie Figure for Deaths Involving COVID-19 Grouping by Age") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

p4_1

# save
ggsave('Pie Group in Age.png', 
       plot = p4_1, path = here("Figure/"),
       width = 180,
       height = 115,
       units = "mm",
       dpi = 300)





