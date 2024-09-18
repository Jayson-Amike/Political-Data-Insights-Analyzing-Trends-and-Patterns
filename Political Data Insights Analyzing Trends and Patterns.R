##Homework #2 Quantitative Homework##
##Q.1 - Set Working Directory, Package Installation,Data Loading##
getwd()
##Tell RStudio where to find the file I want to open and work on##
setwd("C:/Users/jayso/Downloads/RProject")

##Install package
install.packages(c("modelsummary","haven"))
##Load packages
library(modelsummary)
library(haven)

##This allows the file to be read in "haven" as sav "av" represents "haven" in comparison to scv
sgdata = read_sav("release_W5_Singapore_merged_core_20210218.sav")
sgdatacopy = sgdata

##Q.2 Variable and Data Recoding.##
##Converted into character
##Recode "SE2" into "Female"##

sgdatacopy$female = ifelse(sgdatacopy$SE2 %in% 2, 1,
                           ifelse(sgdatacopy$SE2%in%  1, 0, NA))

##Generate a new dichotomous variable
print(sgdatacopy$female)
table(sgdatacopy$SE2, sgdatacopy$female)

##Recode "SE3_1" into "youth"##
sgdatacopy$youth = ifelse(sgdatacopy$SE3_1 < 35, 1,
                          ifelse(sgdatacopy$SE3_1 <= 35, 0, NA))
##Generate a new dichotomous variable
table(sgdatacopy$SE3_1, sgdatacopy$youth)

##Recode "SE4" into "married"##
sgdatacopy$married = ifelse(sgdatacopy$SE4%in% c(2,3), 1, 0)
##Generate a new dichotomous variable
table(sgdatacopy$SE4, sgdatacopy$married)

##Recode "SE5" into "edulevel"##
print(sgdatacopy$SE5)
# Create edulevel based on the conditions
sgdatacopy$edulevel <- ifelse(sgdatacopy$SE5 %in% c(1, 2, 3), 1,
                              ifelse(sgdatacopy$SE5 %in% c(4, 5, 6, 7), 2,
                                     ifelse(sgdatacopy$SE5 %in% c(8, 9), 3, NA)))
table(sgdatacopy$SE5, sgdatacopy$edulevel)

##Recode "SE7a" into "selfreligiosity"
print(sgdatacopy$SE7a) # this is the table data

sgdatacopy$selfreligiosity = ifelse(sgdatacopy$SE7a == 4, 0,
                                    ifelse(sgdatacopy$SE7a == 3, 1,
                                           ifelse(sgdatacopy$SE7a ==  2, 2,
                                                  ifelse(sgdatacopy$SE7a == 1, 3, NA))))
table(sgdatacopy$SE7a, sgdatacopy$selfreligiosity)

##Recode "SE11a" into "chinese"
print(sgdatacopy$SE11a)
sgdatacopy$chinese = ifelse(sgdatacopy$SE11a %in% c(607,802,1001,1505,14136,14140), 1, 0)
##Generate a new dichotomous variable
table(sgdatacopy$SE11a, sgdatacopy$chinese)

##Recode "SE13a" into "incomefeeling"##

print(sgdatacopy$SE13a)
sgdatacopy$incomefeeling = ifelse(sgdatacopy$SE13a%in% c(97 ,98, 99), 5,
                                  c(1, 2, 3, 4, 6, 7, 8, 9, 10))
table(sgdatacopy$SE13a, sgdatacopy$incomefeeling)

##Recode "SE14a" into "econhardship"
print(sgdatacopy$SE14a)
sgdatacopy$econhardship = ifelse(sgdatacopy$SE14a %in% c(9, 8, 0), 0,
                                 c(1, 2, 3, 4, 5))
table(sgdatacopy$SE14a, sgdatacopy$econhardship)

##Recode "Q105" into "govsatis"
sgdatacopy2=sgdatacopy
sgdatacopy=sgdata

# Lesson

sgdatacopy2=sgdatacopy #undo conversion
print(sgdatacopy$Q105) # this is the table data
sgdatacopy$govsatis = ifelse(sgdatacopy$Q105 == 4, 0, # Very dissatisfied
                             ifelse(sgdatacopy$Q105 == 3, 1, # Somewhat dissatisfied
                                    ifelse(sgdatacopy$Q105 == 2, 2, # Somewhat satisfied
                                           ifelse(sgdatacopy$Q105 == 1, 3, # Very satisfied
                                                  ifelse(sgdatacopy$Q105 %in% c(7, 8, 9), 1.5, NA))))) # Other cases

##Recode "Q34" into "papvoter"
#sgdatacopy$Q34 = ifelse(sgdatacopy$Q34)
print(sgdatacopy$Q34)
sgdatacopy$papvoter = ifelse(sgdatacopy$Q34 == 1001 , 1,0)
##Generate a new dichotomous variable
table(sgdatacopy$Q34, sgdatacopy$papvoter)

##Q.3 Answered in Word file##

##Q.4 Data Visualization##
#a. Barplot for "papvoter"
#Exclude NA values from the frequency table calculation
freq_table = table(sgdatacopy$papvoter, useNA = "ifany")
#Find the maximum frequency value (exclusing NA values)
max_frequency = max(freq_table, na.rm = TRUE)
#Print the maximum frequency value
print(max_frequency)
#Check for NA values in the "papvoter" variable
any_na = any(is.na(sgdatacopy$papvoter))
#If it prints TRUE there are NA values
print(any_na)
#It printed TRUE so it is not otherwise FALSE
#There are NA values in the "papvoter" variable

#Get unique values and their counts in the "papvoter" variable
unique_values = unique(sgdatacopy$papvoter)
value_counts = table(sgdatacopy$papvoter)

#Check the "papvoter" variable
summary(sgdatacopy$papvoter)
unique(sgdatacopy$papvoter)
table(sgdatacopy$papvoter)

#Plot the bar chart with adjusted y-axis limit
unique(sgdatacopy$Q34)
sgdatacopy$papvoter = factor(sgdatacopy$papvoter)
barplot(freq_table,
        main = "Distribution of PAP Voters",
        xlab = "PAP Voter (Yes=1, No=0, NA=Missing)",
        ylab = "Frequency",
        #Add to accomodate the maximum frequency
        ylim = c(0, max_frequency),
        legend.text = TRUE,
        beside = TRUE)
#Add a colour coded legended to reflect xlab
legend ("topright",
        legend = c("PAP voter", "Non-PAP Voter", "Missing"),
        fill = c(col = "navyblue", "forestgreen", "hotpink"))
#Add a grid to the plot - accuracy purposes
grid()
#Add horizontal grid lines at every 100 units - accuracy purposes
abline(h = seq(0, max(freq_table) + 50, by = 100),
       col = "black",
       lty = 2)


##b. Barplot for "papvoter" and "govsatis"
##NOT WORKING NEED HELP TO FIX THIS ERROR SHOWING,
##Error in rep.int(space[1L], NR - 1) : invalid 'times' value
sgdatacopy$govsatis = factor(sgdatacopy$govsatis)
sgdatacopy$papvoter = factor(sgdatacopy$papvoter)

contingency_table = xtabs(~ govsatis + papvoter, data = sgdatacopy)

barplot(contingency_table,
        main = "Distribution of PAP Voters by Government Satisfaction",
        xlab = "Government Satisfaction",
        ylab = "Frequency",
        col = c("navyblue", "forestgreen"),
        legend = c("Non-PAP voter", "PAP Voter"),
        beside = TRUE)
