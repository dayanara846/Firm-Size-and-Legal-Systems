# import libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(psych)
library(qwraps2)
options(qwraps2_markup = 'markdown')
library(knitr)
library(kableExtra)
library(DT)
library(sandwich)
library(lmtest)
library(broom)
library(huxtable)
library(reshape)
library(gridExtra)
library(grid)
library(tinytex)
library(readxl)
library(tidyr)
library(writexl)







# clean datasets

###### ------------------------------------------------------------------------------------------------------- Y var <- Firms  -----------------------------------
years <- c(2017,2015,2012,2010,2008,2007)

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Dependent Var")
#Firms17 <- read_excel("us_state_totals_2007-2018.xlsx", sheet = "2017")
# Source: SUSB Annual Data Tables by Establishment Industry in each State (from 1988 - 2018):
#         https://www.census.gov/data/tables/time-series/econ/susb/susb-historical.html



###### First year: 2017
i <- 1
# import dataset while also saving each one of them
my_dat_1 <- assign(paste("Firms",years[i+7],sep="_"), read_excel("us_state_totals_2007-2018.xlsx", sheet = paste(years[i])))
# clean dataset
# identify the row and col index's of the column that has the firm's size
# find the col index of a cell that has some values
col <- which(apply(my_dat_1, 2, function(x) any(grepl("Size|SIZE|size", x))))
# find the col index of a cell that has some values
row <- which(apply(my_dat_1, 1, function(x) any(grepl("Size|SIZE|size", x))))
# change colnames
colnames(my_dat_1) <- my_dat_1[row,]
# identify all firm sizes' categories
col <- as.data.frame(col)
firm_cats <- unique(my_dat_1[ , col[1,1]])
firm_cats <- firm_cats[-c(1,2),] # delete all unnecessary firm sizes
firm_cats <- as.data.frame(firm_cats)

# subsetting by size of firms
FTot<-my_dat_1[ which(my_dat_1[,col[1,1]]==paste(firm_cats[1,1])),]
F1 <- my_dat_1[ which(my_dat_1[,col[1,1]]==paste(firm_cats[2,1])),]
F2 <- my_dat_1[ which(my_dat_1[,col[1,1]]==paste(firm_cats[3,1])),]
F3 <- my_dat_1[ which(my_dat_1[,col[1,1]]==paste(firm_cats[4,1])),]
F4 <- my_dat_1[ which(my_dat_1[,col[1,1]]==paste(firm_cats[5,1])),]
F5 <- my_dat_1[ which(my_dat_1[,col[1,1]]==paste(firm_cats[6,1])),]
F6 <- my_dat_1[ which(my_dat_1[,col[1,1]]==paste(firm_cats[7,1])),]
F7 <- my_dat_1[ which(my_dat_1[,col[1,1]]==paste(firm_cats[8,1])),]
F8 <- my_dat_1[ which(my_dat_1[,col[1,1]]==paste(firm_cats[9,1])),]


# rearrange columns
# find the col index of a cell that has some values 
col_ID <- as.data.frame(which(apply(my_dat_1, 2, function(x) any(grepl("State|state|STATE", x)))))
col_STATE <- as.data.frame(which(apply(my_dat_1, 2, function(x) any(grepl("Name|name|NAME", x)))))
col_SIZE <- col
col_FIRMS <- as.data.frame(which(apply(my_dat_1, 2, function(x) any(grepl("Firms|firms|FIRMS", x)))))

# rearrange 
FTot <- cbind(FTot[ ,col_ID[1,1]], FTot[ , col_STATE[1,1]], FTot[,col_SIZE[1,1]], FTot[,col_FIRMS[1,1]])
F1 <- cbind(F1 [ ,col_ID[1,1]], F1 [ , col_STATE[1,1]], F1 [,col_SIZE[1,1]], F1 [,col_FIRMS[1,1]])
F2 <- cbind(F2 [ ,col_ID[1,1]], F2 [ , col_STATE[1,1]], F2 [,col_SIZE[1,1]], F2 [,col_FIRMS[1,1]])
F3 <- cbind(F3 [ ,col_ID[1,1]], F3 [ , col_STATE[1,1]], F3 [,col_SIZE[1,1]], F3 [,col_FIRMS[1,1]])
F4 <- cbind(F4 [ ,col_ID[1,1]], F4 [ , col_STATE[1,1]], F4 [,col_SIZE[1,1]], F4 [,col_FIRMS[1,1]])
F5 <- cbind(F5 [ ,col_ID[1,1]], F5 [ , col_STATE[1,1]], F5 [,col_SIZE[1,1]], F5 [,col_FIRMS[1,1]])
F6 <- cbind(F6 [ ,col_ID[1,1]], F6 [ , col_STATE[1,1]], F6 [,col_SIZE[1,1]], F6 [,col_FIRMS[1,1]])
F7 <- cbind(F7 [ ,col_ID[1,1]], F7 [ , col_STATE[1,1]], F7 [,col_SIZE[1,1]], F7 [,col_FIRMS[1,1]])
F8 <- cbind(F8 [ ,col_ID[1,1]], F8 [ , col_STATE[1,1]], F8 [,col_SIZE[1,1]], F8 [,col_FIRMS[1,1]])


# change colnames

colnames(FTot) <- c("ID", "State", "Size", "Firms") 
colnames(F1) <- c("ID", "State", "Size", "Firms") 
colnames(F2) <- c("ID", "State", "Size", "Firms")  
colnames(F3) <- c("ID", "State", "Size", "Firms")  
colnames(F4) <- c("ID", "State", "Size", "Firms") 
colnames(F5) <- c("ID", "State", "Size", "Firms") 
colnames(F6) <- c("ID", "State", "Size", "Firms") 
colnames(F7) <- c("ID", "State", "Size", "Firms") 
colnames(F8) <- c("ID", "State", "Size", "Firms") 

# add year variable
FTot$Year<-years[i]
F1$Year<-years[i]
F2$Year<-years[i]
F3$Year<-years[i]
F4$Year<-years[i]
F5$Year<-years[i]
F6$Year<-years[i]
F7$Year<-years[i]
F8$Year<-years[i]

#### all other years



setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Dependent Var")
for (i in (2:length(years))) {
  # i<-2
  # import dataset while also saving each one of them
  my_dat_1 <- assign(paste("Firms",years[i],sep="_"), read_excel("us_state_totals_2007-2018.xlsx", sheet = paste(years[i])))
  # clean dataset
  
  # identify the row and col index's of the column that has the firm's size
  # fill state names blank spaces
  
  my_dat_1 <- my_dat_1 %>% fill(colnames(my_dat_1)[2])
  # identify all firm sizes' categories
  firm_cats <- unique(my_dat_1[ , 3])
  firm_cats <- firm_cats[-c(1:2),] # delete all unnecessary firm sizes
  firm_cats <- as.data.frame(firm_cats)
  
  # subsetting by size of firms
  FTot_1<-my_dat_1[ which(my_dat_1[,3]==paste(firm_cats[1,1])),]
  F1_1 <- my_dat_1[ which(my_dat_1[,3]==paste(firm_cats[2,1])),]
  F2_1 <- my_dat_1[ which(my_dat_1[,3]==paste(firm_cats[3,1])),]
  F3_1 <- my_dat_1[ which(my_dat_1[,3]==paste(firm_cats[4,1])),]
  F4_1 <- my_dat_1[ which(my_dat_1[,3]==paste(firm_cats[5,1])),]
  F5_1 <- my_dat_1[ which(my_dat_1[,3]==paste(firm_cats[6,1])),]
  F6_1 <- my_dat_1[ which(my_dat_1[,3]==paste(firm_cats[7,1])),]
  F7_1 <- my_dat_1[ which(my_dat_1[,3]==paste(firm_cats[8,1])),]
  F8_1 <- my_dat_1[ which(my_dat_1[,3]==paste(firm_cats[9,1])),]
  
  # add ID
  FTot_1[,1]<-1:nrow(FTot_1)
  F1_1[,1]<-1:nrow(F1_1)
  F2_1[,1]<-1:nrow(F2_1)
  F3_1[,1]<-1:nrow(F3_1)
  F4_1[,1]<-1:nrow(F4_1)
  F5_1[,1]<-1:nrow(F5_1)
  F6_1[,1]<-1:nrow(F6_1)
  F7_1[,1]<-1:nrow(F7_1)
  F8_1[,1]<-1:nrow(F8_1)
  
  # select some cols
  FTot_1 <- FTot_1[,1:4]
  F1_1 <- F1_1[,1:4]
  F2_1 <- F2_1[,1:4]
  F3_1 <- F3_1[,1:4]
  F4_1 <- F4_1[,1:4]
  F5_1 <- F5_1[,1:4]
  F6_1 <- F6_1[,1:4]
  F7_1 <- F7_1[,1:4]
  F8_1 <- F8_1[,1:4]
  
  
  # change colnames
  
  colnames(FTot_1) <- c("ID", "State", "Size", "Firms") 
  colnames(F1_1) <- c("ID", "State", "Size", "Firms") 
  colnames(F2_1) <- c("ID", "State", "Size", "Firms")  
  colnames(F3_1) <- c("ID", "State", "Size", "Firms")  
  colnames(F4_1) <- c("ID", "State", "Size", "Firms") 
  colnames(F5_1) <- c("ID", "State", "Size", "Firms") 
  colnames(F6_1) <- c("ID", "State", "Size", "Firms") 
  colnames(F7_1) <- c("ID", "State", "Size", "Firms") 
  colnames(F8_1) <- c("ID", "State", "Size", "Firms") 
  
  # add year variable
  FTot_1$Year<-years[i]
  F1_1$Year<-years[i]
  F2_1$Year<-years[i]
  F3_1$Year<-years[i]
  F4_1$Year<-years[i]
  F5_1$Year<-years[i]
  F6_1$Year<-years[i]
  F7_1$Year<-years[i]
  F8_1$Year<-years[i]
  
  # merge
  FTot <- rbind(FTot, FTot_1)
  F1 <- rbind(F1, F1_1)
  F2 <- rbind(F2, F2_1)
  F3 <- rbind(F3, F3_1)
  F4 <- rbind(F4, F4_1)
  F5 <- rbind(F5, F5_1)
  F6 <- rbind(F6, F6_1)
  F7 <- rbind(F7, F7_1)
  F8 <- rbind(F8, F8_1)                        
  
}


years_2 <- c(2006,2005,2004,2003,2002)
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Dependent Var")
for (i in (1:length(years_2))) {
  # import dataset while also saving each one of them
  my_dat_1 <- assign(paste("Firms",years_2[i],sep="_"), read_excel("us_state_totals_1988-2006.xlsx", sheet = paste(years_2[i])))
  # identify the row and col index's of the column that has the firm's size
  # fill state names blank spaces
  my_dat_1 <- my_dat_1 %>% fill(colnames(my_dat_1)[1])
  my_dat_1 <- my_dat_1[ which(my_dat_1[,2]=="Firms"),]
  # Firms
  FTot_1<-data.frame(ID<-c(1:nrow(my_dat_1)), State<-my_dat_1[,1],  Size <- "1:   Total", Firms <- my_dat_1[,3], Year <- years_2[i])
  F1_1 <- data.frame(ID<-c(1:nrow(my_dat_1)), State<-my_dat_1[,1],  Size <- "02: <5 employees", Firms <- my_dat_1[,4], Year <- years_2[i])
  F2_1 <- data.frame(ID<-c(1:nrow(my_dat_1)), State<-my_dat_1[,1],  Size <- "03: 5-9 employees", Firms <- my_dat_1[,5], Year <- years_2[i])
  F3_1 <- data.frame(ID<-c(1:nrow(my_dat_1)), State<-my_dat_1[,1],  Size <- "04: 10-19 employees", Firms <- my_dat_1[,6], Year <- years_2[i])
  F5_1 <- data.frame(ID<-c(1:nrow(my_dat_1)), State<-my_dat_1[,1],  Size <- "06: 20-99 employees", Firms <- my_dat_1[,7], Year <- years_2[i])
  F6_1 <- data.frame(ID<-c(1:nrow(my_dat_1)), State<-my_dat_1[,1],  Size <- "07: 100-499 employees", Firms <- my_dat_1[,8], Year <- years_2[i])
  F8_1 <- data.frame(ID<-c(1:nrow(my_dat_1)), State<-my_dat_1[,1],  Size <- "09: 500+ employees", Firms <- my_dat_1[,9], Year <- years_2[i])                        
  
  # Change col names
  colnames(FTot_1) <- c("ID", "State", "Size", "Firms", "Year") 
  colnames(F1_1) <- c("ID", "State", "Size", "Firms", "Year")
  colnames(F2_1) <- c("ID", "State", "Size", "Firms", "Year")  
  colnames(F3_1) <- c("ID", "State", "Size", "Firms", "Year")  
  colnames(F4_1) <- c("ID", "State", "Size", "Firms", "Year") 
  colnames(F5_1) <- c("ID", "State", "Size", "Firms", "Year") 
  colnames(F6_1) <- c("ID", "State", "Size", "Firms", "Year") 
  colnames(F7_1) <- c("ID", "State", "Size", "Firms", "Year") 
  colnames(F8_1) <- c("ID", "State", "Size", "Firms", "Year") 
  
  # merge
  FTot <- rbind(FTot, FTot_1)
  F1 <- rbind(F1, F1_1)
  F2 <- rbind(F2, F2_1)
  F3 <- rbind(F3, F3_1)
  F4 <- rbind(F4, F4_1)
  F5 <- rbind(F5, F5_1)
  F6 <- rbind(F6, F6_1)
  F7 <- rbind(F7, F7_1)
  F8 <- rbind(F8, F8_1)    
}



setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Dependent Var")
write_xlsx(FTot,"FTot Cleaned.xlsx")
write_xlsx(F1,"F1 Cleaned.xlsx")
write_xlsx(F2,"F2 Cleaned.xlsx")
write_xlsx(F3,"F3 Cleaned.xlsx")
write_xlsx(F4,"F4 Cleaned.xlsx")
write_xlsx(F5,"F5 Cleaned.xlsx")
write_xlsx(F6,"F6 Cleaned.xlsx")
write_xlsx(F7,"F7 Cleaned.xlsx")
write_xlsx(F8,"F8 Cleaned.xlsx")

###### -------------------------------------------------------------------------------------- X var <- Legal System Quality Index ----------------------------------------




#---------------------------------------------------------------------- PROPERTY CRIMES -------------------------------------------------       

# ------------------ crimes
years<-c(2017, 2015,2012,2010,2008,2007,2006,2005,2004)               
years_2<- c(2003,2002)   
# 2017 
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X/Property Crime")


i<-1
my_dat_1 <- assign(paste("Property_Crime_",years[i],sep="_"), read_excel(paste("Property Crime ",years[i], ".xls",sep="")))
# identify the row and col index's of the column that has the firm's size
# find the col index of a cell that has some values
col <- which(apply(my_dat_1, 2, function(x) any(grepl("Property crime", x))))
# find the col index of a cell that has some values
row <- which(apply(my_dat_1, 1, function(x) any(grepl("Property crime", x))))
# change colnames
colnames(my_dat_1) <- my_dat_1[row,]
# identify all firm sizes' categories
col <- as.data.frame(col)
# only get the property crime
my_dat_1<-my_dat_1[,c(1, 2, col[1,1]+1)]
# change colname
colnames(my_dat_1)[3] <- "Rate per  100,000" 
colnames(my_dat_1)[1] <- "State"  
# fill state names blank spaces
my_dat_1 <- my_dat_1 %>% fill(`State`)
# only select rows that are from the year
my_dat_1<-my_dat_1[my_dat_1$Year == years[1], ] 
# replace all numeric and comma values with nothing
library(stringr)
my_dat_1$State <- str_replace_all(my_dat_1$State, c("1"="", "2"="", "3"="", "4"="", "5"="", "6" = "", "," = "", "7"="", "8"="", "9"="", "Total" = ""))
my_dat_1<-na.omit(my_dat_1)
Crimes<-my_dat_1

# 2015-2004
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X/Property Crime")

for (i in (2:length(years))) {
  my_dat_1 <- assign(paste("Property_Crime_",years[i],sep="_"), read_excel(paste("Property Crime ",years[i], ".xls",sep="")))
  # identify the row and col index's of the column that has the firm's size
  # find the col index of a cell that has some values
  col <- which(apply(my_dat_1, 2, function(x) any(grepl("Property crime", x))))
  # find the col index of a cell that has some values
  row <- which(apply(my_dat_1, 1, function(x) any(grepl("Property crime", x))))
  # change colnames
  colnames(my_dat_1) <- my_dat_1[row,]
  # identify all firm sizes' categories
  col <- as.data.frame(col)
  # only get the property crime
  my_dat_1<-my_dat_1[,c(1, 2, col[1,1]+1)]
  # change colname
  colnames(my_dat_1)[3] <- "Rate per  100,000" 
  colnames(my_dat_1)[1] <- "State"  
  # fill state names blank spaces
  my_dat_1 <- my_dat_1 %>% fill(`State`)
  # only select rows that are from the year
  my_dat_1<-my_dat_1[my_dat_1$Year == years[i], ] 
  # replace all numeric and comma values with nothing
  library(stringr)
  my_dat_1$State <- str_replace_all(my_dat_1$State, c("1"="", "2"="", "3"="", "4"="", "5"="", "6" = "", "," = "", "7"="", "8"="", "9"="", "Total" = ""))
  # delete all incomplete cases
  my_dat_1<-na.omit(my_dat_1)
  # rbind
  Crimes <- rbind(Crimes, my_dat_1)
}



# 2003
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X/Property Crime")
i<-1

my_dat_1 <- assign(paste("Property_Crime_",years_2[i],sep="_"), read_excel(paste("Property Crime ",years_2[i], ".xls",sep="")))
# identify the row and col index's of the column that has the firm's size
# find the col index of a cell that has some values
col <- which(apply(my_dat_1, 2, function(x) any(grepl("Property crime", x))))
# find the col index of a cell that has some values
row <- which(apply(my_dat_1, 1, function(x) any(grepl("Property crime", x))))
# change colnames
colnames(my_dat_1) <- my_dat_1[row,]
# identify all firm sizes' categories
col <- as.data.frame(col)
# only get the property crime estimate
my_dat_1<-my_dat_1[,c(1, col[1,1])]
# only select rows 
# estimated totals
my_dat_2<-my_dat_1[my_dat_1[,1] == "Rate per 100,000 inhabitants", ] 
my_dat_2<-na.omit(my_dat_2)
# state names
list<-c("Metropolitan Statistical Area", "Area actually reporting", "Estimated total", "Cities outside metropolitan areas", "Area actually reporting", "Nonmetropolitan counties", "Area actually reporting", "Estimated total", "State Total", "Rate per 100,000 inhabitants", "Area", "by State, 2003", "Crime in the United States", "Total")
my_dat_3<-my_dat_1
for (j in (1:length(list))) {my_dat_3<-my_dat_3[my_dat_3[,1] != list[j], ]
}

my_dat_3<-na.omit(my_dat_3[,1])
my_dat_3<-my_dat_3[-c(53:55),]
# cbind states and crimes
my_dat_1<-cbind(my_dat_3,my_dat_2[,2])

# change colname
colnames(my_dat_1)[2] <- "Rate per  100,000" 
colnames(my_dat_1)[1] <- "State"  
# replace all numeric and comma values with nothing
library(stringr)
my_dat_1$State <- str_replace_all(my_dat_1$State, c("1"="", "2"="", "3"="", "4"="", "5"="", "6" = "", "," = "", "7"="", "8"="", "9"="", "Total" = ""))
# fix all caps
my_dat_1[,1]<-stringr::str_to_title(my_dat_1[,1])
# add year
my_dat_1<-data.frame(my_dat_1[,1], years_2[i], my_dat_1[,2])
# change colname
colnames(my_dat_1)[3] <- "Rate per  100,000" 
colnames(my_dat_1)[1] <- "State"  
colnames(my_dat_1)[2] <- "Year" 
# rbind
Crimes <- rbind(Crimes, my_dat_1)



# 2002
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X/Property Crime")
i<-2

my_dat_1 <- assign(paste("Property_Crime_",years_2[i],sep="_"), read_excel(paste("Property Crime ",years_2[i], ".xls",sep="")))
# identify the row and col index's of the column that has the firm's size
# find the col index of a cell that has some values
col <- which(apply(my_dat_1, 2, function(x) any(grepl("Property crime2", x))))
# find the col index of a cell that has some values
row <- which(apply(my_dat_1, 1, function(x) any(grepl("Property crime2", x))))
# change colnames
colnames(my_dat_1) <- my_dat_1[row,]
# identify all firm sizes' categories
col <- as.data.frame(col)
# only get the property crime estimate
my_dat_1<-my_dat_1[,c(1, col[1,1])]
# only select rows 
# estimated totals
my_dat_2<-my_dat_1[my_dat_1[,1] == "Rate per 100,000 inhabitants", ] 
my_dat_2<-na.omit(my_dat_2)
# state names
list<-c("Metropolitan Statistical Area", "Area actually reporting", "Estimated total", "Cities outside metropolitan areas", "Area actually reporting", "Nonmetropolitan counties", "Area actually reporting", "Estimated total", "State Total", "Rate per 100,000 inhabitants", "Area", "by State, 2002", "Crime in the United States", "Total", "Rural", "Index of Crime")
my_dat_3<-my_dat_1
for (j in (1:length(list))) {my_dat_3<-my_dat_3[my_dat_3[,1] != list[j], ]
}

my_dat_3<-na.omit(my_dat_3[,1])
my_dat_3<-my_dat_3[-c(53:57),]
# cbind states and crimes
my_dat_1<-cbind(my_dat_3,my_dat_2[,2])

# change colname
colnames(my_dat_1)[2] <- "Rate per  100,000" 
colnames(my_dat_1)[1] <- "State"  
# replace all numeric and comma values with nothing
library(stringr)
my_dat_1$State <- str_replace_all(my_dat_1$State, c("1"="", "2"="", "3"="", "4"="", "5"="", "6" = "", "," = "", "7"="", "8"="", "9"="", "Total" = ""))
# fix all caps
my_dat_1[,1]<-stringr::str_to_title(my_dat_1[,1])
# add year
my_dat_1<-data.frame(my_dat_1[,1], years_2[i], my_dat_1[,2])
# change colname
colnames(my_dat_1)[3] <- "Rate per  100,000" 
colnames(my_dat_1)[1] <- "State"  
colnames(my_dat_1)[2] <- "Year" 
# rbind
Crimes <- rbind(Crimes, my_dat_1)




setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X/Property Crime")
write_xlsx(Crimes,"Property Crimes Cleaned.xlsx")                  

#---------------------------------------------------------------------- CORRUPTION -------------------------------------------------       
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X/Corruption") 
Corrupt <- read_excel("Federal Public Corruption Convictions.xlsx")
# Use regular expressions to separate on multiple characters:
Corrupt <- Corrupt %>% separate(`U.S. Attorney's Office`, c("State","Region"), sep = "([,])")

# Sum all values of the same state
# first year
i<-3
C_year <- aggregate(Corrupt[,i], by=list(State=Corrupt$State), FUN=sum)
# fix California oddity
C_year[,2]<-as.numeric(C_year[,2])
# find the col index of a cell that has some values
row <- which(apply(C_year, 1, function(x) any(grepl("California", x))))
# sum
row<-as.numeric(row)
row<-as.data.frame(row)
new<-as.numeric(C_year[row[1,1],2])+as.numeric(C_year[row[2,1],2])
C_year[row[1,1],2]<-new
C_year<- C_year[-row[2,1],]

# change colnames
colnames(C_year)[2] <- "Corruption Convictions"
C_year$Year <- colnames(Corrupt)[i]
# final dataset
Corruption <- C_year 



# all other years
for (i in (4:length(Corrupt))) {
  C_year <- aggregate(Corrupt[,i], by=list(State=Corrupt$State), FUN=sum)
  # fix California oddity
  C_year[,2]<-as.numeric(C_year[,2])
  # find the col index of a cell that has some values
  row <- which(apply(C_year, 1, function(x) any(grepl("California", x))))
  # sum
  row<-as.numeric(row)
  row<-as.data.frame(row)
  new<-as.numeric(C_year[row[1,1],2])+as.numeric(C_year[row[2,1],2])
  C_year[row[1,1],2]<-new
  C_year<- C_year[-row[2,1],]
  
  # change colnames
  colnames(C_year)[2] <- "Corruption Convictions"
  C_year$Year <- colnames(Corrupt)[i]
  # final dataset
  # Rbind
  Corruption <- rbind(Corruption, C_year)
}   
# export to Excel                         
write_xlsx(Corruption,"Corruption Cleaned.xlsx")                               




# ------------------ Liability Systems
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X/Liability Systems")               
Liability_Systems <- read_excel("Liability Systems.xlsx", sheet = "19-02")
# first value
years<- colnames(Liability_Systems)
A <- Liability_Systems[,c(1,2)]
colnames(A)[1] <- "State"
colnames(A)[2] <- "Liability Systems Ranking"
A$Year <- years[2]

# all other values
for (i in (3:length(Liability_Systems))) {
  B <- Liability_Systems[,c(1,i)]
  colnames(B)[1] <- "State"
  colnames(B)[2] <- "Liability Systems Ranking"
  B$Year <- years[i] 
  A <- rbind(A,B)
}

Liability_Systems <- A

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X/Liability Systems")   
write_xlsx(Liability_Systems,"Liability_Systems Cleaned.xlsx")                  

#---------------------------------------------------------------------- SPENDING -------------------------------------------------                            
# ------------------ Public Spending



years<-c(2017,2015,2012,2010,2008,2007,2006,2005,2004,2002)     
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X/Public Spending")   
# First Year: 2017
i <- 1
# ---------- For Missouri and Wyoming


my_dat <- read_excel(paste("Public Spending - Missouri-Wyoming_",years[i], ".xlsx", sep=""))
# identify rows with spending in  public safety and judicial and legal services
# row index   
# public safety 
Police <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Police protection",arr.ind=TRUE)[1]

Fire <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Fire protection",arr.ind=TRUE)[1]


Correction <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Correction",arr.ind=TRUE)[1]

Capital <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Capital outlay",arr.ind=TRUE)[1]


Inspection <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Protective inspection and regulation",arr.ind=TRUE)[1]

# government administration

Legal <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Judicial and legal",arr.ind=TRUE)[1]


# State
Description <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Description",arr.ind=TRUE)[1]


# spenditures
State_Spenditures <- as.data.frame(which(my_dat == "State", arr.ind=TRUE))$col

my_dat <- my_dat[c(Description, Police, Fire, Correction, Capital, Inspection, Legal),]  
my_dat <- my_dat[,-1] 
# fill in state names
A <- as.data.frame(t(my_dat[1,]))

A <- A %>% fill(`V1`)
my_dat[1,] <- t(A[,1])

# Select spenditure
my_dat <- my_dat[, c(1, State_Spenditures)]

# convert to readible dataframe
my_dat <- t(my_dat)

# set row names
row.names(my_dat) <- 1:nrow(my_dat)
# set col names
colnames(my_dat) <- my_dat[1,]
my_dat <- my_dat[-1,] 
# convert to dataframe                    
my_dat <- as.data.frame(my_dat)
my_dat$Year <- years[i]


# ---------- For Alabama-Mississippi

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X/Public Spending")   
my_dat_2 <- read_excel(paste("Public Spending - US Summary & Alabama-Mississippi_",years[i], ".xlsx", sep=""))
# identify rows with spending in  public safety and judicial and legal services


# row index   
# public safety 
Police <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Police protection", arr.ind=TRUE)[1]

Fire <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Fire protection",arr.ind=TRUE)[1]


Correction <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Correction",arr.ind=TRUE)[1]

Capital <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Capital outlay",arr.ind=TRUE)[1]


Inspection <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Protective inspection and regulation",arr.ind=TRUE)[1]

# government administration

Legal <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Judicial and legal",arr.ind=TRUE)[1]


# State
Description <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Description",arr.ind=TRUE)[1]


# spenditures
State_Spenditures <- as.data.frame(which(my_dat_2 == "State", arr.ind=TRUE))$col



my_dat_2 <- my_dat_2[c(Description, Police, Fire, Correction, Capital, Inspection, Legal),]  
my_dat_2 <- my_dat_2[,-1] 
# fill in state names
A <- as.data.frame(t(my_dat_2[1,]))

A <- A %>% fill(`V1`)
my_dat_2[1,] <- t(A[,1])

# Select spenditure
my_dat_2 <- my_dat_2[, c(1, State_Spenditures)]

# convert to readible dataframe
my_dat_2 <- t(my_dat_2)

# set row names
row.names(my_dat_2) <- 1:nrow(my_dat_2)
# set col names
colnames(my_dat_2) <- my_dat_2[1,]
my_dat_2 <- my_dat_2[-1,]                   
# convert to dataframe
my_dat_2 <- as.data.frame(my_dat_2)
my_dat_2$Year <- years[i]                     



# rbind
my_dat <- rbind(my_dat, my_dat_2)
# convert to numeric
for (j in (2:length(colnames(my_dat)))) { 
  my_dat[,j]<- as.numeric(my_dat[,j])
}
# sum all public spending
my_dat$Enforcement <- rowSums(my_dat[2:length(colnames(my_dat))])
# final dataframe
Spenditure <- my_dat              




# from 2015 and 2012

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X/Public Spending")   
for (i in (2:3)) {
  # ---------- For Missouri and Wyoming
  
  
  my_dat <- read_excel(paste("Public Spending - Missouri-Wyoming_",years[i], ".xlsx", sep=""))
  # identify rows with spending in  public safety and judicial and legal services
  # row index   
  # public safety 
  Police <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Police protection",arr.ind=TRUE)[1]
  
  Fire <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Fire protection",arr.ind=TRUE)[1]
  
  
  Correction <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Correction",arr.ind=TRUE)[1]
  
  Capital <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Capital outlay",arr.ind=TRUE)[1]
  
  
  Inspection <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Protective inspection and regulation",arr.ind=TRUE)[1]
  
  # government administration
  
  Legal <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Judicial and legal",arr.ind=TRUE)[1]
  
  
  # State
  Description <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Description",arr.ind=TRUE)[1]
  
  
  # spenditures
  State_Spenditures <- as.data.frame(which(my_dat == "State", arr.ind=TRUE))$col
  
  my_dat <- my_dat[c(Description, Police, Fire, Correction, Capital, Inspection, Legal),]  
  my_dat <- my_dat[,-1] 
  # fill in state names
  A <- as.data.frame(t(my_dat[1,]))
  
  A <- A %>% fill(`V1`)
  my_dat[1,] <- t(A[,1])
  
  # Select spenditure
  my_dat <- my_dat[, c(1, State_Spenditures)]
  
  # convert to readible dataframe
  my_dat <- t(my_dat)
  
  # set row names
  row.names(my_dat) <- 1:nrow(my_dat)
  # set col names
  colnames(my_dat) <- my_dat[1,]
  my_dat <- my_dat[-1,] 
  # convert to dataframe                    
  my_dat <- as.data.frame(my_dat)
  my_dat$Year <- years[i]
  
  
  
  # ---------- For Alabama-Mississippi
  
  my_dat_2 <- read_excel(paste("Public Spending - US Summary & Alabama-Mississippi_",years[i], ".xlsx", sep=""))
  # identify rows with spending in  public safety and judicial and legal services
  
  
  # row index   
  # public safety 
  Police <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Police protection", arr.ind=TRUE)[1]
  
  Fire <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Fire protection",arr.ind=TRUE)[1]
  
  
  Correction <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Correction",arr.ind=TRUE)[1]
  
  Capital <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Capital outlay",arr.ind=TRUE)[1]
  
  
  Inspection <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Protective inspection and regulation",arr.ind=TRUE)[1]
  
  # government administration
  
  Legal <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Judicial and legal",arr.ind=TRUE)[1]
  
  
  # State
  Description <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Description",arr.ind=TRUE)[1]
  
  
  # spenditures
  State_Spenditures <- as.data.frame(which(my_dat_2 == "State", arr.ind=TRUE))$col
  
  
  
  my_dat_2 <- my_dat_2[c(Description, Police, Fire, Correction, Capital, Inspection, Legal),]  
  my_dat_2 <- my_dat_2[,-1] 
  # fill in state names
  A <- as.data.frame(t(my_dat_2[1,]))
  
  A <- A %>% fill(`V1`)
  my_dat_2[1,] <- t(A[,1])
  
  # Select spenditure
  my_dat_2 <- my_dat_2[, c(1, State_Spenditures)]
  
  # convert to readible dataframe
  my_dat_2 <- t(my_dat_2)
  
  # set row names
  row.names(my_dat_2) <- 1:nrow(my_dat_2)
  # set col names
  colnames(my_dat_2) <- my_dat_2[1,]
  my_dat_2 <- my_dat_2[-1,]                   
  # convert to dataframe
  my_dat_2 <- as.data.frame(my_dat_2)
  my_dat_2$Year <- years[i]                     
  
  
  
  # rbind
  my_dat <- rbind(my_dat, my_dat_2)
  # convert to numeric
  for (j in (2:length(colnames(my_dat)))) { 
    my_dat[,j]<- as.numeric(my_dat[,j])
  }
  # sum all public spending
  my_dat$Enforcement <- rowSums(my_dat[2:length(colnames(my_dat))])
  # final dataframe
  Spenditure <- rbind(Spenditure, my_dat)
}




# from 2010 to 2002

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X/Public Spending")   
for (i in (4:length(years))) {
  # ---------- For Missouri and Wyoming
  
  my_dat <- read_excel(paste("Public Spending - Missouri-Wyoming_",years[i], ".xls", sep=""))
  # identify rows with spending in  public safety and judicial and legal services
  # row index   
  # public safety 
  Police <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Police protection",arr.ind=TRUE)[1]
  
  Fire <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Fire protection",arr.ind=TRUE)[1]
  
  
  Correction <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Correction",arr.ind=TRUE)[1]
  
  Capital <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Capital outlay",arr.ind=TRUE)[1]
  
  
  Inspection <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Protective inspection and regulation",arr.ind=TRUE)[1]
  
  # government administration
  
  Legal <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Judicial and legal",arr.ind=TRUE)[1]
  
  
  # State
  Description <- which(my_dat[, paste("Table 1.  State and Local Government Finances by Level of Government and by State:",years[i], " - Con.", sep="")]=="Description",arr.ind=TRUE)[1]
  
  
  # spenditures
  State_Spenditures <- as.data.frame(which(my_dat == "State", arr.ind=TRUE))$col
  
  my_dat <- my_dat[c(Description, Police, Fire, Correction, Capital, Inspection, Legal),]  
  my_dat <- my_dat[,-1] 
  # fill in state names
  A <- as.data.frame(t(my_dat[1,]))
  
  A <- A %>% fill(`V1`)
  my_dat[1,] <- t(A[,1])
  
  # Select spenditure
  my_dat <- my_dat[, c(1, State_Spenditures)]
  
  # convert to readible dataframe
  my_dat <- t(my_dat)
  
  # set row names
  row.names(my_dat) <- 1:nrow(my_dat)
  # set col names
  colnames(my_dat) <- colnames(Spenditure)[-c(8,9)]
  my_dat <- my_dat[-1,] 
  # convert to dataframe                    
  my_dat <- as.data.frame(my_dat)
  my_dat$Year <- years[i]
  
  
  
  # ---------- For Alabama-Mississippi
  
  my_dat_2 <- read_excel(paste("Public Spending - US Summary & Alabama-Mississippi_",years[i], ".xls", sep=""))
  # identify rows with spending in  public safety and judicial and legal services
  
  
  # row index   
  # public safety 
  Police <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Police protection", arr.ind=TRUE)[1]
  
  Fire <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Fire protection",arr.ind=TRUE)[1]
  
  
  Correction <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Correction",arr.ind=TRUE)[1]
  
  Capital <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Capital outlay",arr.ind=TRUE)[1]
  
  
  Inspection <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Protective inspection and regulation",arr.ind=TRUE)[1]
  
  # government administration
  
  Legal <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Judicial and legal",arr.ind=TRUE)[1]
  
  
  # State
  Description <- which(my_dat_2[, paste("Table 1.  State and Local Government Finances by Level of Government and by State: ",years[i], sep="")]=="Description",arr.ind=TRUE)[1]
  
  
  # spenditures
  State_Spenditures <- as.data.frame(which(my_dat_2 == "State", arr.ind=TRUE))$col
  
  
  
  my_dat_2 <- my_dat_2[c(Description, Police, Fire, Correction, Capital, Inspection, Legal),]  
  my_dat_2 <- my_dat_2[,-1] 
  # fill in state names
  A <- as.data.frame(t(my_dat_2[1,]))
  
  A <- A %>% fill(`V1`)
  my_dat_2[1,] <- t(A[,1])
  
  # Select spenditure
  my_dat_2 <- my_dat_2[, c(1, State_Spenditures)]
  
  # convert to readible dataframe
  my_dat_2 <- t(my_dat_2)
  
  # set row names
  row.names(my_dat_2) <- 1:nrow(my_dat_2)
  # set col names
  colnames(my_dat_2) <- colnames(Spenditure)[-c(8,9)]
  my_dat_2 <- my_dat_2[-1,]                   
  # convert to dataframe
  my_dat_2 <- as.data.frame(my_dat_2)
  my_dat_2$Year <- years[i]                     
  
  
  
  # rbind
  my_dat <- rbind(my_dat, my_dat_2)
  # convert to numeric
  for (j in (2:length(colnames(my_dat)))) { 
    my_dat[,j]<- as.numeric(my_dat[,j])
  }
  # sum all public spending
  my_dat$Enforcement <- rowSums(my_dat[2:length(colnames(my_dat))])
  # final dataframe
  Spenditure <- rbind(Spenditure, my_dat)
}                                                               

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X/Public Spending")                                                               
# save to Excel
write_xlsx(Spenditure,"Spenditure Cleaned.xlsx")                                                



#---------------------------------------------------------------------- Legal Quality (the smaller the ranking, the better quality) -----------   


# change all state names cols  
# Spenditure<- Spenditure_Cleaned
colnames(Spenditure)[1] <- "State"
Spenditure <- Spenditure[, c(1,length(Spenditure)-1, length(Spenditure))]
Spenditure$Enforcement<-Spenditure$Enforcement/1000

Legal_df <- merge(Liability_Systems, Crimes, by=c("State","Year")) # NA's match
Legal_df <- merge(Legal_df, Corruption, by=c("State","Year")) # NA's match     
Legal_df <- merge(Legal_df, Spenditure, by=c("State","Year")) # NA's match


Legal_df$`Rate per  100,000` <- as.numeric(Legal_df$`Rate per  100,000`)

# subset to rank legal system by year      
years <- unique(Legal_df$Year)


i <- 1
A <- Legal_df[Legal_df$Year %in% paste(years[i], sep=""), ]
A$W_aver <- (5*(A$`Liability Systems Ranking`)+1*(A$Enforcement)+4*(A$`Corruption Convictions`)+1*(A$`Rate per  100,000`))/11
A$Legal_Quality <- rank((A$W_aver))
New<-A

for (i in (2:length(years))) {
  A <- Legal_df[Legal_df$Year %in% paste(years[i], sep=""), ]
  A$W_aver <- (0.5*(A$`Liability Systems Ranking`)+.1*(A$Enforcement)+.4*(A$`Corruption Convictions`)+.1*(A$`Rate per  100,000`))/11
  A$Legal_Quality <- rank((A$W_aver))
  New <- rbind(New, A)
}


# Merge
Legal_df <- New[,-7]

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X")                                                               
# save to Excel
write_xlsx(Legal_df,"Legal Ranking Cleaned.xlsx")                                                

###### -------------------------------------------------------------------------------------- Control vars <- ----------------------------------------

#-------------------- Employment

years <- c(2017,2015,2012,2010,2008,2007,2006,2005,2004,2003,2002)

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Employment")
#------ First Year: 2017 
i <- 1
my_dat <- read_excel(paste("Employment ",years[i],".xlsx", sep=""), sheet = "Data")

Employed_rows <- which(my_dat[, paste("...1", sep="")]=="Employed",arr.ind=TRUE)[1]
Percent_rows <- as.data.frame(which(my_dat=="Percent",arr.ind=TRUE)[,2])
States <- as.data.frame(colnames(my_dat))
States <- States[order(colnames(my_dat)),]
States<-States[c(158:length(States))]

my_dat_1 <- my_dat[Employed_rows,Percent_rows[,1]]

# transpose
my_dat_1<-t(my_dat_1)
# delete all % symbols
my_dat_1 <- str_replace_all(my_dat_1, c("%"=""))
# cbind states
my_dat_1 <- cbind(my_dat_1, States)

# change colname
colnames(my_dat_1)[1]<-"Employment"
# change to dataframe
my_dat_1 <- as.data.frame(my_dat_1)
# add year
my_dat_1$Year <- years[i]
# final dataset
Employment<- my_dat_1


#------ from 2010 to 2015

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Employment")
for (i in (2:4)) {
  my_dat <- read_excel(paste("Employment ",years[i],".xlsx", sep=""), sheet = "Data")
  
  Employed_rows <- which(my_dat[, paste("...1", sep="")]=="Employed",arr.ind=TRUE)[1]
  Percent_rows <- as.data.frame(which(my_dat=="Percent",arr.ind=TRUE)[,2])
  States <- as.data.frame(colnames(my_dat))
  States <- States[order(colnames(my_dat)),]
  States<-States[c(158:length(States))]
  
  my_dat_1 <- my_dat[Employed_rows,Percent_rows[,1]]
  
  # transpose
  my_dat_1<-t(my_dat_1)
  # delete all % symbols
  my_dat_1 <- str_replace_all(my_dat_1, c("%"=""))
  # cbind states
  my_dat_1 <- cbind(my_dat_1, States)
  
  # change colname
  colnames(my_dat_1)[1]<-"Employment" 
  # change to dataframe
  my_dat_1 <- as.data.frame(my_dat_1)
  # add year
  my_dat_1$Year <- years[i]
  # merge
  Employment<-rbind(Employment,my_dat_1)
}

# --- from 2002 - 2008                                     
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Employment")

my_dat <- read_excel("Empoyment rate_2002_2008.xlsx") 
# merge
Employment<-rbind(Employment,my_dat)

# save to Excel
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Employment")
write_xlsx(Employment,"Employment Cleaned.xlsx")                                                



#-------------------- GDP
years <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/GDP")
GDP <- read_excel("Gross Domestic Product (GDP) summary, quarterly by state.xls")
GDP<-GDP[,-1]
colnames(GDP) <- GDP[5,]
GDP <- GDP[-(1:5), ] 
GDP <- as.data.frame(GDP)
GDP<-na.omit(GDP)
# convert to numeric
for (i in (2:length(GDP))) {
  GDP[,i]<- as.numeric(GDP[,i])
}
# convert from quarterly to annually
# first year        
i <- 1       
Quarters <- c("Q1", "Q2", "Q3", "Q4")
A <- data.frame(GDP[,paste(years[i], ":", Quarters[1], sep="")]+GDP[,paste(years[i], ":", Quarters[2], sep="")]+GDP[,paste(years[i], ":", Quarters[3], sep="")]+GDP[,paste(years[i], ":", Quarters[4], sep="")])
GDPs<- cbind(GDP$GeoName,A)
GDPs$Year <- years[i]
colnames(GDPs)[1] <- "State"
colnames(GDPs)[2] <- "GDP"
# all other years

for (i in (2:(length(years)))) {
  A <- data.frame(GDP[,paste(years[i], ":", Quarters[1], sep="")]+GDP[,paste(years[i], ":", Quarters[2], sep="")]+GDP[,paste(years[i], ":", Quarters[3], sep="")]+GDP[,paste(years[i], ":", Quarters[4], sep="")])
  colnames(A)[1]<- years[i]
  A <- cbind(GDP$GeoName, A)
  A$Year <- years[i]
  colnames(A)[1]<- "State"
  colnames(A)[2] <- "GDP"
  GDPs<- rbind(GDPs, A)
}


setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/GDP")
write_xlsx(GDPs,"GDP Cleaned.xlsx")    

#-------------------- Education
years <- c(2017,2015,2012,2010)
# First year : 2017
i <- 1
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Human Capital")
my_dat <- read_excel(paste("Educational Attainment ", years[i], ".xlsx", sep=""),     sheet = "Data")
my_dat <- my_dat[c(15,16),]
# replace all numeric and comma values with nothing
(my_dat <- data.frame(sapply(my_dat, function(x) as.numeric(gsub("%", "", x))))) 
# sum rows
my_dat[3,]<-colSums(my_dat)
my_dat<-my_dat[-c(1,2),]
A<- as.data.frame(t(my_dat[1,]))
A <- A %>% fill(`3`, .direction = "up")
my_dat<- as.data.frame(t(A))
my_dat <- my_dat %>% select(-contains("..."))
my_dat<- t(my_dat)
# change col name
colnames(my_dat)[1]<- "Higher_Ed"
# year
my_dat<- as.data.frame(my_dat)
my_dat$Year<- years[i]
my_dat <- tibble::rownames_to_column(my_dat, "State")
my_dat$State <- lapply(my_dat$State, gsub, pattern = ".", replacement = " ", fixed = TRUE)   
# Final dataset
Education <- my_dat

# from 2010-2015

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Human Capital")
#for (i in (2:length(years))) {
i <- 2
  my_dat <- read_excel(paste("Educational Attainment ", years[i], ".xlsx", sep=""),     sheet = "Data")
  my_dat <- my_dat[c(15,16),]
  # replace all % values with nothing
  (my_dat <- data.frame(sapply(my_dat, function(x) as.numeric(gsub("%", "", x))))) 
  # sum rows
  my_dat[3,]<-colSums(my_dat)
  my_dat<-my_dat[-c(1,2),]
  A<- as.data.frame(t(my_dat[1,]))
  A <- A %>% fill(`3`, .direction = "up")
  my_dat<- as.data.frame(t(A))
  my_dat <- my_dat %>% select(-contains("..."))
  my_dat<- t(my_dat)
  # change col name
  colnames(my_dat)[1]<- "Higher_Ed"
  # year
  my_dat<- as.data.frame(my_dat)
  my_dat$Year<- years[i]
  my_dat <- tibble::rownames_to_column(my_dat, "State")
  my_dat$State <- lapply(my_dat$State, gsub, pattern = ".", replacement = " ", fixed = TRUE)   
  # Final dataset
  Education <- rbind(Education, my_dat)
  
  
  
 i <- 3
  my_dat <- read_excel(paste("Educational Attainment ", years[i], ".xlsx", sep=""),     sheet = "Data")
  my_dat <- my_dat[c(14,15),]
  # replace all % values with nothing
  (my_dat <- data.frame(sapply(my_dat, function(x) as.numeric(gsub("%", "", x))))) 
  # sum rows
  my_dat[3,]<-colSums(my_dat)
  my_dat<-my_dat[-c(1,2),]
  A<- as.data.frame(t(my_dat[1,]))
  A <- A %>% fill(`3`, .direction = "up")
  my_dat<- as.data.frame(t(A))
  my_dat <- my_dat %>% select(-contains("..."))
  my_dat<- t(my_dat)
  # change col name
  colnames(my_dat)[1]<- "Higher_Ed"
  # year
  my_dat<- as.data.frame(my_dat)
  my_dat$Year<- years[i]
  my_dat <- tibble::rownames_to_column(my_dat, "State")
  my_dat$State <- lapply(my_dat$State, gsub, pattern = ".", replacement = " ", fixed = TRUE)   
  # Final dataset
  Education <- rbind(Education, my_dat) 
  
  
i <- 4
  my_dat <- read_excel(paste("Educational Attainment ", years[i], ".xlsx", sep=""),     sheet = "Data")
  my_dat <- my_dat[c(14,15),]
  # replace all % values with nothing
  (my_dat <- data.frame(sapply(my_dat, function(x) as.numeric(gsub("%", "", x))))) 
  # sum rows
  my_dat[3,]<-colSums(my_dat)
  my_dat<-my_dat[-c(1,2),]
  A<- as.data.frame(t(my_dat[1,]))
  A <- A %>% fill(`3`, .direction = "up")
  my_dat<- as.data.frame(t(A))
  my_dat <- my_dat %>% select(-contains("..."))
  my_dat<- t(my_dat)
  # change col name
  colnames(my_dat)[1]<- "Higher_Ed"
  # year
  my_dat<- as.data.frame(my_dat)
  my_dat$Year<- years[i]
  my_dat <- tibble::rownames_to_column(my_dat, "State")
  my_dat$State <- lapply(my_dat$State, gsub, pattern = ".", replacement = " ", fixed = TRUE)   
  # Final dataset
  Education <- rbind(Education, my_dat) 
    
  
  
#}


# from 2002-2006
years <- c(2006,2005,2004,2003,2002)
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Human Capital")
for (i in (1:length(years))){
  my_dat <- read_excel(paste("Educational Attainment ", years[i], ".xls", sep=""),     sheet = "Table 13.")
  # rename columns
  colnames(my_dat) <- my_dat[4,]
  # delete rows
  my_dat <- my_dat[-c(1:6),]
  my_dat <- my_dat[-c(52:57),]
  # select Education % col
  my_dat <- my_dat[,c(1, 5)]
  # add year
  my_dat$Year <- years[i]
  # colname
  colnames(my_dat)[2] <- "Higher_Ed"
  # final dataset
  Education <- rbind(Education, my_dat)
}


A <-as.data.frame(Education$State)
A <- t(A)
Education[,1]<- A
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Human Capital")
write_xlsx(Education,"Education Cleaned.xlsx")          
#------------------- Industry
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Jobs by Industry")
# The EG coagglomeration index for many industries

####------ From 2002-2017
#my_dat_2 <- read_excel("Industry 2002.xls")  
# my_dat_1 <- read_excel("Industry 2003-2007.xls")  
my_dat <- read_excel("Industry 2008_2017.xls")


# from 2008-2017 
# clean datasets
colnames(my_dat) <- my_dat[5,]
# delete rows
my_dat <- my_dat[-c(1:5),]
# select industries

# First industry
i <- 1
a <- i*100
industry <- data.frame(which(my_dat[, paste("LineCode", sep="")]==paste(a, sep=""),arr.ind=TRUE))$row
my_dat_1 <- my_dat[industry,]
# final dataframe
Industries <- my_dat_1

# all other industries      
for (i in (2:20)) {
  a <- i*100
  industry <- data.frame(which(my_dat[, paste("LineCode", sep="")]==paste(a, sep=""),arr.ind=TRUE))$row
  my_dat_1 <- my_dat[industry,]
  Industries <- rbind(Industries, my_dat_1)
}

# create dataframe
Industries <- as.data.frame(Industries)
# convert to numeric
for (i in (5:length(Industries))) {
  Industries[,i]<-as.numeric(Industries[,i])
}

# replace all NA's with 0's
Industries[is.na(Industries)] <- 0





# index
# employment average for industries in each state across the years  
# first state in the first year                                                     
state <- unique(Industries$GeoName)
j <- 5 # year / col
i <- 1 # state / row
geo_location <- which(Industries[, paste("GeoName", sep="")]==paste(state[i], sep=""),arr.ind=TRUE)
Xm <- mean((Industries[geo_location,j])/sum(Industries[geo_location,j]))
# make as dataframe
industry_means <- data.frame(state[i], Xm, colnames(Industries[j]))
# colnames
colnames(industry_means)[1] <- "State"
colnames(industry_means)[2] <- "indus_employ_mean"
colnames(industry_means)[3] <- "Year"
# final dataframe
ind_mean <- industry_means

for (j in (5:length(Industries))) {     
  geo_location <- which(Industries[, paste("GeoName", sep="")]==paste(state[i], sep=""),arr.ind=TRUE)
  Xm <- mean((Industries[geo_location,j])/sum(Industries[geo_location,j]))
  # final dataframe
  industry_means <- data.frame(state[i], Xm, colnames(Industries[j]))
  # colnames
  colnames(industry_means)[1] <- "State"
  colnames(industry_means)[2] <- "indus_employ_mean"
  colnames(industry_means)[3] <- "Year" 
  # rbind
  ind_mean <- rbind(ind_mean, industry_means)
}


# all other ones
# j <- 5 # year / col
for (j in (5:length(Industries))) {  
  #  i <- 1 # state / row
  for (i in (2:length(state))) {        
    geo_location <- which(Industries[, paste("GeoName", sep="")]==paste(state[i], sep=""),arr.ind=TRUE)
    Xm <- mean((Industries[geo_location,j])/sum(Industries[geo_location,j]))
    # final dataframe
    industry_means <- data.frame(state[i], Xm, colnames(Industries[j]))
    # colnames
    colnames(industry_means)[1] <- "State"
    colnames(industry_means)[2] <- "indus_employ_mean"
    colnames(industry_means)[3] <- "Year" 
    # rbind
    ind_mean <- rbind(ind_mean, industry_means)
  }
}                                        


# share of employment per industry - mean for each state

# first state                                                      
state <- unique(Industries$GeoName)
j <- 5 # year / col
i <- 1 # state / row
geo_location <- which(Industries[, paste("GeoName", sep="")]==paste(state[i], sep=""),arr.ind=TRUE)
Sm <- Industries[geo_location,c(2,4,j)]
aver <- which(ind_mean[, paste("State", sep="")]==paste(state[i], sep=""),arr.ind=TRUE)
aver <- ind_mean[aver,]
yr <- which(aver[, paste("Year", sep="")]==paste(colnames(Industries)[j], sep=""),arr.ind=TRUE)
aver <- aver[yr,2]
Sm$aver <- aver
Sm$Sm <- as.numeric(Sm[,3])/sum(as.numeric(Sm[,3])) - Sm$aver
Sm$Year <- colnames(Industries)[j]
# colnames
colnames(Sm)[1]<- "State"
colnames(Sm)[2]<- "Industry"
colnames(Sm)[3]<- "Employment"
# delete all 0's
Sm[Sm==0] <- NA
Sm<-Sm[complete.cases(Sm),]
# convert to share

# Index
Sm$EG <- prod(Sm$Sm)/(1-aver^2)

# final dataset
Ind_Index <- Sm

for (j in (5:length(Industries))) {  
  geo_location <- which(Industries[, paste("GeoName", sep="")]==paste(state[i], sep=""),arr.ind=TRUE)
  Sm <- Industries[geo_location,c(2,4,j)]
  aver <- which(ind_mean[, paste("State", sep="")]==paste(state[i], sep=""),arr.ind=TRUE)
  aver <- ind_mean[aver,]
  yr <- which(aver[, paste("Year", sep="")]==paste(colnames(Industries)[j], sep=""),arr.ind=TRUE)
  aver <- aver[yr,2]
  Sm$aver <- aver
  Sm$Sm <- as.numeric(Sm[,3])/sum(as.numeric(Sm[,3])) - Sm$aver
  Sm$Year <- colnames(Industries)[j]
  # colnames
  colnames(Sm)[1]<- "State"
  colnames(Sm)[2]<- "Industry"
  colnames(Sm)[3]<- "Employment"
  # delete all 0's
  Sm[Sm==0] <- NA
  Sm<-Sm[complete.cases(Sm),]
  # convert to share
  
  # Index
  Sm$EG <- prod(Sm$Sm)/(1-aver^2)
  
  # final datset
  Ind_Index <- rbind(Ind_Index, Sm)
}









# all other ones                     
for (j in (5:length(Industries))) {  
  #  i <- 1 # state / row
  for (i in (2:length(state))) {        
    geo_location <- which(Industries[, paste("GeoName", sep="")]==paste(state[i], sep=""),arr.ind=TRUE)
    Sm <- Industries[geo_location,c(2,4,j)]
    aver <- which(ind_mean[, paste("State", sep="")]==paste(state[i], sep=""),arr.ind=TRUE)
    aver <- ind_mean[aver,]
    yr <- which(aver[, paste("Year", sep="")]==paste(colnames(Industries)[j], sep=""),arr.ind=TRUE)
    aver <- aver[yr,2]
    Sm$aver <- aver
    Sm$Sm <- as.numeric(Sm[,3])/sum(as.numeric(Sm[,3])) - Sm$aver
    Sm$Year <- colnames(Industries)[j]
    # colnames
    colnames(Sm)[1]<- "State"
    colnames(Sm)[2]<- "Industry"
    colnames(Sm)[3]<- "Employment"
    # delete all 0's
    Sm[Sm==0] <- NA
    Sm<-Sm[complete.cases(Sm),]
    # convert to share
    
    # Index
    Sm$EG <- prod(Sm$Sm)/(1-aver^2)
    
    # final datset
    Ind_Index <- rbind(Ind_Index, Sm)
  }
}   



EG_Index <- Ind_Index %>%
  group_by(State, Year) %>%
  summarise(EG=unique(EG))                               

EG_Index$EG_norm <- (EG_Index$EG - mean(EG_Index$EG)) / sd(EG_Index$EG)

# export to Excel    
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Jobs by Industry")
write_xlsx(EG_Index,"EG_Index Cleaned.xlsx")                                                         




#---------------- Per capita Income
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Per capita income")

my_dat <- read_excel("Per Capita Personal Income.xls",  sheet = "Sheet0")
colnames(my_dat) <- my_dat[5,]
my_dat <- my_dat[-c(1:5), -1]
my_rows <- as.data.frame(which(my_dat == "Per capita personal income (dollars) 2/", arr.ind=TRUE))$row
my_dat <- my_dat[c(my_rows),]
my_dat <- my_dat[,-c(2,3)]
# delete all *'s
my_dat[3,1] <- "Alaska"
my_dat[13,1] <- "Hawaii"
# colnames
colnames(my_dat)[1] <- "State"
# make final dataset
# first year : 2002
i <- 2
A <- my_dat[,c(1,i)]
colnames(A)[2]<- "Per capita personal income"
A$Year <- colnames(my_dat)[i]
Income <- A

# all other years
for (i in (3:length(my_dat))){
  A <- my_dat[,c(1,i)]
  colnames(A)[2]<- "Per capita personal income"
  A$Year <- colnames(my_dat)[i]
  Income <- rbind(Income, A)
}



setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Per capita income")
write_xlsx(Income,"Income Cleaned.xlsx")         


#--------------Population

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Population")

my_dat <- read_excel("Per Capita Personal Income.xls",  sheet = "Sheet0")
colnames(my_dat) <- my_dat[5,]
my_dat <- my_dat[-c(1:5), -1]
my_rows <- as.data.frame(which(my_dat == "Population (persons) 1/", arr.ind=TRUE))$row
my_dat <- my_dat[c(my_rows),]
my_dat <- my_dat[,-c(2,3)]
# delete all *'s
my_dat[3,1] <- "Alaska"
my_dat[13,1] <- "Hawaii"
# colnames
colnames(my_dat)[1] <- "State"
# make final dataset
# first year : 2002
i <- 2
A <- my_dat[,c(1,i)]
colnames(A)[2] <- "Population"
A$Year <- colnames(my_dat)[i]
Population <- A

# all other years
for (i in (3:length(my_dat))){
  A <- my_dat[,c(1,i)]
  colnames(A)[2]<- "Population"
  A$Year <- colnames(my_dat)[i]
  Population <- rbind(Population, A)
}







setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Population")
write_xlsx(Population,"Population Cleaned.xlsx")   





# import variables
setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Dependent Var")
F1 <- read_excel("F1 Cleaned.xlsx")
F2 <- read_excel("F2 Cleaned.xlsx")
F3 <- read_excel("F3 Cleaned.xlsx")
F4 <- read_excel("F4 Cleaned.xlsx")
F5 <- read_excel("F5 Cleaned.xlsx")
F6 <- read_excel("F6 Cleaned.xlsx")
F7 <- read_excel("F7 Cleaned.xlsx")
F8 <- read_excel("F8 Cleaned.xlsx")
FTot <- read_excel("FTot Cleaned.xlsx")



setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Main X")                                                               
Legal_Ranking <- read_excel("Legal Ranking Cleaned.xlsx") 

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Employment")
Employment <- read_excel("Employment Cleaned.xlsx")                                              

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/GDP")
GDP <- read_excel("GDP Cleaned.xlsx")   

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Human Capital")
Education <- read_excel("Education Cleaned.xlsx")  

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Per capita income")
Income <- read_excel("Income Cleaned.xlsx")

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Population")
Population <- read_excel("Population Cleaned.xlsx")

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Jobs by Industry")
EG_Index <- read_excel("EG_Index Cleaned.xlsx")




# create instrument variable


#------------------------------------------------------------------------------ Size of Market (instrument)
colnames(Employment)[2] <- "State"
Market_Size<-merge(GDP, Population, by=c("State","Year")) 
Market_Size <- merge(Market_Size, Employment, by=c("State","Year"))
Market_Size$GDP<- as.numeric(Market_Size$GDP)
Market_Size$Population<- as.numeric(Market_Size$Population)
Market_Size$Employment<- as.numeric(Market_Size$Employment)
Market_Size$log_Employment <- Market_Size$Employment


Market_Size$log_GDP <- log(Market_Size$GDP)
Market_Size$log_Population <- log(Market_Size$Population)

# instrumental var
iv = lm(log_Employment ~ log_Population + log_GDP, data = Market_Size)
Market_Size$Market_Size<-iv$fitted.values

#------------------------------------------------------------------------------ Generate final dataset         
#####


Final_df <-  merge(FTot, Legal_Ranking[,c(1,2,length(Legal_Ranking))], by=c("State","Year"))
colnames(Final_df)[5] <- "FTot"
Final_df <-  merge(Final_df, Education, by=c("State","Year"))
Final_df <-  merge(Final_df, Income, by=c("State","Year"))
Final_df <-  merge(Final_df, Population, by=c("State","Year"))
Final_df <-  merge(Final_df, EG_Index, by=c("State","Year"))

Final_df <-  merge(Final_df, F1, by=c("State","Year"))
colnames(Final_df)[14] <- "F1"
Final_df <-  merge(Final_df, F2, by=c("State","Year"))
colnames(Final_df)[17] <- "F2"
Final_df <-  merge(Final_df, F3, by=c("State","Year"))
colnames(Final_df)[20] <- "F3"
Final_df <-  merge(Final_df, F4, by=c("State","Year"))
colnames(Final_df)[23] <- "F4"
Final_df <-  merge(Final_df, F5, by=c("State","Year"))
colnames(Final_df)[26] <- "F5"
Final_df <-  merge(Final_df, F6, by=c("State","Year"))
colnames(Final_df)[29] <- "F6"
Final_df <-  merge(Final_df, F7, by=c("State","Year"))
colnames(Final_df)[32] <- "F7"
Final_df <-  merge(Final_df, F8, by=c("State","Year"))
colnames(Final_df)[35] <- "F8"
Final_df <-  merge(Final_df, Market_Size[,c(1,2,9)], by=c("State","Year"))

setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Final Dataset") 
write_xlsx(Final_df,"Final_df.xlsx") 
