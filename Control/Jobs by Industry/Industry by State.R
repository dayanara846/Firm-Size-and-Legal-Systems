setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Jobs by Industry")



#install.packages("devtools")
#devtools::install_github("thomasp85/patchwork") 
library(patchwork)
library(ggplot2)
library(dplyr)



####------ From 2013-2017
my_dat <- read_excel("Industry 2013_2017.xls")


  # from 2013-2017 
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
                  
                  


                  # export to Excel    
                  setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Jobs by Industry")
                  write_xlsx(Industries,"Industries Cleaned 2013_2017.xlsx")                                                         






my_dat <- read_excel("Industry 2008_2012.xls")




####------ From 2008-2012
    # from 2008-2012 
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
              



                # export to Excel    
                setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Jobs by Industry")
                write_xlsx(Industries,"Industries Cleaned 2008_2012.xlsx")                                                         
                               
                



