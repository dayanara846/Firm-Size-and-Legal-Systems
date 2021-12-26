#install.packages("devtools")
#devtools::install_github("thomasp85/patchwork") 
library(patchwork)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)
library(colorspace)
library(ggrepel)
# install.packages("remotes")
# remotes::install_github("wilkelab/practicalgg")
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
library(stargazer)
library(reshape)
library(gridExtra)
library(grid)
library(tinytex)
library(readxl)
library(tidyr)
library(writexl)
library(plm)
library(ggridges)
library(regclass)
theme_set(theme_minimal())


colnames(df1)<- c("legal", "educ", "per_capita_inc", "pop", "Market_size", "HHI_ind", "F8")

df2 <- df1
df2$Year <- Final_df$Year
df2$State <- Final_df$State

df2 <- pdata.frame(df2, index = c("Year","State"), drop.index = TRUE, row.names = TRUE)

# graph 1
  A <- plm(F8 ~ educ + log(per_capita_inc) + log(pop) + Market_size + HHI_ind, data = df2, model = "within")
    df2$A<- A$residuals

      reg1 <- ggplot(df2, aes(legal,A)) +
        geom_point(alpha = 0.2, color = "mediumpurple1") +
      #  geom_smooth(method='lm', formula= y~x, color = "blue")+
        labs(x = "Legal", y="")
      
# graph 2
  A <- plm(F8 ~ legal + log(per_capita_inc) + log(pop) + Market_size + HHI_ind, data = df2, model = "within")
    df2$A<- A$residuals

        reg2 <- ggplot(df2, aes(educ,A)) +
          geom_point(alpha = 0.2, color = "mediumpurple1") + 
         # geom_smooth(method='lm', formula= y~x, color = "blue")+
          labs(y = "", x="Education")

# graph 3
  A <- plm(F8 ~ legal + educ + log(pop) + Market_size + HHI_ind, data = df2, model = "within")
    df2$A<- A$residuals
        
      reg3 <- ggplot(df2, aes(log(per_capita_inc),A)) +
        geom_point(alpha = 0.2, color = "mediumpurple1")  +
      #  geom_smooth(method='lm', formula= y~x, color = "blue")+
        labs(x = "Log of Per Capita Income", y="Std. Errors of Firms with 500+ Employees (%)")

# graph 4
  A <- plm(F8 ~ legal + educ + log(per_capita_inc) + Market_size + HHI_ind, data = df2, model = "within")
    df2$A<- A$residuals
      
      reg4 <- ggplot(df2, aes(log(pop),A)) +
        geom_point(alpha = 0.2, color = "mediumpurple1")  +
      #  geom_smooth(method='lm', formula= y~x, color = "blue")+
        labs(x = "Log of Population Density", y="")

      
# graph 5
  A <- plm(F8 ~ legal + educ + log(per_capita_inc) + log(pop) + HHI_ind, data = df2, model = "within")
    df2$A<- A$residuals    
    
      reg5 <- ggplot(df2, aes(Market_size,A)) +
        geom_point(alpha = 0.2, color = "mediumpurple1")  +
      #  geom_smooth(method='lm', formula= y~x, color = "blue")+
        labs(x = "Market Size", y="")
      
      
# graph 6
  A <- plm(F8 ~ legal + educ + log(per_capita_inc) + log(pop) + Market_size, data = df2, model = "within")
    df2$A<- A$residuals    
    
      reg6 <- ggplot(df2, aes(HHI_ind,A)) +
        geom_point(alpha = 0.2, color = "mediumpurple1")  +
       # geom_smooth(method='lm', formula= y~x, color = "blue")+
        labs(x = "HHI", y="")



grid.arrange(reg1, reg2, reg3, reg4, reg5, reg6, top = "Multiple Scatter Plots Between Residuals and Each Independent Variable", bottom = textGrob("n = 200, Data from 2010,2012,2015, and 2017 U.S. Census Bureau", just ="left",gp=gpar(fontsize=7)))





df1$Year <- Final_df$Year

library(ggcorrplot)

colnames(df1)<- c("Legal Quality Ranking", "Educational Attainment", "Per Capita Income", "Population Density", "Market Size", "Herfindahl-Hirschman Index", "Firms 500+ employees", "Year")
State <- as.numeric(as.factor(Final_df$State))
df <- cbind(df1, State)
corr <- round(cor(df1), 1)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
   lab = TRUE)+
labs(title = "Correlation Matrix", caption = "n = 200, Data from 2010-2017, U.S. Census Bureau | Bureau of Economic Analysis | Chamber Institute for Legal Reform | FBI | Department of Justice")



library(ggplot2)  # The grammar of graphics package
library(maps)     # Provides latitude and longitude data for various maps

df1$State <- Final_df$State
colnames(df1)<- c("legal" , "educ", "per_capita_inc", "pop", "market", "HHI", "Firm_F8", "year", "state")

# load United States state map data
MainStates <- map_data("state")

# read the state population data
StatePopulation <- read.csv("https://raw.githubusercontent.com/ds4stats/r-tutorials/master/intro-maps/data/StatePopulation.csv", as.is = TRUE)

# Use the dplyr package to merge the MainStates and StatePopulation files
df1$region <- tolower(Final_df$State)

MergedStates <- inner_join(MainStates, df1, by = "region")








# for 2010
MergedStates10 <- MergedStates[MergedStates$year %in% "2010", ]
# Create a Choropleth map of the United States

# legal quality
# Create a Choropleth map of the United States
legal_map_10 <- ggplot()
legal_map_10 <- legal_map_10 + geom_polygon( data=MergedStates10, 
          aes(x=long, y=lat, group=group, fill = legal), 
          color="white", size = 0.2) 


legal_map_10 <- legal_map_10 + scale_fill_continuous(name="Legal Quality Ranking") +

          labs(title="Legal Quality Ranking in the Mainland United States for 2010")




# for 2017
MergedStates17 <- MergedStates[MergedStates$year %in% "2017", ]
# Create a Choropleth map of the United States

# legal quality
# Create a Choropleth map of the United States
legal_map_17 <- ggplot()
legal_map_17 <- legal_map_17 + geom_polygon( data=MergedStates17, 
          aes(x=long, y=lat, group=group, fill = legal), 
          color="white", size = 0.2) 


legal_map_17 <- legal_map_17 + scale_fill_continuous(name="Legal Quality Ranking") +

          labs(title="Legal Quality Ranking in the Mainland United States for 2017")






# total firms
Firm_Tot_Map_10 <- ggplot()
Firm_Tot_Map_10 <- Firm_Tot_Map_10 + geom_polygon( data=MergedStates10, 
          aes(x=long, y=lat, group=group, fill = Firm_F8), 
          color="white", size = 0.2) 




Firm_Tot_Map_10 <- Firm_Tot_Map_10 + scale_fill_continuous(name="Firms with 500+ Employees (%)") +

          labs(title="Percentage of Big Firms in the Mainland United States for 2010")




# for 2017


# total firms
Firm_Tot_Map_17 <- ggplot()
Firm_Tot_Map_17 <- Firm_Tot_Map_17 + geom_polygon( data=MergedStates17, 
          aes(x=long, y=lat, group=group, fill = Firm_F8), 
          color="white", size = 0.2) 




Firm_Tot_Map_17 <- Firm_Tot_Map_17 + scale_fill_continuous(name="Firms with 500+ Employees (%)") +

          labs(title="Percentage of Big Firms in the Mainland United States for 2017")




# for 2010
# market share
Market_Map_10 <- ggplot()
Market_Map_10 <- Market_Map_10 + geom_polygon( data=MergedStates10, 
          aes(x=long, y=lat, group=group, fill = market), 
          color="white", size = 0.2) 




Market_Map_10 <- Market_Map_10 + scale_fill_continuous(name="Market Size") +

          labs(title="Market Size of Each State in the Mainland United States for 2010")





# for 2017


# market share
Market_Map_17 <- ggplot()
Market_Map_17 <- Market_Map_17 + geom_polygon( data=MergedStates17, 
          aes(x=long, y=lat, group=group, fill = market), 
          color="white", size = 0.2) 




Market_Map_17 <- Market_Map_17 + scale_fill_continuous(name="Market Size") +

          labs(title="Market Size of Each State in the Mainland United States for 2017")





grid.arrange(Firm_Tot_Map_10, Firm_Tot_Map_17, bottom = textGrob("n = 200, Data from 2010 and 2017, U.S. Census Bureau", just ="left",gp=gpar(fontsize=7)))

df1 <- Final_df[,c("Legal_Quality", "Higher_Ed", "Per_capita_inc", "Population Density", "Market_Size", "HHI", "F1", "F2", "F3", "F5", "F6","F8", "Year", "State")]

# library
library(ggplot2)
 
# create a data frame
Firm1 <- data.frame(Number_of_Employees = rep("0-4", each=nrow(df1)), Year = df1$Year, Firm = df1[,"F1"])
colnames(Firm1)[3] <- "Firm"
Firm2 <- data.frame(Number_of_Employees = rep("5-9", each=nrow(df1)), Year = df1$Year, Firm = df1[,"F2"])
colnames(Firm2)[3] <- "Firm"
Firm3 <- data.frame(Number_of_Employees = rep("10-19", each=nrow(df1)), Year = df1$Year, Firm = df1[,"F3"])
colnames(Firm3)[3] <- "Firm"
Firm5 <- data.frame(Number_of_Employees = rep("20-99", each=nrow(df1)), Year = df1$Year, Firm = df1[,"F5"])
colnames(Firm5)[3] <- "Firm"
Firm6 <- data.frame(Number_of_Employees = rep("100-499", each=nrow(df1)), Year = df1$Year, Firm = df1[,"F6"])
colnames(Firm6)[3] <- "Firm"
Firm8 <- data.frame(Number_of_Employees = rep("500+", each=nrow(df1)), Year = df1$Year, Firm = df1[,"F8"]*as.numeric(Final_df$FTot)/100)
colnames(Firm8)[3] <- "Firm"


my_dat <- rbind(Firm1, Firm2, Firm3, Firm5, Firm6, Firm8)
my_dat$Year <- as.character(my_dat$Year)
my_dat$Firm <- as.numeric(my_dat$Firm)
my_dat$Firm <- log(my_dat$Firm)
# grouped boxplot
ggplot(my_dat, aes(x=Number_of_Employees, y=Firm, fill=Year)) + 
    geom_boxplot() +
labs(x = "Number of Employees", y = "Total Number of Firms (logged)")+
labs(title = "Boxplot of Firm Distribution Across the United States", caption = "n = 200, Data from 2010-2017, U.S. Census Bureau")


setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Data/Control/Jobs by Industry")
Industries <- read_excel("Industries Cleaned 2013_2017.xlsx")

# lollipop 2017
  # Arrange data
    A <- Industries[, c("Description", "2017")]
      A[A==0] <- NA
        A<-A[complete.cases(A),]
    
    
          A <- A %>%
            group_by(Description) %>%
            summarise_at(vars(`2017`), list(name = sum))
    
              A <- A %>%
                arrange(name) 
    
                A$Description <- as.factor(A$Description)
                    A$Description <- ordered(A$Description, levels = A$Description)
                  
              
                      A$name <- A$name/1000
                  # graph
                 
                Lol_2017 <- ggplot(A, aes(x=Description, y=name)) +
                    geom_segment(aes(x=Description, xend=Description, y=0, yend=name), color="skyblue") +
                    geom_point( color="blue", size=4, alpha=0.6) +
                    theme_light() +
                    coord_flip() +
                    theme(
                      panel.grid.major.y = element_blank(),
                      panel.border = element_blank(),
                      axis.ticks.y = element_blank()
                    ) +
        labs(x = "Industry", y = "Jobs", title = "Total Employment by Industry (in Millions) in 2017" , caption = "n = 1,020, Source: Bureau of Economic Analysis")
        
                
                
 Lol_2017 + theme(text = element_text(size = 30))                    # All font sizes
 
 
 

# import data
Inds <- Industries[, c("GeoName", "Description", "2017")]
colnames(Inds) <- c("State", "Industries", "Jobs")
Inds$Year <- 2017
Inds_1 <- Final_df[,c("State", "Year", "F8")]
new_dat <- merge(Inds, Inds_1, by=c("State","Year")) 


state_highlight <- c()

new_dat <- new_dat %>%
  mutate(
    label = ifelse(State %in% state_highlight, State, "")
  )


new_dat <- new_dat[new_dat$Jobs != 0, ]

new_dat$Jobs <- new_dat$Jobs/100 # in 100 Millions 

# change industry names to shorter ones
new_dat$Industries[new_dat$Industries == "Mining, quarrying, and oil and gas extraction"] <- "Mining, oil, gas"
new_dat$Industries[new_dat$Industries == "Government and government enterprises"] <- "Government"
new_dat$Industries[new_dat$Industries == "Arts, entertainment, and recreation"] <- "Art, entertainment"
new_dat$Industries[new_dat$Industries == "Finance and insurance"] <- "Finance"
new_dat$Industries[new_dat$Industries == "Forestry, fishing, and related activities"] <- "Forestry, fishing"
new_dat$Industries[new_dat$Industries == "Accommodation and food services"] <- "Accomodation"
new_dat$Industries[new_dat$Industries == "Educational services"] <- "Education"
new_dat$Industries[new_dat$Industries == "Other services (except government and government enterprises)"] <- "Other"
new_dat$Industries[new_dat$Industries == "Transportation and warehousing"] <- "Transportation"
new_dat$Industries[new_dat$Industries == "Administrative and support and waste management and remediation services"] <- "Administration"
new_dat$Industries[new_dat$Industries == "Health care and social assistance"] <- "Health care"
new_dat$Industries[new_dat$Industries == "Professional, scientific, and technical services"] <- "Professional"
new_dat$Industries[new_dat$Industries == "Real estate and rental and leasing"] <- "Real Estate"
new_dat$Industries[new_dat$Industries == "Management of companies and enterprises"] <- "Management"



######## graph

# Okabe Ito colors
# The last color is used for the regression fit.
region_cols <- c("#E8CACA", "#FFE5A8", "#F8FF97", "#BCFFA4", "#A6FFD3", "#B9E3FF", "#C1CCFF", "#E4CFFF", "#F1CEF4", # 9
                 "#FAC2C1", "#FA889E", "#c1f9fa", "#AF8969", "#B50000", "#4c65a4", "#f4bd00", "#A560EB", "#A560EB", # 9
                 "#d1c2ff", "#b1984c", "#d0c693", "#999999")


ggplot(new_dat, aes(Jobs, F8)) +
  # Adding the regression fit before the points make sure the line stays behind the points.
  geom_smooth(
    aes(color = "y ~ x", fill = "y ~ x"),
    method = "lm", 
    formula = y~x, 
    se = FALSE, # Plot the line only (without confidence bands)
    fullrange = TRUE # The fit spans the full range of the horizontal axis
  ) +
  geom_point(
    aes(color = Industries, fill = Industries),
    size = 2.5, alpha = 0.5, 
    shape = 21 # This is a dot with both border (color) and fill.
  ) +
  # Add auto-positioned text
  geom_text_repel(
    aes(label = label),
    color = "black",
    size = 9/.pt, # font size 9 pt
    point.padding = 0.1, 
    box.padding = 0.6,
    min.segment.length = 0,
    max.overlaps = 1000,
    seed = 7654 # For reproducibility reasons
  ) +
  scale_color_manual(
    name = NULL, # it's one way to omit the legend title
    values = darken(region_cols, 0.3) # dot borders are a darker than the fill
  ) +
  scale_fill_manual(
    name = NULL,
    values = region_cols
  ) +
  # Add labels and customize axes
  scale_x_continuous(
    name = "Full-Time and Part-Time Employment by Industry (in 100 Millions)",
    limits = c(0, max(new_dat$Jobs)+100),
    breaks = c(6000, 6000*2, 6000*3, 6000*4, 6000*5),
    expand = c(0, 0) # This removes the default padding on the ends of the axis
  ) +
  scale_y_continuous(
    name = "Percentage of Firms with 500+ Employees",
    limits = c(0, 7),
    breaks = c(1, 2, 3, 4, 5, 6, 7), # Manually set axis breaks
    expand = c(0, 0)
  )  +
  # Minimal grid theme that only draws horizontal lines
  theme_minimal_hgrid(12, rel_small = 1) +
  # Customize aspects of the legend
  theme(
    legend.position = "top",
    legend.justification = "right",
    legend.text = element_text(size = 9),
    legend.box.spacing = unit(0, "pt")
  ) +
  labs(title = "Total Employment in Big Firms by Industry for 2017" , caption = "n = 1,000, Source: Bureau of Economic Analysis")



 df1 <- Final_df[, c("Legal_Quality", "Higher_Ed", "Per_capita_inc", "Population Density", "Market_Size", "HHI", "F8")]


df1 <- df1 %>% 
   mutate_all(funs(as.numeric(.)))

df1$year <- Final_df$Year

ggplot(df1, aes(x = Per_capita_inc/1000, y = as.factor(year), group = as.factor(year)))+
  geom_density_ridges(fill = "firebrick")+
  labs(x= "Bins for Per Capita Income (in Thousands of Dollars)", y = "Year", title = "Distribution of Per Capita Income by Year", caption = "n = 200, Data from 2010-2017, Bureau of Economic Analysis")


setwd("C:/Users/scyth/OneDrive - Northeastern University/Econometrics/Project Part_2/Part 2")
Final_df <- read_excel("Final_df.xlsx")
  
colnames(Final_df)[8] <- "Per_capita_inc"
  
    
df1 <- data.frame(Final_df$Legal_Quality , Final_df$Higher_Ed, Final_df$Per_capita_inc, Final_df$`Population Density`, Final_df$Market_Size, Final_df$HHI, Final_df$FTot, Final_df$F1, Final_df$F2, Final_df$F3, Final_df$F5, Final_df$F6, Final_df$F8)


df1 <- df1 %>% 
   mutate_all(funs(as.numeric(.)))

df1$year <- Final_df$Year
df1$state <- Final_df$State

colnames(df1)<- c("legal" , "educ", "per_capita_inc", "pop", "market", "HHI","Firm_Tot", "Firm_F1", "Firm_F2", "Firm_F3", "Firm_F5", "Firm_F6", "Firm_F8", "year", "state")
df1$Firm_F8 <- df1$Firm_F8/df1$Firm_Tot*100



df1 <- pdata.frame(df1, index = c("year","state"), drop.index = TRUE, row.names = TRUE)


pm1 <- plm(Firm_F8 ~ legal + educ + log(per_capita_inc) + log(pop) + market + HHI, data = df1, model = "within")
pm2 <- plm(Firm_F8 ~ legal + educ + log(per_capita_inc) + log(pop) + market, data = df1, model = "within")
pm3 <- plm(Firm_F8 ~ legal + educ + log(per_capita_inc) + log(pop), data = df1, model = "within")
pm4 <- plm(Firm_F8 ~ legal + educ + log(per_capita_inc), data = df1, model = "within")
pm5 <- plm(Firm_F8 ~ legal + educ, data = df1, model = "within")
pm6 <- plm(Firm_F8 ~ legal, data = df1, model = "within")

A <- huxreg(pm6, pm5, pm4, pm3,pm2,pm1, statistics = c("N. obs." = "nobs", "R squared" = "r.squared", "Adj R Squared" = "adj.r.squared","P value" = "p.value"), coefs = c("Legal Quality Ranking" = "legal","Educational Attainment" = "educ","Log of Per Capita Income"= "log(per_capita_inc)","Log of Population Density" = "log(pop)","Market Size" = "market","Herfindahl-Hirschman Index" = "HHI"), stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01), error_pos = "same") %>%
  set_caption("Fixed Effects Regression of Percentage of Firms with 500+")  

huxtable::font_size(A) <- 10
# huxtable::guess_knitr_output_format()
huxtable::height(A) <- 1
huxtable::width(A) <- 1
A



