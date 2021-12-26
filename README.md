# Firm-Size-and-Legal-Systems
In this project I explore the relationship between firm size and legal systems in the United States.







---
title: 'Case Closed, More Growth? How Legal Systems Affect Firm Size '
author: | 
  | Dayanara M. Diaz Vargas
  | Department of Economics, Northeastern University
  | Dr. Gustavo Vicentini
  | ECON 7240: Workshop in Applied Econometrics
date: "Due 12/17/2021"
output:
  html_document:
    df_print: paged
  pdf_document: default
---

```{r, message = FALSE, echo= TRUE, warning = FALSE}
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
```







\newpage



# I. Introduction 

Economic disparities drive research on economic growth, as much of it explores the size of existing organizations. For instance, Saborowski & Misch (2019) show that firm size matters for development by illustrating a close correlation with state-level per capita incomes. Similarly, firm size has been found to be related to innovation, as R&D spending and R&D productivity increases with firm size (Anne & Viereggerb, 2020). For this reason, it is imperative to ascertain the determinants of firm size - as it may allow us to develop a better grasp on growth drivers. Similarly, firm size analysis may also be key to explain interesting economic phenomena in other areas such as credit rationing, stock returns and wage gap. In this regard, Vinas F. (2021) shows that small firms seem to be less impacted by credit rationing than medium and large firms during financial crises. Similarly, Hou & van Dijk (2019) demonstrate how firm size can provide different profitability shocks and expected stock returns. Whereas Damir Cosic (2018) shows that a recent trend has been seen where the wage gap has exacerbated in larger firms. Seeing the various benefits for exploring the determinants of firm size, this topic seems as relevant as ever. Regarding this, there are many important factors that have been proven throughout the years that highly influence firm size. For instance, Glaeser, Kallal, Scheinkman, & Schleifer (1991) and Audretsch & Feldman (1999) have found that location specific factors affect firm size, demonstrating that spatial concentration of economic activity and urban variety, (and not specialization) encourages employment growth in industries due to knowledge spillover. Similarly, the literature may point us towards inferring that that industry affects firm size as Audretsch & Acs (1987) proved that firms exhibit a different behavior according to their size and industry - as market concentration, extent of entry barriers, firm distribution and the impact of innovation exhibit different compositions and effects.

More on this, there is a vast literature that sustains that government institutions (later on, referred to just as institutions) affect macroeconomic outcomes (Acemoglu D. , Johnson, Robinson, & Thaicharoen, 2003) (Acemoglu, Johnson, & Robinson, Institutions as a fundamental cause of long-run growth, 2005) (Kose, Prasad, Rogoff, & Wei, 2009). Likewise, regarding the microeconomic performance of an economy, much evidence suggests that institutions affect entrepreneurs' investment behavior (Johnson, McMillan, & Woodruff, 2002) (Ge, Stanley, Eddleston, & Kellermanns, 2017). However, there is limited literature on the relationship between institutions and firm size. Dougherty (2014), Laeven & Woodruff (2007) explored this with data from Mexico, while Lopez-Martin & Perez-Reyna (2021)  did this with cross-country data; both concluding that there is a significant relationship between the quality of the legal system and firm size. However, there does not seem to be any research on the relationship between institutions and firm size in countries with a well-established legal systems. An approach such as this could aid in our understanding of institutions and firm behavior, as it helps mitigate the added endogenous problematics that surge in countries with weak institutions.

That is, in countries were weak institutions prevale, other important factors that may be directly affecting market structures may also prevale. One example of this may be that weak institutions and small firm growth can both be understood as the effect of poor educational attainment in a country, as well as viceversa (in the case of institutions). Moreover, access to information and communications may affect institutions and firms behavior at the same time, as media censorship or poor internet access directly impacts consumers and investors decision making. In conclusion, minimal conditions for the prevalence of firms may not be met in developing economies, such that whenever studying the legal system and firm distribution a spurious relationship may be identified, due to both variables being directly related to an omitted variable that is causing both of them to perform poorly.

As a result, this study attempts to reduce the limitations of previous work, by studying institutions and firm size in a well-developed nation. The performance of firms -in terms of the number of employees it has- will be analyzed through a measure of the efficiency of legal systems in each state. This measure will be constructed in this study as an adaptation of Laeven & Woodruff's (2007) methodology - by taking different metrics on legal system quality and estimating a weighted average. The final analysis will be achieved through a series of fixed effects regressions that control for social, economic and spatial factors.

Thus, the United States of America has been chosen as the region of study, as its states provide an economically diverse environment. This feature is important, as it may help us explain the observed differences in economy-wide firm size distributions. Moreover, the United States is a well-developed country, so minimal conditions for the prevalence of firms are satisfied. That is, basic respect for property rights, efficient judiciary systems and educational levels exist to sustain the complexities of firm management. Additionally, as these are states of the same country, the economic system and financial interruptions (i.e. war, federal political changes, monetary exchange rates, etc.) are held constant. This provides an overview of subtle economic factors for which there is a wide variation across states.

Additionally, as states provide different economic environments across industries, we can come to better understand the effects of industry dynamics and its effects on firm size. Moreover, a cross-state comparison provides us a better understanding of spatial dynamics. Furthermore, states have some legislative autonomy, which aids in the understanding of the effect of subtle particularities in the justice system across firm size distribution. More on this, by selecting the United States as the region of study, we are able to analyze the dynamics between legal systems and firm size in a large, developed economy -the United States of America- where legal system quality and enforcement not only varies across states but are also well established. Even though the relevant commercial laws are national in scope, with only minor variation across states, an important factor regarding the legal systems comparison across states comes from the effect that state laws and state legal enforcement have on the application of law by courts and the ability of claimants to enforce verdicts.

For this reason, an empirical approach has been undertaken in this study, to demonstrate the specific factors of a legal system that affect big firms across America. The study is based on panel data from 2010-2017, collected from the US Census Bureau, the FBI's Universal Crime Report, the Bureau of Labor Statistics, and the Lawsuit Climate Survey -- a Ranking of the States conducted for the U.S. Chamber Institute for Legal Reform by The Harris Poll. The US Census Bureau and the Bureau of Labor Statistics provide a wide array of data with details on States' demographics, firm agglomeration on a location and labor conditions; the FBI's dataset is used to generate a legal system quality index, while the U.S. Chamber for Legal Reform provides an overview of the legal systems across the United States. By using panel data estimators, we aim to explore the effects of the evolution of judicial quality over time and big firm distribution.

The main hypothesis is that legal quality has a positive and statistically significant effect (at the 0.05 significance level) over the percentage of big firms. The purpose of this research is to provide evidence to sustain public policy aimed at job creation through the private sector, as well as a holistic methodology for firms' efficient location selection. 


\newpage

# II. Literature Review
 
The point of departure of firm size and growth is Gibrat's Law, which states that firm's growth rates are independent of their size. From that point, many studies have taken up the task of testing its validity (i.e. Eeckhout (2009); Hart & Oulton (1999)). On this regard, Sutton, J. (1997) illustrates that earlier findings supported Gilbrat's Law, showing that firm growth was independent of size. Yet, these initial studies were mostly done on data from big companies. With time, as further studies included other ranges of firm sizes, Gibrat's Law did not hold. Subsequently, it was even proven that firm growth was negatively related to size. Moreover, research has come to illustrate that newer firms tend grow at a higher rate than older ones. Furthermore, recent studies have even come to demonstrate the relationship between firm size and location, as Komarek & Loveridge (2014) illustrate that the effect of firm size varies by location -proving that there is a positive link between firm size and employment in some regions in the United States, but not for others.

Alternatively, regional economics have also explored the complexities of industry dynamics. A wide array of literature examines the contributing factors of firm entry across regions (Audretsch & Fritsch, 1999; Cuervo, 2008; Seim, 2006). While studies in regional economics have taken up the task of identifying determinants of new firm entry, not many have analyzed the influence that location plays in a firm's long-term performance. Despite this, there are many reasons to believe that location affects firm growth. According to Alfred Marshall's Agglomeration Theory, firms should receive increasing returns from the mix of agglomeration economies - local skilled labor, local supplier linkages, and local knowledge spillovers. Concluding that location and proximity influence firm performance.

Moreover, an organizational perspective is essential for the analysis of firm behavior. In this regard, evidence shows that at the country level, firm size is positively related to financial intermediary development, the efficiency of the legal system and property rights protection (Laeven & Woodruff, 2007; Thorsten, Demirguc-Kunt, & Maksimovic, 2006). Specifically, Thorsten, Demirguc-Kunt, & Maksimovic (2006) have come to observe that small firms are the ones that are most negatively affected by an inefficient legal system. In their research, the authors explain that big firms experience less severe effects from financial, legal, and corruption obstacles (whenever compared to small firms). From this, we may conclude that an improvement in legal system quality may increase the amount of small firms, while also not having any significant effect over the amount of big firms. On the other hand, Laeven & Woodruff (2007) have come to explain the positive relationship between the quality of the legal system and firm size, by pointing out that legal systems reduce the idiosyncratic risk faced by firm owners. This makes sense, because as legal systems get better, companies have better forms of protection. Say, patent rights protect intellectual property, and non-compete clauses that restrict employees from leaving. Therefore, we can intuitively come to reason that firms that rely on abstract resources (i.e., brand names, intellectual property, innovative reputation, etc.) could become larger as the legal system improves. 

Moreover, Dougherty (2014) demonstrates that the effect of legal system (studied through contract enforcement) over firm size in Mexico, is strongest in more capital-intensive industries. Analogously, Burker & Minerva (2014) find that for Italian provinces with shorter civil trials (a measure of judicial quality) the firm size distribution is more compact. Similarly, (Giacomelli & Menon, 2012) explore the causal relationship between judicial efficiency and firm size across Italian municipalities, concluding that decreasing by half the length of civil proceedings, average firm size would increase approximately by 10%. Leading us with the task to further the discussion on legal systems and firm size with an empirical analysis, all of which this project aims to do.

\newpage

# III. Data Description
My data is structured as panel data. I recompiled information from 2010, 2012, 2015 and 2017, on firms by size (as defined by number of employees) across each state of the United States. The data comes from various sources. The Dependent variable is the percentage of firms with more than 500 employees, which will also be referred to as "big firms", and has been retrieved from the US Census Bureau (SUSB Annual Data Tables by Establishment Industry in each State (from 1988 - 2018)). Similarly, our main independent variable is the Legal System Quality Ranking, which is a ranking taken from the weighted average of various variables were the highest number indicates the best quality. The variables for this ranking were Property Crime Rate per 100,000 persons (proxy for property rights protection), taken from the FBI's Report on Crime in the U.S.; Corruption Convictions, which shows the total number of convictions, retrieved from the U.S. Department of Justice's Report to Congress on the Activities and Operations of the Public Integrity Section for 2019; 2019 Lawsuit Climate Survey were the ranking of the States was conducted for the U.S. Chamber Institute for Legal Reform by The Harris Poll; and the State expenses on Public Safety and Judicial and Legal Administration (proxy for resources dedicated to law enforcement) in Millions of dollars, as retrieved from the U.S. Census Bureau, Annual Surveys of State and Local Government Finances. The Weighted Average was estimated by year using the following formula:


$$\frac{5*(Liability Systems Ranking)+ 1*(Public Spending)+ 4*(Corruption Convictions)+ 1*(Rate per  100,000)}{11}$$
  
The weights were decided according to Laeven & Woodruff's (2007) methodology.

One of our control variables is Educational Attainment, which functions as a proxy for human capital, and expresses the percentage of the population over the age of 25 that received a  bachelor's degree or a higher level of education. This data was retrieved from the U.S. Census Bureau, American Community Survey. Similarly, another one of our control variables is Per capita personal income, which doubles as a proxy for individuals' wealth in a state -- this variable is in dollars and has been retrieved from the Bureau of Economic Analysis. Another control variable is Population Density, which portrays the total population in a state divided by its land area in squared kilometers. This variable can be used as a proxy for urbanization. The total population was retrieved from the Bureau of Economic Analysis and the land area of each state was retrieved from the U.S. Census Bureau.

Additionally, we also included an interesting variable named Market Size. This variable portrays the total employment potential in a state, which is useful since it represents a potential market for a company. Market Size is an instrumental variable taken from the employment (percentage of population 25 years or older employed) instrumented with log of GDP (Real Gross Domestic Product (GDP) (Millions of chained 2012 dollars)) and log of Total population. In the regression model of this instrument variable, both independent variables proved to be statistically significant at a significance level of 0.01. That being said, Market Size is a variable that can also be understood as the percentage of the total number of people employed over 25 years of age. The data for Employment was obtained from the U.S. Census Bureau, American Community Survey. Whereas the data for Employment and GDP was collected from the Bureau of Economic Analysis (BEA).

On the other hand, the Herfindahl-Hirschman Index (HHI) was used to control for industry concentration in a State. The Herfindahl-Hirschman Index (HHI) The Herfindahl-Hirschman Index (HHI) is a commonly accepted measure of market concentration. It is calculated by squaring the market share of each firm competing in a market and then summing the resulting numbers. It can range from close to zero to 10,000. It is interpreted as the higher the index, the more jobs are concentrated on a specific industry. For these purposes, the Total Full-Time and Part-Time Employment by NAICS Industry was used to estimate the market share of each industry in the HHI. The data was retrieved from the Bureau of Economic Analysis.

The HHI was estimated by capturing the amount of jobs each of the following industries provide across the States: Forestry, fishing, and related activities; Mining, quarrying, and oil and gas extraction; Utilities; Construction; Manufacturing; Wholesale trade; Retail trade; Transportation and warehousing; Information; Finance and insurance; Real estate and rental and leasing; Professional, scientific, and technical services; Management of companies and enterprises; Administrative and support and waste management and remediation services; Educational services; Health care and social assistance; Arts, entertainment, and recreation; Accommodation and food services; Other services (except government and government enterprises); and Government and government enterprises. These industries were selected, as they are identified by the BEA as the main job generated industries in the United States.

The formula for the EG index goes as follows: $$HHI_{imt} = \sum S_{imt}^2$$


Where:

  * i = industry
  * m = US State unique identifier
  * t = year
  * S = Share of industry i's employment in area m
  

Regarding data manipulations, the Per Capita Income and Population Density variables are logged in the regression, for ease of interpretation. 

Regarding data strengths, we acknowledge that the data covers seven years, which is sufficient to see short-term changes in a state's market structure. Similarly, we have sufficient information to control for population, employment, industry agglomeration and wealth which have proven to be more than sufficient in predicting the causality of firm growth (as stated in the literature). More over, our legal quality variables go on par with intuition and literature. However, we lack sufficient information to identify individual firms by industry, so we are not able to control for that. Literature indicates that the agglomeration of firms of a specific industry impacts firm dynamics. Therefore, industry has a significant impact over firm size, so by not controlling for it, the study may suffer from Omitted Variable Bias. Additionally, we do not have a unique identifier of particular firms, but rather our data presents information on the aggregate level. Therefore, it is not sufficient to understand the complexities of firm growth and how interconnected this may be with legal systems. 



\newpage

# IV. Data Exploration



```{r, message = FALSE, echo= TRUE, warning = FALSE}
Final_df <- read_excel("Final_df.xlsx")
  
colnames(Final_df)[8] <- "Per_capita_inc"


Final_df$F8 <- as.numeric(Final_df$F8)/as.numeric(Final_df$FTot)*100
    
df1 <- data.frame(Final_df$Legal_Quality , Final_df$Higher_Ed, Final_df$Per_capita_inc, Final_df$`Population Density`, Final_df$Market_Size, Final_df$HHI, Final_df$F8)


df1 <- df1 %>% 
   mutate_all(funs(as.numeric(.)))

#df2 <- round(df1[,-c(1,2)])
#df<- cbind(df1[,1],df1[,2], df2)

temp <- describe(df1)

temp <- data.frame(lapply(temp, function(x) round(x, 3)))

rownames(temp) <- c(paste0("Legal Quality Ranking", footnote_marker_number(1)), paste0("Higher Education", footnote_marker_number(2)), paste0("Per Capita Income", footnote_marker_number(3)), paste0("Population Density", footnote_marker_number(4)), paste0("Market Size", footnote_marker_number(5)), paste0("HHI", footnote_marker_number(6)), paste0("Firms 500+ employees", footnote_marker_number(7)))
colnames(temp) <- c("Vars","N","Mean","SD","Median","Trimmed Mean", "MAD","Min","Max","Range","Skew","Kurtosis", "SE")



 kbl(temp[, c(-1,-2,-6)], caption = "Summary Statistics", align = "l", booktabs = T, escape = F) %>%
  kable_paper(latex_options = c("striped"), full_width = F) %>%
  kable_styling(latex_options = "hold_position") %>%
  footnote(general = c("n = 200"),
           number = c("U.S. Census Bureau | Chamber Institute for Legal Reform | FBI | Dept. of Justice", "U.S. Census Bureau", "Bureau of Economic Analysis", "Bureau of Economic Analysis | U.S. Census Bureau", "U.S. Census Bureau | Bureau of Economic Analysis", "Bureau of Economic Analysis", "U.S. Census Bureau")) %>%
  row_spec(0, bold = T)%>%
  column_spec(1, bold = T) %>%
  landscape()


```


The population with higher education does not have a significant difference between its mean and the median, this implies that there are not many outliers. However, when we look at per capita income, we notice that it has a skewness is slightly above 0.5, which leads us into thinking that this data is slightly positively skewed. More on this, most data on market size seems to be highly positively skewed. Having data that is positively skewed, means that it has outliers that are above average. Additionally, we can observe that the median per capita income in the United States from 2010 - 2017 is of \$44,238.5. On the other hand, the broadest range of all the variables pertains to Per Capita Income. Yet, this could very well be due to the fact that most of our variables are in different units of observations. For this reason, it is advisable to continue our analysis to dig further into the underlying behavior of our variables of study.




```{r, echo= TRUE, message = FALSE}

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
```





In the above graphs, we can observe the multivariable fixed effects regression residuals on the Y axis (with the omission of the selected independent), and a selected independent variable on the X axis. This comparison is useful to identify the nature of the relationship between the dependent variable and each independent variable in our regression. From the above graphs, we can observe that we have a linear relationship between each independent variable and the dependent variable. Notice that for the HHI, we can see that all HHI's are below 1500, which means that all states have competitive marketplace (high variety of industries). 




```{r, echo= TRUE, message = FALSE, fig.width = 13, fig.asp = .9}
df1$Year <- Final_df$Year

library(ggcorrplot)

colnames(df1)<- c("Legal Quality Ranking", "Educational Attainment", "Per Capita Income", "Population Density", "Market Size", "Herfindahl-Hirschman Index", "Firms 500+ employees", "Year")
State <- as.numeric(as.factor(Final_df$State))
df <- cbind(df1, State)
corr <- round(cor(df1), 1)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
   lab = TRUE)+
labs(title = "Correlation Matrix", caption = "n = 200, Data from 2010-2017, U.S. Census Bureau | Bureau of Economic Analysis | Chamber Institute for Legal Reform | FBI | Department of Justice")
```



The correlation matrix indicates that Per Capita Income and Higher Education are highly positively correlated. This goes on par with intuition, as higher earning individuals pursue higher education, and viceversa. Similarly, Market Size is highly correlated with the amount of employees firms are willing to contract, as well as population. This makes sense, because population is part of the estimation of market size. 




```{r, echo= TRUE, message = FALSE, results='hide'}
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





```


```{r, echo= TRUE, message = FALSE}
# grid.arrange(legal_map_10, legal_map_17, bottom = textGrob("n = 200, Data from 2010 and 2017, Index of own creation", just ="left",gp=gpar(fontsize=7)))
```


```{r, echo= TRUE, message = FALSE}


grid.arrange(Firm_Tot_Map_10, Firm_Tot_Map_17, bottom = textGrob("n = 200, Data from 2010 and 2017, U.S. Census Bureau", just ="left",gp=gpar(fontsize=7)))
```



From the above we can note that nothing seems to have changed much from 2010-2017 with respect to the percentage of firms with 500+ employees operating in a state or the legal system's quality. Additionally, the state with the most number of firms is California. In addition, some of the states with the best legal system are New York, Ohio and California.


```{r, echo= TRUE, message = FALSE,  fig.width = 10, fig.asp = .8}
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

```



Small firms (those with less than 5 employees) are the most common type of firms, and big firms (those with 500+ employees) are the most uncommon type of firm. However, this does not undermine their effect on each state's economy.

\newpage

```{r, message = FALSE, echo= TRUE, warning = FALSE, fig.width = 20, fig.asp = .4}
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
        
                
                
                           

```


```{r, message = FALSE, echo= TRUE, warning = FALSE, fig.width = 30, fig.asp = .6}
 Lol_2017 + theme(text = element_text(size = 30))                    # All font sizes
```
Since full-time and part-time employment trends are quite similar between 2010 and 2017, we can take the graph above as a representation of the trends that have taken place in this period. The most popular industries are those related to government enterprises. These range all the way from prisons to banks. Similarly, the second employer is the health care industry and last two least popular are utilities and forestry. 
\newpage

```{r, echo= TRUE, message = FALSE, warning = FALSE, fig.width = 11, fig.asp = .5}

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

```
There is negative relationship between Employment and Big Firms across all states. Each point in this graph represents a state (i.e. California, Texas, Nevada, etc.). Where Accomodation and Administration are the industries with the highest amount of job generation for two states (California and Texas). Given this trend we are seeing, it will be interesting to see if this relationship still holds after controlling for GDP and population density.


\newpage
```{r, echo= TRUE, message = FALSE,  fig.width = 5, fig.asp = .92}
df1 <- Final_df[, c("Legal_Quality", "Higher_Ed", "Per_capita_inc", "Population Density", "Market_Size", "HHI", "F8")]


df1 <- df1 %>% 
   mutate_all(funs(as.numeric(.)))

df1$year <- Final_df$Year

ggplot(df1, aes(x = Per_capita_inc/1000, y = as.factor(year), group = as.factor(year)))+
  geom_density_ridges(fill = "firebrick")+
  labs(x= "Bins for Per Capita Income (in Thousands of Dollars)", y = "Year", title = "Distribution of Per Capita Income by Year", caption = "n = 200, Data from 2010-2017, Bureau of Economic Analysis")

```


From this graph, we can observe that the mean per capita income has increased from approximately 38,000 dollars to approximately 48,000 dollars. Similarly, the per capita income in the United States seems to be more concentrated around the mean in 2010 than how it was in 2017. More on this, the per capita income appears to be right skewed for all years, although more in 2017 than in any other years. This means that most states have a per capita income above the mean.




\newpage


# V. Hypothesis and Methodology

Literature has shown that in developing countries, there exists a significant relationship between the quality of a State's legal system and firm size. However, the relationship between legal systems and firm size is yet to be explored thoroughly in well developed countries. This project aims to shed some light into the dynamics that come into play regarding this concept. For these purposes, there are two hypotheses that aim to be tested throughout the project. The main hypothesis is that legal quality have a positive effect on the percentage of big firms situated in a state. Additionally, this study also hypothesizes that the the percentage of big firms present statistically significant relationship with the quality of the legal system. 

Our presumptions are based on the fact that bigger firms are expected to have more complex operations than their smaller counterparts. Now, the more complex a firms' operations are, the more accountability it is expected from management -- mainly to satisfy legal requirements established by outside forces such as the FDIC, SEC, banks, IRS, etc. Thus, as bigger firms are under more scrutiny, they are expected to seek risk aversion aided by the state's legal institutions. For example, this may be seen in a way in which big firms may inquire more effective contract and property rights enforcement than smaller firms, to continue their operations. Additionally, it is assumed that smaller firms may be more able to continue their operations even in the absence of legal institutions, given their operations are at a small enough scale where contracts, patents, and properties are kept to a minimum. 

To control for all the important key takeaways from the literature, this study includes several control variables. Since Saborowski & Misch (2019) found a close correlation between firm size and state-level per capita incomes, this was included as a control for wealth in a state -- since wealthier states may draw more firms. Additionally, given that Glaeser, Kallal, Scheinkman, & Schleifer (1991) and Audretsch & Feldman (1999) demonstrated that urban variety encourages employment growth, this study included control variables for urbanization though population density. This variable in particular will be logged when included in each regressions model, for ease of interpretation, such that it may allow us to observe the effect that a percentage change in population density has over firm distribution. Similarly, as Audretsch & Acs (1987) proved that firms exhibit a different behavior according to their size and industry, making a big case for how market concentration affects firm size, two control variables were introduced -- market size and the Herfindahl-Hirschman Index (HHI). Market size is nothing more than the percentage of people over 25 employed, instrumented with the log of Gross Domestic Product and the log of total population. This approach was chosen, so as to decrease any simultaneity bias between any of the other independent variables (i.e. per capita income, education, etc.) and employment rate. Similarly, the HHI was introduced to control for industry concentration. Additionally, as educational levels support the complexities of firm management, a variable that captures the percentage of the population with a bachelor degree or more was introduced to control for this asset. The HHI was estimated by capturing the share of jobs generated by the twenty main industries identified in the United States.    


To test both hypotheses, this study will apply several fixed effects regressions with data from the years 2010, 2012, 2015, and 2017. To check the effect that legal system quality has over big firms, a regression will be applied were the dependent variable is the percentage of firms and the main independent variable is the legal system quality.


* Model
	
$$ Firms_{it} = \beta_0  + \beta_1(LegalQ_{it}) + \beta_2 (Educ_{it}) + \beta_3(log(PerCapInc_{it})) + \beta_4(log(PopDens_{it})) + \beta_5(MarketSz_{it}) + \beta_6(HHI_{it})  + \epsilon	$$

Where the Y is the percentage of firms with over 500 employees.
	
	
The primary issue that we can expect from this regression model, is multicollinearity. For instance, per capita income can increase with the educational attainment of the population and population density -- and viceversa. Similarly, the population density can increase with the employment (market size in this case), because the more the employment rate increases, the more a place becomes urbanized, and so the bigger its population density may come to be. On the other hand, a high industry concentration may be indicative of entry barriers for other industries through institutional regulations and such, it may also be indicative of legal system quality. That is, companies of a specific industry (i.e. nuclear plant, manufacturing factory, etc.) may not find it viable to enter a market, because the enforcement of certain regulations (which will certainly increase their costs) may deter their entry. Yet, all of these variables are included in similar research studies were no further issues have been presented. More over, as presented in our correlation matrix, none of the independent variabels present a correlation coefficient at or above 0.85. 

In addition to potential multicollinearity issues, there are also concerns over potential omitted variable bias. Firm distribution is affected by many factors, be it institutional or managerial. Where firms may decide to establish themselves in a state that has highly efficient institutions, but may also have costly regulations. On the other hand, firms may also be affected by industry specific factors, which we cannot  account for, since our data is aggregate, and not firm specific. More on this, since our data is not firm specific, we also fail to capture managerial particularities may also affect firm size. Yet, including all variables that impact firm distribution brings many data collection challenges, beginning from confidentiality issues all the way to time constrictions. Therefore, the model has been limited to state-wide variables that prior studies have suggested to have significant impacts on firm behavior. 


\newpage


# VI. Empirical Findings


### Fixed Effects Models



```{r, message = FALSE, echo= TRUE, warning = FALSE}
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


```




\newpage

### VII. Results Interpretation


Overall, the results of our regression model indicate that all of the variables selected have a statistically significant effect over the percentage of big firms established in a state at the 0.01 significance level, with the only exception of Educational Attainment. Educational Attainment portrays a negative relationship with big firms, where it lacks a statistically significant relationship with the dependent variable. Therefore, we can say that the likelihood that the percentage of people over 25 years of age, with a bachelors or more affects the percentage of big firms is not due to random chance is very low. In regards to this, as we add more variables, the standard errors of our main independent variable (Legal Quality Ranking) maintains almost the same value. However, this is not the case for Educational Attainment, the Log of Per Capita Income, and the Log of Population Density. For the first variable, we can observe that as we add the Log of Per Capita Income and the Herfindahl-Hirschman Index (HHI), the standard errors increase. This is because when we add the Log of Per Capita Income and the HHI, the standard errors for the regression coefficient on the Educational Attainemnt increases, suggesting that these three variables are related -- also indicating a multicollinearity problem. This happens because whenever omitting the Log of Per Capita Income and HHI, Educational Attainment captures their effect. Hence, omitting these variables cause omitted variable bias in our regression, so their inclusion is necessary. Likewise, the Log of Per Capita Income decreases its standard errors whenever the HHI is included in the regression, ultimately making the variable statistically significant at the 0.01 significance level. This indicates that by including the HHI, we decrease the standard error for the regression coefficient on the Log of Per Capita Income, suggesting that the HHI and the percentage of Firms with over five hundred employees are related. In this case, multicollinearity is not a concern, and in fact, adding the HHI to our estimation reduces the standard error for the regression coefficient on the Log of Per Capita Income. So, even though excluding the HHI does not generate Omitted Variable Bias, we still include the HHI to reduce the standard errors. Similarly, with respect to the Log of Population Density, we can see a similar behavior in our regression. That is, as we add the Log of Population Density, the standard errors decrease for the Log of Per Capita Income -- making the Log of Per Capita Income statistically significant at the 0.01 significance level. By a similar analysis, this indicates that by including the HHI, we decrease the standard errors for the regression coefficient on the Log of Population Density, indicating once more that the HHI and the percentage of Firms with over five hundred employees are related -- thus, multicollinearity is not a concern but the inclusion of HHI is highly crucial for the soundness of our regression model. Concluding that, whenever controlling for Market Size and HHI, the sum of squared residuals (SSR) is decreased and so Model (6) is the best model, since it corrects for omitted variable bias and reduces squared errors. Thus, all further coefficient analyses will be based on Model (6). 

In our selected model, we can observe that for an additional unit increase on the Legal Quality Ranking, the percentage of big firms decrease by -0.027 percentage points. This means that an improvement in the quality of a state's legal system decreases the percentage of big firms it has. This may be due to the constraints in market power that extensive regulations and a high probability of detection of unlawful practices induce over big firms. Where big firms may strive with a combination of unlawful practices, extensive capital and high market power, the rule of law decreases these chances by imposing constraints on their (otherwise unlimited) unlawful practices. Therefore, our hypothesis that the quality of the legal system has a positive and statistically significant effect (at the 0.05 significance level) over big firms, is partially rejected. Yet, this negative effect could also be due to the nature of the majority of industries included in the HHI, since most of them do not rely on abstract products or services (i.e. brand names, intellectual property, innovative reputation, etc.) which ultimately are the ones that depend the most on contract enforcement. Notice that since the quality of the legal system does prove to be statistically significant at the 0.05 significance level, this part of the hypothesis is accepted.

On the other hand, Educational Attainment seems to decrease its statistical significance whenever we control for per capita income and the HHI. This may be because education influences wages, employment status and industry agglomeration, thus having a powerful impact over the per capita income and HHI of each state. Thus, by omitting per capita income, we are actually capturing the effect of that variable on Educational Attainment (see Model (2) ); the same can be seen in Model (6) when the industry agglomeration (HHI) is controlled for. Similarly, the reasoning behind the negative relationship between Educational Attainment and the percentage of big firms, may rely on the fact that the majority of industries included in the estimation of the HHI are ones that do not require higher education degrees from its workers (i.e. mining, forestry, fishing, etc.). Therefore, as the percentage of people with a higher education increases, the percentage of people looking for work on the service industry increases and so, the percentage of big firms that rely on low skill workers decreases. This may also explain why an additional percentage increase on the per capita income generates a 6.270 percentage point decrease in the percentage of big firms -- as high per capita income may reflect the earnings of workers outside low skill jobs. 

Additionally, in our selected regression model we can observe that a percentage increase in the population density generates a percentage point increase of 0.372 over the percentage of big firms. This is an intuitive result, since population density is indicative of how urbanized and economically developed a state is. Thus, states with a large population density is attractive. Likewise, as the percentage of population 25 years or older employed (see Market Size definition) increases the percentage of big firms increases by 1.190 percentage points. This follows from the positive effect of population density in our regression model, as we can infer that big firms are attracted to locations were a large supply of potential workers is available. Alternatively, an additional unit increase in the Herfindahl-Hirschman Index generates a 0.002 percentage point increase in the percentage of big firms. The HHI portrays industry agglomeration and this makes sense following Alfred Marshall's Agglomeration Theory, where it is believed that firms receive increasing returns from the mix of agglomeration economies (i.e. local skilled labor, local knowledge spillovers, etc.).



\newpage

# VII. Policy Implications

The results presented in this study may be helpful for policy makers trying to increase job creation through the private sector by identifying and further developing the most attractive features for big firms. Specifically so, after exploring the relationship between the legal system quality and the distribution of big firms, legislators could be more proactive in increasing the legal quality of their governments while also focusing on ways to promote employment and industry agglomeration. These two points are very important, because the quality of legal systems proved to be statistically significant, while also withholding negative effects over big firms. This may be an indication of big firms relying on unlawful practices to expand their market power. Thus, it is important for governments to deter unlawful practices and support alternative initiatives that attract big firms -- such as those that promote employment and industry agglomeration.


The first could be achieved by exploring alternatives for decreasing their corruption convictions, increasing their public scrutiny spending, or decreasing property crimes. Similarly, they could explore alternatives to improve all of the aspects taken into account for the Liability Systems Rnaking: Enforcing meaningful venue requirements, Overall treatment of tort and contract litigation, Treatment of class action suits and mass, consolidation suits, Damages, Proportional discovery, Scientific and technical evidence, Trial judges' impartiality, Trial judges' competence, Juries' fairness, and The quality of appellate review. Some initiatives they could take to improve these aspects of the legal systems is by increasing police presence. This may be achieved in non-complicated ways, such as partnerships between police, residents, families, parents, shop owners, building managers, and school officials. As experts indicate that the more those interventions involve partnerships, the more effective those interventions can be. Similarly, another way in which much more specific issues may be reduced, such as trial judges impartiality, is by running simple automated background checks to verify the absence of conflict of interests or questionable ties between the judges and the people participating in their cases.


On the other hand, whenever focusing on promoting employment, governments could provide training programs to the structurally unemployed, pay subsidies to firms that provide training to displaced workers, help the unemployed to relocate to areas where jobs exist, and induce prospective workers to continue or resume their education. Similarly, policymakers could consider spending public money to foster industry clusters in their states. Yet, this may come with various complications, as an analysis of this sort would need to consider the extent of gains a state would receive from industry agglomeration. More over, it should also take into account how much does the productivity of a firm increases when other firms from the same industry locate nearby. The scope of these analyses surpass the capacities of this project, but it is suggested as an exercise for the reader to ponder about them carefully.


Overall, the main policy implication from this work is that there are various aspects of the economy that affect big firm distribution. All the variables studied in this project have a statistically significant effect on the percentage of big firms, and as such, education, employment, per capita income, legal quality and industry agglomeration need to be assessed so as to increase the amount of big firms in each state.



\newpage







# VIII. Bibliography

Acemoglu, D., Johnson, S., & Robinson, J. A. (2005). Institutions as a fundamental cause of long-run growth. Handbook of economic growth, 1, 385-472. Retrieved October 10, 2021

Acemoglu, D., Johnson, S., Robinson, J., & Thaicharoen, Y. (2003). Institutional causes, macroeconomic symptoms: volatility, crises and growth. Journal of monetary economics, 50(1), 49-123. doi:https://doi.org/10.1016/S0304-3932(02)00208-8

Anne, K. M., & Viereggerb, C. (2020, February 25). Reconciling the Firm Size and Innovation Puzzle. Organization Science, 31(2), 477-488. doi:https://doi.org/10.1287/orsc.2019.1310

Audretsch, D. B., & Acs, Z. J. (1987). Innovation. Market Structure, and Firm Size. The Review of Eocnomics adn Statistics, 567-574. doi:https://doi.org/10.2307/1935950

Audretsch, D. B., & Dohse, D. (2007). Location: A Neglected Determinant. Review of World Economics , 143(1). doi:10.1007/s10290-007-0099-7

Audretsch, D. B., & Fritsch, M. (1999). The distribution of firm start-up size across geographic space. 15(3), 239-252. Retrieved October 1, 2021

Audretsch, D., & Feldman, M. P. (1999, February 15). Innovation in cities: Science-based diversity, specialization and localized competition. European Economic Review, 43(2), 409-429. doi:https://doi.org/10.1016/S0014-2921(98)00047-6

Burker, M., & Minerva, G. A. (2014, July). Civic Capital and the Size Distribution of Plants: Short-run Dynamics and Long-run Equilibrium. Journal of Economic Geography, 14(4), 797-847. doi:https://doi.org/10.1093/jeg/lbt032

Cosic, D. (2018). Wage Distribution and Firm Size: The Case of the United States. International Labour Review, 157(3), 357-377. Retrieved October 1, 2021, from https://search-ebscohost-com.ezproxy.neu.edu/login.aspx?direct=true&AuthType=ip,shib&db=ecn&AN=1740472&site=ehost-live&scope=site

Cuervo, A. (2008, March 5). The geographic space and the creation of new firms. International Entrepreneurship and Management Journal, 4, 105-107. Retrieved October 2, 2021

Dougherty, S. M. (2014, September). Legal Reform, Contract Enforcement and Firm Size in Mexico. Review of Internaitonal Economics, 22(4), 825-844. doi:10.1111/roie.12136

Eeckhout, J. (2009). Gibrat's law for (all) cities: Reply. American Economic Review, 99(4), 1676-1683. Retrieved October 1, 2021

Ellison, G., & Glaeser, E. L. (1997). Geographic concentration in US manufacturing industries: a dartboard approach. Journal of political economy, 105(5), 889-927.

Galimberti, J. K. (2020). Forecasting GDP Growth from Outer Space. Oxford Bulletin of Economics and Statistics, 82(4), 697-722. Retrieved October 3, 2021

Ge, J., Stanley, L. J., Eddleston, K., & Kellermanns, F. W. (2017). Institutional deterioration and entrepreneurial investment: The role of political connections. Journal of Business Venturing, 32(4), 405-419. Retrieved 10 21, 2021

Giacomelli, S., & Menon, C. (2012). Firm Size and Judicial Efficiency in Italy: Evidence from the Neighbor's Tribunal. SERC discussion papers , 108.

Glaeser, E. L., Kallal, H. D., Scheinkman, J. A., & Schleifer, A. (1991, July). Growth in Cities. National Bureau of Economic Research.

Hart, P. E., & Oulton, N. (1999). Gibrat, Galton and job generation. International Journal of the Economics of Business, 6(2), 149-164. Retrieved October 1, 2021

Hopenhayn, H. A. (1992). Entry, exit, and firm dynamics in long run equilibrium. Econometrica: Journal of the Econometric Society, 1127-1150. Retrieved October 3, 2021

Hou, K., & van Dijk, M. A. (2019). Resurrecting the Size Effect: Firm Size, Profitability Shocks, and Expected Stock Returns. Review of Financial Studies, 32(7), 2850-2889. doi:https://academic-oup-com.ezproxy.neu.edu/rfs/issue

Johansson, D., Heshmati, A., & Magnus Bjuggren, C. (2010, July 8). Effective Corporate Tax Rates and the Size Distribution of Firms. Journal of Industry, Competition and Trade, 10, 297-317. doi:https://doi.org/10.1007/s10842-010-0085-y

Johnson, S., McMillan, J., & Woodruff, C. (2002). Property rights and finance. American Economic Review, 92(5), 1335-1356.

Komarek, T. M., & Loveridge, S. (2014, February). Too Big? Too Small? Just Right? An Empirical Perspective on Local Firm Size Distribution and Economic Growth in U.S. Counties and High-Poverty Rural Regions. Economic Development Quarterly, 28, 28-41. doi:https://doi-org.ezproxy.neu.edu/http://edq.sagepub.com/content/by/year

Kose, M. A., Prasad, E., Rogoff, K., & Wei, S. J. (2009). Financial globalization: a reappraisal. IMF Staff papers, 56(1), 8-69. Retrieved October 21, 2021

Laeven, L., & Woodruff, C. (2007). The Quality of the Legal System, Firm Ownership, and Firm Size. The Review of Economics and Statistics, 89(4), 601-614. doi:https://doi.org/10.1162/rest.89.4.601

Lopez-Martin, B., & Perez-Reyna, D. (2021, September). Contracts, firm dynamics, and aggregate productivity. Journal of Economic Dynamics & Control, 130. doi:10.1016/j.jedc.2021.104190

Omer, T. C., Molloy, K. H., & Ziebart, D. A. (1993). An Investigation of the Firm Size-Effective Tax Rate Relation in the 1980s. Journal of Accounting, Auditing & Finance, 8(2), 167-182. doi:https://doi.org/10.1177/0148558X9300800206

Saborowski, C., & Misch, F. (2019). Firm Size, Life Cycle Dynamics and Growth Constraints: Evidence from Mexico. International Monetary Fund. Retrieved October 1, 2021, from https://search-ebscohost-com.ezproxy.neu.edu/login.aspx?direct=true&AuthType=ip,shib&db=ecn&AN=1797922&site=ehost-live&scope=site

Seim, K. (2006). An empirical model of firm entry with endogenous product-type choices. The RAND Journal of Economics, 37(3), 619-640. Retrieved October 3, 2021

Sutton, J. (1997). Gibrat's legacy. Journal of economic literature, 35(1), 40-59. Retrieved October 1, 2021

Thorsten, B., Demirguc-Kunt, A., & Maksimovic, V. (2006). The influence of financial and legal institutions on firm size. Journal of Banking & Finance, 30(11), 2995-3015. doi:https://doi.org/10.1016/j.jbankfin.2006.05.006

Vinas, F. (2021, February). How Financial Shocks Transmit to the Real Economy? Banking Business Models and Firm Size. Journal of Banking and Finance, 123. doi:10.1016/j.jbankfin.2020.106009


