#Iteration: 1 - Data Pre-processing

#install.packages("dplyr")
library(dplyr)

### Date packages
#install.packages("tidyverse")
#install.packages("lubridate")
library(tidyverse)
library(lubridate)


MOH <- read.csv("MoHSubset.csv")
head(MOH)
summary(MOH)

# Recode variables in MoHsubset
# Gender: M = 2, F = 1
MOH$GENDER <- ifelse(MOH$GENDER == "M", 2, 1)
# Age
MOH <- MOH %>% 
  mutate(AGEGRP = case_when(
    AGE_DSCH <= 4 ~ 1,
    AGE_DSCH >= 5 & AGE_DSCH <= 14 ~ 2,
    AGE_DSCH >= 15 & AGE_DSCH <= 24 ~ 3,
    AGE_DSCH >= 25 & AGE_DSCH <= 44 ~ 4,
    AGE_DSCH >= 45 & AGE_DSCH <= 64 ~ 5,
    AGE_DSCH >= 65 ~ 6,
    TRUE ~ 10
  )
  )

# Convert to date format
# Need to pre-format EVSTDATE column as a date in Excel first
# Use Custom format mm/d/yyyy option in Excel (need to type format)
# Check for NAs using summary command to ensure correct conversion
MOH$DATE <- as.Date(MOH$EVSTDATE, "%m/%d/%Y")

# Add climate zone variable - based on TLADOM value
# Northern New Zealand - Zone 1
# Climate: Kaitaia (18183), Whangarei (1287), Auckland (1962), Tauranga (1615)
# DHBs: Northland (11), Waitemata (21), Auckland (22), Counties Manukau (23), Bay of Plenty (47)

# Central North Island - Zone 2
# Climate: Hamilton (2112), Taupo (1858), Rotorua (1770)
# DHBs: Waikato (31), Lakes (42)

# South-West North Island - Zone 3
# Climate: New Plymouth (2283), Wanganui (3715), Palmerston North (3243), Wellington (25354)
# DHBs: Taranaki (71), Wanganui (82), Mid Central (81), Capital and Coast (91), Hutt (92)

# Eastern North Island - Zone 4
# Climate: Gisborne (2810), Napier (2980), Masterton (Martinborough - 21938)
# DHBs: Tairawhiti (51), Hawkes Bay (61), Wairarapa (93)

# Northern South Island - Zone 5
# Climate: Nelson (4271), Blenheim (4326)
# DHBs: Nelson Marlborough (101) 

# Western South Island - Zone 6
# Climate: Westport (7342), Hokitika (3910), Milford Sound (Pop. 120 not used)
# DHBs: West Coast (111)

# Eastern South Island - Zone 7
# Climate: Kaikoura (4506), Christchurch (4843), Timaru (5086)
# DHBs: Canterbury (121)  

# Inland South Island - Zone 8
# Climate: Lake Tekapo (Pop. 400 Not used), Manapouri (Pop. 100 Not used), Queenstown (5451), Alexandra (5578)
# DHBs: South Canterbury (123), Otago (131)

# Southern New Zealand - Zone 9
# Climate: Dunedin (7339), Invercargill (11104)
# DHBs: Southland (160) (Dunedin TLA, Invercargill TLA)


MOH <- MOH %>% 
  mutate(ZONE = case_when(
    # Zone 1
    DHBDOM == 11 ~ 1,
    between(DHBDOM, 21, 23) ~ 1,
    DHBDOM == 47 ~ 1,
    # Zone 2
    DHBDOM == 31 ~ 2,
    DHBDOM == 42 ~ 2,
    # Zone 3
    DHBDOM == 71 ~ 3,
    between(DHBDOM, 81, 82) ~ 3,
    between(DHBDOM, 91, 92) ~ 3,
    # Zone 4
    DHBDOM == 51 ~ 4,
    DHBDOM == 61 ~ 4,
    DHBDOM == 93 ~ 4,
    # Zone 5
    DHBDOM == 101 ~ 5,
    # Zone 6
    DHBDOM == 111 ~ 6,
    # Zone 7
    DHBDOM == 121 ~ 7,
    # Zone 8
    DHBDOM == 123 ~ 8,
    DHBDOM == 131 ~ 8,
    # Zone 9
    DHBDOM == 160 ~ 9,
    TRUE ~ 10
  )
  )

# Check for unidentified DHBs
MOH %>% count(ZONE == 10)

# Recode SHTSTAY
# Short stay = 1, Long stay = 0
MOH$SHTSTAY <- ifelse(MOH$Short_Stay_ED_Flag == "N", 0, 1)
# Check 297372 = TRUE
MOH %>% count(SHTSTAY == 1)

# Recode ENDTYPE

MOH <- MOH %>% 
  mutate(ENDCODE = case_when(
    END_TYPE == "DA" ~ 1,
    END_TYPE == "DC" ~ 2,
    END_TYPE == "DD" ~ 3,
    END_TYPE == "DF" ~ 4,
    END_TYPE == "DI" ~ 5,
    END_TYPE == "DL" ~ 6,
    END_TYPE == "DN" ~ 7,
    END_TYPE == "DO" ~ 8,
    END_TYPE == "DP" ~ 9,
    END_TYPE == "DR" ~ 10,
    END_TYPE == "DS" ~ 11,
    END_TYPE == "DT" ~ 12,
    END_TYPE == "DW" ~ 13,
    END_TYPE == "EA" ~ 14,
    END_TYPE == "ED" ~ 15,
    END_TYPE == "EI" ~ 16,
    END_TYPE == "ER" ~ 17,
    END_TYPE == "ES" ~ 18,
    END_TYPE == "ET" ~ 19,
    TRUE ~ 20
  )
  )

# Check - should return all false
MOH %>% count(ENDCODE == 20)


# Recode diag01 - use summary command to ensure no 999 values
# J001 - J006 (acute upper respiratory infections) -> 1
# J10 - J18 (Influenza and pneumonia) -> 2
# J20 - J22 (Other acute lower respiratory infections) -> 3
MOH <- MOH %>% 
  mutate(DIAG = case_when(
    diag01 == "J00" ~ 1,
    diag01 == "J010" ~ 1,
    diag01 == "J011" ~ 1,
    diag01 == "J012" ~ 1,
    diag01 == "J013" ~ 1,
    diag01 == "J014" ~ 1,
    diag01 == "J018" ~ 1,
    diag01 == "J019" ~ 1,
    diag01 == "J020" ~ 1,
    diag01 == "J028" ~ 1,
    diag01 == "J029" ~ 1,
    diag01 == "J030" ~ 1,
    diag01 == "J038" ~ 1,
    diag01 == "J039" ~ 1,
    diag01 == "J040" ~ 1,
    diag01 == "J041" ~ 1,
    diag01 == "J042" ~ 1,
    diag01 == "J050" ~ 1,
    diag01 == "J051" ~ 1,
    diag01 == "J060" ~ 1,
    diag01 == "J068" ~ 1,
    diag01 == "J069" ~ 1,
    diag01 == "J100" ~ 2,
    diag01 == "J101" ~ 2,
    diag01 == "J108" ~ 2,
    diag01 == "J110" ~ 2,
    diag01 == "J111" ~ 2,
    diag01 == "J118" ~ 2,
    diag01 == "J120" ~ 2,
    diag01 == "J121" ~ 2,
    diag01 == "J122" ~ 2,
    diag01 == "J128" ~ 2,
    diag01 == "J129" ~ 2,
    diag01 == "J13" ~ 2,
    diag01 == "J14" ~ 2,
    diag01 == "J150" ~ 2,
    diag01 == "J151" ~ 2,
    diag01 == "J152" ~ 2,
    diag01 == "J153" ~ 2,
    diag01 == "J154" ~ 2,
    diag01 == "J155" ~ 2,
    diag01 == "J156" ~ 2,
    diag01 == "J157" ~ 2,
    diag01 == "J158" ~ 2,
    diag01 == "J159" ~ 2,
    diag01 == "J160" ~ 2,
    diag01 == "J168" ~ 2,
    diag01 == "J170" ~ 2,
    diag01 == "J171" ~ 2,
    diag01 == "J172" ~ 2,
    diag01 == "J173" ~ 2,
    diag01 == "J180" ~ 2,
    diag01 == "J181" ~ 2,
    diag01 == "J182" ~ 2,
    diag01 == "J188" ~ 2,
    diag01 == "J189" ~ 2,
    diag01 == "J200" ~ 3,
    diag01 == "J201" ~ 3,
    diag01 == "J202" ~ 3,
    diag01 == "J204" ~ 3,
    diag01 == "J205" ~ 3,
    diag01 == "J206" ~ 3,
    diag01 == "J208" ~ 3,
    diag01 == "J209" ~ 3,
    diag01 == "J210" ~ 3,
    diag01 == "J218" ~ 3,
    diag01 == "J219" ~ 3,
    diag01 == "J22" ~ 3,
    TRUE ~ 999
  )
  )
MOH %>% count(DIAG == 1) # 81739
MOH %>% count(DIAG == 2) # 150152
MOH %>% count(DIAG == 3) # 120577
MOH %>% count(DIAG == 999)

# Ethnicity
# 3 major groupings; Maori, Pacific and Other 
# Ethnicity codes > 53 not used for this analsysis
# These are records where the ethnicity details are unknown 
MOH <- MOH %>% 
  mutate(ETHNICITY = case_when(
    ETHNICGP == 21 ~ 1,
    between(ETHNICGP, 30, 37) ~ 3,
    between(ETHNICGP, 10, 12) ~ 2,
    between(ETHNICGP, 40, 53) ~ 2,
    TRUE ~ 6
  )
  )

# Add CASEYEAR variable to enable grouping by year
MOH <- MOH %>% 
  mutate(CASEYEAR = year(DATE))


# Standardisation

# Need to use standardised rates NOT raw case numbers
# Libraries used for date calculations
#install.packages("tidyverse")
#install.packages("lubridate")
library(tidyverse)
library(lubridate)

# Count rows within these groups - helps to identify groups with no cases
#Also provides the total number of cases by zone, ethnicity, agegrp, and year

cases.grouped <- MOH %>% count(CASEYEAR, ZONE, ETHNICITY, AGEGRP)

# Use only ethnicity 1 through to 3
cases.filtered <- cases.grouped %>% filter(ETHNICITY <=3)

# Convert to data frame for easier handling and rename count column ('n') to cases                
hospCases.df <- as.data.frame(cases.filtered)
names(hospCases.df)[names(hospCases.df) == "n"] <- "Cases"

# Import population details for each DHB. This data will allow us to 
# calculate a standardized hospital rates for Influenza cases

pop <- read.csv("DHBpopV4.csv")

# Recode data
# Add age groups - last two age groups (65-79 & 80+) in DHBpopv4 are merged to match the hospital data

pop.agegrp <- pop %>% 
  mutate(AGEGRP = case_when(
    agegp == "00-04" ~ 1,
    agegp == "05-14" ~ 2,
    agegp == "15-24" ~ 3,
    agegp == "25-44" ~ 4,
    agegp == "45-64" ~ 5,
    agegp == "65-79" ~ 6,
    agegp == "80+" ~ 6,
    TRUE ~ 999
  )
  )

# Add ethnicity

pop.ethnic <- pop.agegrp %>% 
  mutate(ethnicity = case_when(
    ethnic == "Maori" ~ 1,
    ethnic == "Other" ~ 2,
    ethnic == "Pacific" ~ 3,
    TRUE ~ 999
  )
  )


# Add zone value to each row

pop.recoded <- pop.ethnic %>% 
  mutate(ZONE = case_when(
    # Zone 1 - Northland, Waitemata, Auckland, Counties Manukau, Bay of Plenty
    area == "Northland" ~ 1,
    area == "Waitemata" ~ 1,
    area == "Auckland" ~ 1,
    area == "Counties Manukau" ~ 1,
    area == "Bay of Plenty" ~ 1,
    # Zone 2 - Waikato, Lakes
    area == "Waikato" ~ 2,
    area == "Lakes" ~ 2,
    # Zone 3 - Taranaki, Whanganui, Mid Central, Capital and Coast, Hutt
    area == "Taranaki" ~ 3,
    area == "Whanganui" ~ 3,
    area == "MidCentral" ~ 3,
    area == "Capital and Coast" ~ 3,
    area == "Hutt" ~ 3,
    # Zone 4 - Tairawhiti, Hawkes Bay, Wairarapa
    area == "Tairawhiti" ~ 4,
    area == "Hawkes Bay" ~ 4,
    area == "Wairarapa" ~ 4,
    # Zone 5 - Nelson Marlborough
    area == "Nelson Marlborough" ~ 5,
    # Zone 6 - West Coast
    area == "West Coast" ~ 6,
    # Zone 7 - Canterbury
    area == "Canterbury" ~ 7,
    # Zone 8 - South Canterbury, Otago
    area == "South Canterbury" ~ 8,
    area == "Otago" ~ 8,
    # Zone 9 - 
    area == "Southern" ~ 9,
    TRUE ~ 10
  )
  )

# Don't include rows that have no flu cases in hospCases.df
# Select all rows not in ZONE 6 with ethnicity and agegrp = 3
# Select all rows not in ZONE 8 with ethnicity 3 and agegrp = 3 or 6
# The rows not included do not have any flu cases to perform analysis

pop.filtered <- pop.recoded %>% filter(!(ZONE == 6 & ethnicity == 3 & AGEGRP == 3))
pop.filtered <- pop.filtered %>% filter(!(ZONE == 8 & ethnicity == 3 & AGEGRP == 3))
pop.filtered <- pop.filtered %>% filter(!(ZONE == 8 & ethnicity == 3 & AGEGRP == 6))

# Group data for cases and population to perform a one way anova test
# Pool together data across whole 10 year period

pop.grouped <- pop.filtered %>%
  group_by(ZONE, ethnicity, AGEGRP) %>%
  summarize(StudyPopTotal = sum(Pop))

# Define standard population - 2013, all DHBs, 3 ethnicities and age groups
pop.standard <- pop.filtered %>%
  filter(year == '2013') %>%
  group_by(ZONE, ethnicity, AGEGRP) %>%
  summarize(StandPopTotal = sum(Pop))

# Calculate sum of flu cases for selected grouping
cases.grouped <- hospCases.df %>%
  group_by(ZONE, ETHNICITY, AGEGRP) %>%
  summarize(CaseTotal = sum(Cases))

# Add study population totals to hospital data
hosp.collated <- cases.grouped
hosp.collated$StudyPop <- pop.grouped$StudyPopTotal

# Add standard population totals
hosp.collated$StandPop <- pop.standard$StandPopTotal

# Calculate crude flu rate and std rate for each observation in zones
hosp.collated$crude <- hosp.collated$CaseTotal/hosp.collated$StudyPop*100000

hosp.collated$std <- hosp.collated$CaseTotal/hosp.collated$StudyPop*hosp.collated$StandPop

# Total by Zone and calculate standardized rate
stand.rates <- hosp.collated %>%
  group_by(ZONE) %>%
  summarize(ZoneSTD = sum(std)/sum(StandPop)*100000)

#Iteration: 2 - Statistical analysis

#ANOVA Analysis 

library(tidyverse)
library(ggpubr)
library(rstatix)

#Compute the mean and the SD (standard deviation)

hosp.collated %>%
  group_by(ZONE, ETHNICITY)  %>% 
  summarize(ZoneSTD = sum(std)/sum(StandPop)*100000) %>%
  get_summary_stats(ZoneSTD, type = "mean_sd")

# Find Outliers

stand.rates1 <- hosp.collated %>%
  group_by(ZONE, ETHNICITY) %>%
  summarize(ZoneSTD = sum(std)/sum(StandPop)*100000) 


stand.rates1 %>% 
  group_by(ZONE, ETHNICITY) %>% 
  identify_outliers(ZoneSTD)

#Visualization

ggboxplot(stand.rates1, x = "ZONE", y = "ZoneSTD")

# Build the linear model

modelX = lm(ZoneSTD ~ ZONE, data = stand.rates1)

modelY  <- lm(ZoneSTD ~ ZONE, data = stand.rates)


# Create a QQ plot of residuals

ggqqplot(residuals(modelX)) 
# All the points fall approximately at the reference line 
#which indicates the normality of the data

ggqqplot(residuals(modelY)) 
#Majority of the data points are at reference line
#which indicates the normality of the data

#Shapiro Test

shapiro_test(residuals(modelY))

# A tibble: 1 x 3
# variable          statistic p.value
# <chr>                 <dbl>   <dbl>
# 1 residuals(modelY)   0.949   0.678

# The p-value 0.678 is greater than 0.05. 
#Hence Shapiro test proves the normality of the data.

#Perform ANOVA test
aov <- stand.rates %>% anova_test(ZoneSTD ~ ZONE)
aov
#ANOVA Table (type II tests)
#Effect DFn DFd    F     p    p<.05  ges
#ZONE   1   7     7.57 0.028     *   0.52
#From the above Anova table, it is clear that there are significant differences
#between the groups with p-value = 0.028 and ges = 0.52 (52% of change between number of cases by zone)


#Clustering

hospWeather.df <-  read.csv("hospWeather.csv")

# Use Boruta technique to assess attribute importance and select the variables 
#that contributes more to build models compared to others

#install.packages("Boruta")

library(Boruta)

# Scale variables - ignore YEAR, WEEK and Cases columns
hosp.weather.scaled <- as.data.frame(scale(hospWeather.df[, -c(2:4)], scale = TRUE))

Boruta.CAT <- Boruta(Category~., data = hosp.weather.scaled, doTrace = 2)

# attStats lists attributes in descending order of importance (higher the value to more important the variable)

attStats(Boruta.CAT)

# Visualise the Boruta results

plot(Boruta.CAT, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(Boruta.CAT$ImpHistory),function(i)
  Boruta.CAT$ImpHistory[is.finite(Boruta.CAT$ImpHistory[,i]),i])
names(lz) <- colnames(Boruta.CAT$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(Boruta.CAT$ImpHistory), cex.axis = 0.7)

#the tentative atrributes picked - tmax_max + ZONE + rad_max + ah_max + rh_max + rain_mean

imp.df <- hospWeather.df[, c('tmax_max', 'ZONE','rad_max', 'ah_max', 'rh_max', 'rain_mean', 'Category')]

# Perform correlation analysis

library(gplots)

#Scale the data

imp.df.scaled <- scale(imp.df, center = TRUE, scale = TRUE)

heatmap.2(cor(imp.df.scaled), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(imp.df.scaled),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# Analysis shows correlations between rad, tmax, ah, rh variables and Category

#Clustering Techniques

install.packages(c("cluster", "factoextra"))

#KMeans
library(cluster)
library(factoextra)
set.seed(123)
df.scaled <- scale(imp.df)
km.res <- kmeans(df.scaled[, -c(7)], 3, nstart = 25)
print(km.res)

# K-means clustering with 3 clusters of sizes 1582, 1274, 1384
# 
# Cluster means:
#   tmax_max       ZONE    rad_max     ah_max     rh_max   rain_mean
# 1 -0.8630754  0.4438080 -0.9504619 -0.8559084  0.3570747 -0.05799816
# 2  0.8825426  0.4760129  0.8377745  0.1136978 -0.8299093 -0.41910934
# 3  0.1741518 -0.9454803  0.3152501  0.8736966  0.3557892  0.45209421

#1 - Low, 2 - Normal and 3 - High

# Within cluster sum of squares by cluster:
#   [1] 4206.455 5035.929 5682.344
# (between_SS / total_SS =  41.3 %)

fviz_cluster(km.res, imp.df, ellipse.type = "convex", ggtheme = theme_bw())

#PAM clustering
library(cluster)
library(factoextra)
pam.res <- pam(df.scaled[, -c(7)], 3)
print(pam.res)

# Medoids:
#   ID   tmax_max        ZONE    rad_max     ah_max     rh_max
# [1,] 1905  0.9032366 -0.09490461  1.0361644  0.9706662 -0.3779538
# [2,] 1568 -0.3420063 -0.47452303 -0.2504336  0.2357345  0.1854961
# [3,] 2716 -0.8673432  0.66433224 -1.2323110 -1.1606356  0.4036057
# rain_mean
# [1,] -0.50618748
# [2,]  0.04221485
# [3,] -0.36567644

# Objective function:
#   build     swap 
# 1.800260 1.776735 

fviz_cluster(pam.res, geom = "point", 
             main =  "PAM Clusters",
             ellipse.type = "convex",
             ggtheme = theme_bw())

#Remove Normal =2 cases from the data imp.df

imp.df1 <- subset(imp.df, Category!=2)

#KMeans clustering with new data with out normal cases

set.seed(123)
df.scaled1 <- scale(imp.df1)
km.res1 <- kmeans(df.scaled1[, -c(7)], 2, nstart = 25)
print(km.res1)

# K-means clustering with 2 clusters of sizes 1122, 951
# 
# Cluster means:
#   tmax_max       ZONE    rad_max     ah_max     rh_max   rain_mean
# 1 -0.7011780  0.1477624 -0.7372458 -0.5922753  0.3332528  0.08232842
# 2  0.8272574 -0.1743316  0.8698105  0.6987728 -0.3931753 -0.09713196
#K-means clustering with 2 clusters of sizes 1122, 951

#1 - Low cases and 3 - High cases

# Within cluster sum of squares by cluster:
#   [1] 3636.120 5064.547
# (between_SS / total_SS =  30.0 %)

#Visualization of clusters

fviz_cluster(km.res1, imp.df1, geom = "point",
             main = "KM cluster",
             ellipse.type = "convex",
             ggtheme = theme_gray())


#PAM clustering with new data without using normal level cases

library(cluster)
library(factoextra)
pam.res1 <- pam(df.scaled1[, -c(7)], 2)
print(pam.res1)

# Medoids:
#   ID  tmax_max        ZONE    rad_max     ah_max       rh_max
# 1947 950  0.687712 -0.09929047  0.8041866  0.5720145 -0.003534997
# 1881 916 -0.769566 -0.09929047 -0.7954281 -0.5925899  0.273918950
# rain_mean
# 1947 0.15625073
# 1881 0.09214033

# Objective function:
#   build     swap 
# 1.956989 1.899875 

fviz_cluster(pam.res1, geom = "point",
             # palette.colors(n= Null, palette = "0kabe_Ito", alpha, recycle = FALSE),
             main = "PAM cluster",
             ellipse.type = "convex",
             ggtheme = theme_grey())

#silhouette Analysis for K-Means

# Visualize silhouette information

library("cluster")
library("factoextra")
sil <- silhouette(km.res1$cluster, dist(df.scaled1[, -c(7)]))
fviz_silhouette(sil)

#Average Silhouette width is 0.29
# cluster size ave.sil.width
# 1       1 1122          0.36
# 2       2  951          0.20

# Identify observation with negative silhouette
neg_sil_index <- which(sil[, "sil_width"] < 0)
sil[neg_sil_index, , drop = FALSE]

#Silhouette Analysis for PAM clustering

require(cluster)
sil.pam.res <- pam(df.scaled1[, -c(7)], 2)

# Visualize silhouette information
fviz_silhouette(sil.pam.res)

#Average Silhouette width is 0.28
# cluster size ave.sil.width
# 1       1 1000          0.19
# 2       2 1073          0.37

#Add the cluster values to imp.df1 as a new column
imp.df1$kclust <- as.numeric(km.res1$cluster)

with(imp.df1, table(kclust, Category))

#          Category
#  kclust   1   3
#       1 187 935
#       2 765 186

#Confusion Matrix
#(935+765)/(187+765+935+186) = 82%, 
#This suggests a good accuracy of the clustering results.

#Iteration: 3 - Building Predictive models

#Five different algorithms have been applied on the final data set
# Those are Logistic Regression, KNN, CART, Random Forest and Naive bayes.

#1. Logistic Regression Model

library(ISLR)
library(tidyverse)
library(lubridate)

#Convert the Category variabe into binary

Model_Data <- imp.df1 %>% 
  mutate(FluRate = case_when(
    Category == 1 ~ 0,
    Category == 3 ~ 1,
  )
  )

#Scale the data to normalize the variables

scale(Model_Data, center = TRUE, scale = TRUE)

#Convert FluRate into factor variable.

Model_Data$FluRate <- factor(Model_Data$FluRate)

#Split data into train (60%) and test (40%)

set.seed(1234)
train.index <- sample(c(1:1244), 829, replace = FALSE)  
test.df <- Model_Data[train.index, -c(7) ]
train.df <- Model_Data[-train.index, -c(7) ]

#Build the logistic regression model

log_reg <- glm(FluRate ~ tmax_max + ZONE + rad_max + ah_max + rh_max + rain_mean, data = train.df, family = 'binomial')
summary(log_reg)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.1728  -0.5130   0.2028   0.5698   3.1492  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.15330    1.65811   0.696  0.48671    
# tmax_max    -0.11842    0.02643  -4.481 7.43e-06 ***
# ZONE        -0.36057    0.04468  -8.070 7.05e-16 ***
# rad_max      0.02886    0.01523   1.896  0.05799 .  
# ah_max      -0.80129    0.06852 -11.695  < 2e-16 ***
# rh_max       0.11851    0.01773   6.682 2.35e-11 ***
# rain_mean    0.07758    0.02473   3.137  0.00171 ** 
#   ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1718.32  on 1243  degrees of freedom
# Residual deviance:  946.45  on 1237  degrees of freedom
# AIC: 960.45
# 
# Number of Fisher Scoring iterations: 5

#Predict the model on test data
library(forecast)
library(caret)

p1 <- predict(log_reg, test.df, type='response')
str(p1)

# Convert p1 into factor variable in order to build confusion matrix
pred_glm <- ifelse(p1>0.5, 1, 0)
pred_glm <- as.factor(pred_glm)


#Confusion Matrix

confusionMatrix(pred_glm, test.df$FluRate)

# Confusion Matrix and Statistics
# 
#             Reference
# Prediction   0   1
#           0 326  43
#           1  48 412
# 
# Accuracy : 0.8902          
# 95% CI : (0.8669, 0.9107)
# No Information Rate : 0.5489          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.7781          
# 
# Mcnemar's Test P-Value : 0.675           
#                                           
#             Sensitivity : 0.8717          
#             Specificity : 0.9055          
#          Pos Pred Value : 0.8835          
#          Neg Pred Value : 0.8957          
#              Prevalence : 0.4511          
#          Detection Rate : 0.3932          
#    Detection Prevalence : 0.4451          
#       Balanced Accuracy : 0.8886          
#                                           
#        'Positive' Class : 0        


#Goodness of Fit Test
with(log_reg, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = F))

#the p-value is 1.844561e-163 which way lesser then 0.05. 
#So the model log_reg is statistically significant.



#2. KNN Model


#install library
library(FNN)
library(caret)
library(pROC)
library(mlbench)

#Prepare the data set to be used for the model,
# and remove the Normal cases (=2) from category variable.

imp.df <- hospWeather.df[, c('tmax_max', 'ZONE','rad_max', 'ah_max', 'rh_max', 'rain_mean', 'Category')]
imp.df1.knn <- subset(imp.df, Category!=2)

str(imp.df1.knn)

#Convert the values in Category variable (1= Low and 3= high)
#and then convert it to factor variable.

imp.df1.knn$Category[imp.df1.knn$Category == 1]<- 'Low'
imp.df1.knn$Category[imp.df1.knn$Category == 3]<- 'high'
imp.df1.knn$Category <- factor(imp.df1.knn$Category)

#Data Partition into training = 60% and testing = 40%

set.seed(1234)
ind <- sample(2, nrow(imp.df1.knn), replace = T, prob = c(0.6, 0.4))
training <- imp.df1.knn[ind == 1,]
testing <- imp.df1.knn[ind == 2,]

#Build KNN Model
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)
set.seed(222)
fit <- train(Category ~.,
             data = training,
             method = 'knn',
             tuneLength = 20,
             trControl = trControl,
             preProc = c("center", "scale"))

fit

# k-Nearest Neighbors 
# 
# 1265 samples
# 6 predictor
# 2 classes: 'high', 'Low' 
# 
# Pre-processing: centered (6), scaled (6) 
# Resampling: Cross-Validated (10 fold, repeated 3 times) 
# Summary of sample sizes: 1139, 1139, 1139, 1138, 1138, 1138, ... 
# Resampling results across tuning parameters:
#   
#   k   Accuracy   Kappa    
# 5  0.8619334  0.7204020
# 7  0.8664142  0.7297140
# 9  0.8656248  0.7281800
# 11  0.8677245  0.7324062
# 13  0.8637770  0.7245695
# 15  0.8648331  0.7266410
# 17  0.8664141  0.7299868
# 19  0.8690534  0.7351926
# 21  0.8672057  0.7314517
# 23  0.8658850  0.7286836
# 25  0.8661454  0.7292295
# 27  0.8653558  0.7276252
# 29  0.8656246  0.7281381
# 31  0.8645664  0.7259226
# 33  0.8614022  0.7192990
# 35  0.8621916  0.7208694
# 37  0.8632478  0.7230992
# 39  0.8624562  0.7214629
# 41  0.8648226  0.7263930
# 43  0.8632416  0.7231234
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 19.

plot(fit)


#ROC curve variable importance

varImp(fit)

#Importance
#tmax_max      100.00
#ah_max         83.34
#rad_max        76.84
#rh_max         41.28
#rain_mean      12.60
#ZONE            0.00


#Apply the model on the testing data

pred <- predict(fit, newdata = testing)
confusionMatrix(pred, testing$Category)

# Confusion Matrix and Statistics
# 
#            Reference
# Prediction high Low
#       high  398  68
#       Low    37 305
# 
# Accuracy : 0.87            
# 95% CI : (0.8449, 0.8925)
# No Information Rate : 0.5384          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.737           
# 
# Mcnemar's Test P-Value : 0.003415        
#                                           
#             Sensitivity : 0.9149          
#             Specificity : 0.8177          
#          Pos Pred Value : 0.8541          
#          Neg Pred Value : 0.8918          
#              Prevalence : 0.5384          
#          Detection Rate : 0.4926          
#    Detection Prevalence : 0.5767          
#       Balanced Accuracy : 0.8663          
#                                           
#        'Positive' Class : high       


#3. CART (Classification And Regression Tree) algorithm

#Prepare data set for CART model

imp.df <- hospWeather.df[, c('tmax_max', 'ZONE','rad_max', 'ah_max', 'rh_max', 'rain_mean', 'Category')]
imp.df1.cart <- subset(imp.df, Category!=2)
str(imp.df1.cart)

#Scale the data 

scale(imp.df1.cart, center = TRUE, scale = TRUE)

#Change the category variable to factor variable

imp.df1.cart$CategoryF <- factor(imp.df1.cart$Category)
str(imp.df1.cart)

#partition data into training and validation (train:60 - Valid:40)
set.seed(1234)
pd <- sample(2, nrow(imp.df1.cart), replace = TRUE, prob = c(0.6, 0.4))
traincart <- imp.df1.cart[pd==1,]
validcart <- imp.df1.cart[pd==2,]


#decision tree with party package

library(party)
tree <- ctree(CategoryF~tmax_max+ZONE+rad_max+ah_max+rh_max+rain_mean, 
              data = traincart, controls = ctree_control(mincriterion = 0.99, minsplit = 500))
tree

# Conditional inference tree with 7 terminal nodes
# 
# Response:  CategoryF 
# Inputs:  tmax_max, ZONE, rad_max, ah_max, rh_max, rain_mean 
# Number of observations:  1265 
# 
# 1) tmax_max <= 22; criterion = 1, statistic = 541.581
# 2) ah_max <= 12.9; criterion = 1, statistic = 38.54
# 3) rh_max <= 82; criterion = 1, statistic = 46.902
# 4)*  weights = 10 
# 3) rh_max > 82
# 5) ah_max <= 7.5; criterion = 0.998, statistic = 12.616
# 6)*  weights = 134 
# 5) ah_max > 7.5
# 7) ZONE <= 5; criterion = 1, statistic = 25.865
# 8)*  weights = 281 
# 7) ZONE > 5
# 9)*  weights = 246 
# 2) ah_max > 12.9
# 10)*  weights = 48 
# 1) tmax_max > 22
# 11) ah_max <= 9.9; criterion = 1, statistic = 92.01
# 12)*  weights = 94 
# 11) ah_max > 9.9
# 13)*  weights = 452 

plot(tree)

#predict the model on the test data

predict_tree <- predict(tree, newdata = validcart)

#Confusion Matrix

confusionMatrix(predict_tree, validcart$CategoryF)

# Confusion Matrix and Statistics

#             Reference
# Prediction   1   3
#           1 311  73
#           3  62 362
# 
# Accuracy : 0.8329         
# 95% CI : (0.8054, 0.858)
# No Information Rate : 0.5384         
# P-Value [Acc > NIR] : <2e-16         
# 
# Kappa : 0.6646         
# 
# Mcnemar's Test P-Value : 0.3894         
#                                          
#             Sensitivity : 0.8338         
#             Specificity : 0.8322         
#          Pos Pred Value : 0.8099         
#          Neg Pred Value : 0.8538         
#              Prevalence : 0.4616         
#          Detection Rate : 0.3849         
#    Detection Prevalence : 0.4752         
#       Balanced Accuracy : 0.8330         
#                                          
#        'Positive' Class : 1              
                                         

#4. Random Forest Model

#Prepare data set for the model

imp.df <- hospWeather.df[, c('tmax_max', 'ZONE','rad_max', 'ah_max', 'rh_max', 'rain_mean', 'Category')]
imp.df1.rf <- subset(imp.df, Category!=2)
str(imp.df1.rf)

#Scale the data 

scale(imp.df1.rf, center = TRUE, scale = TRUE)


#Convert Category variable into factor variable

imp.df1.rf$Category <- as.factor(imp.df1.rf$Category) 
str(imp.df1.rf)
table(imp.df1.rf$Category)

# 1    3 
#952 1121 

#data partition into 60-40 split

set.seed(123)
ind <- sample(2, nrow(imp.df1.rf), replace = TRUE, prob = c(0.6, 0.4))
trainrandom <- imp.df1.rf[ind==1,]
testrandom <- imp.df1.rf[ind==2,]

#Build the random forests model

library(randomForest)
set.seed(222)
rf <- randomForest(Category~., data = trainrandom,
                   ntree = 450,
                   mtry = 2,
                   importance = TRUE,
                   proximity = TRUE,
)

print(rf)

# Type of random forest: classification
# Number of trees: 450
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 11.8%
# Confusion matrix:
#   1    3    class.error
# 1 471  86  0.15439856
# 3  63 643  0.08923513

attributes(rf)

#Prediction and confusion matrix - train data
library(caret)
p1 <- predict(rf, trainrandom)
head(p1)


head(trainrandom$Category)
confusionMatrix(p1, trainrandom$Category)

#Accuracy is 100% as the model already has seen the test data
#The model also predicted and classified all the records correctly

#Prediction and confusion matrix - test data
predict_rf <- predict(rf, testrandom)
head(p2)
head(testrandom$Category)
confusionMatrix(predict_rf, testrandom$Category)

# Confusion Matrix and Statistics
# 
#             Reference
# Prediction   1   3
#           1 333  47
#           3  62 368
# 
# Accuracy : 0.8654        
# 95% CI : (0.84, 0.8882)
# No Information Rate : 0.5123        
# P-Value [Acc > NIR] : <2e-16        
# 
# Kappa : 0.7305        
# 
# Mcnemar's Test P-Value : 0.1799        
#                                         
#             Sensitivity : 0.8430        
#             Specificity : 0.8867        
#          Pos Pred Value : 0.8763        
#          Neg Pred Value : 0.8558        
#              Prevalence : 0.4877        
#          Detection Rate : 0.4111        
#    Detection Prevalence : 0.4691        
#       Balanced Accuracy : 0.8649        
#                                         
#        'Positive' Class : 1             
                                        

#Error rate of random forest
plot(rf)

#The below code in comments is useful if we need to fine tune the model for any reasons.
# Tune mtry
# t <- tuneRF(trainrandom[, -7], trainrandom[,7],
#             stepFactor = 0.5,
#             plot = TRUE,
#             ntreeTry = 100,
#             trace = TRUE,
#             improve = 0.05)

#With the tuned values the model did not improve the accuracy on test data.

#No. of nodes for the trees
hist(treesize(rf),
     main = "No. of rows for the Trees",
     col = "green")

#variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 4,
           main = "Top 4 variable importance")
importance(rf)

# 1        3 MeanDecreaseAccuracy MeanDecreaseGini
# tmax_max  45.650799 52.70335             67.03800        224.41193
# ZONE      17.797351 31.51845             38.14676         37.85366
# rad_max   11.229667 23.39180             27.17204        104.99514
# ah_max    46.339293 46.09225             62.05836        144.13088
# rh_max    10.744726 24.97613             28.08973         57.66704
# rain_mean  7.649185 16.21476             17.84215         52.70118

#MeanDecreaseAccuracy = This graph tests how worse the model performs without each variable.
#tmax_max and ah_max imp variables however rain-mean contributed negligence importance
#MeanDecresaseGini = This graph measures how pure the nodes are at the end of the tree without each variable
#tmax_max and ah_max stand among all variables

#partial Dependence plot
partialPlot(rf, trainrandom, tmax_max, "1")
partialPlot(rf, trainrandom, tmax_max, "3")
partialPlot(rf, trainrandom, ah_max, "1")
partialPlot(rf, trainrandom, ah_max, "3")

#Extract single tree
getTree(rf, 5, labelVar = TRUE)

# Multi-dimensional scaling plot of proximity matrix
MDSplot(rf, trainrandom$Category)


#5. Naive Bayes Model

library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

#Pre pare the data to build model

imp.df <- hospWeather.df[, c('tmax_max', 'ZONE','rad_max', 'ah_max', 'rh_max', 'rain_mean', 'Category')]
imp.df1.naive <- subset(imp.df, Category!=2)

str(imp.df1.naive) 

#Convert the values of Category variable into binary 0, 1

imp.df1.naive$Category[imp.df1.naive$Category == 1]<- 0
imp.df1.naive$Category[imp.df1.naive$Category == 3]<- 1
xtabs(~Category+ZONE, data = imp.df1.naive)

#Scale the data to normalize the variables

scale(imp.df1.naive, center = TRUE, scale = TRUE)

#Change the category variable into factor variable

imp.df1.naive$Category <- as.factor(imp.df1.naive$Category)
str(imp.df1.naive)

#Visualization
pairs.default(imp.df1.naive[-7])

#Box plots for all variables vs Category

imp.df1.naive %>% ggplot(aes(x = Category, y = tmax_max, fill = Category)) + 
  geom_boxplot() + 
  ggtitle("Maximum Temperature VS Influenza cases")

# the plot shows that where the tmax_max values are higher, the cases are tend to be low
# and where the tmax_max values are lower the cases are high.
#This tells us we could build a classification model using this data.

imp.df1.naive %>% ggplot(aes(x = Category, y = ZONE, fill = Category)) + 
  geom_boxplot() + 
  ggtitle("Zone VS Influenza cases")

imp.df1.naive %>% ggplot(aes(x = Category, y = rad_max, fill = Category)) + 
  geom_boxplot() + 
  ggtitle("Maximum Radiation VS Influenza cases")

imp.df1.naive %>% ggplot(aes(x = Category, y = ah_max, fill = Category)) + 
  geom_boxplot() + 
  ggtitle("Absolute Humidity VS Influenza cases")

imp.df1.naive %>% ggplot(aes(x = Category, y = rh_max, fill = Category)) + 
  geom_boxplot() + 
  ggtitle("Relative Humidity VS Influenza cases")

imp.df1.naive %>% ggplot(aes(x = Category, y = rain_mean, fill = Category)) + 
  geom_boxplot() + 
  ggtitle("Rain VS Influenza cases")

#All the above plots are indicating that the different values of different variables 
#are contributing to differentiate the number of cases into 'low - 0' and 'high - 1' category.
#This means we could potentially classify the number of cases based on these weather variables. 

#Density Plot of ZONE
imp.df1.naive %>% ggplot(aes(x = ZONE, fill = Category) ) + 
  geom_density(alpha = 0.8, color = 'black') +
  ggtitle("Density Plot of ZONE")

# We could also notice Zones 1-4 have more lower cases than the other zones
#But the significant amount of data is overlapping between zones.

#Density Plot of tmax_max
imp.df1.naive %>% ggplot(aes(x = tmax_max, fill = Category) ) + 
  geom_density(alpha = 0.8, color = 'black') +
  ggtitle("Density Plot of tmax_max")

#Density Plot of rad_max
imp.df1.naive %>% ggplot(aes(x = rad_max, fill = Category) ) + 
  geom_density(alpha = 0.8, color = 'black') +
  ggtitle("Density Plot of rad_max")

#Density Plot of ah_max
imp.df1.naive %>% ggplot(aes(x = ah_max, fill = Category) ) + 
  geom_density(alpha = 0.8, color = 'black') +
  ggtitle("Density Plot of ah_max")

#Density Plot of rh_max
imp.df1.naive %>% ggplot(aes(x = rh_max, fill = Category) ) + 
  geom_density(alpha = 0.8, color = 'black') +
  ggtitle("Density Plot of rh_max")

#Density Plot of rain_mean
imp.df1.naive %>% ggplot(aes(x = rain_mean, fill = Category) ) + 
  geom_density(alpha = 0.8, color = 'black') +
  ggtitle("Density Plot of rain_mean")

#Most of the above plots clearly are making distinction between high and low 
#number of cases. This shows the potential to build a model but the overlap 
#indicates that the model might not be 100% accurate.

#Data Partition into train:60% - Valid:40% split

set.seed(1234)
ind <- sample(2, nrow(imp.df1.naive), replace = T, prob = c(0.6,0.4))
trainnb <- imp.df1.naive[ind ==1,]
testnb <- imp.df1.naive[ind ==2,]

#Build Naive Bayes Model using trainnb data

model <- naive_bayes(Category ~., data = trainnb, usekernel = T)
model

plot(model)

#Predict
p <- predict(model, trainnb, type = 'prob')
head(cbind(p, trainnb))

#confusion matrix - train data
p1 <- predict(model, trainnb)
(tab1 <- table(p1, trainnb$Category))

#       0   1
#    0 474  83
#    1 105 603

1 - sum(diag(tab1)) / sum(tab1)
#[1] 0.1486166
#mis-classification rate is 14.86%
#Accuracy is 85.13%

#confusion matrix and predict the model on test data
predict_nb <- predict(model, testnb)
confusionMatrix(predict_nb, testnb$Category)

# Confusion Matrix and Statistics
# 
#             Reference
# Prediction   0   1
#          0 300  55
#          1  73 380
# 
# Accuracy : 0.8416          
# 95% CI : (0.8146, 0.8661)
# No Information Rate : 0.5384          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.6802          
# 
# Mcnemar's Test P-Value : 0.1329          
#                                           
#             Sensitivity : 0.8043          
#             Specificity : 0.8736          
#          Pos Pred Value : 0.8451          
#          Neg Pred Value : 0.8389          
#              Prevalence : 0.4616          
#          Detection Rate : 0.3713          
#    Detection Prevalence : 0.4394          
#       Balanced Accuracy : 0.8389          
#                                           
#        'Positive' Class : 0               
                                         
#Iteration: 4 - Select the best model 
# Choose the final model out of the 5 modls built according to the confusion matrix results.
#Make table with P-value, sensitivity, specificity,  and  balanced accuracy 
#of all models to find out which model will best suit to predict influenza hospitalizations in New Zealand.

models <- data.frame (Technique = c("Logistic_Regression", "KNN", "CART", "Random_Forest", "Naive_Bayes"),
                      Accuracy = c(0.8902, 0.87, 0.8329, 0.8654, 0.8416),
                      P_Value = c(2e-16, 2e-16, 2e-16, 2e-16, 2e-16),
                      Sensitivity = c(0.8717, 0.9149, 0.8338, 0.8430, 0.8043),
                      Specificity = c(0.9055, 0.8177, 0.8322, 0.8867, 0.8736),
                      POS_Pr_Value = c(0.8835, 0.8541, 0.8099, 0.8763, 0.8451),
                      Neg_Pr_Value = c(0.8957, 0.8918, 0.8538, 0.8558, 0.8389),
                      Balanced_Accuracy = c(0.8886, 0.8663, 0.8330, 0.8649, 0.8389))
models

library(reactable)
reactable(models, defaultColDef = colDef(align = "center"), bordered = TRUE,
          columns = list("Accuracy" = colDef(format = colFormat( percent = TRUE)),
                         "P_Value" = colDef(format = colFormat(percent = TRUE)),
                         "Sensitivity" = colDef(format = colFormat(percent = TRUE)),
                         "Specificity" = colDef(format = colFormat(percent = TRUE)),
                         "POS_Pr_Value" = colDef(format = colFormat(percent = TRUE)),
                         "Neg_Pr_Value" = colDef(format = colFormat(percent = TRUE)),
                         "Balanced_Accuracy" = colDef(format = colFormat(percent = TRUE))
                         
          ))

# Based on the confusion matrix metrics, Logistic regression is the model with 89.02% accuracy.
# So Logistic regression is the final model that could produce quality predictions on Influenza-
#-occurence in New Zealand

