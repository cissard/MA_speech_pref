source("compute_es.R", chdir = TRUE)  #chdir stands for "change directory"

library(metafor)
library(robumeta)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
library(langcog)
library(grid)
library(gridExtra)
library(RCurl)
library(papaja)

# Comment out next set of lines for RECALCULATION
require(RCurl)
u <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRzzqtgNdfoKTMqb4bWyy5LyH5XdrOEy4sl3VNDCnGIyvdrny4wwUBeKPvy8tXczN0ri0yp94Kxgun_/pub?gid=0&single=true&output=csv"
tc <- getURL(u, ssl.verifypeer=FALSE)
DB <- read.csv(textConnection(tc))
write.csv(DB,"MA_speech_pref_data.csv")
# Uncomment next line for OFFLINE MODE
#DB <- read.csv("MA_speech_pref_data.csv", header = T, sep = ",", na.strings = "")

## FIX, remove empty columns
rmcol=NULL
for(mycol in 1:dim(DB)[2]) if(sum(is.na(DB[,mycol]))==length(DB[,mycol])) rmcol=c(rmcol,colnames(DB)[mycol])
rmcol[!(rmcol %in% "corr")]->rmcol
DB[,!(colnames(DB) %in% rmcol)]->DB
dim(DB)

## FIX, DOUBLE CHECK ALEX & CECILE !! REPLACE ALL EMPTY WITH NA
for(mycol in colnames(DB)) DB[DB[,mycol]=="" & !is.na(DB[,mycol]),mycol]<-NA
for(mycol in c("natural","vocal","homospecific","test_lang")) DB[,mycol]<-factor(DB[,mycol]) 
summary(DB)

#unique studies
papers <- levels(factor(DB$short_cite))

#number of unique infants
DB$n<-rowSums( cbind (DB$n_1,DB$n_2), na.rm=TRUE)
temp<-aggregate(n~same_infant,DB,mean)

summary(DB$mean_age_1)

#calculate correlations
for (i in 1:nrow(DB)){
  db = DB[i,]
  if (db$participant_design == "within_two") {
    # Use raw means, SD, and t-values to calculate correlations
    if (is.na(db$corr) & complete(db$x_1, db$x_2, db$SD_1, db$SD_2, db$t)) {
      db$corr = (db$SD_1^2 + db$SD_2^2 - (db$n_1 * (db$x_1 - db$x_2)^2 / db$t^2)) / (2 * db$SD_1 * db$SD_2)
    }
    DB[i,] = db
  }
}

#if all of these measures are not reported, use an imputed correlation value
#we also account for the observation that some re-calculated values are impossible and replace those
DB$corr=as.numeric(as.character(DB$corr)) #ac introduced to avoid error below -- see ^^*^^
for (i in 1:nrow(DB)){
  db = DB[i,]
  if (db$participant_design == "within_two") {
    if (is.na(db$corr) | db$corr > .99 | db$corr < .01){ #ac ^^*^^ In Ops.factor(db$corr, 0.99) : ‘>’ not meaningful for factors error because the NAs were taken as factor
      db$corr = runif(1, min = 0.01, max = 0.99)
    }
    DB[i,] = db
  }
}

#We create variables for effect sizes (ES)
DB$d_calc = NA
DB$d_var_calc = NA
DB$g_calc = NA
DB$g_var_calc = NA
DB$r_calc = NA
DB$r_var_calc = NA
DB$z_calc = NA
DB$z_var_calc = NA
DB$log_odds_calc = NA
DB$log_odds_var_calc = NA
DB$es_method = NA

#we introduce variables d_calc and d_var_calc to distiguish them from the fields d and d_var, which are fields where effect sizes were already available from the source of the data
d_calc <- NA
d_var_calc <- NA
es_method <- "missing"

#start of decision tree where effect sizes are calculated differently based on participant design depending on which data is available, effect sizes are calculated differently
for (i in 1:nrow(DB)){
  db = DB[i,]
  if (db$participant_design == "between") {
    es_method  <- "between"
    #effect size calculation
    if (complete(db$x_1, db$x_2, db$SD_1, db$SD_2)) {
      pooled_SD <- sqrt(((db$n_1 - 1) * db$SD_1 ^ 2 + (db$n_2 - 1) * db$SD_2 ^ 2) / (db$n_1 + db$n_2 - 2)) # Lipsey & Wilson, 3.14
      d_calc <- (db$x_1 - db$x_2) / pooled_SD # Lipsey & Wilson (2001)
    } else if (complete(db$t)) {
      d_calc <- db$t * sqrt((db$n_1 + db$n_2) / (db$n_1 * db$n_2)) # Lipsey & Wilson, (2001)
    } else if (complete(db$F)) {
      d_calc <- sqrt(db$F * (db$n_1 + db$n_2) / (db$n_1 * db$n_2)) # Lipsey & Wilson, (2001)
    } else {d_calc = NA}
    #now that effect size are calculated, effect size variance is calculated
    if (complete(db$n_1, db$n_2, d_calc)) {
      d_var_calc <- ((db$n_1 + db$n_2) / (db$n_1 * db$n_2)) + (d_calc ^ 2 / (2 * (db$n_1 + db$n_2)))
    } else if (complete(db$d, db$d_var)) {
      #if d and d_var were already reported, use those values
      d_calc <- d
      d_var_calc <- d_var
    } else {d_var_calc = NA}
    
  } else if (db$participant_design == "within_two") {
    
    #effect size calculation
    if (complete(db$x_1, db$x_2, db$SD_1, db$SD_2)) {
      pooled_SD <- sqrt((db$SD_1 ^ 2 + db$SD_2 ^ 2) / 2) # Lipsey & Wilson (2001)
      d_calc <- (db$x_1 - db$x_2) / pooled_SD # Lipsey & Wilson (2001)
      es_method  <- "group_means_two"
    } else if (complete(db$t)) {
      wc <- sqrt(2 * (1 - db$corr))
      d_calc <- (db$t / sqrt(db$n_1)) * wc #Dunlap et al., 1996, p.171
      es_method  <- "t_two"
    } else if (complete(db$F)) {
      wc <- sqrt(2 * (1 - db$corr))
      d_calc <- sqrt(db$F / db$n_1) * wc
      es_method  <- "f_two"
    } else {d_calc = NA}
    #now that effect size are calculated, effect size variance is calculated
    if (complete(db$n_1, d_calc)) {
      d_var_calc <- (2 * (1 - db$corr)/ db$n_1) + (d_calc ^ 2 / (2 * db$n_1)) # Lipsey & Wilson (2001)
    } else if (complete(db$d, db$d_var)) {
      #if d and d_var were already reported, use those values
      d_calc <- db$d
      d_var_calc <- db$d_var
      es_method  <- "d_two"
    } else {d_var_calc = NA}
    
  } else if (db$participant_design == "within_one") {
    if (complete(db$x_1, db$x_2, db$SD_1)) {
      d_calc <- (db$x_1 - db$x_2) / db$SD_1
      es_method  <- "group_means_one"
    } else if (complete(db$t)) {
      d_calc <- db$t / sqrt(db$n_1)
      es_method  <- "t_one"
    } else if (complete(db$F)) {
      d_calc <- sqrt(db$F / db$n_1)
      es_method  <- "f_one"
    } else {d_calc = NA}
  }
  
  df <- if (db$participant_design == "between") {
    sum(db$n_1, db$n_2, na.rm = TRUE) - 2
  } else {
    db$n_1 - 1
  }
  J <- 1 - 3 / (4 * (df - 1))
  g_calc <- d_calc * J
  g_var_calc <- J ^ 2 * d_var_calc
  
  if (db$participant_design == "between") {
    a <- (sum(db$n_1, db$n_2, na.rm = TRUE) ^ 2) / prod(db$n_1, db$n_2, na.rm = TRUE)
  } else {
    a <- 4
  }
  r_calc <- d_calc / sqrt(d_calc ^ 2 + a)
  r_var_calc <- a ^ 2 * d_var_calc / (d_calc ^ 2 + a) ^ 3
  
  z_calc <- 0.5 * log((1 + r_calc) / (1 - r_calc))
  z_var_calc = 1 / (db$n_1 - 3)
  
  log_odds_calc <- d_calc * pi / sqrt(3)
  log_odds_var_calc <- d_var_calc * pi ^ 2 / 3
  
  #add the results to the database
  db$d_calc = d_calc
  db$d_var_calc = d_var_calc
  db$g_calc = g_calc
  db$g_var_calc = g_var_calc
  db$r_calc = r_calc
  db$r_var_calc = r_var_calc
  db$z_calc = z_calc
  db$z_var_calc = z_var_calc
  db$log_odds_calc = log_odds_calc
  db$log_odds_var_calc = log_odds_var_calc
  db$es_method = es_method
  
  DB[i,] = db
}
#ac: lots of errors in the loop above - is that ok? 
#Cécile: Hmm weird, I cleaned my environment and reran the code but I didn't get any error... What kind of error do you get?


#Mark effect sizes more than 3 SD away from the mean effect (in both positive and negative directions) as outliers
DB$outlier <- F #create the variable, and set as no by default (majority of cases hopefully!)
DB$outlier[DB$d_calc > mean(DB$d_calc, na.rm = TRUE) + 3*sd(DB$d_calc, na.rm = TRUE) | DB$d_calc < mean(DB$d_calc, na.rm = TRUE) - 3*sd(DB$d_calc, na.rm = TRUE)]<-T 

#Visualize the outliers
outliers<-subset(DB,outlier==T)
outliers

#centering of age (although some rows also have mean_age_2, it is always the same as mean_age_1 in this db, hence the latter is used)
DB$agec<-scale(DB$mean_age_1,scale=F)

# save the complete data base
write.csv(DB,'speech_pref_full_DB.csv')
read.csv('speech_pref_full_DB.csv')->DB

summary(DB)

#info 
table(DB$response_mode)
#summarize the data
mean(DB$g_calc,na.rm=T)
sd(DB$g_calc,na.rm=T)
boxplot(DB$g_calc)
points(DB$g_calc)