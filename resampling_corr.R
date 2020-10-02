# Sanity check: We repeat random imputation of missing correlation between the two measures (done in data_completion.R for the main database), and recompute effect sizes 100 times to check it doesn't change the results.


#set up the dataframe that will contain the results after resampling (one line per resampling)
resamp_stats = base_model$reg_table
resamp_prob = base_model$reg_table$prob

for (s in 1:100){
  
  data = DB2 #make a copy of the database without the imputed correlations
  data$corr_imputed[is.na(data$corr)==T] = NA
  data$corr_imputed[data$participant_design == "within_two"] <- impute(data$corr[data$participant_design == "within_two"],fun='random')
  
  # recompute effect sizes
  # empty the relevant variables
  data$d_calc = NA
  data$d_var_calc = NA
  data$g_calc = NA
  data$g_var_calc = NA
  data$es_method = NA
  
  #effect size calculation
  for (i in 1:nrow(data)){
    db = data[i,]
    if (is.na(db$corr) | db$corr > .99 | db$corr < .01){
      #if correlation between two measures is not reported, use an imputed correlation value
      #we also account for the observation that some re-calculated values are impossible and replace those
      corr <- db$corr_imputed
    }else{corr <- db$corr}
    
    if (complete(db$x_1, db$x_2, db$SD_1, db$SD_2)) {
      pooled_SD <- sqrt((db$SD_1 ^ 2 + db$SD_2 ^ 2) / 2) # Lipsey & Wilson (2001)
      d_calc <- (db$x_1 - db$x_2) / pooled_SD # Lipsey & Wilson (2001)
      es_method  <- "group_means_two"
    } else if (complete(db$t)) {
      wc <- sqrt(2 * (1 - corr))
      d_calc <- (db$t / sqrt(db$n_1)) * wc #Dunlap et al., 1996, p.171
      es_method  <- "t_two"
    } else if (complete(db$F)) {
      wc <- sqrt(2 * (1 - corr))
      d_calc <- sqrt(db$F / db$n_1) * wc
      es_method  <- "f_two"
    } else {d_calc = NA}
    #now that effect sizes are calculated, effect size variance is calculated
    if (complete(db$n_1, d_calc)) {
      d_var_calc <- (2 * (1 - corr)/ db$n_1) + (d_calc ^ 2 / (2 * db$n_1)) # Lipsey & Wilson (2001)
    } else if (complete(db$d, db$d_var)) {
      #if d and d_var were already reported, use those values
      d_calc <- db$d
      d_var_calc <- db$d_var
      es_method  <- "d_two"
    } else {d_var_calc = NA}
    
    df <- db$n_1 - 1
    J <- 1 - 3 / (4 * (df - 1))
    g_calc <- d_calc * J
    g_var_calc <- J ^ 2 * d_var_calc
    
    #add the results to the database
    db$d_calc = d_calc
    db$d_var_calc = d_var_calc
    db$g_calc = g_calc
    db$g_var_calc = g_var_calc
    db$es_method = es_method
    
    data[i,] = db
  }
  
  #Mark effect sizes more than 3 SD away from the mean effect (in both positive and negative directions) as outliers
  data$outlier <- F #create the variable, and set as no by default (majority of cases hopefully!)
  data$outlier[data$d_calc > mean(data$d_calc, na.rm = TRUE) + 3*sd(data$d_calc, na.rm = TRUE) | data$d_calc < mean(data$d_calc, na.rm = TRUE) - 3*sd(data$d_calc, na.rm = TRUE)]<-T 
  
  # Re-fit the meta-analytic model
  #average variance / infant group to build models with CORR, and allow robumeta to detect correlated effect sizes within papers. 
  data = data %>% group_by(same_infant) %>% mutate(g_var_m = mean(g_var_calc))
  base <- robu(g_calc ~ 1, data=data, modelweights = "CORR", studynum = same_infant, var.eff.size = g_var_m, small = T) 
  resamp_stats = bind_rows(resamp_stats,base$reg_table)
  
}

# check that the results don't change in 95 % of cases
resamp_alphas <- ifelse(resamp_stats$prob < 0.05, 1, 0)
resamp_alphas <- length(resamp_alphas[resamp_alphas==0])/length(resamp_alphas)
resamp_alphas