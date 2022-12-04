library(tidyverse)
library(MatchThem) 
library(survey) 
library(knitr)


#load in multiply imputed, weighted data
#multinomial treatment = PA, focal = "Rarely/never"
#estimand = ATT, approach = "within", method = cbps
# load("imp_overall_long.RData")  
load("weightdat_overall.RData")




############ SURVEY DESIGN 
#
# des <- svydesign(ids = ~country, weights = ~1, 
#                                  data = imp_overall_long) 
# save(des, file = "des.RData")
load("des.RData")



########################            ##############################
########################     MHQ    ##############################
########################            ##############################
# > summary(mhm$mhq)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -166.40   -0.30   79.10   67.93  128.70  200.00 

mhq_overall <- with(weightdat_overall, svyglm(mhq ~ -1 + PA,
                                              design = des,
                                              family = gaussian()))

kable(summary(pool(mhq_overall)),
      digits = 3) 


# |term                    | estimate| std.error| statistic|       df| p.value|
# |:-----------------------|--------:|---------:|---------:|--------:|-------:|
# |PARarely/Never          |   47.063|     0.195|   241.576| 341785.0|       0|
# |PALess than once a week |   61.083|     0.370|   164.887| 325561.3|       0|
# |PAOnce a week           |   64.426|     0.491|   131.337| 320433.1|       0|
# |PAFew days a week       |   67.220|     0.309|   217.508| 249767.8|       0|
# |PAEvery day             |   67.904|     0.494|   137.567| 142827.9|       0|
# 


####### double robustness check  ############
mhq_overall_robust <-with(weightdat_overall, svyglm(mhq ~ -1 
                                             + PA 
                                             + age
                                             + sex
                                             + genderdiff
                                             + education
                                             + employment
                                             + relationship
                                             + socialize
                                             + sleep
                                             + meddiagnosis
                                             + mhseeking
                                             + childtrauma
                                             + adulttrauma,
                                                 
                                                 design = des,
                                                 
                                                 family = gaussian())) 



kable(summary(pool(mhq_overall_robust)),
      digits = 3) 


# |term                        | estimate| std.error| statistic|         df| p.value|
# |:---------------------------|--------:|---------:|---------:|----------:|-------:|
# |PARarely/Never              |   -1.065|     1.071|    -0.994|  20765.908|   0.320|
# |PALess than once a week     |   12.754|     1.112|    11.470|  22636.523|   0.000|
# |PAOnce a week               |   16.084|     1.161|    13.851|  23382.165|   0.000|
# |PAFew days a week           |   18.702|     1.100|    17.005|  22557.235|   0.000|
# |PAEvery day                 |   19.586|     1.162|    16.862|  25790.181|   0.000|

# how to interpret the p = 0.320?



###### collider bias check #########
### remove covariates that can plausibly be influenced
### by both exposure and outcome (PA and mental health)
### socialize, relationship, employment, sleep, mhseeking, meddiagnosis

mhq_overall_collider <-with(weightdat_overall, svyglm(mhq ~ -1 
                                                    + PA 
                                                    + age
                                                    + sex
                                                    + genderdiff
                                                    + education
                                                    + childtrauma
                                                    + adulttrauma,
                                                    
                                                    design = des,
                                                    
                                                    family = gaussian())) 



kable(summary(pool(mhq_overall_collider)),
      digits = 3) 

# |term                    | estimate| std.error| statistic|         df| p.value|
# |:-----------------------|--------:|---------:|---------:|----------:|-------:|
# |PARarely/Never          |   33.993|     0.784|    43.375|   8159.516|   0.000|
# |PALess than once a week |   47.902|     0.830|    57.741|  11265.698|   0.000|
# |PAOnce a week           |   51.191|     0.880|    58.161|   9977.521|   0.000|
# |PAFew days a week       |   54.041|     0.813|    66.496|   7171.193|   0.000|
# |PAEvery day             |   54.570|     0.902|    60.523|  11179.089|   0.000|



# 
# library(jtools)
# 
# plot_summs(mhq_overall)





#
#
#
#
#
#
##############################               ######################
##############################      COG      ######################
##############################               ######################
# > summary(mhm$cog)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -100.00   20.10   93.50   81.44  137.30  200.00 

cog_overall <-with(weightdat_overall, svyglm(cog ~ -1 + PA,
                                         
                                               design = des,
                                               family = gaussian()))


kable(summary(pool(cog_overall)),
      digits = 3) 

# |term                    | estimate| std.error| statistic|       df| p.value|
# |:-----------------------|--------:|---------:|---------:|--------:|-------:|
# |PARarely/Never          |   63.180|     0.182|   348.042| 341785.0|       0|
# |PALess than once a week |   75.622|     0.343|   220.366| 315336.3|       0|
# |PAOnce a week           |   78.621|     0.453|   173.603| 303708.1|       0|
# |PAFew days a week       |   81.389|     0.285|   285.338| 269660.4|       0|
# |PAEvery day             |   83.486|     0.452|   184.729| 208368.1|       0|





###### double robustness check ##########
cog_overall_robust <-with(weightdat_overall, svyglm(cog ~ -1 
                                                    + PA 
                                                    + age
                                                    + sex
                                                    + genderdiff
                                                    + education
                                                    + employment
                                                    + relationship
                                                    + socialize
                                                    + sleep
                                                    + meddiagnosis
                                                    + mhseeking
                                                    + childtrauma
                                                    + adulttrauma,
                                                    
                                                    design = des,
                                                    
                                                    family = gaussian())) 



kable(summary(pool(cog_overall_robust)),
      digits = 3) 

# WITHOUT INTERCEPT
# |term                          | estimate| std.error| statistic|         df| p.value|
# |:-----------------------------|--------:|---------:|---------:|----------:|-------:|
# |PARarely/Never                |   25.175|     1.042|    24.171|  25840.443|   0.000|
# |PALess than once a week       |   37.446|     1.082|    34.610|  26740.557|   0.000|
# |PAOnce a week                 |   40.421|     1.129|    35.806|  24728.011|   0.000|
# |PAFew days a week             |   43.075|     1.070|    40.254|  28133.069|   0.000|
# |PAEvery day                   |   45.309|     1.130|    40.093|  27550.391|   0.000|



####### collider bias check ##########
cog_overall_collider <-with(weightdat_overall, svyglm(cog ~ -1 
                                                      + PA 
                                                      + age
                                                      + sex
                                                      + genderdiff
                                                      + education
                                                      + childtrauma
                                                      + adulttrauma,
                                                      
                                                      design = des,
                                                      
                                                      family = gaussian())) 



kable(summary(pool(cog_overall_collider)),
      digits = 3) 

# |term                    | estimate| std.error| statistic|         df| p.value|
# |:-----------------------|--------:|---------:|---------:|----------:|-------:|
# |PARarely/Never          |   49.104|     0.744|    66.031|   5089.966|   0.000|
# |PALess than once a week |   61.446|     0.787|    78.034|   6731.084|   0.000|
# |PAOnce a week           |   64.390|     0.834|    77.192|   6390.519|   0.000|
# |PAFew days a week       |   67.224|     0.770|    87.269|   5188.957|   0.000|
# |PAEvery day             |   69.185|     0.856|    80.802|   6809.861|   0.000|





###############################                       #################
###############################   ADAPT / RESILIENCE  #################
###############################                       #################



adaptresil_overall <-with(weightdat_overall, svyglm(adaptresil ~  
                                               PA 
                                             + age
                                             + sex
                                             + genderdiff
                                             + education
                                             + employment
                                             + relationship
                                             + socialize
                                             + sleep
                                             + meddiagnosis
                                             + mhseeking
                                             + childtrauma
                                             + adulttrauma,
                                             
                                             design = des,
                                             
                                             family = gaussian()))


kable(summary(pool(adaptresil_overall)),
      digits = 3) 

# > summary(mhm$adaptresil)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -100.0    43.9   104.9    89.8   143.9   200.0 

# WITH INTERCEPT
# |term                         | estimate| std.error| statistic|         df| p.value|
# |:----------------------------|--------:|---------:|---------:|----------:|-------:|
# |(Intercept)                  |   34.665|     1.083|    32.011|   5372.983|   0.000|
# |PALess than once a week      |   12.192|     0.333|    36.585| 294918.387|   0.000|
# |PAOnce a week                |   15.295|     0.420|    36.384| 325101.928|   0.000|
# |PAFew days a week            |   19.564|     0.292|    67.066| 292584.708|   0.000|
# |PAEvery day                  |   22.060|     0.426|    51.794| 208896.867|   0.000|



# WITHOUT INTERCEPT
# |term                         | estimate| std.error| statistic|         df| p.value|
# |:--------------------------  |--------:|---------:|---------:|----------:|-------:|
# |PARarely/Never               |   34.665|     1.083|    32.011|   5372.983|   0.000|
# |PALess than once a week      |   46.858|     1.124|    41.678|   6008.758|   0.000|
# |PAOnce a week                |   49.960|     1.170|    42.685|   6759.149|   0.000|
# |PAFew days a week            |   54.229|     1.111|    48.810|   6112.262|   0.000|
# |PAEvery day                  |   56.725|     1.174|    48.330|   6550.329|   0.000|



##############################
##############################   DRIVE / MOTIVATION
##############################


drivemotiv_overall <-with(weightdat_overall, svyglm(drivemotiv ~ -1  
                                                    + PA 
                                                    + age
                                                    + sex
                                                    + genderdiff
                                                    + education
                                                    + employment
                                                    + relationship
                                                    + socialize
                                                    + sleep
                                                    + meddiagnosis
                                                    + mhseeking
                                                    + childtrauma
                                                    + adulttrauma,
                                                    
                                                    design = des,
                                                    
                                                    family = gaussian()))


kable(summary(pool(drivemotiv_overall)),
      digits = 3) 


# > summary(mhm$drivemotiv)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -100.00   29.90   94.30   83.39  137.90  200.00 


# WITHOUT INTERCEPT
# |term                            | estimate| std.error| statistic|         df| p.value|
# |:-------------------------------|--------:|---------:|---------:|----------:|-------:|
# |PARarely/Never                  |   25.237|     1.045|    24.144|  20861.827|   0.000|
# |PALess than once a week         |   36.745|     1.086|    33.850|  21040.779|   0.000|
# |PAOnce a week                   |   39.607|     1.133|    34.945|  23085.802|   0.000|
# |PAFew days a week               |   42.617|     1.074|    39.694|  23920.168|   0.000|
# |PAEvery day                     |   45.512|     1.136|    40.059|  23604.180|   0.000|


########################
########################  MOOD / OUTLOOK
########################


moodoutlook_overall <-with(weightdat_overall, svyglm(moodoutlook ~ -1  
                                                    + PA 
                                                    + age
                                                    + sex
                                                    + genderdiff
                                                    + education
                                                    + employment
                                                    + relationship
                                                    + socialize
                                                    + sleep
                                                    + meddiagnosis
                                                    + mhseeking
                                                    + childtrauma
                                                    + adulttrauma,
                                                    
                                                    design = des,
                                                    
                                                    family = gaussian()))


kable(summary(pool(moodoutlook_overall)),
      digits = 3) 

# > summary(mhm$moodoutlook)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -100.0    -3.0    73.7    67.2   128.8   200.0


# WITHOUT INTERCEPT

# |term                        | estimate| std.error| statistic|         df| p.value|
# |:---------------------------|--------:|---------:|---------:|----------:|-------:|
# |PARarely/Never              |    2.739|     1.035|     2.647|  13238.357|   0.008|
# |PALess than once a week     |   14.860|     1.073|    13.846|  15353.153|   0.000|
# |PAOnce a week               |   17.933|     1.120|    16.012|  15123.844|   0.000|
# |PAFew days a week           |   19.690|     1.063|    18.523|  14477.628|   0.000|
# |PAEvery day                 |   19.912|     1.122|    17.751|  16591.937|   0.000|




#################################
################################# SOCIAL SELF
#################################


socialself_overall <-with(weightdat_overall, svyglm(socialself ~ -1  
                                                    + PA 
                                                    + age
                                                    + sex
                                                    + genderdiff
                                                    + education
                                                    + employment
                                                    + relationship
                                                    + socialize
                                                    + sleep
                                                    + meddiagnosis
                                                    + mhseeking
                                                    + childtrauma
                                                    + adulttrauma,
                                                    
                                                    design = des,
                                                    
                                                    family = gaussian()))


kable(summary(pool(socialself_overall)),
      digits = 3) 

# > summary(mhm$socialself)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -100.00   -5.20   83.90   70.65  138.70  200.00 

# WITHOUT INTERCEPT
# |term                          | estimate| std.error| statistic|         df| p.value|
# |:-----------------------------|--------:|---------:|---------:|----------:|-------:|
# |PARarely/Never                |    7.675|     1.149|     6.680|  16432.288|   0.000|
# |PALess than once a week       |   18.114|     1.191|    15.207|  19122.334|   0.000|
# |PAOnce a week                 |   20.647|     1.245|    16.584|  20173.668|   0.000|
# |PAFew days a week             |   22.043|     1.179|    18.693|  17639.225|   0.000|
# |PAEvery day                   |   22.038|     1.240|    17.766|  18579.580|   0.000|





######################### 
#########################  MIND BODY
#########################



mindbody_overall <-with(weightdat_overall, svyglm(mindbody ~ -1  
                                                    + PA 
                                                    + age
                                                    + sex
                                                    + genderdiff
                                                    + education
                                                    + employment
                                                    + relationship
                                                    + socialize
                                                    + sleep
                                                    + meddiagnosis
                                                    + mhseeking
                                                    + childtrauma
                                                    + adulttrauma,
                                                    
                                                    design = des,
                                                    
                                                    family = gaussian()))


kable(summary(pool(mindbody_overall)),
      digits = 3) 

# summary(mhm$mindbody)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -100.00   20.50   84.10   73.43  123.60  200.00 

# WITHOUT INTERCEPT
# |term                        | estimate| std.error| statistic|         df| p.value|
# |:---------------------------|--------:|---------:|---------:|----------:|-------:|
# |PARarely/Never              |   10.765|     0.979|    10.993|  10590.779|   0.000|
# |PALess than once a week     |   23.995|     1.017|    23.585|  11444.638|   0.000|
# |PAOnce a week               |   27.849|     1.061|    26.240|  13913.513|   0.000|
# |PAFew days a week           |   32.530|     1.006|    32.339|  13556.652|   0.000|
# |PAEvery day                 |   35.609|     1.061|    33.572|  16601.958|   0.000|





















