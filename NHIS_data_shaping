############################################################################
### NHIS (National Health Interview Survey ) Import ########################
############################################################################

# NHIS is America's most detailed householde 
#survey of health status and medical experience

install.packages( "devtools" , repos = "http://cran.rstudio.com/" )
library(devtools)
install.packages("glue")

install_github( "ajdamico/lodown" , dependencies = TRUE ) #type space bar


# all variables NHIS
library(lodown)
lodown( "nhis" , output_dir = file.path( path.expand( "~" ) , "NHIS" ) )

nhis_cat <-
  get_catalog( "nhis" ,
               output_dir = file.path( path.expand( "~" ) , "NHIS" ) )

# 2016 only
nhis_cat2016 <- subset( nhis_cat , year == 2016 )
# 2017 only
nhis_cat2017 <- subset( nhis_cat , year == 2017 )

# download the microdata to your local computer
nhis_cat2016 <- lodown( "nhis" , nhis_cat2016 )
nhis_cat2017 <- lodown( "nhis" , nhis_cat2017 )
unique(nhis_cat$type)# give lists of type

# construct a multipley-inputed, complex sample survey design:
options( survey.lonely.psu = "adjust" )

library(survey)
library(mitools)

# filter for year 2016 personsx type information
nhis_personsx_df <- 
  readRDS( file.path( path.expand( "~" ) , "NHIS" , "2016/personsx.rds" ) )

nhis_nhis_income_list <- 
  readRDS( file.path( path.expand( "~" ) , "NHIS" , "2016/incmimp.rds" ) )

#merge several variables from each types
merge_variables <- c( "hhx" , "fmx" , "fpx" )
#choose common variables from each type "hhx" , "fmx" , "fpx"

#set the variables as numeric
nhis_personsx_df[ merge_variables ] <- 
  sapply( nhis_personsx_df[ merge_variables ] , as.numeric )

#select to keep variables
inc_vars_to_keep <- 
  c( 
    merge_variables , 
    setdiff( 
      names( nhis_income_list[[ 1 ]] ) , # choose ii1 from ths incom lists
      names( nhis_personsx_df )
    )
  ) # setdiff function only choose unique names in income but person

# personsx variables to keep
vars_to_keep <- 
  c( merge_variables , "ppsu" , "pstrat" , "wtfa" ,
     'phstat' , 'sex' , 'hospno' , 'age_p' , 'hinotmyr' , 'notcov' )
nhis_personsx_df <- nhis_personsx_df[ vars_to_keep ]#set as data frame

#complete the data frame as list
nhis_personsx_list <-
  lapply( nhis_income_list ,
          function( w ){
            w <- w[ inc_vars_to_keep ]
            w[ merge_variables ] <- sapply( w[ merge_variables ] , as.numeric )
            result <- merge( nhis_personsx_df , w )
            stopifnot( nrow( result ) == nrow( nhis_personsx_df ) )
            result
          } )

# personsx design       
nhis_design <- 
  svydesign( 
    id = ~ppsu , 
    strata = ~pstrat ,
    nest = TRUE ,
    weights = ~wtfa ,
    data = imputationList( nhis_personsx_list )
  )
#gc()never make lose variables use after rm()
rm( nhis_personsx_list ) ; gc()

#same progress using samadult type
nhis_samadult_df <- 
  readRDS( file.path( path.expand( "~" ) , "NHIS" , "2016/samadult.rds" ) )

nhis_samadult_df[ merge_variables ] <- 
  sapply( nhis_samadult_df[ merge_variables ] , as.numeric )

samadult_vars_to_keep <- 
  c( 
    merge_variables , 
    setdiff( 
      names( nhis_samadult_df ) , 
      names( nhis_personsx_df ) 
    ) 
  )

nhis_personsx_samadult_df <-
  merge( nhis_personsx_df , nhis_samadult_df[ samadult_vars_to_keep ] )

stopifnot( nrow( nhis_personsx_samadult_df ) == nrow( nhis_samadult_df ) )

rm( nhis_personsx_df , nhis_samadult_df ) ; gc()

nhis_samadult_list <-
  lapply( nhis_income_list ,
          function( w ){
            w <- w[ inc_vars_to_keep ]
            w[ merge_variables ] <- sapply( w[ merge_variables ] , as.numeric )
            result <- merge( nhis_personsx_samadult_df , w )
            stopifnot( nrow( result ) == nrow( nhis_personsx_samadult_df ) )
            result
          } )

rm( nhis_income_list , nhis_personsx_samadult_df ) ; gc()

# sample adult design (commented out)
# nhis_samadult_design <- 
# svydesign( 
# id = ~ppsu , 
# strata = ~pstrat ,
# nest = TRUE ,
# weights = ~wtfa_sa ,
# data = imputationList( nhis_samadult_list )
# )

rm( nhis_samadult_list ) ; gc()



#variable recoding
nhis_design <- 
  update( 
    nhis_design , 
    
    one = 1 ,
    
    poverty_category =
      factor( 
        findInterval( povrati3 , 1:4 ) ,
        labels = 
          c( "below poverty" , "100-199%" , "200-299%" , "300-399%" , "400%+" )
      ) ,
    
    fair_or_poor_reported_health = 
      ifelse( phstat %in% 1:5 , as.numeric( phstat >= 4 ) , NA ) ,
    
    sex = factor( sex , labels = c( "male" , "female" ) ) ,
    
    hospno = ifelse( hospno > 366 , NA , hospno )
    
  )
#unweigted counts
MIcombine( with( nhis_design , svyby( ~ one , ~ one , unwtd.count ) ) )

MIcombine( with( nhis_design , svyby( ~ one , ~ poverty_category , unwtd.count ) ) )

#weigted counts
MIcombine( with( nhis_design , svytotal( ~ one ) ) )

MIcombine( with( nhis_design ,
                 svyby( ~ one , ~ poverty_category , svytotal )
) )

#descriptive sta
#calculate the mean
MIcombine( with( nhis_design , svymean( ~ age_p ) ) )#ovreall

MIcombine( with( nhis_design ,
                 svyby( ~ age_p , ~ poverty_category , svymean ) #by groups
) )
#calculate distribution of categorical variables overall and by group
MIcombine( with( nhis_design , svymean( ~ sex ) ) )

MIcombine( with( nhis_design ,
                 svyby( ~ sex , ~ poverty_category , svymean )
) )

#calculate the median o fa linear variable
MIcombine( with( nhis_design ,
                 svyquantile(
                   ~ age_p ,
                   0.5 , se = TRUE 
                 ) ) )

MIcombine( with( nhis_design ,
                 svyby(
                   ~ age_p , ~ poverty_category , svyquantile ,
                   0.5 , se = TRUE ,
                   keep.var = TRUE , ci = TRUE 
                 ) ) )
#eatimate a ratio
MIcombine( with( nhis_design ,
                 svyratio( numerator = ~ hinotmyr , denominator = ~ hospno , na.rm = TRUE )
) )
##################### subetting #############################
sub_nhis_design <- subset(nhis_design, notcov ==1)
str(sub_nhis_design)
#calculate the mean of this subset
MIcombine(with(sub_nhis_design, svymean(~age_p)))


# measures of uncertainty
this_result <-
  MIcombine( with( nhis_design ,
                   svymean( ~ age_p )
  ) )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
  MIcombine( with( nhis_design ,
                   svyby( ~ age_p , ~ poverty_category , svymean )
  ) )

coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )

#Calculate the degrees of freedom of any survey design object
degf(nhis_design$designs[[1]])
#Calculate the complex sample survey-adjusted variance of any statistic
MIcombine(with(nhis_design, svyvar(~age_p)))
#Include the complex sample design effect in the result for a specific statistic
# SRS without replacement
MIcombine( with( nhis_design ,
                 svymean( ~ age_p , deff = TRUE )
) )

# SRS with replacement
MIcombine( with( nhis_design ,
                 svymean( ~ age_p , deff = "replace" )
) )
#Compute confidence intervals for proportions using methods that may be more accurate near 0 and 1. See ?svyciprop for alternatives
MIsvyciprop( ~ fair_or_poor_reported_health , nhis_design ,
             method = "likelihood" , na.rm = TRUE )
#Regression Models and Tests of Association
#Perform a design-based t-test:
  
  MIsvyttest( age_p ~ fair_or_poor_reported_health , nhis_design )
#Perform a chi-squared test of association for survey data:
  
  MIsvychisq( ~ fair_or_poor_reported_health + sex , nhis_design )
#Perform a survey-weighted generalized linear model:
  
  glm_result <- 
  MIcombine( with( nhis_design ,
                   svyglm( age_p ~ fair_or_poor_reported_health + sex )
  ) )

summary( glm_result )

