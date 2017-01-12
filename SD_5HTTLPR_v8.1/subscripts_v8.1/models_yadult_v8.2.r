###########################################
# 5HTT=depression Models
# stress=depression Models  
# all age-inclusive sample
#
# version 8.2
#
# created by Amy Horton, Younghun Han, Chris Amos,
#            Sarah Hartz, and Rob Culverhouse
#
###########################################

cat("=== run simple regression models without gene x stress interaction", "\n" )
cat("=== START of combined young adults models, covariates only", "\n")
cat("===== run young adults models", "\n\n")

###### test if sex,age variables defined for >10% of dataset, test dataset descriptors #######  
if( if10(sd_5htt_ya_life[,"female"]) & if10(sd_5htt_ya_life[,"age"]) ) {  

  attach(sd_5htt_ya_life) ###### make dataframe part of search list
 #################### 
 # covariate models #
 ####################
  snplist <- "placeholder"
  nsnps <- length(snplist)

  file_label <- "_Covar_YAdult_"
  
  both_sexes<-two_level_present("female")
    
 # logistic regression models #
 ##############################

  outcomes <- c("dep_dx_life")
  models<-"~ female + age + birth_decade"	
  LogR_DDcovar.call(outcomes,models,"holder")

  
 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
      outcomes <- c("dep_quant_life","dep_quant_life_z") else
  outcomes <- c("dep_quant_life_z")
  models<- "~ female + age + birth_decade"	
  LinR_QDcovar.call(outcomes,models,"holder")

  detach(sd_5htt_ya_life)
}

if( if10(sd_5htt_ya_curr[,"female"]) & if10(sd_5htt_ya_curr[,"age"]) ) {  

  attach(sd_5htt_ya_curr) ###### make dataframe part of search list
 #################### 
 # covariate models #
 ####################
  snplist <- "placeholder"
  nsnps <- length(snplist)

  file_label <- "_Covar_YAdult_"
  
  both_sexes<-two_level_present("female")
    
 # logistic regression models #
 ##############################

  outcomes <- c("dep_dx","dep_dx_curr")
  models<-"~ female + age + birth_decade"	
  LogR_DDcovar.call(outcomes,models,"holder")

  
 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("dep_q","dep_quant_curr","dep_q_z","dep_quant_curr_z") else
  outcomes <- c("dep_q_z","dep_quant_curr_z")
  models<- "~ female + age + birth_decade"	
  LinR_QDcovar.call(outcomes,models,"holder")

  detach(sd_5htt_ya_curr)
}
cat("=== END of combined young adult models, covariates only models", "\n")


 ########################## 
 # SNP association models #
 ##########################
cat("=== START of combined young adult models, SNP plus covariates", "\n")
cat("===== run young adult models", "\n\n")
if( if10(sd_5htt_ya_life[,"female"]) & if10(sd_5htt_ya_life[,"age"]) ) {  

  attach(sd_5htt_ya_life) ###### make dataframe part of search list
  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_SNP_YAdult_"

  both_sexes<-two_level_present("female")
    
 # logistic regression models #
 ##############################

  outcomes <- c("dep_dx_life")
  models<- "~ female + age + birth_decade + SNP"	
  LogR_DD_SNP.call(outcomes,models,"holder")  	      

  
 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("dep_quant_life","dep_quant_life_z") else
  outcomes <- c("dep_quant_life_z")
  models<-"~ female + age + birth_decade + SNP"	
  LinR_QD_SNP.call(outcomes,models,"holder")  	      

  
  detach(sd_5htt_ya_life)
}

if( if10(sd_5htt_ya_curr[,"female"]) & if10(sd_5htt_ya_curr[,"age"]) ) {  

    attach(sd_5htt_ya_curr) ###### make dataframe part of search list
  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_SNP_YAdult_"

  both_sexes<-two_level_present("female")
    
 # logistic regression models #
 ##############################

  outcomes <- c("dep_dx","dep_dx_curr")
  models<- "~ female + age + birth_decade + SNP"	
  LogR_DD_SNP.call(outcomes,models,"holder")  	      

  
 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("dep_q","dep_quant_curr","dep_q_z","dep_quant_curr_z") else
  outcomes <- c("dep_q_z","dep_quant_curr_z")
  models<-"~ female + age + birth_decade + SNP"	
  LinR_QD_SNP.call(outcomes,models,"holder")  	      

  
  detach(sd_5htt_ya_curr)
}
cat("=== END of combined young adult models, SNP plus covariates models", "\n")



 ############################################# 
 # childhood maltreatment association models #
 #############################################
cat("=== START of young adult models, childhood maltreatment plus covariates", "\n")
if( if10(sd_5htt_ya_life[,"female"]) & if10(sd_5htt_ya_life[,"age"]) ) {  

  attach(sd_5htt_ya_life)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_ChildMal_YAdult_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD_life")
  models<-"~ female + age + birth_decade + child_mal_exp"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_life")
  models<- "~ female + age + birth_decade + child_mal_quant"				 
  LogR_DDQS.call(outcomes, models,"holder")
  

 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT")
    outcomes <- c("QD_life","QDz_life") else
  outcomes <- c("QDz_life")
  models<-"~ female + age + birth_decade + child_mal_exp"	
  LinR_QDDS.call(outcomes, models,"holder")

  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT")
    outcomes <- c("QD_life","QDz_life") else
  outcomes <- c("QDz_life")
  models<-"~ female + age + birth_decade + child_mal_quant"	
  LinR_QDQS.call(outcomes, models,"holder")


  detach(sd_5htt_ya_life)
}
if( if10(sd_5htt_ya_curr[,"female"]) & if10(sd_5htt_ya_curr[,"age"]) ) {  

  attach(sd_5htt_ya_curr)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_ChildMal_YAdult_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD","DD_curr")
  models<-"~ female + age + birth_decade + child_mal_exp"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD","DD_curr")
  models<- "~ female + age + birth_decade + child_mal_quant"				 
  LogR_DDQS.call(outcomes, models,"holder")
  

 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT")
    outcomes <- c("QD","QD_curr","QDz","QDz_curr") else
  outcomes <- c("QDz","QDz_curr")
  models<-"~ female + age + birth_decade + child_mal_exp"	
  LinR_QDDS.call(outcomes, models,"holder")

  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT")
    outcomes <- c("QD","QD_curr","QDz","QDz_curr") else
  outcomes <- c("QDz","QDz_curr")
  models<-"~ female + age + birth_decade + child_mal_quant"	
  LinR_QDQS.call(outcomes, models,"holder")


  detach(sd_5htt_ya_curr)
}
cat("=== END of young adult models, childhood maltreatment plus covariates", "\n")


 #################################### 
 # non CM stress association models #
 ####################################
cat("=== START of young adult models, other than childhood maltreatment stress plus covariates, no time limit", "\n")
if( if10(sd_5htt_ya_life[,"female"]) & if10(sd_5htt_ya_life[,"age"]) ) {  

  attach(sd_5htt_ya_life)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_OtherThanChildMal_YAdult_no_tlimit_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD_life")
  models<-"~ female + age + birth_decade + life_stress_exp"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_life")
  models<-"~ female + age + birth_decade + life_stress_quant"	
  LogR_DDQS.call(outcomes, models,"holder")    
 
 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT")
    outcomes <- c("QD_life","QDz_life") else
  outcomes <- c("QDz_life")
  models<-"~ female + age + birth_decade + life_stress_exp"	
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life","QDz_life") else
  outcomes <- c("QDz_life")
  models<- "~ female + age + birth_decade + life_stress_quant"	
  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_ya_life)
}


if( if10(sd_5htt_ya_curr[,"female"]) & if10(sd_5htt_ya_curr[,"age"]) ) {  

  attach(sd_5htt_ya_curr)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_OtherThanChildMal_YAdult_no_tlimit_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD","DD_curr")
  models<-"~ female + age + birth_decade + life_stress_exp"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD","DD_curr")
  models<-"~ female + age + birth_decade + life_stress_quant"	
  LogR_DDQS.call(outcomes, models,"holder")    
 
 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT")
    outcomes <- c("QD","QD_curr","QDz","QDz_curr") else
  outcomes <- c("QDz","QDz_curr")
  models<-"~ female + age + birth_decade + life_stress_exp"	
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD","QD_curr","QDz","QDz_curr") else
  outcomes <- c("QDz","QDz_curr")
  models<- "~ female + age + birth_decade + life_stress_quant"	
  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_ya_curr)
}

cat("=== END of young adult models, non-CM stress plus covariates, no time limit", "\n")




 #################################### 
 # non CM stress association models #
 ####################################
cat("=== START of young adult models, other than childhood maltreatment stress plus covariates, time ordered", "\n")
if( if10(sd_5htt_other_life_ya[,"female"]) & if10(sd_5htt_other_life_ya[,"age"]) ) {  

  attach(sd_5htt_other_life_ya)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_OtherThanChildMal_YAdult_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD_life_before")
  models<- "~ female + age + birth_decade + life_stress_exp"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_life_before")
  models<-"~ female + age + birth_decade + life_stress_quant"	
  LogR_DDQS.call(outcomes, models,"holder")

 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life_before","QDz_life_before") else
  outcomes <- c("QDz_life_before") 
  models<-"~ female + age + birth_decade + life_stress_exp"	
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life_before","QDz_life_before") else
  outcomes <- c("QDz_life_before")
  models<-"~ female + age + birth_decade + life_stress_quant"	
  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_other_life_ya)
}

if( if10(sd_5htt_other_curr_ya[,"female"]) & if10(sd_5htt_other_curr_ya[,"age"]) ) {  

  attach(sd_5htt_other_curr_ya)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_OtherThanChildMal_YAdult_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD_before","DD_curr_before")
  models<- "~ female + age + birth_decade + life_stress_exp"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_before","DD_curr_before")
  models<-"~ female + age + birth_decade + life_stress_quant"	
  LogR_DDQS.call(outcomes, models,"holder")

 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_before","QD_curr_before","QDz_before","QDz_curr_before") else
  outcomes <- c("QDz_before","QDz_curr_before") 
  models<-"~ female + age + birth_decade + life_stress_exp"	
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_before","QD_curr_before","QDz_before","QDz_curr_before") else
  outcomes <- c("QDz_before","QDz_curr_before")
  models<-"~ female + age + birth_decade + life_stress_quant"	
  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_other_curr_ya)
}

cat("=== END of young adult models, non-childhood maltreatment stress plus covariates, time ordered", "\n")




 #################################### 
 # non CM stress association models #
 ####################################
cat("=== START of young adult models, other than childhood maltreatment stress plus covariates, 5yr time limit", "\n")
if( if10(sd_5htt_other_life_5yr_ya[,"female"]) & if10(sd_5htt_other_life_5yr_ya[,"age"]) ) {  

  attach(sd_5htt_other_life_5yr_ya)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_OtherThanChildMal_YAdult_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD_life_5yr")
  models<-"~ female + age + birth_decade + life_stress_exp_5yr_life"
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_life_5yr")
  models<-"~ female + age + birth_decade + life_stress_quant_5yr_life"
  LogR_DDQS.call(outcomes, models,"holder")   	  


 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life_5yr","QDz_life_5yr") else
  outcomes <- c("QDz_life_5yr")
  models<-"~ female + age + birth_decade + life_stress_exp_5yr_life"
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life_5yr","QDz_life_5yr") else
  outcomes <- c("QDz_life_5yr")
  models<-"~ female + age + birth_decade + life_stress_quant_5yr_life"
  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_other_life_5yr_ya)
}

if( if10(sd_5htt_other_curr_5yr_ya[,"female"]) & if10(sd_5htt_other_curr_5yr_ya[,"age"]) ) {  

  attach(sd_5htt_other_curr_5yr_ya)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_OtherThanChildMal_YAdult_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD_5yr","DD_curr_5yr")
  models<-"~ female + age + birth_decade + life_stress_exp_5yr_curr"
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_5yr","DD_curr_5yr")
  models<-"~ female + age + birth_decade + life_stress_quant_5yr_curr"
  LogR_DDQS.call(outcomes, models,"holder")   	  


 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_5yr","QD_curr_5yr","QDz_5yr","QDz_curr_5yr") else
  outcomes <- c("QDz_5yr","QDz_curr_5yr")
  models<-"~ female + age + birth_decade + life_stress_exp_5yr_curr"
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_5yr","QD_curr_5yr","QDz_5yr","QDz_curr_5yr") else
  outcomes <- c("QDz_5yr","QDz_curr_5yr")
  models<-"~ female + age + birth_decade + life_stress_quant_5yr_curr"
  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_other_curr_5yr_ya)
}

cat("=== END of young adult models, non CM stress plus covariates, 5yr time limit", "\n")






 ############################# 
 # stress association models #
 #############################
cat("=== START of young adult models, combined stress plus covariates", "\n")
cat("===== run young adult models, combined stress plus covariates, no time limit", "\n\n")

if( if10(sd_5htt_combined_ya_life[,"female"]) & if10(sd_5htt_combined_ya_life[,"age"]) ) {  
  attach(sd_5htt_combined_ya_life)
  
  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_Combined_YAdult_no_tlimit_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD_life")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean"	
  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_life")
  models<-"~ female + age + birth_decade + stress_combined_quant"	
#  LogR_DDQS.call(outcomes, models,"holder")    	      	


 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life","QDz_life") else
  outcomes <- c("QDz_life")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean"	
  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy"	
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life","QDz_life") else
  outcomes <- c("QDz_life")
  models<-"~ female + age + birth_decade + stress_combined_quant"	
#  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_combined_ya_life)
}


if( if10(sd_5htt_combined_ya_curr[,"female"]) & if10(sd_5htt_combined_ya_curr[,"age"]) ) {  
  attach(sd_5htt_combined_ya_curr)
  
  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_Combined_YAdult_no_tlimit_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD","DD_curr")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean"	
  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD","DD_curr")
  models<-"~ female + age + birth_decade + stress_combined_quant"	
#  LogR_DDQS.call(outcomes, models,"holder")    	      	


 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD","QD_curr","QDz","QDz_curr") else
  outcomes <- c("QDz","QDz_curr")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean"	
  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy"	
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD","QD_curr","QDz","QDz_curr") else
  outcomes <- c("QDz","QDz_curr")
  models<-"~ female + age + birth_decade + stress_combined_quant"	
#  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_combined_ya_curr)
}
cat("=== END of young adult models, combined stress plus covariates, no time limit", "\n")



 ############################# 
 # stress association models #
 ##############################

cat("=== START of young adult models, combined stress plus covariates, time ordered", "\n")

if( if10(sd_5htt_combined_ya_life[,"female"]) & if10(sd_5htt_combined_ya_life[,"age"]) ) {  
  attach(sd_5htt_combined_ya_life)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_Combined_YAdult_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD_life_before")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean"	
  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_life_before")
  models<-"~ female + age + birth_decade + stress_combined_quant"	
#  LogR_DDQS.call(outcomes, models,"holder")

 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life_before","QDz_life_before") else
  outcomes <- c("QDz_life_before")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean"	
  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy"	
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life_before","QDz_life_before") else
  outcomes <- c("QDz_life_before")
  models<-"~ female + age + birth_decade + stress_combined_quant"	
#  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_combined_ya_life)
}

if( if10(sd_5htt_combined_ya_curr[,"female"]) & if10(sd_5htt_combined_ya_curr[,"age"]) ) {  
  attach(sd_5htt_combined_ya_curr)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_Combined_YAdult_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD_before","DD_curr_before")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean"	
  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_before","DD_curr_before")
  models<-"~ female + age + birth_decade + stress_combined_quant"	
#  LogR_DDQS.call(outcomes, models,"holder")

 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_before","QD_curr_before","QDz_before","QDz_curr_before") else
  outcomes <- c("QDz_before","QDz_curr_before")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean"	
  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy"	
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_before","QD_curr_before","QDz_before","QDz_curr_before") else
  outcomes <- c("QDz_before","QDz_curr_before")
  models<-"~ female + age + birth_decade + stress_combined_quant"	
#  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_combined_ya_curr)
}

cat("=== END of young adult models, combined stress plus covariates, time ordered", "\n")


 ############################# 
 # stress association models #
 ##############################

cat("=== START of young adult models, combined stress plus covariates, 5yr time limit", "\n")
if( if10(sd_5htt_combined_ya_life[,"female"]) & if10(sd_5htt_combined_ya_life[,"age"]) ) {  
  attach(sd_5htt_combined_ya_life)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_Combined_YAdult_"

  both_sexes<-two_level_present("female")

 # logistic regression models #
 ##############################

  outcomes <- c("DD_life_5yr")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean_5yr_life"	
  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy_5yr_life"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_life_5yr")
  models<-"~ female + age + birth_decade + stress_combined_quant_5yr_life"	
#  LogR_DDQS.call(outcomes, models,"holder")
  
  
 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life_5yr","QDz_life_5yr") else
  outcomes <- c("QDz_life_5yr")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean_5yr_life"	
  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy_5yr_life"	
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT")
    outcomes <- c("QD_life_5yr","QDz_life_5yr") else
  outcomes <- c("QDz_life_5yr")
  models<-"~ female + age + birth_decade + stress_combined_quant_5yr_life"	
#  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_combined_ya_life)
}


if( if10(sd_5htt_combined_ya_curr[,"female"]) & if10(sd_5htt_combined_ya_curr[,"age"]) ) {  
  attach(sd_5htt_combined_ya_curr)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_Combined_YAdult_"

  both_sexes<-two_level_present("female")

 # logistic regression models #
 ##############################

  outcomes <- c("DD_5yr","DD_curr_5yr")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean_5yr_curr"	
  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy_5yr_curr"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_5yr","DD_curr_5yr")
  models<-"~ female + age + birth_decade + stress_combined_quant_5yr_curr"	
#  LogR_DDQS.call(outcomes, models,"holder")
  
 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_5yr","QD_curr_5yr","QDz_5yr","QDz_curr_5yr") else
  outcomes <- c("QDz_5yr","QDz_curr_5yr")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean_5yr_curr"	
  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy_5yr_curr"	
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT")
      outcomes <- c("QD_5yr","QD_curr_5yr","QDz_5yr","QDz_curr_5yr") else
  outcomes <- c("QDz_5yr","QDz_curr_5yr")
  models<-"~ female + age + birth_decade + stress_combined_quant_5yr_curr"	
#  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_combined_ya_curr)
}

cat("=== END of young adult models, combined stress plus covariates, 5yr time limit", "\n")

cat("=== END of stress plus covars models in young adult age group", "\n")
