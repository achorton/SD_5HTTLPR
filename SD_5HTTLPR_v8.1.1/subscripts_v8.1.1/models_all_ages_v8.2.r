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

###### test if sex,age variables defined for >10% of dataset, test dataset descriptors #######  
if( if10(sd_5htt[,"female"]) & if10(sd_5htt[,"age"]) ) {  

  attach(sd_5htt) ###### make dataframe part of search list
 #################### 
 # covariate models #
 ####################
  cat("=== START of combined age unrestricted models, covariates only", "\n")
  snplist <- "placeholder"
  nsnps <- length(snplist)

  cat("===== run all ages models", "\n\n")
  file_label <- "_Covar_AllAges_"
  
  both_sexes<-two_level_present("female")
    
 # logistic regression models #
 ##############################

  outcomes <- c("dep_dx","dep_dx_curr","dep_dx_life")
  models<-"~ female + age + birth_decade"	
  LogR_DDcovar.call(outcomes,models,"holder")

  
 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("dep_q","dep_quant_curr","dep_quant_life","dep_q_z","dep_quant_curr_z","dep_quant_life_z") else
  outcomes <- c("dep_q_z","dep_quant_curr_z","dep_quant_life_z")
  models<- "~ female + age + birth_decade"	
  LinR_QDcovar.call(outcomes,models,"holder")

  cat("=== END of combined age unrestricted models, covariates only models", "\n")



 ########################## 
 # SNP association models #
 ##########################
  cat("=== START of combined age unrestricted models, SNP plus covariates", "\n")
  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  cat("===== run all ages models", "\n\n")
  file_label <- "_SNP_AllAges_"

  both_sexes<-two_level_present("female")
    
 # logistic regression models #
 ##############################

  outcomes <- c("dep_dx","dep_dx_curr","dep_dx_life")
  models<- "~ female + age + birth_decade + SNP"	
  LogR_DD_SNP.call(outcomes,models,"holder")  	      

  
 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("dep_q","dep_quant_curr","dep_quant_life","dep_q_z","dep_quant_curr_z","dep_quant_life_z") else
  outcomes <- c("dep_q_z","dep_quant_curr_z","dep_quant_life_z")
  models<-"~ female + age + birth_decade + SNP"	
  LinR_QD_SNP.call(outcomes,models,"holder")  	      

  
  cat("=== END of combined age unrestricted models, SNP plus covariates models", "\n")
  detach(sd_5htt)
}




 ############################################# 
 # childhood maltreatment association models #
 #############################################
if( if10(sd_5htt[,"female"]) & if10(sd_5htt[,"age"]) ) {  

  attach(sd_5htt)

  cat("=== START of age unrestricted models, childhood maltreatment plus covariates", "\n")
  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_ChildMal_AllAges_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD","DD_curr","DD_life")
  models<-"~ female + age + birth_decade + child_mal_exp"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD","DD_curr","DD_life")
  models<- "~ female + age + birth_decade + child_mal_quant"				 
  LogR_DDQS.call(outcomes, models,"holder")
  

 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT")
    outcomes <- c("QD","QD_curr","QD_life","QDz","QDz_curr","QDz_life") else
  outcomes <- c("QDz","QDz_curr","QDz_life")
  models<-"~ female + age + birth_decade + child_mal_exp"	
  LinR_QDDS.call(outcomes, models,"holder")

  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT")
    outcomes <- c("QD","QD_curr","QD_life","QDz","QDz_curr","QDz_life") else
  outcomes <- c("QDz","QDz_curr","QDz_life")
  models<-"~ female + age + birth_decade + child_mal_quant"	
  LinR_QDQS.call(outcomes, models,"holder")


  detach(sd_5htt)
  cat("=== END of all ages models, childhood maltreatment plus covariates", "\n")
}







 #################################### 
 # non CM stress association models #
 ####################################
cat("=== START of age unrestricted models, other than childhood maltreatment stress plus covariates, no time limit", "\n")
if( if10(sd_5htt[,"female"]) & if10(sd_5htt[,"age"]) ) {  

  attach(sd_5htt)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_OtherThanChildMal_AllAges_no_tlimit_"

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

  detach(sd_5htt)
}

if( if10(sd_5htt[,"female"]) & if10(sd_5htt[,"age"]) ) {  

  attach(sd_5htt)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_OtherThanChildMal_AllAges_no_tlimit_"

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

  detach(sd_5htt)
}

cat("=== END of all ages models, non-CM stress plus covariates, no time limit", "\n")




 #################################### 
 # non CM stress association models #
 ####################################
cat("=== START of age unrestricted models, other than childhood maltreatment stress plus covariates, time ordered", "\n")
if( if10(sd_5htt_other_life[,"female"]) & if10(sd_5htt_other_life[,"age"]) ) {  

  attach(sd_5htt_other_life)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_OtherThanChildMal_AllAges_"

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

  detach(sd_5htt_other_life)
}

if( if10(sd_5htt_other_curr[,"female"]) & if10(sd_5htt_other_curr[,"age"]) ) {  

  attach(sd_5htt_other_curr)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_OtherThanChildMal_AllAges_"

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

  detach(sd_5htt_other_curr)
}

cat("=== END of all ages models, non-childhood maltreatment stress plus covariates, time ordered", "\n")




 #################################### 
 # non CM stress association models #
 ####################################
cat("=== START of age unrestricted models, other than childhood maltreatment stress plus covariates, 5yr time limit", "\n")
if( if10(sd_5htt_other_life_5yr[,"female"]) & if10(sd_5htt_other_life_5yr[,"age"]) ) {  

  attach(sd_5htt_other_life_5yr)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_OtherThanChildMal_AllAges_"

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

  detach(sd_5htt_other_life_5yr)
}

if( if10(sd_5htt_other_curr_5yr[,"female"]) & if10(sd_5htt_other_curr_5yr[,"age"]) ) {  

  attach(sd_5htt_other_curr_5yr)

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_OtherThanChildMal_AllAges_"

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

  detach(sd_5htt_other_curr_5yr)
}

cat("=== END of all ages models, non CM stress plus covariates, 5yr time limit", "\n")



 ############################# 
 # stress association models #
 #############################
if( if10(sd_5htt_combined[,"female"]) & if10(sd_5htt_combined[,"age"]) ) {  

  attach(sd_5htt_combined)
  
  cat("=== START of age unrestricted models, combined stress plus covariates", "\n")

  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  cat("===== run all ages models, combined stress plus covariates, no time limit", "\n\n")
  file_label <- "_Stress_Combined_AllAges_no_tlimit_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD","DD_curr","DD_life")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean"	
  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD","DD_curr","DD_life")
  models<-"~ female + age + birth_decade + stress_combined_quant"	
#  LogR_DDQS.call(outcomes, models,"holder")    	      	


 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD","QD_curr","QD_life","QDz","QDz_curr","QDz_life") else
  outcomes <- c("QDz","QDz_curr","QDz_life")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean"	
  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy"	
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD","QD_curr","QD_life","QDz","QDz_curr","QDz_life") else
  outcomes <- c("QDz","QDz_curr","QDz_life")
  models<-"~ female + age + birth_decade + stress_combined_quant"	
#  LinR_QDQS.call(outcomes, models,"holder")

  cat("=== END of all ages models, combined stress plus covariates, no time limit", "\n")
  detach(sd_5htt_combined)
}







 ############################# 
 # stress association models #
 ##############################

if( if10(sd_5htt_combined[,"female"]) & if10(sd_5htt_combined[,"age"]) ) {  

  attach(sd_5htt_combined)

  cat("=== START of age unrestricted models, combined stress plus covariates, time ordered", "\n")
  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_Combined_AllAges_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD_before","DD_curr_before","DD_life_before")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean"	
  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_before","DD_curr_before","DD_life_before")
  models<-"~ female + age + birth_decade + stress_combined_quant"	
#  LogR_DDQS.call(outcomes, models,"holder")

 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_before","QD_curr_before","QD_life_before","QDz_before","QDz_curr_before","QDz_life_before") else
  outcomes <- c("QDz_before","QDz_curr_before","QDz_life_before")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean"	
  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy"	
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_before","QD_curr_before","QD_life_before","QDz_before","QDz_curr_before","QDz_life_before") else
  outcomes <- c("QDz_before","QDz_curr_before","QDz_life_before")
  models<-"~ female + age + birth_decade + stress_combined_quant"	
#  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_combined)
  cat("=== END of all ages models, combined stress plus covariates, time ordered", "\n")
}





 ############################# 
 # stress association models #
 ##############################

if( if10(sd_5htt_combined[,"female"]) & if10(sd_5htt_combined[,"age"]) ) {  

  attach(sd_5htt_combined)

  cat("=== START of age unrestricted models, combined stress plus covariates, 5yr time limit", "\n")
  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_Stress_Combined_AllAges_"

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
  
  outcomes <- c("DD_5yr","DD_curr_5yr")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean_5yr_curr"	
  LogR_DDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy_5yr_curr"	
  LogR_DDDS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_life_5yr")
  models<-"~ female + age + birth_decade + stress_combined_quant_5yr_life"	
#  LogR_DDQS.call(outcomes, models,"holder")
  
  outcomes <- c("DD_5yr","DD_curr_5yr")
  models<-"~ female + age + birth_decade + stress_combined_quant_5yr_curr"	
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
    outcomes <- c("QD_5yr","QD_curr_5yr","QDz_5yr","QDz_curr_5yr") else
  outcomes <- c("QDz_5yr","QDz_curr_5yr")
#  models<-"~ female + age + birth_decade + stress_combined_exp"	
#  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_clean_5yr_curr"	
  LinR_QDDS.call(outcomes, models,"holder")
  models<-"~ female + age + birth_decade + stress_combined_exp_messy_5yr_curr"	
  LinR_QDDS.call(outcomes, models,"holder")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT")
    outcomes <- c("QD_life_5yr","QDz_life_5yr") else
  outcomes <- c("QDz_life_5yr")
  models<-"~ female + age + birth_decade + stress_combined_quant_5yr_life"	
#  LinR_QDQS.call(outcomes, models,"holder")

  if(dep_q_system=="DSM4_SYMPTOM_COUNT")
      outcomes <- c("QD_5yr","QD_curr_5yr","QDz_5yr","QDz_curr_5yr") else
  outcomes <- c("QDz_5yr","QDz_curr_5yr")
  models<-"~ female + age + birth_decade + stress_combined_quant_5yr_curr"	
#  LinR_QDQS.call(outcomes, models,"holder")

  detach(sd_5htt_combined)
  cat("=== END of all ages models, combined stress plus covariates, 5yr time limit", "\n")
}


cat("=== END of stress plus covars models in all-inclusive age group", "\n")
