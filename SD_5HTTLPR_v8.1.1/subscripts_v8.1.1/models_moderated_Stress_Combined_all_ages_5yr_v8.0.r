###########################################
# 5HTTLPR*stress=depression Models
# all ages-inclusive sample, combined
# stressor definition, stressor in <5 years
# preceding depression unless CM
#
# version8.0
#
# created by Amy Horton, Younghun Han, Chris Amos,
#            Sarah Hartz, and Rob Culverhouse
#
###########################################

###### test if sex,age variables defined for >10% of dataset, test dataset descriptors #######  
cat("===== run unrestricted ages, combined stress, timing 5yr models", "\n\n")
if( if10(sd_5htt_combined[,"female"]) & if10(sd_5htt_combined[,"age"]) ) {  
 
  attach(sd_5htt_combined) ###### make dataframe part of search list
 ############################## 
 # one SNP association models #
 ##############################
  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_SNPxStress_Combined_AllAges_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################
	    
  outcomes <- c("DD_life_5yr")
  models<-"~ female + age + birth_decade + SNP + stress_combined_exp_clean_5yr_life"	
  ModLogR_DDDS.call(outcomes, models, "SNP")
  models<-"~ female + age + birth_decade + SNP + stress_combined_exp_messy_5yr_life"	
  ModLogR_DDDS.call(outcomes, models, "SNP")
  
  outcomes <- c("DD_5yr","DD_curr_5yr")
  models<-"~ female + age + birth_decade + SNP + stress_combined_exp_clean_5yr_curr"	
  ModLogR_DDDS.call(outcomes, models, "SNP")
  models<-"~ female + age + birth_decade + SNP + stress_combined_exp_messy_5yr_curr"	
  ModLogR_DDDS.call(outcomes, models, "SNP")
  
  outcomes <- c("DD_life_5yr")
  models<- "~ female + age + birth_decade + SNP + stress_combined_quant_5yr_life"	  
#  ModLogR_DDQS.call(outcomes, models, "SNP")

  outcomes <- c("DD_5yr","DD_curr_5yr")
  models<- "~ female + age + birth_decade + SNP + stress_combined_quant_5yr_curr"	  
#  ModLogR_DDQS.call(outcomes, models, "SNP")

 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life_5yr","QDz_life_5yr") else
  outcomes <- c("QDz_life_5yr")
  models<-"~ female + age + birth_decade + SNP + stress_combined_exp_clean_5yr_life"	
  ModLinR_QDDS.call(outcomes, models, "SNP")
  models<-"~ female + age + birth_decade + SNP + stress_combined_exp_messy_5yr_life"	
  ModLinR_QDDS.call(outcomes, models, "SNP")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_5yr","QD_curr_5yr","QDz_5yr","QDz_curr_5yr") else
  outcomes <- c("QDz_5yr","QDz_curr_5yr")
  models<-"~ female + age + birth_decade + SNP + stress_combined_exp_clean_5yr_curr"	
  ModLinR_QDDS.call(outcomes, models, "SNP")
  models<-"~ female + age + birth_decade + SNP + stress_combined_exp_messy_5yr_curr"	
  ModLinR_QDDS.call(outcomes, models, "SNP")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life_5yr","QDz_life_5yr") else
  outcomes <- c("QDz_life_5yr")
  models<-"~ female + age + birth_decade + SNP + stress_combined_quant_5yr_life"	
#  ModLinR_QDQS.call(outcomes, models, "SNP")

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_5yr","QD_curr_5yr","QDz_5yr","QDz_curr_5yr") else
  outcomes <- c("QDz_5yr","QDz_curr_5yr")
  models<-"~ female + age + birth_decade + SNP + stress_combined_quant_5yr_curr"	
#  ModLinR_QDQS.call(outcomes, models, "SNP")

  detach(sd_5htt_combined)
} 

cat("=== END of combined stress unrestricted ages, timing 5yr models", "\n")


