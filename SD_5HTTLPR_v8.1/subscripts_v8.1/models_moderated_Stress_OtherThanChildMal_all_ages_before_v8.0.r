###########################################
# 5HTTLPR*stress=depression Models
# all age-inclusive sample, non-childhood-
# maltreatment stressors, timing stress
# precedes depression requirement 
#
# version8.0
#
# created by Amy Horton, Younghun Han, Chris Amos,
#            Sarah Hartz, and Rob Culverhouse
#
###########################################

cat("===== run all ages, non-childhood maltreatment stress, timing ordered models", "\n\n")

###### test if sex,age variables defined for >10% of dataset, test dataset descriptors #######  
if(if10(sd_5htt_other_life[,"female"]) & if10(sd_5htt_other_life[,"age"]) ) {  
 
  attach(sd_5htt_other_life) ###### make dataframe part of search list
 ############################## 
 # one SNP association models #
 ##############################
  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_SNPxStress_OtherThanChildMal_AllAges_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD_life_before")
  models<- "~ female + age + birth_decade + SNP + life_stress_exp"	
  ModLogR_DDDS.call(outcomes, models, "SNP")
  
  outcomes <- c("DD_life_before")
  models<- "~ female + age + birth_decade + SNP + life_stress_quant"	
  ModLogR_DDQS.call(outcomes, models, "SNP")  

 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life_before","QDz_life_before") else
  outcomes <- c("QDz_life_before")
  models<-"~ female + age + birth_decade + SNP + life_stress_exp"	
  ModLinR_QDDS.call(outcomes, models, "SNP")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life_before","QDz_life_before") else
  outcomes <- c("QDz_life_before")
  models<- "~ female + age + birth_decade + SNP + life_stress_quant"	
  ModLinR_QDQS.call(outcomes, models, "SNP")

  
  detach(sd_5htt_other_life)
} 

if(if10(sd_5htt_other_curr[,"female"]) & if10(sd_5htt_other_curr[,"age"]) ) {  
 
  attach(sd_5htt_other_curr) ###### make dataframe part of search list
 ############################## 
 # one SNP association models #
 ##############################
  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_SNPxStress_OtherThanChildMal_AllAges_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD_before","DD_curr_before")
  models<- "~ female + age + birth_decade + SNP + life_stress_exp"	
  ModLogR_DDDS.call(outcomes, models, "SNP")
  
  outcomes <- c("DD_before","DD_curr_before")
  models<- "~ female + age + birth_decade + SNP + life_stress_quant"	
  ModLogR_DDQS.call(outcomes, models, "SNP")  

 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_before","QD_curr_before","QDz_before","QDz_curr_before") else
  outcomes <- c("QDz_before","QDz_curr_before")
  models<-"~ female + age + birth_decade + SNP + life_stress_exp"	
  ModLinR_QDDS.call(outcomes, models, "SNP")
  
  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_before","QD_curr_before","QDz_before","QDz_curr_before") else
  outcomes <- c("QDz_before","QDz_curr_before")
  models<- "~ female + age + birth_decade + SNP + life_stress_quant"	
  ModLinR_QDQS.call(outcomes, models, "SNP")

  
  detach(sd_5htt_other_curr)
} 

cat("=== END of non-childhood maltreatment stress age unrestricted models, timing ordered models", "\n")

