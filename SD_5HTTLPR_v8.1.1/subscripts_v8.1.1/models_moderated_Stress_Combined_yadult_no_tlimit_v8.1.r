###########################################
# 5HTTLPR*stress=depression Models
# all-age inclusive, no time restriction on
# combined stressors 
#
# version8.1
#
# created by Amy Horton, Younghun Han, Chris Amos,
#            Sarah Hartz, and Rob Culverhouse
#
###########################################

#cat("=== run moderated regression models ", "\n" )

###### test if sex,age variables defined for >10% of dataset, test dataset descriptors #######  
cat("=== START of combined lifetime stress ages 21-30 models, no time restrictions", "\n")
if( if10(sd_5htt_combined_ya_life[,"female"]) & if10(sd_5htt_combined_ya_life[,"age"]) ) {  
 
  attach(sd_5htt_combined_ya_life) ###### make dataframe part of search list
 ############################## 
 # one SNP association models #
 ##############################
  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_SNPxStress_Combined_YAdult_no_tlimit_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD_life")
##  models<-"~ female + age + birth_decade + SNP + stress_combined_exp"	
##  ModLogR_DDDS.call(outcomes, models, "SNP")
  models<-"~ female + age + birth_decade + SNP + stress_combined_exp_clean"	
  ModLogR_DDDS.call(outcomes, models, "SNP")
  models<-"~ female + age + birth_decade + SNP + stress_combined_exp_messy"	
  ModLogR_DDDS.call(outcomes, models, "SNP")

  
  outcomes <- c("DD_life")
  models<-"~ female + age + birth_decade + SNP + stress_combined_quant"
#  ModLogR_DDQS.call(outcomes, models, "SNP")


 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life","QDz_life") else
  outcomes <- c("QDz_life")

##  models<- "~ female + age + birth_decade + SNP + stress_combined_exp"	
##  ModLinR_QDDS.call(outcomes, models, "SNP")
  models<- "~ female + age + birth_decade + SNP + stress_combined_exp_clean"	
  ModLinR_QDDS.call(outcomes, models, "SNP")
  models<- "~ female + age + birth_decade + SNP + stress_combined_exp_messy"	
  ModLinR_QDDS.call(outcomes, models, "SNP")
  


  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD_life","QDz_life") else
  outcomes <- c("QDz_life")

  models<-"~ female + age + birth_decade + SNP + stress_combined_quant"
#  ModLinR_QDQS.call(outcomes, models, "SNP")
  
  detach(sd_5htt_combined_ya_life)
}

if( if10(sd_5htt_combined_ya_curr[,"female"]) & if10(sd_5htt_combined_ya_curr[,"age"]) ) {  
 
  attach(sd_5htt_combined_ya_curr) ###### make dataframe part of search list
 ############################## 
 # one SNP association models #
 ##############################
  snplist <- if(genotype_SL=="YES") if(genotype_rs25531=="YES") c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else c("add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http") else if(genotype_rs25531=="YES") c("add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531") else NA

  nsnps <- length(snplist)

  file_label <- "_SNPxStress_Combined_YAdult_no_tlimit_"

  both_sexes<-two_level_present("female")
  
 # logistic regression models #
 ##############################

  outcomes <- c("DD","DD_curr")
##  models<-"~ female + age + birth_decade + SNP + stress_combined_exp"	
##  ModLogR_DDDS.call(outcomes, models, "SNP")
  models<-"~ female + age + birth_decade + SNP + stress_combined_exp_clean"	
  ModLogR_DDDS.call(outcomes, models, "SNP")
  models<-"~ female + age + birth_decade + SNP + stress_combined_exp_messy"	
  ModLogR_DDDS.call(outcomes, models, "SNP")

  
  outcomes <- c("DD","DD_curr")
  models<-"~ female + age + birth_decade + SNP + stress_combined_quant"
#  ModLogR_DDQS.call(outcomes, models, "SNP")


 # linear regression models #
 ############################                  

  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD","QD_curr","QDz","QDz_curr") else
  outcomes <- c("QDz","QDz_curr")

##  models<- "~ female + age + birth_decade + SNP + stress_combined_exp"	
##  ModLinR_QDDS.call(outcomes, models, "SNP")
  models<- "~ female + age + birth_decade + SNP + stress_combined_exp_clean"	
  ModLinR_QDDS.call(outcomes, models, "SNP")
  models<- "~ female + age + birth_decade + SNP + stress_combined_exp_messy"	
  ModLinR_QDDS.call(outcomes, models, "SNP")
  


  if(dep_q_system=="DSM4_SYMPTOM_COUNT") 
    outcomes <- c("QD","QD_curr","QDz","QDz_curr") else
  outcomes <- c("QDz","QDz_curr")

  models<-"~ female + age + birth_decade + SNP + stress_combined_quant"
#  ModLinR_QDQS.call(outcomes, models, "SNP")
  
  detach(sd_5htt_combined_ya_curr)
} 
cat("=== END of combined lifetime stress ages 21-30 models no time restriction", "\n")
#cat("=== END of moderated stress models", "\n")

