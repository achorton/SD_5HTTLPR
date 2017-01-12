##############################################################################
##### 	CONFIDENTIAL, FOR USE ONLY BY SD-5HTTLPR PARTICIPANTS	##############
##############################################################################
# 5-HTTLPR GxE analysis
# version 8.1.1.1.extension.2.r  01/25/2016
##############################################################################

######################################################################
###      User-input Section 1. FILE NAMING AND LOCATIONS            ##
###                                                                 ##
###    Please fill-in the following 4 pieces of information about   ##
###   your data file, its location, and the location of the script  ##
######################################################################


#  1.1.  What is the path for the directory containing the dataset and script?  
#        Examples:
#	   On a Mac/Unix/Linux system: "/Users/achorton/SD_5HTTLPR_v8.1.1.extension/"   
#          On a MS Windows system: "C:/Project/SD_5HTTLPR_v8.1.1.extension/"
#            NOTE: direction of slashes is different from MS-Windows convention
#
#        NOTE: There must be a terminal '/'

         INDIR="/Users/LucyGedara/Desktop/SD_5HTTLPR_v8.1.1/"
#         INDIR="/home/amyh/SD_5HTTLPR/SD_5HTTLPR_v8.1.1.extension/"


#  1.2.  Enter study name (LABEL will appear in meta-analysis files).

         LABEL = "ALSPAC_extended"
#         LABEL = "FAKE_extended"


#  1.3.  Choose ancestral POPULATION of dataset using the following codes:
#        African ancestry             -> AF
#        African and European admixed -> AE
#        Asian ancestry               -> AS
#        European ancestry            -> EU
#        Pacific Islander ancestry    -> PI
#        Other ancestry               -> OT(specify)

         POPULATION = "EU"


#  1.4.  What is the name of the formatted dataset created for this analysis?
#
#        NOTE: This must be a CSV (comma delimited) file.

         INDATA="UpdateLE_MA8.csv"
#         INDATA="FAKE2.csv"



#####################################################
### User-input Section 2. DATASET INFORMATION      ##
#####################################################

################################################################################
#
#  Please provide values for the following 18 questions describing your data.
#  These values will be used for stratification of meta-analyses.
#  Instructions are provided in commented lines beginning "#".
# 
#  Each variable is currently filled with a default values.
#
#  If the default does not match you data, please replace the text
#  inside the quote marks with the choice matching your data.       
#                                                                               
################################################################################
#
#      Note 1: The 18 responses in this section must be in ALL CAPS
#
#      Note 2: If your response is  OTHER(specify), please include OTHER and a
#               description in your response, e.g. "OTHER(LAST_18MONTHS)"        
#
################################################################################

# 2.1.  Are your data from a LONGITUDINAL study?
#       Choose from: NO/YES

        longitudinal_data = "YES"  


##################################
####                          ####
#### ASSESSMENT of DEPRESSION ####
####                          ####
##################################

# 2.2.  Source of information on DEPRESSION:
#       Choose from: SELF/PARENT/OTHER(specify)  

        reporting_dep = "SELF"  

#########################################
## Quantitative measures of depression ##
#########################################

# 2.3.  QUANTITATIVE depression measure: 
#       Choose from: DSM4_SYMPTOM_COUNT/ICD10_SYMPTOM_COUNT/
#                    OTHER(specify)/NOT_AVAILABLE
#
#       Please use "DSM4_SYMPTOM_COUNT" if available.

        dep_q_system = "ICD10_SYMPTOM_COUNT"   


# 2.4.  Are the QUANTITATIVE DEPRESSION data based on an IN-PERSON INTERVIEW?
#       Choose from: IN_PERSON/TELEPHONE/MAIL_QUESTIONNAIRE/OTHER(specify)

        dep_q_interview = "IN_PERSON" 


# 2.5.  If CURRENT QUANTITATIVE DEPRESSION was assessed, what period was queried?
#       Choose from: NOT_AVAILABLE/LAST_6MONTHS/LAST_YEAR/OTHER(specify)

        dep_q_query_span = "LAST_6MONTHS"  


#######################################
## Dichotomous measure of depression ##
#######################################

# 2.6.  Depression DIAGNOSIS system: 
#       Choose from: NONE/DSM4/ICD10/OTHER(specify)/NOT_AVAILABLE
#
#       Please provide diagnosis based on DSM4, if possible.

        dep_dx_system = "ICD10"   


# 2.7.  Is the DEPRESSION DIAGNOSIS based on an IN-PERSON INTERVIEW?
#       Choose from: IN_PERSON/TELEPHONE/MAIL_QUESTIONNAIRE/OTHER(specify)/NOT_AVAILABLE

        dep_dx_interview = "IN_PERSON" 


# 2.8.  What period of time was covered in the assessment of
#       CURRENT DEPRESSION DIAGNOSIS?
#       Choose from: NOT_AVAILABLE/LAST_6MONTHS/LAST_YEAR/OTHER(specify)

        dep_dx_query_span= "LAST_6MONTHS"  


##############################
####                      ####
#### ASSESSMENT of STRESS ####
####                      ####
##############################

# 2.9.  Source of the CHILDHOOD MALTREATMENT information for the subject?
#       Choose from: SELF/PARENT/OTHER(specify)/NOT_AVAILABLE

        reporting_childhood_mal = "PARENT"


# 2.10. Source of information on LIFE STRESS (stress other than
#       CHILDHOOD MALTREATMENT)?
#       Choose from: SELF/PARENT/OTHER(specify)/NOT_AVAILABLE

        reporting_life_stress = "PARENT"


# 2.11. What time period was covered in the LIFE STRESS (non-childhood
#       maltreatment) assessment?
#       Choose from: LAST_WEEK/LAST_MONTH/LAST_6MONTHS/LAST_YEAR/
#                    LAST_5YEARS/LIFETIME/OTHER(specify)/NOT_AVAILABLE

        life_stress_time_period = "OTHER(LAST_16YEARS)"

#####################################
## Quantitative measures of stress ##
#####################################

# 2.12. System for evaluating QUANTITATIVE Childhood Maltreatment:
#       Choose from: CTQ/TRAUMA_COUNT/OTHER(specify)/NOT_AVAILABLE
#
#       Please use CTQ, if possible.
#       If TRAUMA_COUNT, please send list of questions used.

        child_mal_q_system = "NOT_AVAILABLE"


# 2.13. System for evaluating QUANTITATIVE LIFE STRESS (non-childhood maltreatment): 
#       Choose from: LTE_Q/TRAUMA_COUNT/OTHER(specify)/NOT_AVAILABLE
#
#       Please use LTE_Q, if possible.
#       If TRAUMA_COUNT, please send list of questions used.

        life_stress_q_system = "OTHER(MODIFIED_LEQ)"


# 2.14. Are the QUANTITATIVE LIFE STRESS (non-childhood maltreatment) data based
#       on an IN-PERSON INTERVIEW? 
#       Choose from: IN_PERSON/TELEPHONE/MAIL_QUESTIONNAIRE/OTHER(specify) 

        interview_stress_q = "MAIL_QUESTIONNAIRE"

####################################
## Dichotomous measures of stress ##
####################################

# 2.15. System for evaluating Childhood maltreatment EXPOSURE:
#       Choose from: CTQ/OTHER(specify)/NOT_AVAILABLE  
#
#       Please use CTQ, if possible.

        child_mal_exp_system = "OTHER(MODIFIED_LEQ)"


# 2.16. System for evaluating stress EXPOSURE other than childhood maltreatment
#       Choose from: LTE_Q/OTHER(specify)/NOT_AVAILABLE        
#
#       Please use LTE_Q, if possible.

        life_stress_exp_system = "OTHER(MODIFIED_LEQ)"


# 2.17. Are the LIFE STRESS EXPOSURE data based on an IN-PERSON INTERVIEW?
#       Choose from: IN_PERSON/TELEPHONE/MAIL_QUESTIONNAIRE/OTHER(specify)

        interview_stress_exp = "MAIL_QUESTIONNAIRE"



##############################
####                      ####
#### PHASING OF GENOTYPES ####
####                      ####
##############################

# 2.18. If the study contains genotyping information for rs25531, are 
#       PHASED haplotypes provided?
#       Choose from: PHASED/UNPHASED/NOT_AVAILABLE

        haplotypes_available = "UNPHASED"




#########################################################
#############                            ################
#############      END OF USER INPUT     ################
#############                            ################
#########################################################

flag_long_dx=TRUE
flag_long_q=TRUE
flag_A_freq=TRUE
options(warn = 1)

install.packages(c("rms"),repos="http://cran.wustl.edu")
library(rms)

SITE <- paste(LABEL,substr(POPULATION,1,2),sep="_")
OUTDIR <- paste(INDIR,"Results_",SITE,"/",sep="")
dir.create(OUTDIR)
unlink(paste(OUTDIR,"*.*", sep=""))
out.file<-paste(OUTDIR,SITE,"_frequencies.csv",sep="")
gz1<-gzfile(paste(out.file,".gz",sep=""), "w")

#write('START:', file=gz1,append=TRUE)
#write('START:', file=gz1,append=TRUE)
#close(gz1)

var.level.check.SNP <- function(ntmp_idx,var.level.flag.vector,model1,tmp) {
  outcomes_f <- as.character(model1[2])
  ntmp<-length(var.level.flag.vector) + 1
#  ntmp<-length(attr(tmp,"variables"))
#  for(i in 2:max(2,ntmp)) {  
  for(i in 2:ntmp_idx) {
    if((length(grep("L_Adum2_rs25531",as.character(attr(tmp,"variables")[[i]]))) == 0) &
       (length(grep("Ldum2_5http",as.character(attr(tmp,"variables")[[i]]))) == 0) &
       var.level.flag.vector[i-1] ) {                
      running.var<-c(rep(1,length(get(outcomes_f))))
      for(j in 2:max(2,(i-1))) {
        if(var.level.flag.vector[j-1]) {
          running.var<-running.var*ifelse(is.na(get(as.character(attr(tmp,"variables")[[j]]))),NA,1)
        }
      }        
      running.var.include.new<-running.var*ifelse(is.na(get(as.character(attr(tmp,"variables")[[i]]))),NA,1)
      var.level<-length(levels(as.factor(ifelse(is.na(running.var.include.new*get(as.character(attr(tmp,"variables")[[i]]))),NA,get(as.character(attr(tmp,"variables")[[i]])) ))))     
        if((length(grep("L_Adum1_rs25531",as.character(attr(tmp,"variables")[[i]]))) > 0) |
           (length(grep("Ldum1_5http",as.character(attr(tmp,"variables")[[i]]))) > 0)) {
            if(var.level < 2) {
                if(length(levels(as.factor(get(as.character(attr(tmp,"variables")[[(i+1)]]))))) < 2) {
                    model1<-update(model1,paste(". ~ . -",
                                                as.character(attr(tmp,"variables")[[i]]),
                                                "-",as.character(attr(tmp,"variables")[[i+1]]),sep=" "))
                    var.level.flag.vector[i-1]<-0
                    var.level.flag.vector[i]<-0            
                }
            }           
          } else {
            if(var.level < 2) {
              model1<-update(model1,paste(". ~ . -",as.character(attr(tmp,"variables")[[i]]),sep=" "))
              var.level.flag.vector[i-1]<-0
            }
          }
      if((var.level >= 2) & (i > 2)) {
        current.level.flag.vector<-var.level.flag.vector
        for(k in 2:(i-1)) {
          if (var.level.flag.vector[k-1]) {
            temp.obj<-var.level.check.SNP(k,var.level.flag.vector,model1,tmp)
            model1<-temp.obj[[1]]
            tmp<-temp.obj[[2]]
            x<-numeric(0)
            for(l in 3:(ntmp + 1)) { 
              x<-append(x,temp.obj[[l]])
            }
            var.level.flag.vector<-x
          }
        }
        level.flag.diff<-1
        for(m in 1:length(var.level.flag.vector)) {
          if(current.level.flag.vector[m]==var.level.flag.vector[m]) (1*level.flag.diff) else 0
        }
        if(!(level.flag.diff)) {
          temp.obj<-var.level.check.SNP(i,var.level.flag.vector,model1,tmp)
          model1<-temp.obj[[1]]
          tmp<-temp.obj[[2]]
          x<-numeric(0)
          for(l in 3:(ntmp + 1)) { 
            x<-append(x,temp.obj[[l]])
          }
          var.level.flag.vector<-x
        }
      }
    }
  }
  return(c(model1,tmp,var.level.flag.vector))
}


var.level.check.STRESS <- function(ntmp_idx,var.level.flag.vector,model1,tmp) {
  outcomes_f <- as.character(model1[2])
  ntmp<-length(var.level.flag.vector) + 1
#  ntmp<-length(attr(tmp,"variables"))
#  for(i in 2:max(2,ntmp)) {  
  for(i in 2:max(2,ntmp_idx)) {  
    if((length(grep("L_Adum2_rs25531",as.character(attr(tmp,"variables")[[i]]))) == 0) &
       (length(grep("Ldum2_5http",as.character(attr(tmp,"variables")[[i]]))) == 0) &
       var.level.flag.vector[i-1] ) {                
      running.var<-c(rep(1,length(get(outcomes_f))))
      for(j in 2:max(2,i-1)) {
        if(var.level.flag.vector[j-1]) {
          running.var<-running.var*ifelse(is.na(get(as.character(attr(tmp,"variables")[[j]]))),NA,1)
        }
      }
      running.var.include.new<-running.var*ifelse(is.na(get(as.character(attr(tmp,"variables")[[i]]))),NA,1)
      var.level<-length(levels(as.factor(ifelse(is.na(running.var.include.new*get(as.character(attr(tmp,"variables")[[i]]))),NA,get(as.character(attr(tmp,"variables")[[i]])) ))))     
      if(var.level < 2) {
        model1<-update(model1,paste(". ~ . -",as.character(attr(tmp,"variables")[[i]]),sep=" "))
        var.level.flag.vector[i-1]<-0
      }
      if((var.level >= 2) & (i > 2)) {
        current.level.flag.vector<-var.level.flag.vector
        for(k in 2:(i-1)) {
          if (var.level.flag.vector[k-1]) {
            temp.obj<-var.level.check.STRESS(k,var.level.flag.vector,model1,tmp)
            model1<-temp.obj[[1]]
            tmp<-temp.obj[[2]]
            x<-numeric(0)
            for(l in 3:(ntmp + 1)) { 
              x<-append(x,temp.obj[[l]])
            }
            var.level.flag.vector<-x
          }
        }
        level.flag.diff<-1
        for(m in 1:length(var.level.flag.vector)) {
          if(current.level.flag.vector[m]==var.level.flag.vector[m]) (1*level.flag.diff) else 0
        }
        if(!(level.flag.diff)) {
          temp.obj<-var.level.check.STRESS(i,var.level.flag.vector,model1,tmp)
          model1<-temp.obj[[1]]
          tmp<-temp.obj[[2]]
          x<-numeric(0)
          for(l in 3:(ntmp + 1)) { 
            x<-append(x,temp.obj[[l]])
          }
          var.level.flag.vector<-x
        }
      }
    }
  }
  return(c(model1,tmp,var.level.flag.vector))
}


var.level.check.by <- function(ntmp_idx,var.level.flag.vector,model1,tmp) {
  outcomes_f <- as.character(model1[2])
  ntmp<-length(var.level.flag.vector) + 1
  for(i in 2:ntmp_idx) {
    running.var<-c(rep(1,length(get(outcomes_f))))
    for(j in 2:max(2,i-1)) {
      if(var.level.flag.vector[j-1]) {
        running.var<-running.var*ifelse(is.na(get(as.character(attr(tmp,"variables")[[j]]))),NA,1)
      }
    }
    running.var.include.new<-running.var*ifelse(is.na(get(as.character(attr(tmp,"variables")[[i]]))),NA,1)
          
    var.level<-length(levels(as.factor(ifelse(is.na(running.var.include.new*get(as.character(attr(tmp,"variables")[[i]]))),NA,get(as.character(attr(tmp,"variables")[[i]])) ))))
    if(var.level < 2) {
      model1<-update(model1,paste(". ~ . -",
                                  as.character(attr(tmp,"variables")[[i]]),sep=" "))
      var.level.flag.vector[i-1]<-0
    }
    if((var.level >= 2) & (i > 2)) {
      current.level.flag.vector<-var.level.flag.vector
      for(k in 2:(i-1)) {
        if (var.level.flag.vector[k]) {
          temp.obj<-var.level.check.by(k,var.level.flag.vector,model1,tmp)
          model1<-temp.obj[[1]]
          tmp<-temp.obj[[2]]
          x<-numeric(0)
          for(l in 3:(ntmp + 1)) { 
            x<-append(x,temp.obj[[l]])
          }
          var.level.flag.vector<-x
        }
      }
      level.flag.diff<-1
      for(m in 1:length(var.level.flag.vector)) {
        if(current.level.flag.vector[m]==var.level.flag.vector[m]) (1*level.flag.diff) else 0
      }
      if(!(level.flag.diff)) {
        temp.obj<-var.level.check.by(i,var.level.flag.vector,model1,tmp)
        model1<-temp.obj[[1]]
        tmp<-temp.obj[[2]]
        x<-numeric(0)
        for(l in 3:(ntmp + 1)) { 
          x<-append(x,temp.obj[[l]])
        }
        var.level.flag.vector<-x
      }    
    }
  }
  return(c(model1,tmp,var.level.flag.vector))  
}


var.level.check.mod <- function(ntmp_idx,var.level.flag.vector,model1,model2,tmp) {
  outcomes_f <- as.character(model1[2])
  ntmp<-length(var.level.flag.vector) + 1
#  ntmp<-length(attr(tmp,"variables"))
#  for(i in 2:max(2,ntmp)) {  
  for(i in 1:max(1,(ntmp_idx -1))) {
#    if((length(grep("L_Adum2_rs25531",as.character(attr(tmp,"variables")[[i+2]]))) == 0) &
#       (length(grep("Ldum2_5http",as.character(attr(tmp,"variables")[[i+2]]))) == 0)) {
    if((length(grep("L_Adum2_rs25531",as.character(attr(tmp,"variables")[[i+1]]))) == 0) &
       (length(grep("Ldum2_5http",as.character(attr(tmp,"variables")[[i+1]]))) == 0) &
       var.level.flag.vector[i] ) {
      running.var<-c(rep(1,length(get(outcomes_f))))*ifelse(is.na(get(outcomes_f)),NA,1)
      if( length(levels(as.factor(get(outcomes_f)))) < 2 ) {
        running.var<-c(rep(1,length(get(outcomes_f))))
      } 
      for(j in 1:max(1,(i-1))) {
        if(var.level.flag.vector[j]) {
#          running.var<-running.var*ifelse(is.na(get(as.character(attr(tmp,"variables")[[j+2]]))),NA,1)
          running.var<-running.var*ifelse(is.na(get(as.character(attr(tmp,"variables")[[j+1]]))),NA,1)
        }
      }        
      running.var.include.new<-running.var*ifelse(is.na(get(as.character(attr(tmp,"variables")[[i+1]]))),NA,1)
#      var.level<-length(levels(as.factor(ifelse(is.na(running.var*get(as.character(attr(tmp,"variables")[[i+2]]))),NA,get(as.character(attr(tmp,"variables")[[i+2]])) ))))     
      var.level<-length(levels(as.factor(ifelse(is.na(running.var.include.new*get(as.character(attr(tmp,"variables")[[i+1]]))),NA,get(as.character(attr(tmp,"variables")[[i+1]])) ))))     
#            if((length(grep("L_Adum1_rs25531",as.character(attr(tmp,"variables")[[i+2]]))) > 0) |
#               (length(grep("Ldum1_5http",as.character(attr(tmp,"variables")[[i+2]]))) > 0)) {
      if((length(grep("L_Adum1_rs25531",as.character(attr(tmp,"variables")[[i+1]]))) > 0) |
         (length(grep("Ldum1_5http",as.character(attr(tmp,"variables")[[i+1]]))) > 0)) {
        if((var.level < 2) & (i < (ntmp - 1))) {
          if(length(levels(as.factor(get(as.character(attr(tmp,"variables")[[(i+2)]]))))) < 2) {
            model1<-update(model1,paste(". ~ . -",
                                        as.character(attr(tmp,"variables")[[i+1]]),
                                        "-",as.character(attr(tmp,"variables")[[i+2]]),
                                        "-",paste(as.character(attr(tmp,"variables")[[i+1]]),
                                                  as.character(attr(tmp,"variables")[[i+3]]),sep=":"),
                                        "-",paste(as.character(attr(tmp,"variables")[[i+2]]),
                                                  as.character(attr(tmp,"variables")[[i+3]]),sep=":"),
                                        sep=" "))
#          if(length(levels(as.factor(get(as.character(attr(tmp,"variables")[[(i+3)]]))))) < 2) {
#            model1<-update(model1,paste(". ~ . -",
#                                        as.character(attr(tmp,"variables")[[i+2]]),
#                                        "-",as.character(attr(tmp,"variables")[[i+3]]),
#                                        "-",paste(as.character(attr(tmp,"variables")[[i+2]]),
#                                                  as.character(attr(tmp,"variables")[[i+4]]),sep=":"),
#                                        "-",paste(as.character(attr(tmp,"variables")[[i+3]]),
#                                                  as.character(attr(tmp,"variables")[[i+4]]),sep=":"),
#                                        sep=" "))
            model2<-update(model2,paste(". ~ . -",
                                        as.character(attr(tmp,"variables")[[i+1]]),
                                        "-",as.character(attr(tmp,"variables")[[i+2]]),sep=" "))
            var.level.flag.vector[i]<-0
            var.level.flag.vector[i+1]<-0            
          }
        }           
      } else {
        if(var.level < 2) {
          model2<-update(model2,paste(". ~ . -",as.character(attr(tmp,"variables")[[i+1]]),sep=" "))
          if((length(grep("rs25531",as.character(attr(tmp,"variables")[[i+1]]))) > 0) |
             (length(grep("5http",as.character(attr(tmp,"variables")[[i+1]]))) > 0)) {
#                if((length(grep("rs25531",as.character(attr(tmp,"variables")[[i+2]]))) > 0) |
#                   (length(grep("5http",as.character(attr(tmp,"variables")[[i+2]]))) > 0)) {
            model1<-update(model1,paste(". ~ . -",
                                        as.character(attr(tmp,"variables")[[i+1]]),
                                        "-",paste(as.character(attr(tmp,"variables")[[i+1]]),
                                                  as.character(attr(tmp,"variables")[[i+2]]),
                                                  sep=":"),sep=" "))
            var.level.flag.vector[i]<-0
          } else {
#            if((length(grep("child_mal",as.character(attr(tmp,"variables")[[i+2]]))) > 0) |
#               (length(grep("stress_combined",as.character(attr(tmp,"variables")[[i+2]]))) > 0) |
#               (length(grep("life_stress",as.character(attr(tmp,"variables")[[i+2]]))) > 0)) {
            if((length(grep("child_mal",as.character(attr(tmp,"variables")[[i+1]]))) > 0) |
               (length(grep("stress_combined",as.character(attr(tmp,"variables")[[i+1]]))) > 0) |
               (length(grep("life_stress",as.character(attr(tmp,"variables")[[i+1]]))) > 0)) {
              tmp2<-terms.formula(model2)
              if((length(grep("L_Adum1_rs25531",attr(tmp2,"term.labels"))) > 0) |
                 (length(grep("L_Adum2_rs25531",attr(tmp2,"term.labels"))) > 0) |
                 (length(grep("Ldum1_5http",attr(tmp2,"term.labels"))) > 0) |
                 (length(grep("Ldum2_5http",attr(tmp2,"term.labels"))) > 0) ) {                      
#                                model1<-update(model1,paste(". ~ . -",
#                                                            as.character(attr(tmp,"variables")[[i+2]]),"-",
#                                                            paste(as.character(attr(tmp,"variables")[[i+1]]),
#                                                                  as.character(attr(tmp,"variables")[[i+2]]),
#                                                                  sep=":"),"-",
#                                                            paste(as.character(attr(tmp,"variables")[[i]]),
#                                                                  as.character(attr(tmp,"variables")[[i+2]]),
#                                                                  sep=":"),sep=" "))
                model1<-update(model1,paste(". ~ . -",
                                            as.character(attr(tmp,"variables")[[i+1]]),"-",
                                            paste(as.character(attr(tmp,"variables")[[i]]),
                                                  as.character(attr(tmp,"variables")[[i+1]]),
                                                  sep=":"),"-",
                                            paste(as.character(attr(tmp,"variables")[[i-1]]),
                                                  as.character(attr(tmp,"variables")[[i+1]]),
                                                  sep=":"),sep=" "))
              } else {
                if((length(grep("rs25531",attr(tmp2,"term.labels"))) > 0) |
                   (length(grep("5http",attr(tmp2,"term.labels"))) > 0)) {
#                                model1<-update(model1,paste(". ~ . -",
#                                                            as.character(attr(tmp,"variables")[[i+2]]),"-",
#                                                            paste(as.character(attr(tmp,"variables")[[i+1]]),
#                                                                  as.character(attr(tmp,"variables")[[i+2]]),
#                                                                  sep=":"),sep=" "))
                  model1<-update(model1,paste(". ~ . -",
                                              as.character(attr(tmp,"variables")[[i+1]]),"-",
                                              paste(as.character(attr(tmp,"variables")[[i]]),
                                                    as.character(attr(tmp,"variables")[[i+1]]),
                                                    sep=":"),sep=" "))
                }
              }
            }  else {
#              model1<-update(model1,paste(". ~ . -",as.character(attr(tmp,"variables")[[i+2]]),sep=" "))
              model1<-update(model1,paste(". ~ . -",as.character(attr(tmp,"variables")[[i+1]]),sep=" "))
            }
          }            
          var.level.flag.vector[i]<-0
        }
      }
      if((var.level >= 2) & (i > 1)) {
        current.level.flag.vector<-var.level.flag.vector
        for(k in 1:(i-1)) {
          if (var.level.flag.vector[k]) {
            temp.obj<-var.level.check.mod((k+1),var.level.flag.vector,model1,model2,tmp)
            model1<-temp.obj[[1]]
            model2<-temp.obj[[2]]
            tmp<-temp.obj[[3]]
            x<-numeric(0)
            for(l in 4:(ntmp + 2)) { 
              x<-append(x,temp.obj[[l]])
            }
            var.level.flag.vector<-x
          }
        }
        level.flag.diff<-1
        for(m in 1:length(var.level.flag.vector)) {
          if(current.level.flag.vector[m]==var.level.flag.vector[m]) (1*level.flag.diff) else 0
        }
        if(!(level.flag.diff)) {
          temp.obj<-var.level.check.mod((i+1),var.level.flag.vector,model1,model2,tmp)
          model1<-temp.obj[[1]]
          model2<-temp.obj[[2]]
          tmp<-temp.obj[[3]]
          x<-numeric(0)
          for(l in 4:(ntmp + 2)) { 
            x<-append(x,temp.obj[[l]])
          }
          var.level.flag.vector<-x
        }
      }
    }
  }
  return(c(model1,model2,tmp,var.level.flag.vector))
}


var.level.check.modlog <- function(ntmp_idx,var.level.flag.vector,model1,model2,tmp) {
  outcomes_f <- as.character(model1[2])
  ntmp<-length(var.level.flag.vector) + 1
#  ntmp<-length(attr(tmp,"variables"))
#  for(i in 2:max(2,ntmp)) {  
  for(i in 2:max(2,ntmp_idx)) {  
    if((length(grep("L_Adum2_rs25531",as.character(attr(tmp,"variables")[[i]]))) == 0) &
       (length(grep("Ldum2_5http",as.character(attr(tmp,"variables")[[i]]))) == 0) &
       var.level.flag.vector[i-1] ) {                

      running.var<-c(rep(1,length(get(outcomes_f))))
      for(j in 2:max(2,i-1)) {
        if(var.level.flag.vector[j-1]) {
          running.var<-running.var*ifelse(is.na(get(as.character(attr(tmp,"variables")[[j]]))),NA,1)
        }
      }
      running.var.include.new<-running.var*ifelse(is.na(get(as.character(attr(tmp,"variables")[[i]]))),NA,1)
      var.level<-length(levels(as.factor(ifelse(is.na(running.var.include.new*get(as.character(attr(tmp,"variables")[[i]]))),NA,get(as.character(attr(tmp,"variables")[[i]])) ))))     
      if((length(grep("L_Adum1_rs25531",as.character(attr(tmp,"variables")[[i]]))) > 0) |
         (length(grep("Ldum1_5http",as.character(attr(tmp,"variables")[[i]]))) > 0)) {
        if((var.level < 2) & (i < (ntmp_idx -1))) {
          if(length(levels(as.factor(get(as.character(attr(tmp,"variables")[[(i+1)]]))))) < 2) {
            model1<-update(model1,paste(". ~ . -",
                                        as.character(attr(tmp,"variables")[[i]]),
                                        "-",as.character(attr(tmp,"variables")[[i+1]]),sep=" "))
            model2<-update(model2,paste(". ~ . -",
                                        as.character(attr(tmp,"variables")[[i]]),"-",
                                        as.character(attr(tmp,"variables")[[i+1]]),"-",
                                        paste(as.character(attr(tmp,"variables")[[i]]),
                                              as.character(attr(tmp,"variables")[[i+2]]),
                                              sep=":"),"-",
                                        paste(as.character(attr(tmp,"variables")[[i+1]]),
                                              as.character(attr(tmp,"variables")[[i+2]]),
                                              sep=":"),sep=" "))
#            model2<-update(model2,paste(". ~ . -",
#                                        as.character(attr(tmp,"variables")[[i]]),
#                                        "-",as.character(attr(tmp,"variables")[[i+1]]),sep=" "))
            var.level.flag.vector[i-1]<-0
            var.level.flag.vector[i]<-0            
          }
        }
      } else {
        if(var.level < 2) {
          model1<-update(model1,paste(". ~ . -",as.character(attr(tmp,"variables")[[i]]),sep=" "))
          if((length(grep("rs25531",as.character(attr(tmp,"variables")[[i]]))) > 0) |
             (length(grep("5http",as.character(attr(tmp,"variables")[[i]]))) > 0)) {
            model2<-update(model2,paste(". ~ . -",
                                        as.character(attr(tmp,"variables")[[i]]),
                                        "-",paste(as.character(attr(tmp,"variables")[[i]]),
                                                  as.character(attr(tmp,"variables")[[i+1]]),
                                                  sep=":"),sep=" "))
          } else {
            if((length(grep("child_mal",as.character(attr(tmp,"variables")[[i]]))) > 0) |
               (length(grep("stress_combined",as.character(attr(tmp,"variables")[[i]]))) > 0) |
               (length(grep("life_stress",as.character(attr(tmp,"variables")[[i]]))) > 0)) {
              tmp2<-terms.formula(model1)
              if((length(grep("L_Adum1_rs25531",attr(tmp2,"term.labels"))) > 0) |
                 (length(grep("L_Adum2_rs25531",attr(tmp2,"term.labels"))) > 0) |
                 (length(grep("Ldum1_5http",attr(tmp2,"term.labels"))) > 0) |
                 (length(grep("Ldum2_5http",attr(tmp2,"term.labels"))) > 0) ) {                      
                model2<-update(model2,paste(". ~ . -",
                                            as.character(attr(tmp,"variables")[[i]]),"-",
                                            paste(as.character(attr(tmp,"variables")[[i-1]]),
                                                  as.character(attr(tmp,"variables")[[i]]),
                                                  sep=":"),"-",
                                            paste(as.character(attr(tmp,"variables")[[i-2]]),
                                                  as.character(attr(tmp,"variables")[[i]]),
                                                  sep=":"),sep=" "))
              } else {
                if((length(grep("rs25531",attr(tmp2,"term.labels"))) > 0) |
                 (length(grep("5http",attr(tmp2,"term.labels"))) > 0)) {
                  model2<-update(model2,paste(". ~ . -",
                                              as.character(attr(tmp,"variables")[[i]]),"-",
                                              paste(as.character(attr(tmp,"variables")[[i-1]]),
                                                    as.character(attr(tmp,"variables")[[i]]),
                                                    sep=":"),sep=" "))
                }
              }
            } else model2<-update(model2,paste(". ~ . -",as.character(attr(tmp,"variables")[[i]]),sep=" "))
          }
          var.level.flag.vector[i-1]<-0
        }
      }
      if(var.level >= 2) {
        if(i > 2) {
          current.level.flag.vector<-var.level.flag.vector
          for(k in 2:(i-1)) {
            if (var.level.flag.vector[k-1]) {
              temp.obj<-var.level.check.modlog(k,var.level.flag.vector,model1,model2,tmp)
              model1<-temp.obj[[1]]
              model2<-temp.obj[[2]]
              tmp<-temp.obj[[3]]
              x<-numeric(0)
              for(l in 4:(ntmp + 2)) { 
                x<-append(x,temp.obj[[l]])
              }
              var.level.flag.vector<-x
            }
          }
          level.flag.diff<-1
          for(m in 1:length(var.level.flag.vector)) {
            if(current.level.flag.vector[m]==var.level.flag.vector[m]) (1*level.flag.diff) else 0
          }
          if(!(level.flag.diff)) {
            temp.obj<-var.level.check.modlog(i,var.level.flag.vector,model1,model2,tmp)
            model1<-temp.obj[[1]]
            model2<-temp.obj[[2]]
            tmp<-temp.obj[[3]]
            x<-numeric(0)
            for(l in 4:(ntmp + 2)) { 
              x<-append(x,temp.obj[[l]])
            }
            var.level.flag.vector<-x
          }
        }
      }
    }
  }
  return(c(model1,model2,tmp,var.level.flag.vector))
}


test_fn2<-function(test_idx,test_model1,test_model2,tmp) {
 idx2<-test_idx + 1
 model3<-update(test_model1,". ~ . - in1")
 model4<-update(test_model2,". ~ . - in2")
 return(c(idx2,model3,model4,tmp))
}



purge.warn <- function() {
  assign("last.warning",NULL,envir=baseenv())
}


provideDimnames <- function (x, sep = "", base = list(LETTERS)){
    dx <- dim(x)
    dnx <- dimnames(x)
    if (new <- is.null(dnx))
        dnx <- vector("list", length(dx))
    k <- length(M <- vapply(base, length, 1L))
    for (i in which(vapply(dnx, is.null, NA))) {
        ii <- 1L + (i - 1L)%%k
        dnx[[i]] <- make.unique(base[[ii]][1L + 0:(dx[i] - 1L)%%M[ii]],
                                sep = sep)
        new <- TRUE
    }
    if (new)
        dimnames(x) <- dnx
    x
}

########################################
# check if variable present in dataset #
########################################

defined.in.dataset<-function(varname,dataframe) {
  attach(dataframe)
  tmp<-(sum(ifelse(is.na(dataframe[,names(dataframe)==varname]),1,0))/length(dataframe[,names(dataframe)==varname]) < 0.9)  
  detach(dataframe)
  return(tmp)
}

exists.to.tab<-function(varname) {
  tmp<-(sum(ifelse(is.na(get(varname)),0,1)))        
  return(ifelse(tmp,1,0))
}
exists.to.tab2<-function(var) {
  tmp<-(sum(ifelse(is.na(var),0,1)))        
  return(ifelse(tmp,1,0))
}

decade_na_flag<-function(vari=birth_decade){
  if(length(vari)>0)
    return(ifelse( sum(ifelse(is.na(vari),1,0))/length(vari) ==1,1,0)) else
  return(TRUE)
}


#################################################
# check if variable present in >=10% of dataset #
#################################################

if10<-function(variable){
  if(length(variable)>0)
    return(sum(ifelse(is.na(variable),1,0))/length(variable) < 0.9) else
  return(FALSE)
}

if10.by<-function(variable1,variable2){
  if(length(variable1)>0)
    return(sum(ifelse(is.na(variable1*variable2),1,0))/length(variable1) < 0.9) else
   return(FALSE) 
}
 

################################################
# check if variable present in multiple states #
################################################

two_level_present<-function(varname){
	return(ifelse(length(levels(as.factor(get(varname))))>1,"YES","NO"))
}
two_level_present_var<-function(var){
	return(ifelse(length(levels(as.factor(var)))>1,"YES","NO"))
}

###############################
# conditional test x SNP freq #
###############################

maf<-function(condition){
   return(colMeans(SNPs[condition,],na.rm=T)/2) }
n<-function(condition){
   return(c(rep(1,dim(SNPs[condition,])[1]))%*%ifelse(is.na(SNPs[condition,]),0,1))}

SNP.freq<-function(condition){
      results<-cbind(c(maf(condition)),c(n(condition)))
      results<-if(genotype_SL=="YES") if(genotype_rs25531=="YES") results[1:2,] else results[1,] else if(genotype_rs25531=="YES") results[2,] else NA
      return(results)}


##############################################
# generate statistics for quantitative trait #
##############################################

get.stat<-function(varA,name.varA){

write(paste('START:',name.varA,sep=""), file=gz1,append=TRUE)
write(paste(name.varA," =",mean(varA,na.rm=TRUE)," +/- ",sd(varA,na.rm=TRUE)," min",min(varA,na.rm=TRUE)," max",max(varA,na.rm=TRUE)," deciles"
            ,sep=" "), file=gz1,append=TRUE)
write(quantile(varA,c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),na.rm=TRUE), file=gz1,append=TRUE)
write(length(na.exclude(varA)), file=gz1,append=TRUE)

write(paste('STOP:',name.varA,sep=""), file=gz1,append=TRUE)
write(" ", file=gz1,append=TRUE)  
write(" ", file=gz1,append=TRUE)  
write(" ", file=gz1,append=TRUE)
}

#############################
# generate frequency tables #
#############################

get.freq<-function(varA,name.varA){
  varA_freq <-table(varA,exclude=NULL)

  varAfreq<-varA_freq   ######### makes a copy
  names(varAfreq)<-NULL ######## with an empty header

  varA.values<-data.frame(
    varA = c(NA,c(names(varA_freq))),
    placeholder = rep(1,(length(names(varA_freq))+1)) )

  varA.rates <-data.frame(
    varA =names(varA_freq),
    varAfreq=varAfreq	) 

  varA.table<-merge(varA.values,varA.rates,all.x=TRUE)[,c(1,4)]
  names(varA.table)[1]<-'varA'
  names(varA.table)[2] <- 'Frequency'

  write(paste('START',name.varA,sep=":"),file=gz1,append=TRUE )
  write(paste(name.varA,'freq',sep=','),file=gz1,append=TRUE )
  write.table(varA.table, file=gz1, quote=FALSE,sep=", ",na=".",  row.names = FALSE, col.names=FALSE, append=TRUE) 
  write(paste('STOP',name.varA,sep=":"),file=gz1,append=TRUE )
  write(" ", file=gz1,append=TRUE)  
  write(" ", file=gz1,append=TRUE)  
  write(" ", file=gz1,append=TRUE)
}


get.xtab<-function(varA,varB,name.varA,name.varB){
  	varA_varB<-as.data.frame(table(varA,varB,exclude=NULL))

	varAfreq<-varA_varB
        levelA<-levels(as.factor(varAfreq$varA))
        levelB<-levels(as.factor(varAfreq$varB))
        nlevelA<-nlevels(as.factor(varAfreq$varA))
        nlevelB<-nlevels(as.factor(varAfreq$varB))

	varAfreq$varA<-as.character(varAfreq$varA)	
	varAfreq$varB<-as.character(varAfreq$varB)

  	varAt<-as.character(c(NA,levelA))
  	varBt<-as.character(c(NA,levelB))   
        
  	temp<-cbind(merge(varAt,varBt),placeholder=rep(1,(nlevelA+1)*(nlevelB+1)))  
	names(temp)[1]<-'varA'
	names(temp)[2]<-'varB'
 	
	varA.freq.values<-data.frame(temp)
	varA.freq.table<-merge(varAfreq,varA.freq.values, all.x=TRUE)[,c(1,2,3)]
        
	write(paste('START',paste(name.varA,name.varB,sep=" x "),sep=":"),file=gz1,append=TRUE )
	write(paste(name.varA,name.varB,'freq',sep=','),file=gz1,append=TRUE )
	write.table(varA.freq.table, file=gz1, append=TRUE, quote=FALSE,sep=", ",na=".",  row.names = FALSE, col.names=FALSE) 
	write(paste('STOP',paste(name.varA,name.varB,sep=" x "),sep=":"),file=gz1,append=TRUE )
        write(" ", file=gz1,append=TRUE)  
        write(" ", file=gz1,append=TRUE)  
        write(" ", file=gz1,append=TRUE)
      }


get.tritab<-function(varA,varB,varC,name.varA,name.varB,name.varC){
  write(paste('START',paste(name.varA,name.varB,name.varC,sep=" x "),sep=":"),file=gz1,append=TRUE )
  levelC<-levels(as.factor(varC))                           
  nlevelC<-nlevels(as.factor(varC))
  if(length(varC[is.na(varC)]) > 0) {
    levelC<-append(levelC,NA)
    nlevelC<-nlevelC + 1
  }
  
  for (i in 1:nlevelC){
    if(is.na(levelC[i])==FALSE){
      varA_varB<-as.data.frame(table(varA[varC==levelC[i]],varB[varC==levelC[i]],
                                     exclude=NULL))
    } else {
      varA_varB<-as.data.frame(table(varA[is.na(varC)],varB[is.na(varC)],
                                     exclude=NULL))
    }

    names(varA_varB)[1]<-'varA'
    names(varA_varB)[2]<-'varB'

    if(dim(varA_varB)[1] > 0){
     
      varAfreq<-varA_varB
      varAfreq$varA<-as.character(varAfreq$varA)	
      varAfreq$varB<-as.character(varAfreq$varB)
      
      levelA<-levels(as.factor(varAfreq$varA))
      levelB<-levels(as.factor(varAfreq$varB))
      nlevelA<-nlevels(as.factor(varAfreq$varA))
      nlevelB<-nlevels(as.factor(varAfreq$varB))
      
      varAt<-as.character(c(NA,levelA))
      varBt<-as.character(c(NA,levelB))   
      
      temp<-cbind(merge(varAt,varBt),placeholder=rep(1,(nlevelA+1)*(nlevelB+1)))  
      names(temp)[1]<-'varA'
      names(temp)[2]<-'varB'
 	
      varA.freq.values<-data.frame(temp)
      varA.freq.table<-merge(varAfreq,varA.freq.values, all.x=TRUE)[,c(1,2,3)]
        
      write("####",file=gz1,append=TRUE )
      write(paste(name.varC,"=",levelC[i],sep=" "),file=gz1,append=TRUE )  
      write(paste(name.varA,name.varB,'freq',sep=','),file=gz1,append=TRUE )
      write.table(varA.freq.table, file=gz1, append=TRUE, quote=FALSE,sep=", ",na=".",  row.names = FALSE, col.names=FALSE)
    }
  }

  write(paste('STOP',paste(name.varA,name.varB,name.varC,sep=" x "),sep=":"),file=gz1,append=TRUE )
  write(" ", file=gz1,append=TRUE)  
  write(" ", file=gz1,append=TRUE)  
  write(" ", file=gz1,append=TRUE)  
}

  
get.quadtab<-function(varA,varB,varC,varD,name.varA,name.varB,name.varC,name.varD){
  write(paste('START',paste(name.varA,name.varB,name.varC,name.varD,sep=" x "),sep=":"),file=gz1,append=TRUE )
  levelD<-levels(as.factor(varD))                           
  nlevelD<-nlevels(as.factor(varD))
  if(length(varD[is.na(varD)]) > 0) {
    levelD<-append(levelD,NA)
    nlevelD<-nlevelD + 1
  }

  for (j in 1:nlevelD){
    if(is.na(levelD[j])==FALSE){
      levelC<-levels(as.factor(varC[(varD==levelD[j]) & !(is.na(varD))]))
      nlevelC<-nlevels(as.factor(varC[varD==levelD[j] & !(is.na(varD))]))
      if(length(varC[is.na(varC) & varD==levelD[j] & !(is.na(varD))]) > 0) {
        levelC<-append(levelC,NA)
        nlevelC<-nlevelC + 1
      }
    } else {
      levelC<-levels(as.factor(varC[is.na(varD)])) 
      nlevelC<-nlevels(as.factor(varC[is.na(varD)]))
      if(length(varC[is.na(varC) & is.na(varD)]) > 0) {
        levelC<-append(levelC,NA)
        nlevelC<-nlevelC + 1
      }
    }

    
    for (i in 1:nlevelC){
      if(is.na(levelD[j])==FALSE){
        if(is.na(levelC[i])==FALSE){
            varA_varB<-as.data.frame(table(varA[varC==levelC[i] &
	    							varD==levelD[j]], varB[varC==levelC[i] &
	    							varD==levelD[j]],
                                           exclude=NULL))
          } else {
            varA_varB<-as.data.frame(table(varA[is.na(varC) & varD==levelD[j]],
	    				   varB[is.na(varC) & varD==levelD[j]], 
                                           exclude=NULL))
          }
      } else {
        if(is.na(levelC[i])==FALSE){
          varA_varB<-as.data.frame(table(varA[(varC==levelC[i]) &
                                              is.na(varD)],
					 varB[(varC==levelC[i]) &
                                              is.na(varD)], 
                                         exclude=NULL))
        } else {
            varA_varB<-as.data.frame(table(varA[is.na(varC) &
                                                is.na(varD)],
					   varB[is.na(varC) &
                                                is.na(varD)], 
                                           exclude=NULL))            
        }
      }

      names(varA_varB)[1]<-'varA'
      names(varA_varB)[2]<-'varB'
     
      if(is.null(dim(varA_varB))== FALSE){
        if(dim(varA_varB)[1] > 0){

          varAfreq<-varA_varB
          varAfreq$varA<-as.character(varAfreq$varA)	
          varAfreq$varB<-as.character(varAfreq$varB)
      
          levelA<-levels(as.factor(varAfreq$varA))
          levelB<-levels(as.factor(varAfreq$varB))
          nlevelA<-nlevels(as.factor(varAfreq$varA))
          nlevelB<-nlevels(as.factor(varAfreq$varB))
      
          varAt<-as.character(c(NA,levelA))
          varBt<-as.character(c(NA,levelB))   
                
          temp<-cbind(merge(varAt,varBt),placeholder=rep(1,(nlevelA+1)*(nlevelB+1)))  
          names(temp)[1]<-'varA'
          names(temp)[2]<-'varB'
 	
          varA.freq.values<-data.frame(temp)
          varA.freq.table<-merge(varAfreq,varA.freq.values, all.x=TRUE)[,c(1,2,3)]
        
          write("####",file=gz1,append=TRUE )
          write(paste(name.varD,"=",levelD[j],sep=" "),file=gz1,append=TRUE )
          write(paste(name.varC,"=",levelC[i],sep=" "),file=gz1,append=TRUE )  
          write(paste(name.varA,name.varB,'freq',sep=','),file=gz1,append=TRUE )
          write.table(varA.freq.table, file=gz1, append=TRUE, quote=FALSE,sep=", ",na=".",  row.names = FALSE, col.names=FALSE)
        }
      }
    }
  }

  write(paste('STOP',paste(name.varA,name.varB,name.varC,name.varD,sep=" x "),sep=":"),file=gz1,append=TRUE )
  write(" ", file=gz1,append=TRUE)  
  write(" ", file=gz1,append=TRUE)  
  write(" ", file=gz1,append=TRUE)  
}

#get.quintab(female,age_cat,add_5http,DD_life,child_mal_exp,
#                                "name.varA","name.varB","name.varC","name.varD","name.varE")
#
#varA     <-female
#varB     <-age_cat
#varC     <-add_5http
#varD     <-DD_life
#varE     <-child_mal_exp
#name.varA<-"name.varA"
#name.varB<-"name.varB"
#name.varC<-"name.varC"
#name.varD<-"name.varD"
#name.varE<-"name.varE"













get.quintab<-function(varA,varB,varC,varD,varE,name.varA,name.varB,name.varC,name.varD,name.varE){
  levelE<-levels(as.factor(varE))
  nlevelE<-nlevels(as.factor(varE))
  if(length(varE[is.na(varE)]) > 0) {
    levelE<-append(levelE,NA)
    nlevelE<-nlevelE + 1
  }
  
  write(paste('START',paste(name.varA,name.varB,name.varC,name.varD,name.varE,sep=" x "),sep=":"),file=gz1,append=TRUE )
  for (k in 1:nlevelE){
    if(is.na(levelE[k])==FALSE) {
      levelD<-levels(as.factor(varD[varE==levelE[k] & !(is.na(varE))]))
      nlevelD<-nlevels(as.factor(varD[varE==levelE[k] & !(is.na(varE))]))
      if(length(varD[is.na(varD) & varE==levelE[k] & !(is.na(varE))]) > 0) {
        levelD<-append(levelD,NA)
        nlevelD<-nlevelD + 1
      }
    } else {
      levelD<-levels(as.factor(varD[is.na(varE)]))
      nlevelD<-nlevels(as.factor(varD[is.na(varE)]))
      if(length(varD[is.na(varD) & is.na(varE)]) > 0) {
        levelD<-append(levelD,NA)
        nlevelD<-nlevelD + 1
      }
    }
  
    for (j in 1:nlevelD) {
      if(is.na(levelE[k])==FALSE) {
        if(is.na(levelD[j])==FALSE) {
          levelC<-levels(as.factor(varC[(varD==levelD[j]) & !(is.na(varD)) &
                                        (varE==levelE[k]) & !(is.na(varE))]))
          nlevelC<-nlevels(as.factor(varC[(varD==levelD[j]) & !(is.na(varD)) &
                                          (varE==levelE[k]) & !(is.na(varE))]))
          if(length(varC[is.na(varC) &
                         (varD==levelD[j]) & !(is.na(varD)) &
                         (varE==levelE[k]) & !(is.na(varE))]) > 0) {
            levelC<-append(levelC,NA)
            nlevelC<-nlevelC + 1
          }
        } else {
          levelC<-levels(as.factor(varC[is.na(varD) &
                                        (varE==levelE[k]) & !(is.na(varE))]))
          nlevelC<-nlevels(as.factor(varC[is.na(varD) &
                                          (varE==levelE[k]) & !(is.na(varE))]))
          if(length(varC[is.na(varC) &
                         is.na(varD) &
                         (varE==levelE[k]) & !(is.na(varE))]) > 0) {
            levelC<-append(levelC,NA)
            nlevelC<-nlevelC + 1
          }
        }
      } else {
        if(is.na(levelD[j])==FALSE) {
          levelC<-levels(as.factor(varC[(varD==levelD[j] & !(is.na(varD))) &
                                        is.na(varE)]))
          nlevelC<-nlevels(as.factor(varC[(varD==levelD[j] & !(is.na(varD))) &
                                          is.na(varE)]))
          if(length(varC[is.na(varC) &
                         (varD==levelD[j]) & !(is.na(varD)) &
                         is.na(varE)]) > 0) {
            levelC<-append(levelC,NA)
            nlevelC<-nlevelC + 1
          }
        } else {
          levelC<-levels(as.factor(varC[is.na(varD) &
                                        is.na(varE)]))
          nlevelC<-nlevels(as.factor(varC[is.na(varD) &
                                          is.na(varE)]))
          if(length(varC[is.na(varC) &
                         is.na(varD) &
                         is.na(varE)]) > 0) {
            levelC<-append(levelC,NA)
            nlevelC<-nlevelC + 1
          }
        }
      }

      for (i in 1:nlevelC){
        if(is.na(levelE[k])==FALSE){
          if(is.na(levelD[j])==FALSE){
            if(is.na(levelC[i])==FALSE){
              varA_varB<-as.data.frame(table(varA[(varC==levelC[i]) &
                                                  (varD==levelD[j]) &
                                                  (varE==levelE[k])],
					     varB[(varC==levelC[i]) &
                                                  (varD==levelD[j]) &
                                                  (varE==levelE[k])],
					     exclude=NULL))

              
            } else {
              varA_varB<-as.data.frame(table(varA[is.na(varC) &
                                                  (varD==levelD[j]) &
                                                  (varE==levelE[k])],
					     varB[is.na(varC) &
                                                  (varD==levelD[j]) &
                                                  (varE==levelE[k])],
					     exclude=NULL))
            }
          } else {
            if(is.na(levelC[i])==FALSE){
              varA_varB<-as.data.frame(table(varA[(varC==levelC[i]) &
                                                  is.na(varD) &
                                                  (varE==levelE[k])],
					     varB[(varC==levelC[i]) &
                                                  is.na(varD) &
                                                  (varE==levelE[k])],
					     exclude=NULL))
            } else {
              varA_varB<-as.data.frame(table(varA[is.na(varC) &
                                                  is.na(varD) &
                                                  (varE==levelE[k])],
					     varB[is.na(varC) &
                                                  is.na(varD) &
                                                  (varE==levelE[k])],
					     exclude=NULL))
              
            }
          }
        } else {
          if(is.na(levelD[j])==FALSE){
            if(is.na(levelC[i])==FALSE){
              varA_varB<-as.data.frame(table(varA[(varC==levelC[i]) &
                                                  (varD==levelD[j]) &
                                                  is.na(varE)],
					     varB[(varC==levelC[i]) &
                                                  (varD==levelD[j]) &
                                                  is.na(varE)],
                                             exclude=NULL))
            } else {
              varA_varB<-as.data.frame(table(varA[is.na(varC) &
                                                  (varD==levelD[j]) &
                                                  is.na(varE)],
                                             varB[is.na(varC) &
                                                  (varD==levelD[j]) &
                                                  is.na(varE)],
					     exclude=NULL))
            }
          } else {
            if(is.na(levelC[i])==FALSE){
              varA_varB<-as.data.frame(table(varA[(varC==levelC[i]) &
                                                  is.na(varD) &
                                                  is.na(varE)],
					     varB[(varC==levelC[i]) &
                                                  is.na(varD) &
                                                  is.na(varE)],
					     exclude=NULL))
            } else {
              varA_varB<-as.data.frame(table(varA[is.na(varC) &
                                                  is.na(varD) &
                                                  is.na(varE)],
					     varB[is.na(varC) &
                                                  is.na(varD) &
                                                  is.na(varE)],
					     exclude=NULL))
            }
          }
        }

      	names(varA_varB)[1]<-'varA'
      	names(varA_varB)[2]<-'varB'
      
        if(dim(varA_varB)[1] > 0){
   
          varAfreq<-varA_varB
          varAfreq$varA<-as.character(varAfreq$varA)	
          varAfreq$varB<-as.character(varAfreq$varB)
      
          levelA<-levels(as.factor(varAfreq$varA))
          levelB<-levels(as.factor(varAfreq$varB))
          nlevelA<-nlevels(as.factor(varAfreq$varA))
          nlevelB<-nlevels(as.factor(varAfreq$varB))

          varAt<-as.character(c(NA,levelA))
          varBt<-as.character(c(NA,levelB))   
                
          temp<-cbind(merge(varAt,varBt),placeholder=rep(1,(nlevelA+1)*(nlevelB+1)))  
          names(temp)[1]<-'varA'
          names(temp)[2]<-'varB'
 	
          varA.freq.values<-data.frame(temp)
          varA.freq.table<-merge(varAfreq,varA.freq.values, all.x=TRUE)[,c(1,2,3)]

        
          write("####",file=gz1,append=TRUE )
          write(paste(name.varE,"=",levelE[k],sep=" "),file=gz1,append=TRUE )
          write(paste(name.varD,"=",levelD[j],sep=" "),file=gz1,append=TRUE )
          write(paste(name.varC,"=",levelC[i],sep=" "),file=gz1,append=TRUE )  
          write(paste(name.varA,name.varB,'freq',sep=','),file=gz1,append=TRUE )
          write.table(varA.freq.table, file=gz1, append=TRUE, quote=FALSE,sep=", ",na=".",  row.names = FALSE, col.names=FALSE)
        }
      }
    }
  }

  write(paste('STOP',paste(name.varA,name.varB,name.varC,name.varD,name.varE,sep=" x "),sep=":"),file=gz1,append=TRUE )
  write(" ", file=gz1,append=TRUE)  
  write(" ", file=gz1,append=TRUE)  
  write(" ", file=gz1,append=TRUE)  
}



get.sextab<-function(varA,varB,varC,varD,varE,varF,name.varA,name.varB,name.varC,name.varD,name.varE,name.varF){
  write(paste('START',paste(name.varA,name.varB,name.varC,name.varD,name.varE,name.varF,sep=" x "),sep=":"),file=gz1,append=TRUE )
  
  levelF<-levels(as.factor(varF))
  nlevelF<-nlevels(as.factor(varF))
  if(length(varF[is.na(varF)]) > 0) {
    levelF<-append(levelF,NA)
    nlevelF<-nlevelF + 1
  }
  
  for (l in 1:nlevelF){
    if(is.na(levelF[l])==FALSE) {
      levelE<-levels(as.factor(varE[varF==levelF[l] & !(is.na(varF))]))
      nlevelE<-nlevels(as.factor(varE[varF==levelF[l] & !(is.na(varF))]))
      if(length(varE[is.na(varE) & varF==levelF[l] & !(is.na(varF))]) > 0) {
        levelE<-append(levelE,NA)
        nlevelE<-nlevelE + 1
      }
    } else {
      levelE<-levels(as.factor(varE[is.na(varF)]))
      nlevelE<-nlevels(as.factor(varE[is.na(varF)]))
      if(length(varE[is.na(varE) & is.na(varF)]) > 0) {
        levelE<-append(levelE,NA)
        nlevelE<-nlevelE + 1
      }
    }

    for (k in 1:nlevelE) {
      if(is.na(levelF[l])==FALSE) {      
        if(is.na(levelE[k])==FALSE) {
          levelD<-levels(as.factor(varD[varE==levelE[k] & !(is.na(varE)) & varF==levelF[l] & !(is.na(varF))]))
          nlevelD<-nlevels(as.factor(varD[varE==levelE[k] & !(is.na(varE)) & varF==levelF[l] & !(is.na(varF))]))
          if(length(varD[is.na(varD) &
                         varE==levelE[k] & !(is.na(varE)) &
                         varF==levelF[l] & !(is.na(varF))]) > 0) {
            levelD<-append(levelD,NA)
            nlevelD<-nlevelD + 1
          }
        } else {
          levelD<-levels(as.factor(varD[is.na(varE) & varF==levelF[l] & !(is.na(varF))]))
          nlevelD<-nlevels(as.factor(varD[is.na(varE) & varF==levelF[l] & !(is.na(varF))]))
          if(length(varD[is.na(varD) &
                         is.na(varE) &
                         varF==levelF[l] & !(is.na(varF))]) > 0) {
            levelD<-append(levelD,NA)
            nlevelD<-nlevelD + 1
          }
        }
      } else {
        if(is.na(levelE[k])==FALSE) {
          levelD<-levels(as.factor(varD[varE==levelE[k] & !(is.na(varE)) & is.na(varF)]))
          nlevelD<-nlevels(as.factor(varD[varE==levelE[k] & !(is.na(varE)) & is.na(varF)]))
          if(length(varD[is.na(varD) &
                         varE==levelE[k] & !(is.na(varE)) &
                         is.na(varF)]) > 0) {
            levelD<-append(levelD,NA)
            nlevelD<-nlevelD + 1
          }
        } else {
          levelD<-levels(as.factor(varD[is.na(varE) & is.na(varF)]))
          nlevelD<-nlevels(as.factor(varD[is.na(varE) & is.na(varF)]))
          if(length(varD[is.na(varD) &
                         is.na(varE) &
                         is.na(varF)]) > 0) {
            levelD<-append(levelD,NA)
            nlevelD<-nlevelD + 1
          }
        }
      }

      for (j in 1:nlevelD){
        if(is.na(levelF[l])==FALSE) {      
          if(is.na(levelE[k])==FALSE) {
            if(is.na(levelD[j])==FALSE) {
              levelC<-levels(as.factor(varC[(varD==levelD[j]) & !(is.na(varD)) &
                                            (varE==levelE[k]) & !(is.na(varE)) &
                                            (varF==levelF[l]) & !(is.na(varF))]))
              nlevelC<-nlevels(as.factor(varC[(varD==levelD[j]) & !(is.na(varD)) &
                                              (varE==levelE[k]) & !(is.na(varE)) &
                                              (varF==levelF[l]) & !(is.na(varF))]))
              if(length(varC[is.na(varC) &
                             (varD==levelD[j]) & !(is.na(varD)) &
                             (varE==levelE[k]) & !(is.na(varE)) &
                             (varF==levelF[l]) & !(is.na(varF))]) > 0) {
                levelC<-append(levelC,NA)
                nlevelC<-nlevelC + 1
              }
            } else {
              levelC<-levels(as.factor(varC[is.na(varD) &
                                            (varE==levelE[k]) & !(is.na(varE)) &
                                            (varF==levelF[l]) & !(is.na(varF))]))
              nlevelC<-nlevels(as.factor(varC[is.na(varD) &
                                              (varE==levelE[k]) & !(is.na(varE)) &
                                              (varF==levelF[l]) & !(is.na(varF))]))
              if(length(varC[is.na(varC) &
                             is.na(varD) &
                             (varE==levelE[k]) & !(is.na(varE)) &
                             (varF==levelF[l]) & !(is.na(varF))]) > 0) {
                levelC<-append(levelC,NA)
                nlevelC<-nlevelC + 1
              }
            }
          } else {
            if(is.na(levelD[j])==FALSE) {
              levelC<-levels(as.factor(varC[(varD==levelD[j]) & !(is.na(varD)) &
                                            is.na(varE) &
                                            (varF==levelF[l]) & !(is.na(varF))]))
              nlevelC<-nlevels(as.factor(varC[(varD==levelD[j]) & !(is.na(varD)) &
                                              is.na(varE) &
                                              (varF==levelF[l]) & !(is.na(varF))]))
              if(length(varC[is.na(varC) &
                             (varD==levelD[j]) & !(is.na(varD)) &
                             is.na(varE) &
                             (varF==levelF[l]) & !(is.na(varF))]) > 0) {
                levelC<-append(levelC,NA)
                nlevelC<-nlevelC + 1
              }
            } else {
              levelC<-levels(as.factor(varC[is.na(varD) &
                                            is.na(varE) &
                                            (varF==levelF[l]) & !(is.na(varF))]))
              nlevelC<-nlevels(as.factor(varC[is.na(varD) &
                                              is.na(varE) &
                                              (varF==levelF[l]) & !(is.na(varF))]))
              if(length(varC[is.na(varC) &
                             is.na(varD) &
                             is.na(varE) &
                             (varF==levelF[l]) & !(is.na(varF))]) > 0) {
                levelC<-append(levelC,NA)
                nlevelC<-nlevelC + 1
              }
            }
          }
        } else {
          if(is.na(levelE[k])==FALSE) {
            if(is.na(levelD[j])==FALSE) {
              levelC<-levels(as.factor(varC[(varD==levelD[j]) & !(is.na(varD)) &
                                            (varE==levelE[k]) & !(is.na(varE)) &
                                            is.na(varF)]))
              nlevelC<-nlevels(as.factor(varC[(varD==levelD[j]) & !(is.na(varD)) &
                                              (varE==levelE[k]) & !(is.na(varE)) &
                                              is.na(varF)]))
              if(length(varC[is.na(varC) &
                             (varD==levelD[j]) & !(is.na(varD)) &
                             (varE==levelE[k]) & !(is.na(varE)) &
                             is.na(varF)]) > 0) {
                levelC<-append(levelC,NA)
                nlevelC<-nlevelC + 1
              }
            } else {
              levelC<-levels(as.factor(varC[is.na(varD) &
                                            (varE==levelE[k]) & !(is.na(varE)) &
                                            is.na(varF)]))
              nlevelC<-nlevels(as.factor(varC[is.na(varD) &
                                              (varE==levelE[k]) & !(is.na(varE)) &
                                              is.na(varF)]))
              if(length(varC[is.na(varC) &
                             is.na(varD) &
                             (varE==levelE[k]) & !(is.na(varE)) &
                             is.na(varF)]) > 0) {
                levelC<-append(levelC,NA)
                nlevelC<-nlevelC + 1
              }
            }
          } else {
            if(is.na(levelD[j])==FALSE) {
              levelC<-levels(as.factor(varC[(varD==levelD[j]) & !(is.na(varD)) &
                                            is.na(varE) &
                                            is.na(varF)]))
              nlevelC<-nlevels(as.factor(varC[(varD==levelD[j]) & !(is.na(varD)) &
                                              is.na(varE) &
                                              is.na(varF)]))
              if(length(varC[is.na(varC) &
                             (varD==levelD[j]) & !(is.na(varD)) &
                             is.na(varE) &
                             is.na(varF)]) > 0) {
                levelC<-append(levelC,NA)
                nlevelC<-nlevelC + 1
              }
            } else {
              levelC<-levels(as.factor(varC[is.na(varD) &
                                            is.na(varE) &
                                            is.na(varF)]))
              nlevelC<-nlevels(as.factor(varC[is.na(varD) &
                                              is.na(varE) &
                                              is.na(varF)]))
              if(length(varC[is.na(varC) &
                             is.na(varD) &
                             is.na(varE) &
                             is.na(varF)]) > 0) {
                levelC<-append(levelC,NA)
                nlevelC<-nlevelC + 1
              }
            }
          }
        }      
        
        for (i in 1:nlevelC) {
          if(is.na(levelF[l])==FALSE) {      
            if(is.na(levelE[k])==FALSE) {
              if(is.na(levelD[j])==FALSE) {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(table(varA[(varC==levelC[i]) &
                                                      (varD==levelD[j]) &
                                                      (varE==levelE[k]) &
                                                      (varF==levelF[l])],
					         varB[(varC==levelC[i]) &
                                                      (varD==levelD[j]) &
                                                      (varE==levelE[k]) &
                                                      (varF==levelF[l])],
                                                 exclude=NULL))
                } else {
                  varA_varB<-as.data.frame(table(varA[is.na(varC) &
                                                      (varD==levelD[j]) &
                                                      (varE==levelE[k]) &
                                                      (varF==levelF[l])],
						 varB[is.na(varC) &
                                                      (varD==levelD[j]) &
                                                      (varE==levelE[k]) &
                                                      (varF==levelF[l])],
                                                 exclude=NULL))
                }
              } else {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(table(varA[(varC==levelC[i]) &
                                                      is.na(varD) &
                                                      (varE==levelE[k]) &
                                                      (varF==levelF[l])],
						 varB[(varC==levelC[i]) &
                                                      is.na(varD) &
                                                      (varE==levelE[k]) &
                                                      (varF==levelF[l])],
                                                 exclude=NULL))

                } else {
                  varA_varB<-as.data.frame(table(varA[is.na(varC) &
                                                      is.na(varD) &
                                                      (varE==levelE[k]) &
                                                      (varF==levelF[l])],
						 varB[is.na(varC) &
                                                      is.na(varD) &
                                                      (varE==levelE[k]) &
                                                      (varF==levelF[l])],
                                                 exclude=NULL))
                }
              }
            } else {
              if(is.na(levelD[j])==FALSE) {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(table(varA[(varC==levelC[i]) &
                                                      (varD==levelD[j]) &
                                                      is.na(varE) &
                                                      (varF==levelF[l])],
						 varB[(varC==levelC[i]) &
                                                      (varD==levelD[j]) &
                                                      is.na(varE) &
                                                      (varF==levelF[l])],
                                                 exclude=NULL))

                } else {
                  varA_varB<-as.data.frame(table(varA[is.na(varC) &
                                                      (varD==levelD[j]) &
                                                      is.na(varE) &
                                                      (varF==levelF[l])],
						 varB[is.na(varC) &
                                                      (varD==levelD[j]) &
                                                      is.na(varE) &
                                                      (varF==levelF[l])],
                                                 exclude=NULL))
                }
              } else {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(table(varA[(varC==levelC[i]) &
                                                      is.na(varD) &
                                                      is.na(varE) &
                                                      (varF==levelF[l])],
						 varB[(varC==levelC[i]) &
                                                      is.na(varD) &
                                                      is.na(varE) &
                                                      (varF==levelF[l])],
                                                 exclude=NULL))
                } else {
                  varA_varB<-as.data.frame(table(varA[is.na(varC) &
                                                      is.na(varD) &
                                                      is.na(varE) &
                                                      (varF==levelF[l])],
						 varB[is.na(varC) &
                                                      is.na(varD) &
                                                      is.na(varE) &
                                                      (varF==levelF[l])],
                                                 exclude=NULL))
                }
              }
            }
          } else {
            if(is.na(levelE[k])==FALSE) {
              if(is.na(levelD[j])==FALSE) {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(table(varA[(varC==levelC[i]) &
                                                      (varD==levelD[j]) &
                                                      (varE==levelE[k]) &
                                                      is.na(varF)],
						 varB[(varC==levelC[i]) &
                                                      (varD==levelD[j]) &
                                                      (varE==levelE[k]) &
                                                      is.na(varF)],
                                                 exclude=NULL))
                } else {
                  varA_varB<-as.data.frame(table(varA[is.na(varC) &
                                                      (varD==levelD[j]) &
                                                      (varE==levelE[k]) &
                                                      is.na(varF)],
						 varB[is.na(varC) &
                                                      (varD==levelD[j]) &
                                                      (varE==levelE[k]) &
                                                      is.na(varF)],
                                                 exclude=NULL))
                }
              } else {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(table(varA[(varC==levelC[i]) &
                                                      is.na(varD) &
                                                      (varE==levelE[k]) &
                                                      is.na(varF)],
						 varB[(varC==levelC[i]) &
                                                      is.na(varD) &
                                                      (varE==levelE[k]) &
                                                      is.na(varF)],
                                                 exclude=NULL))

                } else {
                  varA_varB<-as.data.frame(table(varA[is.na(varC) &
                                                      is.na(varD) &
                                                      (varE==levelE[k]) &
                                                      is.na(varF)],
						 varB[is.na(varC) &
                                                      is.na(varD) &
                                                      (varE==levelE[k]) &
                                                      is.na(varF)],
                                                 exclude=NULL))
                }
              }
            } else {
              if(is.na(levelD[j])==FALSE) {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(table(varA[(varC==levelC[i]) &
                                                      (varD==levelD[j]) &
                                                      is.na(varE) &
                                                      is.na(varF)],
						 varB[(varC==levelC[i]) &
                                                      (varD==levelD[j]) &
                                                      is.na(varE) &
                                                      is.na(varF)],
                                                 exclude=NULL))

                } else {
                  varA_varB<-as.data.frame(table(varA[is.na(varC) &
                                                      (varD==levelD[j]) &
                                                      is.na(varE) &
                                                      is.na(varF)],
						 varB[is.na(varC) &
                                                      (varD==levelD[j]) &
                                                      is.na(varE) &
                                                      is.na(varF)],
                                                 exclude=NULL))
                }
              } else {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(table(varA[(varC==levelC[i]) &
                                                      is.na(varD) &
                                                      is.na(varE) &
                                                      is.na(varF)],
						 varB[(varC==levelC[i]) &
                                                      is.na(varD) &
                                                      is.na(varE) &
                                                      is.na(varF)],
                                                 exclude=NULL))
                } else {
                  varA_varB<-as.data.frame(table(varA[is.na(varC) &
                                                      is.na(varD) &
                                                      is.na(varE) &
                                                      is.na(varF)],
						 varB[is.na(varC) &
                                                      is.na(varD) &
                                                      is.na(varE) &
                                                      is.na(varF)],
                                                 exclude=NULL))
                }
              }
            }
          }

      	  names(varA_varB)[1]<-'varA'
      	  names(varA_varB)[2]<-'varB'

          if(dim(varA_varB)[1] > 0){
        
            varAfreq<-varA_varB
            varAfreq$varA<-as.character(varAfreq$varA)	
            varAfreq$varB<-as.character(varAfreq$varB)
      
            levelA<-levels(as.factor(varAfreq$varA))
            levelB<-levels(as.factor(varAfreq$varB))
            nlevelA<-nlevels(as.factor(varAfreq$varA))
            nlevelB<-nlevels(as.factor(varAfreq$varB))

            varAt<-as.character(c(NA,levelA))
            varBt<-as.character(c(NA,levelB))   
                
            temp<-cbind(merge(varAt,varBt),placeholder=rep(1,(nlevelA+1)*(nlevelB+1)))  
            names(temp)[1]<-'varA'
            names(temp)[2]<-'varB'
 	
            varA.freq.values<-data.frame(temp)
            varA.freq.table<-merge(varAfreq,varA.freq.values, all.x=TRUE)[,c(1,2,3)]

        
            write("####",file=gz1,append=TRUE )          
            write(paste(name.varF,"=",levelF[l],sep=" "),file=gz1,append=TRUE )
            write(paste(name.varE,"=",levelE[k],sep=" "),file=gz1,append=TRUE )
            write(paste(name.varD,"=",levelD[j],sep=" "),file=gz1,append=TRUE )
            write(paste(name.varC,"=",levelC[i],sep=" "),file=gz1,append=TRUE )  
            write(paste(name.varA,name.varB,'freq',sep=','),file=gz1,append=TRUE )
            write.table(varA.freq.table, file=gz1, append=TRUE, quote=FALSE,sep=", ",na=".",  row.names = FALSE, col.names=FALSE)
          }
        }
      }
    }
  }

  write(paste('STOP',paste(name.varA,name.varB,name.varC,name.varD,name.varE,name.varF,sep=" x "),sep=":"),file=gz1,append=TRUE )
  write(" ", file=gz1,append=TRUE)  
  write(" ", file=gz1,append=TRUE)  
  write(" ", file=gz1,append=TRUE)  
}



dep_quant_threshold<-if(dep_q_system=="DSM4_SYMPTOM_COUNT") 1 else 1 
stress_quant_threshold<-if((length(grep("TEST_ALLVARS",INDATA)) > 0) |
                           (length(grep("TEST_DATA",INDATA)) > 0) |
                           (length(grep("LTE_Q",life_stress_q_system)) > 0) ) 2 else 1
child_mal_threshold <- 1
dep_quant_z_threshold<-1
yadult_min_dep_age<-21
gen_5_httlpr_label = "gen5http_S"
rs25531_label = "non_LA_rs25531_haplotype_dosage"


if(life_stress_time_period == "NOT_AVAILABLE") variable_window<-0 else {
    if(life_stress_time_period == "LAST_WEEK") variable_window<-(1/52) else {
        if(life_stress_time_period == "LAST_MONTH") variable_window<-(1/12) else {
            if(life_stress_time_period == "LAST_6MONTHS") variable_window<-(1/2) else {
                if(life_stress_time_period == "LAST_YEAR") variable_window<-1 else {
                    if(life_stress_time_period == "LAST_5YEARS") variable_window<-5 else {            
                        if(life_stress_time_period == "LIFETIME") variable_window<-5000 else {
                            if(length(grep("OTHER", life_stress_time_period)) > 0) {
                                numstring<-sub("OTHER", "", life_stress_time_period, ignore.case =TRUE, fixed=FALSE) 
                                numstring<-sub("(", "", numstring, fixed=TRUE) 
                                numstring<-sub("LAST", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                numstring<-sub("_", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                numstring<-sub("S", "", numstring, ignore.case =TRUE, fixed=FALSE)
                                numstring<-sub(")", "", numstring, fixed=TRUE)

                                if(length(grep("YEAR", life_stress_time_period)) > 0) {
                                    numstring<-sub("YEAR", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                    numstring<-as.numeric(numstring)
                                    variable_window<-if(is.na(numstring)) 1 else numstring
                                } else {
                                    if(length(grep("YR", life_stress_time_period)) > 0) {
                                        numstring<-sub("YR", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                        numstring<-as.numeric(numstring)
                                        variable_window<-if(is.na(numstring)) 1 else numstring
                                    } else {
                                        if (length(grep("Y", life_stress_time_period)) > 0) {
                                            numstring<-sub("Y", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                            numstring<-as.numeric(numstring)
                                            variable_window<-if(is.na(numstring)) 1 else numstring
                                        } else {
                                            if(length(grep("MONTH", life_stress_time_period)) > 0) {
                                                numstring<-sub("MONTH", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                                numstring<-as.numeric(numstring)
                                                variable_window<-if(is.na(numstring)) (1/12) else (numstring / 12)
                                            } else {
                                                if(length(grep("MO", life_stress_time_period)) > 0) {
                                                    numstring<-sub("MO", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                                    numstring<-as.numeric(numstring)
                                                    variable_window<-if(is.na(numstring)) (1/12) else (numstring / 12)
                                                } else {
                                                    if(length(grep("M", life_stress_time_period)) > 0) {
                                                        numstring<-sub("M", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                                        numstring<-as.numeric(numstring)
                                                        variable_window<-if(is.na(numstring)) (1/12) else (numstring / 12) 
                                                    } else {
                                                        if(length(grep("WEEK", life_stress_time_period)) > 0) {
                                                            numstring<-sub("WEEK", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                                            numstring<-as.numeric(numstring)
                                                            variable_window<-if(is.na(numstring)) (1/52) else (numstring / 52) 
                                                        } else {
                                                            if(length(grep("WK", life_stress_time_period)) > 0) {
                                                                numstring<-sub("WK", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                                                numstring<-as.numeric(numstring)
                                                                variable_window<-if(is.na(numstring)) (1/52) else (numstring / 52) 
                                                            } else {
                                                                if(length(grep("W", life_stress_time_period)) > 0) {
                                                                    numstring<-sub("W", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                                                    numstring<-as.numeric(numstring)
                                                                    variable_window<-if(is.na(numstring)) (1/52) else (numstring / 52)
                                                                } else {
                                                                    if(length(grep("DAY", life_stress_time_period)) > 0) {
                                                                        numstring<-sub("DAY", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                                                        numstring<-as.numeric(numstring)
                                                                        variable_window<-if(is.na(numstring)) (1/52) else (numstring / 52)
                                                                    } else {
                                                                        if(length(grep("D", life_stress_time_period)) > 0) {
                                                                            numstring<-sub("D", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                                                            numstring<-as.numeric(numstring)
                                                                            variable_window<-if(is.na(numstring)) (1/52) else (numstring / 52)
                                                                        } else {
                                                                            if(length(grep("HOUR", life_stress_time_period)) > 0) {
                                                                                numstring<-sub("HOUR", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                                                                numstring<-as.numeric(numstring)
                                                                                variable_window<-if(is.na(numstring)) (1/(52*24)) else (numstring / (52 * 24))
                                                                            } else {
                                                                                if(length(grep("H", life_stress_time_period)) > 0) {
                                                                                    numstring<-sub("H", "", numstring, ignore.case =TRUE, fixed=FALSE) 
                                                                                    numstring<-as.numeric(numstring)
                                                                                    variable_window<-if(is.na(numstring)) (1/(52*24)) else (numstring / (52 * 24))
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            rm(numstring)
                        }
                    }
                }
            }
        }
    }
}
### Read in data from file specified in wrapper script, or substitue vector of NA's if missing

data1 <- read.csv(paste(INDIR, INDATA, sep=""), head=TRUE, sep=",", na.strings=".")
n_sample <- length(data1[,1])
num_col<-ncol(data1[,names(data1)=="Age"])


iid    <-data1[,names(data1)=="Id"]

ifelse(class(data1[,names(data1)=="Age"])=="data.frame",
       age    <-c(rep(NA,length(iid))), 
       age    <-as.numeric(data1[,names(data1)=="Age"])) 
ifelse(class(data1[,names(data1)=="Birth_decade"])=="data.frame",
       decade    <-c(rep(NA,length(iid))), 
       decade    <-as.numeric(data1[,names(data1)=="Birth_decade"])) 
ifelse(class(data1[,names(data1)=="Sex"])=="data.frame",
       female <-c(rep(NA,length(iid))) ,
       female <-as.numeric(data1[,names(data1)=="Sex"] ))
ifelse(class(data1[,names(data1)=="Gen5"])=="data.frame",
       gen5http <-c(rep(NA,length(iid))) ,
       gen5http <-as.character(data1[,names(data1)=="Gen5"]))
raw_add_rs25531 <-if(class(data1[,names(data1)=="Rs25531"])=="data.frame") {
    if(class(data1[,names(data1)=="rs25531"])=="data.frame") {
        c(rep(NA,length(iid)))
    } else {
        as.numeric(data1[,names(data1)=="rs25531"])
    }
} else as.numeric(data1[,names(data1)=="Rs25531"])
ifelse(class(data1[,names(data1)=="Dep_dx_curr"])=="data.frame",
       dep_dx_curr  <-c(rep(NA,length(iid))) ,
       dep_dx_curr <-as.numeric(data1[,names(data1)=="Dep_dx_curr"] ))
ifelse(class(data1[,names(data1)=="Dep_dx_life"])=="data.frame",
       dep_dx_life <-c(rep(NA,length(iid))) ,
       dep_dx_life <-as.numeric(data1[,names(data1)=="Dep_dx_life"] ))
ifelse(class(data1[,names(data1)=="Dep_quant_curr"])=="data.frame",
       dep_quant_curr <-c(rep(NA,length(iid))) ,
       dep_quant_curr <-as.numeric(data1[,names(data1)=="Dep_quant_curr"] ))
ifelse(class(data1[,names(data1)=="Dep_quant_life"])=="data.frame",
       dep_quant_life <-c(rep(NA,length(iid))) ,
       dep_quant_life <-as.numeric(data1[,names(data1)=="Dep_quant_life"] ))
ifelse(class(data1[,names(data1)=="Life_stress_exp"])=="data.frame",
       life_stress_exp <-c(rep(NA,length(iid))) ,
       life_stress_exp <-as.numeric(data1[,names(data1)=="Life_stress_exp"] ))
ifelse(class(data1[,names(data1)=="Life_stress_q"])=="data.frame",
       life_stress_quant <-c(rep(NA,length(iid))) ,
       life_stress_quant <-as.numeric(data1[,names(data1)=="Life_stress_q"] ))
ifelse(class(data1[,names(data1)=="Child_mal_exp"])=="data.frame",
       child_mal_exp <-c(rep(NA,length(iid))) ,
       child_mal_exp <-as.numeric(data1[,names(data1)=="Child_mal_exp"] ))
ifelse(class(data1[,names(data1)=="Child_mal_q"])=="data.frame",
       child_mal_quant <-c(rep(NA,length(iid))) ,
       child_mal_quant <-as.numeric(data1[,names(data1)=="Child_mal_q"] ))
ifelse(class(data1[,names(data1)=="CTQ_EA"])=="data.frame",
       e_ab <-c(rep(NA,length(iid))) ,
       e_ab <-as.numeric(data1[,names(data1)=="CTQ_EA"] ))
ifelse(class(data1[,names(data1)=="CTQ_PA"])=="data.frame",
       p_ab <-c(rep(NA,length(iid))) ,
       p_ab <-as.numeric(data1[,names(data1)=="CTQ_PA"] ))
ifelse(class(data1[,names(data1)=="CTQ_SA"])=="data.frame",
       s_ab <-c(rep(NA,length(iid))) ,
       s_ab <-as.numeric(data1[,names(data1)=="CTQ_SA"] ))
ifelse(class(data1[,names(data1)=="CTQ_EN"])=="data.frame",
       e_neg <-c(rep(NA,length(iid))) ,
       e_neg <-as.numeric(data1[,names(data1)=="CTQ_EN"] ))
ifelse(class(data1[,names(data1)=="CTQ_PN"])=="data.frame",
       p_neg <-c(rep(NA,length(iid))) ,
       p_neg <-as.numeric(data1[,names(data1)=="CTQ_PN"] ))
ifelse(class(data1[,names(data1)=="Child_ab_exp"])=="data.frame",
       child_ab_exp <-c(rep(NA,length(iid))),
       child_ab_exp <-as.numeric(data1[,names(data1)=="Child_ab_exp"] ))
ifelse(class(data1[,names(data1)=="Dep_curr_ao"])=="data.frame",
       dep_curr_ao <-c(rep(NA,length(iid))), 
       dep_curr_ao <-as.numeric(data1[,names(data1)=="Dep_curr_ao"] ))
ifelse(class(data1[,names(data1)=="Dep_life_ao"])=="data.frame",
       dep_life_ao <-c(rep(NA,length(iid))), 
       dep_life_ao <-as.numeric(data1[,names(data1)=="Dep_life_ao"] ))
ifelse(class(data1[,names(data1)=="Age_last_stress_before_dep1"])=="data.frame",
       life_stress_alf <-c(rep(NA,length(iid))), 
       life_stress_alf <-as.numeric(data1[,names(data1)=="Age_last_stress_before_dep1"] ))
ifelse(class(data1[,names(data1)=="Age_last_stress_before_dep_curr"])=="data.frame",
       life_stress_alc <-c(rep(NA,length(iid))), 
       life_stress_alc <-as.numeric(data1[,names(data1)=="Age_last_stress_before_dep_curr"] ))


ifelse(class(data1[,names(data1)=="Stress_before_dep1"])=="data.frame",
       sd_timing_before_life <-c(rep(NA,length(iid))), 
       sd_timing_before_life <-as.numeric(data1[,names(data1)=="Stress_before_dep1"] ))
ifelse(class(data1[,names(data1)=="Stress_before_dep_curr"])=="data.frame",
       sd_timing_before_curr <-c(rep(NA,length(iid))), 
       sd_timing_before_curr <-as.numeric(data1[,names(data1)=="Stress_before_dep_curr"] ))
ifelse(class(data1[,names(data1)=="Dep1_5yr_stress_exp"])=="data.frame",
       sd_timing_5yr_life <-c(rep(NA,length(iid))), 
       sd_timing_5yr_life <-as.numeric(data1[,names(data1)=="Dep1_5yr_stress_exp"] ))
ifelse(class(data1[,names(data1)=="Dep_curr_5yr_stress_exp"])=="data.frame",
       sd_timing_5yr_curr <-c(rep(NA,length(iid))), 
       sd_timing_5yr_curr <-as.numeric(data1[,names(data1)=="Dep_curr_5yr_stress_exp"] ))
ifelse(class(data1[,names(data1)=="Dep1_5yr_stress_q"])=="data.frame",
       sd_timing_5yr_life_q <-c(rep(NA,length(iid))), 
       sd_timing_5yr_life_q <-as.numeric(data1[,names(data1)=="Dep1_5yr_stress_q"] ))
ifelse(class(data1[,names(data1)=="Dep_curr_5yr_stress_q"])=="data.frame",
       sd_timing_5yr_curr_q <-c(rep(NA,length(iid))), 
       sd_timing_5yr_curr_q <-as.numeric(data1[,names(data1)=="Dep_curr_5yr_stress_q"] ))
### Write number of non-missing values for each external variable

#out.file.ds_descr<-paste(OUTDIR,SITE,"_frequencies.csv",sep="")
#unlink(out.file.ds_descr)

write("Id", file=gz1,append=TRUE)
#write("Id", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Id"])>0)(length(na.exclude(ifelse(is.character(as.vector(data1[,names(data1)=="Id"])) | (as.numeric(data1[,names(data1)=="Id"])>=0),data1[,names(data1)=="Id"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("ID", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="ID"])>0)(length(na.exclude(ifelse(is.character(as.vector(data1[,names(data1)=="ID"])) | (as.numeric(data1[,names(data1)=="ID"])>=0),data1[,names(data1)=="ID"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Age", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Age"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Age"]>=0,data1[,names(data1)=="Age"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Birth_decade", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Birth_decade"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Birth_decade"]>=0,data1[,names(data1)=="Birth_decade"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Sex", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Sex"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Sex"]>=0,data1[,names(data1)=="Sex"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Gen5", file=gz1,append=TRUE)
write(paste("number defined=",length(grep("-9", data1[,names(data1)=="Gen5"], invert = TRUE)),sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Rs25531", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Rs25531"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Rs25531"]>=0,data1[,names(data1)=="Rs25531"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("rs25531", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="rs25531"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="rs25531"]>=0,data1[,names(data1)=="rs25531"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Dep_dx_curr", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_dx_curr"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_dx_curr"]>=0,data1[,names(data1)=="Dep_dx_curr"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Dep_dx_life", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_dx_life"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_dx_life"]>=0,data1[,names(data1)=="Dep_dx_life"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Dep_quant_curr", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_quant_curr"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_quant_curr"]>=0,data1[,names(data1)=="Dep_quant_curr"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Dep_quant_life", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_quant_life"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_quant_life"]>=0,data1[,names(data1)=="Dep_quant_life"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Life_stress_exp", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Life_stress_exp"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Life_stress_exp"]>=0,data1[,names(data1)=="Life_stress_exp"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Life_stress_q", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Life_stress_q"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Life_stress_q"]>=0,data1[,names(data1)=="Life_stress_q"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Child_mal_exp", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Child_mal_exp"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Child_mal_exp"]>=0,data1[,names(data1)=="Child_mal_exp"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Child_mal_q", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Child_mal_q"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Child_mal_q"]>=0,data1[,names(data1)=="Child_mal_q"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("CTQ_EA", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="CTQ_EA"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="CTQ_EA"]>=0,data1[,names(data1)=="CTQ_EA"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("CTQ_PA", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="CTQ_PA"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="CTQ_PA"]>=0,data1[,names(data1)=="CTQ_PA"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("CTQ_SA", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="CTQ_SA"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="CTQ_SA"]>=0,data1[,names(data1)=="CTQ_SA"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("CTQ_EN", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="CTQ_EN"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="CTQ_EN"]>=0,data1[,names(data1)=="CTQ_EN"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("CTQ_PN", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="CTQ_PN"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="CTQ_PN"]>=0,data1[,names(data1)=="CTQ_PN"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Child_ab_exp", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Child_ab_exp"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Child_ab_exp"]>=0,data1[,names(data1)=="Child_ab_exp"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Dep_curr_ao", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_curr_ao"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_curr_ao"]>=0,data1[,names(data1)=="Dep_curr_ao"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Dep_life_ao", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_life_ao"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_life_ao"]>=0,data1[,names(data1)=="Dep_life_ao"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Age_last_stress_before_dep1", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Age_last_stress_before_dep1"])> 0)(length(na.exclude(ifelse(data1[,names(data1)=="Age_last_stress_before_dep1"]>= -1,data1[,names(data1)=="Age_last_stress_before_dep1"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Age_last_stress_before_dep_curr", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Age_last_stress_before_dep_curr"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Age_last_stress_before_dep_curr"]>= -1,data1[,names(data1)=="Age_last_stress_before_dep_curr"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Stress_before_dep1", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Stress_before_dep1"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Stress_before_dep1"]>=0,data1[,names(data1)=="Stress_before_dep1"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Stress_before_dep_curr", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Stress_before_dep_curr"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Stress_before_dep_curr"]>=0,data1[,names(data1)=="Stress_before_dep_curr"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Dep1_5yr_stress_exp", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep1_5yr_stress_exp"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep1_5yr_stress_exp"]>=0,data1[,names(data1)=="Dep1_5yr_stress_exp"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Dep_curr_5yr_stress_exp", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_curr_5yr_stress_exp"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_curr_5yr_stress_exp"]>=0,data1[,names(data1)=="Dep_curr_5yr_stress_exp"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Dep1_5yr_stress_q", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep1_5yr_stress_q"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep1_5yr_stress_q"]>=0,data1[,names(data1)=="Dep1_5yr_stress_q"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write("Dep_curr_5yr_stress_q", file=gz1,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_curr_5yr_stress_q"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_curr_5yr_stress_q"]>=0,data1[,names(data1)=="Dep_curr_5yr_stress_q"],NA)))) else 0,sep=""), file=gz1,append=TRUE)

write(" ", file=gz1,append=TRUE)
write(" ", file=gz1,append=TRUE)
write(" ", file=gz1,append=TRUE)


### Check that one of the timing variable sets is present in dataset
flag_ao_curr<-0
flag_alsc<-0
flag_ao_life<-0
flag_alsf<-0
flag_Str_before_dep1 <-0       
flag_Str_before_dep_curr <-0   
flag_Dep1_5yr_str_exp <-0      
flag_Dep_curr_5yr_str_exp <-0  
flag_Dep1_5yr_str_quant <-0    
flag_Dep_curr_5yr_str_quant <-0
flag_no_curr_dx <- 1 
flag_no_life_dx <- 1 
for(i in 1:length(names(data1))) {
    if(names(data1)[i] == "Stress_before_dep1"        ) flag_Str_before_dep1 <-1       
    if(names(data1)[i] == "Stress_before_dep_curr"    ) flag_Str_before_dep_curr <-1   
    if(names(data1)[i] == "Dep1_5yr_stress_exp"       ) flag_Dep1_5yr_str_exp <-1      
    if(names(data1)[i] == "Dep_curr_5yr_stress_exp"   ) flag_Dep_curr_5yr_str_exp <-1  
    if(names(data1)[i] == "Dep1_5yr_stress_q"         ) flag_Dep1_5yr_str_quant <-1    
    if(names(data1)[i] == "Dep_curr_5yr_stress_q"     ) flag_Dep_curr_5yr_str_quant <-1
   
    if(names(data1)[i] == "Dep_dx_curr") flag_no_curr_dx<-0 
    if(names(data1)[i] == "Dep_dx_life") flag_no_life_dx<-0
    
    if(names(data1)[i] == "Dep_curr_ao") flag_ao_curr<-1
    if(names(data1)[i] == "Age_last_stress_before_dep1") flag_alsf<-1
    if(names(data1)[i] == "Dep_life_ao") flag_ao_life<-1
    if(names(data1)[i] == "Age_last_stress_before_dep_curr") flag_alsc<-1
}

flag_T_curr<-flag_Str_before_dep_curr + flag_Dep_curr_5yr_str_exp + flag_Dep_curr_5yr_str_quant
flag_T_life<-flag_Str_before_dep1 + flag_Dep1_5yr_str_exp + flag_Dep1_5yr_str_quant


rm(flag_ao_curr,
   flag_alsc,
   flag_ao_life,
   flag_alsf,
   flag_Str_before_dep1,       
   flag_Str_before_dep_curr,   
   flag_Dep1_5yr_str_exp,      
   flag_Dep_curr_5yr_str_exp,  
   flag_Dep1_5yr_str_quant,    
   flag_Dep_curr_5yr_str_quant,
   flag_no_curr_dx, 
   flag_no_life_dx)

 #######
 # age #
 #######
 
replace(age,age<=0,NA)->age
age_cat <- ifelse( 0<=age & age<11, "<=10",
                  ifelse ( 11<=age & age<13, "11 - 12",
                  ifelse ( 13<=age & age<15, "13 - 14",
                  ifelse ( 15<=age & age<18, "15 - 17",
                  ifelse ( 18<=age & age<21, "18 - 20",
                  ifelse ( 21<=age & age<31, "21 - 30",
                  ifelse ( 31<=age & age<51, "31 - 50",
                  ifelse ( 51<=age & age<=65, "51 - 65",
                  ifelse ( 65<age & age<1000, ">65",NA)))))))))
replace(age,age<=0,NA)->age

 ################
 # birth_decade #
 ################
 
replace(decade,decade<1900,NA) -> decade

birth_decade <- ifelse( 1900<=decade & decade<1910, 1900,
                ifelse( 1910<=decade & decade<1920, 1910,
                ifelse( 1920<=decade & decade<1930, 1920,
                ifelse( 1930<=decade & decade<1940, 1930,
                ifelse( 1940<=decade & decade<1950, 1940,
                ifelse( 1950<=decade & decade<1960, 1950,
                ifelse( 1960<=decade & decade<1970, 1960,
                ifelse( 1970<=decade & decade<1980, 1970,
                ifelse( 1980<=decade & decade<1990, 1980,
                ifelse( 1990<=decade & decade<2000, 1990,
                ifelse( 2000<=decade & decade<2010, 2000, NA)))))))))))


 #######
 # sex #
 #######

female <- ifelse((female==0)|(female==1),female,NA)

### list of iid's to include in male-only and female-only analyses
iid_m <- c(rep(NA,length(iid)))
iid_m <- ifelse(female==1,iid,iid_m)
iid_f <- c(rep(NA,length(iid)))
iid_f <- ifelse(female==0,iid,iid_f)

 ############
 # 5-httlpr #
 ############

temp <- ifelse(gen5http=="LL",0,ifelse(
               gen5http=="ll",0,ifelse(	
               gen5http=="SL",1,ifelse(
               gen5http=="sl",1,ifelse(
               gen5http=="LS",1,ifelse(
               gen5http=="ls",1,ifelse(
               gen5http=="SS",2,ifelse(
               gen5http=="ss",2,NA))))))))
add_5http <- as.numeric(temp)

temp2 <- ifelse(gen5http=="LL",0,ifelse(
                gen5http=="ll",0,ifelse(	
                gen5http=="SL",1,ifelse(
                gen5http=="sl",1,ifelse(
                gen5http=="LS",1,ifelse(
                gen5http=="ls",1,ifelse(
                gen5http=="SS",1,ifelse(
                gen5http=="ss",1,NA))))))))
Ldom_5http <- as.numeric(temp2)

temp3 <- ifelse(gen5http=="LL",0,ifelse(
                gen5http=="ll",0,ifelse(	
                gen5http=="SL",0,ifelse(
                gen5http=="sl",0,ifelse(
                gen5http=="LS",0,ifelse(
                gen5http=="ls",0,ifelse(
                gen5http=="SS",1,ifelse(
                gen5http=="ss",1,NA))))))))
Lrec_5http <- as.numeric(temp3)

Ldum1_5http <-  Lrec_5http
Ldum2_5http <-  Ldom_5http
Ldum_5http <- cbind(Ldum1_5http,Ldum2_5http)

L_5http <- (add_5http)	 
rm(temp,temp2,temp3)

genotype_SL<-if(length(levels(as.factor(add_5http)))>0) "YES" else "NO"
haplotype <- c(rep(NA,length(iid)))

 ###########
 # rs25531 #
 ###########

raw_add_rs25531 <- ifelse(((raw_add_rs25531==0)|(raw_add_rs25531==1)|(raw_add_rs25531==2)),raw_add_rs25531,NA)
genotype_rs25531<-if(length(levels(as.factor(raw_add_rs25531)))>0) "YES" else "NO"

### generate warning message if f(G)>=f(A)
if (genotype_rs25531=="YES") {
    num_rs25531_G<-(2*length(na.exclude(raw_add_rs25531[raw_add_rs25531==2])) +
                    length(na.exclude(raw_add_rs25531[raw_add_rs25531==1])))
    num_rs25531_A<-(2*length(na.exclude(raw_add_rs25531[raw_add_rs25531==0])) +
                    length(na.exclude(raw_add_rs25531[raw_add_rs25531==1])))
    err_rs25531<-if( num_rs25531_G/num_rs25531_A > 1) 1 else 0
  

#                           non-LA
#add5 25531 unphased phased add   dom  rec dum1 dum2
# 2     2    SG-SG   absent   2    1    1  rec  dom
# 2     1    SA-SG   absent   2    1    1
# 2     0    SA-SA   S*-S*    2    1    1
# 1     2    LG-SG   absent   2    1    1
# 1     1    LG-SA   LG-S*    2    1    1
# 1     0    LA-SA   LA-S*    1    1    0
# 0     2    LG-LG   LG-LG    2    1    1
# 0     1    LA-LG   LA-LG    1    1    0
# 0     0    LA-LA   LA-LA    0    0    0

    add_rs25531<-ifelse(add_5http==2,
                        ifelse(is.na(raw_add_rs25531),NA,2),
                        ifelse(add_5http==0,
                               raw_add_rs25531,
                               ifelse(add_5http==1,
                                      ifelse(raw_add_rs25531==0,
                                             1,
                                             2),
                                      NA)))
    L_Adom_rs25531<-ifelse(add_rs25531==0,
                           0,
                           ifelse(is.na(add_rs25531),
                                  NA,
                                  1))
    L_Arec_rs25531<-ifelse(add_rs25531==2,
                           1,
                           ifelse(is.na(add_rs25531),
                                  NA,
                                  0))

    L_Adum1_rs25531<-L_Arec_rs25531
    L_Adum2_rs25531<-L_Adom_rs25531   
} else {
    add_rs25531<-rep(NA,length(iid))
    L_Adom_rs25531<-rep(NA,length(iid))
    L_Arec_rs25531<-rep(NA,length(iid))
    L_Adum1_rs25531<-rep(NA,length(iid))
    L_Adum2_rs25531<-rep(NA,length(iid))
}

L_Adum_rs25531<-cbind(L_Adum1_rs25531,L_Adum2_rs25531)



 ########################
 # depression_diagnosis #
 ########################

replace(dep_dx_curr,dep_dx_curr<0,NA)->dep_dx_curr
replace(dep_dx_life,dep_dx_life<0,NA)->dep_dx_life
replace(dep_curr_ao,dep_curr_ao<0,NA)->dep_curr_ao
replace(dep_life_ao,dep_life_ao<0,NA)->dep_life_ao

### test ao<=age if dx, ao=age if control
test1<-ifelse(dep_life_ao<=age,1,0)
test2<-ifelse(dep_curr_ao<=age,1,0)
test1c<-ifelse(dep_dx_life==0,1,0)
test2c<-ifelse(dep_dx_curr==0,1,0)
test1d<-ifelse(dep_dx_life==1,1,0)
test2d<-ifelse(dep_dx_curr==1,1,0)
test1e<-ifelse(dep_life_ao==(age+1),1,0)
test2e<-ifelse(dep_curr_ao==(age+1),1,0)


rm(test1,test1c,test1d,test2,test2c,test2d,test1e,test2e)

 ############################
 # depression_symptom_count #
 ############################

replace(dep_quant_curr,dep_quant_curr<0,NA)->dep_quant_curr
replace(dep_quant_life,dep_quant_life<0,NA)->dep_quant_life


  
 ##################
 # stress_exposed #
 ##################

replace(life_stress_exp,life_stress_exp< 0,NA)->life_stress_exp
### stress age-of-onset = -1 if no stress
replace(life_stress_alf,life_stress_alf< -1,NA)->life_stress_alf
replace(life_stress_alc,life_stress_alc< -1,NA)->life_stress_alc

## passes if no stress (by 2 variables: Age_last_stress_before_dep1 & Life_stress_exp) OR
##           stress exposed w/ 0<=Age_last_stress<=Age
##           (only testing those for whom Life_stress_exp and Age_last_stress_before_dep1 are not missing



 ################
 # stress_count #
 ################

replace(life_stress_quant,life_stress_quant==-9,NA)->life_stress_quant
### check consistency of quantitative and dichotomous life_stress variables, according to insturment used (LTE-Q vs. other)

temp <- data.frame(life_stress_alc,dep_curr_ao,life_stress_alf,dep_life_ao)


 ##########################
 # childhood_maltreatment #
 ##########################

replace(child_mal_exp,child_mal_exp<0,NA)-> child_mal_exp
replace(child_mal_quant,child_mal_quant<0,NA)->child_mal_quant
child_mal_quant2<-c(rep(NA,length(iid)))

### if appropriate CTQ subscores present, calculate child_mal_exp, and 2 quantitative versions (include emotional neglect and emotional abuse... or not.)  Add line to wrapper script indicating which definition used

replace(p_ab,p_ab<5,NA)->p_ab 
replace(p_neg,p_neg<5,NA)->p_neg 
replace(e_ab,e_ab<5,NA)->e_ab 
replace(e_neg,e_neg<5,NA)->e_neg 
replace(s_ab,s_ab<5,NA)->s_ab

temp<-data.frame(e_ab,p_ab,s_ab,e_neg,p_neg)
if ( (defined.in.dataset("p_ab",temp)) &
    (defined.in.dataset("s_ab",temp)) &
    (defined.in.dataset("p_neg",temp)) ) {
    replace(e_ab,e_ab<5,NA)-> e_ab
    replace(p_ab,p_ab<5,NA)-> p_ab
    replace(s_ab,s_ab<5,NA)-> s_ab
    replace(e_neg,e_neg<5,NA)-> e_neg
    replace(p_neg,p_neg<5,NA)-> p_neg

    child_mal_exp<-ifelse(((p_ab >= 10) | (s_ab >= 8) | (p_neg >=13)),1,0)
    child_mal_quant<- p_ab + s_ab + p_neg
    if( (defined.in.dataset("e_ab",temp)) &
       (defined.in.dataset("e_neg",temp)) )  {
        child_mal_quant2<- e_ab + p_ab + s_ab + e_neg + p_neg
        ctq_subtype<-"\"CTQ_5_SUBSCORE\""
    } else ctq_subtype<-"\"CTQ_3_SUBSCORE\""
    child_mal_exp_system<-"CTQ"            
    child_mal_q_system<-"CTQ" 
#    write.table(" ",file=paste(INDIR,wrapper,sep=""),col.names=FALSE,
#                row.names=FALSE,append=TRUE, quote=FALSE)
#    write.table(paste("#child_mal_q_system=",ctq_subtype,sep=""),
#                file=paste(INDIR,wrapper,sep="")
#                ,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
}

 #############################
 # Combined stress defintion #
 ##############################

temp<-data.frame(child_mal_exp,child_mal_quant,p_ab,s_ab,p_neg)

stress_combined_exp_clean<-as.numeric(child_mal_exp|life_stress_exp)

## child_mal_exp is NA and life_stress_exp=1 -> 1
## child_mal_exp is NA and life_stress_exp is NA -> NA
## child_mal_exp is NA and life_stress_exp=0 -> 0
# child_mal_exp is defined and =1 & life_stress_exp is NA ->1
# child_mal_exp is defined and =0 & life_stress_exp is NA -> 0
# child_mal_exp is defined and life_stress_exp is defined -> child|life
stress_combined_exp_messy<-ifelse(is.na(child_mal_exp),   
                                  ifelse(life_stress_exp==1,1,       
                                         ifelse(is.na(life_stress_exp),NA,0)),  
                                  ifelse(is.na(life_stress_exp),ifelse(child_mal_exp==1,1, 
                                                                       0),  
                                         as.numeric(child_mal_exp|life_stress_exp))) 

stress_combined_quant<-(child_mal_quant + life_stress_quant)
stress_combined_quant2<-(child_mal_quant2 + life_stress_quant)


####################################################
# standardize variables and percentiles for stress #
# and depression quantitative variables            #
####################################################

life_stress_quant_z <- life_stress_quant
life_stress_quant_z <- (life_stress_quant_z - mean(life_stress_quant_z,na.rm=TRUE)) / sd(life_stress_quant_z,na.rm=TRUE)

child_mal_quant_z <- child_mal_quant
child_mal_quant_z <- (child_mal_quant_z - mean(child_mal_quant_z,na.rm=TRUE)) / sd(child_mal_quant_z,na.rm=TRUE)

child_mal_quant2_z <- child_mal_quant2
child_mal_quant2_z <- (child_mal_quant2_z - mean(child_mal_quant2_z,na.rm=TRUE)) / sd(child_mal_quant2_z,na.rm=TRUE)

stress_combined_quant_z <- stress_combined_quant
stress_combined_quant_z <- (stress_combined_quant_z - mean(stress_combined_quant_z,na.rm=TRUE)) / sd(stress_combined_quant_z,na.rm=TRUE)

stress_combined_quant2_z <- stress_combined_quant2
stress_combined_quant2_z <- (stress_combined_quant2_z - mean(stress_combined_quant2_z,na.rm=TRUE)) / sd(stress_combined_quant2_z,na.rm=TRUE)

dep_quant_life_z <- dep_quant_life
dep_quant_life_z <- (dep_quant_life_z - mean(dep_quant_life_z,na.rm=TRUE)) / sd(dep_quant_life_z,na.rm=TRUE)

dep_quant_curr_z <- dep_quant_curr
dep_quant_curr_z <- (dep_quant_curr_z - mean(dep_quant_curr_z,na.rm=TRUE)) / sd(dep_quant_curr_z,na.rm=TRUE)

 ###################
 # relative timing #
 ###################

replace(sd_timing_5yr_life_q,sd_timing_5yr_life_q<0,NA)->sd_timing_5yr_life_q
replace(sd_timing_5yr_curr_q,sd_timing_5yr_curr_q<0,NA)->sd_timing_5yr_curr_q

replace(sd_timing_before_curr,sd_timing_before_curr<0,NA)->sd_timing_before_curr
replace(sd_timing_5yr_curr,sd_timing_5yr_curr<0,NA)->sd_timing_5yr_curr 

replace(sd_timing_before_life,sd_timing_before_life<0,NA)->sd_timing_before_life
replace(sd_timing_5yr_life,sd_timing_5yr_life<0,NA)->sd_timing_5yr_life

timing_5yr_curr_q<-c(rep(NA,length(iid)))
timing_5yr_curr<-c(rep(NA,length(iid)))
timing_before_curr<-c(rep(NA,length(iid)))

timing_5yr_life_q<-c(rep(NA,length(iid)))
timing_5yr_life<-c(rep(NA,length(iid)))
timing_before_life<-c(rep(NA,length(iid)))

temp<-data.frame(sd_timing_before_curr,sd_timing_5yr_curr,sd_timing_5yr_curr_q,dep_curr_ao,life_stress_alc,sd_timing_before_life,sd_timing_5yr_life,sd_timing_5yr_life_q,dep_life_ao,life_stress_alf)

### if "sd" timing variables provided, first use those for internal timing variable
if(defined.in.dataset("sd_timing_before_curr",temp)){
    timing_before_curr<-sd_timing_before_curr
}
if(defined.in.dataset("sd_timing_5yr_curr",temp)){
    timing_5yr_curr<-sd_timing_5yr_curr
}
if(defined.in.dataset("sd_timing_5yr_curr_q",temp)){
    timing_5yr_curr_q<-sd_timing_5yr_curr_q
}

if(defined.in.dataset("sd_timing_before_life",temp)){
    timing_before_life<-sd_timing_before_life
}
if(defined.in.dataset("sd_timing_5yr_life",temp)){
    timing_5yr_life<-sd_timing_5yr_life
}
if(defined.in.dataset("sd_timing_5yr_life_q",temp)){
    timing_5yr_life_q<-sd_timing_5yr_life_q
}


### if ages of events variables present, overwrite internal timing variable with calculated intervals

if(defined.in.dataset("life_stress_alc",temp)){
    if(defined.in.dataset("dep_curr_ao", temp)){
## if Age_last_stress_before_dep_curr = -1 (no stress before depression) OR
##    Dep_curr_ao-Age_last_stress_before_dep_curr in [0,5] (Dep_dx_curr cases) OR
##    Dep_curr_ao-Age_last_stress_before_dep_curr in [1,6] (Dep_dx_curr controls-- because
##                                                          ao=age+1)
## --> use life_stress_exp to overwrite timing variables previously based on var28        
        timing_5yr_curr<-ifelse((life_stress_alc == -1),0,
                                ifelse(((0<=(dep_curr_ao-life_stress_alc)) &
                                        ((dep_curr_ao-life_stress_alc)<=5) &
                                        (dep_dx_curr==1)  ) |
                                       ((1<=(dep_curr_ao-life_stress_alc)) &
                                        ((dep_curr_ao-life_stress_alc)<=6) &
                                        (dep_dx_curr==0)),
                                       life_stress_exp,
                                       timing_5yr_curr))

## if Age_last_stress_before_dep_curr = -1 (no stress before depression) OR
##    Dep_curr_ao-Age_last_stress_before_dep_curr in [0,5] (Dep_dx_curr cases OR Dep_quant_curr present) OR
##    Dep_curr_ao-Age_last_stress_before_dep_curr in [1,6] (Dep_dx_curr controls-- because
##                                                          ao=age+1)
## --> use life_stress_quant to overwrite timing variables previously based on var29        
        timing_5yr_curr_q<-ifelse((life_stress_alc == -1),0,
                                  ifelse(((0<=(dep_curr_ao-life_stress_alc)) &
                                          ((dep_curr_ao-life_stress_alc)<=5) &
                                          ((dep_dx_curr ==1) |(!is.na(dep_quant_curr)))) |
                                         ((1<=(dep_curr_ao-life_stress_alc)) &
                                          ((dep_curr_ao-life_stress_alc)<=6) &
                                          (dep_dx_curr==0)),
                                         life_stress_quant,
                                         timing_5yr_curr_q))

## if Age_last_stress_before_dep_curr = -1 (no stress before depression) OR         
##    Dep_curr_ao-Age_last_stress_before_dep_curr in [0,X*] (Dep_dx_curr cases OR Dep_quant_curr present) OR
##    Dep_curr_ao-Age_last_stress_before_dep_curr in [1,X+1] (Dep_dx_curr controls)
## --> 1 to overwrite timing variables previously based on var27        
## * X in years determined from life_stress_time_period in wrapper script      
        timing_before_curr<-ifelse((life_stress_alc == -1) |
                                   ((0<=(dep_curr_ao-life_stress_alc)) &
                                    ((dep_curr_ao-life_stress_alc)<=variable_window) &
                                    ((dep_dx_curr==1) | !is.na(dep_quant_curr)) ) |
                                   ((1<=(dep_curr_ao-life_stress_alc)) &
                                    ((dep_curr_ao-life_stress_alc)<=(variable_window + 1)) &
                                    (dep_dx_curr==0)),
                                   1,
                                   ifelse((((dep_curr_ao-life_stress_alc)<0) |
                                           ((dep_curr_ao-life_stress_alc)>variable_window)),0,
                                          timing_before_curr))
    }
}


if(defined.in.dataset("life_stress_alf",temp)){
    if(defined.in.dataset("dep_life_ao", temp)) {
## if Age_last_stress_before_dep_life = -1 (no stress before depression) OR
##    Dep_life_ao-Age_last_stress_before_dep_life in [0,5] (Dep_dx_life cases) OR
##    Dep_life_ao-Age_last_stress_before_dep_life in [1,6] (Dep_dx_life controls-- because
##                                                          ao=age+1)
## --> use life_stress_exp to overwrite timing variables previously based on var25        
        timing_5yr_life<-ifelse((life_stress_alf == -1),0,
                                ifelse(((0<=(dep_life_ao-life_stress_alf)) &
                                        ((dep_life_ao-life_stress_alf)<=5) &
                                        (dep_dx_life==1)  ) |
                                       ((1<=(dep_life_ao-life_stress_alf)) &
                                        ((dep_life_ao-life_stress_alf)<=6) &
                                        (dep_dx_life==0)),
                                       life_stress_exp,
                                       timing_5yr_life))

## if Age_last_stress_before_dep_life = -1 (no stress before depression) OR
##    Dep_life_ao-Age_last_stress_before_dep_life in [0,5] (Dep_dx_life cases OR Dep_quant_life present) OR
##    Dep_life_ao-Age_last_stress_before_dep_life in [1,6] (Dep_dx_life controls-- because
##                                                          ao=age+1)
## --> use life_stress_quant to overwrite timing variables previously based on var26        
        timing_5yr_life_q<-ifelse((life_stress_alf == -1),0,
                                  ifelse(((0<=(dep_life_ao-life_stress_alf)) &
                                          ((dep_life_ao-life_stress_alf)<=5) &
                                          ((dep_dx_life ==1) |(!is.na(dep_quant_life)))) |
                                         ((1<=(dep_life_ao-life_stress_alf)) &
                                          ((dep_life_ao-life_stress_alf)<=6) &
                                          (dep_dx_life==0)),
                                         life_stress_quant,
                                         timing_5yr_life_q))

## if Age_last_stress_before_dep_life = -1 (no stress before depression) OR         
##    Dep_life_ao-Age_last_stress_before_dep_life in [0,X*] (Dep_dx_life cases OR Dep_quant_life present) OR
##    Dep_life_ao-Age_last_stress_before_dep_life in [1,X+1] (Dep_dx_life controls)
## --> 1 to overwrite timing variables previously based on var24        
## * X in years determined from life_stress_time_period in wrapper script      
        timing_before_life<-ifelse((life_stress_alf == -1) |
                                   ((0<=(dep_life_ao-life_stress_alf)) &
                                    ((dep_life_ao-life_stress_alf)<=variable_window) &
                                    ((dep_dx_life==1) | !is.na(dep_quant_life)) ) |
                                   ((1<=(dep_life_ao-life_stress_alf)) &
                                    ((dep_life_ao-life_stress_alf)<=(variable_window + 1)) &
                                    (dep_dx_life==0)),
                                   1,
                                   ifelse((((dep_life_ao-life_stress_alf)<0) |
                                           ((dep_life_ao-life_stress_alf)>variable_window)),0,
                                          timing_before_life))




#        timing_5yr_life<-ifelse((life_stress_alf == -1),0,
#                                ifelse(((0<=(dep_curr_ao-life_stress_alc)) &
#                                        ((dep_curr_ao-life_stress_alc)<=5) &
#                                        ((dep_dx_curr >0) |
#                                         (dep_quant_curr >= dep_quant_threshold)) ) |
#                                       ((1<=(dep_curr_ao-life_stress_alc)) &
#                                        ((dep_curr_ao-life_stress_alc)<=6) &
#                                        (exists.2.tab(dep_dx_curr >0) |
#                                         exists.2.tab(dep_quant_curr)))
#
#
#                                    (0<=(dep_life_ao-life_stress_alf)) & ((dep_life_ao-life_stress_alf)<=5),
#                                       life_stress_exp,
#                                       timing_5yr_life))
#        timing_5yr_life_q<-ifelse((life_stress_alf == -1),0,
#                                  ifelse(((0<=(dep_curr_ao-life_stress_alc)) &
#                                        ((dep_curr_ao-life_stress_alc)<=5) &
#                                        ((dep_dx_curr >0) |
#                                         (dep_quant_curr >= dep_quant_threshold)) ) |
#                                       ((1<=(dep_curr_ao-life_stress_alc)) &
#                                        ((dep_curr_ao-life_stress_alc)<=6) &
#                                        (exists.2.tab(dep_dx_curr >0) |
#                                         exists.2.tab(dep_quant_curr)))
#
#
#
#                                      (0<=(dep_life_ao-life_stress_alf)) & ((dep_life_ao-life_stress_alf)<=5),
#                                         life_stress_quant,
#                                         timing_5yr_life_q)) 
#        timing_before_life<-ifelse(((0<=(dep_curr_ao-life_stress_alc)) &
#                                        ((dep_curr_ao-life_stress_alc)<=5) &
#                                        ((dep_dx_curr >0) |
#                                         (dep_quant_curr >= dep_quant_threshold)) ) |
#                                       ((1<=(dep_curr_ao-life_stress_alc)) &
#                                        ((dep_curr_ao-life_stress_alc)<=6) &
#                                        (exists.2.tab(dep_dx_curr >0) |
#                                         exists.2.tab(dep_quant_curr)))
#
#            ((life_stress_alf == -1) | ((dep_life_ao-life_stress_alf)>=0 &
#                                                               (dep_life_ao-life_stress_alf)<=variable_window)),1,
#                                   ifelse((((dep_life_ao-life_stress_alf)<0) |
#                                           ((dep_life_ao-life_stress_alf)>variable_window)),0,
#                                          timing_before_life))
    }
}

life_stress_exp_5yr_life<-timing_5yr_life
life_stress_exp_5yr_curr<-timing_5yr_curr
life_stress_quant_5yr_life<-timing_5yr_life_q
life_stress_quant_5yr_curr<-timing_5yr_curr_q

life_stress_quant_5yr_curr_z <- life_stress_quant_5yr_curr
life_stress_quant_5yr_life_z <- life_stress_quant_5yr_life
life_stress_quant_5yr_curr_z <- (life_stress_quant_5yr_curr_z - mean(life_stress_quant_5yr_curr_z,na.rm=TRUE)) / sd(life_stress_quant_5yr_curr_z,na.rm=TRUE)
life_stress_quant_5yr_life_z <- (life_stress_quant_5yr_life_z - mean(life_stress_quant_5yr_life_z,na.rm=TRUE)) / sd(life_stress_quant_5yr_life_z,na.rm=TRUE)

stress_combined_exp_clean_5yr_life<-as.numeric(child_mal_exp|timing_5yr_life)
## child_mal_exp is NA and life_stress_exp_5yr_life=1 -> 1
## child_mal_exp is NA and life_stress_exp_5yr_life is NA -> NA
## child_mal_exp is NA and life_stress_exp_5yr_life=0 -> 0
# child_mal_exp is defined and =1 & life_stress_exp_5yr_life is NA ->1
# child_mal_exp is defined and =0 & life_stress_exp_5yr_life is NA -> 0
# child_mal_exp is defined and life_stress_exp_5yr_life is defined -> child|life_stress_exp_5yr_life
stress_combined_exp_messy_5yr_life<-ifelse(is.na(child_mal_exp),   
                                  ifelse(timing_5yr_life==1,1,       
                                         ifelse(is.na(timing_5yr_life),NA, 
                                                0)),  
                                  ifelse(is.na(timing_5yr_life),ifelse(child_mal_exp==1,1, 
                                                0),  
                                         as.numeric(child_mal_exp|timing_5yr_life))) 

stress_combined_exp_clean_5yr_curr<-as.numeric(child_mal_exp|timing_5yr_curr)
## child_mal_exp is NA and life_stress_exp_5yr_curr=1 -> 1
## child_mal_exp is NA and life_stress_exp_5yr_curr is NA -> NA
## child_mal_exp is NA and life_stress_exp_5yr_curr=0 -> 0
# child_mal_exp is defined and =1 & life_stress_exp_5yr_curr is NA ->1
# child_mal_exp is defined and =0 & life_stress_exp_5yr_curr is NA -> 0
# child_mal_exp is defined and life_stress_exp_5yr_curr is defined -> child|life_stress_exp_5yr_curr
stress_combined_exp_messy_5yr_curr<-ifelse(is.na(child_mal_exp),   
                                  ifelse(timing_5yr_curr==1,1,       
                                         ifelse(is.na(timing_5yr_curr),NA,
                                                0)),  
                                  ifelse(is.na(timing_5yr_curr),ifelse(child_mal_exp==1,1, 
                                                0),  
                                         as.numeric(child_mal_exp|timing_5yr_curr))) 


stress_combined_quant_5yr_curr<-(child_mal_quant + ifelse(is.na(timing_5yr_curr_q>=0),NA,life_stress_quant_5yr_curr))
stress_combined_quant_5yr_life<-(child_mal_quant + ifelse(is.na(timing_5yr_life_q>=0),NA,life_stress_quant_5yr_life))
stress_combined_quant_5yr_curr2<-(child_mal_quant2 + ifelse(is.na(timing_5yr_curr_q>=0),NA,life_stress_quant_5yr_curr))
stress_combined_quant_5yr_life2<-(child_mal_quant2 + ifelse(is.na(timing_5yr_life_q>=0),NA,life_stress_quant_5yr_life))

stress_combined_quant_5yr_curr_z <- stress_combined_quant_5yr_curr
stress_combined_quant_5yr_curr_z <- (stress_combined_quant_5yr_curr_z  -mean(stress_combined_quant_5yr_curr_z,na.rm=TRUE))  /sd(stress_combined_quant_5yr_curr_z,na.rm=TRUE)

stress_combined_quant_5yr_curr2_z <- stress_combined_quant_5yr_curr2
stress_combined_quant_5yr_curr2_z <- (stress_combined_quant_5yr_curr2_z  -mean(stress_combined_quant_5yr_curr2_z,na.rm=TRUE))  /sd(stress_combined_quant_5yr_curr2_z,na.rm=TRUE)

stress_combined_quant_5yr_life_z <- stress_combined_quant_5yr_life
stress_combined_quant_5yr_life_z <- (stress_combined_quant_5yr_life_z  -mean(stress_combined_quant_5yr_life_z,na.rm=TRUE))  /sd(stress_combined_quant_5yr_life_z,na.rm=TRUE)

stress_combined_quant_5yr_life2_z <- stress_combined_quant_5yr_life2
stress_combined_quant_5yr_life2_z <- (stress_combined_quant_5yr_life2_z  -mean(stress_combined_quant_5yr_life2_z,na.rm=TRUE))  /sd(stress_combined_quant_5yr_life2_z,na.rm=TRUE)


 ############################
 # make subsets of datasets #
 # and flags for analyses   #
 ############################

tmp<-ifelse(is.na(age)==FALSE,1,0)
tmp2<-ifelse(is.na(dep_life_ao)==FALSE,1,0)
tmp4<-ifelse(is.na(dep_curr_ao)==FALSE,1,0)

iid_age_narrow_life<- c(rep(NA,length(iid)))
iid_age_narrow_curr <- c(rep(NA,length(iid))) ### will contain ages 21-30 with lifetime onset of depression >=21
###  base on lifetime ao, if available, OR current ao if lifetime ao NOT_AVAILABLE, OR accept all if ao's
###  not defined for individual (Did not use defined.in.dataset which may or may not be wise)
#############################################################################################
###                                                                                       ### 
###  NOTE: if ao's not defined, need to verify it is known no depression prior to age 21  ###
###        in order to include in Caspi replication meta-analyses                         ###
###                                                                                       ### 
#############################################################################################
if(sum(tmp)>0) { ### if age defined
  iid_age_narrow_life<-ifelse(age<=30 &
                              age >=21 &
                              ((((dep_life_ao >= (yadult_min_dep_age+1)) |
                                 (sum(tmp2)==0 & sum(tmp4)==0)) &
                                ((dep_dx_life==0) |
                                 !is.na(dep_quant_life))) |
                               (((dep_life_ao >= (yadult_min_dep_age)) |
                                 (sum(tmp2)==0 & sum(tmp4)==0)) &
                                ((dep_dx_life==1) |
                                 !is.na(dep_quant_life)))),
                              iid,
                              NA)

  iid_age_narrow_curr<-ifelse(age<=30 &
                              age >=21 &
                              ((((dep_curr_ao >= (yadult_min_dep_age+1)) |
                                 (sum(tmp2)==0 & sum(tmp4)==0)) &
                                ((dep_dx_curr==0) |
                                 !is.na(dep_quant_curr))) |
                               (((dep_curr_ao >= (yadult_min_dep_age)) |
                                 (sum(tmp2)==0 & sum(tmp4)==0)) &
                                ((dep_dx_curr==1) |
                                 !is.na(dep_quant_curr)))),
                              iid,
                              NA)
}


 ##########################
 # Dichotomous Depression #
 ###########################

#### list of iids with Depression before Stress (NA is Depression is AFTER Stress)
iid_exclude_curr<-c(rep(NA,length(iid)))
iid_exclude_life<-c(rep(NA,length(iid)))
iid_exclude_life<-ifelse((is.na(timing_before_life)  | (timing_before_life==0)),iid,NA)
iid_exclude_curr<-ifelse((is.na(timing_before_curr)  | (timing_before_curr==0)),iid,NA)

### list of iids without Stress *known* (exposed OR non-exposed) in 5yrs preceding Depression/interview
iid_exclude_curr_5yr<-c(rep(NA,length(iid)))
iid_exclude_life_5yr<-c(rep(NA,length(iid)))
iid_exclude_life_5yr<-ifelse(is.na(timing_5yr_life) & is.na(timing_5yr_life_q),iid,NA)
iid_exclude_curr_5yr<-ifelse(is.na(timing_5yr_curr) & is.na(timing_5yr_curr_q),iid,NA)


### list of those whose current depression is not the first depression-- will be removed in hybrid variable set
iid_DD_exclude2<-ifelse((((dep_dx_life==1) & (dep_dx_curr==0)) | (dep_curr_ao > dep_life_ao)),iid,NA)  
CME_avail<-ifelse(is.na(child_mal_exp)==FALSE,1,0)  ### Child_mal exposure information available

### if appropriate timing available, put dep_dx_curr into DD_ variables
DD_5yr<-ifelse(is.na(timing_5yr_curr)==FALSE|is.na(timing_5yr_curr_q)==FALSE,dep_dx_curr,NA)
DD_before<-ifelse(timing_before_curr,dep_dx_curr,NA)
DD<-dep_dx_curr

### if appropriate timing available, put dep_dx_life into DD_ variables
DD_life_5yr<-ifelse(is.na(timing_5yr_life)==FALSE|is.na(timing_5yr_life_q)==FALSE,dep_dx_life,NA)
DD_life_before<-ifelse(timing_before_life,dep_dx_life,NA)
DD_life<-dep_dx_life

### if appropriate timing available, put dep_dx_curr into DD_curr variables
DD_curr_5yr<-ifelse(is.na(timing_5yr_curr)==FALSE|is.na(timing_5yr_curr_q)==FALSE,dep_dx_curr,NA)
DD_curr_before<-ifelse(timing_before_curr==1,dep_dx_curr,NA)
DD_curr<-dep_dx_curr

### similar variables, but if child_mal_exp=1, add dep_dx_curr value in for some missing data (These variables are for the analyses for which combined stress definition is used)
DD_5yr_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_dx_curr,DD_5yr)
DD_before_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_dx_curr,DD_before)

### add those with child_mal_exp=1 back into the depression definition for combined stress definition analyses
DD_life_5yr_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_dx_life,DD_life_5yr)
DD_life_before_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_dx_life,DD_life_before)

### if child_mal exposure=1, make sure those subjects included in DD_curr_ variables for combined stress analyses
DD_curr_5yr_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_dx_curr,DD_curr_5yr)
DD_curr_before_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_dx_curr,DD_curr_before)

### remove those who had prior depression
DD_5yr<-replace(DD_5yr,(is.na(iid_DD_exclude2)==FALSE),NA)
DD_before<-replace(DD_before,(is.na(iid_DD_exclude2)==FALSE),NA) 	
DD<-replace(DD,is.na(iid_DD_exclude2)==FALSE,NA)
DD_5yr_comb<-replace(DD_5yr_comb,(is.na(iid_DD_exclude2)==FALSE),NA)
DD_before_comb<-replace(DD_before_comb,(is.na(iid_DD_exclude2)==FALSE),NA) 	


 # QD #
 ######
iid_QD_exclude2<-iid_DD_exclude2

### if appropriate timing available, put dep_dx_curr into QD_ variables
QD_5yr<-ifelse(is.na(timing_5yr_curr)==FALSE|is.na(timing_5yr_curr_q)==FALSE,dep_quant_curr,NA)
QD_before<-ifelse(timing_before_curr==1,dep_quant_curr,NA)
QD<-dep_quant_curr

QD_life_5yr<-ifelse(is.na(timing_5yr_life)==FALSE|is.na(timing_5yr_life_q)==FALSE,dep_quant_life,NA)
QD_life_before<-ifelse(timing_before_life,dep_quant_life,NA)
QD_life<-dep_quant_life

QD_curr_5yr<-ifelse(is.na(timing_5yr_curr)==FALSE|is.na(timing_5yr_curr_q)==FALSE,dep_quant_curr,NA)
QD_curr_before<-ifelse(timing_before_curr==1,dep_quant_curr,NA)
QD_curr<-dep_quant_curr

QD_5yr_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_quant_curr,QD_5yr)
QD_before_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_quant_curr,QD_before)

QD_life_5yr_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_quant_life,QD_life_5yr)
QD_life_before_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_quant_life,QD_life_before)

QD_curr_5yr_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_quant_curr,QD_curr_5yr)
QD_curr_before_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_quant_curr,QD_curr_before)

QD_5yr<-replace(QD_5yr,(is.na(iid_QD_exclude2)==FALSE),NA)
QD_before<-replace(QD_before,(is.na(iid_QD_exclude2)==FALSE),NA)
QD<-replace(QD,is.na(iid_QD_exclude2)==FALSE,NA)
QD_5yr_comb<-replace(QD_5yr_comb,(is.na(iid_QD_exclude2)==FALSE),NA)
QD_before_comb<-replace(QD_before_comb,(is.na(iid_QD_exclude2)==FALSE),NA)


 # QDz #
 ######
iid_QDz_exclude2<-iid_DD_exclude2

### if appropriate timing available, put dep_dx_curr into QDz_ variables
QDz_5yr<-ifelse(is.na(timing_5yr_curr)==FALSE|is.na(timing_5yr_curr_q)==FALSE,dep_quant_curr_z,NA)
QDz_before<-ifelse(timing_before_curr==1,dep_quant_curr_z,NA)
QDz<-dep_quant_curr_z

QDz_life_5yr<-ifelse(is.na(timing_5yr_life)==FALSE|is.na(timing_5yr_life_q)==FALSE,dep_quant_life_z,NA)
QDz_life_before<-ifelse(timing_before_life,dep_quant_life_z,NA)
QDz_life<-dep_quant_life_z

QDz_curr_5yr<-ifelse(is.na(timing_5yr_curr)==FALSE|is.na(timing_5yr_curr_q)==FALSE,dep_quant_curr_z,NA)
QDz_curr_before<-ifelse(timing_before_curr==1,dep_quant_curr_z,NA)
QDz_curr<-dep_quant_curr_z

QDz_5yr_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_quant_curr_z,QDz_5yr)
QDz_before_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_quant_curr_z,QDz_before)

QDz_life_5yr_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_quant_life_z,QDz_life_5yr)
QDz_life_before_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_quant_life_z,QDz_life_before)

QDz_curr_5yr_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_quant_curr_z,QDz_curr_5yr)
QDz_curr_before_comb<-ifelse(CME_avail==1 & child_mal_exp==1,dep_quant_curr_z,QDz_curr_before)

QDz_5yr<-replace(QDz_5yr,(is.na(iid_QDz_exclude2)==FALSE),NA)
QDz_before<-replace(QDz_before,(is.na(iid_QDz_exclude2)==FALSE),NA)
QDz<-replace(QDz,is.na(iid_QDz_exclude2)==FALSE,NA)
QDz_5yr_comb<-replace(QDz_5yr_comb,(is.na(iid_QDz_exclude2)==FALSE),NA)
QDz_before_comb<-replace(QDz_before_comb,(is.na(iid_QDz_exclude2)==FALSE),NA)


dep_dx<-dep_dx_curr
dep_dx<-replace(dep_dx,is.na(iid_DD_exclude2)==FALSE,NA)
dep_q<-dep_quant_curr
dep_q<-replace(dep_q,is.na(iid_QD_exclude2)==FALSE,NA)
dep_q_z<-dep_quant_curr_z
dep_q_z<-replace(dep_q_z,is.na(iid_QDz_exclude2)==FALSE,NA)

 ##################################
 # make dataframe for subroutines #
 # clean up                       #
 ##################################
if(1) {
sd_5htt <- data.frame(iid,iid_age_narrow_life,iid_age_narrow_curr,iid_m,iid_f,iid_exclude_curr,iid_exclude_life,iid_exclude_curr_5yr,iid_exclude_life_5yr,female,age,age_cat,birth_decade,add_5http,Ldom_5http,Lrec_5http,Ldum1_5http,Ldum2_5http,add_rs25531,L_Adom_rs25531,L_Arec_rs25531,L_Adum1_rs25531,L_Adum2_rs25531,haplotype,DD,DD_before,DD_5yr,DD_curr,DD_curr_before,DD_curr_5yr,DD_life,DD_life_before,DD_life_5yr,QD,QD_before,QD_5yr,QD_curr,QD_curr_before,QD_curr_5yr,QD_life,QD_life_before,QD_life_5yr,QDz,QDz_before,QDz_5yr,QDz_curr,QDz_curr_before,QDz_curr_5yr,QDz_life,QDz_life_before,QDz_life_5yr,life_stress_exp,life_stress_quant,life_stress_quant_z,life_stress_exp_5yr_life,life_stress_exp_5yr_curr,life_stress_quant_5yr_curr,life_stress_quant_5yr_life,life_stress_quant_5yr_curr_z,life_stress_quant_5yr_life_z,child_mal_exp,child_mal_quant,child_mal_quant2,child_mal_quant_z,child_mal_quant2_z,stress_combined_exp_clean,stress_combined_exp_messy,stress_combined_quant,stress_combined_quant2,stress_combined_quant_z,stress_combined_quant2_z,stress_combined_exp_clean_5yr_life,stress_combined_exp_messy_5yr_life,stress_combined_exp_clean_5yr_curr,stress_combined_exp_messy_5yr_curr,stress_combined_quant_5yr_life,stress_combined_quant_5yr_life2,stress_combined_quant_5yr_life_z,stress_combined_quant_5yr_life2_z,stress_combined_quant_5yr_curr,stress_combined_quant_5yr_curr2,stress_combined_quant_5yr_curr_z,stress_combined_quant_5yr_curr2_z,dep_dx,dep_dx_curr,dep_dx_life,dep_q,dep_quant_curr,dep_quant_life,dep_q_z,dep_quant_curr_z,dep_quant_life_z,dep_curr_ao,dep_life_ao,life_stress_alf,life_stress_alc,timing_5yr_life,timing_5yr_curr,timing_5yr_curr_q,timing_5yr_life_q,timing_before_life,timing_before_curr,raw_add_rs25531)                                                          

nameslist<-c("iid","iid_age_narrow_life","iid_age_narrow_curr","iid_m","iid_f","iid_exclude_curr","iid_exclude_life","iid_exclude_curr_5yr","iid_exclude_life_5yr","female","age","age_cat","birth_decade","add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531","haplotype","DD","DD_before","DD_5yr","DD_curr","DD_curr_before","DD_curr_5yr","DD_life","DD_life_before","DD_life_5yr","QD","QD_before","QD_5yr","QD_curr","QD_curr_before","QD_curr_5yr","QD_life","QD_life_before","QD_life_5yr","QDz","QDz_before","QDz_5yr","QDz_curr","QDz_curr_before","QDz_curr_5yr","QDz_life","QDz_life_before","QDz_life_5yr","life_stress_exp","life_stress_quant","life_stress_quant_z","life_stress_exp_5yr_life","life_stress_exp_5yr_curr","life_stress_quant_5yr_curr","life_stress_quant_5yr_life","life_stress_quant_5yr_curr_z","life_stress_quant_5yr_life_z","child_mal_exp","child_mal_quant","child_mal_quant2","child_mal_quant_z","child_mal_quant2_z","stress_combined_exp_clean","stress_combined_exp_messy","stress_combined_quant","stress_combined_quant2","stress_combined_quant_z","stress_combined_quant2_z","stress_combined_exp_clean_5yr_life","stress_combined_exp_messy_5yr_life","stress_combined_exp_clean_5yr_curr","stress_combined_exp_messy_5yr_curr","stress_combined_quant_5yr_life","stress_combined_quant_5yr_life2","stress_combined_quant_5yr_life_z","stress_combined_quant_5yr_life2_z","stress_combined_quant_5yr_curr","stress_combined_quant_5yr_curr2","stress_combined_quant_5yr_curr_z","stress_combined_quant_5yr_curr2_z","dep_dx","dep_dx_curr","dep_dx_life","dep_q","dep_quant_curr","dep_quant_life","dep_q_z","dep_quant_curr_z","dep_quant_life_z","dep_curr_ao","dep_life_ao","life_stress_alf","life_stress_alc","timing_5yr_life","timing_5yr_curr","timing_5yr_curr_q","timing_5yr_life_q","timing_before_life","timing_before_curr","raw_add_rs25531")

sd_5htt_ya_life<- sd_5htt[is.na(iid_age_narrow_life)==FALSE,]
sd_5htt_ya_curr<- sd_5htt[is.na(iid_age_narrow_curr)==FALSE,]
names(sd_5htt_ya_life)<-nameslist
names(sd_5htt_ya_curr)<-nameslist


sd_5htt_other_life<- sd_5htt[is.na(iid_exclude_life),]
names(sd_5htt_other_life)<-nameslist

sd_5htt_other_curr<- sd_5htt[is.na(iid_exclude_curr),]
names(sd_5htt_other_curr)<-nameslist

sd_5htt_other_life_5yr<- sd_5htt[is.na(iid_exclude_life_5yr),]
names(sd_5htt_other_life_5yr)<-nameslist

sd_5htt_other_curr_5yr<- sd_5htt[is.na(iid_exclude_curr_5yr),]
names(sd_5htt_other_curr_5yr)<-nameslist

sd_5htt_combined<- data.frame(iid,
                              iid_age_narrow_life,
                              iid_age_narrow_curr,
                              iid_m,
                              iid_f,
                              iid_exclude_curr,
                              iid_exclude_life,
                              iid_exclude_curr_5yr,
                              iid_exclude_life_5yr,
                              female,
                              age,
                              age_cat,
                              birth_decade,
                              add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              Ldum1_5http,
                              Ldum2_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              L_Adum1_rs25531,
                              L_Adum2_rs25531,
                              haplotype,
                              DD,
                              DD_before_comb,
                              DD_5yr_comb,
                              DD_curr,
                              DD_curr_before_comb,
                              DD_curr_5yr_comb,
                              DD_life,
                              DD_life_before_comb,
                              DD_life_5yr_comb,
                              QD,
                              QD_before_comb,
                              QD_5yr_comb,
                              QD_curr,
                              QD_curr_before_comb,
                              QD_curr_5yr_comb,
                              QD_life,
                              QD_life_before_comb,
                              QD_life_5yr_comb,
                              QDz,
                              QDz_before_comb,
                              QDz_5yr_comb,
                              QDz_curr,
                              QDz_curr_before_comb,
                              QDz_curr_5yr_comb,
                              QDz_life,
                              QDz_life_before_comb,
                              QDz_life_5yr_comb,
                              life_stress_exp,
                              life_stress_quant,
                              life_stress_quant_z,
                              life_stress_exp_5yr_life,
                              life_stress_exp_5yr_curr,
                              life_stress_quant_5yr_curr,
                              life_stress_quant_5yr_life,
                              life_stress_quant_5yr_curr_z,
                              life_stress_quant_5yr_life_z,
                              child_mal_exp,
                              child_mal_quant,
                              child_mal_quant2,
                              child_mal_quant_z,
                              child_mal_quant2_z,
                              stress_combined_exp_clean,
                              stress_combined_exp_messy,
                              stress_combined_quant,
                              stress_combined_quant2,
                              stress_combined_quant_z,
                              stress_combined_quant2_z,
                              stress_combined_exp_clean_5yr_life,
                              stress_combined_exp_messy_5yr_life,
                              stress_combined_exp_clean_5yr_curr,
                              stress_combined_exp_messy_5yr_curr,
                              stress_combined_quant_5yr_life,
                              stress_combined_quant_5yr_life2,
                              stress_combined_quant_5yr_life_z,
                              stress_combined_quant_5yr_life2_z,
                              stress_combined_quant_5yr_curr,
                              stress_combined_quant_5yr_curr2,
                              stress_combined_quant_5yr_curr_z,
                              stress_combined_quant_5yr_curr2_z,
                              dep_dx,
                              dep_dx_curr,
                              dep_dx_life,
                              dep_q,
                              dep_quant_curr,
                              dep_quant_life,
                              dep_q_z,
                              dep_quant_curr_z,
                              dep_quant_life_z)
nameslist_comb<-c("iid","iid_age_narrow_life","iid_age_narrow_curr","iid_m","iid_f","iid_exclude_curr","iid_exclude_life","iid_exclude_curr_5yr","iid_exclude_life_5yr","female","age","age_cat","birth_decade","add_5http","Ldom_5http","Lrec_5http","Ldum1_5http","Ldum2_5http","add_rs25531","L_Adom_rs25531","L_Arec_rs25531","L_Adum1_rs25531","L_Adum2_rs25531","haplotype","DD","DD_before","DD_5yr","DD_curr","DD_curr_before","DD_curr_5yr","DD_life","DD_life_before","DD_life_5yr","QD","QD_before","QD_5yr","QD_curr","QD_curr_before","QD_curr_5yr","QD_life","QD_life_before","QD_life_5yr","QDz","QDz_before","QDz_5yr","QDz_curr","QDz_curr_before","QDz_curr_5yr","QDz_life","QDz_life_before","QDz_life_5yr","life_stress_exp","life_stress_quant","life_stress_quant_z","life_stress_exp_5yr_life","life_stress_exp_5yr_curr","life_stress_quant_5yr_curr","life_stress_quant_5yr_life","life_stress_quant_5yr_curr_z","life_stress_quant_5yr_life_z","child_mal_exp","child_mal_quant","child_mal_quant2","child_mal_quant_z","child_mal_quant2_z","stress_combined_exp_clean","stress_combined_exp_messy","stress_combined_quant","stress_combined_quant2","stress_combined_quant_z","stress_combined_quant2_z","stress_combined_exp_clean_5yr_life","stress_combined_exp_messy_5yr_life","stress_combined_exp_clean_5yr_curr","stress_combined_exp_messy_5yr_curr","stress_combined_quant_5yr_life","stress_combined_quant_5yr_life2","stress_combined_quant_5yr_life_z","stress_combined_quant_5yr_life2_z","stress_combined_quant_5yr_curr","stress_combined_quant_5yr_curr2","stress_combined_quant_5yr_curr_z","stress_combined_quant_5yr_curr2_z","dep_dx","dep_dx_curr","dep_dx_life","dep_q","dep_quant_curr","dep_quant_life","dep_q_z","dep_quant_curr_z","dep_quant_life_z")
names(sd_5htt_combined)<-nameslist_comb

sd_5htt_other_life_ya<- sd_5htt[(is.na(iid_exclude_life)) & (is.na(iid_age_narrow_life)==FALSE),]
names(sd_5htt_other_life_ya)<-nameslist

sd_5htt_other_curr_ya<- sd_5htt[(is.na(iid_exclude_curr)) & (is.na(iid_age_narrow_curr)==FALSE),]
names(sd_5htt_other_curr_ya)<-nameslist

sd_5htt_other_life_5yr_ya<- sd_5htt[(is.na(iid_exclude_life_5yr)) & (is.na(iid_age_narrow_life)==FALSE),]
names(sd_5htt_other_life_5yr_ya)<-nameslist

sd_5htt_other_curr_5yr_ya<- sd_5htt[(is.na(iid_exclude_curr_5yr)) & (is.na(iid_age_narrow_curr)==FALSE),]
names(sd_5htt_other_curr_5yr_ya)<-nameslist



if(1) {
rm(data1,age,age_cat,birth_decade,decade,female,child_ab_exp,child_mal_exp,child_mal_quant,child_mal_quant_z,child_mal_quant2,child_mal_quant2_z,e_ab,e_neg,p_ab,p_neg,s_ab,life_stress_alc,life_stress_alf,life_stress_exp,life_stress_exp_5yr_curr,life_stress_exp_5yr_life,life_stress_quant,life_stress_quant_5yr_curr,life_stress_quant_5yr_life,life_stress_quant_z,life_stress_quant_5yr_curr_z,life_stress_quant_5yr_life_z,sd_timing_5yr_curr_q,sd_timing_5yr_life_q,sd_timing_5yr_curr,sd_timing_5yr_life,sd_timing_before_curr,sd_timing_before_life,timing_5yr_curr,timing_5yr_curr_q,timing_5yr_life,timing_5yr_life_q,timing_before_curr,timing_before_life,stress_combined_exp_clean,stress_combined_exp_clean_5yr_curr,stress_combined_exp_clean_5yr_life,stress_combined_exp_messy,stress_combined_exp_messy_5yr_curr,stress_combined_exp_messy_5yr_life,stress_combined_quant,stress_combined_quant_5yr_curr,stress_combined_quant_5yr_curr_z,stress_combined_quant_5yr_curr2,stress_combined_quant_5yr_curr2_z,stress_combined_quant_5yr_life,stress_combined_quant_5yr_life_z,stress_combined_quant_5yr_life2,stress_combined_quant_5yr_life2_z,stress_combined_quant_z,stress_combined_quant2,stress_combined_quant2_z,CME_avail,DD,DD_5yr,DD_5yr_comb,DD_before,DD_before_comb,DD_curr,DD_curr_5yr,DD_curr_5yr_comb,DD_curr_before,DD_curr_before_comb,DD_life,DD_life_5yr,DD_life_5yr_comb,DD_life_before,DD_life_before_comb,QD,QD_5yr,QD_5yr_comb,QD_before,QD_before_comb,QD_curr,QD_curr_5yr,QD_curr_5yr_comb,QD_curr_before,QD_curr_before_comb,QD_life,QD_life_5yr,QD_life_5yr_comb,QD_life_before,QD_life_before_comb,QDz,QDz_5yr,QDz_5yr_comb,QDz_before,QDz_before_comb,QDz_curr,QDz_curr_5yr,QDz_curr_5yr_comb,QDz_curr_before,QDz_curr_before_comb,QDz_life,QDz_life_5yr,QDz_life_5yr_comb,QDz_life_before,QDz_life_before_comb,dep_curr_ao,dep_dx,dep_dx_curr,dep_dx_life,dep_life_ao,dep_q,dep_q_z,dep_quant_curr,dep_quant_curr_z,dep_quant_life,dep_quant_life_z,iid,iid_age_narrow_life,iid_age_narrow_curr,iid_DD_exclude2,iid_exclude_curr,iid_exclude_life,iid_exclude_curr_5yr,iid_exclude_life_5yr,iid_f,iid_m,iid_QD_exclude2,iid_QDz_exclude2,gen5http,L_5http,add_5http,Ldom_5http,Ldum_5http,Ldum1_5http,Ldum2_5http,Lrec_5http,raw_add_rs25531,add_rs25531,L_Adom_rs25531,L_Adum_rs25531,L_Adum1_rs25531,L_Adum2_rs25531,L_Arec_rs25531,haplotype,n_sample,temp,tmp,tmp2,tmp4,dep_quant_threshold,stress_quant_threshold,dep_quant_z_threshold,yadult_min_dep_age)
}

attach(sd_5htt_combined)
sd_5htt_combined_ya_life<-sd_5htt_combined[is.na(iid_age_narrow_life)==FALSE,]
names(sd_5htt_combined_ya_life)<-nameslist_comb
sd_5htt_combined_ya_curr<-sd_5htt_combined[is.na(iid_age_narrow_curr)==FALSE,]
names(sd_5htt_combined_ya_curr)<-nameslist_comb
detach(sd_5htt_combined)


attach(sd_5htt)
SNPs <-data.frame(if(genotype_SL=="YES") add_5http else c(rep(NA,length(iid))),if(genotype_rs25531=="YES") add_rs25531 else c(rep(NA,length(iid))))

SNPs.label<-if(genotype_SL=="YES") if(genotype_rs25531=="YES") c(gen_5_httlpr_label,rs25531_label) else c(gen_5_httlpr_label) else if(genotype_rs25531=="YES") c(rs25531_label) else NA
  
nsnps <- length(SNPs)
detach(sd_5htt)
} ########


attach(sd_5htt)



 ############### 
 # Frequencies #		 
 ############### 


dep_dx_label<-if(dep_dx_system == "DSM4") 'DSM-IV MDD Diagnosis' else
if(dep_dx_system == "ICD10") 'ICD10 MDD Diagnosis' else
'Depression Diagnosis'

dep_q_label<-if(dep_q_system == "DSM4_SYMPTOM_COUNT")
               'DSM-IV MDD Symptom Count' else
if(dep_q_system == "ICD10 SYMPTOM COUNT") 'ICD10 MDD Symptom Count' else
'Depression Symptom Count'


#depression_dx<-c(rep(NA, length(iid)))
depression_dx<-numeric(0)
depression_dx_label<-character(0)
#depression_dx_label<-""
if(exists.to.tab("dep_dx_curr")) {
  depression_dx<-replace(depression_dx, 1:length(iid), dep_dx_curr)
  depression_dx_label<-paste("current",dep_dx_label,"raw",sep=" ") 
}
if(exists.to.tab("dep_dx_life")) {
  if(exists.to.tab("dep_dx_curr")) {
    depression_dx<-cbind(dep_dx_curr,dep_dx_life)
    depression_dx_label<-append(depression_dx_label,
                                paste("lifetime",dep_dx_label,"raw",sep=" "))
  }
  else {
    depression_dx<-replace(depression_dx, 1:length(iid), dep_dx_life)
    depression_dx_label<-paste("lifetime",dep_dx_label,"raw",sep=" ")
  }
}

if(exists.to.tab("dep_dx")) {
  depression_dx<-cbind(depression_dx,dep_dx)
  depression_dx_label<-append(depression_dx_label,paste("current",dep_dx_label,"in sample w/ npd_controls, raw",sep=" "))
}
if(exists.to.tab("DD_curr")) {
  depression_dx<-cbind(depression_dx,DD_curr)
  depression_dx_label<-append(depression_dx_label,paste("current",dep_dx_label,"(stress timing information not required)",sep=" "))
}
if(exists.to.tab("DD_life")) {
  depression_dx<-cbind(depression_dx,DD_life)
  depression_dx_label<-append(depression_dx_label,paste("lifetime",dep_dx_label,"(stress timing information not required)",sep=" "))
}
if(exists.to.tab("DD")) {
  depression_dx<-cbind(depression_dx,DD)
  depression_dx_label<-append(depression_dx_label,paste("current",dep_dx_label,"in sample w/ npd_controls (stress timing information not required)",sep=" "))
}
if(exists.to.tab("DD_curr_5yr")) {
  depression_dx<-cbind(depression_dx,DD_curr_5yr)
  depression_dx_label<-append(depression_dx_label,paste("current",dep_dx_label,"(stress in preceding 5 years)",sep=" "))
}
if(exists.to.tab("DD_curr_before")) {
  depression_dx<-cbind(depression_dx,DD_curr_before)
  depression_dx_label<-append(depression_dx_label,paste("current",dep_dx_label,"(stress preceding)",sep=" "))
}
if(exists.to.tab("DD_life_5yr")) {
  depression_dx<-cbind(depression_dx,DD_life_5yr)
  depression_dx_label<-append(depression_dx_label,paste("lifetime",dep_dx_label,"(stress in preceding 5 years)",sep=" "))
}
if(exists.to.tab("DD_life_before")) {
  depression_dx<-cbind(depression_dx,DD_life_before)
  depression_dx_label<-append(depression_dx_label,paste("lifetime",dep_dx_label,"(stress preceding)",sep=" "))
}
if(exists.to.tab("DD_5yr")) {
  depression_dx<-cbind(depression_dx,DD_5yr)
  depression_dx_label<-append(depression_dx_label,paste("current",dep_dx_label,"in sample w/ npd_controls (stress in preceding 5 years)",sep=" "))
}
if(exists.to.tab("DD_before")) {
  depression_dx<-cbind(depression_dx,DD_before)
  depression_dx_label<-append(depression_dx_label,paste("current",dep_dx_label,"in sample w/ npd_controls (stress preceding)",sep=" "))
}


depression_q<-numeric(0)
depression_q_label<-character(0)
#depression_q<-c(rep(NA, length(iid)))
#depression_q_label<-""
if(exists.to.tab("dep_quant_curr")) {
  depression_q<-replace(depression_q, 1:length(iid),dep_quant_curr)
  depression_q_label<-paste("current",dep_q_label,"raw",sep=" ")
}
if(exists.to.tab("dep_quant_life")) {
  if(exists.to.tab("dep_quant_curr")) {
    depression_q<-cbind(dep_quant_curr,dep_quant_life)
    depression_q_label<-append(depression_q_label,
                               paste("lifetime",dep_q_label,"raw",sep=" "))
  }
  else {
    depression_q<-replace(depression_q, 1:length(iid),dep_quant_life)
    depression_q_label<-paste("lifetime",dep_q_label,"raw",sep=" ")
  }
}
if(exists.to.tab("dep_q")) {
  depression_q<-cbind(depression_q,dep_q)
  depression_q_label<-append(depression_q_label,paste("current",dep_q_label,
                                                      "in sample w/ npd_controls, raw",sep=" "))
}
if(exists.to.tab("QD_curr")) {
  depression_q<-cbind(depression_q,QD_curr)
  depression_q_label<-append(depression_q_label,
                             paste("current",dep_q_label,
                                   "(stress timing information not required)",
                                   sep=" "))
}
if(exists.to.tab("QD_life")) {
  depression_q<-cbind(depression_q,QD_life)
  depression_q_label<-append(depression_q_label,
                             paste("lifetime",dep_q_label,
                                   "(stress timing information not required)",
                                   sep=" ")) 
}
if(exists.to.tab("QD")) {
  depression_q<-cbind(depression_q,QD)
  depression_q_label<-append(depression_q_label,paste("current",dep_q_label,
                                                      "in sample w/ npd_controls (stress timing information not required)",sep=" "))
}
if(exists.to.tab("QD_curr_5yr")) {
  depression_q<-cbind(depression_q,QD_curr_5yr)
  depression_q_label<-append(depression_q_label,paste("current",dep_q_label,
                                                      "(stress in preceding 5 years)",sep=" "))
}
if(exists.to.tab("QD_curr_before")) {
  depression_q<-cbind(depression_q,QD_curr_before)
  depression_q_label<-append(depression_q_label,paste("current",dep_q_label,
                                                      "(stress preceding)",sep=" "))
}
if(exists.to.tab("QD_life_5yr")) {
  depression_q<-cbind(depression_q,QD_life_5yr)
  depression_q_label<-append(depression_q_label,paste("lifetime",dep_q_label,
                                                      "(stress in preceding 5 years)",sep=" "))
}
if(exists.to.tab("QD_life_before")) {
  depression_q<-cbind(depression_q,QD_life_before)
  depression_q_label<-append(depression_q_label,paste("lifetime",dep_q_label,
                                                      "(stress preceding)",sep=" "))
}
if(exists.to.tab("QD_5yr")) {
  depression_q<-cbind(depression_q,QD_5yr)
  depression_q_label<-append(depression_q_label,paste("current",dep_q_label,"in sample w/ npd_controls (stress in preceding 5 years)",sep=" "))
}
if(exists.to.tab("QD_before")) {
  depression_q<-cbind(depression_q,QD_before)
  depression_q_label<-append(depression_q_label,paste("current",dep_q_label,"in sample w/ npd_controls (stress preceding)",sep=" "))
}


if(sum(ifelse(is.na(depression_dx),0,1))) {
	if(length(dim(depression_dx))) nphens_dx<-dim(depression_dx)[2] else {
  		depression_dx<-cbind(depression_dx,c(rep(NA,length(depression_dx))))
		nphens_dx<-1
	}
} else nphens_dx<-0

if(sum(ifelse(is.na(depression_q),0,1))) {
	if(length(dim(depression_q))) nphens_q<-dim(depression_q)[2] else {
		nphens_q<-1
  		depression_q<-cbind(depression_q,c(rep(NA,length(depression_q))))
	} 
} else nphens_q<-0

 ##################################
 # determine which SNP definition #
 ###################################

#unlink(out.file)

if(genotype_rs25531=="YES") {
  SNP.tab<-cbind(add_5http,add_rs25531)
names(SNP.tab)<-cbind(c(rep("add_5http",length(SNP.tab[,1]))),c(rep("add_rs25531",length(SNP.tab[,1]))))
  SNP.tab.label<-c("S Dosage","non-LA Dosage")
  get.xtab(add_5http,add_rs25531,'gen5http_S','non-LA_haplotype_dosage') 
  get.xtab(add_5http,raw_add_rs25531,'gen5http_S','rs25531_G') 
} else if(genotype_SL=="YES") {
  SNP.tab.label<-c("S Dosage","(none)")
  SNP.tab<-cbind(add_5http,c(rep(NA,length(iid))))
  names(SNP.tab)<-cbind(c(rep("add_5http",length(SNP.tab[,1]))),c(rep("(none)",length(SNP.tab[,1]))))
} else {
  SNP.tab<-cbind(c(rep(NA,length(iid))),c(rep(NA,length(iid))))
  SNP.tab.label<-c("(none)","(none)")
  names(SNP.tab)<-cbind(c(rep("(none)",length(SNP.tab[,1]))),c(rep("(none)",length(SNP.tab[,1]))))
}

cat("===== generating 1-way frequency tables", "\n" )

 #######
 # sex #
 ########

if(exists.to.tab2(female)) get.freq(female,'Sex')

 ################
 # age category #
 #################

if(exists.to.tab2(age_cat)) get.freq(age_cat,'Age Category')

 #######
 # age #
 ########
if(exists.to.tab2(age)) get.stat(age,'age')
if(exists.to.tab2(age[dep_dx_life==1])) get.stat(age[dep_dx_life==1],'age (cases)')
if(exists.to.tab2(age[dep_dx_life==0])) get.stat(age[dep_dx_life==0],'age (controls)')

 ################
 # birth_decade #
 #################

if(exists.to.tab2(birth_decade)) {
  get.freq(birth_decade,'Decade of Birth')
}

 ####################
 # timing variables #
 ####################

if(exists.to.tab2(timing_before_curr)) get.freq(timing_before_curr,'Timing Before Onset of Current DX')
if(exists.to.tab2(timing_5yr_curr)) get.freq(timing_5yr_curr,'Life stress exposure in 5yr prior to Onset of Current DX')
if(exists.to.tab2(timing_5yr_curr_q)) get.freq(timing_5yr_curr_q,'Number of Life stress events in 5yr prior to Onset of Current DX')
if(exists.to.tab2(timing_before_life)) get.freq(timing_before_life,'Timing Before Lifetime Onset of DX')
if(exists.to.tab2(timing_5yr_life)) get.freq(timing_5yr_life,'Life stress exposure in 5yr prior to Lifetime Onset of DX')
if(exists.to.tab2(timing_5yr_life_q)) get.freq(timing_5yr_life_q,'Number of Life stress events in 5yr prior to Lifetime Onset of DX')

 #################
 # depression_dx #
 #################

for(j in 1: nphens_dx) {
    if(length(depression_dx) > 0 ) {
        if(exists.to.tab2(depression_dx[,j])) 
            get.freq(depression_dx[,j],depression_dx_label[j])
    }
}

 ####################
 # depression_score #
 ####################

for(j in 1: nphens_q) {
    if(length(depression_q) > 0 ) {
        if(exists.to.tab2(depression_q[,j]))
            get.freq(depression_q[,j],depression_q_label[j])
    }
}

 ##################
 # stress_exposed #
 ###################

if(exists.to.tab("child_mal_exp")){
	get.freq(child_mal_exp,'Childhood Maltreatment Exposure')
}

if(exists.to.tab("life_stress_exp")){
	get.freq(life_stress_exp,'Non-Childhood Maltreatment Stress Exposure')
	if(exists.to.tab2(life_stress_exp[is.na(DD_curr_before)==FALSE | 
					  is.na(QD_curr_before)==FALSE])) {
		get.freq(life_stress_exp[is.na(DD_curr_before)==FALSE | 
                	       	 	 is.na(QD_curr_before)==FALSE],
		 	 'Non-Childhood Maltreatment Stress Exposure (timing ordered w.r.t. current depression)')
	}

	if(exists.to.tab2(life_stress_exp[is.na(DD_life_before)==FALSE |
        	        		  is.na(QD_life_before)==FALSE])) {
		get.freq(life_stress_exp[is.na(DD_life_before)==FALSE |
                	       	 	 is.na(QD_life_before)==FALSE],
		 	 'Non-Childhood Maltreatment Stress Exposure (timing ordered w.r.t. first depression)')
	}
    }

if(exists.to.tab2(timing_5yr_curr[is.na(DD_curr_5yr)==FALSE | 
                                  is.na(QD_curr_5yr)==FALSE])) {
    get.freq(timing_5yr_curr[is.na(DD_curr_5yr)==FALSE | 
                             is.na(QD_curr_5yr)==FALSE],
             'Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. current depression)')
}

if(exists.to.tab2(timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                  is.na(QD_life_5yr)==FALSE])) {
    get.freq(timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                             is.na(QD_life_5yr)==FALSE],
             'Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. first depression)')
}

if(exists.to.tab("stress_combined_exp_clean")){
    get.freq(stress_combined_exp_clean,'Combined Stress Exposure (narrower controls)')
    if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_before)==FALSE | 
                                                is.na(QD_curr_before)==FALSE])) {
        get.freq(stress_combined_exp_clean[is.na(DD_curr_before)==FALSE | 
                                           is.na(QD_curr_before)==FALSE],
                 'Combined Stress Exposure (narrower controls) (timing ordered w.r.t. current depression)')
    }

    if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                is.na(QD_life_before)==FALSE])) {
        get.freq(stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                           is.na(QD_life_before)==FALSE],
                 'Combined Stress Exposure (narrower controls) (timing ordered w.r.t. first depression)')
    }

    if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE | 
                                                is.na(QD_curr_5yr)==FALSE])) {
        get.freq(stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE | 
                                           is.na(QD_curr_5yr)==FALSE],
                 'Combined Stress Exposure (narrower controls) (5yr window w.r.t. current depression)')
    }

    if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                is.na(QD_life_5yr)==FALSE])) {
        get.freq(stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                           is.na(QD_life_5yr)==FALSE],
                 'Combined Stress Exposure (narrower controls) (5yr window w.r.t. first depression)')
    }
}

if(exists.to.tab("stress_combined_exp_messy")){
	get.freq(stress_combined_exp_messy,'Combined Stress Exposure (broader controls)')
	if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_before)==FALSE | 
					      is.na(QD_curr_before)==FALSE])) {
		get.freq(stress_combined_exp_messy[is.na(DD_curr_before)==FALSE | 
                       			     is.na(QD_curr_before)==FALSE],
			 'Combined Stress Exposure (broader controls) (timing ordered w.r.t. current depression)')
	}

	if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                			      is.na(QD_life_before)==FALSE])) {
		get.freq(stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                       			     is.na(QD_life_before)==FALSE],
			 'Combined Stress Exposure (broader controls) (timing ordered w.r.t. first depression)')
	}

	if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE | 
					      is.na(QD_curr_5yr)==FALSE])) {
		get.freq(stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE | 
                       			     is.na(QD_curr_5yr)==FALSE],
			 'Combined Stress Exposure (broader controls) (5yr window w.r.t. current depression)')
	}

	if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                			      is.na(QD_life_5yr)==FALSE])) {
		get.freq(stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                       			     is.na(QD_life_5yr)==FALSE],
			 'Combined Stress Exposure (broader controls) (5yr window w.r.t. first depression)')
	}
    }

 ################
 # stress_count #
 #################

if(exists.to.tab("child_mal_quant")){
    get.stat(child_mal_quant,'Childhood Maltreatment Score')
}

if(exists.to.tab("life_stress_quant")){
    get.stat(life_stress_quant,'Non-Childhood Maltreatment Stress Score')
    if(exists.to.tab2(life_stress_quant[is.na(DD_curr_before)==FALSE | 
                                        is.na(QD_curr_before)==FALSE])) {
        get.stat(life_stress_quant[is.na(DD_curr_before)==FALSE | 
                                   is.na(QD_curr_before)==FALSE],
                 'Non-Childhood Maltreatment Stress Score (timing ordered w.r.t. current depression)')
    }

    if(exists.to.tab2(life_stress_quant[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE])) {
        get.stat(life_stress_quant[is.na(DD_life_before)==FALSE |
                                   is.na(QD_life_before)==FALSE],
                 'Non-Childhood Maltreatment Stress Score (timing ordered w.r.t. first depression)')
    }

    if(exists.to.tab2(life_stress_quant[is.na(DD_curr_5yr)==FALSE | 
                                        is.na(QD_curr_5yr)==FALSE])) {
        get.stat(life_stress_quant[is.na(DD_curr_5yr)==FALSE | 
                                   is.na(QD_curr_5yr)==FALSE],
                 'Non-Childhood Maltreatment Stress Score (5yr window w.r.t. current depression)')
    }

    if(exists.to.tab2(life_stress_quant[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE])) {
        get.stat(life_stress_quant[is.na(DD_life_5yr)==FALSE |
                                   is.na(QD_life_5yr)==FALSE],
                 'Non-Childhood Maltreatment Stress Score (5yr window w.r.t. first depression)')
    }
}

 ###################################
 # current depression age of onset #
 ####################################

if(exists.to.tab2(dep_curr_ao[dep_dx_curr==1])){
  get.stat(dep_curr_ao[dep_dx_curr==1],
           'age of onset of current depression (cases)')
}
if(exists.to.tab2(dep_curr_ao[dep_dx_curr==0])){
  get.stat(dep_curr_ao[dep_dx_curr==0],'age of onset of current depression (controls)')
}

 #######################################
 # lifetime age of onset of depression #
 ########################################

if(exists.to.tab2(dep_life_ao[dep_dx_life==1])){
  get.stat(dep_life_ao[dep_dx_life==1],'lifetime age of onset of depression (cases)')
}
if(exists.to.tab2(dep_life_ao[dep_dx_life==0])){
  get.stat(dep_life_ao[dep_dx_life==0],'lifetime age of onset of depression (controls)')
}

 ################################################
 # age at last stress before current depression #
 #################################################

if(exists.to.tab2(life_stress_alc[life_stress_exp==1 & (life_stress_alc > -1)])) {
  get.stat(life_stress_alc[life_stress_exp==1 & (life_stress_alc > -1)],'age of last stress exposure prior to current depression (exposed)')
}
if(exists.to.tab2(life_stress_alc[life_stress_exp==0 & (life_stress_alc > -1)])) {
  get.stat(life_stress_alc[life_stress_exp==0 & (life_stress_alc > -1)],'age of last stress exposure prior to current depression (not exposed)')
}

 ##############################################
 # age at last stress before first depression #
 ###############################################

if(exists.to.tab2(life_stress_alf[life_stress_exp==1 & (life_stress_alf > -1)])) {
  get.stat(life_stress_alf[life_stress_exp==1 & (life_stress_alf > -1)],'age of last stress exposure prior to first depression (exposed)')
}
if(exists.to.tab2(life_stress_alf[life_stress_exp==0 & (life_stress_alf > -1)])) {
  get.stat(life_stress_alf[life_stress_exp==0 & (life_stress_alf > -1)],'age of last stress exposure prior to first depression (not exposed)')
}

 #############
 # genotypes #
 ##############

for(i in 1: nsnps) {
  if (length(levels(as.factor(SNPs[,i])))>0){
    get.freq(SNPs[,i],SNPs.label[i])
  }
}

if (length(levels(as.factor(raw_add_rs25531)))>0){
  get.freq(raw_add_rs25531,"rs25531_G")
}

 #########################
 # 4-way crosstabulation #
 ##########################

cat("===== generating 4-way frequency tables", "\n" )

for(i in 1:length(SNP.tab.label)) {
    if(exists.to.tab("female") &
       exists.to.tab("age_cat") &
       exists.to.tab2("SNP.tab.label[i]")) {
        if(exists.to.tab("birth_decade")) {
            get.quadtab(female,age_cat,SNP.tab[,i],birth_decade,"Sex","Age Category",SNP.tab.label[i],"Decade of Birth")
        }
        
        for(j in 1: nphens_dx) {
            if(length(depression_dx) > 0 ) {
                if(exists.to.tab2(depression_dx[,j])) {
                    get.quadtab(female,age_cat,SNP.tab[,i],depression_dx[,j],"Sex",
                                "Age Category",
                                SNP.tab.label[i],depression_dx_label[j])
                }
            }
        }

        for(j in 1: nphens_q) {
            if(length(depression_q) > 0 ) {
                if(exists.to.tab2(depression_q[,j])) {
                    get.quadtab(female,age_cat,SNP.tab[,i],depression_q[,j],
                                "Sex",
                                "Age Category",
                                SNP.tab.label[i],depression_q_label[j])
                }
            }
        }

        if(exists.to.tab("child_mal_exp"))
            get.quadtab(female,age_cat,SNP.tab[,i],child_mal_exp,"Sex","Age Category",
                        SNP.tab.label[i],"Childhood Maltreatment Exposure")
        if(exists.to.tab("life_stress_exp"))
            get.quadtab(female,age_cat,SNP.tab[,i],life_stress_exp,"Sex","Age Category",
                        SNP.tab.label[i],"Non-Childhood Maltreatment Stress Exposure")
        if(exists.to.tab2(life_stress_exp[is.na(DD_life_before)==FALSE |
                                          is.na(QD_life_before)==FALSE]))
            get.quadtab(female[is.na(DD_life_before)==FALSE |
                               is.na(QD_life_before)==FALSE],
                        age_cat[is.na(DD_life_before)==FALSE |
                                is.na(QD_life_before)==FALSE],
                        SNP.tab[is.na(DD_life_before)==FALSE |
                                is.na(QD_life_before)==FALSE,i],
                        life_stress_exp[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE],
                        "Sex","Age Category",
                        SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure (timing ordered w.r.t. first depression)")

        if(exists.to.tab2(life_stress_exp[is.na(DD_curr_before)==FALSE |
                                          is.na(QD_curr_before)==FALSE]))
            get.quadtab(female[is.na(DD_curr_before)==FALSE |
                               is.na(QD_curr_before)==FALSE],
                        age_cat[is.na(DD_curr_before)==FALSE |
                                is.na(QD_curr_before)==FALSE],
                        SNP.tab[is.na(DD_curr_before)==FALSE |
                                is.na(QD_curr_before)==FALSE,i],
                        life_stress_exp[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE],
                        "Sex","Age Category",
                        SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure (timing ordered w.r.t. current depression)")

        if(exists.to.tab2(timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                          is.na(QD_life_5yr)==FALSE]))
            get.quadtab(female[is.na(DD_life_5yr)==FALSE |
                               is.na(QD_life_5yr)==FALSE],
                        age_cat[is.na(DD_life_5yr)==FALSE |
                                is.na(QD_life_5yr)==FALSE],
                        SNP.tab[is.na(DD_life_5yr)==FALSE |
                                is.na(QD_life_5yr)==FALSE,i],
                        timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE],
                        "Sex","Age Category",
                        SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. first depression)")

        if(exists.to.tab2(timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                          is.na(QD_curr_5yr)==FALSE]))
            get.quadtab(female[is.na(DD_curr_5yr)==FALSE |
                               is.na(QD_curr_5yr)==FALSE],
                        age_cat[is.na(DD_curr_5yr)==FALSE |
                                is.na(QD_curr_5yr)==FALSE],
                        SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                is.na(QD_curr_5yr)==FALSE,i],
                        timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE],
                        "Sex","Age Category",
                        SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. current depression)")
        
        if(exists.to.tab("stress_combined_exp_clean"))
            get.quadtab(female,age_cat,SNP.tab[,i],stress_combined_exp_clean,"Sex",
                        "Age Category",
                        SNP.tab.label[i],"Combined Stress Exposure (clean controls)")
        if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                    is.na(QD_life_before)==FALSE]))
            get.quadtab(female[is.na(DD_life_before)==FALSE |
                               is.na(QD_life_before)==FALSE],
                        age_cat[is.na(DD_life_before)==FALSE |
                                is.na(QD_life_before)==FALSE],
                        SNP.tab[is.na(DD_life_before)==FALSE |
                                is.na(QD_life_before)==FALSE,i],
                        stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                  is.na(QD_life_before)==FALSE],
                        "Sex","Age Category",
                        SNP.tab.label[i],"Combined Stress Exposure (clean controls) (timing ordered w.r.t. first depression)")

        if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                    is.na(QD_life_5yr)==FALSE]))
            get.quadtab(female[is.na(DD_life_5yr)==FALSE |
                               is.na(QD_life_5yr)==FALSE],
                        age_cat[is.na(DD_life_5yr)==FALSE |
                                is.na(QD_life_5yr)==FALSE],
                        SNP.tab[is.na(DD_life_5yr)==FALSE |
                                is.na(QD_life_5yr)==FALSE,i],
                        stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                  is.na(QD_life_5yr)==FALSE],
                        "Sex","Age Category",
                        SNP.tab.label[i],"Combined Stress Exposure (clean controls) (5yr window w.r.t. first depression)")

        if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                    is.na(QD_curr_before)==FALSE]))
            get.quadtab(female[is.na(DD_curr_before)==FALSE |
                               is.na(QD_curr_before)==FALSE],
                        age_cat[is.na(DD_curr_before)==FALSE |
                                is.na(QD_curr_before)==FALSE],
                        SNP.tab[is.na(DD_curr_before)==FALSE |
                                is.na(QD_curr_before)==FALSE,i],
                        stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                  is.na(QD_curr_before)==FALSE],
                        "Sex","Age Category",
                        SNP.tab.label[i],"Combined Stress Exposure (clean controls) (timing ordered w.r.t. current depression)")

        if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                    is.na(QD_curr_5yr)==FALSE]))
            get.quadtab(female[is.na(DD_curr_5yr)==FALSE |
                               is.na(QD_curr_5yr)==FALSE],
                        age_cat[is.na(DD_curr_5yr)==FALSE |
                                is.na(QD_curr_5yr)==FALSE],
                        SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                is.na(QD_curr_5yr)==FALSE,i],
                        stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                  is.na(QD_curr_5yr)==FALSE],
                        "Sex","Age Category",
                        SNP.tab.label[i],"Combined Stress Exposure (clean controls) (5yr window w.r.t. current depression)")

        if(exists.to.tab("stress_combined_exp_messy"))
            get.quadtab(female,age_cat,SNP.tab[,i],stress_combined_exp_messy,"Sex",
                        "Age Category",
                        SNP.tab.label[i],"Combined Stress Exposure (broader controls)")
        if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                    is.na(QD_life_before)==FALSE]))
            get.quadtab(female[is.na(DD_life_before)==FALSE |
                               is.na(QD_life_before)==FALSE],
                        age_cat[is.na(DD_life_before)==FALSE |
                                is.na(QD_life_before)==FALSE],
                        SNP.tab[is.na(DD_life_before)==FALSE |
                                is.na(QD_life_before)==FALSE,i],
                        stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                  is.na(QD_life_before)==FALSE],
                        "Sex","Age Category",
                        SNP.tab.label[i],"Combined Stress Exposure (broader controls) (timing ordered w.r.t. first depression)")

        if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                    is.na(QD_life_5yr)==FALSE]))
            get.quadtab(female[is.na(DD_life_5yr)==FALSE |
                               is.na(QD_life_5yr)==FALSE],
                        age_cat[is.na(DD_life_5yr)==FALSE |
                                is.na(QD_life_5yr)==FALSE],
                        SNP.tab[is.na(DD_life_5yr)==FALSE |
                                is.na(QD_life_5yr)==FALSE,i],
                        stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                  is.na(QD_life_5yr)==FALSE],
                        "Sex","Age Category",
                        SNP.tab.label[i],"Combined Stress Exposure (broader controls) (5yr window w.r.t. first depression)")

        if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                    is.na(QD_curr_before)==FALSE]))
            get.quadtab(female[is.na(DD_curr_before)==FALSE |
                               is.na(QD_curr_before)==FALSE],
                        age_cat[is.na(DD_curr_before)==FALSE |
                                is.na(QD_curr_before)==FALSE],
                        SNP.tab[is.na(DD_curr_before)==FALSE |
                                is.na(QD_curr_before)==FALSE,i],
                        stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                  is.na(QD_curr_before)==FALSE],
                        "Sex","Age Category",
                        SNP.tab.label[i],"Combined Stress Exposure (broader controls) (timing ordered w.r.t. current depression)")

        if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                    is.na(QD_curr_5yr)==FALSE]))
            get.quadtab(female[is.na(DD_curr_5yr)==FALSE |
                               is.na(QD_curr_5yr)==FALSE],
                        age_cat[is.na(DD_curr_5yr)==FALSE |
                                is.na(QD_curr_5yr)==FALSE],
                        SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                is.na(QD_curr_5yr)==FALSE,i],
                        stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                  is.na(QD_curr_5yr)==FALSE],
                        "Sex","Age Category",
                        SNP.tab.label[i],"Combined Stress Exposure (broader controls) (5yr window w.r.t. current depression)")
    }
}

 ##########################
 # 5-way crosstabulations #
 ###########################
        
cat("===== generating 5-way frequency tables", "\n" )

for(j in 1: nphens_dx) {
    if(length(depression_dx) > 0 ) {
        if(exists.to.tab("female") &
           exists.to.tab("age_cat") &
           exists.to.tab("haplotype")) {

            if(exists.to.tab2(depression_dx[,j])) {
                if(exists.to.tab2(child_mal_exp))
                    get.quintab(female,age_cat,haplotype,depression_dx[,j],child_mal_exp,
                                "Sex","Age Category",
                                "Haplotype",depression_dx_label[j],"Childhood Maltreatment Exposure")

                if(exists.to.tab2(life_stress_exp))
                    get.quintab(female,age_cat,haplotype,depression_dx[,j],life_stress_exp,"Sex",
                                "Age Category",
                                "Haplotype",depression_dx_label[j],
                                "Non-Childhood Maltreatment Stress Exposure")

                if(exists.to.tab2(life_stress_exp[is.na(DD_life_before)==FALSE |
                                                  is.na(QD_life_before)==FALSE]))
                    get.quintab(female[is.na(DD_life_before)==FALSE |
                                       is.na(QD_life_before)==FALSE],
                                age_cat[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE],
                                haplotype[is.na(DD_life_before)==FALSE |
                                          is.na(QD_life_before)==FALSE],
                                depression_dx[,j][is.na(DD_life_before)==FALSE |
                                                  is.na(QD_life_before)==FALSE],
                                life_stress_exp[is.na(DD_life_before)==FALSE |
                                                is.na(QD_life_before)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_dx_label[j],
                                "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. first depression)")

                if(exists.to.tab2(timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                                  is.na(QD_life_5yr)==FALSE]))
                    get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                       is.na(QD_life_5yr)==FALSE],
                                age_cat[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE],
                                haplotype[is.na(DD_life_5yr)==FALSE |
                                          is.na(QD_life_5yr)==FALSE],
                                depression_dx[,j][is.na(DD_life_5yr)==FALSE |
                                                  is.na(QD_life_5yr)==FALSE],
                                timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                                is.na(QD_life_5yr)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_dx_label[j],
                                "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. first depression)")
                
                if(exists.to.tab2(life_stress_exp[is.na(DD_curr_before)==FALSE |
                                                  is.na(QD_curr_before)==FALSE]))
                    get.quintab(female[is.na(DD_curr_before)==FALSE |
                                       is.na(QD_curr_before)==FALSE],
                                age_cat[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE],
                                haplotype[is.na(DD_curr_before)==FALSE |
                                          is.na(QD_curr_before)==FALSE],
                                depression_dx[,j][is.na(DD_curr_before)==FALSE |
                                                  is.na(QD_curr_before)==FALSE],
                                life_stress_exp[is.na(DD_curr_before)==FALSE |
                                                is.na(QD_curr_before)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_dx_label[j],
                                "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. current depression)")
                
                if(exists.to.tab2(timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                                  is.na(QD_curr_5yr)==FALSE]))
                    get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                       is.na(QD_curr_5yr)==FALSE],
                                age_cat[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE],
                                haplotype[is.na(DD_curr_5yr)==FALSE |
                                          is.na(QD_curr_5yr)==FALSE],
                                depression_dx[,j][is.na(DD_curr_5yr)==FALSE |
                                                  is.na(QD_curr_5yr)==FALSE],
                                timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                                is.na(QD_curr_5yr)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_dx_label[j],
                                "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. current depression)")
            
                if(exists.to.tab2(stress_combined_exp_clean))
                    get.quintab(female,age_cat,haplotype,depression_dx[,j],stress_combined_exp_clean,
                                "Sex","Age Category",
                                "Haplotype",depression_dx_label[j],"Combined Stress Exposure (clean controls)")
                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                            is.na(QD_life_before)==FALSE]))
                    get.quintab(female[is.na(DD_life_before)==FALSE |
                                       is.na(QD_life_before)==FALSE],
                                age_cat[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE],
                                haplotype[is.na(DD_life_before)==FALSE |
                                          is.na(QD_life_before)==FALSE],
                                depression_dx[,j][is.na(DD_life_before)==FALSE |
                                                  is.na(QD_life_before)==FALSE],
                                stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                          is.na(QD_life_before)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_dx_label[j],
                                "Combined Stress Exposure (clean controls) (timing before w.r.t. first depression)")

                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                            is.na(QD_life_5yr)==FALSE]))
                    get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                       is.na(QD_life_5yr)==FALSE],
                                age_cat[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE],
                                haplotype[is.na(DD_life_5yr)==FALSE |
                                          is.na(QD_life_5yr)==FALSE],
                                depression_dx[,j][is.na(DD_life_5yr)==FALSE |
                                                  is.na(QD_life_5yr)==FALSE],
                                stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                          is.na(QD_life_5yr)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_dx_label[j],
                                "Combined Stress Exposure (clean controls) (5yr window w.r.t. first depression)")
                
                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                            is.na(QD_curr_before)==FALSE]))
                    get.quintab(female[is.na(DD_curr_before)==FALSE |
                                       is.na(QD_curr_before)==FALSE],
                                age_cat[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE],
                                haplotype[is.na(DD_curr_before)==FALSE |
                                          is.na(QD_curr_before)==FALSE],
                                depression_dx[,j][is.na(DD_curr_before)==FALSE |
                                                  is.na(QD_curr_before)==FALSE],
                                stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                          is.na(QD_curr_before)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_dx_label[j],
                                "Combined Stress Exposure (clean controls) (timing before w.r.t. current depression)")

                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                            is.na(QD_curr_5yr)==FALSE]))
                    get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                       is.na(QD_curr_5yr)==FALSE],
                                age_cat[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE],
                                haplotype[is.na(DD_curr_5yr)==FALSE |
                                          is.na(QD_curr_5yr)==FALSE],
                                depression_dx[,j][is.na(DD_curr_5yr)==FALSE |
                                                  is.na(QD_curr_5yr)==FALSE],
                                stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                          is.na(QD_curr_5yr)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_dx_label[j],
                                "Combined Stress Exposure (clean controls) (5yr window w.r.t. current depression)")
                
                if(exists.to.tab2(stress_combined_exp_messy))
                    get.quintab(female,age_cat,haplotype,depression_dx[,j],stress_combined_exp_messy,
                                "Sex","Age Category",
                                "Haplotype",depression_dx_label[j],"Combined Stress Exposure (broader controls)")
                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                            is.na(QD_life_before)==FALSE]))
                    get.quintab(female[is.na(DD_life_before)==FALSE |
                                       is.na(QD_life_before)==FALSE],
                                age_cat[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE],
                                haplotype[is.na(DD_life_before)==FALSE |
                                          is.na(QD_life_before)==FALSE],
                                depression_dx[,j][is.na(DD_life_before)==FALSE |
                                                  is.na(QD_life_before)==FALSE],
                                stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                          is.na(QD_life_before)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_dx_label[j],
                                "Combined Stress Exposure (broader controls) (timing before w.r.t. first depression)")
                
                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                            is.na(QD_life_5yr)==FALSE]))
                    get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                       is.na(QD_life_5yr)==FALSE],
                                age_cat[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE],
                                haplotype[is.na(DD_life_5yr)==FALSE |
                                          is.na(QD_life_5yr)==FALSE],
                                depression_dx[,j][is.na(DD_life_5yr)==FALSE |
                                                  is.na(QD_life_5yr)==FALSE],
                                stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                          is.na(QD_life_5yr)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_dx_label[j],
                                "Combined Stress Exposure (broader controls) (5yr window w.r.t. first depression)")

                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                            is.na(QD_curr_before)==FALSE]))
                    get.quintab(female[is.na(DD_curr_before)==FALSE |
                                       is.na(QD_curr_before)==FALSE],
                                age_cat[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE],
                                haplotype[is.na(DD_curr_before)==FALSE |
                                          is.na(QD_curr_before)==FALSE],
                                depression_dx[,j][is.na(DD_curr_before)==FALSE |
                                                  is.na(QD_curr_before)==FALSE],
                                stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                          is.na(QD_curr_before)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_dx_label[j],
                                "Combined Stress Exposure (broader controls) (timing before w.r.t. current depression)")

                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                            is.na(QD_curr_5yr)==FALSE]))
                    get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                       is.na(QD_curr_5yr)==FALSE],
                                age_cat[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE],
                                haplotype[is.na(DD_curr_5yr)==FALSE |
                                          is.na(QD_curr_5yr)==FALSE],
                                depression_dx[,j][is.na(DD_curr_5yr)==FALSE |
                                                  is.na(QD_curr_5yr)==FALSE],
                                stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                          is.na(QD_curr_5yr)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_dx_label[j],
                                "Combined Stress Exposure (broader controls) (5yr window w.r.t. current depression)")
            }
        }
    }
}


for(j in 1: nphens_q) {
    if(length(depression_q) > 0 ) {
        if(exists.to.tab("female") &
           exists.to.tab("age_cat") &
           exists.to.tab("haplotype")) {

            if(exists.to.tab2(depression_q[,j])) {
                if(exists.to.tab2(child_mal_exp))
                    get.quintab(female,age_cat,haplotype,depression_q[,j],child_mal_exp,
                                "Sex","Age Category",
                                "Haplotype",depression_q_label[j],"Childhood Maltreatment Exposure")

                if(exists.to.tab2(life_stress_exp))
                    get.quintab(female,age_cat,haplotype,depression_q[,j],life_stress_exp,"Sex",
                                "Age Category",
                                "Haplotype",depression_q_label[j],
                                "Non-Childhood Maltreatment Stress Exposure")

                if(exists.to.tab2(life_stress_exp[is.na(DD_life_before)==FALSE |
                                                  is.na(QD_life_before)==FALSE]))
                    get.quintab(female[is.na(DD_life_before)==FALSE |
                                       is.na(QD_life_before)==FALSE],
                                age_cat[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE],
                                haplotype[is.na(DD_life_before)==FALSE |
                                          is.na(QD_life_before)==FALSE],
                                depression_q[,j][is.na(DD_life_before)==FALSE |
                                                 is.na(QD_life_before)==FALSE],
                                life_stress_exp[is.na(DD_life_before)==FALSE |
                                                is.na(QD_life_before)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_q_label[j],
                                "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. first depression)")

                if(exists.to.tab2(timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                                  is.na(QD_life_5yr)==FALSE]))
                    get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                       is.na(QD_life_5yr)==FALSE],
                                age_cat[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE],
                                haplotype[is.na(DD_life_5yr)==FALSE |
                                          is.na(QD_life_5yr)==FALSE],
                                depression_q[,j][is.na(DD_life_5yr)==FALSE |
                                                 is.na(QD_life_5yr)==FALSE],
                                timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                                is.na(QD_life_5yr)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_q_label[j],
                                "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. first depression)")

                if(exists.to.tab2(life_stress_exp[is.na(DD_curr_before)==FALSE |
                                                  is.na(QD_curr_before)==FALSE]))
                    get.quintab(female[is.na(DD_curr_before)==FALSE |
                                       is.na(QD_curr_before)==FALSE],
                                age_cat[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE],
                                haplotype[is.na(DD_curr_before)==FALSE |
                                          is.na(QD_curr_before)==FALSE],
                                depression_q[,j][is.na(DD_curr_before)==FALSE |
                                                 is.na(QD_curr_before)==FALSE],
                                life_stress_exp[is.na(DD_curr_before)==FALSE |
                                                is.na(QD_curr_before)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_q_label[j],
                                "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. current depression)")

                if(exists.to.tab2(timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                                  is.na(QD_curr_5yr)==FALSE]))
                    get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                       is.na(QD_curr_5yr)==FALSE],
                                age_cat[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE],
                                haplotype[is.na(DD_curr_5yr)==FALSE |
                                          is.na(QD_curr_5yr)==FALSE],
                                depression_q[,j][is.na(DD_curr_5yr)==FALSE |
                                                 is.na(QD_curr_5yr)==FALSE],
                                timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                                is.na(QD_curr_5yr)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_q_label[j],
                                "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. current depression)")
                
                if(exists.to.tab2(stress_combined_exp_clean))
                    get.quintab(female,age_cat,haplotype,depression_q[,j],stress_combined_exp_clean,
                                "Sex","Age Category",
                                "Haplotype",depression_q_label[j],"Combined Stress Exposure (clean controls)")
                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                            is.na(QD_life_before)==FALSE]))
                    get.quintab(female[is.na(DD_life_before)==FALSE |
                                       is.na(QD_life_before)==FALSE],
                                age_cat[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE],
                                haplotype[is.na(DD_life_before)==FALSE |
                                          is.na(QD_life_before)==FALSE],
                                depression_q[,j][is.na(DD_life_before)==FALSE |
                                                 is.na(QD_life_before)==FALSE],
                                stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                          is.na(QD_life_before)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_q_label[j],
                                "Combined Stress Exposure (clean controls) (timing before w.r.t. first depression)")

                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                            is.na(QD_life_5yr)==FALSE]))
                    get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                       is.na(QD_life_5yr)==FALSE],
                                age_cat[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE],
                                haplotype[is.na(DD_life_5yr)==FALSE |
                                          is.na(QD_life_5yr)==FALSE],
                                depression_q[,j][is.na(DD_life_5yr)==FALSE |
                                                 is.na(QD_life_5yr)==FALSE],
                                stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                          is.na(QD_life_5yr)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_q_label[j],
                                "Combined Stress Exposure (clean controls) (5yr window w.r.t. first depression)")

                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                            is.na(QD_curr_before)==FALSE]))
                    get.quintab(female[is.na(DD_curr_before)==FALSE |
                                       is.na(QD_curr_before)==FALSE],
                                age_cat[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE],
                                haplotype[is.na(DD_curr_before)==FALSE |
                                          is.na(QD_curr_before)==FALSE],
                                depression_q[,j][is.na(DD_curr_before)==FALSE |
                                                 is.na(QD_curr_before)==FALSE],
                                stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                          is.na(QD_curr_before)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_q_label[j],
                                "Combined Stress Exposure (clean controls) (timing before w.r.t. current depression)")

                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                            is.na(QD_curr_5yr)==FALSE]))
                    get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                       is.na(QD_curr_5yr)==FALSE],
                                age_cat[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE],
                                haplotype[is.na(DD_curr_5yr)==FALSE |
                                          is.na(QD_curr_5yr)==FALSE],
                                depression_q[,j][is.na(DD_curr_5yr)==FALSE |
                                                 is.na(QD_curr_5yr)==FALSE],
                                stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                          is.na(QD_curr_5yr)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_q_label[j],
                                "Combined Stress Exposure (clean controls) (5yr window w.r.t. current depression)")

                if(exists.to.tab2(stress_combined_exp_messy))
                    get.quintab(female,age_cat,haplotype,depression_q[,j],stress_combined_exp_messy,
                                "Sex","Age Category",
                                "Haplotype",depression_q_label[j],"Combined Stress Exposure (broader controls)")
                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                            is.na(QD_life_before)==FALSE]))
                    get.quintab(female[is.na(DD_life_before)==FALSE |
                                       is.na(QD_life_before)==FALSE],
                                age_cat[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE],
                                haplotype[is.na(DD_life_before)==FALSE |
                                          is.na(QD_life_before)==FALSE],
                                depression_q[,j][is.na(DD_life_before)==FALSE |
                                                 is.na(QD_life_before)==FALSE],
                                stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                          is.na(QD_life_before)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_q_label[j],
                                "Combined Stress Exposure (broader controls) (timing before w.r.t. first depression)")

                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                            is.na(QD_life_5yr)==FALSE]))
                    get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                       is.na(QD_life_5yr)==FALSE],
                                age_cat[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE],
                                haplotype[is.na(DD_life_5yr)==FALSE |
                                          is.na(QD_life_5yr)==FALSE],
                                depression_q[,j][is.na(DD_life_5yr)==FALSE |
                                                 is.na(QD_life_5yr)==FALSE],
                                stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                          is.na(QD_life_5yr)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_q_label[j],
                                "Combined Stress Exposure (broader controls) (5yr window w.r.t. first depression)")

                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                            is.na(QD_curr_before)==FALSE]))
                    get.quintab(female[is.na(DD_curr_before)==FALSE |
                                       is.na(QD_curr_before)==FALSE],
                                age_cat[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE],
                                haplotype[is.na(DD_curr_before)==FALSE |
                                          is.na(QD_curr_before)==FALSE],
                                depression_q[,j][is.na(DD_curr_before)==FALSE |
                                                 is.na(QD_curr_before)==FALSE],
                                stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                          is.na(QD_curr_before)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_q_label[j],
                                "Combined Stress Exposure (broader controls) (timing before w.r.t. current depression)")

                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                            is.na(QD_curr_5yr)==FALSE]))
                    get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                       is.na(QD_curr_5yr)==FALSE],
                                age_cat[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE],
                                haplotype[is.na(DD_curr_5yr)==FALSE |
                                          is.na(QD_curr_5yr)==FALSE],
                                depression_q[,j][is.na(DD_curr_5yr)==FALSE |
                                                 is.na(QD_curr_5yr)==FALSE],
                                stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                          is.na(QD_curr_5yr)==FALSE]
                                ,"Sex","Age Category","Haplotype",depression_q_label[j],
                                "Combined Stress Exposure (broader controls) (5yr window w.r.t. current depression)")
            }
        }
    }
}

for(i in 1:length(SNP.tab.label)) {
    if(exists.to.tab("female") &
       exists.to.tab("age_cat") &
       exists.to.tab2("SNP.tab.label[i]")) {

        if(exists.to.tab2(birth_decade)) {
            for(j in 1: nphens_dx) {
                if(length(depression_dx) > 0 ) {
                    if(exists.to.tab2(depression_dx[,j])) {
                        get.quintab(female,age_cat,SNP.tab[,i],birth_decade,depression_dx[,j],
                                    "Sex","Age Category",
                                    SNP.tab.label[i],"Decade of Birth",depression_dx_label[j])
                    }
                }
            }
            
            for(j in 1: nphens_q) {
                if(length(depression_q) > 0 ) {
                    if(exists.to.tab2(depression_q[,j])) {
                        get.quintab(female,age_cat,SNP.tab[,i],birth_decade,depression_q[,j],
                                    "Sex","Age Category",
                                    SNP.tab.label[i],"Decade of Birth",depression_q_label[j])
                    }
                }
            }
        
            if(exists.to.tab2(child_mal_exp))
                get.quintab(female,age_cat,SNP.tab[,i],birth_decade,child_mal_exp,
                            "Sex","Age Category",
                            SNP.tab.label[i],"Decade of Birth","Childhood Maltreatment Exposure")

            if(exists.to.tab2(life_stress_exp))
                get.quintab(female,age_cat,SNP.tab[,i],birth_decade,life_stress_exp,"Sex",
                            "Age Category",
                            SNP.tab.label[i],"Decade of Birth",
                            "Non-Childhood Maltreatment Stress Exposure")

            if(exists.to.tab2(life_stress_exp[is.na(DD_life_before)==FALSE |
                                              is.na(QD_life_before)==FALSE]))
                get.quintab(female[is.na(DD_life_before)==FALSE |
                                   is.na(QD_life_before)==FALSE],
                            age_cat[is.na(DD_life_before)==FALSE |
                                    is.na(QD_life_before)==FALSE],
                            SNP.tab[is.na(DD_life_before)==FALSE |
                                    is.na(QD_life_before)==FALSE,i],
                            birth_decade[is.na(DD_life_before)==FALSE |
                                         is.na(QD_life_before)==FALSE],
                            life_stress_exp[is.na(DD_life_before)==FALSE |
                                            is.na(QD_life_before)==FALSE]
                            ,"Sex","Age Category",SNP.tab.label[i],"Decade of Birth",
                            "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. first depression)")

            if(exists.to.tab2(timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                              is.na(QD_life_5yr)==FALSE]))
                get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                   is.na(QD_life_5yr)==FALSE],
                            age_cat[is.na(DD_life_5yr)==FALSE |
                                    is.na(QD_life_5yr)==FALSE],
                            SNP.tab[is.na(DD_life_5yr)==FALSE |
                                    is.na(QD_life_5yr)==FALSE,i],
                            birth_decade[is.na(DD_life_5yr)==FALSE |
                                         is.na(QD_life_5yr)==FALSE],
                            timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                            is.na(QD_life_5yr)==FALSE]
                            ,"Sex","Age Category",SNP.tab.label[i],"Decade of Birth",
                            "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. first depression)")

            if(exists.to.tab2(life_stress_exp[is.na(DD_curr_before)==FALSE |
                                              is.na(QD_curr_before)==FALSE]))
                get.quintab(female[is.na(DD_curr_before)==FALSE |
                                   is.na(QD_curr_before)==FALSE],
                            age_cat[is.na(DD_curr_before)==FALSE |
                                    is.na(QD_curr_before)==FALSE],
                            SNP.tab[is.na(DD_curr_before)==FALSE |
                                    is.na(QD_curr_before)==FALSE,i],
                            birth_decade[is.na(DD_curr_before)==FALSE |
                                         is.na(QD_curr_before)==FALSE],
                            life_stress_exp[is.na(DD_curr_before)==FALSE |
                                            is.na(QD_curr_before)==FALSE]
                            ,"Sex","Age Category",SNP.tab.label[i],"Decade of Birth",
                            "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. current depression)")

            if(exists.to.tab2(timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                              is.na(QD_curr_5yr)==FALSE]))
                get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                   is.na(QD_curr_5yr)==FALSE],
                            age_cat[is.na(DD_curr_5yr)==FALSE |
                                    is.na(QD_curr_5yr)==FALSE],
                            SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                    is.na(QD_curr_5yr)==FALSE,i],
                            birth_decade[is.na(DD_curr_5yr)==FALSE |
                                         is.na(QD_curr_5yr)==FALSE],
                            timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                            is.na(QD_curr_5yr)==FALSE]
                            ,"Sex","Age Category",SNP.tab.label[i],"Decade of Birth",
                            "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. current depression)")

            if(exists.to.tab2(stress_combined_exp_clean))
                get.quintab(female,age_cat,SNP.tab[,i],birth_decade,stress_combined_exp_clean,
                            "Sex","Age Category",
                            SNP.tab.label[i],"Decade of Birth","Combined Stress Exposure (clean controls)")
            if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                        is.na(QD_life_before)==FALSE]))
                get.quintab(female[is.na(DD_life_before)==FALSE |
                                   is.na(QD_life_before)==FALSE],
                            age_cat[is.na(DD_life_before)==FALSE |
                                    is.na(QD_life_before)==FALSE],
                            SNP.tab[is.na(DD_life_before)==FALSE |
                                    is.na(QD_life_before)==FALSE,i],
                            birth_decade[is.na(DD_life_before)==FALSE |
                                         is.na(QD_life_before)==FALSE],
                            stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                      is.na(QD_life_before)==FALSE]
                            ,"Sex","Age Category",SNP.tab.label[i],"Decade of Birth",
                            "Combined Stress Exposure (clean controls) (timing before w.r.t. first depression)")

            if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                        is.na(QD_life_5yr)==FALSE]))
                get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                   is.na(QD_life_5yr)==FALSE],
                            age_cat[is.na(DD_life_5yr)==FALSE |
                                    is.na(QD_life_5yr)==FALSE],
                            SNP.tab[is.na(DD_life_5yr)==FALSE |
                                    is.na(QD_life_5yr)==FALSE,i],
                            birth_decade[is.na(DD_life_5yr)==FALSE |
                                         is.na(QD_life_5yr)==FALSE],
                            stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                      is.na(QD_life_5yr)==FALSE]
                            ,"Sex","Age Category",SNP.tab.label[i],"Decade of Birth",
                            "Combined Stress Exposure (clean controls) (5yr window w.r.t. first depression)")

            if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                        is.na(QD_curr_before)==FALSE]))
                get.quintab(female[is.na(DD_curr_before)==FALSE |
                                   is.na(QD_curr_before)==FALSE],
                            age_cat[is.na(DD_curr_before)==FALSE |
                                    is.na(QD_curr_before)==FALSE],
                            SNP.tab[is.na(DD_curr_before)==FALSE |
                                    is.na(QD_curr_before)==FALSE,i],
                            birth_decade[is.na(DD_curr_before)==FALSE |
                                         is.na(QD_curr_before)==FALSE],
                            stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                      is.na(QD_curr_before)==FALSE]
                            ,"Sex","Age Category",SNP.tab.label[i],"Decade of Birth",
                            "Combined Stress Exposure (clean controls) (timing before w.r.t. current depression)")

            if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                        is.na(QD_curr_5yr)==FALSE]))
                get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                   is.na(QD_curr_5yr)==FALSE],
                            age_cat[is.na(DD_curr_5yr)==FALSE |
                                    is.na(QD_curr_5yr)==FALSE],
                            SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                    is.na(QD_curr_5yr)==FALSE,i],
                            birth_decade[is.na(DD_curr_5yr)==FALSE |
                                         is.na(QD_curr_5yr)==FALSE],
                            stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                      is.na(QD_curr_5yr)==FALSE]
                            ,"Sex","Age Category",SNP.tab.label[i],"Decade of Birth",
                            "Combined Stress Exposure (clean controls) (5yr window w.r.t. current depression)")

            if(exists.to.tab2(stress_combined_exp_messy))
                get.quintab(female,age_cat,SNP.tab[,i],birth_decade,stress_combined_exp_messy,
                            "Sex","Age Category",
                            SNP.tab.label[i],"Decade of Birth","Combined Stress Exposure (broader controls)")
            if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                        is.na(QD_life_before)==FALSE]))
                get.quintab(female[is.na(DD_life_before)==FALSE |
                                   is.na(QD_life_before)==FALSE],
                            age_cat[is.na(DD_life_before)==FALSE |
                                    is.na(QD_life_before)==FALSE],
                            SNP.tab[is.na(DD_life_before)==FALSE |
                                    is.na(QD_life_before)==FALSE,i],
                            birth_decade[is.na(DD_life_before)==FALSE |
                                         is.na(QD_life_before)==FALSE],
                            stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                      is.na(QD_life_before)==FALSE]
                            ,"Sex","Age Category",SNP.tab.label[i],"Decade of Birth",
                            "Combined Stress Exposure (broader controls) (timing before w.r.t. first depression)")

            if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                        is.na(QD_life_5yr)==FALSE]))
                get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                   is.na(QD_life_5yr)==FALSE],
                            age_cat[is.na(DD_life_5yr)==FALSE |
                                    is.na(QD_life_5yr)==FALSE],
                            SNP.tab[is.na(DD_life_5yr)==FALSE |
                                    is.na(QD_life_5yr)==FALSE,i],
                            birth_decade[is.na(DD_life_5yr)==FALSE |
                                         is.na(QD_life_5yr)==FALSE],
                            stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                      is.na(QD_life_5yr)==FALSE]
                            ,"Sex","Age Category",SNP.tab.label[i],"Decade of Birth",
                            "Combined Stress Exposure (broader controls) (5yr window w.r.t. first depression)")

            if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                        is.na(QD_curr_before)==FALSE]))
                get.quintab(female[is.na(DD_curr_before)==FALSE |
                                   is.na(QD_curr_before)==FALSE],
                            age_cat[is.na(DD_curr_before)==FALSE |
                                    is.na(QD_curr_before)==FALSE],
                            SNP.tab[is.na(DD_curr_before)==FALSE |
                                    is.na(QD_curr_before)==FALSE,i],
                            birth_decade[is.na(DD_curr_before)==FALSE |
                                         is.na(QD_curr_before)==FALSE],
                            stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                      is.na(QD_curr_before)==FALSE]
                            ,"Sex","Age Category",SNP.tab.label[i],"Decade of Birth",
                            "Combined Stress Exposure (broader controls) (timing before w.r.t. current depression)")

            if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                        is.na(QD_curr_5yr)==FALSE]))
                get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                   is.na(QD_curr_5yr)==FALSE],
                            age_cat[is.na(DD_curr_5yr)==FALSE |
                                    is.na(QD_curr_5yr)==FALSE],
                            SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                    is.na(QD_curr_5yr)==FALSE,i],
                            birth_decade[is.na(DD_curr_5yr)==FALSE |
                                         is.na(QD_curr_5yr)==FALSE],
                            stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                      is.na(QD_curr_5yr)==FALSE]
                            ,"Sex","Age Category",SNP.tab.label[i],"Decade of Birth",
                            "Combined Stress Exposure (broader controls) (5yr window w.r.t. current depression)")
        }
        
        if(length(depression_dx) > 0 ) {
            for(j in 1: nphens_dx) {
                if(length(depression_q) > 0 ) {
                    for(k in 1: nphens_q) {
                        if(strsplit(colnames(depression_dx)[j],"_dx")[[1]][1]=="dep" &
                           strsplit(colnames(depression_q)[k],"_q")[[1]][1]=="dep")
                            if((length(strsplit(colnames(depression_dx)[j],"_dx")[[1]])==1 &
                                length(strsplit(colnames(depression_q)[k],"_q")[[1]])==1) |
                               (length(strsplit(colnames(depression_dx)[j],"_dx")[[1]])>1 &
                                length(strsplit(colnames(depression_q)[k],"_quant")[[1]])>1 &
                                (sum(strsplit(colnames(depression_dx)[j],"_dx")[[1]]==
                                     strsplit(colnames(depression_q)[k],"_quant")[[1]])/
                                 length(strsplit(colnames(depression_dx)[j],"_dx")[[1]]==
                                        strsplit(colnames(depression_q)[k],"_quant")[[1]]) ==1)))
                                get.quintab(female,age_cat,SNP.tab[,i],depression_dx[,j],
                                            depression_q[,k],"Sex","Age Category",SNP.tab.label[i],
                                            depression_dx_label[j],depression_q_label[k])
                            else if(strsplit(colnames(depression_dx)[j],"DD_")[[1]]==
                                    strsplit(colnames(depression_q)[k],"QD_")[[1]])
                                get.quintab(female,age_cat,SNP.tab[,i],depression_dx[,j],
                                            depression_q[,k],"Sex","Age Category",SNP.tab.label[i],
                                            depression_dx_label[j],depression_q_label[k])
                    }
                }
            }
        }
    

        if(length(depression_dx) > 0 ) {
            if(exists.to.tab2(child_mal_exp)) { 
                for(j in 1: nphens_dx) {
                    if(length(grep("5y",depression_dx_label[j]))==0 &
                       length(grep("before",depression_dx_label[j]))==0){
                        get.quintab(female,age_cat,SNP.tab[,i],depression_dx[,j],child_mal_exp,
                                    "Sex",
                                    "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                    "Childhood Maltreatment Exposure")
                    }
                }
            }
            
            if(exists.to.tab2(life_stress_exp)) {
                for(j in 1: nphens_dx) {
                    if(length(grep("5y",depression_dx_label[j]))==0 &
                       length(grep("before",depression_dx_label[j]))==0){
                        get.quintab(female,age_cat,SNP.tab[,i],depression_dx[,j],life_stress_exp,
                                    "Sex",
                                    "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                    "Non-Childhood Maltreatment Stress Exposure")
                    }
                }
            }
            
            if(length(depression_dx) > 0 ) {
                if(exists.to.tab2(life_stress_exp[is.na(DD_life_before)==FALSE |
                                                  is.na(QD_life_before)==FALSE]))
                    for(j in 1: nphens_dx) {
                    if(length(grep("before",depression_dx_label[j]))==1){
                        get.quintab(female[is.na(DD_life_before)==FALSE |
                                           is.na(QD_life_before)==FALSE],
                                    age_cat[is.na(DD_life_before)==FALSE |
                                            is.na(QD_life_before)==FALSE],
                                    SNP.tab[is.na(DD_life_before)==FALSE |
                                            is.na(QD_life_before)==FALSE,i],
                                    depression_dx[,j][is.na(DD_life_before)==FALSE |
                                                      is.na(QD_life_before)==FALSE],
                                    life_stress_exp[is.na(DD_life_before)==FALSE |
                                                    is.na(QD_life_before)==FALSE],"Sex",
                                    "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                    "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. first depression)")
                    }
                }
            
                if(exists.to.tab2(timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                                  is.na(QD_life_5yr)==FALSE]))
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==1){
                            get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                               is.na(QD_life_5yr)==FALSE],
                                        age_cat[is.na(DD_life_5yr)==FALSE |
                                                is.na(QD_life_5yr)==FALSE],
                                        SNP.tab[is.na(DD_life_5yr)==FALSE |
                                                is.na(QD_life_5yr)==FALSE,i],
                                        depression_dx[,j][is.na(DD_life_5yr)==FALSE |
                                                          is.na(QD_life_5yr)==FALSE],
                                        timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                                        is.na(QD_life_5yr)==FALSE],"Sex",
                                        "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                        "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. first depression)")
                        }
                    }            

                if(exists.to.tab2(life_stress_exp[is.na(DD_curr_before)==FALSE |
                                                  is.na(QD_curr_before)==FALSE]))
                    for(j in 1: nphens_dx) {
                        if(length(grep("before",depression_dx_label[j]))==1){
                            get.quintab(female[is.na(DD_curr_before)==FALSE |
                                               is.na(QD_curr_before)==FALSE],
                                        age_cat[is.na(DD_curr_before)==FALSE |
                                                is.na(QD_curr_before)==FALSE],
                                        SNP.tab[is.na(DD_curr_before)==FALSE |
                                                is.na(QD_curr_before)==FALSE,i],
                                        depression_dx[,j][is.na(DD_curr_before)==FALSE |
                                                          is.na(QD_curr_before)==FALSE],
                                        life_stress_exp[is.na(DD_curr_before)==FALSE |
                                                        is.na(QD_curr_before)==FALSE],"Sex",
                                        "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                        "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. current depression)")
                        }
                    }

                if(exists.to.tab2(timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                                  is.na(QD_curr_5yr)==FALSE]))
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==1){
                            get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                               is.na(QD_curr_5yr)==FALSE],
                                        age_cat[is.na(DD_curr_5yr)==FALSE |
                                                is.na(QD_curr_5yr)==FALSE],
                                        SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                                is.na(QD_curr_5yr)==FALSE,i],
                                        depression_dx[,j][is.na(DD_curr_5yr)==FALSE |
                                                          is.na(QD_curr_5yr)==FALSE],
                                        timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                                        is.na(QD_curr_5yr)==FALSE],"Sex",
                                        "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                        "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. current depression)")
                        }
                    }
                
                if(exists.to.tab2(stress_combined_exp_clean))
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==0 &
                           length(grep("before",depression_dx_label[j]))==0){
                            get.quintab(female,age_cat,SNP.tab[,i],depression_dx[,j],stress_combined_exp_clean,
                                        "Sex","Age Category",SNP.tab.label[i],depression_dx_label[j],
                                        "Combined Stress Exposure (clean controls)")
                        }
                    }

                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                            is.na(QD_life_before)==FALSE]))
                    for(j in 1: nphens_dx) {
                        if(length(grep("before",depression_dx_label[j]))==1){
                            get.quintab(female[is.na(DD_life_before)==FALSE |
                                               is.na(QD_life_before)==FALSE],
                                        age_cat[is.na(DD_life_before)==FALSE |
                                                is.na(QD_life_before)==FALSE],
                                        SNP.tab[is.na(DD_life_before)==FALSE |
                                                is.na(QD_life_before)==FALSE,i],
                                        depression_dx[,j][is.na(DD_life_before)==FALSE |
                                                          is.na(QD_life_before)==FALSE],
                                        stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                                  is.na(QD_life_before)==FALSE],"Sex",
                                        "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                        "Combined Stress Exposure (clean controls) (timing before w.r.t. first depression)")
                        }
                    }

                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                            is.na(QD_life_5yr)==FALSE]))
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==1){
                            get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                               is.na(QD_life_5yr)==FALSE],
                                        age_cat[is.na(DD_life_5yr)==FALSE |
                                                is.na(QD_life_5yr)==FALSE],
                                        SNP.tab[is.na(DD_life_5yr)==FALSE |
                                                is.na(QD_life_5yr)==FALSE,i],
                                        depression_dx[,j][is.na(DD_life_5yr)==FALSE |
                                                          is.na(QD_life_5yr)==FALSE],
                                        stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                                  is.na(QD_life_5yr)==FALSE],"Sex",
                                        "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                        "Combined Stress Exposure (clean controls) (5yr window w.r.t. first depression)")
                        }
                    }

                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                            is.na(QD_curr_before)==FALSE]))
                    for(j in 1: nphens_dx) {
                        if(length(grep("before",depression_dx_label[j]))==1){
                            get.quintab(female[is.na(DD_curr_before)==FALSE |
                                               is.na(QD_curr_before)==FALSE],
                                        age_cat[is.na(DD_curr_before)==FALSE |
                                                is.na(QD_curr_before)==FALSE],
                                        SNP.tab[is.na(DD_curr_before)==FALSE |
                                                is.na(QD_curr_before)==FALSE,i],
                                        depression_dx[,j][is.na(DD_curr_before)==FALSE |
                                                          is.na(QD_curr_before)==FALSE],
                                        stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                                  is.na(QD_curr_before)==FALSE],"Sex",
                                        "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                        "Combined Stress Exposure (clean controls) (timing before w.r.t. current depression)")
                        }
                    }

                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                            is.na(QD_curr_5yr)==FALSE]))
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==1){
                            get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                               is.na(QD_curr_5yr)==FALSE],
                                        age_cat[is.na(DD_curr_5yr)==FALSE |
                                                is.na(QD_curr_5yr)==FALSE],
                                        SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                                is.na(QD_curr_5yr)==FALSE,i],
                                        depression_dx[,j][is.na(DD_curr_5yr)==FALSE |
                                                          is.na(QD_curr_5yr)==FALSE],
                                        stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                                  is.na(QD_curr_5yr)==FALSE],"Sex",
                                        "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                        "Combined Stress Exposure (clean controls) (5yr window w.r.t. current depression)")
                        }
                    }

                if(exists.to.tab2(stress_combined_exp_messy))
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==0 &
                           length(grep("before",depression_dx_label[j]))==0){
                            get.quintab(female,age_cat,SNP.tab[,i],depression_dx[,j],stress_combined_exp_messy,
                                        "Sex","Age Category",SNP.tab.label[i],depression_dx_label[j],
                                        "Combined Stress Exposure (broader controls)")
                        }
                    }

                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                            is.na(QD_life_before)==FALSE]))
                    for(j in 1: nphens_dx) {
                        if(length(grep("before",depression_dx_label[j]))==1){
                            get.quintab(female[is.na(DD_life_before)==FALSE |
                                               is.na(QD_life_before)==FALSE],
                                        age_cat[is.na(DD_life_before)==FALSE |
                                                is.na(QD_life_before)==FALSE],
                                        SNP.tab[is.na(DD_life_before)==FALSE |
                                                is.na(QD_life_before)==FALSE,i],
                                        depression_dx[,j][is.na(DD_life_before)==FALSE |
                                                          is.na(QD_life_before)==FALSE],
                                        stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                                  is.na(QD_life_before)==FALSE],"Sex",
                                        "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                        "Combined Stress Exposure (broader controls) (timing before w.r.t. first depression)")
                        }
                    }

                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                            is.na(QD_life_5yr)==FALSE]))
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==1){
                            get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                               is.na(QD_life_5yr)==FALSE],
                                        age_cat[is.na(DD_life_5yr)==FALSE |
                                                is.na(QD_life_5yr)==FALSE],
                                        SNP.tab[is.na(DD_life_5yr)==FALSE |
                                                is.na(QD_life_5yr)==FALSE,i],
                                        depression_dx[,j][is.na(DD_life_5yr)==FALSE |
                                                          is.na(QD_life_5yr)==FALSE],
                                        stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                                  is.na(QD_life_5yr)==FALSE],"Sex",
                                        "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                        "Combined Stress Exposure (broader controls) (5yr window w.r.t. first depression)")
                        }
                    }

                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                            is.na(QD_curr_before)==FALSE]))
                    for(j in 1: nphens_dx) {
                        if(length(grep("before",depression_dx_label[j]))==1){
                            get.quintab(female[is.na(DD_curr_before)==FALSE |
                                               is.na(QD_curr_before)==FALSE],
                                        age_cat[is.na(DD_curr_before)==FALSE |
                                                is.na(QD_curr_before)==FALSE],
                                        SNP.tab[is.na(DD_curr_before)==FALSE |
                                                is.na(QD_curr_before)==FALSE,i],
                                        depression_dx[,j][is.na(DD_curr_before)==FALSE |
                                                          is.na(QD_curr_before)==FALSE],
                                        stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                                  is.na(QD_curr_before)==FALSE],"Sex",
                                        "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                        "Combined Stress Exposure (broader controls) (timing before w.r.t. current depression)")
                        }
                    }

                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                            is.na(QD_curr_5yr)==FALSE]))
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==1){
                            get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                               is.na(QD_curr_5yr)==FALSE],
                                        age_cat[is.na(DD_curr_5yr)==FALSE |
                                                is.na(QD_curr_5yr)==FALSE],
                                        SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                                is.na(QD_curr_5yr)==FALSE,i],
                                        depression_dx[,j][is.na(DD_curr_5yr)==FALSE |
                                                          is.na(QD_curr_5yr)==FALSE],
                                        stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                                  is.na(QD_curr_5yr)==FALSE],"Sex",
                                        "Age Category",SNP.tab.label[i],depression_dx_label[j],
                                        "Combined Stress Exposure (broader controls) (5yr window w.r.t. current depression)")
                        }
                    }
            }
        }
    
        if(length(depression_q) > 0 ) {
            if(exists.to.tab2(child_mal_exp)) {
                for(j in 1: nphens_q) {
                    if(length(grep("5y",depression_q_label[j]))==0 &
                       length(grep("before",depression_q_label[j]))==0){
                        get.quintab(female,age_cat,SNP.tab[,i],depression_q[,j],child_mal_exp,"Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Childhood Maltreatment Exposure")
                    }
                }
            }
            
            if(exists.to.tab2(life_stress_exp)) {
                for(j in 1: nphens_q) {
                    if(length(grep("5y",depression_q_label[j]))==0 &
                       length(grep("before",depression_q_label[j]))==0){
                        get.quintab(female,age_cat,SNP.tab[,i],depression_q[,j],life_stress_exp,
                                    "Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Non-Childhood Maltreatment Stress Exposure")
                    }
                }
            }

            if(exists.to.tab2(life_stress_exp[is.na(DD_life_before)==FALSE |
                                              is.na(QD_life_before)==FALSE])) {
                for(j in 1: nphens_q) {
                    if(length(grep("before",depression_q_label[j]))==1){
                        get.quintab(female[is.na(DD_life_before)==FALSE |
                                           is.na(QD_life_before)==FALSE],
                                    age_cat[is.na(DD_life_before)==FALSE |
                                            is.na(QD_life_before)==FALSE],
                                    SNP.tab[is.na(DD_life_before)==FALSE |
                                            is.na(QD_life_before)==FALSE,i],
                                    depression_q[,j][is.na(DD_life_before)==FALSE |
                                                     is.na(QD_life_before)==FALSE],
                                    life_stress_exp[is.na(DD_life_before)==FALSE |
                                                    is.na(QD_life_before)==FALSE],"Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. first depression)")
                    }
                }
            }
            
            if(exists.to.tab2(timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                              is.na(QD_life_5yr)==FALSE])) {
                for(j in 1: nphens_q) {
                    if(length(grep("5y",depression_q_label[j]))==1){
                        get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                           is.na(QD_life_5yr)==FALSE],
                                    age_cat[is.na(DD_life_5yr)==FALSE |
                                            is.na(QD_life_5yr)==FALSE],
                                    SNP.tab[is.na(DD_life_5yr)==FALSE |
                                            is.na(QD_life_5yr)==FALSE,i],
                                    depression_q[,j][is.na(DD_life_5yr)==FALSE |
                                                     is.na(QD_life_5yr)==FALSE],
                                    timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                                    is.na(QD_life_5yr)==FALSE],"Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. first depression)")
                    }
                }
            }

            if(exists.to.tab2(life_stress_exp[is.na(DD_curr_before)==FALSE |
                                              is.na(QD_curr_before)==FALSE])) {
                for(j in 1: nphens_q) {
                    if(length(grep("before",depression_q_label[j]))==1){
                        get.quintab(female[is.na(DD_curr_before)==FALSE |
                                           is.na(QD_curr_before)==FALSE],
                                    age_cat[is.na(DD_curr_before)==FALSE |
                                            is.na(QD_curr_before)==FALSE],
                                    SNP.tab[is.na(DD_curr_before)==FALSE |
                                            is.na(QD_curr_before)==FALSE,i],
                                    depression_q[,j][is.na(DD_curr_before)==FALSE |
                                                     is.na(QD_curr_before)==FALSE],
                                    life_stress_exp[is.na(DD_curr_before)==FALSE |
                                                    is.na(QD_curr_before)==FALSE],"Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. current depression)")
                    }
                }
            }
            
            if(exists.to.tab2(timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                              is.na(QD_curr_5yr)==FALSE])) {
                for(j in 1: nphens_q) {
                    if(length(grep("5y",depression_q_label[j]))==1){
                        get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                           is.na(QD_curr_5yr)==FALSE],
                                    age_cat[is.na(DD_curr_5yr)==FALSE |
                                            is.na(QD_curr_5yr)==FALSE],
                                    SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                            is.na(QD_curr_5yr)==FALSE,i],
                                    depression_q[,j][is.na(DD_curr_5yr)==FALSE |
                                                     is.na(QD_curr_5yr)==FALSE],
                                    timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                                    is.na(QD_curr_5yr)==FALSE],"Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. current depression)")
                    }
                }
            }
            
            if(exists.to.tab2(stress_combined_exp_clean)) {
                for(j in 1: nphens_q) {
                    if(length(grep("5y",depression_q_label[j]))==0 &
                       length(grep("before",depression_q_label[j]))==0){
                        get.quintab(female,age_cat,SNP.tab[,i],depression_q[,j],stress_combined_exp_clean,
                                    "Sex","Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Combined Stress Exposure (clean controls)")
                    }
                }
            }
            
            if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                        is.na(QD_life_before)==FALSE])) {
                for(j in 1: nphens_q) {
                    if(length(grep("before",depression_q_label[j]))==1){
                        get.quintab(female[is.na(DD_life_before)==FALSE |
                                           is.na(QD_life_before)==FALSE],
                                    age_cat[is.na(DD_life_before)==FALSE |
                                            is.na(QD_life_before)==FALSE],
                                    SNP.tab[is.na(DD_life_before)==FALSE |
                                            is.na(QD_life_before)==FALSE,i],
                                    depression_q[,j][is.na(DD_life_before)==FALSE |
                                                     is.na(QD_life_before)==FALSE],
                                    stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                              is.na(QD_life_before)==FALSE],"Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Combined Stress Exposure (clean controls) (timing before w.r.t first depression)")
                    }
                }
            }
            
            if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                        is.na(QD_life_5yr)==FALSE])) {
                for(j in 1: nphens_q) {
                    if(length(grep("5y",depression_q_label[j]))==1){
                        get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                           is.na(QD_life_5yr)==FALSE],
                                    age_cat[is.na(DD_life_5yr)==FALSE |
                                            is.na(QD_life_5yr)==FALSE],
                                    SNP.tab[is.na(DD_life_5yr)==FALSE |
                                            is.na(QD_life_5yr)==FALSE,i],
                                    depression_q[,j][is.na(DD_life_5yr)==FALSE |
                                                     is.na(QD_life_5yr)==FALSE],
                                    stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                              is.na(QD_life_5yr)==FALSE],"Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Combined Stress Exposure (clean controls) (5yr window w.r.t first depression)")
                    }
                }
            }
            
            if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                        is.na(QD_curr_before)==FALSE])) {
                for(j in 1: nphens_q) {
                    if(length(grep("before",depression_q_label[j]))==1){
                        get.quintab(female[is.na(DD_curr_before)==FALSE |
                                           is.na(QD_curr_before)==FALSE],
                                    age_cat[is.na(DD_curr_before)==FALSE |
                                            is.na(QD_curr_before)==FALSE],
                                    SNP.tab[is.na(DD_curr_before)==FALSE |
                                            is.na(QD_curr_before)==FALSE,i],
                                    depression_q[,j][is.na(DD_curr_before)==FALSE |
                                                     is.na(QD_curr_before)==FALSE],
                                    stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                              is.na(QD_curr_before)==FALSE],"Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Combined Stress Exposure (clean controls) (timing before w.r.t current depression)")
                    }
                }
            }
            
            if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                        is.na(QD_curr_5yr)==FALSE])) {
                for(j in 1: nphens_q) {
                    if(length(grep("5y",depression_q_label[j]))==1){
                        get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                           is.na(QD_curr_5yr)==FALSE],
                                    age_cat[is.na(DD_curr_5yr)==FALSE |
                                            is.na(QD_curr_5yr)==FALSE],
                                    SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                            is.na(QD_curr_5yr)==FALSE,i],
                                    depression_q[,j][is.na(DD_curr_5yr)==FALSE |
                                                     is.na(QD_curr_5yr)==FALSE],
                                    stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                              is.na(QD_curr_5yr)==FALSE],"Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Combined Stress Exposure (clean controls) (5yr window w.r.t current depression)")
                    }
                }
            }
        
            if(exists.to.tab2(stress_combined_exp_messy)) {
                for(j in 1: nphens_q) {
                    if(length(grep("5y",depression_q_label[j]))==0 &
                       length(grep("before",depression_q_label[j]))==0){
                        get.quintab(female,age_cat,SNP.tab[,i],depression_q[,j],stress_combined_exp_messy,
                                    "Sex","Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Combined Stress Exposure (broader controls)")
                    }
                }
            }

            if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                        is.na(QD_life_before)==FALSE])) {
                for(j in 1: nphens_q) {
                    if(length(grep("before",depression_q_label[j]))==1){
                        get.quintab(female[is.na(DD_life_before)==FALSE |
                                           is.na(QD_life_before)==FALSE],
                                    age_cat[is.na(DD_life_before)==FALSE |
                                            is.na(QD_life_before)==FALSE],
                                    SNP.tab[is.na(DD_life_before)==FALSE |
                                            is.na(QD_life_before)==FALSE,i],
                                    depression_q[,j][is.na(DD_life_before)==FALSE |
                                                     is.na(QD_life_before)==FALSE],
                                    stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                              is.na(QD_life_before)==FALSE],"Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Combined Stress Exposure (broader controls) (timing before w.r.t first depression)")
                    }
                }
            }

            if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                        is.na(QD_life_5yr)==FALSE])) {
                for(j in 1: nphens_q) {
                    if(length(grep("5y",depression_q_label[j]))==1){
                        get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                           is.na(QD_life_5yr)==FALSE],
                                    age_cat[is.na(DD_life_5yr)==FALSE |
                                            is.na(QD_life_5yr)==FALSE],
                                    SNP.tab[is.na(DD_life_5yr)==FALSE |
                                            is.na(QD_life_5yr)==FALSE,i],
                                    depression_q[,j][is.na(DD_life_5yr)==FALSE |
                                                     is.na(QD_life_5yr)==FALSE],
                                    stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                              is.na(QD_life_5yr)==FALSE],"Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Combined Stress Exposure (broader controls) (5yr window w.r.t first depression)")
                    }
                }
            }

            if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                        is.na(QD_curr_before)==FALSE])) {
                for(j in 1: nphens_q) {
                    if(length(grep("before",depression_q_label[j]))==1){
                        get.quintab(female[is.na(DD_curr_before)==FALSE |
                                           is.na(QD_curr_before)==FALSE],
                                    age_cat[is.na(DD_curr_before)==FALSE |
                                            is.na(QD_curr_before)==FALSE],
                                    SNP.tab[is.na(DD_curr_before)==FALSE |
                                            is.na(QD_curr_before)==FALSE,i],
                                    depression_q[,j][is.na(DD_curr_before)==FALSE |
                                                     is.na(QD_curr_before)==FALSE],
                                    stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                              is.na(QD_curr_before)==FALSE],"Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Combined Stress Exposure (broader controls) (timing before w.r.t current depression)")
                    }
                }
            }

            if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                        is.na(QD_curr_5yr)==FALSE])) {
                for(j in 1: nphens_q) {
                    if(length(grep("5y",depression_q_label[j]))==1){
                        get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                           is.na(QD_curr_5yr)==FALSE],
                                    age_cat[is.na(DD_curr_5yr)==FALSE |
                                            is.na(QD_curr_5yr)==FALSE],
                                    SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                            is.na(QD_curr_5yr)==FALSE,i],
                                    depression_q[,j][is.na(DD_curr_5yr)==FALSE |
                                                     is.na(QD_curr_5yr)==FALSE],
                                    stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                              is.na(QD_curr_5yr)==FALSE],"Sex",
                                    "Age Category",SNP.tab.label[i],depression_q_label[j],
                                    "Combined Stress Exposure (broader controls) (5yr window w.r.t current depression)")
                    }
                }
            }
        }
        
        if(exists.to.tab2(child_mal_exp)){
            if(exists.to.tab2(life_stress_exp)) 
                get.quintab(female,age_cat,SNP.tab[,i],child_mal_exp,life_stress_exp,
                            "Sex","Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Non-Childhood Maltreatment Stress Exposure")

            if(exists.to.tab2(life_stress_exp[is.na(DD_life_before)==FALSE |
                                              is.na(QD_life_before)==FALSE]))
                get.quintab(female[is.na(DD_life_before)==FALSE |
                                   is.na(QD_life_before)==FALSE],
                            age_cat[is.na(DD_life_before)==FALSE |
                                    is.na(QD_life_before)==FALSE],
                            SNP.tab[is.na(DD_life_before)==FALSE |
                                    is.na(QD_life_before)==FALSE,i],
                            child_mal_exp[is.na(DD_life_before)==FALSE |
                                          is.na(QD_life_before)==FALSE],
                            life_stress_exp[is.na(DD_life_before)==FALSE |
                                            is.na(QD_life_before)==FALSE],"Sex",
                            "Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. first depresssion)")

            if(exists.to.tab2(timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                              is.na(QD_life_5yr)==FALSE]))
                get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                   is.na(QD_life_5yr)==FALSE],
                            age_cat[is.na(DD_life_5yr)==FALSE |
                                    is.na(QD_life_5yr)==FALSE],
                            SNP.tab[is.na(DD_life_5yr)==FALSE |
                                    is.na(QD_life_5yr)==FALSE,i],
                            child_mal_exp[is.na(DD_life_5yr)==FALSE |
                                          is.na(QD_life_5yr)==FALSE],
                            timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                            is.na(QD_life_5yr)==FALSE],"Sex",
                            "Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. first depresssion)")

            if(exists.to.tab2(life_stress_exp[is.na(DD_curr_before)==FALSE |
                                              is.na(QD_curr_before)==FALSE]))
                get.quintab(female[is.na(DD_curr_before)==FALSE |
                                   is.na(QD_curr_before)==FALSE],
                            age_cat[is.na(DD_curr_before)==FALSE |
                                    is.na(QD_curr_before)==FALSE],
                            SNP.tab[is.na(DD_curr_before)==FALSE |
                                    is.na(QD_curr_before)==FALSE,i],
                            child_mal_exp[is.na(DD_curr_before)==FALSE |
                                          is.na(QD_curr_before)==FALSE],
                            life_stress_exp[is.na(DD_curr_before)==FALSE |
                                            is.na(QD_curr_before)==FALSE],"Sex",
                            "Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. current depresssion)")

            if(exists.to.tab2(timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                              is.na(QD_curr_5yr)==FALSE]))
                get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                   is.na(QD_curr_5yr)==FALSE],
                            age_cat[is.na(DD_curr_5yr)==FALSE |
                                    is.na(QD_curr_5yr)==FALSE],
                            SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                    is.na(QD_curr_5yr)==FALSE,i],
                            child_mal_exp[is.na(DD_curr_5yr)==FALSE |
                                          is.na(QD_curr_5yr)==FALSE],
                            timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                            is.na(QD_curr_5yr)==FALSE],"Sex",
                            "Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. current depresssion)")
            
            if(exists.to.tab2(stress_combined_exp_clean))
                get.quintab(female,age_cat,SNP.tab[,i],child_mal_exp,stress_combined_exp_clean,
                            "Sex","Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Combined Stress Exposure (clean controls)")
            if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                        is.na(QD_life_before)==FALSE]))
                get.quintab(female[is.na(DD_life_before)==FALSE |
                                   is.na(QD_life_before)==FALSE],
                            age_cat[is.na(DD_life_before)==FALSE |
                                    is.na(QD_life_before)==FALSE],
                            SNP.tab[is.na(DD_life_before)==FALSE |
                                    is.na(QD_life_before)==FALSE,i],
                            child_mal_exp[is.na(DD_life_before)==FALSE |
                                          is.na(QD_life_before)==FALSE],
                            stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                      is.na(QD_life_before)==FALSE],"Sex",
                            "Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Combined Stress Exposure (clean controls) (timing before w.r.t. first depression)")

            if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                        is.na(QD_life_5yr)==FALSE]))
                get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                   is.na(QD_life_5yr)==FALSE],
                            age_cat[is.na(DD_life_5yr)==FALSE |
                                    is.na(QD_life_5yr)==FALSE],
                            SNP.tab[is.na(DD_life_5yr)==FALSE |
                                    is.na(QD_life_5yr)==FALSE,i],
                            child_mal_exp[is.na(DD_life_5yr)==FALSE |
                                          is.na(QD_life_5yr)==FALSE],
                            stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                      is.na(QD_life_5yr)==FALSE],"Sex",
                            "Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Combined Stress Exposure (clean controls) (5yr window w.r.t. first depression)")

            if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                        is.na(QD_curr_before)==FALSE]))
                get.quintab(female[is.na(DD_curr_before)==FALSE |
                                   is.na(QD_curr_before)==FALSE],
                            age_cat[is.na(DD_curr_before)==FALSE |
                                    is.na(QD_curr_before)==FALSE],
                            SNP.tab[is.na(DD_curr_before)==FALSE |
                                    is.na(QD_curr_before)==FALSE,i],
                            child_mal_exp[is.na(DD_curr_before)==FALSE |
                                          is.na(QD_curr_before)==FALSE],
                            stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                      is.na(QD_curr_before)==FALSE],"Sex",
                            "Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Combined Stress Exposure (clean controls) (timing before w.r.t. current depression)")

            if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                        is.na(QD_curr_5yr)==FALSE]))
                get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                   is.na(QD_curr_5yr)==FALSE],
                            age_cat[is.na(DD_curr_5yr)==FALSE |
                                    is.na(QD_curr_5yr)==FALSE],
                            SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                    is.na(QD_curr_5yr)==FALSE,i],
                            child_mal_exp[is.na(DD_curr_5yr)==FALSE |
                                          is.na(QD_curr_5yr)==FALSE],
                            stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                      is.na(QD_curr_5yr)==FALSE],"Sex",
                            "Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Combined Stress Exposure (clean controls) (5yr window w.r.t. current depression)")

            if(exists.to.tab2(stress_combined_exp_messy))
                get.quintab(female,age_cat,SNP.tab[,i],child_mal_exp,stress_combined_exp_messy,
                            "Sex","Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Combined Stress Exposure (broader controls)")
            if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                        is.na(QD_life_before)==FALSE]))
                get.quintab(female[is.na(DD_life_before)==FALSE |
                                   is.na(QD_life_before)==FALSE],
                            age_cat[is.na(DD_life_before)==FALSE |
                                    is.na(QD_life_before)==FALSE],
                            SNP.tab[is.na(DD_life_before)==FALSE |
                                    is.na(QD_life_before)==FALSE,i],
                            child_mal_exp[is.na(DD_life_before)==FALSE |
                                          is.na(QD_life_before)==FALSE],
                            stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                      is.na(QD_life_before)==FALSE],"Sex",
                            "Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Combined Stress Exposure (broader controls) (timing before w.r.t. first depression)")

            if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                        is.na(QD_life_5yr)==FALSE]))
                get.quintab(female[is.na(DD_life_5yr)==FALSE |
                                   is.na(QD_life_5yr)==FALSE],
                            age_cat[is.na(DD_life_5yr)==FALSE |
                                    is.na(QD_life_5yr)==FALSE],
                            SNP.tab[is.na(DD_life_5yr)==FALSE |
                                    is.na(QD_life_5yr)==FALSE,i],
                            child_mal_exp[is.na(DD_life_5yr)==FALSE |
                                          is.na(QD_life_5yr)==FALSE],
                            stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                      is.na(QD_life_5yr)==FALSE],"Sex",
                            "Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Combined Stress Exposure (broader controls) (5yr window w.r.t. first depression)")

            if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                        is.na(QD_curr_before)==FALSE]))
                get.quintab(female[is.na(DD_curr_before)==FALSE |
                                   is.na(QD_curr_before)==FALSE],
                            age_cat[is.na(DD_curr_before)==FALSE |
                                    is.na(QD_curr_before)==FALSE],
                            SNP.tab[is.na(DD_curr_before)==FALSE |
                                    is.na(QD_curr_before)==FALSE,i],
                            child_mal_exp[is.na(DD_curr_before)==FALSE |
                                          is.na(QD_curr_before)==FALSE],
                            stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                      is.na(QD_curr_before)==FALSE],"Sex",
                            "Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Combined Stress Exposure (broader controls) (timing before w.r.t. current depression)")

            if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                        is.na(QD_curr_5yr)==FALSE]))
                get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                                   is.na(QD_curr_5yr)==FALSE],
                            age_cat[is.na(DD_curr_5yr)==FALSE |
                                    is.na(QD_curr_5yr)==FALSE],
                            SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                    is.na(QD_curr_5yr)==FALSE,i],
                            child_mal_exp[is.na(DD_curr_5yr)==FALSE |
                                          is.na(QD_curr_5yr)==FALSE],
                            stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                      is.na(QD_curr_5yr)==FALSE],"Sex",
                            "Age Category",
                            SNP.tab.label[i],"Childhood Maltreatment Exposure",
                            "Combined Stress Exposure (broader controls) (5yr window w.r.t. current depression)")
        }
        

        if(exists.to.tab2(life_stress_exp) & exists.to.tab2(stress_combined_exp_clean))
            get.quintab(female,age_cat,SNP.tab[,i],life_stress_exp,stress_combined_exp_clean,
                        "Sex",
                        "Age Category",SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure",
                        "Combined Stress Exposure (clean controls)")

        if(exists.to.tab2(life_stress_exp[is.na(DD_life_before)==FALSE |
                                          is.na(QD_life_before)==FALSE]) &
           exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                    is.na(QD_life_before)==FALSE]))
            get.quintab(female[is.na(DD_life_before)==FALSE |
                               is.na(QD_life_before)==FALSE],
                        age_cat[is.na(DD_life_before)==FALSE |
                                is.na(QD_life_before)==FALSE],
                        SNP.tab[is.na(DD_life_before)==FALSE |
                                is.na(QD_life_before)==FALSE,i],
                        life_stress_exp[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE]
                        ,stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                   is.na(QD_life_before)==FALSE],"Sex",
                        "Age Category",SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. first depression)",
                        "Combined Stress Exposure (clean controls) (timing before w.r.t. first depression)")

        if(exists.to.tab2(timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                          is.na(QD_life_5yr)==FALSE]) &
           exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                    is.na(QD_life_5yr)==FALSE]))
            get.quintab(female[is.na(DD_life_5yr)==FALSE |
                               is.na(QD_life_5yr)==FALSE],
                        age_cat[is.na(DD_life_5yr)==FALSE |
                                is.na(QD_life_5yr)==FALSE],
                        SNP.tab[is.na(DD_life_5yr)==FALSE |
                                is.na(QD_life_5yr)==FALSE,i],
                        timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE]
                        ,stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                   is.na(QD_life_5yr)==FALSE],"Sex",
                        "Age Category",SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. first depression)",
                        "Combined Stress Exposure (clean controls) (5yr window w.r.t. first depression)")

        if(exists.to.tab2(life_stress_exp[is.na(DD_curr_before)==FALSE |
                                          is.na(QD_curr_before)==FALSE]) &
           exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                    is.na(QD_curr_before)==FALSE]))
            get.quintab(female[is.na(DD_curr_before)==FALSE |
                               is.na(QD_curr_before)==FALSE],
                        age_cat[is.na(DD_curr_before)==FALSE |
                                is.na(QD_curr_before)==FALSE],
                        SNP.tab[is.na(DD_curr_before)==FALSE |
                                is.na(QD_curr_before)==FALSE,i],
                        life_stress_exp[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE]
                        ,stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                   is.na(QD_curr_before)==FALSE],"Sex",
                        "Age Category",SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. current depression)",
                        "Combined Stress Exposure (clean controls) (timing before w.r.t. current depression)")

        if(exists.to.tab2(timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                          is.na(QD_curr_5yr)==FALSE]) &
           exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                    is.na(QD_curr_5yr)==FALSE]))
            get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                               is.na(QD_curr_5yr)==FALSE],
                        age_cat[is.na(DD_curr_5yr)==FALSE |
                                is.na(QD_curr_5yr)==FALSE],
                        SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                is.na(QD_curr_5yr)==FALSE,i],
                        timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE]
                        ,stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                   is.na(QD_curr_5yr)==FALSE],"Sex",
                        "Age Category",SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. current depression)",
                        "Combined Stress Exposure (clean controls) (5yr window w.r.t. current depression)")

        if(exists.to.tab2(life_stress_exp) & exists.to.tab2(stress_combined_exp_messy))
            get.quintab(female,age_cat,SNP.tab[,i],life_stress_exp,stress_combined_exp_messy,
                        "Sex",
                        "Age Category",SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure",
                        "Combined Stress Exposure (broader controls)")

        if(exists.to.tab2(life_stress_exp[is.na(DD_life_before)==FALSE |
                                          is.na(QD_life_before)==FALSE]) &
           exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                    is.na(QD_life_before)==FALSE]))
            get.quintab(female[is.na(DD_life_before)==FALSE |
                               is.na(QD_life_before)==FALSE],
                        age_cat[is.na(DD_life_before)==FALSE |
                                is.na(QD_life_before)==FALSE],
                        SNP.tab[is.na(DD_life_before)==FALSE |
                                is.na(QD_life_before)==FALSE,i],
                        life_stress_exp[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE]
                        ,stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                   is.na(QD_life_before)==FALSE],"Sex",
                        "Age Category",SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. first depression)",
                        "Combined Stress Exposure (broader controls) (timing before w.r.t. first depression)")

        if(exists.to.tab2(timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                          is.na(QD_life_5yr)==FALSE]) &
           exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                    is.na(QD_life_5yr)==FALSE]))
            get.quintab(female[is.na(DD_life_5yr)==FALSE |
                               is.na(QD_life_5yr)==FALSE],
                        age_cat[is.na(DD_life_5yr)==FALSE |
                                is.na(QD_life_5yr)==FALSE],
                        SNP.tab[is.na(DD_life_5yr)==FALSE |
                                is.na(QD_life_5yr)==FALSE,i],
                        timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE]
                        ,stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                   is.na(QD_life_5yr)==FALSE],"Sex",
                        "Age Category",SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. first depression)",
                        "Combined Stress Exposure (broader controls) (5yr window w.r.t. first depression)")

        if(exists.to.tab2(life_stress_exp[is.na(DD_curr_before)==FALSE |
                                          is.na(QD_curr_before)==FALSE]) &
           exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                    is.na(QD_curr_before)==FALSE]))
            get.quintab(female[is.na(DD_curr_before)==FALSE |
                               is.na(QD_curr_before)==FALSE],
                        age_cat[is.na(DD_curr_before)==FALSE |
                                is.na(QD_curr_before)==FALSE],
                        SNP.tab[is.na(DD_curr_before)==FALSE |
                                is.na(QD_curr_before)==FALSE,i],
                        life_stress_exp[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE]
                        ,stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                   is.na(QD_curr_before)==FALSE],"Sex",
                        "Age Category",SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. current depression)",
                        "Combined Stress Exposure (broader controls) (timing before w.r.t. current depression)")

        if(exists.to.tab2(timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                          is.na(QD_curr_5yr)==FALSE]) &
           exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                    is.na(QD_curr_5yr)==FALSE]))
            get.quintab(female[is.na(DD_curr_5yr)==FALSE |
                               is.na(QD_curr_5yr)==FALSE],
                        age_cat[is.na(DD_curr_5yr)==FALSE |
                                is.na(QD_curr_5yr)==FALSE],
                        SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                is.na(QD_curr_5yr)==FALSE,i],
                        timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE]
                        ,stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                   is.na(QD_curr_5yr)==FALSE],"Sex",
                        "Age Category",SNP.tab.label[i],
                        "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. current depression)",
                        "Combined Stress Exposure (broader controls) (5yr window w.r.t. current depression)")
    }
}

 ##########################
 # 6-way crosstabulations #
 ###########################

cat("===== generating 6-way frequency tables", "\n" )

for(i in 1:length(SNP.tab.label)) {
    if(exists.to.tab("female") &
       exists.to.tab("age_cat") &
       exists.to.tab2(SNP.tab.label[i])) {
   
        if(length(depression_dx) > 0 ) {
            if(exists.to.tab2(birth_decade)){  
                if(exists.to.tab2(child_mal_exp)) {  
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==0 &
                           length(grep("before",depression_dx_label[j]))==0){
                            get.sextab(female,age_cat,SNP.tab[,i],birth_decade,depression_dx[,j],
                                       child_mal_exp,"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Childhood Maltreatment Exposure")
                        }
                    }
                }

                if(exists.to.tab2(life_stress_exp)){
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==0 &
                           length(grep("before",depression_dx_label[j]))==0){
                            get.sextab(female,age_cat,SNP.tab[,i],birth_decade,depression_dx[,j],
                                       life_stress_exp,"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Non-Childhood Maltreatment Stress Exposure")
                        }
                    }
                }

                if(exists.to.tab2(life_stress_exp[is.na(DD_life_before)==FALSE |
                                                  is.na(QD_life_before)==FALSE])){
                    for(j in 1: nphens_dx) {
                        if(length(grep("before",depression_dx_label[j]))==1){
                            get.sextab(female[is.na(DD_life_before)==FALSE |
                                              is.na(QD_life_before)==FALSE],
                                       age_cat[is.na(DD_life_before)==FALSE |
                                               is.na(QD_life_before)==FALSE],
                                       SNP.tab[is.na(DD_life_before)==FALSE |
                                               is.na(QD_life_before)==FALSE,i],
                                       birth_decade[is.na(DD_life_before)==FALSE |
                                                    is.na(QD_life_before)==FALSE],
                                       depression_dx[,j][is.na(DD_life_before)==FALSE |
                                                         is.na(QD_life_before)==FALSE],
                                       life_stress_exp[is.na(DD_life_before)==FALSE |
                                                       is.na(QD_life_before)==FALSE],"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. first depression)")
                        }
                    }
                }
                
                if(exists.to.tab2(timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                                  is.na(QD_life_5yr)==FALSE])){
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==1){
                            get.sextab(female[is.na(DD_life_5yr)==FALSE |
                                              is.na(QD_life_5yr)==FALSE],
                                       age_cat[is.na(DD_life_5yr)==FALSE |
                                               is.na(QD_life_5yr)==FALSE],
                                       SNP.tab[is.na(DD_life_5yr)==FALSE |
                                               is.na(QD_life_5yr)==FALSE,i],
                                       birth_decade[is.na(DD_life_5yr)==FALSE |
                                                    is.na(QD_life_5yr)==FALSE],
                                       depression_dx[,j][is.na(DD_life_5yr)==FALSE |
                                                         is.na(QD_life_5yr)==FALSE],
                                       timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                                       is.na(QD_life_5yr)==FALSE],"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. first depression)")
                        }
                    }
                }

                if(exists.to.tab2(life_stress_exp[is.na(DD_curr_before)==FALSE |
                                                  is.na(QD_curr_before)==FALSE])){
                    for(j in 1: nphens_dx) {
                        if(length(grep("before",depression_dx_label[j]))==1){
                            get.sextab(female[is.na(DD_curr_before)==FALSE |
                                              is.na(QD_curr_before)==FALSE],
                                       age_cat[is.na(DD_curr_before)==FALSE |
                                               is.na(QD_curr_before)==FALSE],
                                       SNP.tab[is.na(DD_curr_before)==FALSE |
                                               is.na(QD_curr_before)==FALSE,i],
                                       birth_decade[is.na(DD_curr_before)==FALSE |
                                                    is.na(QD_curr_before)==FALSE],
                                       depression_dx[,j][is.na(DD_curr_before)==FALSE |
                                                         is.na(QD_curr_before)==FALSE],
                                       life_stress_exp[is.na(DD_curr_before)==FALSE |
                                                       is.na(QD_curr_before)==FALSE],"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. current depression)")
                        }
                    }
                }

                if(exists.to.tab2(timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                                  is.na(QD_curr_5yr)==FALSE])){
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==1){
                            get.sextab(female[is.na(DD_curr_5yr)==FALSE |
                                              is.na(QD_curr_5yr)==FALSE],
                                       age_cat[is.na(DD_curr_5yr)==FALSE |
                                               is.na(QD_curr_5yr)==FALSE],
                                       SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                               is.na(QD_curr_5yr)==FALSE,i],
                                       birth_decade[is.na(DD_curr_5yr)==FALSE |
                                                    is.na(QD_curr_5yr)==FALSE],
                                       depression_dx[,j][is.na(DD_curr_5yr)==FALSE |
                                                         is.na(QD_curr_5yr)==FALSE],
                                       timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                                       is.na(QD_curr_5yr)==FALSE],"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. current depression)")
                        }
                    }
                }
                
                if(exists.to.tab2(stress_combined_exp_clean)){
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==0 &
                           length(grep("before",depression_dx_label[j]))==0){
                            get.sextab(female,age_cat,SNP.tab[,i],birth_decade,depression_dx[,j],
                                       stress_combined_exp_clean,"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Combined Stress Exposure (clean controls)")
                        }
                    }
                }
                
                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                            is.na(QD_life_before)==FALSE])){
                    for(j in 1: nphens_dx) {
                        if(length(grep("before",depression_dx_label[j]))==1){
                            get.sextab(female[is.na(DD_life_before)==FALSE |
                                              is.na(QD_life_before)==FALSE],
                                       age_cat[is.na(DD_life_before)==FALSE |
                                               is.na(QD_life_before)==FALSE],
                                       SNP.tab[is.na(DD_life_before)==FALSE |
                                               is.na(QD_life_before)==FALSE,i],
                                       birth_decade[is.na(DD_life_before)==FALSE |
                                                    is.na(QD_life_before)==FALSE],
                                       depression_dx[,j][is.na(DD_life_before)==FALSE |
                                                         is.na(QD_life_before)==FALSE],
                                       stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                                 is.na(QD_life_before)==FALSE],"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Combined Stress Exposure (clean controls) (timing before w.r.t. first depression)")
                        }
                    }
                }

                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                            is.na(QD_life_5yr)==FALSE])){
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==1){
                            get.sextab(female[is.na(DD_life_5yr)==FALSE |
                                              is.na(QD_life_5yr)==FALSE],
                                       age_cat[is.na(DD_life_5yr)==FALSE |
                                               is.na(QD_life_5yr)==FALSE],
                                       SNP.tab[is.na(DD_life_5yr)==FALSE |
                                               is.na(QD_life_5yr)==FALSE,i],
                                       birth_decade[is.na(DD_life_5yr)==FALSE |
                                                    is.na(QD_life_5yr)==FALSE],
                                       depression_dx[,j][is.na(DD_life_5yr)==FALSE |
                                                         is.na(QD_life_5yr)==FALSE],
                                       stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                                 is.na(QD_life_5yr)==FALSE],"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Combined Stress Exposure (clean controls) (5yr window w.r.t. first depression)")
                        }
                    }
                }

                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                            is.na(QD_curr_before)==FALSE])){
                    for(j in 1: nphens_dx) {
                        if(length(grep("before",depression_dx_label[j]))==1){
                            get.sextab(female[is.na(DD_curr_before)==FALSE |
                                              is.na(QD_curr_before)==FALSE],
                                       age_cat[is.na(DD_curr_before)==FALSE |
                                               is.na(QD_curr_before)==FALSE],
                                       SNP.tab[is.na(DD_curr_before)==FALSE |
                                               is.na(QD_curr_before)==FALSE,i],
                                       birth_decade[is.na(DD_curr_before)==FALSE |
                                                    is.na(QD_curr_before)==FALSE],
                                       depression_dx[,j][is.na(DD_curr_before)==FALSE |
                                                         is.na(QD_curr_before)==FALSE],
                                       stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                                 is.na(QD_curr_before)==FALSE],"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Combined Stress Exposure (clean controls) (timing before w.r.t. first depression)")
                        }
                    }
                }
                
                if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                            is.na(QD_curr_5yr)==FALSE])){
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==1){
                            get.sextab(female[is.na(DD_curr_5yr)==FALSE |
                                              is.na(QD_curr_5yr)==FALSE],
                                       age_cat[is.na(DD_curr_5yr)==FALSE |
                                               is.na(QD_curr_5yr)==FALSE],
                                       SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                               is.na(QD_curr_5yr)==FALSE,i],
                                       birth_decade[is.na(DD_curr_5yr)==FALSE |
                                                    is.na(QD_curr_5yr)==FALSE],
                                       depression_dx[,j][is.na(DD_curr_5yr)==FALSE |
                                                         is.na(QD_curr_5yr)==FALSE],
                                       stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                                 is.na(QD_curr_5yr)==FALSE],"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Combined Stress Exposure (clean controls) (5yr window w.r.t. first depression)")
                        }
                    }
                }

                if(exists.to.tab2(stress_combined_exp_messy)){
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==0 &
                           length(grep("before",depression_dx_label[j]))==0){
                            get.sextab(female,age_cat,SNP.tab[,i],birth_decade,depression_dx[,j],
                                       stress_combined_exp_messy,"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Combined Stress Exposure (broader controls)")
                        }
                    }
                }
                
                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                            is.na(QD_life_before)==FALSE])){
                    for(j in 1: nphens_dx) {
                        if(length(grep("before",depression_dx_label[j]))==1){
                            get.sextab(female[is.na(DD_life_before)==FALSE |
                                              is.na(QD_life_before)==FALSE],
                                       age_cat[is.na(DD_life_before)==FALSE |
                                               is.na(QD_life_before)==FALSE],
                                       SNP.tab[is.na(DD_life_before)==FALSE |
                                               is.na(QD_life_before)==FALSE,i],
                                       birth_decade[is.na(DD_life_before)==FALSE |
                                                    is.na(QD_life_before)==FALSE],
                                       depression_dx[,j][is.na(DD_life_before)==FALSE |
                                                         is.na(QD_life_before)==FALSE],
                                       stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                                 is.na(QD_life_before)==FALSE],"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Combined Stress Exposure (broader controls) (timing before w.r.t. first depression)")
                        }
                    }
                }

                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                            is.na(QD_life_5yr)==FALSE])){
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==1){
                            get.sextab(female[is.na(DD_life_5yr)==FALSE |
                                              is.na(QD_life_5yr)==FALSE],
                                       age_cat[is.na(DD_life_5yr)==FALSE |
                                               is.na(QD_life_5yr)==FALSE],
                                       SNP.tab[is.na(DD_life_5yr)==FALSE |
                                               is.na(QD_life_5yr)==FALSE,i],
                                       birth_decade[is.na(DD_life_5yr)==FALSE |
                                                    is.na(QD_life_5yr)==FALSE],
                                       depression_dx[,j][is.na(DD_life_5yr)==FALSE |
                                                         is.na(QD_life_5yr)==FALSE],
                                       stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                                 is.na(QD_life_5yr)==FALSE],"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Combined Stress Exposure (broader controls) (5yr window w.r.t. first depression)")
                        }
                    }
                }

                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                            is.na(QD_curr_before)==FALSE])){
                    for(j in 1: nphens_dx) {
                        if(length(grep("before",depression_dx_label[j]))==1){
                            get.sextab(female[is.na(DD_curr_before)==FALSE |
                                              is.na(QD_curr_before)==FALSE],
                                       age_cat[is.na(DD_curr_before)==FALSE |
                                               is.na(QD_curr_before)==FALSE],
                                       SNP.tab[is.na(DD_curr_before)==FALSE |
                                               is.na(QD_curr_before)==FALSE,i],
                                       birth_decade[is.na(DD_curr_before)==FALSE |
                                                    is.na(QD_curr_before)==FALSE],
                                       depression_dx[,j][is.na(DD_curr_before)==FALSE |
                                                         is.na(QD_curr_before)==FALSE],
                                       stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                                 is.na(QD_curr_before)==FALSE],"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Combined Stress Exposure (broader controls) (timing before w.r.t. first depression)")
                        }
                    }
                }
                
                if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                            is.na(QD_curr_5yr)==FALSE])){
                    for(j in 1: nphens_dx) {
                        if(length(grep("5y",depression_dx_label[j]))==1){
                            get.sextab(female[is.na(DD_curr_5yr)==FALSE |
                                              is.na(QD_curr_5yr)==FALSE],
                                       age_cat[is.na(DD_curr_5yr)==FALSE |
                                               is.na(QD_curr_5yr)==FALSE],
                                       SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                               is.na(QD_curr_5yr)==FALSE,i],
                                       birth_decade[is.na(DD_curr_5yr)==FALSE |
                                                    is.na(QD_curr_5yr)==FALSE],
                                       depression_dx[,j][is.na(DD_curr_5yr)==FALSE |
                                                         is.na(QD_curr_5yr)==FALSE],
                                       stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                                 is.na(QD_curr_5yr)==FALSE],"Sex",
                                       "Age Category",SNP.tab.label[i],"Decade of Birth",
                                       depression_dx_label[j],
                                       "Combined Stress Exposure (broader controls) (5yr window w.r.t. first depression)")
                        }
                    }
                }
            }
        }
            
     if(length(depression_q) > 0 ) {
         if(exists.to.tab2(child_mal_exp)){
             for(j in 1: nphens_q) {
                 if(length(grep("5y",depression_q_label[j]))==0 &
                    length(grep("before",depression_q_label[j]))==0){
                     get.sextab(female,age_cat,SNP.tab[,i],birth_decade,depression_q[,j],
                                child_mal_exp
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Childhood Maltreatment Exposure" )
                 }
             }
         }
        
         if(exists.to.tab2(life_stress_exp)){
             for(j in 1: nphens_q) {
                 if(length(grep("5y",depression_q_label[j]))==0 &
                    length(grep("before",depression_q_label[j]))==0){
                     get.sextab(female,age_cat,SNP.tab[,i],birth_decade,depression_q[,j],
                                life_stress_exp
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Non-Childhood Maltreatment Stress Exposure" )
                 }
             }
         }
         
         if(exists.to.tab2(life_stress_exp[is.na(DD_life_before)==FALSE |
                                           is.na(QD_life_before)==FALSE])){
             for(j in 1: nphens_q) {
                 if(length(grep("before",depression_q_label[j]))==1){
                     get.sextab(female[is.na(DD_life_before)==FALSE |
                                       is.na(QD_life_before)==FALSE],
                                age_cat[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE],
                                SNP.tab[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE,i],
                                birth_decade[is.na(DD_life_before)==FALSE |
                                             is.na(QD_life_before)==FALSE],
                                depression_q[,j][is.na(DD_life_before)==FALSE |
                                                 is.na(QD_life_before)==FALSE],
                                life_stress_exp[is.na(DD_life_before)==FALSE |
                                                is.na(QD_life_before)==FALSE]
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. first depression)" )
                 }
             }
         }

         if(exists.to.tab2(timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                           is.na(QD_life_5yr)==FALSE])){
             for(j in 1: nphens_q) {
                 if(length(grep("5y",depression_q_label[j]))==1){
                     get.sextab(female[is.na(DD_life_5yr)==FALSE |
                                       is.na(QD_life_5yr)==FALSE],
                                age_cat[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE],
                                SNP.tab[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE,i],
                                birth_decade[is.na(DD_life_5yr)==FALSE |
                                             is.na(QD_life_5yr)==FALSE],
                                depression_q[,j][is.na(DD_life_5yr)==FALSE |
                                                 is.na(QD_life_5yr)==FALSE],
                                timing_5yr_life[is.na(DD_life_5yr)==FALSE |
                                                is.na(QD_life_5yr)==FALSE]
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. first depression)" )
                 }
             }
         }

         if(exists.to.tab2(life_stress_exp[is.na(DD_curr_before)==FALSE |
                                           is.na(QD_curr_before)==FALSE])){
             for(j in 1: nphens_q) {
                 if(length(grep("before",depression_q_label[j]))==1){
                     get.sextab(female[is.na(DD_curr_before)==FALSE |
                                       is.na(QD_curr_before)==FALSE],
                                age_cat[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE],
                                SNP.tab[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE,i],
                                birth_decade[is.na(DD_curr_before)==FALSE |
                                             is.na(QD_curr_before)==FALSE],
                                depression_q[,j][is.na(DD_curr_before)==FALSE |
                                                 is.na(QD_curr_before)==FALSE],
                                life_stress_exp[is.na(DD_curr_before)==FALSE |
                                                is.na(QD_curr_before)==FALSE]
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Non-Childhood Maltreatment Stress Exposure (timing before w.r.t. first depression)" )
                 }
             }
         }

         if(exists.to.tab2(timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                           is.na(QD_curr_5yr)==FALSE])){
             for(j in 1: nphens_q) {
                 if(length(grep("5y",depression_q_label[j]))==1){
                     get.sextab(female[is.na(DD_curr_5yr)==FALSE |
                                       is.na(QD_curr_5yr)==FALSE],
                                age_cat[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE],
                                SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE,i],
                                birth_decade[is.na(DD_curr_5yr)==FALSE |
                                             is.na(QD_curr_5yr)==FALSE],
                                depression_q[,j][is.na(DD_curr_5yr)==FALSE |
                                                 is.na(QD_curr_5yr)==FALSE],
                                timing_5yr_curr[is.na(DD_curr_5yr)==FALSE |
                                                is.na(QD_curr_5yr)==FALSE]
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Non-Childhood Maltreatment Stress Exposure (5yr window w.r.t. first depression)" )
                 }
             }
         }
         
         if(exists.to.tab2(stress_combined_exp_clean)){
             for(j in 1: nphens_q) {
                 if(length(grep("5y",depression_q_label[j]))==0 &
                    length(grep("before",depression_q_label[j]))==0){
                     get.sextab(female,age_cat,SNP.tab[,i],birth_decade,depression_q[,j],
                                stress_combined_exp_clean
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Combined Stress Exposure (clean controls)" )
                 }
             }
         }

         if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                     is.na(QD_life_before)==FALSE])){
             for(j in 1: nphens_q) {
                 if(length(grep("before",depression_q_label[j]))==1){
                     get.sextab(female[is.na(DD_life_before)==FALSE |
                                       is.na(QD_life_before)==FALSE],
                                age_cat[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE],
                                SNP.tab[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE,i],
                                birth_decade[is.na(DD_life_before)==FALSE |
                                             is.na(QD_life_before)==FALSE],
                                depression_q[,j][is.na(DD_life_before)==FALSE |
                                                 is.na(QD_life_before)==FALSE],
                                stress_combined_exp_clean[is.na(DD_life_before)==FALSE |
                                                          is.na(QD_life_before)==FALSE]
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Combined Stress Exposure (clean controls) (timing before w.r.t. first depression)" )
                 }
             }
         }

         if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                     is.na(QD_life_5yr)==FALSE])){
             for(j in 1: nphens_q) {
                 if(length(grep("5y",depression_q_label[j]))==1){
                     get.sextab(female[is.na(DD_life_5yr)==FALSE |
                                       is.na(QD_life_5yr)==FALSE],
                                age_cat[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE],
                                SNP.tab[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE,i],
                                birth_decade[is.na(DD_life_5yr)==FALSE |
                                             is.na(QD_life_5yr)==FALSE],
                                depression_q[,j][is.na(DD_life_5yr)==FALSE |
                                                 is.na(QD_life_5yr)==FALSE],
                                stress_combined_exp_clean[is.na(DD_life_5yr)==FALSE |
                                                          is.na(QD_life_5yr)==FALSE]
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Combined Stress Exposure (clean controls) (5yr window w.r.t. first depression)" )
                 }
             }
         }

         if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                     is.na(QD_curr_before)==FALSE])){
             for(j in 1: nphens_q) {
                 if(length(grep("before",depression_q_label[j]))==1){
                     get.sextab(female[is.na(DD_curr_before)==FALSE |
                                       is.na(QD_curr_before)==FALSE],
                                age_cat[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE],
                                SNP.tab[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE,i],
                                birth_decade[is.na(DD_curr_before)==FALSE |
                                             is.na(QD_curr_before)==FALSE],
                                depression_q[,j][is.na(DD_curr_before)==FALSE |
                                                 is.na(QD_curr_before)==FALSE],
                                stress_combined_exp_clean[is.na(DD_curr_before)==FALSE |
                                                          is.na(QD_curr_before)==FALSE]
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Combined Stress Exposure (clean controls) (timing before w.r.t. current depression)" )
                 }
             }
         }

         if(exists.to.tab2(stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                     is.na(QD_curr_5yr)==FALSE])){
             for(j in 1: nphens_q) {
                 if(length(grep("5y",depression_q_label[j]))==1){
                     get.sextab(female[is.na(DD_curr_5yr)==FALSE |
                                       is.na(QD_curr_5yr)==FALSE],
                                age_cat[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE],
                                SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE,i],
                                birth_decade[is.na(DD_curr_5yr)==FALSE |
                                             is.na(QD_curr_5yr)==FALSE],
                                depression_q[,j][is.na(DD_curr_5yr)==FALSE |
                                                 is.na(QD_curr_5yr)==FALSE],
                                stress_combined_exp_clean[is.na(DD_curr_5yr)==FALSE |
                                                          is.na(QD_curr_5yr)==FALSE]
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Combined Stress Exposure (clean controls) (5yr window w.r.t. current depression)" )
                 }
             }
         }

         if(exists.to.tab2(stress_combined_exp_messy)){
             for(j in 1: nphens_q) {
                 if(length(grep("5y",depression_q_label[j]))==0 &
                    length(grep("before",depression_q_label[j]))==0){
                     get.sextab(female,age_cat,SNP.tab[,i],birth_decade,depression_q[,j],
                                stress_combined_exp_messy
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Combined Stress Exposure (broader controls)" )
                 }
             }
         }

         if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                     is.na(QD_life_before)==FALSE])){
             for(j in 1: nphens_q) {
                 if(length(grep("before",depression_q_label[j]))==1){
                     get.sextab(female[is.na(DD_life_before)==FALSE |
                                       is.na(QD_life_before)==FALSE],
                                age_cat[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE],
                                SNP.tab[is.na(DD_life_before)==FALSE |
                                        is.na(QD_life_before)==FALSE,i],
                                birth_decade[is.na(DD_life_before)==FALSE |
                                             is.na(QD_life_before)==FALSE],
                                depression_q[,j][is.na(DD_life_before)==FALSE |
                                                 is.na(QD_life_before)==FALSE],
                                stress_combined_exp_messy[is.na(DD_life_before)==FALSE |
                                                          is.na(QD_life_before)==FALSE]
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Combined Stress Exposure (broader controls) (timing before w.r.t. first depression)" )
                 }
             }
         }

         if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                     is.na(QD_life_5yr)==FALSE])){
             for(j in 1: nphens_q) {
                 if(length(grep("5y",depression_q_label[j]))==1){
                     get.sextab(female[is.na(DD_life_5yr)==FALSE |
                                       is.na(QD_life_5yr)==FALSE],
                                age_cat[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE],
                                SNP.tab[is.na(DD_life_5yr)==FALSE |
                                        is.na(QD_life_5yr)==FALSE,i],
                                birth_decade[is.na(DD_life_5yr)==FALSE |
                                             is.na(QD_life_5yr)==FALSE],
                                depression_q[,j][is.na(DD_life_5yr)==FALSE |
                                                 is.na(QD_life_5yr)==FALSE],
                                stress_combined_exp_messy[is.na(DD_life_5yr)==FALSE |
                                                          is.na(QD_life_5yr)==FALSE]
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Combined Stress Exposure (broader controls) (5yr window w.r.t. first depression)" )
                 }
             }
         }

         if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                     is.na(QD_curr_before)==FALSE])){
             for(j in 1: nphens_q) {
                 if(length(grep("before",depression_q_label[j]))==1){
                     get.sextab(female[is.na(DD_curr_before)==FALSE |
                                       is.na(QD_curr_before)==FALSE],
                                age_cat[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE],
                                SNP.tab[is.na(DD_curr_before)==FALSE |
                                        is.na(QD_curr_before)==FALSE,i],
                                birth_decade[is.na(DD_curr_before)==FALSE |
                                             is.na(QD_curr_before)==FALSE],
                                depression_q[,j][is.na(DD_curr_before)==FALSE |
                                                 is.na(QD_curr_before)==FALSE],
                                stress_combined_exp_messy[is.na(DD_curr_before)==FALSE |
                                                          is.na(QD_curr_before)==FALSE]
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Combined Stress Exposure (broader controls) (timing before w.r.t. current depression)" )
                 }
             }
         }

         if(exists.to.tab2(stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                     is.na(QD_curr_5yr)==FALSE])){
             for(j in 1: nphens_q) {
                 if(length(grep("5y",depression_q_label[j]))==1){
                     get.sextab(female[is.na(DD_curr_5yr)==FALSE |
                                       is.na(QD_curr_5yr)==FALSE],
                                age_cat[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE],
                                SNP.tab[is.na(DD_curr_5yr)==FALSE |
                                        is.na(QD_curr_5yr)==FALSE,i],
                                birth_decade[is.na(DD_curr_5yr)==FALSE |
                                             is.na(QD_curr_5yr)==FALSE],
                                depression_q[,j][is.na(DD_curr_5yr)==FALSE |
                                                 is.na(QD_curr_5yr)==FALSE],
                                stress_combined_exp_messy[is.na(DD_curr_5yr)==FALSE |
                                                          is.na(QD_curr_5yr)==FALSE]
                                ,"Sex","Age Category",
                                SNP.tab.label[i],"Decade of Birth",depression_q_label[j],
                                "Combined Stress Exposure (broader controls) (5yr window w.r.t. current depression)" )
                 }
             }
         }
     }
    }
}

#genotpye freqs for a stress(any) plus a depression_dxorq plus genotype plus sex

tmp<-c(rep(NA,length(add_5http)))
tmp[(!is.na(child_mal_exp) | 
     !is.na(child_mal_quant_z) | 
     !is.na(life_stress_exp) | 
     !is.na(life_stress_quant_z)) | 
     (!is.na(DD_life) |
     !is.na(DD_curr) |
     !is.na(QD_life) |
     !is.na(QD_curr) |
     !is.na(QDz_life) |
     !is.na(QDz_curr))] <- add_5http[(!is.na(child_mal_exp) | 
                       	             !is.na(child_mal_quant_z) | 
                      		     !is.na(life_stress_exp) | 
                      		     !is.na(life_stress_quant_z)) | 
		      		     (!is.na(DD_life) |
                      		     !is.na(DD_curr) |
                      		     !is.na(QD_life) |
                      		     !is.na(QD_curr) |
                      		     !is.na(QDz_life) |
                      		     !is.na(QDz_curr))]

  get.xtab(tmp,add_5http,'filtered__gen5http_S','raw_genotype')

rm(timing_before_curr,timing_before_life,timing_5yr_curr,timing_5yr_life,timing_5yr_curr_q,timing_5yr_life_q)

detach(sd_5htt) 

close(gz1)

