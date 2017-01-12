##############################################################################
##### 	CONFIDENTIAL, FOR USE ONLY BY SD-5HTTLPR PARTICIPANTS	##############
##############################################################################
# 5-HTTLPR GxE analysis
# version 8.1.r
##############################################################################
#
# 
# created by Amy Horton, Younghun Han, Chris Amos, Sarah Hartz, and Rob Culverhouse
#
# This script evaluates associations between 5-HTTLPR genotypes,  
# measures of stress exposure, and depression phenotypes.
#
#########################################################################     
#                                                                       #
# For instructions on how to code your dataset for this analysis, see   #
#                                                                       #
#           Formatting_data_for_5-HTTLPR_meta-analysis.doc              #
#                                                                       #
# This file was attached to the package delivery email                  #
#                                                                       #
#########################################################################

########################################################################
#####                                                              #####
#####      Two sections of required user input follow:             #####
#####                                                              #####
########################################################################


######################################################################
###      User-input Section 1. FILE NAMING AND LOCATIONS            ##
###                                                                 ##
###    Please fill-in the following 4 pieces of information about   ##
###   your data file, its location, and the location of the script  ##
######################################################################


#  1.1.  What is the path for the directory containing the dataset and script?  
#        Examples:
#	   On a Mac/Unix/Linux system: "/Users/achorton/SD_5HTTLPR_v8.1/"   
#          On a MS Windows system: "C:/Project/SD_5HTTLPR_v8.1/"
#            NOTE: direction of slashes is different from MS-Windows convention
#
#        NOTE: There must be a terminal '/'

         INDIR="//Users/LucyGedara/Desktop/SD_5HTTLPR_v8.1.1/"


#  1.2.  Enter study name (LABEL will appear in meta-analysis files).

         LABEL = "ALSPAC"


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

         INDATA="UpdateLE_MA2.csv"


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



version<-"8.1.1"
subscript.dir<-paste(INDIR, "subscripts_v",version,"/", sep="")
source(paste(subscript.dir, "SD_5HTTLPR_invoker_v8.1.1.r", sep=""))
