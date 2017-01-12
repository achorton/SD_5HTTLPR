################################################################################
# 5-HTTLPR invoker
# function call wrapper
# version 8.1
################################################################################
#
# created by Amy Horton, Younghun Han, Chris Amos, 
#            Sarah Hartz, and Rob Culverhouse
#
################################################################################


################################################################################
#############################################################
#                                                           #
# Do not reset the following variables unless instructed to #
# do so by the script during an incomplete run              #
#                                                           #
#############################################################
flag_long_dx=TRUE
flag_long_q=TRUE
flag_A_freq=TRUE
################################################################################
################################################################################

options(warn = 1)

wrapper<-paste("SD_5HTTLPR_script_v",version,".r", sep="")

# Load packages and functions
install.packages(c("rms"),repos="http://cran.wustl.edu")
library(rms)
#library(pequod)
source(paste(subscript.dir, "SD_5HTTLPR_meta_functionA_v8.0.4.r", sep=""))

SITE <- paste(LABEL,substr(POPULATION,1,2),sep="_")
OUTDIR <- paste(INDIR,"Results_",SITE,"/",sep="")
dir.create(OUTDIR)
out.error<-paste(OUTDIR,SITE,"_models_not_run_invariant_input.txt",sep="")
unlink(out.error)
warning_file<-paste(OUTDIR,SITE,"_warnings.log", sep="")
unlink(warning_file)


# Load and calculate variables
source(paste(subscript.dir, "SD_5HTTLPR_frequenciesA_v8.1.1.r", sep=""))
# this script generates SITE_frequencies_AllAges.csv and SITE_frequencies_YAdult.csv, which contain crosstabs and SITE_SNP_frequencies_AllAges.csv and SITE_SNP_frequencies_YAdult.csv, which contain variable by genotype tables

# Frequency tables
attach(sd_5htt)
source(paste(subscript.dir, "SD_5HTTLPR_xtabs_v8.0.1.r", sep=""))
detach(sd_5htt)

outdir<- paste(OUTDIR,"Basic/",sep="")
dir.create(outdir)
unlink(paste(outdir,"*.*", sep=""))

###########################
# All age inclusive       #
# basic regression models #
###########################

source(paste(subscript.dir, "models_all_ages_v8.2.r", sep=""))

outdir_mod<- paste(OUTDIR,"Stress/Moderated/",sep="")
outdir_stress<- paste(OUTDIR,"Stress/",sep="")
dir.create(outdir_stress)
dir.create(outdir_mod)
unlink(paste(outdir_mod,"*.*", sep=""))

##########################################################
# models for moderated regression tests of gene x stress #
##########################################################

source(paste(subscript.dir, "models_moderated_Stress_Combined_all_ages_no_tlimit_v8.0.r", sep=""))
source(paste(subscript.dir, "models_moderated_Stress_Combined_all_ages_before_v8.0.r", sep=""))
source(paste(subscript.dir, "models_moderated_Stress_Combined_all_ages_5yr_v8.0.r", sep=""))
source(paste(subscript.dir, "models_moderated_Stress_ChildMal_all_ages_v8.0.r", sep=""))
source(paste(subscript.dir, "models_moderated_Stress_OtherThanChildMal_all_ages_no_tlimit_v8.0.r", sep=""))
source(paste(subscript.dir, "models_moderated_Stress_OtherThanChildMal_all_ages_before_v8.0.r", sep=""))
source(paste(subscript.dir, "models_moderated_Stress_OtherThanChildMal_all_ages_5yr_v8.1.r", sep=""))


###########################
# Young adult test group  #
# basic regression models #
###########################

source(paste(subscript.dir, "models_yadult_v8.2.r", sep=""))

##########################################################
# models for moderated regression tests of gene x stress #
##########################################################

source(paste(subscript.dir, "models_moderated_Stress_Combined_yadult_no_tlimit_v8.1.r", sep=""))
source(paste(subscript.dir, "models_moderated_Stress_Combined_yadult_before_v8.1.r", sep=""))
source(paste(subscript.dir, "models_moderated_Stress_Combined_yadult_5yr_v8.1.r", sep=""))
source(paste(subscript.dir, "models_moderated_Stress_ChildMal_yadult_v8.1.r", sep=""))
source(paste(subscript.dir, "models_moderated_Stress_OtherThanChildMal_yadult_no_tlimit_v8.1.r", sep=""))
source(paste(subscript.dir, "models_moderated_Stress_OtherThanChildMal_yadult_before_v8.0.r", sep=""))
source(paste(subscript.dir, "models_moderated_Stress_OtherThanChildMal_yadult_5yr_v8.1.r", sep=""))

