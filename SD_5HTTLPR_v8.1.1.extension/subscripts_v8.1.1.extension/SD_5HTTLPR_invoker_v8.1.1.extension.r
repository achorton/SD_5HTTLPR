################################################################################
# 5-HTTLPR invoker
# function call wrapper
# version 8.1.1.extension
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
source(paste(subscript.dir, "SD_5HTTLPR_meta_functionA_v8.1.1.extension.r", sep=""))

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
source(paste(subscript.dir, "SD_5HTTLPR_xtabs_v8.1.1.extension.r", sep=""))
detach(sd_5htt)
