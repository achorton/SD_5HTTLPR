#*****************************************************************************
#* 5-HTTLPR GxE analysis variable pre-processing
#* version 8.1
#*****************************************************************************
#*
#* 
#* created by Amy Horton, Younghun Han, Chris Amos, 
#*            Sarah Hartz, and Rob Culverhouse
#*
#*****************************************************************************
     
dep_quant_threshold<-if(dep_q_system=="DSM4_SYMPTOM_COUNT") 1 else 1 
stress_quant_threshold<-if((length(grep("TEST_ALLVARS",INDATA)) > 0) |
                           (length(grep("TEST_DATA",INDATA)) > 0) |
                           (length(grep("LTE_Q",life_stress_q_system)) > 0) ) 2 else 1
child_mal_threshold <- 1
dep_quant_z_threshold<-1
yadult_min_dep_age<-21
gen_5_httlpr_label = "gen5http_S"
rs25531_label = "non_LA_rs25531_haplotype_dosage"
unlink(paste(OUTDIR,"*.*", sep=""))

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
if(!(is.null(num_col))) {
    if( is.na(num_col) | (num_col <  1) ) {
        cat("Error reading datafile",paste(INDIR, INDATA, sep=""),"\nPlease verify that input file is in comma-separated format and ignore the immediately following Error message regarding missing value where TRUE/FALSE needed","\n",file="", append=TRUE)
        cat("Error reading datafile",paste(INDIR, INDATA, sep=""),"\nPlease verify that input file is in comma-separated format","\n",file=warning_file, append=TRUE)
    }
}


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

out.file.ds_descr<-paste(OUTDIR,SITE,"_frequencies.csv",sep="")
unlink(out.file.ds_descr)

write("Id", file=out.file.ds_descr,append=FALSE)
#write("Id", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Id"])>0)(length(na.exclude(ifelse(is.character(as.vector(data1[,names(data1)=="Id"])) | (as.numeric(data1[,names(data1)=="Id"])>=0),data1[,names(data1)=="Id"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("ID", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="ID"])>0)(length(na.exclude(ifelse(is.character(as.vector(data1[,names(data1)=="ID"])) | (as.numeric(data1[,names(data1)=="ID"])>=0),data1[,names(data1)=="ID"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Age", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Age"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Age"]>=0,data1[,names(data1)=="Age"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Birth_decade", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Birth_decade"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Birth_decade"]>=0,data1[,names(data1)=="Birth_decade"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Sex", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Sex"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Sex"]>=0,data1[,names(data1)=="Sex"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Gen5", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",length(grep("-9", data1[,names(data1)=="Gen5"], invert = TRUE)),sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Rs25531", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Rs25531"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Rs25531"]>=0,data1[,names(data1)=="Rs25531"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("rs25531", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="rs25531"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="rs25531"]>=0,data1[,names(data1)=="rs25531"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Dep_dx_curr", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_dx_curr"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_dx_curr"]>=0,data1[,names(data1)=="Dep_dx_curr"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Dep_dx_life", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_dx_life"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_dx_life"]>=0,data1[,names(data1)=="Dep_dx_life"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Dep_quant_curr", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_quant_curr"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_quant_curr"]>=0,data1[,names(data1)=="Dep_quant_curr"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Dep_quant_life", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_quant_life"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_quant_life"]>=0,data1[,names(data1)=="Dep_quant_life"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Life_stress_exp", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Life_stress_exp"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Life_stress_exp"]>=0,data1[,names(data1)=="Life_stress_exp"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Life_stress_q", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Life_stress_q"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Life_stress_q"]>=0,data1[,names(data1)=="Life_stress_q"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Child_mal_exp", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Child_mal_exp"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Child_mal_exp"]>=0,data1[,names(data1)=="Child_mal_exp"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Child_mal_q", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Child_mal_q"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Child_mal_q"]>=0,data1[,names(data1)=="Child_mal_q"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("CTQ_EA", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="CTQ_EA"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="CTQ_EA"]>=0,data1[,names(data1)=="CTQ_EA"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("CTQ_PA", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="CTQ_PA"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="CTQ_PA"]>=0,data1[,names(data1)=="CTQ_PA"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("CTQ_SA", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="CTQ_SA"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="CTQ_SA"]>=0,data1[,names(data1)=="CTQ_SA"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("CTQ_EN", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="CTQ_EN"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="CTQ_EN"]>=0,data1[,names(data1)=="CTQ_EN"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("CTQ_PN", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="CTQ_PN"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="CTQ_PN"]>=0,data1[,names(data1)=="CTQ_PN"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Child_ab_exp", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Child_ab_exp"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Child_ab_exp"]>=0,data1[,names(data1)=="Child_ab_exp"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Dep_curr_ao", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_curr_ao"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_curr_ao"]>=0,data1[,names(data1)=="Dep_curr_ao"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Dep_life_ao", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_life_ao"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_life_ao"]>=0,data1[,names(data1)=="Dep_life_ao"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Age_last_stress_before_dep1", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Age_last_stress_before_dep1"])> 0)(length(na.exclude(ifelse(data1[,names(data1)=="Age_last_stress_before_dep1"]>= -1,data1[,names(data1)=="Age_last_stress_before_dep1"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Age_last_stress_before_dep_curr", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Age_last_stress_before_dep_curr"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Age_last_stress_before_dep_curr"]>= -1,data1[,names(data1)=="Age_last_stress_before_dep_curr"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Stress_before_dep1", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Stress_before_dep1"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Stress_before_dep1"]>=0,data1[,names(data1)=="Stress_before_dep1"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Stress_before_dep_curr", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Stress_before_dep_curr"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Stress_before_dep_curr"]>=0,data1[,names(data1)=="Stress_before_dep_curr"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Dep1_5yr_stress_exp", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep1_5yr_stress_exp"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep1_5yr_stress_exp"]>=0,data1[,names(data1)=="Dep1_5yr_stress_exp"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Dep_curr_5yr_stress_exp", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_curr_5yr_stress_exp"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_curr_5yr_stress_exp"]>=0,data1[,names(data1)=="Dep_curr_5yr_stress_exp"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Dep1_5yr_stress_q", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep1_5yr_stress_q"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep1_5yr_stress_q"]>=0,data1[,names(data1)=="Dep1_5yr_stress_q"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write("Dep_curr_5yr_stress_q", file=out.file.ds_descr,append=TRUE)
write(paste("number defined=",if(length(data1[,names(data1)=="Dep_curr_5yr_stress_q"])>0)(length(na.exclude(ifelse(data1[,names(data1)=="Dep_curr_5yr_stress_q"]>=0,data1[,names(data1)=="Dep_curr_5yr_stress_q"],NA)))) else 0,sep=""), file=out.file.ds_descr,append=TRUE)

write(" ", file=out.file.ds_descr,append=TRUE)
write(" ", file=out.file.ds_descr,append=TRUE)
write(" ", file=out.file.ds_descr,append=TRUE)

rm(out.file.ds_descr)

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

if(!((((flag_ao_curr * flag_alsc)  + flag_T_curr) + flag_no_curr_dx) * (((flag_ao_life * flag_alsf) + flag_T_life) + flag_no_life_dx))) {
    cat("Timing variable(s) missing from dataset","\n",
        "If available, please code (age of onset and age of last stress before first/current depression)","\n",
        "or (three timing interval variables) if lifetime/current depression defined in your dataset","\n",file=warning_file, append=TRUE)
}

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

if(sum(na.exclude(ifelse(10*trunc((2014-age)/10,0)<birth_decade,1,0))) > 0) {
    cat("Error: clash between age and birth_decade for some individuals: ",as.matrix(na.exclude(iid[10*trunc((2014-age)/10,0)<birth_decade])),"\n", file=warning_file, append=TRUE)
}

 #######
 # sex #
 #######

if(!is.na(sum(ifelse(female==2,1,0))) & sum(ifelse(female==2,1,0))>0) {
    cat("Check sex encoding... some individuals encoded as 2.  Sex = 0 (male), 1 (female), or -9 (missing)",as.matrix(na.exclude(iid[female==2])),"\n", file=warning_file, append=TRUE)
}

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
  
    if(err_rs25531 & flag_A_freq) {
        cat("check rs25531 genotype coding... freq(G) (= ",num_rs25531_G/(num_rs25531_A + num_rs25531_G),") > freq(A) (=",num_rs25531_A/(num_rs25531_A + num_rs25531_G),")", "\n",file=warning_file, append=TRUE)
        rm(err_rs25531,num_rs25531_A,num_rs25531_G)
    }

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

if((haplotypes_available=="PHASED") &
   (sum(na.exclude(ifelse(
       ((raw_add_rs25531!=0) & (add_5http==2)) |
       ((raw_add_rs25531==2) & (add_5http==1))
       ,1,0))) >0) ) {
    cat("error: clash between L/S genotype and haplotype coding for at least some individuals:",
        "\n",
        as.matrix(na.exclude(iid[((raw_add_rs25531!=0) &
                                  (add_5http==2)) |
                                 ((raw_add_rs25531==2) &
                                  (add_5http==1))])),
        "\n",file=warning_file, append=TRUE)
}


 ########################
 # depression_diagnosis #
 ########################

replace(dep_dx_curr,dep_dx_curr<0,NA)->dep_dx_curr
replace(dep_dx_life,dep_dx_life<0,NA)->dep_dx_life
replace(dep_curr_ao,dep_curr_ao<0,NA)->dep_curr_ao
replace(dep_life_ao,dep_life_ao<0,NA)->dep_life_ao

if(sum(ifelse(is.na(dep_dx_life),1,0))/length(dep_dx_life) ==1) {
    if (longitudinal_data=="YES")  {
        if (flag_long_dx) {
            cat("lifetime depression diagnosis missing from dataset...If this is not correct contact Amy Horton achorton@wustl.edu\n",file=warning_file, append=TRUE)
        }
    }
}

if(sum(ifelse(na.omit(dep_dx_life<dep_dx_curr),1,0)) >0) {
    cat("error in lifetime/current depression diagnosis, lifetime status does not include current status for at least some individuals:","\n",as.matrix(na.omit(iid[dep_dx_life<dep_dx_curr])),"\n",file=warning_file, append=TRUE)
}

if(sum(ifelse(na.omit(dep_life_ao>dep_curr_ao),1,0)) >0) {
    cat("error in lifetime/current depression age of onset, lifetime onset does not include current onset for at least some individuals:","\n",as.matrix(na.omit(na.exclude(iid[(dep_life_ao>dep_curr_ao)]))),"\n",file=warning_file, append=TRUE)
}

### test ao<=age if dx, ao=age if control
test1<-ifelse(dep_life_ao<=age,1,0)
test2<-ifelse(dep_curr_ao<=age,1,0)
test1c<-ifelse(dep_dx_life==0,1,0)
test2c<-ifelse(dep_dx_curr==0,1,0)
test1d<-ifelse(dep_dx_life==1,1,0)
test2d<-ifelse(dep_dx_curr==1,1,0)
test1e<-ifelse(dep_life_ao==(age+1),1,0)
test2e<-ifelse(dep_curr_ao==(age+1),1,0)

if(sum(na.exclude(ifelse((test1 & test1d) | test1c ,0,1)),na.rm=TRUE)) {
    cat("error: lifetime depression diagnosis clashes with age/age_of_onset for at least some individual cases","\n",as.matrix(na.exclude(ifelse((test1 & test1d)| test1c,NA,iid))),"\n",
        file=warning_file, append=TRUE)
}

if(sum(na.exclude(ifelse((test1e & test1c) | test1d,0,1)),na.rm=TRUE)) {
    cat("error: lifetime depression diagnosis clashes with age/age_of_onset for at least some individual controls","\n",as.matrix(na.exclude(ifelse((test1e & test1c)| test1d,NA,iid))),"\n",
        file=warning_file, append=TRUE)
}

if(sum(na.exclude(ifelse((test2 & test2d) | test2c,0,1)),na.rm=TRUE)) {
    cat("error: current depression diagnosis clashes with age/age_of_onset for at least some individual cases","\n",as.matrix(na.exclude(ifelse((test2 & test2d)|test2c,NA,iid))),"\n",
        file=warning_file, append=TRUE)
}

if(sum(na.exclude(ifelse((test2e & test2c)|test2d,0,1)),na.rm=TRUE)) {
    cat("error: current depression diagnosis clashes with age/age_of_onset for at least some individual controls","\n",as.matrix(na.exclude(ifelse((test2e & test2c)|test2d,NA,iid))),"\n",
        file=warning_file, append=TRUE)
}

rm(test1,test1c,test1d,test2,test2c,test2d,test1e,test2e)

 ############################
 # depression_symptom_count #
 ############################

replace(dep_quant_curr,dep_quant_curr<0,NA)->dep_quant_curr
replace(dep_quant_life,dep_quant_life<0,NA)->dep_quant_life

### check for lifetime info if "LONGITUDINAL" listed in dataset description questions

if(sum(ifelse(is.na(dep_quant_life),1,0))/length(dep_quant_life) ==1) {
    if (longitudinal_data=="YES")  {
        if (flag_long_q) {
            cat("lifetime quantitative depression measure missing from dataset ...If this is not correct, contact Amy Horton achorton@wustl.edu\n",file=warning_file, append=TRUE)
        }
    }
} 

### check consistency bewteen current and lifetime quantitative depression
if(sum(ifelse(na.omit(dep_quant_life<dep_quant_curr),1,0)) >0) {
    cat("error in lifetime/current depression quantitative trait, lifetime status does not include current status for at least some individuals","\n",as.matrix(na.omit(iid[dep_quant_life<dep_quant_curr])),"\n",file=warning_file, append=TRUE)
}

  
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
if(sum(na.exclude(ifelse( ( (life_stress_alf== -1) &
                            (life_stress_exp==0) ) |
                          ( (life_stress_alf>=0) &
                            (life_stress_alf<=age) &
                            (life_stress_exp==1) )
                         ,0,1)[is.na(life_stress_alf)==FALSE &
                               is.na(life_stress_exp)==FALSE])) > 0) {
    cat("error related to non-childhood maltreatment exposure, age_last_stress_before_dep1 does not conform to coding rules in regard to age for at least some individuals","\n",        
        as.matrix(na.omit(iid[!(((life_stress_alf== -1) &
                                 (life_stress_exp==0) ) |
                                ((life_stress_alf>=0) &
                                 (life_stress_alf<=age) &
                                 (life_stress_exp==1) )) &
                              is.na(life_stress_alf)==FALSE &
                              is.na(life_stress_exp)==FALSE])),
        "\n",
        file=warning_file, append=TRUE)
}

## similarly (to above) for Life_stress_exp and Age_last_stress_before_dep_curr
if(sum(na.exclude(ifelse(((life_stress_alc== -1) &
                          (life_stress_exp==0) ) |
                         ((life_stress_alc>=0) &
                          (life_stress_alc<=age) &
                          (life_stress_exp==1)  )
                         ,0,1)[is.na(life_stress_alc)==FALSE &
                               is.na(life_stress_exp)==FALSE])) > 0) {
    cat("error related to non-childhood maltreatment exposure, age_last_stress_before_dep_curr does not conform to coding rules in regard to age for at least some individuals","\n",as.matrix(na.omit(iid[!(((life_stress_alc== -1) &
                          (life_stress_exp==0) ) |
                         ((life_stress_alc>=0) &
                          (life_stress_alc<=age) &
                          (life_stress_exp==1) )) & is.na(life_stress_alc)==FALSE &
                               is.na(life_stress_exp)==FALSE])),"\n",file=warning_file, append=TRUE)
}


 ################
 # stress_count #
 ################

replace(life_stress_quant,life_stress_quant==-9,NA)->life_stress_quant
### check consistency of quantitative and dichotomous life_stress variables, according to insturment used (LTE-Q vs. other)

if(sum(na.exclude(ifelse(((life_stress_alf== -1) &
                          (life_stress_quant<stress_quant_threshold)) |
                         ((life_stress_alf>=0) &
                          (life_stress_alf<=age) &
                          (life_stress_quant>=stress_quant_threshold))
                         ,0,1)[is.na(life_stress_alf)==FALSE &
                               is.na(life_stress_quant)==FALSE])) >0) {
    cat("error related to non-childhood maltreatment quantitative trait, age_last_stress_before_dep1 does not conform to coding rules in regard to age for at least some individuals","\n",as.matrix(na.omit(iid[!(((life_stress_alf== -1) &
                          (life_stress_quant<stress_quant_threshold)) |
                         ((life_stress_alf>=0) &
                          (life_stress_alf<=age) &
                          (life_stress_quant>=stress_quant_threshold))) & is.na(life_stress_alf)==FALSE &
                               is.na(life_stress_quant)==FALSE])),"\n",file=warning_file, append=TRUE)
}
if(sum(na.exclude(ifelse(((life_stress_alc== -1) &
                          (life_stress_quant<stress_quant_threshold)) |
                         ((life_stress_alc>=0) &
                          (life_stress_alc<=age) &
                          (life_stress_quant>=stress_quant_threshold))
                         ,0,1)[is.na(life_stress_alc)==FALSE &
                               is.na(life_stress_quant)==FALSE])) >0) {
    cat("error related to non-childhood maltreatment quantitative trait, age_last_stress_before_dep_curr does not conform to coding rules in regard to age for at least some individuals","\n",as.matrix(na.omit(iid[!(((life_stress_alc== -1) &
                          (life_stress_quant<stress_quant_threshold)) |
                         ((life_stress_alc>=0) &
                          (life_stress_alc<=age) &
                          (life_stress_quant>=stress_quant_threshold))) & is.na(life_stress_alc)==FALSE &
                               is.na(life_stress_quant)==FALSE])),"\n",file=warning_file, append=TRUE)
}
if((life_stress_exp_system=="LTE_Q") &
   ( (sum(ifelse(na.omit((life_stress_quant<stress_quant_threshold) & (life_stress_exp!=0)),1,0)) >0) |
    (sum(ifelse(na.omit((life_stress_quant<stress_quant_threshold) & is.na(life_stress_exp)),1,0)) >0) |
    (sum(ifelse(na.omit((life_stress_quant>=stress_quant_threshold) & (life_stress_exp!=1)),1,0)) >0) |
    (sum(ifelse(na.omit((life_stress_quant>=stress_quant_threshold) & is.na(life_stress_exp)),1,0)) >0)  ) ) {
    cat("error in non-childhood maltreatment stress traits-- exposure status and quantitative trait clash for at least some individuals","\n",as.matrix(na.exclude(iid[((life_stress_quant<stress_quant_threshold) & (life_stress_exp!=0)) |
                                                                                                                                                                       ((life_stress_quant<stress_quant_threshold) &
                                                     is.na(life_stress_exp)) |
                                                                                                                                                                       ((life_stress_quant>=stress_quant_threshold) &
                                                     (life_stress_exp!=1)) |
                                                                                                                                                                       ((life_stress_quant>=stress_quant_threshold) &
                                                     is.na(life_stress_exp))])),"\n",file=warning_file,
        append=TRUE)
}


temp <- data.frame(life_stress_alc,dep_curr_ao,life_stress_alf,dep_life_ao)

if(defined.in.dataset("life_stress_alc",temp) &
   defined.in.dataset("dep_curr_ao",temp) &
   (sum(na.exclude(ifelse(
       !(dep_curr_ao==(age + 1)) &
       !(life_stress_alc== -1) &
       ((dep_curr_ao<=age) &
        (dep_curr_ao>=0) &
        (life_stress_alc >= 0) &
        (dep_curr_ao < life_stress_alc)),1,0))) > 0) ) {
    cat("error: clash between current depression age of onset and age of last stress prior to current depression","\n",
        as.matrix(na.exclude(iid[!(dep_curr_ao==(age + 1)) &
                                 !(life_stress_alc== -1) &
                                 ((dep_curr_ao<=age) &
                                  (dep_curr_ao>=0) &
                                  (life_stress_alc >= 0) &
                                  (dep_curr_ao < life_stress_alc))])),"\n",file=warning_file,
        append=TRUE)
}
if(defined.in.dataset("life_stress_alf",temp) &
   defined.in.dataset("dep_life_ao",temp) &
    (sum(na.exclude(ifelse(
    !(dep_life_ao==(age + 1)) &
   !(life_stress_alf== -1) &
   ((dep_life_ao<=age) &
    (dep_life_ao>=0) &
    (life_stress_alf >= 0) &
   (dep_life_ao < life_stress_alf)),1,0))) > 0) ) {
    cat("error: clash between first depression age of onset and age of last stress prior to first depression","\n",
        as.matrix(na.exclude(iid[!(dep_life_ao==(age + 1)) &
                                 !(life_stress_alf== -1) &
                                 ((dep_life_ao<=age) &
                                  (dep_life_ao>=0) &
                                  (life_stress_alf >= 0) &
                                  (dep_life_ao < life_stress_alf))])),"\n",file=warning_file,
        append=TRUE)
}

 ##########################
 # childhood_maltreatment #
 ##########################

replace(child_mal_exp,child_mal_exp<0,NA)-> child_mal_exp
replace(child_mal_quant,child_mal_quant<0,NA)->child_mal_quant
child_mal_quant2<-c(rep(NA,length(iid)))

### if appropriate CTQ subscores present, calculate child_mal_exp, and 2 quantitative versions (include emotional neglect and emotional abuse... or not.)  Add line to wrapper script indicating which definition used
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
    write.table(" ",file=paste(INDIR,wrapper,sep=""),col.names=FALSE,
                row.names=FALSE,append=TRUE, quote=FALSE)
    write.table(paste("#child_mal_q_system=",ctq_subtype,sep=""),
                file=paste(INDIR,wrapper,sep="")
                ,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
}

 #############################
 # Combined stress defintion #
 ##############################

temp<-data.frame(child_mal_exp,child_mal_quant,p_ab,s_ab,p_neg)
if((defined.in.dataset("child_mal_exp",temp)) &
   (defined.in.dataset("child_mal_quant",temp)) &
   !((defined.in.dataset("p_ab",temp)) &
     (defined.in.dataset("s_ab",temp)) &
     (defined.in.dataset("p_neg",temp))) &
   (sum(na.exclude(ifelse(((child_mal_exp==1) &
                           (child_mal_quant < child_mal_threshold)) |
                          ((child_mal_exp==0) &
                           (child_mal_quant >= child_mal_threshold)),
                          1,0))) > 0)  ) {
    cat("possible error in childhood maltreatment traits: assuming 1 event=exposed, exposure status and quantitative trait clash for at least some individuals",as.matrix(na.exclude(iid[((child_mal_exp==1) &
                           (child_mal_quant < child_mal_threshold)) |
                          ((child_mal_exp==0) &
                           (child_mal_quant >= child_mal_threshold))])),"\n",
        file=warning_file,append=TRUE)
}

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

if((life_stress_exp_system=="LTE_Q") &
   ( (sum(na.omit(ifelse(((sd_timing_5yr_life_q<stress_quant_threshold) & (sd_timing_5yr_life==1) & is.na(sd_timing_5yr_life)==FALSE),1,0))) >0) |
     (sum(na.omit(ifelse(((sd_timing_5yr_life_q>=stress_quant_threshold) & (sd_timing_5yr_life==0) & is.na(sd_timing_5yr_life)==FALSE),1,0))) >0)  ) ) {
    cat("error in non-childhood maltreatment stress traits in 5yr window before first depression-- exposure status and quantitative trait clash for at least some individuals","\n",as.matrix(na.exclude(iid[((sd_timing_5yr_life_q<stress_quant_threshold) & (sd_timing_5yr_life==1) & is.na(sd_timing_5yr_life)==FALSE) | ((sd_timing_5yr_life_q>=stress_quant_threshold) & (sd_timing_5yr_life==0) & is.na(sd_timing_5yr_life)==FALSE)])),"\n",file=warning_file, append=TRUE)
}
if((life_stress_exp_system=="LTE_Q") &
   ( (sum(na.omit(ifelse(((sd_timing_5yr_curr_q<stress_quant_threshold) & (sd_timing_5yr_curr==1) & is.na(sd_timing_5yr_curr)==FALSE),1,0))) >0) |
    (sum(na.omit(ifelse(((sd_timing_5yr_curr_q>=stress_quant_threshold) & (sd_timing_5yr_curr==0) & is.na(sd_timing_5yr_curr)==FALSE),1,0))) >0)  ) ) {
    cat("error in non-childhood maltreatment stress traits in 5yr window before current depression-- exposure status and quantitative trait clash for at least some individuals","\n",as.matrix(na.exclude(iid[((sd_timing_5yr_curr_q<stress_quant_threshold) & (sd_timing_5yr_curr==1) & is.na(sd_timing_5yr_curr)==FALSE) | ((sd_timing_5yr_curr_q>=stress_quant_threshold) & (sd_timing_5yr_curr==0) & is.na(sd_timing_5yr_curr)==FALSE)])),"\n",file=warning_file, append=TRUE)
}

if(sum(ifelse(na.omit(life_stress_alf>dep_curr_ao & sd_timing_before_curr==1),1,0)) >0) {
    cat("error: age_last_stress_before_dep1/dep_curr_ao contradicts Stress_before_dep_curr for at least some individuals:","\n",as.matrix(na.omit(iid[(life_stress_alf>dep_curr_ao & sd_timing_before_curr==1)])),"\n",file=warning_file, append=TRUE)
}

if( (sum(na.omit(ifelse(sd_timing_before_life==0 & sd_timing_5yr_life==1 & variable_window>5,1,0)))) >0) {
    cat("error: non-childhood maltreatment stress exposure in 5yr window before first depression and non-childhood maltreatment stress exposure (prior to first depression) clash for at least some individuals","\n",as.matrix(na.exclude(iid[sd_timing_before_life==0 & sd_timing_5yr_life==1 & variable_window>5])),"\n",file=warning_file, append=TRUE)
}

if( (sum(na.omit(ifelse(sd_timing_before_life==0 & sd_timing_5yr_life_q>=stress_quant_threshold & variable_window>5,1,0)))) >0) {
    cat("error: non-childhood maltreatment stress quantitative trait in 5yr window before first depression and non-childhood maltreatment stress exposure (prior to first depression) clash for at least some individuals","\n",as.matrix(na.exclude(iid[sd_timing_before_life==0 & sd_timing_5yr_life==1 & variable_window>5])),"\n",file=warning_file, append=TRUE)
}

if( (sum(na.omit(ifelse(sd_timing_before_curr==0 & sd_timing_5yr_curr==1 & variable_window>5,1,0)))) >0) {
    cat("error: non-childhood maltreatment stress exposure in 5yr window before current depression and non-childhood maltreatment stress exposure (prior to current depression) clash for at least some individuals","\n",as.matrix(na.exclude(iid[sd_timing_before_curr==0 & sd_timing_5yr_curr==1 & variable_window>5])),"\n",file=warning_file, append=TRUE)
}

if( (sum(na.omit(ifelse(sd_timing_before_curr==0 & sd_timing_5yr_curr_q>=stress_quant_threshold & variable_window>5,1,0)))) >0) {
    cat("error: non-childhood maltreatment stress quantitative trait in 5yr window before current depression and non-childhood maltreatment stress exposure (prior to current depression) clash for at least some individuals","\n",as.matrix(na.exclude(iid[sd_timing_before_curr==0 & sd_timing_5yr_curr==1 & variable_window>5])),"\n",file=warning_file, append=TRUE)
}

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
