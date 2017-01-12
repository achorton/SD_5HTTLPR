#*****************************************************************************
#* 5-HTTLPR GxE analysis cross-tabulation calls
#* version 8.1.1.extension
#*****************************************************************************
#*
#* 
#* created by Amy Horton, Younghun Han, Chris Amos, 
#*            Sarah Hartz, and Rob Culverhouse
#*
#*****************************************************************************


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
out.file<-paste(OUTDIR,SITE,"_frequencies.csv",sep="")
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

rm(timing_before_curr,timing_before_life,timing_5yr_curr,timing_5yr_life,timing_5yr_curr_q,timing_5yr_life_q)

