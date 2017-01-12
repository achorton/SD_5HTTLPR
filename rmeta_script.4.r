#####################################################################
#                                                                   #
# 08/02/13 by Amy Horton                                            #
#                                                                   #
# R script for 5-HTT analysis to be invoked by Regression_parser.pl #
#                                                                   #
#####################################################################

#invoke program as: cat rmeta_script.r | R --slave --args 
#		    		       	   ---file      in.file.fullpath 
#					   ---effects   random|fixed1 <random|fixed2 ...>
#					   ---outcome   outcome
#		    		       	   ---term      estimate_varname1 <estimate_varname2 ...>
#                                  	   ---study     studyname1 studyname2 <studyname3 ...>
#					   ---label     Beta|OR|HR
#					   ---logscale  TRUE|FALSE
#					   ---clip      left_clip_for_plot right_clip_for_plot
#					   ---axis      left_end_of_x_axis right_end_of_x_axis tick_interval
#					   ---groupname group_label_for_plot_filename
#					   ---stratum   full|0|1|2
#					   ---sex       males-only|females-only|combined-sexes
#					   ---condition condition_prefix
#					   ---age       YA|All
##					   ---model     term_in_model1 <term_in_model2 ...>

#cat /home/amyh/SD_5HTTLPR/rmeta_script.r | R --slave --args ---file /home/amyh/SD_5HTTLPR/Returned/EU_studies_gt49_le20/meta_raw_files/female__age__birth_decade__add_5http__child_mal_exp__add_5http_x_child_mal_exp__null__null.csv ---effects random ---outcome DD_life ---term child_mal_exp ---study COGA_EU CHDS_EU DECC_EU ---label OR ---logscale TRUE ---groupname _testing_102814 ---stratum full ---sex "combined-sexes" ---condition DepDx_DSM4_ ---age All ---clip 0 3 ---axis 0 3 0.5


### turning off command parsing block to allow Yinjiao to edit the above block and run this inside R using:
###   source("/home/amyh/SD_5HTTLPR/rmeta_script.4.r")
#####################################
# Begin section for Yinjiao to edit #
# alter to make plot of your choice #
#####################################

lgscl<-TRUE
effects<-"random"
age<-"All"
result_label<-"OR"
left_clip<-0
right_clip<-3
left_tick<-0
right_tick<-3
intrvl<-0.5
subgroupname<-"_Yinjiao_testing_121715"
stratum<-"full"
sample_sex<-"combined-sexes"
cond<-"DepDx_DSM4_"
outcome<-"DD_life"
term<-"child_mal_exp"
studylist<-c("COGA_EU","CHDS_EU","DECC_EU")
setwd("YOURDIRECTORY")
in.file.name<-"/home/amyh/SD_5HTTLPR/Returned/EU_studies_gt49_le20/meta_raw_files/female__age__birth_decade__add_5http__child_mal_exp__add_5http_x_child_mal_exp__null__null.csv"

##############################################
# end section to edit to alter function call #
##############################################



library(rmeta)
datestamp<-Sys.Date()

### turning off command parsing block to allow Yinjiao to edit the above block and run this inside R using:
###   source("/home/amyh/SD_5HTTLPR/rmeta_script.4.r")
if(0) {
args <- commandArgs()

#[1] "/usr/bin/R"   	      "--slave"
#[3] "--args"                 "all"
#[5] "your"                   "base"

term_flag<-0
study_flag<-0 
effects_flag<-0
for (i in 4:length(args)) {
#    cat(paste("Args[",i,"] = ",args[i],"\n",sep=""))

    if (length(grep("---",args[i]))>0) {
       term_flag<-0
       study_flag<-0 
       effects_flag<-0
       outcome_flag<-0
       model_flag<-0
       if (args[i] == "---file") {
       	  in.file.fullpath=args[i+1]
       	  wd<-dirname(in.file.fullpath)
       	  setwd(paste(wd))
       	  in.file.name<-basename(in.file.fullpath)
       } else {	  
       if (args[i] == "---effects") {
	    effects_flag<-1
       } else {	
       if (args[i] == "---outcome") {
	    outcome_flag<-1
       } else {	  
       if (args[i] == "---term") {
            term_flag<-1
       } else {	  
       if (args[i] == "---age") {
            age<-args[i+1]
       } else {
       if (args[i] == "---study") {
            study_flag<-1
       } else {	  
       if (args[i] == "---label") {
       	    result_label<-args[i+1]
       } else {	  
       if (args[i] == "---logscale") {
       	    lgscl<-as.logical(args[i+1])
       }	else {  
       if (args[i] == "---clip") {
       	    left_clip<-as.numeric(args[i+1])
	    right_clip<-as.numeric(args[i+2])
       } else {	  
       if (args[i] == "---axis") {
       	    left_tick<-as.numeric(args[i+1])
	    right_tick<-as.numeric(args[i+2])
       	    intrvl<-as.numeric(args[i+3])
       } else {	  
       if (args[i] == "---groupname") {
            subgroupname<-args[i+1]
       } else {
       if (args[i] == "---stratum") {
       	  stratum<-args[i+1]
       } else {
       if (args[i] == "---sex") {
          sample_sex<-args[i+1]
       } else {
       if (args[i] == "---condition") {
       	  cond<-args[i+1]
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

    } else {
      	   if (term_flag) { 
	      if(!(term_flag==2)) {
	        term<-args[i]
		term_flag=2
	      } else term<-append(term,args[i])
	   } else {
      	   if (effects_flag) { 
	      if(!(effects_flag==2)) {
	      	effects<-args[i]
		effects_flag=2
	      } else effects<-append(effects,args[i])
	   }  else {
      	   if (study_flag) { 
	      if(!(study_flag==2)) {
	      	studylist<-args[i]
		study_flag=2
	      } else studylist<-append(studylist,args[i])
	    }  else {
      	   if (outcome_flag) { 
	      if(!(outcome_flag==2)) {
	      	outcome<-args[i]
		outcome_flag=2
	      } else outcome<-append(outcome,args[i])
	    }
	    }
	 }
      }
   }
}
} ######### end turned off block


out.file.het="heterogeneities_by_test.txt"

if(!exists("wd")) wd<-getwd() 
if(!exists("effects")) effects <-"random"
if(!exists("outcome")) outcome <-"QD"
if(!exists("result_label")) result_label<-"Beta"
if(!exists("lgscl")) lgscl<-FALSE
if(!exists("right_clip")) right_clip<-1.5
if(!exists("left_clip")) left_clip<--1.5
if(!exists("right_tick")) right_tick<-1.5
if(!exists("left_tick")) left_tick<--1.5
if(!exists("intrvl")) intrvl<-0.25
if(!exists("subgroupname")) subgroupname<-"_blankname"
if(!exists("stratum")) stratum<-"full"
if(!exists("sample_sex")) sample_sex<-"combined-sexes"
if(!exists("cond")) cond<-"DepDx_DSM4_"

if(0) {
cat(paste("working dir:",wd,"\n",sep=""))
cat(paste("infile:",in.file.name,"\n",sep=""))
cat(paste("Length(effects) = ",length(effects),"\n",sep=""))

cat("effects:")
for (j in 1:max(1,length(effects))) cat(effects[j]," ")
cat("\n")

cat("outcome:")
for (j in 1:max(1,length(outcome))) cat(outcome[j]," ")
cat("\n")

cat("term:")
for (j in 1:max(1,length(term))) cat(term[j]," ")
cat("\n")

cat("study:")
for (j in 1:max(1,length(studylist))) cat(studylist[j]," ")
cat("\n")

cat(paste("label:",result_label,"\n",sep=""))
cat(paste("logscale:",lgscl,"\n",sep=""))
cat(paste("clip:"," left: ",left_clip," right: ",right_clip,"\n",sep=""))
cat(paste("axis:"," left: ",left_tick," right: ",right_tick," spacing: ",intrvl,"\n",sep=""))
cat(paste("groupname:",subgroupname,"\n",sep=""))
cat(paste("stratum: ",stratum,"\n",sep=""))
cat(paste("sample_sex: ",sample_sex,"\n",sep=""))
cat(paste("cond: ",cond,"\n",sep=""))

}############

source("/home/amyh/SD_5HTTLPR/title_forestplot.R")

codingstringparse<-function(vec) {
    genestring<-""
    if(length(vec) > 0) {
        for(i in 1:length(vec)) {
	      if(length(grep("dum",vec)) > 0) {
	          if(length(grep("dum1",vec)) > 0 &&
		     length(grep("dum2",vec)) > 0) {
	                 genestring<-"dummy_coded.both"
		    } else genestring<-"dummy_coded.single"
	      } else if(length(grep("dom",vec)) > 0) {
	          genestring<-"dominant"
	      } else if(length(grep("rec",vec)) > 0) {
	          genestring<-"recessive"
	      } else if(length(grep("add",vec)) > 0) {
	          genestring<-"additive"
	      }
	  }
    }
    return(genestring)
}


genestringparse<-function(string) {
    genestring<-""
    string<-gsub(".csv","",string)
    string<-gsub("__null","",string)
    terms<-strsplit(string,"__")[[1]]

    gene<-c(terms[grep("5http",terms)],terms[grep("rs25531",terms)])
    if(length(gene) > 0) {
        for(i in 1:length(gene)) {
	      if(length(grep("5http",gene)) > 0) {
	          if(length(grep("dum",gene)) > 0) {
		      genestring<-"dummy-coded S/L"
		  } else if(length(grep("dom",gene)) > 0) {
		      genestring<-"S/L (S dominant)"
		  } else if(length(grep("rec",gene)) > 0) {
		      genestring<-"S/L (S recessive)"
		  } else if(length(grep("add",gene)) > 0) {
		      genestring<-"S/L (additive)"
		  }
	      } else if(length(grep("rs25531",gene)) > 0) {
	          if(length(grep("dum",gene)) > 0) {
		      genestring<-"dummy-coded haplotype"
		  } else if(length(grep("dom",gene)) > 0) {
		      genestring<-"haplotype (LA recessive)"
		  } else if(length(grep("rec",gene)) > 0) {
		      genestring<-"haplotype (LA dominant)"
		  } else if(length(grep("add",gene)) > 0) {
		      genestring<-"haplotype (additive)"
		  }
	      }  
	 }
    }
    return(genestring)
}


modelstringparse<-function(string) {
    covarstring<-""
    string<-gsub(".csv","",string)
    string<-gsub("__null","",string)
    terms<-strsplit(string,"__")[[1]]

    stress<-c(terms[grep("_exp",terms)],terms[grep("_quant",terms)])
    gene<-c(terms[grep("5http",terms)],terms[grep("rs25531",terms)])

    interaction<-terms[grep("_x_",terms)]
    stress<-stress[grep("_x_",stress,invert=TRUE)]
    gene<-gene[grep("_x_",gene,invert=TRUE)]

#    terms<-strsplit(string,"__")[[1]]
    terms<-terms[grep("_exp",terms,invert=TRUE)]
    terms<-terms[grep("_quant",terms,invert=TRUE)]
    terms<-terms[grep("_5http",terms,invert=TRUE)]
    terms<-terms[grep("_rs25531",terms,invert=TRUE)]

#    if(!is.na(terms[1]) & length(terms) > 0) {
    if(length(terms) > 0) {
        if(!is.na(terms[1])) {
            covarstring<-terms[1]
            if(length(terms) > 1) {
                for(i in 2:length(terms)) {
	            covarstring<-paste(covarstring," + ",terms[i],sep="")
		}
	    }
	}
    }  
#    if(!is.na(gene[1]) & length(gene) > 0) {
    if(length(gene) > 0) {
        if(!is.na(gene[1])) {
    	    covarstring<-paste(covarstring," + Gene",sep="")
	}
    }
#    if(!is.na(stress) & length(stress) > 0) {
    if(length(stress) > 0) {
        if(!is.na(stress)) {
    	    covarstring<-paste(covarstring," + Stress",sep="")
	}
    }
    if(length(interaction) > 0) {
	covarstring<-paste(covarstring," + GxE",sep="")
    }
    return(covarstring)
}


outname.maker<-function(string,stratum,effects,outcome) {
    string<-gsub(".csv","",string)
    string<-gsub("__null","",string)
    terms<-strsplit(string,"__")[[1]]

    stress<-c(terms[grep("_exp",terms)],terms[grep("_quant",terms)])
    gene<-c(terms[grep("5http",terms)],terms[grep("rs25531",terms)])

    interaction<-terms[grep("_x_",terms)]
    stress<-stress[grep("_x_",stress,invert=TRUE)]
    gene<-gene[grep("_x_",gene,invert=TRUE)]

    terms<-terms[grep("_exp",terms,invert=TRUE)]
    terms<-terms[grep("_quant",terms,invert=TRUE)]
    terms<-terms[grep("_5http",terms,invert=TRUE)]
    terms<-terms[grep("_rs25531",terms,invert=TRUE)]


#empty values in directories?

    covarstring<-character(0)
    if(length(terms) > 0) {
        if(!is.na(terms[1])) {
            covarstring<-if(length(grep("female",terms)) > 0) {
                             if(length(grep("age",terms)) > 0) {
            	                 if(length(grep("birth_decade",terms)) > 0) {
	            	             "D"
                                 } else "B"
                             } else 
            	                 if(length(grep("birth_decade",terms)) > 0) {
	        	             "C"
                                 } else "A"
                         } else if(length(grep("age",terms)) > 0) {
            	             if(length(grep("birth_decade",terms)) > 0) {
	       	                 "G"
                             } else "E"
                         } else 
            	             if(length(grep("birth_decade",terms)) > 0) {
	        	         "F"
                             } else "H"
	} else covarstring<-"H"
    } else covarstring<-"H"

    namestring<-"LEADSTRING"
    if(length(covarstring) > 0) {
        namestring<-paste(namestring,covarstring,".",sep="")
    }  
    if(length(grep("full",stratum)) == 0 ) {
        namestring<-paste(namestring,stratum,".",sep="")
    }
    if(length(effects) > 0) {
        namestring<-paste(namestring,effects,".",sep="")
    }
    if(length(outcome) > 0) {
        namestring<-paste(namestring,outcome,".",sep="")
    }
#    if(!is.na(gene[1]) & length(gene) > 0) {
    if(length(gene) > 0) {
        if(!is.na(gene[1])) {
            variant<-variantstringparse(gene)
            namestring<-paste(namestring,variant,".",sep="")
	}
    }
#    if(!is.na(stress) & length(stress) > 0) {
    if(length(stress) > 0) {
        if(!is.na(stress)) {
            namestring<-paste(namestring,stress,".",sep="")
	}
    }
    if(length(interaction) > 0) {
        namestring<-paste(namestring,"GxE",".",sep="")
    }

    namestring<-paste(namestring,"pdf",sep="")
    namestring<-gsub("LEADSTRING","",namestring)
}


stressstringparse<-function(string) {
    stressstring<-""
    string<-gsub(".csv","",string)
    string<-gsub("__null","",string)
    terms<-strsplit(string,"__")[[1]]

    stress<-c(terms[grep("_exp",terms)],terms[grep("_quant",terms)])
    if(length(stress) > 0) {
        for(i in 1:length(stress)) {
	      if(length(grep("life_stress",stress)) > 0) {
	          if(length(grep("exp",stress)) > 0) {
		      stressstring<-"Non-Childhood Maltreatment Stress Exposure"
		  } else if(length(grep("exp",stress)) > 0) {
		      stressstring<-"Non-Childhood Maltreatment Quantitative"
		  }
	      } else   if(length(grep("child_mal",stress)) > 0) {
	          if(length(grep("exp",stress)) > 0) {
		      stressstring<-"Childhood Maltreatment Stress Exposure"
		  } else if(length(grep("exp",stress)) > 0) {
		      stressstring<-"Childhood Maltreatment Quantitative"
		  }
	      } else   if(length(grep("stress_combined",stress)) > 0) {
	          if(length(grep("clean",stress)) > 0) {
		      stressstring<-"Combined Stress Exposure (clean controls)"
		  } else if(length(grep("messy",stress)) > 0) {
		      stressstring<-"Combined Stress Exposure (controls may be missing one stress)"
		  }
	      }
	}
    }
    return(stressstring)
}

variantstringparse<-function(vec) {
    genestring<-""
    if(length(vec) > 0) {
        for(i in 1:length(vec)) {
	      if(length(grep("5http",vec)) > 0) 
	          genestring<-"5http" else 
		  if(length(grep("rs25531",vec)) > 0)
	              genestring<-"rs25531"
        }
    }
    return(genestring)
}


do_rmeta_het<-function(in.file.name,cond,age,outcome,term,studylist,subgroupname,stratum,sample_sex,out.file.het,out.file.main,result_label,lgscl,effects,left_clip,right_clip,left_tick,right_tick,intrvl,title=""){
  data1=read.table(file=in.file.name,sep="\t",header=TRUE)

#reading in necessary variables
# this part reads in the necessary values from the data.frame named CPD into strings 
# which are necessary for the functions being used here.
# "full.add.beta" => full strat, blue SNP, additive model, beta values
  names<-as.character(data1$name) 
  data1$name<-gsub("-","_",names)
  studylist<-gsub("-","_",studylist)

  full.add.name=data1[data1$strat==stratum
		& data1$snp==term 
		& data1$sample_sex==sample_sex
		& data1$age_group==age
		& data1$phen==outcome
		& data1$cond==cond,]$name


  full.add.name.trimmed<-""
  for (i in 1:length(full.add.name)){
#  for (i in 1:length(data1$namestudy)){
    if(length(grep(full.add.name[i],studylist))>0){
#    if(length(grep(data1$namestudy[i],studylist))>0){
	full.add.name.trimmed<-append(full.add.name.trimmed,as.character(full.add.name[i]))
    }	
  }



  full.add.beta=data1[data1$strat==stratum
	& data1$snp==term 
	& data1$sample_sex==sample_sex
	& data1$age_group==age
	& data1$phen==outcome
	& data1$cond==cond,]$beta

#subsetting to only those studies listed in vector studylist
  full.add.beta.trimmed<-""
  for (i in 1:length(full.add.beta)){
    if(length(grep(full.add.name[i],studylist))>0){
	full.add.beta.trimmed<-append(full.add.beta.trimmed,full.add.beta[i])
    }	
  }
#  for (i in 1:length(data1$namestudy)){
#    if(length(grep(data1$namestudy[i],studylist))>0){
#	full.add.beta.trimmed<-append(full.add.beta.trimmed,full.add.beta[i])
#    }	
#  }



  full.add.se=  data1[data1$strat==stratum
		& data1$snp==term 
		& data1$sample_sex==sample_sex
		& data1$age_group==age
		& data1$phen==outcome
		& data1$cond==cond,]$se

  full.add.se.trimmed<-""
  for (i in 1:length(full.add.se)){
    if(length(grep(full.add.name[i],studylist))>0){
#  for (i in 1:length(data1$namestudy)){
#    if(length(grep(data1$namestudy[i],studylist))>0){
	full.add.se.trimmed<-append(full.add.se.trimmed,full.add.se[i])
    }	
  }


  full.add.N=   data1[data1$strat==stratum
		& data1$snp==term 
		& data1$sample_sex==sample_sex
		& data1$age_group==age
		& data1$phen==outcome
		& data1$cond==cond,]$combined

  full.add.N.trimmed<-""
  for (i in 1:length(full.add.N)){
    if(length(grep(full.add.name[i],studylist))>0){
#  for (i in 1:length(data1$namestudy)){
#    if(length(grep(data1$namestudy[i],studylist))>0){
	full.add.N.trimmed<-append(full.add.N.trimmed,full.add.N[i])
    }	
  }


  if(length(na.exclude(full.add.beta.trimmed)) > 1){
  	full.add.beta.trimmed<-  as.numeric(na.exclude(full.add.beta.trimmed[-1]))
  	full.add.se.trimmed<-    as.numeric(na.exclude(full.add.se.trimmed[-1]))
  	full.add.name.trimmed<-as.character(na.exclude(full.add.name.trimmed[-1]))
  	full.add.N.trimmed<-     as.numeric(na.exclude(full.add.N.trimmed[-1]))
  if(length(full.add.beta.trimmed)<2) stop()


  full.add.beta.trimmed.AE<-full.add.beta.trimmed[grep("_AE",full.add.name.trimmed)]
  full.add.se.trimmed.AE<-full.add.se.trimmed[grep("_AE",full.add.name.trimmed)]
  full.add.N.trimmed.AE<-full.add.N.trimmed[grep("_AE",full.add.name.trimmed)]
  full.add.name.trimmed.AE<-full.add.name.trimmed[grep("_AE",full.add.name.trimmed)]

  full.add.beta.trimmed.AF<-full.add.beta.trimmed[grep("_AF",full.add.name.trimmed)]
  full.add.se.trimmed.AF<-full.add.se.trimmed[grep("_AF",full.add.name.trimmed)]
  full.add.N.trimmed.AF<-full.add.N.trimmed[grep("_AF",full.add.name.trimmed)]
  full.add.name.trimmed.AF<-full.add.name.trimmed[grep("_AF",full.add.name.trimmed)]

  full.add.beta.trimmed.AS<-full.add.beta.trimmed[grep("_AS",full.add.name.trimmed)]
  full.add.se.trimmed.AS<-full.add.se.trimmed[grep("_AS",full.add.name.trimmed)]
  full.add.N.trimmed.AS<-full.add.N.trimmed[grep("_AS",full.add.name.trimmed)]
  full.add.name.trimmed.AS<-full.add.name.trimmed[grep("_AS",full.add.name.trimmed)]

  full.add.beta.trimmed.HP<-full.add.beta.trimmed[grep("_HP",full.add.name.trimmed)]
  full.add.se.trimmed.HP<-full.add.se.trimmed[grep("_HP",full.add.name.trimmed)]
  full.add.N.trimmed.HP<-full.add.N.trimmed[grep("_HP",full.add.name.trimmed)]
  full.add.name.trimmed.HP<-full.add.name.trimmed[grep("_HP",full.add.name.trimmed)]

  full.add.beta.trimmed.PI<-full.add.beta.trimmed[grep("_PI",full.add.name.trimmed)]
  full.add.se.trimmed.PI<-full.add.se.trimmed[grep("_PI",full.add.name.trimmed)]
  full.add.N.trimmed.PI<-full.add.N.trimmed[grep("_PI",full.add.name.trimmed)]
  full.add.name.trimmed.PI<-full.add.name.trimmed[grep("_PI",full.add.name.trimmed)]

  full.add.beta.trimmed.EU<-full.add.beta.trimmed[grep("_EU",full.add.name.trimmed)]
  full.add.se.trimmed.EU<-full.add.se.trimmed[grep("_EU",full.add.name.trimmed)]
  full.add.N.trimmed.EU<-full.add.N.trimmed[grep("_EU",full.add.name.trimmed)]
  full.add.name.trimmed.EU<-full.add.name.trimmed[grep("_EU",full.add.name.trimmed)]




  full.add.beta.trimmed.AE<-full.add.beta.trimmed.AE[order(full.add.name.trimmed.AE)]
  full.add.se.trimmed.AE<-full.add.se.trimmed.AE[order(full.add.name.trimmed.AE)]
  full.add.N.trimmed.AE<-full.add.N.trimmed.AE[order(full.add.name.trimmed.AE)]
  full.add.name.trimmed.AE<-full.add.name.trimmed.AE[order(full.add.name.trimmed.AE)]

  full.add.beta.trimmed.AF<-full.add.beta.trimmed.AF[order(full.add.name.trimmed.AF)]
  full.add.se.trimmed.AF<-full.add.se.trimmed.AF[order(full.add.name.trimmed.AF)]
  full.add.N.trimmed.AF<-full.add.N.trimmed.AF[order(full.add.name.trimmed.AF)]
  full.add.name.trimmed.AF<-full.add.name.trimmed.AF[order(full.add.name.trimmed.AF)]

  full.add.beta.trimmed.AS<-full.add.beta.trimmed.AS[order(full.add.name.trimmed.AS)]
  full.add.se.trimmed.AS<-full.add.se.trimmed.AS[order(full.add.name.trimmed.AS)]
  full.add.N.trimmed.AS<-full.add.N.trimmed.AS[order(full.add.name.trimmed.AS)]
  full.add.name.trimmed.AS<-full.add.name.trimmed.AS[order(full.add.name.trimmed.AS)]

  full.add.beta.trimmed.HP<-full.add.beta.trimmed.HP[order(full.add.name.trimmed.HP)]
  full.add.se.trimmed.HP<-full.add.se.trimmed.HP[order(full.add.name.trimmed.HP)]
  full.add.N.trimmed.HP<-full.add.N.trimmed.HP[order(full.add.name.trimmed.HP)]
  full.add.name.trimmed.HP<-full.add.name.trimmed.HP[order(full.add.name.trimmed.HP)]

  full.add.beta.trimmed.PI<-full.add.beta.trimmed.PI[order(full.add.name.trimmed.PI)]
  full.add.se.trimmed.PI<-full.add.se.trimmed.PI[order(full.add.name.trimmed.PI)]
  full.add.N.trimmed.PI<-full.add.N.trimmed.PI[order(full.add.name.trimmed.PI)]
  full.add.name.trimmed.PI<-full.add.name.trimmed.PI[order(full.add.name.trimmed.PI)]

  full.add.beta.trimmed.EU<-full.add.beta.trimmed.EU[order(full.add.name.trimmed.EU)]
  full.add.se.trimmed.EU<-full.add.se.trimmed.EU[order(full.add.name.trimmed.EU)]
  full.add.N.trimmed.EU<-full.add.N.trimmed.EU[order(full.add.name.trimmed.EU)]
  full.add.name.trimmed.EU<-full.add.name.trimmed.EU[order(full.add.name.trimmed.EU)]


  full.add.beta.trimmed<-c(full.add.beta.trimmed.EU,
			       full.add.beta.trimmed.AE,
                               full.add.beta.trimmed.AF,
                               full.add.beta.trimmed.AS,
                               full.add.beta.trimmed.HP,
                               full.add.beta.trimmed.PI)
  full.add.se.trimmed<-c(full.add.se.trimmed.EU,
			     full.add.se.trimmed.AE,
                             full.add.se.trimmed.AF,
                             full.add.se.trimmed.AS,
                             full.add.se.trimmed.HP,
                             full.add.se.trimmed.PI)
  full.add.N.trimmed<-c(full.add.N.trimmed.EU,
			    full.add.N.trimmed.AE,
                            full.add.N.trimmed.AF,
                            full.add.N.trimmed.AS,
                            full.add.N.trimmed.HP,
                            full.add.N.trimmed.PI)
  full.add.name.trimmed<-c(full.add.name.trimmed.EU,
			       full.add.name.trimmed.AE,
                               full.add.name.trimmed.AF,
                               full.add.name.trimmed.AS,
                               full.add.name.trimmed.HP,
                               full.add.name.trimmed.PI)


#  full.add.beta.trimmed<-full.add.beta.trimmed[order(full.add.name.trimmed)]
#  full.add.se.trimmed<-full.add.se.trimmed[order(full.add.name.trimmed)]
#  full.add.N.trimmed<-full.add.N.trimmed[order(full.add.name.trimmed)]
#  full.add.name.trimmed<-full.add.name.trimmed[order(full.add.name.trimmed)]

#calculating the sample total
        total=sum(full.add.N.trimmed,na.rm=TRUE)

#running rmeta function
        effects.full.add=meta.summaries(full.add.beta.trimmed,full.add.se.trimmed,
                                        names=full.add.name.trimmed,logscale=lgscl,
					method=paste(effects,sep=""))

#write output heterogeneity statistic (het,Q,P,I^2) to output file, out.file.het
       if (effects=="random") {
          write.table(paste(in.file.name,"\tcondition: ",cond,"\tage: ",age,"\toutcome: ",outcome,"\tterm: ",term,"\tstratum: ",stratum,"\tsex: ",sample_sex,"\tsubgroup: ",subgroupname,

	  		"\theterogeneity_variance: ",
			effects.full.add$tau,
			"\tQ: ",
		    	effects.full.add$het[1],
			"\theterogeneity_df: ",		
			effects.full.add$het[2],
			"\theterogeneity_P: ",
			effects.full.add$het[3],
     			"\theterogeneity_I^2: ",
		    	(effects.full.add$het[1]-effects.full.add$het[2])*100/effects.full.add$het[1],
			paste("\tmeta_",result_label,": ",sep=""),
			if(lgscl==TRUE)
				format(exp(effects.full.add$summary),digits=3) else
		    		format((effects.full.add$summary),digits=3)
			,"\tmeta_lower_5: ",
			if(lgscl==TRUE)
				format(exp(effects.full.add$summary-
					1.96*effects.full.add$se.summary), digits=2) else	
				format((effects.full.add$summary-
					1.96*effects.full.add$se.summary),digits=2)	
			,"\tmeta_upper_5: ",				
		  	if(lgscl==TRUE)
				format(exp(effects.full.add$summary+
					1.96*effects.full.add$se.summary),digits=3) else
				format((effects.full.add$summary+
					1.96*effects.full.add$se.summary),digits=3)
			,"\tmeta_P: ",		  				
			format(pnorm(-1*abs(effects.full.add$test[1]))*(2),digits=2),
			sep=""),
		    file=out.file.het,append=TRUE,col.names=FALSE, row.names=FALSE,
		    quote=FALSE)

  	  cat(paste(in.file.name,"\ncondition: ",cond,"\nage: ",age,"\noutcome: ",outcome,"\nterm: ",term,"\nstratum: ",stratum,"\nsex: ",sample_sex,"\nsubgroup: ",subgroupname,"\n",sep=" "))
  	  cat(paste("heterogeneity variance","Q?","		df","het P-value",
	          "		I^2",result_label,"lower","upper","P","\n",sep="\t"))
  	  cat(paste(effects.full.add$tau,effects.full.add$het[1],effects.full.add$het[2],
		  effects.full.add$het[3],(effects.full.add$het[1]-
		  effects.full.add$het[2])*100/effects.full.add$het[1],
		  if(lgscl==TRUE)
		    format(exp(effects.full.add$summary),digits=3) else
		    format((effects.full.add$summary),digits=3),
		  if(lgscl==TRUE)
					       format(exp(effects.full.add$summary-
							  1.96*
							  effects.full.add$se.summary),
					              digits=2) else	
					       format((effects.full.add$summary-
							  1.96*
							  effects.full.add$se.summary),
					              digits=2)	
					     ,if(lgscl==TRUE)
						format(exp(effects.full.add$summary+
							  1.96*
							  effects.full.add$se.summary),
						      digits=3) else
						format((effects.full.add$summary+
							  1.96*
							  effects.full.add$se.summary),
						      digits=3),
		  format(pnorm(-1*abs(effects.full.add$test[1]))*(2),digits=2)
		  ,sep="\t"))
  	  cat("\n")
       }

### Yinjiao!!  throw in a function to "translate" full.add.name.trimmed case-by-cae to the desired printout name ####


	tabletext=cbind(c("Study",as.character(full.add.name.trimmed),NA,NA,NA,NA,"Summary"),
		c(result_label,
		  if(lgscl==TRUE)
		    format(exp(effects.full.add$effects),digits=2) else
		    format((effects.full.add$effects),digits=2)
                  ,NA,NA,NA,result_label,
		  if(lgscl==TRUE)
		    format(exp(effects.full.add$summary),digits=3) else
		    format((effects.full.add$summary),digits=3)
		  ),
		c("95% C.I.",paste("(",paste(if(lgscl==TRUE)
					       format(exp(effects.full.add$effects-
							  1.96*
							  effects.full.add$stderrs),
					              digits=2) else	
					       format((effects.full.add$effects-
							  1.96*
							  effects.full.add$stderrs),
					              digits=2)	
					     ,if(lgscl==TRUE)
						format(exp(effects.full.add$effects+
							  1.96*
							  effects.full.add$stderrs),
						      digits=3) else
						format((effects.full.add$effects+
							  1.96*
							  effects.full.add$stderrs),
						      digits=3)
		  ,sep="-"),")",sep=""),NA,"Total",NA,"95% C.I.",
		  paste("(",paste(if(lgscl==TRUE)
				    format(exp(effects.full.add$summary-
			                       1.96*effects.full.add$se.summary),
				           digits=3) else
				    format((effects.full.add$summary-
			                       1.96*effects.full.add$se.summary),
				           digits=3)
				  ,if(lgscl==TRUE)
				    format(exp(effects.full.add$summary+
					       1.96*effects.full.add$se.summary),
					   digits=3) else
				    format((effects.full.add$summary+
					       1.96*effects.full.add$se.summary),
					   digits=3)
				  ,sep="-"),")",sep="")),
#		c(" ",rep(NA,length(as.character(full.add.name.trimmed))),NA,NA,NA,NA,NA),
		c("N",as.character(full.add.N.trimmed),NA,
                                     as.character(total),NA,"P",
		                     format(pnorm(-1*abs(effects.full.add$test[1]))*
				                  (2),digits=2)))



	if(lgscl==TRUE) {
		m=c(NA,exp(effects.full.add$effects),NA,NA,NA,NA,
		    exp(effects.full.add$summary))
		l=c(NA,exp(effects.full.add$effects-
			   1.96*effects.full.add$stderrs),left_clip,
		    NA,NA,NA,
	    	    exp(effects.full.add$summary-
		        1.96*effects.full.add$se.summary))
		u=c(NA,exp(effects.full.add$effects+
			   1.96*effects.full.add$stderrs),right_clip,
		    NA,NA,NA,
	    	    exp(effects.full.add$summary+
			1.96*effects.full.add$se.summary))
	    } else {
		m=c(NA,(effects.full.add$effects),NA,NA,NA,NA,
		    (effects.full.add$summary))
		l=c(NA,(effects.full.add$effects-
			1.96*effects.full.add$stderrs),left_clip,
		    NA,NA,NA,
	    	    (effects.full.add$summary-
		     1.96*effects.full.add$se.summary))
		u=c(NA,(effects.full.add$effects+
			1.96*effects.full.add$stderrs),right_clip,
		    NA,NA,NA,
	    	    (effects.full.add$summary+
		     1.96*effects.full.add$se.summary))
	    }

	    l2<-ifelse(l<left_clip,left_clip,l)
	    u2<-ifelse(u>right_clip,right_clip,u)
#write to a pdf file
       dir.create(paste(wd,"/N_summary",sep=""), mode = "0777")
#       model_name<-strsplit(out.file.main,"__+")[[1]]
       model_name<-c(outcome,strsplit(in.file.name,"__+")[[1]],cond,age,outcome,term,effects,stratum,sample_sex,subgroupname)

	N_summary_text=cbind(c(rep(model_name[2],length(full.add.name.trimmed) + 1)),
				c(rep(model_name[3],length(full.add.name.trimmed) + 1)),
				c(rep(model_name[4],length(full.add.name.trimmed) + 1)),
				c(rep(model_name[5],length(full.add.name.trimmed) + 1)),
				c(rep(model_name[6],length(full.add.name.trimmed) + 1)),
				c(rep(model_name[7],length(full.add.name.trimmed) + 1)),
				c(rep(model_name[8],length(full.add.name.trimmed) + 1)),
				c(rep(strsplit(model_name[9],".csv")[[1]][1],length(full.add.name.trimmed) + 1)),
				c(rep(model_name[10],length(full.add.name.trimmed) + 1)),
				c(rep(model_name[11],length(full.add.name.trimmed) + 1)),
				c(rep(model_name[12],length(full.add.name.trimmed) + 1)),
				c(rep(model_name[13],length(full.add.name.trimmed) + 1)),
				c(rep(model_name[15],length(full.add.name.trimmed) + 1)),
				c(rep(model_name[16],length(full.add.name.trimmed) + 1)),
				c(rep(model_name[17],length(full.add.name.trimmed) + 1)),
				c(as.character(full.add.name.trimmed),"Total"),
				c(rep(NA,length(full.add.name.trimmed)),length(full.add.name.trimmed)),
				c(as.character(full.add.N.trimmed),as.character(total)))

	write.table(N_summary_text, file="N_summary/N_summary.csv", quote=FALSE, sep=", ", na=".",
		row.names = FALSE, col.names=FALSE,append=TRUE)

#       pdf(file=paste("PDF/",out.file.main,sep=""),height=10,width=10)
       pdf(file=out.file.main,height=10,width=10)

# ACTUAL forestplot function
# "zero=1" puts a line at OR=1
# "is.summary" is a just string of TRUE and FALSE where TRUE th lines will become bold-faced and plots will become summary style not the ordinary style
# "clip" function clips the confidence intervals for individual studies
# "xticks" function puts the tick marks on the plot
# "col=meta.colors(line="black")" makes individual plot lines darker

       my.forestplot(tabletext,title,as.numeric(m),as.numeric(l2),as.numeric(u2),zero=1,is.summary=c(TRUE,rep(FALSE,length(
		full.add.beta.trimmed)),FALSE,
		  TRUE,FALSE,TRUE,TRUE),
		  graphwidth=unit(3,"inches"),xticks=seq(left_tick,right_tick,by=intrvl),
		  col=meta.colors(line="black"))

# this line shuts off the pdf function and saves the pdf file
       dev.off()
  }
}


for (n in 1:length(outcome)) {
    for (k in 1:length(term)) {
    	if(length(grep("YA",age))> 0) agestring<-"YAdults" else if(length(grep("All",age))> 0) agestring<-"AllAges" 

	if(length(grep("full",stratum)) > 0) routinestring<-"CRAN_module" else routinestring<-"GEMINI_script.mod"

	if(length(grep("females-only",sample_sex)) > 0) sexstring<-"females" else
	    if(length(grep("males-only",sample_sex)) > 0) sexstring<-"males" else
	    if(length(grep("combined-sexes",sample_sex)) > 0) sexstring<-"both"

	dir.create(paste(wd,"PDF",sep="/"))
        dir1<-paste(wd,"PDF",paste(agestring,subgroupname,sep="__"),sep="/")
	dir.create(dir1)
	dir2<-paste(dir1,routinestring,sep="/")
	dir.create(dir2)
	dir3<-paste(dir2,cond,sep="/")
	dir.create(dir3)
	tmpstring<-codingstringparse(in.file.name)
	if(tmpstring == "") tmpstring<-"no_gene"
	dir4<-paste(dir3,tmpstring,sep="/")
	dir.create(dir4)
	dir5<-paste(dir4,sexstring,sep="/")
	dir.create(dir5)

	term2=character(0)
	term2<-if(length(grep("_x_",term[k])) > 0) "GxE" else
                   if(length(grep("5http",term[k])) > 0 || length(grep("rs25531",term[k])) > 0) "Gene" else
	   	   if(length(grep("stress",term[k])) > 0 || length(grep("child_mal",term[k])) > 0) "Env" 

        if(length(term2) > 0) {
    	    dir6<-paste(dir5,term2,sep="/")
    	    dir7<-paste(dir6,term[k],sep="/")
    	    dir.create(dir6)
    	    dir.create(dir7)
    	    dirstring<-dir7
        } else {
    	    dir6<-paste(dir5,term[k],sep="/")
    	    dir.create(dir6)
    	    dirstring<-dir6
        }

    	title<-paste("Term : ",term[k],
		     "\n","Stress : ",stressstringparse(in.file.name),
		     "\n","Gene : ",genestringparse(in.file.name),
		     "\n","Model : ",outcome[n]," ~ ",modelstringparse(in.file.name),
		     "\n",effects[k]," effects, in ",sample_sex,", ",age," ages",
		     sep="") 
    	out.file.main<-paste(dirstring,outname.maker(in.file.name,stratum,
						     effects[k],outcome[n]),
			     sep="/")

cat("arguments:",in.file.name,cond,age,outcome[n],term[k],studylist,subgroupname,stratum,sample_sex,out.file.het,out.file.main,
		 result_label,lgscl,effects[k],left_clip,right_clip,left_tick,right_tick,intrvl,title)

    	do_rmeta_het(in.file.name,cond,age,outcome[n],term[k],studylist,subgroupname,stratum,sample_sex,out.file.het,out.file.main,
		 result_label,lgscl,effects[k],left_clip,right_clip,left_tick,right_tick,intrvl,title)
		 }
}

