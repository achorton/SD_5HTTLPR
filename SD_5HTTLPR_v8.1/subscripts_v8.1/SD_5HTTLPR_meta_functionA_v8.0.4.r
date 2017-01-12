#*****************************************************************************
#* 5-HTTLPR GxE analysis subroutine definitions
#* version 8.0.4
#*****************************************************************************
#*
#* 
#* created by Amy Horton, Younghun Han, Chris Amos, 
#*            Sarah Hartz, and Rob Culverhouse
#*
#*****************************************************************************

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
write(paste('START:',name.varA,sep=""), file=out.file,append=TRUE)
write(paste(name.varA," =",mean(varA,na.rm=TRUE)," +/- ",sd(varA,na.rm=TRUE)," min",min(varA,na.rm=TRUE)," max",max(varA,na.rm=TRUE)," deciles"
            ,sep=" "), file=out.file,append=TRUE)
write(quantile(varA,c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),na.rm=TRUE), file=out.file,append=TRUE)
write(length(na.exclude(varA)), file=out.file,append=TRUE)

write(paste('STOP:',name.varA,sep=""), file=out.file,append=TRUE)
write(" ", file=out.file,append=TRUE)  
write(" ", file=out.file,append=TRUE)  
write(" ", file=out.file,append=TRUE)
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

  write(paste('START',name.varA,sep=":"),file=out.file,append=TRUE )
  write(paste(name.varA,'freq',sep=','),file=out.file,append=TRUE )
  write.table(varA.table, file=out.file, quote=FALSE,sep=", ",na=".",  row.names = FALSE, col.names=FALSE, append=TRUE) 
  write(paste('STOP',name.varA,sep=":"),file=out.file,append=TRUE )
  write(" ", file=out.file,append=TRUE)  
  write(" ", file=out.file,append=TRUE)  
  write(" ", file=out.file,append=TRUE)
}


get.xtab<-function(varA,varB,name.varA,name.varB){
  	varA_varB<-as.data.frame(table(varA,varB,exclude=NA))

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
        
	write(paste('START',paste(name.varA,name.varB,sep=" x "),sep=":"),file=out.file,append=TRUE )
	write(paste(name.varA,name.varB,'freq',sep=','),file=out.file,append=TRUE )
	write.table(varA.freq.table, file=out.file, append=TRUE, quote=FALSE,sep=", ",na=".",  row.names = FALSE, col.names=FALSE) 
	write(paste('STOP',paste(name.varA,name.varB,sep=" x "),sep=":"),file=out.file,append=TRUE )
        write(" ", file=out.file,append=TRUE)  
        write(" ", file=out.file,append=TRUE)  
        write(" ", file=out.file,append=TRUE)
      }


get.tritab<-function(varA,varB,varC,name.varA,name.varB,name.varC){
  write(paste('START',paste(name.varA,name.varB,name.varC,sep=" x "),sep=":"),file=out.file,append=TRUE )
  levelC<-levels(as.factor(varC))                           
  nlevelC<-nlevels(as.factor(varC))
  if(length(varC[is.na(varC)]) > 0) {
    levelC<-append(levelC,NA)
    nlevelC<-nlevelC + 1
  }
  
  for (i in 1:nlevelC){
    if(is.na(levelC[i])==FALSE){
      varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(varC==levelC[i]),
                                     exclude=NA))
    } else {
      varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC)),
                                     exclude=NA))
    }

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
        
      write("####",file=out.file,append=TRUE )
      write(paste(name.varC,"=",levelC[i],sep=" "),file=out.file,append=TRUE )  
      write(paste(name.varA,name.varB,'freq',sep=','),file=out.file,append=TRUE )
      write.table(varA.freq.table, file=out.file, append=TRUE, quote=FALSE,sep=", ",na=".",  row.names = FALSE, col.names=FALSE)
    }
  }

  write(paste('STOP',paste(name.varA,name.varB,name.varC,sep=" x "),sep=":"),file=out.file,append=TRUE )
  write(" ", file=out.file,append=TRUE)  
  write(" ", file=out.file,append=TRUE)  
  write(" ", file=out.file,append=TRUE)  
}

  
get.quadtab<-function(varA,varB,varC,varD,name.varA,name.varB,name.varC,name.varD){
  write(paste('START',paste(name.varA,name.varB,name.varC,name.varD,sep=" x "),sep=":"),file=out.file,append=TRUE )
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
            varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
            					   	 		   (varD==levelD[j])),
                                           exclude=NA))
          } else {
            varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                           (varD==levelD[j])),
                                           exclude=NA))
          }
      } else {
        if(is.na(levelC[i])==FALSE){
          varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
                                                                         is.na(varD)),
                                         exclude=NA))
        } else {
            varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                           is.na(varD)),
                                           exclude=NA))            
        }
      }
     
#########      
#      write("varA_varB data.frame created", file=out.file,append=TRUE)
#      write(paste(name.varD,"=",levelD[j],sep=" "), file=out.file,append=TRUE)
#      write(paste(name.varC,"=",levelC[i],sep=" "), file=out.file,append=TRUE)
#      write(paste(name.varA," x ",name.varB,sep=" "), file=out.file,append=TRUE)
#      write(dim(varA_varB)[1], file=out.file,append=TRUE)
#      write("dim(varA_varB) call made", file=out.file,append=TRUE)
#
#########
      if(is.null(dim(varA_varB))== FALSE){
##########
#        write("is.null(dim(varA_varB)) call made", file=out.file,append=TRUE)
##########                
        if(dim(varA_varB)[1] > 0){
##########
#          write("dim(varA_varB) > 0", file=out.file,append=TRUE)
##########        
          varAfreq<-varA_varB
          varAfreq$varA<-as.character(varAfreq$varA)	
          varAfreq$varB<-as.character(varAfreq$varB)
##########
#          write("varAfreq$varA call made", file=out.file,append=TRUE)
##########        
      
          levelA<-levels(as.factor(varAfreq$varA))
          levelB<-levels(as.factor(varAfreq$varB))
##########
#          write("levels call made", file=out.file,append=TRUE)
##########        
          nlevelA<-nlevels(as.factor(varAfreq$varA))
          nlevelB<-nlevels(as.factor(varAfreq$varB))
##########
#          write("nlevels call made", file=out.file,append=TRUE)
##########        
      
          varAt<-as.character(c(NA,levelA))
          varBt<-as.character(c(NA,levelB))   
                
          temp<-cbind(merge(varAt,varBt),placeholder=rep(1,(nlevelA+1)*(nlevelB+1)))  
##########
#          write("first merge call made", file=out.file,append=TRUE)
##########        
          names(temp)[1]<-'varA'
          names(temp)[2]<-'varB'
##########
#          write("names call made", file=out.file,append=TRUE)
##########        
 	
          varA.freq.values<-data.frame(temp)
##########
#          write("varA.freq.values<-data.frame(temp) call made", file=out.file,append=TRUE)
##########        
          varA.freq.table<-merge(varAfreq,varA.freq.values, all.x=TRUE)[,c(1,2,3)]
##########
#          write("second merge call made", file=out.file,append=TRUE)
##########    
        
          write("####",file=out.file,append=TRUE )
          write(paste(name.varD,"=",levelD[j],sep=" "),file=out.file,append=TRUE )
          write(paste(name.varC,"=",levelC[i],sep=" "),file=out.file,append=TRUE )  
          write(paste(name.varA,name.varB,'freq',sep=','),file=out.file,append=TRUE )
          write.table(varA.freq.table, file=out.file, append=TRUE, quote=FALSE,sep=", ",na=".",  row.names = FALSE, col.names=FALSE)
        }
      }
    }
  }

  write(paste('STOP',paste(name.varA,name.varB,name.varC,name.varD,sep=" x "),sep=":"),file=out.file,append=TRUE )
  write(" ", file=out.file,append=TRUE)  
  write(" ", file=out.file,append=TRUE)  
  write(" ", file=out.file,append=TRUE)  
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
  
  write(paste('START',paste(name.varA,name.varB,name.varC,name.varD,name.varE,sep=" x "),sep=":"),file=out.file,append=TRUE )
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
              varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
                                                                             (varD==levelD[j]) &
                                                                             (varE==levelE[k])),exclude=NA))

              
            } else {
              varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                             (varD==levelD[j]) &
                                                                             (varE==levelE[k])),exclude=NA))
            }
          } else {
            if(is.na(levelC[i])==FALSE){
              varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
                                                                             is.na(varD) &
                                                                             (varE==levelE[k])),exclude=NA))
            } else {
              varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                             is.na(varD) &
                                                                             (varE==levelE[k])),exclude=NA))
              
            }
          }
        } else {
          if(is.na(levelD[j])==FALSE){
            if(is.na(levelC[i])==FALSE){
              varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
                                                                             (varD==levelD[j]) &
                                                                             is.na(varE)),exclude=NA))
            } else {
              varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                             (varD==levelD[j]) &
                                                                             is.na(varE)),exclude=NA))
            }
          } else {
            if(is.na(levelC[i])==FALSE){
              varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
                                                                             is.na(varD) &
                                                                             is.na(varE)),exclude=NA))
            } else {
              varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                             is.na(varD) &
                                                                             is.na(varE)),exclude=NA))
            }
          }
        }
      
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

        
          write("####",file=out.file,append=TRUE )
          write(paste(name.varE,"=",levelE[k],sep=" "),file=out.file,append=TRUE )
          write(paste(name.varD,"=",levelD[j],sep=" "),file=out.file,append=TRUE )
          write(paste(name.varC,"=",levelC[i],sep=" "),file=out.file,append=TRUE )  
          write(paste(name.varA,name.varB,'freq',sep=','),file=out.file,append=TRUE )
          write.table(varA.freq.table, file=out.file, append=TRUE, quote=FALSE,sep=", ",na=".",  row.names = FALSE, col.names=FALSE)
        }
      }
    }
  }

  write(paste('STOP',paste(name.varA,name.varB,name.varC,name.varD,name.varE,sep=" x "),sep=":"),file=out.file,append=TRUE )
  write(" ", file=out.file,append=TRUE)  
  write(" ", file=out.file,append=TRUE)  
  write(" ", file=out.file,append=TRUE)  
}



get.sextab<-function(varA,varB,varC,varD,varE,varF,name.varA,name.varB,name.varC,name.varD,name.varE,name.varF){
  write(paste('START',paste(name.varA,name.varB,name.varC,name.varD,name.varE,name.varF,sep=" x "),sep=":"),file=out.file,append=TRUE )
  
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
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
                                                                                 (varD==levelD[j]) &
                                                                                 (varE==levelE[k]) &
                                                                                 (varF==levelF[l])),
                                                 exclude=NA))
                } else {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                                 (varD==levelD[j]) &
                                                                                 (varE==levelE[k]) &
                                                                                 (varF==levelF[l])),
                                                 exclude=NA))
                }
              } else {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
                                                                                 is.na(varD) &
                                                                                 (varE==levelE[k]) &
                                                                                 (varF==levelF[l])),
                                                 exclude=NA))

                } else {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                                 is.na(varD) &
                                                                                 (varE==levelE[k]) &
                                                                                 (varF==levelF[l])),
                                                 exclude=NA))
                }
              }
            } else {
              if(is.na(levelD[j])==FALSE) {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
                                                                                 (varD==levelD[j]) &
                                                                                 is.na(varE) &
                                                                                 (varF==levelF[l])),
                                                 exclude=NA))

                } else {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                                 (varD==levelD[j]) &
                                                                                 is.na(varE) &
                                                                                 (varF==levelF[l])),
                                                 exclude=NA))
                }
              } else {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
                                                                                 is.na(varD) &
                                                                                 is.na(varE) &
                                                                                 (varF==levelF[l])),
                                                 exclude=NA))
                } else {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                                 is.na(varD) &
                                                                                 is.na(varE) &
                                                                                 (varF==levelF[l])),
                                                 exclude=NA))
                }
              }
            }
          } else {
            if(is.na(levelE[k])==FALSE) {
              if(is.na(levelD[j])==FALSE) {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
                                                                                 (varD==levelD[j]) &
                                                                                 (varE==levelE[k]) &
                                                                                 is.na(varF)),
                                                 exclude=NA))
                } else {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                                 (varD==levelD[j]) &
                                                                                 (varE==levelE[k]) &
                                                                                 is.na(varF)),
                                                 exclude=NA))
                }
              } else {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
                                                                                 is.na(varD) &
                                                                                 (varE==levelE[k]) &
                                                                                 is.na(varF)),
                                                 exclude=NA))

                } else {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                                 is.na(varD) &
                                                                                 (varE==levelE[k]) &
                                                                                 is.na(varF)),
                                                 exclude=NA))
                }
              }
            } else {
              if(is.na(levelD[j])==FALSE) {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
                                                                                 (varD==levelD[j]) &
                                                                                 is.na(varE) &
                                                                                 is.na(varF)),
                                                 exclude=NA))

                } else {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                                 (varD==levelD[j]) &
                                                                                 is.na(varE) &
                                                                                 is.na(varF)),
                                                 exclude=NA))
                }
              } else {
                if(is.na(levelC[i])==FALSE) {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=((varC==levelC[i]) &
                                                                                 is.na(varD) &
                                                                                 is.na(varE) &
                                                                                 is.na(varF)),
                                                 exclude=NA))
                } else {
                  varA_varB<-as.data.frame(xtabs(~varA + varB, subset=(is.na(varC) &
                                                                                 is.na(varD) &
                                                                                 is.na(varE) &
                                                                                 is.na(varF)),
                                                 exclude=NA))
                }
              }
            }
          }

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

        
            write("####",file=out.file,append=TRUE )          
            write(paste(name.varF,"=",levelF[l],sep=" "),file=out.file,append=TRUE )
            write(paste(name.varE,"=",levelE[k],sep=" "),file=out.file,append=TRUE )
            write(paste(name.varD,"=",levelD[j],sep=" "),file=out.file,append=TRUE )
            write(paste(name.varC,"=",levelC[i],sep=" "),file=out.file,append=TRUE )  
            write(paste(name.varA,name.varB,'freq',sep=','),file=out.file,append=TRUE )
            write.table(varA.freq.table, file=out.file, append=TRUE, quote=FALSE,sep=", ",na=".",  row.names = FALSE, col.names=FALSE)
          }
        }
      }
    }
  }

  write(paste('STOP',paste(name.varA,name.varB,name.varC,name.varD,name.varE,name.varF,sep=" x "),sep=":"),file=out.file,append=TRUE )
  write(" ", file=out.file,append=TRUE)  
  write(" ", file=out.file,append=TRUE)  
  write(" ", file=out.file,append=TRUE)  
}


######################################
# run linear or logistic regression  #
# for 1-way SNP Association Models   #
######################################

oneSNP <- function(idx, snplist_f, outcomes_f, models_f, type_f, var.na, out.file, data_f) {
  if(idx%%5!=0){
    model1.text <- paste( outcomes_f,models_f, sep=" ")
    if(idx%%5==4)
      label_model1 <- gsub("SNP", paste(snplist_f,"+",snplist[idx+1],sep=" ")
                           , model1.text ) else    
    label_model1 <- gsub("SNP", snplist_f , model1.text )
    label_model <- gsub("~","=", label_model1)
    model1 <- as.formula(label_model1)
    
    tmp<-terms.formula(model1)
    ntmp<-length(attr(tmp,"variables"))
    
    var.level.flag.vector<-c(rep(1,ntmp-1))
    temp.obj<-var.level.check.SNP(ntmp,var.level.flag.vector,model1,tmp)	
    model1<-temp.obj[[1]]
    tmp<-temp.obj[[2]]
    x<-numeric(0)
    for(l in 3:(ntmp + 1)) { 
      x<-append(x,temp.obj[[l]])
    }
    var.level.flag.vector<-x
    
    tmp<-terms.formula(model1)
    ntmp<-length(attr(tmp,"variables"))
    
    running.fem<-c(rep(1,length(get(outcomes_f))))
    for(j in 2:ntmp) {
        running.fem<-running.fem*ifelse(is.na(get(as.character(attr(tmp,"variables")[[j]]))),NA,1)
    }
    fems<-levels(as.factor(ifelse(is.na(running.fem*female),NA,female)))
    nfemlevels<-length(fems)

    fem_levels<-if (nfemlevels==2) "combined-sex sample" else
    if (nfemlevels==1) {
        if (fems[1]==0) "female-only sample" else
        if (fems[1]==1) "male-only sample"
    }
        
    if(ntmp > 2) {
        present<-c(rep(1,length(get(outcomes_f))))*ifelse(is.na(get(as.character(attr(tmp,"variables")[[2]]))),0,1)
        levels_test<-1
        not_na_test<-1
        for(i in 3: ntmp) {
            var.level<-length(levels(as.factor(get(as.character(attr(tmp,"variables")[[i]])))))
            levels_test<-if(var.level < 2) 0 else levels_test*var.level
            present<-present*ifelse(is.na(get(as.character(attr(tmp,"variables")[[i]]))),0,1)
            not_na_test<-not_na_test*ifelse(exists.to.tab2(get(as.character(attr(tmp,"variables")[[i]]))),1,0)
        }

        tot<-sum(na.exclude(present))

        header<-paste("START",label_model, sep="  " )
        footer<-paste("STOP",label_model, sep="  " )
        cat("=== running ",label_model, "\n")

        if((levels_test>1) & not_na_test & tot>2){
            write.table(t(header),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )

            if (1){
#            if (sum(ifelse(is.na(get(var.na)),1,0))/length(get(var.na)) < 0.9){
                write.table(paste("N=",tot,sep=""),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )

                out <- glm(model1, family=type_f, data=data_f)
                summary_out <- summary(out)
     
                nparms <- length(summary_out$coefficients[,1])
                top.row<-c("outcome", "parameter",names(as.data.frame(summary_out$coefficients)) ,
                           "dev_df","deviance","vcov label",paste(rep("vcov",nparms),c(1:nparms)))
        
                results<-cbind(c(rep(outcomes_f,nparms)),row.names(summary_out$coefficients),
                               summary_out$coefficients,
                               matrix(rep( c(summary_out$df[2],summary_out$deviance),nparms),ncol=2, byrow=T),
                               c(1:nparms),vcov(out))

                write.table(t(top.row),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                write.table(results,file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
            }
            write.table(t(footer),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE) 
        } else {
            write.table(paste(label_model,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
    } else {
        write.table(paste(label_model,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
    } 
}
}


oneSTRESS <- function(snplist_f, outcomes_f, models_f, type_f, var.na, out.file, data_f) {
    model1.text <- paste( outcomes_f,models_f, sep=" ")
    label_model1 <- gsub("SNP", snplist_f , model1.text )
    label_model <- gsub("~","=", label_model1)
    model1 <- as.formula(label_model1)

    tmp<-terms.formula(model1)
    ntmp<-length(attr(tmp,"variables"))

    var.level.flag.vector<-c(rep(1,ntmp-1))
    temp.obj<-var.level.check.STRESS(ntmp,var.level.flag.vector,model1,tmp)
    model1<-temp.obj[[1]]
    tmp<-temp.obj[[2]]
    x<-numeric(0)
    for(l in 3:(ntmp + 1)) { 
      x<-append(x,temp.obj[[l]])
    }
    var.level.flag.vector<-x

    if(0) {    
    for(i in 2:ntmp) {
        running.var<-c(rep(1,length(get(outcomes_f))))
        for(j in 2:max(2,i)) {
            if(var.level.flag.vector[j-1]) {
                running.var<-running.var*ifelse(is.na(get(as.character(attr(tmp,"variables")[[j]]))),NA,1)
            }
        }        
        var.level<-length(levels(as.factor(ifelse(is.na(running.var*get(as.character(attr(tmp,"variables")[[i]]))),NA,get(as.character(attr(tmp,"variables")[[i]])) ))))     
        if(var.level < 2) {
            model1<-update(model1,paste(". ~ . -",as.character(attr(tmp,"variables")[[i]]),sep=" "))
            var.level.flag.vector[i-1]<-0
        }
    }
    }
    
    tmp<-terms.formula(model1)
    ntmp<-length(attr(tmp,"variables"))
       
    running.fem<-c(rep(1,length(get(outcomes_f))))
    for(j in 2:ntmp) {
        running.fem<-running.fem*ifelse(is.na(get(as.character(attr(tmp,"variables")[[j]]))),NA,1)
    }
    fems<-levels(as.factor(ifelse(is.na(running.fem*female),NA,female)))
    nfemlevels<-length(fems)

    fem_levels<-if (nfemlevels==2) "combined-sex sample" else
    if (nfemlevels==1) {
        if (fems[1]==0) "female-only sample" else
        if (fems[1]==1) "male-only sample"
    }
        
 
    if(ntmp > 2) {
        present<-c(rep(1,length(get(outcomes_f))))*ifelse(is.na(get(as.character(attr(tmp,"variables")[[2]]))),0,1)
        levels_test<-1
        not_na_test<-1
        for(i in 3: ntmp) {
            var.level<-length(levels(as.factor(get(as.character(attr(tmp,"variables")[[i]])))))
            levels_test<-if(var.level < 2) 0 else levels_test*var.level
            present<-present*ifelse(is.na(get(as.character(attr(tmp,"variables")[[i]]))),0,1)
            not_na_test<-not_na_test*ifelse(exists.to.tab2(get(as.character(attr(tmp,"variables")[[i]]))),1,0)
        }

        tot<-sum(na.exclude(present))

        header<-paste("START",label_model, sep="  " )
        footer<-paste("STOP",label_model, sep="  " )
        cat("=== running ",label_model, "\n")

        if((levels_test>1) & not_na_test & tot>2){
            write.table(t(header),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )

            if (1){
#            if (sum(ifelse(is.na(get(var.na)),1,0))/length(get(var.na)) < 0.9){
                write.table(paste("N=",tot,sep=""),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )

                out <- glm(model1, family=type_f, data=data_f)
                summary_out <- summary(out)
     
                nparms <- length(summary_out$coefficients[,1])
                top.row<-c("outcome", "parameter",names(as.data.frame(summary_out$coefficients)) ,
                           "dev_df","deviance","vcov label",paste(rep("vcov",nparms),c(1:nparms)))
     
                results<-cbind(c(rep(outcomes_f,nparms)),row.names(summary_out$coefficients),
                               summary_out$coefficients,
                               matrix(rep( c(summary_out$df[2],summary_out$deviance),nparms),ncol=2, byrow=T),
                               c(1:nparms),vcov(out))

                write.table(t(top.row),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                write.table(results,file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
            }
            write.table(t(footer),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE) 
        } else {
            write.table(paste(label_model,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
    } else  {
        write.table(paste(label_model,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
    }
}


#########################################
# run stratified regression             #
# for 1-way SNP Association Models      #
#########################################  

oneSNP.by <- function(idx,snplist_f, outcomes_f, models_f,by.list_f, type_f,out.file,data_f) {
    if(idx%%5!=0){  
        model1.text <- paste( outcomes_f,models_f, sep=" ")
        
        model1<-as.formula(model1.text)
        model1<-update(model1,paste(". ~ . -",by.list_f,sep=" "))

        if(idx%%5==4){
          by.list_f <- gsub("SNP", snplist[idx-3] , by.list_f)
        } else { 
          by.list_f <- gsub("SNP", snplist_f , by.list_f)
        }

        tmp<-as.character(model1)
        label_model <- outcomes_f
        label_model<-paste(label_model,"=",tmp[3],sep=" ")  
        label_model <- paste(label_model,"stratified by",by.list_f,sep=" ")
        
        tmp<-terms.formula(model1)
        ntmp<-length(attr(tmp,"variables"))
        
        var.level.flag.vector<-c(rep(1,ntmp-1))
        temp.obj<-var.level.check.by(ntmp,var.level.flag.vector,model1,tmp)
        model1<-temp.obj[[1]]
        tmp<-temp.obj[[2]]
        x<-numeric(0)
        for(l in 3:(ntmp + 1)) { 
          x<-append(x,temp.obj[[l]])
        }
        var.level.flag.vector<-x
        
        header<-paste("START",label_model, sep="  " )
        footer<-paste("STOP",label_model, sep="  " )

        cat("=== running ",label_model, "\n")

            tmp<-terms.formula(model1)
            ntmp<-length(attr(tmp,"variables"))
    
            running.fem<-c(rep(1,length(get(outcomes_f))))
            for(j in 2:ntmp) {
                running.fem<-running.fem*ifelse(is.na(get(as.character(attr(tmp,"variables")[[j]]))),NA,1)
            }
            fems<-levels(as.factor(ifelse(is.na(running.fem*female),NA,female)))
            nfemlevels<-length(fems)
            
            fem_levels<-if (nfemlevels==2) "combined-sex sample" else
            if (nfemlevels==1) {
                if (fems[1]==0) "female-only sample" else
                if (fems[1]==1) "male-only sample"
            }

            if(ntmp > 2) {
                present<-c(rep(1,length(get(outcomes_f))))*ifelse(is.na(get(as.character(attr(tmp,"variables")[[2]]))),0,1)

                levels_test<-1
                not_na_test<-1
                for(i in 3: ntmp) {
                    levels_test<-levels_test*length(levels(as.factor(get(as.character(attr(tmp,"variables")[[i]])))))
                    present<-present*ifelse(is.na(get(as.character(attr(tmp,"variables")[[i]]))),0,1)
                    not_na_test<-not_na_test*ifelse(exists.to.tab2(get(as.character(attr(tmp,"variables")[[i]]))),1,0)
                }    
                tot<-sum(na.exclude(present))

                present0<-c(rep(1,length(get(outcomes_f))))
                present0<-present0*ifelse((is.na(get(as.character(attr(tmp,"variables")[[2]])))),0,1)
                present0<-present0*ifelse((get(by.list_f) != 0),0,1)

                for(i in 3: ntmp) {
                    present0<-present0*ifelse((is.na(get(as.character(attr(tmp,"variables")[[i]])))),0,1)
                }
                tot0<-sum(na.exclude(present0))

                present1<-c(rep(1,length(get(outcomes_f))))
                present1<-present1*ifelse((is.na(get(as.character(attr(tmp,"variables")[[2]])))),0,1)
                present1<-present1*ifelse((get(by.list_f) != 1),0,1)
        
                for(i in 3: ntmp) {
                    present1<-present1*ifelse((is.na(get(as.character(attr(tmp,"variables")[[i]])))),0,1)
                }
                tot1<-sum(na.exclude(present1))

                present2<-c(rep(1,length(get(outcomes_f))))
                present2<-present2*ifelse((is.na(get(as.character(attr(tmp,"variables")[[2]])))),0,1)
                present2<-present2*ifelse((get(by.list_f) != 2),0,1)

                for(i in 3: ntmp) {
                    present2<-present2*ifelse((is.na(get(as.character(attr(tmp,"variables")[[i]])))),0,1)
#                    if (by.filter_num == i) present2<-present2*ifelse((get(as.character(attr(tmp,"variables")[[i]])) != 2),0,1)
                }
                tot2<-sum(na.exclude(present2))
    
                if((levels_test>1) & not_na_test & tot>2 & tot0>2 & tot1>2 & tot2>2 ){ 
                    byvar<- data_f[,by.list_f]
                    for(i in 2:ntmp) {
                        byvar <- ifelse(is.na(data_f[,as.character(attr(tmp,"variables")[[i]])]),NA,byvar)
                    }
                    frq <- table(data_f[, outcomes_f], byvar)
                    remove.list <- colnames(frq)
                    if (type_f=='binomial') {
                        for(i in 1:dim(frq)[1]){
                            for(j in 1:dim(frq)[2]) {
                                if(frq[i,j]==0) replace(byvar,byvar==remove.list[j],NA) -> byvar
                            }
                        }
                    }
                    if (type_f=='gaussian') {
                        test<-c(rep(0,dim(frq)[2]))
                        for(i in 1:dim(frq)[1]){ 
                            for(j in 1:dim(frq)[2]) { 
                                test[j]<-if(frq[i,j]>0) 1+test[j] else test[j]
                            }
                        }
                        for(j in 1:dim(frq)[2]) {
                            if(test[j]<2) replace(byvar,byvar==remove.list[j],NA) -> byvar
                        }
                    }

                    for(i in 1:length(levels(as.factor(byvar)))){
                        if(sum(na.exclude(ifelse(byvar==levels(as.factor(byvar))[i],1,0)))<3){
                            byvar[byvar==levels(as.factor(byvar))[i]]<-NA
                        }
                    }

                    if(length(levels(as.factor(byvar)))) {     
                        out <- by(data_f, byvar, function(x) glm(model1, family=type_f,data=x))
                        summary_out <- by(data_f, byvar, function(x) summary(glm(model1, family=type_f,data=x)))
      
                        nby      <- nlevels(factor(byvar))
                        level.by <- levels(factor(byvar))
                        name.parms <- row.names(coef(summary(glm(model1, family=type_f,data=data_f))))
                        nparms <- length(name.parms)
      
                        out_coef <- matrix(NA, nrow=nby*nparms, ncol=4)
                        out_vcov <- matrix(NA, nrow=nby*nparms, ncol=nparms)

                        tmp3<-c(NA)
                        for(i in 1:nby) {

                            tmp.coef <- coef(summary_out[[i]])
                            tmp1 <- matrix(NA, nrow=nparms, ncol=4)
                            row.names(tmp1) <- name.parms
        
                            for(j in 1:length(row.names(tmp.coef))){                                 
                                tmp1[(row.names(tmp.coef)[j]),] <- tmp.coef[j,]
                            }
                            out_coef[((i-1)*nparms+1):(i*nparms),] <- tmp1
      
                            tmp.vcov <- matrix(sapply(out[i],vcov),ncol=dim(tmp.coef)[1])
                            tmp2 <- matrix(NA, nrow=nparms, ncol=nparms)
                            rownames(tmp2) <- name.parms
                            colnames(tmp2) <- name.parms

                            tmp3 <- append(tmp3,row.names(tmp1))
                            for(j in 1:length(row.names(tmp.coef))){
                                for(k in 1:length(row.names(tmp.coef))) {
                                    tmp2[(row.names(tmp.coef)[j]),(row.names(tmp.coef)[k])] <- tmp.vcov[j,k]
                                }
                            }
                            out_vcov[((i-1)*nparms+1):(i*nparms),] <- tmp2
                        }

                        df <- rep(sapply(out,df.residual),each=nparms)
                        dev <- rep(sapply(out,deviance),each=nparms)
                        dfdev <- cbind(df,dev)

                        top.row<-c("outcome", by.list_f, "parameter",
                                   "Estimate", "Std. Error", "z value",
                                   "Pr(>|z|)" ,"dev_df","deviance","vcov label",
                                   paste(rep("vcov",nparms),c(1:nparms)))

                        results<-cbind(c(rep(outcomes_f,nparms*nby)),
                                       row.names(dfdev),
                                       tmp3[2:length(tmp3)],
#                     rep(row.names(summary(glm(model1, family=type_f))$coefficients),nby),
                                       out_coef, dfdev, rep(c(1:nparms),nby),
                                       out_vcov)
                        
                        write.table(t(header),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )
                        write.table(paste("N0=",tot0,"=N1=",tot1,"=N2=",tot2,sep=""),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )

                        write.table(t(top.row),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                        write.table(results,file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                        write.table(t(footer),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                        write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE) 	
                    } else {
                        write.table(paste(label_model,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
                    }
                } else {
                    write.table(paste(label_model,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
                }
            } else { write.table(paste(label_model,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
                 }
#        } else { write.table(paste(label_model,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
#             }
    }
}
  

#######################################
# run moderated linear regression     #
# for 1-way SNP Association Models    #
#######################################  
oneSNP.mod <- function(idx,snplist_f, outcomes_f, models_f,type_f,out.file,data_f) {
  if(idx%%5!=0){  
    model1.text <- paste( outcomes_f,models_f, sep=" ")
    if(idx%%5==4){      
      label_model1 <- gsub("SNP", paste(snplist_f,"+",snplist[idx+1],
                                        sep=" "), model1.text)
    } else label_model1 <- gsub("SNP", snplist_f , model1.text )
  
    label_model <- gsub("~","=", label_model1)                 
    model1 <- as.formula(label_model1)

    tmp<-terms.formula(model1)
    ntmp<-length(attr(tmp,"variables"))
    
    if((length(grep("L_Adum1_rs25531",attr(tmp,"term.labels"))) > 0) |
       (length(grep("Ldum1_5http",attr(tmp,"term.labels"))) > 0) ) {                      
      model1int <- update(model1,paste(". ~ . -",
                                       as.character(attr(tmp,"variables")[[(ntmp-2)]]),"-",
                                       as.character(attr(tmp,"variables")[[(ntmp-1)]]),"-",
                                       as.character(attr(tmp,"variables")[[(ntmp)]]),"-",
                                       paste(as.character(attr(tmp,"variables")[[(ntmp-1)]]),
                                             as.character(attr(tmp,"variables")[[(ntmp)]]),
                                             sep=":"),sep=" ")
                          )
      model1 <- update(model1int,paste(". ~ . +",
                                       as.character(attr(tmp,"variables")[[(ntmp-2)]]),"+",
                                       as.character(attr(tmp,"variables")[[(ntmp-1)]]),"+",
                                       as.character(attr(tmp,"variables")[[(ntmp)]]),"+",
                                       paste(as.character(attr(tmp,"variables")[[(ntmp-2)]]),
                                             as.character(attr(tmp,"variables")[[(ntmp)]]),
                                             sep=":"),"+",
                                       paste(as.character(attr(tmp,"variables")[[(ntmp-1)]]),
                                             as.character(attr(tmp,"variables")[[(ntmp)]]),
                                             sep=":")))
    }

    label_model <-paste(as.character(model1)[2],"~",as.character(model1)[3],sep=" ")

    tmp<-terms.formula(model1)
    ntmp<-length(attr(tmp,"variables"))

    flag4<-1  # is it dummy-coded (reversed)
    for(i in 3:ntmp) {
      if(length(grep("dum",as.character(attr(tmp,"variables")[[i]]))) > 0) (flag5<-0) else (flag5<-flag4)
      flag4<-flag5*flag4
    }
    if(flag4) {
      model2 <- update(model1,paste(". ~ . -",paste(as.character(attr(tmp,"variables")[[(ntmp-1)]]),as.character(attr(tmp,"variables")[[(ntmp)]]),sep=":"),sep=" "))
    } else {
      model2 <- update(model1,paste(". ~ . -",
                                    paste(as.character(attr(tmp,"variables")[[(ntmp-1)]]),
                                          as.character(attr(tmp,"variables")[[(ntmp)]]),sep=":"),"-",
                                    paste(as.character(attr(tmp,"variables")[[(ntmp-2)]]),
                                          as.character(attr(tmp,"variables")[[(ntmp)]]),sep=":"),sep=" "))
    }
    label_model2 <-paste(as.character(model2)[2],"=",as.character(model2)[3],sep=" ")
    
    vec.length<-ntmp-1
    var.level.flag.vector<-c(rep(1,vec.length))
    temp.obj<-var.level.check.mod(ntmp,var.level.flag.vector,model1,model2,tmp)
    model1<-temp.obj[[1]]
    model2<-temp.obj[[2]]
    tmp<-temp.obj[[3]]
    x<-numeric(0)
    for(l in 4:(ntmp + 2)) { 
      x<-append(x,temp.obj[[l]])
    }
    var.level.flag.vector<-x
    
    tmp<-terms.formula(model1)
    ntmp<-length(attr(tmp,"variables"))
        
    running.fem<-c(rep(1,length(get(outcomes_f))))
    for(j in 2:ntmp) {
        running.fem<-running.fem*ifelse(is.na(get(as.character(attr(tmp,"variables")[[j]]))),NA,1)
    }
    fems<-levels(as.factor(ifelse(is.na(running.fem*female),NA,female)))
    nfemlevels<-length(fems)

    fem_levels<-if (nfemlevels==2) "combined-sex sample" else
    if (nfemlevels==1) {
        if (fems[1]==0) "female-only sample" else
        if (fems[1]==1) "male-only sample"
    }
        
    if(ntmp > 2) {
        present2<-c(rep(1,length(get(outcomes_f))))*ifelse(is.na(get(as.character(attr(tmp,"variables")[[2]]))),0,1)
        levels_test2<-1
        not_na_test2<-1
                                        #      iftest on model1 at this point:is it dum,
        flag2<-1  #  does it have interaction (reversed)
        flag4<-1  # is it dummy-coded (reversed)
        dummy.var.vec<-c(rep(0,ntmp))
        SNP.var.vec<-c(rep(0,ntmp))
        stress.index<-2
        for(i in 1:length(attr(tmp,"order"))) {
            if((attr(tmp,"order"))[(i)] > 1) (flag3<-0) else (flag3<-flag2)
            flag2<-flag3*flag2
        }
        for(i in 3:ntmp) {
            if(length(grep("dum",as.character(attr(tmp,"variables")[[i]]))) > 0) {
                (flag5<-0)
                dummy.var.vec[i]<-1
            } else (flag5<-flag4)
            if((length(grep("child_mal",
                            as.character(attr(tmp,"variables")[[i]]))) > 0) |
               (length(grep("stress_combined",
                            as.character(attr(tmp,"variables")[[i]]))) > 0) |
               (length(grep("life_stress",
                            as.character(attr(tmp,"variables")[[i]]))) > 0)) stress.index<-i 
            if((length(grep("5http",
                            as.character(attr(tmp,"variables")[[i]]))) > 0) |
               (length(grep("rs25531",
                            as.character(attr(tmp,"variables")[[i]]))) > 0)) SNP.var.vec[i]<-1
                
            flag4<-flag5*flag4
            levels_test2<-levels_test2*length(levels(as.factor(get(as.character(attr(tmp,"variables")[[i]])))))
            present2<-present2*ifelse(is.na(get(as.character(attr(tmp,"variables")[[i]]))),0,1)
            not_na_test2<-not_na_test2*ifelse(exists.to.tab2(get(as.character(attr(tmp,"variables")[[i]]))),1,0)
        }
   
        if(!flag2) {  # interaction present
            if(flag4) { #not dummy coded but with interaction in model1
                for(i in 3:ntmp) {
                    if(SNP.var.vec[i]) {
                        model2 <- update(model1,paste(". ~ . -",paste(as.character(attr(tmp,"variables")[[(i)]]),as.character(attr(tmp,"variables")[[stress.index]]),sep=":"),sep=" "))
                        inter<-get(as.character(attr(tmp,"variables")[[i]])) * get(as.character(attr(tmp,"variables")[[stress.index]]))
                    }
                }
                present<-present2*ifelse(is.na(inter),0,1)
                levels_test<-length(levels(as.factor(inter)))
                not_na_test<-not_na_test2*
                    ifelse(exists.to.tab2(inter),1,0)
            } else {  #IS dummy coded
                for(i in 3:ntmp) {
                    inter1<-c(rep(NA,length(outcomes_f)))
                    inter2<-c(rep(NA,length(outcomes_f)))
                    if(dummy.var.vec[i]==1) {
                        model2 <- update(model1,paste(". ~ . -",
                                                      paste(as.character(attr(tmp,
                                                                              "variables")[[i]]),
                                                            as.character(attr(tmp,
                                                                              "variables")[[stress.index]]),
                                                            sep=":"),
                                                      sep=" "))
                        if(length(grep("dum1",as.character(attr(tmp,"variables")[[i]]))) > 0) {
                            inter1<-get(as.character(attr(tmp,"variables")[[i]])) * get(as.character(attr(tmp,"variables")[[stress.index]]))
                        }
                        if(length(grep("dum2",as.character(attr(tmp,"variables")[[i]]))) > 0)   {
                            inter2<-get(as.character(attr(tmp,"variables")[[i]])) * get(as.character(attr(tmp,"variables")[[stress.index]]))
                        }
                    }
                }
                not_na_test<-not_na_test2*
                    ifelse(exists.to.tab2(inter1 * inter2),1,0)

                present<-present2*ifelse(is.na(inter1),0,1)*ifelse(is.na(inter2),0,1)  
                levels_test<-length(levels(as.factor(inter1)))*length(levels(as.factor(inter2)))
            }
        } else { #no interaction
            model2<-model1
            present<-present2
            levels_test<-0
            not_na_test<-not_na_test2
        }

        tot2<-sum(na.exclude(present2))
        tot<-sum(na.exclude(present))
        
        header<-paste("START",label_model, sep="  " )
        footer<-paste("STOP",label_model, sep="  " )
        cat("=== running ",label_model, "\n")

        if((levels_test2>1) & not_na_test2 & tot2>2) {  
            if((levels_test>1) & not_na_test & tot>2){
                flag<-1

#                testout2<-1
#                out2<-glm(model2,data=data_f,family=type_f)
#
#                for (l in 2:length(out2$coeff)){
#                    testout2<-testout2*( if(is.na(out2$coeff[l])) 1 else 0 )
#                }

#                if(!testout2){ #######if at least 1 non-NA term in smaller model
                out <- lmres(model1, data_f, residual_centering=FALSE, centered="none")
                summary_out <- summary.lmres(out, type="nested")
                Ftop.row<-colnames(out$F_change)
                
                write.table(t(header),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(t(Ftop.row),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                write.table(out$F_change,file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                
                out2_coef <- coef(summary(out$Stepfin))
                out2_vcov <- vcov(out$Stepfin)

                name2.parms <- rownames(coef(summary(out$Stepfin)))
                nparms2 <- length(name2.parms)

                df2 <- rep(df.residual(out$Stepfin),each=nparms2)
                dev2 <- rep(deviance(out$Stepfin),each=nparms2)

                dfdev2 <- cbind(df2,dev2)
                top.row2<-c("outcome", "parameter","Estimate", "Std. Error", "t value", "Pr(>|t|)" ,
                            "dev_df","deviance","vcov label",paste(rep("vcov",nparms2),c(1:nparms2)))

                results2<-cbind(c(rep(outcomes_f,nparms2)),
                                name2.parms,
                                out2_coef,
                                dfdev2,
                                c(1:nparms2),
                                out2_vcov)

                out_coef <- coef(summary(out$StepI))
                out_vcov <- vcov(out$StepI)
                
                name.parms <- rownames(coef(summary(out$StepI)))
                nparms <- length(name.parms)
                
                df <- rep(df.residual(out$StepI),each=nparms)
                dev <- rep(deviance(out$StepI),each=nparms)
                
                dfdev <- cbind(df,dev)
                top.row<-c("outcome", "parameter","Estimate", "Std. Error", "t value", "Pr(>|t|)" ,
                           "dev_df","deviance","vcov label",paste(rep("vcov",nparms),c(1:nparms)))
                
                results<-cbind(c(rep(outcomes_f,nparms)),
                               name.parms,
                               out_coef,
                               dfdev,
                               c(1:nparms),
                               out_vcov)
#                }
##            } else {
#                out <- glm(model2, family=type_f, data=data_f)
#                summary_out <- summary(out)
#                
#                nparms <- length(summary_out$coefficients[,1])
#                top.row<-c("outcome", "parameter",
#                           names(as.data.frame(summary_out$coefficients)) ,
#                           "dev_df","deviance","vcov label",
#                           paste(rep("vcov",nparms),c(1:nparms)))
#                
#                results<-cbind(c(rep(outcomes_f,nparms)),
#                               row.names(summary_out$coefficients),
#                               summary_out$coefficients,
#                               matrix(rep( c(summary_out$df[2],summary_out$deviance),
#                                          nparms),ncol=2, byrow=T),
#                               c(1:nparms),vcov(out))
#                
#                write.table(paste(label_model,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                                        #	    write.table(t(header),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )
                                        #      	    write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
 #           }
            } else {
                flag<-0
                out <- glm(model2, family=type_f, data=data_f)
                summary_out <- summary(out)
                
                nparms <- length(summary_out$coefficients[,1])
                top.row<-c("outcome", "parameter",
                           names(as.data.frame(summary_out$coefficients)) ,
                           "dev_df","deviance","vcov label",
                           paste(rep("vcov",nparms),c(1:nparms)))
                
                results<-cbind(c(rep(outcomes_f,nparms)),
                               row.names(summary_out$coefficients),
                               summary_out$coefficients,
                               matrix(rep( c(summary_out$df[2],summary_out$deviance),nparms),ncol=2, byrow=T),
                               c(1:nparms),vcov(out))
                
                write.table(paste(label_model,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                write.table(t(header),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)

            }
        
            write.table(paste("N=",tot,sep=""),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )
            write.table(t(top.row),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
            write.table(results,file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")	
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)  
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)  

            if(flag){
                write.table(paste("N=",tot2,sep=""),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )
                write.table(t(top.row2),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                write.table(results2,file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")	
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE) 
                flag<-0
            }

            write.table(t(footer),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE) 	
            rm(flag)
        } else {
            write.table(paste(label_model2,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            write.table(paste(label_model,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        } 
    } else {
        write.table(paste(label_model,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
    } 
}
}


#########################################
# run moderated logistic regression     #
# for 1-way SNP Association Models      #
#########################################  

oneSNP.modlog <- function(idx,snplist_f, outcomes_f, models_f, interaction_f, type_f,out.file,data_f) {
  if(idx%%5!=0){  
    model1.text <- paste( outcomes_f,models_f, sep=" ")
    if(idx%%5==4){
      label_model1 <- gsub("SNP", paste(snplist_f,"+",snplist[idx+1],
                                        sep=" "), model1.text )
    } else {
      label_model1 <- gsub("SNP", snplist_f , model1.text )
    }

    model1 <- as.formula(label_model1)

    tmp<-terms.formula(model1)
    ntmp<-length(attr(tmp,"variables"))

    if(idx%%5==4){
        inter1<-get(as.character(attr(tmp,"variables")[[ntmp-2]])) * get(as.character(attr(tmp,"variables")[[ntmp]]))
        inter2<-get(as.character(attr(tmp,"variables")[[ntmp-1]])) * get(as.character(attr(tmp,"variables")[[ntmp]]))
        if(levels_test2<-length(levels(as.factor(inter1)))*length(levels(as.factor(inter2)))) {
            present2_mult<-ifelse(is.na(inter1),0,1)*ifelse(is.na(inter2),0,1)  
            not_na_test2_mult<-ifelse((exists.to.tab2(get(as.character(attr(tmp,"variables")[[ntmp-1]])) * get(as.character(attr(tmp,"variables")[[ntmp]])) )) &
                                      (exists.to.tab2(get(as.character(attr(tmp,"variables")[[ntmp-2]])) * get(as.character(attr(tmp,"variables")[[ntmp]])) )),1,0)
            model2 <- update(model1,paste(". ~ . +",
                                          paste(as.character(attr(tmp,"variables")[[(ntmp-2)]]),as.character(attr(tmp,"variables")[[(ntmp)]]),sep=":"),"+",
                                          paste(as.character(attr(tmp,"variables")[[(ntmp-1)]]),as.character(attr(tmp,"variables")[[(ntmp)]]),sep=":"),sep=" "))

            label_model3_tag<-paste(paste(as.character(attr(tmp,"variables")[[ntmp-1]]),as.character(attr(tmp,"variables")[[ntmp]]),sep=":"),"+",paste(as.character(attr(tmp,"variables")[[ntmp-2]]),as.character(attr(tmp,"variables")[[ntmp]]),sep=":"),sep="  ")
        } else {
            present2_mult<-0
            not_na_test2_mult<-0
            model2<-model1
            label_model3_tag<-NA
        }
    } else {
        inter<-get(as.character(attr(tmp,"variables")[[ntmp-1]])) * get(as.character(attr(tmp,"variables")[[ntmp]]))
        if(levels_test2<-length(levels(as.factor(inter)))) {
            present2_mult<-ifelse(is.na(inter),0,1)  
            not_na_test2_mult<-ifelse(exists.to.tab2( get(as.character(attr(tmp,"variables")[[ntmp-1]])) * get(as.character(attr(tmp,"variables")[[ntmp]])) ),1,0)
            model2 <- update(model1,paste(". ~ . +",paste(as.character(attr(tmp,"variables")[[ntmp-1]]),as.character(attr(tmp,"variables")[[ntmp]]),sep=":"),sep=" "))
            label_model3_tag<-paste(as.character(attr(tmp,"variables")[[ntmp-1]]),as.character(attr(tmp,"variables")[[ntmp]]),sep=":")
        } else {
            present2_mult<-0
            not_na_test2_mult<-0
            model2<-model1
            label_model3_tag<-NA
        }
    }

    var.level.flag.vector<-c(rep(1,ntmp-1))
    temp.obj<-var.level.check.modlog(ntmp,var.level.flag.vector,model1,model2,tmp)
    model1<-temp.obj[[1]]
    model2<-temp.obj[[2]]
    tmp<-temp.obj[[3]]
    x<-numeric(0)
    for(l in 4:(ntmp + 2)) { 
      x<-append(x,temp.obj[[l]])
    }
    var.level.flag.vector<-x
    
    tmp<-terms.formula(model1)
    ntmp<-length(attr(tmp,"variables"))

    dummy.var.vec<-c(rep(0,ntmp))
    SNP.var.vec<-c(rep(0,ntmp))
    stress.index<-2

    for(i in 3:ntmp) {
        if(length(grep("dum",as.character(attr(tmp,"variables")[[i]]))) > 0) dummy.var.vec[i]<-1
        if((length(grep("child_mal",
                        as.character(attr(tmp,"variables")[[i]]))) > 0) |
           (length(grep("stress_combined",
                        as.character(attr(tmp,"variables")[[i]]))) > 0) |
           (length(grep("life_stress",
                        as.character(attr(tmp,"variables")[[i]]))) > 0)) stress.index<-i
        if((length(grep("5http",
                        as.character(attr(tmp,"variables")[[i]]))) > 0) |
           (length(grep("rs25531",
                        as.character(attr(tmp,"variables")[[i]]))) > 0)) SNP.var.vec[i]<-1
    }
    
    if(idx%%5==4){
        inter1<-c(rep(NA,length(outcomes_f)))
        inter2<-c(rep(NA,length(outcomes_f)))
        for(i in 3:ntmp) {
            if(dummy.var.vec[i]==1) {
                if(length(grep("dum1",as.character(attr(tmp,"variables")[[i]]))) > 0) {
                    inter1<-get(as.character(attr(tmp,"variables")[[i]])) * get(as.character(attr(tmp,"variables")[[stress.index]]))
                }
                if(length(grep("dum2",as.character(attr(tmp,"variables")[[i]]))) > 0)   {
                    inter2<-get(as.character(attr(tmp,"variables")[[i]])) * get(as.character(attr(tmp,"variables")[[stress.index]]))
                }                           
            }
        }
        
        if(levels_test2<-length(levels(as.factor(inter1)))*length(levels(as.factor(inter2)))) {
            not_na_test2_mult<-ifelse(exists.to.tab2(inter1 * inter2),1,0)
            present2_mult<-ifelse(is.na(inter1),0,1)*ifelse(is.na(inter2),0,1)
        } else {
            present2_mult<-0
            not_na_test2_mult<-0
        }
    } else {
        inter<-c(rep(NA,length(outcomes_f)))
        for(i in 3:ntmp) {
            if(SNP.var.vec[i]==1) {
                inter<-get(as.character(attr(tmp,"variables")[[i]])) * get(as.character(attr(tmp,"variables")[[stress.index]]))
            }
        }
        if(levels_test2<-length(levels(as.factor(inter)))) {
            present2_mult<-ifelse(is.na(inter),0,1)  
            not_na_test2_mult<-ifelse(exists.to.tab2(inter),1,0)
        } else {
            present2_mult<-0
            not_na_test2_mult<-0
        }
    }
    
    label_model <-paste(as.character(model1)[2],"~",as.character(model1)[3],sep=" ")
    label_model3<-if(is.na(label_model3_tag)) label_model1 else paste(label_model1,"+", label_model3_tag,sep="  ")
       
    running.fem<-c(rep(1,length(get(outcomes_f))))
    for(j in 2:ntmp) {
      running.fem<-running.fem*ifelse(is.na(get(as.character(attr(tmp,"variables")[[j]]))),NA,1)
    }
    fems<-levels(as.factor(ifelse(is.na(running.fem*female),NA,female)))
    nfemlevels<-length(fems)

    fem_levels<-if (nfemlevels==2) "combined-sex sample" else
    if (nfemlevels==1) {
        if (fems[1]==0) "female-only sample" else
        if (fems[1]==1) "male-only sample"
    }
         
    if(ntmp > 2) {
        flag<-0
        header<-paste("START",label_model3, sep="  " )
        footer<-paste("STOP",label_model3, sep="  " )
        cat("=== running ",label_model3, "\n")

        levels_test<-1
        not_na_test<-1
        present<-c(rep(1,length(get(outcomes_f))))*ifelse(is.na(get(as.character(attr(tmp,"variables")[[2]]))),0,1)

        for(i in 3:ntmp) {    
            levels_test<-levels_test*length(levels(as.factor(get(as.character(attr(tmp,"variables")[[i]])))))
            not_na_test<-not_na_test*ifelse(exists.to.tab2(get(as.character(attr(tmp,"variables")[[i]]))),1,0)
            present<-present*ifelse(is.na(get(as.character(attr(tmp,"variables")[[i]]))),0,1)
        }
        
        tot<-sum(na.exclude(present))

        present2<-present*present2_mult  
        not_na_test2<-not_na_test*not_na_test2_mult

        tot2<-sum(na.exclude(present2))
        
        if((levels_test>1) & not_na_test & tot>2){
            out <- glm(model1, family=type_f,data=data_f)  
            summary_out <- summary(out)
            out_vcov <- vcov(out)
            nparms <- length(summary_out$coefficients[,1])
            top.row<-c("outcome", "parameter",names(as.data.frame(summary_out$coefficients)) ,
                       "dev_df","deviance","vcov label",paste(rep("vcov",nparms),c(1:nparms)))
            
            results<-cbind(c(rep(outcomes_f,nparms)),
                           row.names(summary_out$coefficients),
                           summary_out$coefficients,
                           matrix(rep( c(summary_out$df[2],summary_out$deviance),nparms),ncol=2, byrow=T),
                           c(1:nparms),out_vcov)
    
            name.parms <- row.names(coef(summary_out))
            
            write.table(t(header),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )

            if((levels_test2>1) & not_na_test2 & tot2>2) {
                testout<-1

                for (l in 2:length(out$coeff)){
                    testout<-testout*( if(is.na(out$coeff[l])) 1 else 0 )
                }
                if(!testout) {
                    out2 <- glm(model2, family=type_f,data=data_f)

                    if((is.na(out2$coeff[ntmp])==FALSE) &
                       (out$df.residual > 0) &
                       (out2$df.residual > 0) &
                       (out$df.residual > out2$df.residual)) {
                        flag<-1
                        mod_comp<-anova(out, out2, dispersion = 1, test="Chi")      
                        mod_comp.row<-colnames(mod_comp)
                        if (mod_comp$Df[2]==0 | is.na(mod_comp$Df[2])) flag<-0

                        summary_out2 <- summary(out2)
                        out2_vcov <- vcov(out2)
                        nparms2 <- length(summary_out2$coefficients[,1])
                        top.row2<-c("outcome", "parameter",names(as.data.frame(summary_out2$coefficients)) ,
                                    "dev_df","deviance","vcov label",paste(rep("vcov",nparms2),c(1:nparms2)))
                    
                        results2<-cbind(c(rep(outcomes_f,nparms2)),
                                        row.names(summary_out2$coefficients),
                                        summary_out2$coefficients,
                                        matrix(rep( c(summary_out2$df[2],summary_out2$deviance),nparms2),ncol=2, byrow=T),
                                        c(1:nparms2),out2_vcov)
                    
                        name.parms2 <- row.names(coef(summary_out2))
                    
                        write.table(t(mod_comp.row),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                        write.table(mod_comp,file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                        write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                    } else {
                        write.table(paste(label_model3,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
                        flag<-0
                    }
                } else {
                    write.table(paste(label_model3,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
                    flag<-0
                  }
            } else {
                write.table(paste(label_model3,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
                flag<-0
            }

            write.table(paste("N=",tot,sep=""),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )    
            write.table(t(top.row),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
            write.table(results,file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")	 
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
        
            if(flag){
                write.table(paste("N=",tot2,sep=""),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )
                write.table(t(top.row2),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                write.table(results2,file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                flag<-0
            }
            write.table(t(footer),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            rm(flag)
        } else {
            write.table(paste(label_model1,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            write.table(paste(label_model3,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
    } else {
        write.table(paste(label_model1,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)       
    }
  }
}





#########################################
# run moderated logistic regression     #
# for 1-way SNP Association Models      #
#########################################  
oneSNP.modlin <- function(idx,snplist_f, outcomes_f, models_f, interaction_f, type_f,out.file,data_f) {
  if(idx%%5!=0){  
    model1.text <- paste( outcomes_f,models_f, sep=" ")
    if(idx%%5==4){
      label_model1 <- gsub("SNP", paste(snplist_f,"+",snplist[idx+1],
                                        sep=" "), model1.text )
    } else {
      label_model1 <- gsub("SNP", snplist_f , model1.text )
    }

    label_model <- gsub("~","=", label_model1)                 
    model1 <- as.formula(label_model1)

    tmp<-terms.formula(model1)
    ntmp<-length(attr(tmp,"variables"))
    
    if(idx%%5==4) {
      model1int <- update(model1,paste(". ~ . -",
                                       as.character(attr(tmp,"variables")[[(ntmp-2)]]),"-",
                                       as.character(attr(tmp,"variables")[[(ntmp-1)]]),"-",
                                       as.character(attr(tmp,"variables")[[(ntmp)]]),"-",
                                       paste(as.character(attr(tmp,"variables")[[(ntmp-1)]]),
                                             as.character(attr(tmp,"variables")[[(ntmp)]]),
                                             sep=":"),"-",
                                       paste(as.character(attr(tmp,"variables")[[(ntmp-2)]]),
                                             as.character(attr(tmp,"variables")[[(ntmp)]]),
                                             sep=":"),sep=" ")
                          )
      model1 <- update(model1int,paste(". ~ . +",
                                       as.character(attr(tmp,"variables")[[(ntmp-2)]]),"+",
                                       as.character(attr(tmp,"variables")[[(ntmp-1)]]),"+",
                                       as.character(attr(tmp,"variables")[[(ntmp)]])))

      inter1<-get(as.character(attr(tmp,"variables")[[ntmp-2]])) * get(as.character(attr(tmp,"variables")[[ntmp]]))
      inter2<-get(as.character(attr(tmp,"variables")[[ntmp-1]])) * get(as.character(attr(tmp,"variables")[[ntmp]]))
      if(levels_test2<-length(levels(as.factor(inter1)))*length(levels(as.factor(inter2)))) {
          present2_mult<-ifelse(is.na(inter1),0,1)*ifelse(is.na(inter2),0,1)  
          not_na_test2_mult<-ifelse((exists.to.tab2(get(as.character(attr(tmp,"variables")[[ntmp-1]])) * get(as.character(attr(tmp,"variables")[[ntmp]])) )) &
                                    (exists.to.tab2(get(as.character(attr(tmp,"variables")[[ntmp-2]])) * get(as.character(attr(tmp,"variables")[[ntmp]])))) ,1,0)
          model2 <- update(model1,paste(". ~ . +",
                                        paste(as.character(attr(tmp,"variables")[[(ntmp-2)]]),as.character(attr(tmp,"variables")[[(ntmp)]]),sep=":"),"+",
                                        paste(as.character(attr(tmp,"variables")[[(ntmp-1)]]),as.character(attr(tmp,"variables")[[(ntmp)]]),sep=":"),sep=" "))

          label_model3_tag<-paste(paste(as.character(attr(tmp,"variables")[[ntmp-2]]),as.character(attr(tmp,"variables")[[ntmp]]),sep=":"),"+",paste(as.character(attr(tmp,"variables")[[ntmp-1]]),as.character(attr(tmp,"variables")[[ntmp]]),sep=":"),sep="  ")
      } else {
          present2_mult<-0
          not_na_test2_mult<-0
          model2<-model1
          label_model3_tag<-NA
      }
  } else {
      model1 <- update(model1,paste(". ~ . -",
                                    paste(as.character(attr(tmp,"variables")[[(ntmp-1)]]),
                                          as.character(attr(tmp,"variables")[[(ntmp)]]),
                                          sep=":"),sep=" "))
      inter<-get(as.character(attr(tmp,"variables")[[ntmp-1]])) * get(as.character(attr(tmp,"variables")[[ntmp]]))
      if(levels_test2<-length(levels(as.factor(inter)))) {
          present2_mult<-ifelse(is.na(inter),0,1)  
          not_na_test2_mult<-ifelse(exists.to.tab2(get(as.character(attr(tmp,"variables")[[ntmp-1]])) * get(as.character(attr(tmp,"variables")[[ntmp]]))),1,0)
          model2 <- update(model1,paste(". ~ . +",paste(as.character(attr(tmp,"variables")[[ntmp-1]]),as.character(attr(tmp,"variables")[[ntmp]]),sep=":"),sep=" "))
          label_model3_tag<-paste(as.character(attr(tmp,"variables")[[ntmp-1]]),as.character(attr(tmp,"variables")[[ntmp]]),sep=":")
      } else {
          present2_mult<-0
          not_na_test2_mult<-0
          model2<-model1
          label_model3_tag<-NA
      }
  }

    var.level.flag.vector<-c(rep(1,ntmp-1))
    temp.obj<-var.level.check.modlog(ntmp,var.level.flag.vector,model1,model2,tmp)
    model1<-temp.obj[[1]]
    model2<-temp.obj[[2]]
    tmp<-temp.obj[[3]]
    x<-numeric(0)
    for(l in 4:(ntmp + 2)) { 
      x<-append(x,temp.obj[[l]])
    }
    var.level.flag.vector<-x
    
    tmp<-terms.formula(model1)
    ntmp<-length(attr(tmp,"variables"))

    dummy.var.vec<-c(rep(0,ntmp))
    SNP.var.vec<-c(rep(0,ntmp))
    stress.index<-2
    for(i in 3:ntmp) {
        if(length(grep("dum",as.character(attr(tmp,"variables")[[i]]))) > 0) dummy.var.vec[i]<-1
        if((length(grep("child_mal",
                        as.character(attr(tmp,"variables")[[i]]))) > 0) |
           (length(grep("stress_combined",
                        as.character(attr(tmp,"variables")[[i]]))) > 0) |
           (length(grep("life_stress",
                        as.character(attr(tmp,"variables")[[i]]))) > 0)) stress.index<-i
        if((length(grep("5http",
                        as.character(attr(tmp,"variables")[[i]]))) > 0) |
           (length(grep("rs25531",
                        as.character(attr(tmp,"variables")[[i]]))) > 0)) SNP.var.vec[i]<-1
    }

    if(idx%%5==4){ 
        inter1<-c(rep(NA,length(outcomes_f)))
        inter2<-c(rep(NA,length(outcomes_f)))
        for(i in 3:ntmp) {
            if(dummy.var.vec[i]==1) {
                if(length(grep("dum1",as.character(attr(tmp,"variables")[[i]]))) > 0) {
                    inter1<-get(as.character(attr(tmp,"variables")[[i]])) * get(as.character(attr(tmp,"variables")[[stress.index]]))
                }
                if(length(grep("dum2",as.character(attr(tmp,"variables")[[i]]))) > 0)   {
                    inter2<-get(as.character(attr(tmp,"variables")[[i]])) * get(as.character(attr(tmp,"variables")[[stress.index]]))
                }                           
            }
        }
         
        if(levels_test2<-length(levels(as.factor(inter1)))*length(levels(as.factor(inter2)))) {
            present2_mult<-ifelse(is.na(inter1),0,1)*ifelse(is.na(inter2),0,1)  
            not_na_test2_mult<-ifelse(exists.to.tab2(inter1 * inter2),1,0)
        } else {
            present2_mult<-0
            not_na_test2_mult<-0
        }
    } else {
        inter<-c(rep(NA,length(outcomes_f)))
        for(i in 3:ntmp) {
            if(SNP.var.vec[i]==1) {
                inter<-get(as.character(attr(tmp,"variables")[[i]])) * get(as.character(attr(tmp,"variables")[[stress.index]]))
            }
        }
        if(levels_test2<-length(levels(as.factor(inter)))) {
            present2_mult<-ifelse(is.na(inter),0,1)  
            not_na_test2_mult<-ifelse(exists.to.tab2(inter),1,0)
        } else {
            present2_mult<-0
            not_na_test2_mult<-0
        }
    }
    
    label_model <-paste(as.character(model1)[2],"~",as.character(model1)[3],sep=" ")
    label_model3<-if(is.na(label_model3_tag)) label_model else paste(label_model,"+", label_model3_tag,sep="  ")
    
    running.fem<-c(rep(1,length(get(outcomes_f))))
    for(j in 2:ntmp) {
      running.fem<-running.fem*ifelse(is.na(get(as.character(attr(tmp,"variables")[[j]]))),NA,1)
    }
    fems<-levels(as.factor(ifelse(is.na(running.fem*female),NA,female)))
    nfemlevels<-length(fems)

    fem_levels<-if (nfemlevels==2) "combined-sex sample" else
    if (nfemlevels==1) {
        if (fems[1]==0) "female-only sample" else
        if (fems[1]==1) "male-only sample"
    }
         
    if(ntmp > 2) {
        flag<-0
        header<-paste("START",label_model3, sep="  " )
        footer<-paste("STOP",label_model3, sep="  " )
        cat("=== running ",label_model3, "\n")

        levels_test<-1
        not_na_test<-1
        present<-c(rep(1,length(get(outcomes_f))))*ifelse(is.na(get(as.character(attr(tmp,"variables")[[2]]))),0,1)

        for(i in 3:ntmp) {    
            levels_test<-levels_test*length(levels(as.factor(get(as.character(attr(tmp,"variables")[[i]])))))
            not_na_test<-not_na_test*ifelse(exists.to.tab2(get(as.character(attr(tmp,"variables")[[i]]))),1,0)
            present<-present*ifelse(is.na(get(as.character(attr(tmp,"variables")[[i]]))),0,1)
        }
        
        tot<-sum(na.exclude(present))

        present2<-present*present2_mult  
        not_na_test2<-not_na_test*not_na_test2_mult

        tot2<-sum(na.exclude(present2))
        
        if((levels_test>1) & not_na_test & tot>2){
            out <- glm(model1, family=type_f,data=data_f)  
            summary_out <- summary(out)
            out_vcov <- vcov(out)
            nparms <- length(summary_out$coefficients[,1])
            top.row<-c("outcome", "parameter",names(as.data.frame(summary_out$coefficients)) ,
                       "dev_df","deviance","vcov label",paste(rep("vcov",nparms),c(1:nparms)))
            
            results<-cbind(c(rep(outcomes_f,nparms)),
                           row.names(summary_out$coefficients),
                           summary_out$coefficients,
                           matrix(rep( c(summary_out$df[2],summary_out$deviance),nparms),ncol=2, byrow=T),
                           c(1:nparms),out_vcov)
    
            name.parms <- row.names(coef(summary_out))
            
            write.table(t(header),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )

            if((levels_test2>1) & not_na_test2 & tot2>2) {
                testout<-1

                for (l in 2:length(out$coeff)){
                    testout<-testout*( if(is.na(out$coeff[l])) 1 else 0 )
                }
                if(!testout) {
                    out2 <- glm(model2, family=type_f,data=data_f)

                    if((is.na(out2$coeff[ntmp])==FALSE) &
                       (out$df.residual > 0) &
                       (out2$df.residual > 0) &
                       (out$df.residual > out2$df.residual)) {
                        flag<-1
                        mod_comp<-anova(out, out2, test="Chi")      
                        mod_comp.row<-colnames(mod_comp)
                        if (mod_comp$Df[2]==0 | is.na(mod_comp$Df[2])) flag<-0
                       
                        summary_out2 <- summary(out2)
                        out2_vcov <- vcov(out2)
                        nparms2 <- length(summary_out2$coefficients[,1])
                        top.row2<-c("outcome", "parameter",names(as.data.frame(summary_out2$coefficients)) ,
                                    "dev_df","deviance","vcov label",paste(rep("vcov",nparms2),c(1:nparms2)))
                    
                        results2<-cbind(c(rep(outcomes_f,nparms2)),
                                        row.names(summary_out2$coefficients),
                                        summary_out2$coefficients,
                                        matrix(rep( c(summary_out2$df[2],summary_out2$deviance),nparms2),ncol=2, byrow=T),
                                        c(1:nparms2),out2_vcov)
                    
                        name.parms2 <- row.names(coef(summary_out2))
                            
                        write.table(t(mod_comp.row),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                        write.table(mod_comp,file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                    }
                } else {
                    write.table(paste(label_model3,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
                    flag<-0
                }
            } else {
                write.table(paste(label_model3,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
                flag<-0
            }

            write.table(paste("N=",tot,sep=""),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )    
            write.table(t(top.row),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
            write.table(results,file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")	 
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
        
            if(flag){
                write.table(paste("N=",tot2,sep=""),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE )
                write.table(t(top.row2),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                write.table(results2,file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
                flag<-0
            }
            write.table(t(footer),file=out.file,col.names=FALSE,   row.names=FALSE,append=TRUE, quote=FALSE,sep="\t")
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            rm(flag)
        } else {
            write.table(paste(label_model1,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            write.table(paste(label_model3,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
    } else {
        write.table(paste(label_model1,"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)       
    }
  }
}





ModLogR_DDDS.call<-function(outcomes_f, models_f, by.var_f) {
    cat("===== run depression dx w/ dichotomous stress_exposure input", "\n" )
 
    outcomes <- outcomes_f
    noutcomes <- length(outcomes)
    type<- 'binomial'
    
    model1.text <- paste( outcomes_f[1],models_f[1], sep=" ")    
    model1 <- as.formula(model1.text)
    vars<-as.character(attr(terms.formula(model1),"variables"))
    stress_f<-vars[length(vars)]    
    
    models<-if(decade_na_flag()) c(paste("~",vars[3],"+",vars[6],"+",stress_f,sep=" "),
                                   paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress_f,sep=" ")
                                   ) else c(paste("~",vars[3],"+",vars[6],"+",stress_f,sep=" "),
                                            paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stress_f,sep=" "),
                                            paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress_f,sep=" "),
                                            paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stress_f,sep=" ")
                                            )

    interact<-c(rep(paste(vars[6],stress_f,sep=":"), length(models)))
    by.list <-  c(rep(by.var_f, length(models)))

    nmodels <- length(models)
    
    rm(model1,model1.text)
    
    if(both_sexes=="YES"){      
      cat("===== combined sex subset", "\n" )      
      fem_levels<-"combined-sex sample"

      if(noutcomes==1) {
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              Ldum1_5http,
                              Ldum2_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              L_Adum1_rs25531,
                              L_Adum2_rs25531,
                              get(outcomes[1]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      if(noutcomes==2) {
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              Ldum1_5http,
                              Ldum2_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              L_Adum1_rs25531,
                              L_Adum2_rs25531,
                              get(outcomes[1]),
                              get(outcomes[2]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      if(noutcomes==3) {
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              Ldum1_5http,
                              Ldum2_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              L_Adum1_rs25531,
                              L_Adum2_rs25531,
                              get(outcomes[1]),
                              get(outcomes[2]),
                              get(outcomes[3]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      
        attach(temp_df)
      
      for(i in 1: noutcomes) {
        out.file<-paste(outdir_mod,SITE,file_label, outcomes[i],"_DS.txt",sep="")
                
        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:nmodels) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {	    
                write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP.modlog(j,snplist[j], outcomes[i], models[k],interact[k], type,out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models[k],by.list[k],type,out.file,temp_df)
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      } 
      detach(temp_df)
    }
    
    if (sum(ifelse(is.na(iid_f),1,0))/length(iid_f) < 0.9) {
      cat("===== female only subset", "\n" )
      fem_levels<-"female-only sample"
      if(noutcomes==1) {
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              Ldum1_5http[is.na(iid_f)==FALSE],
                              Ldum2_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              L_Adum1_rs25531[is.na(iid_f)==FALSE],
                              L_Adum2_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      if(noutcomes==2) {
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              Ldum1_5http[is.na(iid_f)==FALSE],
                              Ldum2_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              L_Adum1_rs25531[is.na(iid_f)==FALSE],
                              L_Adum2_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              get(outcomes[2])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      if(noutcomes==3) {
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              Ldum1_5http[is.na(iid_f)==FALSE],
                              Ldum2_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              L_Adum1_rs25531[is.na(iid_f)==FALSE],
                              L_Adum2_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              get(outcomes[2])[is.na(iid_f)==FALSE],
                              get(outcomes[3])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }

      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir_mod,SITE,file_label, outcomes[i],"_DS.txt",sep="")

        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:nmodels) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP.modlog(j,snplist[j], outcomes[i], models[k],interact[k], type,out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models[k],by.list[k],type,out.file,temp_df)
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      } 
      detach(temp_df)
    }

    if (sum(ifelse(is.na(iid_m),1,0))/length(iid_m) < 0.9) {
      cat("===== male only subset", "\n" )
      fem_levels<-"male-only sample"
      if(noutcomes==1) {
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              Ldum1_5http[is.na(iid_m)==FALSE],
                              Ldum2_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              L_Adum1_rs25531[is.na(iid_m)==FALSE],
                              L_Adum2_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      if(noutcomes==2) {
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              Ldum1_5http[is.na(iid_m)==FALSE],
                              Ldum2_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              L_Adum1_rs25531[is.na(iid_m)==FALSE],
                              L_Adum2_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              get(outcomes[2])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      if(noutcomes==3) {
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              Ldum1_5http[is.na(iid_m)==FALSE],
                              Ldum2_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              L_Adum1_rs25531[is.na(iid_m)==FALSE],
                              L_Adum2_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              get(outcomes[2])[is.na(iid_m)==FALSE],
                              get(outcomes[3])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }

      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir_mod,SITE,file_label, outcomes[i],"_DS.txt",sep="")

        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:nmodels) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP.modlog(j,snplist[j], outcomes[i], models[k],interact[k],type,out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models[k],by.list[k],type,out.file,temp_df)
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      } 
      detach(temp_df)
    }
  }



ModLogR_DDQS.call<-function(outcomes_f, models_f, by.var_f) {
  cat("===== run depression dx w/ normalized quantitative stress input", "\n" )
 
  outcomes <- outcomes_f
  noutcomes <- length(outcomes)
  type<- 'binomial'

  model1.text <- paste( outcomes_f[1],models_f[1], sep=" ")    
  model1 <- as.formula(model1.text)
  vars<-as.character(attr(terms.formula(model1),"variables"))
  stress_f<-vars[length(vars)]    
  stress2_f<-paste(stress_f,"2",sep="")
  stressz_f<-paste(stress_f,"_z",sep="")
  stress2z_f<-paste(stress_f,"2_z",sep="")

  rm(model1,model1.text)

  models<-if(decade_na_flag()) c(paste("~",vars[3],"+",vars[6],"+",stressz_f,sep=" "),
                                 paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stressz_f,sep=" "),
                                 paste("~",vars[3],"+",vars[6],"+",stress2z_f,sep=" "),
                                 paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress2z_f,sep=" ")    
                                 ) else c(paste("~",vars[3],"+",vars[6],"+",stressz_f,sep=" "),
                                          paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stressz_f,sep=" "),    
                                          paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stressz_f,sep=" "),
                                          paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stressz_f,sep=" "),
                                          paste("~",vars[3],"+",vars[6],"+",stress2z_f,sep=" "),
                                          paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stress2z_f,sep=" "),    
                                          paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress2z_f,sep=" "),
                                          paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stress2z_f,sep=" ")
                                          )
 
  models.raw<-if(decade_na_flag()) c(paste("~",vars[3],"+",vars[6],"+",stress_f,sep=" "),
                                     paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress_f,sep=" "),
                                     paste("~",vars[3],"+",vars[6],"+",stress2_f,sep=" "),
                                     paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress2_f,sep=" ")    
                                     ) else c(paste("~",vars[3],"+",vars[6],"+",stress_f,sep=" "),
                                              paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stress_f,sep=" "),    
                                              paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress_f,sep=" "),
                                              paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stress_f,sep=" "),
                                              paste("~",vars[3],"+",vars[6],"+",stress2_f,sep=" "),
                                              paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stress2_f,sep=" "),    
                                              paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress2_f,sep=" "),
                                              paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stress2_f,sep=" ")
                                              )  
  
  interact<-c(rep(paste(vars[6],stressz_f,sep=":"), length(models)/2),rep(paste(vars[6],stress2z_f,sep=":"), length(models)/2))
  interact.raw<-c(rep(paste(vars[6],stress_f,sep=":"),length(models.raw)/2),rep(paste(vars[6],stress2_f,sep=":"),length(models.raw)/2))
  by.list <-  c(rep(by.var_f, length(models)))

  nmodels <- length(models)

  if(both_sexes=="YES"){      
    cat("===== combined sex subset", "\n" )      
    fem_levels<-"combined-sex sample"
      if(noutcomes==1) {
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              Ldum1_5http,
                              Ldum2_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              L_Adum1_rs25531,
                              L_Adum2_rs25531,
                              get(outcomes[1]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f),                       
                              get(stressz_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      if(noutcomes==2) {
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              Ldum1_5http,
                              Ldum2_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              L_Adum1_rs25531,
                              L_Adum2_rs25531,
                              get(outcomes[1]),
                              get(outcomes[2]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f),                       
                              get(stressz_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      if(noutcomes==3) {
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              Ldum1_5http,
                              Ldum2_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              L_Adum1_rs25531,
                              L_Adum2_rs25531,
                              get(outcomes[1]),
                              get(outcomes[2]),
                              get(outcomes[3]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f),                       
                              get(stressz_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
    
        attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir_mod,SITE,file_label, outcomes[i],"_QS.txt",sep="")
                
        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:(nmodels/2)) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP.modlog(j,snplist[j], outcomes[i], models[k],interact[k], type,out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models[k],by.list[k],type,out.file,temp_df)

                if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {

                  write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(paste(outcomes[i],models.raw[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  oneSNP.modlog(j,snplist[j], outcomes[i], models.raw[k],interact.raw[k], type,out.file,temp_df)
                  oneSNP.by(j,snplist[j],outcomes[i],models.raw[k],by.list[k],type,out.file,temp_df)
                }
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
            for(k in (nmodels/2 + 1):nmodels) {
              if ( (two_level_present_var(get(stress2_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP.modlog(j,snplist[j], outcomes[i], models[k],interact[k], type,out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models[k],by.list[k],type,out.file,temp_df)
                if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
                  write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(paste(outcomes[i],models.raw[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  oneSNP.modlog(j,snplist[j], outcomes[i], models.raw[k],interact.raw[k], type,out.file,temp_df)
                  oneSNP.by(j,snplist[j],outcomes[i],models.raw[k],by.list[k],type,out.file,temp_df)
                }
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      } 
      detach(temp_df)
    }
    
  if (sum(ifelse(is.na(iid_f),1,0))/length(iid_f) < 0.9) {
    cat("===== female only subset", "\n" )
    fem_levels<-"female-only sample"
      if(noutcomes==1) {
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              Ldum1_5http[is.na(iid_f)==FALSE],
                              Ldum2_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              L_Adum1_rs25531[is.na(iid_f)==FALSE],
                              L_Adum2_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE],
                              get(stressz_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      if(noutcomes==2) {
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              Ldum1_5http[is.na(iid_f)==FALSE],
                              Ldum2_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              L_Adum1_rs25531[is.na(iid_f)==FALSE],
                              L_Adum2_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              get(outcomes[2])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE],        
                              get(stressz_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      if(noutcomes==3) {
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              Ldum1_5http[is.na(iid_f)==FALSE],
                              Ldum2_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              L_Adum1_rs25531[is.na(iid_f)==FALSE],
                              L_Adum2_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              get(outcomes[2])[is.na(iid_f)==FALSE],
                              get(outcomes[3])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE],
                              get(stressz_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
        
        attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir_mod,SITE,file_label, outcomes[i],"_QS.txt",sep="")
        
        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:(nmodels/2)) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP.modlog(j,snplist[j], outcomes[i], models[k],interact[k], type,out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models[k],by.list[k],type,out.file,temp_df)
                if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
                  write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(paste(outcomes[i],models.raw[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  oneSNP.modlog(j,snplist[j], outcomes[i], models.raw[k],interact.raw[k], type,out.file,temp_df)
                  oneSNP.by(j,snplist[j],outcomes[i],models.raw[k],by.list[k],type,out.file,temp_df)
                }
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
            for(k in (nmodels/2 + 1):nmodels) {
              if ( (two_level_present_var(get(stress2_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP.modlog(j,snplist[j], outcomes[i], models[k],interact[k], type,out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models[k],by.list[k],type,out.file,temp_df)
                if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
                  write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(paste(outcomes[i],models.raw[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  oneSNP.modlog(j,snplist[j], outcomes[i], models.raw[k],interact.raw[k], type,out.file,temp_df)
                  oneSNP.by(j,snplist[j],outcomes[i],models.raw[k],by.list[k],type,out.file,temp_df)
                }
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      } 
      detach(temp_df)
    }

    if (sum(ifelse(is.na(iid_m),1,0))/length(iid_m) < 0.9) {
      cat("===== male only subset", "\n" )      
      fem_levels<-"male-only sample"
      if(noutcomes==1) {
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              Ldum1_5http[is.na(iid_m)==FALSE],
                              Ldum2_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              L_Adum1_rs25531[is.na(iid_m)==FALSE],
                              L_Adum2_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE],
                              get(stressz_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      if(noutcomes==2) {
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              Ldum1_5http[is.na(iid_m)==FALSE],
                              Ldum2_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              L_Adum1_rs25531[is.na(iid_m)==FALSE],
                              L_Adum2_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              get(outcomes[2])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE],
                              get(stressz_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      if(noutcomes==3) {
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              Ldum1_5http[is.na(iid_m)==FALSE],
                              Ldum2_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              L_Adum1_rs25531[is.na(iid_m)==FALSE],
                              L_Adum2_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              get(outcomes[2])[is.na(iid_m)==FALSE],
                              get(outcomes[3])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE],
                              get(stressz_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "Ldum1_5http",
                            "Ldum2_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            "L_Adum1_rs25531",
                            "L_Adum2_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
    
        attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir_mod,SITE,file_label, outcomes[i],"_QS.txt",sep="")
        
        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:(nmodels/2)) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP.modlog(j,snplist[j], outcomes[i], models[k],interact[k], type,out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models[k],by.list[k],type,out.file,temp_df)
                if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
                  write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(paste(outcomes[i],models.raw[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  oneSNP.modlog(j,snplist[j], outcomes[i], models.raw[k],interact.raw[k], type,out.file,temp_df)
                  oneSNP.by(j,snplist[j],outcomes[i],models.raw[k],by.list[k],type,out.file,temp_df)
                }
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
            for(k in (nmodels/2 + 1):nmodels) {
              if ( (two_level_present_var(get(stress2_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP.modlog(j,snplist[j], outcomes[i], models[k],interact[k], type,out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models[k],by.list[k],type,out.file,temp_df)
                if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
                  write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(paste(outcomes[i],models.raw[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  oneSNP.modlog(j,snplist[j], outcomes[i], models.raw[k],interact.raw[k], type,out.file,temp_df)
                  oneSNP.by(j,snplist[j],outcomes[i],models.raw[k],by.list[k],type,out.file,temp_df)
                }
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      } 
      detach(temp_df)
    }
}
  



 # linear regression models #
 ############################                  


ModLinR_QDDS.call<-function(outcomes_f, models_f, by.var_f) {
  cat("===== run quantitative depression measure w/ dichotomous stress_exposure input", "\n" )

  outcomes <- outcomes_f
  noutcomes <- length(outcomes)
  type<- 'gaussian'
 
  model1.text <- paste( outcomes_f[1],models_f[1], sep=" ")    
  model1 <- as.formula(model1.text)
  vars<-as.character(attr(terms.formula(model1),"variables"))

  stress_f<-vars[length(vars)]    
#  if(length(grep("5y",depression_dx_label[j]))==0
  inter<-paste(vars[6],stress_f,sep=":")
  
  rm(model1,model1.text)

  models<-if(decade_na_flag()) c(paste("~",vars[3],"+",vars[6],"+",stress_f,"+",inter,sep=" "),
                                 paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress_f,"+",inter,sep=" ")
                                 ) else c(paste("~",vars[3],"+",vars[6],"+",stress_f,"+",inter,sep=" "),
                                          paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stress_f,"+",inter,sep=" "),    
                                          paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress_f,"+",inter,sep=" "),
                                          paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stress_f,"+",inter,sep=" ")
                                          )

  models2<-if(decade_na_flag()) c(paste("~",vars[3],"+",vars[6],"+",stress_f,sep=" "),
                                 paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress_f,sep=" ")
                                 ) else c(paste("~",vars[3],"+",vars[6],"+",stress_f,sep=" "),
                                          paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stress_f,sep=" "),    
                                          paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress_f,sep=" "),
                                          paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stress_f,sep=" ")
                                          )

  interact<-c(rep(inter, length(models)))
  by.list <-  c(rep(by.var_f, length(models)))

  nmodels <- length(models)
    
  if(both_sexes=="YES"){      
    cat("===== combined sex subset", "\n" )      
    fem_levels<-"combined-sex sample"

    if(noutcomes==1) { temp_df<-data.frame(add_5http,
         Ldom_5http,
         Lrec_5http,
         Ldum1_5http,
         Ldum2_5http,
         add_rs25531,
         L_Adom_rs25531,
         L_Arec_rs25531,
         L_Adum1_rs25531,
         L_Adum2_rs25531,
         get(outcomes[1]),
         female,
         age,
         birth_decade,
         get(stress_f)
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==2) { temp_df<-data.frame(add_5http,
         Ldom_5http,
         Lrec_5http,
         Ldum1_5http,
         Ldum2_5http,
         add_rs25531,
         L_Adom_rs25531,
         L_Arec_rs25531,
         L_Adum1_rs25531,
         L_Adum2_rs25531,
         get(outcomes[1]),
         get(outcomes[2]),
         female,
         age,
         birth_decade,
         get(stress_f)
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==3) { temp_df<-data.frame(add_5http,
         Ldom_5http,
         Lrec_5http,
         Ldum1_5http,
         Ldum2_5http,
         add_rs25531,
         L_Adom_rs25531,
         L_Arec_rs25531,
         L_Adum1_rs25531,
         L_Adum2_rs25531,
         get(outcomes[1]),
         get(outcomes[2]),
         get(outcomes[3]),
         female,
         age,
         birth_decade,
         get(stress_f)
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==4) { temp_df<-data.frame(add_5http,
         Ldom_5http,
         Lrec_5http,
         Ldum1_5http,
         Ldum2_5http,
         add_rs25531,
         L_Adom_rs25531,
         L_Arec_rs25531,
         L_Adum1_rs25531,
         L_Adum2_rs25531,
         get(outcomes[1]),
         get(outcomes[2]),
         get(outcomes[3]),
         get(outcomes[4]),
         female,
         age,
         birth_decade,
         get(stress_f)
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==5) { temp_df<-data.frame(add_5http,
         Ldom_5http,
         Lrec_5http,
         Ldum1_5http,
         Ldum2_5http,
         add_rs25531,
         L_Adom_rs25531,
         L_Arec_rs25531,
         L_Adum1_rs25531,
         L_Adum2_rs25531,
         get(outcomes[1]),
         get(outcomes[2]),
         get(outcomes[3]),
         get(outcomes[4]),
         get(outcomes[5]),
         female,
         age,
         birth_decade,
         get(stress_f)
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           outcomes[5],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==6) { temp_df<-data.frame(add_5http,
         Ldom_5http,
         Lrec_5http,
         Ldum1_5http,
         Ldum2_5http,
         add_rs25531,
         L_Adom_rs25531,
         L_Arec_rs25531,
         L_Adum1_rs25531,
         L_Adum2_rs25531,
         get(outcomes[1]),
         get(outcomes[2]),
         get(outcomes[3]),
         get(outcomes[4]),
         get(outcomes[5]),
         get(outcomes[6]),
         female,
         age,
         birth_decade,
         get(stress_f)
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           outcomes[5],
                           outcomes[6],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir_mod,SITE,file_label, outcomes[i],"_DS.txt",sep="")        
        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:nmodels) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
#                oneSNP.mod(j,snplist[j],outcomes[i],models[k],type,out.file,temp_df)
                oneSNP.modlin(j,snplist[j],outcomes[i],models[k],interact[k],"gaussian",out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models2[k],by.list[k],type,out.file,temp_df)
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      }
      detach(temp_df)
  }
    
    if (sum(ifelse(is.na(iid_f),1,0))/length(iid_f) < 0.9) {
      cat("===== female only subset", "\n" )      
      fem_levels<-"female-only sample"
    if(noutcomes==1) { temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
         Ldom_5http[is.na(iid_f)==FALSE],
         Lrec_5http[is.na(iid_f)==FALSE],
         Ldum1_5http[is.na(iid_f)==FALSE],
         Ldum2_5http[is.na(iid_f)==FALSE],
         add_rs25531[is.na(iid_f)==FALSE],
         L_Adom_rs25531[is.na(iid_f)==FALSE],
         L_Arec_rs25531[is.na(iid_f)==FALSE],
         L_Adum1_rs25531[is.na(iid_f)==FALSE],
         L_Adum2_rs25531[is.na(iid_f)==FALSE],
         get(outcomes[1])[is.na(iid_f)==FALSE],
         female[is.na(iid_f)==FALSE],
         age[is.na(iid_f)==FALSE],
         birth_decade[is.na(iid_f)==FALSE],
         get(stress_f)[is.na(iid_f)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==2) { temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
         Ldom_5http[is.na(iid_f)==FALSE],
         Lrec_5http[is.na(iid_f)==FALSE],
         Ldum1_5http[is.na(iid_f)==FALSE],
         Ldum2_5http[is.na(iid_f)==FALSE],
         add_rs25531[is.na(iid_f)==FALSE],
         L_Adom_rs25531[is.na(iid_f)==FALSE],
         L_Arec_rs25531[is.na(iid_f)==FALSE],
         L_Adum1_rs25531[is.na(iid_f)==FALSE],
         L_Adum2_rs25531[is.na(iid_f)==FALSE],
         get(outcomes[1])[is.na(iid_f)==FALSE],
         get(outcomes[2])[is.na(iid_f)==FALSE],
         female[is.na(iid_f)==FALSE],
         age[is.na(iid_f)==FALSE],
         birth_decade[is.na(iid_f)==FALSE],
         get(stress_f)[is.na(iid_f)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==3) { temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
         Ldom_5http[is.na(iid_f)==FALSE],
         Lrec_5http[is.na(iid_f)==FALSE],
         Ldum1_5http[is.na(iid_f)==FALSE],
         Ldum2_5http[is.na(iid_f)==FALSE],
         add_rs25531[is.na(iid_f)==FALSE],
         L_Adom_rs25531[is.na(iid_f)==FALSE],
         L_Arec_rs25531[is.na(iid_f)==FALSE],
         L_Adum1_rs25531[is.na(iid_f)==FALSE],
         L_Adum2_rs25531[is.na(iid_f)==FALSE],
         get(outcomes[1])[is.na(iid_f)==FALSE],
         get(outcomes[2])[is.na(iid_f)==FALSE],
         get(outcomes[3])[is.na(iid_f)==FALSE],
         female[is.na(iid_f)==FALSE],
         age[is.na(iid_f)==FALSE],
         birth_decade[is.na(iid_f)==FALSE],
         get(stress_f)[is.na(iid_f)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==4) { temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
         Ldom_5http[is.na(iid_f)==FALSE],
         Lrec_5http[is.na(iid_f)==FALSE],
         Ldum1_5http[is.na(iid_f)==FALSE],
         Ldum2_5http[is.na(iid_f)==FALSE],
         add_rs25531[is.na(iid_f)==FALSE],
         L_Adom_rs25531[is.na(iid_f)==FALSE],
         L_Arec_rs25531[is.na(iid_f)==FALSE],
         L_Adum1_rs25531[is.na(iid_f)==FALSE],
         L_Adum2_rs25531[is.na(iid_f)==FALSE],
         get(outcomes[1])[is.na(iid_f)==FALSE],
         get(outcomes[2])[is.na(iid_f)==FALSE],
         get(outcomes[3])[is.na(iid_f)==FALSE],
         get(outcomes[4])[is.na(iid_f)==FALSE],
         female[is.na(iid_f)==FALSE],
         age[is.na(iid_f)==FALSE],
         birth_decade[is.na(iid_f)==FALSE],
         get(stress_f)[is.na(iid_f)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==5) { temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
         Ldom_5http[is.na(iid_f)==FALSE],
         Lrec_5http[is.na(iid_f)==FALSE],
         Ldum1_5http[is.na(iid_f)==FALSE],
         Ldum2_5http[is.na(iid_f)==FALSE],
         add_rs25531[is.na(iid_f)==FALSE],
         L_Adom_rs25531[is.na(iid_f)==FALSE],
         L_Arec_rs25531[is.na(iid_f)==FALSE],
         L_Adum1_rs25531[is.na(iid_f)==FALSE],
         L_Adum2_rs25531[is.na(iid_f)==FALSE],
         get(outcomes[1])[is.na(iid_f)==FALSE],
         get(outcomes[2])[is.na(iid_f)==FALSE],
         get(outcomes[3])[is.na(iid_f)==FALSE],
         get(outcomes[4])[is.na(iid_f)==FALSE],
         get(outcomes[5])[is.na(iid_f)==FALSE],
         female[is.na(iid_f)==FALSE],
         age[is.na(iid_f)==FALSE],
         birth_decade[is.na(iid_f)==FALSE],
         get(stress_f)[is.na(iid_f)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           outcomes[5],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==6) { temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
         Ldom_5http[is.na(iid_f)==FALSE],
         Lrec_5http[is.na(iid_f)==FALSE],
         Ldum1_5http[is.na(iid_f)==FALSE],
         Ldum2_5http[is.na(iid_f)==FALSE],
         add_rs25531[is.na(iid_f)==FALSE],
         L_Adom_rs25531[is.na(iid_f)==FALSE],
         L_Arec_rs25531[is.na(iid_f)==FALSE],
         L_Adum1_rs25531[is.na(iid_f)==FALSE],
         L_Adum2_rs25531[is.na(iid_f)==FALSE],
         get(outcomes[1])[is.na(iid_f)==FALSE],
         get(outcomes[2])[is.na(iid_f)==FALSE],
         get(outcomes[3])[is.na(iid_f)==FALSE],
         get(outcomes[4])[is.na(iid_f)==FALSE],
         get(outcomes[5])[is.na(iid_f)==FALSE],
         get(outcomes[6])[is.na(iid_f)==FALSE],
         female[is.na(iid_f)==FALSE],
         age[is.na(iid_f)==FALSE],
         birth_decade[is.na(iid_f)==FALSE],
         get(stress_f)[is.na(iid_f)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           outcomes[5],
                           outcomes[6],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    

      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir_mod,SITE,file_label, outcomes[i],"_DS.txt",sep="")

        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:nmodels) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
#                oneSNP.mod(j,snplist[j],outcomes[i],models[k],type,out.file,temp_df)
                oneSNP.modlin(j,snplist[j],outcomes[i],models[k],interact[k],"gaussian",out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models2[k],by.list[k],type,out.file,temp_df)
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      }
      detach(temp_df)
    }

  
    if (sum(ifelse(is.na(iid_m),1,0))/length(iid_m) < 0.9) {
      cat("===== male only subset", "\n" )
      fem_levels<-"male-only sample"
    if(noutcomes==1) { temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
         Ldom_5http[is.na(iid_m)==FALSE],
         Lrec_5http[is.na(iid_m)==FALSE],
         Ldum1_5http[is.na(iid_m)==FALSE],
         Ldum2_5http[is.na(iid_m)==FALSE],
         add_rs25531[is.na(iid_m)==FALSE],
         L_Adom_rs25531[is.na(iid_m)==FALSE],
         L_Arec_rs25531[is.na(iid_m)==FALSE],
         L_Adum1_rs25531[is.na(iid_m)==FALSE],
         L_Adum2_rs25531[is.na(iid_m)==FALSE],
         get(outcomes[1])[is.na(iid_m)==FALSE],
         female[is.na(iid_m)==FALSE],
         age[is.na(iid_m)==FALSE],
         birth_decade[is.na(iid_m)==FALSE],
         get(stress_f)[is.na(iid_m)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==2) { temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
         Ldom_5http[is.na(iid_m)==FALSE],
         Lrec_5http[is.na(iid_m)==FALSE],
         Ldum1_5http[is.na(iid_m)==FALSE],
         Ldum2_5http[is.na(iid_m)==FALSE],
         add_rs25531[is.na(iid_m)==FALSE],
         L_Adom_rs25531[is.na(iid_m)==FALSE],
         L_Arec_rs25531[is.na(iid_m)==FALSE],
         L_Adum1_rs25531[is.na(iid_m)==FALSE],
         L_Adum2_rs25531[is.na(iid_m)==FALSE],
         get(outcomes[1])[is.na(iid_m)==FALSE],
         get(outcomes[2])[is.na(iid_m)==FALSE],
         female[is.na(iid_m)==FALSE],
         age[is.na(iid_m)==FALSE],
         birth_decade[is.na(iid_m)==FALSE],
         get(stress_f)[is.na(iid_m)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==3) { temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
         Ldom_5http[is.na(iid_m)==FALSE],
         Lrec_5http[is.na(iid_m)==FALSE],
         Ldum1_5http[is.na(iid_m)==FALSE],
         Ldum2_5http[is.na(iid_m)==FALSE],
         add_rs25531[is.na(iid_m)==FALSE],
         L_Adom_rs25531[is.na(iid_m)==FALSE],
         L_Arec_rs25531[is.na(iid_m)==FALSE],
         L_Adum1_rs25531[is.na(iid_m)==FALSE],
         L_Adum2_rs25531[is.na(iid_m)==FALSE],
         get(outcomes[1])[is.na(iid_m)==FALSE],
         get(outcomes[2])[is.na(iid_m)==FALSE],
         get(outcomes[3])[is.na(iid_m)==FALSE],
         female[is.na(iid_m)==FALSE],
         age[is.na(iid_m)==FALSE],
         birth_decade[is.na(iid_m)==FALSE],
         get(stress_f)[is.na(iid_m)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==4) { temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
         Ldom_5http[is.na(iid_m)==FALSE],
         Lrec_5http[is.na(iid_m)==FALSE],
         Ldum1_5http[is.na(iid_m)==FALSE],
         Ldum2_5http[is.na(iid_m)==FALSE],
         add_rs25531[is.na(iid_m)==FALSE],
         L_Adom_rs25531[is.na(iid_m)==FALSE],
         L_Arec_rs25531[is.na(iid_m)==FALSE],
         L_Adum1_rs25531[is.na(iid_m)==FALSE],
         L_Adum2_rs25531[is.na(iid_m)==FALSE],
         get(outcomes[1])[is.na(iid_m)==FALSE],
         get(outcomes[2])[is.na(iid_m)==FALSE],
         get(outcomes[3])[is.na(iid_m)==FALSE],
         get(outcomes[4])[is.na(iid_m)==FALSE],
         female[is.na(iid_m)==FALSE],
         age[is.na(iid_m)==FALSE],
         birth_decade[is.na(iid_m)==FALSE],
         get(stress_f)[is.na(iid_m)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==5) { temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
         Ldom_5http[is.na(iid_m)==FALSE],
         Lrec_5http[is.na(iid_m)==FALSE],
         Ldum1_5http[is.na(iid_m)==FALSE],
         Ldum2_5http[is.na(iid_m)==FALSE],
         add_rs25531[is.na(iid_m)==FALSE],
         L_Adom_rs25531[is.na(iid_m)==FALSE],
         L_Arec_rs25531[is.na(iid_m)==FALSE],
         L_Adum1_rs25531[is.na(iid_m)==FALSE],
         L_Adum2_rs25531[is.na(iid_m)==FALSE],
         get(outcomes[1])[is.na(iid_m)==FALSE],
         get(outcomes[2])[is.na(iid_m)==FALSE],
         get(outcomes[3])[is.na(iid_m)==FALSE],
         get(outcomes[4])[is.na(iid_m)==FALSE],
         get(outcomes[5])[is.na(iid_m)==FALSE],
         female[is.na(iid_m)==FALSE],
         age[is.na(iid_m)==FALSE],
         birth_decade[is.na(iid_m)==FALSE],
         get(stress_f)[is.na(iid_m)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           outcomes[5],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
    if(noutcomes==6) { temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
         Ldom_5http[is.na(iid_m)==FALSE],
         Lrec_5http[is.na(iid_m)==FALSE],
         Ldum1_5http[is.na(iid_m)==FALSE],
         Ldum2_5http[is.na(iid_m)==FALSE],
         add_rs25531[is.na(iid_m)==FALSE],
         L_Adom_rs25531[is.na(iid_m)==FALSE],
         L_Arec_rs25531[is.na(iid_m)==FALSE],
         L_Adum1_rs25531[is.na(iid_m)==FALSE],
         L_Adum2_rs25531[is.na(iid_m)==FALSE],
         get(outcomes[1])[is.na(iid_m)==FALSE],
         get(outcomes[2])[is.na(iid_m)==FALSE],
         get(outcomes[3])[is.na(iid_m)==FALSE],
         get(outcomes[4])[is.na(iid_m)==FALSE],
         get(outcomes[5])[is.na(iid_m)==FALSE],
         get(outcomes[6])[is.na(iid_m)==FALSE],
         female[is.na(iid_m)==FALSE],
         age[is.na(iid_m)==FALSE],
         birth_decade[is.na(iid_m)==FALSE],
         get(stress_f)[is.na(iid_m)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           outcomes[5],
                           outcomes[6],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f)
                     }
    
      
      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir_mod,SITE,file_label, outcomes[i],"_DS.txt",sep="")

        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:nmodels) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
#                oneSNP.mod(j,snplist[j],outcomes[i],models[k],type,out.file,temp_df)
                oneSNP.modlin(j,snplist[j],outcomes[i],models[k],interact[k],"gaussian",out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models2[k],by.list[k],type,out.file,temp_df)
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      }
      detach(temp_df)
    }
}  




 ModLinR_QDQS.call<-function(outcomes_f, models_f, by.var_f) {
   cat("===== run quantitative depression measure w/ quantitative stress input", "\n" )

   type<- 'gaussian'
   outcomes <- outcomes_f
   noutcomes <- length(outcomes)

   model1.text <- paste( outcomes_f[1],models_f[1], sep=" ")    
   model1 <- as.formula(model1.text)
   vars<-as.character(attr(terms.formula(model1),"variables"))

   stress_f<-vars[length(vars)]    
   stress2_f<-paste(stress_f,"2",sep="")
   stressz_f<-paste(stress_f,"_z",sep="")
   stress2z_f<-paste(stress_f,"2_z",sep="")

################## add dummy code processing here
   inter<-paste(vars[6],stressz_f,sep=":")
   inter2<-paste(vars[6],stress2z_f,sep=":")
   inter.raw<-paste(vars[6],stress_f,sep=":")
   inter2.raw<-paste(vars[6],stress2_f,sep=":")
  
   rm(model1,model1.text)

   models<-if(decade_na_flag()) c(paste("~",vars[3],"+",vars[6],"+",stressz_f,"+",inter,sep=" "),
                                  paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stressz_f,"+",inter,sep=" "),
                                  paste("~",vars[3],"+",vars[6],"+",stress2z_f,"+",inter2,sep=" "),
                                  paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress2z_f,"+",inter2,sep=" ")    
                                  ) else c(paste("~",vars[3],"+",vars[6],"+",stressz_f,"+",inter,sep=" "),
                                           paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stressz_f,"+",inter,sep=" "),    
                                           paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stressz_f,"+",inter,sep=" "),
                                           paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stressz_f,"+",inter,sep=" "),
                                           paste("~",vars[3],"+",vars[6],"+",stress2z_f,"+",inter2,sep=" "),
                                           paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stress2z_f,"+",inter2,sep=" "),    
                                           paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress2z_f,"+",inter2,sep=" "),
                                           paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stress2z_f,"+",inter2,sep=" ")
                                           )
   models2<-if(decade_na_flag()) c(paste("~",vars[3],"+",vars[6],"+",stressz_f,sep=" "),
                                   paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stressz_f,sep=" "),
                                   paste("~",vars[3],"+",vars[6],"+",stress2z_f,sep=" "),
                                   paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress2z_f,sep=" ")    
                                   ) else c(paste("~",vars[3],"+",vars[6],"+",stressz_f,sep=" "),
                                            paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stressz_f,sep=" "),    
                                            paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stressz_f,sep=" "),
                                            paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stressz_f,sep=" "),
                                            paste("~",vars[3],"+",vars[6],"+",stress2z_f,sep=" "),
                                            paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stress2z_f,sep=" "),    
                                            paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress2z_f,sep=" "),
                                            paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stress2z_f,sep=" ")
                                            )
   models.raw<-if(decade_na_flag()) c(paste("~",vars[3],"+",vars[6],"+",stress_f,"+",inter.raw,sep=" "),
                                      paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress_f,"+",inter.raw,sep=" "),
                                      paste("~",vars[3],"+",vars[6],"+",stress2_f,"+",inter2.raw,sep=" "),
                                      paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress2_f,"+",inter2.raw,sep=" ")    
                                      ) else c(paste("~",vars[3],"+",vars[6],"+",stress_f,"+",inter.raw,sep=" "),
                                               paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stress_f,"+",inter.raw,sep=" "),    
                                               paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress_f,"+",inter.raw,sep=" "),
                                               paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stress_f,"+",inter.raw,sep=" "),
                                               paste("~",vars[3],"+",vars[6],"+",stress2_f,"+",inter2.raw,sep=" "),
                                               paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stress2_f,"+",inter2.raw,sep=" "),    
                                               paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress2_f,"+",inter2.raw,sep=" "),
                                               paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stress2_f,"+",inter2.raw,sep=" ")
                                               )  
   models2.raw<-if(decade_na_flag()) c(paste("~",vars[3],"+",vars[6],"+",stress_f,sep=" "),
                                       paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress_f,sep=" "),
                                       paste("~",vars[3],"+",vars[6],"+",stress2_f,sep=" "),
                                       paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress2_f,sep=" ")    
                                       ) else c(paste("~",vars[3],"+",vars[6],"+",stress_f,sep=" "),
                                                paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stress_f,sep=" "),    
                                                paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress_f,sep=" "),
                                                paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stress_f,sep=" "),
                                                paste("~",vars[3],"+",vars[6],"+",stress2_f,sep=" "),
                                                paste("~",vars[3],"+",vars[5],"+",vars[6],"+",stress2_f,sep=" "),    
                                                paste("~",vars[3],"+",vars[4],"+",vars[6],"+",stress2_f,sep=" "),
                                                paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],"+",stress2_f,sep=" ")
                                                )  
   
   by.list <-  c(rep(by.var_f, length(models)))
   nmodels <- length(models)

   if(both_sexes=="YES"){      
     cat("===== combined sex subset", "\n" )      
     fem_levels<-"combined-sex sample"
    if(noutcomes==1) {
        temp_df<-data.frame(add_5http,
                            Ldom_5http,
                            Lrec_5http,
                            Ldum1_5http,
                            Ldum2_5http,
                            add_rs25531,
                            L_Adom_rs25531,
                            L_Arec_rs25531,
                            L_Adum1_rs25531,
                            L_Adum2_rs25531,
                            get(outcomes[1]),
                            female,
                            age,
                            birth_decade,
                            get(stress_f),
                            get(stressz_f),
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
         )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f,
                          stressz_f,
                          stress2_f,
                          stress2z_f)
                     }
    
    if(noutcomes==2) {
        temp_df<-data.frame(add_5http,
                            Ldom_5http,
                            Lrec_5http,
                            Ldum1_5http,
                            Ldum2_5http,
                            add_rs25531,
                            L_Adom_rs25531,
                            L_Arec_rs25531,
                            L_Adum1_rs25531,
                            L_Adum2_rs25531,
                            get(outcomes[1]),
                            get(outcomes[2]),
                            female,
                            age,
                            birth_decade,
                            get(stress_f),
                            get(stressz_f),
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          outcomes[2],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f,
                          stressz_f,
                          stress2_f,
                          stress2z_f)
                     }
    
    if(noutcomes==3) {
        temp_df<-data.frame(add_5http,
                            Ldom_5http,
                            Lrec_5http,
                            Ldum1_5http,
                            Ldum2_5http,
                            add_rs25531,
                            L_Adom_rs25531,
                            L_Arec_rs25531,
                            L_Adum1_rs25531,
                            L_Adum2_rs25531,
                            get(outcomes[1]),
                            get(outcomes[2]),
                            get(outcomes[3]),
                            female,
                            age,
                            birth_decade,
                            get(stress_f),
                            get(stressz_f),
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
         )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f,
                          stressz_f,
                          stress2_f,
                          stress2z_f)
    }
    
     if(noutcomes==4) {
         temp_df<-data.frame(add_5http,
                             Ldom_5http,
                             Lrec_5http,
                             Ldum1_5http,
                             Ldum2_5http,
                             add_rs25531,
                             L_Adom_rs25531,
                             L_Arec_rs25531,
                             L_Adum1_rs25531,
                             L_Adum2_rs25531,
                             get(outcomes[1]),
                             get(outcomes[2]),
                             get(outcomes[3]),
                             get(outcomes[4]),
                             female,
                             age,
                             birth_decade,
                             get(stress_f),
                             get(stressz_f),
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f,
                           stressz_f,
                           stress2_f,
                           stress2z_f)
     }
    
     if(noutcomes==5) {
         temp_df<-data.frame(add_5http,
                             Ldom_5http,
                             Lrec_5http,
                             Ldum1_5http,
                             Ldum2_5http,
                             add_rs25531,
                             L_Adom_rs25531,
                             L_Arec_rs25531,
                             L_Adum1_rs25531,
                             L_Adum2_rs25531,
                             get(outcomes[1]),
                             get(outcomes[2]),
                             get(outcomes[3]),
                             get(outcomes[4]),
                             get(outcomes[5]),
                             female,
                             age,
                             birth_decade,
                             get(stress_f),
                             get(stressz_f),
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                             )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           outcomes[5],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f,
                           stressz_f,
                           stress2_f,
                           stress2z_f)
     }
    
     if(noutcomes==6) {
         temp_df<-data.frame(add_5http,
                             Ldom_5http,
                             Lrec_5http,
                             Ldum1_5http,
                             Ldum2_5http,
                             add_rs25531,
                             L_Adom_rs25531,
                             L_Arec_rs25531,
                             L_Adum1_rs25531,
                             L_Adum2_rs25531,
                             get(outcomes[1]),
                             get(outcomes[2]),
                             get(outcomes[3]),
                             get(outcomes[4]),
                             get(outcomes[5]),
                             get(outcomes[6]),
                             female,
                             age,
                             birth_decade,
                             get(stress_f),
                             get(stressz_f),
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                             )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           outcomes[5],
                           outcomes[6],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f,
                           stressz_f,
                           stress2_f,
                           stress2z_f)
                     }     
     
     attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir_mod,SITE,file_label, outcomes[i],"_QS.txt",sep="")
                
        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:(nmodels/2)) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
#                oneSNP.mod(j,snplist[j],outcomes[i],models[k],type,out.file,temp_df)
                oneSNP.modlin(j,snplist[j],outcomes[i],models[k],inter,"gaussian",out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models2[k],by.list[k],type,out.file,temp_df)
                if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
                  write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(paste(outcomes[i],models2.raw[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)                  
                  write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
#                  oneSNP.mod(j,snplist[j], outcomes[i], models.raw[k],type,out.file,temp_df)
                  oneSNP.modlin(j,snplist[j],outcomes[i],models.raw[k],inter.raw,"gaussian",out.file,temp_df)
                  oneSNP.by(j,snplist[j],outcomes[i],models2.raw[k],by.list[k],type,out.file,temp_df)
                }
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
            for(k in (nmodels/2 + 1):nmodels) {
              if ( (two_level_present_var(get(stress2_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
#                oneSNP.mod(j,snplist[j],outcomes[i],models[k],type,out.file,temp_df)
                oneSNP.modlin(j,snplist[j],outcomes[i],models[k],0,"gaussian",out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models2[k],by.list[k],type,out.file,temp_df)
                if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
                  write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(paste(outcomes[i],models2.raw[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)                  
                  write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
#                  oneSNP.mod(j,snplist[j], outcomes[i], models.raw[k],type,out.file,temp_df)
                oneSNP.modlin(j,snplist[j],outcomes[i],models.raw[k],inter.raw,"gaussian",out.file,temp_df)
                  oneSNP.by(j,snplist[j],outcomes[i],models2.raw[k],by.list[k],type,out.file,temp_df)
                }
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      }
      detach(temp_df)
    }
    
    if (sum(ifelse(is.na(iid_f),1,0))/length(iid_f) < 0.9) {
      cat("===== female only subset", "\n" )
      fem_levels<-"female-only sample"
    if(noutcomes==1) {
        temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                            Ldom_5http[is.na(iid_f)==FALSE],
                            Lrec_5http[is.na(iid_f)==FALSE],
                            Ldum1_5http[is.na(iid_f)==FALSE],
                            Ldum2_5http[is.na(iid_f)==FALSE],
                            add_rs25531[is.na(iid_f)==FALSE],
                            L_Adom_rs25531[is.na(iid_f)==FALSE],
                            L_Arec_rs25531[is.na(iid_f)==FALSE],
                            L_Adum1_rs25531[is.na(iid_f)==FALSE],
                            L_Adum2_rs25531[is.na(iid_f)==FALSE],
                            get(outcomes[1])[is.na(iid_f)==FALSE],
                            female[is.na(iid_f)==FALSE],
                            age[is.na(iid_f)==FALSE],
                            birth_decade[is.na(iid_f)==FALSE],
                            get(stress_f)[is.na(iid_f)==FALSE],
                            get(stressz_f)[is.na(iid_f)==FALSE],
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
         )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f,
                          stressz_f,
                          stress2_f,
                          stress2z_f)
                     }
    
    if(noutcomes==2) {
        temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                            Ldom_5http[is.na(iid_f)==FALSE],
                            Lrec_5http[is.na(iid_f)==FALSE],
                            Ldum1_5http[is.na(iid_f)==FALSE],
                            Ldum2_5http[is.na(iid_f)==FALSE],
                            add_rs25531[is.na(iid_f)==FALSE],
                            L_Adom_rs25531[is.na(iid_f)==FALSE],
                            L_Arec_rs25531[is.na(iid_f)==FALSE],
                            L_Adum1_rs25531[is.na(iid_f)==FALSE],
                            L_Adum2_rs25531[is.na(iid_f)==FALSE],
                            get(outcomes[1])[is.na(iid_f)==FALSE],
                            get(outcomes[2])[is.na(iid_f)==FALSE],
                            female[is.na(iid_f)==FALSE],
                            age[is.na(iid_f)==FALSE],
                            birth_decade[is.na(iid_f)==FALSE],
                            get(stress_f)[is.na(iid_f)==FALSE],
                            get(stressz_f)[is.na(iid_f)==FALSE],
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          outcomes[2],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f,
                          stressz_f,
                          stress2_f,
                          stress2z_f)
                     }
    
    if(noutcomes==3) {
        temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                            Ldom_5http[is.na(iid_f)==FALSE],
                            Lrec_5http[is.na(iid_f)==FALSE],
                            Ldum1_5http[is.na(iid_f)==FALSE],
                            Ldum2_5http[is.na(iid_f)==FALSE],
                            add_rs25531[is.na(iid_f)==FALSE],
                            L_Adom_rs25531[is.na(iid_f)==FALSE],
                            L_Arec_rs25531[is.na(iid_f)==FALSE],
                            L_Adum1_rs25531[is.na(iid_f)==FALSE],
                            L_Adum2_rs25531[is.na(iid_f)==FALSE],
                            get(outcomes[1])[is.na(iid_f)==FALSE],
                            get(outcomes[2])[is.na(iid_f)==FALSE],
                            get(outcomes[3])[is.na(iid_f)==FALSE],
                            female[is.na(iid_f)==FALSE],
                            age[is.na(iid_f)==FALSE],
                            birth_decade[is.na(iid_f)==FALSE],
                            get(stress_f)[is.na(iid_f)==FALSE],
                            get(stressz_f)[is.na(iid_f)==FALSE],
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
         )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f,
                          stressz_f,
                          stress2_f,
                          stress2z_f)
    }
    
     if(noutcomes==4) {
         temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                             Ldom_5http[is.na(iid_f)==FALSE],
                             Lrec_5http[is.na(iid_f)==FALSE],
                             Ldum1_5http[is.na(iid_f)==FALSE],
                             Ldum2_5http[is.na(iid_f)==FALSE],
                             add_rs25531[is.na(iid_f)==FALSE],
                             L_Adom_rs25531[is.na(iid_f)==FALSE],
                             L_Arec_rs25531[is.na(iid_f)==FALSE],
                             L_Adum1_rs25531[is.na(iid_f)==FALSE],
                             L_Adum2_rs25531[is.na(iid_f)==FALSE],
                             get(outcomes[1])[is.na(iid_f)==FALSE],
                             get(outcomes[2])[is.na(iid_f)==FALSE],
                             get(outcomes[3])[is.na(iid_f)==FALSE],
                             get(outcomes[4])[is.na(iid_f)==FALSE],
                             female[is.na(iid_f)==FALSE],
                             age[is.na(iid_f)==FALSE],
                             birth_decade[is.na(iid_f)==FALSE],
                             get(stress_f)[is.na(iid_f)==FALSE],
                             get(stressz_f)[is.na(iid_f)==FALSE],
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f,
                           stressz_f,
                           stress2_f,
                           stress2z_f)
     }
    
     if(noutcomes==5) {
         temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                             Ldom_5http[is.na(iid_f)==FALSE],
                             Lrec_5http[is.na(iid_f)==FALSE],
                             Ldum1_5http[is.na(iid_f)==FALSE],
                             Ldum2_5http[is.na(iid_f)==FALSE],
                             add_rs25531[is.na(iid_f)==FALSE],
                             L_Adom_rs25531[is.na(iid_f)==FALSE],
                             L_Arec_rs25531[is.na(iid_f)==FALSE],
                             L_Adum1_rs25531[is.na(iid_f)==FALSE],
                             L_Adum2_rs25531[is.na(iid_f)==FALSE],
                             get(outcomes[1])[is.na(iid_f)==FALSE],
                             get(outcomes[2])[is.na(iid_f)==FALSE],
                             get(outcomes[3])[is.na(iid_f)==FALSE],
                             get(outcomes[4])[is.na(iid_f)==FALSE],
                             get(outcomes[5])[is.na(iid_f)==FALSE],
                             female[is.na(iid_f)==FALSE],
                             age[is.na(iid_f)==FALSE],
                             birth_decade[is.na(iid_f)==FALSE],
                             get(stress_f)[is.na(iid_f)==FALSE],
                             get(stressz_f)[is.na(iid_f)==FALSE],
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                             )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           outcomes[5],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f,
                           stressz_f,
                           stress2_f,
                           stress2z_f)
     }
    
     if(noutcomes==6) {
         temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                             Ldom_5http[is.na(iid_f)==FALSE],
                             Lrec_5http[is.na(iid_f)==FALSE],
                             Ldum1_5http[is.na(iid_f)==FALSE],
                             Ldum2_5http[is.na(iid_f)==FALSE],
                             add_rs25531[is.na(iid_f)==FALSE],
                             L_Adom_rs25531[is.na(iid_f)==FALSE],
                             L_Arec_rs25531[is.na(iid_f)==FALSE],
                             L_Adum1_rs25531[is.na(iid_f)==FALSE],
                             L_Adum2_rs25531[is.na(iid_f)==FALSE],
                             get(outcomes[1])[is.na(iid_f)==FALSE],
                             get(outcomes[2])[is.na(iid_f)==FALSE],
                             get(outcomes[3])[is.na(iid_f)==FALSE],
                             get(outcomes[4])[is.na(iid_f)==FALSE],
                             get(outcomes[5])[is.na(iid_f)==FALSE],
                             get(outcomes[6])[is.na(iid_f)==FALSE],
                             female[is.na(iid_f)==FALSE],
                             age[is.na(iid_f)==FALSE],
                             birth_decade[is.na(iid_f)==FALSE],
                             get(stress_f)[is.na(iid_f)==FALSE],
                             get(stressz_f)[is.na(iid_f)==FALSE],
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                             )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           outcomes[5],
                           outcomes[6],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f,
                           stressz_f,
                           stress2_f,
                           stress2z_f)
                     }     
         
      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir_mod,SITE,file_label, outcomes[i],"_QS.txt",sep="")
        
        for(j in 1: nsnps){
          if(j%%5){                                     
            for(k in 1:(nmodels/2)) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                #oneSNP.mod(j,snplist[j],outcomes[i],models[k],type,out.file,temp_df)
                oneSNP.modlin(j,snplist[j],outcomes[i],models[k],inter,"gaussian",out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models2[k],by.list[k],type,out.file,temp_df)
                if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
                  write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(paste(outcomes[i],models2.raw[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)                  
                  write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  #oneSNP.mod(j,snplist[j], outcomes[i], models.raw[k],type,out.file,temp_df)
                oneSNP.modlin(j,snplist[j],outcomes[i],models.raw[k],inter.raw,"gaussian",out.file,temp_df)
                  oneSNP.by(j,snplist[j],outcomes[i],models2.raw[k],by.list[k],type,out.file,temp_df)
                }
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
            for(k in (nmodels/2 + 1):nmodels) {
              if ( (two_level_present_var(get(stress2_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                #oneSNP.mod(j,snplist[j],outcomes[i],models[k],type,out.file,temp_df)
                oneSNP.modlin(j,snplist[j],outcomes[i],models[k],inter,"gaussian",out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models2[k],by.list[k],type,out.file,temp_df)
                if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
                  write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(paste(outcomes[i],models2.raw[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)                  
                  write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  #oneSNP.mod(j,snplist[j], outcomes[i], models.raw[k],type,out.file,temp_df)
                  oneSNP.modlin(j,snplist[j],outcomes[i],models.raw[k],inter.raw,"gaussian",out.file,temp_df)
                  oneSNP.by(j,snplist[j],outcomes[i],models2.raw[k],by.list[k],type,out.file,temp_df)
                }
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      }    
      detach(temp_df)
    }

    if (sum(ifelse(is.na(iid_m),1,0))/length(iid_m) < 0.9) {
      cat("===== male only subset", "\n" )      
      fem_levels<-"male-only sample"
    if(noutcomes==1) {
        temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                            Ldom_5http[is.na(iid_m)==FALSE],
                            Lrec_5http[is.na(iid_m)==FALSE],
                            Ldum1_5http[is.na(iid_m)==FALSE],
                            Ldum2_5http[is.na(iid_m)==FALSE],
                            add_rs25531[is.na(iid_m)==FALSE],
                            L_Adom_rs25531[is.na(iid_m)==FALSE],
                            L_Arec_rs25531[is.na(iid_m)==FALSE],
                            L_Adum1_rs25531[is.na(iid_m)==FALSE],
                            L_Adum2_rs25531[is.na(iid_m)==FALSE],
                            get(outcomes[1])[is.na(iid_m)==FALSE],
                            female[is.na(iid_m)==FALSE],
                            age[is.na(iid_m)==FALSE],
                            birth_decade[is.na(iid_m)==FALSE],
                            get(stress_f)[is.na(iid_m)==FALSE],
                            get(stressz_f)[is.na(iid_m)==FALSE],
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
         )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f,
                          stressz_f,
                          stress2_f,
                          stress2z_f)
                     }
    
    if(noutcomes==2) {
        temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                            Ldom_5http[is.na(iid_m)==FALSE],
                            Lrec_5http[is.na(iid_m)==FALSE],
                            Ldum1_5http[is.na(iid_m)==FALSE],
                            Ldum2_5http[is.na(iid_m)==FALSE],
                            add_rs25531[is.na(iid_m)==FALSE],
                            L_Adom_rs25531[is.na(iid_m)==FALSE],
                            L_Arec_rs25531[is.na(iid_m)==FALSE],
                            L_Adum1_rs25531[is.na(iid_m)==FALSE],
                            L_Adum2_rs25531[is.na(iid_m)==FALSE],
                            get(outcomes[1])[is.na(iid_m)==FALSE],
                            get(outcomes[2])[is.na(iid_m)==FALSE],
                            female[is.na(iid_m)==FALSE],
                            age[is.na(iid_m)==FALSE],
                            birth_decade[is.na(iid_m)==FALSE],
                            get(stress_f)[is.na(iid_m)==FALSE],
                            get(stressz_f)[is.na(iid_m)==FALSE],
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          outcomes[2],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f,
                          stressz_f,
                          stress2_f,
                          stress2z_f)
                     }
    
    if(noutcomes==3) {
        temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                            Ldom_5http[is.na(iid_m)==FALSE],
                            Lrec_5http[is.na(iid_m)==FALSE],
                            Ldum1_5http[is.na(iid_m)==FALSE],
                            Ldum2_5http[is.na(iid_m)==FALSE],
                            add_rs25531[is.na(iid_m)==FALSE],
                            L_Adom_rs25531[is.na(iid_m)==FALSE],
                            L_Arec_rs25531[is.na(iid_m)==FALSE],
                            L_Adum1_rs25531[is.na(iid_m)==FALSE],
                            L_Adum2_rs25531[is.na(iid_m)==FALSE],
                            get(outcomes[1])[is.na(iid_m)==FALSE],
                            get(outcomes[2])[is.na(iid_m)==FALSE],
                            get(outcomes[3])[is.na(iid_m)==FALSE],
                            female[is.na(iid_m)==FALSE],
                            age[is.na(iid_m)==FALSE],
                            birth_decade[is.na(iid_m)==FALSE],
                            get(stress_f)[is.na(iid_m)==FALSE],
                            get(stressz_f)[is.na(iid_m)==FALSE],
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                            if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
         )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f,
                          stressz_f,
                          stress2_f,
                          stress2z_f)
    }
    
     if(noutcomes==4) {
         temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                             Ldom_5http[is.na(iid_m)==FALSE],
                             Lrec_5http[is.na(iid_m)==FALSE],
                             Ldum1_5http[is.na(iid_m)==FALSE],
                             Ldum2_5http[is.na(iid_m)==FALSE],
                             add_rs25531[is.na(iid_m)==FALSE],
                             L_Adom_rs25531[is.na(iid_m)==FALSE],
                             L_Arec_rs25531[is.na(iid_m)==FALSE],
                             L_Adum1_rs25531[is.na(iid_m)==FALSE],
                             L_Adum2_rs25531[is.na(iid_m)==FALSE],
                             get(outcomes[1])[is.na(iid_m)==FALSE],
                             get(outcomes[2])[is.na(iid_m)==FALSE],
                             get(outcomes[3])[is.na(iid_m)==FALSE],
                             get(outcomes[4])[is.na(iid_m)==FALSE],
                             female[is.na(iid_m)==FALSE],
                             age[is.na(iid_m)==FALSE],
                             birth_decade[is.na(iid_m)==FALSE],
                             get(stress_f)[is.na(iid_m)==FALSE],
                             get(stressz_f)[is.na(iid_m)==FALSE],
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
         )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f,
                           stressz_f,
                           stress2_f,
                           stress2z_f)
     }
    
     if(noutcomes==5) {
         temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                             Ldom_5http[is.na(iid_m)==FALSE],
                             Lrec_5http[is.na(iid_m)==FALSE],
                             Ldum1_5http[is.na(iid_m)==FALSE],
                             Ldum2_5http[is.na(iid_m)==FALSE],
                             add_rs25531[is.na(iid_m)==FALSE],
                             L_Adom_rs25531[is.na(iid_m)==FALSE],
                             L_Arec_rs25531[is.na(iid_m)==FALSE],
                             L_Adum1_rs25531[is.na(iid_m)==FALSE],
                             L_Adum2_rs25531[is.na(iid_m)==FALSE],
                             get(outcomes[1])[is.na(iid_m)==FALSE],
                             get(outcomes[2])[is.na(iid_m)==FALSE],
                             get(outcomes[3])[is.na(iid_m)==FALSE],
                             get(outcomes[4])[is.na(iid_m)==FALSE],
                             get(outcomes[5])[is.na(iid_m)==FALSE],
                             female[is.na(iid_m)==FALSE],
                             age[is.na(iid_m)==FALSE],
                             birth_decade[is.na(iid_m)==FALSE],
                             get(stress_f)[is.na(iid_m)==FALSE],
                             get(stressz_f)[is.na(iid_m)==FALSE],
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                             )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           outcomes[5],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f,
                           stressz_f,
                           stress2_f,
                           stress2z_f)
     }
    
     if(noutcomes==6) {
         temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                             Ldom_5http[is.na(iid_m)==FALSE],
                             Lrec_5http[is.na(iid_m)==FALSE],
                             Ldum1_5http[is.na(iid_m)==FALSE],
                             Ldum2_5http[is.na(iid_m)==FALSE],
                             add_rs25531[is.na(iid_m)==FALSE],
                             L_Adom_rs25531[is.na(iid_m)==FALSE],
                             L_Arec_rs25531[is.na(iid_m)==FALSE],
                             L_Adum1_rs25531[is.na(iid_m)==FALSE],
                             L_Adum2_rs25531[is.na(iid_m)==FALSE],
                             get(outcomes[1])[is.na(iid_m)==FALSE],
                             get(outcomes[2])[is.na(iid_m)==FALSE],
                             get(outcomes[3])[is.na(iid_m)==FALSE],
                             get(outcomes[4])[is.na(iid_m)==FALSE],
                             get(outcomes[5])[is.na(iid_m)==FALSE],
                             get(outcomes[6])[is.na(iid_m)==FALSE],
                             female[is.na(iid_m)==FALSE],
                             age[is.na(iid_m)==FALSE],
                             birth_decade[is.na(iid_m)==FALSE],
                             get(stress_f)[is.na(iid_m)==FALSE],
                             get(stressz_f)[is.na(iid_m)==FALSE],
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                             if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                             )
         names(temp_df)<-c("add_5http",
			   "Ldom_5http",
                           "Lrec_5http",
                           "Ldum1_5http",
                           "Ldum2_5http",
                           "add_rs25531",
                           "L_Adom_rs25531",
                           "L_Arec_rs25531",
                           "L_Adum1_rs25531",
                           "L_Adum2_rs25531",
                           outcomes[1],
                           outcomes[2],
                           outcomes[3],
                           outcomes[4],
                           outcomes[5],
                           outcomes[6],
                           "female",
                           "age",
                           "birth_decade",
                           stress_f,
                           stressz_f,
                           stress2_f,
                           stress2z_f)
                     }     
         
      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir_mod,SITE,file_label, outcomes[i],"_QS.txt",sep="")
        
        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:(nmodels/2)) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                #oneSNP.mod(j,snplist[j],outcomes[i],models[k],type,out.file,temp_df)
                oneSNP.modlin(j,snplist[j],outcomes[i],models[k],inter,"gaussian",out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models2[k],by.list[k],type,out.file,temp_df)
                if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
                  write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(paste(outcomes[i],models2.raw[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)                  
                  write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  #oneSNP.mod(j,snplist[j], outcomes[i], models.raw[k],type,out.file,temp_df)
                  oneSNP.modlin(j,snplist[j],outcomes[i],models.raw[k],inter.raw,"gaussian",out.file,temp_df)
                  oneSNP.by(j,snplist[j],outcomes[i],models2.raw[k],by.list[k],type,out.file,temp_df)
                }
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
            for(k in (nmodels/2 + 1):nmodels) {
              if ( (two_level_present_var(get(stress2_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
                write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                #oneSNP.mod(j,snplist[j],outcomes[i],models[k],type,out.file,temp_df)
                oneSNP.modlin(j,snplist[j],outcomes[i],models[k],inter,"gaussian",out.file,temp_df)
                oneSNP.by(j,snplist[j],outcomes[i],models2[k],by.list[k],type,out.file,temp_df)
                if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
                  write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  write.table(paste(outcomes[i],models2.raw[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)                  
                  write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                  #oneSNP.mod(j,snplist[j], outcomes[i], models.raw[k],type,out.file,temp_df)
                  oneSNP.modlin(j,snplist[j],outcomes[i],models.raw[k],inter.raw,"gaussian",out.file,temp_df)
                  oneSNP.by(j,snplist[j],outcomes[i],models2.raw[k],by.list[k],type,out.file,temp_df)
                }
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      }    
      detach(temp_df)
    }
 }

LogR_DDcovar.call<-function(outcomes_f,models_f,by.var_f){
  cat("===== run depression dx", "\n" )
 
  outcomes <-outcomes_f
  noutcomes <- length(outcomes)
  type<- 'binomial'

  model1.text <- paste( outcomes_f[1],models_f[1], sep=" ")    
  model1 <- as.formula(model1.text)
  vars<-as.character(attr(terms.formula(model1),"variables"))

  models<-if(decade_na_flag())c(paste("~",vars[3],sep=" "),
                                paste("~",vars[4],sep=" "),
                                paste("~",vars[3],"+",vars[4],sep=" ")
                                ) else c(paste("~",vars[3],sep=" "),
                                         paste("~",vars[5],sep=" "),
                                         paste("~",vars[4],sep=" "),
                                         paste("~",vars[3],"+",vars[5],sep=" "),
                                         paste("~",vars[3],"+",vars[4],sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",vars[5],sep=" ")
                                         )
                               
  nmodels <- length(models)
  var.na<-"dep_dx"
    
  if(both_sexes=="YES"){
    cat("===== combined sex subset", "\n" )      
    fem_levels<-"combined-sex sample"

    if(noutcomes==1) {
    temp_df<-data.frame(get(outcomes[1]),
                        female,
                        age,
                        birth_decade
                        )
    names(temp_df)<-c(outcomes[1],
                      "female",
                      "age",
                      "birth_decade")
}

    if(noutcomes==2) {
    temp_df<-data.frame(get(outcomes[1]),
                        get(outcomes[2]),
                        female,
                        age,
                        birth_decade
                        )
    names(temp_df)<-c(outcomes[1],
                      outcomes[2],
                      "female",
                      "age",
                      "birth_decade")
}

    if(noutcomes==3) {
    temp_df<-data.frame(get(outcomes[1]),
                        get(outcomes[2]),
                        get(outcomes[3]),
                        female,
                        age,
                        birth_decade
                        )
    names(temp_df)<-c(outcomes[1],
                      outcomes[2],
                      outcomes[3],
                      "female",
                      "age",
                      "birth_decade")
}

    attach(temp_df)
      
    for(i in 1: noutcomes) {
      out.file<-paste(outdir,SITE,file_label, outcomes[i],".txt",sep="")
                
      for(j in 1:nsnps){
        for(k in 1:nmodels) {
          if (two_level_present(outcomes[i])=="YES") {	    
            write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSNP(j,snplist[j],outcomes[i],models[k],type,var.na,out.file,temp_df)
          }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      }
    } 
    detach(temp_df)
  }
    
  if (sum(ifelse(is.na(iid_f),1,0))/length(iid_f) < 0.9) {
    cat("===== female only subset", "\n" )
    fem_levels<-"female-only sample"
    if(noutcomes==1) {
    temp_df<-data.frame(get(outcomes[1])[is.na(iid_f)==FALSE],
                        female[is.na(iid_f)==FALSE],
                        age[is.na(iid_f)==FALSE],
                        birth_decade[is.na(iid_f)==FALSE]
                        )
    names(temp_df)<-c(outcomes[1],
                      "female",
                      "age",
                      "birth_decade")
}

    if(noutcomes==2) {
    temp_df<-data.frame(get(outcomes[1])[is.na(iid_f)==FALSE],
                        get(outcomes[2])[is.na(iid_f)==FALSE],
                        female[is.na(iid_f)==FALSE],
                        age[is.na(iid_f)==FALSE],
                        birth_decade[is.na(iid_f)==FALSE]
                        )
    names(temp_df)<-c(outcomes[1],
                      outcomes[2],
                      "female",
                      "age",
                      "birth_decade")
}

    if(noutcomes==3) {
    temp_df<-data.frame(get(outcomes[1])[is.na(iid_f)==FALSE],
                        get(outcomes[2])[is.na(iid_f)==FALSE],
                        get(outcomes[3])[is.na(iid_f)==FALSE],
                        female[is.na(iid_f)==FALSE],
                        age[is.na(iid_f)==FALSE],
                        birth_decade[is.na(iid_f)==FALSE]
                        )
    names(temp_df)<-c(outcomes[1],
                      outcomes[2],
                      outcomes[3],
                      "female",
                      "age",
                      "birth_decade")
}
    attach(temp_df)
    
    for(i in 1: noutcomes) {
      out.file<-paste(outdir,SITE,file_label, outcomes[i],".txt",sep="")

      for(j in 1: nsnps){
        for(k in 1:nmodels) {
          if (two_level_present(outcomes[i])=="YES") {	    
            write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSNP(j,snplist[j],outcomes[i],models[k],type,var.na,out.file,temp_df)
          }else 
            write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      }
    } 
    detach(temp_df)
  }

  if (sum(ifelse(is.na(iid_m),1,0))/length(iid_m) < 0.9) {
    cat("===== male only subset", "\n" )
    fem_levels<-"male-only sample"
    if(noutcomes==1) {
    temp_df<-data.frame(get(outcomes[1])[is.na(iid_m)==FALSE],
                        female[is.na(iid_m)==FALSE],
                        age[is.na(iid_m)==FALSE],
                        birth_decade[is.na(iid_m)==FALSE]
                        )
    names(temp_df)<-c(outcomes[1],
                      "female",
                      "age",
                      "birth_decade")
}

    if(noutcomes==2) {
    temp_df<-data.frame(get(outcomes[1])[is.na(iid_m)==FALSE],
                        get(outcomes[2])[is.na(iid_m)==FALSE],
                        female[is.na(iid_m)==FALSE],
                        age[is.na(iid_m)==FALSE],
                        birth_decade[is.na(iid_m)==FALSE]
                        )
    names(temp_df)<-c(outcomes[1],
                      outcomes[2],
                      "female",
                      "age",
                      "birth_decade")
}

    if(noutcomes==3) {
    temp_df<-data.frame(get(outcomes[1])[is.na(iid_m)==FALSE],
                        get(outcomes[2])[is.na(iid_m)==FALSE],
                        get(outcomes[3])[is.na(iid_m)==FALSE],
                        female[is.na(iid_m)==FALSE],
                        age[is.na(iid_m)==FALSE],
                        birth_decade[is.na(iid_m)==FALSE]
                        )
    names(temp_df)<-c(outcomes[1],
                      outcomes[2],
                      outcomes[3],
                      "female",
                      "age",
                      "birth_decade")
}
    attach(temp_df)

    for(i in 1: noutcomes) {
      out.file<-paste(outdir,SITE,file_label, outcomes[i],".txt",sep="")

      for(j in 1: nsnps){
        for(k in 1:nmodels) {
          if (two_level_present(outcomes[i])=="YES") {	    
            write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSNP(j,snplist[j],outcomes[i],models[k],type,var.na,out.file,temp_df)
          }else 
            write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
          }
      }
    } 
    detach(temp_df)
  }
}  

  
 # linear regression models #
 ############################                  
LinR_QDcovar.call<-function(outcomes_f,models_f,by.var_f){
  cat("===== run quantitative depression measure", "\n" )

  outcomes <-outcomes_f
  noutcomes <- length(outcomes)
  type<- 'gaussian'

  model1.text <- paste( outcomes_f[1],models_f[1], sep=" ")    
  model1 <- as.formula(model1.text)
  vars<-as.character(attr(terms.formula(model1),"variables"))

  models<-if(decade_na_flag())c(paste("~",vars[3],sep=" "),
                                paste("~",vars[4],sep=" "),
                                paste("~",vars[3],"+",vars[4],sep=" ")
                                ) else c(paste("~",vars[3],sep=" "),
                                         paste("~",vars[5],sep=" "),
                                         paste("~",vars[4],sep=" "),
                                         paste("~",vars[3],"+",vars[5],sep=" "),
                                         paste("~",vars[3],"+",vars[4],sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",vars[5],sep=" ")
                                         )
 
  nmodels <- length(models)
  var.na<-"dep_q"
    
  if(both_sexes=="YES"){
    cat("===== combined sex subset", "\n" )      
    fem_levels<-"combined-sex sample"
    if(noutcomes==1){
      temp_df<-data.frame(get(outcomes[1]),
                          female,
                          age,
                          birth_decade
                          )
      names(temp_df)<-c(outcomes[1],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==2){
      temp_df<-data.frame(get(outcomes[1]),
                          get(outcomes[2]),
                          female,
                          age,
                          birth_decade
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==3){
      temp_df<-data.frame(get(outcomes[1]),
                          get(outcomes[2]),
                          get(outcomes[3]),
                          female,
                          age,
                          birth_decade
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==4){
      temp_df<-data.frame(get(outcomes[1]),
                          get(outcomes[2]),
                          get(outcomes[3]),
                          get(outcomes[4]),
                          female,
                          age,
                          birth_decade
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==5){
      temp_df<-data.frame(get(outcomes[1]),
                          get(outcomes[2]),
                          get(outcomes[3]),
                          get(outcomes[4]),
                          get(outcomes[5]),
                          female,
                          age,
                          birth_decade
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        outcomes[5],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==6){
      temp_df<-data.frame(get(outcomes[1]),
                          get(outcomes[2]),
                          get(outcomes[3]),
                          get(outcomes[4]),
                          get(outcomes[5]),
                          get(outcomes[6]),
                          female,
                          age,
                          birth_decade
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        outcomes[5],
                        outcomes[6],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    attach(temp_df)
      
      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],".txt",sep="")
                
        for(j in 1: nsnps){
          for(k in 1:nmodels) {
            if (two_level_present(outcomes[i])=="YES") {	    
              write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSNP(j,snplist[j],outcomes[i],models[k],type,var.na,out.file,temp_df)
	    }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
          }
        }
      }
      detach(temp_df)
    }
    
    if (sum(ifelse(is.na(iid_f),1,0))/length(iid_f) < 0.9) {
      cat("===== female only subset", "\n" )
      fem_levels<-"female-only sample"
    if(noutcomes==1){
      temp_df<-data.frame(get(outcomes[1])[is.na(iid_f)==FALSE],
                          female[is.na(iid_f)==FALSE],
                          age[is.na(iid_f)==FALSE],
                          birth_decade[is.na(iid_f)==FALSE]
                          )
      names(temp_df)<-c(outcomes[1],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==2){
      temp_df<-data.frame(get(outcomes[1])[is.na(iid_f)==FALSE],
                          get(outcomes[2])[is.na(iid_f)==FALSE],
                          female[is.na(iid_f)==FALSE],
                          age[is.na(iid_f)==FALSE],
                          birth_decade[is.na(iid_f)==FALSE]
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==3){
      temp_df<-data.frame(get(outcomes[1])[is.na(iid_f)==FALSE],
                          get(outcomes[2])[is.na(iid_f)==FALSE],
                          get(outcomes[3])[is.na(iid_f)==FALSE],
                          female[is.na(iid_f)==FALSE],
                          age[is.na(iid_f)==FALSE],
                          birth_decade[is.na(iid_f)==FALSE]
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==4){
      temp_df<-data.frame(get(outcomes[1])[is.na(iid_f)==FALSE],
                          get(outcomes[2])[is.na(iid_f)==FALSE],
                          get(outcomes[3])[is.na(iid_f)==FALSE],
                          get(outcomes[4])[is.na(iid_f)==FALSE],
                          female[is.na(iid_f)==FALSE],
                          age[is.na(iid_f)==FALSE],
                          birth_decade[is.na(iid_f)==FALSE]
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==5){
      temp_df<-data.frame(get(outcomes[1])[is.na(iid_f)==FALSE],
                          get(outcomes[2])[is.na(iid_f)==FALSE],
                          get(outcomes[3])[is.na(iid_f)==FALSE],
                          get(outcomes[4])[is.na(iid_f)==FALSE],
                          get(outcomes[5])[is.na(iid_f)==FALSE],
                          female[is.na(iid_f)==FALSE],
                          age[is.na(iid_f)==FALSE],
                          birth_decade[is.na(iid_f)==FALSE]
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        outcomes[5],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==6){
      temp_df<-data.frame(get(outcomes[1])[is.na(iid_f)==FALSE],
                          get(outcomes[2])[is.na(iid_f)==FALSE],
                          get(outcomes[3])[is.na(iid_f)==FALSE],
                          get(outcomes[4])[is.na(iid_f)==FALSE],
                          get(outcomes[5])[is.na(iid_f)==FALSE],
                          get(outcomes[6])[is.na(iid_f)==FALSE],
                          female[is.na(iid_f)==FALSE],
                          age[is.na(iid_f)==FALSE],
                          birth_decade[is.na(iid_f)==FALSE]
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        outcomes[5],
                        outcomes[6],
                        "female",
                        "age",
                        "birth_decade")
    }

      attach(temp_df)
      
      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],".txt",sep="")

        for(j in 1: nsnps){
          for(k in 1:nmodels) {
            if (two_level_present(outcomes[i])=="YES") {	    
              write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSNP(j,snplist[j],outcomes[i],models[k],type,var.na,out.file,temp_df)
	    }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
          }
        }
      }
      detach(temp_df)
    }

    if (sum(ifelse(is.na(iid_m),1,0))/length(iid_m) < 0.9) {
      cat("===== male only subset", "\n" )
      fem_levels<-"male-only sample"
    if(noutcomes==1){
      temp_df<-data.frame(get(outcomes[1])[is.na(iid_m)==FALSE],
                          female[is.na(iid_m)==FALSE],
                          age[is.na(iid_m)==FALSE],
                          birth_decade[is.na(iid_m)==FALSE]
                          )
      names(temp_df)<-c(outcomes[1],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==2){
      temp_df<-data.frame(get(outcomes[1])[is.na(iid_m)==FALSE],
                          get(outcomes[2])[is.na(iid_m)==FALSE],
                          female[is.na(iid_m)==FALSE],
                          age[is.na(iid_m)==FALSE],
                          birth_decade[is.na(iid_m)==FALSE]
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==3){
      temp_df<-data.frame(get(outcomes[1])[is.na(iid_m)==FALSE],
                          get(outcomes[2])[is.na(iid_m)==FALSE],
                          get(outcomes[3])[is.na(iid_m)==FALSE],
                          female[is.na(iid_m)==FALSE],
                          age[is.na(iid_m)==FALSE],
                          birth_decade[is.na(iid_m)==FALSE]
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==4){
      temp_df<-data.frame(get(outcomes[1])[is.na(iid_m)==FALSE],
                          get(outcomes[2])[is.na(iid_m)==FALSE],
                          get(outcomes[3])[is.na(iid_m)==FALSE],
                          get(outcomes[4])[is.na(iid_m)==FALSE],
                          female[is.na(iid_m)==FALSE],
                          age[is.na(iid_m)==FALSE],
                          birth_decade[is.na(iid_m)==FALSE]
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==5){
      temp_df<-data.frame(get(outcomes[1])[is.na(iid_m)==FALSE],
                          get(outcomes[2])[is.na(iid_m)==FALSE],
                          get(outcomes[3])[is.na(iid_m)==FALSE],
                          get(outcomes[4])[is.na(iid_m)==FALSE],
                          get(outcomes[5])[is.na(iid_m)==FALSE],
                          female[is.na(iid_m)==FALSE],
                          age[is.na(iid_m)==FALSE],
                          birth_decade[is.na(iid_m)==FALSE]
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        outcomes[5],
                        "female",
                        "age",
                        "birth_decade")
    }
    
    if(noutcomes==6){
      temp_df<-data.frame(get(outcomes[1])[is.na(iid_m)==FALSE],
                          get(outcomes[2])[is.na(iid_m)==FALSE],
                          get(outcomes[3])[is.na(iid_m)==FALSE],
                          get(outcomes[4])[is.na(iid_m)==FALSE],
                          get(outcomes[5])[is.na(iid_m)==FALSE],
                          get(outcomes[6])[is.na(iid_m)==FALSE],
                          female[is.na(iid_m)==FALSE],
                          age[is.na(iid_m)==FALSE],
                          birth_decade[is.na(iid_m)==FALSE]
                          )
      names(temp_df)<-c(outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        outcomes[5],
                        outcomes[6],
                        "female",
                        "age",
                        "birth_decade")
    }

      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],".txt",sep="")

        for(j in 1: nsnps){
          for(k in 1:nmodels) {
            if (two_level_present(outcomes[i])=="YES") {	    
              write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSNP(j,snplist[j],outcomes[i],models[k],type,var.na,out.file,temp_df)
	    }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
          }
        }
      }
      detach(temp_df)
    }
} 



LogR_DD_SNP.call<-function(outcomes_f,models_f,by.var_f){
  cat("===== run depression dx", "\n" )
 
  outcomes <-outcomes_f
  noutcomes <- length(outcomes)
  type<- 'binomial'

  model1.text <- paste( outcomes_f[1],models_f[1], sep=" ")    
  model1 <- as.formula(model1.text)
  vars<-as.character(attr(terms.formula(model1),"variables"))

  models<-if(decade_na_flag())c(paste("~",vars[3],"+",vars[6],sep=" "),
                                paste("~",vars[3],"+",vars[4],"+",vars[6],sep=" ")
                                ) else c(paste("~",vars[3],"+",vars[6],sep=" "),
                                         paste("~",vars[3],"+",vars[5],"+",vars[6],sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",vars[6],sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],sep=" ")
                                         )
                               
  nmodels <- length(models)
  var.na<-"dep_dx"
    
  if(both_sexes=="YES"){
    cat("===== combined sex subset", "\n" )      
    fem_levels<-"combined-sex sample"

    if(noutcomes==1) {
        temp_df<-data.frame(add_5http,
                            Ldom_5http,
                            Lrec_5http,
                            Ldum1_5http,
                            Ldum2_5http,
                            add_rs25531,
                            L_Adom_rs25531,
                            L_Arec_rs25531,
                            L_Adum1_rs25531,
                            L_Adum2_rs25531,
                            get(outcomes[1]),
                            female,
                            age,
                            birth_decade
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          "female",
                          "age",
                          "birth_decade")
    }
    
    if(noutcomes==2) {
        temp_df<-data.frame(add_5http,
                            Ldom_5http,
                            Lrec_5http,
                            Ldum1_5http,
                            Ldum2_5http,
                            add_rs25531,
                            L_Adom_rs25531,
                            L_Arec_rs25531,
                            L_Adum1_rs25531,
                            L_Adum2_rs25531,
                            get(outcomes[1]),
                            get(outcomes[2]),
                            female,
                            age,
                            birth_decade
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          outcomes[2],
                          "female",
                          "age",
                          "birth_decade")
    }
    
    if(noutcomes==3) {
        temp_df<-data.frame(add_5http,
                            Ldom_5http,
                            Lrec_5http,
                            Ldum1_5http,
                            Ldum2_5http,
                            add_rs25531,
                            L_Adom_rs25531,
                            L_Arec_rs25531,
                            L_Adum1_rs25531,
                            L_Adum2_rs25531,
                            get(outcomes[1]),
                            get(outcomes[2]),
                            get(outcomes[3]),
                            female,
                            age,
                            birth_decade
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          "female",
                          "age",
                          "birth_decade")
    }
    
    attach(temp_df)
      
      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],".txt",sep="")
                
        for(j in 1:nsnps){
          if(j%%5){
            for(k in 1:nmodels) {
              if (two_level_present(outcomes[i])=="YES") {	    
                write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP(j,snplist[j],outcomes[i],models[k],type,var.na,out.file,temp_df)
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      } 
      detach(temp_df)
    }
    
    if (sum(ifelse(is.na(iid_f),1,0))/length(iid_f) < 0.9) {
      cat("===== female only subset", "\n" )
      fem_levels<-"female-only sample"
    if(noutcomes==1) {
        temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                            Ldom_5http[is.na(iid_f)==FALSE],
                            Lrec_5http[is.na(iid_f)==FALSE],
                            Ldum1_5http[is.na(iid_f)==FALSE],
                            Ldum2_5http[is.na(iid_f)==FALSE],
                            add_rs25531[is.na(iid_f)==FALSE],
                            L_Adom_rs25531[is.na(iid_f)==FALSE],
                            L_Arec_rs25531[is.na(iid_f)==FALSE],
                            L_Adum1_rs25531[is.na(iid_f)==FALSE],
                            L_Adum2_rs25531[is.na(iid_f)==FALSE],
                            get(outcomes[1])[is.na(iid_f)==FALSE],
                            female[is.na(iid_f)==FALSE],
                            age[is.na(iid_f)==FALSE],
                            birth_decade[is.na(iid_f)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          "female",
                          "age",
                          "birth_decade")
    }
    
    if(noutcomes==2) {
        temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                            Ldom_5http[is.na(iid_f)==FALSE],
                            Lrec_5http[is.na(iid_f)==FALSE],
                            Ldum1_5http[is.na(iid_f)==FALSE],
                            Ldum2_5http[is.na(iid_f)==FALSE],
                            add_rs25531[is.na(iid_f)==FALSE],
                            L_Adom_rs25531[is.na(iid_f)==FALSE],
                            L_Arec_rs25531[is.na(iid_f)==FALSE],
                            L_Adum1_rs25531[is.na(iid_f)==FALSE],
                            L_Adum2_rs25531[is.na(iid_f)==FALSE],
                            get(outcomes[1])[is.na(iid_f)==FALSE],
                            get(outcomes[2])[is.na(iid_f)==FALSE],
                            female[is.na(iid_f)==FALSE],
                            age[is.na(iid_f)==FALSE],
                            birth_decade[is.na(iid_f)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          outcomes[2],
                          "female",
                          "age",
                          "birth_decade")
    }
    
    if(noutcomes==3) {
        temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                            Ldom_5http[is.na(iid_f)==FALSE],
                            Lrec_5http[is.na(iid_f)==FALSE],
                            Ldum1_5http[is.na(iid_f)==FALSE],
                            Ldum2_5http[is.na(iid_f)==FALSE],
                            add_rs25531[is.na(iid_f)==FALSE],
                            L_Adom_rs25531[is.na(iid_f)==FALSE],
                            L_Arec_rs25531[is.na(iid_f)==FALSE],
                            L_Adum1_rs25531[is.na(iid_f)==FALSE],
                            L_Adum2_rs25531[is.na(iid_f)==FALSE],
                            get(outcomes[1])[is.na(iid_f)==FALSE],
                            get(outcomes[2])[is.na(iid_f)==FALSE],
                            get(outcomes[3])[is.na(iid_f)==FALSE],
                            female[is.na(iid_f)==FALSE],
                            age[is.na(iid_f)==FALSE],
                            birth_decade[is.na(iid_f)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          "female",
                          "age",
                          "birth_decade")
    }
    

      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],".txt",sep="")

        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:nmodels) {
              if (two_level_present(outcomes[i])=="YES") {	    
                write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP(j,snplist[j],outcomes[i],models[k],type,var.na,out.file,temp_df)
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      } 
      detach(temp_df)
    }

    if (sum(ifelse(is.na(iid_m),1,0))/length(iid_m) < 0.9) {
      cat("===== male only subset", "\n" )
      fem_levels<-"male-only sample"
    if(noutcomes==1) {
        temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                            Ldom_5http[is.na(iid_m)==FALSE],
                            Lrec_5http[is.na(iid_m)==FALSE],
                            Ldum1_5http[is.na(iid_m)==FALSE],
                            Ldum2_5http[is.na(iid_m)==FALSE],
                            add_rs25531[is.na(iid_m)==FALSE],
                            L_Adom_rs25531[is.na(iid_m)==FALSE],
                            L_Arec_rs25531[is.na(iid_m)==FALSE],
                            L_Adum1_rs25531[is.na(iid_m)==FALSE],
                            L_Adum2_rs25531[is.na(iid_m)==FALSE],
                            get(outcomes[1])[is.na(iid_m)==FALSE],
                            female[is.na(iid_m)==FALSE],
                            age[is.na(iid_m)==FALSE],
                            birth_decade[is.na(iid_m)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          "female",
                          "age",
                          "birth_decade")
    }
    
    if(noutcomes==2) {
        temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                            Ldom_5http[is.na(iid_m)==FALSE],
                            Lrec_5http[is.na(iid_m)==FALSE],
                            Ldum1_5http[is.na(iid_m)==FALSE],
                            Ldum2_5http[is.na(iid_m)==FALSE],
                            add_rs25531[is.na(iid_m)==FALSE],
                            L_Adom_rs25531[is.na(iid_m)==FALSE],
                            L_Arec_rs25531[is.na(iid_m)==FALSE],
                            L_Adum1_rs25531[is.na(iid_m)==FALSE],
                            L_Adum2_rs25531[is.na(iid_m)==FALSE],
                            get(outcomes[1])[is.na(iid_m)==FALSE],
                            get(outcomes[2])[is.na(iid_m)==FALSE],
                            female[is.na(iid_m)==FALSE],
                            age[is.na(iid_m)==FALSE],
                            birth_decade[is.na(iid_m)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          outcomes[2],
                          "female",
                          "age",
                          "birth_decade")
    }
    
    if(noutcomes==3) {
        temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                            Ldom_5http[is.na(iid_m)==FALSE],
                            Lrec_5http[is.na(iid_m)==FALSE],
                            Ldum1_5http[is.na(iid_m)==FALSE],
                            Ldum2_5http[is.na(iid_m)==FALSE],
                            add_rs25531[is.na(iid_m)==FALSE],
                            L_Adom_rs25531[is.na(iid_m)==FALSE],
                            L_Arec_rs25531[is.na(iid_m)==FALSE],
                            L_Adum1_rs25531[is.na(iid_m)==FALSE],
                            L_Adum2_rs25531[is.na(iid_m)==FALSE],
                            get(outcomes[1])[is.na(iid_m)==FALSE],
                            get(outcomes[2])[is.na(iid_m)==FALSE],
                            get(outcomes[3])[is.na(iid_m)==FALSE],
                            female[is.na(iid_m)==FALSE],
                            age[is.na(iid_m)==FALSE],
                            birth_decade[is.na(iid_m)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "Ldum1_5http",
                          "Ldum2_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          "L_Adum1_rs25531",
                          "L_Adum2_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          "female",
                          "age",
                          "birth_decade")
    }
    
      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],".txt",sep="")

        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:nmodels) {
              if (two_level_present(outcomes[i])=="YES") {	    
                write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP(j,snplist[j],outcomes[i],models[k],type,var.na,out.file,temp_df)
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      } 
      detach(temp_df)
    }
}  

LinR_QD_SNP.call<-function(outcomes_f,models_f,by.var_f){
  cat("===== run quantitative depression measure", "\n" )

  outcomes <-outcomes_f
  noutcomes <- length(outcomes)
  type<- 'gaussian'

  model1.text <- paste( outcomes_f[1],models_f[1], sep=" ")    
  model1 <- as.formula(model1.text)
  vars<-as.character(attr(terms.formula(model1),"variables"))

  models<-if(decade_na_flag())c(paste("~",vars[3],"+",vars[6],sep=" "),
                                paste("~",vars[3],"+",vars[4],"+",vars[6],sep=" ")
                                ) else c(paste("~",vars[3],"+",vars[6],sep=" "),
                                         paste("~",vars[3],"+",vars[5],"+",vars[6],sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",vars[6],sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",vars[5],"+",vars[6],sep=" ")
                                         )

  nmodels <- length(models)
  var.na<-"dep_q"
    
  if(both_sexes=="YES"){
    cat("===== combined sex subset", "\n" )      
    fem_levels<-"combined-sex sample"

    if(noutcomes==1){
      temp_df<-data.frame(add_5http,
                          Ldom_5http,
                          Lrec_5http,
                          Ldum1_5http,
                          Ldum2_5http,
                          add_rs25531,
                          L_Adom_rs25531,
                          L_Arec_rs25531,
                          L_Adum1_rs25531,
                          L_Adum2_rs25531,
                          get(outcomes[1]),
                          female,
                          age,
                          birth_decade
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==2){
      temp_df<-data.frame(add_5http,
                          Ldom_5http,
                          Lrec_5http,
                          Ldum1_5http,
                          Ldum2_5http,
                          add_rs25531,
                          L_Adom_rs25531,
                          L_Arec_rs25531,
                          L_Adum1_rs25531,
                          L_Adum2_rs25531,
                          get(outcomes[1]),
                          get(outcomes[2]),
                          female,
                          age,
                          birth_decade
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==3){
      temp_df<-data.frame(add_5http,
                          Ldom_5http,
                          Lrec_5http,
                          Ldum1_5http,
                          Ldum2_5http,
                          add_rs25531,
                          L_Adom_rs25531,
                          L_Arec_rs25531,
                          L_Adum1_rs25531,
                          L_Adum2_rs25531,
                          get(outcomes[1]),
                          get(outcomes[2]),
                          get(outcomes[3]),
                          female,
                          age,
                          birth_decade
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==4){
      temp_df<-data.frame(add_5http,
                          Ldom_5http,
                          Lrec_5http,
                          Ldum1_5http,
                          Ldum2_5http,
                          add_rs25531,
                          L_Adom_rs25531,
                          L_Arec_rs25531,
                          L_Adum1_rs25531,
                          L_Adum2_rs25531,
                          get(outcomes[1]),
                          get(outcomes[2]),
                          get(outcomes[3]),
                          get(outcomes[4]),
                          female,
                          age,
                          birth_decade
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==5){
      temp_df<-data.frame(add_5http,
                          Ldom_5http,
                          Lrec_5http,
                          Ldum1_5http,
                          Ldum2_5http,
                          add_rs25531,
                          L_Adom_rs25531,
                          L_Arec_rs25531,
                          L_Adum1_rs25531,
                          L_Adum2_rs25531,
                          get(outcomes[1]),
                          get(outcomes[2]),
                          get(outcomes[3]),
                          get(outcomes[4]),
                          get(outcomes[5]),
                          female,
                          age,
                          birth_decade
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        outcomes[5],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==6){
      temp_df<-data.frame(add_5http,
                          Ldom_5http,
                          Lrec_5http,
                          Ldum1_5http,
                          Ldum2_5http,
                          add_rs25531,
                          L_Adom_rs25531,
                          L_Arec_rs25531,
                          L_Adum1_rs25531,
                          L_Adum2_rs25531,
                          get(outcomes[1]),
                          get(outcomes[2]),
                          get(outcomes[3]),
                          get(outcomes[4]),
                          get(outcomes[5]),
                          get(outcomes[6]),
                          female,
                          age,
                          birth_decade
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        outcomes[5],
                        outcomes[6],
                        "female",
                        "age",
			"birth_decade")
    }
    
    attach(temp_df)
      
      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],".txt",sep="")
                
        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:nmodels) {
              if (two_level_present(outcomes[i])=="YES") {	    
                write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP(j,snplist[j],outcomes[i],models[k],type,var.na,out.file,temp_df)
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      }
      detach(temp_df)
    }
    
    if (sum(ifelse(is.na(iid_f),1,0))/length(iid_f) < 0.9) {
      cat("===== female only subset", "\n" )
      fem_levels<-"female-only sample"
    if(noutcomes==1){
      temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                          Ldom_5http[is.na(iid_f)==FALSE],
                          Lrec_5http[is.na(iid_f)==FALSE],
                          Ldum1_5http[is.na(iid_f)==FALSE],
                          Ldum2_5http[is.na(iid_f)==FALSE],
                          add_rs25531[is.na(iid_f)==FALSE],
                          L_Adom_rs25531[is.na(iid_f)==FALSE],
                          L_Arec_rs25531[is.na(iid_f)==FALSE],
                          L_Adum1_rs25531[is.na(iid_f)==FALSE],
                          L_Adum2_rs25531[is.na(iid_f)==FALSE],
                          get(outcomes[1])[is.na(iid_f)==FALSE],
                          female[is.na(iid_f)==FALSE],
                          age[is.na(iid_f)==FALSE],
                          birth_decade[is.na(iid_f)==FALSE]
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==2){
      temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                          Ldom_5http[is.na(iid_f)==FALSE],
                          Lrec_5http[is.na(iid_f)==FALSE],
                          Ldum1_5http[is.na(iid_f)==FALSE],
                          Ldum2_5http[is.na(iid_f)==FALSE],
                          add_rs25531[is.na(iid_f)==FALSE],
                          L_Adom_rs25531[is.na(iid_f)==FALSE],
                          L_Arec_rs25531[is.na(iid_f)==FALSE],
                          L_Adum1_rs25531[is.na(iid_f)==FALSE],
                          L_Adum2_rs25531[is.na(iid_f)==FALSE],
                          get(outcomes[1])[is.na(iid_f)==FALSE],
                          get(outcomes[2])[is.na(iid_f)==FALSE],
                          female[is.na(iid_f)==FALSE],
                          age[is.na(iid_f)==FALSE],
                          birth_decade[is.na(iid_f)==FALSE]
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==3){
      temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                          Ldom_5http[is.na(iid_f)==FALSE],
                          Lrec_5http[is.na(iid_f)==FALSE],
                          Ldum1_5http[is.na(iid_f)==FALSE],
                          Ldum2_5http[is.na(iid_f)==FALSE],
                          add_rs25531[is.na(iid_f)==FALSE],
                          L_Adom_rs25531[is.na(iid_f)==FALSE],
                          L_Arec_rs25531[is.na(iid_f)==FALSE],
                          L_Adum1_rs25531[is.na(iid_f)==FALSE],
                          L_Adum2_rs25531[is.na(iid_f)==FALSE],
                          get(outcomes[1])[is.na(iid_f)==FALSE],
                          get(outcomes[2])[is.na(iid_f)==FALSE],
                          get(outcomes[3])[is.na(iid_f)==FALSE],
                          female[is.na(iid_f)==FALSE],
                          age[is.na(iid_f)==FALSE],
                          birth_decade[is.na(iid_f)==FALSE]
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==4){
      temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                          Ldom_5http[is.na(iid_f)==FALSE],
                          Lrec_5http[is.na(iid_f)==FALSE],
                          Ldum1_5http[is.na(iid_f)==FALSE],
                          Ldum2_5http[is.na(iid_f)==FALSE],
                          add_rs25531[is.na(iid_f)==FALSE],
                          L_Adom_rs25531[is.na(iid_f)==FALSE],
                          L_Arec_rs25531[is.na(iid_f)==FALSE],
                          L_Adum1_rs25531[is.na(iid_f)==FALSE],
                          L_Adum2_rs25531[is.na(iid_f)==FALSE],
                          get(outcomes[1])[is.na(iid_f)==FALSE],
                          get(outcomes[2])[is.na(iid_f)==FALSE],
                          get(outcomes[3])[is.na(iid_f)==FALSE],
                          get(outcomes[4])[is.na(iid_f)==FALSE],
                          female[is.na(iid_f)==FALSE],
                          age[is.na(iid_f)==FALSE],
                          birth_decade[is.na(iid_f)==FALSE]
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==5){
      temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                          Ldom_5http[is.na(iid_f)==FALSE],
                          Lrec_5http[is.na(iid_f)==FALSE],
                          Ldum1_5http[is.na(iid_f)==FALSE],
                          Ldum2_5http[is.na(iid_f)==FALSE],
                          add_rs25531[is.na(iid_f)==FALSE],
                          L_Adom_rs25531[is.na(iid_f)==FALSE],
                          L_Arec_rs25531[is.na(iid_f)==FALSE],
                          L_Adum1_rs25531[is.na(iid_f)==FALSE],
                          L_Adum2_rs25531[is.na(iid_f)==FALSE],
                          get(outcomes[1])[is.na(iid_f)==FALSE],
                          get(outcomes[2])[is.na(iid_f)==FALSE],
                          get(outcomes[3])[is.na(iid_f)==FALSE],
                          get(outcomes[4])[is.na(iid_f)==FALSE],
                          get(outcomes[5])[is.na(iid_f)==FALSE],
                          female[is.na(iid_f)==FALSE],
                          age[is.na(iid_f)==FALSE],
                          birth_decade[is.na(iid_f)==FALSE]
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        outcomes[5],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==6){
      temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                          Ldom_5http[is.na(iid_f)==FALSE],
                          Lrec_5http[is.na(iid_f)==FALSE],
                          Ldum1_5http[is.na(iid_f)==FALSE],
                          Ldum2_5http[is.na(iid_f)==FALSE],
                          add_rs25531[is.na(iid_f)==FALSE],
                          L_Adom_rs25531[is.na(iid_f)==FALSE],
                          L_Arec_rs25531[is.na(iid_f)==FALSE],
                          L_Adum1_rs25531[is.na(iid_f)==FALSE],
                          L_Adum2_rs25531[is.na(iid_f)==FALSE],
                          get(outcomes[1])[is.na(iid_f)==FALSE],
                          get(outcomes[2])[is.na(iid_f)==FALSE],
                          get(outcomes[3])[is.na(iid_f)==FALSE],
                          get(outcomes[4])[is.na(iid_f)==FALSE],
                          get(outcomes[5])[is.na(iid_f)==FALSE],
                          get(outcomes[6])[is.na(iid_f)==FALSE],
                          female[is.na(iid_f)==FALSE],
                          age[is.na(iid_f)==FALSE],
                          birth_decade[is.na(iid_f)==FALSE]
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        outcomes[5],
                        outcomes[6],
                        "female",
                        "age",
			"birth_decade")
    }
    
      attach(temp_df)
      
      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],".txt",sep="")

        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:nmodels) {
              if (two_level_present(outcomes[i])=="YES") {	    
                write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP(j,snplist[j],outcomes[i],models[k],type,var.na,out.file,temp_df)
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      }
      detach(temp_df)
    }

    if (sum(ifelse(is.na(iid_m),1,0))/length(iid_m) < 0.9) {
      cat("===== male only subset", "\n" )
      fem_levels<-"male-only sample"
    if(noutcomes==1){
      temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                          Ldom_5http[is.na(iid_m)==FALSE],
                          Lrec_5http[is.na(iid_m)==FALSE],
                          Ldum1_5http[is.na(iid_m)==FALSE],
                          Ldum2_5http[is.na(iid_m)==FALSE],
                          add_rs25531[is.na(iid_m)==FALSE],
                          L_Adom_rs25531[is.na(iid_m)==FALSE],
                          L_Arec_rs25531[is.na(iid_m)==FALSE],
                          L_Adum1_rs25531[is.na(iid_m)==FALSE],
                          L_Adum2_rs25531[is.na(iid_m)==FALSE],
                          get(outcomes[1])[is.na(iid_m)==FALSE],
                          female[is.na(iid_m)==FALSE],
                          age[is.na(iid_m)==FALSE],
                          birth_decade[is.na(iid_m)==FALSE]
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==2){
      temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                          Ldom_5http[is.na(iid_m)==FALSE],
                          Lrec_5http[is.na(iid_m)==FALSE],
                          Ldum1_5http[is.na(iid_m)==FALSE],
                          Ldum2_5http[is.na(iid_m)==FALSE],
                          add_rs25531[is.na(iid_m)==FALSE],
                          L_Adom_rs25531[is.na(iid_m)==FALSE],
                          L_Arec_rs25531[is.na(iid_m)==FALSE],
                          L_Adum1_rs25531[is.na(iid_m)==FALSE],
                          L_Adum2_rs25531[is.na(iid_m)==FALSE],
                          get(outcomes[1])[is.na(iid_m)==FALSE],
                          get(outcomes[2])[is.na(iid_m)==FALSE],
                          female[is.na(iid_m)==FALSE],
                          age[is.na(iid_m)==FALSE],
                          birth_decade[is.na(iid_m)==FALSE]
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==3){
      temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                          Ldom_5http[is.na(iid_m)==FALSE],
                          Lrec_5http[is.na(iid_m)==FALSE],
                          Ldum1_5http[is.na(iid_m)==FALSE],
                          Ldum2_5http[is.na(iid_m)==FALSE],
                          add_rs25531[is.na(iid_m)==FALSE],
                          L_Adom_rs25531[is.na(iid_m)==FALSE],
                          L_Arec_rs25531[is.na(iid_m)==FALSE],
                          L_Adum1_rs25531[is.na(iid_m)==FALSE],
                          L_Adum2_rs25531[is.na(iid_m)==FALSE],
                          get(outcomes[1])[is.na(iid_m)==FALSE],
                          get(outcomes[2])[is.na(iid_m)==FALSE],
                          get(outcomes[3])[is.na(iid_m)==FALSE],
                          female[is.na(iid_m)==FALSE],
                          age[is.na(iid_m)==FALSE],
                          birth_decade[is.na(iid_m)==FALSE]
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==4){
      temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                          Ldom_5http[is.na(iid_m)==FALSE],
                          Lrec_5http[is.na(iid_m)==FALSE],
                          Ldum1_5http[is.na(iid_m)==FALSE],
                          Ldum2_5http[is.na(iid_m)==FALSE],
                          add_rs25531[is.na(iid_m)==FALSE],
                          L_Adom_rs25531[is.na(iid_m)==FALSE],
                          L_Arec_rs25531[is.na(iid_m)==FALSE],
                          L_Adum1_rs25531[is.na(iid_m)==FALSE],
                          L_Adum2_rs25531[is.na(iid_m)==FALSE],
                          get(outcomes[1])[is.na(iid_m)==FALSE],
                          get(outcomes[2])[is.na(iid_m)==FALSE],
                          get(outcomes[3])[is.na(iid_m)==FALSE],
                          get(outcomes[4])[is.na(iid_m)==FALSE],
                          female[is.na(iid_m)==FALSE],
                          age[is.na(iid_m)==FALSE],
                          birth_decade[is.na(iid_m)==FALSE]
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==5){
      temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                          Ldom_5http[is.na(iid_m)==FALSE],
                          Lrec_5http[is.na(iid_m)==FALSE],
                          Ldum1_5http[is.na(iid_m)==FALSE],
                          Ldum2_5http[is.na(iid_m)==FALSE],
                          add_rs25531[is.na(iid_m)==FALSE],
                          L_Adom_rs25531[is.na(iid_m)==FALSE],
                          L_Arec_rs25531[is.na(iid_m)==FALSE],
                          L_Adum1_rs25531[is.na(iid_m)==FALSE],
                          L_Adum2_rs25531[is.na(iid_m)==FALSE],
                          get(outcomes[1])[is.na(iid_m)==FALSE],
                          get(outcomes[2])[is.na(iid_m)==FALSE],
                          get(outcomes[3])[is.na(iid_m)==FALSE],
                          get(outcomes[4])[is.na(iid_m)==FALSE],
                          get(outcomes[5])[is.na(iid_m)==FALSE],
                          female[is.na(iid_m)==FALSE],
                          age[is.na(iid_m)==FALSE],
                          birth_decade[is.na(iid_m)==FALSE]
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        outcomes[5],
                        "female",
                        "age",
			"birth_decade")
    }
    
    if(noutcomes==6){
      temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                          Ldom_5http[is.na(iid_m)==FALSE],
                          Lrec_5http[is.na(iid_m)==FALSE],
                          Ldum1_5http[is.na(iid_m)==FALSE],
                          Ldum2_5http[is.na(iid_m)==FALSE],
                          add_rs25531[is.na(iid_m)==FALSE],
                          L_Adom_rs25531[is.na(iid_m)==FALSE],
                          L_Arec_rs25531[is.na(iid_m)==FALSE],
                          L_Adum1_rs25531[is.na(iid_m)==FALSE],
                          L_Adum2_rs25531[is.na(iid_m)==FALSE],
                          get(outcomes[1])[is.na(iid_m)==FALSE],
                          get(outcomes[2])[is.na(iid_m)==FALSE],
                          get(outcomes[3])[is.na(iid_m)==FALSE],
                          get(outcomes[4])[is.na(iid_m)==FALSE],
                          get(outcomes[5])[is.na(iid_m)==FALSE],
                          get(outcomes[6])[is.na(iid_m)==FALSE],
                          female[is.na(iid_m)==FALSE],
                          age[is.na(iid_m)==FALSE],
                          birth_decade[is.na(iid_m)==FALSE]
                          )
      names(temp_df)<-c("add_5http",
                        "Ldom_5http",
                        "Lrec_5http",
                        "Ldum1_5http",
                        "Ldum2_5http",
                        "add_rs25531",
                        "L_Adom_rs25531",
                        "L_Arec_rs25531",
                        "L_Adum1_rs25531",
                        "L_Adum2_rs25531",
                        outcomes[1],
                        outcomes[2],
                        outcomes[3],
                        outcomes[4],
                        outcomes[5],
                        outcomes[6],
                        "female",
                        "age",
			"birth_decade")
    }
    
      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],".txt",sep="")

        for(j in 1: nsnps){
          if(j%%5){
            for(k in 1:nmodels) {
              if (two_level_present(outcomes[i])=="YES") {	    
                write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
                oneSNP(j,snplist[j],outcomes[i],models[k],type,var.na,out.file,temp_df)
              }else write.table(paste(outcomes[i],models[k],"SNP=",snplist[j],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
            }
          }
        }
      }
      detach(temp_df)
    }
} 




LogR_DDDS.call<-function(outcomes_f, models_f, by.var_f) {
  cat("===== run depression dx w/ dichotomous stress_exposure input", "\n" )
  
  outcomes <- outcomes_f
  noutcomes <- length(outcomes)
  type<- 'binomial'
    
  model1.text <- paste( outcomes_f[1],models_f[1], sep=" ")    
  model1 <- as.formula(model1.text)
  vars<-as.character(attr(terms.formula(model1),"variables"))
  stress_f<-vars[length(vars)]
  
  models<-if(decade_na_flag())c(paste("~",vars[3],"+",stress_f,sep=" "),
                                paste("~",vars[3],"+",vars[4],"+",stress_f,sep=" ")
                                ) else c(paste("~",vars[3],"+",stress_f,sep=" "),
                                         paste("~",vars[3],"+",vars[5],"+",stress_f,sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",stress_f,sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",vars[5],"+",stress_f,sep=" ")
                                         )
    
    nmodels <- length(models)
    var.na <-  stress_f

    if(both_sexes=="YES"){
      cat("===== combined sex subset", "\n" )      
      fem_levels<-"combined-sex sample"

      if(noutcomes==1) {
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              get(outcomes[1]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      
      if(noutcomes==2) {
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              get(outcomes[1]),
                              get(outcomes[2]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      
      if(noutcomes==3) {
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              get(outcomes[1]),
                              get(outcomes[2]),
                              get(outcomes[3]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      
      attach(temp_df)
      
      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],"_DS.txt",sep="")
                
        for(k in 1:nmodels) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
	  }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      } 
      detach(temp_df)
    }
    
    if (sum(ifelse(is.na(iid_f),1,0))/length(iid_f) < 0.9) {
      cat("===== female only subset", "\n" )
      fem_levels<-"female-only sample"
      if(noutcomes==1) {
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      
      if(noutcomes==2) {
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              get(outcomes[2])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      
      if(noutcomes==3) {
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              get(outcomes[2])[is.na(iid_f)==FALSE],
                              get(outcomes[3])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      
      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],"_DS.txt",sep="")

        for(k in 1:nmodels) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
	  }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      } 
      detach(temp_df)
    }

    if (sum(ifelse(is.na(iid_m),1,0))/length(iid_m) < 0.9) {
      cat("===== male only subset", "\n" )
      fem_levels<-"male-only sample"
      if(noutcomes==1) {
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      
      if(noutcomes==2) {
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              get(outcomes[2])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      
      if(noutcomes==3) {
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              get(outcomes[2])[is.na(iid_m)==FALSE],
                              get(outcomes[3])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f)
      }
      
      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],"_DS.txt",sep="")

        for(k in 1:nmodels) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
	  }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      } 
      detach(temp_df)
    }
}



LogR_DDQS.call<-function(outcomes_f, models_f, by.var_f) {
  cat("===== run depression dx w/ normalized quantitative stress input", "\n" )

  outcomes <- outcomes_f
  noutcomes <- length(outcomes)
  type<- 'binomial'
    
  model1.text <- paste( outcomes_f[1],models_f[1], sep=" ")    
  model1 <- as.formula(model1.text)
  vars<-as.character(attr(terms.formula(model1),"variables"))
  stress_f<-vars[length(vars)]
  stress2_f<-paste(stress_f,"2",sep="")
  stressz_f<-paste(stress_f,"_z",sep="")
  stress2z_f<-paste(stress_f,"2_z",sep="")
  
  models<-if(decade_na_flag())c(paste("~",vars[3],"+",stressz_f,sep=" "),
                                paste("~",vars[3],"+",vars[4],"+",stressz_f,sep=" "),
                                paste("~",vars[3],"+",stress2z_f,sep=" "),
                                paste("~",vars[3],"+",vars[4],"+",stress2z_f,sep=" ")
                                ) else c(paste("~",vars[3],"+",stressz_f,sep=" "),
                                         paste("~",vars[3],"+",vars[5],"+",stressz_f,sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",stressz_f,sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",vars[5],"+",stressz_f,sep=" "),
                                         paste("~",vars[3],"+",stress2z_f,sep=" "),
                                         paste("~",vars[3],"+",vars[5],"+",stress2z_f,sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",stress2z_f,sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",vars[5],"+",stress2z_f,sep=" ")
                                         )
  models.raw<-if(decade_na_flag())c(paste("~",vars[3],"+",stress_f,sep=" "),
                                    paste("~",vars[3],"+",vars[4],"+",stress_f,sep=" "),
                                    paste("~",vars[3],"+",stress2_f,sep=" "),
                                    paste("~",vars[3],"+",vars[4],"+",stress2_f,sep=" ")
                                    ) else c(paste("~",vars[3],"+",stress_f,sep=" "),
                                             paste("~",vars[3],"+",vars[5],"+",stress_f,sep=" "),
                                             paste("~",vars[3],"+",vars[4],"+",stress_f,sep=" "),
                                             paste("~",vars[3],"+",vars[4],"+",vars[5],"+",stress_f,sep=" "),
                                             paste("~",vars[3],"+",stress2_f,sep=" "),
                                             paste("~",vars[3],"+",vars[5],"+",stress2_f,sep=" "),
                                             paste("~",vars[3],"+",vars[4],"+",stress2_f,sep=" "),
                                             paste("~",vars[3],"+",vars[4],"+",vars[5],"+",stress2_f,sep=" ")
                                             )

  nmodels <- length(models)
  var.na<-stressz_f

  if(both_sexes=="YES"){
    cat("===== combined sex subset", "\n" )      
    fem_levels<-"combined-sex sample"
      if(noutcomes==1) {
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              get(outcomes[1]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f),
                              get(stressz_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      
      if(noutcomes==2) {
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              get(outcomes[1]),
                              get(outcomes[2]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f),
                              get(stressz_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      
      if(noutcomes==3) {
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              get(outcomes[1]),
                              get(outcomes[2]),
                              get(outcomes[3]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f),
                              get(stressz_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      
    attach(temp_df)
      
      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],"_QS.txt",sep="")
                
        for(k in 1:(nmodels/2)) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
            if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
              write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models.raw[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSTRESS(snplist[1],outcomes[i],models.raw[k],type,var.na,out.file,temp_df)
	    }
          }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
        for(k in (nmodels/2 + 1):nmodels) {
              if ( (two_level_present_var(get(stress2_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
            if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
              write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models.raw[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSTRESS(snplist[1],outcomes[i],models.raw[k],type,var.na,out.file,temp_df)
	    }
          }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      } 
      detach(temp_df)
    }
    
    if (sum(ifelse(is.na(iid_f),1,0))/length(iid_f) < 0.9) {
      cat("===== female only subset", "\n" )
      fem_levels<-"female-only sample"
      if(noutcomes==1) {
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE],
                              get(stressz_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      
      if(noutcomes==2) {
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              get(outcomes[2])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE],
                              get(stressz_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      
      if(noutcomes==3) {
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              get(outcomes[2])[is.na(iid_f)==FALSE],
                              get(outcomes[3])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE],
                              get(stressz_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
            
      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],"_QS.txt",sep="")
        
        for(k in 1:(nmodels/2)) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
            if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
              write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models.raw[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSTRESS(snplist[1],outcomes[i],models.raw[k],type,var.na,out.file,temp_df)
	    }
          }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
	for(k in (nmodels/2 + 1):nmodels) {
              if ( (two_level_present_var(get(stress2_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
            if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
              write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models.raw[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSTRESS(snplist[1],outcomes[i],models.raw[k],type,var.na,out.file,temp_df)
	    }
          }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      } 
      detach(temp_df)
    }

    if (sum(ifelse(is.na(iid_m),1,0))/length(iid_m) < 0.9) {
      cat("===== male only subset", "\n" )
      fem_levels<-"male-only sample"
      
      if(noutcomes==1) {
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE],
                              get(stressz_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      
      if(noutcomes==2) {
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              get(outcomes[2])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE],
                              get(stressz_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      
      if(noutcomes==3) {
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              get(outcomes[2])[is.na(iid_m)==FALSE],
                              get(outcomes[3])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE],
                              get(stressz_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f)
      }
      
      attach(temp_df)
      
      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],"_QS.txt",sep="")
        
        for(k in 1:(nmodels/2)) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
            if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
              write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models.raw[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSTRESS(snplist[1],outcomes[i],models.raw[k],type,var.na,out.file,temp_df)
	    }
          }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
        for(k in (nmodels/2 + 1):nmodels) {
              if ( (two_level_present_var(get(stress2_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
            if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
              write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models.raw[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSTRESS(snplist[1],outcomes[i],models.raw[k],type,var.na,out.file,temp_df)
	    }
          }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      } 
      detach(temp_df)
    }
}
  

 # linear regression models #
 ############################                  
LinR_QDDS.call<-function(outcomes_f, models_f, by.var_f) {
  cat("===== run quantitative depression measure w/ dichotomous stress_exposure input", "\n" )

  outcomes <- outcomes_f
  noutcomes <- length(outcomes)
  type<- 'gaussian'
  
  model1.text <- paste( outcomes_f[1],models_f[1], sep=" ")    
  model1 <- as.formula(model1.text)
  vars<-as.character(attr(terms.formula(model1),"variables"))
  stress_f<-vars[length(vars)]
  
  models<-if(decade_na_flag())c(paste("~",vars[3],"+",stress_f,sep=" "),
                                paste("~",vars[3],"+",vars[4],"+",stress_f,sep=" ")
                                ) else c(paste("~",vars[3],"+",stress_f,sep=" "),
                                         paste("~",vars[3],"+",vars[5],"+",stress_f,sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",stress_f,sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",vars[5],"+",stress_f,sep=" ")
                                         )

  nmodels <- length(models)    
  var.na <- stress_f

  if(both_sexes=="YES"){
    cat("===== combined sex subset", "\n" )      
    fem_levels<-"combined-sex sample"
     if(noutcomes==1){
        temp_df<-data.frame(add_5http,
                            Ldom_5http,
                            Lrec_5http,
                            add_rs25531,
                            L_Adom_rs25531,
                            L_Arec_rs25531,
                            get(outcomes[1]),
                            female,
                            age,
                            birth_decade,
                            get(stress_f)
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
    }
    if(noutcomes==2){
        temp_df<-data.frame(add_5http,
                            Ldom_5http,
                            Lrec_5http,
                            add_rs25531,
                            L_Adom_rs25531,
                            L_Arec_rs25531,
                            get(outcomes[1]),
                            get(outcomes[2]),
                            female,
                            age,
                            birth_decade,
                            get(stress_f)
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }
    if(noutcomes==3){
        temp_df<-data.frame(add_5http,
                            Ldom_5http,
                            Lrec_5http,
                            add_rs25531,
                            L_Adom_rs25531,
                            L_Arec_rs25531,
                            get(outcomes[1]),
                            get(outcomes[2]),
                            get(outcomes[3]),
                            female,
                            age,
                            birth_decade,
                            get(stress_f)
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }
    if(noutcomes==4){
        temp_df<-data.frame(add_5http,
                            Ldom_5http,
                            Lrec_5http,
                            add_rs25531,
                            L_Adom_rs25531,
                            L_Arec_rs25531,
                            get(outcomes[1]),
                            get(outcomes[2]),
                            get(outcomes[3]),
                            get(outcomes[4]),
                            female,
                            age,
                            birth_decade,
                            get(stress_f)
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          outcomes[4],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }
    if(noutcomes==5){
        temp_df<-data.frame(add_5http,
                            Ldom_5http,
                            Lrec_5http,
                            add_rs25531,
                            L_Adom_rs25531,
                            L_Arec_rs25531,
                            get(outcomes[1]),
                            get(outcomes[2]),
                            get(outcomes[3]),
                            get(outcomes[4]),
                            get(outcomes[5]),
                            female,
                            age,
                            birth_decade,
                            get(stress_f)
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          outcomes[4],
                          outcomes[5],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }

    if(noutcomes==6){
        temp_df<-data.frame(add_5http,
                            Ldom_5http,
                            Lrec_5http,
                            add_rs25531,
                            L_Adom_rs25531,
                            L_Arec_rs25531,
                            get(outcomes[1]),
                            get(outcomes[2]),
                            get(outcomes[3]),
                            get(outcomes[4]),
                            get(outcomes[5]),
                            get(outcomes[6]),
                            female,
                            age,
                            birth_decade,
                            get(stress_f)
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          outcomes[4],
                          outcomes[5],
                          outcomes[6],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }

      attach(temp_df)
      
      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],"_DS.txt",sep="")
                
        for(k in 1:nmodels) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
	  }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      }
      detach(temp_df)
    }
    
    if (sum(ifelse(is.na(iid_f),1,0))/length(iid_f) < 0.9) {
      cat("===== female only subset", "\n" )
      fem_levels<-"female-only sample"
     if(noutcomes==1){
        temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                            Ldom_5http[is.na(iid_f)==FALSE],
                            Lrec_5http[is.na(iid_f)==FALSE],
                            add_rs25531[is.na(iid_f)==FALSE],
                            L_Adom_rs25531[is.na(iid_f)==FALSE],
                            L_Arec_rs25531[is.na(iid_f)==FALSE],
                            get(outcomes[1])[is.na(iid_f)==FALSE],
                            female[is.na(iid_f)==FALSE],
                            age[is.na(iid_f)==FALSE],
                            birth_decade[is.na(iid_f)==FALSE],
                            get(stress_f)[is.na(iid_f)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
    }
    if(noutcomes==2){
        temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                            Ldom_5http[is.na(iid_f)==FALSE],
                            Lrec_5http[is.na(iid_f)==FALSE],
                            add_rs25531[is.na(iid_f)==FALSE],
                            L_Adom_rs25531[is.na(iid_f)==FALSE],
                            L_Arec_rs25531[is.na(iid_f)==FALSE],
                            get(outcomes[1])[is.na(iid_f)==FALSE],
                            get(outcomes[2])[is.na(iid_f)==FALSE],
                            female[is.na(iid_f)==FALSE],
                            age[is.na(iid_f)==FALSE],
                            birth_decade[is.na(iid_f)==FALSE],
                            get(stress_f)[is.na(iid_f)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }
    if(noutcomes==3){
        temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                            Ldom_5http[is.na(iid_f)==FALSE],
                            Lrec_5http[is.na(iid_f)==FALSE],
                            add_rs25531[is.na(iid_f)==FALSE],
                            L_Adom_rs25531[is.na(iid_f)==FALSE],
                            L_Arec_rs25531[is.na(iid_f)==FALSE],
                            get(outcomes[1])[is.na(iid_f)==FALSE],
                            get(outcomes[2])[is.na(iid_f)==FALSE],
                            get(outcomes[3])[is.na(iid_f)==FALSE],
                            female[is.na(iid_f)==FALSE],
                            age[is.na(iid_f)==FALSE],
                            birth_decade[is.na(iid_f)==FALSE],
                            get(stress_f)[is.na(iid_f)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }
    if(noutcomes==4){
        temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                            Ldom_5http[is.na(iid_f)==FALSE],
                            Lrec_5http[is.na(iid_f)==FALSE],
                            add_rs25531[is.na(iid_f)==FALSE],
                            L_Adom_rs25531[is.na(iid_f)==FALSE],
                            L_Arec_rs25531[is.na(iid_f)==FALSE],
                            get(outcomes[1])[is.na(iid_f)==FALSE],
                            get(outcomes[2])[is.na(iid_f)==FALSE],
                            get(outcomes[3])[is.na(iid_f)==FALSE],
                            get(outcomes[4])[is.na(iid_f)==FALSE],
                            female[is.na(iid_f)==FALSE],
                            age[is.na(iid_f)==FALSE],
                            birth_decade[is.na(iid_f)==FALSE],
                            get(stress_f)[is.na(iid_f)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          outcomes[4],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }
    if(noutcomes==5){
        temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                            Ldom_5http[is.na(iid_f)==FALSE],
                            Lrec_5http[is.na(iid_f)==FALSE],
                            add_rs25531[is.na(iid_f)==FALSE],
                            L_Adom_rs25531[is.na(iid_f)==FALSE],
                            L_Arec_rs25531[is.na(iid_f)==FALSE],
                            get(outcomes[1])[is.na(iid_f)==FALSE],
                            get(outcomes[2])[is.na(iid_f)==FALSE],
                            get(outcomes[3])[is.na(iid_f)==FALSE],
                            get(outcomes[4])[is.na(iid_f)==FALSE],
                            get(outcomes[5])[is.na(iid_f)==FALSE],
                            female[is.na(iid_f)==FALSE],
                            age[is.na(iid_f)==FALSE],
                            birth_decade[is.na(iid_f)==FALSE],
                            get(stress_f)[is.na(iid_f)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          outcomes[4],
                          outcomes[5],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }

    if(noutcomes==6){
        temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                            Ldom_5http[is.na(iid_f)==FALSE],
                            Lrec_5http[is.na(iid_f)==FALSE],
                            add_rs25531[is.na(iid_f)==FALSE],
                            L_Adom_rs25531[is.na(iid_f)==FALSE],
                            L_Arec_rs25531[is.na(iid_f)==FALSE],
                            get(outcomes[1])[is.na(iid_f)==FALSE],
                            get(outcomes[2])[is.na(iid_f)==FALSE],
                            get(outcomes[3])[is.na(iid_f)==FALSE],
                            get(outcomes[4])[is.na(iid_f)==FALSE],
                            get(outcomes[5])[is.na(iid_f)==FALSE],
                            get(outcomes[6])[is.na(iid_f)==FALSE],
                            female[is.na(iid_f)==FALSE],
                            age[is.na(iid_f)==FALSE],
                            birth_decade[is.na(iid_f)==FALSE],
                            get(stress_f)[is.na(iid_f)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          outcomes[4],
                          outcomes[5],
                          outcomes[6],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }
      
      attach(temp_df)
      
      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],"_DS.txt",sep="")

        for(k in 1:nmodels) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
	  }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      }
      detach(temp_df)
    }

    if (sum(ifelse(is.na(iid_m),1,0))/length(iid_m) < 0.9) {
      cat("===== male only subset", "\n" )
      fem_levels<-"male-only sample"
     if(noutcomes==1){
        temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                            Ldom_5http[is.na(iid_m)==FALSE],
                            Lrec_5http[is.na(iid_m)==FALSE],
                            add_rs25531[is.na(iid_m)==FALSE],
                            L_Adom_rs25531[is.na(iid_m)==FALSE],
                            L_Arec_rs25531[is.na(iid_m)==FALSE],
                            get(outcomes[1])[is.na(iid_m)==FALSE],
                            female[is.na(iid_m)==FALSE],
                            age[is.na(iid_m)==FALSE],
                            birth_decade[is.na(iid_m)==FALSE],
                            get(stress_f)[is.na(iid_m)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
    }
    if(noutcomes==2){
        temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                            Ldom_5http[is.na(iid_m)==FALSE],
                            Lrec_5http[is.na(iid_m)==FALSE],
                            add_rs25531[is.na(iid_m)==FALSE],
                            L_Adom_rs25531[is.na(iid_m)==FALSE],
                            L_Arec_rs25531[is.na(iid_m)==FALSE],
                            get(outcomes[1])[is.na(iid_m)==FALSE],
                            get(outcomes[2])[is.na(iid_m)==FALSE],
                            female[is.na(iid_m)==FALSE],
                            age[is.na(iid_m)==FALSE],
                            birth_decade[is.na(iid_m)==FALSE],
                            get(stress_f)[is.na(iid_m)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }
    if(noutcomes==3){
        temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                            Ldom_5http[is.na(iid_m)==FALSE],
                            Lrec_5http[is.na(iid_m)==FALSE],
                            add_rs25531[is.na(iid_m)==FALSE],
                            L_Adom_rs25531[is.na(iid_m)==FALSE],
                            L_Arec_rs25531[is.na(iid_m)==FALSE],
                            get(outcomes[1])[is.na(iid_m)==FALSE],
                            get(outcomes[2])[is.na(iid_m)==FALSE],
                            get(outcomes[3])[is.na(iid_m)==FALSE],
                            female[is.na(iid_m)==FALSE],
                            age[is.na(iid_m)==FALSE],
                            birth_decade[is.na(iid_m)==FALSE],
                            get(stress_f)[is.na(iid_m)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }
    if(noutcomes==4){
        temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                            Ldom_5http[is.na(iid_m)==FALSE],
                            Lrec_5http[is.na(iid_m)==FALSE],
                            add_rs25531[is.na(iid_m)==FALSE],
                            L_Adom_rs25531[is.na(iid_m)==FALSE],
                            L_Arec_rs25531[is.na(iid_m)==FALSE],
                            get(outcomes[1])[is.na(iid_m)==FALSE],
                            get(outcomes[2])[is.na(iid_m)==FALSE],
                            get(outcomes[3])[is.na(iid_m)==FALSE],
                            get(outcomes[4])[is.na(iid_m)==FALSE],
                            female[is.na(iid_m)==FALSE],
                            age[is.na(iid_m)==FALSE],
                            birth_decade[is.na(iid_m)==FALSE],
                            get(stress_f)[is.na(iid_m)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          outcomes[4],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }
    if(noutcomes==5){
        temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                            Ldom_5http[is.na(iid_m)==FALSE],
                            Lrec_5http[is.na(iid_m)==FALSE],
                            add_rs25531[is.na(iid_m)==FALSE],
                            L_Adom_rs25531[is.na(iid_m)==FALSE],
                            L_Arec_rs25531[is.na(iid_m)==FALSE],
                            get(outcomes[1])[is.na(iid_m)==FALSE],
                            get(outcomes[2])[is.na(iid_m)==FALSE],
                            get(outcomes[3])[is.na(iid_m)==FALSE],
                            get(outcomes[4])[is.na(iid_m)==FALSE],
                            get(outcomes[5])[is.na(iid_m)==FALSE],
                            female[is.na(iid_m)==FALSE],
                            age[is.na(iid_m)==FALSE],
                            birth_decade[is.na(iid_m)==FALSE],
                            get(stress_f)[is.na(iid_m)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          outcomes[4],
                          outcomes[5],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }

    if(noutcomes==6){
        temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                            Ldom_5http[is.na(iid_m)==FALSE],
                            Lrec_5http[is.na(iid_m)==FALSE],
                            add_rs25531[is.na(iid_m)==FALSE],
                            L_Adom_rs25531[is.na(iid_m)==FALSE],
                            L_Arec_rs25531[is.na(iid_m)==FALSE],
                            get(outcomes[1])[is.na(iid_m)==FALSE],
                            get(outcomes[2])[is.na(iid_m)==FALSE],
                            get(outcomes[3])[is.na(iid_m)==FALSE],
                            get(outcomes[4])[is.na(iid_m)==FALSE],
                            get(outcomes[5])[is.na(iid_m)==FALSE],
                            get(outcomes[6])[is.na(iid_m)==FALSE],
                            female[is.na(iid_m)==FALSE],
                            age[is.na(iid_m)==FALSE],
                            birth_decade[is.na(iid_m)==FALSE],
                            get(stress_f)[is.na(iid_m)==FALSE]
                            )
        names(temp_df)<-c("add_5http",
                          "Ldom_5http",
                          "Lrec_5http",
                          "add_rs25531",
                          "L_Adom_rs25531",
                          "L_Arec_rs25531",
                          outcomes[1],
                          outcomes[2],
                          outcomes[3],
                          outcomes[4],
                          outcomes[5],
                          outcomes[6],
                          "female",
                          "age",
                          "birth_decade",
                          stress_f
                          )
        }
      
      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],"_DS.txt",sep="")

        for(k in 1:nmodels) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
	  }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      }
      detach(temp_df)
    }
}


LinR_QDQS.call<-function(outcomes_f, models_f, by.var_f) {
  cat("===== run quantitative depression measure w/ quantitative stress input", "\n" )
  
  outcomes <- outcomes_f
  noutcomes <- length(outcomes)
  type<- 'gaussian'
    
  model1.text <- paste( outcomes_f[1],models_f[1], sep=" ")    
  model1 <- as.formula(model1.text)
  vars<-as.character(attr(terms.formula(model1),"variables"))
  stress_f<-vars[length(vars)]
  stress2_f<-paste(stress_f,"2",sep="")
  stressz_f<-paste(stress_f,"_z",sep="")
  stress2z_f<-paste(stress_f,"2_z",sep="")
  
  models<-if(decade_na_flag())c(paste("~",vars[3],"+",stressz_f,sep=" "),
                                paste("~",vars[3],"+",vars[4],"+",stressz_f,sep=" "),
                                paste("~",vars[3],"+",stress2z_f,sep=" "),
                                paste("~",vars[3],"+",vars[4],"+",stress2z_f,sep=" ")
                                ) else c(paste("~",vars[3],"+",stressz_f,sep=" "),
                                         paste("~",vars[3],"+",vars[5],"+",stressz_f,sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",stressz_f,sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",vars[5],"+",stressz_f,sep=" "),
                                         paste("~",vars[3],"+",stress2z_f,sep=" "),
                                         paste("~",vars[3],"+",vars[5],"+",stress2z_f,sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",stress2z_f,sep=" "),
                                         paste("~",vars[3],"+",vars[4],"+",vars[5],"+",stress2z_f,sep=" ")
                                         )

  models.raw<-if(decade_na_flag())c(paste("~",vars[3],"+",stress_f,sep=" "),
                                    paste("~",vars[3],"+",vars[4],"+",stress_f,sep=" "),
                                    paste("~",vars[3],"+",stress2_f,sep=" "),
                                    paste("~",vars[3],"+",vars[4],"+",stress2_f,sep=" ")
                                    ) else c(paste("~",vars[3],"+",stress_f,sep=" "),
                                             paste("~",vars[3],"+",vars[5],"+",stress_f,sep=" "),
                                             paste("~",vars[3],"+",vars[4],"+",stress_f,sep=" "),
                                             paste("~",vars[3],"+",vars[4],"+",vars[5],"+",stress_f,sep=" "),
                                             paste("~",vars[3],"+",stress2_f,sep=" "),
                                             paste("~",vars[3],"+",vars[5],"+",stress2_f,sep=" "),
                                             paste("~",vars[3],"+",vars[4],"+",stress2_f,sep=" "),
                                             paste("~",vars[3],"+",vars[4],"+",vars[5],"+",stress2_f,sep=" ")
                                             )

  nmodels <- length(models)
  var.na <- stressz_f

    if(both_sexes=="YES"){
      cat("===== combined sex subset", "\n" )      
      fem_levels<-"combined-sex sample"
      if(noutcomes==1){
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              get(outcomes[1]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f),
                              get(stressz_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      if(noutcomes==2){
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              get(outcomes[1]),
                              get(outcomes[2]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f),
                              get(stressz_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      if(noutcomes==3){
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              get(outcomes[1]),
                              get(outcomes[2]),
                              get(outcomes[3]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f),
                              get(stressz_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      if(noutcomes==4){
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              get(outcomes[1]),
                              get(outcomes[2]),
                              get(outcomes[3]),
                              get(outcomes[4]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f),
                              get(stressz_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            outcomes[4],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      if(noutcomes==5){
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              get(outcomes[1]),
                              get(outcomes[2]),
                              get(outcomes[3]),
                              get(outcomes[4]),
                              get(outcomes[5]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f),
                              get(stressz_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            outcomes[4],
                            outcomes[5],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }

      if(noutcomes==6){
          temp_df<-data.frame(add_5http,
                              Ldom_5http,
                              Lrec_5http,
                              add_rs25531,
                              L_Adom_rs25531,
                              L_Arec_rs25531,
                              get(outcomes[1]),
                              get(outcomes[2]),
                              get(outcomes[3]),
                              get(outcomes[4]),
                              get(outcomes[5]),
                              get(outcomes[6]),
                              female,
                              age,
                              birth_decade,
                              get(stress_f),
                              get(stressz_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2_f),
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http))) else get(stress2z_f)
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            outcomes[4],
                            outcomes[5],
                            outcomes[6],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      
      attach(temp_df)
      
      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],"_QS.txt",sep="")
                
        for(k in 1:(nmodels/2)) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
            if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
              write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models.raw[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSTRESS(snplist[1],outcomes[i],models.raw[k],type,var.na,out.file,temp_df)
	    }
          }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
        for(k in (nmodels/2 + 1):nmodels) {
              if ( (two_level_present_var(get(stress2_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
            if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
              write.table("===== combined sex analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models.raw[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSTRESS(snplist[1],outcomes[i],models.raw[k],type,var.na,out.file,temp_df)
	    }
          }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      }
      detach(temp_df)
    }
    
    if (sum(ifelse(is.na(iid_f),1,0))/length(iid_f) < 0.9) {
      cat("===== female only subset", "\n" )
      fem_levels<-"female-only sample"
      
      if(noutcomes==1){
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE],
                              get(stressz_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      if(noutcomes==2){
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              get(outcomes[2])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE],
                              get(stressz_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      if(noutcomes==3){
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              get(outcomes[2])[is.na(iid_f)==FALSE],
                              get(outcomes[3])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE],
                              get(stressz_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      if(noutcomes==4){
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              get(outcomes[2])[is.na(iid_f)==FALSE],
                              get(outcomes[3])[is.na(iid_f)==FALSE],
                              get(outcomes[4])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE],
                              get(stressz_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            outcomes[4],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      if(noutcomes==5){
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              get(outcomes[2])[is.na(iid_f)==FALSE],
                              get(outcomes[3])[is.na(iid_f)==FALSE],
                              get(outcomes[4])[is.na(iid_f)==FALSE],
                              get(outcomes[5])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE],
                              get(stressz_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            outcomes[4],
                            outcomes[5],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }

      if(noutcomes==6){
          temp_df<-data.frame(add_5http[is.na(iid_f)==FALSE],
                              Ldom_5http[is.na(iid_f)==FALSE],
                              Lrec_5http[is.na(iid_f)==FALSE],
                              add_rs25531[is.na(iid_f)==FALSE],
                              L_Adom_rs25531[is.na(iid_f)==FALSE],
                              L_Arec_rs25531[is.na(iid_f)==FALSE],
                              get(outcomes[1])[is.na(iid_f)==FALSE],
                              get(outcomes[2])[is.na(iid_f)==FALSE],
                              get(outcomes[3])[is.na(iid_f)==FALSE],
                              get(outcomes[4])[is.na(iid_f)==FALSE],
                              get(outcomes[5])[is.na(iid_f)==FALSE],
                              get(outcomes[6])[is.na(iid_f)==FALSE],
                              female[is.na(iid_f)==FALSE],
                              age[is.na(iid_f)==FALSE],
                              birth_decade[is.na(iid_f)==FALSE],
                              get(stress_f)[is.na(iid_f)==FALSE],
                              get(stressz_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2_f)[is.na(iid_f)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_f)==FALSE]))) else get(stress2z_f)[is.na(iid_f)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            outcomes[4],
                            outcomes[5],
                            outcomes[6],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      
      attach(temp_df)

      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],"_QS.txt",sep="")
        
        for(k in 1:(nmodels/2)) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
            if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
              write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models.raw[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSTRESS(snplist[1],outcomes[i],models.raw[k],type,var.na,out.file,temp_df)
	    }
          }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
        for(k in (nmodels/2 +1):nmodels) {
              if ( (two_level_present_var(get(stress2_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
            if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
              write.table("===== only females analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models.raw[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSTRESS(snplist[1],outcomes[i],models.raw[k],type,var.na,out.file,temp_df)
	    }
          }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      }    
      detach(temp_df)
    }

    if (sum(ifelse(is.na(iid_m),1,0))/length(iid_m) < 0.9) {
      cat("===== male only subset", "\n" )
      fem_levels<-"male-only sample"
      if(noutcomes==1){
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE],
                              get(stressz_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      if(noutcomes==2){
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              get(outcomes[2])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE],
                              get(stressz_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      if(noutcomes==3){
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              get(outcomes[2])[is.na(iid_m)==FALSE],
                              get(outcomes[3])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE],
                              get(stressz_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      if(noutcomes==4){
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              get(outcomes[2])[is.na(iid_m)==FALSE],
                              get(outcomes[3])[is.na(iid_m)==FALSE],
                              get(outcomes[4])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE],
                              get(stressz_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            outcomes[4],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }
      if(noutcomes==5){
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              get(outcomes[2])[is.na(iid_m)==FALSE],
                              get(outcomes[3])[is.na(iid_m)==FALSE],
                              get(outcomes[4])[is.na(iid_m)==FALSE],
                              get(outcomes[5])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE],
                              get(stressz_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            outcomes[4],
                            outcomes[5],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }

      if(noutcomes==6){
          temp_df<-data.frame(add_5http[is.na(iid_m)==FALSE],
                              Ldom_5http[is.na(iid_m)==FALSE],
                              Lrec_5http[is.na(iid_m)==FALSE],
                              add_rs25531[is.na(iid_m)==FALSE],
                              L_Adom_rs25531[is.na(iid_m)==FALSE],
                              L_Arec_rs25531[is.na(iid_m)==FALSE],
                              get(outcomes[1])[is.na(iid_m)==FALSE],
                              get(outcomes[2])[is.na(iid_m)==FALSE],
                              get(outcomes[3])[is.na(iid_m)==FALSE],
                              get(outcomes[4])[is.na(iid_m)==FALSE],
                              get(outcomes[5])[is.na(iid_m)==FALSE],
                              get(outcomes[6])[is.na(iid_m)==FALSE],
                              female[is.na(iid_m)==FALSE],
                              age[is.na(iid_m)==FALSE],
                              birth_decade[is.na(iid_m)==FALSE],
                              get(stress_f)[is.na(iid_m)==FALSE],
                              get(stressz_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2_f)[is.na(iid_m)==FALSE],
                              if((stress_f=="life_stress_quant")|(stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")) c(rep(NA,length(add_5http[is.na(iid_m)==FALSE]))) else get(stress2z_f)[is.na(iid_m)==FALSE]
                              )
          names(temp_df)<-c("add_5http",
                            "Ldom_5http",
                            "Lrec_5http",
                            "add_rs25531",
                            "L_Adom_rs25531",
                            "L_Arec_rs25531",
                            outcomes[1],
                            outcomes[2],
                            outcomes[3],
                            outcomes[4],
                            outcomes[5],
                            outcomes[6],
                            "female",
                            "age",
                            "birth_decade",
                            stress_f,
                            stressz_f,
                            stress2_f,
                            stress2z_f
                            )
      }      
      
      attach(temp_df)
      
      for(i in 1: noutcomes) {
        out.file<-paste(outdir,SITE,file_label, outcomes[i],"_QS.txt",sep="")
        
        for(k in 1:(nmodels/2)) {
              if ( (two_level_present_var(get(stress_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
            if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
              write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models.raw[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSTRESS(snplist[1],outcomes[i],models.raw[k],type,var.na,out.file,temp_df)
	    }
          }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
        for(k in (nmodels/2 + 1):nmodels) {
              if ( (two_level_present_var(get(stress2_f)[is.na(get(outcomes[i]))==FALSE])=="YES") & (two_level_present(outcomes[i])=="YES") ) {
            write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(paste(outcomes[i],models[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
            oneSTRESS(snplist[1],outcomes[i],models[k],type,var.na,out.file,temp_df)
            if((stress_f=="child_mal_quant" & child_mal_q_system=="CTQ") | (((stress_f=="stress_combined_quant_5yr_life")|(stress_f=="stress_combined_quant_5yr_curr")|(stress_f=="stress_combined_quant")) & child_mal_q_system=="CTQ" & life_stress_q_system=="LTE_Q") | (((stress_f=="life_stress_quant_5yr_life")|(stress_f=="life_stress_quant_5yr_curr")|(stress_f=="life_stress_quant")) & life_stress_q_system=="LTE_Q")) {
              write.table("===== only males analysis ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(paste(outcomes[i],models.raw[k],sep=" "),file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              write.table(" ",file=out.file,col.names=FALSE,  row.names=FALSE,append=TRUE, quote=FALSE)
              oneSTRESS(snplist[1],outcomes[i],models.raw[k],type,var.na,out.file,temp_df)
	    }
          }else write.table(paste(outcomes[i],models[k],"in",fem_levels,"in",file_label,sep=" "),file=out.error,col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
        }
      }    
      detach(temp_df)
    }
}

