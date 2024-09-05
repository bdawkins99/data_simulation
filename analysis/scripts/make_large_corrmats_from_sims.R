# the following two functions can be sourced at the end of the case-control and paired sim scripts, respectively
# there is an example after each function is defined that is commented out

################################################################################################################################################################
# for case-control sims
################################################################################################################################################################

# Makes a big matrix containing all case and control correlation matrices from make_case_ctrl_corrs() function
#
# parameters:
#
# case.list - $cases list element of make_case_ctrl_corrs() function (Example: my.corrmats$cases)
# ctrl.list - $ctrls list element of make_case_ctrl_corrs() function (Example: my.corrmats$ctrls)
# save.file - (logical) will save .rds file if true
# file.name - (character) name of file if you would like to save it (do not include .rds extension)
#
# output:
# 
# big.case.ctrl.mat - (data.frame) each row is an ROI pair and each column is a subject with case/control identifier
make_big_case_ctrl_mat <- function(case.list, ctrl.list, save.file=FALSE, file.name=NULL){
  
  big.case.mat <- NULL
  case.names <- character()
  sub.count <- 1
  for(i in 1:length(case.list)){
    
    case.mat.tmp <- case.list[[i]]$corrmat
    diag(case.mat.tmp) <- NA
    melted.case.mat <- melt(case.mat.tmp, na.rm=T)
    
    case.names[i] <- paste("sub",sub.count, "_case",sep="")
    sub.count <- sub.count + 1
    if(i == 1){
      corr.pair.names <- apply(melted.case.mat[,c("Var1","Var2")], 1, function(x)paste("ROI", x[1], ".", "ROI", x[2], sep=""))
    }
    
    big.case.mat <- cbind(big.case.mat, melted.case.mat[,"value"])
    
  }
  big.case.mat <- as.data.frame(big.case.mat)
  colnames(big.case.mat) <- case.names
  row.names(big.case.mat) <- corr.pair.names
  
  big.ctrl.mat <- NULL
  ctrl.names <- character()
  for(i in 1:length(ctrl.list)){
    
    ctrl.mat.tmp <- ctrl.list[[i]]$corrmat
    diag(ctrl.mat.tmp) <- NA
    melted.ctrl.mat <- melt(ctrl.mat.tmp, na.rm=T)
    
    ctrl.names[i] <- paste("sub",sub.count, "_ctrl",sep="")
    sub.count <- sub.count + 1
    
    big.ctrl.mat <- cbind(big.ctrl.mat, melted.ctrl.mat[,"value"])
    
  }
  big.ctrl.mat <- as.data.frame(big.ctrl.mat)
  colnames(big.ctrl.mat) <- ctrl.names
  row.names(big.ctrl.mat) <- corr.pair.names
  
  big.case.ctrl.mat <- data.frame(big.case.mat, big.ctrl.mat)
  output.list <- list(case.ctrl.corrmats=big.case.ctrl.mat,
                      degree.vec=case.list[[1]]$deg.vec,
                      adjacency.mat=case.list[[1]]$A.mat,
                      functional.ROIs=case.list[[1]]$sig.vars,
                      diffcor.ROI.partners=case.list[[1]]$interacting.partners)
  
  if(save.file==T){
    if(!is.null(file.name)){
      saveRDS(output.list, paste(file.name, ".rds"))
    }else{
      saveRDS(output.list, "case_ctrl_corrmats.rds")
    }
  }
  
  return(output.list)
}

# example of creating large matrix (actually a data.frame), containing all case and control correlation matrices
# you can save the file if you want by setting save.file=TRUE

#case.ctrl.corrmats <- make_big_case_ctrl_mat(case.list=my.corrmats$cases, ctrl.list=my.corrmats$ctrls, save.file=F, file.name="example_case_ctrl_fmri_sim")
#str(case.ctrl.corrmats)

################################################################################################################################################################
# for paired sims
################################################################################################################################################################

# Makes a big matrix containing all pre and post correlation matrices from make_paired_corrmats() function
#
# parameters:
#
# pre.list - $baseline.mats list element of make_paired_corrmats() function (Example: my.corrmats$baseline.mats)
# post.list - $followup.mats list element of make_paired_corrmats() function (Example: my.corrmats$followup.mats)
# save.file - (logical) will save .rds file if true
# file.name - (character) name of file if you would like to save it (do not include .rds extension)
#
# output:
# 
# big.paired.mat - (data.frame) each row is an ROI pair and each column is a subject with pre/post identifier
make_big_pre_post_mat <- function(pre.list, post.list, save.file=FALSE, file.name=NULL){
  
  big.pre.mat <- NULL
  for(i in 1:length(pre.list)){
    
    pre.mat.tmp <- pre.list[[i]]$corrmat
    diag(pre.mat.tmp) <- NA
    melted.pre.mat <- melt(pre.mat.tmp, na.rm=T)
    
    if(i == 1){
      corr.pair.names <- apply(melted.pre.mat[,c("Var1","Var2")], 1, function(x)paste("ROI", x[1], ".", "ROI", x[2], sep=""))
    }
    
    big.pre.mat <- cbind(big.pre.mat, melted.pre.mat[,"value"])
    
  }
  big.pre.mat <- as.data.frame(big.pre.mat)
  row.names(big.pre.mat) <- corr.pair.names
  
  big.post.mat <- NULL
  for(i in 1:length(post.list)){
    
    post.mat.tmp <- post.list[[i]]
    diag(post.mat.tmp) <- NA
    melted.post.mat <- melt(post.mat.tmp, na.rm=T)
    
    big.post.mat <- cbind(big.post.mat, melted.post.mat[,"value"])
    
  }
  big.post.mat <- as.data.frame(big.post.mat)
  row.names(big.post.mat) <- corr.pair.names
  
  big.paired.mat <- data.frame(big.post.mat, big.post.mat)
  
  nsubs <- length(pre.list)
  colnames(big.paired.mat) <- c(paste("sub", 1:nsubs, "_pre", sep=""), paste("sub", 1:nsubs, "_post", sep=""))
  
  output.list <- list(paired.corrmats=big.paired.mat,
                      degree.vec=pre.list[[1]]$deg.vec,
                      adjacency.mat=pre.list[[1]]$A.mat,
                      functional.ROIs=pre.list[[1]]$sig.vars,
                      diffcor.ROI.partners=pre.list[[1]]$interacting.partners)
  
  if(save.file==T){
    if(!is.null(file.name)){
      saveRDS(output.list, paste(file.name, ".rds"))
    }else{
      saveRDS(output.list, "paired_corrmats.rds")
    }
  }
  
  return(output.list)
}

# example of creating large matrix (actually a data.frame), containing all pre and post correlation matrices
# you can save the file if you want by setting save.file=TRUE

#paired.corrmats <- make_big_pre_post_mat(pre.list=my.corrmats$baseline.mats, post.list=my.corrmats$followup.mats, save.file=F, file.name="example_paired_fmri_sim")
#str(paired.corrmats)





