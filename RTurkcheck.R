# Making multiple plots in one graph
# Takes up to several ggplot plotting objects()
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

###########################
#    T-Test Comparisons   #
###########################

library(data.table)
library(xtable)

#########
# t_out #
#########

t_out <- function(test) {
  # Single t-test table creation line #
  # test = htest class object, result of t.test() X, Y operation  
  val1 <- test$estimate[1]
  val2 <- test$estimate[2]
  diff <- abs(val1 - val2)
  stat <- test$statistic
  p.val <- test$p.value
  vec_out <- round(c(val1, val2, diff, stat, p.val),4)
  names(vec_out) <- c("W1_Mean", "W2_Mean", "Abs_Diff", "T_Stat", "P_Value")
  return(vec_out)
  
}

################
# t_generalize #
################

t_generalize <- function(w1, w2, cnames1, cnames2) {
  # Arguments #  
  # w1 = wave1 dataset
  # w2 = wave2 dataset, which is w1 and w2 merged, actually.
  # cnames1; cnames2 = character vector of column names; must be same name
  
  # Error Handling #
  if( !identical(cnames1, cnames2) ) {
    stop("Error: cnames1 and cnames2 are not identical. Make them identical real good")
  }
  
  # Print Dimensions #
  cat("Wave 1 Dimensions:\n")
  print(dim(w1))
  cat("Wave 2 Dimensions:\n")
  print(dim(w2))
  cat("\n")
  
  # Select Columns
  col_select1 <- colnames(w1) %in% cnames1
  col_select2 <- colnames(w2) %in% cnames2
  
  # select appropriate columns
  w1 <- w1[,col_select1]
  w2 <- w2[,col_select2]

  # Create matrix holder
  ttest_collect <- matrix(NA, nrow=ncol(w1), ncol=5) # nrow=length of t_out() result
  
  # Loop over columns and fill down ttest_collect rows
  for (i in 1:ncol(w1)){
    ttest_collect[i,] <- t_out(t.test(w1[,i], w2[,i]))
  }
  
  # Label columns and rows
  colnames(ttest_collect) <- c("W1_Mean", "W2_Mean", "Abs_Diff", "T_Stat", "P_Value")
  rownames(ttest_collect) <- cnames1
  # Return Matrix
  return(ttest_collect)
  
}


##################################
# Variable Desriptive Statistics #
##################################

sum_func <- function(dat){
  
  dat_min <- apply(dat, 2, min, na.rm=T)
  dat_max <- apply(dat, 2, max, na.rm=T)
  dat_mean <- apply(dat, 2, mean, na.rm=T)
  dat_sd <- apply(dat, 2, sd, na.rm=T)
  return(data.frame(dat_min, dat_max, dat_mean, dat_sd))
}

########################################################
# aov_treat_compare: Check Across Treatment Conditions #
# ANOVA test across treatment conditions               #
########################################################

aov_treat_compare <- function (dat, var_names, treat_var){
  
  # dat = dataframe, the final survey dataframe
  # var_names = character vector of demographic variables to compare across treatment condition
  # treat_var = character vector (1) name of treatment indicator 
  
  # Conduct ANOVA test and put into matrix for table output #
  aov_mat <- matrix(NA, nrow=length(var_names), ncol=2)
  for (i in 1:length(aov_vec)){
    aov_mat[i,] <- unlist ( summary ( aov(formula ( 
    paste (var_names[i], "~ as.factor(", treat_var, ")", sep="" )), data = dat )))[c(7,9)]
  }
  # Label Datasets #
  colnames(aov_mat) <- c("F-Value", "P-Value")
  rownames(aov_mat) <- var_names
  aov_mat <- round(aov_mat, 4)
  return (aov_mat)
}
