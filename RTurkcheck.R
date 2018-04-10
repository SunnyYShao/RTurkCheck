# Making multiple plots in one graph
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

t_out <- function(test) {
  
  val1 <- test$estimate[1]
  val2 <- test$estimate[2]
  stat <- test$statistic
  p.val <- test$p.value
  return(c ( val1, val2, stat, p.val) )
  
}
#out <- rbind(wh, fem, age, inc, edu, trump, party, key, wall, ban)
#colnames(out) <- c("W1_Mean", "W2_Mean", "T_Stat", "P_Value")
#row.names(out) <- c("White", "Female", "Age", "Income", "Education", "Vote Trump",
#                    "Party ID", "Keystone", "Wall", "Muslim Ban")
#xtable(out, caption="Wave 1 and Wave 2 difference of means comparisons. Attrition is random.")

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

