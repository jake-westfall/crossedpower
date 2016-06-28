# format contents of 'Additional info' box
print.info <- function(x){
  for(i in names(x)){
    if(i=="ncp") cat("Noncentrality parameter:",toString(round(x[i],2)),"\n")
    else if(i=="df") cat("Degrees of freedom:",toString(round(x[i],1)),"\n")
    else if(i=="d") cat("Effect size d:",toString(round(x[i],2)),"\n")
    else if(i=="E") cat("Residual VPC:",toString(round(x[i],3)),"\n")
    else if(i=="P") cat("Participant intercept VPC:",toString(round(x[i],3)),"\n")
    else if(i=="S") cat("Stimulus intercept VPC",toString(round(x[i],3)),"\n")
    else if(i=="PS") cat("Participant-by-Stimulus VPC:",toString(round(x[i],3)),"\n")
    else if(i=="PC") cat("Participant slope VPC:",toString(round(x[i],3)),"\n")
    else if(i=="SC") cat("Stimulus slope VPC:",toString(round(x[i],3)),"\n")
  }
  invisible(x)
}

# format contents of 'Technical output' box
print.debug <- function(x, digits = max(3, getOption("digits")-3)){
  cat("parameter estimates:", toString(x$par), "\n")
  cat("objective:", toString(x$fval), "\n")
  cat("number of function evaluations:", toString(x$feval), "\n")
  cat("integer error code (zero means success):", toString(x$ierr), "\n")
  cat("message:", toString(x$msg))
  invisible(x)
}