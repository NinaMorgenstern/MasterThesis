LogLoss <- function (data, lev = NULL, model = NULL) 
{ 
  obs <- data[, "obs"]
  cls <- levels(obs) #find class names
  probs <- data[, cls[2]] #use second class name
  probs <- pmax(pmin(as.numeric(probs), 1 - 1e-15), 1e-15) #bound probability
  logPreds <- log(probs)        
  log1Preds <- log(1 - probs)
  real <- (as.numeric(data$obs) - 1)
  out <- c(mean(real * logPreds + (1 - real) * log1Preds)) * -1
  names(out) <- c("LogLoss")
  out
}


