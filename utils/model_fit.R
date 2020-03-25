fit_exponential <- function(x, min.x =1, first.n=-1, not.enough.samples.err = F){
  ######## model #########
  # a = a0*e^(kt) or ln(a) = ln(a0) + kt
  # where,
  # t = time (days)
  # a = counts
  # a0 = initial count
  # k = growth rate
  ########################
  require(stats)
  
  x = as.numeric(x)
  counts = as.numeric(x[x>=min.x])
  stopifnot(length(counts) > 1)
  sample.start.idx = min(which(x>=min.x))
  if(first.n > 0){
    if(not.enough.samples.err == T && length(counts) < first.n)
      stop(sprintf("Not enough samples - required: %s < observed: %s", first.n, length(counts)))
    counts = counts[1:min(first.n, length(counts))]
  }
  days = 1:length(counts)
  exponential.model <- lm(log(counts) ~ days)
  to_return = list(a0=as.numeric(exponential.model$coefficients[1]),
                   k=as.numeric(exponential.model$coefficients[2]),
                   n.samples = length(counts),
                   sample.start.idx = sample.start.idx,
                   model = exponential.model)
  return(to_return)
}
