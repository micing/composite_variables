# This function recalculates data from Eaker et al 2004 to use the relaxed group as reference 
# instead of the reported strained group. Since groups are split at the median, 
# the unexposed group is the same size as the job strain group and variance should be identical.


fixEaker2004 = function(data){
  
  cohorts = c("Eaker2004f", "Eaker2004m")
  
  for (c in cohorts) {
  
    d=data[data$cohort==c, ]
  
    r=d$relaxed2
    a=d$active2
    p=d$passive2
    s=d$strain2
    
    d$strain2 = s/r
    d$strain_l2 = exp(log(s/r) - (log(d$relaxed_u2)-log(d$relaxed_l2))/2)
    d$strain_u2 = exp(log(s/r) + (log(d$relaxed_u2)-log(d$relaxed_l2))/2)
    
    d$active2 = a/s/r
    d$active_l2 = exp(log(a/s/r) - (log(d$active_u2)-log(d$active_l2))/2)
    d$active_u2 = exp(log(a/s/r) + (log(d$active_u2)-log(d$active_l2))/2)
    
    d$passive2 = p/s/r
    d$passive_l2 = exp(log(p/s/r) - (log(d$passive_u2)-log(d$passive_l2))/2)
    d$passive_u2 = exp(log(p/s/r) + (log(d$passive_u2)-log(d$passive_l2))/2)
    
    d$relaxed2 = 1
    d$relaxed_l2 = NA
    d$relaxed_u2 = NA
    
    d$estimate = "RR"
  
    data[data$cohort==c, ]=d
    
  }
  return(data)
}