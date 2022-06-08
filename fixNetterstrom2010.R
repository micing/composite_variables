# This is an approximate transformation of odds ratios reported by Netterstrom (2010) to relative risks. 
# It assumes that the null hypothesis is true and that the unadjusted baseline risk for the whole sample 
# also is the risk for all unexposed groups.

fixNetterstrom2010 = function(data) {

  RR = function(OR, p) {
  OR/(1-p+(p*OR))
  }
  
  toFix = c("active", "active_l", "active_u",
            "passive", "passive_l", "passive_u",
            "strain", "strain_l", "strain_u",
            "active2", "active_l2", "active_u2",
            "passive2", "passive_l2", "passive_u2",
            "strain2", "strain_l2", "strain_u2") 

  cohorts = c("Netterstrøm2010m", "Netterstrøm2010f")

  for (c in cohorts) {
    p=data[data$cohort==c, "events" ] / data[data$cohort==c, "n" ]
    for (f in toFix) {
      data[data$cohort==c, f ] = RR(data[data$cohort==c, f ], p)
    } 
    data[data$cohort==c, "estimate"] = "RR"
  }
  return(data)
}

