library("tidyverse")
library("googlesheets4")
library("metafor")

source("fixEaker2004.R")
source("fixNetterstrom2010.R")

# url = "https://docs.google.com/spreadsheets/d/1Ltx60VFZ5JNz5ICdNrXA3gFu9jD03AETltAm8zwDlUU/edit?usp=sharing"
# data = read_sheet(url, sheet="Dataset")
# save(data, file="data/job_strain_meta_data.RData")

load("data/job_strain_meta_data.RData")

##### Functions #####

## standard error from 95% confidence intervals of relative risks
se = function(lower, upper){
  (log(upper)-log(lower))/(qnorm(.975)*2)
}

# Perform meta-analyses on all terms in the job strain model
job_strain_meta = function(d, slab=d$study, method="REML") {
  demand_rma=rma(measure="RR", yi=log(d$demand_rr), sei=d$demand_se, slab=slab, method=method)
  control_rma=rma(measure="RR", yi=log(d$control_rr), sei=d$control_se, slab=slab, method=method)
  
  # NB: one study is based on additive estimates only, and lacks estimates for the interaction demandXcontrol
  d2= d %>% filter(!is.na(d$demandXcontrol_rr)) 
  slab2 = d2$study
  demandXcontrol_rma = list(ci.lb=NA, ci.ub=NA, b=NA)
  if (dim(d2)[1] > 0) {
    demandXcontrol_rma=rma(measure="RR", yi=log(d2$demandXcontrol_rr), sei=d2$demandXcontrol_se, slab=slab2, method=method)
  }
  
  list(
    n=sum(d$n),
    k=dim(d)[[1]],
    events=sum(d$events),
    demand=demand_rma,
    control=control_rma,
    demandXcontrol=demandXcontrol_rma)
}

meta_summary = function(m) {
  tibble(
    n=m$n,
    k=m$k,
    events=m$events,
    demand_rr=exp(m$demand$b),
    demand_lower=exp(m$demand$ci.lb),
    demand_upper=exp(m$demand$ci.ub),
    demand_se = se(demand_lower, demand_upper),
    control_rr = exp(m$control$b),
    control_lower = exp(m$control$ci.lb),
    control_upper = exp(m$control$ci.ub),
    control_se = se(control_lower, control_upper),
    demandXcontrol_rr=exp(m$demandXcontrol$b),
    demandXcontrol_lower=exp(m$demandXcontrol$ci.lb),
    demandXcontrol_upper=exp(m$demandXcontrol$ci.ub),
    demandXcontrol_se = se(demandXcontrol_lower, demandXcontrol_upper))
}

##### Prepare data #####

d=data %>% rename_with(tolower) %>% fixEaker2004() %>% fixNetterstrom2010() %>%
  gather(key, value, relaxed:strain_u2) %>% 
  mutate(adjusted = grepl("2", key), key = gsub("2", "", key)) %>%
  spread(key, value) %>% 
  rename(demand_rr=active, demand_lower = active_l, demand_upper=active_u, 
         control_rr=passive, control_lower=passive_l, control_upper=passive_u,
         strain_rr =strain, strain_lower = strain_l, strain_upper=strain_u) %>%
  mutate(demandXcontrol_rr = exp(log(strain_rr)-log(demand_rr)-log(control_rr))) %>%
  mutate(demand_se = se(demand_lower, demand_upper),
         control_se = se(control_lower, control_upper),
         demandXcontrol_se = sqrt(demand_se^2+control_se^2),
         demandXcontrol_lower = exp(log(demandXcontrol_rr) - demandXcontrol_se * qnorm(.975)),
         demandXcontrol_upper = exp(log(demandXcontrol_rr) + demandXcontrol_se * qnorm(.975))) %>%
  filter(estimate %in% c("RR", "RH", "HR" ) & !is.na(demand_rr) & !is.na(control_rr)) %>%
  select(study, order, n, events, adjusted, demand_rr:control_upper, demandXcontrol_rr:demandXcontrol_upper) %>% arrange(order) 

cohorts = d %>% group_by(study, adjusted) %>% summarise(k=n())

# pick fully adjusted estimates for all studies that have it, unadjusted (only sex + age) for the others 
d0 = d %>% left_join(cohorts) %>% filter(adjusted == 1 | 
                                           (adjusted == 0 & study %in% c("Reed1989","André-Petersson2007", "Kivimäki2012")))
d1 = d0 %>% filter(k > 1) 
d2 = d0 %>% filter(k == 1)

#### perform meta analysises at level 1 on all studies reporting more than one cohort ####
for (s in unique(d1$study)) {
  d2 = d2 %>% bind_rows(d1 %>% filter(study == s) %>% job_strain_meta() %>% meta_summary() %>% mutate(study = s))
}

#### Final meta-analysis and forest plots ####
d3 = d2 %>% select(-adjusted, -k, -order) %>% 
  left_join(d0 %>% group_by(study) %>% filter(row_number()==1) %>% select(study, adjusted, order, k), by="study") %>% arrange(order)

meta = d3 %>% job_strain_meta()

op=par(no.readonly=TRUE) 
png(filename = "png/figure_3.png", width = 700, height = 2000, res=300)
par(mfrow=c(3,1), mar=c(3,0,3,0))
meta$demandXcontrol %>% forest(atransf = exp, main="\nJob strain interaction\n(demandXcontrol)")
meta$demand %>% forest(atransf = exp, main="\n\nHigh job demand")
meta$control %>% forest(atransf = exp, main="\n\nLow job control")
par(op)
dev.off()

meta %>% meta_summary()


