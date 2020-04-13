library(ioutil)
library(miscutil)
library(stats)
library(R0)

state_cases_fn = "data/processed/us_state_time_series_cases_2020-04-11.txt"
min_count = 1
min_day = 5
covid19_generation_time_mean = 7.4
covid19_generation_time_sd = 5.2
covid19_generation_time_dist = "weibull"
R0_fit_fn = "data/processed/us_state_R0_TD_fit.txt"
R0_fit_plt_fn = "results/covid19_weather/us_state_R0_TD_fit_plot.pdf"

########## compute R0 ####################
state_cases_df = read_df(state_cases_fn)

### each state must have data for >= 5 days [min_day] after reaching >= 5 cases [min_count]
n_days = rowSums(state_cases_df >= min_count)
state_cases_df = state_cases_df[n_days>= min_day,]

### compute R0 and plot
pdf(R0_fit_plt_fn)
mfrow_orig = par("mfrow")
par(mfrow=c(3,3))
R0_fits = lapply(1:nrow(state_cases_df), function(ridx){
  state = rownames(state_cases_df)[ridx]
  print(sprintf("computing R0 for %s ...", state))
  all_cases = state_cases_df[ridx,]
  cases = all_cases[as.numeric(all_cases)>=min_count]
  
  all_cases_dates = as.Date(names(all_cases), format = "%m/%d/%y")
  cases_dates = as.Date(names(cases), format = "%m/%d/%y")
  
  days.R0 = length(cases)
  
  R0s = est.R0.TD(epid = as.numeric(cases),
                   GT = generation.time(covid19_generation_time_dist, 
                                        val = c(covid19_generation_time_mean, covid19_generation_time_sd)), 
                   t = cases_dates)
  to_ret = rep(NA, length(all_cases_dates))
  names(to_ret) = as.character(all_cases_dates)
  to_ret[names(R0s$R)] = R0s$R
  plot(R0s)
  legend("topright", state)
  
  return(list(days.R0 = days.R0,
              R0.TD = to_ret,
              R0.TD.est = R0s))
})
names(R0_fits) = rownames(state_cases_df)
par(mfrow=mfrow_orig)
dev.off()

R0_TD_df = as.data.frame(do.call(rbind, lapply(R0_fits, function(x) x$R0.TD)))
target_cols = c("Province_State", "days.R0",  colnames(R0_TD_df))
days.R0 = do.call(c, lapply(R0_fits, function(x) x$days.R0))
R0_TD_df$days.R0 = days.R0
R0_TD_df$Province_State = rownames(R0_TD_df)
R0_TD_df = R0_TD_df[,target_cols]

### save
write_df(R0_TD_df, file = R0_fit_fn, row.names = F, col.names = T)
