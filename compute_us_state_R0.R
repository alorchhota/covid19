library(ioutil)
library(miscutil)
library(stats)
library(R0)

state_cases_fn = "data/processed/us_state_time_series_cases_0020-04-11.txt"
min_count = 5
min_day = 5
covid19_generation_time_mean = 4.7
covid19_generation_time_sd = 2.9
covid19_generation_time_dist = "lognormal"
last_analysis_day = as.Date('3/20/20', format = "%m/%d/%y")
R0_fit_fn = "data/processed/us_state_R0_fit.txt"
R0_fit_plt_fn = "results/covid19_weather/us_state_R0_fit_plot.pdf"

########## compute R0 ####################
state_cases_df = read_df(state_cases_fn)

### use data until 3/20/2020
col_dates = as.Date(colnames(state_cases_df), format = "%m/%d/%y")
state_cases_df = state_cases_df[,col_dates <= last_analysis_day]

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
  
  R0s = estimate.R(epid = as.numeric(cases), 
                   GT = generation.time(covid19_generation_time_dist, 
                                        val = c(covid19_generation_time_mean, covid19_generation_time_sd)), 
                   t = cases_dates,
                   methods = c("EG", "ML", "TD", "SB"))
  
  R0.EG = R0s$estimates[['EG']]$R
  R0.ML = R0s$estimates[['ML']]$R
  
  R0.TDs = R0s$estimates[['TD']]$R
  R0.TDs = R0.TDs[R0.TDs>0]
  R0.TD = as.numeric(R0.TDs[length(R0.TDs)])
  R0.TD3 = mean(as.numeric(R0.TDs[max(length(R0.TDs)-2, 0):length(R0.TDs)]))
  
  R0.SBs = R0s$estimates[['SB']]$R
  R0.SBs = R0.SBs[R0.SBs>0]
  R0.SB = as.numeric(R0.SBs[length(R0.SBs)])
  R0.SB3 = mean(as.numeric(R0.SBs[max(length(R0.SBs)-2, 0):length(R0.SBs)]))
  
  ### plot 
  plot(x = all_cases_dates[all_cases>0],
       y = all_cases[all_cases>0],
       xlab = "", ylab = "Confirmed cases", 
       main = sprintf("%s : R0.EG = %.2f", state, R0.EG),
       pch = 19)
  lines(cases_dates[as.integer(names(R0s$estimates[['EG']]$pred))], R0s$estimates[['EG']]$pred, lwd=2, col = "red")
  legend("topleft", legend = c("Observed", "Fitted"), col = c("black", "red"), lty = c(0,1), pch = c(19,NA))
  
  return(list(days.R0 = days.R0,
              R0.EG = R0.EG,
              R0.ML = R0.ML,
              R0.TD = R0.TD,
              R0.TD3 = R0.TD3,
              R0.SB = R0.SB,
              R0.SB3 = R0.SB3))
})
names(R0_fits) = rownames(state_cases_df)
par(mfrow=mfrow_orig)
dev.off()

R0_fit_df = do.call(rbind, R0_fits)
R0_fit_df = cbind(data.frame(Province_State = rownames(R0_fit_df), stringsAsFactors = F), R0_fit_df)
for(cidx in 2:ncol(R0_fit_df)){
  R0_fit_df[,cidx] = as.numeric(R0_fit_df[,cidx])
}
R0_fit_df$R0.mean = rowMeans(R0_fit_df[,c("R0.EG", "R0.ML", "R0.TD3", "R0.SB3")])


### save
write_df(R0_fit_df, file = R0_fit_fn, row.names = F, col.names = T)
