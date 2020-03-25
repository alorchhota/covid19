library(ioutil)
library(miscutil)
library(stats)
library(fiftystater)

source('utils/model_fit.R')

csse_data_dir = "/Users/ashissaha/github/COVID-19"
us_states_fn = "data/us_sates.txt"
fiftystater_states_fn = "data/us_sates_fiftystater.txt"
out_pfx = "results/spread_rate/us_spread_rate"
min_samples = 5

time_series_fn = sprintf("%s/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", csse_data_dir)

### read data
us_state_df = read_df(us_states_fn, header = T, row.names = F)
fiftystater_states_df = read_df(fiftystater_states_fn, header = T, row.names = F)
time_series_df = read_df(fn = time_series_fn, sep = ",", quote = "\"", header = T, row.names = F, check.names = F, stringsAsFactors = F)

### exponential fit and plot
state_growth_df = NULL
data_dates = as.Date(colnames(time_series_df)[5:ncol(time_series_df)], format = c("%m/%d/%y"))
fit_plt_fn = sprintf("%s_exponential_fit.pdf", out_pfx)
pdf(file = fit_plt_fn)
mfrow_orig = par("mfrow")
par(mfrow=c(3,3))
for(i in 1:nrow(time_series_df)){
  #print(sprintf("%s: %s - %s ", i, time_series_df[i,1], time_series_df[i,2]))
  state = time_series_df[i,1]
  if(time_series_df[i,2] != "US") 
    next
  if(! (state %in% us_state_df$state))
    next
  
  efit = tryCatch(fit_exponential(time_series_df[i,5:ncol(time_series_df)]), error = function(e) NULL)
  if(!is.null(efit)){
    df = data.frame(state = state, 
                    sample = efit$n.samples, 
                    sample.start.idx = efit$sample.start.idx, 
                    a0 = efit$a0, 
                    k = efit$k, 
                    pval_k = summary(efit$model)$coefficients[2,"Pr(>|t|)"],
                    stringsAsFactors = F)
    if(is.null(state_growth_df)){
      state_growth_df = df
    } else {
      state_growth_df = rbind(state_growth_df, df)
    }
    
    ## plot exponential fit
    plt_dates = data_dates[efit$sample.start.idx : (efit$sample.start.idx + efit$n.samples - 1)]
    observed_counts = time_series_df[i, 4 + efit$sample.start.idx: (efit$sample.start.idx + efit$n.samples - 1)]
    fitted_counts = exp(efit$model$fitted.values)
    plot(x = plt_dates,
         y = observed_counts,
         xlab = "Date", ylab = "Confirmed case", 
         main = sprintf("%s : Rate = %s", state, sprintf("%.2f",efit$k)),
         pch = 19)
    lines(plt_dates, fitted_counts,lwd=2, col = "red")
    legend("topleft", legend = c("Observed", "Fitted"), col = c("black", "red"), lty = c(0,1), pch = c(19,NA))
  }
  
}

par(mfrow=mfrow_orig)
dev.off()

### save growth rate data
growth_rate_out_fn = sprintf("%s.txt", out_pfx)
write_df(state_growth_df, file = growth_rate_out_fn)

### plot growth rate in map
plt_data = merge(x = state_growth_df, y = time_series_df[,c("Province/State", "Lat", "Long")], by.x = "state", by.y = "Province/State", all.x = T, all.y = F)
plt_data = plt_data[plt_data$sample>=min_samples, ]   # exclude estimates from small number of samples
plt_data = plt_data[!is.na(plt_data$pval_k) & plt_data$pval_k <= 0.05, ]    # exclude not-significant estimates
plt <- ggplot() + geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group),color="white", fill="grey92" ) +
  geom_point(data=plt_data, aes(x=Long, y=Lat, size = k, color=k)) +
  scale_size(name="", range = c(2, 7)) +
  guides(size=guide_legend("k")) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

pdf(file = sprintf("%s_map.pdf", out_pfx), width = 10, height = 6)
print(plt)
dev.off()
