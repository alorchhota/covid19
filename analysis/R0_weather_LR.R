library(ioutil)
library(miscutil)
library(stats)
library(corrplot)
library(ggplot2)

r0_fn = "data/processed/us_state_R0_fit.txt"
cov_fn = "data/processed/us_state_meta.txt"
out_pfx = "results/covid19_weather/R0_weather"

r0_df = read_df(r0_fn, header = T, row.names = F)
cov_df = read_df(cov_fn, header = T, row.names = F)

combined_df = merge(r0_df, cov_df, by.x = "Province_State",  by.y = "state", all = F)
lmfit = lm(data = combined_df, 
           R0.mean ~ march_1_15_temp + march_1_15_humidity + 
                    log(population_density) + log(GDP_per_capita) + days.R0)
summary(lmfit)

saveRDS(lmfit, file = sprintf("%s_lmfit.rds", out_pfx))

corr = cor(combined_df[,c('R0.EG', 'R0.ML', 'R0.TD', 'R0.TD3', 'R0.SB', 'R0.SB3', 'R0.mean', 
                          'days.R0', 'population', 'land_area_sqmi', 'Total_GDP_USD_2019_Q3', 
                          'march_1_15_temp', 'march_1_15_humidity', 
                          'population_density', 'GDP_per_capita')])

pdf(sprintf("%s_corrplot.pdf", out_pfx))
corrplot(corr)
plot(combined_df$march_1_15_temp, combined_df$R0.mean)
plot(combined_df$march_1_15_humidity, combined_df$R0.mean)
plot(log(combined_df$population_density), combined_df$R0.mean)
plot(log(combined_df$GDP_per_capita), combined_df$R0.mean)
plot(combined_df$days.R0, combined_df$R0.mean)
dev.off()

