library(ioutil)
library(miscutil)

state_fn = "data/us_sates.txt"
population_fn = "data/us_population.csv"
area_fn = "data/us_area.txt"
gdp_fn = "data/us_gdp_2019_Q3.csv"
temp_fn = "data/state_temp.csv"
humidity_fn = "data/state_humidity.csv"
meta_fn = "data/processed/us_state_meta.txt"

state_df = read_df(state_fn, header = T, row.names = F)
population_df = read_df(population_fn, sep = ",", header = T, row.names = F, quote = '"')
area_df = read_df(area_fn, sep = "\t", header = T, row.names = F)
gdp_df = read_df(gdp_fn, header = T, row.names = F)

temp_df = read_df(temp_fn, sep = ',', header = T, row.names = F)
temp_dates = as.Date(colnames(temp_df), format = "%Y-%m-%d")
temp_df$march_1_15_temp = rowMeans(temp_df[, (temp_dates>= as.Date("2020-03-01")) & 
                                          (temp_dates <= as.Date("2020-03-15")) & 
                                          (!is.na(temp_dates)) ], 
                                   na.rm = T)
 
humidity_df = read_df(humidity_fn, sep = ',', header = T, row.names = F)
humidity_dates = as.Date(colnames(humidity_df), format = "%Y-%m-%d")
humidity_df$march_1_15_humidity = rowMeans(humidity_df[, (humidity_dates>= as.Date("2020-03-01")) & 
                                                  (humidity_dates <= as.Date("2020-03-15")) & 
                                                  (!is.na(humidity_dates)) ], 
                                           na.rm = T)


meta_df = merge(state_df, population_df[,c('State', '2019')], by.x = "state", by.y = "State", all = F)
meta_df = merge(meta_df, area_df[,c('state', 'land_area_sqmi')], by.x = "state", by.y = "state", all = F)
meta_df = merge(meta_df, gdp_df[,c('State/Region', 'Total_GDP_USD_2019_Q3')], by.x = "state", by.y = "State/Region", all = F)
meta_df = merge(meta_df, temp_df[,c('name', 'march_1_15_temp')], by.x = "state", by.y = "name", all = F)
meta_df = merge(meta_df, humidity_df[,c('name', 'march_1_15_humidity')], by.x = "state", by.y = "name", all = F)

colnames(meta_df) = gsub(pattern = "^2019$", replacement = "population", colnames(meta_df))
meta_df$population = as.numeric(gsub(",","",meta_df$population))
meta_df$land_area_sqmi = as.numeric(gsub(",","",meta_df$land_area_sqmi))
meta_df$population_density = meta_df$population / meta_df$land_area_sqmi
meta_df$Total_GDP_USD_2019_Q3 = as.numeric(gsub(",","", meta_df$Total_GDP_USD_2019_Q3))
meta_df$GDP_per_capita = meta_df$Total_GDP_USD_2019_Q3 / meta_df$population * 1e6

write_df(meta_df, file = meta_fn, row.names = F, col.names = T)
