library(ioutil)

########## settings ####################
us_states_fn = "data/us_sates.txt"
csse_data_dir = "/Users/ashissaha/github/COVID-19"
state_level_time_series_last_day = as.Date('03/21/2020', format = '%m/%d/%Y')

time_series_dir = sprintf("%s/csse_covid_19_data/csse_covid_19_time_series", csse_data_dir)
time_series_cases_fn = sprintf("%s/time_series_covid19_confirmed_US.csv", time_series_dir)
time_series_deaths_fn = sprintf("%s/time_series_covid19_deaths_US.csv", time_series_dir)

state_level_time_series_dir = sprintf("%s/archived_data/archived_time_series", csse_data_dir)
state_level_time_series_cases_fn = sprintf("%s/time_series_19-covid-Confirmed_archived_0325.csv", state_level_time_series_dir)
state_level_time_series_deaths_fn = sprintf("%s/time_series_19-covid-Deaths_archived_0325.csv", state_level_time_series_dir)

########## aggrgate cases and deaths ####################
### read data
us_states_df = read_df(us_states_fn, header = T, row.names = F)

time_series_cases_df = read_df(time_series_cases_fn, sep = ',', header = T, row.names = F, quote = '"')
time_series_deaths_df = read_df(time_series_deaths_fn, sep = ',', header = T, row.names = F, quote = '"')
time_series_cases_df = time_series_cases_df[time_series_cases_df$Province_State %in% us_states_df$state,,drop=F]
time_series_deaths_df = time_series_deaths_df[time_series_deaths_df$Province_State %in% us_states_df$state,,drop=F]
stopifnot(all(rownames(time_series_cases_df) == rownames(time_series_deaths_df)))

state_level_time_series_cases_df = read_df(state_level_time_series_cases_fn, sep = ',', header = T, row.names = F, quote = '"')
state_level_time_series_deaths_df = read_df(state_level_time_series_deaths_fn, sep = ',', header = T, row.names = F, quote = '"')
colnames(state_level_time_series_cases_df) = gsub(pattern = "Province/State", replacement = "Province_State", x = colnames(state_level_time_series_cases_df))
colnames(state_level_time_series_deaths_df) = gsub(pattern = "Province/State", replacement = "Province_State", x = colnames(state_level_time_series_deaths_df))
state_level_time_series_cases_df = state_level_time_series_cases_df[state_level_time_series_cases_df$Province_State %in% us_states_df$state,,drop=F]
state_level_time_series_deaths_df = state_level_time_series_deaths_df[state_level_time_series_deaths_df$Province_State %in% us_states_df$state,,drop=F]
stopifnot(all(rownames(state_level_time_series_cases_df) == rownames(state_level_time_series_deaths_df)))


### latest date
time_series_dates = lapply(colnames(time_series_cases_df), function(cn){
  tryCatch(as.Date(cn, format = "%m/%d/%Y"), error = function(e){return(NA)} )
})
date_start_col = min(which(!is.na(time_series_dates), arr.ind = T))
date_end_col = max(which(!is.na(time_series_dates), arr.ind = T))
latest_date = time_series_dates[[date_end_col]]
stopifnot(date_end_col == ncol(time_series_cases_df))
stopifnot(sum(!is.na(time_series_dates)) == date_end_col - date_start_col + 1)

state_cases_fn = sprintf("data/processed/us_state_time_series_cases_%s.txt", latest_date)
state_deaths_fn = sprintf("data/processed/us_state_time_series_deaths_%s.txt", latest_date)


### aggregate state counts
combine_state_counts <- function(county_counts_df, date_start_col, date_end_col){
  per_state_counts = sapply(date_start_col:date_end_col, function(col_idx){
    tapply(county_counts_df[,col_idx], INDEX = county_counts_df$Province_State, FUN = sum)
  })
  per_state_count_df = as.data.frame( cbind( data.frame(Province_State=rownames(per_state_counts), stringsAsFactors = F), as.data.frame(per_state_counts) ), stringsAsFactors = F)
  colnames(per_state_count_df) = c("Province_State", colnames(county_counts_df)[date_start_col:date_end_col])
  return(per_state_count_df)
}

state_cases_df = combine_state_counts(time_series_cases_df, date_start_col, date_end_col)
state_deaths_df = combine_state_counts(time_series_deaths_df, date_start_col, date_end_col)

### manual data correction
state_cases_df["Rhode Island", "3/12/2020"] = 5
state_cases_df["South Dakota", "3/12/2020"] = 8


### combine old state level data and new aggregated state-level data
state_level_target_cols = c(which(colnames(state_level_time_series_cases_df) == "Province_State"), 
                            which(as.Date(colnames(state_level_time_series_cases_df), format = '%m/%d/%y') <= state_level_time_series_last_day ))
state_level_time_series_cases_df = state_level_time_series_cases_df[ ,state_level_target_cols, drop=F ]
state_level_time_series_deaths_df = state_level_time_series_deaths_df[ ,state_level_target_cols, drop=F ]

state_target_cols = c(which(colnames(state_cases_df) == "Province_State"), 
                            which(as.Date(colnames(state_cases_df), format = '%m/%d/%y') > state_level_time_series_last_day ))
state_cases_df = state_cases_df[, state_target_cols, drop = F]
state_deaths_df = state_deaths_df[, state_target_cols, drop = F]

combined_states_df = merge(state_level_time_series_cases_df, state_cases_df, by = "Province_State")
combined_deaths_df = merge(state_level_time_series_deaths_df, state_deaths_df, by = "Province_State")

# save into file
write_df(combined_states_df, file = state_cases_fn, row.names = F, col.names = T)
write_df(combined_deaths_df, file = state_deaths_fn, row.names = F, col.names = T)

