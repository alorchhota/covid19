progdir="/Users/ashissaha/github/covid"

cd "$progdir"
Rscript process_us_state_data.R   # TODO: make output file configurable
Rscript process_metadata.R        
# compute global R0 using data until 2020/03/20
Rscript compute_us_state_R0.R     # TODO: make inpute/output file configurable   
# compute time-dependent R0 using data until the last date available
Rscript compute_us_state_R0_TD.R  # TODO: make inpute/output file configurable

