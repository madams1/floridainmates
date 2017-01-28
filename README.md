# floridainmates

This package provides access to a data set collected from the [Florida Department of Corrections](http://www.dc.state.fl.us/) describing the state's inmate population. Details on the current release are available [here](https://github.com/madams1/floridainmates/releases).


### Install
```r
devtools::install_github("madams1/floridainmates")
devtools::install_github("ropenscilabs/datastorr")
```

### Get and load the data
```r
inmate_data <- floridainmates::floridainmates() # downloads/loads data
```

### Data collection

Uses [rvest](https://github.com/hadley/rvest) to scrape data on each individual inmate, and `parallel::mclapply` to scrape inmate pages in parallel.

Most of the specifics can be found in [get_inmate_data.R](https://github.com/madams1/floridainmates/blob/master/get_inmate_data.R) and [R/get_data_functions.R](https://github.com/madams1/floridainmates/blob/master/R/get_data_functions.R).
