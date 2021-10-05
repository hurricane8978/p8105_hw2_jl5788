homework2
================
hurricane
10/5/2021

# problem 1

## Read and clean Mr.Trash Wheel sheet

``` r
trash_wheel=
  read_excel("data/trash_wheel_collection.xlsx",range="A2:N408") %>%   janitor::clean_names() %>% 
  na.omit(dumpster) %>% 
  mutate_at(vars(sports_balls),funs(round(sports_balls,0)))
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
  #mutate(sports_balls=round(pull(trash_wheel,sports_balls)))
```

## Read and clean precipatation data for 2018 and 2019

``` r
precip2018_df=
  read_excel("data/trash_wheel_collection.xlsx",sheet=5,range="A2:B15") %>% 
  janitor::clean_names() %>% 
  na.omit() %>% 
  mutate(year="2018") %>% 
  select(year,everything())
  

precip2019_df=
  read_excel("data/trash_wheel_collection.xlsx",sheet=4,range="A2:B15") %>% 
  janitor::clean_names() %>% 
  na.omit() %>% 
  mutate(year="2019") %>% 
  select(year,everything())

precip_2018_2019_df=bind_rows(precip2018_df,precip2019_df) %>% 
  janitor::clean_names() %>% 
  mutate(month.name[month]) %>% 
  select(!month) %>% 
  rename("month"="month.name[month]") %>% 
  select(year,month,total) 
```

# Problem 2

``` r
pols_month_df=
  read_csv("data/fivethirtyeight_datasets/pols-month.csv") %>% 
  janitor::clean_names() %>% 
  separate(mon,into=c("year","month","day"),sep="-") %>% 
  mutate(month=month.name[as.numeric(month)]) %>%            pivot_longer(prez_gop:rep_dem,names_to="president",values_to ="value") %>%
  select(!day) %>% 
  filter(!president == "prez_gop",!president == "prez_dem")
```

    ## Rows: 822 Columns: 9

    ## ─ Column specification ────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp_df=
  read_csv("data/fivethirtyeight_datasets/snp.csv") %>% 
  separate(date,into=c("month","day","year"),sep="/") %>% 
  mutate(month=month.name[as.numeric(month)]) %>% 
  select(year,everything()) 
```

    ## Rows: 787 Columns: 2

    ## ─ Column specification ────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
