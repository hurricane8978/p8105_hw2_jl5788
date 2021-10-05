homework2
================
hurricane
10/5/2021

``` r
library(tidyverse)
```

    ## ─ Attaching packages ──────────────────── tidyverse 1.3.1 ─

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ─ Conflicts ───────────────────── tidyverse_conflicts() ─
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

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

## Read and clean precipatation data for 2018 and 2019

``` r
precip2018_df=
  read_excel("data/trash_wheel_collection.xlsx",sheet=5,range="A2:B15") %>% 
  na.omit()
  

precip2019_df=
  read_excel("data/trash_wheel_collection.xlsx",sheet=4,range="A2:B15") %>% 
  na.omit()
```
