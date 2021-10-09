homework2
================
hurricane
10/5/2021

# problem 1

## Read and clean Mr.Trash Wheel sheet

``` r
trash_wheel=
  read_excel("data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx",range="A2:N535") %>%   
  janitor::clean_names() %>% 
  na.omit(dumpster) %>% 
  mutate(sports_balls=round(sports_balls,0)) 
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

The first sheet of Mr.Trash Wheel dataset includes 453 rows and 14
variables which provide us with information about weight of trash
collected by Mr.Trash Wheel from May 2014 to January 2021. Furthermore,
it lists weight of different types of trash like in seventh and eighth
columns which shows plastic\_bottles, polystyrene columns and so on.

Mr.Trash Wheel dataset also includes every month’s precipitation data
for 2018 and 2019, which has 18 rows and 3 columns.

The total precipitation in 2018 is 70.33 and the median number of sports
balls in a dumpster in 2019 is 9.

# Problem 2

### Import and clean pols-month data

``` r
pols_month_df=
  read_csv("data/fivethirtyeight_datasets/pols-month.csv") %>% 
  janitor::clean_names() %>% 
  separate(mon,into=c("year","month","day"),sep="-") %>%
  mutate(year=as.integer(year),month=as.integer(month),day=as.integer(day)) %>% 
  mutate(month.name[month]) %>%
  select(-month) %>% 
  rename("month"="month.name[month]") %>% 
  mutate(president=ifelse(prez_gop==1,"gop","dem")) %>%
  select(!prez_gop & !prez_dem & !day) %>% 
  mutate(month=substring(month,0,3),year=as.numeric(year)) %>% 
  select(year,month,everything()) %>% 
  mutate(year=as.integer(year))
```

### Import and clean snp data

``` r
snp_df=
  read_csv("data/fivethirtyeight_datasets/snp.csv") %>%
  mutate(date=lubridate::mdy(date)) %>% 
  separate(date,into=c("year","month","day"),sep="-") %>% 
  mutate(year=as.integer(year),month=as.integer(month),day=as.integer(day)) %>% 
  mutate(month.name[month])%>% 
  select(-month) %>% 
  rename("month"="month.name[month]") %>% 
  mutate(month=substring(month,0,3)) %>% 
  select(year,month,everything()) %>% 
  mutate(year=ifelse(year>2049,year-100,year)) %>% 
  select(year,everything(),-day) %>%
  arrange(year,month)
```

### Import and clean unemplyment data

``` r
unemployment_df=
  read_csv("data/fivethirtyeight_datasets/unemployment.csv") %>% 
  pivot_longer(2:13, names_to="month",values_to="unemployment_rate"
  ) %>% 
  janitor::clean_names() %>% 
  arrange(year,month) %>% 
  mutate(year=as.integer(year))
```

### Join three dataset together

``` r
merge_snp_unemployment_df1=
  left_join(pols_month_df,snp_df,by=c("year","month"))
  
merge_snp_unemployment_df2=
  left_join(merge_snp_unemployment_df1,unemployment_df,by=c("year","month"))
```

After cleaning, the file “pols-month” contains 822 observations of 9
variables related to the number of national politicians who are
democratic or republican from 1947 to 2015.

After cleaning, the file “snp” contains 787 observations of 3 variables
related to Standard & Poor’s stock market index (S&P), often used as a
representative measure of stock market as a whole.

After cleaning, the file “unemployment” contains 816 observations of 3
variables.

The three-joint dataset contains 822 observations of 11 variables from
1947 to 2015. The key variables include president, unemployment rate and
so on.

## Problem 3

### Clean baby name

``` r
baby_name_df=
  read_csv("data/Popular_Baby_Names.csv") %>% 
  janitor::clean_names() %>% 
  mutate(childs_first_name=tolower(childs_first_name)) %>% 
  mutate(childs_first_name=str_to_title(childs_first_name)) %>% 
  mutate(ethnicity = replace(ethnicity,ethnicity=="ASIAN AND PACI","ASIAN AND PACIFIC ISLANDER")) %>% 
  mutate(ethnicity = replace(ethnicity,ethnicity=="BLACK NON HISP","BLACK NON HISPANIC")) %>% 
  mutate(ethnicity = replace(ethnicity,ethnicity=="WHITE NON HISP","WHITE NON HISPANIC")) %>% 
  distinct() %>% 
  arrange(year_of_birth)
```

### Well-structured, reader-friendly table

``` r
baby_name_df=
  read_csv("data/Popular_Baby_Names.csv") %>% 
  janitor::clean_names() %>% 
  mutate(childs_first_name=tolower(childs_first_name)) %>% 
  mutate(childs_first_name=str_to_title(childs_first_name)) %>% 
  distinct() %>% 
  arrange(ethnicity,year_of_birth) %>% 
  filter(childs_first_name=="Olivia") %>% 
  mutate(ethnicity = replace(ethnicity,ethnicity=="ASIAN AND PACI","ASIAN AND PACIFIC ISLANDER")) %>% 
  mutate(ethnicity = replace(ethnicity,ethnicity=="BLACK NON HISP","BLACK NON HISPANIC")) %>% 
  mutate(ethnicity = replace(ethnicity,ethnicity=="WHITE NON HISP","WHITE NON HISPANIC")) %>% 
  select(-count,-gender,-childs_first_name) %>% 
pivot_wider(
    names_from = "year_of_birth",
    values_from = "rank"
    ) 
```

## a table showing the most popular name among male children over time

``` r
baby_name_df=
  read_csv("data/Popular_Baby_Names.csv") %>% 
  janitor::clean_names() %>% 
  mutate(childs_first_name=tolower(childs_first_name)) %>% 
  mutate(childs_first_name=str_to_title(childs_first_name)) %>% 
  distinct() %>% 
  arrange(ethnicity,year_of_birth) %>% 
  filter(gender=="MALE") %>% 
  mutate(ethnicity = replace(ethnicity,ethnicity=="ASIAN AND PACI","ASIAN AND PACIFIC ISLANDER")) %>% 
  mutate(ethnicity = replace(ethnicity,ethnicity=="BLACK NON HISP","BLACK NON HISPANIC")) %>% 
  mutate(ethnicity = replace(ethnicity,ethnicity=="WHITE NON HISP","WHITE NON HISPANIC")) %>% 
  arrange(year_of_birth) %>% 
  filter(rank=="1") %>% 
  select(-gender,-count,-rank) %>% 
  pivot_wider(
    names_from="year_of_birth",
    values_from="childs_first_name"
  ) 
```

## A scatter plot

``` r
baby_name_df=
  read_csv("data/Popular_Baby_Names.csv") %>% 
  janitor::clean_names() %>% 
  mutate(childs_first_name=tolower(childs_first_name)) %>% 
  mutate(childs_first_name=str_to_title(childs_first_name)) %>% 
  distinct() %>% 
  arrange(ethnicity,year_of_birth) %>% 
  mutate(ethnicity = replace(ethnicity,ethnicity=="ASIAN AND PACI","ASIAN AND PACIFIC ISLANDER")) %>% 
  mutate(ethnicity = replace(ethnicity,ethnicity=="BLACK NON HISP","BLACK NON HISPANIC")) %>% 
  mutate(ethnicity = replace(ethnicity,ethnicity=="WHITE NON HISP","WHITE NON HISPANIC")) %>% 
  arrange(year_of_birth) %>% 
  filter(gender=="MALE",ethnicity=="WHITE NON HISPANIC",year_of_birth=="2016") %>%
  select(childs_first_name,count,rank)
```

    ## Rows: 19418 Columns: 6

    ## ─ Column specification ────────────────────────────
    ## Delimiter: ","
    ## chr (3): Gender, Ethnicity, Child's First Name
    ## dbl (3): Year of Birth, Count, Rank

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#library(ggplot2)

ggplot(baby_name_df,aes(x=rank,y=count),main="Relation of Rank and count of Name  Scatter Plot") + 
  geom_point(size=0.5) +
  geom_rug(col="steelblue",alpha=0.1, size=1.5) + ggtitle("Relation of Rank and count of Name  Scatter Plot")
```

![](homework2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
