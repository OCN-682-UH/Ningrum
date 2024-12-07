Homework Week 13 - For Loop and Map
================
Retno K. Ningrum
2024-12-07

``` r
library(tidyverse)
library(here)
library(purrr)
library(dplyr)
```

\###read data

``` r
df1 <- read_csv(here("week13", "week13_homework", "data", "TP1.csv"))

df2 <- read_csv(here("week13", "week13_homework", "data", "TP2.csv"))

df3 <- read_csv(here("week13", "week13_homework", "data", "TP3.csv"))

df4 <- read_csv(here("week13", "week13_homework", "data", "TP4.csv"))
```

### check the data

``` r
glimpse(df1)
```

    ## Rows: 5,981
    ## Columns: 7
    ## $ PoolID          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ Foundation_spp  <chr> "Phyllospadix", "Phyllospadix", "Phyllospadix", "Phyll…
    ## $ Removal_Control <chr> "Control", "Control", "Control", "Control", "Control",…
    ## $ Date.Time       <chr> "6/16/19 0:01", "6/16/19 0:16", "6/16/19 0:31", "6/16/…
    ## $ Temp.C          <dbl> 10.21, 10.08, 10.16, 10.12, 10.08, 10.08, 10.04, 10.04…
    ## $ Intensity.lux   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ LoggerDepth     <dbl> 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, …

``` r
glimpse(df2)
```

    ## Rows: 5,974
    ## Columns: 7
    ## $ PoolID          <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    ## $ Foundation_spp  <chr> "Phyllospadix", "Phyllospadix", "Phyllospadix", "Phyll…
    ## $ Removal_Control <chr> "Removal", "Removal", "Removal", "Removal", "Removal",…
    ## $ Date.Time       <chr> "6/16/19 0:14", "6/16/19 0:29", "6/16/19 0:44", "6/16/…
    ## $ Temp.C          <dbl> 10.12, 10.12, 10.08, 9.91, 9.95, 9.78, 9.56, 9.56, 9.3…
    ## $ Intensity.lux   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ LoggerDepth     <dbl> 0.295, 0.295, 0.295, 0.295, 0.295, 0.295, 0.295, 0.295…

``` r
glimpse(df3)
```

    ## Rows: 5,972
    ## Columns: 7
    ## $ PoolID          <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, …
    ## $ Foundation_spp  <chr> "Phyllospadix", "Phyllospadix", "Phyllospadix", "Phyll…
    ## $ Removal_Control <chr> "Removal", "Removal", "Removal", "Removal", "Removal",…
    ## $ Date.Time       <chr> "6/16/19 0:14", "6/16/19 0:29", "6/16/19 0:44", "6/16/…
    ## $ Temp.C          <dbl> 10.12, 10.08, 10.04, 9.95, 9.78, 9.69, 9.48, 9.39, 9.2…
    ## $ Intensity.lux   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ LoggerDepth     <dbl> 0.265, 0.265, 0.265, 0.265, 0.265, 0.265, 0.265, 0.265…

``` r
glimpse(df4)
```

    ## Rows: 5,995
    ## Columns: 7
    ## $ PoolID          <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, …
    ## $ Foundation_spp  <chr> "Phyllospadix", "Phyllospadix", "Phyllospadix", "Phyll…
    ## $ Removal_Control <chr> "Control", "Control", "Control", "Control", "Control",…
    ## $ Date.Time       <chr> "6/16/19 0:10", "6/16/19 0:25", "6/16/19 0:40", "6/16/…
    ## $ Temp.C          <dbl> 10.25, 10.21, 10.12, 9.99, 9.99, 9.82, 9.65, 9.44, 9.3…
    ## $ Intensity.lux   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ LoggerDepth     <dbl> 0.095, 0.095, 0.095, 0.095, 0.095, 0.095, 0.095, 0.095…

## Direction

bring in all 4 files and calculate the mean and standard deviation of
both temperature (Temp.C) and light (Intensity.lux) for each tide pool.
Use both a for loop and map() functions in your script. (Basically, do
it twice).

### Using For Loop

``` r
#create a list consist with all data frame we want to calculate
all_df_list <- list(df1, df2, df3, df4)

#initialize an empty list to store the result
results_for_loop <- list()

#calculate the mean and standart deviation using for loop 
for (i in seq_along(all_df_list)) {
  df <- all_df_list[[i]]                              #assign "df" with all data in all_df_list
  mean_temp <- mean(df$Temp.C, na.rm = TRUE)          #calculate mean temp of all df in all_df_list
  mean_light <- mean(df$Intensity.lux, na.rm = TRUE)  #calculate mean light of all df in all_df_list
  sd_temp <- sd(df$Temp.C, na.rm = TRUE)              #calculate sd temp of all df in all_df_list
  sd_light <- sd(df$Intensity.lux, na.rm = TRUE)      #calculate sd light of all df in all_df_list
  
  results_for_loop[[i]] <- data.frame(   #store result in a data frame named result_for_loop
    Pool_ID  = unique(df$PoolID), #consist with Pool_ID, assign it with the character in PoolId
    Mean_Temp = mean_temp,        #consist with Mean_Temp, assign to mean temp calculated in each df
    SD_Temp = sd_temp,            #consist with SD_Temp, assign to SD temp calculated in each df
    mean_light = mean_light,      #consist with Mean_Light, assign to mean light calculated in each df
    SD_Light = sd_light)          #consist with SD_Light, assign to SD light calculated in each df
}


#create new data frame to combine the for loop result
final_loop <- do.call(rbind,    #execute a function call, with rbind (stack one on top of the other)
                      results_for_loop)  #using the result_for_loop data frame

#check the data
final_loop
```

    ##   Pool_ID Mean_Temp  SD_Temp mean_light  SD_Light
    ## 1       1  13.27092 2.324037   426.8262  1660.553
    ## 2       2  13.17498 2.308312  5603.1554 11928.919
    ## 3       3  13.10386 2.317297  5605.0705 12101.099
    ## 4       4  13.22150 2.269209   655.1037  2088.839

### Using Purr Package

``` r
#calculate the mean and standart deviation using for loop 
final_purr <- map(all_df_list, ~ {   #create new dataset, by making a list of all_df_list
  df <- .x                           #assign the df with current element data frame (the list)
  data.frame(                        #create new data frame 
    Pool_ID = unique(df$PoolID),     #by using unique character in PoolID of the list
    Mean_Temp = mean(df$Temp.C, na.rm = TRUE), #by calculate mean temp each data in the list
    SD_Temp = sd(df$Temp.C, na.rm = TRUE),     #by calculate sd temp each data in the list
    mean_light = mean(df$Intensity.lux, na.rm = TRUE), #by calculate mean light each data in the list
    SD_light = sd(df$Intensity.lux, na.rm = TRUE) #by calculate sd light each data in the list
  )
})

#the result of map() will be 4 set of dataframe along with the calculated mean and sd of temp and light. then we can combine all these 4 dataframe with bind_rows
final_purr <-bind_rows(final_purr)

#check the data
final_purr 
```

    ##   Pool_ID Mean_Temp  SD_Temp mean_light  SD_light
    ## 1       1  13.27092 2.324037   426.8262  1660.553
    ## 2       2  13.17498 2.308312  5603.1554 11928.919
    ## 3       3  13.10386 2.317297  5605.0705 12101.099
    ## 4       4  13.22150 2.269209   655.1037  2088.839
