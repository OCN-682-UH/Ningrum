Tidy Tuesday : Monster Movie
================
Retno K. Ningrum
2024-11-06

## Load all Libraries

``` r
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gganimate)
library(tidytext)
library(ggwordcloud)
library(here)
```

## Read and View Data

``` r
monster_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-29/monster_movies.csv')

glimpse(monster_movies)
```

    ## Rows: 630
    ## Columns: 10
    ## $ tconst          <chr> "tt0016123", "tt0023236", "tt0031208", "tt0033879", "t…
    ## $ title_type      <chr> "movie", "movie", "movie", "movie", "movie", "movie", …
    ## $ primary_title   <chr> "The Monster", "The Monster Walks", "The Human Monster…
    ## $ original_title  <chr> "The Monster", "The Monster Walks", "The Dark Eyes of …
    ## $ year            <dbl> 1925, 1932, 1939, 1941, 1941, 1942, 1942, 1942, 1944, …
    ## $ runtime_minutes <dbl> 86, 57, 73, 59, 65, 77, 73, 63, 86, 62, 61, 295, 201, …
    ## $ genres          <chr> "Comedy,Horror,Mystery", "Horror,Mystery", "Crime,Dram…
    ## $ simple_title    <chr> "the monster", "the monster walks", "the human monster…
    ## $ average_rating  <dbl> 6.2, 4.1, 5.7, 6.1, 6.0, 3.5, 6.1, 6.1, 5.7, 4.8, 4.9,…
    ## $ num_votes       <dbl> 1412, 1120, 1579, 1953, 799, 1969, 1938, 1588, 504, 12…

## Data Cleaning and Manipulation

After looking the data and find my interest, I am deciding to create a
wordcloud. I want to know what words mostly people used to create a
monster-genre movie. Also, I want to see whether the wording has changed
overtime by using gganimate().

``` r
#Data Cleaning and Manipulation
clean_df <- monster_movies %>%             #create new clean dataset
  select(primary_title, year) %>%          #only select these two variables
  unnest_tokens(output = word,             #split column into token, using word (default)
                input = primary_title) %>% #using data in "primary_title"
  count(year, word, sort = TRUE) %>%       #count by grouping in year and word
  filter(str_length(word)>3)               #filter only use data with >3 characters
  
glimpse(clean_df)      #check overall data
```

    ## Rows: 1,276
    ## Columns: 3
    ## $ year <dbl> 2015, 2021, 2018, 2019, 2020, 2013, 2014, 2016, 2012, 2023, 2003,…
    ## $ word <chr> "monster", "monster", "monster", "monster", "monster", "monster",…
    ## $ n    <int> 20, 20, 19, 19, 17, 15, 15, 15, 14, 14, 13, 12, 12, 11, 11, 11, 1…

## Creating Plot

``` r
#Create the Wordcloud
wordcloud <- ggplot(clean_df,         #use ggplot, with clean_df dataset
                    aes(label = word, #use only the label
                        size = n,     #size each label following the n column (size or total)
                        color = n)) + #color by "n"
  geom_text_wordcloud_area() +        #Create word cloud with `ggwordcloud`
  scale_size_area(max_size = 80) +    #Scale word sizes (adjust max_size as needed)
  scale_color_gradient(low = "blue",  #Set the color, where low "n" is blue
                       high = "red") +  #high "n" is red
  theme_minimal() +                     #use this theme
  theme(plot.title = element_text(size = 20, hjust = 0.5),      #set the title size and position
        plot.subtitle = element_text(size = 16, hjust = 0.5)) + #set the subtitle size & position
  labs(title = "Word Most Used in Monster Movie",  #the title
       subtitle = "Year: {closest_state}") +       #the subtitle, following the year
  transition_states(year, transition_length = 5,  #set the relative length of transition
                    state_length = 2,             #set the relative pause at the sttes
                    wrap = FALSE)                 #do not wrap the animation

wordcloud
```

<img src="../output/monster_movie_wordcloud -1.gif" style="display: block; margin: auto;" />

``` r
#assign the output folder
#output <- here("tidytuesday", "tidytuesday_0_monster", "output", "animate(wordcloud, monster_movie_wordcloud.gif")

#render the animation
#animate(wordcloud, nframes = 200,           #use data wordcloud, set the frame into 200
#        fps = 2, width = 800,               #set only 2 frame per seconds, set width 800
#        height = 600,                       #set height 600
#        renderer = gifski_renderer(output)) #render it to the output folder
```

## Something New I learned :

1.  Learned to use unnest_tokens() from the tidytext package to split a
    text column into individual word tokens, making it easier to analyze
    word frequency in titles.  
2.  Discovered how to use geom_text_wordcloud_area() from the
    ggwordcloud package in ggplot2 to visualize word frequency as a word
    cloud, with word size representing frequency.  
3.  experience setting output options for animations in R, specifically
    using animate() to control speed, dimensions, and format (e.g.,
    .gif) of the animation.  
4.  Realized that animate() code may interfere with knitting in R
    Markdown. To resolve this, I commented out the animate() code during
    knitting (using \#), then ran the code separately to produce the
    animation file with the desired speed and settings.
