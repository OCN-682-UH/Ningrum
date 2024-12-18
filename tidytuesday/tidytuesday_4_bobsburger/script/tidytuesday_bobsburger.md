Tidy Tuesday - Bob’s Burger
================
Retno K. Ningrum
2024-11-25

Load all libraries

``` r
library(tidyverse)  #use ggplot and dplyr package 
library(gridExtra)  #to combine 2 plot in one frame
library(viridis)    #color choice pallate
library(hrbrthemes) #theme I used to create the plot
library(ggridges)   #to create the ridge plot
library(grid)       #to add annotation
```

Read the data

``` r
episode_metrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-19/episode_metrics.csv')
```

Manipulate the data

``` r
#Rename season category
episode_metrics <- episode_metrics %>%
  #change the name category by adding "season " then the value in season
  mutate(season = paste0("season ", season))  

#Because I want the season 1 to 14 organized following the value 1 to 14, then I have to change the data type into factor then arrange their level.
episode_metrics$season <- factor(episode_metrics$season, levels = c("season 1", "season 2", "season 3", "season 4", "season 5", "season 6", "season 7", "season 8", "season 9", "season 10", "season 11", "season 12", "season 13", "season 14"))
```

Looking at the data, I want to create two plots and present those two
plots into one output figures using new package **GridExtra()**

``` r
#create the plot
unique_words <- episode_metrics %>%       #use data in episode_metrics
  #add ggplot frame
  ggplot(aes(x = unique_words,            #assign x axis with unique_words
             y = season,                  #assign y axis with season
             fill = season)) +            #set different color each season
  #add geom boxplot
  geom_boxplot(outlier.shape = NA) +      #do not include the outlier
  #add point of real data using jitter type (point spread out)
  geom_jitter( color = "brown",     #set point data color 
               size = 0.8) +        #set the size of point
  #use the viridis_d pallete color for the "fill" in season
  scale_fill_viridis_d() +
  #use the theme_minimal() for the plot theme
  theme_minimal() +
  #customize the theme
  theme(
    legend.position = "none",                           #do not add any legend
    plot.title = element_text(hjust = 0.5,              #set the title position into center
                              face = "bold",            #set bold for title
                              size = 11,                #set size for title
                              margin = margin(b = 20)), #add margin between title to plot
    axis.title.y = element_blank()) +         #remove y axis title        
  #assign the wording for title and x/y axis  
  labs( 
      title = "Unique Words per Episode",  #write the title
      x = "Total Unique Words") +          #write the x axis label
  #adjust the frame 
  xlim(NA,1250)     #set the maximum value of x axis


ratio <- episode_metrics %>%  #use this data
  ggplot() +                  #assign ggplot frame
  #create ridges density plot
  geom_density_ridges(
    aes(x = question_ratio,        #assign x is the question_ratio
        y = season,                #assign y is the season
        fill = "question"),  #set the color as "question_color" pallete 
    alpha = 0.5,                   #set the transparency, because data might overlap
    scale = 3                      #set the scale of ridges
  ) +
  #create the 2nd density ridges plot
  geom_density_ridges(
    aes(x = exclamation_ratio,       #assign x is exclamation_ratio
        y = season,                  #assign y is season
        fill = "exclamation"), #set the color as "exclamation_color"
    alpha = 0.5,                     #set the transparency
    scale = 3                        #set the scale of ridges
  ) +
  #now assign the color manually
  scale_fill_manual(
    values = c("question" = "palegreen1",  #set the color in question_color
               "exclamation" = "yellow3"), #set the color in exclamation_color
    name = "") +                          #not adding any title on the fill (not necessary) 
  #assign the wording for title and x/y axis
  labs(
    title = "The Ratio of Question and Exclamation",  #set the title
    x = "ratio") +                                                         #set the x axis
  #use theme_minimal() as the type of the plot
  theme_minimal() +
  #customize the title
  theme(
    plot.title = element_text(hjust = 0.5,           #for title, put in the center alignment
                              face = "bold",         #then bold the title
                              size = 11,             #then set the size
                              margin = margin(b=20)),#add margin between title to plot
     axis.title.y = element_blank())                 #do not add any words in y axis

#now is to create annotation (the source of the data)
source_text <- textGrob(
  "Source: Data Science Learning Community (2024). Tidy Tuesday: A weekly social data project. https://tidytues.day", 
  gp = gpar(fontsize = 10, fontface = "italic"),  #Adjust Font size and style
  hjust = 0.5,                                    #Center the text horizontally
  x = 0.5                                         #Position it in the center
)

#now arrange all the plots
grid.arrange(
  #set up layout, first row
  arrangeGrob(unique_words, ratio, nrow = 1),#unique_words plot on left, ratio on right, in one row 
  source_text,                               #add source_text in the second row
  nrow = 2,                                  #set up total row 2 
  heights = c(10,1))                         #adjust heights in first main row 10, and second row is 1
```

<img src="../output/words_bobsburger-1.png" style="display: block; margin: auto;" />

### Few things I learned :

1.  The data has very disturbing outlier, and there is a way to remove
    the outlier by using outlier.shape = NA in the geom_boxplot(). I
    remove outlier for the visualization purposes, and I consider the
    outlier data is not very important.  
2.  Because the outlier, there is a huge empty space on the higher value
    of x. For the visualization purposes, I learn new code to set the x
    limit by using xlim()  
3.  First time also use the ggridges package, and learned how to create
    a ridges histogram and overlap more than one data into one plot.  
4.  First time also use text_Grob()
