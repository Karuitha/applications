# Sample R Script for DIME-STC Application ----
# Author: JOHN KARUITHA
# PLEASE NOTE THE REQUIRE INTERNET TO RUN
# THIS IS BECAUSE SOME PACKAGES HAVE TO BE DOWNLOADED AND LOADED
## Working with the gapminder dataset in R
## Loading the package manager pacman
## 

# BAKGOUND ----
## I will use gapminder data and scrape another set of data
## From the internet
## I clean the data and do some plots.
## Then I run some regressions on the data. 

## The skills I illustrate are:
# R Programming.
# Getting and cleaning data
# Scrapping data from websites.
# Joining datasets.
# Regular expressions.
# Data visualization in ggplot2.
# Data manipulation using dplyr.
# Regression analysis. 

## NB: You can find more of my work using R on <www.rpubs.com/Karuitha>

## Install and load the package manager pacman
if(!require(pacman)){
        
        ## Internet connection required
        install.packages("pacman")
}


## Load the required packages 
## Internet connection required
pacman::p_load(tidyverse, gapminder, GGally, skimr, tidyquant, 
               
               plotly, gghalves, rvest, countrycode, patchwork,
               
               data.table, plm)

## Set the background theme for plots 
theme_set(tidyquant::theme_tq())

# THE GAPMINDER DATA ----
## Load the required dataset which is inbuilt in R
### The data has 1704 observations from 1952-2007
### The data explores population, life expectancy, and incomes globally
my_data <- gapminder::gapminder

# SCRAPPING DATA FROM THE INTERNET ----
## For comparison, I get data for 2020/21 from Wikipedia, courtesy of WHO & IMF
my_url <- "https://en.wikipedia.org/wiki/List_of_countries_by_life_expectancy"

## Scrape the data  from the website
my_comp_data <- read_html(my_url) %>% 
        
        html_nodes("table") %>% 
        
        html_table() %>% 
        
        .[[1]]

## Set names and remove continents among country names
my_comp_data <- my_comp_data[,1:4] %>% 
        
        set_names(c("country", "life_exp_all",
                    
                    "life_exp_f",
                    
                    "life_exp_m")) %>% 
        
        filter(!country %in% c("Country", "",
                               
                               "Europe", 
                               
                               "Western Pacific",
                               
                               "Americas", "World",
                               
                               "South-East Asia",
                               
                               "Eastern Mediterranean",
                               
                               "Africa", "Micronesia")) %>% 
        
        ## For each country, match it with continent
        mutate(continent = countrycode(sourcevar = country,
                                       
                                       origin = "country.name",
                                       
                                       destination = "continent")) %>% 
        
        ## Convert variables to numeric from characters
        mutate(across(.cols = -c("country", "continent"), 
                      
                      .fns = as.numeric)) %>% 
        
        mutate(year = 2020) %>% 
        
        select(-life_exp_f, -life_exp_m) %>% 
        
        rename(lifeExp = life_exp_all)

## Similarly, I get the GDP data and population data for 2020 from wikipedia 
## (original source is IMF)

url_2 <- "https://en.wikipedia.org/wiki/List_of_IMF_ranked_countries_by_GDP"


second_gdp_data_2021 <- read_html(url_2) %>% 
        
        html_nodes("table") %>% 
        
        html_table() %>% 
        
        .[[2]] %>% 
        
        set_names(c("country", "gdp_nominal_usd_millions",
                    
                    "gdp_nominal_usd_per_capita",
                    
                    "gdp_ppp_usd_millions", "gdp_usd_ppp_per_capita",
                    
                    "pop", "ppp")) %>% 
        
        mutate(across(.cols = -c("country", "ppp"), 
                      
                      .fns = parse_number)) %>% 
        
        ## Attach corresponding continent names
        mutate(continent = countrycode(sourcevar = country,
                                       
                                       origin = "country.name",
                                       
                                       destination = "continent")) %>% 
        
        mutate(year = 2020) %>% 
        
        ## Select relevant variables
        select(country, continent, year, pop, gdp_nominal_usd_per_capita) %>% 
        
        rename(gdpPercap = gdp_nominal_usd_per_capita) %>% 
        
        mutate(pop = pop * 1000)

## JOINING THE DATA ----
### Here, I combine all the three datasets into one 
final_data <- second_gdp_data_2021 %>% 
        
        full_join(my_comp_data, by = "country") %>% 
        
        select(-ends_with(".y")) %>% 
        
        set_names(names(.) %>% str_replace_all("\\.x", "")) %>% 
        
        mutate(country = case_when(
                
                country == "Swaziland" ~ "Eswatini",
                
                country == "DR Congo" ~ "Congo, Dem. Rep.",
                
                country == "North Korea" ~ "Korea, Dem. Rep.",
                
                str_detect(country, "[Bb]runei") ~ "Brunei Darussalam",
                
                country == "Ivory Coast" ~ "Cote d'Ivoire",
                
                country == "Congo" ~ 'Congo, Rep.',
                
                country == "São Tomé and Príncipe" ~ 'Sao Tome and Principe',
                
                country == "Slovakia" ~ 'Slovak Republic',
                
                country == "South Korea" ~ 'Korea, Rep.',
                
                country == "Yemen" ~ 'Yemen, Rep.',
                
                TRUE ~ country
        )) %>% 
        
        rbind(my_data, use.names = TRUE) %>% 
        
        filter(year != 1) 

# EXPLORING THE DATA ----
## Exploring the my_data dataset
names(final_data) ## variable names
head(final_data) ## First six rows of the data
tail(final_data) ## Last six rows of the data
glimpse(final_data) ##structure of the data

# DATA VISUALIZATION ----
## Distribution of life expectancy by continent
final_data %>% 
        
        filter(year %in% c(2007, 2020)) %>% 
        
        ggplot(mapping = aes(x = lifeExp, fill = continent)) + 
        
        geom_density(alpha = 0.5) + 
        
        labs(x = "Life Expectancy", 
             
             y = "Density") + 
        
        facet_wrap(~ year)
## 10 Most populous countries 2007
final_data %>% 
        
        ## Filter data for 2007
        filter(year %in% c(2007, 2020)) %>% 
        
        group_by(year) %>% 
        
        ## Arrange data in descending order
        arrange(desc(pop)) %>% 
        
        ## Get the top 10
        slice(1:20) %>% 
        
        ungroup() %>% 
        
        ## Plot the data
        ggplot(mapping = aes(x = fct_reorder(country, 
                                             
                                             pop, max), y = pop, 
                             
                             fill = factor(year))) + 
        
        geom_col(position = "dodge") + 
        
        ## Convert y axis into numbers and not scientific notation
        scale_y_continuous(labels = scales::comma) +
        
        ## Add labels
        labs(x = "Population", y = "country",
             
             title = "Top 10 Most Populous Countries, 2007 and 2020") + 
        
        scale_fill_brewer(palette = 4) + 
        
        coord_flip()
        
## GDP per Capita versus Life Expectancy 2007/2021
final_data %>% 
        
        filter(year %in% c(2007, 2020)) %>% 
        
        drop_na(gdpPercap, lifeExp) %>% 
        
        filter(gdpPercap <= 50000) %>% 
        
        ggplot(mapping = aes(x = gdpPercap, y = lifeExp, 
                             
                             size = pop, col = continent)) +
        
        geom_point(show.legend = TRUE, shape = 1, stroke = 3,
                   
                   alpha = 0.7, size = 10) + 
                
                labs(x = "GDP per Capita", y = "Life Expectancy",
                     
                     title = "GDP per Capita vs Life Expectancy, 2007",
                     
                     caption = "Size Captures Population") + 
        
        facet_wrap(~ year) + 
        
        theme(legend.title = element_blank()) + 
        
        scale_color_manual(values = c("red", "blue", "purple", "green", 
                                      
                                      "black"))


## Zooming in to Life Expectancy by continent
final_data %>% 
                
                filter(year %in% c(2007, 2020)) %>% 
        
        ggplot(mapping = aes(x = fct_reorder(continent, lifeExp, median), 
                             
                             y = lifeExp, 
                             
                             fill = continent)) + 
        
        geom_half_violin(width = 1.4, show.legend = FALSE, alpha = 0.5) +
        
        geom_boxplot(col = "black", show.legend = FALSE, width = 0.1) +
        
        scale_fill_viridis_d() + 
        
        labs(x = "Continent", y = "Life Expectancy",
             
             title = "Life Expectancy by Continent: 2007 and 2020") + 
                
                facet_wrap(~ year)

# SUMMARY STATISTICS -----
## Numeric variables
final_data %>% 
        
        select(where(is.numeric)) %>% 
        
        skim_without_charts()

## Character variables 
final_data %>% 
        
        select(where(is.character)) %>% 
        
        skim_without_charts()


# REGRESSION ANALYSIS ----
## I run a simple panel linear regression
## This is for illustrative purposes only
my_panel_model <- plm(lifeExp ~ gdpPercap + factor(year), 
            
            data = final_data, 
    
            index = c("country", "year"),
            
            model = "within")

summary(my_panel_model)


summary(my_panel_model)  

## I run a pooled OLS
my_pooling_model <- plm(lifeExp ~ gdpPercap + factor(year), 
                      
                      data = final_data, 
                      
                      index = c("country", "year"),
                      
                      model = "pooling")

summary(my_pooling_model)

## LM test for choice between fixed effects versus pooled OLS
plm::pFtest(my_panel_model, my_pooling_model)
## The test supports existence of significant effects.
## So the fixed effects model is better than OLS. 

## I run  first difference model
my_fd_model <- plm(lifeExp ~ gdpPercap + factor(year), 
                        
                        data = final_data, 
                        
                        index = c("country", "year"),
                        
                        model = "fd")

summary(my_fd_model)

## I run the between model
my_between_model <- plm(lifeExp ~ gdpPercap + factor(year), 
                   
                   data = final_data, 
                   
                   index = c("country", "year"),
                   
                   model = "between")

summary(my_between_model)

## Some of the models show an inverse relationship between
## Income and life expectancy which would go against theory.
## The fixed effects model seems to do much better.
## The random effects model cannot converge. However, as this is
## Just illustrative, there is a huge case for omitted variables
## That leads to singularity. 

## NB: You can find more of my work on <www.rpubs.com/Karuitha>
