

## VectorSurvR Package Documentation and User Guide

VectorSurv provides public health agencies the tools to manage, visualize and analyze the spread of vector-borne diseases and make informed decisions to protect public health.
 
The 'vectorsurvR' package is intended for users of [VectorSurv](https://vectorsurv.org/), a public health vector borne disease surveillance system. The package contains functions tailored to data retrieved from the VectorSurv database. A valid VectorSurv username and password is required for data retrieval. Those without agency access can use sample datasets in place of real data. This documentation covers the functions in 'vectorsurvR' and introduces users to methods of R programming. The purpose of this documentation is to introduce and guide users with limited programming experience. 


```{r setup, include = TRUE}

##If not installed
##Install from cran with (recommended)
install.packages("vectorsurvR")

##Or install developing version from our github with
devtools::install_github("UCD-DART/vectorsurvR")

##Load package
library(vectorsurvR)

```



## Data Retrieval 

**getToken()**

*Description*

`getToken()` returns a token needed to run `getArthroCollections()` and `getPools()`. The function prompts users for their Gateway credentials. If credentials are accepted, the function returns a user token needed to obtain data and a list of agencies the user has access to. 

*Usage*

`getToken()`

*Arguments*

```{r, results='hide'}

token = getToken()

```

**getArthroCollections(...)**

*Description*

`getArthroCollections(...) ` obtains collections data for a range of years. It prompts the user for their Gateway username and password before retrieving the associated data.  You can only retrieve data from agencies linked to your Gateway account. 

*Usage*

`getArthroCollections(token,start_year, end_year, arthropod, agency_id = NULL)`

*Arguments*

- token: access token retrieved from `getToken()`
- start_year: Beginning of year range 
- end_year: End of year range
- arthropod: Type of pools to retrieve: 'tick' , 'mosquito'
- agency_id: Default to NULL returns data for all available agencies, specifying a single agency id will return data just for that agency. This parameter is best used by accounts with access to multiple agencies. 

```{r}
#Example
collections = getArthroCollections(token, 2022,2023, 'mosquito')
```


**getPools(...)**

*Description*

`getPools()` similar to `getArthroCollections()` obtains pools on a year range (start_year, end_year). It prompts the user for their Gateway username and password before retrieving the associated data. `getPools()` retrieve data for both mosquito and tick pools. 

*Usage*

`getPools(token, start_year, end_year, arthropod, agency_id = NULL)`
*Arguments*

- token: access token retrieved from `getToken()`
- start_year: Beginning of year range 
- end_year: End of year range
- arthropod: Type of collections to retrieve: 'tick' , 'mosquito'
- agency_id: Default to NULL returns data for all available agencies, specifying a single agency id will return data just for that agency. This parameter is best used by accounts with access to multiple agencies. 

```{r}
#Example
pools = getPools(token, 2022,2023, 'mosquito')
```



## Write Data to file

You can save retrieved data as a .csv file in your current directory using `write.csv()`.
That same data can be retrieved using `read.csv()`. Writing data to a .csv can make the rendering process more efficient when generating reports in R. We recommend that you write the data pulled from our API into a csv and then load that data when generating reports. 


```{r}
#creates a file named "collections_18_23.csv" in your current directory
write.csv(x = collections, file = "collections_22_23.csv")

#loads collections data
collections = read.csv("collections_22_23.csv")

```


## Data Processing

The 'vectorsurvR' package comes with two sample datasets which can be used in place of real collections and pools data. 

Data can be subset to contain columns of interest. Subsetting can also be used to reorder the columns in a data frame.Do not subset collections or pools data before inputting them into VectorSurv calculator functions to avoid losing essential columns. It is recommended to subset after calculations are complete and before inputting into a table generator. **Remember, subsetting, filtering, grouping and summarising will not change the value of the data unless it is reassigned to the same variable name.** We recommend creating a new variable for processed data.  

### Subsetting 

```{r}
#Subset using column names or index number


colnames(collections) #displays column names and associated index



#Subseting by name
head(collections[c("collection_date", "species_display_name", "num_count")])

#by index
head(collections[c(3, 23, 17)])

#to save a subset
collections_subset = collections[c(3, 23, 17)]
```

### Filtering and subsetting in 'dplyr' 

'dplyr' is a powerful package for filtering and sub-setting data. It follows logic similar to SQL queries.

For more information on data manipulation using 'dplyr' [Click Here](https://datacarpentry.org/dc_zurich/R-ecology/04-dplyr.html)

'dplyr' utilizes the pipe operator `%>%` to send data into functions. The `head()` function returns the first few rows of data, specifying `head(1)` tells the software to return only the first row for viewing purposes. Remove `head()` to see all the data or reassign the data to a new variable.

```{r}
#NOTE: library was loaded above
library(dplyr)

#Subsetting columns with 'select()'
sample_collections %>%
  dplyr::select(collection_date, species_display_name, num_count) %>% head()

```
Below are more examples for filtering data.

```{r}

#filtering with dplyr 'filter'
collections_pip = collections %>%
  filter(species_display_name == "Cx pipiens")

#filtering multiple arguments using '%in%'
collections_pip_tar = collections %>%
  filter(species_display_name %in% c("Cx pipiens", "Cx tarsalis"),
         site %in% c(119819, 102832)) #filters site codes

```

### Grouping and Summarising 

In addition to filtering and sub-setting, data can be group by variables and summarised.

```{r}
#groups by species and collection date and sums the number counted

collections %>%
  group_by(collection_date, species_display_name) %>%
  summarise(sum_count = sum(num_count, na.rm = T)) %>%
  head()


#groups by species and collection date and takes the average the number counted

collections %>%
  group_by(collection_date, species_display_name) %>%
  summarise(avg_count = mean(num_count, na.rm = T)) %>%
  head()
```

### Pivoting

Data can be manipulated into long and wide (spreadsheet) forms using `pivot_wider()` and `pivot_longer()` from the 'tidyr' package. By default data from the API is in long form. Here we pivot on species and sex condition names using num_count as values. The end result is data with num_count values in the columns named species_sex. For more on pivoting see `??pivot_longer()` and `??pivot_wider()`.

```{r}
library(tidyr)

collections_wide = pivot_wider(
  collections,
  names_from = c("species_display_name","sex_name"),
  values_from = "num_count"
)
```


## Calculations


### Abundance

**getAbundance(...)**

*Description*

`getAbundance()` uses any amount of **mosquito** collections data to calculate the abundance for the specified parameters. The function calculates using the methods of the Gateway Abundance calculator.


*Usage*

`getAbundance(collections,interval, species_list = NULL, trap_list = NULL, species_separate = FALSE)`

*Arguments*

- collections: Mosquito collections data retrieved from `getArthroCollections(...)` 
- interval: Calculation interval for abundance, accepts "collection_date","Biweek","Week", and "Month.
- species_list: Species filter for calculating abundance. Species_display_name is the accepted notation. To see a list of species present in your data run `unique(collections$species_display_name)`. If species is unspecified, the default `NULL` will return data for all species in data.
- trap_list: Trap filter for calculating abundance. Trap_acronym is the is the accepted notation. Run `unique(collections$trap_acronym)` to see trap types present in your data. If trap_list is unspecified, the default `NULL` will return data for all trap types.
- species_separate: Should the species in species_list have abundance calculated separately? Setting to FALSE calculates the combined abundance. The same result can be performed by calculating on one species at the time.


```{r}
getAbundance(
  collections,
  interval = "Biweek",
  species_list = c("Cx tarsalis", "Cx pipiens"),
  trap_list = "CO2",
  species_separate = FALSE
)

```



### Abundance Anomaly (comparison to 5 year average)

**getAbundanceAnomaly()**

*Description*

`getAbundanceAnomaly(...)` requires at least five years prior to the target_year of **mosquito** collections data to calculate for the specified parameters. The function uses the methods of the Gateway Abundance Anomaly calculator, and will not work if there is fewer than five years of data present.  


*Usage*

`getAbundanceAnomaly(collections,interval,target_year, species_list = NULL, trap_list = NULL, species_separate = FALSE)`

*Arguments*

- collections: Collections data retrieved from `getArthroCollections(...) `
- interval: Calculation interval for abundance, accepts "collection_date","Biweek","Week", and "Month.
- target_year: Year to calculate analysis on. Collections data must have a year range of at least (target_year - 5, target_year).
- species_list: Species filter for calculating abundance. Species_display_name is the accepted notation. To see a list of species present in your data run `unique(collections$species_display_name)`. If species is unspecified, the default `NULL` will return data for all species in data.
- trap_list: Trap filter for calculating abundance. Trap_acronym is the is the accepted notation. Run `unique(collections$trap_acronym)` to see trap types present in your data. If trap_list is unspecified, the default `NULL` will return data for all trap types.
- species_separate: Should the species in species_list have abundance calculated separately? Setting to FALSE calculates the combined abundance. The same result can be performed by calculating on one species at the time.


```{r}
collections_18_23 = getArthroCollections(token, 2018,2023, 'mosquito')

getAbundanceAnomaly(collections_18_23,
                    interval = "Biweek",
                    target_year = 2023,
                    species_list = c("Cx tarsalis", "Cx pipiens"),
                    trap_list = "CO2",
                    species_separate = FALSE) 
```

### Infection Rate

**getInfectionRate()**

*Description*

`getInfectionRate(...)` estimates the arbovirus infection rate based on testing pools of mosquitoes.

*Usage*

`getInfectionRate(pools,interval, target_year, target_disease,pt_estimate, scale = 1000, species_list = c(NULL), trap_list = c(NULL))`

*Arguments*

- pools: Pools data retrieved from `getPools(...) `
- interval: Calculation interval for abundance, accepts: "Biweek","Week", and "Month.
- target_year: Year to calculate infection rate for. This year must be present in the data.
- target_disease: The disease to calculate infection rate for--i.e. "WNV". Disease acronyms are the accepted input. To see a list of disease acronyms, run `unique(pools$target_acronym)`.
- pt_estimate: The estimation type for infection rate. Options include: "mle","bc-"mle", "mir"
- scale: Constant to multiply result
- species_list: Species filter for calculating abundance. Species_display_name is the accepted notation. To see a list of species present in your data run `unique(pools$species_display_name)`. If species is unspecified, the default `NULL` will return data for all species in data.
- trap_list: Trap filter for calculating abundance. Trap_acronym is the is the accepted notation. Run `unique(pools$trap_acronym)` to see trap types present in your data. If trap_list is unspecified, the default `NULL` will return data for all trap types.



```{r}
IR = getInfectionRate(pools, 
                      interval = "Week",
                      target_disease = "WNV",
                      pt_estimate = "mle", 
                      scale = 1000,
                      species_list = c("Cx pipiens"),
                      trap_list = c("CO2","GRVD") )
IR
```

### Vector Index


**getVectorIndex()**

*Description*

`getVectorIndex(...)` The vector index is the relative abundance of infected mosquitoes and is a way to quickly estimate the risk of arbovirus transmission in an area. Vector index is the product of the abundance and infection rate for a given time interval: $Vector Index = Infection Rate * Abundance$


*Usage*

`getVectorIndex(collections, pools, interval,               ,                           target_disease,                           pt_estimate,species_list=NULL, trap_list = NULL)`

*Arguments*
- collections: collections data retrieved from `getArthroCollections(...)`
- pools: Pools data retrieved from `getPools(...)`

**Note: Years from pools and collections data must overlap**

- interval: Calculation interval for abundance, accepts "collection_date","Biweek","Week", and "Month.

- target_disease: The disease to calculate infection rate. Disease acronyms are the accepted input. To see a list of disease acronyms, run `unique(pools$target_acronym)`.

- pt_estimate: The estimation type for infection rate. Options include: "mle","bc-"mle", "mir".
- species_list: Species filter for calculating abundance. Species_display_name is the accepted notation. To see a list of species present in your data run `unique(pools$species_display_name)`. If species is unspecified, the default `NULL` will return data for all species in data.
- trap_list: Trap filter for calculating abundance. Trap_acronym is the is the accepted notation. Run `unique(pools$trap_acronym)` to see trap types present in your data. If trap_list is unspecified, the default `NULL` will return data for all trap types.


```{r}

pools = getPools(token,2023,2023, 'mosquito')
collections = getArthroCollections(token,2023,2023, 'mosquito')

getVectorIndex(collections, pools, interval = "Biweek",
                           
                           target_disease = "WNV", pt_estimate = "bc-mle",
                           species_list=c("Cx tarsalis"), 
                           
                           trap_list =  c("CO2"))

```

## Tables

**getPoolsComparisionTable()**

*Description*

`getPoolsComparisionTable()` produces a frequency table for positive and negative pools counts by year and species. The more years present in the data, the larger the table.

*Usage*

`getPoolsComparisionTable(pools,target_disease, species_separate=F)`

*Arguments*

- pools: Pools data retrieved from `getPools(...)` 
- target_disease: The disease to calculate infection rate for--i.e. "WNV". Disease acronyms are the accepted input. To see a list of disease acronyms, run `unique(pools$target_acronym)`.
- species_separate: Should the pools comparison be split by species of each pool. Default is `FALSE.`

```{r}
getPoolsComparisionTable(
  pools,
  interval = "Week",
  target_disease = "WNV",
  species_separate = T
)
```


### Styling Dataframes with 'kable'

Professional looking tables can be produced using the 'kable' and 'kableExtra' packages.

```{r}


library(kableExtra)

AbAnOutput = getAbundance(
  collections,
  interval = "Biweek",
  
  species_list = c("Cx tarsalis", "Cx pipiens"),
  trap_list = "CO2",
  species_separate = FALSE
)
head(AbAnOutput)

#kable table where column names, font_size, style and much more can be customized

AbAnOutput %>%
  kbl(col.names = c("Disease Year", "Biweek", "Count", "Trap Events", "Abundance")) %>%
  kable_styling(
    bootstrap_options = "striped",
    font_size = 14,
    latex_options = "scale_down"
  ) %>%
  footnote(general = "Table X: Combined biweekly Abundance Calculation for Cx. tarsalis, pipiens in CO2 traps", general_title = "")

```

### Data using 'datatables'

Interactive html only tables can be produced using the 'DT' package. 'DT' tables allow for sorting and filtering with in a webpage. These are ideal for viewing data but are not compatible with pdf or word formats. 

```{r}
library(DT)

AbAnOutput %>%
  datatable(colnames =  c("Disease Year", "Biweek", "Count", "Trap Events", "Abundance"))
```




## Charts and Graphs

'ggplot2' is a easy to use plotting library in R. 'ggplot2' syntax consists of creating a ggplot object with a dataframe and adding subsequent arguments to that object. Aesthetics (`aes()`) in ggplot represents the data mapping aspect of the plot. A simple example using collections is shown below.



```{r}
library(ggplot2)
library(lubridate)
#creates a month column and translates numerics
collections$month = as.factor(month(collections$collection_date))


collections_sums = collections %>%
  filter(
    species_display_name %in% c(
      "Cx tarsalis",
      "Cx pipiens",
      "An freeborni",
      "Cs incidens",
      "Ae melanimon",
      "Cs inornata",
      "Cx stigmatosoma",
      "Cx erythrothorax",
      "Ae vexans",
      "I pacificus"
    )
  ) %>%
  group_by(month, species_display_name) %>%
  summarise(sum_count = sum(num_count, na.rm = T)) %>% arrange(desc(sum_count), .by_group = T)



#ggplot with dots a values for each species of interest

ggplot(data = collections_sums,
       aes(x = month, y = sum_count, color = species_display_name)) +
  geom_point()

#bar chart
ggplot(data = collections_sums,
       aes(x = month, y = sum_count, fill = species_display_name)) +
  geom_bar(stat = "identity")


#adding labels
ggplot(data = collections_sums,
       aes(x = month, y = sum_count, fill = species_display_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Mosquito Counts by Month and Species",
       x = "Month",
       y = "Sum of Mosquitoes",
       fill = "Species")
```

When plotting with libraries in R, it is easiest when the data is prepared in long form. Most calculator outputs from our functions are in wide form. The following wrapper functions help process and plot this data. 

**processAbunAnom()**

*Description*

`processAbunAnom()` processes the output returned from `getAbundanceAnomaly()` into a long form suitable for plotting in 'ggplot'

*Usage*

`processAbunAnom(AbAnomOutput)`

*Arguments*

- AbAnomOutput: Output from returned `getAbundanceAnomaly()`

```{r}


collections = getArthroCollections(token, 2018, 2023, 'mosquito')

AbAnOut = getAbundanceAnomaly(
  collections,
  interval = "Biweek",
  target_year = 2023,
  species_list = c("Cx tarsalis", "Cx pipiens"),
  species_separate = TRUE
)



AbAnOut_L = processAbunAnom(AbAnOut)
```



We can take the output of `processAbunAnom()` and create a plot comparing the target year abundance to the five year average.

```{r}


AbAnOut_L %>%  filter(Abundance_Type %in% c("2023_Abundance",
                                            "Five_Year_Avg")) %>%
  ggplot(aes(x = Biweek,
             y = Abundance_Calculation,
             color = Abundance_Type)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ species_display_name) +
  labs(title = "2023 Abundance Anomaly", y = "")

```

We can also create a plot which displays the percent change from the five year average.

```{r}
AbAnOut_L %>%
  filter(Abundance_Type == "Delta") %>%
  mutate(Change = ifelse(Abundance_Calculation > 0, "Increase", "Decrease")) %>%
  ggplot(aes(x = Biweek,
             y = Abundance_Calculation,
             fill = Change)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ species_display_name) +
  labs(x = "Biweek",
       y = "Percent Change",
       title = "Relative Abundance 2023, % Change from 5-year average",
       fill = "Relative Change")

```

**plotInfectionRate()**

*Description*

`plotInfectionRate()` plots the output returned from `getInfectionRate()` with confidence intervals using 'ggplot2'.

*Usage*

`plotInfectionRate(InfRtOutput, year)`

*Arguments*

- InfRtOutput: Output from returned `getInfectionRate()`
- year: Desired year of plot (year must be in data)

```{r}
IR = getInfectionRate(
  pools,
  interval = "Week",
  target_disease = "WNV",
  pt_estimate = "mle",
  species_list = c("Cx pipiens"),
  trap_list = c("CO2", "GRVD")
)

plotInfectionRate(InfRtOutput = IR, year = 2023)

```


## Additional Table Examples

We can highlight rows and columns, add headers, and customize footnotes. For more information please [Click Here](https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Grouped_Columns__Rows) 

```{r}
collections = getArthroCollections(token, 2021, 2023, 'mosquito')

table(collections$trap_acronym, collections$surv_year) %>%
  kbl(align = "c") %>%
  kable_paper(
    full_width = F,
    html_font = "arial",
    lightable_options = "striped",
  ) %>%
  add_header_above(c("Trap Type", "Years" = 3)) %>%
  footnote(general = "Table X: Traps deployed by year", general_title = "") %>%
  row_spec(c(3, 9, 10), background = "yellow") %>%
  column_spec(c(4), background = "orange")
```




