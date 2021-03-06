Mini Data-Analysis Deliverable 3
================

# Welcome to your last milestone in your mini data analysis project!

In Milestone 1, you explored your data and came up with research
questions. In Milestone 2, you obtained some results by making summary
tables and graphs.

In this (3rd) milestone, you’ll be sharpening some of the results you
obtained from your previous milestone by:

-   Manipulating special data types in R: factors and/or dates and
    times.
-   Fitting a model object to your data, and extract a result.
-   Reading and writing data as separate files.

**NOTE**: The main purpose of the mini data analysis is to integrate
what you learn in class in an analysis. Although each milestone provides
a framework for you to conduct your analysis, it’s possible that you
might find the instructions too rigid for your data set. If this is the
case, you may deviate from the instructions – just make sure you’re
demonstrating a wide range of tools and techniques taught in this class.

## Instructions

**To complete this milestone**, edit [this very `.Rmd`
file](https://raw.githubusercontent.com/UBC-STAT/stat545.stat.ubc.ca/master/content/mini-project/mini-project-3.Rmd)
directly. Fill in the sections that are tagged with
`<!--- start your work here--->`.

**To submit this milestone**, make sure to knit this `.Rmd` file to an
`.md` file by changing the YAML output settings from
`output: html_document` to `output: github_document`. Commit and push
all of your work to your mini-analysis GitHub repository, and tag a
release on GitHub. Then, submit a link to your tagged release on canvas.

**Points**: This milestone is worth 40 points (compared to the usual 30
points): 30 for your analysis, and 10 for your entire mini-analysis
GitHub repository. Details follow.

**Research Questions**: In Milestone 2, you chose two research questions
to focus on. Wherever realistic, your work in this milestone should
relate to these research questions whenever we ask for justification
behind your work. In the case that some tasks in this milestone don’t
align well with one of your research questions, feel free to discuss
your results in the context of a different research question.

# Setup

Begin by loading your data and the tidyverse package below:

``` r
library(datateachr) # <- might contain the data you picked!
library(tidyverse)
```

From Milestone 2, you chose two research questions. What were they? Put
them here.

<!-------------------------- Start your work below ---------------------------->

1.  *For the units at least have one separate meter for items( gas,
    hydro or water), does the number of storeys change during period
    (built before 1950, between 1950 and 1960, between 1960 and 1990 and
    after 1990)? What about the buildings after 2015?*
2.  *For the the units at least have one separate meter for items(gas,
    hydro or water), what is the relationship between the number of
    units and storeys?*
    <!----------------------------------------------------------------------------->

# Exercise 1: Special Data Types (10)

For this exercise, you’ll be choosing two of the three tasks below –
both tasks that you choose are worth 5 points each.

But first, tasks 1 and 2 below ask you to modify a plot you made in a
previous milestone. The plot you choose should involve plotting across
at least three groups (whether by facetting, or using an aesthetic like
colour). Place this plot below (you’re allowed to modify the plot if
you’d like). If you don’t have such a plot, you’ll need to make one.
Place the code for your plot below.

<!-------------------------- Start your work below ---------------------------->

I choose this plot to modify to complete task1 and task2.

``` r
apt_buildings2 <- apt_buildings %>% 
 select(-amenities,-prop_management_company_name) %>% 
 mutate(build_year=factor(case_when(year_built<=1900 ~ "<=1900",
                                    year_built<=1925 ~ "1901-1925",
                             year_built<=1950 ~ "1926-1950",
                             year_built<=1975 ~ "1951-1975",
                             year_built<=2000 ~ "1976-2000",
                             TRUE ~ ">2000"))) %>% drop_na()

apt_buildings2 %>% 
 filter(air_conditioning!="NONE")%>% 
 mutate(o2010=if_else(year_built>"2015", ">2015","others")) %>%ggplot(aes(x = build_year,y=no_of_units)) +
 scale_y_log10("number of units")+
 geom_boxplot(outlier.shape=NA,width=0.3)+
 geom_jitter(aes(color=o2010,alpha=o2010),width=0.2)+
 theme_minimal()+
 xlab("build year")+
 ggtitle("The number of units in different period")+
 scale_color_discrete("")+
 scale_alpha_manual("",values=c(1,0.3))
```

![](mini-data-analysis3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

We can see that in this graph, the build year is random and let people
hard to find the change trend of the number of units as time goes on.
<!----------------------------------------------------------------------------->

Now, choose two of the following tasks.

1.  Produce a new plot that reorders a factor in your original plot,
    using the `forcats` package (3 points). Then, in a sentence or two,
    briefly explain why you chose this ordering (1 point here for
    demonstrating understanding of the reordering, and 1 point for
    demonstrating some justification for the reordering, which could be
    subtle or speculative.)

2.  Produce a new plot that groups some factor levels together into an
    “other” category (or something similar), using the `forcats` package
    (3 points). Then, in a sentence or two, briefly explain why you
    chose this grouping (1 point here for demonstrating understanding of
    the grouping, and 1 point for demonstrating some justification for
    the grouping, which could be subtle or speculative.)

3.  If your data has some sort of time-based column like a date (but
    something more granular than just a year):

    1.  Make a new column that uses a function from the `lubridate` or
        `tsibble` package to modify your original time-based column. (3
        points)
        -   Note that you might first have to *make* a time-based column
            using a function like `ymd()`, but this doesn’t count.
        -   Examples of something you might do here: extract the day of
            the year from a date, or extract the weekday, or let 24
            hours elapse on your dates.
    2.  Then, in a sentence or two, explain how your new column might be
        useful in exploring a research question. (1 point for
        demonstrating understanding of the function you used, and 1
        point for your justification, which could be subtle or
        speculative).
        -   For example, you could say something like “Investigating the
            day of the week might be insightful because penguins don’t
            work on weekends, and so may respond differently”.

<!-------------------------- Start your work below ---------------------------->

**Task 1**:

``` r
apt_buildings2 %>% 
 filter(air_conditioning!="NONE")%>% 
 mutate(o2010=if_else(year_built>"2015", ">2015","others")) %>% 
 mutate(build_year=fct_relevel(build_year,c("<=1900", '1901-1925', '1926-1950', '1951-1975', '1976-2000','>2000'))) %>% 
 #mutate(build_year = fct_reorder(build_year, no_of_units)) %>% 
 ggplot(aes(x = build_year,y=no_of_units)) +
 scale_y_log10("number of units")+
 geom_boxplot(outlier.shape=NA,width=0.3)+
 geom_jitter(aes(color=o2010,alpha=o2010),width=0.2)+
 theme_minimal()+
 xlab("build year")+
 ggtitle("The number of units in different period")+
 scale_color_discrete("")+
 scale_alpha_manual("",values=c(1,0.3))
```

![](mini-data-analysis3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

For the graph above, I change the build year by hand. Using
`fct_relevel` to reorder the build year and the order of time goes on.
By reordering the build year in this graph, it is easy for us to find
the change trend of the number of units now. We can see that as time
goes on, the number of units(the mean) is increasing, expect the
building built before 1900.

``` r
apt_buildings2 %>% 
 filter(air_conditioning!="NONE")%>% 
 mutate(o2010=if_else(year_built>"2015", ">2015","others")) %>% 
 mutate(build_year = fct_reorder(build_year, no_of_units,mean)) %>% 
 ggplot(aes(x = build_year,y=no_of_units)) +
 scale_y_log10("number of units")+
 geom_boxplot(outlier.shape=NA,width=0.3)+
 geom_jitter(aes(color=o2010,alpha=o2010),width=0.2)+
 theme_minimal()+
 xlab("build year")+
 ggtitle("The number of units in different period")+
 scale_color_discrete("")+
 scale_alpha_manual("",values=c(1,0.3))
```

![](mini-data-analysis3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

For this graph above, I use `fct_reorder` to reorder the build year by
the mean of the number of units. In this time, the build year will have
the order in which units are incremented. It it clear that as the number
of the units increase, the build year(period) tends to increase expect
“\<=1900”. Plus, it is also clear to see in which period, the number of
units is the largest and in which period it is the smallest. To sum up,
reorder is a good way to show the graph in a clear and organized manner.
However, we can see that for the buildings built before 1900 and between
1901-1925 are less, so we will do the next task.
<!----------------------------------------------------------------------------->

<!-------------------------- Start your work below ---------------------------->

**Task 2**:

``` r
apt_buildings2 %>% 
 filter(air_conditioning!="NONE")%>% 
 mutate(o2010=if_else(year_built>"2015", ">2015","others")) %>% 
 mutate(build_year = fct_lump(build_year, n = 3)) %>% 
 mutate(build_year = fct_reorder(build_year, no_of_units)) %>% 
 ggplot(aes(x = build_year,y=no_of_units)) +
 scale_y_log10("number of units")+
 geom_boxplot(outlier.shape=NA,width=0.3)+
 geom_jitter(aes(color=o2010,alpha=o2010),width=0.2)+
 theme_minimal()+
 xlab("build year")+
 ggtitle("The number of units in different period")+
 scale_color_discrete("")+
 scale_alpha_manual("",values=c(1,0.3))
```

![](mini-data-analysis3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Because there are a few buildings built before 1950, it might influence
the analysis about these periods. I use`fct_lump` and set `n=3`, which
means in descending order, just to retain the period of the first three
number of units.And put other period in one group. I put them together
in a new group named “others”. This will let this group have more
variables and let the analysis more representative and meaningful.

<!----------------------------------------------------------------------------->

# Exercise 2: Modelling

## 2.0 (no points)

Pick a research question, and pick a variable of interest (we’ll call it
“Y”) that’s relevant to the research question. Indicate these.

<!-------------------------- Start your work below ---------------------------->

**Research Question**: *For the the units at least have one separate
meter for items(gas, hydro or water), what is the relationship between
the number of units and storeys?*

**Variable of interest**: *no_of_storeys*

<!----------------------------------------------------------------------------->

## 2.1 (5 points)

Fit a model or run a hypothesis test that provides insight on this
variable with respect to the research question. Store the model object
as a variable, and print its output to screen. We’ll omit having to
justify your choice, because we don’t expect you to know about model
specifics in STAT 545.

-   **Note**: It’s OK if you don’t know how these models/tests work.
    Here are some examples of things you can do here, but the sky’s the
    limit.
    -   You could fit a model that makes predictions on Y using another
        variable, by using the `lm()` function.
    -   You could test whether the mean of Y equals 0 using `t.test()`,
        or maybe the mean across two groups are different using
        `t.test()`, or maybe the mean across multiple groups are
        different using `anova()` (you may have to pivot your data for
        the latter two).
    -   You could use `lm()` to test for significance of regression.

<!-------------------------- Start your work below ---------------------------->

I use `lm()` function to predict Y (no_of_storeys) by using another
variable (no_of_units), here is the result:

``` r
data4 <- apt_buildings %>%
 select(id,year_built,ends_with("meters"),no_of_storeys,no_of_units) %>% 
 drop_na() %>% 
 pivot_longer(cols=ends_with("meters"),names_to="item",values_to="YorN") %>% 
 filter(YorN=="YES") %>%
 distinct(id, .keep_all = TRUE)
glimpse(data4)
```

    ## Rows: 2,176
    ## Columns: 6
    ## $ id            <dbl> 10359, 10360, 10361, 10362, 10363, 10364, 10365, 10366, ~
    ## $ year_built    <dbl> 1967, 1970, 1927, 1959, 1943, 1952, 1959, 1971, 1969, 19~
    ## $ no_of_storeys <dbl> 17, 14, 4, 5, 4, 4, 4, 7, 32, 7, 5, 7, 3, 3, 12, 4, 3, 7~
    ## $ no_of_units   <dbl> 218, 206, 34, 42, 25, 34, 14, 105, 571, 169, 87, 71, 12,~
    ## $ item          <chr> "separate_hydro_meters", "separate_hydro_meters", "separ~
    ## $ YorN          <chr> "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES", ~

``` r
lm <- lm(no_of_storeys~no_of_units,data4)
lm
```

    ## 
    ## Call:
    ## lm(formula = no_of_storeys ~ no_of_units, data = data4)
    ## 
    ## Coefficients:
    ## (Intercept)  no_of_units  
    ##     4.59977      0.03515

``` r
broom::tidy(lm)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   4.60    0.123         37.5 2.95e-238
    ## 2 no_of_units   0.0352  0.000785      44.8 7.09e-311

We can also seen the relationship on the graph:

``` r
plot(data4$no_of_units,data4$no_of_storeys,xlim = c(0, 800),x_lab="number of units", y_lab="number of storeys", main="Relationship between units and storeys")
func <- function(x){
 4.59977+0.03515*x
}
curve(func,add=T)
```

![](mini-data-analysis3_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
<!----------------------------------------------------------------------------->

## 2.2 (5 points)

Produce something relevant from your fitted model: either predictions on
Y, or a single value like a regression coefficient or a p-value.

-   Be sure to indicate in writing what you chose to produce.
-   Your code should either output a tibble (in which case you should
    indicate the column that contains the thing you’re looking for), or
    the thing you’re looking for itself.
-   Obtain your results using the `broom` package if possible. If your
    model is not compatible with the broom function you’re needing, then
    you can obtain your results by some other means, but first indicate
    which broom function is not compatible.

<!-------------------------- Start your work below ---------------------------->

Then, I choose some number of units( built after 2000) in this dataset
to predict their corresponding number of storeys.

``` r
data4%>%
 filter(year_built>="2000")%>%
 select(c(id, no_of_units))%>%
 broom::augment(lm,newdata=.)
```

    ## # A tibble: 59 x 3
    ##       id no_of_units .fitted
    ##    <dbl>       <dbl>   <dbl>
    ##  1 10386         116    8.68
    ##  2 10478          19    5.27
    ##  3 10524         466   21.0 
    ##  4 10527          15    5.13
    ##  5 10530         146    9.73
    ##  6 10991         332   16.3 
    ##  7 11060          89    7.73
    ##  8 11212          44    6.15
    ##  9 11287          46    6.22
    ## 10 11289         188   11.2 
    ## # ... with 49 more rows

<!----------------------------------------------------------------------------->

# Exercise 3: Reading and writing data

Get set up for this exercise by making a folder called `output` in the
top level of your project folder / repository. You’ll be saving things
there.

## 3.1 (5 points)

Take a summary table that you made from Milestone 2 (Exercise 1.2), and
write it as a csv file in your `output` folder. Use the `here::here()`
function.

-   **Robustness criteria**: You should be able to move your Mini
    Project repository / project folder to some other location on your
    computer, or move this very Rmd file to another location within your
    project repository / folder, and your code should still work.
-   **Reproducibility criteria**: You should be able to delete the csv
    file, and remake it simply by knitting this Rmd file.

<!-------------------------- Start your work below ---------------------------->

``` r
ifelse(dir.exists(here::here("output")),"it has been there",dir.create(here::here("output"))) 
```

    ## [1] TRUE

``` r
datatable <- apt_buildings %>%
 drop_na() %>% 
 group_by(window_type) %>%
 dplyr::summarise(storeys_mean = mean(no_of_units),
units_mean=mean(no_of_storeys))

write_csv(datatable, here::here("output", "datatable.csv"))
```

First, creating an output folder named “output”. Then choosing one
summary table and name it as `datatable`, saving it into output folder
named “datatable.csv”.

<!----------------------------------------------------------------------------->

## 3.2 (5 points)

Write your model object from Exercise 2 to an R binary file (an RDS),
and load it again. Be sure to save the binary file in your `output`
folder. Use the functions `saveRDS()` and `readRDS()`.

-   The same robustness and reproducibility criteria as in 3.1 apply
    here.

<!-------------------------- Start your work below ---------------------------->

I save the model `lm` named “model.rds” into output folder. And then
read it again to put it into the page:

``` r
saveRDS(lm, here::here("output","model.rds"))
readRDS(here::here("output","model.rds"))
```

    ## 
    ## Call:
    ## lm(formula = no_of_storeys ~ no_of_units, data = data4)
    ## 
    ## Coefficients:
    ## (Intercept)  no_of_units  
    ##     4.59977      0.03515

<!----------------------------------------------------------------------------->

# Tidy Repository

Now that this is your last milestone, your entire project repository
should be organized. Here are the criteria we’re looking for.

## Main README (3 points)

There should be a file named `README.md` at the top level of your
repository. Its contents should automatically appear when you visit the
repository on GitHub.

Minimum contents of the README file:

-   In a sentence or two, explains what this repository is, so that
    future-you or someone else stumbling on your repository can be
    oriented to the repository.
-   In a sentence or two (or more??), briefly explains how to engage
    with the repository. You can assume the person reading knows the
    material from STAT 545A. Basically, if a visitor to your repository
    wants to explore your project, what should they know?

Once you get in the habit of making README files, and seeing more README
files in other projects, you’ll wonder how you ever got by without them!
They are tremendously helpful.

## File and Folder structure (3 points)

You should have at least four folders in the top level of your
repository: one for each milestone, and one output folder. If there are
any other folders, these are explained in the main README.

Each milestone document is contained in its respective folder, and
nowhere else.

Every level-1 folder (that is, the ones stored in the top level, like
“Milestone1” and “output”) has a `README` file, explaining in a sentence
or two what is in the folder, in plain language (it’s enough to say
something like “This folder contains the source for Milestone 1”).

## Output (2 points)

All output is recent and relevant:

-   All Rmd files have been `knit`ted to their output, and all data
    files saved from Exercise 3 above appear in the `output` folder.
-   All of these output files are up-to-date – that is, they haven’t
    fallen behind after the source (Rmd) files have been updated.
-   There should be no relic output files. For example, if you were
    knitting an Rmd to html, but then changed the output to be only a
    markdown file, then the html file is a relic and should be deleted.

Our recommendation: delete all output files, and re-knit each
milestone’s Rmd file, so that everything is up to date and relevant.

PS: there’s a way where you can run all project code using a single
command, instead of clicking “knit” three times. More on this in STAT
545B!

## Error-free code (1 point)

This Milestone 3 document knits error-free. (We’ve already graded this
aspect for Milestone 1 and 2)

## Tagged release (1 point)

You’ve tagged a release for Milestone 3. (We’ve already graded this
aspect for Milestone 1 and 2)
