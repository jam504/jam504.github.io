---
layout: post
title: 'STAT 1261/2260: Principles of Data Science'
subtitle: Homework 5
---




1. Re-create the data graphic, "Youngest Male Names" (again, your result will be different from the chart below). You can recycle some of the codes above. In particular, the youngest men names are given by the ascending order or 
`median_age`. Your chart should be restricted to birth names given to at least 100,000 male Americans since 1900. Use `filter()` to filter names with at least 100,000 `est_num_alive`. Can you make the color of the bars Carolina blue? 





```r
com_male <- BabynamesDist %>%
  filter(sex == "M") %>%
  group_by(name) %>% 
  mutate(N=n()) %>% 
  filter(N>=2) %>% 
  summarise(
    est_num_alive = sum(est_alive_today),
    q1_age = wtd.quantile(age_today, est_alive_today, probs = 0.25),
    median_age = wtd.quantile(age_today, est_alive_today, probs = 0.5),
    q3_age = wtd.quantile(age_today, est_alive_today, probs = 0.75)
  ) %>%
  filter(est_num_alive >= 100000) %>%
  arrange(median_age) %>%
  head(25)
com_male
```

```
## # A tibble: 25 x 5
##    name    est_num_alive q1_age median_age q3_age
##    <chr>           <dbl>  <dbl>      <dbl>  <dbl>
##  1 Jayden        108040.      5          7      9
##  2 Gavin         123965.      6          9     14
##  3 Elijah        173890.      7         10     16
##  4 Jackson       137388.      6         10     14
##  5 Mason         146945.      7         10     16
##  6 Ethan         301095.      7         11     15
##  7 Isaiah        147415.      7         11     16
##  8 Noah          235747.      7         11     16
##  9 Angel         175959.      8         12     22
## 10 Connor        153266.      8         12     17
## # ... with 15 more rows
```


```r
w_plot <- ggplot(
  data = com_male, 
  aes(x = reorder(name, -median_age),  y = median_age)) + 
  xlab(NULL) +
  ylab(NULL) +
  labs(
    title = "Youngest Male Names",
    subtitle = "By estimated median age for Americans alive as of Jan. 1, 2010"
  ) +
  scale_y_discrete(breaks=c("10","15","20","25","30"),
        labels=c("10 years old","15","20","25","30"))
```



```r
w_plot + 
  geom_linerange(
    aes(ymin = q1_age, ymax = q3_age),
    color = "#99badd", 
    size = 5, 
    alpha = 0.8
    ) + 
  geom_point(fill = "#ed3324", colour = "white", size = 3, shape = 21) +
  geom_point(aes(y=24,x=24),fill = "#ed3324", colour = "white", size = 3, shape = 21) +
  geom_text(aes(y=26,x=24),label="median",cex=3.5)+
  geom_text(aes(y=10,x=8),label="25th",cex=3.5)+
  geom_text(aes(y=22,x=8),label="75th percentile",cex=3.5)+
  geom_point(aes(y=8,x=8),shape=17)+
  geom_point(aes(y=25,x=8),shape=17)+
  coord_flip()
```

<img src="HW5_Margasak_files/figure-html/unnamed-chunk-2-1.png" width="70%" style="display: block; margin: auto;" />

2. Create a new variable with value  $| 0.5 -  boys / total |$, using `mutate()`.  List the top 10 years in which the name "Jackie" was given to M and F babies most equally. (Hint: arrange the variable created above.)

```r
jackie <- BabynamesDist %>%
  filter(name == "Jackie") %>%
  group_by(year) %>%
  summarise(
    N = n(), 
    total = sum(n), 
    boys = sum(ifelse(sex == "M",n, 0))
  ) %>%
  mutate(ratio=abs((0.5 - boys / total))) %>%
  arrange(ratio) %>%
  head(10)
jackie 
```

```
## # A tibble: 10 x 5
##     year     N total  boys   ratio
##    <dbl> <int> <int> <dbl>   <dbl>
##  1  2006     2   237   119 0.00211
##  2  1997     2   388   195 0.00258
##  3  1925     2   543   276 0.00829
##  4  1999     2   371   189 0.00943
##  5  1956     2  3839  1855 0.0168 
##  6  1927     2   938   487 0.0192 
##  7  1926     2   694   361 0.0202 
##  8  2003     2   282   147 0.0213 
##  9  2002     2   312   148 0.0256 
## 10  1955     2  3798  2000 0.0266
```


Use `babynames` data set to answer the following questions. These questions may be challenging. Think carefully about what data transformations are needed to answer each question. In your report, include both code chunks and the result. 

3. Which year had the highest number of births?


```r
highestbirths <- babynames %>%
  group_by(year) %>%
  summarise(
    totalbirths = sum(n)
  ) %>%
  arrange(desc(totalbirths)) %>%
  head(1)
highestbirths
```

```
## # A tibble: 1 x 2
##    year totalbirths
##   <dbl>       <int>
## 1  1957     4200007
```


4. In a single pipeline, compute the earliest and latest year that each name appears. 


```r
earliestandlatest <- babynames %>%
  group_by(name) %>%
  summarize(
    earliest = min(year),
    latest = max(year)
  ) %>%
  head(25)
earliestandlatest
```

```
## # A tibble: 25 x 3
##    name      earliest latest
##    <chr>        <dbl>  <dbl>
##  1 Aaban         2007   2017
##  2 Aabha         2011   2016
##  3 Aabid         2003   2016
##  4 Aabir         2016   2016
##  5 Aabriella     2008   2017
##  6 Aada          2015   2015
##  7 Aadam         1987   2017
##  8 Aadan         2003   2017
##  9 Aadarsh       2000   2017
## 10 Aaden         2001   2017
## # ... with 15 more rows
```

 
5. Among popular names (let's say at least 1% of the births in a given year), which name is the *youngest* -- meaning that its first appearance as a popular name is the most recent? 


```r
popname <- babynames %>%
  mutate(is_popular = prop >= 0.01) %>%
  filter(is_popular == TRUE) %>%
  group_by(name) %>%
  summarize(earliest = min(year)) %>%
  arrange(desc(earliest)) %>%
  head(1)
popname
```

```
## # A tibble: 1 x 2
##   name   earliest
##   <chr>     <dbl>
## 1 Olivia     2014
```


6. It seems like there is more diversity of names now than in the past. How have the number of names used changed over time? Has it been the same for boys and girls? 


```r
namechange <- babynames %>%
  group_by(year, sex) %>%
  summarise(UniqueNames = n_distinct(name)) %>%
  arrange(desc(UniqueNames))
namechange
```

```
## # A tibble: 276 x 3
## # Groups:   year [138]
##     year sex   UniqueNames
##    <dbl> <chr>       <int>
##  1  2007 F           20560
##  2  2008 F           20457
##  3  2009 F           20179
##  4  2006 F           20050
##  5  2010 F           19811
##  6  2011 F           19560
##  7  2012 F           19498
##  8  2013 F           19231
##  9  2005 F           19182
## 10  2014 F           19181
## # ... with 266 more rows
```

```r
g <- ggplot(data = namechange) +
  geom_smooth(mapping = aes(x = year, y = UniqueNames))
g + facet_wrap(~ sex)
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

<img src="HW5_Margasak_files/figure-html/unnamed-chunk-7-1.png" width="70%" style="display: block; margin: auto;" />

Based on the graphs, the number of unique names has jumped up for both boys and girls but it has been a significantly larger jump for girls since the 1960s.

7. Find the most popular names of the 1990s. 


```r
nineties <- babynames %>%
  filter(year >= 1990 & year < 2000) %>%
  group_by(name) %>%
  summarize(num_births = sum(n)) %>%
  arrange(desc(num_births))
nineties
```

```
## # A tibble: 45,928 x 2
##    name        num_births
##    <chr>            <int>
##  1 Michael         464249
##  2 Christopher     361251
##  3 Matthew         352341
##  4 Joshua          330046
##  5 Jessica         303854
##  6 Ashley          303125
##  7 Jacob           298926
##  8 Nicholas        275906
##  9 Andrew          273515
## 10 Daniel          273347
## # ... with 45,918 more rows
```

















