P8130_final_project
================
Leonor Rui
2024-12-03

# Appendix

- Data Import

``` r
survival_df = read_csv("data/Project_2_data.csv") |>
  janitor::clean_names()
```

- Data Description

``` r
str(survival_df)
```

    ## spc_tbl_ [4,024 × 16] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ age                   : num [1:4024] 68 50 58 58 47 51 51 40 40 69 ...
    ##  $ race                  : chr [1:4024] "White" "White" "White" "White" ...
    ##  $ marital_status        : chr [1:4024] "Married" "Married" "Divorced" "Married" ...
    ##  $ t_stage               : chr [1:4024] "T1" "T2" "T3" "T1" ...
    ##  $ n_stage               : chr [1:4024] "N1" "N2" "N3" "N1" ...
    ##  $ x6th_stage            : chr [1:4024] "IIA" "IIIA" "IIIC" "IIA" ...
    ##  $ differentiate         : chr [1:4024] "Poorly differentiated" "Moderately differentiated" "Moderately differentiated" "Poorly differentiated" ...
    ##  $ grade                 : chr [1:4024] "3" "2" "2" "3" ...
    ##  $ a_stage               : chr [1:4024] "Regional" "Regional" "Regional" "Regional" ...
    ##  $ tumor_size            : num [1:4024] 4 35 63 18 41 20 8 30 103 32 ...
    ##  $ estrogen_status       : chr [1:4024] "Positive" "Positive" "Positive" "Positive" ...
    ##  $ progesterone_status   : chr [1:4024] "Positive" "Positive" "Positive" "Positive" ...
    ##  $ regional_node_examined: num [1:4024] 24 14 14 2 3 18 11 9 20 21 ...
    ##  $ reginol_node_positive : num [1:4024] 1 5 7 1 1 2 1 1 18 12 ...
    ##  $ survival_months       : num [1:4024] 60 62 75 84 50 89 54 14 70 92 ...
    ##  $ status                : chr [1:4024] "Alive" "Alive" "Alive" "Alive" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   Age = col_double(),
    ##   ..   Race = col_character(),
    ##   ..   `Marital Status` = col_character(),
    ##   ..   `T Stage` = col_character(),
    ##   ..   `N Stage` = col_character(),
    ##   ..   `6th Stage` = col_character(),
    ##   ..   differentiate = col_character(),
    ##   ..   Grade = col_character(),
    ##   ..   `A Stage` = col_character(),
    ##   ..   `Tumor Size` = col_double(),
    ##   ..   `Estrogen Status` = col_character(),
    ##   ..   `Progesterone Status` = col_character(),
    ##   ..   `Regional Node Examined` = col_double(),
    ##   ..   `Reginol Node Positive` = col_double(),
    ##   ..   `Survival Months` = col_double(),
    ##   ..   Status = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

Numeric variables include `age`, `tumor_size`, `regional_node_examined`,
`reginol_node_positive`, and `survival_months`.

These are continuous variables that can be used for our later regression
analysis.

Categorical variables include `race`, `marital_status`, `t_stage`,
`n_stage`, `x6th_stage`, `differentiate`, `grade`, `a_stage`,
`estrogen_status`, `progesterone_status`, and `status`.

Then we will convert these variables into factors.

``` r
survival_df = survival_df |>
  mutate(
    race = factor(race),
    marital_status = factor(marital_status),
    t_stage = factor(t_stage),
    n_stage = factor(n_stage),
    x6th_stage = factor(x6th_stage),
    differentiate = factor(differentiate),
    grade = factor(grade),
    a_stage = factor(a_stage),
    estrogen_status = factor(estrogen_status),
    progesterone_status = factor(progesterone_status),
    status = factor(status)
  ) |>
  mutate(
    differentiate = factor(differentiate, levels = c("Well differentiated", 
                                                     "Moderately differentiated",
                                                     "Poorly differentiated",
                                                     "Undifferentiated")),
    differentiate = relevel(differentiate, ref = "Well differentiated")
  )
```

``` r
summary(survival_df)
```

    ##       age           race        marital_status t_stage   n_stage   x6th_stage 
    ##  Min.   :30.00   Black: 291   Divorced : 486   T1:1603   N1:2732   IIA :1305  
    ##  1st Qu.:47.00   Other: 320   Married  :2643   T2:1786   N2: 820   IIB :1130  
    ##  Median :54.00   White:3413   Separated:  45   T3: 533   N3: 472   IIIA:1050  
    ##  Mean   :53.97                Single   : 615   T4: 102             IIIB:  67  
    ##  3rd Qu.:61.00                Widowed  : 235                       IIIC: 472  
    ##  Max.   :69.00                                                                
    ##                    differentiate                   grade          a_stage    
    ##  Well differentiated      : 543   1                   : 543   Distant :  92  
    ##  Moderately differentiated:2351   2                   :2351   Regional:3932  
    ##  Poorly differentiated    :1111   3                   :1111                  
    ##  Undifferentiated         :  19   anaplastic; Grade IV:  19                  
    ##                                                                              
    ##                                                                              
    ##    tumor_size     estrogen_status progesterone_status regional_node_examined
    ##  Min.   :  1.00   Negative: 269   Negative: 698       Min.   : 1.00         
    ##  1st Qu.: 16.00   Positive:3755   Positive:3326       1st Qu.: 9.00         
    ##  Median : 25.00                                       Median :14.00         
    ##  Mean   : 30.47                                       Mean   :14.36         
    ##  3rd Qu.: 38.00                                       3rd Qu.:19.00         
    ##  Max.   :140.00                                       Max.   :61.00         
    ##  reginol_node_positive survival_months   status    
    ##  Min.   : 1.000        Min.   :  1.0   Alive:3408  
    ##  1st Qu.: 1.000        1st Qu.: 56.0   Dead : 616  
    ##  Median : 2.000        Median : 73.0               
    ##  Mean   : 4.158        Mean   : 71.3               
    ##  3rd Qu.: 5.000        3rd Qu.: 90.0               
    ##  Max.   :46.000        Max.   :107.0

The majority of patients in the dataset are White, accounting for
approximately 84.82% of the total population. Black patients make up
7.23%, and patients classified as “Other” constitute 7.95%. This
imbalance suggests that the dataset is heavily skewed towards White
patients, which could influence the generalizability of the findings to
other racial groups.

The wide range of values in variables such as `tumor_size`,
`regional_node_examined`, and `survival_months` indicates the need to
explore relationships and their potential nonlinearities with survival,
giving us a possible analytical regression model.

``` r
colSums(is.na(survival_df))
```

    ##                    age                   race         marital_status 
    ##                      0                      0                      0 
    ##                t_stage                n_stage             x6th_stage 
    ##                      0                      0                      0 
    ##          differentiate                  grade                a_stage 
    ##                      0                      0                      0 
    ##             tumor_size        estrogen_status    progesterone_status 
    ##                      0                      0                      0 
    ## regional_node_examined  reginol_node_positive        survival_months 
    ##                      0                      0                      0 
    ##                 status 
    ##                      0

We can conclude that no missing values are present in this dataset
across all variables.

``` r
survival_df |>
  group_by(differentiate, race) |>
  summarise(count = n(), .groups = "drop") |>
  pivot_wider(
    names_from = differentiate,
    values_from = count,
    values_fill = list(count = 0)
  )
```

    ## # A tibble: 3 × 5
    ##   race  `Well differentiated` `Moderately differentiated` Poorly differentiate…¹
    ##   <fct>                 <int>                       <int>                  <int>
    ## 1 Black                    32                         141                    115
    ## 2 Other                    46                         180                     94
    ## 3 White                   465                        2030                    902
    ## # ℹ abbreviated name: ¹​`Poorly differentiated`
    ## # ℹ 1 more variable: Undifferentiated <int>

This table shows the frequency of different levels of `differentiate` by
races.

``` r
survival_df |>
  group_by(x6th_stage, status) |>
  summarise(count = n(), .groups = "drop") |>
  pivot_wider(
    names_from = status,
    values_from = count
  )
```

    ## # A tibble: 5 × 3
    ##   x6th_stage Alive  Dead
    ##   <fct>      <int> <int>
    ## 1 IIA         1209    96
    ## 2 IIB          995   135
    ## 3 IIIA         866   184
    ## 4 IIIB          47    20
    ## 5 IIIC         291   181

This table shows the frequency of different levels of `status` by 6th
stage.

- Data Visualization

# Distributions of the numeric variables

## Age

``` r
survival_df |> 
  ggplot(aes(age)) + 
  geom_histogram(fill = "light blue", color = "black") + 
  theme_minimal() +
  labs(
    title = "Distribution of age",
    x = "Age",
    y = "Frequency"
  )
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

The histogram shows the age distribution of patients. Most patients are
aged between 40 and 70 years. The data is well spread across middle and
older age groups, making it possible for age-related analysis.
Therefore, age will likely be a significant predictor for later
analysis.

## Tumor size

``` r
ggplot(survival_df, aes(x = tumor_size)) +
  geom_histogram(fill = "light blue", color = "black") +
  scale_x_continuous(breaks = seq(0, max(survival_df$tumor_size, na.rm = TRUE), by = 5)) +
  labs(
    title = "Distribution of Tumor Size",
    x = "Tumor Size (mm)",
    y = "Frequency"
  )
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-9-1.png" width="90%" />

This is the distribution of all tumor sizes, and most of the tumor sizes
are smaller than 50 mm. We can find that the most frequent size is
around 19 mm, followed by around 14 mm. This distribution is
right-skewed, so we will use the log transformation for this variable.

## Examined regional node

``` r
ggplot(survival_df, aes(x = regional_node_examined)) +
  geom_histogram(fill = "light blue", color = "black") +
  scale_x_continuous(breaks = seq(0, max(survival_df$regional_node_examined, na.rm = TRUE), by = 5)) +
  labs(
    title = "Distribution of Examined Regional Node",
    x = "Examined Regional Node",
    y = "Frequency"
  )
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />

This plot maps the frequency of different number of examined regional
nodes for each subject. The number of examined regional nodes for most
subjects are smaller than 30, and the subjects with nearly 12 examined
regional nodes are the most.

## Positive regional node

``` r
ggplot(survival_df, aes(x = reginol_node_positive)) +
  geom_histogram(fill = "light blue", color = "black") +
  scale_x_continuous(breaks = seq(0, max(survival_df$reginol_node_positive, na.rm = TRUE), by = 5)) +
  labs(
    title = "Distribution of Positive Reginol Node",
    x = "Positive Reginol Node",
    y = "Frequency"
  )
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-11-1.png" width="90%" />

Then is the distribution of different number of positive reginol node
for each subject. Over 2500 subjects only have 1 or 2 positive reginol
nodes, which is the most frequent number of positive reginol nodes. It
is strongly right-skewed, so we will use the log transformation for this
variable.

## Cancer Grades

``` r
ggplot(survival_df, aes(x = grade)) +
    geom_bar(fill = "skyblue") +
    labs(title = "Frequency Distribution of Cancer Grades",
         x = "Grade",
         y = "Count") +
    theme_minimal()
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-12-1.png" width="90%" />

This bar chart provides an overview of how cancer cases are distributed
across grades. Grade 2 represents the majority of cases, suggesting it
is the most frequently observed grade, while Grade IV is exceedingly
rare.

# Bewteen Variables

## Survival months by status

``` r
ggplot(survival_df, aes(x = survival_months, fill = status)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Distribution of Survival Months", x = "Survival Months", y = "Frequency") +
  theme_minimal()
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-13-1.png" width="90%" />

The Dead group is concentrated in the shorter survival months, while the
Alive group is predominant in longer survival months, particularly
beyond 60 months.

## Tumor sizes by t_stage

``` r
ggplot(survival_df, aes(x = tumor_size, y = t_stage)) +
  geom_boxplot(fill = "light blue") +
  scale_x_continuous(breaks = seq(0, max(survival_df$tumor_size, na.rm = TRUE), by = 10)) +
  labs(
    title = "Distribution of Tumor Sizes by T_stage",
    x = "Tumor Sizes",
    y = "T_stage"
  )
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-14-1.png" width="90%" />

In this plot, we explore the tumor size distribution at different T
stages. From T1 to T3, as the stage changes, both the mean tumor sizes
and IQR become larger. At T4 stage, the IQR of tumor sizes is much
larger than others, and the mean size is smaller than the mean size at
T3 stage. There are some outliers both ar T1 stage and T3 stage.

## Survival months by a_stage based on status(alive/dead)

``` r
ggplot(survival_df, aes(x = survival_months, y = a_stage)) +
  geom_boxplot(fill = "light blue") +
  scale_x_continuous(breaks = seq(0, max(survival_df$survival_months, na.rm = TRUE), by = 10)) +
  labs(
    title = "Distribution of Survival Months by A_stage",
    x = "Survival Months",
    y = "A_stage"
  ) +
  facet_grid(~ status)
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-15-1.png" width="90%" />

Through this plot, we can find that subjects with Distant stage have
fewer survival months than subjects with Regional stage. However, the
IQR of the survival months of subjects with Distant stage is much larger
than subjects with Regional stage.

## Estrogen Status by Tumor Size Based on race

``` r
ggplot(survival_df, aes(x = progesterone_status, y = tumor_size)) +
  geom_boxplot(fill = "light blue") +
  labs(
    title = "Distribution of Estrogen Status by Tumor Size",
    x = "Estrogen Status",
    y = "Tumor Size"
  ) +
  facet_grid(~ race)
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-16-1.png" width="90%" />

While the overall patterns are consistent, with Negative estrogen status
generally associated with slightly larger tumor sizes, the variability
and prevalence of outliers differ between groups. The White group shows
the greatest spread in tumor size, while the Other group displays the
least variability.

## Tumor Size by Grade

``` r
ggplot(survival_df, aes(x = grade, y = tumor_size)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Tumor Size Distribution by Grade",
       x = "Grade",
       y = "Tumor Size (mm)") +
  theme_minimal()
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-17-1.png" width="90%" />

Lower grades (1–3) exhibit comparable tumor size distributions, with
slight increases in variability as the grade increases.

Grade IV stands out due to its higher median and broader range,
suggesting that more aggressive tumor grades are associated with larger
tumor sizes.

## Proportion of Grade by Estrogen Status

``` r
ggplot(survival_df, aes(x = grade, fill = estrogen_status)) +
    geom_bar(position = "fill") +
    labs(title = "Proportion of Grade by Estrogen Status",
         x = "Grade",
         y = "Proportion",
         fill = "Race") +
    theme_minimal()
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-18-1.png" width="90%" />

As tumor grade increases, the proportion of Negative estrogen status
gradually increases, becoming more prominent in the anaplastic Grade IV
category. Conversely, the dominance of the Positive estrogen status
decreases with higher tumor grades.

## Proportion of Grade by Marital Status

``` r
ggplot(survival_df, aes(x = grade, fill = marital_status)) +
    geom_bar(position = "fill") +
    labs(title = "Proportion of Grade by Marital Status",
         x = "Grade",
         y = "Proportion",
         fill = "Race") +
    theme_minimal()
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-19-1.png" width="90%" />

Across all grades, the “Married” group consistently constitutes the
largest proportion of individuals, dominating every tumor grade
category.

The “Single” group is the second-largest proportion in most grades,
particularly Grades 2 and 3.

The “Widowed” group and “Divorced” group make up smaller proportions
across all tumor grades.

## Relationship Between Age and Tumor Size across status

``` r
ggplot(survival_df, aes(x = age, y = tumor_size, color = status)) +
    geom_point(alpha = 0.5, size = 0.6) +
  geom_smooth(method = "lm", color = "green") +
  facet_wrap(~ status) +
    labs(title = "Relationship Between Age and Tumor Size",
         x = "Age (years)",
         y = "Tumor Size (mm)",
         color = "Status") +
    theme_minimal()
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-20-1.png" width="90%" />

This figure highlights the differences in tumor size distribution and
trends with age between individuals who are alive and those who are
deceased. While the “Alive” group shows no significant relationship
between age and tumor size, the “Dead” group exhibits a pattern where
larger tumors are associated with younger ages.

## Age vs. Tumor Size Across Grades

``` r
ggplot(survival_df, aes(x = age, y = tumor_size)) +
    geom_point(color = "lightblue", size = 0.8, alpha = 0.5) +
  geom_smooth(method = "lm") +
    facet_wrap(~ marital_status) +
    labs(title = "Age vs. Tumor Size Across Grades",
         x = "Age (years)",
         y = "Tumor Size (mm)") +
    theme_minimal()
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-21-1.png" width="90%" />

Divorced: Tumor size seems to remain fairly constant with age, as the
trend line is relatively flat.

Married: A slight negative trend is observable, suggesting that tumor
size may decrease marginally with age.

Separated: The data is sparse, but the trend shows a slightly negative
relationship, with wide confidence intervals due to fewer observations.

Single: A modest negative trend is observed, indicating a potential
decline in tumor size with increasing age.

Widowed: A more apparent negative trend is evident compared to other
groups, suggesting a stronger decrease in tumor size with age.

## Positive Reginol Node vs Survival Months Across Cancer Grade

``` r
ggplot(survival_df, aes(x = reginol_node_positive, y = survival_months)) +
  geom_point(color = "light blue", size = 0.5, alpha = 0.5)  +
  facet_wrap(.~grade) +
  geom_smooth(method = "lm") +
  labs(
    title = "Distribution of Positive Reginol Node and Survival Months by Cancer Grade",
    x = "Positive Reginol Node",
    y = "Survival Months"
  ) +
  theme_minimal()
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-22-1.png" width="90%" />

According to the trend lines, as the cancer grade increases, the
negative correlation between the number of positive reginol nodes and
the survival months becomes stronger. At the Grade IV, the correlation
is strong. AS the number of positive reginol nodes increases, the
survival months will decrease.

# Transformations

``` r
survival_df = survival_df |>
  mutate(
    log_tumor_size = log(tumor_size),
    log_reginol_node_positive = log(reginol_node_positive)
  )
```

Since variables `tumor_size` and `reginol_node_positive` are skewed to
the right, we need to use the log transformation and add new variables
`log_tumor_size` and `log_reginol_node_positive` for further analysis.

``` r
survival_df |> 
  pivot_longer(
    cols = c(age, tumor_size, regional_node_examined, reginol_node_positive, survival_months),
    names_to = "variable",
    values_to = "value"
  ) |>
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(variable ~ .,  scales = "free")
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-24-1.png" width="90%" />

# Model Building

## Preparation

``` r
full_glm = glm(status ~ age + race + marital_status + t_stage + n_stage + x6th_stage +
                 differentiate + grade + a_stage + tumor_size + estrogen_status +
                 progesterone_status + regional_node_examined + reginol_node_positive, 
               data = survival_df, family = binomial)

alias(full_glm)$Complete %>%
  as.data.frame() %>%  
  rownames_to_column("aliased_variables") %>% 
  as.tibble() %>% 
  pivot_longer(
    cols = -aliased_variables,
    names_to = "aliased_with", 
    values_to = "value"
  ) %>% 
  filter(value != 0) %>% 
  select(-value)
```

    ## # A tibble: 4 × 2
    ##   aliased_variables         aliased_with                          
    ##   <chr>                     <chr>                                 
    ## 1 x6th_stageIIIC            n_stageN3                             
    ## 2 grade2                    differentiateModerately differentiated
    ## 3 grade3                    differentiatePoorly differentiated    
    ## 4 gradeanaplastic; Grade IV differentiateUndifferentiated

First, we use variables 1-14 to build a logistic regression. In this
full model, we observe that the number of each level in the variable
`grade` is the same as the variable `differentiate`, and the number of
level `IIIC` in the variable `x6th_stage` is exactly the same with level
`N3` in the variable `n_stage`.

By looking up the relevant information on the staging system for breast
cancer, we can see that the breast cancer grade (i.e. variable `grade`)
is based on how much the cancer cells look like normal cells, which is
highly similar to the meaning of the variable `differentiate`.
Therefore, the variable `grade` can be removed from the model.

In addition, the AJCC system (variable `x6th_stage`) is based on 7
aspects: the extent (size) of the tumor (T), the spread to nearby lymph
nodes (N), the spread (metastasis) to distant sites (M), Estrogen
Receptor (ER) status, Progesterone Receptor (PR) status, HER2 status and
grade of the cancer (G). It can be seen that some of the evaluation
criteria are already included in other variables.

However, since the AJCC system is complex and levels other than `IIIC`
do not correlate with other variables, we cannot simply remove this
variable from the model. We will discuss this variable further when it
comes to this.

## Logistic Regression Model

Adjusted R^2 increases as predictor number increases, but need to
simplify the model.

``` r
survival_df = 
  survival_df |>
  mutate(status = if_else(status == "Dead", 1, 0))

subsets = regsubsets(status ~ age + race + marital_status + t_stage + n_stage + x6th_stage +
                       differentiate + grade + a_stage + tumor_size + estrogen_status +
                       progesterone_status + regional_node_examined + reginol_node_positive, 
                     data = survival_df, nbest = 1)
```

    ## Reordering variables and trying again:

``` r
subset_summary = summary(subsets)

plot(subset_summary$adjr2, type = "b", 
     xlab = "Number of Predictors", ylab = "Adjusted R^2", 
     main = "Distribution of Adjusted R^2 of Different Models")
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-26-1.png" width="90%" />

Automated procedure

``` r
full_glm = glm(status ~ age + race + marital_status + t_stage + n_stage + x6th_stage +
                 differentiate + a_stage + tumor_size + estrogen_status +
                 progesterone_status + regional_node_examined + reginol_node_positive, 
               data = survival_df, family = binomial)

step(full_glm)
```

    ## Start:  AIC=3002
    ## status ~ age + race + marital_status + t_stage + n_stage + x6th_stage + 
    ##     differentiate + a_stage + tumor_size + estrogen_status + 
    ##     progesterone_status + regional_node_examined + reginol_node_positive
    ## 
    ##                          Df Deviance    AIC
    ## - x6th_stage              3   2953.7 2997.7
    ## - tumor_size              1   2952.0 3000.0
    ## - a_stage                 1   2952.0 3000.0
    ## - t_stage                 3   2957.2 3001.2
    ## <none>                        2952.0 3002.0
    ## - marital_status          4   2960.1 3002.1
    ## - n_stage                 1   2958.9 3006.9
    ## - race                    2   2967.1 3013.1
    ## - estrogen_status         1   2969.2 3017.2
    ## - age                     1   2970.8 3018.8
    ## - progesterone_status     1   2972.0 3020.0
    ## - regional_node_examined  1   2978.7 3026.7
    ## - reginol_node_positive   1   2978.8 3026.8
    ## - differentiate           3   2986.0 3030.0
    ## 
    ## Step:  AIC=2997.73
    ## status ~ age + race + marital_status + t_stage + n_stage + differentiate + 
    ##     a_stage + tumor_size + estrogen_status + progesterone_status + 
    ##     regional_node_examined + reginol_node_positive
    ## 
    ##                          Df Deviance    AIC
    ## - tumor_size              1   2953.8 2995.8
    ## - a_stage                 1   2953.8 2995.8
    ## <none>                        2953.7 2997.7
    ## - marital_status          4   2961.7 2997.7
    ## - n_stage                 2   2965.5 3005.5
    ## - t_stage                 3   2968.8 3006.8
    ## - race                    2   2969.2 3009.2
    ## - estrogen_status         1   2971.2 3013.2
    ## - age                     1   2972.1 3014.1
    ## - progesterone_status     1   2973.6 3015.6
    ## - regional_node_examined  1   2980.4 3022.4
    ## - reginol_node_positive   1   2981.1 3023.1
    ## - differentiate           3   2987.4 3025.4
    ## 
    ## Step:  AIC=2995.75
    ## status ~ age + race + marital_status + t_stage + n_stage + differentiate + 
    ##     a_stage + estrogen_status + progesterone_status + regional_node_examined + 
    ##     reginol_node_positive
    ## 
    ##                          Df Deviance    AIC
    ## - a_stage                 1   2953.8 2993.8
    ## <none>                        2953.8 2995.8
    ## - marital_status          4   2961.8 2995.8
    ## - n_stage                 2   2965.7 3003.7
    ## - race                    2   2969.2 3007.2
    ## - estrogen_status         1   2971.2 3011.2
    ## - age                     1   2972.1 3012.1
    ## - progesterone_status     1   2973.7 3013.7
    ## - t_stage                 3   2980.8 3016.8
    ## - regional_node_examined  1   2980.6 3020.6
    ## - reginol_node_positive   1   2981.1 3021.1
    ## - differentiate           3   2987.4 3023.4
    ## 
    ## Step:  AIC=2993.77
    ## status ~ age + race + marital_status + t_stage + n_stage + differentiate + 
    ##     estrogen_status + progesterone_status + regional_node_examined + 
    ##     reginol_node_positive
    ## 
    ##                          Df Deviance    AIC
    ## <none>                        2953.8 2993.8
    ## - marital_status          4   2961.8 2993.8
    ## - n_stage                 2   2965.8 3001.8
    ## - race                    2   2969.3 3005.3
    ## - estrogen_status         1   2971.3 3009.3
    ## - age                     1   2972.1 3010.1
    ## - progesterone_status     1   2973.7 3011.7
    ## - t_stage                 3   2982.7 3016.7
    ## - regional_node_examined  1   2980.6 3018.6
    ## - reginol_node_positive   1   2981.1 3019.1
    ## - differentiate           3   2987.4 3021.4

    ## 
    ## Call:  glm(formula = status ~ age + race + marital_status + t_stage + 
    ##     n_stage + differentiate + estrogen_status + progesterone_status + 
    ##     regional_node_examined + reginol_node_positive, family = binomial, 
    ##     data = survival_df)
    ## 
    ## Coefficients:
    ##                            (Intercept)                                     age  
    ##                               -2.28383                                 0.02380  
    ##                              raceOther                               raceWhite  
    ##                               -0.93458                                -0.51482  
    ##                  marital_statusMarried                 marital_statusSeparated  
    ##                               -0.21103                                 0.66914  
    ##                   marital_statusSingle                   marital_statusWidowed  
    ##                               -0.06461                                 0.01749  
    ##                              t_stageT2                               t_stageT3  
    ##                                0.41111                                 0.55159  
    ##                              t_stageT4                               n_stageN2  
    ##                                1.09878                                 0.43629  
    ##                              n_stageN3  differentiateModerately differentiated  
    ##                                0.58717                                 0.53276  
    ##     differentiatePoorly differentiated           differentiateUndifferentiated  
    ##                                0.91903                                 1.86486  
    ##                estrogen_statusPositive             progesterone_statusPositive  
    ##                               -0.74802                                -0.58416  
    ##                 regional_node_examined                   reginol_node_positive  
    ##                               -0.03594                                 0.07968  
    ## 
    ## Degrees of Freedom: 4023 Total (i.e. Null);  4004 Residual
    ## Null Deviance:       3445 
    ## Residual Deviance: 2954  AIC: 2994

``` r
automated_glm = glm(formula = status ~ age + race + marital_status + t_stage + n_stage +
                      differentiate + estrogen_status + progesterone_status +
                      regional_node_examined + reginol_node_positive, 
                    family = binomial, data = survival_df)

summary(automated_glm)
```

    ## 
    ## Call:
    ## glm(formula = status ~ age + race + marital_status + t_stage + 
    ##     n_stage + differentiate + estrogen_status + progesterone_status + 
    ##     regional_node_examined + reginol_node_positive, family = binomial, 
    ##     data = survival_df)
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                            -2.283832   0.438484  -5.208 1.90e-07
    ## age                                     0.023801   0.005610   4.243 2.21e-05
    ## raceOther                              -0.934582   0.248456  -3.762 0.000169
    ## raceWhite                              -0.514824   0.161664  -3.185 0.001450
    ## marital_statusMarried                  -0.211035   0.141636  -1.490 0.136229
    ## marital_statusSeparated                 0.669140   0.388139   1.724 0.084713
    ## marital_statusSingle                   -0.064613   0.174816  -0.370 0.711675
    ## marital_statusWidowed                   0.017493   0.221070   0.079 0.936929
    ## t_stageT2                               0.411113   0.113030   3.637 0.000276
    ## t_stageT3                               0.551595   0.148772   3.708 0.000209
    ## t_stageT4                               1.098780   0.244529   4.493 7.01e-06
    ## n_stageN2                               0.436292   0.128371   3.399 0.000677
    ## n_stageN3                               0.587168   0.234546   2.503 0.012300
    ## differentiateModerately differentiated  0.532760   0.183777   2.899 0.003744
    ## differentiatePoorly differentiated      0.919027   0.192377   4.777 1.78e-06
    ## differentiateUndifferentiated           1.864863   0.553833   3.367 0.000759
    ## estrogen_statusPositive                -0.748017   0.177506  -4.214 2.51e-05
    ## progesterone_statusPositive            -0.584158   0.127516  -4.581 4.63e-06
    ## regional_node_examined                 -0.035938   0.007172  -5.011 5.41e-07
    ## reginol_node_positive                   0.079684   0.015301   5.208 1.91e-07
    ##                                           
    ## (Intercept)                            ***
    ## age                                    ***
    ## raceOther                              ***
    ## raceWhite                              ** 
    ## marital_statusMarried                     
    ## marital_statusSeparated                .  
    ## marital_statusSingle                      
    ## marital_statusWidowed                     
    ## t_stageT2                              ***
    ## t_stageT3                              ***
    ## t_stageT4                              ***
    ## n_stageN2                              ***
    ## n_stageN3                              *  
    ## differentiateModerately differentiated ** 
    ## differentiatePoorly differentiated     ***
    ## differentiateUndifferentiated          ***
    ## estrogen_statusPositive                ***
    ## progesterone_statusPositive            ***
    ## regional_node_examined                 ***
    ## reginol_node_positive                  ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3444.7  on 4023  degrees of freedom
    ## Residual deviance: 2953.8  on 4004  degrees of freedom
    ## AIC: 2993.8
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
vif(automated_glm)
```

    ##                            GVIF Df GVIF^(1/(2*Df))
    ## age                    1.107154  1        1.052214
    ## race                   1.062865  2        1.015359
    ## marital_status         1.129117  4        1.015295
    ## t_stage                1.101923  3        1.016308
    ## n_stage                3.806780  2        1.396817
    ## differentiate          1.117097  3        1.018627
    ## estrogen_status        1.475424  1        1.214670
    ## progesterone_status    1.427536  1        1.194795
    ## regional_node_examined 1.477809  1        1.215651
    ## reginol_node_positive  4.248357  1        2.061154

Variance Inflation Factor is a commonly used method for detecting
multicollinearity in regression models. VIF is generally calculated for
the continuous variables, and Generalized Variance Inflation Factor
(GVIF) is used for evaluating the multicollinearity for categorical
variables.

The adjusted GVIF (i.e. GVIF^(1/(2\*Df))) values are corrected for the
degree of freedom and provide a scale similar to VIF. The high adjusted
GVIF values (GVIF \> 2) indicate the presence of moderate to strong
multicollinearity.

The table shows that most variables do not show multicollinearity, with
the exception of `reginol_node_positive`. Since its adjusted GVIF is not
much different from 2, we will keep this variable for now.

### Model Diagnostics

``` r
augment(automated_glm) |>
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted value", y = "Residual")
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-28-1.png" width="90%" />

``` r
augment_quantile(automated_glm) |>
  ggplot(aes(x = .fitted, y = .quantile.resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted value", y = "Randomized quantile residual")
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-28-2.png" width="90%" />

By randomizing the quantile residuals, we resolve the problem that the
RVF plot always shows a pattern in logistic regression because of the
binary response variable. Since in the randomized quantile residual
vs. fitted value plot, the residuals distribute randomly around the 0.5
horizontal line, the residual assumption is met and the model is a good
fit.

``` r
plot(automated_glm, which = 5)
```

<img src="P8130_final_project_files/figure-gfm/unnamed-chunk-29-1.png" width="90%" />

The residual vs. leverage plot indicates that observations 3527, 1561,
and 3074 may be potential outliers, but they are not necessarily
influential.

### Likelihood ratio test

Test the validity of the automated logistic regression model

``` r
lrtest(automated_glm, full_glm)
```

    ## Likelihood ratio test
    ## 
    ## Model 1: status ~ age + race + marital_status + t_stage + n_stage + differentiate + 
    ##     estrogen_status + progesterone_status + regional_node_examined + 
    ##     reginol_node_positive
    ## Model 2: status ~ age + race + marital_status + t_stage + n_stage + x6th_stage + 
    ##     differentiate + a_stage + tumor_size + estrogen_status + 
    ##     progesterone_status + regional_node_examined + reginol_node_positive
    ##   #Df  LogLik Df  Chisq Pr(>Chisq)
    ## 1  20 -1476.9                     
    ## 2  25 -1476.0  5 1.7712     0.8798

``` r
final_glm = automated_glm
```

According to the likelihood ratio test, the full model is not
significantly more effective in predicting survival status than the
automated model, so we will use the automated model as the final
logistic model.

### Odds Ratios

``` r
final_glm_summary = summary(final_glm)

final_glm_df = 
  as.data.frame(final_glm_summary$coefficients) |>
  janitor::clean_names() |>
  mutate(
    odds_ratio = exp(estimate)
  ) |>
  rename(p_value = pr_z)

final_glm_df
```

    ##                                           estimate   std_error     z_value
    ## (Intercept)                            -2.28383153 0.438483562 -5.20847695
    ## age                                     0.02380073 0.005609963  4.24258193
    ## raceOther                              -0.93458158 0.248455746 -3.76156155
    ## raceWhite                              -0.51482432 0.161663812 -3.18453657
    ## marital_statusMarried                  -0.21103457 0.141635616 -1.48998234
    ## marital_statusSeparated                 0.66913999 0.388138549  1.72397201
    ## marital_statusSingle                   -0.06461326 0.174815803 -0.36960768
    ## marital_statusWidowed                   0.01749330 0.221070476  0.07912998
    ## t_stageT2                               0.41111286 0.113029538  3.63721613
    ## t_stageT3                               0.55159473 0.148771968  3.70765235
    ## t_stageT4                               1.09878030 0.244529438  4.49344792
    ## n_stageN2                               0.43629245 0.128370873  3.39868728
    ## n_stageN3                               0.58716755 0.234545523  2.50342683
    ## differentiateModerately differentiated  0.53276008 0.183776817  2.89895151
    ## differentiatePoorly differentiated      0.91902700 0.192377125  4.77721560
    ## differentiateUndifferentiated           1.86486331 0.553833001  3.36719427
    ## estrogen_statusPositive                -0.74801661 0.177506343 -4.21402748
    ## progesterone_statusPositive            -0.58415843 0.127515990 -4.58106024
    ## regional_node_examined                 -0.03593764 0.007171744 -5.01100482
    ## reginol_node_positive                   0.07968369 0.015301426  5.20759867
    ##                                             p_value odds_ratio
    ## (Intercept)                            1.903970e-07  0.1018931
    ## age                                    2.209628e-05  1.0240862
    ## raceOther                              1.688559e-04  0.3927502
    ## raceWhite                              1.449860e-03  0.5976056
    ## marital_statusMarried                  1.362289e-01  0.8097461
    ## marital_statusSeparated                8.471290e-02  1.9525574
    ## marital_statusSingle                   7.116748e-01  0.9374299
    ## marital_statusWidowed                  9.369292e-01  1.0176472
    ## t_stageT2                              2.756007e-04  1.5084956
    ## t_stageT3                              2.091896e-04  1.7360193
    ## t_stageT4                              7.007917e-06  3.0005041
    ## n_stageN2                              6.771009e-04  1.5469611
    ## n_stageN3                              1.229971e-02  1.7988859
    ## differentiateModerately differentiated 3.744128e-03  1.7036280
    ## differentiatePoorly differentiated     1.777392e-06  2.5068500
    ## differentiateUndifferentiated          7.593717e-04  6.4550535
    ## estrogen_statusPositive                2.508567e-05  0.4733044
    ## progesterone_statusPositive            4.626245e-06  0.5575749
    ## regional_node_examined                 5.414656e-07  0.9647004
    ## reginol_node_positive                  1.913001e-07  1.0829445

### Cross Validation

``` r
log_loss = function(actual, predicted) {
  
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
  
}

glm_fit = function(data) {
  
  fit = glm(formula = status ~ age + race + marital_status + t_stage + n_stage + 
        differentiate + estrogen_status + progesterone_status + 
        regional_node_examined + reginol_node_positive, 
        family = binomial, data = data)
  
  return(fit)
  
}

cv_df = 
  crossv_kfold(survival_df, k = 5) |>
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  )


cv_res_df = 
  cv_df |>
  mutate(
    final_model = map(train, \(x) glm_fit(data = x)),
    log_loss = map2_dbl(final_model, test, \(model, test_data) {
      predicted_probs = predict(model, newdata = test_data, type = "response")
      actual_outcomes = test_data$status
      log_loss(actual_outcomes, predicted_probs)
    })
  )

cv_res_df = 
  cv_df |>
  mutate(
    final_model = map(train, \(x) glm_fit(data = x)),
    predicted_probs = map2(final_model, test, \(fit, data)
                               predict(fit, newdata = data, type = "response")),
    actual_outcomes = map(test, pull, status),
    log_loss = map2_dbl(actual_outcomes, predicted_probs, \(x, y) log_loss(x, y)),
    auc = map2_dbl(actual_outcomes, predicted_probs, \(x, y) {
      roc_obj = roc(x, y)  
      auc(roc_obj)  
    })
  )

cv_res_df %>% 
  select(log_loss, auc)
```

    ## # A tibble: 5 × 2
    ##   log_loss   auc
    ##      <dbl> <dbl>
    ## 1    0.373 0.720
    ## 2    0.365 0.708
    ## 3    0.385 0.773
    ## 4    0.371 0.764
    ## 5    0.364 0.763

``` r
CV_log_loss = mean(cv_res_df$log_loss)
CV_log_loss
```

    ## [1] 0.371593

Since the CV_log_loss is very small, the model is a good fit by cross
validation.

- Evaluate the performance of your model(s). Is your model achieving
  similar performance between the majority race group “White” and the
  minority “Black” (or “Black” + “Other”)? If not, could you try to
  improve the fairness (i.e., reducing the gap of prediction performance
  between the majority and minority) of your model(s)?

## Survival Analysis

``` r
surv_obj = Surv(time = survival_df$survival_months, event = survival_df$status)
```

### Kaplan Meier Curve

``` r
km_fit = survfit(surv_obj ~ 1, data = survival_df)

ggsurvfit(km_fit, type = "survival", linewidth = 1) +
  labs(x = "Months", 
       y = "Overall survival",
       title = "Kaplan-Meier Survival Curve") +
  add_confidence_interval() +
  add_risktable() +
  scale_ggsurvfit()
```

<img src="P8130_final_project_files/figure-gfm/km_curve-1.png" width="90%" />

### Cox Regression

``` r
cox_model = coxph(surv_obj ~ 
                    age + race + marital_status + t_stage + n_stage + x6th_stage +
                       differentiate + a_stage + tumor_size + estrogen_status +
                       progesterone_status + regional_node_examined + reginol_node_positive, 
                  data = as.data.frame(survival_df))
cox.zph(cox_model)
```

    ##                          chisq df       p
    ## age                     0.1328  1 0.71556
    ## race                    0.9335  2 0.62702
    ## marital_status          2.6670  4 0.61500
    ## t_stage                 0.2144  3 0.97523
    ## n_stage                 1.7178  2 0.42362
    ## x6th_stage              3.8545  3 0.27761
    ## differentiate           1.8899  3 0.59557
    ## a_stage                 5.2218  1 0.02231
    ## tumor_size              0.9310  1 0.33460
    ## estrogen_status        28.9294  1 7.5e-08
    ## progesterone_status    32.1281  1 1.4e-08
    ## regional_node_examined  0.0187  1 0.89124
    ## reginol_node_positive   0.0324  1 0.85707
    ## GLOBAL                 57.2155 24 0.00016

``` r
ggforest(cox_model)
```

<img src="P8130_final_project_files/figure-gfm/forest_plot-1.png" width="90%" />

The x-axis of this forest plot represents hazard ratios. HR = 1 means no
significant difference compared to the reference, and a HR higher than 1
means it increases the hazard ratio of the event, death, and a HR lower
than 1 decreases it.
