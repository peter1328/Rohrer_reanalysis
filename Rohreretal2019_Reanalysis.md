---
title: "Rohreretal2019_Reanalysis"
output: 
  html_document: 
    df_print: default
    highlight: espresso
    keep_md: yes
    smart: no
    theme: spacelab
---
author: "Peter A. Edelsbrunner"
output:
  html_document:
    theme: cerulean
encoding: UTF-8
editor_options: 
  chunk_output_type: console
---

<style>
pre {
  overflow-x: auto;
}
pre code {
  word-wrap: normal;
  white-space: pre;
}
</style>

---

# {.tabset}

## Relevant parts for this re-analysis from the manuscript

### Test
 
 We tested students on five days in March—a different date for
 each school. Students were tested during their regular class meeting
 in the presence of their teacher and at least one author. Students
 who were absent on their assigned test day did not take the test. We
 asked teachers not to inform students of the test in advance, and
 teachers received no information about the test content in advance.
 Each test booklet included a cover sheet and four test pages,
 each printed on one side only. The test included four graph
 problems (page 1), four inequality problems (page 2), four expression
 problems (page 3), and four circle problems (page 4). We
 chose to block the test problems because some researchers have
 suggested that an interleaving test format would favor students
 who interleaved their practice, and, if this is true, our choice of a
 blocked format would have worked against an interleaving benefit.
 We chose the sequence of these blocks (graph problems, then
                                        inequality problems, then expression problems, and then circle
                                        problems) because we believe it ordered the four kinds of problems
 from least to most time-consuming. None of the test problems
 had appeared previously in the study. Every student saw the same
 test problems, but we created four versions of the test by reordering
 the problems within each page. An author distributed every test
 booklet to students and ensured that adjacent students received
 different test versions. In addition, teachers separated students’
 desks or required students to use dividers that ostensibly prevented
 them from seeing other students’ tests. Students were allotted 25
 min and allowed to use a calculator.
 Every test was scored at the school on the day of the test by the
 four authors and two research assistants. Scorers were blind to
 condition, and each answer was marked as correct or not. Two
 scorers independently scored each test. Discrepancies were rare
 (83 in 12,592), and the four authors later met and resolved each
 discrepancy. The internal consistency reliability of the test was
 high (for the 16 items, Cronbach’s alpha = .89).

### Worksheets

 The worksheets included critical problems and filler problems.
 The critical problems were like the kinds of problems seen on the
 test, and these consisted of four kinds: Expression (A), Inequality
 (B), Graph (C), and Circle (D). An example of each is shown in
 Figure 1.

### Results

 The effect
 size was large, Cohen’s d = 0.83, 95% CI = [0.68, 0.97], where
 d = (M1 - M2)/SDpooled, and M and SD are based on the studentlevel
 data (not class means). We observed a positive effect for each
 of the 15 teachers, ds = 0.23–1.48.
 We also found a positive interleaving effect for each of the four
 kinds of critical problems (A, B, C, and D), but the effect sizes are
 misleading. Ranked from largest to smallest, the Cohen’s d values
 for the four kinds equaled 0.86 (A), 0.63 (B), 0.40 (C), and 0.34
 (D), and this rank order corresponds to the order in which the
 blocked group saw these kinds of problems during the practice
 phase—not coincidentally, we believe. That is, the largest effect
 was observed for A problems (expressions), but the blocked group
 worked the A problems early in the practice phase, long before the
 test, thereby disadvantaging the blocked group and thus inflating
 the interleaving effect. By contrast, the D problems (circles) produced
 the smallest interleaving effect, yet the blocked group
 worked the D problems near the end of the practice phase, which
 shortened their test delay, and the shorter test delay likely boosted
 their test scores and thus dampened the interleaving effect. In brief,
these unavoidable scheduling confounds likely contributed to the
large differences in the effect sizes of the four kinds of critical
problems. However, there was no such confound for the critical
problems as a whole because, when averaged across all four kinds,
the time interval between each practice problem and the test date
was nearly equal for the two groups (the slight difference favored
the blocked condition).

## Data load & Preps


```r
library(brms)
```

```
## Loading required package: Rcpp
```

```
## Registered S3 methods overwritten by 'ggplot2':
##   method         from 
##   [.quosures     rlang
##   c.quosures     rlang
##   print.quosures rlang
```

```
## Registered S3 method overwritten by 'xts':
##   method     from
##   as.zoo.xts zoo
```

```
## Loading 'brms' package (version 2.8.0). Useful instructions
## can be found by typing help('brms'). A more detailed introduction
## to the package is available through vignette('brms_overview').
```

```r
library(plyr)
library(ggplot2)
library(lavaan)
```

```
## This is lavaan 0.6-3
```

```
## lavaan is BETA software! Please report any bugs.
```

```r
library(effsize)
library(tidyverse)
```

```
## -- Attaching packages -------------------------------------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --
```

```
## v tibble  2.1.1     v purrr   0.3.2
## v tidyr   0.8.3     v dplyr   0.8.1
## v readr   1.3.1     v stringr 1.4.0
## v tibble  2.1.1     v forcats 0.4.0
```

```
## -- Conflicts ----------------------------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::arrange()   masks plyr::arrange()
## x purrr::compact()   masks plyr::compact()
## x dplyr::count()     masks plyr::count()
## x dplyr::failwith()  masks plyr::failwith()
## x dplyr::filter()    masks stats::filter()
## x dplyr::id()        masks plyr::id()
## x dplyr::lag()       masks stats::lag()
## x dplyr::mutate()    masks plyr::mutate()
## x dplyr::rename()    masks plyr::rename()
## x dplyr::summarise() masks plyr::summarise()
## x dplyr::summarize() masks plyr::summarize()
```

```r
library(reshape)
```

```
## 
## Attaching package: 'reshape'
```

```
## The following object is masked from 'package:dplyr':
## 
##     rename
```

```
## The following objects are masked from 'package:tidyr':
## 
##     expand, smiths
```

```
## The following objects are masked from 'package:plyr':
## 
##     rename, round_any
```

```r
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
```


```r
setwd("C:/Users/petere/Downloads/Rohreretal2019JEPInterleaving_DataPreregSheets/")
rohrer <- read.csv("C:/Users/petere/Downloads/Rohreretal2019JEPInterleaving_DataPreregSheets/OSF_test_scores_rohrer.txt", header = TRUE, sep = "\t")
rohrer <- rohrer[, -22]
```


```r
rohrer$mean <- rowMeans(rohrer[, c(6:21)])
rohrer$mean_G <- rowMeans(rohrer[, c(6:9)])
rohrer$mean_I <- rowMeans(rohrer[, c(10:13)])
rohrer$mean_E <- rowMeans(rohrer[, c(14:17)])
rohrer$mean_C <- rowMeans(rohrer[, c(18:21)])

rohrer$Group_numeric <- as.numeric(rohrer$Group) -1
```


```r
rohrer_long <- melt(rohrer, id = c("School", "Teacher", "Class", "Group", "Student",
                                                            "mean",
                                                            "mean_G",
                                                            "mean_I",
                                                            "mean_E",
                                                            "mean_C",
                                   "Group_numeric"))

rohrer_long$dim <- ifelse(rohrer_long$variable == "G1" | rohrer_long$variable == "G2" | rohrer_long$variable == "G3" | rohrer_long$variable == "G4", "G", "NA")
rohrer_long$dim <- ifelse(rohrer_long$variable == "E1" | rohrer_long$variable == "E2" | rohrer_long$variable == "E3" | rohrer_long$variable == "E4", "E", rohrer_long$dim)
rohrer_long$dim <- ifelse(rohrer_long$variable == "I1" | rohrer_long$variable == "I2" | rohrer_long$variable == "I3" | rohrer_long$variable == "I4", "I", rohrer_long$dim)
rohrer_long$dim <- ifelse(rohrer_long$variable == "C1" | rohrer_long$variable == "C2" | rohrer_long$variable == "C3" | rohrer_long$variable == "C4", "C", rohrer_long$dim)
```

## Plots descriptives manifest effect sizes


```r
ggplot(rohrer, aes(x = Group, y = mean)) +
  geom_violin() +
  geom_boxplot()
```

![](Rohreretal2019_Reanalysis_files/figure-html/plots-1.png)<!-- -->

```r
ggplot(rohrer, aes(x = Group, y = mean)) +
  geom_violin() +
  geom_boxplot() +
  facet_wrap(~ Teacher)
```

![](Rohreretal2019_Reanalysis_files/figure-html/plots-2.png)<!-- -->

```r
names(rohrer)
```

```
##  [1] "School"        "Teacher"       "Class"         "Group"        
##  [5] "Student"       "G1"            "G2"            "G3"           
##  [9] "G4"            "I1"            "I2"            "I3"           
## [13] "I4"            "E1"            "E2"            "E3"           
## [17] "E4"            "C1"            "C2"            "C3"           
## [21] "C4"            "mean"          "mean_G"        "mean_I"       
## [25] "mean_E"        "mean_C"        "Group_numeric"
```

```r
melt(rohrer[23:26]) %>%
  ggplot(aes(x = variable, y = value)) +
  geom_violin()
```

```
## Using  as id variables
```

![](Rohreretal2019_Reanalysis_files/figure-html/plots-3.png)<!-- -->

```r
table(rohrer[23])
```

```
## 
##    0 0.25  0.5 0.75    1 
##  452   14    6   36  279
```

```r
table(rohrer[24])
```

```
## 
##    0 0.25  0.5 0.75    1 
##  236   40   85  111  315
```

```r
table(rohrer[25])
```

```
## 
##    0 0.25  0.5 0.75    1 
##  336  101   64  121  165
```

```r
table(rohrer[26])
```

```
## 
##    0 0.25  0.5 0.75    1 
##  182   29  201   32  343
```

```r
table(rohrer[22])
```

```
## 
##      0 0.0625  0.125 0.1875   0.25 0.3125  0.375 0.4375    0.5 0.5625 
##     52     26     58     40     65     44     49     37     65     48 
##  0.625 0.6875   0.75 0.8125  0.875 0.9375      1 
##     42     43     53     21     54     41     49
```

```r
hist(rohrer[, 22], breaks = 8, col = "blue")
```

![](Rohreretal2019_Reanalysis_files/figure-html/plots-4.png)<!-- -->

```r
#Note: the individual scales are strongly bimodal and also the overall scale visibly shows that some students master one type of problems, some two, some three, and some four - the individual item types have a strong bimodal nature. Difficult to analyze and make informative - one analytic aim would be to ditinguish between mastered or non-mastered, which would probably be nice to challenge with a mixture model - but a four-dimensional mixture is quite complicated, and also how to compute something d-equivalent from that?... Well, since the four individual scales have this strong floor vs. ceiling-nature, perhaps the overall Cohen's d of about .80 across all items is really not a bad measure of the effect. First I thought it's not good because the four scales intercorrelate only very mildly, but that might again at least partially be due to the strange floor/ceiling-structure...
```


```r
cohen.d(mean ~ Group, data = rohrer)
```

```
## 
## Cohen's d
## 
## d estimate: -0.8276764 (large)
## 95 percent confidence interval:
##      lower      upper 
## -0.9735001 -0.6818527
```

```r
cohen.d(mean_G ~ Group, data = rohrer)
```

```
## 
## Cohen's d
## 
## d estimate: -0.8643238 (large)
## 95 percent confidence interval:
##      lower      upper 
## -1.0106671 -0.7179806
```

```r
cohen.d(mean_I ~ Group, data = rohrer)
```

```
## 
## Cohen's d
## 
## d estimate: -0.6278861 (medium)
## 95 percent confidence interval:
##      lower      upper 
## -0.7712479 -0.4845242
```

```r
cohen.d(mean_E ~ Group, data = rohrer)
```

```
## 
## Cohen's d
## 
## d estimate: -0.3993891 (small)
## 95 percent confidence interval:
##      lower      upper 
## -0.5407326 -0.2580457
```

```r
cohen.d(mean_C ~ Group, data = rohrer)
```

```
## 
## Cohen's d
## 
## d estimate: -0.3356791 (small)
## 95 percent confidence interval:
##      lower      upper 
## -0.4766164 -0.1947418
```


```r
library(psych)
```

```
## 
## Attaching package: 'psych'
```

```
## The following object is masked from 'package:effsize':
## 
##     cohen.d
```

```
## The following object is masked from 'package:lavaan':
## 
##     cor2cov
```

```
## The following objects are masked from 'package:ggplot2':
## 
##     %+%, alpha
```

```
## The following object is masked from 'package:brms':
## 
##     cs
```

```r
describeBy(rohrer$mean, rohrer$Group)
```

```
## 
##  Descriptive statistics by group 
## group: block
##    vars   n mean   sd median trimmed  mad min max range skew kurtosis   se
## X1    1 389 0.38 0.27   0.31    0.36 0.28   0   1     1 0.45     -0.7 0.01
## -------------------------------------------------------- 
## group: inter
##    vars   n mean   sd median trimmed  mad min max range  skew kurtosis
## X1    1 398 0.61 0.29   0.62    0.62 0.37   0   1     1 -0.33    -1.05
##      se
## X1 0.01
```

```r
#Intercorrelations between overal score and scores on four dimensions
cor(subset(rohrer[22:27]))
```

```
##                    mean    mean_G    mean_I    mean_E    mean_C
## mean          1.0000000 0.7188844 0.7348007 0.6997546 0.6709773
## mean_G        0.7188844 1.0000000 0.3727190 0.3052155 0.2766474
## mean_I        0.7348007 0.3727190 1.0000000 0.3721497 0.3326658
## mean_E        0.6997546 0.3052155 0.3721497 1.0000000 0.3358695
## mean_C        0.6709773 0.2766474 0.3326658 0.3358695 1.0000000
## Group_numeric 0.3827815 0.3971055 0.2998582 0.1960555 0.1657187
##               Group_numeric
## mean              0.3827815
## mean_G            0.3971055
## mean_I            0.2998582
## mean_E            0.1960555
## mean_C            0.1657187
## Group_numeric     1.0000000
```

```r
cor(subset(rohrer[22:27], Group_numeric == 0)) #Block
```

```
## Warning in cor(subset(rohrer[22:27], Group_numeric == 0)):
## Standardabweichung ist Null
```

```
##                    mean    mean_G    mean_I    mean_E    mean_C
## mean          1.0000000 0.6078837 0.7370615 0.6481533 0.6541588
## mean_G        0.6078837 1.0000000 0.2891313 0.1764495 0.1748940
## mean_I        0.7370615 0.2891313 1.0000000 0.3236812 0.3028666
## mean_E        0.6481533 0.1764495 0.3236812 1.0000000 0.2391742
## mean_C        0.6541588 0.1748940 0.3028666 0.2391742 1.0000000
## Group_numeric        NA        NA        NA        NA        NA
##               Group_numeric
## mean                     NA
## mean_G                   NA
## mean_I                   NA
## mean_E                   NA
## mean_C                   NA
## Group_numeric             1
```

```r
cor(subset(rohrer[22:27], Group_numeric == 1)) #Inter
```

```
## Warning in cor(subset(rohrer[22:27], Group_numeric == 1)):
## Standardabweichung ist Null
```

```
##                    mean    mean_G    mean_I    mean_E    mean_C
## mean          1.0000000 0.7182238 0.6740949 0.7275720 0.6844480
## mean_G        0.7182238 1.0000000 0.2976268 0.3148883 0.2905593
## mean_I        0.6740949 0.2976268 1.0000000 0.3496459 0.2982286
## mean_E        0.7275720 0.3148883 0.3496459 1.0000000 0.3950329
## mean_C        0.6844480 0.2905593 0.2982286 0.3950329 1.0000000
## Group_numeric        NA        NA        NA        NA        NA
##               Group_numeric
## mean                     NA
## mean_G                   NA
## mean_I                   NA
## mean_E                   NA
## mean_C                   NA
## Group_numeric             1
```

## CFAs and SEMs


```r
CFA <- '
G =~ G1 + G2 + G3 + G4
I =~ I1 + I2 + I3 + I4
E =~ E1 + E2 + E3 + E4
C =~ C1 + C2 + C3 + C4'

CFA.fit <- cfa(CFA, data = rohrer, estimator = "WLSMV")
summary(CFA.fit, std = TRUE, fit = TRUE)
```

```
## lavaan 0.6-3 ended normally after 134 iterations
## 
##   Optimization method                           NLMINB
##   Number of free parameters                         38
## 
##   Number of observations                           787
## 
##   Estimator                                       DWLS      Robust
##   Model Fit Test Statistic                     413.939     592.278
##   Degrees of freedom                                98          98
##   P-value (Chi-square)                           0.000       0.000
##   Scaling correction factor                                  0.762
##   Shift parameter                                           48.711
##     for simple second-order correction (Mplus variant)
## 
## Model test baseline model:
## 
##   Minimum Function Test Statistic            31481.394   11861.048
##   Degrees of freedom                               120         120
##   P-value                                        0.000       0.000
## 
## User model versus baseline model:
## 
##   Comparative Fit Index (CFI)                    0.990       0.958
##   Tucker-Lewis Index (TLI)                       0.988       0.948
## 
##   Robust Comparative Fit Index (CFI)                            NA
##   Robust Tucker-Lewis Index (TLI)                               NA
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.064       0.080
##   90 Percent Confidence Interval          0.058  0.070       0.074  0.086
##   P-value RMSEA <= 0.05                          0.000       0.000
## 
##   Robust RMSEA                                                  NA
##   90 Percent Confidence Interval                                NA     NA
## 
## Standardized Root Mean Square Residual:
## 
##   SRMR                                           0.056       0.056
## 
## Parameter Estimates:
## 
##   Information                                 Expected
##   Information saturated (h1) model        Unstructured
##   Standard Errors                           Robust.sem
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   G =~                                                                  
##     G1                1.000                               0.458    0.938
##     G2                1.036    0.017   60.845    0.000    0.475    0.968
##     G3                1.035    0.017   60.232    0.000    0.475    0.969
##     G4                1.034    0.017   59.250    0.000    0.474    0.968
##   I =~                                                                  
##     I1                1.000                               0.415    0.845
##     I2                0.921    0.038   24.356    0.000    0.382    0.766
##     I3                0.998    0.031   31.908    0.000    0.414    0.847
##     I4                0.966    0.036   26.800    0.000    0.401    0.808
##   E =~                                                                  
##     E1                1.000                               0.396    0.817
##     E2                1.046    0.040   26.009    0.000    0.414    0.849
##     E3                0.798    0.049   16.312    0.000    0.316    0.636
##     E4                1.000    0.045   22.386    0.000    0.396    0.812
##   C =~                                                                  
##     C1                1.000                               0.306    0.667
##     C2                1.533    0.073   20.984    0.000    0.469    0.937
##     C3                1.012    0.031   32.426    0.000    0.309    0.663
##     C4                1.550    0.073   21.285    0.000    0.474    0.947
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   G ~~                                                                  
##     I                 0.076    0.007   11.209    0.000    0.399    0.399
##     E                 0.059    0.007    8.389    0.000    0.328    0.328
##     C                 0.040    0.005    7.552    0.000    0.283    0.283
##   I ~~                                                                  
##     E                 0.068    0.006   10.629    0.000    0.417    0.417
##     C                 0.045    0.005    8.702    0.000    0.357    0.357
##   E ~~                                                                  
##     C                 0.045    0.005    9.055    0.000    0.372    0.372
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .G1                0.029    0.005    5.278    0.000    0.029    0.120
##    .G2                0.015    0.004    3.749    0.000    0.015    0.062
##    .G3                0.015    0.004    3.723    0.000    0.015    0.061
##    .G4                0.015    0.004    3.748    0.000    0.015    0.063
##    .I1                0.069    0.008    8.306    0.000    0.069    0.286
##    .I2                0.103    0.010   10.700    0.000    0.103    0.414
##    .I3                0.068    0.008    8.301    0.000    0.068    0.283
##    .I4                0.086    0.009    9.326    0.000    0.086    0.348
##    .E1                0.078    0.009    9.163    0.000    0.078    0.333
##    .E2                0.066    0.008    8.104    0.000    0.066    0.279
##    .E3                0.147    0.010   14.595    0.000    0.147    0.596
##    .E4                0.081    0.009    8.632    0.000    0.081    0.341
##    .C1                0.117    0.007   16.883    0.000    0.117    0.555
##    .C2                0.030    0.006    5.037    0.000    0.030    0.122
##    .C3                0.122    0.007   16.501    0.000    0.122    0.561
##    .C4                0.026    0.005    4.818    0.000    0.026    0.103
##     G                 0.210    0.007   31.856    0.000    1.000    1.000
##     I                 0.172    0.009   19.739    0.000    1.000    1.000
##     E                 0.157    0.010   16.328    0.000    1.000    1.000
##     C                 0.093    0.008   11.170    0.000    1.000    1.000
```

```r
#Fit quite ok

SEM <- '
G =~ G1 + G2 + G3 + G4
I =~ I1 + I2 + I3 + I4
E =~ E1 + E2 + E3 + E4
C =~ C1 + C2 + C3 + C4
G + E + I + C ~ Group_numeric
'

SEM.fit <- cfa(SEM, data = rohrer, estimator = "WLSMV", std.lv = TRUE)
summary(SEM.fit, std = TRUE, fit = TRUE)
```

```
## lavaan 0.6-3 ended normally after 120 iterations
## 
##   Optimization method                           NLMINB
##   Number of free parameters                         43
## 
##   Number of observations                           787
## 
##   Estimator                                       DWLS      Robust
##   Model Fit Test Statistic                     423.469     624.593
##   Degrees of freedom                               110         110
##   P-value (Chi-square)                           0.000       0.000
##   Scaling correction factor                                  0.743
##   Shift parameter                                           54.731
##     for simple second-order correction (Mplus variant)
## 
## Model test baseline model:
## 
##   Minimum Function Test Statistic            32420.040   12265.804
##   Degrees of freedom                               136         136
##   P-value                                        0.000       0.000
## 
## User model versus baseline model:
## 
##   Comparative Fit Index (CFI)                    0.990       0.958
##   Tucker-Lewis Index (TLI)                       0.988       0.948
## 
##   Robust Comparative Fit Index (CFI)                            NA
##   Robust Tucker-Lewis Index (TLI)                               NA
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.060       0.077
##   90 Percent Confidence Interval          0.054  0.066       0.071  0.083
##   P-value RMSEA <= 0.05                          0.003       0.000
## 
##   Robust RMSEA                                                  NA
##   90 Percent Confidence Interval                                NA     NA
## 
## Standardized Root Mean Square Residual:
## 
##   SRMR                                           0.053       0.053
## 
## Parameter Estimates:
## 
##   Information                                 Expected
##   Information saturated (h1) model        Unstructured
##   Standard Errors                           Robust.sem
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   G =~                                                                  
##     G1                0.420    0.009   46.375    0.000    0.458    0.938
##     G2                0.435    0.008   52.888    0.000    0.475    0.968
##     G3                0.435    0.008   52.503    0.000    0.475    0.969
##     G4                0.434    0.008   52.633    0.000    0.474    0.968
##   I =~                                                                  
##     I1                0.394    0.011   36.310    0.000    0.416    0.847
##     I2                0.361    0.013   28.503    0.000    0.381    0.762
##     I3                0.393    0.011   36.524    0.000    0.415    0.849
##     I4                0.380    0.012   32.620    0.000    0.401    0.807
##   E =~                                                                  
##     E1                0.387    0.012   31.684    0.000    0.396    0.817
##     E2                0.404    0.011   36.554    0.000    0.414    0.849
##     E3                0.309    0.016   19.360    0.000    0.316    0.635
##     E4                0.387    0.013   30.248    0.000    0.396    0.813
##   C =~                                                                  
##     C1                0.303    0.014   22.392    0.000    0.307    0.670
##     C2                0.462    0.007   66.351    0.000    0.468    0.936
##     C3                0.306    0.014   22.647    0.000    0.311    0.665
##     C4                0.467    0.006   75.407    0.000    0.474    0.947
## 
## Regressions:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   G ~                                                                   
##     Group_numeric     0.875    0.084   10.363    0.000    0.802    0.401
##   E ~                                                                   
##     Group_numeric     0.431    0.079    5.439    0.000    0.421    0.211
##   I ~                                                                   
##     Group_numeric     0.674    0.083    8.143    0.000    0.639    0.319
##   C ~                                                                   
##     Group_numeric     0.332    0.074    4.498    0.000    0.327    0.164
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##  .G ~~                                                                  
##    .I                 0.312    0.036    8.643    0.000    0.312    0.312
##    .E                 0.272    0.038    7.098    0.000    0.272    0.272
##    .C                 0.240    0.035    6.861    0.000    0.240    0.240
##  .I ~~                                                                  
##    .E                 0.377    0.036   10.411    0.000    0.377    0.377
##    .C                 0.326    0.036    8.954    0.000    0.326    0.326
##  .E ~~                                                                  
##    .C                 0.350    0.035    9.894    0.000    0.350    0.350
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .G1                0.029    0.005    5.306    0.000    0.029    0.120
##    .G2                0.015    0.004    3.775    0.000    0.015    0.062
##    .G3                0.015    0.004    3.767    0.000    0.015    0.061
##    .G4                0.015    0.004    3.792    0.000    0.015    0.062
##    .I1                0.068    0.008    8.274    0.000    0.068    0.283
##    .I2                0.105    0.010   10.853    0.000    0.105    0.420
##    .I3                0.067    0.008    8.278    0.000    0.067    0.280
##    .I4                0.086    0.009    9.357    0.000    0.086    0.348
##    .E1                0.078    0.009    9.162    0.000    0.078    0.333
##    .E2                0.066    0.008    8.131    0.000    0.066    0.280
##    .E3                0.147    0.010   14.554    0.000    0.147    0.596
##    .E4                0.081    0.009    8.566    0.000    0.081    0.339
##    .C1                0.116    0.007   16.615    0.000    0.116    0.551
##    .C2                0.031    0.006    5.103    0.000    0.031    0.124
##    .C3                0.122    0.007   16.274    0.000    0.122    0.558
##    .C4                0.026    0.005    4.823    0.000    0.026    0.103
##    .G                 1.000                               0.839    0.839
##    .I                 1.000                               0.898    0.898
##    .E                 1.000                               0.956    0.956
##    .C                 1.000                               0.973    0.973
##     Group_numeric     0.250    0.000 1227.262    0.000    0.250    1.000
```

```r
#Model fit is kind of ok, factor interrelations quite low (!) -> reason for inflated overall estimate (+ more overall vairance stemming from higher number of items?)?

SEM_unidim <- '
GIEC =~ G1 + G2 + G3 + G4 + I1 + I2 + I3 + I4 + E1 + E2 + E3 + E4 + C1 + C2 + C3 + C4
GIEC ~ Group_numeric'

SEM_unidim.fit <- cfa(SEM_unidim, data = rohrer, estimator = "WLSMV")
summary(SEM_unidim.fit, std = TRUE, fit = TRUE)
```

```
## lavaan 0.6-3 ended normally after 100 iterations
## 
##   Optimization method                           NLMINB
##   Number of free parameters                         34
## 
##   Number of observations                           787
## 
##   Estimator                                       DWLS      Robust
##   Model Fit Test Statistic                    6277.971    4498.338
##   Degrees of freedom                               119         119
##   P-value (Chi-square)                           0.000       0.000
##   Scaling correction factor                                  1.417
##   Shift parameter                                           67.403
##     for simple second-order correction (Mplus variant)
## 
## Model test baseline model:
## 
##   Minimum Function Test Statistic            32420.040   12265.804
##   Degrees of freedom                               136         136
##   P-value                                        0.000       0.000
## 
## User model versus baseline model:
## 
##   Comparative Fit Index (CFI)                    0.809       0.639
##   Tucker-Lewis Index (TLI)                       0.782       0.587
## 
##   Robust Comparative Fit Index (CFI)                            NA
##   Robust Tucker-Lewis Index (TLI)                               NA
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.257       0.216
##   90 Percent Confidence Interval          0.251  0.262       0.211  0.222
##   P-value RMSEA <= 0.05                          0.000       0.000
## 
##   Robust RMSEA                                                  NA
##   90 Percent Confidence Interval                                NA     NA
## 
## Standardized Root Mean Square Residual:
## 
##   SRMR                                           0.188       0.188
## 
## Parameter Estimates:
## 
##   Information                                 Expected
##   Information saturated (h1) model        Unstructured
##   Standard Errors                           Robust.sem
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   GIEC =~                                                               
##     G1                1.000                               0.403    0.825
##     G2                1.059    0.016   64.361    0.000    0.427    0.870
##     G3                1.055    0.016   64.313    0.000    0.425    0.869
##     G4                1.052    0.017   62.354    0.000    0.424    0.866
##     I1                0.707    0.030   23.237    0.000    0.285    0.581
##     I2                0.662    0.034   19.691    0.000    0.267    0.535
##     I3                0.705    0.030   23.586    0.000    0.284    0.581
##     I4                0.690    0.031   22.190    0.000    0.278    0.560
##     E1                0.592    0.032   18.233    0.000    0.239    0.493
##     E2                0.626    0.031   20.069    0.000    0.252    0.517
##     E3                0.510    0.036   14.101    0.000    0.206    0.414
##     E4                0.597    0.033   18.261    0.000    0.241    0.494
##     C1                0.474    0.033   14.470    0.000    0.191    0.417
##     C2                0.943    0.024   38.849    0.000    0.380    0.760
##     C3                0.483    0.033   14.492    0.000    0.195    0.417
##     C4                0.949    0.024   39.878    0.000    0.383    0.765
## 
## Regressions:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##   GIEC ~                                                                
##     Group_numeric     0.312    0.025   12.473    0.000    0.774    0.387
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
##    .G1                0.076    0.005   15.688    0.000    0.076    0.319
##    .G2                0.058    0.004   15.014    0.000    0.058    0.243
##    .G3                0.059    0.004   15.016    0.000    0.059    0.245
##    .G4                0.060    0.004   15.132    0.000    0.060    0.249
##    .I1                0.160    0.006   25.223    0.000    0.160    0.663
##    .I2                0.178    0.007   26.204    0.000    0.178    0.714
##    .I3                0.158    0.006   25.723    0.000    0.158    0.662
##    .I4                0.169    0.007   25.684    0.000    0.169    0.686
##    .E1                0.178    0.006   28.110    0.000    0.178    0.757
##    .E2                0.174    0.006   27.201    0.000    0.174    0.732
##    .E3                0.205    0.006   33.786    0.000    0.205    0.829
##    .E4                0.179    0.006   27.655    0.000    0.179    0.756
##    .C1                0.174    0.006   28.198    0.000    0.174    0.826
##    .C2                0.105    0.005   19.202    0.000    0.105    0.422
##    .C3                0.180    0.006   29.448    0.000    0.180    0.826
##    .C4                0.104    0.005   19.433    0.000    0.104    0.415
##    .GIEC              0.138    0.006   23.446    0.000    0.850    0.850
##     Group_numeric     0.250    0.000 1227.262    0.000    0.250    1.000
```

```r
#Model fit is really bad
```

## Bayesian multilevel model


```r
#Bayesian multilevel-model

#t-Priors for Bernoulli for logits of item solution probabilities
m1priors <- c(
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(student_t(3, 0, 2.5), class = "b")
)

#Simple model: 
rohrer_brms <- brm(value ~ Group + (1|variable + Student + Teacher + Class),
                   family = "Bernoulli",
                   data = rohrer_long,
                   chains = 4,
                   cores = 4,
                   iter = 4000,
                   warmup = 1000,
                   control = list(adapt_delta = 0.95),
                   prior = m1priors,
                   seed = 666)
summary(rohrer_brms)
plot(rohrer_brms)

logit2prob(-0.81)
logit2prob(0.97)
#based on logit-interpretation:
#Average solution prob .31 vs. .73?

start_time <- Sys.time()
# Time difference of 1.000327 mins
##Interaction of intervention effect with four task dimensions
rohrer_brms_int <- brm(value ~ Group*dim + (1|variable + Student + Teacher + Class),
                       family = "Bernoulli",
                       data = rohrer_long,
                       chains = 4,
                       cores = 4,
                       iter = 4000,
                       warmup = 1000,
                       control = list(adapt_delta = 0.95),
                       prior = m1priors,
                       seed = 666)
#Start: 6pm; finished: 7:15om
end_time <- Sys.time()
#Model estimation took:
end_time - start_time

save(rohrer_brms, file = "rohrer_brms_fit.rda", 
     compress = "xz")
save(rohrer_brms_int, file = "rohrer_brms_int.rda", 
     compress = "xz")
```

## #Model with random slope


```r
#Bayesian multilevel-model

#t-Priors for Bernoulli for logits of item solution probabilities
m1priors <- c(
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(student_t(3, 0, 2.5), class = "b")
)

start_time <- Sys.time()
# Time difference of 1.000327 mins
##Interaction of intervention effect with four task dimensions
rohrer_long$TeaCla <- interaction(rohrer_long$Teacher, rohrer_long$Class)
str(rohrer_long$TeaCla)
rohrer_brms_int_rs <- brm(value ~ Group*dim + (1|variable + Student + Teacher + Class) + (Group*dim|TeaCla),
                       family = "Bernoulli",
                       data = rohrer_long,
                       chains = 4,
                       cores = 4,
                       iter = 4000,
                       warmup = 1000,
                       control = list(adapt_delta = 0.95),
                       prior = m1priors,
                       seed = 666)
#Start: 6pm; finished: 7:15om
end_time <- Sys.time()
#Model estimation took:
end_time - start_time

save(rohrer_brms_int_rs, file = "rohrer_brms_int_rs.rda", 
     compress = "xz")
```

## #Model with random slope and school (full maximum model)


```r
start_time <- Sys.time()

rohrer_brms_int_full <- brm(value ~ Group*dim + (1*Group*dim|Student + Teacher + Class + School),
                       family = "Bernoulli",
                       data = rohrer_long,
                       chains = 4,
                       cores = 4,
                       iter = 4000,
                       warmup = 1000,
                       control = list(adapt_delta = 0.95),
                       seed = 666)
end_time <- Sys.time()
end_time - start_time

save(rohrer_brms_int_full, file = "rohrer_brms_int_rs.rda",
     compress = "xz")
```

### Load fitted models


```r
load("rohrer_brms_fit.rda")
load("rohrer_brms_int.rda")
load("rohrer_brms_int_rs.rda")
#load("rohrer_brms_int_full.rda")

summary(rohrer_brms)
```

```
## Warning: There were 3 divergent transitions after warmup. Increasing adapt_delta above 0.95 may help.
## See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
##  Family: bernoulli 
##   Links: mu = logit 
## Formula: value ~ Group + (1 | variable + Student + Teacher + Class) 
##    Data: rohrer_long (Number of observations: 12592) 
## Samples: 4 chains, each with iter = 4000; warmup = 1000; thin = 1;
##          total post-warmup samples = 12000
## 
## Group-Level Effects: 
## ~Class (Number of levels: 6) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     0.42      0.21     0.17     0.98       3405 1.00
## 
## ~Student (Number of levels: 29) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     0.48      0.12     0.28     0.75       2152 1.00
## 
## ~Teacher (Number of levels: 15) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     0.48      0.11     0.32     0.73       2860 1.00
## 
## ~variable (Number of levels: 16) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     0.55      0.12     0.38     0.83       2491 1.00
## 
## Population-Level Effects: 
##            Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept     -0.82      0.29    -1.39    -0.25       3094 1.00
## Groupinter     0.97      0.04     0.89     1.05      13116 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

```r
plot(rohrer_brms)
```

![](Rohreretal2019_Reanalysis_files/figure-html/investigate.bayesianmultilevelmodels-1.png)<!-- -->![](Rohreretal2019_Reanalysis_files/figure-html/investigate.bayesianmultilevelmodels-2.png)<!-- -->

```r
summary(rohrer_brms_int)
```

```
## Warning: There were 3 divergent transitions after warmup. Increasing adapt_delta above 0.95 may help.
## See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
##  Family: bernoulli 
##   Links: mu = logit 
## Formula: value ~ Group * dim + (1 | variable + Student + Teacher + Class) 
##    Data: rohrer_long (Number of observations: 12592) 
## Samples: 4 chains, each with iter = 4000; warmup = 1000; thin = 1;
##          total post-warmup samples = 12000
## 
## Group-Level Effects: 
## ~Class (Number of levels: 6) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     0.43      0.21     0.18     0.98       4812 1.00
## 
## ~Student (Number of levels: 29) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     0.50      0.13     0.29     0.79       2265 1.00
## 
## ~Teacher (Number of levels: 15) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     0.49      0.11     0.33     0.75       3132 1.00
## 
## ~variable (Number of levels: 16) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     0.27      0.07     0.17     0.44       3836 1.00
## 
## Population-Level Effects: 
##                 Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept          -0.11      0.29    -0.70     0.48       3304 1.00
## Groupinter          0.53      0.08     0.37     0.69       7821 1.00
## dimE               -0.96      0.21    -1.37    -0.55       5095 1.00
## dimG               -1.56      0.22    -2.00    -1.13       5108 1.00
## dimI               -0.39      0.21    -0.80     0.02       5220 1.00
## Groupinter:dimE     0.10      0.11    -0.11     0.32       9051 1.00
## Groupinter:dimG     1.16      0.11     0.94     1.38       9656 1.00
## Groupinter:dimI     0.52      0.11     0.31     0.73       9115 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

```r
plot(rohrer_brms_int)
```

![](Rohreretal2019_Reanalysis_files/figure-html/investigate.bayesianmultilevelmodels-3.png)<!-- -->![](Rohreretal2019_Reanalysis_files/figure-html/investigate.bayesianmultilevelmodels-4.png)<!-- -->![](Rohreretal2019_Reanalysis_files/figure-html/investigate.bayesianmultilevelmodels-5.png)<!-- -->

### Descriptive evlauation of mixed model-estimates


```r
describeBy(rohrer$mean_C, rohrer$Group)
```

```
## 
##  Descriptive statistics by group 
## group: block
##    vars   n mean   sd median trimmed  mad min max range  skew kurtosis
## X1    1 389 0.54 0.42    0.5    0.54 0.74   0   1     1 -0.13    -1.59
##      se
## X1 0.02
## -------------------------------------------------------- 
## group: inter
##    vars   n mean   sd median trimmed  mad min max range  skew kurtosis
## X1    1 398 0.67 0.37   0.75    0.71 0.37   0   1     1 -0.66    -0.97
##      se
## X1 0.02
```

```r
logit2prob(-0.11)
```

```
## [1] 0.4725277
```

```r
logit2prob(-0.11+0.53)
```

```
## [1] 0.6034832
```

```r
describeBy(rohrer$mean_G, rohrer$Group)
```

```
## 
##  Descriptive statistics by group 
## group: block
##    vars   n mean   sd median trimmed mad min max range skew kurtosis   se
## X1    1 389 0.21 0.39      0    0.13   0   0   1     1 1.43     0.12 0.02
## -------------------------------------------------------- 
## group: inter
##    vars   n mean   sd median trimmed mad min max range  skew kurtosis   se
## X1    1 398 0.58 0.47      1     0.6   0   0   1     1 -0.36    -1.81 0.02
```

```r
logit2prob(-0.11-1.56)
```

```
## [1] 0.1584242
```

```r
logit2prob(-0.11+0.53-1.56+1.16)
```

```
## [1] 0.5049998
```

```r
describeBy(rohrer$mean_E, rohrer$Group)
```

```
## 
##  Descriptive statistics by group 
## group: block
##    vars   n mean  sd median trimmed mad min max range skew kurtosis   se
## X1    1 389 0.32 0.4      0    0.27   0   0   1     1 0.72    -1.19 0.02
## -------------------------------------------------------- 
## group: inter
##    vars   n mean  sd median trimmed  mad min max range skew kurtosis   se
## X1    1 398 0.48 0.4    0.5    0.47 0.74   0   1     1 0.07    -1.61 0.02
```

```r
logit2prob(-0.11-0.96)
```

```
## [1] 0.2554031
```

```r
logit2prob(-0.11+0.53-0.96+0.10)
```

```
## [1] 0.391741
```

```r
describeBy(rohrer$mean_I, rohrer$Group)
```

```
## 
##  Descriptive statistics by group 
## group: block
##    vars   n mean   sd median trimmed  mad min max range skew kurtosis   se
## X1    1 389 0.44 0.43    0.5    0.43 0.74   0   1     1 0.19    -1.72 0.02
## -------------------------------------------------------- 
## group: inter
##    vars   n mean   sd median trimmed mad min max range  skew kurtosis   se
## X1    1 398  0.7 0.38      1    0.75   0   0   1     1 -0.91    -0.72 0.02
```

```r
logit2prob(-0.11-0.39)
```

```
## [1] 0.3775407
```

```r
logit2prob(-0.11+0.53-0.39+0.52)
```

```
## [1] 0.6341356
```

```r
#Result: Model always understimates observed means via model-implied soluation probability; with about 7% on average. Else, everything seems fine (& to fit? -> model predictions form posteriors!)
```

## Mixture-approach


```r
rohrer$sum <- rowSums(rohrer[, c(6:21)])
rohrer$sum_G <- rowSums(rohrer[, c(6:9)])
rohrer$sum_I <- rowSums(rohrer[, c(10:13)])
rohrer$sum_E <- rowSums(rohrer[, c(14:17)])
rohrer$sum_C <- rowSums(rohrer[, c(18:21)])

library(MplusAutomation)
setwd("C:/Users/petere/Downloads/Rohreretal2019JEPInterleaving_DataPreregSheets")
write.table(rohrer[, c(29:32, 27)], "r_mplus.dat", sep = "\t", na = "9999", row.names = FALSE, col.names = FALSE)
print(paste(names(rohrer[, c(29:32, 27)]), collapse = " "))

LPA <- '

[[init]]
iterators = p;
p = 1:6;
filename = "LPA_[[p]].inp";
outputDirectory = "C:/Users/petere/Downloads/Rohreretal2019JEPInterleaving_DataPreregSheets/LPA/LPA_[[p]]/";

[[/init]]

TITLE:
LPA_[[p]]profiles

DATA:
FILE IS
"C:/Users/petere/Downloads/Rohreretal2019JEPInterleaving_DataPreregSheets/r_mplus.dat";

VARIABLE:

NAMES ARE
sum_G sum_I sum_E sum_C Group;

CATEGORICAL ARE
sum_G sum_I sum_E sum_C;

MISSING ARE ALL(9999);

CLASSES = p([[p]]);

ANALYSIS:
type is mixture;
starts = 4000 1000;
process = 4;

MODEL:

%OVERALL%
p ON group;

OUTPUT:
sampstat residual tech1 tech4 tech11 tech14;

PLOT:
type is plot1 plot2 plot3;
SERIES IS sum_G (1) sum_I (2) sum_E (3) sum_C (4);

'

#Write template into textfile
write(LPA, file = "LPA.txt")

#Write Mplus input files based on template for 1-5 profiles
createModels("LPA.txt")

#Run Mplus input files
setwd("C:/Users/petere/Downloads/Rohreretal2019JEPInterleaving_DataPreregSheets/LPA/")
runModels(recursive = TRUE)
showSummaryTable(readModels(target = "C:/Users/petere/Downloads/Rohreretal2019JEPInterleaving_DataPreregSheets/LPA", what = "summaries", recursive = TRUE))
```

