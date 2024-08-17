## Two arm biofeedback study

## Background

This study compares a control group with a training group on two virtual reality
games aiming at teaching participants a breathing technique and subsequently to apply
this technique to a stressful environment 


### Study Design

Both groups came in over two sessions. In the first session both groups took a baseline 
physiological measurement (seated at rest) and then went on to a virtual reality stressor (horror) experience. Then the training groups did two sessions of the slow breathing training.

In the second session, training group participants did two more sessions of the slow breathing 
training. Both groups then took part in the biofeedback stressor, which asked them to control their
breathing to keep their heart rate low to avoid detection by a monster. 

The physiological measurements reported here are Heart Rate (HR), heart
rate variability as indexed by SDNN, and respiration rate (resp).

Initial analysis was conducted using a paired t-test for the HR and SDNN
difference from baseline to the biofeedback stressor.
    
    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  training_t2$hr_diff and control_t2$hr_diff
    ## t = -1.7889, df = 46.746, p-value = 0.08011
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -12.0874724   0.7096286
    ## sample estimates:
    ## mean of x mean of y 
    ##  6.256058 11.944980
    
    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  training_t2$sdnn_diff and control_t2$sdnn_diff
    ## t = 2.7679, df = 47.986, p-value = 0.007993
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.8620884 5.4402607
    ## sample estimates:
    ## mean of x mean of y 
    ## 3.3696583 0.2184838


### Two-way repeated measures ANOVA for HR 

We then did a two-way repeated measures ANOVA with the horror virtual reality from
session 1 and the biofeedback stressor from session 2 to examine the differences 
between control and training groups 

    ## ANOVA Table (type II tests)
    ## 
    ##       Effect DFn DFd      F        p p<.05   ges
    ## 1      group   1  52  2.840 0.098000       0.035
    ## 2       time   1  52 12.789 0.000764     * 0.075
    ## 3 group:time   1  52  1.935 0.170000       0.012
    
    ## # A tibble: 2 × 9
    ##   group    Effect   DFn   DFd     F     p `p<.05`   ges p.adj
    ##   <fct>    <chr>  <dbl> <dbl> <dbl> <dbl> <chr>   <dbl> <dbl>
    ## 1 control  time       1    26 10.3  0.004 "*"     0.112 0.008
    ## 2 training time       1    26  2.97 0.097 ""      0.038 0.097
    
    ## `summarise()` has grouped output by 'group'. You can override using the
    ## `.groups` argument.
    
![unnamed-chunk-4-1](https://github.com/user-attachments/assets/9317ffd9-07ad-4ae2-a0d0-a72818a2fea9)



### Two-way repeated measures ANOVA for SDNN 

    
    ## ANOVA Table (type II tests)
    ## 
    ##       Effect DFn DFd      F     p p<.05   ges
    ## 1      group   1  52  5.599 0.022     * 0.071
    ## 2       time   1  52 10.685 0.002     * 0.055
    ## 3 group:time   1  52  5.350 0.025     * 0.029
    
    ## # A tibble: 2 × 10
    ##   time    .y.   group1  group2      n1    n2      p p.signif  p.adj p.adj.signif
    ## * <fct>   <chr> <chr>   <chr>    <int> <int>  <dbl> <chr>     <dbl> <chr>       
    ## 1 horror  SDNN  control training    27    27 0.341  ns       0.341  ns          
    ## 2 dungeon SDNN  control training    27    27 0.0078 **       0.0078 **
    
    ## # A tibble: 2 × 9
    ##   group    Effect   DFn   DFd      F        p `p<.05`   ges    p.adj
    ##   <fct>    <chr>  <dbl> <dbl>  <dbl>    <dbl> <chr>   <dbl>    <dbl>
    ## 1 control  time       1    26  0.371 0.548    ""      0.004 0.548   
    ## 2 training time       1    26 20.3   0.000124 "*"     0.19  0.000248
    
    ## # A tibble: 2 × 10
    ##   group    .y.   group1 group2     n1    n2 statistic    df   p.adj p.adj.signif
    ##   <fct>    <chr> <chr>  <chr>   <int> <int>     <dbl> <dbl>   <dbl> <chr>       
    ## 1 control  SDNN  horror dungeon    27    27    -0.609    26 5.48e-1 ns          
    ## 2 training SDNN  horror dungeon    27    27    -4.50     26 1.24e-4 ***


![unnamed-chunk-6-1](https://github.com/user-attachments/assets/245081be-1a99-4386-843f-bb2d416cb545)
![unnamed-chunk-5-2](https://github.com/user-attachments/assets/c733412c-5a0f-4a50-8b7f-3ea948777f64)


## Respiration analysis

We also examined the difference in respiration between the control and
training groups with a T test 


        ##  Welch Two Sample t-test
        ## 
        ## data:  controlResp$contDiff and trainResp$diff
        ## t = 5.2694, df = 45.32, p-value = 0.000003669
        ## alternative hypothesis: true difference in means is not equal to 0
        ## 95 percent confidence interval:
        ##  3.288220 7.355833
        ## sample estimates:
        ## mean of x mean of y 
        ## -1.507252 -6.829279    


![unnamed-chunk-7-1](https://github.com/user-attachments/assets/a7bd9c95-ddf1-4a9f-8033-653911862260)

# Combined graphs for manuscript 

![figure3](https://github.com/user-attachments/assets/2941b3ea-f223-4a78-8053-649164be13c6)


# Uncorrected SDNN data

Any of these analyses can be run using uncorrected SDNN, as seen in the supplement. The uncorrected SDNN data is available in a subfolder of the data folder, and will work in the R script provided

