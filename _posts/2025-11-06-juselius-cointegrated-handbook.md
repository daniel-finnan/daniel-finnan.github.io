---
layout: post
title: Juselius Cointegrated Handbook
output:
  md_document:
    variant: gfm
    preserve_yaml: true
katex: true    
---

This is a sample blog post

{% highlight r %}
library(readxl)
library(zoo)
{% endhighlight %}

``` r
library(readxl)
library(zoo)
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(ggplot2)
library(kableExtra)
library(vars)
```

    ## Loading required package: MASS

    ## Loading required package: strucchange

    ## Loading required package: sandwich

    ## Loading required package: urca

    ## Loading required package: lmtest

``` r
library(mvnormalTest)
library(moments)
library(tsDyn)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
library(car)
```

    ## Loading required package: carData

``` r
options(scipen = 999) # Turn off scientific notation
```

## Data

Data is sourced from [Estima
website](https://estima.com/textbook_juselius.shtml) and companion for
RATS/CATS software.

NOTE - There’s some interesting work on VAR using West German data

``` r
data <- read_excel("~/code/R/Johansen/cointegrated_var_model_handbook.xls")
```

    ## New names:
    ## • `` -> `...1`



{% highlight r %}
data$date <- as.Date(as.yearqtr(data$...1, format = "%Y-%q"))
# The real money stock series is defined as the difference between the nominal M3 series (lm3n)
# and the price levels (lpy)
data$Lm3r <- data$Lm3n - data$Lpy
# four-quarter moving average of inflation MA4DPY
data$Ma4dpy <- rollmean(data$Dpy, k=4, fill=NA, align='right')
# There is creation of other dummy variables (page 10) for later in book 
# dt754, is a “transitory blip” dummy
# There's also detrending:
# trend-adjusted series tradjlyr

#D831 - 1 for t = 1983.1, ..., 2003.4, 0 otherwise
data$D831 <- ifelse(data$date >= "1983-01-01" & data$date <= "2003-10-01", 1, 0)
# Dtr 754t = 1 for t = 1975:4, −0.5 for 1976:1 and 1976:2, 0 otherwise;
data$D754 <- ifelse(data$date == "1975-10-01", 1, 
                    ifelse(data$date == "1976-01-01", -0.5, 
                           ifelse(data$date == "1976-04-01", -0.5, 0)))
#Dp 764t = 1 for t = 1976:4, 0 otherwise.
data$D764 <- ifelse(data$date == "1976-10-01", 1, 0)
{% endhighlight %}

## Description

| Variable | Name | Description |
|----|----|----|
| $`m_t^r`$ | Lm3rC | The (corrected) log of the real M3 money stock |
| $`\Delta p_t`$ | Dpy | The quarterly inflation rate |
| $`y_t^r`$ | Lyr | The log of real income (the implicit price deflator of GNE) |
| $`R_{m,t}`$ | Rm | An average deposit rate, or own interest on money stock |
| $`R_{b,t}`$ | Rb | The long-term government bond rate |
|  | Lyp | The log of the price levels |
|  | Lm3n | The log of the nominal M3 money stock |

$R_{b,t}$

# VAR

Vector given by $`[m_t^r, y_t^r, \Delta p_t, R_{m,t}, R_{b,t}]`$ and
$`t = 1973Q1, ..., 2003Q1`$

NOTE: Skipped standard graphs for moment, page 41

# Unrestricted VAR(2) Model

``` r
model <- c("Lm3r", "Lyr", "Dpy", "Rm", "Rb")
var_2 <- VAR(data[,model], p = 2, type = "const", season = 4)
summary(var_2)
```

    ## 
    ## VAR Estimation Results:
    ## ========================= 
    ## Endogenous variables: Lm3r, Lyr, Dpy, Rm, Rb 
    ## Deterministic variables: const 
    ## Sample size: 119 
    ## Log Likelihood: 2241.279 
    ## Roots of the characteristic polynomial:
    ## 1.001 0.8649 0.7633 0.7009 0.6003 0.4803 0.3167 0.3167 0.1661 0.1661
    ## Call:
    ## VAR(y = data[, model], p = 2, type = "const", season = 4L)
    ## 
    ## 
    ## Estimation results for equation Lm3r: 
    ## ===================================== 
    ## Lm3r = Lm3r.l1 + Lyr.l1 + Dpy.l1 + Rm.l1 + Rb.l1 + Lm3r.l2 + Lyr.l2 + Dpy.l2 + Rm.l2 + Rb.l2 + const + sd1 + sd2 + sd3 
    ## 
    ##          Estimate Std. Error t value    Pr(>|t|)    
    ## Lm3r.l1  0.547348   0.100821   5.429 0.000000368 ***
    ## Lyr.l1   0.124534   0.238750   0.522    0.603041    
    ## Dpy.l1  -0.928778   0.419109  -2.216    0.028844 *  
    ## Rm.l1    3.974254   3.014195   1.319    0.190201    
    ## Rb.l1   -8.050637   2.162542  -3.723    0.000319 ***
    ## Lm3r.l2  0.190356   0.090257   2.109    0.037319 *  
    ## Lyr.l2   0.119417   0.225733   0.529    0.597909    
    ## Dpy.l2  -0.548185   0.423027  -1.296    0.197863    
    ## Rm.l2    1.064765   3.251236   0.327    0.743945    
    ## Rb.l2    3.062731   2.336972   1.311    0.192867    
    ## const   -0.027763   0.486652  -0.057    0.954615    
    ## sd1     -0.037852   0.010903  -3.472    0.000752 ***
    ## sd2     -0.003754   0.009823  -0.382    0.703078    
    ## sd3     -0.030551   0.010795  -2.830    0.005578 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Residual standard error: 0.03687 on 105 degrees of freedom
    ## Multiple R-Squared: 0.9831,  Adjusted R-squared: 0.981 
    ## F-statistic: 468.5 on 13 and 105 DF,  p-value: < 0.00000000000000022 
    ## 
    ## 
    ## Estimation results for equation Lyr: 
    ## ==================================== 
    ## Lyr = Lm3r.l1 + Lyr.l1 + Dpy.l1 + Rm.l1 + Rb.l1 + Lm3r.l2 + Lyr.l2 + Dpy.l2 + Rm.l2 + Rb.l2 + const + sd1 + sd2 + sd3 
    ## 
    ##          Estimate Std. Error t value             Pr(>|t|)    
    ## Lm3r.l1 -0.009249   0.040137  -0.230             0.818211    
    ## Lyr.l1   1.039091   0.095046  10.932 < 0.0000000000000002 ***
    ## Dpy.l1  -0.226733   0.166848  -1.359             0.177083    
    ## Rm.l1   -1.685214   1.199953  -1.404             0.163151    
    ## Rb.l1   -2.177891   0.860909  -2.530             0.012899 *  
    ## Lm3r.l2  0.033212   0.035931   0.924             0.357435    
    ## Lyr.l2  -0.160072   0.089864  -1.781             0.077761 .  
    ## Dpy.l2  -0.098821   0.168407  -0.587             0.558598    
    ## Rm.l2   -0.429324   1.294319  -0.332             0.740778    
    ## Rb.l2    2.689349   0.930350   2.891             0.004672 ** 
    ## const    0.706692   0.193736   3.648             0.000414 ***
    ## sd1     -0.004910   0.004340  -1.131             0.260587    
    ## sd2     -0.002672   0.003910  -0.683             0.495860    
    ## sd3     -0.006613   0.004298  -1.539             0.126841    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Residual standard error: 0.01468 on 105 degrees of freedom
    ## Multiple R-Squared: 0.9899,  Adjusted R-squared: 0.9886 
    ## F-statistic: 791.5 on 13 and 105 DF,  p-value: < 0.00000000000000022 
    ## 
    ## 
    ## Estimation results for equation Dpy: 
    ## ==================================== 
    ## Dpy = Lm3r.l1 + Lyr.l1 + Dpy.l1 + Rm.l1 + Rb.l1 + Lm3r.l2 + Lyr.l2 + Dpy.l2 + Rm.l2 + Rb.l2 + const + sd1 + sd2 + sd3 
    ## 
    ##           Estimate Std. Error t value Pr(>|t|)    
    ## Lm3r.l1 -0.0076411  0.0238410  -0.321 0.749226    
    ## Lyr.l1  -0.1095159  0.0564569  -1.940 0.055085 .  
    ## Dpy.l1   0.0215867  0.0991062   0.218 0.827997    
    ## Rm.l1   -2.4642389  0.7127626  -3.457 0.000789 ***
    ## Rb.l1    1.7904343  0.5113734   3.501 0.000681 ***
    ## Lm3r.l2 -0.0161155  0.0213430  -0.755 0.451896    
    ## Lyr.l2   0.0982936  0.0533788   1.841 0.068381 .  
    ## Dpy.l2   0.2094544  0.1000326   2.094 0.038680 *  
    ## Rm.l2    1.8213377  0.7688155   2.369 0.019663 *  
    ## Rb.l2   -1.5511853  0.5526207  -2.807 0.005963 ** 
    ## const    0.2293712  0.1150779   1.993 0.048836 *  
    ## sd1     -0.0004933  0.0025782  -0.191 0.848647    
    ## sd2      0.0004701  0.0023228   0.202 0.840001    
    ## sd3     -0.0026665  0.0025527  -1.045 0.298608    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Residual standard error: 0.008718 on 105 degrees of freedom
    ## Multiple R-Squared: 0.5816,  Adjusted R-squared: 0.5298 
    ## F-statistic: 11.23 on 13 and 105 DF,  p-value: 0.00000000000001032 
    ## 
    ## 
    ## Estimation results for equation Rm: 
    ## =================================== 
    ## Rm = Lm3r.l1 + Lyr.l1 + Dpy.l1 + Rm.l1 + Rb.l1 + Lm3r.l2 + Lyr.l2 + Dpy.l2 + Rm.l2 + Rb.l2 + const + sd1 + sd2 + sd3 
    ## 
    ##           Estimate Std. Error t value             Pr(>|t|)    
    ## Lm3r.l1 -0.0045457  0.0032824  -1.385             0.169035    
    ## Lyr.l1  -0.0010458  0.0077730  -0.135             0.893236    
    ## Dpy.l1   0.0277559  0.0136449   2.034             0.044457 *  
    ## Rm.l1    1.0483052  0.0981330  10.682 < 0.0000000000000002 ***
    ## Rb.l1    0.2979184  0.0704058   4.231            0.0000498 ***
    ## Lm3r.l2  0.0042705  0.0029385   1.453             0.149127    
    ## Lyr.l2   0.0002739  0.0073492   0.037             0.970336    
    ## Dpy.l2  -0.0165370  0.0137725  -1.201             0.232558    
    ## Rm.l2   -0.1580810  0.1058503  -1.493             0.138321    
    ## Rb.l2   -0.2593507  0.0760847  -3.409             0.000927 ***
    ## const    0.0073898  0.0158439   0.466             0.641886    
    ## sd1     -0.0003256  0.0003550  -0.917             0.361064    
    ## sd2     -0.0005794  0.0003198  -1.812             0.072868 .  
    ## sd3     -0.0001298  0.0003515  -0.369             0.712530    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Residual standard error: 0.0012 on 105 degrees of freedom
    ## Multiple R-Squared: 0.9732,  Adjusted R-squared: 0.9699 
    ## F-statistic: 293.7 on 13 and 105 DF,  p-value: < 0.00000000000000022 
    ## 
    ## 
    ## Estimation results for equation Rb: 
    ## =================================== 
    ## Rb = Lm3r.l1 + Lyr.l1 + Dpy.l1 + Rm.l1 + Rb.l1 + Lm3r.l2 + Lyr.l2 + Dpy.l2 + Rm.l2 + Rb.l2 + const + sd1 + sd2 + sd3 
    ## 
    ##           Estimate Std. Error t value             Pr(>|t|)    
    ## Lm3r.l1 -0.0085460  0.0043582  -1.961             0.052536 .  
    ## Lyr.l1   0.0335708  0.0103204   3.253             0.001537 ** 
    ## Dpy.l1   0.0322156  0.0181167   1.778             0.078261 .  
    ## Rm.l1    0.0728848  0.1302934   0.559             0.577087    
    ## Rb.l1    1.2728431  0.0934794  13.616 < 0.0000000000000002 ***
    ## Lm3r.l2  0.0073280  0.0039015   1.878             0.063121 .  
    ## Lyr.l2  -0.0362931  0.0097577  -3.719             0.000323 ***
    ## Dpy.l2   0.0005054  0.0182860   0.028             0.978001    
    ## Rm.l2   -0.0607354  0.1405399  -0.432             0.666513    
    ## Rb.l2   -0.3650805  0.1010194  -3.614             0.000465 ***
    ## const    0.0274489  0.0210363   1.305             0.194802    
    ## sd1      0.0001513  0.0004713   0.321             0.748853    
    ## sd2      0.0006676  0.0004246   1.572             0.118908    
    ## sd3      0.0008322  0.0004666   1.784             0.077394 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## Residual standard error: 0.001594 on 105 degrees of freedom
    ## Multiple R-Squared: 0.9829,  Adjusted R-squared: 0.9808 
    ## F-statistic: 465.6 on 13 and 105 DF,  p-value: < 0.00000000000000022 
    ## 
    ## 
    ## 
    ## Covariance matrix of residuals:
    ##              Lm3r           Lyr          Dpy            Rm            Rb
    ## Lm3r  0.001359269  0.0000462249 -0.000107964 -0.0000071778 -0.0000100653
    ## Lyr   0.000046225  0.0002154225 -0.000014965 -0.0000009526  0.0000020151
    ## Dpy  -0.000107964 -0.0000149654  0.000076007  0.0000023026  0.0000034054
    ## Rm   -0.000007178 -0.0000009526  0.000002303  0.0000014408  0.0000006287
    ## Rb   -0.000010065  0.0000020151  0.000003405  0.0000006287  0.0000025398
    ## 
    ## Correlation matrix of residuals:
    ##          Lm3r      Lyr     Dpy       Rm       Rb
    ## Lm3r  1.00000  0.08542 -0.3359 -0.16220 -0.17130
    ## Lyr   0.08542  1.00000 -0.1170 -0.05407  0.08615
    ## Dpy  -0.33589 -0.11695  1.0000  0.22004  0.24510
    ## Rm   -0.16220 -0.05407  0.2200  1.00000  0.32868
    ## Rb   -0.17130  0.08615  0.2451  0.32868  1.00000

# Table 3.1 The roots of the VAR(2) model.

``` r
roots_var_2 <- vars::roots(var_2, modulus = FALSE) # switch option to get Modulus
df <- data.frame(Real = Re(roots_var_2), Imaginary = Im(roots_var_2), Modulus = Mod(roots_var_2))
ggplot(df, aes(x=Real, y=Imaginary)) +
  geom_point(size = 3) + 
  annotate("path",
   x=0+1*cos(seq(0,2*pi,length.out=100)),
   y=0+1*sin(seq(0,2*pi,length.out=100))) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(panel.background = element_blank(),
    aspect.ratio = 1)
```

![](juselius_cointegrated_handbook_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
kable(df,
      digits = 2,
      format = "html")
```

<table>

<thead>

<tr>

<th style="text-align:right;">

Real
</th>

<th style="text-align:right;">

Imaginary
</th>

<th style="text-align:right;">

Modulus
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

1.00
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

1.00
</td>

</tr>

<tr>

<td style="text-align:right;">

0.86
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

0.86
</td>

</tr>

<tr>

<td style="text-align:right;">

0.76
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

0.76
</td>

</tr>

<tr>

<td style="text-align:right;">

0.70
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

0.70
</td>

</tr>

<tr>

<td style="text-align:right;">

0.60
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

0.60
</td>

</tr>

<tr>

<td style="text-align:right;">

0.48
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

0.48
</td>

</tr>

<tr>

<td style="text-align:right;">

-0.31
</td>

<td style="text-align:right;">

0.04
</td>

<td style="text-align:right;">

0.32
</td>

</tr>

<tr>

<td style="text-align:right;">

-0.31
</td>

<td style="text-align:right;">

-0.04
</td>

<td style="text-align:right;">

0.32
</td>

</tr>

<tr>

<td style="text-align:right;">

0.07
</td>

<td style="text-align:right;">

0.15
</td>

<td style="text-align:right;">

0.17
</td>

</tr>

<tr>

<td style="text-align:right;">

0.07
</td>

<td style="text-align:right;">

-0.15
</td>

<td style="text-align:right;">

0.17
</td>

</tr>

</tbody>

</table>

The unrestricted VAR(2) model was estimated based on the following
assumptions:

``` math

x_t = \Pi_1x_{t-1} + \Pi_2x_{t-2} + \Phi D_t + \epsilon_t
t = 1, ..., T,  \epsilon_t \sim I N_p(0, \Omega)
```
where $`D_t = [Dq1_t, Dq2_t, Dq_3t, \mu_0]`$ contains three centred
seasonal dummies.

# Table 4.1 The estimates of the unrestricted VAR model in levels.

``` r
results <- summary(var_2)
coef(var_2$varresult$Lm3r)
```

    ##      Lm3r.l1       Lyr.l1       Dpy.l1        Rm.l1        Rb.l1      Lm3r.l2 
    ##  0.547347959  0.124534066 -0.928777528  3.974254476 -8.050636840  0.190355763 
    ##       Lyr.l2       Dpy.l2        Rm.l2        Rb.l2        const          sd1 
    ##  0.119417135 -0.548185434  1.064764629  3.062731148 -0.027762698 -0.037851850 
    ##          sd2          sd3 
    ## -0.003754395 -0.030550677

``` r
vcov(var_2$varresult$Lm3r)
```

    ##               Lm3r.l1        Lyr.l1         Dpy.l1         Rm.l1         Rb.l1
    ## Lm3r.l1  0.0101648877 -0.0018133447  0.01257942784  0.0133487032  0.0234998807
    ## Lyr.l1  -0.0018133447  0.0570015328  0.01582760520  0.1017185218 -0.0598807939
    ## Dpy.l1   0.0125794278  0.0158276052  0.17565269552 -0.1262829238 -0.0904987853
    ## Rm.l1    0.0133487032  0.1017185218 -0.12628292382  9.0853698930 -2.6868976455
    ## Rb.l1    0.0234998807 -0.0598807939 -0.09049878534 -2.6868976455  4.6765864203
    ## Lm3r.l2 -0.0072475666  0.0011659282 -0.00632952973 -0.0234832790 -0.0110018223
    ## Lyr.l2  -0.0005995054 -0.0498862637 -0.01100014788 -0.1157874164  0.0524280486
    ## Dpy.l2   0.0060675731  0.0141847048  0.02446749315 -0.2763316528 -0.0845469214
    ## Rm.l2   -0.0560627473  0.0332627684  0.26378065584 -8.2032306222  2.3726440402
    ## Rb.l2    0.0260564882  0.0362565241  0.06381065933  2.0530738822 -4.1450349447
    ## const   -0.0017759904 -0.0465647753 -0.07366204656  0.1637776546 -0.0301077446
    ## sd1     -0.0004033856 -0.0004182729 -0.00099216864 -0.0067853741  0.0036586740
    ## sd2      0.0000898001 -0.0002583590 -0.00004169361 -0.0005292964  0.0030669993
    ## sd3     -0.0003962824 -0.0002617145 -0.00106033467  0.0015538577 -0.0009832388
    ##                Lm3r.l2        Lyr.l2        Dpy.l2        Rm.l2        Rb.l2
    ## Lm3r.l1 -0.00724756661 -0.0005995054  0.0060675731 -0.056062747  0.026056488
    ## Lyr.l1   0.00116592824 -0.0498862637  0.0141847048  0.033262768  0.036256524
    ## Dpy.l1  -0.00632952973 -0.0110001479  0.0244674932  0.263780656  0.063810659
    ## Rm.l1   -0.02348327905 -0.1157874164 -0.2763316528 -8.203230622  2.053073882
    ## Rb.l1   -0.01100182227  0.0524280486 -0.0845469214  2.372644040 -4.145034945
    ## Lm3r.l2  0.00814636319 -0.0023836671  0.0018448658  0.020995172  0.022014465
    ## Lyr.l2  -0.00238366706  0.0509553659 -0.0123742215  0.086317186 -0.084350085
    ## Dpy.l2   0.00184486582 -0.0123742215  0.1789515408  0.337873711  0.099440988
    ## Rm.l2    0.02099517236  0.0863171863  0.3378737105 10.570537553 -3.544569606
    ## Rb.l2    0.02201446455 -0.0843500851  0.0994409883 -3.544569606  5.461438656
    ## const    0.00279497175  0.0121516820 -0.0629556927 -0.618259300  0.027788189
    ## sd1      0.00034512164  0.0004199562  0.0002341356  0.006653136 -0.004398352
    ## sd2     -0.00005475332  0.0001736653 -0.0002291538 -0.001603038 -0.001874016
    ## sd3      0.00039380582  0.0001883771  0.0001892883 -0.002762270  0.001323688
    ##                 const            sd1            sd2            sd3
    ## Lm3r.l1 -0.0017759904 -0.00040338563  0.00008980010 -0.00039628241
    ## Lyr.l1  -0.0465647753 -0.00041827287 -0.00025835898 -0.00026171451
    ## Dpy.l1  -0.0736620466 -0.00099216864 -0.00004169361 -0.00106033467
    ## Rm.l1    0.1637776546 -0.00678537410 -0.00052929642  0.00155385767
    ## Rb.l1   -0.0301077446  0.00365867402  0.00306699930 -0.00098323879
    ## Lm3r.l2  0.0027949718  0.00034512164 -0.00005475332  0.00039380582
    ## Lyr.l2   0.0121516820  0.00041995616  0.00017366535  0.00018837708
    ## Dpy.l2  -0.0629556927  0.00023413564 -0.00022915380  0.00018928833
    ## Rm.l2   -0.6182592999  0.00665313565 -0.00160303761 -0.00276226980
    ## Rb.l2    0.0277881891 -0.00439835188 -0.00187401592  0.00132368763
    ## const    0.2368301523  0.00036749668  0.00037411531  0.00053628982
    ## sd1      0.0003674967  0.00011887502  0.00004529107  0.00006673692
    ## sd2      0.0003741153  0.00004529107  0.00009648823  0.00004379496
    ## sd3      0.0005362898  0.00006673692  0.00004379496  0.00011653425

``` r
# The correlation matrix of residuals is omega hat
omega_hat <- results$corres
# Sigma hat
sigma_hat <- c(
  sd(var_2$varresult$Lm3r$residuals),
  sd(var_2$varresult$Lyr$residuals),
  sd(var_2$varresult$Dpy$residuals),
  sd(var_2$varresult$Rm$residuals),
  sd(var_2$varresult$Rb$residuals)
  )
# Log likelihood
logLik(var_2)
```

    ## 'log Lik.' 2241.279 (df=70)

``` r
log(omega_hat)
```

    ## Warning in log(omega_hat): NaNs produced

    ##           Lm3r       Lyr       Dpy        Rm        Rb
    ## Lm3r  0.000000 -2.460134       NaN       NaN       NaN
    ## Lyr  -2.460134  0.000000       NaN       NaN -2.451672
    ## Dpy        NaN       NaN  0.000000 -1.513956 -1.406093
    ## Rm         NaN       NaN -1.513956  0.000000 -1.112672
    ## Rb         NaN -2.451672 -1.406093 -1.112672  0.000000

``` r
# shown with standardized residuals
resid_Lm3r <- rstandard(var_2$varresult$Lm3r) # Is this the correct variable?
plot(x = data$date[3:121], y = resid_Lm3r, type = "l", main = "Residuals from M3 equation")
```

![](juselius_cointegrated_handbook_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
resid_Lyr <- rstandard(var_2$varresult$Lyr)
plot(x = data$date[3:121], y = resid_Lyr, type = "l", main = "Residuals from real income equation")
```

![](juselius_cointegrated_handbook_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
resid_Dpy <- rstandard(var_2$varresult$Dpy)
plot(x = data$date[3:121], y = resid_Dpy, type = "l", main = "Residuals from inflation rate equation")
```

![](juselius_cointegrated_handbook_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
resid_Rm <- rstandard(var_2$varresult$Rm)
plot(x = data$date[3:121], y = resid_Rm, type = "l", main = "Residuals from the M3 interest rate equation")
```

![](juselius_cointegrated_handbook_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

``` r
resid_Rb <- rstandard(var_2$varresult$Rb)
plot(x = data$date[3:121], y = resid_Rb, type = "l", main = "Residuals from the bond rate equation")
```

![](juselius_cointegrated_handbook_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->

``` r
# From Fig 3.3
plot(x = data$date, y = data$Ma4dpy, type = "l", main = "A four quarter moving average of inflation")
```

![](juselius_cointegrated_handbook_files/figure-gfm/unnamed-chunk-6-6.png)<!-- -->

# ECM representation

## The ECM formulation with m = 1

Table 4.2 The estimates of the VECM with m = 1 “transitory”

``` r
model <- c("Lm3r", "Lyr", "Dpy", "Rm", "Rb")
# Very good results here for both m = 1, m = 2
# The ECM formulation with m = 1
vecm_m1 <- ca.jo(data[,model], K = 2, season = 4, ecdet = "const", spec = "transitory")
# OLS regressions of an unrestricted VECM
vecm_m1 <- cajools(vecm_m1)
summary(vecm_m1)
```

    ## Response Lm3r.d :
    ## 
    ## Call:
    ## lm(formula = Lm3r.d ~ sd1 + sd2 + sd3 + Lm3r.dl1 + Lyr.dl1 + 
    ##     Dpy.dl1 + Rm.dl1 + Rb.dl1 + Lm3r.l1 + Lyr.l1 + Dpy.l1 + Rm.l1 + 
    ##     Rb.l1 + constant - 1, data = data.mat)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.078343 -0.017980 -0.004791  0.015506  0.188871 
    ## 
    ## Coefficients:
    ##           Estimate Std. Error t value  Pr(>|t|)    
    ## sd1      -0.037852   0.010903  -3.472  0.000752 ***
    ## sd2      -0.003754   0.009823  -0.382  0.703078    
    ## sd3      -0.030551   0.010795  -2.830  0.005578 ** 
    ## Lm3r.dl1 -0.190356   0.090257  -2.109  0.037319 *  
    ## Lyr.dl1  -0.119417   0.225733  -0.529  0.597909    
    ## Dpy.dl1   0.548185   0.423027   1.296  0.197863    
    ## Rm.dl1   -1.064765   3.251236  -0.327  0.743945    
    ## Rb.dl1   -3.062731   2.336972  -1.311  0.192867    
    ## Lm3r.l1  -0.262296   0.061775  -4.246 0.0000471 ***
    ## Lyr.l1    0.243951   0.090468   2.697  0.008162 ** 
    ## Dpy.l1   -1.476963   0.635247  -2.325  0.021994 *  
    ## Rm.l1     5.039019   1.802622   2.795  0.006165 ** 
    ## Rb.l1    -4.987906   1.359395  -3.669  0.000384 ***
    ## constant -0.027763   0.486652  -0.057  0.954615    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.03687 on 105 degrees of freedom
    ## Multiple R-squared:  0.4757, Adjusted R-squared:  0.4058 
    ## F-statistic: 6.805 on 14 and 105 DF,  p-value: 0.000000001066
    ## 
    ## 
    ## Response Lyr.d :
    ## 
    ## Call:
    ## lm(formula = Lyr.d ~ sd1 + sd2 + sd3 + Lm3r.dl1 + Lyr.dl1 + Dpy.dl1 + 
    ##     Rm.dl1 + Rb.dl1 + Lm3r.l1 + Lyr.l1 + Dpy.l1 + Rm.l1 + Rb.l1 + 
    ##     constant - 1, data = data.mat)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.032823 -0.009230  0.000073  0.010937  0.051281 
    ## 
    ## Coefficients:
    ##           Estimate Std. Error t value Pr(>|t|)    
    ## sd1      -0.004910   0.004340  -1.131 0.260587    
    ## sd2      -0.002672   0.003910  -0.683 0.495860    
    ## sd3      -0.006613   0.004298  -1.539 0.126841    
    ## Lm3r.dl1 -0.033212   0.035931  -0.924 0.357435    
    ## Lyr.dl1   0.160072   0.089864   1.781 0.077761 .  
    ## Dpy.dl1   0.098821   0.168407   0.587 0.558598    
    ## Rm.dl1    0.429324   1.294319   0.332 0.740778    
    ## Rb.dl1   -2.689349   0.930350  -2.891 0.004672 ** 
    ## Lm3r.l1   0.023964   0.024593   0.974 0.332079    
    ## Lyr.l1   -0.120981   0.036015  -3.359 0.001090 ** 
    ## Dpy.l1   -0.325554   0.252892  -1.287 0.200812    
    ## Rm.l1    -2.114538   0.717625  -2.947 0.003959 ** 
    ## Rb.l1     0.511458   0.541176   0.945 0.346785    
    ## constant  0.706692   0.193736   3.648 0.000414 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.01468 on 105 degrees of freedom
    ## Multiple R-squared:  0.3237, Adjusted R-squared:  0.2336 
    ## F-statistic: 3.591 on 14 and 105 DF,  p-value: 0.00007621
    ## 
    ## 
    ## Response Dpy.d :
    ## 
    ## Call:
    ## lm(formula = Dpy.d ~ sd1 + sd2 + sd3 + Lm3r.dl1 + Lyr.dl1 + Dpy.dl1 + 
    ##     Rm.dl1 + Rb.dl1 + Lm3r.l1 + Lyr.l1 + Dpy.l1 + Rm.l1 + Rb.l1 + 
    ##     constant - 1, data = data.mat)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0181514 -0.0066410 -0.0003969  0.0056380  0.0250555 
    ## 
    ## Coefficients:
    ##            Estimate Std. Error t value  Pr(>|t|)    
    ## sd1      -0.0004933  0.0025782  -0.191   0.84865    
    ## sd2       0.0004701  0.0023228   0.202   0.84000    
    ## sd3      -0.0026665  0.0025527  -1.045   0.29861    
    ## Lm3r.dl1  0.0161155  0.0213430   0.755   0.45190    
    ## Lyr.dl1  -0.0982936  0.0533788  -1.841   0.06838 .  
    ## Dpy.dl1  -0.2094544  0.1000326  -2.094   0.03868 *  
    ## Rm.dl1   -1.8213377  0.7688155  -2.369   0.01966 *  
    ## Rb.dl1    1.5511853  0.5526207   2.807   0.00596 ** 
    ## Lm3r.l1  -0.0237566  0.0146078  -1.626   0.10688    
    ## Lyr.l1   -0.0112223  0.0213927  -0.525   0.60098    
    ## Dpy.l1   -0.7689589  0.1502161  -5.119 0.0000014 ***
    ## Rm.l1    -0.6429012  0.4262636  -1.508   0.13450    
    ## Rb.l1     0.2392491  0.3214544   0.744   0.45838    
    ## constant  0.2293712  0.1150779   1.993   0.04884 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.008718 on 105 degrees of freedom
    ## Multiple R-squared:  0.5904, Adjusted R-squared:  0.5357 
    ## F-statistic: 10.81 on 14 and 105 DF,  p-value: 0.000000000000008778
    ## 
    ## 
    ## Response Rm.d :
    ## 
    ## Call:
    ## lm(formula = Rm.d ~ sd1 + sd2 + sd3 + Lm3r.dl1 + Lyr.dl1 + Dpy.dl1 + 
    ##     Rm.dl1 + Rb.dl1 + Lm3r.l1 + Lyr.l1 + Dpy.l1 + Rm.l1 + Rb.l1 + 
    ##     constant - 1, data = data.mat)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0028122 -0.0006743 -0.0000191  0.0006554  0.0048715 
    ## 
    ## Coefficients:
    ##            Estimate Std. Error t value Pr(>|t|)    
    ## sd1      -0.0003256  0.0003550  -0.917 0.361064    
    ## sd2      -0.0005794  0.0003198  -1.812 0.072868 .  
    ## sd3      -0.0001298  0.0003515  -0.369 0.712530    
    ## Lm3r.dl1 -0.0042705  0.0029385  -1.453 0.149127    
    ## Lyr.dl1  -0.0002739  0.0073492  -0.037 0.970336    
    ## Dpy.dl1   0.0165370  0.0137725   1.201 0.232558    
    ## Rm.dl1    0.1580810  0.1058503   1.493 0.138321    
    ## Rb.dl1    0.2593507  0.0760847   3.409 0.000927 ***
    ## Lm3r.l1  -0.0002752  0.0020112  -0.137 0.891431    
    ## Lyr.l1   -0.0007718  0.0029453  -0.262 0.793803    
    ## Dpy.l1    0.0112189  0.0206817   0.542 0.588654    
    ## Rm.l1    -0.1097758  0.0586879  -1.871 0.064199 .  
    ## Rb.l1     0.0385676  0.0442578   0.871 0.385506    
    ## constant  0.0073898  0.0158439   0.466 0.641886    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0012 on 105 degrees of freedom
    ## Multiple R-squared:  0.4063, Adjusted R-squared:  0.3271 
    ## F-statistic: 5.132 on 14 and 105 DF,  p-value: 0.0000002961
    ## 
    ## 
    ## Response Rb.d :
    ## 
    ## Call:
    ## lm(formula = Rb.d ~ sd1 + sd2 + sd3 + Lm3r.dl1 + Lyr.dl1 + Dpy.dl1 + 
    ##     Rm.dl1 + Rb.dl1 + Lm3r.l1 + Lyr.l1 + Dpy.l1 + Rm.l1 + Rb.l1 + 
    ##     constant - 1, data = data.mat)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0055534 -0.0006877 -0.0001130  0.0008586  0.0042890 
    ## 
    ## Coefficients:
    ##            Estimate Std. Error t value Pr(>|t|)    
    ## sd1       0.0001513  0.0004713   0.321 0.748853    
    ## sd2       0.0006676  0.0004246   1.572 0.118908    
    ## sd3       0.0008322  0.0004666   1.784 0.077394 .  
    ## Lm3r.dl1 -0.0073280  0.0039015  -1.878 0.063121 .  
    ## Lyr.dl1   0.0362931  0.0097577   3.719 0.000323 ***
    ## Dpy.dl1  -0.0005054  0.0182860  -0.028 0.978001    
    ## Rm.dl1    0.0607354  0.1405399   0.432 0.666513    
    ## Rb.dl1    0.3650805  0.1010194   3.614 0.000465 ***
    ## Lm3r.l1  -0.0012180  0.0026703  -0.456 0.649242    
    ## Lyr.l1   -0.0027223  0.0039106  -0.696 0.487885    
    ## Dpy.l1    0.0327211  0.0274596   1.192 0.236102    
    ## Rm.l1     0.0121494  0.0779212   0.156 0.876396    
    ## Rb.l1    -0.0922374  0.0587620  -1.570 0.119499    
    ## constant  0.0274489  0.0210363   1.305 0.194802    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.001594 on 105 degrees of freedom
    ## Multiple R-squared:  0.3801, Adjusted R-squared:  0.2974 
    ## F-statistic: 4.598 on 14 and 105 DF,  p-value: 0.000001958

\#The ECM formulation with m = 2 “long-run”

``` r
vecm_m2 <- ca.jo(data[,model], K = 2, season = 4, ecdet = "const", spec = "longrun")
vecm_m2 <- cajools(vecm_m2)
summary(vecm_m2)
```

    ## Response Lm3r.d :
    ## 
    ## Call:
    ## lm(formula = Lm3r.d ~ sd1 + sd2 + sd3 + Lm3r.dl1 + Lyr.dl1 + 
    ##     Dpy.dl1 + Rm.dl1 + Rb.dl1 + Lm3r.l2 + Lyr.l2 + Dpy.l2 + Rm.l2 + 
    ##     Rb.l2 + constant - 1, data = data.mat)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.078343 -0.017980 -0.004791  0.015506  0.188871 
    ## 
    ## Coefficients:
    ##           Estimate Std. Error t value  Pr(>|t|)    
    ## sd1      -0.037852   0.010903  -3.472  0.000752 ***
    ## sd2      -0.003754   0.009823  -0.382  0.703078    
    ## sd3      -0.030551   0.010795  -2.830  0.005578 ** 
    ## Lm3r.dl1 -0.452652   0.100821  -4.490 0.0000184 ***
    ## Lyr.dl1   0.124534   0.238750   0.522  0.603041    
    ## Dpy.dl1  -0.928778   0.419109  -2.216  0.028844 *  
    ## Rm.dl1    3.974254   3.014195   1.319  0.190201    
    ## Rb.dl1   -8.050637   2.162542  -3.723  0.000319 ***
    ## Lm3r.l2  -0.262296   0.061775  -4.246 0.0000471 ***
    ## Lyr.l2    0.243951   0.090468   2.697  0.008162 ** 
    ## Dpy.l2   -1.476963   0.635247  -2.325  0.021994 *  
    ## Rm.l2     5.039019   1.802622   2.795  0.006165 ** 
    ## Rb.l2    -4.987906   1.359395  -3.669  0.000384 ***
    ## constant -0.027763   0.486652  -0.057  0.954615    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.03687 on 105 degrees of freedom
    ## Multiple R-squared:  0.4757, Adjusted R-squared:  0.4058 
    ## F-statistic: 6.805 on 14 and 105 DF,  p-value: 0.000000001066
    ## 
    ## 
    ## Response Lyr.d :
    ## 
    ## Call:
    ## lm(formula = Lyr.d ~ sd1 + sd2 + sd3 + Lm3r.dl1 + Lyr.dl1 + Dpy.dl1 + 
    ##     Rm.dl1 + Rb.dl1 + Lm3r.l2 + Lyr.l2 + Dpy.l2 + Rm.l2 + Rb.l2 + 
    ##     constant - 1, data = data.mat)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.032823 -0.009230  0.000073  0.010937  0.051281 
    ## 
    ## Coefficients:
    ##           Estimate Std. Error t value Pr(>|t|)    
    ## sd1      -0.004910   0.004340  -1.131 0.260587    
    ## sd2      -0.002672   0.003910  -0.683 0.495860    
    ## sd3      -0.006613   0.004298  -1.539 0.126841    
    ## Lm3r.dl1 -0.009249   0.040137  -0.230 0.818211    
    ## Lyr.dl1   0.039091   0.095046   0.411 0.681705    
    ## Dpy.dl1  -0.226733   0.166848  -1.359 0.177083    
    ## Rm.dl1   -1.685214   1.199953  -1.404 0.163151    
    ## Rb.dl1   -2.177891   0.860909  -2.530 0.012899 *  
    ## Lm3r.l2   0.023964   0.024593   0.974 0.332079    
    ## Lyr.l2   -0.120981   0.036015  -3.359 0.001090 ** 
    ## Dpy.l2   -0.325554   0.252892  -1.287 0.200812    
    ## Rm.l2    -2.114538   0.717625  -2.947 0.003959 ** 
    ## Rb.l2     0.511458   0.541176   0.945 0.346785    
    ## constant  0.706692   0.193736   3.648 0.000414 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.01468 on 105 degrees of freedom
    ## Multiple R-squared:  0.3237, Adjusted R-squared:  0.2336 
    ## F-statistic: 3.591 on 14 and 105 DF,  p-value: 0.00007621
    ## 
    ## 
    ## Response Dpy.d :
    ## 
    ## Call:
    ## lm(formula = Dpy.d ~ sd1 + sd2 + sd3 + Lm3r.dl1 + Lyr.dl1 + Dpy.dl1 + 
    ##     Rm.dl1 + Rb.dl1 + Lm3r.l2 + Lyr.l2 + Dpy.l2 + Rm.l2 + Rb.l2 + 
    ##     constant - 1, data = data.mat)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0181514 -0.0066410 -0.0003969  0.0056380  0.0250555 
    ## 
    ## Coefficients:
    ##            Estimate Std. Error t value             Pr(>|t|)    
    ## sd1      -0.0004933  0.0025782  -0.191             0.848647    
    ## sd2       0.0004701  0.0023228   0.202             0.840001    
    ## sd3      -0.0026665  0.0025527  -1.045             0.298608    
    ## Lm3r.dl1 -0.0076411  0.0238410  -0.321             0.749226    
    ## Lyr.dl1  -0.1095159  0.0564569  -1.940             0.055085 .  
    ## Dpy.dl1  -0.9784133  0.0991062  -9.872 < 0.0000000000000002 ***
    ## Rm.dl1   -2.4642389  0.7127626  -3.457             0.000789 ***
    ## Rb.dl1    1.7904343  0.5113734   3.501             0.000681 ***
    ## Lm3r.l2  -0.0237566  0.0146078  -1.626             0.106885    
    ## Lyr.l2   -0.0112223  0.0213927  -0.525             0.600976    
    ## Dpy.l2   -0.7689589  0.1502161  -5.119            0.0000014 ***
    ## Rm.l2    -0.6429012  0.4262636  -1.508             0.134500    
    ## Rb.l2     0.2392491  0.3214544   0.744             0.458375    
    ## constant  0.2293712  0.1150779   1.993             0.048836 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.008718 on 105 degrees of freedom
    ## Multiple R-squared:  0.5904, Adjusted R-squared:  0.5357 
    ## F-statistic: 10.81 on 14 and 105 DF,  p-value: 0.000000000000008778
    ## 
    ## 
    ## Response Rm.d :
    ## 
    ## Call:
    ## lm(formula = Rm.d ~ sd1 + sd2 + sd3 + Lm3r.dl1 + Lyr.dl1 + Dpy.dl1 + 
    ##     Rm.dl1 + Rb.dl1 + Lm3r.l2 + Lyr.l2 + Dpy.l2 + Rm.l2 + Rb.l2 + 
    ##     constant - 1, data = data.mat)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0028122 -0.0006743 -0.0000191  0.0006554  0.0048715 
    ## 
    ## Coefficients:
    ##            Estimate Std. Error t value  Pr(>|t|)    
    ## sd1      -0.0003256  0.0003550  -0.917    0.3611    
    ## sd2      -0.0005794  0.0003198  -1.812    0.0729 .  
    ## sd3      -0.0001298  0.0003515  -0.369    0.7125    
    ## Lm3r.dl1 -0.0045457  0.0032824  -1.385    0.1690    
    ## Lyr.dl1  -0.0010458  0.0077730  -0.135    0.8932    
    ## Dpy.dl1   0.0277559  0.0136449   2.034    0.0445 *  
    ## Rm.dl1    0.0483052  0.0981330   0.492    0.6236    
    ## Rb.dl1    0.2979184  0.0704058   4.231 0.0000498 ***
    ## Lm3r.l2  -0.0002752  0.0020112  -0.137    0.8914    
    ## Lyr.l2   -0.0007718  0.0029453  -0.262    0.7938    
    ## Dpy.l2    0.0112189  0.0206817   0.542    0.5887    
    ## Rm.l2    -0.1097758  0.0586879  -1.871    0.0642 .  
    ## Rb.l2     0.0385676  0.0442578   0.871    0.3855    
    ## constant  0.0073898  0.0158439   0.466    0.6419    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0012 on 105 degrees of freedom
    ## Multiple R-squared:  0.4063, Adjusted R-squared:  0.3271 
    ## F-statistic: 5.132 on 14 and 105 DF,  p-value: 0.0000002961
    ## 
    ## 
    ## Response Rb.d :
    ## 
    ## Call:
    ## lm(formula = Rb.d ~ sd1 + sd2 + sd3 + Lm3r.dl1 + Lyr.dl1 + Dpy.dl1 + 
    ##     Rm.dl1 + Rb.dl1 + Lm3r.l2 + Lyr.l2 + Dpy.l2 + Rm.l2 + Rb.l2 + 
    ##     constant - 1, data = data.mat)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0055534 -0.0006877 -0.0001130  0.0008586  0.0042890 
    ## 
    ## Coefficients:
    ##            Estimate Std. Error t value Pr(>|t|)   
    ## sd1       0.0001513  0.0004713   0.321  0.74885   
    ## sd2       0.0006676  0.0004246   1.572  0.11891   
    ## sd3       0.0008322  0.0004666   1.784  0.07739 . 
    ## Lm3r.dl1 -0.0085460  0.0043582  -1.961  0.05254 . 
    ## Lyr.dl1   0.0335708  0.0103204   3.253  0.00154 **
    ## Dpy.dl1   0.0322156  0.0181167   1.778  0.07826 . 
    ## Rm.dl1    0.0728848  0.1302934   0.559  0.57709   
    ## Rb.dl1    0.2728431  0.0934794   2.919  0.00430 **
    ## Lm3r.l2  -0.0012180  0.0026703  -0.456  0.64924   
    ## Lyr.l2   -0.0027223  0.0039106  -0.696  0.48789   
    ## Dpy.l2    0.0327211  0.0274596   1.192  0.23610   
    ## Rm.l2     0.0121494  0.0779212   0.156  0.87640   
    ## Rb.l2    -0.0922374  0.0587620  -1.570  0.11950   
    ## constant  0.0274489  0.0210363   1.305  0.19480   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.001594 on 105 degrees of freedom
    ## Multiple R-squared:  0.3801, Adjusted R-squared:  0.2974 
    ## F-statistic: 4.598 on 14 and 105 DF,  p-value: 0.000001958

Not reproduced m = 3

# On page 73, there is a table with all R_2, these are simply the Multiple R-squared

\#“Therefore, when the variables are integrated of first order, the R2
makes sense \#only when the dependent variable is given as ∆xi,t . In
this case R2 measure the explanatory power of the regressor variables as
compared to the random walk \#model.””

``` r
df <- data.frame(r_squared = c(
summary(vecm_m2)$`Response Lm3r.d`$r.squared,
summary(vecm_m2)$`Response Lyr.d`$r.squared,
summary(vecm_m2)$`Response Dpy.d`$r.squared,
summary(vecm_m2)$`Response Rm.d`$r.squared,
summary(vecm_m2)$`Response Rb.d`$r.squared))

kable(df,
      digits = 2,
      format = "html")
```

<table>

<thead>

<tr>

<th style="text-align:right;">

r_squared
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

0.48
</td>

</tr>

<tr>

<td style="text-align:right;">

0.32
</td>

</tr>

<tr>

<td style="text-align:right;">

0.59
</td>

</tr>

<tr>

<td style="text-align:right;">

0.41
</td>

</tr>

<tr>

<td style="text-align:right;">

0.38
</td>

</tr>

</tbody>

</table>

# Lag length determination

``` r
# Check these likelihood lag selections
info_crit <- VARselect(data[,model], lag.max = 5, season = 4, type = "const")

var_1 <- VAR(data[,model], p = 1, type = "const", season = 4)
var_2 <- VAR(data[,model], p = 2, type = "const", season = 4)
var_3 <- VAR(data[,model], p = 3, type = "const", season = 4)
var_4 <- VAR(data[,model], p = 4, type = "const", season = 4)
var_5 <- VAR(data[,model], p = 5, type = "const", season = 4)

num_regr <- function(var_model) {
  return (var_model$K + ((var_model$K * var_model$p) - 1))
}
log_lik <- function(var_model) {
  return (logLik(var_model)[1])
}
# Not entirely sure these log likelihood values are correct
LM1 <- function(var_model) {
  return (serial.test(var_model, lags.bg = 1, type = "BG")$serial$p.value)
}
Lmk <- function(var_model) {
  return (serial.test(var_model, lags.bg = var_model$p, type = "BG")$serial$p.value)
}
# Serial correlation stats don't look quite right either, maybe ES correction?
# There's more on this on page 74
df <- data.frame(
  "Model" = c("VAR(1)", "VAR(2)", "VAR(3)", "VAR(4)", "VAR(5)"),
  "k" = c(1, 2, 3, 4, 5),
  "regr" = c(num_regr(var_1), num_regr(var_2), num_regr(var_3), num_regr(var_4), num_regr(var_5)),
  "Log-Lik" = c(log_lik(var_1), log_lik(var_2), log_lik(var_3), log_lik(var_4), log_lik(var_5)),
  "SC" = c(info_crit$criteria[3,1], info_crit$criteria[3,2], info_crit$criteria[3,3], info_crit$criteria[3,4], info_crit$criteria[3,5]),
  "H-Q" = c(info_crit$criteria[2,1], info_crit$criteria[2,2], info_crit$criteria[2,3], info_crit$criteria[2,4], info_crit$criteria[2,5]),
  "AIC" = c(info_crit$criteria[1,1], info_crit$criteria[1,2], info_crit$criteria[1,3], info_crit$criteria[1,4], info_crit$criteria[1,5]),
  "LM(1)" = c(LM1(var_1), LM1(var_2), LM1(var_3), LM1(var_4), LM1(var_5)),
  "Lm(k)" = c(Lmk(var_1), Lmk(var_2), Lmk(var_3), Lmk(var_4), Lmk(var_5))
)
```

# Tests of residual heteroscedasticity

``` r
# We have to convert back to a VAR from VECM, whereas text appears to be using VECM directly
# Not sure if this is actually making any difference?
# We want univariate, i.e. applied for each element of vector
# This reproduces exactly page 74
arch <- arch.test(var_2, lags.single = 2, multivariate.only = FALSE)
df <- data.frame(
  "variable" = c("Lm3r", "Lyr", "Dpy", "Rm", "Rb"),
  "Chi-squared" = c(arch$arch.uni$Lm3r$statistic,
                    arch$arch.uni$Lyr$statistic,
                    arch$arch.uni$Dpy$statistic,
                    arch$arch.uni$Rm$statistic,
                    arch$arch.uni$Rb$statistic),
  "p-value" = c(arch$arch.uni$Lm3r$p.value,
                arch$arch.uni$Lyr$p.value,
                arch$arch.uni$Dpy$p.value,
                arch$arch.uni$Rm$p.value,
                arch$arch.uni$Rb$p.value)
)
```

# Normality tests page 77

# I’ve put together both the Jacque-Bera test and Shenton-Bowman test here

``` r
# Jarque-Beta tests
# H0: Data is normally distributed
# H1: Data is not normally distributed
# < sig level -> Sufficient evidence not normally distributed
JB_test <- normality.test(var_2, multivariate.only = FALSE)
JB_stat <- c(
  JB_test$jb.uni$Lm3r$statistic,
  JB_test$jb.uni$Lyr$statistic,
  JB_test$jb.uni$Dpy$statistic,
  JB_test$jb.uni$Rm$statistic,
  JB_test$jb.uni$Rb$statistic
)
JB_pvalue <- c(
  JB_test$jb.uni$Lm3r$p.value,
  JB_test$jb.uni$Lyr$p.value,
  JB_test$jb.uni$Dpy$p.value,
  JB_test$jb.uni$Rm$p.value,
  JB_test$jb.uni$Rb$p.value
)
# Shenton-Bowman test in text
SB_Lm3r <- msk(vecm_m2$residuals[,"Lm3r.d"], B = 1000)
SB_Lyr <- msk(vecm_m2$residuals[,"Lyr.d"], B = 1000)
SB_Dpy <- msk(vecm_m2$residuals[,"Dpy.d"], B = 1000)
SB_Rm <- msk(vecm_m2$residuals[,"Rm.d"], B = 1000)
SB_Rb <- msk(vecm_m2$residuals[,"Rb.d"], B = 1000)

SB_stat <- c(
  SB_Lm3r$mv.test['Statistic'],
  SB_Lyr$mv.test['Statistic'],
  SB_Dpy$mv.test['Statistic'],
  SB_Rm$mv.test['Statistic'],
  SB_Rb$mv.test['Statistic']
)
SB_pvalue <- c(
  SB_Lm3r$mv.test['p-value'],
  SB_Lyr$mv.test['p-value'],
  SB_Dpy$mv.test['p-value'],
  SB_Rm$mv.test['p-value'],
  SB_Rb$mv.test['p-value']
)
skew <- c(
  skewness(vecm_m2$residuals[,"Lm3r.d"]),
  skewness(vecm_m2$residuals[,"Lyr.d"]),
  skewness(vecm_m2$residuals[,"Dpy.d"]),
  skewness(vecm_m2$residuals[,"Rm.d"]),
  skewness(vecm_m2$residuals[,"Rb.d"])
)
# Excess kurtosis - 3
kurt <- c(
  kurtosis(vecm_m2$residuals[,"Lm3r.d"]) - 3,
  kurtosis(vecm_m2$residuals[,"Lyr.d"]) -3,
  kurtosis(vecm_m2$residuals[,"Dpy.d"]) -3,
  kurtosis(vecm_m2$residuals[,"Rm.d"]) -3,
  kurtosis(vecm_m2$residuals[,"Rb.d"] -3)
)
df <- data.frame(
  "variable" = c("Lm3r", "Lyr", "Dpy", "Rm", "Rb"),
  "Jarque-Beta statistic" = JB_stat,
  "Jarque Beta p-value" = JB_pvalue,
  "Shenton-Bowman statistic" = SB_stat,
  "Shenton-Bowman p-value" = SB_pvalue,
  "Skewness" = skew,
  "Excess kurtosis" = kurt
)


# This somewhat confirms same results from text, using Shenton-Bowman
# Univariate - applied to residuals of each equation in system
# Lm3r = not normal
# Rm = not normal
# Rb = not normal
# The Jacque-Bera test also confirms this for univariate

# Multivariate
JB_test$jb.mul$JB$statistic[1,1]
```

    ## [1] 277.4227

``` r
JB_test$jb.mul$JB$p.value[1,1]
```

    ## [1] 0

``` r
SB_test <- msk(vecm_m2$residuals, B = 1000)
SB_mv_stat <- unname(SB_test$mv.test['Statistic'])
SB_mv_pvalue <- unname(SB_test$mv.test['p-value'])
```

# Table 6.3 (says VAR, but is actually VECM form), using corrected m3 & dummies

``` r
model <- c("Lm3rC", "Lyr", "Dpy", "Rm", "Rb")
dummies <- c("D754", "D764", "D831")
vecm_m2 <- ca.jo(data[,model], K = 2, ecdet = "trend", spec = "transitory", dumvar = data[,dummies])
vecm_m2 <- cajools(vecm_m2)
summary(vecm_m2)
```

    ## Response Lm3rC.d :
    ## 
    ## Call:
    ## lm(formula = Lm3rC.d ~ constant + D754 + D764 + D831 + Lm3rC.dl1 + 
    ##     Lyr.dl1 + Dpy.dl1 + Rm.dl1 + Rb.dl1 + Lm3rC.l1 + Lyr.l1 + 
    ##     Dpy.l1 + Rm.l1 + Rb.l1 + trend.l1 - 1, data = data.mat)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.069765 -0.012210  0.000509  0.015232  0.056849 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value  Pr(>|t|)    
    ## constant   0.6402765  0.4851382   1.320  0.189806    
    ## D754       0.0100238  0.0205102   0.489  0.626069    
    ## D764      -0.0175672  0.0259004  -0.678  0.499112    
    ## D831       0.0316246  0.0123224   2.566  0.011699 *  
    ## Lm3rC.dl1  0.0020671  0.0959670   0.022  0.982856    
    ## Lyr.dl1    0.0021718  0.1508191   0.014  0.988538    
    ## Dpy.dl1    0.3302837  0.2829747   1.167  0.245805    
    ## Rm.dl1    -0.3306618  2.0973254  -0.158  0.875031    
    ## Rb.dl1    -1.6628274  1.6295891  -1.020  0.309908    
    ## Lm3rC.l1  -0.2598536  0.0556646  -4.668 0.0000091 ***
    ## Lyr.l1     0.1326333  0.0744400   1.782  0.077709 .  
    ## Dpy.l1    -0.5040939  0.4531472  -1.112  0.268519    
    ## Rm.l1      2.8118755  1.2554776   2.240  0.027239 *  
    ## Rb.l1     -3.3441388  0.9639297  -3.469  0.000761 ***
    ## trend.l1   0.0003988  0.0002768   1.441  0.152676    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02431 on 104 degrees of freedom
    ## Multiple R-squared:  0.3586, Adjusted R-squared:  0.2661 
    ## F-statistic: 3.876 on 15 and 104 DF,  p-value: 0.00001802
    ## 
    ## 
    ## Response Lyr.d :
    ## 
    ## Call:
    ## lm(formula = Lyr.d ~ constant + D754 + D764 + D831 + Lm3rC.dl1 + 
    ##     Lyr.dl1 + Dpy.dl1 + Rm.dl1 + Rb.dl1 + Lm3rC.l1 + Lyr.l1 + 
    ##     Dpy.l1 + Rm.l1 + Rb.l1 + trend.l1 - 1, data = data.mat)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.028503 -0.010049  0.001867  0.010749  0.028992 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## constant   0.8856212  0.2877009   3.078 0.002662 ** 
    ## D754       0.0328618  0.0121632   2.702 0.008056 ** 
    ## D764       0.0063310  0.0153597   0.412 0.681052    
    ## D831      -0.0058924  0.0073075  -0.806 0.421881    
    ## Lm3rC.dl1  0.0112214  0.0569112   0.197 0.844077    
    ## Lyr.dl1    0.1734956  0.0894401   1.940 0.055113 .  
    ## Dpy.dl1    0.0970420  0.1678122   0.578 0.564327    
    ## Rm.dl1     0.7167656  1.2437744   0.576 0.565669    
    ## Rb.dl1    -2.3599039  0.9663932  -2.442 0.016294 *  
    ## Lm3rC.l1   0.0248752  0.0330107   0.754 0.452823    
    ## Lyr.l1    -0.1495818  0.0441451  -3.388 0.000994 ***
    ## Dpy.l1    -0.3027488  0.2687293  -1.127 0.262507    
    ## Rm.l1     -2.0428440  0.7445344  -2.744 0.007155 ** 
    ## Rb.l1      0.5004614  0.5716381   0.875 0.383327    
    ## trend.l1   0.0001985  0.0001642   1.209 0.229224    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.01442 on 104 degrees of freedom
    ## Multiple R-squared:  0.3538, Adjusted R-squared:  0.2606 
    ## F-statistic: 3.795 on 15 and 104 DF,  p-value: 0.00002447
    ## 
    ## 
    ## Response Dpy.d :
    ## 
    ## Call:
    ## lm(formula = Dpy.d ~ constant + D754 + D764 + D831 + Lm3rC.dl1 + 
    ##     Lyr.dl1 + Dpy.dl1 + Rm.dl1 + Rb.dl1 + Lm3rC.l1 + Lyr.l1 + 
    ##     Dpy.l1 + Rm.l1 + Rb.l1 + trend.l1 - 1, data = data.mat)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0208478 -0.0055057 -0.0001513  0.0056247  0.0218008 
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t value    Pr(>|t|)    
    ## constant  -0.00770875  0.16686288  -0.046      0.9632    
    ## D754      -0.01355740  0.00705448  -1.922      0.0574 .  
    ## D764       0.00250649  0.00890842   0.281      0.7790    
    ## D831      -0.00578734  0.00423827  -1.365      0.1750    
    ## Lm3rC.dl1  0.00980182  0.03300776   0.297      0.7671    
    ## Lyr.dl1   -0.11376441  0.05187410  -2.193      0.0305 *  
    ## Dpy.dl1   -0.20243005  0.09732893  -2.080      0.0400 *  
    ## Rm.dl1    -1.59646347  0.72137338  -2.213      0.0291 *  
    ## Rb.dl1     0.85127614  0.56049584   1.519      0.1318    
    ## Lm3rC.l1  -0.00080210  0.01914581  -0.042      0.9667    
    ## Lyr.l1     0.00589113  0.02560358   0.230      0.8185    
    ## Dpy.l1    -0.84880064  0.15585961  -5.446 0.000000347 ***
    ## Rm.l1     -0.48296982  0.43182052  -1.118      0.2660    
    ## Rb.l1      0.23356407  0.33154285   0.704      0.4827    
    ## trend.l1  -0.00017903  0.00009521  -1.880      0.0629 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.008361 on 104 degrees of freedom
    ## Multiple R-squared:  0.6268, Adjusted R-squared:  0.5729 
    ## F-statistic: 11.64 on 15 and 104 DF,  p-value: 0.0000000000000003412
    ## 
    ## 
    ## Response Rm.d :
    ## 
    ## Call:
    ## lm(formula = Rm.d ~ constant + D754 + D764 + D831 + Lm3rC.dl1 + 
    ##     Lyr.dl1 + Dpy.dl1 + Rm.dl1 + Rb.dl1 + Lm3rC.l1 + Lyr.l1 + 
    ##     Dpy.l1 + Rm.l1 + Rb.l1 + trend.l1 - 1, data = data.mat)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0027497 -0.0005354  0.0000291  0.0006334  0.0032102 
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t value   Pr(>|t|)    
    ## constant  -0.01364744  0.02117278  -0.645     0.5206    
    ## D754       0.00037173  0.00089512   0.415     0.6788    
    ## D764       0.00567645  0.00113037   5.022 0.00000213 ***
    ## D831       0.00145914  0.00053778   2.713     0.0078 ** 
    ## Lm3rC.dl1 -0.00901711  0.00418827  -2.153     0.0336 *  
    ## Lyr.dl1    0.00191954  0.00658216   0.292     0.7712    
    ## Dpy.dl1    0.01111286  0.01234980   0.900     0.3703    
    ## Rm.dl1     0.07587674  0.09153311   0.829     0.4090    
    ## Rb.dl1     0.29583547  0.07111979   4.160 0.00006573 ***
    ## Lm3rC.l1  -0.00173126  0.00242936  -0.713     0.4777    
    ## Lyr.l1     0.00351575  0.00324877   1.082     0.2817    
    ## Dpy.l1     0.02411237  0.01977660   1.219     0.2255    
    ## Rm.l1     -0.11105098  0.05479253  -2.027     0.0452 *  
    ## Rb.l1      0.04743211  0.04206857   1.127     0.2621    
    ## trend.l1  -0.00001500  0.00001208  -1.242     0.2171    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.001061 on 104 degrees of freedom
    ## Multiple R-squared:  0.5406, Adjusted R-squared:  0.4743 
    ## F-statistic: 8.157 on 15 and 104 DF,  p-value: 0.000000000006661
    ## 
    ## 
    ## Response Rb.d :
    ## 
    ## Call:
    ## lm(formula = Rb.d ~ constant + D754 + D764 + D831 + Lm3rC.dl1 + 
    ##     Lyr.dl1 + Dpy.dl1 + Rm.dl1 + Rb.dl1 + Lm3rC.l1 + Lyr.l1 + 
    ##     Dpy.l1 + Rm.l1 + Rb.l1 + trend.l1 - 1, data = data.mat)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0043582 -0.0006784 -0.0000440  0.0005830  0.0034904 
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t value Pr(>|t|)    
    ## constant  -0.01442783  0.03048010  -0.473 0.636954    
    ## D754      -0.00084259  0.00128861  -0.654 0.514634    
    ## D764       0.00036161  0.00162726   0.222 0.824580    
    ## D831      -0.00218967  0.00077419  -2.828 0.005614 ** 
    ## Lm3rC.dl1 -0.01647491  0.00602938  -2.732 0.007389 ** 
    ## Lyr.dl1    0.03620848  0.00947561   3.821 0.000226 ***
    ## Dpy.dl1    0.00186874  0.01777864   0.105 0.916490    
    ## Rm.dl1    -0.04073025  0.13177007  -0.309 0.757863    
    ## Rb.dl1     0.28196571  0.10238329   2.754 0.006950 ** 
    ## Lm3rC.l1   0.00251684  0.00349728   0.720 0.473350    
    ## Lyr.l1     0.00069084  0.00467689   0.148 0.882855    
    ## Dpy.l1     0.00355531  0.02847018   0.125 0.900861    
    ## Rm.l1      0.11645792  0.07887874   1.476 0.142854    
    ## Rb.l1     -0.14439173  0.06056146  -2.384 0.018928 *  
    ## trend.l1  -0.00002369  0.00001739  -1.362 0.176031    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.001527 on 104 degrees of freedom
    ## Multiple R-squared:  0.436,  Adjusted R-squared:  0.3547 
    ## F-statistic:  5.36 on 15 and 104 DF,  p-value: 0.00000007444

``` r
# Good results, but can't properly specify deterministic part, Rm & Rb are noticeably less accurate
```

# Normalization page 122

``` r
# This is normalized according to text, bit hit and miss if they correspond,
# Pretty sure this is good normalization, but remember it is arbitrary
# Note the order seems to be mixed up
# Remember there is a way of getting t-stats, etc, going via Pfaff v27i04

vecm_m2 <- ca.jo(data[,model], K = 2, ecdet = "trend", spec = "transitory", dumvar = data[,dummies])
# summary(vecm_m2$rlm)

vecm_m2@GAMMA
```

    ##             constant         D754          D764         D831    Lm3rC.dl1
    ## Lm3rC.d  0.640276494  0.010023763 -0.0175672415  0.031624596  0.002067102
    ## Lyr.d    0.885621151  0.032861826  0.0063310351 -0.005892424  0.011221365
    ## Dpy.d   -0.007708747 -0.013557396  0.0025064867 -0.005787338  0.009801816
    ## Rm.d    -0.013647439  0.000371734  0.0056764505  0.001459142 -0.009017110
    ## Rb.d    -0.014427833 -0.000842594  0.0003616058 -0.002189675 -0.016474910
    ##              Lyr.dl1      Dpy.dl1      Rm.dl1     Rb.dl1
    ## Lm3rC.d  0.002171845  0.330283687 -0.33066184 -1.6628274
    ## Lyr.d    0.173495589  0.097042030  0.71676564 -2.3599039
    ## Dpy.d   -0.113764407 -0.202430048 -1.59646347  0.8512761
    ## Rm.d     0.001919539  0.011112856  0.07587674  0.2958355
    ## Rb.d     0.036208476  0.001868744 -0.04073025  0.2819657

``` r
vecm_m2@DELTA
```

    ##                 [,1]             [,2]            [,3]             [,4]
    ## [1,]  0.000516494904  0.0000377954218 -0.000084963684 -0.0000047122020
    ## [2,]  0.000037795422  0.0001816425591 -0.000003875506 -0.0000001950332
    ## [3,] -0.000084963684 -0.0000038755056  0.000061101889  0.0000019942384
    ## [4,] -0.000004712202 -0.0000001950332  0.000001994238  0.0000009837638
    ## [5,] -0.000002289914  0.0000024849239  0.000001788772  0.0000005499878
    ##                  [,5]
    ## [1,] -0.0000022899139
    ## [2,]  0.0000024849239
    ## [3,]  0.0000017887718
    ## [4,]  0.0000005499878
    ## [5,]  0.0000020387689

``` r
vecm_m2@PI
```

    ##             Lm3rC.l1        Lyr.l1       Dpy.l1      Rm.l1       Rb.l1
    ## Lm3rC.d -0.259853603  0.1326333454 -0.504093944  2.8118755 -3.34413884
    ## Lyr.d    0.024875212 -0.1495817958 -0.302748778 -2.0428440  0.50046145
    ## Dpy.d   -0.000802095  0.0058911268 -0.848800638 -0.4829698  0.23356407
    ## Rm.d    -0.001731260  0.0035157529  0.024112367 -0.1110510  0.04743211
    ## Rb.d     0.002516844  0.0006908393  0.003555315  0.1164579 -0.14439173
    ##               trend.l1
    ## Lm3rC.d  0.00039879695
    ## Lyr.d    0.00019854212
    ## Dpy.d   -0.00017902885
    ## Rm.d    -0.00001500185
    ## Rb.d    -0.00002369302

``` r
vecm_m2@W
```

    ##             Lm3rC.l1       Lyr.l1        Dpy.l1         Rm.l1         Rb.l1
    ## Lm3rC.d -0.050143851 -0.112001301 -0.0558325305 -0.0372553844 -0.0046205353
    ## Lyr.d   -0.005844156  0.034819056  0.0328456245 -0.0351609541 -0.0017843591
    ## Dpy.d   -0.042504458  0.018343917  0.0149535819  0.0064970057  0.0019078590
    ## Rm.d     0.001357615 -0.003491481  0.0009712355 -0.0009646093  0.0003959802
    ## Rb.d    -0.000100005  0.007241700 -0.0027094238 -0.0023168194  0.0004013931
    ##                           trend.l1
    ## Lm3rC.d  0.00000000000005215255984
    ## Lyr.d   -0.00000000000007706225447
    ## Dpy.d   -0.00000000000000008801795
    ## Rm.d     0.00000000000000174865174
    ## Rb.d    -0.00000000000000023954478

``` r
# Beta
beta <- vecm_m2@Vorg
v_1 <- beta[,1]
alpha_1 <- v_1 / v_1['Dpy.l1']
v_2 <- beta[,2]
alpha_v2 <- v_2 / v_2['Lm3rC.l1']
v_3 <- beta[,3]
alpha_v3 <- v_3 / v_3['Rm.l1']
v_4 <- beta[,4]
alpha_v4 <- v_4 / v_4['Lyr.l1']
v_5 <- beta[,5]
alpha_v5 <- v_5 / v_5['Rb.l1']

# This normalization gets me near Juselius result, but problem is deterministic
# spec doesn't allow it in Pfaff package. I've tried in tsDyn too, but don't
# like this function.
# vecm_tsDyn <- VECM(data = data[,model], lag = 2, r = 4, include = "trend", estim = "ML", exogen = data[,dummies])
# summary(vecm_tsDyn)
# print(vecm_tsDyn)
# coefA(vecm_tsDyn)
# coefB(vecm_tsDyn)
# # Think this is combined effects, big pi
# coefPI(vecm_tsDyn)
# vecm_tsDyn$model.specific$beta
# vecm_tsDyn$model.specific$coint

# summary(var_2)
# causality(vecm_m2, cause = "Lm3r")
# restrict(var_2, method = "ser")
# 
# 
# var_2$varresult$Lyr
# Anova(var_2$varresult$Lm3r, var_2$varresult$Lyr)
# linearHypothesis(var_2$varresult$Lm3r, c("Lm3r.l1 = 0"))
# linearHypothesis(var_2$varresult$Lyr, c("Lm3r.l1 = 0"))
# linearHypothesis(var_2$varresult$Dpy, c("Lm3r.l1 = 0"))
# linearHypothesis(var_2$varresult$Rm, c("Lm3r.l1 = 0"))
# linearHypothesis(var_2$varresult$Rb, c("Lm3r.l1 = 0"))
```
