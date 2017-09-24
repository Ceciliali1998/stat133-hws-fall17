hw01-Cecilia-Li
================

``` r
load("data/nba2017-salary-points.RData")
```

``` r
ls()
```

    ## [1] "experience" "player"     "points"     "points1"    "points2"   
    ## [6] "points3"    "position"   "salary"     "team"

**1) A Bit of Data Processing**

``` r
#Create a New salary Variable
salary_new <- 
  as.numeric(format(round(salary / 1000000, 2), nsmall <-  2))
```

``` r
#Create a New experience Variable
experience_new <- as.integer(replace(experience,experience == 'R',0))
```

``` r
#Create a New position Variable
position_new <- factor(position)
#Create More Descriptive Labels
levels(position_new) <-
  c('center',
  'small_fwd',
  'power_fwd',
  'shoot_guard',
  'point_guard')
  table(position)
```

    ## position
    ##  C PF PG SF SG 
    ## 89 89 85 83 95

**2) Scatterplot of Points and Salary**

``` r
plot(
  points,
  salary_new,
  main = 'Scatterplot of Points and Salary',
  xlab = 'Points',
  ylab = 'Salary (in millions)',
  col = 'red'
  )
```

![](hw01-Cecilia-Li_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png) **3) Correlation between Points and Salary**

``` r
#number of individuals
n <- length(player)
n
```

    ## [1] 441

``` r
#mean of variable x (points)
mean_x <- sum(points)/n
mean_x
```

    ## [1] 546.6054

``` r
#mean of variable y (salaries)
mean_y <- sum(salary_new)/n
mean_y
```

    ## [1] 6.186689

``` r
#variance of x
var_x <- sum((points - mean_x)**2)/(n-1)
var_x
```

    ## [1] 239136.2

``` r
#variance of y
var_y <- sum((salary_new - mean_y)**2)/(n-1)
var_y
```

    ## [1] 43.19524

``` r
#sd of x
sd_x <- sqrt(var_x)
sd_x
```

    ## [1] 489.0156

``` r
#sd of y
sd_y <- sqrt(var_y)
sd_y
```

    ## [1] 6.572309

``` r
#covariance between X and Y
covariance <- sum((points-mean_x)*(salary_new-mean_y))/(n-1)
covariance
```

    ## [1] 2046.424

``` r
#correlation between X and Y
correlation <- covariance/((sd_x)*(sd_y))
correlation
```

    ## [1] 0.6367296

**4) Simple Linear Regression**

``` r
#b1 the slope term
b1 <- (correlation)*(sd_y/sd_x)
b1
```

    ## [1] 0.008557567

``` r
#b0 the intercept
b0 <- mean_y-(b1*mean_x)
b0
```

    ## [1] 1.509077

``` r
#The vector of predicated values
y_hat <- b0 + b1*points
y_hat
```

    ##   [1]  9.655880  5.959011  9.159542  1.594652  3.751159 20.327167 10.058086
    ##   [8]  2.090991  5.916224  4.067789  1.834264  7.311107  8.654645  5.017679
    ##  [15]  3.032324  7.293992  1.586095  3.040881  2.844057  1.560422  6.361217
    ##  [22]  4.512783  2.638675  2.929633 11.281818  4.701049 17.049618 18.230563
    ##  [29]  5.342867  6.900344  1.628883  7.841676  2.792712 18.795362  6.968804
    ##  [36]  2.424736  2.921075  9.715783 13.010447  3.674141  6.951689  2.698578
    ##  [43]  3.468759  5.317194  4.307401  4.333074 16.732988  2.202240  1.517634
    ##  [50]  1.714458  2.989536  5.103255 16.955485  9.065408 10.605770 10.708461
    ##  [57]  2.279258  2.826942  3.947983  2.373391 13.609476 10.083759  3.819620
    ##  [64]  2.031088  8.363688  3.708371  4.855085  2.955305  5.231618 12.171805
    ##  [71]  1.723016  5.308636 11.290376 17.186539  9.647323 10.280583  4.136250
    ##  [78]  4.863643  5.154600  8.072731  6.446793  6.027472  5.368539  2.724251
    ##  [85]  2.219355  3.443087  7.353895  4.264613  6.087375  8.483494  1.688786
    ##  [92] 12.240266  2.090991  3.494432  1.877052  3.023766  6.900344 11.547103
    ##  [99] 16.698758  2.013973  8.474936  1.859937  5.248733  1.971185  4.213268
    ## [106]  3.999329 10.888170  3.057996  4.675376 17.049618  2.270700  4.050674
    ## [113]  7.875907  3.562893  6.113048  8.688875  7.747543 14.199949 12.710932
    ## [120]  9.852704  2.424736  6.121605  3.186360  4.281728  2.347718  5.762187
    ## [127] 10.083759  1.774361  7.054380  4.709607 10.965188  4.632589  3.451644
    ## [134]  3.143572  2.595888  2.022531  7.995712  8.072731 10.468849 10.965188
    ## [141]  1.543307  2.715694  7.944367  4.410092 12.813623  2.724251  1.936955
    ## [148]  1.808591  6.977362  8.988390  6.669290  1.663113 17.169424  8.183979
    ## [155]  8.774451  7.867349  1.774361 11.470085  4.179038  1.996858 15.706080
    ## [162]  2.322045  8.654645 11.384509  3.494432  6.891786 11.743927  5.753630
    ## [169]  3.862408  1.851379  2.347718  5.146043  3.348954  2.570215  6.532368
    ## [176] 10.229237  5.642382  3.913753  6.780538  2.210797 10.460292 11.495757
    ## [183]  6.968804  4.307401  1.628883  4.221825 10.888170  1.509077  1.705901
    ## [190]  4.067789  2.056761 10.408946  7.174186  6.558041  1.791476  6.874671
    ## [197]  3.246263  7.978597  6.292757  8.902815  6.044587  2.775597  6.267084
    ## [204]  1.842822  5.317194  1.765804  2.322045 14.679172  5.514018  6.155836
    ## [211]  5.984684  5.171715  5.291521  2.587330  3.297608  4.564128  7.285434
    ## [218]  9.373481  5.205946  7.576392  6.421120  1.671670  4.213268  8.149749
    ## [225]  6.018914  2.767039  5.548248 14.816093  2.664348 16.416358  2.484639
    ## [232]  3.922311  4.837970 18.615653  5.154600  2.313488  5.762187  6.438235
    ## [239]  4.102019  2.621560  4.820855  1.723016  5.642382 17.665763  3.614238
    ## [246] 12.146132  5.933339  8.004270  8.286670  6.968804  2.039646  1.628883
    ## [253]  8.509166 11.923636  1.534749 21.670705  1.620325  4.444322  6.018914
    ## [260]  6.977362  9.886935  5.822090  9.518959  2.005416  2.245027  9.116754
    ## [267] 12.770835  4.007886  1.543307 10.956631 10.314813  1.594652 11.547103
    ## [274] 10.135104  5.650939  7.593507  2.202240  6.113048  3.100784  3.930868
    ## [281]  4.401534  5.034794  5.582479  8.603300 15.209741  2.758481  6.481023
    ## [288]  7.627737  1.697343  2.364833  7.910137 11.239030  5.188830  5.274406
    ## [295]  4.983449  5.976127  5.608151  2.749924 10.349043  5.111812  1.628883
    ## [302]  2.262143  1.791476  1.877052 23.399333  3.075111  9.253675  3.280493
    ## [309] 10.640001  5.146043  3.126457  3.306166  2.005416  5.180273  7.405240
    ## [316]  2.921075 13.883319 13.618034  7.011592  6.224296  6.523811  2.416179
    ## [323]  1.979743 10.306256  6.061702  8.740221 17.229327 18.829592  3.220590
    ## [330]  6.523811  2.176567  4.110577  8.124076  4.940661  4.307401  2.347718
    ## [337]  3.374626  1.774361 11.307491  3.751159  6.669290  8.791566  8.449263
    ## [344]  7.388125  4.119135  6.532368  2.219355  3.605681  1.748689 11.957866
    ## [351]  1.543307  8.526282 11.067879  3.280493 19.471410  1.603210  2.253585
    ## [358]  5.231618  5.051909  2.792712  7.499374  3.793947 10.314813  2.236470
    ## [365]  1.954070  6.326987  5.479788  1.919840  1.577537  5.248733  8.089846
    ## [372]  4.504225  5.924781 14.499463  4.769510  1.851379  3.117899  2.792712
    ## [379]  3.331838  9.193772  9.946838  5.000564  5.454115  5.916224  5.745072
    ## [386]  4.743837  9.210887  5.839206  2.570215  5.531133  2.484639  2.185124
    ## [393]  6.318429  3.982213  7.336780  2.903960  6.737750  2.048203 18.050854
    ## [400]  3.194917  2.407621  8.492051  1.611767 19.146222  4.016444  4.957776
    ## [407]  1.894167  8.663203  8.115518  3.297608  9.116754  7.841676  2.613003
    ## [414]  9.929723  2.535985  3.939426 11.820945  9.852704  5.351424  5.146043
    ## [421]  1.996858  8.278112  4.786625  3.571450  4.940661  2.963863  4.469995
    ## [428]  6.754865  6.600829  2.946748 16.279437  2.758481  1.534749 13.404095
    ## [435]  5.223061  5.094697  7.952925  1.628883  9.647323  5.308636  4.906431

``` r
#Summary statistics of y_hat
summary(y_hat)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.509   2.844   5.206   6.187   8.184  23.399

*1）Regression Equation*

$\\hat Y = b\_0 + b\_1\*X $

$\\hat Y = 0.008557567\*X + 1.509077$

*2）Interpretation of the slope coefficient*

*b*<sub>1</sub> = 0.008557567
 is the slope coefficient of the regression equation, and it tells the amount of change in salary that can be expected to the result from an unit increase in point.

*3）Interpretation of the intercept*

*b*<sub>0</sub> = 1.509077
 is the intercept of the regression equation, and it represents the expected value of y (salary) when x (points) equals 0.

*4）Predicted salary*

0 points: 1.509077 million dollars

100 points: 2.3648337 million dollars

500 points: 5.7878605 million dollars

1000 points: 10.060644 million dollars

2000 points: 18.624211 million dollars

**5) Plotting the regression line**

``` r
plot(
  points,
  salary_new,
  main = 'Regression and Lowess Lines',
  xlab = 'Points',
  ylab = 'Salary (in millions)',
  col = 'grey'
  )
abline(a = 1.509077, b = 0.008557567, lwd = 4, col = 'blue')
lines(lowess(points, salary_new), lwd = 4, col = 'red')
text(x = c(2400,2500), y = c(20,30) , labels = c('regression','lowess'), col = c('blue','red'))
```

![](hw01-Cecilia-Li_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-1.png) **6) Regression residuals and Coefficient of Determination R^2**

``` r
#the vector of residuals
is.vector(residuals)
```

    ## [1] FALSE

``` r
#the Residual Sum of Squares
RSS<- sum((salary_new - y_hat) ** 2)
RSS
```

    ## [1] 11300.45

``` r
#the Total Sum of Squares
TSS <- sum((salary_new - mean_y)**2)
TSS
```

    ## [1] 19005.91

``` r
#the coefficient of determination
R <- 1 - (RSS/TSS)
R
```

    ## [1] 0.4054246

**7) Exploring Position and Experience**

``` r
#A scatterplot of Years-of-Experience and Salary
plot(
  experience_new,
  salary_new,
  main = 'Scatterplot with lowess smooth',
  xlab = 'Years of Experience',
  ylab = 'Salary (in millions)',
  col = 'grey'
  )
lines(lowess(experience_new, salary_new), lwd = 4, col = 'red')
```

![](hw01-Cecilia-Li_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-1.png)

3D-scatterplot of Points, Experience, and Salary
================================================

install.packages('scatterplot3d')

``` r
scatterplot3d::scatterplot3d (points, experience_new, salary_new, xlab = 'points', ylab = 'experience', zlab = 'salary', main = '3D Scatterplot', color = 'red', scale.y = 1.3, angle = 55)
```

![](hw01-Cecilia-Li_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-13-1.png)

``` r
#Boxplot
boxplot(salary_new ~ position_new, xlab = 'Position', ylab = 'Salary (in millions)')
```

![](hw01-Cecilia-Li_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-14-1.png) *1) concise descriptions for the plots of this section*

Scatterplot:

From "Scatterplot with lowess smooth"", it is shown that salary is related with the years in experience. As years of experience increases up until 7, salary also increases. After playing for 7 years, the salaries plateau and decrease after that. The graph suggests that salaries of NBA players increase in their early years and decrease as time progresses.

3D Scatterplot:

The 3D scatterplot represents the relationship between points, salary, and experience, with on the x-axis, z-axis and on the y-axis respectively.

Boxplot:

The boxplot presents the relationship between position and salary. This shows the min, max, median, and quartiles of each distribution.

*2) does Experience seem to be related with Salary?*

Yes. From the 3D scatterplot, it could be approximately concluded that there is a positive relationship between experience and salary. Compared with players with more than 10 years of experience, those have 5 years or fewer experience earn relatively lower salaries according to the 3D scatterplot. So as years of experience increase, salaries increase as well.

*3) does Position seem to be related with Salary?*

No. According to the boxplot showing 5 different positions, the median salaries for players with different positions are roughly the same. The distributions are slightly different, for example the middle 50 % of players with position center earn higher salaries than those play point guard. But the five distributions are both right-skewes, thus it's hard to draw a conclusion about the relationship between salaries and positions.

**8) Comments and Reflections**

• What things were hard, even though you saw them in class?

Using the right functions, and learning to apply them was extremely difficult. Also, writing out the code without a structure taught was also hard.

• What was easy(-ish) even though we haven’t done it in class?

Finding the mean/length/standard deviation was easy given my background in statistics.

• If this was the first time you were using git, how do you feel about it?

It is extremely complicated and confusing.

• If this was the first time using GitHub, how do you feel about it?

It is extremely complicated and confusing.

• Did you need help to complete the assignment? If so, what kind of help? Who helped you?

I needed help with the assignment set-up and got my lab GSI to walk me through the tutorial.

• How much time did it take to complete this HW?

Around 8 hours.

• What was the most time consuming part?

Learning the functions and writing the code, because we were not taught this in lecture.

• Was there anything that you did not understand? or fully grasped?

I still do not know how to memorize all the functions and knowing the syntax.

• Was there anything frustrating in particular?

Learning how to push my assignment onto Github.

• Was there anything exciting? Something that you feel proud of? (Don’t be shy, we won’t tell anyone).

I like to convince myself that I know how to code in statistics when working with RStudio.
