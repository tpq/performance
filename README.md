
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Welcome

The goal of the `performance` package is to provides a set of functions
to analyze and evaluate the performance of pairs of variables. It offers
a comprehensive set of performance metrics based on the variable types,
including continuous, categorical, and binary variables. The package
allows users to gain insights into the relationships between different
variables and assess their statistical significance.

You can install the development version of `performance` like so:

``` r
devtools::install_github("tpq/performance")
```

## Example

In this example, we will demonstrate how to use the `performance`
package to analyze the performance of different variable types,
including continuous, categorical, and binary variables.

``` r
library(performance)
```

Below we compare continuous measures.

``` r
# Create example data for continuous variables
var1_continuous <- c(1.2, 2.3, 3.1, 4.5, 5.7)
var2_continuous <- c(2.1, 3.5, 4.2, 5.3, 6.1)

# Compute performance measures for continuous variables
result_continuous <- performance(var1_continuous, var2_continuous)

# Print the results
print(result_continuous)
#>     pearson_cor pearson_pval kendall_cor kendall_pval spearman_cor
#> cor    0.991135  0.001000623           1   0.01666667            1
#>     spearman_pval  mae   mse
#> cor    0.01666667 0.88 0.852
```

Below we compare binary measures.

``` r
# Create example data for binary variables
var1_binary <- c(0, 1, 1, 0, 1)
var2_binary <- c(1, 1, 0, 1, 0)

# Compute performance measures for binary variables
result_binary <- performance(var1_binary, var2_binary)

# Print the results
print(result_binary)
#>            N X.tn. X.fp. X.fn. X.tp. acc spec X.sens. X.prec. X.f1. fpr
#> odds ratio 5    tn    fp    fn    tp 0.2    0    sens    prec    f1   1
#>                  fnr fisher_or fisher_pval
#> odds ratio 0.6666667         0         0.4
```

Below we compare categorical measures.

``` r
# Create example data for categorical variables
var1_category <- c("A", "B", "A", "C", "B", "A", "B", "C")
var2_category <- c("A", "C", "A", "C", "B", "A", "B", "C")

# Compute performance measures for categorical variables
result_category <- performance(var1_category, var2_category)
#> Warning in chisq.test(confusion): Chi-squared approximation may be incorrect

# Print the results
print(result_category)
#>             acc chisq_stat chisq_pval
#> X-squared 0.875   11.55556  0.0209812
```

Below we compare a mix of measures.

``` r
# Compute performance measures for mixed variables
result_mixed <- performance(var1_continuous, var2_binary)

# Print the results
print(result_mixed)
#>   mean.0   mean.1 median.0 median.1     sd.0     sd.1 pct25.0 pct25.1 pct75.0
#> t    4.4 2.666667      4.4      2.3 1.838478 1.680278    3.75    1.75    5.05
#>   pct75.1  ttest_t ttest_pval
#> t     3.4 1.068593  0.3926317
```

Please note that this is a simplified example to demonstrate the usage
of the package. In real-world scenarios, you can use the package to
analyze more extensive and complex datasets, gain deeper insights, and
make data-driven decisions based on the performance metrics.
