#' Performance for Two Continuous Variables
#'
#' The \code{continuous_x_continuous} function calculates various performance
#'  measures for paired continuous variables.
#'
#' @param a A numeric vector representing the first continuous variable.
#' @param b A numeric vector representing the second continuous variable.
#'
#' @details The function performs the following calculations:
#' \itemize{
#' \item Pearson correlation coefficient and p-value.
#' \item Kendall's tau correlation coefficient and p-value.
#' \item Spearman's rank correlation coefficient and p-value.
#' \item Mean Absolute Error (MAE) between \code{a} and \code{b}.
#' \item Mean Squared Error (MSE) between \code{a} and \code{b}.
#' }
#'
#' @return A data frame containing the computed performance measures.
#'
#' @examples
#' # Example data
#' a <- c(1.2, 2.3, 3.1, 4.5, 5.7)
#' b <- c(1.0, 2.7, 3.0, 4.2, 5.8)
#'
#' # Compute performance measures for paired continuous variables
#' result <- continuous_x_continuous(a, b)
#' print(result)
#'
#' @export
continuous_x_continuous <- function(a, b) {
  # run some correlations
  cp <- cor.test(a, b, method = "pearson")
  ck <- cor.test(a, b, method = "kendall")
  cs <- cor.test(a, b, method = "spearman")

  data.frame(
    "pearson_cor" = cp$estimate,
    "pearson_pval" = cp$p.value,
    "kendall_cor" = ck$estimate,
    "kendall_pval" = ck$p.value,
    "spearman_cor" = ck$estimate,
    "spearman_pval" = ck$p.value,
    "mae" = mean(abs(a - b)),
    "mse" = mean((a - b) ^ 2)
  )
}

#' Performance for Two Binary Variables
#'
#' The \code{binary_x_binary} function calculates various performance measures
#'  for paired binary variables.
#'
#' @param a A logical vector representing the first binary variable.
#' @param b A logical vector representing the second binary variable.
#'
#' @details The function calculates the following performance measures for
#'  binary classification tasks:
#' \itemize{
#' \item True Negative (TN), False Positive (FP), False Negative (FN), and
#'  True Positive (TP) counts.
#' \item Accuracy (ACC) - the proportion of correctly classified instances over
#'  the total number of instances.
#' \item Specificity (SPEC) - the proportion of true negatives over the total
#'  number of actual negatives.
#' \item Sensitivity (SENS) - the proportion of true positives over the total
#'  number of actual positives (also known as Recall or True Positive Rate).
#' \item Precision (PREC) - the proportion of true positives over the total
#'  number of predicted positives (also known as Positive Predictive Value).
#' \item F1-score (F1) - the harmonic mean of Precision and Sensitivity, used
#'  for imbalanced datasets.
#' \item False Positive Rate (FPR) - the proportion of false positives over
#'  the total number of actual negatives.
#' \item False Negative Rate (FNR) - the proportion of false negatives over
#'  the total number of actual positives.
#' \item Fisher's exact test for comparing the two binary variables,
#'  providing odds ratio and p-value.
#' }
#'
#' @return A data frame containing the computed performance measures.
#'
#' @examples
#' # Example data
#' a <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
#' b <- c(FALSE, FALSE, TRUE, TRUE, TRUE)
#'
#' # Compute performance measures for paired binary variables
#' result <- binary_x_binary(a, b)
#' print(result)
#'
#' @export
binary_x_binary <- function(a, b) {
  # turn 0 and 1 to FALSE and TRUE
  a <- as.logical(a)
  b <- as.logical(b)

  # ensure confusion matrix gets filled with 0s
  confusion <-
    table(factor(a, levels = c("FALSE", "TRUE")),
          factor(b, levels = c("FALSE", "TRUE")))
  tn <- confusion[1, 1]
  fp <- confusion[2, 1]
  fn <- confusion[1, 2]
  tp <- confusion[2, 2]

  # pre-compute precision and recall for f1
  sens <- tp / (tp + fn)
  prec <- tp / (tp + fp)
  f1 <- 2 * (prec * sens) / (prec + sens)

  # run fisher exact test
  fisher <- fisher.test(confusion)

  data.frame(
    "N" = tn + fp + fn + tp,
    "tn" = tn,
    "fp" = fp,
    "fn" = fn,
    "tp" = tp,
    "acc" = (tp + tn) / (tp + tn + fp + fn),
    "spec" = tn / (fp + tn),
    "sens" = sens,
    "prec" = prec,
    "f1" = f1,
    "fpr" = fp / (fp + tn),
    "fnr" = fn / (fn + tp),
    "fisher_or" = fisher$estimate,
    "fisher_pval" = fisher$p.value
  )
}

#' Performance for Two Categorical Variables
#'
#' The \code{category_x_category} function calculates the accuracy and performs
#' a chi-squared test for paired categorical variables.
#'
#' @param a A character vector representing the first categorical variable.
#' @param b A character vector representing the second categorical variable.
#'
#' @details The function calculates the following performance measures for
#'  paired categorical variables:
#' \itemize{
#' \item Accuracy (ACC) - the proportion of correctly classified instances over
#'  the total number of instances.
#' \item Chi-squared statistic (CHISQ_STAT) - the chi-squared test
#'  statistic value.
#' \item Chi-squared p-value (CHISQ_PVAL) - the p-value from the
#'  chi-squared test.
#' }
#'
#' @return A data frame containing the computed performance measures.
#'
#' @examples
#' # Example data
#' a <- c("Red", "Blue", "Green", "Green", "Blue")
#' b <- c("Red", "Blue", "Blue", "Green", "Red")
#'
#' # Compute performance measures for paired categorical variables
#' result <- category_x_category(a, b)
#' print(result)
#'
#' @export
category_x_category <- function(a, b) {
  # ensure confusion matrix gets filled with 0s
  all_levels <- union(a, b)
  confusion <-
    table(factor(a, levels = all_levels),
          factor(b, levels = all_levels))

  # run chi-squared test
  chisq <- chisq.test(confusion)

  data.frame(
    "acc" = sum(diag(confusion)) / sum(confusion),
    "chisq_stat" = chisq$statistic,
    "chisq_pval" = chisq$p.value
  )
}

#' continuous_x_category Function
#'
#' The \code{continuous_x_category} function computes summary statistics and
#'  performs an ANOVA test for paired continuous and categorical variables.
#'
#' @param a A numeric vector representing the continuous variable.
#' @param b A character vector representing the categorical variable.
#'
#' @details The function calculates the following summary statistics for each
#'  category in the \code{b} variable:
#' \itemize{
#' \item Mean - the mean value of the continuous variable within each category.
#' \item Median - the median value of the continuous variable within each category.
#' \item Standard Deviation (SD) - the standard deviation of the continuous
#'  variable within each category.
#' \item 25th Percentile (Pct25) - the 25th percentile value of the continuous
#'  variable within each category.
#' \item 75th Percentile (Pct75) - the 75th percentile value of the continuous
#'  variable within each category.
#' }
#'
#' The function also performs an ANOVA test to compare the means of the
#'  continuous variable across different categories.
#'  The ANOVA test results include:
#' \itemize{
#' \item \code{anova_f}: The F-statistic value from the ANOVA test.
#' \item \code{anova_pval}: The p-value from the ANOVA test.
#' }
#'
#' @return A data frame containing the computed performance measures.
#'
#' @examples
#' # Example data
#' a <- c(10, 12, 15, 8, 7)
#' b <- c("A", "B", "B", "A", "B")
#'
#' # Compute summary statistics and perform ANOVA test for paired continuous and categorical variables
#' result <- continuous_x_category(a, b)
#' print(result)
#'
#' @export
continuous_x_category <- function(a, b) {
  widen <- function(df) {
    res <- t(df[, 2])
    colnames(res) <- df[, 1]
    res
  }

  # run summary stats
  means = aggregate(a ~ b, FUN = mean)
  medians = aggregate(a ~ b, FUN = median)
  sds = aggregate(a ~ b, FUN = sd)
  pct25s = aggregate(a ~ b, FUN = quantile, probs = .25)
  pct75s = aggregate(a ~ b, FUN = quantile, probs = .75)

  # run anova test
  m <- lm(a ~ b)
  av <- aov(m)

  data.frame(
    "mean" = widen(means),
    "median" = widen(medians),
    "sd" = widen(sds),
    "pct25" = widen(pct25s),
    "pct75" = widen(pct75s),
    "anova_f" = summary(av)[[1]]$`F value`[1],
    "anova_pval" = summary(av)[[1]]$`Pr(>F)`[1]
  )
}

#' continuous_x_binary Function
#'
#' The \code{continuous_x_binary} function computes summary statistics and
#'  performs a t-test for paired continuous and binary variables.
#'
#' @param a A numeric vector representing the continuous variable.
#' @param b A logical vector representing the binary variable.
#'
#' @details The function calculates the following summary statistics for each
#'  binary category in the \code{b} variable:
#' \itemize{
#' \item Mean - the mean value of the continuous variable within each category.
#' \item Median - the median value of the continuous variable within each category.
#' \item Standard Deviation (SD) - the standard deviation of the continuous
#'  variable within each category.
#' \item 25th Percentile (Pct25) - the 25th percentile value of the continuous
#'  variable within each category.
#' \item 75th Percentile (Pct75) - the 75th percentile value of the continuous
#'  variable within each category.
#' }
#'
#' The function also performs a t-test to compare the means of the
#'  continuous variable between the two categories.
#'  The t-test results include:
#' \itemize{
#' \item \code{ttest_t}: The t-statistic value from the t-test.
#' \item \code{ttest_pval}: The p-value from the t-test.
#' }
#'
#' @return A data frame containing the computed performance measures.
#'
#' @examples
#' # Example data
#' a <- c(10, 12, 15, 8, 7)
#' b <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
#'
#' # Compute summary statistics and perform t-test for paired continuous and binary variables
#' result <- continuous_x_binary(a, b)
#' print(result)
#'
#' @export
continuous_x_binary <- function(a, b) {
  widen <- function(df) {
    res <- t(df[, 2])
    colnames(res) <- df[, 1]
    res
  }

  # run summary stats
  means = aggregate(a ~ b, FUN = mean)
  medians = aggregate(a ~ b, FUN = median)
  sds = aggregate(a ~ b, FUN = sd)
  pct25s = aggregate(a ~ b, FUN = quantile, probs = .25)
  pct75s = aggregate(a ~ b, FUN = quantile, probs = .75)

  # run t-test
  t_test <- t.test(a ~ b)

  data.frame(
    "mean" = widen(means),
    "median" = widen(medians),
    "sd" = widen(sds),
    "pct25" = widen(pct25s),
    "pct75" = widen(pct75s),
    "ttest_t" = t_test$statistic,
    "ttest_pval" = t_test$p.value
  )
}
