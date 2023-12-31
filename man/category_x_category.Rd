% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2-backend.R
\name{category_x_category}
\alias{category_x_category}
\title{Performance for Two Categorical Variables}
\usage{
category_x_category(a, b)
}
\arguments{
\item{a}{A character vector representing the first categorical variable.}

\item{b}{A character vector representing the second categorical variable.}
}
\value{
A data frame containing the computed performance measures.
}
\description{
The \code{category_x_category} function calculates the accuracy and performs
a chi-squared test for paired categorical variables.
}
\details{
The function calculates the following performance measures for
 paired categorical variables:
\itemize{
\item Accuracy (ACC) - the proportion of correctly classified instances over
 the total number of instances.
\item Chi-squared statistic (CHISQ_STAT) - the chi-squared test
 statistic value.
\item Chi-squared p-value (CHISQ_PVAL) - the p-value from the
 chi-squared test.
}
}
\examples{
# Example data
a <- c("Red", "Blue", "Green", "Green", "Blue")
b <- c("Red", "Blue", "Blue", "Green", "Red")

# Compute performance measures for paired categorical variables
result <- category_x_category(a, b)
print(result)

}
