#' Description of a Single Numerical Variable
#'
#' This function describes a single numerical variable using a table with mean, SD, median, IQR, min, and max values.
#' @param x A numerical variable
#' @keywords numerical
#' @export
#' @examples descriptive.numerical()

descriptive.numerical <- function(x){
        require(knitr)
        mean.x <- mean(x, na.rm = TRUE)
        sd.x <- sd(x, na.rm = TRUE)
        median.x <- median(x, na.rm = TRUE)
        iqr.x <- IQR(x, na.rm = TRUE)
        min.x <- min(x, na.rm = TRUE)
        max.x <- max(x, na.rm = TRUE)
        descriptive.x <- round(c(mean.x, sd.x, median.x, iqr.x, min.x, max.x), digits = 1)
        descriptive.names <- c("Mean", "Standard Deviation", "Median", "Interquartile Range",
                               "Mininum", "Maximum")
        final.table <- cbind("Statistics" = descriptive.names, "Values" = descriptive.x)
        kable(final.table, align = c("l", "c"))
}

#' Description of a Single Categorical Variable
#'
#' This function describes a single categorical variable using a table of absolute and relative frequencies.
#' @param x A categorical (factor) variable
#' @keywords factor
#' @export
#' @examples descriptive.categorical()

descriptive.categorical <- function(x){
        require(knitr)
        table.x <- table(x)
        percentage.x <- 100*prop.table(table.x)
        final.table <- cbind("No. Cases" = format(table.x, digits = 2),
                             "%" = format(percentage.x, digits = 2))
        kable(final.table, align = c("c", "c"))
}

#' Comparison of a Grouped Numerical Variable
#'
#' This function describes a grouped numerical variable using a table with mean, SD, median, IQR, min, and max values by groups.
#' @param x A numerical variable
#' @param y The grouping (factor) variable
#' @keywords factor, numerical
#' @export
#' @examples descriptive.numerical.group()

descriptive.numerical.group <- function(x, y){
        require(knitr)
        mean.x <- tapply(x, INDEX = y, FUN = mean, na.rm = TRUE)
        sd.x <- tapply(x, INDEX = y, FUN = sd, na.rm = TRUE)
        median.x <- tapply(x, INDEX = y, FUN = median, na.rm = TRUE)
        iqr.x <- tapply(x, INDEX = y, FUN = IQR, na.rm = TRUE)
        min.x <- tapply(x, INDEX = y, FUN = min, na.rm = TRUE)
        max.x <- tapply(x, INDEX = y, FUN = max, na.rm = TRUE)
        final.table <- round(rbind("Mean" = mean.x, "Standard Deviation" = sd.x,
                                     "Median" = median.x, "Interquartile Range" = iqr.x,
                                     "Minimum" = min.x, "Maximum" = max.x), digits = 1)
        kable(final.table, align = c("c"))
}

#' Comparison of Categorical Variables in a n-by-2 Contingency Table
#'
#' This function compares 2 categorical variables in a n-by-2 contingency table.
#' @param x A categorical (factor) variable
#' @param y A 2-level categorical (factor) variable
#' @keywords factor, numerical
#' @export
#' @examples descriptive.categorical.group()

descriptive.categorical.group <- function(x, y){
        require(knitr)
        table.compare <- table(x, y)
        table.proportion.1 <- round(prop.table(table.compare[, 1])*100, 1)
        table.proportion.2 <- round(prop.table(table.compare[, 2])*100, 1)
        table.final <- cbind(table.compare[, 1], table.proportion.1,
                             table.compare[, 2], table.proportion.2)
        matrix.final <- matrix(table.final, ncol = nlevels(y)*2,
                               dimnames = list(c(levels(x)),
                                               c(levels(y)[1], "%", levels(y)[2], "%")))
        kable(matrix.final, align = c("c"))
}

#' Logistic Regression Table
#'
#' This function creates a logistic regression table including OR, 95% CI, and P values (unadjusted and adjusted using Hommel's correction).
#' @param outcome The outcome variable. Should be a binary factor.
#' @param predictors A list of independent variables. They can include variable labels.
#' @param varlabels The variable labels. Should be a vector of labels.
#' @keywords logistic, regression
#' @export
#' @examples logistic.table()

logistic.table <- function(outcome, predictors, varlabels){
        require(knitr)
        model <- list()
        OR.center <- list()
        OR.low <- list()
        OR.high <- list()
        OR.p <- list()
        for(i in 1:length(predictors)){
                model[[i]] <- glm(outcome ~ predictors[[i]], family = binomial)
                OR.center[[i]] <-format(exp(coef(model[[i]]))[2], digits = 2, nsmall = 2)
                OR.low[[i]] <- format(exp(confint(model[[i]]))[2, 1], digits = 2, nsmall = 2)
                OR.high[[i]] <- format(exp(confint(model[[i]]))[2, 2], digits = 2, nsmall = 2)
                OR.p[[i]] <- format(summary(model[[i]])$coef[2, 4], digits = 2, width  = 6)
        }
        OR.center <- unlist(OR.center)
        OR.low <- unlist(OR.low)
        OR.high <- unlist(OR.high)
        OR.p <- unlist(OR.p)
        OR.p.adj <- p.adjust(OR.p, method = c("hommel"))
        OR.p.adj <- format(OR.p.adj, digits = 2, nsmall = 2)
        OR.table <- cbind("Variables" = varlabels,
                          "OR" = OR.center,
                          "Lower 95% CI" = OR.low,
                          "Higher 95% CI" = OR.high,
                          "Raw P value" = OR.p,
                          "Adjusted P value" = OR.p.adj
        )
        kable(OR.table, row.names = FALSE, align = c("l", "c", "c", "c", "c", "c"))
}

#' Cox's Regression Table
#'
#' This function creates a Cox's proportional hazards regression table including OR, 95% CI, and P values (unadjusted and adjusted using Hommel's correction).
#' @param outcome The outcome variable. Should be a binary factor.
#' @param fu Time to event interval. Should be a numerical variable.
#' @param predictors A list of independent variables. They can include variable labels.
#' @param varlabels The variable labels. Should be a vector of labels.
#' @keywords cox, regression
#' @export
#' @examples cox.table()

cox.table <- function(outcome, fu, predictors, varlabels){
        require(survival)
        require(knitr)
        outcome <- as.numeric(outcome)
        survival.obj <- Surv(fu, outcome)
        model <- list()
        HR.center <- list()
        HR.low <- list()
        HR.high <- list()
        HR.p <- list()
        for(i in 1:length(predictors)){
                model[[i]] <- coxph(survival.obj ~ predictors[[i]])
                HR.center[[i]] <-format(summary(model[[i]])$conf.int[1], digits = 2, nsmall = 2)
                HR.low[[i]] <- format(summary(model[[i]])$conf.int[2], digits = 2, nsmall = 2)
                HR.high[[i]] <- format(summary(model[[i]])$conf.int[3], digits = 2, nsmall = 2)
                HR.p[[i]] <- format(summary(model[[i]])$logtest[3], digits = 2, width  = 6)
        }
        HR.center <- unlist(HR.center)
        HR.low <- unlist(HR.low)
        HR.high <- unlist(HR.high)
        HR.p <- unlist(HR.p)
        HR.p.adj <- p.adjust(HR.p, method = c("hommel"))
        HR.p.adj <- format(HR.p.adj, digits = 2, nsmall = 2)
        HR.table <- cbind("Variables" = varlabels,
                          "HR" = HR.center,
                          "Lower 95% CI" = HR.low,
                          "Higher 95% CI" = HR.high,
                          "Raw P value" = HR.p,
                          "Adjusted P value" = HR.p.adj
        )
        kable(HR.table, row.names = FALSE, align = c("l", "c", "c", "c", "c", "c"))
}
