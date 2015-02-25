#' Nice Colors for Plots
#'
#' This function creates the color pallete that will be used for plots.
#' @param n Number of levels.
#' @keywords colors
#' @export
#' @examples nice.colors()

nice.colors <- function(n) {
        hues <- seq(15, 375, length = n+1)
        hcl(h = hues, l = 65, c = 100)[1:n]
}

#' Plots for Single Numerical Variable
#'
#' This function describes single numerical variables using histograms and boxplots.
#' @param x A numerical variable.
#' @param label A label for the axes.
#' @keywords numerical
#' @export
#' @examples plot.numerical()

numerical.plot <- function(x, label){
        par(cex.lab = 1.5)
        Q1.x <- round(quantile(x, 0.25, na.rm = TRUE), 1)
        Q2.x <- round(median(x, na.rm = TRUE), 1)
        Q3.x <- round(quantile(x, 0.75, na.rm = TRUE), 1)
        max.x <- round(max(x, na.rm = TRUE), 1)
        min.x <- round(min(x, na.rm = TRUE), 1)
        hist(x, ylab = "Frequency", xlab = label, main = "",
             col = nice.colors(2)[1])
        boxplot(x, col = nice.colors(2)[2], ylab = label)
        if (max.x > 0){text(x = 1.35, y = max.x, labels = paste("Max =", max.x))}
        if (Q1.x > 0){text(x = 0.65, y = Q1.x, labels = paste("Q1 =", Q1.x))}
        if (Q2.x > 0){text(x = 1.35, y = Q2.x, labels = paste("Q2 =", Q2.x))}
        if (Q3.x > 0){text(x = 0.65, y = Q3.x, labels = paste("Q3 =", Q3.x))}
        text(x = 1.35, y = min.x, labels = paste("Min =", min.x))
        par(mar = c(5, 4, 4, 2) + 0.1)
}

#' Plots for Single Categorical Variable
#'
#' This function describes single categorical variables using (horizontal or vertical) barplots.
#' @param x A factor variable.
#' @param align Barplot alignment, either "v" (default) for vertical plots or "h" for horizontal plots.
#' @param Left margin for horizontal plots. Default to 4.
#' @param ... Other graphical parameters.
#' @keywords factor
#' @export
#' @examples plot.categorical()

categorical.plot <- function(x, align = "v", left = 4, ...){
        x.table <- table(x)
        max.value <- x.table[which.max(x.table)] + 0.33*x.table[which.max(x.table)]
        range <- c(0, max.value)
        nl <- nlevels(x)
        if (align == "v") {
                par(mar = c(2,2,2,0))
                Plot <- barplot(x.table, col = nice.colors(nl),
                                ylab = "No. Patients", cex.lab = 1.25, ylim = range,
                                ...)
                text(Plot, paste("N =", x.table), y = x.table, pos = 3)
        }
        if (align == "h"){
                par(mar = c(5, left, 4, 2) + 0.1, las = 1)
                Plot <- barplot(x.table, col = nice.colors(nl),
                                horiz = TRUE,
                                xlab = "No. Patients", cex.lab = 1.25, xlim = range,
                                ...)
                text(Plot, paste("N =", x.table), x = x.table, pos = 4)
                par(mar = c(5, 4, 4, 2) + 0.1)
        }

}

#' Plots for 2 Categorical Variables
#'
#' This function describes categorical variables from a n-by-n contingency table using juxtaposed bars and applies the Fisher's exact test.
#' @param x The first factor variable.
#' @param y The second factor variable.
#' @param align Barplot alignment, either "v" (default) for vertical plots or "h" for horizontal plots.
#' @param Left margin for horizontal plots. Default to 4.
#' @param ... Other graphical parameters.
#' @keywords factor
#' @export
#' @examples plot.categorical.group()

categorical.group.plot <- function(x, y, align = "v", left = 4, ...){
        par(mar = c(5, 4, 1, 2) + 0.1)
        xy.table <- table(x, y)
        max.value <- xy.table[which.max(xy.table)] + 0.33*xy.table[which.max(xy.table)]
        range <- c(0, max.value)
        nl <- nlevels(x)
        compare.fisher <- fisher.test(x, y)
        P <- format(compare.fisher$p.value, digits = 2, width = 6)
        if (align == "v"){
                plot <- barplot(xy.table, beside = TRUE, col = nice.colors(nl),
                                ylab = "No. Cases", cex.lab = 1.25, ylim = range,
                                legend.text = TRUE,
                                args.legend = list(x = "topright", bty = "n"),
                                ...)
                text(plot, paste(xy.table), y = xy.table, pos = 3)
                legend("topleft", bty = "n", paste("Fisher's exact test P value =", P))
        }
        if (align == "h"){
                par(mar = c(5, left, 4, 2) + 0.1, las = 1)
                plot <- barplot(xy.table, beside = TRUE, col = nice.colors(nl),
                                xlab = "No. Cases", cex.lab = 1.25, xlim = range,
                                horiz = TRUE,
                                legend.text = TRUE,
                                args.legend = list(x = "topright", bty = "n"),
                                ...)
                text(plot, paste(xy.table), x = xy.table, pos = 4)
                legend("right", bty = "n", paste("Fisher's exact test P value =", P))
        }
        par(mar = c(5, 4, 4, 2) + 0.1)
}

#' Plots for Grouped Numerical Variables
#'
#' This function describes grouped numerical variables using boxplots and applies the Kruskal-Wallis test.
#' @param x The numerical variable.
#' @param y The grouping factor variable.
#' @param ... Other graphical parameters.
#' @keywords factor, numerical
#' @export
#' @examples plot.numerical.group()

numerical.group.plot <- function(x, y, ...){
        par(mar = c(5, 4, 1, 2))
        KW <- kruskal.test(x ~ y)
        max.value <- x[which.max(x)] + 0.1*x[which.max(x)]
        min.value <- x[which.min(x)]
        range <- c(min.value, max.value)
        nl <- nlevels(y)
        KW.plot <- boxplot(x ~ y,
                cex.lab = 1.25, col = nice.colors(nl),
                ylim = range,
                ...)
        legend("topleft", bty = "n",
                paste("Kruskal-Wallis test P value =", format(KW$p.value, digits = 2, width = 6)))
        par(mar = c(5, 4, 4, 2) + 0.1)
}

#' Plots for Survival Curves
#'
#' This function plots survival curves using the Kaplan-Meier method and compares them using the log-rank (Mantel-Cox) test.
#' @param x The grouping (predictor) variable.
#' @param fu Time to event interval. Should be a numeric variable.
#' @param outcome The outcome variable.
#' @param title Plot main title.
#' @param Position of the legend showing the levels of the predictor levels. Default is "topright".
#' @param logrank Position of the legend showing the P value from the log-rank test. Default is "bottomleft".
#' @param ... Other graphical parameters.
#' @keywords factor, numerical, survival
#' @export
#' @examples survival.plot()

survival.plot <- function(x, fu, outcome, title, position = "topright", logrank = "bottomleft", ...){
        require(survival)
        par(mar = c(5, 6, 4, 2) + 0.1)
        outcome <- as.numeric(outcome)
        survival.obj <- Surv(fu, outcome)
        survival.lr <- survdiff(survival.obj ~ x)
        survival.p <- pchisq(survival.lr$chisq, df = 1, lower = FALSE)
        survival.x <- survfit(survival.obj ~ x)
        plot(survival.x, cex = 2, main = title, cex.main = 1.75,
             xlab = "", ylab = "", cex.lab = 1.5,
             col =c(1,2,4,3), mark = c(2,0,5,1), lty = c(2,1,3,6), ...)
        legend(x = position, legend = levels(x), pch = c(2,0,5,1), lty = c(2,1,3,6),
               col = c(1,2,4,3), bty = "n", cex = 1.25)
        legend(x = logrank, bty = "n",
               paste("P value (log-rank test) =", format(survival.p, digits = 2, width = 6)),
               cex = 1.25)
        par(mar = c(5, 4, 4, 2) + 0.1)
}

#' P value for Survival Curves
#'
#' This function estimates the P value from comparing survival curves using the log-rank (Mantel-Cox) test.
#' @param x The grouping (predictor) variable.
#' @param fu Time to event interval. Should be a numeric variable.
#' @param outcome The outcome variable.
#' @keywords factor, numerical, survival
#' @export
#' @examples survival.plot()

survival.p <- function(x, fu, outcome){
        require(survival)
        outcome <- as.numeric(outcome)
        survival.obj <- Surv(fu, outcome)
        survival.lr <- survdiff(survival.obj ~ x)
        survival.p <- pchisq(survival.lr$chisq, df = 1, lower = FALSE)
        format(survival.p, digits = 2, width = 6)
}

#' Forest plots for Logistic Regression Estimates
#'
#' This function creates forest plots using OR and 95\% CI from a logistic regression.
#' @param outcome The outcome variable. Should be a binary factor.
#' @param predictors A list of independent variables. They can include variable labels.
#' @param varlabels The variable labels. Should be a vector of labels.
#' @keywords forest, logistic
#' @export
#' @examples logistic.plot()

logistic.plot <- function(outcome, predictors, varlabels){
        require(ggplot2)
        model <- list()
        OR.center <- list()
        OR.low <- list()
        OR.high <- list()
        for(i in 1:length(predictors)){
                model[[i]] <- glm(outcome ~ predictors[[i]], family = binomial)
                OR.center[[i]] <-format(exp(coef(model[[i]]))[2], digits = 1, nsmall = 1)
                OR.low[[i]] <- format(exp(confint(model[[i]]))[2, 1], digits = 1, nsmall = 1)
                OR.high[[i]] <- format(exp(confint(model[[i]]))[2, 2], digits = 1, nsmall = 1)
        }
        OR.center <- unlist(OR.center)
        OR.low <- unlist(OR.low)
        OR.high <- unlist(OR.high)
        OR.table <- cbind(OR.center, OR.low, OR.high)
        ggplot(data = data.frame(OR.table),
               aes(x = varlabels, y = OR.center, ymin = OR.low, ymax = OR.high)) +
                geom_errorbar(width = 0.25, size = 0.75) +
                geom_hline(y = 1, linetype = "longdash") +
                geom_point(size = 2, shape = 15) +
                coord_flip() +
                xlab("") + ylab("Odds ratios") +
                theme_classic(base_size = 15, base_family = "")
}
