# Source 
library(shiny)
library(ggplot2)
library(pwr)
library(greekLetters)
library(ggExtra)

# Define support functions
freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("a", length(x1)), rep("b", length(x2)))
  )
  
  ggplot(df, aes(x, colour = g)) +
    geom_freqpoly(binwidth = binwidth, size = 1) +
    coord_cartesian(xlim = xlim)
}

myhist <- function(x1, x2, binwidth = 0.5, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("a", length(x1)), rep("b", length(x2)))
  )
  
  ggplot(df, aes(x, fill = g)) +
    geom_histogram(binwidth = binwidth, size = 1) +
    coord_cartesian(xlim = xlim) +
    theme(legend.position = "none")  
}

myhist_bpmarg <- function(x1, x2, binwidth = 0.5, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("a", length(x1)), rep("b", length(x2)))
  )
  
  p <- ggplot(df, aes(x, fill = g)) +
    geom_histogram(binwidth = binwidth, size = 1) +
    coord_cartesian(xlim = xlim)
  
  ggMarginal(p, type="boxplot")
}

t_test <- function(x1, x2) {
  test <- t.test(x1, x2)
  
  # use sprintf() to format t.test() results compactly
  sprintf(
    "t[%3.3f]: %2.3f\np value: %0.3f\nDiff means: %2.3f\nCI: [%0.2f, %0.2f]",
    test$parameter, test$statistic,
    test$p.value, 
    test$estimate[1]-test$estimate[2],
    test$conf.int[1], test$conf.int[2]
  )
}

t_test_power <- function(n1, n2, d) {
  test_result <- pwr.t2n.test(n1, n2, d)
  
  sprintf("n1: %1d\nn2: %1d\npower: %0.3f\nalpha: %0.3f", 
          test_result$n1, test_result$n2, test_result$power,
          test_result$sig.level)
}

t_test_power_plot <- function(n1, d) {
  test_result <- pwr.t2n.test(n1=n1, d=d, power=0.8, alternative="two.sided")
  plot(test_result)
}
