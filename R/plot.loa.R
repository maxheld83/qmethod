#' @title Plot loadings
#' @description Plots the loadings of a factor pair.
#' @param x, y Numerical vector with loadings and names as Q-sort names.
#' @param xname, yname Character vector of length one, with factor names.
#' Defaults to \code{NULL}
#' @param names Logical, indicating whether rownames (of Q-sorts) from the loadings table should be printed.
#' Should be set to \code{FALSE} when there are very many Q-sorts, or when there are no meaningful names.
#' @param points Logical, indicating whether a point should be plotted at the precise location of a Q-sort.
#' Defaults to \code{FALSE}.
#' Defaults to \code{TRUE}.
#' @param alpha Numerical vector of length one between 0 and 1, indicating transparency of points.
#' Set to below 1 to avoid overplotting.
#' Defaults to \code{1}, in which case points are not transparent.
#' @param density Logical, indicating whether a 2-d density estimate should be added.
#' Helpful for very many Q-sorts with much overplotting.
#' Defaults to \code{FALSE}.
#' @param grid Logical, indicating whether the \emph{combinations} of plots should be returned as a grid or individually, when \code{quietly = FALSE}.
#' Defaults to \code{TRUE}.
#' @param quietly Logical, indicating whether a summary plot should be printed.
#' Defaults to \code{FALSE}.
#' @details During rotation procedures and final interpretation in Q methodology, researchers need to inspect the loadings of participant Q-sorts (as people-variables) on the extracted factors.
#' Instead of a tabular view, this function creates a simple plot.
#' @author Maximilian Held
