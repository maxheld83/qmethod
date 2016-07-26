#   flaglines <- TRUE
#   nstat <- 36
#   results <- job$qalt
#   names <- FALSE
#   points <- TRUE
#   alpha <- 1/3
#   density <- TRUE
#   grid <- FALSE

# set up loadings
# if (flaglines) {
#   # below is duplicate code from qflag
#   thold <- list("0.05" = 1.96/sqrt(nstat),  # this is the accepted truncated variant https://en.wikipedia.org/wiki/1.96,
#                 "0.01" = 2.58/sqrt(nstat))
# }


# if (flaglines) {
#   flagdf <- data.frame(
#     xmin = c(-1, 0-thold$`0.01`, 0+thold$`0.01`, 0+thold$`0.05`),
#     xmax = c(0-thold$`0.01`, 0-thold$`0.05`, 1, 0+thold$`0.01`),
#     ymin = rep(-1, 4),
#     ymax = rep(1, 4),
#     thold = c("0.01", "0.05", "0.01", "0.05"),
#     factor = rep(as.character(v), 4)
#   )
#   flagdf
#   g + annotate(geom = "rect", data = flagdf, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))
#   g
#   g + geom_rect(data = flagdf, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha=thold))
#   s <- ggplot(flagdf) + geom_rect(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, alpha = thold, fill = factor)) + scale_alpha_discrete(range = c(1/2, 1/4))
#   s
#
#
#   xmin = -1, xmax = 0-thold$`0.01`, ymin = -1, ymax = 1, alpha = 1/2)
# }
