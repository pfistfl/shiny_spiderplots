library(ggplot2)
library(gridExtra)
library(grid)

# Add a shared legend for  gridExtra.
# Obtained from
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1,
  position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  
  legend <- lapply(seq_len(length(plots)), function(x) {
    g <- ggplotGrob(plots[[x]] + theme(legend.position = position))$grobs
    leg <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    return(leg)
  })
  
  lheight <- sum(legend[[1]]$height)
  lwidth <- sum(legend[[1]]$width)
  legend <- do.call(what = arrangeGrob, c(legend, nrow = 2, ncol = 2))

  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            nrow = 2,
                                            heights = unit.c(unit(1, "npc")- lheight -lheight ,
                                                             lheight + lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}


# dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
# p1 <- qplot(carat, price, data = dsamp, colour = clarity)
# p2 <- qplot(cut, price, data = dsamp, colour = clarity)
# p3 <- qplot(color, price, data = dsamp, colour = clarity)
# p4 <- qplot(depth, price, data = dsamp, colour = clarity)
# grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 4, nrow = 1)
p = grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 2, nrow = 2)
ggsave(p, filename = "Desktop/out.pdf", device = "pdf", dpi = 500, scale =1.15)
