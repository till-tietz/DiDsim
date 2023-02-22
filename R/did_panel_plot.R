#' generate a DiD panel plot
#'
#' @param data A df of DiD panel data with unit, group, year and treatment status indicators
#' @param unit character vector specifying the name of the unit variable
#' @param year character vector specifying the name of the year variable
#' @param group character vector specifying the name of the group variable
#' @param treat character vector specifying the name of the treatment variable
#' @return a ggplot object
#' @export
#'
#' @import ggplot2


did_panel_plot <- function(data, unit, year, group, treat){

  data <- data[order(data[,group], decreasing = TRUE),]
  data$unit <- rep(1:max(data[,unit]), each = length(unique(data[,year])))

  data$x_start <- data[[year]]
  data$x_end <- data[[year]] + 1
  data$y_start <- data[[unit]]
  data$y_end <- data[[unit]]
  data[[treat]] <- factor(data[[treat]],
                          labels = c("Under Control","Under Treatment"))


  ggplot(data = data, aes(x = x_start, xend = x_end,
                              y = y_start, yend = y_end,
                              color = factor(.data[[group]]),
                              alpha = .data[[treat]]))+
    geom_segment(key_glyph = "rect")+
    scale_alpha_discrete("Treatment Status",range = c(0.4,1))+
    scale_x_continuous(breaks = c(min(data$x_start):max(data$x_end)))+
    scale_colour_discrete("Group")+
    ylab("Unit")+
    xlab("Time")+
    theme_bw()+
    guides(color = guide_legend(order = 1),
           alpha = guide_legend(order = 2))


}
