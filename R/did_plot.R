#' generate a DiD plot
#'
#' @param data A df of DiD panel data with year, outcome and group indicators
#' @param year character vector specifying the name of the year variable
#' @param y character vector specifying the name of the outcome variable
#' @param group character vector specifying the name of the group variable
#' @return a ggplot object
#' @export
#'
#' @import dplyr
#' @import magrittr
#' @import ggplot2

did_plot <- function(data, year, y, group){

  d <- data%>%
    dplyr::group_by_at(c(year,group))%>%
    dplyr::summarise_at(y, mean)%>%
    `colnames<-`(c("year","group","y"))

  ggplot(data = d, aes(x = year, y = y,
                       color = factor(group), group = factor(group)))+
    geom_point()+
    geom_line()+
    scale_x_continuous(breaks = sort(unique(d[["year"]])))+
    xlab("Time")+
    guides(color = guide_legend(title="Group"))+
    theme_bw()
}




