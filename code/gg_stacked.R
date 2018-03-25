#' A ggplot2 wrapper for visualizing multilevel data structures.
#'
#' @param data A data.frame
#' @param x A variable to display (2-10) categories
#' @param group A group indicator
#' @param legend.title The legend Title
#' @param labels The Labels
#' @param colours The Colours
#' @return A ggplot2 object.
#'
#' @author Simon Roth
#'
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
#' @export
gg_stacked <- function(data, x, group, legend.title = NULL, labels = NULL, colour = NULL){
  
  dat <- ess_work %>%
    dplyr::rename(group = country, x = imm_div) %>%
    dplyr::select(group, x)
  
  dat <- data.frame(x = data[[x]], group = data[[group]])
  
  gg1 <- dat %>%
    dplyr::group_by(group, x) %>%
    dplyr::filter(!is.na(x)) %>%
    dplyr::mutate(nrow = n()) %>%
    dplyr::tally() %>%
    dplyr::mutate(all = sum(n, na.rm = T)) %>%
    dplyr::mutate(perc = round(n/all*100, 2)) %>%
    dplyr::mutate(text = ifelse(perc > 5, paste0(perc,"%"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(x, desc(perc)) %>%
    dplyr::mutate(group = factor(group, levels = unique(group))) %>%
    ggplot2::ggplot(aes(x = group, y = perc, fill = as.factor(x))) +
    ggplot2::geom_bar(stat="identity", position = "stack", alpha = .7) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(aes(label=text), position=position_stack(vjust=0.5)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "top", text = element_text(size= 8)) +
    ggplot2::labs(x = NULL, y = NULL) +
    viridis::scale_fill_viridis(discrete = T)
  
  if(!is.null(colour) &
     !is.null(labels) &
     !is.null(legend.title)) gg1 <- gg1 + ggplot2::scale_fill_manual(legend.title, values = colour, labels = labels)
  
  return(gg1)
}
