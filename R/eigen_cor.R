#' Correlation of eigentraits across responses
#'
#' @param object object with `eigen` element
#'
#' @return data frame of class `eigen_cor`
#' @export
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#'
eigen_cor <- function(object) {
  # Get ID from individual response
  ID <- dplyr::arrange(
    object$individual$ID,
    ID)
  
  # Get eigen data frames.
  object <- purrr::transpose(object)$eigen
  
  for(i in c("cellmean","signal")) {
    object[[i]] <- 
      as.data.frame(
        tidyr::unite(
          dplyr::left_join(
            ID,
            dplyr::mutate(
              object[[i]],
              ID = rownames(object[[i]])),
            by = "ID"),
          ID, ID, animal))
    
    # Put row names back and remove ID column.
    rownames(object[[i]]) <- object[[i]]$ID
    object[[i]]$ID <- NULL
  }
  
  responses <- names(object)
  nresp <- length(responses)
  out <- NULL
  for(x in seq(1, nresp - 1)) {
    for(y in seq(x + 1, nresp)) {
      # Correlation of responses x and y
      cors <- cor(object[[x]], object[[y]], use = "p")
      # Get row(left) and column(right) module names and ravel.
      left <- c(rownames(cors)[row(cors)])
      right <- c(colnames(cors)[col(cors)])
      # Ravel correlation matrix
      cors <- c(cors)
      
      out <- dplyr::bind_rows(
        out,
        dplyr::tibble(
          response1 = responses[x],
          response2 = responses[y],
          module1 = left,
          module2 = right,
          corr = cors))
    }
  }
      
  class(out) <- c("eigen_cor", class(out))
  # Keep module names in order as attribute.
  attr(out, "modules") <- lapply(object, names)
  out
}
#' GGplot of Eigentrait Correlations
#'
#' @param object object of class `eigen_cor`
#' @param x facet name
#' @param y color name
#' @param main title
#' @param ... additional parameters
#'
#' @return ggplot object
#' @rdname eigen_cor
#' @export
#' @importFrom ggplot2 aes element_blank facet_wrap geom_point ggplot ggtitle
#'             scale_color_manual scale_y_discrete theme ylab
#' @importFrom dplyr filter
#'
ggplot_eigen_cor <- function(object, x, y,
                             main = paste("facet by", y, "with color", x),
                             ...) {
  modules <- attr(object, "modules")
  
  if(!(all(c(x,y) %in% names(modules))))
    return(plot_null(paste(x, "and", y, "must be valid responses")))
  
  # Restrict to two responses
  object <- dplyr::filter(
    object,
    response1 %in% c(x,y) & response2 %in% c(x,y))
  
  if(!(x %in% object$response1)) {
    # Need to switch 1 and 2.
    names(object) <- names(object)[c(2,1,4,3,5)]
  }
  
  # Turn module colors into factors ordered by count
  xes <- c(x,y)
  for(i in 1:2) {
    object[[paste0("module", i)]] <- factor(
      object[[paste0("module", i)]],
      modules[[xes[i]]])
  }

  
  # Module colors are factors ordered by count.
  modcolors <- modules[[x]]
  names(modcolors) <- modcolors
  
  ggplot2::ggplot(object) +
    ggplot2::aes(corr, module1, col = module1) +
    ggplot2::geom_vline(xintercept = 0, col = "gray") +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ module2) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::scale_color_manual(values = modcolors, name = x) +
    ggplot2::ylab(x) +
    ggplot2::xlab("correlation") +
    ggplot2::ggtitle(main) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
}
#' @rdname eigen_cor
#' @export
#' @method autoplot eigen_cor
autoplot.eigen_cor <- function(object, ...)
  ggplot_eigen_cor(object, ...)
