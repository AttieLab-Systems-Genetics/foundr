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
  if(is.null(object))
    return(NULL)
  
  # Get ID from first response.
  # Assume these agree across elements of object.
  ID <- dplyr::arrange(
    object[[1]]$ID,
    ID)
  
  # Get eigen data frames.
  object <- purrr::transpose(object)$eigen
  
  if("animal" %in% names(ID)) {
    reduced_response <- c("cellmean","signal")
    m <- match(reduced_response, names(object), nomatch = 0)
    if(any(m > 0)) {
      reduced_response <- reduced_response[m > 0]  
      for(i in reduced_response) {
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
    }
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
#' @param colorname color name
#' @param facetname facet name
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
ggplot_eigen_cor <- function(object, colorname, facetname,
                             main = paste("facet by", facetname,
                                          "with", colorname, "color"),
                             ...) {
  modules <- attr(object, "modules")
  
  object <- subset(object, colorname, facetname)
  
  if(is.null(object))
    return(plot_null("no eigen_cor object"))
  
  # Module colors are factors ordered by count.
  modcolors <- modules[[colorname]]
  names(modcolors) <- modcolors
  
  ggplot2::ggplot(object) +
    ggplot2::aes(corr, .data[[colorname]], col = .data[[colorname]]) +
    ggplot2::geom_vline(xintercept = 0, col = "gray") +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ .data[[facetname]]) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::scale_color_manual(values = modcolors, name = colorname) +
    ggplot2::xlab("correlation") +
    ggplot2::ylab(colorname) +
    ggplot2::ggtitle(main) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
}
#' @rdname eigen_cor
#' @export
#' @method autoplot eigen_cor
autoplot.eigen_cor <- function(object, ...)
  ggplot_eigen_cor(object, ...)

#' Subset of eigen_cor object
#'
#' @param object of class `eigen_cor`
#' @param colorname color name
#' @param facetname facet name
#'
#' @return data frame with colorname, facetname, correlation
#' @export
#' @importFrom dplyr filter
#'
subset_eigen_cor <- function(object, colorname, facetname, ...) {
  if(is.null(object))
    return(NULL)
  if(colorname == facetname)
    return(NULL)
  
  modules <- attr(object, "modules")
  xes <- c(colorname, facetname)
  
  if(!(all(xes %in% names(modules))))
    return(NULL)
  
  # Restrict to the two responses colorname, facetname
  object <- dplyr::filter(
    object,
    response1 %in% xes & response2 %in% xes)
  
  if(!(colorname %in% object$response1)) {
    # Need to switch 1 and 2.
    names(object) <- names(object)[c(2,1,4,3,5)]
    object <- object[c(4,3,5)]
  } else
    object <- object[3:5]
  names(object)[1:2] <- xes
  
  # Significant digits for correlations
  object[3] <- signif(object[3], 4)
  
  # Turn module colors into factors ordered by count
  for(i in xes) {
    object[[i]] <- factor(
      object[[i]],
      modules[[i]])
  }
  object
}
#' @export
#' @rdname eigen_cor
#' @method subset eigen_cor
#'
subset.eigen_cor <- function(x, ...)
  subset_eigen_cor(x, ...)
  
