#' Correlation of eigentraits across responses
#'
#' @param object object with `eigen` element
#'
#' @return data frame of class `eigen_cor`
#' @export
#' @importFrom dplyr arrange mutate
#' @importFrom tidyr pivot_longer
#' @importFrom tibble rownames_to_column
#' @importFrom rlang .data
#'
eigen_cor <- function(object) {
  if(is.null(object))
    return(NULL)
  
  # Get ID from first response.
  # Assume these agree across elements of object.
  IDdf <- dplyr::arrange(
    object[[1]]$ID,
    .data$ID)
  
  # Get eigen data frames.
  object <- purrr::transpose(object)$eigen

  if("animal" %in% names(IDdf)) {
    reduced_response <- c("cellmean","signal")
    m <- match(reduced_response, names(object), nomatch = 0)
    if(any(m > 0)) {
      reduced_response <- reduced_response[m > 0]  
      for(i in reduced_response) {
        object[[i]] <- 
          # Unite ID and animal as one column and make a data frame.
          as.data.frame(
            tidyr::unite(
              # Left join ID data frame with response object.
              # This generates rows of object for each animal.
              dplyr::left_join(
                IDdf,
                # Convert rownames to column named "ID".
                tibble::rownames_to_column(
                  object[[i]],
                  var = "ID"),
                by = "ID"),
              ID,
              .data$ID, .data$animal))
        
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
#' @param object,x object of class `eigen_cor`
#' @param facetname facet name
#' @param colorname color name
#' @param main title
#' @param ... additional parameters
#'
#' @return ggplot object
#' @rdname eigen_cor
#' @export
#' @importFrom ggplot2 aes element_blank facet_wrap geom_point ggplot ggtitle
#'             scale_color_manual scale_y_discrete theme ylab
#' @importFrom rlang .data
#'
ggplot_eigen_cor <- function(object, facetname, colorname,
                             main = paste("facet by", facetname,
                                          "with", colorname, "color"),
                             ...) {
  modules <- attr(object, "modules")
  
  object <- subset(object, facetname, colorname, ...)
  
  if(is.null(object))
    return(plot_null("no eigen_cor object"))
  
  # Module colors are factors ordered by count.
  modcolors <- modules[[colorname]]
  names(modcolors) <- modcolors
  
  ggplot2::ggplot(object) +
    ggplot2::aes(.data$corr, .data[[colorname]], col = .data[[colorname]],
                 facet = .data[[facetname]]) +
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
#' @param x of class `eigen_cor`
#' @param facetname facet name
#' @param colorname color name
#' @param facetmodules names of color modules to keep
#' @param colormodules names of color modules to keep
#'
#' @return data frame with colorname, facetname, correlation
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
subset_eigen_cor <- function(x,
                             facetname,
                             colorname,
                             facetmodules = flev,
                             colormodules = clev,
                             ...) {
  if(is.null(x))
    return(NULL)
  if(colorname == facetname)
    return(NULL)
  
  modules <- attr(x, "modules")
  fcnames <- c(facetname, colorname)
  
  if(!(all(fcnames %in% names(modules))))
    return(NULL)
  
  flev <- modules[[facetname]]
  clev <- modules[[colorname]]
  
  # Restrict to the two responses colorname, facetname.
  x <- dplyr::filter(
    x,
    response1 %in% fcnames,
    response2 %in% fcnames)

  if(!(colorname %in% x$response1)) {
    # Need to switch 1 and 2.
    names(x) <- names(x)[c(2,1,4,3,5)]
    x <- x[c(4,3,5)]
  } else
    x <- x[3:5]
  names(x)[1:2] <- fcnames

  # Restrict to  selected modules.
  x <- dplyr::filter(
    x,
    .data[[facetname]] %in% facetmodules,
    .data[[colorname]] %in% colormodules)
  
  # Significant digits for correlations
  x[3] <- signif(x[3], 4)

  x
}
#' @export
#' @rdname eigen_cor
#' @method subset eigen_cor
#'
subset.eigen_cor <- function(x, ...)
  subset_eigen_cor(x, ...)
  
