#' Prepare biplot signal data
#'
#' @param object data frame with appropriate columns
#' @param datatraits `dataset: trait` names to include
#' @param orders column names used for ordering and plot sizes
#'
#' @return data frame
#' 
#' @importFrom dplyr across any_of distinct filter mutate select
#' @importFrom tidyr pivot_wider unite
#' @export
#' @rdname biplot_pca
#'
biplot_signal <- function(object,
                        datatraits = unique(object$datatrait),
                        orders = c("module","kME","p.value","size")) {
  
  if(is.null(object)) return(NULL)

  factors = c("strain", "animal", "sex", "condition")
  factors <- factors[factors %in% names(object)]
  orders <- orders[orders %in% names(object)]
  modulename <- attr(object, "modulename")
  
  # Unite `dataset` and `trait` as `datatrait`.
  object <- tidyr::unite(
    object,
    datatrait,
    .data$dataset, .data$trait,
    sep = ": ", remove = FALSE)

  # Get columns with orders.  
  ordout <- dplyr::distinct(
    dplyr::select(
      object,
      dplyr::any_of(c("datatrait", orders))))
  
  # Revise ordout so that large value is more important.
  if("p.value" %in% orders)
    ordout$p.value <- -log10(ordout$p.value)
  if("module" %in% orders)
    ordout$module <- 1 + max(ordout$module) - ordout$module
  
  out <- dplyr::mutate(
    # Pivot `traits` to columns, leaving `factors` in place. 
    tidyr::pivot_wider(
      dplyr::filter(
        object,
        datatrait %in% datatraits),
      id_cols = factors,
      names_from = "trait", values_from = "value"),
    # Fill in missing data by `trait` with mean.
    dplyr::across(where(is.numeric), function(x) {
      m <- mean(x, na.rm = TRUE)
      x[is.na(x)] <- m
      x
    }))
  
  list(data = out, factors = factors, orders = ordout, modulename = modulename)
}

#' Prepare biplot stats
#'
#' @param object data frame with appropriate columns
#' @param datatraits `dataset: trait` names to include
#' @param orders column names used for ordering and plot sizes
#'
#' @return data frame
#' 
#' @importFrom dplyr across any_of distinct filter mutate select
#' @importFrom tidyr pivot_wider unite
#' @export
#' @rdname biplot_pca
#'
biplot_stat <- function(object,
                        datatraits = unique(object$datatrait),
                        orders = terms) {
  
  if(is.null(object)) return(NULL)
  
  # Colors for terms
  # selectInput on terms as with strains
  # check if SD standardized (so rawSD = 1)
  # check about transpose and all that.
  
  
  terms <- term_stats(object, signal = FALSE, drop_noise = FALSE)
  
  orders <- match.arg(orders)
  
  modulename <- attr(object, "modulename")

  # Unite `dataset` and `trait` as `datatrait`.
  object <- tidyr::unite(
    object,
    datatrait,
    .data$dataset, .data$trait,
    sep = ": ", remove = FALSE)
  
  # Get columns with orders. 
  # Not quite right yet. Need to filter on 
  ordout <- dplyr::distinct(
    dplyr::select(
      dplyr::filter(
        object,
        .data$term == orders),
      dplyr::any_of(c("datatrait", "p.value"))))

  out <- dplyr::mutate(
    # Pivot `traits` to columns, leaving `factors` in place. 
    tidyr::pivot_wider(
      dplyr::filter(
        dplyr::select(
          object,
          dplyr::any_of(c("datatrait", "term", "SD"))),
        term %in% terms),
      names_from = "term", values_from = "SD"),
    # Fill in missing data by `trait` with mean.
    dplyr::across(where(is.numeric), function(x) {
      m <- mean(x, na.rm = TRUE)
      x[is.na(x)] <- m
      x
    }))
  
  list(data = out, orders = ordout, modulename = modulename)
}

#' Get PCA components from biplot data
#'
#' @param bip object from `biplot_signal`
#' @param transpose transpose for `princomp` if `TRUE` 
#'
#' @return data frame of PCA components
#' 
#' @importFrom ordr as_tbl_ord confer_inertia mutate_cols mutate_rows
#' @importFrom stats princomp
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @export
#'
biplot_pca <- function(bip, size = c("module","kME","p.value","size"),
                       strain = "NONE", threshold) {
  
  if(is.null(bip)) return(NULL)
  
  size <- match.arg(size)
  factors <- bip$factors
  # Size values by orders; set up if `NULL`.
  orders <- bip$orders
  if(is.null(orders)) {
    orders <- list(1)
    names(orders) <- size
  }
  modulename <- bip$modulename
  bip <- as.data.frame(bip$data)
  
  # Kludge for biplot_stat for now.
  if(is.null(factors)) {
    rownames(bip) <- bip$datatrait
    bip <- bip[, -match("datatrait", colnames(bip))]
  } else {
    rownames(bip) <- bip$strain
    bip <- bip[,-match(factors, colnames(bip))]
    bip <- t(bip)
  }
  # Remove rows with empty data.
  keep <- apply(bip, 1, function(x) !any(is.na(x)))
  bip <- bip[keep,]
  orders <- orders[keep,]
  
  # The `princomp` routine cannot handle wide tables.
  if(ncol(bip) > nrow(bip)) return(NULL)
  
  strain_color <- rep("NO", nrow(bip))
  if(strain != "NONE" & strain %in% colnames(bip)) {
    strain_color[bip[,strain] >= threshold["SD"]] <- "UP"
    strain_color[bip[,strain] <= -threshold["SD"]] <- "DOWN"
  }
  if(!is.null(modulename))
    strain_color[modulename == rownames(bip)] <- "EIGEN"
  
  stroke <- c(NO = 1, UP = 2, DOWN = 2, EIGEN = 3)

  ordr::mutate_cols(
    ordr::mutate_rows(
      # Redistribute inertia between rows and columns in ordination.
      ordr::confer_inertia(
        ordr::as_tbl_ord(
          # Principal components.
          stats::princomp(bip, cor = TRUE)),
        1),
      datatrait = rownames(bip),
      size = orders[[size]],
      color = strain_color,
      stroke = stroke[strain_color]),
    strain = colnames(bip))
}

#' Biplot using ggplot2 via ordr package
#'
#' @param bip_pca object of class bip_pca
#' @param axes axes to plot
#' @param scale.factor scale factor for arrows vs points
#'
#' @return object of class ggplot2
#' 
#' @importFrom ggplot2 aes
#' @importFrom ordr geom_cols_vector geom_cols_text_radiate geom_rows_point ggbiplot
#' @importFrom RColorBrewer brewer.pal
#' @export
#' @rdname biplot_pca
#'
biggplot <- function(bip_pca, axes = 1:2, scale.factor = 2) {
  
  if(is.null(bip_pca))
    return(plot_null("Fewer rows than columns"))
  
  CB_colors <- RColorBrewer::brewer.pal(n = 4, name = "Dark2")
  names(CB_colors) <- c("DOWN", "UP", "NO", "EIGEN")
  
  termnames <- rownames(bip_pca$loadings)
  if(all(termnames %in% names(foundr::CCcolors))) {
    term_colors <- foundr::CCcolors
  } else {
    term_colors <- RColorBrewer::brewer.pal(n = max(3, length(termnames)),
                                            name = "Dark2")
    names(term_colors)[seq_along(termnames)] <- termnames
  }
  
  if(is.numeric(axes))
    axes <- paste0("Comp.", axes)
  
  # Percent variation explained by axis.
  pct <- bip_pca$sdev^2
  pct <- round(100 * pct^2 / sum(pct^2))
  names(pct) <- colnames(bip_pca$loadings)
  
  ordr::ggbiplot(bip_pca, 
                 ggplot2::aes(x = .data[[axes[1]]],
                              y = .data[[axes[2]]]),
                 sec.axes = "cols", scale.factor = scale.factor) +
    ordr::geom_rows_point(ggplot2::aes(size = .data$size, color = .data$color,
                                       stroke = .data$stroke,
                                       shape = .data$color)) +
    ordr::geom_cols_vector(ggplot2::aes(color = .data$strain),
                           size = 2) +
    ordr::geom_cols_text_radiate(
      ggplot2::aes(label = .data$strain), col = "black") +
    ggplot2::scale_shape_manual(
      values = c(DOWN = 1, NO = 1, UP = 1, EIGEN = 5)) +
    ggplot2::scale_color_manual(
      values = c(term_colors, CB_colors)) +
    ggplot2::labs(x = paste0(axes[1], " (", pct[axes[1]], "%)"),
                  y = paste0(axes[2], " (", pct[axes[2]], "%)"))
}

#' BiPlot for Evidence vs Spread
#'
#' @param object data frame
#' @param ordername name of order column
#' @param xlab x label
#' @param threshold vector of threshold values
#' @param strain strain name to highlight in biplot
#' @param axes axes to plot
#' @param ... additional paramaters ignored
#'
#' @return gg plot object
#' @export
#' @rdname biplot_pca
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggtitle
#' @importFrom rlang .data
#'
biplot_evidence <- function(object, ordername, xlab,
                            threshold, strain = "NONE",
                            axes = 1:2, ...) {
  if(inherits(object, "conditionContrasts")) {
    condition_biplot(object, ordername, xlab, threshold, strain, axes, ...)
  } else {
    variation_biplot(object, ordername, xlab, threshold, strain, axes, ...)
  }
}
condition_biplot <- function(object, ordername, xlab,
                             threshold, strain = "NONE",
                             axes = 1:2, ...) {
  # Filter on vertical threshold
  if(ordername %in% c("p.value", "module")) {
    object <- dplyr::filter(object, .data[[ordername]] <= threshold[ordername])
  } else {
    object <- dplyr::filter(object, .data[[ordername]] >= threshold[ordername])
  }
  
  bip_pca <- biplot_pca(biplot_signal(object), size = ordername,
                        strain = strain, threshold)
  p <- biggplot(bip_pca, axes, scale.factor = 4)
  p <- theme_template(p, legend_position = "none")
  if(strain != "NONE")
    xlab <- paste(xlab, "colored by", strain)
  p + ggplot2::ggtitle(xlab)
}
variation_biplot <- function(object, ordername, xlab,
                             threshold, strain = "NONE",
                             axes = 1:2,
                             terms = term_stats(object, signal = FALSE, drop_noise = FALSE),
                             ...) {
  # *** Fix dataset issue and SD vs p.value.
  # *** Decide about size and strain and threshold.
  
  # Restrict to desired terms.
  object <- dplyr::filter(
    object,
    term %in% terms)
  
  bip_pca <- biplot_pca(biplot_stat(object,
                                    orders = terms[1]),
                        size = ordername,
                        strain = strain, threshold)
  p <- biggplot(bip_pca, axes, scale.factor = 4)
  p <- theme_template(p, legend_position = "none")
  if(strain != "NONE")
    xlab <- paste(xlab, "colored by", strain)
  p + ggplot2::ggtitle(xlab)
}

