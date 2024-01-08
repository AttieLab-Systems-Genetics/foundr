#' Prepare biplot data
#'
#' @param dat data frame with appropriate columns
#' @param traits trait names to include
#' @param factors design column names
#'
#' @return data frame
#' 
#' @importFrom dplyr across filter mutate
#' @importFrom tidyr pivot_wider
#' @export
#'
biplot_data <- function(dat,
                        traits = unique(dat$trait),
                        factors = c("strain", "animal", "sex", "condition"),
                        orders = c("module","kME","p.value","size")) {
  factors <- factors[factors %in% names(dat)]
  orders <- orders[orders %in% names(dat)]
  modulename <- attr(dat, "modulename")
  
  ordout <- dplyr::distinct(
    dplyr::select(
      dat,
      dplyr::any_of(c("trait", orders))))
  
  # Revise ordout so that large value is more important.
  if("p.value" %in% orders)
    ordout$p.value <- -log10(ordout$p.value)
  if("module" %in% orders)
    ordout$module <- 1 + max(ordout$module) - ordout$module
  
  out <- dplyr::mutate(
    # Pivot `traits` to columns, leaving `factors` in place. 
    tidyr::pivot_wider(
      dplyr::filter(
        dat,
        trait %in% traits),
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

#' Get PCA components from biplot data
#'
#' @param bip object from `biplot_data`
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
  size <- match.arg(size)
  factors <- bip$factors
  orders <- bip$orders
  modulename <- bip$modulename
  bip <- as.data.frame(bip$data)
  
  rownames(bip) <- bip$strain
  bip <- bip[,-match(factors, colnames(bip))]
  bip <- t(bip)
  
  # The `princomp` routine cannot handle wide tables.
  if(ncol(bip) > nrow(bip)) return(NULL)
  
  if(strain != "NONE") {
    strain_color <- "NO"
    strain_color[bip[,strain] >= threshold["SD"]] <- "UP"
    strain_color[bip[,strain] <= -threshold["SD"]] <- "DOWN"
  } else
    strain_color <- "NO"
  if(!is.null(modulename))
    strain_color[modulename == rownames(bip)] <- "EIGEN"
  
  ordr::mutate_cols(
    ordr::mutate_rows(
      # Redistribute inertia between rows and columns in ordination.
      ordr::confer_inertia(
        ordr::as_tbl_ord(
          # Principal components.
          stats::princomp(bip, cor = TRUE)),
        1),
      trait = rownames(bip),
      size = orders[[size]],
      color = strain_color),
    strain = colnames(bip))
}

#' Biplot using ggplot2 via ordr package
#'
#' @param bip_pca 
#' @param scale.factor 
#'
#' @return object of class ggplot2
#' 
#' @importFrom ggplot2 aes
#' @importFrom ordr geom_cols_vector geom_cols_text_radiate geom_rows_point ggbiplot
#' @export
#'
biggplot <- function(bip_pca, scale.factor = 2) {
  if(is.null(bip_pca))
    return(plot_null("Fewer traits than strains"))
  
  CB_colors <- RColorBrewer::brewer.pal(n = 3, name = "Dark2")
  
  ordr::ggbiplot(bip_pca, sec.axes = "cols", scale.factor = scale.factor) +
    ordr::geom_rows_point(ggplot2::aes(size = .data$size, color = .data$color),
                          shape = 1, stroke = 1) +
    ordr::geom_cols_vector(ggplot2::aes(color = .data$strain), size = 2) +
    ordr::geom_cols_text_radiate(
      ggplot2::aes(label = .data$strain), col = "black") +
    ggplot2::scale_color_manual(
      values = c(foundr::CCcolors, 
                 c(DOWN = CB_colors[1], NO = CB_colors[3], UP = CB_colors[2],
                   EIGEN = "red")))
}
