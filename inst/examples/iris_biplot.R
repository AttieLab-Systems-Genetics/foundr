# Example taken from ordr::ggbiplot
# Also partly from https://corybrunson.github.io/ordr/
library(ordr)

# PCA of iris data
iris_pca <- ordinate(iris, cols = 1:4, prcomp, scale = TRUE)

# row-principal predictive biplot
p <- ggbiplot(iris_pca, sec.axes = "cols", scale.factor = 2) +
  geom_rows_point(aes(color = Species, shape = Species)) +
  stat_rows_ellipse(aes(color = Species), alpha = .5, level = .99) +
  geom_cols_vector() +
  geom_cols_text_radiate(aes(label = name)) +
  expand_limits(y = c(-3.5, NA)) +
  ggtitle("PCA of Anderson's iris measurements",
          "99% confidence ellipses; variables use top & right axes")

# GGplot
p

# Plotly of GGplot, but "geom_GeomAxis() has yet to be implemented in plotly"
plotly::ggplotly(p)
