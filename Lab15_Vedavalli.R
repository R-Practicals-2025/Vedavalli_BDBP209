#Lab 15
#Apr 26 2025
#V.Linear Regression
x = c(0.5, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)
y = c(0.87, 7.12, 14.01, 24.37, 39.058, 58.62, 83.92)
#(1)3rd degree polynomial
model_quadratic = lm(y ~ poly(x,2,raw=T))
model_cubic=lm(y~poly(x,3,raw=T))
print(summary(model))
print(summary(model_cubic))
# model_quart=lm(y~poly(x,4,raw=T))
print(summary(model_quart))

# (2) Coefficient of x term
cat("Coefficient of x:",coef(model)[2],"\n")
#
# (3) Coefficient of x^2 term
cat("Coefficient of x^2:",coef(model)[3],"\n")

# (4) Coefficient of x^3 term
cat("Coefficient of x^3:",coef(model)[4],"\n")

# (5)Plot the original data and the fitted curve in the same graph. Both should show the
#data points clearly.

plot(x, y, main = "Data and Fitted quadratic and cubic Degree Polynomial", 
     xlab = "x", ylab = "y", pch = 19, col = "darkred")
x_seq=seq(min(x), max(x), length.out = 100)

y_pred_quad = predict(model_quadratic, newdata = data.frame(x=x_seq))
y_pred_cubic = predict(model_cubic, newdata = data.frame(x=x_seq))

lines(x_seq, y_pred_quad, col = "chartreuse4",lwd=2)
lines(x_seq, y_pred_cubic, col = "turquoise", lwd=2) 
legend("topleft", legend = c("Original Data", "Fitted quad Curve","Fitted cubic curve"), 
       col = c("darkred", "chartreuse4","turquoise"), pch = c(19, NA, NA), lty = c(NA, 1,1))
####

#VI. Multiple linear regression

#regression In this example we load the trees data set shipping with the
# R-package datasets. The trees data set provides measurements of the girth, height and volume
# of timber in 31 felled black cherry trees, also known as Prunus serotina. The height of the trees
# is given in feet (ft) and the volume is given in cubic feet (cft). The girth is the diameter of the
# tree (in inches) measured at 4 ft 6 in (approx. 1.37 m) above the ground.

#(i) Read the trees dataset into a dataframe, and convert all the quantities into SI unit of metres. 
#Volume will be in m3.The conversions are 1in = 0.0254m, 1ft = 0.3048m and left = 0.028317m3
# Load dataset
library(datasets)
data(trees)
head(trees)

df=trees  # copy to a new df

df$Girth=df$Girth * 0.0254
df$Height=df$Height * 0.3048 
df$Volume=df$Volume * 0.028317

head(df)
#
#(ii)Our goal is to build a linear regression model with Volume being the dependent variable
# and Height and Girth being the independent (explanatory) variables. We will do this
# in two ways: Use matrix methods, and also by step-wise fitting of one coefficient at a
# time. A multiple regression model is built by finding the optimal set of coefficients β.
# The regression model is yi = β0 + summation over i βixij εi with the objective function being
#ε =summation over i=1 to n(yi − xiTβ)^2

#two varibles we use scatter plot, but for multivariate we use splom() from the library-lattice
library(lattice)
splom(df, xlab = "Scatter Plot Matrix (in SI Units)")


# (iii) Save the Volume column as response vector y
y <- df$Volume

# (iv) Initialize intercept β0 (vector of 1s)
beta0 <- rep(1, nrow(df))

# (v) Build X matrix by combining beta0, Girth, and Height
X <- cbind(beta0, df$Girth, df$Height)

# (vi) Solve for beta using matrix method
model1 <- solve(t(X) %*% X) %*% t(X) %*% y

# Print β0, β1, and β2
cat("β0 (Intercept):", model1[1], "\n")
cat("β1 (Girth coefficient):", model1[2], "\n")
cat("β2 (Height coefficient):", model1[3], "\n")

# (vii) Predict volume for given new Girth and Height
# New Girth values: 0.3, 0.4, 0.5 meters
# New Height values: 20, 21, 22 meters
# Construct new X matrix for prediction
new_X <- cbind(1, c(0.3, 0.4, 0.5), c(20, 21, 22))

# Predicted volumes (matrix multiplication)
predicted_volumes_model1 <- new_X %*% model1
cat("Predicted volumes (matrix method):", predicted_volumes_model1, "\n")

# (viii) Now using lm() method
model2 <- lm(Volume ~ Girth + Height, data = df)

# View coefficients from model2
summary(model2)

# Compare coefficients from model1 and model2
cat("Coefficients from lm():\n")
print(coef(model2))

# (ix) Predict using predict() function
newdata <- data.frame(Girth = c(0.3, 0.4, 0.5), Height = c(20, 21, 22))
predicted_volumes_model2 <- predict(model2, newdata = newdata)

cat("Predicted volumes (lm predict() method):\n")
print(predicted_volumes_model2)

# (VII) Non-linear regression

# (1) Enter the data sets
xdat <- c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2,
          2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0)

ydat <- c(-0.09, 0.59, 0.42, 2.03, 3.43, 1.84, 5.30, 4.40, 6.67, 7.40,
          7.34, 8.76, 10.25, 10.93, 13.78, 14.21, 17.82, 21.83, 23.04,
          24.25, 27.48)

# (2) Create a data frame
data <- data.frame(xdat, ydat)

# (3) Use nls() to fit y = a * x^n
# Guess: visually, data is increasing more than linearly but not explosively → guess n = 2
model <- nls(ydat ~ a * xdat^n, data = data, start = list(a = 1, n = 2))

# (4) Print summary of fit
summary(model)

# (5) How many degrees of freedom are there?
# Degrees of freedom = number of observations - number of parameters
cat("Degrees of freedom:", df.residual(model), "\n")

# (6) Generate fitted curve
xseq <- seq(1, 3, by = 0.1)
a_est <- coef(model)["a"]
n_est <- coef(model)["n"]
yfit <- a_est * xseq^n_est

# Plot original data and fitted curve
plot(xdat, ydat, main = "Data and Fitted Curve", xlab = "x", ylab = "y", pch = 16, col = "blue")
lines(xseq, yfit, col = "red", lwd = 2)
legend("topleft", legend = c("Original Data", "Fitted Curve"), col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1))


# (VIII) Clustering methods

# (1) Hierarchical Clustering
# (i)
library(tidyverse)
library(dplyr)
library(RColorBrewer)

# (ii)
spellman <- read.csv("spellman-wide.csv")
dim(spellman)  # Prints dimensions

# (iii) Print first 5 rows and first 8 columns
spellman[1:5, 1:8]

# (iv) Create correlation matrix and distance matrix
spellman_cor <- spellman %>%
  select(-time, -expt) %>%
  cor(use = "pairwise.complete.obs")

spellman_dist <- as.dist(1 - spellman_cor)

# (v) Generate the hierarchical clustering and plot it
spellman_tree <- hclust(spellman_dist, method = "complete")
plot(spellman_tree)

# Decrease font size
plot(spellman_tree, cex = 0.1)

# (vi) Dendrogram without labels
library(dendextend)

spellman_dend <- as.dendrogram(spellman_tree)
plot(spellman_dend, leaflab = "none")

# (vii) Cut tree into 4 clusters
clusters <- cutree(spellman_dend, k = 4)
table(clusters)

clusters[1:6]  # Print first 6 genes' clusters


# (viii) Colour branches and plot
plotc <- color_branches(spellman_tree, k = 4)
plot(plotc, leaflab = "none")

# Now for k = 8
plotc8 <- color_branches(spellman_tree, k = 8)
plot(plotc8, leaflab = "none")
table(cutree(spellman_tree, k = 8))


# (ix) Create dataframe with gene names and cluster number
clusters_df <- data.frame(gene = names(clusters), cluster = clusters)

# Which cluster does YALO22C belong to?
clusters_df %>% filter(gene == "YALO22C")


# (x) Names of genes in cluster 3
cluster3_genes <- clusters_df %>%
  filter(cluster == 3) %>%
  pull(gene)

cluster3_genes  # Print if you want


# Convert to long format
spellman_long <- spellman %>%
  gather(gene, expression, -expt, -time)

head(spellman_long)

# Generate color scheme
color_scheme <- rev(brewer.pal(8, "RdBu"))

# (xi) Generate heatmap of expression for first cluster

# Plot heatmap
spellman_long %>%
  filter(gene %in% cluster3_genes & expt == "alpha") %>%
  ggplot(aes(x = time, y = gene)) +
  geom_tile(aes(fill = expression)) +
  scale_fill_gradientn(colors = color_scheme, limits = c(-2, 2)) +
  theme(axis.text.y = element_text(size = 6))

# (xii) Combining heatmap and dendrogram
# (a) Cut tree into subtrees
sub_trees <- cut(spellman_dend, h = 1.48)
sub_trees$lower

# (b) Extract third subtree
cluster3_tree <- sub_trees$lower[[3]]
cluster3_tree

# (c) Plot the subtree
cluster3_tree %>%
  set("labels_cex", 0.45) %>%
  set("labels_col", "red") %>%
  plot(horiz = TRUE)

# (d) Heatmap with dendrogram
library(gplots)

# Filter data for 'alpha' experiment
alpha_factor <- filter(spellman, expt == "alpha")

# Create matrix
alpha_mtx <- alpha_factor %>%
  select(-time, -expt) %>%
  as.matrix()

# Set row names
rownames(alpha_mtx) <- alpha_factor$time

# Transpose matrix
transposed_alpha_mtx <- t(alpha_mtx)

# Heatmap
heatmap.2(transposed_alpha_mtx,
          Rowv = cluster3_tree, # Use the dendrogram
          Colv = NULL, # Keep columns order
          dendrogram = "row",
          breaks = seq(-2, 2, length.out = 9),
          col = color_scheme,
          trace = "none",
          density.info = "none",
          xlab = "Time (mins)")



# (2) K-means clustering
# Load packages
library(tidyverse)
library(ggplot2)
library(dplyr)

# Read the Airbnb listings data
listings <- read.csv("listings_airbnb.csv")

# Print number of rows and column names
cat("Number of rows:", nrow(listings), "\n")
cat("Column names:\n")
print(names(listings))

# Scatter plot of number of reviews vs price
ggplot(listings, aes(number_of_reviews, price, color = room_type, shape = room_type)) +
  geom_point(alpha = 0.25) +
  xlab("Number of reviews") +
  ylab("Price")

# Normalize 'price' and 'number_of_reviews' columns
listings[, c("price", "number_of_reviews")] <- scale(listings[, c("price", "number_of_reviews")])

# Create a subset with price, minimum_nights, and number_of_reviews
airbnb_2cols <- listings[, c("price", "minimum_nights", "number_of_reviews")]
print(head(airbnb_2cols))

# Perform K-means clustering with 3 clusters
set.seed(23)
km_out <- kmeans(na.omit(airbnb_2cols), centers = 3, nstart = 20)
print(km_out)

# Find best number of clusters using scree plot (elbow method)
nclusters <- 10
wss <- numeric(nclusters)

for (k in 1:nclusters) {
  km_out <- kmeans(na.omit(airbnb_2cols), centers = k, nstart = 20)
  wss[k] <- km_out$tot.withinss
}

# Create a scree plot
wss_df <- tibble(clusters = 1:nclusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')

scree_plot +
  geom_hline(
    yintercept = wss,
    linetype = 'dashed',
    col = c(rep('#000000', 4), '#FF0000', rep('#000000', 5))
  )

# From the plot, choose 5 clusters
set.seed(23)
km_out_final <- kmeans(na.omit(airbnb_2cols), centers = 5, nstart = 20)

# Attach cluster ID to the data
airbnb_2cols_clustered <- na.omit(airbnb_2cols)
airbnb_2cols_clustered$cluster_id <- factor(km_out_final$cluster)

# Plot data colored by cluster ID
ggplot(airbnb_2cols_clustered, aes(number_of_reviews, price, color = cluster_id)) +
  geom_point(alpha = 0.25) +
  xlab("Number of reviews") +
  ylab("Price")






















