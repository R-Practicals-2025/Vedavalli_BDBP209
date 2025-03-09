#Ex 5
#(5) Write a script to recreate the plot in the slide with the plot title ’This is a graph’.
x<-c(1,3,5,7,9,11)
y<-c(2,7,5,10,8,10)
plot(x,y,lty="dashed",col="pink",type="o",xlab = "Time",ylab = "Performance",main = "This is a graph",col.main="blue",lwd=3)

labels=c(1,3,5,7,9,11)
text(x+0.3,y,labels,col="red")
legend("topleft", inset=0.05, lty="dashed", legend="Per curve", 
       col="pink")
# Ex 6
#Plot a histogram representation of hypergeometric distribution with N=500, K=50 and n=30

# Function to compute factorial manually
custom_factorial <- function(n) {
  if (n == 0) return(1)  # Base case: 0! = 1
  prod(1:n)  # Product of all integers from 1 to n
}

# Function to compute combinations (nCr) using logarithms to prevent overflow
combinations <- function(n, r) {
  if (r > n) return(0)  # If r is greater than n, return 0 (invalid case)
  exp(lfactorial(n) - (lfactorial(r) + lfactorial(n - r)))  # Using log factorial for stability
}

# Function to manually compute the Hypergeometric Distribution PMF
hypergeom_distribution_manual <- function(N, K, n) {
  x <- 0:n  # Possible values for number of successes in the sample
  probabilities <- numeric(length(x))  # Initialize vector to store probabilities
  
  # Loop through each possible success count and calculate its probability
  for (i in 1:length(x)) {
    num_successes <- x[i]  # Current number of successes being considered
    
    # Compute probability using the hypergeometric formula
    numerator <- combinations(K, num_successes) * combinations(N - K, n - num_successes)
    denominator <- combinations(N, n)
    probabilities[i] <- numerator / denominator
  }
  
  return(list(x = x, probabilities = probabilities))  # Return computed values
}

# Define parameters
N <- 500  # Total population size
K <- 50   # Number of successful items in the population
n <- 30   # Sample size (number of draws)

# Compute the hypergeometric probability distribution
hypergeom_data <- hypergeom_distribution_manual(N, K, n)

# Plot the histogram using a simple barplot (without ggplot2)
barplot(hypergeom_data$probabilities, 
        names.arg = hypergeom_data$x,  # X-axis labels (success counts)
        col = "yellow",  # Bar color
        border = "black",  # Bar border color
        main = "Histogram of Hypergeometric Distribution",  # Title
        xlab = "Number of Successes",  # X-axis label
        ylab = "Probability",  # Y-axis label
        ylim = c(0, max(hypergeom_data$probabilities) * 1.2))  # Adjust y-axis for better visibility

# Add a grid for better readability
grid()

#Ex 7
#Write few lines of script to explore what happens to a hypergeomtric distribution when n is increased and gets closer to N. Show that it approaches the binomial distribution by
#plotting histograms of both hypergeometric and binomial on the same plot. Use a 3x3 grid of 9 graphs to show the convergence of hypergeometric to binomial distribution.
# Function to compute factorial

# Function to compute factorial manually
factorial_custom <- function(n) {
  if (n == 0) return(1)  # Base case: 0! = 1
  prod(1:n)  # Compute factorial by multiplying numbers from 1 to n
}

# Function to compute combinations (nCr)
combinations <- function(n, r) {
  if (r > n) return(0)  # If r is greater than n, return 0 (invalid case)
  factorial_custom(n) / (factorial_custom(r) * factorial_custom(n - r))  # Compute nCr using factorial formula
}

# Function to compute probability mass function (PMF) of the hypergeometric distribution
hypergeom_distribution <- function(N, K, n, x) {
  numerator <- combinations(K, x) * combinations(N - K, n - x)  # Select x successes from K and remaining from N-K
  denominator <- combinations(N, n)  # Total ways to select n items from N
  return(numerator / denominator)  # Return probability value
}

# Function to compute probability mass function (PMF) of the binomial distribution
binom_distribution <- function(n, p, x) {
  combinations(n, x) * (p^x) * ((1 - p)^(n - x))  # Binomial formula: nCx * p^x * (1-p)^(n-x)
}

# Function to plot histograms for hypergeometric and binomial distributions
plot_histogram <- function(hyper_probs, binom_probs, x_vals, n) {
  # Create an empty plot with appropriate axis limits
  plot(NULL, xlim = c(min(x_vals), max(x_vals)), ylim = c(0, max(c(hyper_probs, binom_probs))),
       xlab = "Number of Successes", ylab = "Probability",
       main = paste("n =", n), type = "n")
  
  # Draw bars for hypergeometric probabilities (left side of each x value)
  rect(x_vals - 0.25, 0, x_vals, hyper_probs, col = "navy", border = "black")
  
  # Draw bars for binomial probabilities (right side of each x value)
  rect(x_vals, 0, x_vals + 0.25, binom_probs, col = "green", border = "black")
  
  # Add a legend to distinguish between hypergeometric and binomial distributions
  legend("topright", legend = c("Hypergeometric", "Binomial"), fill = c("navy", "green"))
}

# Define key parameters
N <- 100  # Total population size
K <- 40   # Number of successful items in the population
n_values <- seq(10, N, length.out = 9)  # Generate a sequence of sample sizes
p <- K / N  # Probability of success in the binomial distribution

# Set up a 3x3 grid for plotting multiple histograms
par(mfrow = c(3, 3))

# Loop through different sample sizes and generate histograms
for (n in n_values) {
  x_vals <- 0:n  # Possible success values in the sample
  
  # Compute probabilities for the hypergeometric distribution
  hyper_probs <- sapply(x_vals, function(x) hypergeom_distribution(N, K, n, x))
  
  # Compute probabilities for the binomial distribution
  binom_probs <- sapply(x_vals, function(x) binom_distribution(n, p, x))
  
  # Plot the histogram comparing both distributions
  plot_histogram(hyper_probs, binom_probs, x_vals, n)
}

# Reset the plotting grid to default (single plot)
par(mfrow = c(1, 1))

#Ex 8
#(8) On the same plot, draw 3 Poisson distributions with λ values of 3,20,45 (Code the probability distribution function).
# Function to compute factorial manually
factorial_custom <- function(n) {
  if (n == 0) return(1)  # Base case: 0! = 1
  prod(1:n)  # Compute factorial by multiplying numbers from 1 to n
}

# Function to compute the Poisson probability mass function (PMF)
poisson_pmf <- function(lambda, x) {
  (lambda^x * exp(-lambda)) / factorial_custom(x)  # Poisson formula: (λ^x * e^(-λ)) / x!
}

# Define the range of x values (number of events)
x_vals <- 0:60  # Choose a sufficiently wide range to cover all distributions

# Define different values for lambda (mean number of occurrences)
lambdas <- c(3, 20, 45)  # Representing different levels of event occurrence rates

# Compute Poisson probabilities for each lambda
poisson_probabilities <- lapply(lambdas, function(lambda) sapply(x_vals, function(x) poisson_pmf(lambda, x)))

# Set up the plot with the first Poisson distribution
plot(x_vals, poisson_probabilities[[1]], type = "h", lwd = 2, col = "purple",
     main = "Poisson Distributions (λ = 3, 20, 45)",  # Title
     xlab = "x (Number of Events)", ylab = "Probability",  # Axis labels
     ylim = c(0, max(unlist(poisson_probabilities))))  # Set y-axis limit to fit all distributions

# Add additional Poisson distributions to the same plot
lines(x_vals, poisson_probabilities[[2]], type = "h", lwd = 2, col = "dodgerblue")  # λ = 20 (green)
lines(x_vals, poisson_probabilities[[3]], type = "h", lwd = 2, col = "darkgreen")  # λ = 45 (red)

# Add a legend to differentiate lambda values
legend("topright", legend = c("λ = 3", "λ = 20", "λ = 45"),
       fill = c("purple", "dodgerblue", "darkgreen"))
#

#Ex 9
#Load the csv file for heights and weights of 25000 people and do the following:
# Load the dataset containing heights and weights of 25,000 people
data <- read.csv('/home/ibab/Downloads/SOCR-HeightWeight.csv')

# Print column names and dataset dimensions to check the structure of the data
print(colnames(data))  # Displays the names of all columns
print(dim(data))  # Shows the total number of rows and columns

# View dataset structure to verify data types
str(data)

# Extract height and weight columns
height <- data$Height.Inches.
weight <- data$Weight.Pounds.

# Remove any NA values if they exist
height <- height[!is.na(height)]
weight <- weight[!is.na(weight)]

# (i) Compute and display mean and standard deviation for height
mean_height <- mean(height, na.rm = TRUE)
sd_height <- sd(height, na.rm = TRUE)

cat("Mean Height:", mean_height, "\nStandard Deviation Height:", sd_height, "\n")

# Plot histogram for height
hist(height, 
     breaks = "Sturges",  # Default bin selection method
     prob = TRUE,  # Convert histogram to a probability density plot
     col = "lightgray", 
     border = "black",
     main = "Histogram of Height",  
     xlim = range(height), 
     xlab = "Height (Inches)", 
     ylab = "Probability Density")

grid()  # Add grid lines for better visibility

# (ii) Compute and display mean and standard deviation for weight
mean_weight <- mean(weight, na.rm = TRUE)
sd_weight <- sd(weight, na.rm = TRUE)

cat("Mean Weight:", mean_weight, "\nStandard Deviation Weight:", sd_weight, "\n")

# Plot histogram for weight
hist(weight, 
     breaks = "Sturges",  
     prob = TRUE,  
     col = "lightgray", 
     border = "black",
     main = "Histogram of Weight",  
     xlim = range(weight), 
     xlab = "Weight (Pounds)", 
     ylab = "Probability Density")

grid()

# (iii) Generate Gaussian (Normal) distribution curves for height and weight

# Function to calculate Z-score (standardized value)
zcalc <- function(x, mu, sd) {
  return((x - mu) / sd)  # Formula: Z = (X - μ) / σ
} 

# Gaussian Probability Density Function (PDF)
gaussian_pdf <- function(z) {
  return((1 / sqrt(2 * pi)) * exp(-0.5 * z^2))  # Standard normal formula
}

# Compute Z-scores for height and weight
z_height <- zcalc(height, mean_height, sd_height)
z_weight <- zcalc(weight, mean_weight, sd_weight)

# Sort Z-scores for smoother Gaussian curves
z_height_sorted <- sort(z_height)
z_weight_sorted <- sort(z_weight)

# Compute corresponding Gaussian PDF values
pdf_height <- gaussian_pdf(z_height_sorted)
pdf_weight <- gaussian_pdf(z_weight_sorted)

# Set a common y-axis limit for both curves
ylim_max <- max(pdf_height, pdf_weight)

# Plot Gaussian curve for heights
plot(z_height_sorted, pdf_height, type = "l", col = "red", lwd = 2, lty=2,
     main = "Gaussian Curves (Standardized Heights & Weights)", 
     xlab = "Z-score", ylab = "Probability Density", ylim = c(0, ylim_max))

# Add Gaussian curve for weights
lines(z_weight_sorted, pdf_weight, col = "green", lwd = 2, lty=3)

# Add legend to differentiate height and weight curves
legend("topright", legend = c("Height", "Weight"), col = c("red", "green"), lwd = 2, lty=c(2,3))

# (iv) Effect of changing the bin size in histograms

# Function to plot histograms with different bin sizes
plot_histogram <- function(data, bins, title) {
  hist(data, breaks = bins, col = "lightblue", border = "black",
       main = title, xlab = "Value", ylab = "Frequency")
}

# Arrange 3 plots side by side to observe bin size effects
par(mfrow = c(1, 3))  
plot_histogram(height, 10, "Bins = 10")  # Fewer bins → less detail
plot_histogram(height, 30, "Bins = 30")  # Moderate bin count
plot_histogram(height, 50, "Bins = 50")  # More bins → more granularity
par(mfrow = c(1, 1))  # Reset plotting layout

#(10) Plot the PDF and CPDF for the uniform distribution U(1,2). Find a way to shade the region under the PDF up to x = 1.5.
# Probability Density Function (PDF) for a Uniform Distribution
uniform_pdf_function <- function(x, a, b) {
  if (x < a || x > b) {
    return(0)  # Zero probability outside [a, b]
  } else {
    return(1 / (b - a))  # Constant density within range
  }
}

# Cumulative Distribution Function (CDF) for a Uniform Distribution
uniform_cdf_function <- function(x, a, b) {
  if (x < a) {
    return(0)  # Zero probability before 'a'
  } else if (x > b) {
    return(1)  # Probability reaches 1 after 'b'
  } else {
    return((x - a) / (b - a))  # Linear increase within [a, b]
  }
}

# Plot Uniform PDF with shaded area up to a given x
plot_uniform_pdf_function <- function(a, b, shade_up_to) {
  x_vals <- seq(a - 1, b + 1, length.out = 100)
  pdf_vals <- sapply(x_vals, function(x) uniform_pdf_function(x, a, b))
  
  plot(x_vals, pdf_vals, type = "l", col = "blue", lwd = 2,
       main = paste("Uniform PDF: U(", a, ",", b, ")"),
       xlab = "x", ylab = "Density", ylim = c(0, 1.2))
  
  # Shade area under the curve
  shade_x_vals <- seq(a, shade_up_to, length.out = 100)
  for (x in shade_x_vals) {
    lines(c(x, x), c(0, uniform_pdf_function(x, a, b)), col = rgb(0, 0, 1, 0.3), lwd = 1)
  }
}

# Plot Uniform CDF with shaded area up to a given x
plot_uniform_cdf_function <- function(a, b, shade_up_to) {
  x_vals <- seq(a - 1, b + 1, length.out = 100)
  cdf_vals <- sapply(x_vals, function(x) uniform_cdf_function(x, a, b))
  
  plot(x_vals, cdf_vals, type = "l", col = "red", lwd = 2,
       main = paste("Uniform CDF: U(", a, ",", b, ")"),
       xlab = "x", ylab = "Cumulative Probability")
  
  # Shade area under the curve
  shade_x_vals <- seq(a, shade_up_to, length.out = 100)
  for (x in shade_x_vals) {
    lines(c(x, x), c(0, uniform_cdf_function(x, a, b)), col = rgb(1, 0, 0, 0.3), lwd = 1)
  }
}

# Plot both graphs side by side
par(mfrow = c(1, 2))
plot_uniform_pdf_function(1, 2, 1.5)
plot_uniform_cdf_function(1, 2, 1.5)

#Ex 11
#Plot the PDF and CPDF for the exponential distribution with λ = 10. Shade the region under the PDF up to x = 2.8.
# Exponential Probability Density Function (PDF)
exp_pdf <- function(x, lambda) {
  ifelse(x >= 0, lambda * exp(-lambda * x), 0)  # Defined for x ≥ 0
}

# Exponential Cumulative Distribution Function (CDF)
exp_cdf <- function(x, lambda) {
  ifelse(x >= 0, 1 - exp(-lambda * x), 0)  # CDF starts at 0, approaches 1
}

# Set parameters
lambda <- 10
x_vals <- seq(0, 5, length.out = 300)
pdf_vals <- exp_pdf(x_vals, lambda)
cdf_vals <- exp_cdf(x_vals, lambda)

# Plot PDF with shaded region
par(mfrow = c(1, 2))  # Arrange plots side by side
plot(x_vals, pdf_vals, type = "l", col = "blue", lwd = 2,
     main = "Exponential PDF (λ=10)", xlab = "x", ylab = "Density")

# Shade PDF up to x = 2.8 with a dark red fill
polygon(c(0, seq(0, 2.8, length.out = 100), 2.8), 
        c(0, exp_pdf(seq(0, 2.8, length.out = 100), lambda), 0), 
        col = rgb(0.6, 0, 0, 0.7), border = NA)

# Plot CDF
plot(x_vals, cdf_vals, type = "l", col = "red", lwd = 2,
     main = "Exponential CDF (λ=10)", xlab = "x", ylab = "Probability")

par(mfrow = c(1,1))  # Reset plot layout
#
#Ex 12
#Plot the PDF and CPDF for the Gamma distribution with α = 5 and θ = 3.
# Gamma Probability Density Function (PDF)
gamma_density <- function(x, alpha, theta) {
  ifelse(x > 0, (x^(alpha-1) * exp(-x/theta)) / (theta^alpha * gamma(alpha)), 0)  # Defined for x > 0
}

# Gamma Cumulative Distribution Function (CDF)
gamma_distribution <- function(x, alpha, theta) {
  ifelse(x > 0, pgamma(x, shape = alpha, scale = theta), 0)  # Uses pgamma() for cumulative probability
}

# Set parameters
alpha <- 5
theta <- 3
x_vals <- seq(0, 30, length.out = 300)
pdf_vals <- gamma_density(x_vals, alpha, theta)
cdf_vals <- gamma_distribution(x_vals, alpha, theta)

# Plot PDF
par(mfrow = c(1, 2))  # Arrange plots side by side
plot(x_vals, pdf_vals, type = "l", col = "darkgreen", lwd = 2,
     main = "Gamma PDF (α=5, θ=3)", xlab = "x", ylab = "Density")

# Plot CDF
plot(x_vals, cdf_vals, type = "l", col = "purple", lwd = 2,
     main = "Gamma CDF (α=5, θ=3)", xlab = "x", ylab = "Probability")

par(mfrow = c(1,1))  # Reset plot layout
#
#Ex 13
#Plot the PDF and CPDF for the Chi-square distribution for 20 degrees of freedom. Shade the region under the PDF up to x = 1.0.
# Define the range of x values
x_vals <- seq(0, 50, length.out = 100)

# Compute the Chi-square PDF and CDF for df = 20
chi_sq_density <- dchisq(x_vals, df = 20)  # Probability density function
chi_sq_distribution <- pchisq(x_vals, df = 20)  # Cumulative distribution function

# Plot PDF with shaded region
par(mfrow = c(1, 2))  # Arrange plots side by side
plot(x_vals, chi_sq_density, type = "l", col = "darkorange", lwd = 2, 
     ylab = "Density", xlab = "x", main = "Chi-square PDF (df=20)")
polygon(c(0, seq(0, 1, length.out = 50), 1), 
        c(0, dchisq(seq(0, 1, length.out = 50), df = 20), 0), 
        col = rgb(0, 0.5, 1, 0.5), border = NA)  # Light blue shading
lines(x_vals, chi_sq_density, col = "darkorange", lwd = 2)

# Plot CDF
plot(x_vals, chi_sq_distribution, type = "l", col = "darkred", lwd = 2, 
     ylab = "Cumulative Probability", xlab = "x", main = "Chi-square CDF (df=20)")

par(mfrow = c(1,1))  # Reset plot layout
