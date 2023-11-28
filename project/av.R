# Load necessary libraries
library(ggplot2)
library(ggpubr)

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Read the dataset
star_data <- read.csv("project.csv")

# Step 1: Descriptive Statistics
summary(star_data$Temperature..K.)
summary(star_data$Luminosity.L.Lo.)
summary(star_data$Radius.R.Ro.)
summary(star_data$Absolute.magnitude.Mv.)

normalize <- function(v) {
  if (length(v) == 0 || all(is.na(v))) {
    warning("Input vector is empty or all values are NA.")
    return(NULL)
  }
  
  mean_v <- mean(v, na.rm = TRUE)
  sd_v <- sd(v, na.rm = TRUE)
  
  if (sd_v == 0) {
    warning("Standard deviation is zero. Returning original vector.")
    return(v)
  }
  
  normalized_v <- (v - mean_v) / sd_v
  return(normalized_v)
}

star_data$temp_norm <- normalize(star_data$Temperature..K.)
star_data$lumi_norm <-normalize(star_data$Luminosity.L.Lo.)
star_data$rad_norm <- normalize(star_data$Radius.R.Ro.)
star_data$absmag_norm <- normalize(star_data$Absolute.magnitude.Mv.)

summary(star_data$temp_norm)
summary(star_data$lumi_norm)
summary(star_data$rad_norm)
summary(star_data$absmag_norm)

# Alternatively, you can use the summary function for the whole dataset:
# summary(star_data)

# Step 2: Data Distribution - Histograms for numerical columns

# Histogram for Temperature
p1 <- ggplot(star_data, aes(x = Temperature..K.)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  ggtitle("Histogram of Temperature (K)") +
  xlab("Temperature (K)") +
  ylab("Frequency")

# Histogram for Luminosity
p2 <- ggplot(star_data, aes(x = Luminosity.L.Lo.)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  ggtitle("Histogram of Luminosity (L/Lo)") +
  xlab("Luminosity (L/Lo)") +
  ylab("Frequency")

# Histogram for Radius
p3 <- ggplot(star_data, aes(x = Radius.R.Ro.)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black") +
  ggtitle("Histogram of Radius (R/Ro)") +
  xlab("Radius (R/Ro)") +
  ylab("Frequency")

# Histogram for Absolute Magnitude
p4 <- ggplot(star_data, aes(x = Absolute.magnitude.Mv.)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  ggtitle("Histogram of Absolute Magnitude (Mv)") +
  xlab("Absolute Magnitude (Mv)") +
  ylab("Frequency")

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)