HW 4
================
Derrick Platero
2024-10-30

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
# Define the data
width_positions <- c(0, 2.5, 4, 5, 6, 6.3, 7.3, 8.6, 9.85)  # Width positions (x)
depths <- c(0, 1.1, 2.5, 4.0, 5.0, 6.0, 6.3, 7.3, 8.6)      # Corresponding depths (y)

# Initialize total area variable
total_area <- 0

# Loop through each segment to calculate trapezoidal area
# since there are multiple 'width/depth' measurements
for (i in 2:length(width_positions)) {
  # Calculate the width and average depth for each segment
  dx <- width_positions[i] - width_positions[i - 1]
  avg_depth <- (depths[i] + depths[i - 1]) / 2
  
  # Add the area of the current trapezoid to the total area
  total_area <- total_area + (avg_depth * dx)
}

# Print the total cross-sectional area
cat("Total Cross-Sectional Area:", total_area, "m^2\n")
```

    ## Total Cross-Sectional Area: 38.4025 m^2

``` r
# Total Cross-Sectional Area: 38.4025 m^2
```

``` r
# Manning’s Equation and Discharge Calculation
# Define the known values
wetted_perimeter <- 10.7  # Wetted perimeter (P) in meters

# Calculate the hydraulic radius (R = A (38.4025 m^2) / P (10.7))
hydraulic_radius <- total_area / wetted_perimeter
cat("Hydraulic Radius (R):", hydraulic_radius, "m\n")
```

    ## Hydraulic Radius (R): 3.589019 m

``` r
# Median Grain Size (D50) in meters
D50 <- 0.008  # 8 mm in meters

# Calculate Manning's roughness coefficient using Strickler's method
n_strickler <- (D50^(1/6)) / (hydraulic_radius^(1/6))
cat("Manning's Roughness Coefficient (n) using Strickler's method:", n_strickler, "\n")
```

    ## Manning's Roughness Coefficient (n) using Strickler's method: 0.3614257

``` r
# Define the slope (S) and Manning's roughness coefficient (n)
slope <- 0.0074  # Water surface slope givem on lab day
                  # Manning's roughness coefficient for a natural stream
                  # From the USGS Guide & Strickler method for selecting Manning's Roughness Coefficients

# Calculate the bankfull discharge using Manning's equation with Strickler's n
discharge <- (1 / n_strickler) * total_area * (hydraulic_radius^(2/3)) * (slope^(1/2))
cat("Bankfull Discharge (Q):", discharge, "m^3/s\n")
```

    ## Bankfull Discharge (Q): 21.42595 m^3/s

``` r
# Hydraulic Radius (R): 3.589019 m
# Manning's Roughness Coefficient (n) using Strickler's method: 0.3614257 
# Bankfull Discharge (Q): 21.42595 m^3/s
```

``` r
# constants and values
rho_w <- 1000  # Fluid density (kg/m³) for water at standard temp. and pressure ( 4C)
g <- 9.81      # Gravitational acceleration (m/s²)
R <- 3.589     # Hydraulic radius (m)
S <- 0.0074    # Water surface slope
F <- 1         # Friction factor (assuming 1 for uniform flow)

# Calculate mean bed shear stress (tau) in Pascals
tau <- rho_w * g * R * S * F

# Print the result
cat("Mean Bed Shear Stress (τ):", tau, "Pa\n")
```

    ## Mean Bed Shear Stress (τ): 260.5399 Pa

``` r
# Mean Bed Shear Stress (τ): 260.5399 Pa
```

``` r
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# Load and process data
pebble_data <- read_csv("F:/sed trans dep/pammel fluvial pred pebbles.csv")
```

    ## Rows: 100 Columns: 4

    ## ── Column specification ───────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): GrainSize_mm_1, GrainSize_mm_2, GrainSize_mm_3, GrainSize_mm_4
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Combine data from all groups into one vector, converting to numeric
all_grain_sizes <- c(
  as.numeric(pebble_data$GrainSize_mm_1),
  as.numeric(pebble_data$GrainSize_mm_2),
  as.numeric(pebble_data$GrainSize_mm_3),
  as.numeric(pebble_data$GrainSize_mm_4)
)
```

    ## Warning: NAs introduced by coercion
    ## Warning: NAs introduced by coercion
    ## Warning: NAs introduced by coercion
    ## Warning: NAs introduced by coercion

``` r
# Remove NA values (from non-numeric entries like 'Fines' and 'Sand')
all_grain_sizes <- na.omit(all_grain_sizes)

# Calculate D50 (median grain size)
D50 <- median(all_grain_sizes)
cat("Median Grain Size (D50):", D50, "mm\n")
```

    ## Median Grain Size (D50): 8 mm

``` r
# Calculate D84 (84th percentile grain size)
D84 <- quantile(all_grain_sizes, 0.84)
cat("84th Percentile Grain Size (D84):", D84, "mm\n")
```

    ## 84th Percentile Grain Size (D84): 16 mm

``` r
# Convert grain sizes to meters for further calculations
grain_sizes_m <- all_grain_sizes / 1000  # mm to meters
D50_m <- D50 / 1000
D84_m <- D84 / 1000

# Constants
rho_s <- 2650  # Sediment density (kg/m³) from sediment transport modelling in riverine environments
rho <- 1000    # Water density (kg/m³)
g <- 9.81      # Gravitational acceleration (m/s²)
tau <- 260.5399  # Shear stress (Pa)

# Calculate critical Shields stress (tau_c*) for each grain size
tau_c_star <- 0.086 * (grain_sizes_m / D50_m)^(-0.9)

# Calculate Shields parameter for each grain size
shields_parameter <- tau / ((rho_s - rho) * g * grain_sizes_m)

# Determine which size fractions are mobile
mobile_fraction <- grain_sizes_m[shields_parameter > tau_c_star]
cat("Mobile grain sizes (m):", mobile_fraction, "\n")
```

    ## Mobile grain sizes (m): 0.002 0.002 0.002 0.002 0.002 0.002 0.0028 0.0028 0.0028 0.0028 0.0028 0.0028 0.004 0.004 0.004 0.004 0.004 0.004 0.004 0.004 0.004 0.004 0.004 0.004 0.004 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.016 0.016 0.016 0.016 0.016 0.016 0.0226 0.0226 0.032 0.032 0.032 0.045 0.002 0.002 0.002 0.002 0.0028 0.0028 0.0028 0.0028 0.004 0.004 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.0226 0.0226 0.0226 0.0226 0.0226 0.0226 0.0226 0.032 0.032 0.032 0.045 0.128 0.0028 0.0028 0.0028 0.0028 0.004 0.004 0.004 0.004 0.004 0.004 0.004 0.004 0.004 0.004 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.008 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.0226 0.0226 0.0226 0.0028 0.0028 0.0028 0.0028 0.0028 0.0028 0.0028 0.004 0.004 0.004 0.004 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0057 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.0113 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.016 0.0226 0.0226 0.0226 0.0226 0.0226 0.045 0.064

``` r
# Calculate the fraction of the bed surface mobilized
mobilized_fraction <- length(mobile_fraction) / length(grain_sizes_m)
cat("Fraction of bed surface mobilized:", mobilized_fraction, "\n")
```

    ## Fraction of bed surface mobilized: 1

``` r
# Calculate bedload transport rate using Recking’s formula (using D50)
s <- rho_s / rho
bedload_transport_D50 <- tau * sqrt(g * (s - 1) * (mobile_fraction^3))
cat("Bedload transport rate (kg/s/m) for mobile fractions (D50):", bedload_transport_D50, "\n")
```

    ## Bedload transport rate (kg/s/m) for mobile fractions (D50): 0.09375527 0.09375527 0.09375527 0.09375527 0.09375527 0.09375527 0.1553058 0.1553058 0.1553058 0.1553058 0.1553058 0.1553058 0.26518 0.26518 0.26518 0.26518 0.26518 0.26518 0.26518 0.26518 0.26518 0.26518 0.26518 0.26518 0.26518 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 3.56134 3.56134 6.000338 6.000338 6.000338 10.00621 0.09375527 0.09375527 0.09375527 0.09375527 0.1553058 0.1553058 0.1553058 0.1553058 0.26518 0.26518 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 3.56134 3.56134 3.56134 3.56134 3.56134 3.56134 3.56134 6.000338 6.000338 6.000338 10.00621 48.0027 0.1553058 0.1553058 0.1553058 0.1553058 0.26518 0.26518 0.26518 0.26518 0.26518 0.26518 0.26518 0.26518 0.26518 0.26518 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 0.7500422 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 3.56134 3.56134 3.56134 0.1553058 0.1553058 0.1553058 0.1553058 0.1553058 0.1553058 0.1553058 0.26518 0.26518 0.26518 0.26518 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 0.4510898 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 1.259124 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 2.12144 3.56134 3.56134 3.56134 3.56134 3.56134 10.00621 16.97152

``` r
# Calculate bedload transport rate using D84 in Recking’s formula
bedload_transport_D84 <- rho_s * sqrt(g * (s - 1)) * D84_m * mobile_fraction
cat("Bedload transport rate using D84 (kg/s/m):", bedload_transport_D84, "\n")
```

    ## Bedload transport rate using D84 (kg/s/m): 0.3411712 0.3411712 0.3411712 0.3411712 0.3411712 0.3411712 0.4776396 0.4776396 0.4776396 0.4776396 0.4776396 0.4776396 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 3.855234 3.855234 5.458739 5.458739 5.458739 7.676351 0.3411712 0.3411712 0.3411712 0.3411712 0.4776396 0.4776396 0.4776396 0.4776396 0.6823423 0.6823423 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 3.855234 3.855234 3.855234 3.855234 3.855234 3.855234 3.855234 5.458739 5.458739 5.458739 7.676351 21.83496 0.4776396 0.4776396 0.4776396 0.4776396 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.6823423 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.364685 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 3.855234 3.855234 3.855234 0.4776396 0.4776396 0.4776396 0.4776396 0.4776396 0.4776396 0.4776396 0.6823423 0.6823423 0.6823423 0.6823423 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 0.9723378 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 1.927617 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 2.729369 3.855234 3.855234 3.855234 3.855234 3.855234 7.676351 10.91748

``` r
# Calculate total bedload transport over one day (24 hours) for D50 and D84
total_bedload_D50 <- sum(bedload_transport_D50) * 86400  # for 24 hours
total_bedload_D84 <- sum(bedload_transport_D84) * 86400  # for 24 hours
cat("Total bedload transport for D50 over one day (kg/m):", total_bedload_D50, "\n")
```

    ## Total bedload transport for D50 over one day (kg/m): 41570564

``` r
cat("Total bedload transport for D84 over one day (kg/m):", total_bedload_D84, "\n")
```

    ## Total bedload transport for D84 over one day (kg/m): 52986722

``` r
# Constants and parameters
g <- 9.81                  # Gravitational acceleration (m/s²)
kappa <- 0.41              # von Karman constant
Ca <- 1200 / 1e6           # Concentration at reference height in kg/m³ (1200 mg/L to kg/m³)
z_ref <- 0.05              # Reference height in meters (5 cm)
channel_depth <- 3.0       # Total depth of the channel in meters (example value)
num_slices <- 100          # Number of depth slices for integration

# Constants for ws-settling velocity calculation
rho_s <- 2650         # Sediment density (kg/m³)
rho <- 1000           # Water density (kg/m³)
D <- 0.187 / 1000     # Particle diameter in meters (0.187 mm)
g <- 9.81             # Gravitational acceleration (m/s²)
mu <- 0.001           # Dynamic viscosity of water (Pa·s)

# Calculate settling velocity (ws) using Stokes' law
ws <- ((rho_s - rho) * g * D^2) / (18 * mu)
cat("Settling velocity (ws):", ws, "m/s\n")
```

    ## Settling velocity (ws): 0.03144587 m/s

``` r
# Parameters from previous calculations
u_star <- sqrt(g * R * slope)  # Shear velocity 
z0 <- 3.5 * D84_m / 12.2       # Roughness height based on D84
Ro <- ws / (kappa * u_star)    # Rouse number 

# Define concentration profile function based on Rouse equation
concentration_profile <- function(z) {
  Ca * (z_ref / z)^Ro
}

# Define velocity profile function
velocity_profile <- function(z) {
  u_star * (1 / kappa) * log(z / z0)
}

# Discretize depth into slices
slice_heights <- seq(z0, channel_depth, length.out = num_slices)
slice_thickness <- diff(slice_heights)[1]  # Uniform slice thickness
```

``` r
# Initialize total sediment flux
total_flux <- 0

# Loop through each slice and calculate flux
for (i in 1:(num_slices - 1)) {
  # Mid-point of each slice for better approximation
  z_mid <- (slice_heights[i] + slice_heights[i + 1]) / 2
  
  # Concentration and velocity at the mid-point
  concentration <- concentration_profile(z_mid)
  velocity <- velocity_profile(z_mid)
  
  # Flux for this slice
  flux_slice <- concentration * velocity * slice_thickness
  total_flux <- total_flux + flux_slice
}

# Convert to kg/day by multiplying with cross-sectional area and seconds per day
cross_sectional_area <- R * channel_depth  # Example cross-sectional area
total_flux_kg_day <- total_flux * cross_sectional_area * 86400

cat("Total suspended sand transport (kg/day):", total_flux_kg_day, "\n")
```

    ## Total suspended sand transport (kg/day): 14091.54

``` r
# Constants and parameters
w <- 9.85                    # Channel width in meters 
g <- 9.81                   # Gravitational acceleration (m/s²)
kappa <- 0.41               # von Karman constant
Ca <- 1200 / 1e6            # Concentration at reference height in kg/m³ (1200 mg/L to kg/m³)
z_ref <- 0.05               # Reference height in meters (5 cm)
channel_depth <- 3.0        # Total depth of the channel in meters
num_slices <- 100           # Number of depth slices for integration

# Parameters from previous calculations
u_star <- sqrt(g * R * slope)  # Shear velocity 
z0 <- 3.5 * D84_m / 12.2       # Roughness height based on D84
Ro <- ws / (kappa * u_star)    # Rouse number

# Define concentration profile function based on Rouse equation
concentration_profile <- function(z) {
  Ca * (z_ref / z)^Ro
}

# Define velocity profile function (logarithmic)
velocity_profile <- function(z) {
  u_star * (1 / kappa) * log(z / z0)
}

# Discretize depth into slices
slice_heights <- seq(z0, channel_depth, length.out = num_slices)
slice_thickness <- diff(slice_heights)[1]  # Uniform slice thickness

# Initialize total sediment flux per unit width
q_s <- 0

# Loop through each slice and calculate concentration * velocity at the midpoint
for (i in 1:(num_slices - 1)) {
  # Midpoint of each slice for better approximation
  z_mid <- (slice_heights[i] + slice_heights[i + 1]) / 2
  
  # Calculate concentration and velocity at the midpoint
  concentration <- concentration_profile(z_mid)
  velocity <- velocity_profile(z_mid)
  
  # Flux contribution for this slice
  flux_slice <- concentration * velocity * slice_thickness
  q_s <- q_s + flux_slice
}

# Scale by channel width to get total suspended load flux Q_s
Q_s <- w * q_s

# Convert to kg/day by multiplying by seconds per day
Q_s_kg_day <- Q_s * 86400

cat("Total suspended sand transport (Q_s) in kg/day:", Q_s_kg_day, "\n")
```

    ## Total suspended sand transport (Q_s) in kg/day: 12891.4

``` r
# Constants and parameters
h <- 3.0                      # Total water depth in meters
z_ref <- 0.05                 # Reference height (m)
Ca <- 1200 / 1e6              # Concentration at reference height in kg/m³
Ro <- ws / (kappa * u_star)   # Rouse number

# Define a function for concentration profile based on Rouse equation
concentration_profile <- function(z, h, a, Ro) {
  (h - z) / z * (a / (h - a))^Ro
}

# Generate a sequence of depths from z_ref to h
z_values <- seq(z_ref, h, length.out = 100)
C_Ca_values <- concentration_profile(z_values, h, z_ref, Ro)

# Plot normalized concentration profile
plot(C_Ca_values, z_values / h, type = "l", col = "red", lwd = 2,
     xlab = "C / Ca", ylab = "z / H", main = "Concentration Profile with Depth")
```

![](Platero_HW4_files/figure-gfm/plot%201-1.png)<!-- -->

``` r
# Constants and parameters
h <- 3.0                      # Total water depth in meters
z_ref <- 0.05                 # Reference height (m)
Ca <- 1200 / 1e6              # Concentration at reference height in kg/m³
Ro <- ws / (kappa * u_star)   # Rouse number 

# Define a function for concentration profile based on Rouse equation
concentration_profile <- function(z, h, a, Ro) {
  (h - z) / z * (a / (h - a))^Ro
}

# Generate a sequence of depths from z_ref to h
z_values <- seq(z_ref, h, length.out = 100)
C_Ca_values <- concentration_profile(z_values, h, z_ref, Ro)

# Plot normalized concentration profile
plot(C_Ca_values, z_values / h, type = "l", col = "red", lwd = 2,
     xlab = "C / Ca", ylab = "z / H", main = "Concentration Profile with Depth")
```

``` r
# Empirical constants (example values)
a <- 0.1   # coefficient
b <- 1.7   # exponent (for fine sediment discharge)

# Generate a range of flow discharge values
Q_values <- seq(0, 30, by = 1)  # Discharge values in m³/s
Qs_values <- a * Q_values^b     # Calculate Qs for each Q

# Plot sediment discharge vs flow discharge
plot(Q_values, Qs_values, type = "l", col = "blue", lwd = 2,
     xlab = "Flow Discharge (Q, m³/s)", ylab = "Sediment Discharge (Q_s, metric tons/event)",
     main = "Sediment Discharge vs Flow Discharge")
```

![](Platero_HW4_files/figure-gfm/Plot%202-1.png)<!-- -->
