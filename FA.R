#This dataset contains 90 responses for 14 different variables
# that customers consider while purchasing car. 
# The survey questions were framed using 5-point scale 
# with 1 being very low and 5 being very high.
data <- read.csv(file.choose(),header=TRUE)
head(data)
library(psych)
library(GPArotation)
# Number of factors
# We'll be using `Psych` package's `fa.parallel` function
# to execute parallel analysis to find the no of factors
parallel <- fa.parallel(data, fm = 'minres', fa = 'fa')
# The blue line shows eigenvalues of actual data and the two red lines 
# (placed on top of each other) show simulated and resampled data. 
# Here we look at the large drops in the actual data and spot the point 
# where it levels off to the right. Also we locate the point of inflection - 
# the point where the gap between simulated data and actual data tends to be minimum.
# Looking at this plot and parallel analysis, anywhere between 2 to 5 
# factors factors would be good choice.

# Let's start off with 3 as the number of factors. 
# In order to perform factor analysis, we'll use `psych`
# package's fa()function
threefactor <- fa(data,nfactors = 3,rotate = "oblimin",fm="minres")
print(threefactor)

# Let's establish a cut off to improve visibility
print(threefactor$loadings,cutoff = 0.3)

# As you can see two variables have become insignificant and two other have double-loading. 
# Next, lets  consider '4' factors
fourfactor <- fa(data,nfactors = 4,rotate = "oblimin",fm="minres")
print(fourfactor$loadings,cutoff = 0.3)

# We can see that it results in only single-loading. This is known as simple structure
print(fourfactor)

# The root mean square of residuals (RMSR) is 0.05. 
# This is acceptable as this value should be closer to 0. 
# Next we should check RMSEA (root mean square error of approximation) index. 
# Its value, 0.001 shows good model fit as it's below 0.05

# Naming the Factors
# Aesthetics - Exterior Lokks, color
# Functional Benefits - Safety, Space & Comfort, Technology, Aftersales Service, Fuel Type
# Economic Value - Price, Resale Value, Fuel Efficiency, Maintenance
# Credibility - Test drive, Product Reviews, Testimonials