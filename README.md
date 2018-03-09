TSD R package
=====

The `TSD` package provides functions for reading, writing and other processing of data in the Time Step Data (TSD) format (and other utilities used by the packages SimradRaw, sonR, cpplot3d and echoIBM).The TSD format was created for the purpose of storing data organized in time steps possibly with different dimensions at different time steps. It was created in 2010 for personal use by the package author, but the NetCDF 4 format or HDF5 format largely covers the functionality of the TSD format.

Version: 1.1
Required R version: 3.3.3

Installation
=====

``` r
# Install the packages that TSD depends on. Note that this updates all the specified packages to the latest (binary) version. To skip installing already installed packages, run install.packages(setdiff(dep.pck, installed.packages()[,"Package"]), repos="http://cran.us.r-project.org") instead:
dep.pck <- c("devtools", "pbapply")
install.packages(dep.pck, repos="http://cran.us.r-project.org")

# Install TSD and also the packages that TSD depends on which are on GitHub (by Holmin):
# On Windows you will need Rtools to complete the installations. Check if you have this by running Sys.getenv('PATH'), and go to https://cran.r-project.org/bin/windows/Rtools/ to install Rtools if not. Note that if you need to run R as administrator due to security settings, it is advised to install the pakcages in plain R, and not using Rstudio. Close Rstudio, open R and run the installation, and reopen Rstudio.

dep.pck.git <- c("arnejohannesholmin/TSD")
# If you want to install the lastest development versions, run devtools::install_github(dep.pck.git, ref="develop") instead:
devtools::install_github(dep.pck.git)

```

# For changes log see https://github.com/arnejohannesholmin/TSD/NEWS

Examples
=====

``` r
# Write some data to a TSD file (all variable must have 4 character names):
dat <- list(
	var1=list(
		array(runif(2*3*4), dim=c(2,3,4)), 
		array(runif(7*4), dim=c(7,4))
		), 
	var2=list(
		
		c("Time step 1", "ebaerb"), 
		c("Last time step")
		), 
	var3=list(
		complex(real=1:12, imaginary=runif(12)), 
		NULL
		)
	)
TSDfile <- tempfile()
write.TSD(dat, TSDfile, numt=2)
datread <- read.TSD(TSDfile, t="all")

# Differs in precision of the first variable 'var1':
all.equal(dat, datread)

# Set the first varialbe to double precision:
write.TSD(dat, TSDfile, numt=2, header=list(dtyp=list(var1="doub")))
datread <- read.TSD(TSDfile, t="all")
all.equal(dat, datread)
```

License
=====

The TSD package is licensed under the LGPL-3.)

