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
