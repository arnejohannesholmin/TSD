#*********************************************
#*********************************************
#' Returns the variable names required in echoIBM() and related functions, given as four character strings of names as stored in the TSD format. The aim of this function is to avoid errors due to misspelling.
#'
#' @param var		The variable classes for which variable names are requested. Use names(labl.TSD()) to see implemented variable classes are.
#' @param adds		A vector of additional variables to include.
#' @param list.out	Logical: If FALSE, unlist the output when multiple elements are requested in var.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname labl.TSD
#'
labl.TSD <- function(var=NULL, adds=NULL, list.out=TRUE){
	
	############### LOG: ###############
	# Start: 2011-08-05 - Clean version.
	# Update: 2011-08-07 - Changed to have as input the name of the variable class, and added the option of adding varibles using 'adds'.
	# Last: 2013-01-07 - Added relevant beams, ctd, and pdns variables, and segmentation variables.
	
	########## Preparation ##########
	# Time variables:
	acousticnames <- c("mvbs", "vbsc", "tlns")
	
	timenames <- c("utim", "mtim", "ftim", "indt")
	#timenames <- c("utim", "mtim", "ctim", "ftim", "indt")
	# The variable names of the vessel:
	compressed_vesselnames <- c("log1",	"lon1",	"lat1",	"utm1",	"log2",	"lon2",	"lat2",	"utm2")
	vesselnames = c("psxv", "psyv", "pszv", "rtxv", "rtyv", "rtzv", "lonv", "latv", "lat0", "lon0", "ispv", "sadv")
	
	# The compactly specified dynamic variable names of the school:
	compactschool <- c("psxS", "psyS", "pszS", "utmS", "ut9S", "aspS", "thtS", "phiS", "szxS", "szyS", "szzS", "rtxS", "rtyS", "rtzS", "scls", "SDxf", "SDyf", "SDzf", "sEdx", "sEdy", "sEdz", "shpS", "rhoS", "nbfS", "volS","volE", "MEsz", "SDsz", "PDsz", "seed", "sEds", "plHS", "hazS", "helS", "oazS", "oelS")
	
	# The dynamic variable names of the school:
	#dynschoolnames <- c("psxf", "psyf", "pszf", "rtxf", "rtyf", "rtzf", "vlxf", "vlyf", "vlzf", "size", "Sctr", "Spar", "Sobj", "Sang")
	dynschoolnames         <- c("psxf", "psyf", "pszf", "rtxf", "rtyf", "rtzf", "vlxf", "vlyf", "vlzf", "size", "scls")
	dynschoolnames_echoIBM <- c(dynschoolnames, "transducerposL", "lenl", "etaC", "etar4", "etaomega", "sgbs", "fish")
	
	# The static variable names of the school:
	staticschoolnames <- c("indl", "acca", "size", "lenl", "mass", "tilt", "epsl", "gamw", "gaml", "obln", "zeta", "pbpf", "ssil", "grff", "graf", "gref", "grsf", "grif", "dbpf", "ebpf", "ssif", "sgbs", "epss", "spow", "surv")
	
	specialBemasnames  <- unique(c("beams", "cali", "calf", "rad1", "rad2", "bwt1", "bwt2", "pbp1", "gra1", "gre1", "grs1", "gri1", "dbp1", "ebp1", "pbp2", "gra2", "gre2", "grs2", "gri2", "dbp2", "ebp2"))
	beamsnamesGeneral  <- unique(c("esnm", "asps", "numb", "indi", "freq", "absr", "sint", "rres", "plsl", "psze", "lenb", "dira", "dire", "dirx", "diry", "dirz", "bwtl", "bwtt", "eqba", "sacr", "tpow", "gain", "bmmd"))
	relevantbeamsnames <- unique(c(beamsnamesGeneral, c("bwtx", "bwty", "bwth", "bwtv", "gai1", "gai2")))
	beamsnames         <- unique(c(specialBemasnames, relevantbeamsnames))
	beamsnames1D       <- c("esnm", "asps", "numb", "sint", "rres", "plsl", "psze", "bmmd", "rofs", "sofs")
	beamsnames2D       <- c("indi", "freq", "absr", "lenb", "dirx", "diry", "dirz", "dira", "dire", "bwtl", "bwtt", "bwth", "bwtv", "bwtx", "bwty", "eqba", "sacr", "tpow", "gain", "gai1", "gai2", "Cgai", "Csac", "Ctcr", "Ccal")
			
	# The variable names of the ctd-data:
	ctdnames <- c("ctd", "lonc", "latc", "pszc", "ihpr", "slty", "temp", "cond", "flur", "oxvt", "isps", "rho0", "gacc", "hpr0", "asps")
	relevantctdnames <- c("rho0", "gacc", "hpr0", "temp", "slty", "ihpr", "pszc", "asps")
	requiredctdnames <- c("hpr0", "temp")
	# The variable names of relevant periodic noise parameters:
	relevantpdnsnames <- c("acfq", "badb", "pns1", "pns2", "harm", "bgns")
	# The variable names of the segmentation data:
	segmentationdatanames <- c("pr0s", "psis", "sgs0", "sgsc", "sgsE", "sgsi", "sgsI", "sgbt", "sgbE")
	allsegmentationdatanames <- c("sgsc", "Xtha", "Xtvl", "Xtbr", "Xtbb", "Xtbt", "Xtbs", "XtTR", "XtTB", "XtTT", "XtTS", "Xsbr", "XSBR", "Xvvl", "Xvha", "Xmsv", "XmSv", "Xasv", "XaSv", "Xxsv","XxSv", "Xqsv", "XqSv", "Xpsv", "XpSv", "Xvsv", "XvSv", "Xvsa", "XvSa", "Xcmx", "Xcmy", "Xcmz", "Xhra", "Xcex", "Xcey", "Xcez", "nlmp", "Xebt", "XeBT", "anis", "anio", "mdf1", "mdf3", "dBan", "dBb1", "dBb2", "dBb3", "dBb4", "dBbs", "smty", "sfnr", "Xcsz", "Xsb1", "Xsbg")
	segmentationnames <- sort(unique(c(segmentationdatanames, allsegmentationdatanames, timenames)))
	voxelsnames <- c("psxx", "psyx", "pszx", "volx", "harx")
	# Noise variables
	bgnsnames <- c("bgns", "badb", "pns1", "pns2", "pns3", "harm", "rspd", "rsdM", "bgnM", "bgn0", "bg0M", "pn1M", "pn2M", "pn3M", "pn3I", "pn30", "harM", "hM12", "DPrf", "mDrf", "acfq", "LOWP", "BEGP", "UPPP", "fnvb", "fnvp", "fnvt", "ntry", "NTRY", "FTth", "prex", "pn2I")
	nrnsnames <- c("nrns", "nrn0", "snrn", "snr0", "thrn", "thrs", "zers", "utim")
	nrnpnames <- c("nrnp", "nr0p", "snrp", "sn0p", "thrp", "thps", "zerp", "utmp")
	nrnanames <- c("nrna", "nr0a", "snra", "sn0a", "thra", "thas", "zera", "utma")
	hinsnames <- c("hins", "hini", "HINS")
	requirednoisenames <- c("bgns", "nrnp")
	requiredcorrnames <- c("crb1", "olpb")
	# Variables for functions:
	echoIBM.generate_oneschool_labl <- c("psxf", "psyf", "pszf", "vlxf", "vlyf", "vlzf", "rtzf", "rtxf", "scls", "size", "rhoS", "nbfS")
	#echoIBM.generate_oneschool_labl <- c("psxf", "psyf", "pszf", "vlxf", "vlyf", "vlzf", "rtzf", "rtxf", "size")
	echoIBM_dynVars <- setdiff(echoIBM.generate_oneschool_labl, c("scls", "rhoS", "nbfS"))
	applotoutput <- c("tvbs", "tvol", "aniS", "psxS", "psyS", "pszS", "nseg", "sgmt", "psxs", "psys", "pszs", "vbss", "vols", "dsts", "anio", "psxo", "psyo", "pszo", "szxo", "szyo", "szzo", "ango", "typo", "psxr", "psyr", "pszr")
	
	
	EKRaw2TSD_beamsnames <- c(
		"esnm", # 1  -  1 value per ping
		"indt", # 2  -  1 value per ping
		"mtim", # 3  -  1 value per ping
		"asps", # 4  -  1 value per ping
		"numb", # 5  -  1 value per ping
		"indi", # 6  -  numb values per ping
		"freq", # 7  -  numb values per ping
		"absr", # 8  -  numb values per ping
		"sint", # 9  -  1 value per ping
		"rres", # 9  -  1 value per ping
		"plsl", # 10  -  1 value per ping
		"psze", # 11  -  1 value per ping
		"lenb", # 12  -  numb values per ping
		"dirx", # 13  -  numb values per ping
		"diry", # 14  -  numb values per ping
		"dirz", # 15  -  numb values per ping
		"dira", # 16  -  numb values per ping
		"dire", # 17  -  numb values per ping
		"bwtl", # 18  -  numb values per ping
		"bwtt", # 19  -  numb values per ping
		"bwth", # 20  -  numb values per ping
		"bwtv", # 21  -  numb values per ping
		"bwtx", # 22  -  numb values per ping
		"bwty", # 23  -  numb values per ping
		"eqba", # 24  -  numb values per ping
		"sacr", # 25  -  numb values per ping
		"tpow", # 26  -  numb values per ping
		"gain", # 27  -  numb values per ping
		"gai1", # 28  -  numb values per ping
		"gai2", # 29  -  numb values per ping
		"bmmd", # 30  -  numb values per ping
		"Cgai", # 31  -  numb values per ping
		"Csac", # 32  -  numb values per ping
		"Ctcr", # 33  -  numb values per ping
		"Ccal", # 10  -  numb value per ping
		"rofs", # 10  -  1 value per ping
		"sofs", # 10  -  1 value per ping
		"nmtc" # 34  -  1 value per ping
	)
	
	EKRaw2TSD_vesselnames <- c(
		"indt", # 1  -  1 value per ping
		"mtim", # 2  -  1 value per ping
		"pszv", # 3  -  1 value per ping
		"rtxv", # 4  -  1 value per ping
		"rtyv", # 5  -  1 value per ping
		"rtzv", # 6  -  1 value per ping
		"latv", # 7  -  1 value per ping
		"lonv", # 8  -  1 value per ping
		"ispv", # 9  -  1 value per ping
		"sadv", # 10  -  1 value per ping
		"terr", # 10  -  1 value per ping
		"nmtc", # 11 - 1 value per ping
		compressed_vesselnames
	)

	EKRaw2TSD_pingsnames <- c(
		#"indt", # 1  -  1 value per ping
		"mtim", # 2  -  1 value per ping
		"vbsc", # 3  -  max(lenb) * numb values per ping
		"angl", # 4  -  max(lenb) * numb values per ping
		"angt", # 10  -  1 value per ping
		# 2018-08-15: Added the number of beams and length of the beams, in order to make the pings files more self-sufficient:
		"numb", # 5  -  1 value per ping
		"lenb", # 12  -  numb values per ping
		"nmtc" # 5  -  max(lenb) * numb values per ping
	)

	EKRaw2TSD_ctdnames <- c(
		"mtim", # 1  -  1 value per raw time step
		"lonc", # 2  -  1 value per raw time step
		"latc", # 3  -  1 value per raw time step
		"pszc", # 4  -  #depths value per raw time step
		"ihpr", # 5  -  #depths value per raw time step
		"temp", # 6  -  #depths value per raw time step
		"slty", # 7  -  #depths value per raw time step
		"isps", # 8  -  #depths value per raw time step
		"rho0", # 9  -  #depths value per raw time step
		"gacc", # 10  -  #depths value per raw time step
		"hpr0", # 11  -  #depths value per raw time step
		"asps" # 12  -  #depths value per raw time step
	)
		
	EKRaw2TSD_rawvesselnames <- c(
		"imtm", # 1  -  1 value per raw time step
		"iltv", # 1  -  1 value per raw time step
		"ilnv", # 1  -  1 value per raw time step
		"irzv", # 1  -  1 value per raw time step
		"iisv", # 1  -  1 value per raw time step
		"isdv" # 1  -  1 value per raw time step
	)

	# Gather the names in a list having the apropriate names:
	varnames <- list(
		a = acousticnames, 
		ds = dynschoolnames, 
		dse = dynschoolnames_echoIBM, 
		ss = staticschoolnames, 
		cs = compactschool, 
		v = vesselnames, 
		b = beamsnames, 
		rb = relevantbeamsnames, 
		b1 = beamsnames1D, 
		b2 = beamsnames2D, 
		ctd = ctdnames, 
		rc = relevantctdnames, 
		reqc = requiredctdnames, 
		t = timenames, 
		rp = relevantpdnsnames, 
		sd = segmentationdatanames, 
		sg = segmentationnames, 
		#as = c("vbsc", "mvbs","vbsC", "vbsA",segmentationdatanames), 
		as = c(acousticnames,segmentationdatanames), 
		vx = voxelsnames, 
		bgns = bgnsnames, 
		nrns = nrnsnames, 
		nrnp = nrnpnames, 
		nrna = nrnanames, 
		hins = hinsnames, 
		noise = c(bgnsnames, nrnsnames, nrnpnames, nrnanames, hinsnames),
		reqnoise = requirednoisenames, 
		reqcorr = requiredcorrnames, 
		echoibm.generate_oneschool_labl = echoIBM.generate_oneschool_labl,
		echoibm_dynvars = echoIBM_dynVars, 
		applotoutput = applotoutput,
		ekraw2tsd_b = EKRaw2TSD_beamsnames,
		ekraw2tsd_v = EKRaw2TSD_vesselnames,
		ekraw2tsd_p = EKRaw2TSD_pingsnames, 
		ekraw2tsd_c = EKRaw2TSD_ctdnames, 
		ekraw2tsd_r = EKRaw2TSD_rawvesselnames
	)
	
	
	########## Execution and output ##########
	# Extract the requested names:
	if(length(var)==0){
		var <- names(varnames)
	}
	out <- varnames[tolower(var)]
	if(length(var)==1 || !list.out){
		out <- unlist(out)
	}
	
	c(out, adds)
}
