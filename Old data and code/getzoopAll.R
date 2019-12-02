getzoopAll <- function( file = "allZoopAbunBM201805", fname = "allZoopAbunBM201805.Rdata",
                        path ="C:/1Current/R-files/1R-projects/ZoopDatabase/Rdata/", 
                        include.fld = c("year", "julday", "station", "temp",  "sal.surf","sal.bott"), 
                        spcode = character(0), taxon = character(0), order = character(0), 
                        stage = character(0), years = integer(0),juldays = integer(0), sals = numeric(0), 
                        tot = F, biomass=F)
{
  #  Function to get zooplankton abundance or biomass from combined zooplankton file
  #  produced by Zooplankton Combine all data Revised 16 May 2018.R
  # Input:
  #  include.fld Character vector of variable names from fld and dates to include (as well as rowIndex, or "all")
  #  spcode:  character vector of species names from sp$spcode
  #  taxon:  character vector of taxon names form neozoo.spp$taxon
  #  stage:  character vector of life stages, if any
  #  years:  Range of years to include (or all if missing)
  # juldays: Range of julian days to include (or all if missing)
  # sals:    Range of SURFACE salinities to include (or all if missing)
  #     If sals is not missing, samples with missing S values are excluded
  #  tot    TRUE to sum the plankton abundance or biomass and give a single value
  #  biomass: FALSE to get abundance (#/m3), TRUE for biomass (mgC/m3)
  # Added May 2010:
  # drop.missing T means drop if cbcode or pumpcode are F, netpump="", and net and pump taxa included
  # trimNames  Drop the ".n",".p",".b" from names (generally should not)
  # Added November 2012:
  # The following are generally not changed:
  #  sp    Data frame of species identities, plus which sample they come from (N, P, or B)
  #  zp    Zooplankton pump data in table format by sample index
  #  zn    Zooplankton net data in table format by sample index
  #  fld    Field data; first column is a sample index
  # dates Median date of surveys
  # Output:
  # Data frame containing all samples in the included years, selected data from fld, 
  # and abundance data for the groups specified above.  Note that the selection criteria
  # are applied in sequence to the species list.  Any variables not specified are taken to be "all."
  #-------------------------------------
  cat("Using source file:", file, "\n")
  fn <- paste0(path, fname)
  cat(fn,"\n")
  load(file=fn,  envir=.GlobalEnv)
  sp <-  get(file)$taxonomy
  fld <- get(file)$header
  sta <- get(file)$stationsAll
  if (biomass){
    zp <-  get(file)$allBugsmgCm3	
  }
  else
  {
    zp <-  get(file)$Abundance	
  }
  
  # Set up the list of taxa in sp
  v <-c("spcode", "taxon", "order", "stage")
  for (i in 1:length(v))
  {
    gv <- get(v[i])
    if (length(gv) > 0)
    {
      sp <- sp[match(sp[, match(v[i], names(sp))], gv, nomatch = 0) > 0,  ]
    }
  }
  if (nrow(sp) == 0)
    stop("Combination of inputs yields no output")
  
  # Set up the sample and field data for the selected variables and range of years
  x <- fld
  if(length(years) > 0)
    x <- x[x$year >= min(years) & x$year <= max(years),  ]
  if(length(juldays) > 0)
    x <- x[x$julday >= min(juldays) & x$julday <= max(juldays),  ]
  if(length(sals) > 0)
  {
    x <- x[!is.na(x$sal.surf),  ]
    x <- x[x$sal.surf >= min(sals) & x$sal.surf <= max(sals),  ]
  }
  if (all(include.fld != "all"))
  {
    if(length(include.fld) > 0 && include.fld[1] != "")
      x <- x[, c("rowIndex", "source", include.fld)]
    else x <- x[, c("rowIndex", "source")]
  }
  # Get the zooplankton data by column number (v) corresponding to the taxa incuded, 
  # and sum or block the data
  sid <- x$rowIndex
  zp <- zp[match(zp$rowIndex, sid, nomatch = 0) > 0, c("rowIndex", sp$spcode) ]
  if (tot)
  {
    zp$total <- rowSums(zp[,-1])
    zp <- zp[,c(1, ncol(zp))]
  }
  
  xx <- merge(x, zp, by="rowIndex")
  txt <- paste(sp$spcode, collapse=c(","))
  if (tot) {txt <- paste("Total of:", txt)}
  if (biomass) {txt <- paste ("Biomass -", txt)}
  else {txt <- paste ("Abundance -", txt)}
  cat("Saving", ncol(xx), "columns by", nrow(xx), "rows\n", txt )
  
  xx
}


