Zooplankton Synthesis in the Sacramento San Joaquin Delta
================
11/21/2019

# Introduction

Code and files related to zooplankton synthesis

The function `Zooplankton Synthesizer function.R` takes zooplankton data
from different surveys and integrates the data according to
user-specified parameter choices. The shiny app is a GUI (graphical user
interface) that allows folks without R experience to run the zooplankton
synthesizer function and download the resulting data. The shiny app also
allows folks with all experience levels to easily visualize the data.

The `Zoop data downloader.R` function downloads the zooplankton datasets
from their respective online sources and converts them to a consistent
format.

There is also some old code that Rosie used to merge the EMP zooplankton
and 20mm code with FRP data and some code from Wim. We don’t know what
it does yet.

# Shiny app

The R shiny app can be accessed in [a limited form hosted
online](https://iepsynthesis.shinyapps.io/ZoopSynth/).

The full shiny app can be installed by downloading and running [the
installer](https://deltacouncil.box.com/s/3zjd9g0u0koamsmqoe2deyjmrcegwooz).
This desktop version of the app can only be installed on windows
computers and google chrome must be installed. If R is not already
installed on your computer, this installer will install R as well. After
installation, the app can be opened like any other desktop windows
application. It may take a while to start up (especially the first time
you open it). The app will open in a google chrome tab. Simply close the
tab to exit the app.

# Community or taxon-specific analyses?

The biggest problem with integrating zooplankton datasets is variability
in taxonomic resolution. To resolve this, we have developed 2 approaches
to consolidating inconsistent data to “least common denominator taxa.”
Depending on what type of analysis you wish to run, you may wish for
different types of synthesized data.

## For community data analyzers

*I want to analyze the community composition at whatever taxonomic level
lets me use all these datasets.*

  - Consistent taxonomic categories
  - No plankters counted more than once
  - Sacrifices some taxonomic resolution
  - Removes taxa with no relatives in all datasets (eg., Annelida)

## For specific taxa analyzers

*I want all possible data on these specific taxa.*

  - Calculates total CPUE for higher taxonomic levels
  - Some plankters appear in multiple nested taxa (e.g., Calanoida,
    Copepoda)
  - Perserves taxonomic resolution and creates taxonomic categories that
    are comparable across all datasets
  - Labels taxa that are comparable across all datasets, warns about
    those that are not.

# Size classes

We have integrated zooplankton data from 3 net size classes:

1.  Macro (500-505
    ![mu](https://latex.codecogs.com/gif.latex?%24%5Cmu%24)m): Amphipods
    and mysids
2.  Meso (150 - 160
    ![mu](https://latex.codecogs.com/gif.latex?%24%5Cmu%24)m): Copepods,
    cladocera
3.  Micro (43 ![mu](https://latex.codecogs.com/gif.latex?%24%5Cmu%24)m):
    Copepods, rotifers

Nets accurately sample zooplankton larger than the mesh size.
Zooplankton smaller than the mesh size are still captured and often
recorded in datasets, but the resulting CPUEs are not accurate. To
account for this we:

1.  Resolve taxonomic resolution separately within each net size class.
2.  If `Data = 'Taxa'`, we mark “summed groups” with the net size class
    from which they were derived.
3.  All potentially undersampled data are marked with a flag
    `Undersampled == TRUE`
4.  For the plots in the shiny app, all data with `Undersampled == TRUE`
    can be removed. However, data downloaded from the app do contain
    undersampled data.

# Unresolved issues

For many studies, taxonomic resolution has changed over time. This could
confound analyses of zooplankton communities and abundances over time.
