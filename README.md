Zooplankton Synthesis in the Sacramento San Joaquin Delta
================

# Introduction

Code and files related to zooplankton synthesis

The code has been incorporated into a new R package `zooper`.

[<img src="Logo/zooperhex3.png" height="200" />](https://github.com/InteragencyEcologicalProgram/zooper)

R users can install the R package to download and integrate zooplankton
datasets with our code.

We have also produced a shiny app (see below), which is a GUI (graphical
user interface) that allows folks without R experience to integrate and
download zooplankton datasets. It offers the same functionality as the
`zooper` package, without requiring the users to write code. The shiny
app also allows folks of all experience levels to easily visualize the
data.

# Shiny app

The R shiny app can be accessed
[here](https://deltascience.shinyapps.io/ZoopSynth/).

The full shiny app can also be downloaded and installed to your computer
by downloading and running [the
installer](https://deltacouncil.box.com/s/3zjd9g0u0koamsmqoe2deyjmrcegwooz).
This desktop version of the app can only be installed on windows
computers and google chrome must be installed. If R is not already
installed on your computer, this installer will install R as well.

*NOTE: during installation, you will probably receive 1-2 scary looking
errror messages saying “Access is denied.” These can be safely ignored,
the app will still install correctly.*

After installation, the app can be opened like any other desktop windows
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

1.  Macro (500-505 μm): Amphipods and mysids (NOTE: Prior to 1974 EMP
    macrozooplankton were sampled with a 930 μm mesh net)
2.  Meso (150 - 160 μm): Copepods, cladocera
3.  Micro (43 μm): Copepods, rotifers

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

# Changing taxonomic resolution over time

For many studies, taxonomic resolution has changed over time. This could
confound analyses of zooplankton communities and abundances over time.
To account for this, we have implemented a solution for the “Community”
option, which can be selected as an option when the “Community” data
type is selected.

We first find all taxa that were not counted every year across the date
range the user inputs (but taking into account the years non-native
species were introduced and each survey first started sampling). These
taxa are then summed to higher taxonomic levels, as is done for taxa
that were not counted across all datasets.

Often, non-native species are not added to zooplankton species list the
same year they are first detected in the system. To allow for some lag
between introduction years and the years these species were first
counted, you can adjust the slider to choose the number of lag years
(currently defaults to 2 years).
