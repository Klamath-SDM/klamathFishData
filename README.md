
<!-- README.md is generated from README.Rmd. Please edit that file -->

# klamathFishData

<!-- badges: start -->

<!-- badges: end -->

The **`klamathFishData`** package is an R data package developed by
FlowWest that provides curated environmental and biological datasets on
fish in the Klamath River Basin. The package is intended to support
research, modeling, and restoration planning by making commonly used
fisheries datasets easily accessible in a consistent format.

## Installation

You can install the development version of klamathFishData from
[GitHub](https://github.com/Klamath-SDM/klamathFishData/tree/main) with:

``` r
# install.packages("devtools")
devtools::install_github("Klamath-SDM/klamathFishData")
```

## Data sources

The data in this package comes from a variety of public sources,
including:

- California Department of Fish and Wildlife (CDFW)
- U.S. Geological Survey (USGS)
- Literature-based habitat models

## Example

The example below demonstrates how to filter spawner escapement data for
adult spring Chinook Salmon.

``` r
library(klamathFishData)
library(tidyverse)

salmon_spawner_escapement |>
  filter(species == "spring chinook salmon" & lifestage == "adult")
#> # A tibble: 315 × 13
#>     year location                species origin lifestage estimate_type estimate
#>    <dbl> <chr>                   <chr>   <chr>  <chr>     <chr>            <dbl>
#>  1  2016 trinity river hatchery  spring… hatch… adult     <NA>              1830
#>  2  2017 trinity river hatchery  spring… hatch… adult     <NA>              1134
#>  3  2018 trinity river hatchery  spring… hatch… adult     <NA>              2488
#>  4  2016 salmon river            spring… wild   adult     <NA>               406
#>  5  2017 salmon river            spring… wild   adult     <NA>               133
#>  6  2018 salmon river            spring… wild   adult     <NA>               171
#>  7  2016 other klamath tributar… spring… wild   adult     <NA>                 1
#>  8  2017 other klamath tributar… spring… wild   adult     <NA>                 2
#>  9  2018 other klamath tributar… spring… wild   adult     <NA>                11
#> 10  2016 trinity river           spring… wild   adult     <NA>              1331
#> # ℹ 305 more rows
#> # ℹ 6 more variables: confidence_interval <dbl>, lower_bounds_estimate <dbl>,
#> #   upper_bounds_estimate <dbl>, estimation_method <chr>,
#> #   is_complete_estimate <lgl>, source <chr>
```

## What’s Included

The package contains cleaned, documented data objects including:

- `sucker_adult_estimates` — This dataset compiles sucker modeled
  estimates and credible intervals from fisheries models such as
  abundance and survival estimates
- `salmon_juvenile_abundance` — This dataset compiles salmon modeled
  estimates and credible intervals from fisheries abundance models
- `data_location_lookup` - Latitude and longitude information for
  fisheries data collection locations, used to support spatial mapping
  of datasets such as `sucker_adult_estimates` and
  `salmon_juvenile_abundance.`
- `salmon_spawner_escapement` - Data extracted from [CDFW’s Fall-Run
  Megatable](https://wildlife.ca.gov/Conservation/Fishes/Chinook-Salmon/Anadromous-Assessment)
  and [CDFW’s Spring-Run
  Megatable](https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=100231&inline)
- `avian_predation_estimates` - This dataset combines avian predation
  rate estimates and PIT-tag availability and recovery data for juvenile
  fishes in the Upper Klamath Basin.
- And more in development…

Use `data(package = "klamathFishData")` to view all datasets included.
