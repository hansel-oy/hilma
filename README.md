# hilma

[![R build status](https://github.com/hansel-oy/hilma/workflows/R-CMD-check/badge.svg)](https://github.com/hansel-oy/hilma/actions)
[![codecov](https://codecov.io/gh/hansel-oy/hilma/branch/master/graphs/badge.svg)](https://codecov.io/gh/hansel-oy/hilma)

An R package for accessing the [Hilma](https://www.hankintailmoitukset.fi) [API](https://hns-hilma-prod-apim.portal.azure-api.net).


## Installation

You can install the development version with:

``` r
devtools::install_github("hansel-oy/hilma")
```


## Example

Below is a simple example for fetching notices that satisfy certain criteria:

``` r
library(hilma)
library(magrittr)
library(lubridate)

# this can be obtained by registering to the API at
# https://hns-hilma-prod-apim.portal.azure-api.net
api_key <- "your-personal-apikey"

## Retrieve all the results published between yesterday and today
query1 <- all_notices_published_since_filter(today() %m-% days(1)) %>% 
  query_and(all_notices_published_until_filter(today))
  
fetch_notices(query1, api_key)

# Retrieve notices that include dynamic purchasing systems (DPS) or 
# framework agreements and that do not exceed the EU threshold
query2 <- dps_filter() %>% 
  query_or(framework_agreement_filter()) %>% 
  query_and(is_national_filter())

fetch_notices(query2, api_key)

```

