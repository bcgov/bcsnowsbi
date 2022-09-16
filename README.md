
<!--
Copyright 2022 Province of British Columbia
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
-->
<!-- badges: start -->

[![Lifecycle:Stable](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](Redirect-URL)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
<!-- badges: end -->

# bcsnowsbi

This package contains functions that calculate the snow basin index
(SBI) value for a particular basin or basins within British Columbia,
Canada. Snow basin index values are used by the BC River Forecast Centre
within their monthly/bimonthly Snow Bulletin to decsribe how much snow
is present within a specific area relative to typical conditions.

### Features

The package contains several functions for retrieving snow basin index
values for a particlar basin within the province (details below).

This package also depends on two snow-specific packages to function:

1.  bcsnowdata() - retrieves raw data from both automated and manual
    snow survey sites. This package also contains functions to determine

2.  bcsnowstats() - This function calculates statistics for specific
    sites that are used within the SBI calculation (specifically,
    percentiles as well as normals for the period of interest). Note
    that the sbi functions within the bcsnowsbi() package will
    automatically assign the appropriate normal time period based on the
    water year of the survey period you are looking for SBI values for
    (more detail in Usage section of this document).

### Installation

bcsnowsbi() can be installed directly from github.

### Usage

#### How SBI Values are Calculated for British Columbia Basins

Snow basin indices (SBI) values are calculated for subbasins across
British Columbia during the snow accumulation and melt season. They are
a means of determining how much snow is within a basin relative to
normal conditions for that basin for that time of year, a major
determinant of flood risk in the lead up to the spring melt (freshet)
season. SBI values are calculated as the mean snow water equivalent of
all of the manual and automated snow measurements within a particular
basin divided by the mean snow normal values for these same sites. Snow
normals are calculated as the arithmetic mean of snow water equivalent
values for a particular day across a defined 30-year period (more detail
regarding how snow normal values are calculated can be found within
bcsnowstats() package documentation).

#### Ways of Returning SBI Values Using bcsnowsbi()

There are several ways to return SBI values for a particular basin using
bcsnowsbi().

Firstly, SBI values can be calculated and returned using the
sbi_bybasin_function(). This allows the user to return SBI values for
one, several, or all basins within BC.

    # Example of how to use the sbi_bybasin_function()
    # Retrieve SBI values for the Okanagan for Feb 1st, 2021
    # No sites that should be removed from analysis ("exceptions"), or incorrect manual sites

    library(bcsnowsbi)

    SBI_test <- sbi_bybasin_function(date_sbi = "01-04-2022",
                                     all_basins = "Yes", # What basin to analyze SBI values for. Can also be "Yes" to get all SBI values for all basins
                                     exceptions = c(), # No sites to remove from analysis
                                     incorrect_sites = c(), incorrect_data = c(), # No incorrect manual sites. Otherwise, can specify the site name and data that a manual site should be (in cases where it data was incorrectly entered).
                                     save_csv = "No")

#### Additional Analyis Details

bcsnowsbi() uses bcsnowstats() in order to calculate statistics from raw
data retrieved by bcsnowdata().

Most critically, bcsnowsbi() uses SWE normals calculated through
bcsnowsbi(); these snow normals are values of mean SWE taken over a
defined 30-year period for each calendar day (more detail about snow
normal calculations is present within documentation for the
bcsnowstats() package). This package will first define what this normal
period should be depending on the year of the date that the user wishes
to calculate SBI values for. For example, if SBI values for January 1st,
2021 are calculated, a normal period of 1990-2020 will be automatically
assigned (normals are then calculated through the bcsnowstats()
function).

### Project Status

In progress

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/bcsnowsbi/issues/).

### How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### Source Data

This package uses snow-related data present on the BC Data Catalogue.
This includes:

-   Manual snow survey data ([current
    year](https://catalogue.data.gov.bc.ca/dataset/current-season-manual-snow-survey-data)
    and
    [archived](https://catalogue.data.gov.bc.ca/dataset/archive-manual-snow-survey-data)
    data; also includes [site
    locations](https://catalogue.data.gov.bc.ca/dataset/manual-snow-survey-locations))
-   Automated snow weather data ([current
    year](https://catalogue.data.gov.bc.ca/dataset/current-season-automated-snow-weather-station-data)
    and
    [archived](https://catalogue.data.gov.bc.ca/dataset/archive-automated-snow-weather-station-data)
    data; also includes [site
    locations](https://catalogue.data.gov.bc.ca/dataset/automated-snow-weather-station-locations))
-   [Snow basin administrative
    areas](https://catalogue.data.gov.bc.ca/dataset/snow-survey-administrative-basin-areas)

### License

[![Creative Commons
License](https://i.creativecommons.org/l/by/4.0/88x31.png)](http://creativecommons.org/licenses/by/4.0/)

    Copyright 2022 Province of British Columbia

    This work is licensed under the Creative Commons Attribution 4.0 International License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/.

------------------------------------------------------------------------

*This project was created using the
[bcgovr](https://github.com/bcgov/bcgovr) package.*
