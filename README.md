StreetLight Use Estimates & Parks
================
12 June 2023

- <a href="#overview" id="toc-overview">Overview</a>
  - <a href="#funding-and-acknowledgements"
    id="toc-funding-and-acknowledgements">Funding and Acknowledgements</a>
  - <a href="#resources" id="toc-resources">Resources</a>
- <a href="#getting-started" id="toc-getting-started">Getting started</a>
  - <a href="#file-organization" id="toc-file-organization">File
    organization</a>
  - <a href="#set-up" id="toc-set-up">Set-up</a>
- <a href="#changelog" id="toc-changelog">Changelog</a>

# Overview

Understanding visitation to Minnesota’s parks and trails is essential
for planning, programming, and investment decisions. Visitation
estimates generally rely on methods such as intercept surveys, in-field
visitation counts, and automated trail counters. Visitation estimates
using passively-generated data sources may offer opportunities to
complement existing strategies.

This project used aggregated and anonymized location-based services
(LBS) data to estimate and evaluate visitation to Minnesota parks and
trails. LBS data gives information about when and where people travel.
This approach provides unprecedented detail about how visitors use parks
and trails and broadly describes who those visitors are. Visitation and
use patterns can be analyzed at annual, monthly, weekly, and hourly time
intervals. This data makes it possible to understand how people travel
to parks and trails and where they are coming from. This data is
intended to supplement, but not replace existing data used for decision
making.

## Funding and Acknowledgements

This Project was funded with Legacy Partnership Research Funds from the
State of Minnesota [Parks and Trails Legacy
Fund](https://www.legacy.mn.gov/parks-trails-fund) in collaboration with
the [Minnesota Department of Natural
Resources](https://www.dnr.state.mn.us/), the [Greater Minnesota
Regional Parks and Trails Commission](https://www.gmrptcommission.org/),
and the [Metropolitan Council](https://metrocouncil.org/).

We thank park managers and staff from across the state for their
cooperation in sharing data and providing feedback to establish and
refine our methodology.

## Resources

- The findings and resources produced by this research are available on
  an interactive project website [here]().
- [StreetLight Data, Inc.](https://www.streetlightdata.com/) was the LBS
  data provider for this research.

# Getting started

## File organization

The code used to conduct primary analyses are located in three folders:
`parks`, `trails`, and `visitors`. Each folder contains a tutorial
document (`park_tutorial.RMD`, `trail_tutorial.Rmd`, and
`visitor_tutorial.RMD`, respectively) which calls additional scripts to
conduct each step of the analysis. These scripts are numbered in the
order in which they are called.

Complete technical documentation is generated via
`legaacy-LBS-parktrail-research-documentation.RMd`; higher level summary
texts are generated in the `documentation` folder.

The `data-raw` folder contains data obtained from external sources;
`data-intermediate` contains partially processed data, individual
StreetLight (LBS) analysis downloads, or other internally-produced data;
`data-processed` contains the final products of this research.

## Set-up

This project uses
[`streetlightR`](https://metropolitan-council.github.io/streetlightR/)
to conduct LBS analyses and
[`councilR`](https://github.com/Metropolitan-Council/councilR) for
plotting. Users will additionally require a StreetLight API key (request
via StreetLight Support Team) and a [Census API
Key](https://api.census.gov/data/key_signup.html).

Initially, you will need to save some parameters to your machine.

``` r
require(keyring)
key_set(service = "StreetLightAPI")

usethis::edit_r_environ() 
# When the `.Renviron` file comes up in the editor, save the following parameters:

# `STREETLIGHT_LOGIN` = "your email"
# `STREETLIHT_API_KEY` = "your API key"
# `CENSUS_API_KEY` = "your API key"
# 
# Save and close the `.Renviron` file and Restart R.
```

# Changelog

- June 12, 2023: Initial release of project website and results.
