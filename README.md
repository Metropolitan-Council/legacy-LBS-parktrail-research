
# Minnesota Parks and Trails

*A Minnesota Legacy research project*

Understanding visitation to Minnesota’s parks and trails is essential
for planning, programming, and investment decisions. Visitation
estimates generally rely on methods such as intercept surveys, in-field
visitation counts, and automated trail counters. Visitation estimates
using passively generated data sources may offer opportunities to
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

This project was funded with Legacy Partnership Research Funds from the
[State of Minnesota Parks and Trails Legacy
Fund](https://www.legacy.mn.gov/parks-trails-fund). The joint research
project was conducted in collaboration with the [Minnesota Department of
Natural Resources](https://www.dnr.state.mn.us/), the [Greater Minnesota
Regional Parks and Trails Commission](https://www.gmrptcommission.org/),
and the [Metropolitan Council](https://metrocouncil.org/). We thank
staff from across the different organizations and cooperating
implementing agencies for their cooperation in sharing data and
providing feedback.

<figure>
<img src="documentation/images/partner-logos.png"
alt="Funding partner logos" />
<figcaption aria-hidden="true">Funding partner logos</figcaption>
</figure>

## This repository

This repository contains R code, tabular and spatial data files, and
documentation behind this research project.

### File organization

The code used to conduct primary analyses are located in three folders:
`/parks`, `/trails`, and `/visitors`. Each folder contains a tutorial
document (`park_tutorial.Rmd`, `trail_tutorial.Rmd`, and
`visitor_tutorial.Rmd`, respectively) which calls additional scripts to
conduct each step of the analysis. These scripts are numbered in the
order in which they are called.

Complete technical documentation is generated via
`legacy-LBS-parktrail-research-documentation.Rmd`; higher level summary
texts are generated in the `documentation` folder.

The `/data-raw` folder contains data obtained from external sources;
`/data-intermediate` contains partially processed data, individual
StreetLight (LBS) analysis downloads, or other internally-produced data;
`/data-processed` contains the final products of this research.

The `/figures` folder contains two sub-folders: `storymap` and
`factsheets`. `storymap` contains individual plots and images used for
online StoryMaps. `factsheets` contains single-page PDF reports for each
park and trail with information like weekly total annual visits, weekly
visit trends, mode share, hourly use, visitor home locations,
generalized visitor demographics, and unit geography, organized by
agency and unit type. Data is generally available from 2019 to April
2022.

To re-render plots and factsheets properly, ensure you have the Avenir
font installed on your machine. Avenir is available for free in various
places online.

### Set-up

Before running any code, be sure to open `R/_load_packages.R` and ensure
you have all necessary packages installed.

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
require(usethis)
keyring::key_set(service = "StreetLightAPI")

usethis::edit_r_environ()
# When the `.Renviron` file comes up in the editor, save the following parameters:

# `STREETLIGHT_LOGIN` = "your email"
# `STREETLIHT_API_KEY` = "your API key"
# `CENSUS_API_KEY` = "your API key"
#
# Save and close the `.Renviron` file and Restart R.
```

## Important details

General contact: <research@metc.state.mn.us>.

- **Contributing** Before contributing to this repository, please review
  the [contribution guide](CONTRIBUTING.md).
- **Code of Conduct** Please note that this repository is released with
  a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing
  to this project, you agree to abide by its terms.
- **License** Code is released with an [MIT license](LICENSE.md). Data
  provided is for informational purposes. Please open an issue if you
  have any questions regarding licensing.
- Thanks to our contributors.
  - Raven McKnight [@ravenmcknight](https://github.com/ravenmcknight)
  - Ellen Esch [@ehesch](https://github.com/ehesch)
  - Liz Roten [@eroten](https://github.com/eroten)
  - Senior Manager, Joel Huting
    [@joelhuting](https://github.com/joelhuting-r)
