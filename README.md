
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quincunx <img src='man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/quincunx)](https://CRAN.R-project.org/package=quincunx)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![](https://img.shields.io/badge/doi-10.1093/bioinformatics/btab522-blue.svg)](https://doi.org/10.1093/bioinformatics/btab522)
<!-- badges: end -->

The goal of `{quincunx}` is to provide programmatic access to the
[Polygenic Score (PGS) Catalog](https://www.pgscatalog.org/), an open
resource for [polygenic
scores](https://en.wikipedia.org/wiki/Polygenic_score) and associated
metadata describing their development and respective assessment.

Browse the online documentation at
[maialab.org/quincunx](https://maialab.org/quincunx/) to get started.

## Installation

Install `{quincunx}` from CRAN:

``` r
# Install from CRAN
install.packages("quincunx")
```

You can instead install the development version of `{quincunx}` by
setting [Ramiro Magno’s universe](https://ramiromagno.r-universe.dev/)
repository:

``` r
options(repos = c(ramiromagno = 'https://ramiromagno.r-universe.dev',
                CRAN = 'https://cloud.r-project.org'))

install.packages('quincunx')
```

## Cheatsheet

<a href="https://github.com/ramiromagno/cheatsheets/blob/master/quincunx/quincunx_cheatsheet.pdf"><img src="https://raw.githubusercontent.com/ramiromagno/cheatsheets/master/quincunx/quincunx_cheatsheet.png" width="615" height="225"/></a>

## Citing this work

`{quincunx}` was published in Bioinformatics in 2021:
<https://doi.org/10.1093/bioinformatics/btab522>.

To generate a citation for this publication from within R:

``` r
citation('quincunx')
#> 
#> To cite quincunx in publications use:
#> 
#>   Ramiro Magno, Isabel Duarte, Ana-Teresa Maia, quincunx: an R package
#>   to query, download and wrangle PGS Catalog data, Bioinformatics,
#>   btab522, 16 July 2021, Pages 1-3,
#>   https://doi.org/10.1093/bioinformatics/btab522
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {quincunx: an R package to query, download and wrangle PGS Catalog data},
#>     author = {Ramiro Magno and Isabel Duarte and Ana-Teresa Maia},
#>     journal = {Bioinformatics},
#>     year = {2021},
#>     pages = {1--3},
#>     url = {https://doi.org/10.1093/bioinformatics/btab522},
#>   }
```

## Citing PGS Catalog publications

Also, please do not forget to cite the authors behind the original
studies and the papers associated with the PGS Catalog project:

-   Lambert, S.A., Gil, L., Jupp, S. et al. The Polygenic Score Catalog
    as an open database for reproducibility and systematic evaluation.
    Nat Genet 53, 420–425 (2021). doi:
    [10.1038/s41588-021-00783-5](https://doi.org/10.1038/s41588-021-00783-5)
-   Wand, H., Lambert, S.A., Tamburro, C. et al. Improving reporting
    standards for polygenic scores in risk prediction studies. Nature
    591, 211–219 (2021). doi:
    [10.1038/s41586-021-03243-6](https://doi.org/10.1038/s41586-021-03243-6)

## Terms of use

Please note that if you use the data provided by the PGS Catalog either
directly or via `{quincunx}` you agree to abide to the [EMBL-EBI Terms
of Use](https://www.ebi.ac.uk/about/terms-of-use/).

## Code of Conduct

Please note that the `{quincunx`} project is released with a
[Contributor Code of
Conduct](https://maialab.org/quincunx/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## Acknowledgements

This work would have not been possible without the precious feedback
from the [PGS Catalog team](https://www.pgscatalog.org/), particularly
[Samuel Lambert](https://www.ebi.ac.uk/about/people/samuel-lambert) and
[Laurent Gil](https://www.sanger.ac.uk/person/gil-laurent/).

## Package name and hex sticker

The package name `{quincunx}` is another name for [Galton
Board](https://en.wikipedia.org/wiki/Bean_machine), that so nicely
exemplifies the [Central Limit
Theorem](https://en.wikipedia.org/wiki/Central_limit_theorem), which in
turn is a key concept of genetics, namely, [Fisher’s infinitesimal
model](https://doi.org/10.1016/j.tpb.2017.09.003)… which leads us to
Polygenic Scores, the key concept of the PGS Catalog.

The bird in the hex sticker is a *Porphyrio porphyrio*, an emblematic
species native to the Ria Formosa Natural Park, which is a wildlife
reserve surrounding the University of Algarve, where the authors are
affiliated.
