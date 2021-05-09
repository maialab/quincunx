## code to prepare `stages` dataset goes here

library(tibble)

stages <-
  tibble::tribble(
    ~ stage,
    ~ symbol,
    ~ name,
    ~ definition,
    # Row 1
    'gwas',
    'G',
    'Source of Variant Associations (GWAS)',
    'Describes the samples used to define the variant associations/effect-sizes used in the PGS. These data are extracted from the NHGRI-EBI GWAS Catalog when a study ID (GCST) is available.',
    # Row 2
    'dev',
    'D',
    'Score Development/Training',
    'Describes the samples used to develop or train the score (e.g. not used for variant discovery ("gwas" stage), and non-overlapping with the samples used to evaluate the PGS predictive ability ("eval" stage))',
    # Row 3
    'eval',
    'E',
    'PGS Evaluation',
    'Information about the samples used in PGS performance evaluation. These samples have a PGS Catalog Sample Set (PSS) ID to link them to their associated performance metrics (and across different PGS)',
    # Row 4
    'gwas/dev',
    'G,D',
    'Development',
    'Combination of the stages "gwas" and "dev".',
    # Row 5
    'gwas/dev/eval',
    'G,D,E',
    'All stages',
    'Combination of all the stages: "gwas", "dev" and "eval".'
  )

readr::write_csv(stages, "data-raw/stages.csv")
usethis::use_data(stages, compress = "xz", overwrite = TRUE, version = 2)
