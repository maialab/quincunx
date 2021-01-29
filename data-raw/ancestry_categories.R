# Title: Table 1 Ancestry categories: distinct regional population groupings used in this framework.
# Source: Morales, J., Welter, D., Bowler, E.H. et al. A standardized framework
#         for representation of ancestry data in genomics studies, with application to
#         the NHGRI-EBI GWAS Catalog. Genome Biol 19, 21 (2018).
#         https://doi.org/10.1186/s13059-018-1396-2
# How to run: just source this file.
# Documentation source: R/ancestry_categories.R

library(tibble)

ancestry_categories <-
  tibble::tribble(
    ~ ancestry,
    ~ definition,
    ~ examples,
    # Row 1
    'Aboriginal Australian',
    'Includes individuals who either self-report or have been described by authors as Australian Aboriginal. These are expected to be descendants of early human migration into Australia from Eastern Asia and can be distinguished from other Asian populations by mtDNA and Y chromosome variation',
    'Martu Australian Aboriginal',
    # Row 2
    'African American or Afro-Caribbean',
    'Includes individuals who either self-report or have been described by authors as African American or Afro-Caribbean. This category also includes individuals who genetically cluster with reference populations from this region, for example, 1000 Genomes and/or HapMap ACB or ASW populations. We note that there is likely to be significant admixture with European ancestry populations',
    'African American, African Caribbean',
    # Row 3
    'African unspecified',
    'Includes individuals that either self-report or have been described as African, but there was not sufficient information to allow classification as African American, Afro-Caribbean or Sub-Saharan African',
    'African, non-Hispanic black',
    # Row 4
    'Asian unspecified',
    'Includes individuals that either self-report or have been described as Asian but there was not sufficient information to allow classification as East Asian, Central Asian, South Asian, or South-East Asian',
    'Asian, Asian American',
    # Row 5
    'Central Asian',
    'Includes individuals who either self-report or have been described by authors as Central Asian. We note that there does not appear to be a suitable reference population for this population and efforts are required to fill this gap',
    'Silk Road (founder/genetic isolate)',
    # Row 6
    'East Asian',
    'Includes individuals who either self-report or have been described by authors as East Asian or one of the sub-populations from this region (e.g., Chinese). This category also includes individuals who genetically cluster with reference populations from this region, for example, 1000 Genomes and/or HapMap CDX, CHB, CHS, and JPT populations',
    'Chinese, Japanese, Korean',
    # Row 7
    'European',
    'Includes individuals who either self-report or have been described by authors as European, Caucasian, white, or one of the sub-populations from this region (e.g., Dutch). This category also includes individuals who genetically cluster with reference populations from this region, for example, 1000 Genomes and/or HapMap CEU, FIN, GBR, IBS, and TSI populations',
    'Spanish, Swedish',
    # Row 8
    'Greater Middle Eastern (Middle Eastern, North African, or Persian)',
    'Includes individuals who self-report or were described by authors as Middle Eastern, North African, Persian, or one of the sub-populations from this region (e.g., Saudi Arabian) [32]. We note there is heterogeneity in this category with different degrees of admixture as well as levels of genetic isolation. We note that there does not appear to be a suitable reference population for this category and efforts are required to fill this gap',
    'Tunisian, Arab, Iranian',
    # Row 9
    'Hispanic or Latin American',
    'Includes individuals who either self-report or are described by authors as Hispanic, Latino, Latin American, or one of the sub-populations from this region. This category includes individuals with known admixture of primarily European, African, and Native American ancestries, though some may have also a degree of Asian (e.g., Peru). We also note that the levels of admixture vary depending on the country, with Caribbean countries carrying higher levels of African admixture when compared to South American countries, for example. This category also includes individuals who genetically cluster with reference populations from this region, for example, 1000 Genomes and/or HapMap CLM, MXL, PEL, and PUR populations',
    'Brazilian, Mexican',
    # Row 10
    'Native American',
    'Includes indigenous individuals of North, Central, and South America, descended from the original human migration into the Americas from Siberia [34]. We note that there does not appear to be a suitable reference population for this category and efforts are required to fill this gap',
    'Pima Indian, Plains American Indian',
    # Row 11
    'Not reported',
    'Includes individuals for which no ancestry or country of recruitment information is available',
    NA_character_,
    # Row 12
    'Oceanian',
    'Includes individuals that either self-report or have been described by authors as Oceanian or one of the sub-populations from this region (e.g., Native Hawaiian) [35]. We note that there does not appear to be a suitable reference population for this category and efforts are required to fill this gap',
    'Solomon Islander, Micronesian',
    # Row 13
    'Other',
    'Includes individuals where an ancestry descriptor is known but insufficient information is available to allow assignment to one of the other categories',
    'Surinamese, Russian',
    # Row 14
    'Other admixed ancestry',
    'Includes individuals who either self-report or have been described by authors as admixed and do not fit the definition of the other admixed categories already defined (“African American or Afro-Caribbean” or “Hispanic or Latin American”)',
    NA_character_,
    # Row 15
    'South Asian',
    'Includes individuals who either self-report or have been described by authors as South Asian or one of the sub-populations from this region (e.g., Asian Indian). This category also includes individuals who genetically cluster with reference populations from this region, for example, 1000 Genomes and/or HapMap BEB, GIH, ITU, PJL, and STU populations',
    'Bangladeshi, Sri Lankan Sinhalese',
    # Row 16
    'South East Asian',
    'Includes individuals who either self-report or have been described by authors as South East Asian or one of the sub-populations from this region (e.g., Vietnamese). This category also includes individuals who genetically cluster with reference populations from this region, for example, 1000 Genomes KHV population. We note that East Asian and South East Asian populations are often conflated. However, recent studies indicate a unique genetic background for South East Asian populations',
    'Thai, Malay',
    # Row 17
    'Sub-Saharan African',
    'Includes individuals who either self-report or have been described by authors as Sub-Saharan African or one of the sub-populations from this region (e.g., Yoruban). This category also includes individuals who genetically cluster with reference populations from this region, for example, 1000 Genomes and/or HapMap ESN, LWK, GWD, MSL, MKK, and YRI populations',
    'Yoruban, Gambian',
  )


readr::write_csv(ancestry_categories, "data-raw/ancestry_categories.csv")
usethis::use_data(ancestry_categories, compress = "xz", overwrite = TRUE, version = 2)
