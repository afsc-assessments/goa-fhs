SELECT
    gap_products.akfin_biomass.species_code,
    gap_products.akfin_biomass.year,                  AS year,
    SUM(gap_products.akfin_biomass.biomass_mt)       AS biomass,
    SUM(gap_products.akfin_biomass.population_count) AS population,
    SUM(gap_products.akfin_biomass.biomass_var)      AS varbio,
    SUM(gap_products.akfin_biomass.population_var)   AS varpop,
    SUM(gap_products.akfin_biomass.n_haul)           AS numhauls,
    SUM(gap_products.akfin_biomass.n_count)          AS numcaught
FROM
    gap_products.akfin_biomass

WHERE gap_products.akfin_biomass.area_id
    in '99903'
AND gap_products.akfin_biomass.survey_definition_id
     in '47'
AND gap_products.akfin_biomass.species_code
    = '10130'
AND gap_products.akfin_biomass.year
    >= '1984'
GROUP BY
    gap_products.akfin_biomass.species_code,
    gap_products.akfin_biomass.year
ORDER BY
    year