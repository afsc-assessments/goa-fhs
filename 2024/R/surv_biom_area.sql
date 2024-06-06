SELECT
gap_products.akfin_biomass.species_code,
gap_products.akfin_biomass.year,
gap_products.akfin_biomass.biomass_mt,
gap_products.akfin_biomass.population_count,
gap_products.akfin_biomass.biomass_var,
gap_products.akfin_biomass.population_var,
gap_products.akfin_biomass.n_haul,
gap_products.akfin_biomass.n_count,
gap_products.akfin_biomass.area_id,
gap_products.akfin_area.area_id,
gap_products.akfin_area.description
FROM
gap_products.akfin_biomass
INNER JOIN gap_products.akfin_area ON gap_products.akfin_biomass.area_id = gap_products.akfin_area.area_id
WHERE gap_products.akfin_biomass.area_id
in ('803','804','805')
AND gap_products.akfin_biomass.survey_definition_id
in '47'
AND gap_products.akfin_biomass.species_code
= '10130'
AND gap_products.akfin_biomass.year
>= '1984'
AND gap_products.akfin_area.design_year
>= '1984' 
ORDER BY
year