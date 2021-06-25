# lucas look up table


luts = list(
    
    strata2005 = data.frame(STR05 = 1:7,
                            class05 = c("arable_land", "permanent_crops", "grass",
                                        "wooded_shrubed", "bare_land", "artificial", "water")),
    strata2018 = data.frame(STR18 = 1:9,
                            class18 = c("arable_land", "permament_crops", "grass", "wooded",
                                        "shrubs", "bare_land", "artifical",
                                        "inland_water", "transitional_water"))
)
