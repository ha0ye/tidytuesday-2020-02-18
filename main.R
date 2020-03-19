library(tidyverse)
library(wbstats)
library(rnaturalearth)
library(rnaturalearthdata)

food_consumption <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv') %>%
    # recode country names for compatibility:
    #   - note: the original data are from UN FAO, which uses "Congo" to refer to 
    #           "Republic of the Congo", http://www.fao.org/faostat/en/#country/46
    mutate(country = recode(country, 
                            USA = "United States", 
                            `Hong Kong SAR. China` = "Hong Kong SAR, China", 
                            `South Korea` = "Korea, Rep.", 
                            Macedonia = "Macedonia", 
                            `Taiwan. ROC` = "Taiwan"))

pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018) %>%
    rename(population = value) %>%
    select(population, country) %>%
    mutate(country = recode(country, 
                            `Egypt, Arab Rep.` = "Egypt", 
                            `Bahamas, The` = "Bahamas", 
                            `Iran, Islamic Rep.` = "Iran", 
                            `Congo, Rep.` = "Congo", 
                            `Gambia, The` = "Gambia", 
                            `Russian Federation` = "Russia", 
                            `Venezuela, RB` = "Venezuela", 
                            `Eswatini` = "Swaziland", 
                            `Slovak Republic` = "Slovakia"))

setdiff(food_consumption$country, pop_data$country)

dat <- food_consumption %>%
    left_join(pop_data) %>%
    mutate(emissions_pop = co2_emmission * population) %>% 
    as_tibble() %>%
    mutate(country = recode(country, 
                            `Czech Republic` = "Czech Rep.", 
                            `French Polynesia` = "Fr. Polynesia", 
                            `Hong Kong SAR, China` = "Hong Kong", 
                            `Bosnia and Herzegovina` = "Bosnia and Herz.", 
                            `Korea, Rep.` = "Korea"))

emissions_summary <- dat %>%
    count(country, wt = emissions_pop, name = "emissions")

world_map <- ne_countries(scale = "medium", returnclass = "sf")
setdiff(world_map$name, emissions_summary$country)
setdiff(emissions_summary$country, world_map$name)

to_plot <- world_map %>%
    left_join(emissions_summary, by = c("name" = "country"))

p <- ggplot(to_plot) + 
    geom_sf(aes(fill = emissions)) + 
    theme_bw() + 
    labs(title = "Total food-based emissions (kg CO2) by country")

png("total_emissions_map.png", width = 1000, height = 600)
print(p)
dev.off()


