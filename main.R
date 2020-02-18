library(tidyverse)

food_consumption <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv') %>%
    # recode country names for compatibility:
    #   - note: the original data are from UN FAO, which uses "Congo" to refer to 
    #           "Republic of the Congo", http://www.fao.org/faostat/en/#country/46
    mutate(country = recode(country, 
                            USA = "United States", 
                            `Hong Kong SAR. China` = "Hong Kong SAR, China", 
                            Bahamas = "Bahamas, The", 
                            Venezuela = "Venezuela, RB", 
                            Russia = "Russian Federation", 
                            Swaziland = "Eswatini", 
                            `South Korea` = "Korea, Rep.", 
                            Macedonia = "Macedonia", 
                            Egypt = "Egypt, Arab Rep.", 
                            Slovakia = "Slovak Republic", 
                            `Taiwan. ROC` = "Taiwan", 
                            Iran = "Iran, Islamic Rep.", 
                            Congo = "Congo, Rep.",
                            Gambia = "Gambia, The"))

library(wbstats)
pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018) %>%
    rename(population = value) %>%
    select(population, country)

setdiff(food_consumption$country, pop_data$country)

dat <- food_consumption %>%
    left_join(pop_data) %>%
    mutate(emissions_pop = co2_emmission * population) %>% 
    as_tibble()

emissions_summary <- dat %>%
    count(country, wt = emissions_pop, name = "emissions")

library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")
to_plot <- world %>%
    left_join(emissions_summary, by = c("name" = "country"))

p <- ggplot(to_plot) + 
    geom_sf(aes(fill = emissions)) + 
    theme_bw() + 
    labs(title = "Total food-based emissions (kg CO2) by country")

pdf("total_emissions_map.pdf", width = 10, height = 6)
print(p)
dev.off()


