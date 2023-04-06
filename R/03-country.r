##################################################
#                 Let's make animated maps with R
#                 Milos Popovic
#                 2023/04/02
##################################################
# libraries we need
libs <- c(
    "tidyverse", "sf", "rnaturalearth",
    "wbstats", "gganimate", "classInt"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = TRUE))

# 1. WB DATA
#-----------
kpi_df <- wbstats::wb_search(pattern = "internet users")
fix(kpi_df)

internet_df <- wbstats::wb_data(
    indicator = "IT.NET.USER.ZS",
    country = "countries_only",
    start_date = 2001, end_date = 2022
)

internet_df

internet_world_df <- internet_df |>
    dplyr::select(, c(1, 4:5))

names(internet_world_df) <- c(
    "iso2", "Year", "users"
)

internet_world_df

# 2. WORLD SHAPEFILE
#-------------------
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

world_sf <- rnaturalearth::ne_countries(
    type = "countries", scale = "small"
) |>
    sf::st_as_sf() |>
    sf::st_transform(crsLONGLAT)

head(world_sf)
names(world_sf)
plot(sf::st_geometry(world_sf))

# 2B. FILTER INDIA
#-------------------------
# India
internet_india_df <- internet_world_df |>
    dplyr::filter(iso2 == "IN") |>
    dplyr::filter(Year < 2021)

print(internet_india_df, n = 21)

# 3. JOIN DATA & SHAPEFILE
#-------------------------
internet_india_sf <- dplyr::right_join(
    world_sf, internet_india_df,
    by = c("iso_a2" = "iso2")
)

internet_india_sf

# 4. BREAKS
#----------
vmin <- min(internet_india_sf$users, na.rm = T)
vmax <- max(internet_india_sf$users, na.rm = T)
brk <- round(classIntervals(
    internet_india_sf$users,
    n = 6,
    style = "fisher"
)
$brks, 1) |>
    head(-1) |>
    tail(-1) |>
    append(vmax)
breaks <- c(vmin, brk)

cols <- rev(c(
    "#001f3a", "#30314e", "#5c435f",
    "#8d5369", "#c0636c", "#dc8177",
    "#e6a988"
))

# 5. ANIMATE
#-----------
get_animated_india_map <- function() {
    india_map <- ggplot(
        data = internet_india_sf,
        aes(fill = users)
    ) +
        geom_sf(color = "white", size = 0.05) +
        scale_fill_gradientn(
            name = "",
            colours = cols,
            breaks = breaks,
            labels = round(breaks, 1),
            limits = c(vmin, vmax),
            na.value = "grey70"
        ) +
        coord_sf() +
        guides(fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(1.5, units = "mm"),
            keywidth = unit(15, units = "mm"),
            title.position = "top",
            title.hjust = .5,
            label.hjust = .5,
            nrow = 1,
            byrow = T,
            reverse = F,
            label.position = "bottom"
        )) +
        theme_minimal() +
        theme(
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = c(.5, -.015),
            legend.text = element_text(size = 11, color = "grey10"),
            panel.grid.major = element_line(color = "white", size = .2),
            panel.grid.minor = element_blank(),
            plot.title = element_text(
                face = "bold", size = 20,
                color = "grey10", hjust = .5, vjust = -3
            ),
            plot.subtitle = element_text(
                size = 40, color = "#c43c4e",
                hjust = .5, vjust = -1
            ),
            plot.caption = element_text(
                size = 10, color = "grey10",
                hjust = .5, vjust = -10
            ),
            plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "lines"),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            legend.background = element_rect(fill = "white", color = NA),
            panel.border = element_blank()
        ) +
        labs(
            x = "",
            y = "",
            title = "Internet users (% of population)",
            subtitle = "Year: {as.integer(closest_state)}",
            caption = "©2023 Milos Popovic (https://milospopovic.net)
        World Development Indicators, The World Bank"
        )

    return(india_map)
}

india_map <- get_animated_india_map()
print(india_map)

timelapse_india_map <- india_map +
    transition_states(Year) +
    enter_fade() +
    exit_fade() +
    ease_aes("quadratic-in-out", interval = .2)

animated_india <- gganimate::animate(
    timelapse_india_map,
    nframes = 120,
    duration = 20,
    start_pause = 3,
    end_pause = 30,
    height = 6,
    width = 7.15,
    res = 300,
    units = "in",
    fps = 15,
    renderer = gifski_renderer(loop = T)
)

gganimate::anim_save(
    "internet_users_india.gif", animated_india
)
