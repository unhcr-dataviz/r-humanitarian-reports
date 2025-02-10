
# Local CSV ---------------------------------------------------------------

library(tidyverse)

sdn_demo <- read_csv("data/sdn_demo.csv")

head(sdn_demo)


# Refugees package --------------------------------------------------------

library(refugees)

wrl_pop <- population

wrl_pop


# ACLED API ---------------------------------------------------------------

library(racled)

sdn_event <- read_acled(
  country = "Sudan",
  key = , #enter your ACLED API key
  email = #enter your ACLED API email
)

head(sdn_event)


# HDX ---------------------------------------------------------------------

library(rhdx)
library(sf)

sdn_adm1 <- pull_dataset("cod-ab-sdn") |> 
  get_resource(4) |> 
  # get_resource_layers() |> 
  read_resource(
    layer = "sdn_admbnda_adm1_cbs_nic_ssa_20200831"
  )

ggplot() +
  geom_sf(data = sdn_adm1)

sdn_idp <-
  pull_dataset("sudan-displacement-situation-countrywide-idps-iom-dtm") |>
  get_resource(1) |>
  read_resource(
    sheet = "MASTER LIST (ADMIN1)",
    skip = 1
  ) |> 
  janitor::clean_names() |> 
  select(adm1_pcode = x2, idp = total)


# Wrangling ---------------------------------------------------------------

sdn_ref <- wrl_pop |> 
  filter(coo_iso == "SDN") |> 
  filter(year > max(year) - 20 & max(year)) |> 
  summarise(refugees = sum(refugees, na.rm = TRUE),
            .by = year) |> 
  mutate(year = as.factor(year))

sdn_demo_graph <- sdn_demo |> 
  pivot_longer(cols = Girls:Men) |> 
  rename(country = `...1`) |> 
  mutate(name = fct_rev(as_factor(name)),
         country = fct_rev(as_factor(country)))
  


sdn_event_sf <- sdn_event |> 
  filter(year == 2025) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  filter(event_type != "Strategic developments")
  

# Visual ------------------------------------------------------------------

library(unhcrthemes)


# Evolution of ref from Sudan
ggplot(data = sdn_ref,
       aes(x = year, y = refugees)) +
  geom_col(fill = unhcr_pal(n = 1, "pal_blue")) +
  labs(
    title = "Evolution of refugees from Sudan",
    caption = "Source: UNHCR"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.8e6, by = 0.3e6),
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) + 
  theme_unhcr(
    axis_title = FALSE,
    grid = "Y"
  )
  
ggplot(data = sdn_ref,
       aes(x = year, y = refugees, group = 1)) +
  geom_line(color = unhcr_pal(n = 1, "pal_blue"), size = 1) +
  labs(
    title = "Evolution of refugees from Sudan",
    caption = "Source: UNHCR"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.8e6, by = 0.3e6),
    labels = scales::label_number(scale_cut = scales::cut_short_scale()),
    limits = c(0, 1.8e6)
  ) + 
  theme_unhcr(
    axis_title = FALSE,
    grid = "Y"
  )

# Demographic
ggplot(
  data = sdn_demo_graph,
  aes(x = value, y = country, fill = name)
) +
  geom_col() +
  scale_fill_unhcr_d(
    palette = "pal_unhcr", nmax = 10, order = c(6, 2, 5, 1),
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_x_continuous(
    expand = expansion(c(0, .1)),
    labels = scales::label_percent()
  ) +
  labs(
    title = "Demographic breakdown of new arrivals",
    caption = "Source: UNHCR"
  ) +
  theme_unhcr(
    axis_title = FALSE,
    axis = "Y",
    grid = "X"
  )

# Map
ggplot() +
  geom_sf(data = sdn_adm1, fill = "transparent") +
  geom_sf(data = sdn_event_sf,
          aes(size = fatalities, color = event_type),
          alpha = 0.3) +
  scale_color_unhcr_d() +
  theme_unhcr(void = TRUE)
