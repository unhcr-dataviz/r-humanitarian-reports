
# Package -----------------------------------------------------------------

library(tidyverse) # data manipulation and viz
library(unhcrthemes) # unhcr ggplot style
library(refugees) # unhcr official stats
library(rhdx) # access HDX data
library(racled) # access ACLED data
library(sf) # handle geo data


# Data ingestion ----------------------------------------------------------

# local
sdn_demo <- read_csv("data/sdn_demo.csv")

head(sdn_demo)

# refugee package

wrl_pop <- population

# acled


# hdx



# Data wrangling ----------------------------------------------------------

sdn_ref <- wrl_pop |> 
  filter(coo_iso == "SDN") |> 
  filter(year > max(year) - 50 & max(year)) |> 
  summarise(refugees = sum(refugees, na.rm = TRUE),
            .by = year) |> 
  mutate(year = as.factor(year))


# Dataviz -----------------------------------------------------------------

# Evolution of ref from Sudan
ggplot(data = )


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
  geom_line() +
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
