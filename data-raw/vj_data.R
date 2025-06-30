library(tidyverse)

df <- read_csv("~/data/archive/acitglob_kumarathunge_2020/data/Kumarathunge-aci-tglob-8595d961d4c8/Data/PPC-TGlob_V1.0.csv")

# filter data to field - native environment (seed source lon/lat can be interpreted as lon/lat for forcing data)
df <- df |>
  filter(Growth_condition == "Field (NE)")

df_sites <- df |>
  ungroup() |>
  mutate(vj = Vcmax/Jmax) |>
  group_by(Location) |>
  summarise(
    lon = mean(seed_source_longitude),
    lat = mean(seed_source_latitude),
    vcmax = mean(Vcmax),
    jmax = mean(Jmax),
    vj = mean(vj)
    )

df_sites |>
  ggplot(aes(x = vj)) +
  geom_histogram(bins = 9)
