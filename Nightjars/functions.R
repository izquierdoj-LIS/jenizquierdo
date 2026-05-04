library(dplyr)
library(ggplot2)
library(readxl)
library(sf)
library(maps)
library(patchwork)

hawks <- read_excel("Downloads/commonnighthawks.xlsx")
View(hawks)

nrow(hawks)
summary(hawks$LONGITUDE)
summary(hawks$LATITUDE)
hawks <- na.omit(hawks)

hawks_fl <- hawks %>%
  filter(STATE == "Florida")

ggplot(hawks_fl, aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point() +
  coord_fixed() +
  labs(title = "Map of Locations")

hawks_us <- hawks %>%
  filter(COUNTRY == "United States")

state_counts <- hawks_us %>%
  count(STATE)


ggplot(state_counts, aes(x = reorder(STATE, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Observation Counts by State",
       x = "State", y = "Count")

hawks_fl <- hawks_us %>%
  filter(STATE == "Florida")

county_counts <- hawks_fl %>%
  count(COUNTY)

ggplot(county_counts, aes(x = COUNTY, y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Observations by Florida County",
       x = "County",
       y = "Count")

us_counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))

fl_counties <- us_counties %>%
  filter(grepl("florida", ID))

ggplot(fl_counties) +
  geom_sf(fill = "white", color = "black") +
  labs(title = "Florida Counties Map")

hawks_fl_sf <- st_as_sf(hawks_fl,
                        coords = c("LONGITUDE", "LATITUDE"),
                        crs = 4326)
ggplot() +
  geom_sf(data = fl_counties, fill = "white", color = "gray") +
  geom_sf(data = hawks_fl_sf, color = "red", alpha = 0.4, size = 1) +
  labs(title = "Observations in Florida Counties")

county_counts <- hawks_fl %>%
  count(COUNTY)

fl_counties$county_name <- tolower(sub(".*,", "", fl_counties$ID))
county_counts$COUNTY <- tolower(county_counts$COUNTY)

map_data <- left_join(fl_counties, county_counts,
                      by = c("county_name" = "COUNTY"))

ggplot(map_data) +
  geom_sf(aes(fill = n), color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Observations by Florida County",
       fill = "Count")

us_states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
state_counts$STATE <- tolower(state_counts$STATE)
map_data <- left_join(us_states, state_counts,
                      by = c("ID" = "STATE"))

ggplot(map_data) +
  geom_sf(aes(fill = n), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "gray90") +
  labs(title = "Observation Counts by State",
       fill = "Count")

top_states <- state_counts %>%
  slice_max(n, n = 10)

top_map <- us_states %>%
  filter(ID %in% tolower(top_states$STATE)) %>%
  left_join(top_states, by = c("ID" = "STATE"))

ggplot(top_map) +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c() +
  labs(title = "Top 10 States by Observations")

top_states <- state_counts %>%
  slice_max(n, n = 10)

ggplot(top_states, aes(x = STATE, y = n, fill = STATE)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Top States by Observations",
       x = "State", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

hawks_tx <- hawks_us %>%
  filter(STATE == "Texas")

county_counts_tx <- hawks_tx %>%
  count(COUNTY, sort = TRUE)

top_tx <- county_counts_tx %>%
  slice_max(n, n = 15)

ggplot(top_tx,
       aes(x = reorder(COUNTY, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Texas Counties by Observations")

us_counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))


county_counts_fl <- hawks_fl %>%
  count(COUNTY)

us_counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))

fl_counties <- us_counties %>%
  filter(grepl("florida", ID))

county_counts_fl$COUNTY <- tolower(county_counts_fl$COUNTY)

fl_counties$county <- tolower(sub(".*,", "", fl_counties$ID))

map_fl <- left_join(fl_counties,
                    county_counts_fl,
                    by = c("county" = "COUNTY"))

ggplot(map_fl) +
  geom_sf(aes(fill = n), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "gray90") +
  labs(title = "Florida County Observation Heatmap",
       fill = "Count") +
  theme_minimal()

tx_counties <- us_counties %>%
  filter(grepl("texas", ID))

county_counts_tx$COUNTY <- tolower(county_counts_tx$COUNTY)
tx_counties$county <- tolower(sub(".*,", "", tx_counties$ID))

map_tx <- left_join(tx_counties,
                    county_counts_tx,
                    by = c("county" = "COUNTY"))

ggplot(map_tx) +
  geom_sf(aes(fill = n), color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Texas Observation Counts by County",
       fill = "Count")

p_fl <- ggplot(map_fl) +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c() +
  labs(title = "Florida Counties") +
  theme_minimal()

p_tx <- ggplot(map_tx) +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c() +
  labs(title = "Texas Counties") +
  theme_minimal()

p_fl + p_tx
