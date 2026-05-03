set.seed(123)

states <- c("FL","GA","AL","SC","NC")
years <- 2015:2026

nightjar_counts <- expand.grid(
  state = states,
  year = years
)

nightjar_counts$species <- "CHNA"
nightjar_counts$effort <- sample(5:20, nrow(nightjar_counts), replace = TRUE)

nightjar_counts$count <- rpois(
  nrow(nightjar_counts),
  lambda = 20 + (nightjar_counts$year - 2015) * 0.3
)

usethis::use_data(nightjar_counts, overwrite = TRUE)

clean_nightjar_data <- function(df) {
  df |>
    dplyr::mutate(
      state = toupper(state),
      year = as.integer(year),
      species = as.character(species),
      count = as.numeric(count),
      effort = as.numeric(effort)
    ) |>
    dplyr::filter(!is.na(state), !is.na(year))
}

summarise_state_trends <- function(df) {

  df |>
    dplyr::group_by(state, year) |>
    dplyr::summarise(
      total_count = sum(count, na.rm = TRUE),
      total_effort = sum(effort, na.rm = TRUE),
      cpue = total_count / total_effort,
      .groups = "drop"
    )
}

plot_state_trends <- function(df, state_filter = NULL) {

  if (!is.null(state_filter)) {
    df <- df |> dplyr::filter(state %in% state_filter)
  }

  ggplot2::ggplot(df, ggplot2::aes(x = year, y = cpue, color = state)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "Nightjar Population Trends (CPUE)",
      x = "Year",
      y = "Catch per Unit Effort"
    ) +
    ggplot2::theme_minimal()
}

plot_state_map <- function(df, year_filter) {

  df_year <- df |>
    dplyr::filter(year == year_filter)

  states_map <- maps::map_data("state")

  state_summary <- df_year |>
    dplyr::group_by(state) |>
    dplyr::summarise(cpue = sum(count) / sum(effort), .groups = "drop") |>
    dplyr::mutate(region = tolower(state.name[match(state, state.abb)]))

  ggplot2::ggplot(states_map) +
    ggplot2::geom_polygon(
      aes(long, lat, group = group),
      fill = "grey90",
      color = "white"
    ) +
    ggplot2::geom_map(
      data = state_summary,
      map = states_map,
      aes(map_id = region, fill = cpue),
      color = "white"
    ) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::theme_void() +
    ggplot2::labs(title = paste("Nightjar CPUE -", year_filter))
}

devtools::document()
devtools::load_all()
summarise_state_trends(nightjar_counts)
usethis::use_data(nightjar_counts, overwrite = TRUE)
data("nightjar_counts")

devtools::install()
library(Nightjars)

data("nightjar_counts")

nightjar_counts |>
  clean_nightjar_data() |>
  summarise_state_trends() |>
  plot_state_trends()

lm(cpue ~ year, data = nightjar_counts)

summarise_state_trends <- function(df) {

  df |>
    dplyr::group_by(state, year) |>
    dplyr::summarise(
      total_count = sum(count, na.rm = TRUE),
      total_effort = sum(effort, na.rm = TRUE),
      cpue = total_count / total_effort,
      .groups = "drop"
    )
}

df <- summarise_state_trends(nightjar_counts)
names(df)

lm(cpue ~ year, data = df)

usethis::use_git()

list.files(recursive = TRUE)
usethis::use_git_ignore()

getwd()
usethis::proj_sitrep()
rlang::last_trace()

usethis::proj_sitrep()
usethis::use_readme_rmd(open = FALSE)
usethis::use_git()

usethis::use_git_config()
usethis::use_github()
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)`
