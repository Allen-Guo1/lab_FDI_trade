#################################
#### Last lab assignment
#### Top U.S. export destinations, 2015 and 2025

library(tidyverse)
library(scales)
library(readxl)

rm(list = ls())

years_to_plot <- c(2015, 2025)

country_code_is_aggregate <- function(cty_code) {
  substr(cty_code, 1, 1) == "0" |
    substr(cty_code, 2, 2) == "X" |
    substr(cty_code, 1, 1) == "-"
}

get_exports_from_api <- function() {
  api_key <- Sys.getenv("CENSUS_API_KEY")

  if (api_key == "") {
    api_key <- Sys.getenv("CENSUS_KEY")
  }

  if (api_key == "" || !requireNamespace("censusapi", quietly = TRUE)) {
    return(NULL)
  }

  exports_cty_yr <- censusapi::getCensus(
    name = "timeseries/intltrade/exports/naics",
    key = api_key,
    vars = c("ALL_VAL_YR", "YEAR", "CTY_CODE", "CTY_NAME"),
    time = "from 2015",
    MONTH = "12",
    show_call = TRUE
  )

  exports_cty_yr |>
    filter(YEAR %in% as.character(years_to_plot)) |>
    filter(!country_code_is_aggregate(CTY_CODE)) |>
    transmute(
      year = as.integer(YEAR),
      cty_code = CTY_CODE,
      country = CTY_NAME,
      exports_bil = as.numeric(ALL_VAL_YR) / 1000000000,
      data_source = "Census International Trade API"
    )
}

get_exports_from_country_workbook <- function() {
  country_workbook_url <- "https://www.census.gov/foreign-trade/balance/country.xlsx"
  country_workbook <- tempfile(fileext = ".xlsx")

  download.file(country_workbook_url, country_workbook, mode = "wb", quiet = TRUE)

  read_excel(country_workbook, sheet = "country", col_types = "text") |>
    filter(year %in% as.character(years_to_plot)) |>
    filter(!country_code_is_aggregate(CTY_CODE)) |>
    transmute(
      year = as.integer(year),
      cty_code = CTY_CODE,
      country = CTYNAME,
      exports_bil = parse_number(EYR) / 1000,
      data_source = "Census country balance workbook"
    )
}

exports_cty_yr <- tryCatch(
  get_exports_from_api(),
  error = function(e) {
    message("Census API call failed, using Census country workbook fallback: ", e$message)
    NULL
  }
)

if (is.null(exports_cty_yr)) {
  exports_cty_yr <- get_exports_from_country_workbook()
}

top10_exports <- exports_cty_yr |>
  filter(year %in% years_to_plot, exports_bil > 0) |>
  group_by(year) |>
  slice_max(order_by = exports_bil, n = 10, with_ties = FALSE) |>
  arrange(year, desc(exports_bil)) |>
  mutate(rank = row_number()) |>
  ungroup()

print(top10_exports)

plot_data <- top10_exports |>
  arrange(year, exports_bil) |>
  mutate(
    country_year = factor(
      paste(country, year, sep = "___"),
      levels = unique(paste(country, year, sep = "___"))
    )
  )

export_plot <- ggplot(plot_data, aes(x = exports_bil, y = country_year, fill = factor(year))) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_text(
    aes(label = dollar(exports_bil, suffix = "B", accuracy = 0.1)),
    hjust = -0.08,
    color = "#1F2933",
    fontface = "bold",
    size = 3.4
  ) +
  facet_wrap(~year, scales = "free_y") +
  scale_y_discrete(labels = \(x) str_remove(x, "___.*$")) +
  scale_x_continuous(
    labels = dollar_format(suffix = "B", accuracy = 1),
    expand = expansion(mult = c(0, 0.26))
  ) +
  scale_fill_manual(values = c("2015" = "#2A6F97", "2025" = "#C16622")) +
  labs(
    title = "Top 10 Destinations for U.S. Goods Exports",
    subtitle = "Annual export value by country, 2015 and 2025",
    x = "Export value, billions of U.S. dollars",
    y = NULL,
    caption = paste("Source:", first(plot_data$data_source), "| U.S. Census Bureau")
  ) +
  theme_minimal(base_size = 12) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(color = "#1F2933"),
    axis.text = element_text(color = "#1F2933"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 13),
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(10, 30, 10, 10)
  )

output_file <- file.path("figures", "us_exports_top10_2015_2025.png")
dir.create(dirname(output_file), showWarnings = FALSE)

# Writing first to a temporary ASCII-only path avoids graphics-device issues
# on machines where the project directory contains non-ASCII characters.
temp_plot_file <- tempfile(fileext = ".png")

ggsave(
  filename = temp_plot_file,
  plot = export_plot,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)

file.copy(temp_plot_file, output_file, overwrite = TRUE)
