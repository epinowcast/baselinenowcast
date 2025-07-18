library(epinowcast)
library(baselinenowcast)
library(hexSticker)
library(here)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magick)

# Create the data for the plot in the hex sticker
# Set dates
nowcast_date <- "2021-08-01"
eval_date <- "2021-10-01"

# Load the Germany COVID-19 hospital data from the epinowcast package
target_data <- germany_covid19_hosp[location == "DE"][age_group == "00+"] |>
  enw_filter_report_dates(latest_date = eval_date) |>
  enw_filter_reference_dates(
    latest_date = nowcast_date
  )

# Filter observations for the latest available reported data
latest_data <- enw_latest_data(target_data)

# Filter the observed data to include only the nowcast date
observed_data <- enw_filter_report_dates(
  target_data,
  latest_date = nowcast_date
)

# Get the observed data for the latest reference date
obs_data_by_reference_date <- enw_latest_data(observed_data)

# Set parameters for the nowcast
max_delay <- 30
n_training_volume <- 3 * max_delay
n_history_delay <- 0.5 * n_training_volume
n_retrospective_nowcasts <- 0.5 * n_training_volume

# Preprocess the observed data
training_data <- enw_filter_reference_dates(
  observed_data,
  include_days = n_training_volume - 1
)

# Preprocess the training data
latest_training_data <- enw_latest_data(training_data)

# Filter the target data to include only the latest reference date
target_data <- enw_filter_reference_dates(
  latest_data,
  include_days = n_training_volume - 1
)

# Preprocess the observed data for nowcasting
pobs <- enw_preprocess_data(
  obs = training_data,
  max_delay = max_delay + 1
)

# Extract the reporting triangle data.frame from the preprocessed observed data
reporting_triangle_df <- select(
  pobs$new_confirm[[1]],
  reference_date,
  delay,
  new_confirm
)

# Convert the reporting triangle data.frame to a wide format matrix
reporting_triangle <- reporting_triangle_df |>
  pivot_wider(names_from = delay, values_from = new_confirm) |>
  select(-reference_date) |>
  as.matrix()

# Summarise the reporting triangle data by time
triangle_df <- as.data.frame(reporting_triangle) |>
  mutate(time = row_number()) |>
  pivot_longer(!time,
    values_to = "count",
    names_prefix = "V",
    names_to = "delay"
  ) |>
  mutate(delay = as.numeric(delay))

# Estimate the delay probability mass function (PMF)
delay_pmf <- estimate_delay(
  reporting_triangle = reporting_triangle,
  max_delay = max_delay,
  n = n_history_delay
)

# Make the delay PMF a data frame
delay_df <- data.frame(
  delay = 0:(length(delay_pmf) - 1),
  pmf = delay_pmf
)

# Apply the delay PMF to the reporting triangle to get point nowcasts
point_nowcast_matrix <- apply_delay(
  rep_tri_to_nowcast = reporting_triangle,
  delay_pmf = delay_pmf
)

# Add the point nowcast matrix to the target data
point_nowcast_df <- target_data |>
  mutate(nowcast = rowSums(point_nowcast_matrix))

# Preprocess the training data for nowcasting
prep_latest_data <- latest_training_data |>
  mutate(type = "Real-time data") |>
  select(type, reference_date, count = confirm)

# Create list of truncated reporting triangles
trunc_rep_tri_list <- truncate_triangles(
  reporting_triangle,
  n = n_retrospective_nowcasts
)

# Generate reporting triangles
retro_rep_tri_list <- construct_triangles(trunc_rep_tri_list)

# Generate retrospective point nowcasts
retro_pt_nowcast_mat_list <- fill_triangles(
  reporting_triangle_list = retro_rep_tri_list,
  n = n_history_delay
)

# Estimate a vector of negative binomial dispersion parameters from the observations and estimates at each horizon
disp_params <- estimate_uncertainty(
  pt_nowcast_mat_list = retro_pt_nowcast_mat_list,
  trunc_rep_tri_list = trunc_rep_tri_list,
  reporting_triangle_list = retro_rep_tri_list,
  n = n_retrospective_nowcasts
)

# Generate multiple draws of a nowcast combining observed and predicted values
nowcast_draws_df <- sample_nowcasts(
  point_nowcast_matrix,
  reporting_triangle,
  dispersion = disp_params,
  draws = 100
)

# Prepare the data
latest_data_prepped <- latest_training_data |>
  mutate(time = row_number()) |>
  rename(obs_confirm = confirm) |>
  mutate(reference_date = as.Date(reference_date))

# Prepare the final data with the latest confirmed counts
final_data_prepped <- target_data |>
  select(reference_date, final_confirm = confirm) |>
  mutate(reference_date = as.Date(reference_date))

# Join the nowcast draws with the latest observed data and final confirmed counts
obs_with_nowcast_draws_df <- nowcast_draws_df |>
  left_join(latest_data_prepped, by = "time") |>
  left_join(final_data_prepped, by = "reference_date") |>
  filter(reference_date >= "2021-07-01")

# Combine the nowcast draws with the observed and final confirmed counts
combined_data <- obs_with_nowcast_draws_df |>
  select(reference_date, obs_confirm, final_confirm) |>
  distinct() |>
  pivot_longer(
    cols = c(obs_confirm, final_confirm),
    names_to = "type",
    values_to = "count"
  ) |>
  mutate(type = case_when(
    type == "obs_confirm" ~ "Observed data",
    type == "final_confirm" ~ "Final observed data"
  ))

# Plot with draws for nowcast only
hex_plot <- ggplot() +
  # Add nowcast draws as thin gray lines
  geom_line(
    data = obs_with_nowcast_draws_df,
    aes(
      x = reference_date,
      y = pred_count,
      group = draw,
      color = "Nowcast draw"
    ),
    linewidth = 0.2
  ) +
  # Add observed data and final data once
  geom_line(
    data = combined_data,
    aes(
      x = reference_date,
      y = count,
      color = type
    )
  ) +
  scale_color_manual(
    values = c(
      "Nowcast draw" = "gray",
      "Observed data" = "darkred",
      "Final observed data" = "black"
    ),
    name = ""
  ) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme_void() +
  theme_transparent() +
  theme(
    legend.position = "none",
    panel.background = element_blank()
  )

# make and save hexsticker
sticker(
  hex_plot,
  package = "baselinenowcast",
  p_size = 15,
  p_color = "#646770",
  s_x = 1,
  s_y = 0.85,
  s_width = 1.3,
  s_height = 0.85,
  h_fill = "#ffffff",
  h_color = "#646770",
  filename = file.path("man", "figures", "logo.png"),
  u_color = "#646770",
  u_size = 3.5
)

# Make outside of hex sticker transparent
pp <- image_read(here("man", "figures", "logo.png"))

# Helper function to fill corners with transparency
fill_corner <- function(img, x, y, fuzz = 30) {
  image_fill(img,
    color = "transparent",
    refcolor = "white",
    fuzz = fuzz,
    point = paste0("+", x, "+", y)
  )
}

fuzz <- 30

logo <- fill_corner(pp, 1, 2, fuzz = fuzz) |>
  fill_corner(image_info(pp)$width - 1, 2, fuzz = fuzz) |>
  fill_corner(1, image_info(pp)$height - 1, fuzz = fuzz) |>
  fill_corner(image_info(pp)$width - 1, image_info(pp)$height - 1, fuzz = fuzz)

image_write(image = logo, path = here("man", "figures", "logo.png"))

usethis::use_logo(here("man", "figures", "logo.png"))
pkgdown::build_favicons(pkg = ".", overwrite = TRUE)
