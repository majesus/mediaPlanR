# Builds every example dataset of mediaPlanR. Run from the package root:
#   source("data-raw/datasets.R")
#
# Datasets whose name ends in "_example" are original illustrative inputs, not
# derived from any published source, and are ready for do.call() on the
# matching function. csd_kim2005, msad_kim2005 and mbd_cheong2007 reproduce
# the minimal factual inputs (reach and duplication figures) published by
# Kim (2005) and Cheong (2007), so that users can verify that the package
# reproduces the published worked examples.

save_dataset <- function(name, value) {
  assign(name, value)
  save(list = name, file = file.path("data", paste0(name, ".rda")),
       compress = "bzip2", version = 2)
}

square <- function(values, n) matrix(values, nrow = n, ncol = n)

# --- Original illustrative inputs -------------------------------------------

# Sainsbury and Binomial: audiences (people per insertion) and population.
save_dataset("ratings_example", list(
  audiences = c(3e5, 4e5, 2e5),
  population = 1e6
))

# Beta-Binomial: audience after one and two insertions, population, insertions.
save_dataset("beta_binomial_example", list(
  A1 = 5e5, A2 = 5.5e5, P = 1e6, n = 5
))

# Metheringham: three vehicles with several insertions each. The diagonal of
# the duplication matrix is the audience duplicated between two insertions in
# the same vehicle.
save_dataset("metheringham_example", list(
  audiences = c(1.5e6, 8e5, 1.2e6),
  insertions = c(4, 3, 5),
  duplication_matrix = square(c(150000, 200000, 180000,
                                200000, 120000, 140000,
                                180000, 140000, 170000), 3),
  population = 1e7
))

# Hofmans accumulation: reach after one and two insertions of one vehicle.
save_dataset("hofmans_accumulation_example", list(
  R1 = 0.06, R2 = 0.103, N = 5
))

# Agostini and Hofmans duplication: three vehicles with one insertion each and
# the audience duplicated between every pair (the diagonal is ignored).
save_dataset("duplication_example", list(
  audiences = c(3e5, 4e5, 2e5),
  population = 1e6,
  duplication_matrix = square(c(NA, 140000, 70000,
                                140000, NA, 90000,
                                70000, 90000, NA), 3)
))

# CANEX: two vehicles, three and two insertions.
save_dataset("canex_example", list(
  vehicles_data = data.frame(k = c(3, 2), R1 = c(0.30, 0.12),
                             R2 = c(0.40, 0.18)),
  duplications = square(c(NA, 0.05, 0.05, NA), 2),
  population = 1e6
))

# fit_bbd_to_reach(): three vehicles and an external schedule reach in people.
save_dataset("bbd_reach_example", list(
  insertions = c(5, 7, 4),
  audiences = c(5e5, 5.5e5, 6e5),
  reach = 8.5e5,
  universe = 1e6
))

save_dataset("csd_example", list(
  vehicles_data = data.frame(insertions = c(3, 2, 4), R1 = c(0.35, 0.18, 0.10),
                             R2 = c(0.44, 0.24, 0.15)),
  duplications = square(c(NA, 0.05, 0.02,
                          0.05, NA, 0.015,
                          0.02, 0.015, NA), 3),
  aggregation_order = 1:3
))

save_dataset("msad_example", list(
  vehicles_data = data.frame(insertions = c(3, 2), R1 = c(0.35, 0.18),
                             R2 = c(0.44, 0.24)),
  duplications = square(c(NA, 0.05, 0.05, NA), 2),
  aggregation_order = 1:2
))

save_dataset("mbd_example", list(
  vehicles_data = data.frame(insertions = c(2, 3), R1 = c(0.2, 0.3),
                             R2 = c(0.27, 0.38)),
  duplications = square(c(NA, 0.05, 0.05, NA), 2),
  aggregation_order = 1:2
))

# --- Published inputs reused for validation ----------------------------------

# Kim (2005), Tables 4.2.2.1-4.2.2.2: three vehicles, TD forward order.
kim2005_vehicles <- data.frame(insertions = c(2, 2, 2),
                               R1 = c(0.4902, 0.0333, 0.0300),
                               R2 = c(0.5805, 0.0502, 0.0371))
kim2005_duplications <- square(c(NA, 0.0157, 0.0139,
                                 0.0157, NA, 0.0003,
                                 0.0139, 0.0003, NA), 3)
save_dataset("csd_kim2005", list(
  vehicles_data = kim2005_vehicles, duplications = kim2005_duplications,
  aggregation_order = 1:3
))
save_dataset("msad_kim2005", list(
  vehicles_data = kim2005_vehicles, duplications = kim2005_duplications,
  aggregation_order = 1:3
))

# Cheong (2007), Chapter 4.2: vehicle A (2 insertions), B (1) and C (3).
save_dataset("mbd_cheong2007", list(
  vehicles_data = data.frame(insertions = c(2, 1, 3),
                             R1 = c(0.146, 0.110, 0.252),
                             R2 = c(0.191, NA, 0.318)),
  duplications = square(c(NA, 0.032, 0.063,
                          0.032, NA, 0.041,
                          0.063, 0.041, NA), 3),
  aggregation_order = 1:3
))
