## code to prepare `DATASET` dataset goes here
# Storage format for points from HVF tests.
# The default is left to right, top to bottom (LTR TTB) for a right eye ("OD").
# This results in storage nasal to temporal, superior to inferior regardless
# of whether the test results are for a right or left eye.
#
# For very little reason and with incomplete testing, I have also defined the
# coordinates from left to right, top to bottom for a left eye ("OS").
# This results in storage temporal to nasal, superior to inferior regardless
# of whether the test results are for a right or left eye.
# Do not use OS.

# Most test are symmetric so the storage formats are nearly equivalent
# Some noted differences are the location of the blind spot in some tests and
# non-symmetry in 24-2(C) tests.

# Because 24-2C is a superset of 24-2, The first 54 points are arranged to
# be the same in the two formats. The extra 10 points in 24-2 are appended
# to the original 54. Within the 10 points, they are again left to right
# top to bottom.

# helper function
hvf_seq <- function(left = -right, right, by = 6L) {
  as.integer(seq.int(left, right, by))
}

HVF_XY_defaults <- list(
  "OD" = list(
    "24-2" = list(
      npoints = 54L,
      blindspotindices = c(26L, 35L),
      X = unlist(mapply(
        FUN = hvf_seq,
        left = c(-9, -15, -21, -27, -27, -21, -15, -9),
        right = c(9,  15,  21,  21,  21,  21,  15,  9))),
      Y = rep(
        c(21L, 15L, 9L, 3L, -3L, -9L, -15L, -21L),
        times = c(4, 6, 8, 9, 9, 8, 6, 4))),
    "24-2C" = list(
      npoints = 64L,
      blindspotindices = c(26L, 35L),
      X = c(
        unlist(mapply(
          FUN = hvf_seq,
          left = c(-9, -15, -21, -27, -27, -21, -15, -9),
          right = c(9,  15,  21,  21,  21,  21,  15,  9))),
        c(5L, -1L, 1L, 7L, -5L, -7L, -7L, -1L, 5L, 1L)),
      Y = c(
        rep(
          c(21L, 15L, 9L, 3L, -3L, -9L, -15L, -21L),
          times = c(4, 6, 8, 9, 9, 8, 6, 4)),
        c(7L, 5L, 5L, 5L, 1L, -1L, -5L, -5L, -7L, -9L))),
    "30-2" = list(
      npoints = 76L,
      blindspotindices = c(36L, 46L),
      X = unlist(mapply(
        FUN = hvf_seq,
        right = c(9, 15, 21, 27, 27, 27, 27, 21, 15, 9))),
      Y = rep(
        c(27L, 21L, 15L, 9L, 3L, -3L, -9L, -15L, -21L, -27L),
        times = c(4, 6, 8, 10, 10, 10, 10, 8, 6, 4))),
    "10-2" = list(
      npoints = 68L,
      blindspotindices = integer(0),
      X = unlist(mapply(
        FUN = hvf_seq,
        right = c(1, 5, 7, 7, 9, 9, 7, 7, 5, 1),
        by = 2L)),
      Y = rep(
        c(9L, 7L, 5L, 3L, 1L, -1L, -3L, -5L, -7L, -9L),
        times = c(2, 6, 8, 8, 10, 10, 8, 8, 6, 2))),
    "60-4" = list(
      npoints = 60L,
      blindspotindices = integer(0),
      X = unlist(mapply(
        FUN = hvf_seq,
        left  = c(-30, -54, -54, 30, -54, 30, -54, 30, -54, 30, -42, -42, -18),
        right = c( 30,  54, -30, 54, -30, 54, -30, 54, -30, 54,  42,  42,  18),
        by = 12L)),
      Y = rep(
        c(42L, 30L, 18L, 6L, -6L, -18L, -30L, -42L, -54L),
        times = c(6, 10, 6, 6, 6, 6, 8, 8, 4))),
    "S36" = list(
      npoints = 36L,
      blindspotindices = integer(0),
      X = c(
        -20L, 0L, 20L,
        -30L, -20L, 0L, 20L, 30L,
        -30L, 0L, 30L, -20L, 20L,
        -40L, 0L, 40L, -20L, 20L,
        -40L, -10L, 10L, 40L,
        -20L, 0L, 20L,
        -40L, 40L,
        -50L, -30L, -10L, 10L, 30L, 50L,
        -20L, 0L, 20L),
      Y = rep(
        c(52L, 44L, 36L, 32L, 24L, 20L, 15L, 10L, 5L, 0L, -10L),
        times = c(3L, 5L, 3L, 2L, 3L, 2L, 4L, 3L, 2L, 6L, 3L))),
    "EM100" = list( # Visual Field Esterman Monocular Test Pattern
      npoints = 100L,
      blindspotindices = integer(0), # ???
      X = c(
        -23L, 23L,
        -41L, -17L, -6L, 6L, 17L, 34L, 57L,
        -51L, -33L, -20L, -10L, -3L, 3L, 10L, 20L, 30L, 42L, 57L, 73L,
        -52L, -33L, -20L, -13L, -8L, 8L, 13L, 20L, 30L, 42L, 57L, 75L,
        -52L, -33L, -20L, -13L, -8L, 8L, 13L, 20L, 30L, 42L, 57L, 76L,
        -51L, -33L, -20L, -13L, -8L, -3L, 3L, 8L, 13L, 20L, 30L, 42L, 57L, 76L,
        -46L, -33L, -20L, -13L, -8L, -3L, 3L, 8L, 13L, 20L, 30L, 42L, 57L, 75L,
        -41L, -20L, -13L, -8L, -3L, 3L, 8L, 13L, 20L, 30L, 42L, 57L, 74L,
        -33L, -17L, -5L, 5L, 17L, 33L, 49L, 69L,
        -17L, 5L, 29L, 55L,
        30L,
        0L),
      Y = rep(
        c(36L, 21L, 10L, 3L, -3L, -8L, -13L, -21L, -30L, -43L, -53L, -57L),
        times = c(2L, 7L, 12L, 12L, 12L, 14L, 14L, 13L, 8L, 4L, 1L, 1L)))),


  "OS" = list(
    "24-2" = list(
      npoints = 54L,
      blindspotindices = c(20L, 29L),
      X = unlist(mapply(
        FUN = hvf_seq,
        left = c(-9, -15, -21, -21, -21, -21, -15, -9),
        right = c(9,  15,  21,  27,  27,  21,  15,  9))),
      Y = rep(
        c(21L, 15L, 9L, 3L, -3L, -9L, -15L, -21L),
        times = c(4, 6, 8, 9, 9, 8, 6, 4))),
    "24-2C" = list(
      npoints = 64L,
      blindspotindices = c(26L, 35L),
      X = c(
        unlist(mapply(
          FUN = hvf_seq,
          left = c(-9, -15, -21, -21, -21, -21, -15, -9),
          right = c(9,  15,  21,  27,  27,  21,  15,  9))),
        c(-5L, -7L, -1L, 1L, 5L, 7L, 1L, 7L, -5L, -1L)),
      Y = c(
        rep(
          c(21L, 15L, 9L, 3L, -3L, -9L, -15L, -21L),
          times = c(4, 6, 8, 9, 9, 8, 6, 4)),
        c(7L, 5L, 5L, 5L, 1L, -1L, -5L, -5L, -7L, -9L))),
    "30-2" = list(
      npoints = 76L,
      blindspotindices = c(31L, 41L),
      X = unlist(mapply(
        FUN = hvf_seq,
        right = c(9, 15, 21, 27, 27, 27, 27, 21, 15, 9))),
      Y = rep(
        c(27L, 21L, 15L, 9L, 3L, -3L, -9L, -15L, -21L, -27L),
        times = c(4, 6, 8, 10, 10, 10, 10, 8, 6, 4))),
    "10-2" = list(
      npoints = 68L,
      blindspotindices = integer(0),
      X = unlist(mapply(
        FUN = hvf_seq,
        right = c(1, 5, 7, 7, 9, 9, 7, 7, 5, 1),
        by = 2L)),
      Y = rep(
        c(9L, 7L, 5L, 3L, 1L, -1L, -3L, -5L, -7L, -9L),
        times = c(2, 6, 8, 8, 10, 10, 8, 8, 6, 2))),
    "60-4" = list(
      npoints = 60L,
      blindspotindices = integer(0),
      X = unlist(mapply(
        FUN = hvf_seq,
        left  = c(-30, -54, -54, 30, -54, 30, -54, 30, -54, 30, -42, -42, -18),
        right = c( 30,  54, -30, 54, -30, 54, -30, 54, -30, 54,  42,  42,  18),
        by = 12L)),
      Y = rep(
        c(42L, 30L, 18L, 6L, -6L, -18L, -30L, -42L, -54L),
        times = c(6, 10, 6, 6, 6, 6, 8, 8, 4))),
    "S36" = list(
      npoints = 36L,
      blindspotindices = integer(0),
      X = c(
        -20L, 0L, 20L,
        -30L, -20L, 0L, 20L, 30L,
        -30L, 0L, 30L, -20L, 20L,
        -40L, 0L, 40L, -20L, 20L,
        -40L, -10L, 10L, 40L,
        -20L, 0L, 20L,
        -40L, 40L,
        -50L, -30L, -10L, 10L, 30L, 50L,
        -20L, 0L, 20L),
      Y = rep(
        c(52L, 44L, 36L, 32L, 24L, 20L, 15L, 10L, 5L, 0L, -10L),
        times = c(3L, 5L, 3L, 2L, 3L, 2L, 4L, 3L, 2L, 6L, 3L))),
    "EM100" = list( # Visual Field Esterman Monocular Test Pattern
      npoints = 100L,
      blindspotindices = integer(0), # ???
      X = c(
        -23L, 23L,
        -57L, -34L, -17L, -6L, 6L, 17L, 41L,
        -73L, -57L, -42L, -30L, -20L, -10L, -3L, 3L, 10L, 20L, 33L, 51L,
        -75L, -57L, -42L, -30L, -20L, -13L, -8L, 8L, 13L, 20L, 33L, 52L,
        -76L, -57L, -42L, -30L, -20L, -13L, -8L, 8L, 13L, 20L, 33L, 52L,
        -76L, -57L, -42L, -30L, -20L, -13L, -8L, -3L, 3L, 8L, 13L, 20L, 33L, 51L,
        -75L, -57L, -42L, -30L, -20L, -13L, -8L, -3L, 3L, 8L, 13L, 20L, 33L, 46L,
        -74L, -57L, -42L, -30L, -20L, -13L, -8L, -3L, 3L, 8L, 13L, 20L, 41L,
        -69L, -49L, -33L, -17L, -5L, 5L, 17L, 33L,
        -55L, -29L, -5L, 17L,
        -30L,
        0L),
      Y = rep(
        c(36L, 21L, 10L, 3L, -3L, -8L, -13L, -21L, -30L, -43L, -53L, -57L),
        times = c(2L, 7L, 12L, 12L, 12L, 14L, 14L, 13L, 8L, 4L, 1L, 1L)))),

  "OU" = list(
    "EB120" = list( # "Visual Field Esterman Binocular Test Pattern"
      npoints = 120L,
      blindspotindices = integer(0),
      X = c(
        -23L, 23L,
        -57L, -34L, -17L, -6L, 6L, 17L, 34L, 57L,
        -73L, -57L, -42L, -30L, -20L, -10L, -3L, 3L, 10L, 20L, 30L, 42L, 57L, 73L,
        -75L, -57L, -42L, -30L, -20L, -13L, -8L, 8L, 13L, 20L, 30L, 42L, 57L, 75L,
        -76L, -57L, -42L, -30L, -20L, -13L, -8L, 8L, 13L, 20L, 30L, 42L, 57L, 76L,
        -76L, -57L, -42L, -30L, -20L, -13L, -8L, -3L, 3L, 8L, 13L, 20L, 30L, 42L, 57L, 76L,
        -75L, -57L, -42L, -30L, -20L, -13L, -8L, -3L, 3L, 8L, 13L, 20L, 30L, 42L, 57L, 75L,
        -74L, -57L, -42L, -30L, -20L, -13L, -8L, -3L, 3L, 8L, 13L, 20L, 30L, 42L, 57L, 74L,
        -69L, -49L, -33L, -17L, -5L, 5L, 17L, 33L, 49L, 69L,
        -55L, -29L, -8L, 8L, 29L, 55L,
        -30L, 30L,
        -8L, 8L),
      Y = rep(
        c(36L, 21L, 10L, 3L, -3L, -8L, -13L, -21L, -30L, -43L, -53L, -57L),
        times = c(2L, 8L, 14L, 14L, 14L, 16L, 16L, 16L, 10L, 6L, 2L, 2L)))))


# add data.frame format to each component
HVF_XY_defaults <- sapply(
  HVF_XY_defaults,
  FUN = function(lst2){
    sapply(
      lst2,
      FUN = function(lst) {
        c(lst, XY = list(data.frame(X = lst$X, Y = lst$Y)))
      },
      USE.NAMES = TRUE, simplify = FALSE)
  },
  USE.NAMES = TRUE, simplify = FALSE)


usethis::use_data(HVF_XY_defaults, internal = TRUE)
