
mat <- matrix(c(0,1, 1,1, 1, 0, 0,1), ncol = 2, byrow = TRUE, dimnames = list(paste0(seq(4)), c("pika", "sala")))

test_that("similarity index works", {

test_jaccard <- get_temporal_vegdist(
  dist_mat = 1 - as.matrix(vegan::vegdist(mat, method = "jaccard")),
  drop_first_year = TRUE
)

test_hillebrand <- get_temporal_vegdist(
    dist_mat = compute_dist(mat = mat, fun = compute_hillebrand,
      similarity = TRUE),
    drop_first_year = TRUE
)

test_total <- get_temporal_vegdist(
    dist_mat = compute_dist(mat = mat,
      fun = compute_codyn_turnover, type = "total"),
    drop_first_year = TRUE
)

test_appearance <- get_temporal_vegdist(
    dist_mat = compute_dist(mat = mat,
      fun = compute_codyn_turnover, type = "appearance"),
    drop_first_year = TRUE
)

test_disappearance <- get_temporal_vegdist(
    dist_mat = compute_dist(mat = mat,
      fun = compute_codyn_turnover, type = "disappearance"),
    drop_first_year = TRUE
)

expected_similarity <- c("2" = .5, "3" = 0, "4" = 1)
expected_turnover <- 1 - c("2" = .5, "3" = 0, "4" = 1)
expected_appearance <- c("2" = .5, "3" = .5, "4" = 0)
expected_disappearance <- c("2" = 0, "3" = 0.5, "4" = 0)

testthat::expect_equal(test_jaccard, expected_similarity)
testthat::expect_equal(test_hillebrand, expected_similarity)
testthat::expect_equal(test_total, expected_turnover)
testthat::expect_equal(test_appearance, expected_appearance)
testthat::expect_equal(test_disappearance, expected_disappearance)

})
