library(testthat)

context("Remove missing")
test_that("remove all NA from the vector", {
  expect_equal(remove_missing(c(1, 2, 3, NA)), c(1, 2, 3))
  expect_equal(remove_missing(c(1, 4, 5, 6)), c(1, 4, 5, 6))
  expect_equal(remove_missing(c(NA, NA, NA, 3)), 3)
  expect_equal(remove_missing(c(1, 2, 3, 4, NA, NA)), c(1, 2, 3, 4))
})

context("Get minimum")
test_that("get the minimum of the vector", {
  expect_equal(get_minimum(c(1, 2, 3, NA), na.rm = TRUE), 1)
  expect_equal(get_minimum(c(1, 4, 5, 6), na.rm = TRUE), 1)
  expect_equal(get_minimum(c(NA, NA, NA, 3), na.rm = TRUE), 3)
  expect_equal(get_minimum(c(1, 2, 3, 4, NA, NA), na.rm = TRUE), 1)
})

context("Get maximum")
test_that("get the maximum of the vector", {
  expect_equal(get_maximum(c(1, 2, 3, NA), na.rm = TRUE), 3)
  expect_equal(get_maximum(c(1, 4, 5, 6), na.rm = TRUE), 6)
  expect_equal(get_maximum(c(NA, NA, NA, 3), na.rm = TRUE), 3)
  expect_equal(get_maximum(c(1, 2, 3, 4, NA, NA), na.rm = TRUE), 4)
})

context("Get range")
test_that("get the range of the vector", {
  expect_equal(get_range(c(1, 2, 3, NA), na.rm = TRUE), 2)
  expect_equal(get_range(c(1, 4, 5, 6), na.rm = TRUE), 5)
  expect_equal(get_range(c(NA, NA, NA, 3), na.rm = TRUE), 0)
  expect_equal(get_range(c(1, 2, 3, 4, NA, NA), na.rm = TRUE), 3)
})

context("Get 10th percentile")
test_that("get the 10th percentile of the vector", {
  expect_equal(get_percentile10(c(1, 2, 3, NA), na.rm = TRUE), 1.2)
  expect_equal(get_percentile10(c(1, 4, 5, 6), na.rm = TRUE), 1.9)
  expect_equal(get_percentile10(c(NA, NA, NA, 3), na.rm = TRUE), 3)
  expect_equal(get_percentile10(c(1, 2, 3, 4, NA, NA), na.rm = TRUE), 1.3)
})

context("Get 90th percentile")
test_that("get the 90th percentile of the vector", {
  expect_equal(get_percentile90(c(1, 2, 3, NA), na.rm = TRUE), 2.8)
  expect_equal(get_percentile90(c(1, 4, 5, 6), na.rm = TRUE), 5.7)
  expect_equal(get_percentile90(c(NA, NA, NA, 3), na.rm = TRUE), 3)
  expect_equal(get_percentile90(c(1, 2, 3, 4, NA, NA), na.rm = TRUE), 3.7)
})

context("Get median")
test_that("get the median of the vector", {
  expect_equal(get_median(c(1, 2, 3, NA), na.rm = TRUE), 2)
  expect_equal(get_median(c(1, 4, 5, 6), na.rm = TRUE), 4.5)
  expect_equal(get_median(c(NA, NA, NA, 3), na.rm = TRUE), 3)
  expect_equal(get_median(c(1, 2, 3, 4, NA, NA), na.rm = TRUE), 2.5)
})

context("Get average")
test_that("get the average of the vector", {
  expect_equal(get_average(c(1, 2, 3, NA), na.rm = TRUE), 2)
  expect_equal(get_average(c(1, 4, 5, 6), na.rm = TRUE), 4)
  expect_equal(get_average(c(NA, NA, NA, 3), na.rm = TRUE), 3)
  expect_equal(get_average(c(1, 2, 3, 4, NA, NA), na.rm = TRUE), 2.5)
})

context("Get standard deviation")
test_that("get the standard deviation of the vector", {
  expect_equal(get_stdev(c(1, 2, 3, NA), na.rm = TRUE), sd(c(1, 2, 3)))
  expect_equal(get_stdev(c(1, 4, 5, 6), na.rm = TRUE), sd(c(1, 4, 5, 6)))
  expect_equal(get_stdev(c(NA, NA, NA, 3), na.rm = TRUE), NaN)
  expect_equal(get_stdev(c(1, 2, 3, 4, NA, NA), na.rm = TRUE), sd(c(1, 2, 3, 4)))
})

context("Get first quartile")
test_that("get the first quartile of the vector", {
  expect_equal(get_quartile1(c(1, 2, 3, NA), na.rm = TRUE), 1.5)
  expect_equal(get_quartile1(c(1, 4, 5, 6), na.rm = TRUE), 3.25)
  expect_equal(get_quartile1(c(NA, NA, NA, 3), na.rm = TRUE), 3)
  expect_equal(get_quartile1(c(1, 2, 3, 4, NA, NA), na.rm = TRUE), 1.75)
})

context("Get third quartile")
test_that("get the third quartile of the vector", {
  expect_equal(get_quartile3(c(1, 2, 3, NA), na.rm = TRUE), 2.5)
  expect_equal(get_quartile3(c(1, 4, 5, 6), na.rm = TRUE), 5.25)
  expect_equal(get_quartile3(c(NA, NA, NA, 3), na.rm = TRUE), 3)
  expect_equal(get_quartile3(c(1, 2, 3, 4, NA, NA), na.rm = TRUE), 3.25)
})

context("Counting missing values")
test_that("count_missing counts number of NAs in a vector",{
  expect_equal(count_missing(c(1,2,NA,3)), 1)
  expect_equal(count_missing(c(1,2,NA,NA)), 2)
  expect_equal(count_missing(c(1,NA,NA,NA)), 3)
  expect_equal(count_missing(c(1,2,3,4)), 0)
})

context("Summary of statistics")
test_that("summary_stats displays summary of statistics",{
  expect_equal(unname(summary_stats(c(1,2,3,4))["minimum"]), list(1))
  expect_equal(unname(summary_stats(c(1,2,3,4))["maximum"]), list(4))
  expect_equal(unname(summary_stats(c(1,2,3,4))["mean"]), list(2.5))
  expect_equal(unname(summary_stats(c(1,2,3,4))["median"]), list(2.5))
})

context("print out summary of statistics")
test_that("print_stats prints summary_stats",{
  x <- summary_stats(c(1,2,3,4))
  expect_output(print_stats(x[1]), "minimum : 1.0000")
  expect_output(print_stats(x[2]), "percent10 : 1.3000")
  expect_output(print_stats(x[3]), "quartile1 : 1.7500")
  expect_output(print_stats(x[4]), "median : 2.5000")
})

context("Rescale to 100")
test_that("rescale100 rescales numbers in a 100 scale", {
  expect_equal(rescale100(c(100), 0, 100), c(100))
  expect_equal(rescale100(c(10, 10), 0, 20), c(50, 50))
  expect_equal(rescale100(c(1, 1), 0, 20), c(5,5))
  expect_equal(rescale100(c(2,2), 0, 20), c(10,10))
})

context("Drop lowest value")
test_that("drop_lowest drops the lowest value", {
  expect_equal(drop_lowest(c(1,2,3)), c(2,3))
  expect_equal(drop_lowest(c(2,3)), c(3))
  expect_equal(drop_lowest(c(1,2,3, 4)), c(2,3,4))
  expect_equal(drop_lowest(c(3)), numeric(0))
})

context("Assign score to homeworks")
test_that("score_homework computes average of homework grades",{
  expect_equal(score_homework(c(1,2,3,4), drop = FALSE), 2.5)
  expect_equal(score_homework(c(2,3,4,5), drop = FALSE), 3.5)
})

test_that("score_homework drops the lowest score when drop == TRUE",{
  expect_equal(score_homework(c(1,2,3,4), drop = TRUE), 3)
  expect_equal(score_homework(c(2,3,4,5), drop = TRUE), 4)
})

context("Assign score to quizzes")
test_that("score_quiz computes average of quiz grades",{
  expect_equal(score_quiz(c(1,2,3,4), drop = FALSE), 2.5)
  expect_equal(score_quiz(c(2,3,4,5), drop = FALSE), 3.5)
})

test_that("score_quiz drops the lowest score when drop == TRUE",{
  expect_equal(score_quiz(c(1,2,3,4), drop = TRUE), 3)
  expect_equal(score_quiz(c(2,3,4,5), drop = TRUE), 4)
})

context("Grade lab based on attendance")
test_that("score_lab assigns lab score according to attendances", {
  expect_equal(score_lab(1), 0)
  expect_equal(score_lab(11), 100)
  expect_equal(score_lab(7), 20)
  expect_equal(score_lab(8), 40)
})


