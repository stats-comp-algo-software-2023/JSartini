# Tests to make sure that the vital helper function are_all_close
# operates appropriately
test_that("are_all_close behaves as expected", {
  # Create first example data vector
  rand_example_vector = rnorm(50)

  # Check that changes by less than machine precision result in true output
  expect_true(are_all_close( rand_example_vector,
      rand_example_vector + sample(c(1, -1), 50, replace = TRUE)*(.Machine$double.eps/2)
  ))

  # Create more specific example data vector for latter 2 tests
  fix_example_vector = c(0.5, -3, 2, 0.8, -6)

  # Ensure sufficiently large absolute deviations (randomly assigned signs) returns a false output
  expect_false(are_all_close( fix_example_vector,
                             fix_example_vector + sample(c(1, -1), 5, replace = TRUE)*(1e-4),
                              1e-5, 1
  ))

  # Test that sufficiently large relative deviations returns a false output
  expect_false(are_all_close( fix_example_vector,
                              fix_example_vector * (1 + sample(c(1,-1), 5, replace = TRUE)*(1e-2)),
                              1e-3, 0.5
  ))
})
