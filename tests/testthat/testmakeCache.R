test_that("makeCacheMatrix is a list", {
  a <- matrix(c(3,1,2,2,1,3,3,2,1), ncol = 3, byrow = TRUE)
  x <- makeCacheMatrix(a)
  expect_equal(is.list(x),T)
})

test_that("Cache is set when called", {
  a <- matrix(c(3,1,2,2,1,3,3,2,1), ncol = 3, byrow = TRUE)
  x <- makeCacheMatrix(a)
  ## The list has getInverse property
  expect_equal(is.null(x$getInverse),F)
  ## When we call the function the cache is null 
  expect_equal(is.null(x$getInverse()),T)
  ## The matrix is the same 
  expect_equal(a,x$get())
  result <- cacheSolve(x)
  ## Now the inverse is set
  expect_equal(is.null(x$getInverse()),F)
  ## And its equal to result
  expect_equal(result,x$getInverse())
})