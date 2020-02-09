test_that("package works", {
  st <- Storage$new(path = tempdir())
  expect_is(
    st, 
    "Storage"
  )
  expect_is(
    st, 
    "R6"
  )
  nsp <- paste0(
    sample(letters, 24), 
    collapse = ""
  )
  st$set("pouet", nsp)
  
  res <- st$get(nsp)
  expect_equal(
    res, 
    "pouet"
  )
  expect_is(
    res, 
    "character"
  )
  
  st$rm(nsp)
  
  expect_error(
    st$get(nsp)
  )
  
  nsp <- paste0(
    sample(letters, 24), 
    collapse = ""
  )
  
  st$set(iris, "pouet", nsp)
  res <- st$get("pouet", nsp)
  expect_equal(
    res, 
    iris
  )
  expect_is(
    res, 
    "data.frame"
  )
  
  st$rm("pouet", nsp)
  
  st$remove_namespace(
    "global"
  )
  
  expect_error(
    st$get("pouet")
  )
  
})
