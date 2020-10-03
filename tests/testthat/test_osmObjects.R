# Martynas Lukosevicius: Check what we do with strange city names
# Martynas Lukosevicius: Check what we do with keys which not exist
# Martynas Lukosevicius: Find a small city which has one supermarket and check coordinates


context("osmObjects")

test_that("Strange name city", {
  expect_error(osmObjects(213, "fuel"))
 #expect_error(osmObjects("123", "fuel")) # finds city in ljiublijana
  expect_error(osmObjects("apsdmap aspdlasa12", "fuel"))
  expect_error(osmObjects("", "fuel"))
 #expect_error(osmObjects("%sad", "fuel")) # city Szágy
})

test_that("Object passed not in the list", {
  expect_error(osmObjects("Linköping", "123"))
  expect_error(osmObjects("Linköping", ""))
  expect_error(osmObjects("Linköping", "cash"))
})

test_that("Strange city and wrong object", {
  expect_error(osmObjects("", "123"))
  expect_error(osmObjects("", ""))
  expect_error(osmObjects("123", ""))
  expect_error(osmObjects("asfaa", "cash"))
  expect_error(osmObjects("%sad", "cash"))
})


test_that("getBoundingBox rejects strange city", {
  expect_error(getBoundingBox(213)) # reject integer
  #expect_error(getBoundingBox("123")) # finds city in ljiublijana
  expect_error(getBoundingBox("apsdmap aspdlasa12"))
  #expect_error(getBoundingBox("%sad")) # city Szágy
})

bbox <- matrix(nrow = 2, ncol = 2)

test_that("getElements rejects strange bbox matrix", {
  expect_error(getElements(bbox, "amenity", "pharmacy"))
})

bbox[1,] <- c("sad", 23)
bbox[2,] <- c(23, 42)

test_that("getElements rejects strange bbox matrix 2", {
  expect_error(getElements(bbox, "amenity", "pharmacy"))
})

bbox <- matrix(nrow = 2, ncol = 2)
bbox[1,] <- c(32, 23)
bbox[2,1] <- 32


test_that("getElements rejects strange bbox matrix 2", {
  expect_error(getElements(bbox, "amenity", "pharmacy"))
})

bbox <- matrix(nrow = 2, ncol = 2) # Retrieve error if NAs
bbox[1, ] <- c(32, 23)
bbox[2, ] <- c(32, 32)

test_that("getElements rejects strange key", {
  expect_error(getElements(bbox, "safsa", "pharmacy"))
  expect_error(getElements(bbox, "", "pharmacy"))
  expect_error(getElements(bbox, 2352, "pharmacy"))
})


test_that("getElements rejects strange value", {
  expect_error(getElements(bbox, "amenity", "cash"))
  expect_error(getElements(bbox, "amenity", ""))
  expect_error(getElements(bbox, "amenity", 123))
})


London <- osmObjects("London", "hospital")
test_that("Desired output with big city", {
  expect_equal(class(London$coordinates), "data.frame")
  expect_false(dim(London$Coordinates)[1] == 0 )
})
