test_that("CzyFilm return logical", {
   expect_equal(is.logical(CzyFilm("http://www.imdb.com/title/tt0395699/")), TRUE)
})
test_that("PobierzFilm return list", {
   expect_equal(is.list(PobierzFilm("http://www.imdb.com/title/tt0395699/")), TRUE)
})
test_that("PobierzLinki return TRUE", {
   expect_equal(PobierzLinki(gatunki = c("film_noir")), TRUE)
})


