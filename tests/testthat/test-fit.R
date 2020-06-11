test_that("Test if the blblm function can work", {

  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)

  expect_vector(coef(fit))
  expect_vector(confint(fit, c("wt", "hp")))
  expect_vector(sigma(fit))
  expect_vector(sigma(fit, confidence = TRUE))
  expect_vector(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170))))
  expect_vector(predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE))

  }
)

