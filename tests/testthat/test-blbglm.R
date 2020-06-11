test_that("glm works", {



  d.AD <- data.frame(treatment = gl(3,3),
                     outcome   = gl(3,1,9),
                     counts    = c(18,17,15, 20,10,20, 25,13,12))
  # glm.D93 <- glm(counts ~ outcome + treatment, d.AD, family = poisson())

  expect_invisible(new_glm <- blbglm(counts ~ outcome + treatment, data = d.AD, family = poisson(),m = 2,B = 10)
  )

})


