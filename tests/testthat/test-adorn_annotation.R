test_that("adorn_annotation adds annotation to the ggplot", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) + ggplot2::geom_point()
  p_annotated <- adorn_annotation(p)

  # Check if the returned object is still a ggplot
  expect_s3_class(p_annotated, "ggplot")

  # Check that an annotation was added
  annotations <- ggplot2::ggplot_build(p_annotated)$data[[2]]
  expect_true(nrow(annotations) > 0)
  expect_equal(annotations$label[1], "<-- Epidemic control")
})
