# test_fmt.R
# ::rtemisutils::
# 2026- EDG rtemis.org

# %% fmt
test_that("fmt works", {
  expect_equal(
    fmt("Hello", col = "#008080", bold = TRUE),
    "\033[1;38;2;0;128;128mHello\033[0m"
  )
})
