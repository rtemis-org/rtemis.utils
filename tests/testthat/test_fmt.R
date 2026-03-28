# test_fmt.R
# ::rtemis.utils::
# 2026- EDG rtemis.org

# %% fmt
test_that("fmt works with ansi output", {
  expect_equal(
    fmt("Hello", col = "#008080", bold = TRUE, output_type = "ansi"),
    "\033[1;38;2;0;128;128mHello\033[0m"
  )
})

test_that("fmt works with plain output", {
  expect_equal(
    fmt("Hello", col = "#008080", bold = TRUE, output_type = "plain"),
    "Hello"
  )
})
