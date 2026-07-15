test_that("pp_figure_placeholders() moves figures and inserts placeholders", {
  local_temp_wd()

  dir_create("foo_reprex_files/figure-gfm")
  write_lines("not really a png", "foo_reprex_files/figure-gfm/blah-1.png")
  write_lines("not really a png", "foo_reprex_files/figure-gfm/blah-2.png")
  # fmt: skip
  md_lines <- c(
    "``` r",
    "plot(1:3)",
    "```",
    "![](foo_reprex_files/figure-gfm/blah-1.png)<!-- -->",
    "![](foo_reprex_files/figure-gfm/blah-2.png)",
    "![](https://i.imgur.com/woc4vHs.png)<!-- -->",
    "![](some-other-local-file.png)",
    "some prose"
  )
  write_lines(md_lines, "foo_reprex.md")

  pp_figure_placeholders("foo_reprex.md")

  out <- read_lines("foo_reprex.md")
  expect_equal(out[1:3], md_lines[1:3])
  expect_equal(out[4], "**Insert plot here:** `reprex-plots/plot-1.png`")
  expect_equal(out[5], "**Insert plot here:** `reprex-plots/plot-2.png`")
  # links to remote figures and to files reprex did not create are left as is
  expect_equal(out[6:8], md_lines[6:8])

  expect_true(file_exists("reprex-plots/plot-1.png"))
  expect_true(file_exists("reprex-plots/plot-2.png"))
  expect_false(file_exists("foo_reprex_files/figure-gfm/blah-1.png"))
  expect_false(file_exists("some-other-local-file.png"))
})

test_that("local_figures = TRUE saves figures to reprex-plots, with placeholders", {
  skip_on_cran()
  local_temp_wd()

  # note: no `input` filepath and no `wd`, so the reprex itself is rendered in
  # a subdirectory of the session temp dir, but figures must still land here
  out <- reprex(
    input = c("plot(1:3)", "hist(rnorm(100))"),
    local_figures = TRUE
  )

  placeholder <- grep("Insert plot here", out, value = TRUE)
  expect_length(placeholder, 2)
  expect_match(placeholder[1], "`reprex-plots/plot-1.png`", fixed = TRUE)
  expect_match(placeholder[2], "`reprex-plots/plot-2.png`", fixed = TRUE)
  expect_true(file_exists("reprex-plots/plot-1.png"))
  expect_true(file_exists("reprex-plots/plot-2.png"))

  # no figure link is left behind
  expect_no_match(out, "!\\[\\]")
})

test_that("local figures are embedded in the HTML preview", {
  skip_on_cran()
  local_temp_wd()

  write_lines("plot(1:3)\n", "foo.R")
  reprex(input = "foo.R", local_figures = TRUE, render = FALSE)

  withr::local_envvar(c(RMARKDOWN_PREVIEW_DIR = "."))
  rlang::local_interactive(FALSE)

  capture.output(
    reprex_render("foo_reprex.R", html_preview = TRUE),
    type = "message"
  )

  # the markdown holds the placeholder, not a figure link
  md_lines <- read_lines("foo_reprex.md")
  expect_match(md_lines, "Insert plot here", all = FALSE)
  expect_true(file_exists("reprex-plots/plot-1.png"))

  # but the preview shows the actual figure, embedded as a data URI
  preview_lines <- read_lines("foo_reprex_preview.html")
  expect_match(preview_lines, "data:image/png;base64", all = FALSE)
  expect_no_match(preview_lines, "Insert plot here")
})
