
## mf_theme
expect_silent(mf_theme())
expect_error(mf_theme("NoT a thEmE"))
expect_silent(mf_theme(x = list(
  name = "custom",
  bg = "black",
  fg = "yellow",
  mar = c(5, 5, 5, 5),
  tab = TRUE,
  pos = "right",
  inner = FALSE,
  line = 4,
  cex = 2,
  font = 3
)))
expect_silent(mf_theme(bg = "darkslategrey", fg = "cornsilk3",
                       mar = c(2, 2, 4, 2),
                       tab = FALSE, pos = "center", inner = FALSE,
                       line = 2, cex = 2, font = 4)
)
mtq <- mf_get_mtq()
expect_silent(mf_map(mtq, "POP", "prop", leg_frame = TRUE, add = FALSE))
mf_map(mtq)
expect_silent(mf_theme(NULL))
