mtq <- mf_get_mtq()
expect_silent(mf_export(mtq, export = "png", height = 600,
                        filename = tempfile()))
dev.off()
expect_silent(mf_export(mtq, export = "png", filename = tempfile()))
dev.off()

expect_silent(mf_export(mtq, export = "png", filename = tempfile(),
                        width = 800, height = 800))
dev.off()

expect_silent(mf_export(mtq, export = "svg", filename = tempfile(),
                        width = 6, height = 6))
dev.off()
expect_silent(mf_export(mtq, theme = "darkula", export = "svg",
                        filename = tempfile()))
dev.off()
expect_message(mf_export(mtq, export = "svg", width = 51,
                         filename = tempfile()))
dev.off()

expect_silent(mf_export(mtq, export = "svg", height = 7,
                        filename = tempfile()))
dev.off()

b <- raster::brick(system.file("external/rlogo.grd", package="raster"))
expect_silent(mf_export(b))
dev.off()
