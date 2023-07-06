# library(mapsf)
# # library(osrm)
# # library(rmapshaper)
# #
# # mtq <- mf_get_mtq()
# # m <- mtq[c(19,11,15,3,1,14,25,18),]
# # poly <- rmapshaper::ms_simplify(m, keep = 0.01)
# # point <- st_centroid(poly)
# # route1 <- osrmRoute(src = point[1,], dst = point[7,],
# #                     returnclass="sf", overview = "full")
# # route2 <- osrmRoute(src = point[7,], dst = point[8,],
# #                     returnclass="sf", overview = "full")
# # route3 <- osrmRoute(src = point[8,], dst = point[5,],
# #                     returnclass="sf", overview = "full")
# # route4 <- osrmRoute(src = point[5,], dst = point[4,],
# #                     returnclass="sf", overview = "full")
# # route5 <- osrmRoute(src = point[4,], dst = point[3,],
# #                     returnclass="sf", overview = "full")
# # route6 <- osrmRoute(src = point[3,], dst = point[2,],
# #                     returnclass="sf", overview = "full")
# # line <- do.call(rbind, list(route1, route2, route3, route4, route5))
# # line <- rmapshaper::ms_simplify(line, keep = 0.01)
# #
# # save(list=c("point", "poly", "line"), file = "dt.RData")
# load('vignettes/dt.RData')
#
# library(mapsf)
# mf_export(poly,filename = "vignettes/fig/th_default.svg", width = 4, theme = "default")
# mf_shadow(poly, add = T)
# mf_map(poly, add = T)
# mf_title("default")
# dev.off()
#
#
# custom <- list(
#   name = "custom",
#   bg = NA,
#   fg = "grey20",
#   mar = c(0, 0, 0, 0),
#   tab = TRUE,
#   pos = "center",
#   inner = TRUE,
#   line = 2,
#   cex = 1.5,
#   font = 3
# )
# mf_theme(custom)
#
# svg("fig/point.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_map(point,  pch = 4, col = "black",lwd = 2, cex = 1, add = T)
# dev.off()
#
#
# # Base map
# svg("fig/point.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_map(point,  pch = 4, col = "black",lwd = 2, cex = 1, add = T)
# dev.off()
#
#
# svg("fig/line.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_map(line,  lwd = 2, cex = 1, add = T, col = "grey20")
# dev.off()
#
#
# svg("fig/poly.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_map(poly, lwd = 2, add = T)
# dev.off()
#
#
# # Proportional
# svg("fig/point_p.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_prop(point, var = "POP", inches = .1, leg_pos = NA)
# dev.off()
# svg("fig/poly_p.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_prop(poly, var = "POP", inches = .1, leg_pos = NA)
# dev.off()
# svg("fig/line_p.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# set.seed(666)
# line$dd <- sample(c(1:5), size = 5, replace = F)
# mf_prop(line, var = "dd", lwd_max = 5, leg_pos = "n")
# dev.off()
#
# # Typology
# svg("fig/point_t.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_typo(point, var = "STATUS", leg_pos = NA, add = T)
# dev.off()
# svg("fig/line_t.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_typo(line, var = "src", leg_pos = NA, lwd = 2, add = T)
# dev.off()
# svg("fig/poly_t.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_typo(poly, var = "STATUS", leg_pos = NA, add = T)
# dev.off()
#
#
# # choropleth
# svg("fig/point_c.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_choro(point, var = "MED", leg_pos = NA, add = T)
# dev.off()
# svg("fig/line_c.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_choro(line, var = "duration", lwd = 2, leg_pos = NA, add = T)
# dev.off()
# svg("fig/poly_c.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_choro(poly, var = "MED", leg_pos = NA, add = T)
# dev.off()
#
#
# # Symbology
# svg("fig/point_s.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_symb(point, var = "STATUS", leg_pos = NA,
#          pch = c(21,24), pal = c(2,2) )
# dev.off()
# svg("fig/poly_s.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_symb(poly, var = "STATUS", leg_pos = NA,
#          pch = c(21,24), pal = c(2,2))
# dev.off()
#
#
#
# # prop choro
# svg("fig/point_pc.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_prop_choro(point, var = c("POP", "MED"), inches = .1, leg_pos = NA)
# dev.off()
# svg("fig/poly_pc.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_prop_choro(poly, var = c("POP", "MED"), inches = .1, leg_pos = NA)
# dev.off()
#
#
# # prop typo
# svg("fig/point_pt.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_prop_typo(point, var = c("POP", "STATUS"), inches = .1, leg_pos = NA)
# dev.off()
# svg("fig/poly_pt.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_prop_typo(poly, var = c("POP", "STATUS"), inches = .1, leg_pos = NA)
# dev.off()
#
# dev.off()
# svg("vignettes/fig/line_pt.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# line$cat = c("A", "A", "A", "B", "B")
# mf_prop_typo(x = line, var = c("dd", "cat"), lwd_max = 5, leg_pos = c(NA, NA))
# dev.off()
#
# getwd()
#
# # symb choro
# svg("fig/point_sc.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_symb_choro(point, var = c("STATUS", "MED"), leg_pos = NA,
#           pch = c(21,24), pal = "Mint")
# dev.off()
# svg("fig/poly_sc.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_symb_choro(poly, var = c("STATUS", "MED"), leg_pos = NA,
#          pch = c(21,24), pal = "Mint")
# dev.off()
#
#
#
#
# # dot density
# svg("fig/poly_dd.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_map(poly)
# mf_map_dots(poly, var = c("CHOM"), n = 5, pch = ".",
#           cex = .1, col = "tomato4", leg_pos = "n")
# dev.off()
#
#
# # disc
# svg("fig/poly_d.svg", width = 1, height = 1, bg = NA)
# a <- mf_get_borders(poly)
# mf_init(poly)
# mf_map(poly, add = T, border = "white")
# mf_map_disc(a, df = poly, var = c("CHOM"), sizemax = 7,
#          leg_pos = "n", threshold = .5)
# dev.off()
#
#
# # grad
# # choropleth
# svg("fig/point_g.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_grad(point, var = "POP", leg_pos = NA, add = T, nbreaks = 2, pch = 22, cex = c(1,2))
# dev.off()
# svg("fig/line_g.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_grad(line, var = "duration", leg_pos = NA, add = T, lwd = c(1,3), nbreaks = 2)
# dev.off()
# svg("fig/poly_g.svg", width = 1, height = 1, bg = NA)
# mf_init(poly)
# mf_grad(poly, var = "POP", leg_pos = NA, add = T, nbreaks = 2, pch = 22, cex = c(1,2))
# dev.off()
#
#
#
#
#
# # theme
# th <- c("default", "brutal", "ink",
# "dark", "agolalight", "candy", "darkula", "iceberg", "green", "nevermind",
# "jsk")
# par(bg = "cornsilk")
# for (i in th){
#   mf_init(poly, theme = i, export = "svg",
#           filename = paste0("fig/th_", i, ".svg"), width = 4)
#   mf_shadow(mtq, add = T)
#   mf_map(poly, add = T)
#   mf_title(i)
#   dev.off()
# }
#
#
# # annotations
# mf_init(poly, theme = "agolalight",
#         export = "svg",
#         filename = paste0("fig/deco.svg"), width = 6)
# mf_shadow(mtq, add = T)
# mf_map(poly, add = T)
# mf_annotation(x = point[7,], txt = "Annotation", cex = 1.2, s = 1.5, halo = T)
# mf_label(poly, var = 'LIBGEO')
# mf_layout()
# dev.off()
#
#
#
#
#
#
