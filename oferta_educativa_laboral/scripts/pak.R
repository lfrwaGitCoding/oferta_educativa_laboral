library(pak)

dir.exists(".github")
devel <- "~/Documents/work/science/devel/github/antoniojbt/oferta_educativa_laboral/"
pak_loc <- file.path(paste0(devel, ".github/"))
dir.exists(pak_loc)

pak::lockfile_create(
    pkg = c("deps::oferta_educativa_laboral",
            "AntonioJBT/episcout"),
    lockfile = paste0(pak_loc, "pkg.lock")
    )
