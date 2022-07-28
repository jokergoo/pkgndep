context("Test pkgndep")

library(pkgndep)

if(!grepl("devel", R.version$status)) {
	db = available.packages(repos = BiocManager::repositories())
	p1 = tools::package_dependencies("cola", db = db, recursive = TRUE)[[1]]
	db2 = reformat_db(db)
	p2 = db2$package_dependencies("cola", recursive = TRUE, simplify = TRUE)


	# test will be added later
	test_that("Test package_dependencies() on cola", {
	    
	    expect_that(sort(p1), is_identical_to(sort(p2)))

	})

	db = available.packages(repos = BiocManager::repositories())
	p1 = tools::package_dependencies("ggplot2", db = db, recursive = TRUE)[[1]]
	db2 = reformat_db(db)
	p2 = db2$package_dependencies("ggplot2", recursive = TRUE, simplify = TRUE)

	test_that("Test package_dependencies() on ggplot2", {
	    
	    expect_that(sort(p1), is_identical_to(sort(p2)))

	})

	tb = parent_dependency("ComplexHeatmap")

	dcf = read.dcf(system.file("DESCRIPTION", package = "ComplexHeatmap"))

	v1 = pkgndep:::env$pkg_db_snapshot$meta["ComplexHeatmap", "Version"]
	v2 = dcf[1, "Version"]

	if(v1 == v2) {
		depends = strsplit(dcf[1, "Depends"], ",\\s+")[[1]]
		imports = strsplit(dcf[1, "Imports"], ",\\s+")[[1]]
		suggests = strsplit(dcf[1, "Suggests"], ",\\s+")[[1]]

		depends = gsub("\\s+\\(.*\\)", "", depends); depends = depends[depends != "R"]
		imports = gsub("\\s+\\(.*\\)", "", imports)
		suggests = gsub("\\s+\\(.*\\)", "", suggests)

		test_that("parent_dependency", {
			expect_that(sort(depends), is_identical_to(sort(tb$parents[tb$dep_fields == "Depends"])))
			expect_that(sort(imports), is_identical_to(sort(tb$parents[tb$dep_fields == "Imports"])))
			expect_that(sort(suggests), is_identical_to(sort(tb$parents[tb$dep_fields == "Suggests"])))
		})
	}


	test_that("parent relations", {
		expect_that(is_parent("grid", "ComplexHeatmap"), is_identical_to(TRUE))
		expect_that(is_upstream("grid", "ggrepel"), is_identical_to(TRUE))
	})

	test_that("test github packages", {
		x = pkgndep("https://github.com/jokergoo/ComplexHeatmap")
	})
}
