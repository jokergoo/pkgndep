
# == title (variable:heaviness_db)
# Version of the heaviness database
#
# == example
# heaviness_db
heaviness_db = list(
	built_date = "2022-06-08",
	n_pkg_cran = 18638,
	n_pkg_bioc = 3438,
	bioc_version = "3.15"
	
)
class(heaviness_db) = "heaviness_db_info"

# == title
# Print the heaviness_db_info object
#
# == param
# -x A ``heaviness_db_info`` object.
# -... Other arguments
#
# == value
# No value is returned.
#
# == example
# heaviness_db
print.heaviness_db_info = function(x, ...) {
	cat("Dependency heaviness database\n")
	cat("  Packages on CRAN:", x$n_pkg_cran, "\n")
	cat("  Packages on Bioc: ", x$n_pkg_bioc, " (bioc", x$bioc_version, ")\n", sep = "")
	cat("  Built date:", x$built_date, "\n")
}
