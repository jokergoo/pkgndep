

ENV = new.env()

DEFAULT_LOADED_BASE_PKGS = c("base", "stats", "graphics", "grDevices", "utils", "datasets", "methods")

FIELDS = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")


if(grepl("devel", R.version$status)) {  # only for CRAN check on R-devel where BiocManager::version() throws error 
    bioc_version = packageVersion("BiocVersion")[, 1:2]
    bioc_version = as.character(bioc_version)
} else {
    bioc_version = BiocManager::version()
    bioc_version = as.character(bioc_version)
}   



# == title (variable:ALL_BIOC_RELEASES)
# All Bioconductor releases 
#
# == value
# A data frame
#
# == example
# ALL_BIOC_RELEASES
ALL_BIOC_RELEASES = read.table(textConnection(
"Release;Date;Software packages;R;URL
3.16;2022-11-02;2183;4.2.2;https://bioconductor.org/packages/3.16/
3.15;2022-04-27;2140;4.2.0;https://bioconductor.org/packages/3.15/
3.14;2021-10-27;2083;4.1.2;https://bioconductor.org/packages/3.14/
3.13;2021-05-20;2042;4.1.0;https://bioconductor.org/packages/3.13/
3.12;2020-10-28;1974;4.0.3;https://bioconductor.org/packages/3.12/
3.11;2020-04-28;1903;4.0.0;https://bioconductor.org/packages/3.11/
3.10;2019-10-30;1823;3.6.1;https://bioconductor.org/packages/3.10/
3.9;2019-05-03;1741;3.6.0;https://bioconductor.org/packages/3.9/
3.8;2018-10-31;1649;3.5.1;https://bioconductor.org/packages/3.8/
3.7;2018-05-01;1560;3.5.0;https://bioconductor.org/packages/3.7/
3.6;2017-10-31;1473;3.4.2;https://bioconductor.org/packages/3.6/
3.5;2017-04-25;1383;3.4.0;https://bioconductor.org/packages/3.5/
3.4;2016-10-18;1296;3.3.1;https://bioconductor.org/packages/3.4/
3.3;2016-05-04;1211;3.3.0;https://bioconductor.org/packages/3.3/
3.2;2015-10-14;1104;3.2.2;https://bioconductor.org/packages/3.2/
3.1;2015-04-17;1024;3.2.0;https://bioconductor.org/packages/3.1/
3.0;2014-10-14;934;3.1.1;https://bioconductor.org/packages/3.0/
2.14;2014-04-14;824;3.1.0;https://bioconductor.org/packages/2.14/
2.13;2013-10-15;749;3.0.2;https://bioconductor.org/packages/2.13/
2.12;2013-04-04;671;3.0.0;https://bioconductor.org/packages/2.12/
2.11;2012-10-03;610;2.15.1;https://bioconductor.org/packages/2.11/
2.10;2012-04-02;554;2.15.0;https://bioconductor.org/packages/2.10/
2.9;2011-11-01;517;2.14.0;https://bioconductor.org/packages/2.9/
2.8;2011-04-14;466;2.13.0;https://bioconductor.org/packages/2.8/
2.7;2010-10-18;418;2.12.0;https://bioconductor.org/packages/2.7/
2.6;2010-04-23;389;2.11.0;https://bioconductor.org/packages/2.6/
2.5;2009-10-28;352;2.10.0;https://bioconductor.org/packages/2.5/
2.4;2009-04-21;320;2.9.0;https://bioconductor.org/packages/2.4/BiocViews.html
2.3;2008-10-22;294;2.8.0;https://bioconductor.org/packages/2.3/BiocViews.html
2.2;2008-05-01;260;2.7.0;https://bioconductor.org/packages/2.2/BiocViews.html
2.1;2007-10-08;233;2.6.0;https://bioconductor.org/packages/2.1/BiocViews.html
2.0;2007-04-26;214;2.5.0;https://bioconductor.org/packages/2.0/BiocViews.html
1.9;2006-10-04;188;2.4.0;https://bioconductor.org/packages/1.9/BiocViews.html
1.8;2006-04-27;172;2.3.0;https://bioconductor.org/packages/1.8/BiocViews.html
1.7;2005-10-14;141;2.2.0;https://bioconductor.org/packages/bioc/1.7/src/contrib/html/
1.6;2005-05-18;123;2.1.0;https://bioconductor.org/packages/bioc/1.6/src/contrib/html/
1.5;2004-10-25;100;2.0.0;https://bioconductor.org/packages/bioc/1.5/src/contrib/html/
#1.4;2004-05-17;81;1.9;
#1.3;2003-10-30;49;1.8;
#1.2;2003-05-29;30;1.7;
#1.1;2002-11-19;20;1.6;
#1.0;2002-05-01;15;1.5;
"), header = TRUE, sep = ";", colClasses = rep("character", 5))



# == title
# Global parameters for pkgndep
#
# == param
# -... Arguments for the parameters, see "details" section
# -RESET Reset to default values.
# -READ.ONLY Please ignore.
# -LOCAL Pllease ignore.
# -ADD Please ignore.
# 
# == details
# There are following parameters:
# 
# -``bioc_version`` The bioconductor version. By default it is the version corresponding to the R version under use. Please note this option
#     is only for switching between bioc release version and development version, while not for switching to very old bioc versions.
# -``heaviness_db_version`` The version of the heaviness database. The value can be the corresponding bioc version, the R version or the corresponding date for the bioc release. All supported values are in the object `ALL_BIOC_RELEASES`.
#
# == example
# pkgndep_opt
pkgndep_opt = function(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE) {}

pkgndep_opt = setGlobalOptions(
    tmp_dir = list(.value = ".", .visible = FALSE),
    add_link = list(.value = FALSE, .visible = FALSE),
    bioc_version = bioc_version,
    heaviness_db_version = list(
        .value = ALL_BIOC_RELEASES$Date[1],
        .filter = function(x) {
            rm(list = ls(envir = ENV), envir = ENV)
            if(grepl("^\\d\\.\\d\\.\\d$", x)) {
                x = ALL_BIOC_RELEASES$Date[ALL_BIOC_RELEASES$R == x]
            } else if(grepl("^\\d\\.\\d+$", x)) {
                x = ALL_BIOC_RELEASES$Date[ALL_BIOC_RELEASES$Release == x]
            }
            if(length(x) == 0) {
                stop_wrap("Wrong value for `heaviness_db_version`. Check the values in the object `heaviness_db_version`.")
            }
            x
        }
    ),
    db_file_template = list(
        .value = function(file, version) {
            paste0("https://pkgndep.github.io/", version, "/", file)
        },
        .class = "function",
        .visible = FALSE
    )
)


