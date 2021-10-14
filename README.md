# Check the Heaviness of Package Dependencies

[![R-CMD-check](https://github.com/jokergoo/pkgndep/workflows/R-CMD-check/badge.svg)](https://github.com/jokergoo/pkgndep/actions)
[![CRAN](https://www.r-pkg.org/badges/version/pkgndep)](https://cran.r-project.org/web/packages/pkgndep/index.html)


When developing R packages, we should try to avoid directly setting
dependencies on "heavy packages". The "heaviness" for a package means, the
number of additional dependent packages it brings to. If your package directly depends
on a heavy package, it would bring several consequences:

1. Users need to install a lot of additional packages when installing your
   package (which brings the risk that installation of some packages
   may fail and it makes your package cannot be installed neither). 
2. The namespaces that are loaded into your R session after loading your package (by
   `library(your-pkg)`) will be huge (you can see the loaded namespaces by `sessionInfo()`).
3. You package will be "heavy" as well and it may take long time to load your package.

In the DESCRIPTION file of your package, those "directly dependent pakcages"
are always listed in the `Depends` and `Imports` fields. To get rid of the heavy
packages that are not often used in your package, it is better to move them
into the `Suggests`/`Enhances` fields and to load them only when they are needed.

Here the **pkgndep** package checks the heaviness of the dependency packages of your
package. For each package listed in the `Depends`, `Imports` and
`Suggests`/`Enhances` fields in the DESCRIPTION file, it opens a new R session, only loads the
package and counts the number of namespaces that are loaded. The summary of
the dependency is visualized by a customized heatmap.

As an example, I am developing a package called
[**cola**](https://github.com/jokergoo/cola/) which depends on [a lot of other
packages](https://github.com/jokergoo/ComplexHeatmap/blob/master/DESCRIPTION).
The dependency heatmap looks like (please drag the figure to a new tab to see it in the actual size):

<p><img src='https://user-images.githubusercontent.com/449218/137299488-17792dcd-79ca-4f2c-8a22-be19dd91e169.png' width="100%" /></p>

In the heatmap, rows are the packages listed in `Depends`, `Imports` and `Suggests` fields,
columns are the namespaces that are loaded if each of the dependency package is only loaded to a new R session.
The two barplots on the right show the number of additional namespaces that are loaded by each dependency package
and the number of imported functions/methods/classes from each dependency package.

We can see if all the packages are put in the `Depends` or `Imports` field, 210 namespaces
will be loaded after `library(cola)`. Some of the heavy packages such as
**WGCNA**, **clusterProfiler** and **ReactomePA** (the last three packages in the heatmap rows) are not very frequently used in **cola**,
moving them to `Suggests` field and loading them only when they are needed
helps to speed up loading **cola**. Now the number of namespaces are reduced
to only 50 after executing `library(cola)`.


## Installation

Prior to installing this package, you'll need to install the Bioconductor package [**ComplexHeatmap**](https://bioconductor.org/packages/release/bioc/html/ComplexHeatmap.html) by

```r
BiocManager::install("ComplexHeatmap")
```

The **pkgndep** package can be installed from CRAN by

```r
install.packages("pkgndep")
```

## Usage

To use this package:

```r
library(pkgndep)
x = pkgndep("package-name")
plot(x)
```

or

```r
x = pkgndep("path-to-the-package")
plot(x)
```

An executable example:

```r
library(pkgndep)
x = pkgndep("ComplexHeatmap")
## ========== checking ComplexHeatmap ==========
## Loading methods to a new R session... 7 namespaces loaded.
## Loading grid to a new R session... 8 namespaces loaded.
## Loading graphics to a new R session... 7 namespaces loaded.
## Loading stats to a new R session... 7 namespaces loaded.
## Loading grDevices to a new R session... 7 namespaces loaded.
## Loading circlize to a new R session... 12 namespaces loaded.
## Loading GetoptLong to a new R session... 11 namespaces loaded.
## Loading colorspace to a new R session... 8 namespaces loaded.
## Loading clue to a new R session... 9 namespaces loaded.
## Loading RColorBrewer to a new R session... 8 namespaces loaded.
## Loading GlobalOptions to a new R session... 8 namespaces loaded.
## Loading png to a new R session... 8 namespaces loaded.
## Loading digest to a new R session... 8 namespaces loaded.
## Loading IRanges to a new R session... 12 namespaces loaded.
## Loading matrixStats to a new R session... 8 namespaces loaded.
## Loading foreach to a new R session... 10 namespaces loaded.
## Loading doParallel to a new R session... 12 namespaces loaded.
## Loading testthat to a new R session... 11 namespaces loaded.
## Loading knitr to a new R session... 10 namespaces loaded.
## Loading markdown to a new R session... 8 namespaces loaded.
## Loading dendsort to a new R session... 8 namespaces loaded.
## Loading jpeg to a new R session... 8 namespaces loaded.
## Loading tiff to a new R session... 8 namespaces loaded.
## Loading fastcluster to a new R session... 8 namespaces loaded.
## Loading EnrichedHeatmap to a new R session... 43 namespaces loaded.
## Loading dendextend to a new R session... 36 namespaces loaded.
## Loading grImport to a new R session... 10 namespaces loaded.
## Loading grImport2 to a new R session... 13 namespaces loaded.
## Loading glue to a new R session... 8 namespaces loaded.
## Loading GenomicRanges to a new R session... 20 namespaces loaded.
## Loading gridtext to a new R session... 12 namespaces loaded.
## Loading pheatmap to a new R session... 17 namespaces loaded.
## Loading gridGraphics to a new R session... 9 namespaces loaded.
## Loading gplots to a new R session... 12 namespaces loaded.
## Loading rmarkdown to a new R session... 16 namespaces loaded.
## Loading Cairo to a new R session... 8 namespaces loaded.
x
## ComplexHeatmap version 2.9.4
## 31 namespaces loaded if only loading packages in Depends and Imports
## 95 namespaces loaded after loading all packages in Depends, Imports and Suggests
plot(x)
```

<img src="https://user-images.githubusercontent.com/449218/137299794-77d9d3b5-c2d8-4f29-98a1-3e8944ca1ea8.png" width="100%" />

## License

MIT @ Zuguang Gu
