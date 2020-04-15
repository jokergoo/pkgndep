# Check the Heaviness of Package Dependencies

When developing R packages, we should try to avoid directly setting
dependencies to "heavy packages". The "heaviness" for a package means, the
additional dependent packages it brings to. If your package directly depends
on a heavy package, it brings several consequences:

1. Users need to install a lot of additional packages if they install your
   package ( which brings the risk that installation of some of the packages
   may fail and it makes your package cannot be installed). 
2. The namespaces that are loaded into your R session after loading your package (by
   `library(your-pkg)`) will be huge (you can see it by `sessionInfo()`).

And you package will be "heavy" as well and it may take long time to load your
package.

In the DESCRIPTION file of your package, those "directly dependent pakcages"
are always listed in the "Depends" or "Imports" fields. To remove the heavy
packages that are not offen used in your package, it is better to move them
into the "Suggests" fields and load them only when they are needed.

Here **pkgndep** package checks the heaviness of the packages that your
package depends on. For each package listed in the "Depends", "Imports" and
"Suggests" fields in the DESCRIPTION file, it opens a new R session, loads the
package and counts the number of namespaces that are loaded. The summary of
the dependencies is visualized by a customized heatmap.

As an example, I am developing a package 
[**cola**](https://jokergoo.github.com/cola) which depends on a lot of [other
packages](https://github.com/jokergoo/cola/blob/6d5f5a7737fd273c36ff50f35a60bf2b671ed84d/DESCRIPTION).
The dependency heatmap looks like (click to view in the original size):

![cola](https://user-images.githubusercontent.com/449218/79337498-e6bec180-7f25-11ea-9861-293eba8931bc.png)

In the heatmap, rows are the packages listed in "Depends", "Imports" and "Suggests",
columns are the namespaces that are loaded if each of the package is only loaded to a new R session.
The barplots on the right show the number of namespaces that are brought by each package.

We can see if all the packages are put in the "Imports" field, 166 namespaces
will be loaded after `library(cola)`. Some of the heavy packages such as
**WGCNA** and **clusterProfiler** are not very frequently used in **cola**,
moving them to "Suggests" field and loading them only when they are needed
helps to speed up loading **cola**. Now the number of namespaces are reduced
to 25 after `library(cola)`.

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
# ========== checking ComplexHeatmap ==========
# Loading methods to a new R session... 7 namespaces loaded.
# Loading grid to a new R session... 8 namespaces loaded.
# Loading graphics to a new R session... 7 namespaces loaded.
# Loading stats to a new R session... 7 namespaces loaded.
# Loading grDevices to a new R session... 7 namespaces loaded.
# Loading circlize to a new R session... 12 namespaces loaded.
# Loading GetoptLong to a new R session... 10 namespaces loaded.
# Loading colorspace to a new R session... 8 namespaces loaded.
# Loading clue to a new R session... 9 namespaces loaded.
# Loading RColorBrewer to a new R session... 8 namespaces loaded.
# Loading GlobalOptions to a new R session... 8 namespaces loaded.
# Loading parallel to a new R session... 8 namespaces loaded.
# Loading png to a new R session... 8 namespaces loaded.
# Loading testthat to a new R session... 11 namespaces loaded.
# Loading knitr to a new R session... 10 namespaces loaded.
# Loading markdown to a new R session... 8 namespaces loaded.
# Loading dendsort to a new R session... 8 namespaces loaded.
# Loading Cairo to a new R session... 8 namespaces loaded.
# Loading jpeg to a new R session... 8 namespaces loaded.
# Loading tiff to a new R session... 8 namespaces loaded.
# Loading fastcluster to a new R session... 8 namespaces loaded.
# Loading dendextend to a new R session... 36 namespaces loaded.
# Loading grImport to a new R session... 10 namespaces loaded.
# Loading grImport2 to a new R session... 13 namespaces loaded.
# Loading glue to a new R session... 8 namespaces loaded.
# Loading GenomicRanges to a new R session... 19 namespaces loaded.
# Loading gridtext to a new R session... 12 namespaces loaded.
x
# ComplexHeatmap version 2.3.4
# 14 namespaces loaded if only load packages in Depends and Imports
# 57 namespaces loaded for loading all packages in Depends, Imports and Suggests
plot(x)
```

![ComplexHeatmap](https://user-images.githubusercontent.com/449218/79369104-b80a1080-7f50-11ea-8e20-a8e69f69b40f.png)


## Statistics

I ran **pkgndep** on all packages that are installed in my computer. The table
of number of loaded namespaces as well as the dependency heatmaps are
available at https://jokergoo.github.io/pkgndep/stat/.

For a quick look, the top 10 packages with the largest dependencies are:

And the top 10 packages with the largest dependencies where packages in "Suggests" are also loaded are:


## License
MIT @ Zuguang Gu
