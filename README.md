# Analyzing Dependency Heaviness of R Packages

[![R-CMD-check](https://github.com/jokergoo/pkgndep/workflows/R-CMD-check/badge.svg)](https://github.com/jokergoo/pkgndep/actions)
[![CRAN](https://www.r-pkg.org/badges/version/pkgndep)](https://cran.r-project.org/web/packages/pkgndep/index.html)


When developing R packages, we should try to avoid directly setting
dependencies on "heavy packages". The "heaviness" for a package means, the
number of additional dependency packages it brings to. If your package directly depends
on a heavy package, it would bring several consequences:

1. Users need to install a lot of additional packages when installing your
   package which brings the risk that installation of some packages
   may fail and it makes your package cannot be installed. 
2. The namespaces that are loaded into your R session after loading your package will be huge (you can see the loaded namespaces by `sessionInfo()`).
3. You package will be "heavy" as well and it may take long time to load your package.

In the DESCRIPTION file of your package, there are "direct dependency
pakcages" listed in the `Depends`, `Imports` and `LinkingTo` fields. There are
also "indirect dependency packages" that can be found recursively for each of
the direct dependency packages. Here what we called "dependency packages" are
the union of the direct and indirect dependency packages.

There are also packages listed in `Suggests` and `Enhances` fields in
DESCRIPTION file, but they are not enforced to be installed when installing
your package. Of course, they also have "indirect dependency packages". To get
rid of the heavy packages that are not often used in your package, it is
better to move them into the `Suggests`/`Enhances` fields and to load/install
them only when they are needed.

Here the **pkgndep** package checks the heaviness of the dependency packages
of your package. For each package listed in the `Depends`, `Imports`,
`LinkingTo` and `Suggests`/`Enhances` fields in the DESCRIPTION file,
**pkgndep** checks how many additional packages your package requires. The
summary of the dependency is visualized by a customized heatmap.

As an example, I am developing a package called
[**cola**](https://github.com/jokergoo/cola/) which depends on [a lot of other
packages](https://github.com/jokergoo/ComplexHeatmap/blob/master/DESCRIPTION).
The dependency heatmap looks like follows:

![image](https://user-images.githubusercontent.com/449218/140655626-f2062b6e-c11f-4dc0-b6b9-d954feffc4ad.png)


In the heatmap, rows are the packages listed in `Depends`, `Imports` and
`Suggests` fields, columns are the additional dependency packages required for
each row package. The barplots on the right show the number of required
package, the number of imported functions/methods/classes (parsed from
NAMESPACE file) and the quantitative measure "heaviness" (the definition of
heaviness will be introduced later).

We can see if all the packages are put in the `Depends` or `Imports` field
(i.e. movig all suggsted packages to `Imports`), in total 248
packages are required, which are really a lot. Actually some of the heavy
packages such as **WGCNA**, **clusterProfiler** and **ReactomePA** (the last
three packages in the heatmap rows) are not very frequently used in **cola**,
moving them to `Suggests` field and using them only when they are needed
greatly helps to reduce the heaviness of **cola**. Now the number of required
packages are reduced to only 64.

## Citation

Gu Z. et al., pkgndep: a tool for analyzing dependency heaviness of R packages. Bioinformatics 2022. https://doi.org/10.1093/bioinformatics/btac449

Gu Z, On the Dependency Heaviness of CRAN/Bioconductor Ecosystem. arXiv 2022. https://doi.org/10.48550/arXiv.2208.11674

## Installation

The **pkgndep** package can be installed from CRAN by

```r
install.packages("pkgndep")
```

## Usage

To use this package:

```r
library(pkgndep)
pkg = pkgndep("package-name")
dependency_heatmap(pkg)
```

or

```r
pkg = pkgndep("path-of-the-package")
dependency_heatmap(pkg)
```

An executable example:

```r
library(pkgndep)
pkg = pkgndep("ComplexHeatmap")
pkg
```

```
## ComplexHeatmap, version 2.9.4
## 30 additional packages are required for installing 'ComplexHeatmap'
## 117 additional packages are required if installing packages listed in all fields in DESCRIPTION
```

```r
dependency_heatmap(pkg)
```

![image](https://user-images.githubusercontent.com/449218/140655659-2ca142c5-067f-4f76-a0d2-00d0aea49c96.png)

## Heaviness database

There is an integrated dependency heaviness database for all R packages for a lot of R/Bioc versions. The database can be obtained by:

```r
heaviness_database()
```

## License

MIT @ Zuguang Gu
