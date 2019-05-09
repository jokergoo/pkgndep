One good practice to develop R packages is to keep the amount of dependent
packages that are loaded into R to a small number. There are some packages
that have complex dependency from other packages. If you depend on these kind
of packages for your package, huge number of other packages will be attached to the R session
when your package is loaded.

Here **pkgndep** package opens a fresh R session for each package in the
**Depends**, **Imports** and **Suggests** fields of your package and each
time, it only loads one package and it collects the packages that are loaded
into R directly or indirectly.

As an example, I am developing a package called
[**cola**](https://jokergoo.github.com/cola) which depends on a lot of [other
packages](https://github.com/jokergoo/cola/blob/6d5f5a7737fd273c36ff50f35a60bf2b671ed84d/DESCRIPTION).
The dependency heatmap looks like:

![image](https://user-images.githubusercontent.com/449218/57465887-cf8c4400-727f-11e9-96c2-f9eea72a2dad.png)

We can see if all the packages are put in the **Imports** field, 156 packages
will be attached when **cola** is loaded. Since some of the packages like
**WGCNA** and **clusterProfiler** are not very frequently used in the package,
moving them to **Suggests** field and loading them only when they need to be
used will help to speed up the loading of **cola** and also make the loaded
namespace clean.


To use this package:

```r
pkgndep::pkgndep("package_name")
```

or

```r
pkgndep::pkgndep("path-to-the-package")
```
