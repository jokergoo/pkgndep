One good practice to develop R packages is to keep the amount of dependent
packages that are loaded into R to a small number. There are some packages
that have complex dependency from other packages. If you depend on these kind
of packages for your package, actually you need to be careful because huge amount
of packages will be loaded.

Here **pkgndep** package opens a fresh R session for each package in the
**Depends** and **Imports** fields of your package and each time, it only
loads one package and it collects the packages that are loaded into R directly
or indirectly.

As an example, I once developed a package called **cola**. This package has
quite a lot of [dependent
packages](https://github.com/jokergoo/cola/blob/6d5f5a7737fd273c36ff50f35a60bf2b671ed84d/DESCRIPTION).
Before I made any optimization of the package, there are in total 104 packages
that are loaded in R!

If I run **pkgndep**, from following heatmap,

![test](https://user-images.githubusercontent.com/449218/51384929-0ba76780-1b1e-11e9-9f31-b31097e7372f.png)

I can see basically it is due to the **data.tree** and **dendextend** packages
that they import A LOT of packages in to R. Since they are only used in a
small part of the **cola** package, I can move them to **Suggests** fields and
load them when they need to be used.
