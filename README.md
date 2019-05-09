One good practice to develop R packages is to keep the amount of dependent packages
that are loaded into R to a small number. There are some packages
that have complex dependency from other packages. If you depend on these kind of packages
for your package, actually you need to be careful and maybe you need to make optimization
of your code.

Here **pkgndep** package opens a fresh R session for each package in the **Depends**, **Imports** and **Suggests** fields of your package and each time, it only loads one package and it 
collects the packages that are loaded into R directly or indirectly.

As an example, I once developed a package called [**cola**](https://jokergoo.github.com/cola). This package depends on a lot
of [other packages](https://github.com/jokergoo/cola/blob/6d5f5a7737fd273c36ff50f35a60bf2b671ed84d/DESCRIPTION). It is better to move some packages to **Suggests** field.

![image](https://user-images.githubusercontent.com/449218/57465887-cf8c4400-727f-11e9-96c2-f9eea72a2dad.png)

We can see there are some packages imports A LOT of other packages in to R. Since they are only used
in a small part of the **cola** package, they can be moved to **Suggests** fields and load them
when they need to be used.
