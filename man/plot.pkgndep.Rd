\name{plot.pkgndep}
\alias{plot.pkgndep}
\title{
Plot method
}
\description{
Plot method
}
\usage{
\method{plot}{pkgndep}(x, pkg_fontsize = 10, title_fontsize = 12, legend_fontsize = 8,
    fix_size = !dev.interactive(), ...)
}
\arguments{

  \item{x}{The object from \code{\link{pkgndep}}.}
  \item{pkg_fontsize}{Fontsize for the package names.}
  \item{title_fontsize}{Fontsize for the titles.}
  \item{legend_fontsize}{Fontsize for the legends.}
  \item{fix_size}{Should the rows and columns in the heatmap have fixed size?}
  \item{...}{Other arguments.}

}
\details{
If \code{fix_size} is set to \code{TRUE}. The size of the whole plot can be obtained by:

  \preformatted{
    size = plot(x, fix_size = TRUE)  }

where \code{size} is a \code{\link[grid]{unit}} object with the width and height of the whole heatmap, in unit \code{mm}.
If you want to save the plot in to e.g. a PDF file that has the same size of the heatmap, you
need to make the plot twice. First save the plot into a null device, just to obtain the size 
of the plot:

  \preformatted{
    pdf(NULL) # a null device
    size = plot(x, fix_size = TRUE)
    dev.off()
    width = convertX(size[1], "inches", valueOnly = TRUE)
    height = convertY(size[2], "inches", valueOnly = TRUE)
    pdf(..., width = width, height = height)
    plot(x)
    dev.off()  }

If there are no dependency packages stored in \code{x}, \code{NULL} is returned.
}
\value{
A length-tow \code{\link[grid]{unit}} object which corresponds to the width and height of the plot.
}
\examples{
# See examples in `pkgndep()`.
}
