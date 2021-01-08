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
    fix_size = !dev.interactive(), unit = "in", ...)
}
\arguments{

  \item{x}{The object from \code{\link{pkgndep}}.}
  \item{pkg_fontsize}{Fontsize for the package names.}
  \item{title_fontsize}{Fontsize for the titles.}
  \item{legend_fontsize}{Fontsize for the legends.}
  \item{fix_size}{Should the rows and columns in the heatmap have fixed size?}
  \item{unit}{The unit of the returned figure width and height.}
  \item{...}{Other arguments.}

}
\details{
If \code{fix_size} is set to \code{TRUE}. The size of the whole plot can be obtained by:

  \preformatted{
    size = plot(x, fix_size = TRUE)  }

where \code{size} is a \code{\link[grid]{unit}} object with the width and height of the whole heatmap.
If you want to save the plot in to e.g. a PDF file that has the same size of the heatmap, you
need to make the plot twice. First save the plot into a null device, just to obtain the size 
of the plot:

  \preformatted{
    pdf(NULL) # a null device
    size = plot(x, fix_size = TRUE)
    dev.off()
    width = as.numeric(size[1])
    height = as.numeric(size[2])
    pdf(..., width = width, height = height)
    plot(x)
    dev.off()  }

If there are no dependency packages stored in \code{x}, \code{NULL} is returned.
}
\value{
A list of two units that correspond to the width and height of the plot.
}
\examples{
# See examples in `pkgndep()`.
}
