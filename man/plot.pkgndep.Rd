\name{plot.pkgndep}
\alias{plot.pkgndep}
\title{
Make the dependency heatmap
}
\description{
Make the dependency heatmap
}
\usage{
\method{plot}{pkgndep}(x, pkg_fontsize = 10*cex, title_fontsize = 12*cex,
    legend_fontsize = 10*cex, fix_size = !dev.interactive(), cex = 1,
    help = TRUE, file = NULL, res = 144, ...)
}
\arguments{

  \item{x}{An object from \code{\link{pkgndep}}.}
  \item{pkg_fontsize}{Font size for the package names.}
  \item{title_fontsize}{Font size for the title.}
  \item{legend_fontsize}{Font size for the legends.}
  \item{fix_size}{Should the rows and columns in the heatmap have fixed size?}
  \item{cex}{A factor multiplicated to all font sizes.}
  \item{help}{Whether to print help message?}
  \item{file}{A path of the figure. The size of the figure is automatically calculated.}
  \item{res}{Resolution of the figure (only for png and jpeg).}
  \item{...}{Other arguments.}

}
\details{
If \code{fix_size} is set to \code{TRUE}. The size of the whole plot can be obtained by:

  \preformatted{
    size = plot(x, fix_size = TRUE)  }

where \code{size} is a numeric vector of length two which are the width and height of the whole heatmap.
If \code{file} argument is set, the size of the figure is automatically calculated.

If there are no dependency packages stored in \code{x}, \code{NULL} is returned.
}
\value{
A vector of two numeric values (in inches) that correspond to the width and height of the plot.
}
\examples{
# See examples in `pkgndep()`.
}
