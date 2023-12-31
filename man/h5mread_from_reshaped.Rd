\name{h5mread_from_reshaped}

\alias{h5mread_from_reshaped}

\title{Read data from a virtually reshaped HDF5 dataset}

\description{
  An \code{\link{h5mread}} wrapper that reads data from a virtually
  reshaped HDF5 dataset.
}

\usage{
h5mread_from_reshaped(filepath, name, dim, starts, noreduce=FALSE,
                      as.integer=FALSE, method=0L)
}

\arguments{
  \item{filepath}{
    The path (as a single string) to the HDF5 file where the dataset
    to read from is located, or an \link{H5File} object.

    Note that you must create and use an \link{H5File} object if the HDF5
    file to access is stored in an Amazon S3 bucket. See \code{?\link{H5File}}
    for how to do this.

    Also please note that \link{H5File} objects must NOT be used in the
    context of parallel evaluation at the moment.
  }
  \item{name}{
    The name of the dataset in the HDF5 file.
  }
  \item{dim}{
    A vector of dimensions that describes the virtual reshaping i.e.
    the reshaping that is virtually applied upfront to the HDF5 dataset
    to read from.

    Note that the HDF5 dataset is treated as read-only so never gets
    \emph{effectively} reshaped, that is, the dataset dimensions encoded
    in the HDF5 file are not mmodified.

    Also please note that arbitrary reshapings are not supported.
    Only reshapings that reduce the number of dimensions by collapsing
    a group of consecutive dimensions into a single dimension are supported.
    For example, reshaping a 10 x 3 x 5 x 1000 array as a 10 x 15 x 1000
    array or as a 150 x 1000 matrix is supported.
  }
  \item{starts}{
    A multidimensional subsetting index \emph{with respect to the reshaped
    dataset}, that is, a list with one list element per dimension in the
    reshaped dataset.

    Each list element in \code{starts} must be a vector of valid
    positive indices along the corresponding dimension in the reshaped dataset.
    An empty vector (\code{integer(0)}) is accepted and indicates an empty
    selection along that dimension. A \code{NULL} is accepted and indicates
    a \emph{full} selection along the dimension so has the same meaning
    as a missing subscript when subsetting an array-like object with \code{[}.
    (Note that for \code{[} a \code{NULL} subscript indicates an empty
    selection.)
  }
  \item{noreduce, as.integer, method}{
    See \code{?\link{h5mread}} for a description of these arguments.
  }
}

\value{
  An array.
}

\seealso{
  \itemize{
    \item \link{H5File} objects.

    \item \code{\link{h5mread}}.
  }
}

\examples{
## ---------------------------------------------------------------------
## BASIC USAGE
## ---------------------------------------------------------------------
a1 <- array(1:350, c(10, 5, 7))
A1 <- writeHDF5Array(a1, name="A1")

## Collapse the first 2 dimensions:
h5mread_from_reshaped(path(A1), "A1", dim=c(50, 7),
                      starts=list(8:11, NULL))
h5mread_from_reshaped(path(A1), "A1", dim=c(50, 7),
                      starts=list(8:11, NULL))

## Collapse the last 2 dimensions:
h5mread_from_reshaped(path(A1), "A1", dim=c(10, 35),
                      starts=list(NULL, 3:11))

a2 <- array(1:150000 + 0.1*runif(150000), c(10, 3, 5, 1000))
A2 <- writeHDF5Array(a2, name="A2")

## Collapse the 2nd and 3rd dimensions:
h5mread_from_reshaped(path(A2), "A2", dim=c(10, 15, 1000),
                      starts=list(NULL, 8:11, 999:1000))

## Collapse the first 3 dimensions:
h5mread_from_reshaped(path(A2), "A2", dim=c(150, 1000),
                      starts=list(71:110, 999:1000))
}
\keyword{utilities}
