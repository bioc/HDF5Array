\name{h5mread}

\alias{get_h5mread_returned_type}
\alias{h5mread}

\title{An alternative to \code{rhdf5::h5read}}

\description{
  An efficient and flexible alternative to \code{rhdf5::h5read()}.
}

\usage{
h5mread(filepath, name, starts=NULL, counts=NULL, noreduce=FALSE,
        as.vector=NA, as.integer=FALSE, as.sparse=FALSE,
        method=0L, use.H5Dread_chunk=FALSE)

get_h5mread_returned_type(filepath, name, as.integer=FALSE)
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
  \item{starts, counts}{
    \code{starts} and \code{counts} are used to specify the \emph{array
    selection}. Each argument can be either \code{NULL} or a list with
    one list element per dimension in the dataset.

    If \code{starts} and \code{counts} are both \code{NULL}, then the entire
    dataset is read.

    If \code{starts} is a list, each list element in it must be a vector of
    valid positive indices along the corresponding dimension in the dataset.
    An empty vector (\code{integer(0)}) is accepted and indicates an empty
    selection along that dimension. A \code{NULL} is accepted and indicates
    a \emph{full} selection along the dimension so has the same meaning
    as a missing subscript when subsetting an array-like object with \code{[}.
    (Note that for \code{[} a \code{NULL} subscript indicates an empty
    selection.)

    Each list element in \code{counts} must be \code{NULL} or a vector
    of non-negative integers of the same length as the corresponding
    list element in \code{starts}. Each value in the vector indicates how
    many positions to select starting from the associated start value.
    A \code{NULL} indicates that a single position is selected for each
    value along the corresponding dimension.

    If \code{counts} is \code{NULL}, then each index in each \code{starts}
    list element indicates a single position selection along the corresponding
    dimension. Note that in this case the \code{starts} argument is
    equivalent to the \code{index} argument of \code{\link[rhdf5]{h5read}}
    and \code{\link[S4Arrays]{extract_array}} (with the caveat that
    \code{\link[rhdf5]{h5read}} doesn't accept empty selections).

    Finally note that when \code{counts} is not \code{NULL} then the
    selection described by \code{starts} and \code{counts} must be
    \emph{strictly ascending} along each dimension.
  }
  \item{noreduce}{
    TODO
  }
  \item{as.vector}{
    Should the data be returned in a vector instead of an array?
    By default (i.e. when set to \code{NA}), the data is returned in an
    ordinary array when reading from a multidimensional dataset, and in
    an ordinary vector when reading from a 1D dataset.
    You can override this by setting \code{as.vector} to \code{TRUE}
    or \code{FALSE}.
  }
  \item{as.integer}{
    TODO
  }
  \item{as.sparse}{
    TODO
  }
  \item{method}{
    TODO
  }
  \item{use.H5Dread_chunk}{
    TODO
  }
}

\details{
  DETAILS COMING SOON...
}

\value{
  \code{h5mread()} returns an ordinary array or vector if \code{as.sparse}
  is \code{FALSE} (the default), and a \link[SparseArray]{COO_SparseArray}
  object if \code{as.sparse} is \code{TRUE}.

  \code{get_h5mread_returned_type()} returns the type of the array or
  vector that will be returned by \code{h5mread()}.
  Equivalent to (but more efficient than):
  \preformatted{  typeof(h5mread(filepath, name, rep(list(integer(0)), ndim)))
  } where \code{ndim} is the number of dimensions (a.k.a. \emph{rank} in
  HDF5 jargon) of the dataset.
}

\seealso{
  \itemize{
    \item \link{H5File} objects.

    \item \code{\link[rhdf5]{h5read}} in the \pkg{rhdf5} package.

    \item \code{\link[S4Arrays]{extract_array}} in the \pkg{S4Arrays}
          package.

    \item \link[SparseArray]{COO_SparseArray} objects in the \pkg{SparseArray}
          package.

    \item The \code{\link[TENxBrainData]{TENxBrainData}} dataset (in the
          \pkg{TENxBrainData} package).

    \item \code{\link{h5mread_from_reshaped}} to read data from a virtually
          reshaped HDF5 dataset.
  }
}

\examples{
## ---------------------------------------------------------------------
## BASIC USAGE
## ---------------------------------------------------------------------
m0 <- matrix((runif(600) - 0.5) * 10, ncol=12)
M0 <- writeHDF5Array(m0, name="M0")

m <- h5mread(path(M0), "M0")
stopifnot(identical(m0, m))

m <- h5mread(path(M0), "M0", starts=list(NULL, c(3, 12:8)))
stopifnot(identical(m0[ , c(3, 12:8)], m))

m <- h5mread(path(M0), "M0", starts=list(integer(0), c(3, 12:8)))
stopifnot(identical(m0[NULL , c(3, 12:8)], m))

m <- h5mread(path(M0), "M0", starts=list(1:5, NULL), as.integer=TRUE)
storage.mode(m0) <- "integer"
stopifnot(identical(m0[1:5, ], m))

a0 <- array(1:350, c(10, 5, 7))
A0 <- writeHDF5Array(a0, filepath=path(M0), name="A0")
h5ls(path(A0))

a <- h5mread(path(A0), "A0", starts=list(c(2, 7), NULL, 6),
                             counts=list(c(4, 2), NULL, NULL))
stopifnot(identical(a0[c(2:5, 7:8), , 6, drop=FALSE], a))

## Load the data in a sparse array representation:

m1 <- matrix(c(5:-2, rep.int(c(0L, 99L), 11)), ncol=6)
M1 <- writeHDF5Array(m1, name="M1", chunkdim=c(3L, 2L))

index <- list(5:3, NULL)
m <- h5mread(path(M1), "M1", starts=index)
coo <- h5mread(path(M1), "M1", starts=index, as.sparse=TRUE)
class(coo)  # COO_SparseArray object (see ?COO_SparseArray)
as(coo, "dgCMatrix")
stopifnot(identical(m, as.array(coo)))

## ---------------------------------------------------------------------
## PERFORMANCE
## ---------------------------------------------------------------------
library(ExperimentHub)
hub <- ExperimentHub()

## With the "sparse" TENxBrainData dataset
## ---------------------------------------
fname0 <- hub[["EH1039"]]
h5ls(fname0)  # all datasets are 1D datasets

index <- list(77 * sample(34088679, 5000, replace=TRUE))
## h5mread() is about 4x faster than h5read():
system.time(a <- h5mread(fname0, "mm10/data", index))
system.time(b <- h5read(fname0, "mm10/data", index=index))
stopifnot(identical(a, as.vector(b)))

index <- list(sample(1306127, 7500, replace=TRUE))
## h5mread() is about 20x faster than h5read():
system.time(a <- h5mread(fname0, "mm10/barcodes", index))
system.time(b <- h5read(fname0, "mm10/barcodes", index=index))
stopifnot(identical(a, as.vector(b)))

## With the "dense" TENxBrainData dataset
## --------------------------------------
fname1 <- hub[["EH1040"]]
h5ls(fname1)  # "counts" is a 2D dataset

set.seed(33)
index <- list(sample(27998, 300), sample(1306127, 450))
## h5mread() is about 2x faster than h5read():
system.time(a <- h5mread(fname1, "counts", index))
system.time(b <- h5read(fname1, "counts", index=index))
stopifnot(identical(a, b))

## Alternatively 'as.sparse=TRUE' can be used to reduce memory usage:
system.time(coo <- h5mread(fname1, "counts", index, as.sparse=TRUE))
stopifnot(identical(a, as.array(coo)))

## The bigger the selection, the greater the speedup between
## h5read() and h5mread():
\dontrun{
  index <- list(sample(27998, 1000), sample(1306127, 1000))
  ## h5mread() about 4x faster than h5read() (12s vs 48s):
  system.time(a <- h5mread(fname1, "counts", index))
  system.time(b <- h5read(fname1, "counts", index=index))
  stopifnot(identical(a, b))

  ## With 'as.sparse=TRUE' (about the same speed as with 'as.sparse=FALSE'):
  system.time(coo <- h5mread(fname1, "counts", index, as.sparse=TRUE))
  stopifnot(identical(a, as.array(coo)))
}
}
\keyword{utilities}
