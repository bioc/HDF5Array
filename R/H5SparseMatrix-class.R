### =========================================================================
### H5SparseMatrix objects
### -------------------------------------------------------------------------
###


setClass("H5SparseMatrix",
    contains="DelayedMatrix",
    representation(seed="H5SparseMatrixSeed")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

setMethod("DelayedArray", "H5SparseMatrixSeed",
    function(seed) new_DelayedArray(seed, Class="H5SparseMatrix")
)

### Works directly on an H5SparseMatrixSeed derivative, in which case it must
### be called with a single argument.
H5SparseMatrix <- function(filepath, group)
{
    if (is(filepath, "H5SparseMatrixSeed")) {
        if (!missing(group))
            stop(wmsg("H5SparseMatrix() must be called with a single argument ",
                      "when passed an H5SparseMatrixSeed object"))
        seed <- filepath
    } else {
        seed <- H5SparseMatrixSeed(filepath, group)
    }
    DelayedArray(seed)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Taking advantage of sparsity
###

setMethod("nzcount", "H5SparseMatrix", function(x) nzcount(x@seed))

setMethod("read_sparse_block", "H5SparseMatrix",
    function(x, viewport) read_sparse_block(x@seed, viewport)
)

setMethod("extractNonzeroDataByCol", "H5SparseMatrix",
    function(x, j) extractNonzeroDataByCol(x@seed, j)
)

setMethod("extractNonzeroDataByRow", "H5SparseMatrix",
    function(x, i) extractNonzeroDataByCol(x@seed, i)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion to dgCMatrix
###

.from_H5SparseMatrix_to_dgCMatrix <- function(from) as(from@seed, "dgCMatrix")
setAs("H5SparseMatrix", "dgCMatrix", .from_H5SparseMatrix_to_dgCMatrix)
setAs("H5SparseMatrix", "sparseMatrix", .from_H5SparseMatrix_to_dgCMatrix)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion to SVT_SparseMatrix
###

### TODO: Replace the 4 specific coercion methods below with 4 more general
### coercion methods from DelayedArray to [SVT_]SparseArray/Matrix defined
### in the DelayedArray package. They should simply do 'as(from@seed, to)'
### if 'from' is pristine i.e. if 'isPristine(from, ignore.dimnames=TRUE)'
### is TRUE.
.from_H5SparseMatrix_to_SVT_SparseMatrix <-
    function(from) as(from@seed, "SVT_SparseMatrix")

setAs("H5SparseMatrix", "SVT_SparseMatrix",
    .from_H5SparseMatrix_to_SVT_SparseMatrix)
setAs("H5SparseMatrix", "SVT_SparseArray",
    .from_H5SparseMatrix_to_SVT_SparseMatrix)
setAs("H5SparseMatrix", "SparseMatrix",
    .from_H5SparseMatrix_to_SVT_SparseMatrix)
setAs("H5SparseMatrix", "SparseArray",
    .from_H5SparseMatrix_to_SVT_SparseMatrix)

