### =========================================================================
### TENxMatrix objects
### -------------------------------------------------------------------------
###
### Note that we could just wrap a TENxMatrixSeed object in a DelayedMatrix
### object to represent and manipulate a 10x Genomics dataset as a
### DelayedMatrix object. So, strictly speaking, we don't really need the
### TENxMatrix class. However, we define this class mostly for cosmetic
### reasons, that is, to hide the DelayedMatrix class from the user.
### So the user will see and manipulate TENxMatrix objects instead of
### DelayedMatrix objects.
###


setClass("TENxMatrix",
    contains="DelayedMatrix",
    representation(seed="TENxMatrixSeed")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

setMethod("DelayedArray", "TENxMatrixSeed",
    function(seed) new_DelayedArray(seed, Class="TENxMatrix")
)

### Works directly on a TENxMatrixSeed object, in which case it must be
### called with a single argument.
TENxMatrix <- function(filepath, group="matrix")
{
    if (is(filepath, "TENxMatrixSeed")) {
        if (!missing(group))
            stop(wmsg("TENxMatrix() must be called with a single argument ",
                      "when passed a TENxMatrixSeed object"))
        seed <- filepath
    } else {
        seed <- TENxMatrixSeed(filepath, group)
    }
    DelayedArray(seed)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Taking advantage of sparsity
###

setMethod("nzcount", "TENxMatrix", function(x) nzcount(x@seed))

setMethod("read_sparse_block", "TENxMatrix",
    function(x, viewport) read_sparse_block(x@seed, viewport)
)

setMethod("extractNonzeroDataByCol", "TENxMatrix",
    function(x, j) extractNonzeroDataByCol(x@seed, j)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion to dgCMatrix
###

.from_TENxMatrix_to_dgCMatrix <- function(from) as(from@seed, "dgCMatrix")
setAs("TENxMatrix", "dgCMatrix", .from_TENxMatrix_to_dgCMatrix)
setAs("TENxMatrix", "sparseMatrix", .from_TENxMatrix_to_dgCMatrix)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion to SVT_SparseMatrix
###

### TODO: Replace the 4 specific coercion methods below with 4 more general
### coercion methods from DelayedArray to [SVT_]SparseArray/Matrix defined
### in the DelayedArray package. They should simply do 'as(from@seed, to)'
### if 'from' is pristine i.e. if 'isPristine(from, ignore.dimnames=TRUE)'
### is TRUE.
.from_TENxMatrix_to_SVT_SparseMatrix <-
    function(from) as(from@seed, "SVT_SparseMatrix")

setAs("TENxMatrix", "SVT_SparseMatrix",
    .from_TENxMatrix_to_SVT_SparseMatrix)
setAs("TENxMatrix", "SVT_SparseArray",
    .from_TENxMatrix_to_SVT_SparseMatrix)
setAs("TENxMatrix", "SparseMatrix",
    .from_TENxMatrix_to_SVT_SparseMatrix)
setAs("TENxMatrix", "SparseArray",
    .from_TENxMatrix_to_SVT_SparseMatrix)

