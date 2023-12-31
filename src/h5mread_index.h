#ifndef _H5MREAD_STARTS_H_
#define _H5MREAD_STARTS_H_

#include "H5DSetDescriptor.h"
#include <Rdefines.h>

SEXP _h5mread_index(
	const H5DSetDescriptor *h5dset,
	SEXP index,
	int method,
	int use_H5Dread_chunk,
	int *ans_dim
);

#endif  /* _H5MREAD_STARTS_H_ */

