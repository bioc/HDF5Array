#ifndef _H5MREAD_HELPERS_H_
#define _H5MREAD_HELPERS_H_

#include "H5DSetDescriptor.h"
#include "uaselection.h"
#include "hdf5.h"

/* A data structure for representing a viewport on a HDF5 dataset. */
typedef struct {
	hsize_t *h5off, *h5dim;
	int *off, *dim; // same as h5off and h5dim but stored as int
} H5Viewport;

#define	ALLOC_ALL_FIELDS		0
#define	ALLOC_H5OFF_AND_H5DIM		1
#define	ALLOC_OFF_AND_DIM		2

int _alloc_H5Viewport(
	H5Viewport *vp,
	int ndim,
	int mode
);

void _free_H5Viewport(H5Viewport *vp);

hid_t _create_mem_space(
	int ndim,
	const int *dim
);

int _select_H5Viewport(
	hid_t space_id,
	const H5Viewport *vp
);

int _add_H5Viewport_to_h5selection(
	hid_t space_id,
	const H5Viewport *vp
);

int _read_h5selection(
	const H5DSetDescriptor *h5dset,
	hid_t mem_type_id,
	hid_t mem_space_id,
	void *mem,
	const H5Viewport *mem_vp
);

int _read_H5Viewport(
	const H5DSetDescriptor *h5dset,
	const H5Viewport *h5dset_vp,
	hid_t mem_type_id,
	hid_t mem_space_id,
	void *mem,
	const H5Viewport *mem_vp
);

void _init_in_offset(
	int ndim,
	SEXP index,
	const hsize_t *h5chunkdim,
	const H5Viewport *mem_vp,
	const H5Viewport *h5dset_vp,
	size_t *in_offset
);


/****************************************************************************
 * Inline helper functions
 */

static inline int _next_midx(int ndim, const int *max_idx_plus_one,
			     int *midx_buf)
{
	int along, i;

	for (along = 0; along < ndim; along++) {
		i = midx_buf[along] + 1;
		if (i < max_idx_plus_one[along]) {
			midx_buf[along] = i;
			break;
		}
		midx_buf[along] = 0;
	}
	return along;
}

static inline void _update_in_offset(int ndim, SEXP index,
		const hsize_t *h5chunkdim, const H5Viewport *mem_vp,
		const int *inner_midx, int inner_moved_along,
		size_t *in_offset)
{
	SEXP start;
	int i1, i0, along, h5along, di;
	long long int in_off_inc;

	start = GET_LIST_ELT(index, inner_moved_along);
	if (start != R_NilValue) {
		i1 = mem_vp->off[inner_moved_along] +
		     inner_midx[inner_moved_along];
		i0 = i1 - 1;
		in_off_inc = _get_trusted_elt(start, i1) -
			     _get_trusted_elt(start, i0);
	} else {
		in_off_inc = 1;
	}
	if (inner_moved_along >= 1) {
		along = inner_moved_along - 1;
		h5along = ndim - inner_moved_along;
		do {
			in_off_inc *= h5chunkdim[h5along];
			di = 1 - mem_vp->dim[along];
			start = GET_LIST_ELT(index, along);
			if (start != R_NilValue) {
				i1 = mem_vp->off[along];
				i0 = i1 - di;
				in_off_inc += _get_trusted_elt(start, i1) -
					      _get_trusted_elt(start, i0);
			} else {
				in_off_inc += di;
			}
			along--;
			h5along++;
		} while (along >= 0);
	}
	*in_offset += in_off_inc;
	return;
}

#endif  /* _H5MREAD_HELPERS_H_ */

