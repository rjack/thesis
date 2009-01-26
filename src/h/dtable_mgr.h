#ifndef ULB_DTABLE_MGR_H
#define ULB_DTABLE_MGR_H

#include <sys/types.h>

#include "types.h"


/*******************************************************************************
				   Typedefs
*******************************************************************************/

typedef bool (*use_checker_t)(void *, int);
typedef void (*unused_setter_t)(void *, int);


/*******************************************************************************
			     Function prototypes
*******************************************************************************/

int
dtable_add (void **array, size_t *array_len, size_t *array_used,
            size_t elem_size, use_checker_t is_used);
/*
 * Add an entry to the given table.
 * Return
 * 	the new handle,
 * or	-1 in case of error.
 */


void
dtable_remove (void **array, size_t *array_used, unsigned int handle,
               unused_setter_t set_unused);
/*
 * Remove the element specified by the given handle from the table array.
 */


void
dtable_clear (void **array, size_t *array_len, size_t *array_used,
              size_t elem_size);
/*
 * Free unused table memory.
 */


bool
dtable_is_valid_handle (void **table, size_t table_used, unsigned int handle,
                        use_checker_t is_used);
/*
 * Return
 * 	TRUE if handle is valid,
 * or	FALSE otherwise.
 */


#endif /* ULB_DTABLE_MGR_H */
