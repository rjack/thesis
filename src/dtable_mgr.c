#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>

#include "h/dtable_mgr.h"


/*******************************************************************************
			      Exported functions
*******************************************************************************/

int
dtable_add (void **table, size_t *table_len, size_t *table_used, size_t
            elem_size, use_checker_t is_used)
{
	int new_handle;

	assert (*table_used <= *table_len);
	assert (elem_size > 0);
	assert (is_used != NULL);

	/* Search unused slot. */
	for (new_handle = 0;
	     new_handle < *table_used && is_used (*table, new_handle);
	     new_handle++);

	/* Array is full, must enqueue. */
	if (new_handle == *table_used) {
		if (*table_len == *table_used) {
			void *new_table;
			new_table = realloc (*table,
			                     (*table_len + 1) * elem_size);
			if (!new_table)
				return -1;
			*table = new_table;
			(*table_len)++;
		}
		(*table_used)++;
	}

	return new_handle;
}


void
dtable_remove (void **table, size_t *table_used, int handle,
               unused_setter_t set_unused)
{
	assert (handle >= 0);
	assert (handle <= *table_used);

	set_unused (*table, handle);
	if (handle == *table_used - 1)
		(*table_used)--;
}


void
dtable_clear (void **table, size_t *table_len, size_t *table_used,
              size_t elem_size)
{
	void *new_table;

	if (*table_len == *table_used)
		return;

	if (*table_used == 0) {
		free (*table);
		*table = NULL;
		*table_len = 0;
	} else {
		new_table = realloc (*table, *table_used * elem_size);
		if (new_table) {
			*table = new_table;
			*table_len = *table_used;
		}
	}
}


bool
dtable_is_valid_handle (void **table, size_t table_used, int handle,
                        use_checker_t is_used)
{
	if (handle >= 0
	    && handle <= table_used
	    && is_used (*table, handle))
		return TRUE;
	return FALSE;
}
