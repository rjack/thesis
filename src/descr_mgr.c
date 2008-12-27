#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>

#include "types.h"


int
dm_create (void **array, size_t *array_len, size_t *array_used, size_t
           elem_size, bool (*is_used)(void *, int))
{
	int new_handle;

	assert (*array_used <= *array_len);
	assert (elem_size > 0);
	assert (is_used != NULL);

	/* Search unused slot. */
	for (new_handle = 0;
	     new_handle < *array_used && is_used (*array, new_handle);
	     new_handle++);

	/* Array is full, must enqueue. */
	if (new_handle == *array_used) {
		if (*array_len == *array_used) {
			void *new_array;
			new_array = realloc (*array,
			                     (*array_len + 1) * elem_size);
			if (!new_array)
				return -1;
			*array = new_array;
			(*array_len)++;
		}
		(*array_used)++;
	}

	return new_handle;
}


void
dm_destroy (void **array, size_t *array_used, int handle,
            void (*set_unused)(void *, int))
{
	assert (handle >= 0);
	assert (handle <= *array_used);

	set_unused (*array, handle);
	if (handle == *array_used - 1)
		*array_used--;
}


void
dm_garbage_collect (void **array, size_t *array_len, size_t *array_used,
                    size_t elem_size)
{
	void *new_array;

	if (*array_len == *array_used)
		return;

	if (*array_used == 0) {
		free (*array);
		*array = NULL;
		*array_len = 0;
	} else {
		new_array = realloc (*array, *array_used * elem_size);
		if (new_array) {
			*array = new_array;
			*array_len = *array_used;
		}
	}
}
