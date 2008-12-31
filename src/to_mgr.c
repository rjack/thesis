#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "h/crono.h"
#include "h/dtable_mgr.h"
#include "h/to_mgr.h"
#include "h/types.h"


/*******************************************************************************
				    Types
*******************************************************************************/

struct timeout {
	/* Durata del timeout e cronometro. */
	struct timeval to_maxval;
	crono_t to_crono;
};


/*******************************************************************************
			       Static variables
*******************************************************************************/

static struct timeout *table_ = NULL;
static size_t table_len_ = 0;
static size_t table_used_ = 0;


/*******************************************************************************
			       Static functions
*******************************************************************************/

static bool
is_used (struct timeout *table, timeout_t handle)
{
	struct timeout *to;

	to = &(table[handle]);

	if (to->to_maxval.tv_sec ==  -1
	    && to->to_maxval.tv_usec ==  -1)
		return FALSE;
	return TRUE;
}


static void
set_unused (struct timeout *table, timeout_t handle)
{
	struct timeout *to;

	to = &(table[handle]);

	to->to_maxval.tv_sec = -1;
	to->to_maxval.tv_usec = -1;
	memset (&(to->to_crono), 0, sizeof(to->to_crono));
}


#ifndef NDEBUG
static bool
is_valid_handle (timeout_t handle)
{
	return dtable_is_valid_handle ((void **)&table_, table_used_, handle,
                                       (use_checker_t)is_used);
}
#endif /* NDEBUG */


/*******************************************************************************
			      Exported functions
*******************************************************************************/

timeout_t
tmout_create (const struct timeval *value)
{
	timeout_t new_handle;

	if (tv_cmp (value, &time_0ms) <= 0)
		return -1;

	new_handle = dtable_add ((void **)&table_, &table_len_, &table_used_,
	                         sizeof(struct timeout),
	                         (use_checker_t)is_used);
	if (new_handle != -1)
		tmout_set (new_handle, value);

	return new_handle;
}


void
tmout_destroy (timeout_t handle)
{
	assert (is_valid_handle (handle));

	dtable_remove ((void **)&table_, &table_used_, handle,
	               (unused_setter_t)set_unused);
}


void
tmout_set (timeout_t handle, const struct timeval *max)
{
	struct timeout *tmout;

	assert (is_valid_handle (handle));
	assert (max != NULL);

	tmout = &(table_[handle]);
	memcpy (&(tmout->to_maxval), max, sizeof(*max));
}


void
tmout_start (timeout_t handle, const struct timeval *now)
{
	struct timeout *tmout;

	assert (is_valid_handle (handle));
	assert (now != NULL);

	tmout = &(table_[handle]);
	crono_start (&(tmout->to_crono), now);
}


bool
tmout_left (timeout_t handle, const struct timeval *now,
            struct timeval *result)
{
	struct timeval elapsed;
	struct timeval left;
	struct timeout *tmout;

	assert (is_valid_handle (handle));
	assert (now != NULL);

	tmout = &(table_[handle]);
	crono_measure (&(tmout->to_crono), now, &elapsed);
	tv_diff (&left, &(tmout->to_maxval), &elapsed);
	if (result)
		memcpy (result, &left, sizeof(*result));
	if (tv_cmp (&left, &time_0ms) <= 0)
		return FALSE;
	return TRUE;
}


void
tmout_print (timeout_t handle)
{
	struct timeout *tmout;

	assert (is_valid_handle (handle));

	tmout = &(table_[handle]);

	printf ("at ");
	crono_print (&(tmout->to_crono));

	printf (" max ");
	tv_print (&(tmout->to_maxval));
}


void
tm_garbage_collect (void)
{
	dtable_clear ((void **)&table_, &table_len_, &table_used_,
	              sizeof(*table_));
}


void
tm_min_left_overall (struct timeval *min_result, const struct timeval *now)
{
	int i;
	struct timeval left;

	for (i = 0; i < table_used_; i++)
		if (is_used (table_, i)) {
			tmout_left (i, now, &left);
			tv_min (min_result, min_result, &left);
		}
}
