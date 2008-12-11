#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

#include "util.h"
#include "types.h"
#include "crono.h"




/*******************************************************************************
			      Funzioni pubbliche
*******************************************************************************/


/*
 * Timeout.
 */

void
timeout_set (timeout_t *to, const struct timeval *max)
{
	assert (to != NULL);
	assert (max != NULL);

	memcpy (&to->to_maxval, max, sizeof(struct timeval));
}


timeout_t *
timeout_create (const struct timeval *value)
{
	timeout_t *new;

	new = my_alloc (sizeof(timeout_t));

	if (value != NULL)
		timeout_set (new, value);

	return new;
}


void
timeout_start (timeout_t *to, const struct timeval *now)
{
	assert (to != NULL);
	assert (now != NULL);

	crono_start (&to->to_crono, now);
}


void
timeout_left (timeout_t *to, const struct timeval *now,
              struct timeval *result)
{
	struct timeval elapsed;

	crono_measure (&to->to_crono, now, &elapsed);
	tv_diff (result, &to->to_maxval, &elapsed);
}


void
timeout_print (const timeout_t *to)
{
	printf ("at ");
	crono_print (&to->to_crono);

	printf (" max ");
	tv_print (&to->to_maxval);
}


/*
 * Cronometri.
 */

void
crono_measure (crono_t *cr, const struct timeval *now,
               struct timeval *result)
{
	assert (result != NULL);
	assert (cr != NULL);
	assert (now != NULL);

	tv_diff (&(cr->cr_elapsed), now, &(cr->cr_start));

	crono_read (cr, result);
}


void
crono_read (crono_t *cr, struct timeval *result)
{
	assert (cr != NULL);
	assert (result != NULL);
	memcpy (result, &cr->cr_elapsed, sizeof(struct timeval));
}


void
crono_start (crono_t *cr, const struct timeval *now)
{
	assert (cr != NULL);

	cr->cr_elapsed.tv_sec = 0;
	cr->cr_elapsed.tv_usec = 0;
	memcpy (&cr->cr_start, now, sizeof(struct timeval));
}


void
crono_print (const crono_t *cr)
{
	printf ("start ");
	tv_print (&cr->cr_start);
	printf (" elapsed ");
	tv_print (&cr->cr_elapsed);
}


/*
 * Strutture timeval.
 */

int
tv_cmp (const struct timeval *tv_1, const struct timeval *tv_2)
{
	struct timeval diff;

	assert (tv_1 != NULL);
	assert (tv_2 != NULL);

	tv_diff (&diff, tv_1, tv_2);

	assert (diff.tv_usec >= 0);

	if (diff.tv_sec == 0) {
		if (diff.tv_usec == 0)
			return 0;
		return 1;
	}

	if (diff.tv_sec > 0)
		return 1;
	return -1;
}


void
tv_min (struct timeval *result, const struct timeval *tv_1,
        const struct timeval *tv_2)
{
	assert (result != NULL);
	assert (tv_1 != NULL);
	assert (tv_2 != NULL);

	if (tv_cmp (tv_1, tv_2) < 0) {
		if (result != tv_1)
			memcpy (result, tv_1, sizeof(struct timeval));
	} else {
		if (result != tv_2)
			memcpy (result, tv_2, sizeof(struct timeval));
	}
}


void
gettime (struct timeval *tv)
{
	assert (tv != NULL);

	gettimeofday (tv, NULL);
	tv_normalize (tv);
}


double
tv2d (struct timeval *tv, bool must_free)
{
	double result;

	assert (tv != NULL);
	assert (must_free == TRUE || must_free == FALSE);

	result = tv->tv_sec + ((double)tv->tv_usec / (double)ONE_MILLION);

	if (must_free)
		free (tv);

	return result;
}


void
tv_diff (struct timeval *result, const struct timeval *min,
         const struct timeval *sub)
{
	struct timeval mymin;

	mymin = *min;

	while (mymin.tv_usec < sub->tv_usec) {
		mymin.tv_sec--;
		mymin.tv_usec += ONE_MILLION;
	}

	result->tv_sec = mymin.tv_sec - sub->tv_sec;
	result->tv_usec = mymin.tv_usec - sub->tv_usec;

	assert (tv_is_normalized (result));
}


int
tv_is_normalized (const struct timeval *tv)
{
	assert (tv != NULL);

	return (tv->tv_usec < ONE_MILLION ? TRUE : FALSE);
}


void
tv_normalize (struct timeval *tv)
{
	assert (tv != NULL);

	if (tv->tv_usec >= ONE_MILLION) {
		tv->tv_sec += (tv->tv_usec / ONE_MILLION);
		tv->tv_usec = (tv->tv_usec % ONE_MILLION);
	}
}

void
tv_print (const struct timeval *tv)
{
	printf ("%ldsec %ldusec", tv->tv_sec, tv->tv_usec);
}
