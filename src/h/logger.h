#ifndef ULB_LOGGER_H
#define ULB_LOGGER_H

#include <stdio.h>

#include "crono.h"


/*******************************************************************************
				    Macros
*******************************************************************************/

/*
 * Wrapper for printf, adds timestamp before and newline after the format
 * string.
 *
 * Example:
 * 	log ("Ciao");
 * 	log ("come va?");
 * outputs
 * 	12345634:224563 Ciao
 * 	12345634:224565 Come va?
 *
 */
#define     log(fmt, ...)                                    \
	{                                                    \
		struct timeval now;                          \
		gettime (&now);                              \
		printf ("%ld:%.6ld " fmt "\n",               \
		        now.tv_sec, now.tv_usec,             \
		        __VA_ARGS__);                        \
	}


/*
 * Same as log, but outputs on standard error.
 * fflush on stdout is needed because stdout may be buffered, while stderr
 * is unbuffered.
 */
#define     log_err(fmt, ...)                                \
	{                                                    \
		struct timeval now;                          \
		gettime (&now);                              \
		fflush (stdout);                             \
		fprintf (stderr,                             \
			 "%ld:%.6ld " fmt " on %s:%d\n",     \
		         now.tv_sec, now.tv_usec,            \
		         __VA_ARGS__, __FILE__, __LINE__);   \
	}


#endif /* ULB_LOGGER_H */
