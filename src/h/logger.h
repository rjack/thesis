#ifndef ULB_LOGGER_H
#define ULB_LOGGER_H

#include "crono.h"


/*******************************************************************************
				    Macros
*******************************************************************************/

/*
 * Wrapper for printf, adds timestamp before and newline after the format
 * string.
 *
 * Example:
 * 	LOG ("Ciao");
 * 	LOG ("come va?");
 * outputs
 * 	12345634:224563 Ciao
 * 	12345634:224565 Come va?
 *
 */
#define     LOG(fmt, ...)                           \
	{                                           \
		struct timeval now;                 \
		gettime (&now);                     \
		printf ("%ld:%.6ld " fmt "\n",      \
		        now.tv_sec, now.tv_usec,    \
		        __VA_ARGS__);               \
	}


#endif /* ULB_LOGGER_H */
