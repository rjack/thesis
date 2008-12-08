#ifndef CRONO_H
#define CRONO_H

#include "types.h"
#include <sys/time.h>


/*
 * Cronometro.
 */
typedef struct {
	struct timeval cr_elapsed;
	struct timeval cr_start;
} crono_t;


/*
 * Timeout.
 */
typedef struct timeout_t {
	/* Durata del timeout e cronometro. */
	struct timeval to_maxval;
	crono_t to_crono;
} timeout_t;


double tv2d(struct timeval *tv, bool must_free);
int tv_cmp(const struct timeval *tv_1, const struct timeval *tv_2);
int tv_is_normalized(const struct timeval *tv);
void crono_measure(crono_t *cr, const struct timeval *now, struct timeval *result);
void crono_print(const crono_t *cr);
void crono_read(crono_t *cr, struct timeval *result);
void crono_start(crono_t *cr, const struct timeval *now);
void gettime(struct timeval *tv);
void timeout_left(timeout_t *to, const struct timeval *now, struct timeval *result);
void timeout_print(const timeout_t *to);
void timeout_set(timeout_t *to, const struct timeval *max);
void timeout_start(timeout_t *to, const struct timeval *now);
void tv_diff(struct timeval *result, const struct timeval *min, const struct timeval *sub);
void tv_min(struct timeval *result, const struct timeval *tv_1, const struct timeval *tv_2);
void tv_normalize(struct timeval *tv);
void tv_print(const struct timeval *tv);

#endif /* CRONO_H */
