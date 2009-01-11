#ifndef ULB_PROTO_CRONO_H
#define ULB_PROTO_CRONO_H

#include "types.h"
#include <sys/time.h>


#define     HUGE_TV_SEC     10000000


/*
 * Cronometro.
 */
typedef struct {
	struct timeval cr_elapsed;
	struct timeval cr_start;
} crono_t;

void crono_measure(crono_t *cr, struct timeval *result);
void crono_read(crono_t *cr, struct timeval *result);
void crono_start(crono_t *cr);
void crono_print(const crono_t *cr);
int tv_cmp(const struct timeval *tv_1, const struct timeval *tv_2);
void tv_min(struct timeval *result, const struct timeval *tv_1, const struct timeval *tv_2);
void gettime(struct timeval *tv);
double tv2d(struct timeval *tv, bool must_free);
void tv_diff(struct timeval *result, const struct timeval *min, const struct timeval *sub);
int tv_is_normalized(const struct timeval *tv);
void tv_normalize(struct timeval *tv);
void tv_print(const struct timeval *tv);
void tv_set(struct timeval *tv, time_t secs, suseconds_t usecs);

#endif /* ULB_PROTO_CRONO_H */
