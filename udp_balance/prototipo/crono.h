#ifndef CRONO_H
#define CRONO_H

#include "types.h"

void timeout_set(timeout_t *to, const struct timeval *max);
void timeout_start(timeout_t *to, const struct timeval *now);
void timeout_left(timeout_t *to, const struct timeval *now, struct timeval *result);
void crono_measure(crono_t *cr, const struct timeval *now, struct timeval *result);
void crono_read(crono_t *cr, struct timeval *result);
void crono_start(crono_t *cr, const struct timeval *now);
int tv_cmp(const struct timeval *tv_1, const struct timeval *tv_2);
void tv_min(struct timeval *result, const struct timeval *tv_1, const struct timeval *tv_2);
void gettime(struct timeval *tv);
double tv2d(struct timeval *tv, bool must_free);
void tv_diff(struct timeval *result, const struct timeval *min, const struct timeval *sub);
int tv_is_normalized(const struct timeval *tv);
void tv_normalize(struct timeval *tv);

#endif /* CRONO_H */
