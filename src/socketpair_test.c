#include <assert.h>
#include <errno.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>


#define     ARRAYLEN(x)     (sizeof((x)) / sizeof(*(x)))

static int
launch_coin (void)
{
	return random () % 2;
}


int
main (int argc, char **argv)
{
	int i;
	int err;
	char *name[2] = {"Alice", "Bob"};
	int sfd[2];
	struct pollfd pfd[2];
	char dg_payload[100] = {'a'};

	srandom (time (NULL) + getpid ());

	err = socketpair (AF_UNIX, SOCK_DGRAM, 0, sfd);
	if (err) {
		perror ("socketpair");
		exit (EXIT_FAILURE);
	}

	for (i = 0; i < ARRAYLEN (sfd); i++)
		pfd[i].fd = sfd[i];

	while (1) {
		int talk;
		int nready;

		for (i = 0; i < ARRAYLEN (pfd); i++) {
			pfd[i].events = 0;
			pfd[i].events |= POLLIN;
			pfd[i].events |= POLLERR;
		}

		talk = launch_coin ();
		printf ("Now talking: %s\n", name[talk]);
		pfd[talk].events |= POLLOUT;

		do {
			nready = poll (pfd, ARRAYLEN (pfd), -1);
		} while (nready == -1 && errno == EINTR);
		if (nready == -1) {
			perror ("poll");
			exit (EXIT_FAILURE);
		}

		for (i = 0; i < ARRAYLEN (pfd); i++) {
			if (pfd[i].revents & POLLOUT) {
				ssize_t nsent;

				do {
					nsent = send (pfd[1 - i].fd, dg_payload, ARRAYLEN (dg_payload), 0);
				} while (nsent == -1 && errno == EINTR);
				if (nsent == -1) {
					perror ("send");
					exit (EXIT_FAILURE);
				}
				printf ("%s sends %d bytes\n", name[i], nsent);
			}

			if (pfd[i].revents & POLLIN) {
				ssize_t nrecv;

				do {
					nrecv = recv (pfd[i].fd, dg_payload, ARRAYLEN (dg_payload), 0);
				} while (nrecv == -1 && errno == EINTR);
				if (nrecv == -1) {
					perror ("recv");
					exit (EXIT_FAILURE);
				}
				printf ("%s recvs %d bytes\n", name[i], nrecv);
			}
		}
	}

	return EXIT_SUCCESS;
}
