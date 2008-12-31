#define ULP_PROTO_MAIN

#include <assert.h>
#include <errno.h>
#include <netinet/in.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "dgram.h"
#include "types.h"
#include "util.h"

#define     LOCAL_IP       PX_REM_IP
#define     LOCAL_PORT     PX_REM_PORT


int
main (int argc, char *argv[])
{
	fd_t fd;
	dgram_t *datagram;
	struct sockaddr_in cli_addr;
	socklen_t cli_addrlen;

	datagram = NULL;
	cli_addrlen = sizeof(cli_addr);
	memset (&cli_addr, 0, cli_addrlen);
	cli_addr.sin_family = AF_INET;

	fd = socket_bound_conn (LOCAL_IP, LOCAL_PORT, NULL, NULL);
	if (fd == -1)
		exit (EXIT_FAILURE);

	for (;;)
		if (datagram == NULL) {
			datagram = dgram_read (fd, &cli_addr, &cli_addrlen);
			if (datagram->dg_datalen == 0) {
				printf ("ricevuto keepalive\n");
				fflush (stdin);
				dgram_destroy (datagram);
				datagram = NULL;
			} else {
				printf ("ricevuto datagram: ");
				dgram_print (datagram);
				printf ("\n");
				fflush (stdout);
			}
		} else {
			dgram_write (fd, datagram, &cli_addr, cli_addrlen);
			dgram_destroy (datagram);
			datagram = NULL;
		}

	return 0;
}
