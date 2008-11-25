#define     _POSIX_C_SOURCE     1     /* per getaddrinfo */

#include <assert.h>
#include <netdb.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "util.h"
#include "types.h"


char *
my_strdup (const char *str)
{
	char *new_str;
	size_t len;

	assert (str != NULL);

	len = strlen (str) + 1;
	new_str = my_alloc (len * sizeof(char));
	strcpy (new_str, str); 

	return new_str;
}


list_node_t *
new_node (void *ptr)
{
	list_node_t *new;

	new = my_alloc (sizeof(list_node_t));
	new->n_ptr = ptr;
	return new;
}


fd_t
socket_bound (const char *bind_ip, const char *bind_port)
/* Ritorna un socket AF_INET SOCK_DGRAM e lo binda all'indirizzo e alla porta
 * dati. */
{
	int err;
	fd_t new_sfd;
	struct addrinfo addr_hints;
	struct addrinfo *addr_results;
	struct addrinfo *res;

	/* getaddrinfo hints */
	addr_hints.ai_family = AF_INET;
	addr_hints.ai_socktype = SOCK_DGRAM;
	addr_hints.ai_protocol = IPPROTO_UDP;
	addr_hints.ai_flags = AI_NUMERICSERV;
	addr_hints.ai_next = NULL;
	addr_hints.ai_addr = NULL;
	addr_hints.ai_addrlen = 0;
	addr_hints.ai_canonname = NULL;

	/* getaddrinfo */
	err = getaddrinfo (bind_ip, bind_port, &addr_hints, &addr_results);
	if (err) {
		fprintf (stderr, "getaddrinfo: %s\n", gai_strerror (err));
		goto getaddrinfo_err;
	}

	/* Prova gli addrinfo ritornati. */
	new_sfd = -1;
	res = addr_results;
	while (new_sfd == -1 && res != NULL) {
		assert (res->ai_family == AF_INET);
		assert (res->ai_socktype == SOCK_DGRAM);
		assert (res->ai_protocol == IPPROTO_UDP);

		new_sfd = socket (AF_INET, SOCK_DGRAM, IPPROTO_UDP);
		if (new_sfd == -1)
			continue;

		err = bind (new_sfd, res->ai_addr, res->ai_addrlen);
		if (err) {
			close (new_sfd);
			new_sfd = -1;
			res = res->ai_next;
		}
	}
	if (new_sfd == -1)
		goto bind_err;

	freeaddrinfo (addr_results);
	return new_sfd;

bind_err:
	freeaddrinfo (addr_results);
getaddrinfo_err:
	return -1;
}


void *
my_alloc (size_t nbytes)
/* Alloca nbytes di memoria. Se non riesce dealloca roba inutilizzata e
 * riprova. Se ancora non riesce, esce dal programma. */
{
	void *new;

	assert (nbytes > 0);

	new = malloc (nbytes);
	if (new == NULL) {
		collect_garbage ();
		new = malloc (nbytes);
		if (new == NULL) {
			perror ("Errore allocazione");
			exit (EXIT_FAILURE);
		}
	}

	return new;
}


void
collect_garbage (void)
/* Dealloca tutte le strutture dati che sono rimaste in memoria a girarsi i
 * pollici. */
{
	/* TODO collect_dgram_garbage (); */
}
