#define     _POSIX_C_SOURCE     1     /* per getaddrinfo */

#include <assert.h>
#include <errno.h>
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
socket_bound_conn (const char *bind_ip, const char *bind_port,
                   const char *conn_ip, const char *conn_port)
{
	int err;
	fd_t new_sfd;
	struct addrinfo addr_hints;
	struct addrinfo *bind_results;
	struct addrinfo *conn_results;
	struct addrinfo *cur_bind;
	struct addrinfo *cur_conn;
	bool must_connect;

	must_connect = (conn_ip != NULL || conn_port != NULL) ? TRUE : FALSE;

	/* getaddrinfo hints */
	addr_hints.ai_family = AF_INET;
	addr_hints.ai_socktype = SOCK_DGRAM;
	addr_hints.ai_protocol = IPPROTO_UDP;
	addr_hints.ai_flags = AI_NUMERICSERV;
	addr_hints.ai_next = NULL;
	addr_hints.ai_addr = NULL;
	addr_hints.ai_addrlen = 0;
	addr_hints.ai_canonname = NULL;

	/* getaddrinfo bind */
	err = getaddrinfo (bind_ip, bind_port, &addr_hints, &bind_results);
	if (err) {
		fprintf (stderr, "getaddrinfo: %s\n", gai_strerror (err));
		goto getaddrinfo_bind_err;
	}

	if (must_connect) {
		err = getaddrinfo (conn_ip, conn_port, &addr_hints,
		                   &conn_results);
		if (err) {
			fprintf (stderr, "getaddrinfo: %s\n",
			         gai_strerror (err));
			goto getaddrinfo_conn_err;
		}
	}

	/* Prova gli addrinfo ritornati. */
	new_sfd = -1;
	cur_bind = bind_results;
	cur_conn = must_connect ? conn_results : NULL;

	while (new_sfd == -1
	       && cur_bind != NULL
	       && (!must_connect || cur_conn != NULL)) {

		new_sfd = socket (AF_INET, SOCK_DGRAM, IPPROTO_UDP);
		if (new_sfd == -1) {
			perror ("socket (socket_bound_conn)");
			exit (EXIT_FAILURE);
		}

		do {
			err = bind (new_sfd, cur_bind->ai_addr,
			            cur_bind->ai_addrlen);
		} while (err == -1 && errno == EINTR);
		if (err) {
			close (new_sfd);
			new_sfd = -1;
			cur_bind = cur_bind->ai_next;
		} else if (must_connect) {
			do {
				err = connect (new_sfd, cur_conn->ai_addr,
				               cur_conn->ai_addrlen);
			} while (err == -1 && errno == EINTR);
			if (err) {
				close (new_sfd);
				new_sfd = -1;
				cur_conn = cur_conn->ai_next;
			}
		}
	}
	if (new_sfd == -1)
		goto bind_conn_err;

	freeaddrinfo (bind_results);
	freeaddrinfo (conn_results);
	return new_sfd;

bind_conn_err:
	freeaddrinfo (conn_results);
getaddrinfo_conn_err:
	freeaddrinfo (bind_results);
getaddrinfo_bind_err:
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
