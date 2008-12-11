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

#include "crono.h"
#include "util.h"
#include "types.h"


int
cmp_ptr (void *ptr_1, void *ptr_2)
{
	if (ptr_1 == ptr_2)
		return 0;
	if (ptr_1 < ptr_2)
		return -1;
	return 1;
}

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


char *
my_strncpy (char *dest, const char *src, size_t nbytes)
{
	strncpy (dest, src, nbytes);
	if (nbytes > 0)
		dest[nbytes - 1] = '\0';

	return dest;
}


bool
parse_im_msg (char **ifname_result, char **cmd_result, char **ip_result,
              const char *msg, size_t msg_len)
{
	/* Formato farlocco msg configurazione iface manager:
	 * "ifname {up|down} xxx.xxx.xxx.xxx\n" */

	int i;
	size_t ifname_len;
	size_t cmd_len;
	size_t ip_len;

	assert (msg != NULL);
	assert (msg_len > 0);

	/*
	 * Copia di ifname in ifname_result.
	 */
	/* i avanza fino alla fine di ifname */
	for (i = 0; i < msg_len && msg[i] != ' '; i++);
	if (i == msg_len)
		goto ifname_parse_error;

	ifname_len = ++i;     /* incremento conta terminatore */
	*ifname_result = my_alloc (ifname_len * sizeof(char));
	memcpy (*ifname_result, msg, ifname_len);
	(*ifname_result)[ifname_len - 1] = '\0';

	/*
	 * Copia di cmd in cmd_result.
	 */
	/* i avanza fino alla fine di cmd */
	for (msg = &msg[i], i = 0, msg_len -= ifname_len;
	     i < msg_len && msg[i] != ' ';
	     i++);
	if (i == msg_len)
		goto cmd_parse_error;

	cmd_len = ++i;
	*cmd_result = my_alloc (cmd_len * sizeof(char));
	memcpy (*cmd_result, msg, cmd_len);
	(*cmd_result)[cmd_len - 1] = '\0';
	if (strcmp (*cmd_result, "up") != 0
	    && strcmp (*cmd_result, "down") != 0)
		goto bad_cmd_error;

	/*
	 * Copia di ip in ip_result.
	 */
	/* i avanza fino alla fine di xxx.xxx.xxx.xxx */
	for (msg = &msg[i], i = 0, msg_len -= cmd_len;
	     i < msg_len && msg[i] != '\n';
	     i++);
	if (i == msg_len)
		goto ip_parse_error;
	ip_len = ++i;
	*ip_result = my_alloc (ip_len * sizeof(char));
	memcpy (*ip_result, msg, ip_len);
	(*ip_result)[ip_len - 1] = '\0';

	return TRUE;

ip_parse_error:
bad_cmd_error:
	free (*cmd_result);
cmd_parse_error:
	free (*ifname_result);
ifname_parse_error:
	fprintf (stderr, "Errore parsing nome interfaccia\n");
	return FALSE;
}


fd_t
socket_bound_conn (const char *loc_ip, const char *loc_port,
                   const char *rem_ip, const char *rem_port)
{
	int err;
	fd_t new_sfd;
	struct addrinfo addr_hints;
	struct addrinfo *bind_results;
	struct addrinfo *conn_results;
	struct addrinfo *cur_bind;
	struct addrinfo *cur_conn;
	bool must_connect;

	must_connect = (rem_ip != NULL || rem_port != NULL) ? TRUE : FALSE;

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
	err = getaddrinfo (loc_ip, loc_port, &addr_hints, &bind_results);
	if (err) {
		fprintf (stderr, "getaddrinfo: %s\n", gai_strerror (err));
		goto getaddrinfo_bind_err;
	}

	if (must_connect) {
		err = getaddrinfo (rem_ip, rem_port, &addr_hints,
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

	/*
	 * Imposta l'opzione IP_RECVERR per abilitare la MSG_ERRQUEUE.
	 */
	err = 1;     /* usato come optval */
	err = setsockopt (new_sfd, IPPROTO_IP, IP_RECVERR, &err, sizeof(err));
	if (err == -1) {
		perror ("setsockopt");
		goto setsockopt_err;
	}

	freeaddrinfo (bind_results);
	if (must_connect)
		freeaddrinfo (conn_results);

	return new_sfd;


setsockopt_err:
bind_conn_err:
	close (new_sfd);
	if (must_connect)
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
