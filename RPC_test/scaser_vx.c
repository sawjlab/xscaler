/*
  scaser_vx.c

  Main routine for scaSrvr.

*/

#include <stdio.h>
#include <rpc/rpc.h>
#include "scaserRpc.h"

static void scaserprog_1();

#if 0
main()
#else
void scaSrvr()
#endif
{
	SVCXPRT *transp;

	localServerInit();

	(void)pmap_unset(SCASERPROG, SCASERVERS);

	transp = svcudp_create(RPC_ANYSOCK);
	if (transp == NULL) {
		(void)fprintf(stderr, "cannot create udp service.\n");
		exit(1);
	}
	if (!svc_register(transp, SCASERPROG, SCASERVERS, scaserprog_1, IPPROTO_UDP)) {
		(void)fprintf(stderr, "unable to register (SCASERPROG, SCASERVERS, udp).\n");
		/*		exit(1);*/
	}

	transp = svctcp_create(RPC_ANYSOCK, 0, 0);
	if (transp == NULL) {
		(void)fprintf(stderr, "cannot create tcp service.\n");
		exit(1);
	}
	if (!svc_register(transp, SCASERPROG, SCASERVERS, scaserprog_1, IPPROTO_TCP)) {
		(void)fprintf(stderr, "unable to register (SCASERPROG, SCASERVERS, tcp).\n");
		exit(1);
	}
	svc_run();
	(void)fprintf(stderr, "svc_run returned\n");
	exit(1);
}

