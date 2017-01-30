/*
 * A program to pause or result a run
 */

#include <stdio.h>
#include <stdlib.h> /* getenv, exit */
#include <rpc/rpc.h>

main(argc, argv)
	int argc;
	char *argv[];
{
	char *host;
	CLIENT *handle;
	int *scalers,*overflows;
	int channels,iclock,i;
	int tsgo;

	if (argc < 3) {
		printf("usage: %s server_host 0/1 (pause/resume) \n", argv[0]);
		exit(1);
	}
	host = argv[1];
	tsgo = atoi(argv[2]);

	if(!(handle=scaserOpen(host))) {
	  fprintf(stderr,"Error connecting to %s\n",host);
	  exit(1);
	}

	if(tsgo == 0) {
	  printf("Pausing run\n");
	  scaserTsGo(handle,tsgo);
	} else if (tsgo == 1) {
	  printf("Resuming run\n");
	  scaserTsGo(handle,tsgo);
	}	  
	  
	exit(0);
}
