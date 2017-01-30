#include <stdio.h>
#include <rpc/rpc.h>
#include "scaserRpc_vx.h"

void
scaserprog_1(rqstp, transp)
	struct svc_req *rqstp;
	SVCXPRT *transp;
{
	union {
		GETREQUEST scaser_getscalers_1_arg;
		GETREQUEST scaser_geteorscalers_1_arg;
	} argument;
	char *result;
	bool_t (*xdr_argument)(), (*xdr_result)();
	char *(*local)();

	switch (rqstp->rq_proc) {
	case NULLPROC:
		(void)svc_sendreply(transp, xdr_void, (char *)NULL);
		return;

	case SCASER_CLEAR:
		xdr_argument = xdr_void;
		xdr_result = xdr_int;
		local = (char *(*)()) scaser_clear_1;
		break;

	case SCASER_GETINFO:
		xdr_argument = xdr_void;
		xdr_result = xdr_INFO;
		local = (char *(*)()) scaser_getinfo_1;
		break;

	case SCASER_GETSCALERS:
		xdr_argument = xdr_GETREQUEST;
		xdr_result = xdr_VALUES;
		local = (char *(*)()) scaser_getscalers_1;
		break;

	case SCASER_GETEORSCALERS:
		xdr_argument = xdr_GETREQUEST;
		xdr_result = xdr_VALUES;
		local = (char *(*)()) scaser_geteorscalers_1;
		break;

	default:
		svcerr_noproc(transp);
		return;
	}
	bzero((char *)&argument, sizeof(argument));
	if (!svc_getargs(transp, xdr_argument, &argument)) {
		svcerr_decode(transp);
		return;
	}
	result = (*local)(&argument, rqstp);
	if (result != NULL && !svc_sendreply(transp, xdr_result, result)) {
		svcerr_systemerr(transp);
	}
	if (!svc_freeargs(transp, xdr_argument, &argument)) {
		(void)fprintf(stderr, "unable to free arguments\n");
		exit(1);
	}
}

