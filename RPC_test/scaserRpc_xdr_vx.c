#include <rpc/rpc.h>
#include "scaserRpc_vx.h"


bool_t
xdr_SLOTINFO(xdrs, objp)
	XDR *xdrs;
	SLOTINFO *objp;
{
	if (!xdr_string(xdrs, &objp->scalertype, ~0)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->nchans)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_INFO(xdrs, objp)
	XDR *xdrs;
	INFO *objp;
{
	if (!xdr_int(xdrs, &objp->slots)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->channels)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->runinprogress)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->clock_channel)) {
		return (FALSE);
	}
	if (!xdr_array(xdrs, (char **)&objp->slotinfo.slotinfo_val, (u_int *)&objp->slotinfo.slotinfo_len, ~0, sizeof(SLOTINFO), xdr_SLOTINFO)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_GETREQUEST(xdrs, objp)
	XDR *xdrs;
	GETREQUEST *objp;
{
	if (!xdr_int(xdrs, &objp->first)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->count)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_VALUE(xdrs, objp)
	XDR *xdrs;
	VALUE *objp;
{
	if (!xdr_int(xdrs, &objp->val)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->ovf)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_VALUES(xdrs, objp)
	XDR *xdrs;
	VALUES *objp;
{
	if (!xdr_int(xdrs, &objp->first)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->count)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->read_counter)) {
		return (FALSE);
	}
	if (!xdr_array(xdrs, (char **)&objp->value.value_val, (u_int *)&objp->value.value_len, ~0, sizeof(VALUE), xdr_VALUE)) {
		return (FALSE);
	}
	return (TRUE);
}


