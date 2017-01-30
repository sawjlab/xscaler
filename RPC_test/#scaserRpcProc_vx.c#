/* Actual rpc routines on server side.   */
#include <stdio.h>
#include <rpc/rpc.h>
#include <stdlib.h>
#include <string.h>

#include "scaserRpc_vx.h"

void rpcTaskInit();

void localServerInit()
{
  (void) rpcTaskInit();

  /* Done in setup_scalers */
  /*  set_runstatus(0); */

}

int *scaser_clear_1(void *arg, CLIENT *clnt)
{
  static int result;

  /* printf("Ack: scaser_clear_1\n");*/
  clear_scalers(0);

  result = 0;
  return(&result);
}

INFO *scaser_getinfo_1(void *arg, CLIENT *clnt)
{
  static INFO result;
  int slot;

  /*  printf("Ack: scaser_getinfo_1\n");*/
  
  xdr_free(xdr_INFO, (void *) &result);

  result.channels = get_num_scalers();
  result.slots = result.channels/64;
  result.runinprogress = get_runstatus();
  result.slotinfo.slotinfo_len = result.slots;
  result.slotinfo.slotinfo_val = (SLOTINFO *)
    malloc(result.slotinfo.slotinfo_len
	   *sizeof(SLOTINFO));

  for(slot=0; slot<result.slots; slot++) {
    result.slotinfo.slotinfo_val[slot].scalertype=(char *)malloc(3);
    strcpy(result.slotinfo.slotinfo_val[slot].scalertype,"F1");
    result.slotinfo.slotinfo_val[slot].nchans=64;
  }

  return(&result);
}

VALUES *scaser_getscalers_1(GETREQUEST *arg, CLIENT *clnt)
{
  static VALUES result;
  int chan;
  int ntotscalers;
  int eor;

  /*  printf("Ack: scaser_getscalers_1\n");*/

  xdr_free(xdr_VALUES, (void *) &result);
  
  ntotscalers = get_num_scalers();

  /* Change first and count to legal values */
  result.first = ((arg->first < 0)?0:arg->first); /* Not negative */
  result.count = ((arg->count <= 0)?ntotscalers:arg->count); /* Not negative */
  if(result.first + result.count > ntotscalers) {
    result.count = ntotscalers - result.first;
    if(result.count < 0) result.count = 0;
  }
  result.value.value_len = result.count;
  result.value.value_val = (VALUE *) malloc(result.value.value_len*sizeof(VALUE));
  result.read_counter = scalers_copy(result.value.value_val
				     ,result.first,result.count);

  return(&result);
}

VALUES *scaser_geteorscalers_1(GETREQUEST *arg, CLIENT *clnt)
{
  static VALUES result;
  int chan;
  int ntotscalers;

  /*  printf("Ack: scaser_getscalers_1\n");*/

  xdr_free(xdr_VALUES, (void *) &result);
  
  ntotscalers = get_num_scalers();

  /* Change first and count to legal values */
  result.first = ((arg->first < 0)?0:arg->first); /* Not negative */
  result.count = ((arg->count <= 0)?ntotscalers:arg->count); /* Not negative */
  if(result.first + result.count > ntotscalers) {
    result.count = ntotscalers - result.first;
    if(result.count < 0) result.count = 0;
  }
  result.value.value_len = result.count;
  result.value.value_val = (VALUE *) malloc(result.value.value_len*sizeof(VALUE));
  result.read_counter = eor_scalers_copy(result.value.value_val
				     ,result.first,result.count);

  return(&result);
}
