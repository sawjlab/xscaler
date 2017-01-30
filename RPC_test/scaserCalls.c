/* scaserCalls.c

   Simple interface routines to scaler server RPC calls.

   */
#include "scaserCalls.h"
#include <stdio.h>
#include <stdlib.h>

CLIENT *scaserOpen(char *host)
{
  CLIENT *clnt;
  clnt = clnt_create(host, SCASERPROG, SCASERVERS, "tcp");
  if (clnt == (CLIENT *) NULL) {
    clnt_pcreateerror(host);
    return(NULL);
  }
  return(clnt);
}

int scaserClose(CLIENT *clnt) {
  clnt_destroy(clnt);
  return(1);
}

int scaserClear(CLIENT *clnt) {
  int *result;
  char *scaser_clear_1_arg;

  result = scaser_clear_1((void *)&scaser_clear_1_arg, clnt);
  if(result == (int *) NULL) {
    clnt_perror(clnt, "scaser_clear_1 call failed");
    return(0);
  }
  return(1);
}
int scaserGetInfo(CLIENT *clnt, int *slots, int *channels, int *rip, int *clock)
{
  INFO  *result;
  char *scaser_getinfo_1_arg;

  result = scaser_getinfo_1((void *)&scaser_getinfo_1_arg, clnt);
  if (result == (INFO *) NULL) {
    clnt_perror(clnt, "scaser_getinfo_1 call failed");
    return(0);
  }
  if(slots) *slots = result->slots;
  if(channels) *channels = result->channels;
  if(rip) *rip = result->runinprogress;
  if(clock) *clock = result->clock_channel;

  xdr_free(xdr_INFO, (void *) result);

  return(1);
}

int scaserReadScalers(CLIENT *clnt, int first, int count, int *values,
		      int *overflows, int *events)
{
  VALUES  *result;
  GETREQUEST  scaser_getscalers_1_arg;
  int actualcount;
  int i;

  scaser_getscalers_1_arg.first = first;
  scaser_getscalers_1_arg.count = count;
  result = scaser_getscalers_1(&scaser_getscalers_1_arg, clnt);
  if (result == (VALUES *) NULL) {
    clnt_perror(clnt, "call failed");
    return(0);
  }
  actualcount = result->count;
  if(events) *events = result->read_counter;
  if(values) {
    for(i=0;i<actualcount;i++) {
      values[i] = result->value.value_val[i].val;
    }
  }
  if(overflows) {
    for(i=0;i<actualcount;i++) {
      overflows[i] = result->value.value_val[i].ovf;
    }
  }
  xdr_free(xdr_VALUES, (void *) result);
  return(actualcount);
}
int scaserReadEorScalers(CLIENT *clnt, int first, int count, int *values,
		      int *overflows, int *events)
{
  VALUES  *result;
  GETREQUEST  scaser_getscalers_1_arg;
  int actualcount;
  int i;

  scaser_getscalers_1_arg.first = first;
  scaser_getscalers_1_arg.count = count;
  result = scaser_geteorscalers_1(&scaser_getscalers_1_arg, clnt);
  if (result == (VALUES *) NULL) {
    clnt_perror(clnt, "call failed");
    return(0);
  }
  actualcount = result->count;
  if(events) *events = result->read_counter;
  if(values) {
    for(i=0;i<actualcount;i++) {
      values[i] = result->value.value_val[i].val;
    }
  }
  if(overflows) {
    for(i=0;i<actualcount;i++) {
      overflows[i] = result->value.value_val[i].ovf;
    }
  }
  xdr_free(xdr_VALUES, (void *) result);
  return(actualcount);
}
#if 0
int scaserTsGo(CLIENT *clnt, int onoff)
{
  int *result;
  int scaser_tsgo_1_arg;

  scaser_tsgo_1_arg = onoff;
  result = scaser_tsgo_1(&scaser_tsgo_1_arg, clnt);
  
  if(result == (int *) NULL) {
    clnt_perror(clnt, "scaser_tsgo_1 call failed");
    return(0);
  }
  return(1);
}
#endif
