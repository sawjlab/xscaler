#define VERSION "V1.112"
/*#define READWHENASKED 1*/
#define HELICITYSCALERSINSERVER 1
#define HELICITYSCALERSBETWEENRUNS 1
/*#define DISABLEHELICITYSCALERSWHILEREADINGBETWEENRUNS 1*/
/*#define USE_VXMEMPROBE 1*/
/*
Scaler routines for VME scalers read by CRL files.

The readout is triggered with a forced sync of the TS.

Config line options:

  tscaler= number of seconds between scaler reads
  bscaler=lowest base address of lecroy and caen scaler 
  nscaler=number of lecroy and caen scaler
  bsscaler=lowest base address of struck scaler
  nsscaler=number of struck scalers
  scaler= name of file containing scaler specs

12.12.2007 (saw) Fix the trigger supervisor header word.
                 Fix trigger supervisor scaler reading in general

*/

#include <stdlib.h>
#include <stdio.h>
#include <sysLib.h>
#include <sysSymTbl.h>
#include <taskLib.h>

#define VX_FP_TASK 0x0008  /* What do we really need here??? */


#define MODEL_LECROY 1
#define MODEL_CAEN 2
#define MODEL_STRUCK 3
#define MODEL_STRUCK_3801 4
#define MODEL_EMPTY 0
/* Which channel of the helicity scalers is the clock? */
#define HELICITY_CLOCK_CHANNEL 1

#define SSCALER_CODA_OFFSET 640
#define ERROR_BUFFER_ADDRESS 0x8000
#define EVSCALER_ERROR_BUFFER_ADDRESS 0x8020
char *modelstrings[]={"Empty","Lecroy","CAEN","Struck","Struck_3801"};

#define CAEN_ID 0x0818
#define LECROY_ID 0x407

int HELICITY_DEBUG=0;

unsigned int *scalerBuffer;
unsigned int *scalerOverflows;
unsigned int *eorBuffer;
unsigned int *eorOverflows;
unsigned int *scalerTemp;
unsigned int *scalerTempBuffer;	/* Accumulate extra helicity events in FIFO */
unsigned int *scalerTempOverflows; /* and add into scalerBuffer when done */
int scalerRunInProgress=0;
int scalerBufferSize=0;
int scalerServerExists=0;
int scaler_event_counter=0;
int eor_scaler_event_counter=0;
int sscaler_coda_offset=SSCALER_CODA_OFFSET;
int sscaler_address_increment=0x1000;
int scaler_tid=0;

/* scalerBuffer layout for ROC 9

SCALER_MODULE_COUNT = 0
putenv "SSCALER_MODULE_COUNT=12"
putenv "HSCALER_MODULE_COUNT=3"
putenv "ESCALER_CHANNEL_COUNT=4"

0-383    Regular scalers
384-511  Helicity scalers
512-543  Event scaler

*/

/* LeCroy and CAEN  scalers */

struct scaler_ptrs {
  int model;
  unsigned short *id;
  unsigned short *reset;
  unsigned long *scaler;
};
static struct scaler_ptrs sptrs[32];

/* Struck scalers */

struct sscaler_ptrs {
  int model;
  unsigned long *control;
  unsigned short *clear;
  unsigned short *enable;
  unsigned short *disable;
  unsigned long *reset;
  unsigned long *scaler;
  unsigned long *shadow;
};
struct fifo_ptrs {
  int model;
  int version;
  unsigned long *csr;
  unsigned long *irq;
  unsigned long *cdr;
  unsigned long *wfifo;
  unsigned long *clear;
  unsigned long *next;
  unsigned long *enable;
  unsigned long *disable;
  unsigned long *reset;
  volatile unsigned long *fifo;
};
static struct fifo_ptrs ssptrs[32];

volatile struct vme_ts {
    volatile unsigned long csr;
    volatile unsigned long csr2;
    volatile unsigned long trig;
    volatile unsigned long roc;
    volatile unsigned long sync;
    volatile unsigned long trigCount;
    volatile unsigned long trigData;
    volatile unsigned long lrocData;
    volatile unsigned long prescale[8];
    volatile unsigned long timer[5];
    volatile unsigned long intVec;
    volatile unsigned long rocBufStatus;
    volatile unsigned long lrocBufStatus;
    volatile unsigned long rocAckStatus;
    volatile unsigned long userData1;
    volatile unsigned long userData2;
    volatile unsigned long state;
    volatile unsigned long test;
    volatile unsigned long blank1;
    volatile unsigned long scalAssign;
    volatile unsigned long scalControl;
    volatile unsigned long scaler[18];
    volatile unsigned long scalEvent;
    volatile unsigned long scalLive1;
    volatile unsigned long scalLive2;
    volatile unsigned long id;
  } VME_TS;
extern volatile struct vme_ts *ts;

#define TS_SCALER_COUNT 21
#define TS_SCALER_PROGRAM 0xdc6210
#define TS_ADDR 0xed0000


static int num_scalers;
static int num_sscalers;
static int num_hscalers;
static int num_escalers;
static int num_escaler_chans;
static int use_ts_scalers;
static int scaler_readout_period;
static void *base_scal_addr;
static void *base_sscal_addr;
void *lladdr;
static int event_channel;	/* Channel with event count */
static int clock_channel;	/* Channel with clock count (ticks) */

long scaler_readout_killer_flag;

void start_scaler_task();
void clear_scalers(int coda);
void enable_scalers();
void kill_scaler_task();
void set_runstatus(int status);

void read_event_scalers(long **dabufpp)
     /* Return -2 if FIFO is FULL before read
	       -1 if FIFO is empty before read (an error)
	        0 if FIFO is empty after read
		1 if FIFO has data after read (OK if buffered and not sync */
{
  int escaler_index;
  int csr;
  long *dabufp;
  int jj, splitboth, scal_header;
  int extra_events,error_code;
  unsigned int dummy;

  /*  logMsg("INFO dabufpp=%x, sync=%d\n",dabufpp,sync);*/
  dabufp = *dabufpp;
  if(num_escalers <= 0) return;

  if(num_hscalers ==3) splitboth = 1;
  else splitboth=0;

  extra_events = 0;
  error_code = 0;

  escaler_index = num_sscalers+num_hscalers;

  for(jj=0;jj<num_escaler_chans;jj++) {
    scalerTemp[jj] = 0;
  }

  csr = *ssptrs[escaler_index].csr;
  if(csr & 0x1000) {
    logMsg("ERROR Event scaler FIFO full.  This is BAD.  CSR=0x%x %x %d\n",csr,ssptrs[escaler_index].csr,escaler_index);
    *(ssptrs[escaler_index].clear) = 1; 
    extra_events = 1000000;
    error_code = 1;
    goto EXIT;
  } else if(csr & 0x100) {
    logMsg("ERROR Event scaler FIFO is empty.  This is BAD\n");
    extra_events = 0;
    error_code = 2;
    goto EXIT;
  }
  for(jj=0;jj<num_escaler_chans;jj++) {
    scalerTemp[jj] = *ssptrs[escaler_index].fifo;
  }

  scal_header = num_escaler_chans
    + ((32*(num_sscalers+2*num_hscalers+splitboth) + sscaler_coda_offset)<<16);
  *(dabufp)++ = scal_header;
  for(jj=0; jj < num_escaler_chans ; jj++) {
    int sbindex = num_scalers*16 + (escaler_index+splitboth)*32 + jj;
    unsigned int temp = scalerBuffer[sbindex];
    scalerBuffer[sbindex] += scalerTemp[jj];
    if(scalerBuffer[sbindex] < temp) scalerOverflows[sbindex]++;
    *(dabufp)++ = scalerBuffer[sbindex];
  }
  csr = *ssptrs[escaler_index].csr;

#if 0
  if(!(csr & 0x100)) { /* FIFO is not empty */
    error_code = 3;
    /* Now drain the buffer, counting events */
    while(!((*ssptrs[escaler_index].csr) & 0x100)) {
      extra_events++;
      for(jj=0; jj<num_escaler_chans; jj++) {
	dummy = *ssptrs[escaler_index].fifo;
      }
    }
    logMsg("ERROR %d extra event scaler events in FIFO\n",extra_events);
  }

#endif

  *dabufpp = dabufp;
  return;

EXIT:
  *(dabufp)++ = (0x00000002) 
    + (EVSCALER_ERROR_BUFFER_ADDRESS << 16);
  *(dabufp)++ = extra_events;
  *(dabufp)++ = error_code;
  *dabufpp = dabufp;
  return;
}

void check_event_scaler_syncronization(long **dabufpp)
     /* This routine should be called for sync events.  It will make sure
        that the FIFO is empty. */
{
  int escaler_index;
  int csr;
  long *dabufp;
  int extra_events,error_code;
  int jj,scal_header;
  unsigned int dummy;

  escaler_index = num_sscalers+num_hscalers;
  csr = *ssptrs[escaler_index].csr;
  error_code=0;

  if(!(csr & 0x100)) { /* FIFO is not empty */
    error_code = 3;
    /* Now drain the buffer, counting events */
    while(!((*ssptrs[escaler_index].csr) & 0x100)) {
      extra_events++;
      for(jj=0; jj<num_escaler_chans; jj++) {
	dummy = *ssptrs[escaler_index].fifo;
      }
    }
    logMsg("ERROR %d extra event scaler events in FIFO\n",extra_events);
  }
  if(error_code == 0) return;

  dabufp = *dabufpp;
  *(dabufp)++ = (0x00000002) 
    + (EVSCALER_ERROR_BUFFER_ADDRESS << 16);
  *(dabufp)++ = extra_events;
  *(dabufp)++ = error_code;
  *dabufpp = dabufp;
  return;
}
void read_helicity_scalers(long **dabufpp)
     /* 0 In between run.  Read for scaler server only */
     /* ptr Coda running.  dabufp */
     /* The helicity scalers get strobed externally rather than interally.
	The external strobe is not blocked when the DAQ is busy (or between
	runs for that matter.  Therefore, the read is not guaranteed to occur
	when the scaler is quiet.  This *may* be why vmec9 crashes more than
	other components of the GeN DAQ.  
	In this version, we won't do any aditional checking to see if the
	FIFO has data, but we will use vmeMemProbe to read it out.
	*/
{
  int jj, kk , scal_header;
  int bad, badbit, empty, helicity, purehelicity, csr;
  long *dabufp;
  int nevents;
  int splitboth;
  static int last_missing=0;
  int missing;
#ifdef USE_VXMEMPROBE
  int status;
#endif

  /* In addition to the scaler data, we will include a status "bank" within
     the event.  */


  if(dabufpp) dabufp = *dabufpp;
  else dabufp = 0;

  if(num_hscalers <0) return;

  if(num_hscalers ==3) splitboth = 1;
  else splitboth=0;

  nevents = 0;			/* Number of events found in FIFO */

  /* First check for FIFO full */
  bad = 0;
  badbit = 1;
  /*  if(!scalerRunInProgress) {printf("CFF\n"); taskDelay(15);}*/
  for(kk=0; kk<num_hscalers; kk++) {
#ifdef USE_VXMEMPROBE
    status = vxMemProbe(ssptrs[num_sscalers+kk].csr, 0, 4, &csr);
    if(status != 0) {
      logMsg("vxMemProbe FULL CHECK csr=%x,status=%x\n",csr,status);
    }
#else
    csr = *ssptrs[num_sscalers+kk].csr;
#endif
    if(csr & 0x1000) {
      logMsg("ERROR 3801 scaler #%d FIFO Full",num_sscalers+kk);
      bad += badbit; /* To indicate that scalers will need to be reset */
    }
    badbit <<= 1;
  }
  /*  if(!scalerRunInProgress) {printf("CFE\n"); taskDelay(15);}*/
  for(kk=0; kk<num_hscalers; kk++) {
#ifdef USE_VXMEMPROBE
    status = vxMemProbe(ssptrs[num_sscalers+kk].csr, 0, 4, &csr);
    if(status != 0) {
      logMsg("INFO vxMemProbe EMPTY CHECK csr=%x,status=%x",csr,status);
    }
#else
    csr = *ssptrs[num_sscalers+kk].csr;
#endif
    if(csr & 0x100) {
      if(scalerRunInProgress)
	logMsg("ERROR 3801 scaler #%d has no data @%d\n",num_sscalers+kk,tickGet());
      bad += badbit; /* To indicate that scalers will need to be reset */
    }
    badbit <<= 1;
  }

  if(bad) {			/* Reset FIFO's, return */
    /* Unless exactly 3 scalers were empty, clear all the scalers together */
    if(bad != 0x38) {
      for(kk=0; kk<num_hscalers; kk++) {
	*(ssptrs[num_sscalers+kk].clear) = 1; 
      }
    }
    goto EXIT;
  }



  /* Clear out the scalerTempBuffer and scalerTempOverflow arrays */
  for(kk=0; kk<(num_hscalers+splitboth); kk++) {
    for(jj=0; jj<32 ; jj++) {
      scalerTempBuffer[kk*32+jj] = 0;
      scalerTempOverflows[kk*32+jj] = 0;
    }
  }
  

  /*  if(!scalerRunInProgress) {printf("RDH\n"); taskDelay(15);}*/
  missing = 0;
  do {
    /*    if(!scalerRunInProgress) printf("X");*/
    /* Read one Latch's worth of scalers */
    bad = 0;
    helicity = 0;
    purehelicity = 0;
    for(kk=0; kk<num_hscalers; kk++) {
      unsigned int chan;

      for(jj=0; jj<32 ; jj++) {
      /* Note want int scalervalue in 24 bit mode since
         it is possible to have an overflow after 24 bit 
         events the value goes negative. However in 32 bit mode
         if you declair the scalervalue int only you limit the
         value to 1/2 the 32 bit struck scaler range. So below
         checking for overflow is usless in 32 bit mode */

	unsigned int scalervalue;
	int sbindex = kk*32+jj;
#ifdef USE_VXMEMPROBENO
	status = vxMemProbe(ssptrs[num_scalers+kk].fifo, 0,
			    4, &scalerTemp[sbindex]);
	if(status != 0) {
	  logMsg("INFO vxMemProbe %d %x"
		 ,num_sscalers+kk,status);
	}
#else
	csr = *ssptrs[num_sscalers+kk].csr;
	if(csr & 0x100) {	/* Check every read first */
	  bad++;
	} else {
	  scalerTemp[sbindex] = *ssptrs[num_sscalers+kk].fifo;
	}
#endif

#ifdef BIT24MODE
	scalervalue = (scalerTemp[sbindex] & 0x00ffffff);
#else
	scalervalue = scalerTemp[sbindex];
#endif

#ifdef BIT24MODE
	chan = (scalerTemp[sbindex]&0x1f000000)>>24;
	if(chan!=jj) bad++;	/* Channel number is in high byte */
#endif
	if(jj==HELICITY_CLOCK_CHANNEL) {
	  /* Count 1 count as Zero */
	  if(kk==0 && scalervalue > 1) helicity += 1;
	  else if (kk==1 && scalervalue > 1) helicity += 2;
	  if(kk==0 && scalervalue != 0) purehelicity += 1;
	  else if (kk==1 && scalervalue != 0) purehelicity += 2;
	}
      }
    }
    /* Add the counts into scalerTempBuffer */
    if(helicity<=0) {
      logMsg("ERROR No counts in helicity clock scalers@%d\n",tickGet());
    } else if(helicity==3) {
      logMsg("ERROR Counts in both helicity scalers @%d\n",tickGet());
      /*      logMsg("%d %d %d\n",scalerTemp[0*32+HELICITY_CLOCK_CHANNEL],scalerTemp[1*32+HELICITY_CLOCK_CHANNEL],scalerTemp[2*32+HELICITY_CLOCK_CHANNEL]);*/
      logMsg("ERROR %d %d %d\n",scalerTemp[0*32+0],scalerTemp[1*32+0],scalerTemp[2*32+0]);
      logMsg("ERROR %d %d %d\n",scalerTemp[0*32+1],scalerTemp[1*32+1],scalerTemp[2*32+1]);
      logMsg("ERROR %d %d %d\n",scalerTemp[0*32+2],scalerTemp[1*32+2],scalerTemp[2*32+2]);
      logMsg("ERROR %d %d %d\n",scalerTemp[0*32+3],scalerTemp[1*32+3],scalerTemp[2*32+3]);
      logMsg("ERROR %d %d %d\n",scalerTemp[0*32+4],scalerTemp[1*32+4],scalerTemp[2*32+4]);
    } else if(purehelicity == 3) {
      logMsg("WARN Counts in both helicity scalers @%d\n",tickGet());
      logMsg("WARN %d %d %d\n",scalerTemp[0*32+0],scalerTemp[1*32+0],scalerTemp[2*32+0]);
      logMsg("WARN %d %d %d\n",scalerTemp[0*32+1],scalerTemp[1*32+1],scalerTemp[2*32+1]);
      logMsg("WARN %d %d %d\n",scalerTemp[0*32+2],scalerTemp[1*32+2],scalerTemp[2*32+2]);
      logMsg("WARN %d %d %d\n",scalerTemp[0*32+3],scalerTemp[1*32+3],scalerTemp[2*32+3]);
      logMsg("WARN %d %d %d\n",scalerTemp[0*32+4],scalerTemp[1*32+4],scalerTemp[2*32+4]);
    } else if(scalerTemp[2*32+HELICITY_CLOCK_CHANNEL] < 325000 ||
	      scalerTemp[2*32+HELICITY_CLOCK_CHANNEL] > 325010) {
      logMsg("WARN Not 1/30 sec in clock scaler @%d\n",tickGet());
      logMsg("WARN %d %d %d\n",scalerTemp[0*32+0],scalerTemp[1*32+0],scalerTemp[2*32+0]);
      logMsg("WARN %d %d %d\n",scalerTemp[0*32+1],scalerTemp[1*32+1],scalerTemp[2*32+1]);
      logMsg("WARN %d %d %d\n",scalerTemp[0*32+2],scalerTemp[1*32+2],scalerTemp[2*32+2]);
      logMsg("WARN %d %d %d\n",scalerTemp[0*32+3],scalerTemp[1*32+3],scalerTemp[2*32+3]);
      logMsg("WARN %d %d %d\n",scalerTemp[0*32+4],scalerTemp[1*32+4],scalerTemp[2*32+4]);
    }      
    for(kk=0; kk<num_hscalers; kk++) {
      for(jj=0;  jj<32 ; jj++) {
	unsigned int scalervalue;
	int sbindex = kk*32+jj;
#ifdef BIT24MODE
	scalervalue = (scalerTemp[sbindex] & 0x00ffffff);
#else
	scalervalue = scalerTemp[sbindex];
#endif
	if(kk < 2 || !splitboth) {
	  unsigned int temp = scalerTempBuffer[sbindex];
	  scalerTempBuffer[sbindex] += scalervalue;
	  if(scalerTempBuffer[sbindex] < temp) scalerTempOverflows[sbindex]++;
	} else { /* The both helicity scaler */
	  if (helicity==1) { /* Positive helicity */
	    unsigned int temp = scalerTempBuffer[sbindex];
	    scalerTempBuffer[sbindex] += scalervalue;
	    if(scalerTempBuffer[sbindex] < temp) scalerTempOverflows[sbindex]++;
	  } else if (helicity==2) {
	    unsigned int temp = scalerTempBuffer[sbindex+32];
	    scalerTempBuffer[sbindex+32] += scalervalue;
	    if(scalerTempBuffer[sbindex+32] < temp) scalerTempOverflows[sbindex+32]++;
	  }
	}
      }
    }
    nevents++;
    /* Check if one or modules are empty */
    empty = 0;
    for(kk=0; kk<num_hscalers; kk++) {
      csr = *ssptrs[num_sscalers+kk].csr;
      if (csr & 0x100) {	/* Empty FIFO */
	empty++;
	missing += 1<<kk;
      }
    }
  } while (!empty && !bad);

  if(nevents > 1 && dabufp) {
    logMsg("INFO %d events not triggered @%d",nevents-1,tickGet());  
  }
  /*  if(!scalerRunInProgress) printf("Y\n");*/

  /* Now put the data into the event */
  if((empty == num_hscalers 
      || ((missing ==1 || missing ==3) && last_missing==7))
     && !bad) {
    if(missing==1 || missing ==3) {
	logMsg("INFO Accepting missing=%d because last_missing==%d @%d",missing,last_missing,tickGet());
    }
    for(kk=0; kk < (num_hscalers+splitboth); kk++) {
      scal_header = (0x00000020) + ((32*(kk+num_sscalers) + sscaler_coda_offset)<<16); /* Scalers start at 640 */
      if(dabufp) *(dabufp)++ = scal_header;

      for(jj=0; jj<32 ; jj++) { /* First put running sums into event */
	int sbindex = num_scalers*16+(num_sscalers+kk)*32+jj;
	unsigned int temp = scalerBuffer[sbindex];

	scalerBuffer[sbindex] += scalerTempBuffer[32*kk+jj];
	if(scalerBuffer[sbindex] < temp) scalerOverflows[sbindex]++;
	scalerOverflows[sbindex] += scalerTempOverflows[32*kk+jj];

	if(dabufp) *(dabufp)++ = scalerBuffer[sbindex];

      }
    }
    for(kk=0; kk < num_hscalers; kk++) {
    
      /* Now copy raw data with the header address bumped up by 3*32 */
      /* Bump up by 4*32 if we have 3 helicity scalers */
      scal_header = (0x00000020)
	+ ((32*(kk+num_sscalers) + sscaler_coda_offset)<<16)
	+ ((32*(num_hscalers+splitboth)) << 16);

      if(dabufp) *(dabufp)++ =  scal_header;
      for(jj=0; jj<32 ; jj++) {
	if(dabufp) *(dabufp)++ = scalerTemp[32*kk+jj];
      }
      /* Make sure the FIFO's are empty */
      /* Should probably move this to just after reading */
      /* We don't really need this check, because we should only be in this
	 if statement if all three scalers are empty. */
      /*
      while(!((*ssptrs[num_sscalers+kk].csr) & 0x100)) {
	unsigned int dummy;
	if(!scalerRunInProgress) printf("%d",kk);
	dummy = *ssptrs[num_sscalers+kk].fifo;
      }
      */
    }
  } else {
    bad <<= 24;
    if(empty != num_hscalers) { /* All OK, do what needs to be done */
      bad += (empty << 16);
      /* What are the OK missing patterns and what are the not OK missing
	 patterns
	 0 not possible.  Something had to be empty for loop to exit
	 1 First slot empty, but other filled.  Data came while checking first
	 2 Pathalogical
	 3 First two slots empty
	 4 Pathlogical
	 5 pathlogical
	 6 pathlogical
	 7 Should have been treated as normal
	 */
      if(missing==1 || missing == 3) {
	logMsg("ERROR Some helicity scalers missing data, bad=%x, missing=%x, last_missing=%x @ %d",bad,missing,last_missing,tickGet());
      } else {
	logMsg("ERROR Some helicity scalers missing data, bad=%x, missing=%x, last_missing=%x @ %d",bad,missing,last_missing,tickGet());
      }
    } else if(bad) logMsg("ERROR Incomplete helicity events bad=%x",bad);
  }
  last_missing = missing;

EXIT:
  if(dabufp) {
    *(dabufp)++ = (0x00000002) 
      + (ERROR_BUFFER_ADDRESS << 16);
    *(dabufp)++ = nevents;
    *(dabufp)++ = bad;
    *dabufpp = dabufp;
  }
  /*  if(!scalerRunInProgress) printf("Z\n");*/
  return;
}

void read_scalers(long **dabufpp)
     /* 0 In between run.  Read for scaler server only */
     /* ptr Coda running.  dabufp */
{
  int jj, kk , scal_header;
  int temp;
  long *dabufp;
  int scaler_temp[32];

 /* Define scaler header*/

  if(dabufpp) {
    dabufp = *dabufpp;
    /*    printf("Reading event into 0x%x\n",dabufp);*/
  }
  else dabufp = 0;

  if(num_scalers !=0) { /* Do nothing if nothing defined */
    for(kk=0; kk<num_scalers; kk++) {
      /* Use the 2nd byte of the VME address to assign a scaler # */
      scal_header = 0x00000010 + (((int) sptrs[kk].reset & 0xff00) << 12);
      if(dabufp) *(dabufp)++ =  scal_header;
      for(jj=0; jj<16 ; jj++) {
        temp = sptrs[kk].scaler[jj];
	if(kk==1 && jj==2) {
	  /*	  	printf("%d: %u\n",kk*16+jj,temp);*/
	}
        if(dabufp) *(dabufp)++ = temp;
	if(temp < scalerBuffer[kk*16+jj]) scalerOverflows[kk*16+jj]++;
	scalerBuffer[kk*16+jj] = temp;
	/*	printf("%d: %u\n",kk*16+jj,scalerBuffer[kk*16+jj]);*/
      }
    }
  }

  if(num_sscalers !=0) { /* Do nothing if nothing defined */
    for(kk=0; kk<num_sscalers; kk++) {
      /* First empty the fifo */
      while(!(*ssptrs[kk].csr & 0x100)) {
	int dummy;
	dummy = *ssptrs[kk].fifo;
      }
      *ssptrs[kk].next = 1;	/* Load Next Event */
    }
    /* How do I know if the FIFO has been loaded? */
    for(kk=0; kk<num_sscalers; kk++) {
      scal_header = (0x00000020) + 
	((32*kk + sscaler_coda_offset)<<16); /* Scalers start at 640 */
      /* Should check here for FIFO not empty */
      if((ssptrs[kk].version & 1) != 0) {/* 32 bit mode */
	for(jj=0; jj<32 ; jj++) {
	  if(*ssptrs[kk].csr & 0x100) { /* Fifo is empty.  This is not good */
	    printf("Scaler %d emptied early\n",kk);
	    break;
	  }
	  scaler_temp[jj] = *ssptrs[kk].fifo;
	}
	if(*ssptrs[kk].csr & 0x100) { /* Fifo is empty.  This is good */
	  if(dabufp) *(dabufp)++ =  scal_header;
	  for(jj=0; jj<32; jj++) {
	    int sbindex=num_scalers*16+kk*32+jj;
	    unsigned int temp = scalerBuffer[sbindex];
	    scalerBuffer[sbindex] += scaler_temp[jj];
	    if(dabufp) *(dabufp)++ = scalerBuffer[sbindex];
	    if(scalerBuffer[sbindex] < temp) scalerOverflows[sbindex]++;
	  }
	} else {		/* Fifo is not empty.  Data is JUNK.  Empty header */
	  if(dabufp) *(dabufp)++ =  scal_header&0xffff0000;
	  logMsg("ERROR 3801 FIFO at %x not empty at end of scaler read.  This is BAD",
		   ssptrs[kk].csr);
	}
      } else {		/* 24 bit word.  Put data word, not totals in data  */
	for(jj=0; jj<32 ; jj++) {
	  scaler_temp[jj] = *ssptrs[kk].fifo;
	}
	if(dabufp) *(dabufp)++ =  scal_header;
	for(jj=0; jj<32 ; jj++) {
	  int sbindex = num_scalers*16+kk*32+jj;
	  unsigned int temp = scalerBuffer[sbindex];
	  scalerBuffer[sbindex] += (scaler_temp[jj] & 0x00ffffff);
	  if(dabufp) *(dabufp)++ = scalerBuffer[sbindex];
	  if(scalerBuffer[sbindex] < temp) scalerOverflows[sbindex]++;
	}
	/* Should check for FIFO empty.  But we don't use the 24 bit mode here */
      }
    }
  }

  if(use_ts_scalers) {
    int ioff=num_scalers*16+num_sscalers*32+(num_hscalers+HELICITYSCALERSINSERVER*(num_hscalers==3?1:0))*32+num_escaler_chans;
    unsigned int temp;

    /*    scal_header = ((((int) sptrs[num_scalers-1].reset & 0xff00)+0x0100) << 12)
	  +TS_SCALER_COUNT;*/
    scal_header = TS_SCALER_COUNT+((32*num_sscalers+sscaler_coda_offset)<<16);
    if(dabufp) *(dabufp)++ =  scal_header;

    /* Trigger supervisor scalers don't clear on read */
    for(jj=0;jj<18;jj++) {
      temp = ts->scaler[jj];
      if(temp < scalerBuffer[ioff+jj]) scalerOverflows[ioff+jj]++;
      scalerBuffer[ioff+jj]=temp;
      if(dabufp) *(dabufp)++ = scalerBuffer[ioff+jj];
    }
    jj=18;
    temp = ts->scalEvent;
    if(temp < scalerBuffer[ioff+jj]) scalerOverflows[ioff+jj]++;
    scalerBuffer[ioff+jj]=temp;
    if(dabufp) *(dabufp)++ = scalerBuffer[ioff+jj];
    jj++;
    temp = ts->scalLive1;
    if(temp < scalerBuffer[ioff+jj]) scalerOverflows[ioff+jj]++;
    scalerBuffer[ioff+jj]=temp;
    if(dabufp) *(dabufp)++ = scalerBuffer[ioff+jj];
    jj++;
    temp = ts->scalLive2;
    if(temp < scalerBuffer[ioff+jj]) scalerOverflows[ioff+jj]++;
    scalerBuffer[ioff+jj]=temp;
    if(dabufp) *(dabufp)++ = scalerBuffer[ioff+jj];
  }
  if(dabufpp) {
    *dabufpp = dabufp;
    /*    printf("Pointer now at 0x%x\n",dabufp);*/
  }


  if(num_scalers !=0 || num_sscalers !=0) 
    scaler_event_counter++;

  scalerBuffer[event_channel] = scaler_event_counter;
  scalerOverflows[event_channel] = 0;
  scalerBuffer[clock_channel] = tickGet();
  scalerOverflows[clock_channel] = 0;
}

#if 0
void read_struckshadow(long **dabufpp)
{                           /* reads just the shadow register of the struck
                            scaler. requires external latch signal */
  int jj, kk , scal_header;
  long *dabufp;

  if(dabufpp) dabufp = *dabufpp;
  else dabufp = 0;

  if(num_sscalers !=0) { /* Do nothing if nothing defined */
    for(kk=0; kk<num_sscalers; kk++) {
      scal_header = (0x02000020) + 
                     2*((int) ssptrs[kk].control & 0x0ff00000);
      if(dabufp) *(dabufp)++ =  scal_header;
      for(jj=0; jj<32 ; jj++)
        if(dabufp) *(dabufp)++ = ssptrs[kk].shadow[jj];
    }
  }
}
#endif

void setup_scalers(int struckmode)
{
  int jj,res;
  char *S_ADDR, *SS_ADDR;
  int sscaler_address_increment=0x1000;
  void *scal_addr;
  char *s;
  void (*pscaSrvr)();
  SYM_TYPE symtype;
  STATUS symstat;
  int i;

  logMsg("INFO Setup scalers.  Version %s @%d",VERSION,tickGet());
  s = getenv("SCALER_MODULE_COUNT");
  if(s) num_scalers = atol(s);
  else num_scalers = 0;

  if(num_scalers > 0) {
    s = getenv("SCALER_ADDRESS_BASE");
    if(s) sscanf(s," %x",(unsigned int *)&S_ADDR);
    else S_ADDR = 0;
    if(!S_ADDR) {
      logMsg("ERROR Invalid scaler address");
    }
  }

  s = getenv("SSCALER_MODULE_COUNT");
  if(s) num_sscalers = atol(s);
  else num_sscalers = 0;
  
  s = getenv("SSCALER_CODA_OFFSET");
  if(s) sscaler_coda_offset=atol(s);
  else sscaler_coda_offset=SSCALER_CODA_OFFSET;

  s = getenv("USE_TS_SCALERS");
  if(s) use_ts_scalers=atol(s);
  else use_ts_scalers=0;

  s = getenv("HSCALER_MODULE_COUNT");
  if(s) num_hscalers = atol(s);
  else num_hscalers = 0;
  
  s = getenv("ESCALER_CHANNEL_COUNT"); /* Escalers, not to be confused with
					  Emeters */
  if(s) num_escaler_chans = atol(s);
  else num_escaler_chans = 0;
  if(num_escaler_chans > 24) num_escaler_chans = 32;
  else if(num_escaler_chans < 0) num_escaler_chans = 0;
  if(num_escaler_chans > 0) num_escalers = 1;
  else num_escalers = 0;

  logMsg("INFO %d channels of event scaler defined\n",num_escaler_chans);

  if((num_sscalers + num_hscalers + num_escalers) > 0) {
    s = getenv("SSCALER_ADDRESS_BASE");
    if(s) sscanf(s," %x",(unsigned int *)&SS_ADDR);
    else SS_ADDR = 0;
    if(!SS_ADDR) {
      logMsg("ERROR Invalid Struck scaler address");
    } else {
      logMsg("INFO SIS3801 base address=0x%x",SS_ADDR);
    }
  }

  s = getenv("SSCALER_ADDRESS_INCREMENT");
  if(s) sscanf(s," %x",(unsigned int *)&sscaler_address_increment);
  else sscaler_address_increment=0x1000;

  if(num_scalers + num_sscalers + num_hscalers + num_escalers <= 0) {
    logMsg("WARN No scalers defined");
  }

  if (num_scalers > 0) {
    res = (unsigned long) sysBusToLocalAdrs(0x39,S_ADDR,(char **) &lladdr);
    if (res != 0) {
      printf("Error in sysBusToLocalAdrs res=%d \n",res);
    }
    else base_scal_addr = lladdr;
  }

  if ((num_sscalers+num_hscalers+num_escalers) > 0) {
#if 1
    res = (unsigned long) sysBusToLocalAdrs(0x39,SS_ADDR,(char **) &lladdr);
    if (res != 0) {
      printf("Error in sysBusToLocalAdrs res=%d \n",res);
    }
    else base_sscal_addr = lladdr;
    printf("Base address=0x%x\n",lladdr);
#else
    base_sscal_addr = SS_ADDR;
#endif
  }

  if (num_scalers > 0)
    logMsg("INFO Configuring %d LeCroy and CAEN scalers at base address %x\n",
	   num_scalers,base_scal_addr);

  for(jj=0; jj<num_scalers; jj++) {
    scal_addr = ((base_scal_addr) + ((jj<<8)));
    if(*(unsigned short *)(scal_addr + 0xfc) == CAEN_ID) {
      sptrs[jj].model = MODEL_CAEN;
      sptrs[jj].reset = (unsigned short *) (scal_addr + 0x50);
      sptrs[jj].scaler = (unsigned long *) (scal_addr + 0x10);
    } else {		/* Lecroy */
      sptrs[jj].model = MODEL_LECROY;
      sptrs[jj].reset = (unsigned short *) (scal_addr + 0x0);
      sptrs[jj].scaler = (unsigned long *) (scal_addr + 0x80);
    }
    printf("Scaler %d: %x\n",jj,sptrs[jj].scaler);
  }

#if 0
  if (struckmode == 0) {
    sctrl = 0x0;
    logMsg("INFO Struck Scaler(s): No control inputs enabled\n");
  }
  else if (struckmode == 1) {
    sctrl = 0x4;
    logMsg("INFO Struck Scaler(s): Inp 1 disables all channels\n");
  }
  else if (struckmode == 2) {
    sctrl = 0x8;
    logMsg("INFO Struck Scaler(s): Inp 1 disables ch 1-16, Inp 3 disables ch 17-32\n");
  }
  else {
    sctrl = 0x0;
    logMsg("WARN STRUCK SCALER CONTROL INPUTS ARE UNDEFINED!!!\n");
  }
#endif

  if (num_sscalers + num_hscalers + num_escalers > 0) {
    logMsg("INFO Configuring %d Struck scalers at base address %x\n",
	     (num_sscalers+num_hscalers+num_escalers),base_sscal_addr);
    for(jj=0; jj<(num_sscalers+num_hscalers+num_escalers); jj++) {
      scal_addr = (base_sscal_addr + (jj*sscaler_address_increment));
      ssptrs[jj].model = MODEL_STRUCK_3801;
      ssptrs[jj].csr = (unsigned long *) scal_addr;
      ssptrs[jj].irq = (unsigned long *) (scal_addr + 0x4);
      ssptrs[jj].cdr = (unsigned long *) (scal_addr + 0xc);
      ssptrs[jj].wfifo = (unsigned long *) (scal_addr + 0x10);
      ssptrs[jj].clear = (unsigned long *) (scal_addr + 0x20);
      ssptrs[jj].next = (unsigned long *) (scal_addr + 0x24);
      ssptrs[jj].enable = (unsigned long *) (scal_addr + 0x28);
      ssptrs[jj].disable = (unsigned long *) (scal_addr + 0x2c);
      ssptrs[jj].reset = (unsigned long *) (scal_addr + 0x60);
      ssptrs[jj].fifo = (unsigned long *) (scal_addr + 0x100);
      ssptrs[jj].version =  (*(ssptrs[jj].irq) >> 12) & 0xf;
      /* Do all this at prestart time */
#if 0
      *(ssptrs[jj].reset) = 1;  /* reset module */
      *(ssptrs[jj].csr) = 0x40000; /* Enable external disable */
#endif
      logMsg("INFO Struck 3801 at %x, version %d\n",ssptrs[jj].csr
	       ,ssptrs[jj].version);
    }
  }

  scalerBufferSize = 16*num_scalers + 32*(num_sscalers+num_hscalers+num_escalers);
  /* Last two channels for clock information */
  /* If there are exactly 3 helicity scalers, assume they are arranged
     +, -, both and that we will break out the both into + and - when we
     accumulate */
  if(num_hscalers == 3) scalerBufferSize += 32;
  /*  escaler_index = scalerBufferSize - 32;*/
  if(use_ts_scalers) {
    res = (unsigned long) sysBusToLocalAdrs(0x39, (char *)TS_ADDR, (char **) &lladdr);
    if (res != 0) {
      printf("Error obtaining Trigger Supervisor Address%d \n",res);
      use_ts_scalers=0;
    } else {
      ts = (struct vme_ts *)lladdr;
      ts->scalAssign = TS_SCALER_PROGRAM;
      scalerBufferSize += TS_SCALER_COUNT;
    }
  }

  event_channel = scalerBufferSize++;
  clock_channel = scalerBufferSize++;
  logMsg("event_channel=%d\n",event_channel);
  logMsg("clock_channel=%d\n",clock_channel);

  scalerBuffer = (unsigned int *) malloc(scalerBufferSize*sizeof(int));
  scalerOverflows = (unsigned int *) malloc(scalerBufferSize*sizeof(int));
  eorBuffer = (unsigned int *) malloc(scalerBufferSize*sizeof(int));
  eorOverflows = (unsigned int *) malloc(scalerBufferSize*sizeof(int));
  scalerTemp = (unsigned int *) malloc(num_hscalers*32*sizeof(int));
  scalerTempBuffer = (unsigned int *) malloc((num_hscalers+1)*32*sizeof(int));
  scalerTempOverflows = (unsigned int *) malloc((num_hscalers+1)*32*sizeof(int));
  for(i=0;i<scalerBufferSize;i++) {
    scalerBuffer[i] = 0;
    scalerOverflows[i] = 0;
    eorBuffer[i] = 0;
    eorOverflows[i] = 0;
  }

  printf("Scaler buffer \"scalerBuffer\" of %d words alocated and initialized at 0x%x\n",scalerBufferSize,scalerBuffer);
  printf("Scaler overflow buffer \"scalerOverflows\" of %d words alocated and initialized at 0x%x\n",scalerBufferSize,scalerOverflows);
  scalerRunInProgress=0;        /* If !=0, then CODA is running */


  /* Check if the scaler server is loaded, and if so, start up both the
     scaler server and the between run's readout */
  symstat = symFindByName(sysSymTbl,"scaSrvr",(char **)&pscaSrvr,&symtype);
  if(symstat == 0) {
    
    /* Start the scaler server */
    logMsg("INFO Starting scaler server\n");
    taskSpawn("scSrvr",140,VX_FP_TASK,20000,(FUNCPTR) *pscaSrvr,0,0,0,0,0,0,0,0,0,0);

    logMsg("INFO Scaler server spawned\n");

    s = getenv("SCALER_PERIOD");

    scaler_readout_period = atol(s);
    if(scaler_readout_period <= 0) {
      scaler_readout_period = 2;
    }


    if (scaler_readout_period > 0) { 
      logMsg("INFO In between runs, read out scalers every %d seconds\n",
	       scaler_readout_period);
    }
    clear_scalers(0);
    enable_scalers();
#ifndef READWHENASKED
    set_runstatus(0);
#endif
  }
  logMsg("Exiting setup_scalers\n");
}

void enable_scalers()
{
  int jj;

  if (num_sscalers + num_hscalers + num_escalers != 0) {
    for(jj=0;jj<(num_sscalers+num_hscalers+num_escalers);jj++) {
      if(jj == num_sscalers+num_hscalers) {
	if(num_escaler_chans < 32) {
	  *(ssptrs[jj].cdr) = (1<<num_escaler_chans);
	}
      }
      *(ssptrs[jj].clear) = 1 ;     /* Clear FIFO */
      *(ssptrs[jj].csr) = 0x40408;	/* Set MODE 2 for so input 4 is
					   disable counting */
      if(jj>=num_sscalers) /* Enable external LNE for */
	*(ssptrs[jj].csr) = 0x10000;	/* helicity scalers and event scaler */
      *(ssptrs[jj].enable) = 1 ;     /* Enable Next clock logic */
      /* Apparantly we need to send a LNE before scalers will count */
      *(ssptrs[jj].next) = 1;	/* Start counting */
    }
  }
}

void enable_helicity_scalers(int enable)
{
  int jj;

  if ((num_hscalers) != 0) {
    for(jj=num_sscalers;jj<(num_sscalers+num_hscalers);jj++) {
      if(enable==0) *(ssptrs[jj].disable) = 1 ;
      else *(ssptrs[jj].enable) = 1 ;
    }
  }
  /* scalerRunInProgress = 0; ??? */
}

void disable_scalers()
{
  int jj;

  if ((num_sscalers+num_hscalers+num_escalers) != 0) {
    for(jj=0;jj<(num_sscalers+num_hscalers+num_escalers);jj++) {
      *(ssptrs[jj].disable) = 1 ; 
    }
  }
  /* scalerRunInProgress = 0; ??? */
}

void clear_scalers(int coda)
{
  int jj,i;
  /* Don't let scaler server clear scalers if run is going */
  if(!coda && scalerRunInProgress) return;

/* Reset and Check if scalers are present */
  if(num_scalers != 0) { /* Do nothing if nothing defined */
    logMsg("INFO Reseting %d Scalers",num_scalers);
    for(jj=0; jj<num_scalers ; jj++) {
      *(sptrs[jj].reset) = 1;
      /* Clear the corresponding entries in the scaler buffer */
      for(i=0;i<16;i++) {
	  eorBuffer[jj*16 + i] = scalerBuffer[jj*16 + i];
	  scalerBuffer[jj*16 + i] = 0;
	  eorOverflows[jj*16 + i] = scalerOverflows[jj*16 + i];
	  scalerOverflows[jj*16 + i] = 0;
      }
    }
    eor_scaler_event_counter = scaler_event_counter;
  }
  if((num_sscalers+num_hscalers+num_escalers) != 0) { /* Do nothing if nothing defined */
    logMsg("INFO Clear %d Struck scalers\n",(num_sscalers+num_hscalers+num_escalers));
    for(jj=0; jj<(num_sscalers+num_hscalers+num_escalers) ; jj++) {
      /*      logMsg("Clearing scaler %d\n",jj);*/
      /* *(ssptrs[jj].reset) = 1;
	 /* *(ssptrs[jj].csr) = 0x40000; /* Enable external disable */
      *(ssptrs[jj].clear) = 1; /* Clear the FIFO */
      /* Clear the corresponding entries in the scaler buffer */
      *(ssptrs[jj].next) = 1; /* Load the scalers */
      while(!(*ssptrs[jj].csr & 0x100)) { /* Empty the Fifo */
	/* Should put in a counter to make sure we don't get stuck here */
	int dummy;
	dummy = *ssptrs[jj].fifo;
      }
      for(i=0;i<32;i++) {
	  scalerBuffer[num_scalers*16 + jj*32 + i] = 0;
	  scalerOverflows[num_scalers*16+jj*32 + i] = 0;
      }
    }
  }
  /* Clear out the extra buffer when we split the "both" into + and - */
  if(num_hscalers==3) {
    for(i=0; i<32; i++) {
      scalerBuffer[num_scalers*16 + (num_sscalers+num_hscalers+num_escalers)*32 + i] = 0;
      scalerOverflows[num_scalers*16 + (num_sscalers+num_hscalers+num_escalers)*32 + i] = 0;
    }
  }
  if(use_ts_scalers) {
    int ioff=num_scalers*16+num_sscalers*32+(num_hscalers+HELICITYSCALERSINSERVER*(num_hscalers==3?1:0))*32+num_escaler_chans;
    ts->scalControl=0xfffff; /* Reset scalers */
    ts->scalAssign=TS_SCALER_PROGRAM;
    for(i=0;i<TS_SCALER_COUNT;i++){
      scalerBuffer[ioff+i]=0;
      scalerOverflows[ioff+i]=0;
    }
  }
  scaler_event_counter = 0;
}

/* Return the number of channels of scalers modules */
int get_num_scalers()
{
#if 0
#ifdef HELICITYSCALERSINSERVER
  return(num_scalers*16+num_sscalers*32+(num_hscalers+(num_hscalers==3?1:0))*32+num_escaler_chans+use_ts_scalers*TS_SCALER_COUNT);
#else
  return(num_scalers*16+num_sscalers*32+num_escaler_chans+use_ts_scalers*TS_SCALER_COUNT);
#endif
#else
  return(scalerBufferSize);
#endif
}

void set_runstatus(int status)
{
  scalerRunInProgress = status;
  if(status) {			/* CODA run in progress */
    logMsg("INFO Scalers going to CODA mode.  Version %s @%d",VERSION,tickGet());
    kill_scaler_task();
  } else {
    logMsg("INFO Scalers going to between run mode.  Version %s @%d",VERSION,tickGet());
    start_scaler_task();
  }
}

int get_runstatus()
{
  return(scalerRunInProgress);
}


/* Copy scalers into array created by scalers server.  Assume that the
   scaler server made first and count sensible values */
struct VALUE {
	int val;
	int ovf;
};
typedef struct VALUE VALUE;

int scalers_copy(VALUE *values, int first, int count)
     /* Not used right now */
{
  int i;

#ifdef READWHENASKED
  if(!scalerRunInProgress) {
    read_scalers(0);
#ifdef HELICITYSCALERSINSERVER
#ifdef HELICITYSCALERSBETWEENRUNS
    if(num_hscalers > 0) {
      enable_helicity_scalers(0);
      read_helicity_scalers(0);
      enable_helicity_scalers(1);
    }
#endif
#endif
  }
#endif
  for(i=0;i<count;i++) {
    values[i].val = scalerBuffer[first+i];
    values[i].ovf = scalerOverflows[first+i];
    /*    printf("%d: %d %d\n", first+i, values[i].val, values[i].ovf);*/
  }
  return(scaler_event_counter);
}

int eor_scalers_copy(VALUE *values, int first, int count)
{
  int i;

  for(i=0;i<count;i++) {
    values[i].val = eorBuffer[first+i];
    values[i].ovf = eorOverflows[first+i];
    /*    printf("%d: %d %d\n", first+i, values[i].val, values[i].ovf);*/
  }
  return(eor_scaler_event_counter);
}

void scalerReadoutTask()
{
  scaler_readout_killer_flag = 0;
  scaler_event_counter = 0;
  taskDelay(120);

  while(1) {
    if(scaler_readout_killer_flag) {
      scaler_tid = 0;
      exit(1);
    }

    if(!scalerRunInProgress) {
      /*            logMsg("Read scalers @%d\n",tickGet());*/
      read_scalers(0);
#ifdef HELICITYSCALERSINSERVER
#ifdef HELICITYSCALERSBETWEENRUNS
      if(num_hscalers > 0) {
#ifdef DISABLEHELICITYSCALERSWHILEREADINGBETWEENRUNS
      enable_helicity_scalers(0);
#endif
      read_helicity_scalers(0);
#ifdef DISABLEHELICITYSCALERSWHILEREADINGBETWEENRUNS
      enable_helicity_scalers(1);
#endif
      }
#endif
#endif
    }
    /*    if(scalerRunInProgress) {*/
      taskDelay(scaler_readout_period*60);
      /*    }*/
}
}

void kill_scaler_task()
{
  scaler_readout_killer_flag = 1;
}

void start_scaler_task()
{
  if((num_scalers+num_sscalers) <=0) return; /* Do nothing if nothing defined */
  /* Make sure that if there is a scaler task already running, it gets killed */
  if(scaler_tid) {
    kill_scaler_task(); /* Ask existing task to die */
    taskDelay(scaler_readout_period*60);
    if(scaler_tid) taskDelete(scaler_tid); /* Kill it if it won't die */
  }

  scaler_tid = taskSpawn("scalerReadoutTask",115,VX_FP_TASK,20000
	    ,(FUNCPTR) scalerReadoutTask,0,0,0,0,0,0,0,0,0,0); 
}

