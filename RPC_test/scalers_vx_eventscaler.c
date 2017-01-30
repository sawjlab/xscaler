/*#define READWHENASKED 1*/
#define HELICITYSCALERSINSERVER 1
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
char *modelstrings[]={"Empty","Lecroy","CAEN","Struck","Struck_3801"};

#define CAEN_ID 0x0818
#define LECROY_ID 0x407

int HELICITY_DEBUG=0;

unsigned int *scalerBuffer;
unsigned int *scalerOverflows;
unsigned int *scalerTemp;
unsigned int *scalerTempBuffer;	/* Accumulate extra helicity events in FIFO */
unsigned int *scalerTempOverflows; /* and add into scalerBuffer when done */
int scalerRunInProgress=0;
int scalerBufferSize=0;
int scalerServerExists=0;
int scaler_event_counter=0;


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

static int num_scalers;
static int num_sscalers;
static int num_hscalers;
static int num_escalers;
static int num_escaler_chans;
static int escaler_index;
static int scaler_readout_period;
static void *base_scal_addr;
static void *base_sscal_addr;
void *lladdr;

long scaler_readout_killer_flag;

void start_scaler_task();
void clear_scalers(int coda);
void enable_scalers();
void kill_scaler_task();
void set_runstatus(int status);

int read_event_scalers(long **dabufpp)
     /* Return -2 if FIFO is FULL before read
	       -1 if FIFO is empty before read (an error)
	        0 if FIFO is empty after read
		1 if FIFO has data after read (OK if buffered and not sync */
{
  int escaler_index;
  int csr;
  int jj;

  dabufp = *dabufpp;
  if(num_escalers <= 0) return(-1);

  if(num_hscalers ==3) splitboth = 1;
  else splitboth=0;

  escaler_index = num_sscalers+num_hscalers;
  csr = *ssptrs[escaler_index].csr;
  if(csr & 0x1000) {
    logMsg("ERROR Event scaler FIFO full.  This is BAD\n");
    return(-2);
  } else if(csr & 0x100) {
    logMsg("ERROR Event scaler FIFO is empty.  This is BAD\n");
    return(-1);
  }
  for(jj=0;jj<num_escaler_chans;jj++) {
    int sbindex = escaler_index*32 + jj;
    scalerTemp[sbindex] = *ssptrs[escaler_index].fifo;
  }
  scal_header = (0x00000020)
    + ((32*(kk+(num_sscalers+num_hscalers+splitboth+1)) + SSCALER_CODA_OFFSET)<<16); /* Scalers start at 640 */
  *(dabufp)++ = scal_header;
  for(jj=0; jj < num_escaler_chans ; jj++) {
    int sbindex = num_scalers*16 + (escaler_index+splitboth)*32 + jj;
    unsigned int temp = scalerBuffer[sbindex];
    scalerBuffer[sbindex] += scalerTemp[jj];
    if(scalerBuffer[sbindex] < temp) scalerOverflows[sbindex]++;
    *(dabufp)++ = scalerBuffer[sbindex];
  }
  csr = *ssptrs[escaler_index].csr;
  *dabufpp = dabufp;

  if(csr & 0x100) return(0);	/* FIFO is empty */
  return(1);
}

void read_helicity_scalers(long **dabufpp)
     /* 0 In between run.  Read for scaler server only */
     /* ptr Coda running.  dabufp */
{
  int jj, kk , scal_header;
  int bad, badbit, empty, helicity, csr;
  long *dabufp;
  int nevents;
  int splitboth;

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
  for(kk=0; kk<num_hscalers; kk++) {
    csr = *ssptrs[num_sscalers+kk].csr;
    if(csr & 0x1000) {
      logMsg("ERROR 3801 scaler #%d FIFO Full\n",num_sscalers+kk);
      bad += badbit; /* To indicate that scalers will need to be reset */
    }
    badbit << 1;
  }
  for(kk=0; kk<num_hscalers; kk++) {
    csr = *ssptrs[num_sscalers+kk].csr;
    if(csr & 0x100) {
      logMsg("ERROR 3801 scaler #%d has no data\n",num_sscalers+kk);
      bad += badbit; /* To indicate that scalers will need to be reset */
    }
    badbit << 1;
  }

  if(bad) {			/* Reset FIFO's, return */
    if(bad >= 0x10) {
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
  

  do {

    /* Read one Latch's worth of scalers */
    bad = 0;
    helicity = 0;
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
	scalerTemp[sbindex] = *ssptrs[num_sscalers+kk].fifo;

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
	  if(kk==0 && scalervalue != 0) helicity += 1;
	  else if (kk==1 && scalervalue != 0) helicity += 2;
	}
      }
    }
    /* Add the counts into scalerTempBuffer */
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
	  if(helicity<=0) {
	    logMsg("ERROR No counts in helicity clock scalers\n");
	  } else if (helicity>=3) {
	    logMsg("ERROR No counts in helicity clock scalers\n");
	  } else if (helicity==1) { /* Positive helicity */
	    unsigned int temp = scalerTempBuffer[sbindex];
	    scalerTempBuffer[sbindex] += scalervalue;
	    if(scalerTempBuffer[sbindex] < temp) scalerTempOverflows[sbindex]++;
	  } else {
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
      }
    }
  } while (!empty && !bad);

  if(nevents > 1 && dabufp) {
    logMsg("INFO %d events not triggered\n",nevents-1);  
  }
    

  /* Now put the data into the event */
  if(empty == num_hscalers && !bad) {
    for(kk=0; kk < (num_hscalers+splitboth); kk++) {
      scal_header = (0x00000020) + ((32*(kk+num_sscalers) + SSCALER_CODA_OFFSET)<<16); /* Scalers start at 640 */
      if(dabufp) *(dabufp)++ =  scal_header;

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
	+ ((32*(kk+num_sscalers) + SSCALER_CODA_OFFSET)<<16)
	+ ((32*(num_hscalers+splitboth)) << 16);

      if(dabufp) *(dabufp)++ =  scal_header;
      for(jj=0; jj<32 ; jj++) {
	if(dabufp) *(dabufp)++ = scalerTemp[32*kk+jj];
      }
      /* Make sure the FIFO's are empty */
      while(!((*ssptrs[num_sscalers+kk].csr) & 0x100)) {
	unsigned int dummy;
	dummy = *ssptrs[num_sscalers+kk].fifo;
      }
    }
  } else {
    bad <<= 24;
    if(empty != num_hscalers) { /* All OK, do what needs to be done */
      bad += (empty << 16);
      logMsg("ERROR Not all helicity scalers had same amount of data\n");
    }
  }

EXIT:
  if(dabufp) {
    *(dabufp)++ = (0x00000002) 
      + ((32*(num_sscalers + 2*num_hscalers + splitboth)
	  + SSCALER_CODA_OFFSET)<<16);
    *(dabufp)++ = nevents;
    *(dabufp)++ = bad;
    *dabufpp = dabufp;
  }
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
	((32*kk + SSCALER_CODA_OFFSET)<<16); /* Scalers start at 640 */
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
	  logMsg("ERROR 3801 FIFO at %x not empty at end of scaler read.  This is BAD\n",
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

  if(dabufpp) {
    *dabufpp = dabufp;
    /*    printf("Pointer now at 0x%x\n",dabufp);*/
  }


  if(num_scalers !=0 || num_sscalers !=0) 
    scaler_event_counter++;
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
  void *scal_addr;
  char *s;
  void (*pscaSrvr)();
  SYM_TYPE symtype;
  STATUS symstat;
  int i;

  s = getenv("SCALER_MODULE_COUNT");
  if(s) num_scalers = atol(s);
  else num_scalers = 0;

  if(num_scalers > 0) {
    s = getenv("SCALER_ADDRESS_BASE");
    if(s) sscanf(s," %x",(unsigned int *)&S_ADDR);
    else S_ADDR = 0;
    if(!S_ADDR) {
      daLogMsg("ERROR","Invalid scaler address");
    }
  }

  s = getenv("SSCALER_MODULE_COUNT");
  if(s) num_sscalers = atol(s);
  else num_sscalers = 0;
  
  if(num_sscalers > 0) {
    s = getenv("SSCALER_ADDRESS_BASE");
    if(s) sscanf(s," %x",(unsigned int *)&SS_ADDR);
    else SS_ADDR = 0;
    if(!SS_ADDR) {
      daLogMsg("ERROR","Invalid Struck scaler address");
    } else {
      daLogMsg("INFO","SIS3801 base address=0x%x\n",SS_ADDR);
    }
  }

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

  daLogMsg("INFO","%d channels of event scaler defined\n",num_escalers);

  if(num_scalers + num_sscalers + num_hscalers + num_escalers <= 0) {
    daLogMsg("WARN","No scalers defined\n");
  }

  if (num_scalers > 0) {
    res = (unsigned long) sysBusToLocalAdrs(0x39,S_ADDR,(char **) &lladdr);
    if (res != 0) {
      printf("Error in sysBusToLocalAdrs res=%d \n",res);
    }
    else base_scal_addr = lladdr;
  }

  if ((num_sscalers+num_hscalers+num_escalers) > 0) {
#if 0
    res = (unsigned long) sysBusToLocalAdrs(0x39,SS_ADDR,(char **) &lladdr);
    if (res != 0) {
      printf("Error in sysBusToLocalAdrs res=%d \n",res);
    }
    else base_sscal_addr = lladdr;
#else
    base_sscal_addr = SS_ADDR;
#endif
  }

  if (num_scalers > 0)
    daLogMsg("INFO","Configuring %d LeCroy and CAEN scalers at base address %x \n",
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
    daLogMsg("INFO","Struck Scaler(s): No control inputs enabled\n");
  }
  else if (struckmode == 1) {
    sctrl = 0x4;
    daLogMsg("INFO","Struck Scaler(s): Inp 1 disables all channels\n");
  }
  else if (struckmode == 2) {
    sctrl = 0x8;
    daLogMsg("INFO","Struck Scaler(s): Inp 1 disables ch 1-16, Inp 3 disables ch 17-32\n");
  }
  else {
    sctrl = 0x0;
    daLogMsg("WARNING","STRUCK SCALER CONTROL INPUTS ARE UNDEFINED!!!\n");
  }
#endif

  if (num_sscalers + num_hscalers + num_escalers > 0) {
    daLogMsg("INFO","Configuring %d Struck scalers at base address %x \n",
	     (num_sscalers+num_hscalers),base_sscal_addr);
    for(jj=0; jj<(num_sscalers+num_hscalers); jj++) {
      scal_addr = (base_sscal_addr + (jj*0x1000));
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
      daLogMsg("INFO","Struck 3801 at %x, version %d\n",ssptrs[jj].csr
	       ,ssptrs[jj].version);
    }
  }

  scalerBufferSize = 16*num_scalers + 32*(num_sscalers+num_hscalers+num_escalers);
  /* If there are exactly 3 helicity scalers, assume they are arranged
     +, -, both and that we will break out the both into + and - when we
     accumulate */
  if(num_hscalers == 3) scalerBufferSize += 32;
  escaler_index = scalerBufferSize - 32;

  scalerBuffer = (unsigned int *) malloc(scalerBufferSize*sizeof(int));
  scalerOverflows = (unsigned int *) malloc(scalerBufferSize*sizeof(int));
  scalerTemp = (unsigned int *) malloc(num_hscalers*32*sizeof(int));
  scalerTempBuffer = (unsigned int *) malloc((num_hscalers+1)*32*sizeof(int));
  scalerTempOverflows = (unsigned int *) malloc((num_hscalers+1)*32*sizeof(int));
  for(i=0;i<scalerBufferSize;i++) {
    scalerBuffer[i] = 0;
    scalerOverflows[i] = 0;
  }

  printf("Scaler buffer \"scalerBuffer\" of %d words alocated and initialized at 0x%x\n",scalerBufferSize,scalerBuffer);
  printf("Scaler overflow buffer \"scalerOverflows\" of %d words alocated and initialized at 0x%x\n",scalerBufferSize,scalerOverflows);
  scalerRunInProgress=0;        /* If !=0, then CODA is running */


  /* Check if the scaler server is loaded, and if so, start up both the
     scaler server and the between run's readout */
  symstat = symFindByName(sysSymTbl,"scaSrvr",(char **)&pscaSrvr,&symtype);
  if(symstat == 0) {
    
    /* Start the scaler server */
    daLogMsg("INFO","Starting scaler server");

    taskSpawn("scSrvr",140,VX_FP_TASK,20000,(FUNCPTR) *pscaSrvr,0,0,0,0,0,0,0,0,0,0);

    s = getenv("SCALER_PERIOD");

    scaler_readout_period = atol(s);
    if(scaler_readout_period <= 0) {
      scaler_readout_period = 2;
    }


    if (scaler_readout_period > 0) { 
      daLogMsg("INFO","In between runs, read out scalers every %d seconds\n",
	       scaler_readout_period);
    }

    clear_scalers(0);
    enable_scalers();
#ifndef READWHENASKED
    set_runstatus(0);
#endif
  }
}

void enable_scalers()
{
  int jj;

  if (num_sscalers + num_hscalers + num_escalers != 0) {
    for(jj=0;jj<(num_sscalers+num_hscalers+num_escalers);jj++) {
      *(ssptrs[jj].clear) = 1 ;     /* Clear FIFO */
      *(ssptrs[jj].csr) = 0x408;	/* Set MODE 2 for so input 4 is
					   disable counting */
      if(jj>=num_sscalers) /* Enable external LNE for */
	*(ssptrs[jj].csr) = 0x10000;	/* helicity scalers and event scaler */
      *(ssptrs[jj].enable) = 1 ;     /* Enable Next clock logic */
      /* Apparantly we need to send a LNE before scalers will count */
      *(ssptrs[jj].next) = 1;	/* Start counting */
    }
  }
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
    daLogMsg("INFO","Reseting %d Scalers\n",num_scalers);
    for(jj=0; jj<num_scalers ; jj++) {
      *(sptrs[jj].reset) = 1;
      /* Clear the corresponding entries in the scaler buffer */
      for(i=0;i<16;i++) {
	  scalerBuffer[jj*16 + i] = 0;
	  scalerOverflows[jj*16 + i] = 0;
      }
    }
  }
  if((num_sscalers+num_hscalers+num_escalers) != 0) { /* Do nothing if nothing defined */
    daLogMsg("INFO","Clear %d Struck scalers\n",(num_sscalers+num_hscalers));
    for(jj=0; jj<(num_sscalers+num_hscalers+num_escalers) ; jj++) {
      *(ssptrs[jj].reset) = 1;
      *(ssptrs[jj].csr) = 0x40000; /* Enable external disable */
      *(ssptrs[jj].clear) = 1; /* Clear the FIFO */
      /* Clear the corresponding entries in the scaler buffer */
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
  scaler_event_counter = 0;
}

/* Return the number of channels of scalers modules */
int get_num_scalers()
{
#ifdef HELICITYSCALERSINSERVER
  return(num_scalers*16+num_sscalers*32+(num_hscalers+(num_hscalers==3?1:0))*32);
#else
  return(num_scalers*16+num_sscalers*32);
#endif
}

void set_runstatus(int status)
{
  scalerRunInProgress = status;
  if(status) {			/* CODA run in progress */
    kill_scaler_task();
  } else {
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
    if(num_hscalers > 0) read_helicity_scalers(0);
#endif
  }
#endif
  for(i=0;i<count;i++) {
    values[i].val = scalerBuffer[first+i];
    values[i].ovf = scalerOverflows[first+i];
  }
  return(scaler_event_counter);
}

void scalerReadoutTask()
{
  scaler_readout_killer_flag = 0;
  scaler_event_counter = 0;
  taskDelay(120);

  while(1) {
    if(scaler_readout_killer_flag) exit(1);

    if(!scalerRunInProgress) {
      read_scalers(0);
#ifdef HELICITYSCALERSINSERVER
      /*      if(num_hscalers > 0) read_helicity_scalers(0);*/
#endif
    }
    taskDelay(scaler_readout_period*60);
  }
}

void kill_scaler_task()
{
  scaler_readout_killer_flag = 1;
}

void start_scaler_task()
{
  if((num_scalers+num_sscalers) <=0) return; /* Do nothing if nothing defined */
  taskSpawn("scalerReadoutTask",115,VX_FP_TASK,20000
	    ,(FUNCPTR) scalerReadoutTask,0,0,0,0,0,0,0,0,0,0); 
}

