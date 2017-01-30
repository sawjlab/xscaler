/*-----------------------------------------------------------------------------
 *
 * Description:
 *	Hall C Scaler Server RPCGEN input file
 *	
 * Author:  Stephen A. Wood 04.05.2001
 *
 * Revision History:
 *   $Log:$
 *
 */

struct SLOTINFO {
  string scalertype<>;
  int nchans;
};
  
struct INFO {
  int slots;
  int channels;
  int runinprogress;
  int clock_channel;
  struct SLOTINFO slotinfo<>;
};

struct GETREQUEST {
  int first;
  int count;
};

struct VALUE {
  int val;
  int ovf;
};

struct VALUES {
  int first;
  int count;
  int read_counter;
  struct VALUE value<>;
};

program SCASERPROG {
  version SCASERVERS {
    int SCASER_CLEAR(void) = 1; /* Clear the scalers */
    INFO SCASER_GETINFO(void) = 2; /* Return information about scalers */
    VALUES SCASER_GETSCALERS(GETREQUEST) = 3; /* Return actual scaler values */
    VALUES SCASER_GETEORSCALERS(GETREQUEST) = 4; /* Return actual scaler values */
	} = 1;
} = 0x2c0da020;
