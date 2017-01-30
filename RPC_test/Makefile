#-----------------------------------------------------------------------------
#  Description:
# 	Makefile for Hall C Scaler Server system
# 	
#  Author:  Stephen Wood
# 
#  Revision History:
#    $Log:$

VXWORKSROOT=/site/vxworks

#Check Operating system we are using
ifndef OSNAME
  OSNAME := $(subst -,_,$(shell uname))
endif

ifndef ARCH
  ARCH = VXWORKSPPC55
endif
 
ifeq ($(OSNAME),Linux)
  LIBDIR = $(CODA)/$(ARCH)/lib

  ifeq ($(ARCH),VXWORKSPPC54)
    GCC_EXEC_PREFIX=$(VXWORKSROOT)/compilers/Linux/gnu-ppc/lib/gcc-lib/
    COMPILER_PATH=$(VXWORKSROOT)/compilers/Linux/gnu-ppc/powerpc-wrs-vxworks/bin
    INCDIR=$(VXWORKSROOT)/5.4/ppc/target/h
    CCVX = ccppc
    LDVX = ldppc
    VXDEFS = -mcpu=604 -DCPU=PPC604 -DVXWORKS -D_GNU_TOOL -DVXWORKSPPC
    VXINCS = -Wall -fno-for-scope -fno-builtin -fvolatile -fstrength-reduce \
	-mlongcall -I. -I$(INCDIR)
#    RPCCOMVX = $(VXWORKSROOT)/5.2/vw/unsupported/rpc4.0/rpcgen
    RPCCOMVX = rpcgen/rpcgen
    CCOPTS = -fPIC
  endif

  ifeq ($(ARCH),VXWORKSPPC55)
    GCC_EXEC_PREFIX=$(VXWORKSROOT)/5.5/ppc/host/x86-linux/lib/gcc-lib/
    COMPILER_PATH=$(VXWORKSROOT)/5.5/ppc/host/x86-linux/bin
    INCDIR=$(VXWORKSROOT)/5.5/ppc/target/h
    CCVX = ccppc
    LDVX = ldppc
    VXDEFS = -mcpu=604 -DCPU=PPC604 -DVXWORKS -D_GNU_TOOL -DVXWORKSPPC -mlongcall
    VXINCS = -Wall -fno-for-scope -fno-builtin -fvolatile -fstrength-reduce \
	 -I. -I$(INCDIR)
#    RPCCOMVX = $(VXWORKSROOT)/5.2/vw/unsupported/rpc4.0/rpcgen
    RPCCOMVX = rpcgen/rpcgen
  endif

  ifeq ($(ARCH),VXWORKS68K51)
    INCDIR=$(VXWORKSROOT)/5.3/68k/target/h
    CC = cc68k
    DEFS = -DCPU=MC68040 -DVXWORKS -DVXWORKS68K51
    INCS = -Wall -mc68020 -fvolatile -fstrength-reduce -nostdinc \
	-I. -I$(INCDIR)
  endif
 
  RPCCOM = /usr/bin/rpcgen 
  CFLAGS = -O $(DEFS)
 
endif

ifeq ($(OSNAME),SunOS)
  CC = cc
  CFLAGS = -O -KPIC
  RPCCOM = /usr/bin/rpcgen

  lib_targets = libscaser.a
  include_targets = scaser.h

  HOSTARCH = sun4-solaris2
  VXARCH = ppc
   
  export GCC_EXEC_PREFIX=$(VXWORKSROOT)/5.3/gnu/ppc/sun4-solaris2/lib/gcc-lib/
  VXROOT = $(VXWORKSROOT)/5.3/gnu/$(VXARCH)/$(HOSTARCH)
#	-B$(VXROOT)/lib/gcc-lib/ 
  CCVX     = $(VXROOT)/bin/cc$(VXARCH) \
	-nostdinc -ansi -pedantic -O2 -fstrength-reduce -Wall \
	-ansi -mcpu=604 --no-builtin -DvxWorks -DV5_vxWorks \
	-I$(VXWORKSROOT)/5.3/ppc/target/h -DCPU=PPC604 -D_GNU_TOOL

  LDVX   = $(VXROOT)/bin/ld$(VXARCH)
  RPCCOMVX = $(VXROOT)/bin/rpcgen
endif

ifeq ($(OSTYPE),SunOS)
  CC = cc
  CFLAGS = -O -KPIC
  RPCCOM = /usr/bin/rpcgen

  lib_targets = libscaser.a
  include_targets = scaser.h

  HOSTARCH = sun4-solaris2
  VXARCH = ppc
   
  export GCC_EXEC_PREFIX=$(VXWORKSROOT)/5.3/gnu/ppc/sun4-solaris2/lib/gcc-lib/
  VXROOT = $(VXWORKSROOT)/5.3/gnu/$(VXARCH)/$(HOSTARCH)
#	-B$(VXROOT)/lib/gcc-lib/ 
  CCVX     = $(VXROOT)/bin/cc$(VXARCH) \
	-nostdinc -ansi -pedantic -O2 -fstrength-reduce -Wall \
	-ansi -mcpu=604 --no-builtin -DvxWorks -DV5_vxWorks \
	-I$(VXWORKSROOT)/5.3/ppc/target/h -DCPU=PPC604 -D_GNU_TOOL

  LDVX   = $(VXROOT)/bin/ld$(VXARCH)
  RPCCOMVX = $(VXROOT)/bin/rpcgen
endif


#include $(sources:.c=.d)

GEN = scaserRpc_svc.c scaserRpc_clnt.c scaserRpc_xdr.c scaserRpc.h \
	scaserRpc_svc_vx.c scaserRpc_xdr_vx.c

TARGETS = libscaser.a scaSrvr getscalers

#all: $(TARGETS) scalersf1_vx.o scalers1877_vx.o scalersv767_vx.o scalersf1noerr_vx.o
all: $(TARGETS) scalers_vx.o

.PRECIOUS: $(GEN)

#%_vx.o: %_vx.c
#	export GCC_EXEC_PREFIX=$(GCC_EXEC_PREFIX);\
#	export COMPILER_PATH=$(COMPILER_PATH);\
#	$(CCVX) $(VXDEFS) $(VXINC) -c -o $@ $<

%_vx.o: %_vx.c
	export GCC_EXEC_PREFIX=$(GCC_EXEC_PREFIX);\
	export PATH=$(PATH):$(COMPILER_PATH);\
	$(CCVX) $(VXDEFS) $(VXINC) -c -o $@ $<

%.o: %.c
	gcc $(CCOPTS) -c -o $@ $<

#
#  RPC generated files: client and server stubs, header file, XDR file
#

scaserRpc_svc.c : scaserRpc.x
	$(RM) $@
	$(RPCCOM) -o $@ -m scaserRpc.x

scaserRpc_clnt.c : scaserRpc.x
	$(RM) $@
	$(RPCCOM) -o $@ -l scaserRpc.x

scaserRpc_xdr.c : scaserRpc.x
	$(RM) $@
	$(RPCCOM) -o $@ -c scaserRpc.x

scaserRpc.h : scaserRpc.x
	$(RM) $@
	$(RPCCOM) -o $@ -h scaserRpc.x

scaserRpc_vx.x: scaserRpc.x
	cp $< $@

scaserRpc_vx.h : scaserRpc_vx.x
	$(RM) $@
	$(RPCCOM) -o $@ -h $<

scaserRpc_svc_vx.c : scaserRpc_vx.x
	$(RM) $@
	$(RPCCOMVX) -o $@ -m $<

scaserRpc_xdr_vx.c : scaserRpc_vx.x
	$(RM) $@
	$(RPCCOMVX) -o $@ -c $<

#
#  Server executables for VxWorks
#

scaSrvr: scaser_vx.o scaserRpc_svc_vx.o scaserRpc_xdr_vx.o scaserRpcProc_vx.o
	export GCC_EXEC_PREFIX=$(GCC_EXEC_PREFIX);\
	export COMPILER_PATH=$(COMPILER_PATH);\
	export PATH=$(PATH):$(COMPILER_PATH);\
	$(LDVX) -o $@ -r $^

scaserRpc_xdr_vx.o: scaserRpc_xdr_vx.c scaserRpc_vx.h

scaserRpc_svc_vx.o: scaserRpc_svc_vx.c scaserRpc_vx.h

# z_vx.o: z_vx.c

scalersf1_vx.o: scalersf1_vx.c

scalers_newhelicitystuff_vx.o: scalers_newhelicitystuff_vx.c

rebootlog_vx.o: rebootlog_vx.c

#
#  Client library for Unix
#

libscaser.a : scaserRpc_clnt.o scaserRpc_xdr.o scaserCalls.o
	ar r $@ $^
	ranlib $@

libscaser.so: scaserRpc_clnt.o scaserRpc_xdr.o scaserCalls.o
	gcc -shared -o $@ $^

scaserCalls.c: scaserCalls.h

scaserRpc_clnt.o: scaserRpc_clnt.c scaserRpc.h

scaserRpc_xdr.o: scaserRpc_xdr.c scaserRpc.h

getscalers: getscalers.c libscaser.a
	$(CC) -o $@ $^ -lnsl

#
#  Error message header file used by everything
#

#ceMsgTbl.c sc_msg.h: sc.msg
#	makeMsgTbl

#facility: $(FACILITY)
#	grep 'Slow Controls' < $(FACILITY) > facility

#
#  Cleanup
#

clean cleanup:
	rm -f *.o $(GEN)
