## makefile 

#FDEBUG = -g
#FFLAGS = $(FDEBUG)  -ffixed-line-length-0
#FC = g77
#FLAGS = -ffixed-line-length-0
FC = pgf77
FLAGS = -Mextend -O3
#FLAGS = -Mextend 

#BIN = /data/isis/dna/software/x
BIN = /Users/rmartinshort/Documents/Berkeley/Alaska/FORTRAN/GroupCodes/Shoot/GroupSoftwareTest/bin

BFOR = shootray_evt.f azdel.f bisec.f bpt.f coordl.f \
	decomq.f derivs.f derivs_pq.f dfcns.f dfint.f dgauleg.f \
	dintw_kd.f dintw_kntbl.f driver_rk4.f driver_pq_rk4.f \
	euler_rot.f euler_subs.f eval_kntstr.f fftsubs.f findr.f \
	getfid.f getvmdl.f getv_cmdl.f getv_smdl.f \
	hessian.f intrprayp.f locates.f \
	pkv.f prjlag.f projectv.f projqn.f \
	ray2xyz.f raylag.f rayptbl.f rfft.f rotate.f rtp2xyz.f \
	setvmdl.f shootout.f signature.f \
	tracer.f tracer_pq.f \
	velodisc.f velomodel.f xyz2rtp.f
#BOBJ = $(BFOR:.f.o)
shootray_evt: $(BFOR)
	$(FC) $(FLAGS) $(BFOR) -o $(BIN)/shootray_evt 
#BOBJ: $(BFOR)

#BOBJS = azdel.o bisec.o bpt.o coordl.o \
	decomq.o derivs.o derivs_pq.o dfcns.o dfint.o dgauleg.o \
	dintw_kd.o dintw_kntbl.o driver_rk4.o driver_pq_rk4.o \
	euler_rot.o euler_subs.o eval_kntstr.o fftsubs.o findr.o \
	getfid.o getvmdl.o getv_cmdl.o getv_smdl.o \
	hessian.o intrprayp.o locates.o \
	pkv.o prjlag.o projectv.o projqn.o \
	ray2xyz.o raylag.o rayptbl.o rfft.o rotate.o rtp2xyz.o \
	setvmdl.o shootout.o signature.o \
	tracer.o tracer_pq.o \
	velodisc.o velomodel.o xyz2rtp.o
#shootray_evt: shootray_evt.f $(BOBJS)
	#$(FC) shootray_evt.f $(FLAGS) -o $(BIN)/shootray_evt $(BOBJS) 
 
CFOR = buildG_tbt.f
buildG_tbt: $(CFOR)
	$(FC) $(CFOR) $(FLAGS) -o $(BIN)/buildG_tbt

DFOR = buildG_tbt_dif.f
buildG_tbt_dif: $(DFOR)
	$(FC) $(DFOR) $(FLAGS) -o $(BIN)/buildG_tbt_dif

*.f.o:
	$(FC) $(FLAGS) -c $*.f

#clean:
#	rm -f *.o






