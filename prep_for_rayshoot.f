      program sr2hungsr

c     code to convert "shoot.params" file from RA's crosscorr code 
c     to file for Shu-Huei's shootray_evt code

c     Richard Allen - june 2007

c 	shoot.params ...the input file, format:
c
c  888 format(i7,1x,f15.5,1x,a5,1x,a8,1x,f9.4,1x,f9.4,1x,f9.4,1x,f9.4,1x,
c               f9.4,1x,f9.4,1x,f8.3,1x,f7.2,1x,f7.2,1x,f8.5,1x,f8.5,1x,f8.5)
c
c  read(31,888,end=999) evday,evtime,phasej,stat1,slatd,slond,stelev,elatd,
c                               elond,deps,delta,rdum,iaz,kres,stdev,rav
c
c      INPUTS for each ray to be traced:
c      evday,                   event yearjdy for ID only
c      evtime,                  event epoch time for ID only
c      phasej,                  phase eg P
c      stat1,slatd,slond,stelev,        station name, lat, lon, elev
c      elatd,elond,deps,                event lat lon depth
c      delta,rdum,iaz,          epi dist (deg) <NOTUSED> ev to st azimuth
c      kres,stdev,rav           tt resids, standard deviations, x-corr coef
c
c RWP update 1.28.2013
c added parameter NSTA for the max number of actual stations
c added lists for locations for station tracking
c changed new station check to look for new station if the location
c changes. This allows for repeated station names and is part of a full
c scale location overhaul

c RMS edit 12/2015 for readability

      implicit none

      integer NRAY,NSTA
c      parameter (NRAY=25000,NSTA=1500)
c WBH need to add more rays!
      parameter (NRAY=39000,NSTA=1900)
      integer evday(NRAY)
      real slatd(NRAY),slond(NRAY),stelev(NRAY)
      real elatd(NRAY),elond(NRAY),deps(NRAY)
      real delta(NRAY),rdum(NRAY),iaz(NRAY)
      real kres(NRAY),stdev(NRAY),rav(NRAY)
      real slatdlist(NSTA),slondlist(NSTA),stelevlist(NSTA)
      double precision evtime(NRAY)
      character*8 phasej(NRAY),stat1(NRAY)

      integer i,ii,j,nrays,nphase,evstart,nevent,nphasetot,nstats
      integer ln,leng,ln2
      double precision currevtime
      character*8 phase,statlist(NSTA),freqid
      character*80 infile,outfile,datstafile
      logical filefound,newstat

c
ccc   READ shoot.params FILE INTO MEMORY
c
ccc   original input lines from shootsr.r
c88   format(i7,1x,f15.5,1x,a5,1x,a8,1x,f9.4,1x,f9.4,1x,f9.4,1x,f9.4,
c    & 1x,f9.4,1x,f9.4,1x,f8.3,1x,f7.2,1x,f7.2,1x,f8.5,1x,f8.5,1x,f8.5)
c     read(31,888,end=999) evday,evtime,phasej,stat1,slatd,slond,stelev,
c    &       elatd,elond,deps,delta,rdum,iaz,kres,stdev,rav

      inquire(file='shoot.params',exist=filefound)

      if (filefound) then
        infile='shoot.params'
        write (*,*) "Using shoot.params file"

      else
        write (*,*) "Enter name of shoot.params file"
        read (*,*) infile
      endif

      open(10, file=infile)
      i=1
10    read(10,*,end=999) evday(i),evtime(i),phasej(i),
     &             stat1(i),slatd(i),slond(i),stelev(i),
     &             elatd(i),elond(i),deps(i),delta(i),rdum(i),
     &             iaz(i),kres(i),stdev(i),rav(i)
      i=i+1
      goto 10
999   continue
      nrays=i-1
      close(10)

      write (*,*) "Number of lines read: ",nrays

ccc   which phase?

      write(*,*) "Enter the phase you want to extract"
      write(*,*) "(can only ray-trace one phase at a time)"
      read (*,*) phase

ccc   freq identifyer?

      write(*,*) "Enter the freq identifyer for this data eg b"
      read (*,*) freqid

ccc   name of output file

      ln=leng(phase)
      ln2=leng(freqid)
      outfile=phase(1:ln)//"."//freqid(1:ln2)//".datalist"
      open(11, file=outfile)

ccc   now scan through identifying events
      nphase=0
      nphasetot=0
      nevent=0
      nstats=0
      evstart=1
      currevtime=evtime(1)
      
      do i=1,nray

        if (abs(evtime(i)-currevtime) .lt. 0.001) then
          if (phase.eq.phasej(i)) nphase=nphase+1

        else if (abs(evtime(i)-currevtime) .gt. 0.001 .OR. i.eq.nray) then
c         new event OR last data => output last event
c         only output if there are some of the desired phase...
          if (nphase .gt. 0) then
            write(*,'(a6,i7,f18.5,a10,i5,a20,i5)') "event ",
     &          evday(evstart),evtime(evstart)," # phase: ",nphase,
     &          "  ev ended at line: ",i-1

            write (11,'(a2,i9,a1,f18.5)') "> ",evday(evstart)," ",evtime(evstart)
	          write (11,*) nphase

            do ii=evstart,i-1

              if (phase.eq.phasej(ii)) then
                write (11,14) stat1(ii),slatd(ii),slond(ii),stelev(ii),elatd(ii),elond(ii),deps(ii),kres(ii),stdev(ii),0.0

ccc             Also count stations: stations are identified by their name and their location. If all these match, then the station is recorded
                newstat=.TRUE.
                do j=1,nstats
                  if (stat1(ii) .eq. statlist(j)) then
                    if (slatd(ii) .eq. slatdlist(j)) then
                      if (slond(ii) .eq. slondlist(j)) then
                      newstat=.FALSE.
                      endif
                    endif
                  endif
                enddo
                
                if (newstat) then
                  nstats=nstats+1
                  statlist(nstats)=stat1(ii)
                  slatdlist(nstats)=slatd(ii)
                  slondlist(nstats)=slond(ii)
c                 stelevlist(nstats)=stelev(ii) In the past we checked station elevation too, but not really necessary
                endif
              endif
            enddo

c           keep total data count
            nevent=nevent+1
            nphasetot=nphasetot+nphase
          endif

c         begin next event
          nphase=0
          evstart=i
          currevtime=evtime(i)
          if (phase.eq.phasej(i)) nphase=1
        endif
      enddo

      ln=leng(phase)
      ln2=leng(freqid)
      datstafile=phase(1:ln)//"."//freqid(1:ln2)//".datsta"
      open(13,file=datstafile)
      write(13,*)nphasetot,nstats,nevent

      write(*,*)
      write(*,*)'Total number of events  : ',nevent
      write(*,*)'Total number of stations: ',nstats
      write(*,*)'Total number of data    : ',nphasetot

      write(*,*)
      write(*,*)'Output files:'
      write(*,*)outfile
      write(*,*)datstafile

      write(*,*)

14    format(a5,2(f11.6,1x),f9.4,1x,2(f9.4,1x),f6.1,f9.4,1x,f9.5,f10.3)

      end
