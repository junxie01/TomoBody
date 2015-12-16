program prep_rayshoot

  !     code to convert "shoot.params" file from RA's crosscorr code
  !     to file for Shu-Huei's shootray_evt code

  !     Richard Allen - june 2007
  !     RMS - Rewritten for f95 Dec 2015

  ! 	shoot.params ...the input file, format:
  !
  !  888 format(i7,1x,f15.5,1x,a5,1x,a8,1x,f9.4,1x,f9.4,1x,f9.4,1x,f9.4,1x,
  !               f9.4,1x,f9.4,1x,f8.3,1x,f7.2,1x,f7.2,1x,f8.5,1x,f8.5,1x,f8.5)
  !
  !  read(31,888,end=999) evday,evtime,phasej,statl,slatd,slond,stelev,elatd,
  !                               elond,deps,delta,rdum,iaz,kres,stdev,rav
  !
  !      INPUTS for each ray to be traced:
  !      evday,                   event yearjdy for ID only
  !      evtime,                  event epoch time for ID only
  !      phasej,                  phase eg P
  !      statl,slatd,slond,stelev,        station name, lat, lon, elev
  !      elatd,elond,deps,                event lat lon depth
  !      delta,rdum,iaz,          epi dist (deg) <NOTUSED> ev to st azimuth
  !      kres,stdev,rav           tt resids, standard deviations, x-corr coef
  !
  ! RWP update 1.28.2013
  ! added parameter NSTA for the max number of actual stations
  ! added lists for locations for station tracking
  ! changed new station check to look for new station if the location
  ! changes. This allows for repeated station names and is part of a full
  ! scale location overhaul

  ! Remember to copile with -ffree-line-length-none, otherwise the data won't be read in properly
  ! Compiles with gfortran

  implicit none

  integer, parameter :: NRAY=39000, NSTA=1900
  integer :: evday(NRAY), OpenStatus, linecount
  real :: slatd(NRAY),slond(NRAY),stelev(NRAY)
  real :: elatd(NRAY),elond(NRAY),deps(NRAY)
  real :: delta(NRAY),rdum(NRAY),iaz(NRAY)
  real :: kres(NRAY),stdev(NRAY),rav(NRAY)
  real :: slatdlist(NSTA),slondlist(NSTA),stelevlist(NSTA)
  double precision :: evtime(NRAY)
  character*8 phasej(NRAY),statl(NRAY)

  integer :: i,ii,j,nrays,nphase,evstart,nevent,nphasetot,nstats
  integer :: phaseln,freqln
  double precision currevtime
  character(Len=8) phase,statlist(NSTA),freqid
  character(Len=80) infile,outfile,datstafile

  logical filefound,newstat

  !
  !   READ shoot.params FILE INTO MEMORY
  !

  inquire(file='shoot.params',exist=filefound)

  if (filefound) then
    infile='shoot.params'
    print *, "Using shoot.params file"

  else
    print *, "Enter name of shoot.params file"
    read (*,*) infile
  endif

  ! Determine the number of lines in the file and read data into the arrays
  call linesinfile(infile,linecount)

  print *, linecount

  open (unit=99, file=infile,status='old',action='read',iostat=OpenStatus)

  if (OpenStatus > 0) STOP "*** Cannot open the file *** "

  do i =1, linecount
    read(99,*) evday(i),evtime(i),phasej(i),statl(i),slatd(i),slond(i),stelev(i),elatd(i),elond(i),deps(i),delta(i),rdum(i),iaz(i),kres(i),stdev(i),rav(i)
  end do

  close(99)

  nrays = linecount - 1

  print *, 'Number of lines read: ',nrays

  !    which phase
  print *, 'Enter the phase you want to extract'
  print *,  '(can only ray-trace one phase at a time)'
  read (*,*) phase

  !    freq identifyer?

  print *, 'Enter the freq identifyer for this data eg b'
  read (*,*) freqid

  !    name of output file - uses len_trim from f95

  phaseln=len_trim(phase)
  freqln=len_trim(freqid)

  outfile=phase(1:phaseln)//"."//freqid(1:freqln)//".datalist"
  open(11, file=outfile)

  !  now scan through identifying events
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
      !new event OR last data => output last event. Only output if there are some of the desired phase...

      if (nphase .gt. 0) then

        write(*,'(a6,i7,f18.5,a10,i5,a20,i5)') "event ",evday(evstart),evtime(evstart)," # phase: ",nphase,"  ev ended at line: ",i-1
        write (11,'(a2,i9,a1,f18.5)') "> ",evday(evstart)," ",evtime(evstart)
        write (11,*) nphase

        do ii=evstart,i-1

          if (phase.eq.phasej(ii)) then

            !Write this to the file

            write (11,14) statl(ii),slatd(ii),slond(ii),stelev(ii),elatd(ii),elond(ii),deps(ii),kres(ii),stdev(ii),0.0

            !Note the 14 here - tells us about the expected format of the file
            14    format(a5,2(f11.6,1x),f9.4,1x,2(f9.4,1x),f6.1,f9.4,1x,f9.5,f10.3)

            !Also count stations: stations are identified by their name and their location. If all these match, then the station is recorded

            newstat=.TRUE.
            do j=1,nstats
              if (statl(ii) .eq. statlist(j)) then
                if (slatd(ii) .eq. slatdlist(j)) then
                  if (slond(ii) .eq. slondlist(j)) then
                    newstat=.FALSE.
                  endif
                endif
              endif
            enddo

            if (newstat) then
              nstats=nstats+1
              statlist(nstats)=statl(ii)
              slatdlist(nstats)=slatd(ii)
              slondlist(nstats)=slond(ii)
              !                 stelevlist(nstats)=stelev(ii) In the past we checked station elevation too, but not really necessary
            endif
          endif
        enddo

        !            keep total data count
        nevent=nevent+1
        nphasetot=nphasetot+nphase
      endif

      !         begin next event
      nphase=0
      evstart=i
      currevtime=evtime(i)
      if (phase.eq.phasej(i)) nphase=1
    endif
  enddo

  datstafile=phase(1:phaseln)//"."//freqid(1:freqln)//".datsta"
  open(13,file=datstafile)
  write(13,*)nphasetot,nstats,nevent

  print *,
  print *, 'Total number of events  : ',nevent
  print *, 'Total number of stations: ',nstats
  print *, 'Total number of data    : ',nphasetot

  print *,
  print *, 'Output files:'
  print *, outfile
  print *, datstafile




end program prep_rayshoot

!----------------------------------------------------------

subroutine linesinfile(infile,linecount)
  implicit none

  integer :: linecount
  character(len=80):: infile

  linecount = 0

  print *, 'Counting number of lines in file', infile
  open (99,file=infile)
  do
    read(99,*,END=10)

    linecount = linecount + 1
  end do
  10 close(99)

  print *, 'Number of lines = ', linecount
end subroutine linesinfile
