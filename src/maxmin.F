  MODULE maxmin_module

      public :: maxmin2d
      public :: maxmin_orig,maxmin_fold,maxmin_host

  CONTAINS

      subroutine maxmin_orig(izz,jzz,kzz,f,nstat,rstat,kmin,kmax,amax,amin)
      use input, only : ni,nj,nk,ngxy,ngz,stat_out,myid,ierr,myi1,myj1, &
          timestats,time_stat,mytime
#ifdef MPI
      use mpi
#endif
      implicit none

      integer :: izz,jzz,kzz,nstat,kmin,kmax
      real, dimension(stat_out) :: rstat
      real, dimension(1-ngxy:izz+ngxy,1-ngxy:jzz+ngxy,1-ngz:kzz+ngz) :: f
      character(len=6) :: amax,amin

!-----------------------------------------------------------------------

      integer :: i,j,k
      integer :: imax,jmax,imin,jmin
      integer, dimension(nk+1) :: imaxt,jmaxt,kmaxt,imint,jmint,kmint
      real, dimension(nk+1) :: tmax,tmin
      real :: fmax,fmin,rmax,rmin
      real :: tmaxt, tmint
      integer :: loc
      integer :: foundmin,foundmax
      integer :: foo(3)
      real, dimension(2) :: mmax,nmax,mmin,nmin

!-----------------------------------------------------------------------

      imin = 1
      jmin = 1
      kmin = 1
      imax = 1
      jmax = 1
      kmax = 1
      fmax = -1.e30
      fmin =  1.e30

      !$acc parallel default(present) private(i,j,k) &
      !$acc   reduction(min:fmin) reduction(max:fmax)

      !$acc loop gang vector collapse(3) reduction(min:fmin) reduction(max:fmax)
      do k=1,kzz
      do j=1,jzz
      do i=1,izz
        fmax = max(f(i,j,k),fmax)
        fmin = min(f(i,j,k),fmin)
        if(f(i,j,k).eq.fmax)then
          imax=i
          jmax=j
          kmax=k
        endif
        if(f(i,j,k).eq.fmin)then
          imin=i
          jmin=j
          kmin=k
        endif
      enddo
      enddo
      enddo
      !$acc end parallel

#ifdef MPI
      mmax(1)=fmax
      mmax(2)=myid
      call MPI_ALLREDUCE(mmax,nmax,1,MPI_2REAL,MPI_MAXLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmax(2))
      imax=imax+myi1-1
      jmax=jmax+myj1-1
      call MPI_BCAST(imax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(kmax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)

      mmin(1)=fmin
      mmin(2)=myid
      call MPI_ALLREDUCE(mmin,nmin,1,MPI_2REAL,MPI_MINLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmin(2))
      imin=imin+myi1-1
      jmin=jmin+myj1-1
      call MPI_BCAST(imin,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmin,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(kmin,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)

      fmax=nmax(1)
      fmin=nmin(1)

    if(myid.eq.0)then
#endif
      write(6,100) amax,fmax,imax,jmax,kmax,    &
                   amin,fmin,imin,jmin,kmin
100   format(2x,'stat:: ',a6,':',1x,g13.6,i5,i5,i5,    &
             4x,a6,':',1x,g13.6,i5,i5,i5)

      nstat = nstat + 1
      rstat(nstat) = fmax
      nstat = nstat + 1
      rstat(nstat) = fmin
#ifdef MPI
    endif
#endif

      if(timestats.ge.1) time_stat=time_stat+mytime()

      end subroutine maxmin_orig


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine maxmin_fold(izz,jzz,kzz,f,nstat,rstat,kmin,kmax,amax,amin)
      use input, only : ni,nj,nk,ngxy,ngz,stat_out,myid,ierr,myi1,myj1, &
          timestats,time_stat,mytime
#ifdef MPI
      use mpi
#endif
      implicit none

      integer :: izz,jzz,kzz,nstat,kmin,kmax
      real, dimension(stat_out) :: rstat
      real, dimension(1-ngxy:izz+ngxy,1-ngxy:jzz+ngxy,1-ngz:kzz+ngz) :: f
      character(len=6) :: amax,amin

!-----------------------------------------------------------------------

      integer :: i,j,k,idx
      integer :: imax,jmax,imin,jmin
      integer, dimension(izz*jzz) :: imaxt,jmaxt,kmaxt,imint,jmint,kmint
      real, dimension(izz*jzz) :: tmax,tmin
      real :: fmax,fmin,rmax,rmin
      real :: tmaxt, tmint
      real :: val
      integer :: loc
      integer RANGE, PAIRS
      integer :: foundmin,foundmax
      integer :: foo(3)
      real, dimension(2) :: mmax,nmax,mmin,nmin

!-----------------------------------------------------------------------

!$acc data create(imaxt,jmaxt,kmaxt,imint,jmint,kmint,tmax,tmin)
      imin = 1
      jmin = 1
      kmin = 1
      imax = 1
      jmax = 1
      kmax = 1
      fmax = -1.e30
      fmin =  1.e30
      !$acc parallel loop gang vector collapse(2) default(present)
      do j=1,jzz
      do i=1,izz
         idx = (j-1)*izz+i
         tmax(idx)  = -1.e30
         tmin(idx)  = 1.e30
         imaxt(idx) = 0
         jmaxt(idx) = 0
         kmaxt(idx) = 0
         imint(idx) = 0
         jmint(idx) = 0
         kmint(idx) = 0
      enddo
      enddo


      !$acc parallel loop gang vector collapse(2) default(present) private(val)
      do j=1,jzz
      do i=1,izz
      !$acc loop seq private(idx)
      do k=1,kzz
        idx = (j-1)*izz+i
        val = f(i,j,k)
        if(val .gt. tmax(idx))then
          tmax(idx)  = val
          imaxt(idx) = i
          jmaxt(idx) = j
          kmaxt(idx) = k
        endif
        if(val .lt. tmin(idx))then
          tmin(idx)  = val
          imint(idx) = i
          jmint(idx) = j
          kmint(idx) = k
        endif
      enddo
      enddo
      enddo

!      !$acc parallel
!         print *,'maxmin_fold: tmin(1:3): ',tmin(1),tmin(2),tmin(3)
!      !$acc end parallel

      PAIRS=izz*jzz/2
      RANGE=(izz*jzz+1)/2
      do while ( RANGE > 1 )
!! Now fold the 2-d array with half the threads each time.
!$acc parallel loop gang vector default(present) private(i)
        do i=1,PAIRS
          if ( tmax(i) .le. tmax(RANGE)) then
             tmax(i)  = tmax(RANGE+i)
             imaxt(i) = imaxt(RANGE+i)
             jmaxt(i) = jmaxt(RANGE+i)
             kmaxt(i) = kmaxt(RANGE+i)
          endif
          if ( tmin(i) .ge. tmin(RANGE)) then
             tmin(i)  = tmin(RANGE+i)
             imint(i) = imint(RANGE+i)
             jmint(i) = jmint(RANGE+i)
             kmint(i) = kmint(RANGE+i)
          endif
        enddo
        PAIRS=RANGE/2
        RANGE=(RANGE+1)/2
      enddo
      !$acc update host(tmin,tmax,imaxt,jmaxt,kmaxt,imint,jmint,kmint)

      fmax=tmax(1)
      fmin=tmin(1)
      imax=imaxt(1)
      jmax=jmaxt(1)
      kmax=kmaxt(1)
      imin=imint(1)
      jmin=jmint(1)
      kmin=kmint(1)

#ifdef MPI
      mmax(1)=fmax
      mmax(2)=myid
      call MPI_ALLREDUCE(mmax,nmax,1,MPI_2REAL,MPI_MAXLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmax(2))
      imax=imax+myi1-1
      jmax=jmax+myj1-1
      call MPI_BCAST(imax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(kmax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)

      mmin(1)=fmin
      mmin(2)=myid
      call MPI_ALLREDUCE(mmin,nmin,1,MPI_2REAL,MPI_MINLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmin(2))
      imin=imin+myi1-1
      jmin=jmin+myj1-1
      call MPI_BCAST(imin,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmin,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(kmin,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)

      fmax=nmax(1)
      fmin=nmin(1)

    if(myid.eq.0)then
#endif
      print *,'maxmin_fold: MPI rank: ',nmin(2),nmax(2)
      write(6,100) amax,fmax,imax,jmax,kmax,    &
                   amin,fmin,imin,jmin,kmin
100   format(2x,'stat:: ',a6,':',1x,g13.6,i5,i5,i5,    &
             4x,a6,':',1x,g13.6,i5,i5,i5)

      nstat = nstat + 1
      rstat(nstat) = fmax
      nstat = nstat + 1
      rstat(nstat) = fmin
#ifdef MPI
    endif
#endif
!$acc end data

      if(timestats.ge.1) time_stat=time_stat+mytime()

      end subroutine maxmin_fold

      subroutine maxmin_host(izz,jzz,kzz,f,nstat,rstat,kmin,kmax,amax,amin)
      use input, only : ni,nj,nk,ngxy,ngz,stat_out,myid,ierr,myi1,myj1, &
          timestats,time_stat,mytime
#ifdef MPI
      use mpi
#endif
      implicit none

      integer :: izz,jzz,kzz,nstat,kmin,kmax
      real, dimension(stat_out) :: rstat
      real, dimension(1-ngxy:izz+ngxy,1-ngxy:jzz+ngxy,1-ngz:kzz+ngz) :: f
      character(len=6) :: amax,amin

!-----------------------------------------------------------------------

      integer :: i,j,k,idx
      integer :: imax,jmax,imin,jmin
      integer, dimension(izz*jzz) :: imaxt,jmaxt,kmaxt,imint,jmint,kmint
      real, dimension(izz*jzz) :: tmax,tmin
      real :: fmax,fmin,rmax,rmin
      real :: tmaxt, tmint
      real :: val
      integer :: loc
      integer RANGE, PAIRS
      integer :: foundmin,foundmax
      integer :: foo(3)
      real, dimension(2) :: mmax,nmax,mmin,nmin

!-----------------------------------------------------------------------
      !$acc update host(f)
      imin = 1
      jmin = 1
      kmin = 1
      imax = 1
      jmax = 1
      kmax = 1
      fmax = -1.e30
      fmin =  1.e30

      do k=1,kzz
      do j=1,jzz
      do i=1,izz
        fmax = max(f(i,j,k),fmax)
        fmin = min(f(i,j,k),fmin)
        if(f(i,j,k).eq.fmax)then
          imax=i
          jmax=j
          kmax=k
        endif
        if(f(i,j,k).eq.fmin)then
          imin=i
          jmin=j
          kmin=k
        endif
      enddo
      enddo
      enddo


#ifdef MPI
      mmax(1)=fmax
      mmax(2)=myid
      call MPI_ALLREDUCE(mmax,nmax,1,MPI_2REAL,MPI_MAXLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmax(2))
      imax=imax+myi1-1
      jmax=jmax+myj1-1
      call MPI_BCAST(imax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(kmax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)

      mmin(1)=fmin
      mmin(2)=myid
      call MPI_ALLREDUCE(mmin,nmin,1,MPI_2REAL,MPI_MINLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmin(2))
      imin=imin+myi1-1
      jmin=jmin+myj1-1
      call MPI_BCAST(imin,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmin,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(kmin,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)

      fmax=nmax(1)
      fmin=nmin(1)

    if(myid.eq.0)then
      print *,'maxmin_host: MPI rank: ',nmin(2),nmax(2)
#endif
      write(6,100) amax,fmax,imax,jmax,kmax,    &
                   amin,fmin,imin,jmin,kmin
100   format(2x,'stat:: ',a6,':',1x,g13.6,i5,i5,i5,    &
             4x,a6,':',1x,g13.6,i5,i5,i5)

      nstat = nstat + 1
      rstat(nstat) = fmax
      nstat = nstat + 1
      rstat(nstat) = fmin
#ifdef MPI
    endif
#endif

      if(timestats.ge.1) time_stat=time_stat+mytime()

      end subroutine maxmin_host


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine maxmin2d(izz,jzz,f,nstat,rstat,amax,amin)
      use input, only : ni,nj,nk,ngxy,ngz,stat_out,myid,ierr,myi1,myj1, &
          timestats,time_stat,mytime
#ifdef MPI
      use mpi
#endif
      implicit none
        
      integer :: izz,jzz,nstat
      real, dimension(stat_out) :: rstat
      real, dimension(1-ngxy:izz+ngxy,1-ngxy:jzz+ngxy) :: f
      character(len=6) :: amax,amin
        
!-----------------------------------------------------------------------
          
      integer :: i,j
      integer :: imax,jmax,imin,jmin
      integer, dimension(jzz) :: imaxt,jmaxt,imint,jmint
      real, dimension(jzz) :: tmax,tmin
      real :: fmax,fmin,rmax,rmin
      real :: tmaxt,tmint 
      integer :: loc
      real, dimension(2) :: mmax,nmax,mmin,nmin
      integer :: foundmax,foundmin
          
!-----------------------------------------------------------------------
      imin=1
      imax=1
      jmin=1
      jmax=1
      fmax = -1.e30
      fmin =  1.e30

      !$acc parallel default(present) private(i,j) &
      !$acc    reduction(min:fmin) reduction(max:fmax)
      !$acc loop gang vector collapse(2)
      do j=1,jzz
      do i=1,izz
        fmax = max(f(i,j),fmax)
        fmin = min(f(i,j),fmin)
        if(f(i,j).eq.fmax)then
          imax=i
          jmax=j
        endif
        if(f(i,j).eq.fmin)then
          imin=i
          jmin=j
        endif
      enddo
      enddo
      !$acc end parallel

#ifdef MPI
      mmax(1)=fmax
      mmax(2)=myid
      call MPI_ALLREDUCE(mmax,nmax,1,MPI_2REAL,MPI_MAXLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmax(2))
      imax=imax+(myi1-1)
      jmax=jmax+(myj1-1)
      call MPI_BCAST(imax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)

      mmin(1)=fmin
      mmin(2)=myid
      call MPI_ALLREDUCE(mmin,nmin,1,MPI_2REAL,MPI_MINLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmin(2))
      imin=imin+(myi1-1)
      jmin=jmin+(myj1-1)
      call MPI_BCAST(imin,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmin,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)

      fmax=nmax(1)
      fmin=nmin(1)

    if(myid.eq.0)then
#endif
      write(6,100) amax,fmax,imax,jmax,1,    &
                   amin,fmin,imin,jmin,1
100   format(2x,'stat:: ',a6,':',1x,g13.6,i5,i5,i5,    &
             4x,a6,':',1x,g13.6,i5,i5,i5)

      nstat = nstat + 1
      rstat(nstat) = fmax
      nstat = nstat + 1
      rstat(nstat) = fmin
#ifdef MPI
    endif
#endif

      if(timestats.ge.1) time_stat=time_stat+mytime()

      end subroutine maxmin2d

      subroutine maxmin2dhalo(izz,jzz,f,amax,amin)
      use input, only : ni,nj,nk,ngxy,ngz,stat_out,myid,ierr,myi1,myj1, &
          timestats,time_stat,mytime
#ifdef MPI
      use mpi
#endif
      implicit none

      integer :: izz,jzz
      real, dimension(1-ngxy:izz+ngxy,1-ngxy:jzz+ngxy) :: f
      character(len=6) :: amax,amin

!-----------------------------------------------------------------------

      integer :: i,j
      integer :: imax,jmax,imin,jmin
      integer, dimension(jzz) :: imaxt,jmaxt,imint,jmint
      real, dimension(jzz) :: tmax,tmin
      real :: fmax,fmin,rmax,rmin
      real :: tmaxt,tmint
      integer :: loc
      real, dimension(2) :: mmax,nmax,mmin,nmin
      integer :: foundmax,foundmin

!-----------------------------------------------------------------------
      imin=1
      imax=1
      jmin=1
      jmax=1
      fmax = -1.e30
      fmin =  1.e30

      !$acc parallel default(present) private(i,j) &
      !$acc    reduction(min:fmin) reduction(max:fmax)
      !$acc loop gang vector collapse(2)
      do j=1-ngxy,jzz+ngxy
      do i=1-ngxy,izz+ngxy
        fmax = max(f(i,j),fmax)
        fmin = min(f(i,j),fmin)
        if(f(i,j).eq.fmax)then
          imax=i
          jmax=j
        endif
        if(f(i,j).eq.fmin)then
          imin=i
          jmin=j
        endif
      enddo
      enddo
      !$acc end parallel

#ifdef MPI
      mmax(1)=fmax
      mmax(2)=myid
      call MPI_ALLREDUCE(mmax,nmax,1,MPI_2REAL,MPI_MAXLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmax(2))
      imax=imax+(myi1-1)
      jmax=jmax+(myj1-1)
      call MPI_BCAST(imax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)

      mmin(1)=fmin
      mmin(2)=myid
      call MPI_ALLREDUCE(mmin,nmin,1,MPI_2REAL,MPI_MINLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmin(2))
      imin=imin+(myi1-1)
      jmin=jmin+(myj1-1)
      call MPI_BCAST(imin,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmin,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)

      fmax=nmax(1)
      fmin=nmin(1)

    if(myid.eq.0)then
#endif
      write(6,100) amax,fmax,imax,jmax,1,    &
                   amin,fmin,imin,jmin,1
100   format(2x,'stat:: ',a6,':',1x,g13.6,i5,i5,i5,    &
             4x,a6,':',1x,g13.6,i5,i5,i5)

#ifdef MPI
    endif
#endif

      if(timestats.ge.1) time_stat=time_stat+mytime()

      end subroutine maxmin2dhalo


  END MODULE maxmin_module
