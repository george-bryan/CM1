  MODULE maxmin_module

  CONTAINS

      subroutine maxmin(izz,jzz,kzz,f,nstat,rstat,kmin,kmax,amax,amin)
      use input
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

      end subroutine maxmin


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine maxmin2d(izz,jzz,f,nstat,rstat,amax,amin)
      use input
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

  END MODULE maxmin_module
