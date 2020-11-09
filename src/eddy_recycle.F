  module eddy_recycle
  implicit none

  public

    logical :: do_recycle,do_recycle_w,do_recycle_s,do_recycle_e,do_recycle_n
    real :: recy_width,recy_depth,recy_source_w,recy_source_s
    integer :: irecywe,jrecywe,irecysn,jrecysn,krecy
    integer :: irc1,irc2,ircs1,ircs2,iris1,iris2,irb
    integer :: jrc1,jrc2,jrcs1,jrcs2,jris1,jris2,jrb
    real :: xloc_recy_w,xloc_recy_e,yloc_recy_s,yloc_recy_n

  CONTAINS

!-----------------------------------------------------------------------

    subroutine eddy_recycle_setup

      ! Use eddy recycling?

    do_recycle_w  =  .false.   ! near west boundary
    do_recycle_s  =  .false.   ! near south boundary


    do_recycle_e  =  .false.   ! do not change ... this has not been implemented yet
    do_recycle_n  =  .false.   ! do not change ... this has not been implemented yet


    !--------------------------------------
    ! width and depth (in meters) of recycled data:

    recy_width  =   400.0
    recy_depth  =  2000.0


    !--------------------------------------
    ! distance (in meters) from boundary where eddies come from
    ! (i.e., the source region of the recycled data)

    recy_source_w  =  2000.0
    recy_source_s  =  2000.0


    end subroutine eddy_recycle_setup

!-----------------------------------------------------------------------
!   Eddy recycler on west side of domain:

    subroutine do_eddy_recyw(dt,xh,xf,yh,yf,zh,zf,u3d,v3d,w3d,uten1,vten1,wten1,urecyw,vrecyw,wrecyw,trecyw,out3d)

    use input
    use constants
#ifdef MPI
    use mpi
#endif

      real, intent(in) :: dt
      real, intent(in), dimension(ib:ie) :: xh
      real, intent(in), dimension(ib:ie+1) :: xf
      real, intent(in), dimension(jb:je) :: yh
      real, intent(in), dimension(jb:je+1) :: yf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: u3d
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: v3d
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: w3d
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: uten1
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: vten1
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: wten1
      real, intent(inout), dimension(irecywe,jrecywe,krecy) :: urecyw,vrecyw,wrecyw,trecyw
      real, intent(inout) , dimension(ib3d:ie3d,jb3d:je3d,kb3d:ke3d,nout3d) :: out3d

      integer :: i,j,k,n,ii,proc,jdisp
      real :: tem,tscale
#ifdef MPI
      integer, dimension(mpi_status_size) :: status
#endif

        urecyw = 0.0
        vrecyw = 0.0
        wrecyw = 0.0

        ! 200719: shift data a tad, repeat excessive recycling
        jdisp = nint(var9)
        jdisp = 0
        jdisp = ngxy

#ifndef MPI
        ! shared memory version:

          do k=1,krecy
            do j=1,nj
            do i=irc1,irc2
              ii = i-irc1+1
              urecyw(ii,j,k) = u3d(i,j+jdisp,k)
              wrecyw(ii,j,k) = w3d(i,j+jdisp,k)
            enddo
            enddo
            do j=1,nj+1
            do i=irc1,irc2
              ii = i-irc1+1
              vrecyw(ii,j,k) = v3d(i,j+jdisp,k)
            enddo
            enddo
          enddo

#else
        ! MPI version:

        ! note:  a quick but ineffecient way to share data  (for now ... maybe improve later)

        if( irb.ge.1 )then
          ! fill data:
          do k=1,krecy
            do j=1,jrecywe-1
            do i=ircs1,ircs2
              ii = irb + (i-ircs1)
              urecyw(ii,j,k) = u3d(i,j+jdisp,k)
              wrecyw(ii,j,k) = w3d(i,j+jdisp,k)
            enddo
            enddo
            do j=1,jrecywe
            do i=ircs1,ircs2
              ii = irb + (i-ircs1)
              vrecyw(ii,j,k) = v3d(i,j+jdisp,k)
            enddo
            enddo
          enddo
        endif

        ! left-most processor receives data, collects, then sends back:
        myloc:  &
        IF( myi.eq.1 )THEN

          ! I am left-most proc in this row:

          ! loop through all procs in this row:
          do n=2,nodex

            ! receive data, add to recy arrays:
            proc = (myj-1)*nodex + n - 1
            call mpi_recv(trecyw,irecywe*jrecywe*krecy,MPI_REAL,proc,7701,MPI_COMM_WORLD,status,ierr)
            do k=1,krecy
            do j=1,jrecywe
            do i=1,irecywe
              urecyw(i,j,k) = urecyw(i,j,k) + trecyw(i,j,k)
            enddo
            enddo
            enddo
            call mpi_recv(trecyw,irecywe*jrecywe*krecy,MPI_REAL,proc,7702,MPI_COMM_WORLD,status,ierr)
            do k=1,krecy
            do j=1,jrecywe
            do i=1,irecywe
              vrecyw(i,j,k) = vrecyw(i,j,k) + trecyw(i,j,k)
            enddo
            enddo
            enddo
            call mpi_recv(trecyw,irecywe*jrecywe*krecy,MPI_REAL,proc,7703,MPI_COMM_WORLD,status,ierr)
            do k=2,krecy
            do j=1,jrecywe
            do i=1,irecywe
              wrecyw(i,j,k) = wrecyw(i,j,k) + trecyw(i,j,k)
            enddo
            enddo
            enddo
          enddo

          do n=2,nodex
            ! send final data back to all procs in row:
            proc = (myj-1)*nodex + n - 1
            call mpi_send(urecyw,irecywe*jrecywe*krecy,MPI_REAL,proc,7704,MPI_COMM_WORLD,ierr)
            call mpi_send(vrecyw,irecywe*jrecywe*krecy,MPI_REAL,proc,7705,MPI_COMM_WORLD,ierr)
            call mpi_send(wrecyw,irecywe*jrecywe*krecy,MPI_REAL,proc,7706,MPI_COMM_WORLD,ierr)
          enddo

        ELSE  myloc

          ! I am NOT left-most proc in this row:

          proc = (myj-1)*nodex + 1 - 1

          ! send to left-most proc:
          call mpi_send(urecyw,irecywe*jrecywe*krecy,MPI_REAL,proc,7701,MPI_COMM_WORLD,ierr)
          call mpi_send(vrecyw,irecywe*jrecywe*krecy,MPI_REAL,proc,7702,MPI_COMM_WORLD,ierr)
          call mpi_send(wrecyw,irecywe*jrecywe*krecy,MPI_REAL,proc,7703,MPI_COMM_WORLD,ierr)

          ! receive from left-most proc:
          call mpi_recv(urecyw,irecywe*jrecywe*krecy,MPI_REAL,proc,7704,MPI_COMM_WORLD,status,ierr)
          call mpi_recv(vrecyw,irecywe*jrecywe*krecy,MPI_REAL,proc,7705,MPI_COMM_WORLD,status,ierr)
          call mpi_recv(wrecyw,irecywe*jrecywe*krecy,MPI_REAL,proc,7706,MPI_COMM_WORLD,status,ierr)

        ENDIF  myloc

#endif

        ! add recycle tendencies:
        IF( iris1.ge.1 )THEN
          ! relaxation time scale (seconds):
          ! (do not let this fall below 4 times the time step)
          tscale = max( 10.0 , 4.0*dt )
!!!          if( myid.eq.0 ) print *,'  tscale = ',tscale
          kloop:  &
          do k=1,krecy
            do j=1,nj
            do i=iris1,iris2+1
              ii = myi1-1 + i
              tem = max( 0.0 , min( 1.0 , 1.0-(zh(1,1,k)-0.9*recy_depth)/(0.1*recy_depth)  )  )  &
                   *max( 0.0 , min( 1.0 , 1.0-( (xf(i)-minx)-0.8*recy_width)/(0.2*recy_width)  )  )
              tem = ( 1.0/tscale )*tem
              uten1(i,j,k) = uten1(i,j,k)-tem*( u3d(i,j,k)-urecyw(ii,j,k) )
            enddo
            enddo
            do j=1,nj+1
            do i=iris1,iris2
              ii = myi1-1 + i
              tem = max( 0.0 , min( 1.0 , 1.0-(zh(1,1,k)-0.9*recy_depth)/(0.1*recy_depth)  )  )  &
                   *max( 0.0 , min( 1.0 , 1.0-( (xh(i)-minx)-0.8*recy_width)/(0.2*recy_width)  )  )
              tem = ( 1.0/tscale )*tem
              vten1(i,j,k) = vten1(i,j,k)-tem*( v3d(i,j,k)-vrecyw(ii,j,k) )
            enddo
            enddo
          if( k.ge.2 )then
            do j=1,nj
            do i=iris1,iris2
              ii = myi1-1 + i
              tem = max( 0.0 , min( 1.0 , 1.0-(zf(1,1,k)-0.9*recy_depth)/(0.1*recy_depth)  )  )  &
                   *max( 0.0 , min( 1.0 , 1.0-( (xh(i)-minx)-0.8*recy_width)/(0.2*recy_width)  )  )
              tem = ( 1.0/tscale )*tem
              wten1(i,j,k) = wten1(i,j,k)-tem*( w3d(i,j,k)-wrecyw(ii,j,k) )
            enddo
            enddo
          endif
          enddo  kloop
        ENDIF

    end subroutine do_eddy_recyw

!-----------------------------------------------------------------------
!   Eddy recycler on south side of domain:

    subroutine do_eddy_recys(dt,xh,xf,yh,yf,zh,zf,u3d,v3d,w3d,uten1,vten1,wten1,urecys,vrecys,wrecys,trecys,out3d)

    use input
    use constants
#ifdef MPI
    use mpi
#endif

      real, intent(in) :: dt
      real, intent(in), dimension(ib:ie) :: xh
      real, intent(in), dimension(ib:ie+1) :: xf
      real, intent(in), dimension(jb:je) :: yh
      real, intent(in), dimension(jb:je+1) :: yf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: u3d
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: v3d
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: w3d
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: uten1
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: vten1
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: wten1
      real, intent(inout), dimension(irecysn,jrecysn,krecy) :: urecys,vrecys,wrecys,trecys
      real, intent(inout) , dimension(ib3d:ie3d,jb3d:je3d,kb3d:ke3d,nout3d) :: out3d

      integer :: i,j,k,n,jj,proc,idisp
      real :: tem,tscale
#ifdef MPI
      integer, dimension(mpi_status_size) :: status
#endif

        urecys = 0.0
        vrecys = 0.0
        wrecys = 0.0

        ! 200719: shift data a tad, repeat excessive recycling
        idisp = nint(var9)
        idisp = 0
        idisp = ngxy

#ifndef MPI
        ! shared memory version:

          do k=1,krecy
            do j=jrc1,jrc2
            do i=1,ni
              jj = j-jrc1+1
              vrecys(i,jj,k) = v3d(i+idisp,j,k)
              wrecys(i,jj,k) = w3d(i+idisp,j,k)
            enddo
            enddo
            do j=jrc1,jrc2
            do i=1,ni+1
              jj = j-jrc1+1
              urecys(i,jj,k) = u3d(i+idisp,j,k)
            enddo
            enddo
          enddo

#else
        ! MPI version:

        ! note:  a quick but ineffecient way to share data  (for now ... maybe improve later)

        if( jrb.ge.1 )then
          ! fill data:
          do k=1,krecy
            do j=jrcs1,jrcs2
            do i=1,irecysn-1
              jj = jrb + (j-jrcs1)
              vrecys(i,jj,k) = v3d(i+idisp,j,k)
              wrecys(i,jj,k) = w3d(i+idisp,j,k)
            enddo
            enddo
            do j=jrcs1,jrcs2
            do i=1,irecysn
              jj = jrb + (j-jrcs1)
              urecys(i,jj,k) = u3d(i+idisp,j,k)
            enddo
            enddo
          enddo
        endif

        ! south-most processor receives data, collects, then sends back:
        myloc:  &
        IF( myj.eq.1 )THEN

          ! I am south-most proc in this column:

          ! loop through all procs in this column:
          do n=2,nodey

            ! receive data, add to recy arrays:
            proc = myid + (n-1)*nodex
            call mpi_recv(trecys,irecysn*jrecysn*krecy,MPI_REAL,proc,7711,MPI_COMM_WORLD,status,ierr)
            do k=1,krecy
            do j=1,jrecysn
            do i=1,irecysn
              urecys(i,j,k) = urecys(i,j,k) + trecys(i,j,k)
            enddo
            enddo
            enddo
            call mpi_recv(trecys,irecysn*jrecysn*krecy,MPI_REAL,proc,7712,MPI_COMM_WORLD,status,ierr)
            do k=1,krecy
            do j=1,jrecysn
            do i=1,irecysn
              vrecys(i,j,k) = vrecys(i,j,k) + trecys(i,j,k)
            enddo
            enddo
            enddo
            call mpi_recv(trecys,irecysn*jrecysn*krecy,MPI_REAL,proc,7713,MPI_COMM_WORLD,status,ierr)
            do k=2,krecy
            do j=1,jrecysn
            do i=1,irecysn
              wrecys(i,j,k) = wrecys(i,j,k) + trecys(i,j,k)
            enddo
            enddo
            enddo
          enddo

          do n=2,nodey
            ! send final data back to all procs in column:
            proc = myid + (n-1)*nodex
            call mpi_send(urecys,irecysn*jrecysn*krecy,MPI_REAL,proc,7714,MPI_COMM_WORLD,ierr)
            call mpi_send(vrecys,irecysn*jrecysn*krecy,MPI_REAL,proc,7715,MPI_COMM_WORLD,ierr)
            call mpi_send(wrecys,irecysn*jrecysn*krecy,MPI_REAL,proc,7716,MPI_COMM_WORLD,ierr)
          enddo

        ELSE  myloc

          ! I am NOT south-most proc in this column:

          proc = myi-1

          ! send to south-most proc:
          call mpi_send(urecys,irecysn*jrecysn*krecy,MPI_REAL,proc,7711,MPI_COMM_WORLD,ierr)
          call mpi_send(vrecys,irecysn*jrecysn*krecy,MPI_REAL,proc,7712,MPI_COMM_WORLD,ierr)
          call mpi_send(wrecys,irecysn*jrecysn*krecy,MPI_REAL,proc,7713,MPI_COMM_WORLD,ierr)

          ! receive from south-most proc:
          call mpi_recv(urecys,irecysn*jrecysn*krecy,MPI_REAL,proc,7714,MPI_COMM_WORLD,status,ierr)
          call mpi_recv(vrecys,irecysn*jrecysn*krecy,MPI_REAL,proc,7715,MPI_COMM_WORLD,status,ierr)
          call mpi_recv(wrecys,irecysn*jrecysn*krecy,MPI_REAL,proc,7716,MPI_COMM_WORLD,status,ierr)

        ENDIF  myloc

#endif

        ! add recycle tendencies:
        IF( jris1.ge.1 )THEN
          ! relaxation time scale (seconds):
          ! (do not let this fall below 4 times the time step)
          tscale = max( 10.0 , 4.0*dt )
!!!          if( myid.eq.0 ) print *,'  tscale = ',tscale
          kloop:  &
          do k=1,krecy
            do j=jris1,jris2+1
            do i=1,ni+1
              jj = myj1-1 + j
              tem = max( 0.0 , min( 1.0 , 1.0-(zh(1,1,k)-0.9*recy_depth)/(0.1*recy_depth)  )  )  &
                   *max( 0.0 , min( 1.0 , 1.0-( (yh(j)-miny)-0.8*recy_width)/(0.2*recy_width)  )  )
              tem = ( 1.0/tscale )*tem
              uten1(i,j,k) = uten1(i,j,k)-tem*( u3d(i,j,k)-urecys(i,jj,k) )
            enddo
            enddo
            do j=jris1,jris2
            do i=1,ni
              jj = myj1-1 + j
              tem = max( 0.0 , min( 1.0 , 1.0-(zh(1,1,k)-0.9*recy_depth)/(0.1*recy_depth)  )  )  &
                   *max( 0.0 , min( 1.0 , 1.0-( (yf(j)-miny)-0.8*recy_width)/(0.2*recy_width)  )  )
              tem = ( 1.0/tscale )*tem
              vten1(i,j,k) = vten1(i,j,k)-tem*( v3d(i,j,k)-vrecys(i,jj,k) )
            enddo
            enddo
          if( k.ge.2 )then
            do j=jris1,jris2
            do i=1,ni
              jj = myj1-1 + j
              tem = max( 0.0 , min( 1.0 , 1.0-(zf(1,1,k)-0.9*recy_depth)/(0.1*recy_depth)  )  )  &
                   *max( 0.0 , min( 1.0 , 1.0-( (yh(j)-miny)-0.8*recy_width)/(0.2*recy_width)  )  )
              tem = ( 1.0/tscale )*tem
              wten1(i,j,k) = wten1(i,j,k)-tem*( w3d(i,j,k)-wrecys(i,jj,k) )
            enddo
            enddo
          endif
          enddo  kloop
        ENDIF

    end subroutine do_eddy_recys

!-----------------------------------------------------------------------

  end module eddy_recycle
