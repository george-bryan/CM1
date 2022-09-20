#undef _GPUDIRECT

#ifdef _GPUDIRECT

#define _GPUDIRECT01
#define _GPUDIRECT02
#define _GPUDIRECT03
#define _GPUDIRECT04
#define _GPUDIRECT05

#define _GPUDIRECT07
#define _GPUDIRECT09
#define _GPUDIRECT10
#define _GPUDIRECT11
#define _GPUDIRECT12
#define _GPUDIRECT13

#define _GPUDIRECT17
#define _GPUDIRECT18
#define _GPUDIRECT19
#define _GPUDIRECT20
#define _GPUDIRECT21
#define _GPUDIRECT22


! 06,08,14 generates an runtime ERROR
#undef _GPUDIRECT06  
#undef _GPUDIRECT08 
#undef _GPUDIRECT14

!Activating the following directive generates incorrect results.
#define _GPUDIRECT15
#define _GPUDIRECT16




#endif

  MODULE comm_module

  implicit none
  private
  public :: nabor

  !GPU enabled routines 
  public :: comm_2d_start_GPU,comm_2dns_end_GPU,comm_2dew_end_GPU
  public :: comm_1s_start_GPU,comm_1s_end_GPU
  public :: comm_1w_start_GPU,comm_1w_end_GPU
  public :: comm_1p_start_GPU,comm_1p_end_GPU
  public :: comm_1t_start_GPU,comm_1t_end_GPU
  public :: comm_2sn_start_GPU,comm_2sn_end_GPU
  public :: comm_3s_start_GPU,comm_3s_end_GPU
  public :: comm_3t_start_GPU,comm_3t_end_GPU
  public :: comm_3w_start_GPU,comm_3w_end_GPU
  public :: comm_3u_start_GPU,comm_3u_end_GPU
  public :: comm_3v_start_GPU,comm_3v_end_GPU
  public :: comm_1s2d_start_GPU,comm_1s2d_end_GPU
  public :: comm_2we_start_GPU,comm_2we_end_GPU
  public :: comm_2d_corner_GPU
  public :: comm_1s_tend_halo_GPU
  public :: getcorneru_GPU,getcornerv_GPU,getcornert_GPU,getcornerw_GPU,getcorner_GPU
  public :: getcorneru3_GPU,getcornerv3_GPU,getcornerw3_GPU
  public :: prepcorners3_GPU,prepcornert_GPU,prepcorners_GPU

  !CPU only routines
  public :: comm_2d_start, comm_2dns_end,comm_2dew_end
  public :: comm_2d_corner,comm_all_s
  public :: comm_1s_start,comm_1s_end
  public :: comm_3s_start,comm_3s_end
  public :: comm_3t_start,comm_3t_end
  public :: comm_3u_start,comm_3u_end
  public :: comm_3v_start,comm_3v_end
  public :: comm_3w_start,comm_3w_end
  public :: comm_1s2d_start,comm_1s2d_end
  public :: getcorneru3,getcornerv3,getcorner3_2d,getcorner
  public :: comm_droplet_number,comm_droplet_value

  CONTAINS

!-----------------------------------------------------------------------
!  message passing routines
!-----------------------------------------------------------------------


      integer function nabor(i,j,nx,ny)
      implicit none
      integer i,j,nx,ny
      integer newi,newj

      newi=i
      newj=j

      if ( newi .lt.  1 ) newi = nx
      if ( newi .gt.  nx) newi = 1

      if ( newj .lt.  1 ) newj = ny
      if ( newj .gt.  ny) newj = 1

      nabor = (newi-1) + (newj-1)*nx

      end function nabor

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

#ifdef MPI
      subroutine getcorner(s,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe,ibs,ibn, &
          mynw,mysw,myne,myse,ierr,timestats,time_mptk1,mytime
      use mpi
      implicit none
 
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: s
      real, intent(inout), dimension(nk) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
 
      integer k,nn,nr,nrb,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag1,tag2,tag3,tag4
      integer, dimension(mpi_status_size,8) :: status1

!-----

      nr = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1

!------------------------------------------------------------------

      tag1=5001

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
        call mpi_irecv(nw2,nk,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        index_nw = nr
      endif

!-----

      tag2=5002

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
        call mpi_irecv(sw2,nk,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        index_sw = nr
      endif

!-----

      tag3=5003

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
        call mpi_irecv(ne2,nk,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        index_ne = nr
      endif

!-----

      tag4=5004

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
        call mpi_irecv(se2,nk,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        index_se = nr
      endif

!------------------------------------------------------------------

      nrb = 4

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          se1(k)=s(ni,1,k)
        enddo
        nrb = nrb + 1
        call mpi_isend(se1,nk,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
      endif
 
!-----

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          ne1(k)=s(ni,nj,k)
        enddo
        nrb = nrb + 1
        call mpi_isend(ne1,nk,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
      endif
 
!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          sw1(k)=s(1,1,k)
        enddo
        nrb = nrb + 1
        call mpi_isend(sw1,nk,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
      endif
 
!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          nw1(k)=s(1,nj,k)
        enddo
        nrb = nrb + 1
        call mpi_isend(nw1,nk,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
      endif
 
!-----

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_nw)then
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          s(0,nj+1,k)=nw2(k)
        enddo
      elseif(index.eq.index_sw)then
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          s(0,0,k)=sw2(k)
        enddo
      elseif(index.eq.index_ne)then
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          s(ni+1,nj+1,k)=ne2(k)
        enddo
      elseif(index.eq.index_se)then
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          s(ni+1,0,k)=se2(k)
        enddo
      endif

      enddo

!-----

    nrb = nrb-4

    if( nrb.ge.1 )then
      call MPI_WAITALL(nrb,reqs(5:5+nrb-1),status1(1:mpi_status_size,5:5+nrb-1),ierr)
    endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()

      end subroutine getcorner

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine getcorner_GPU(s,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe,ibs,ibn, &
          mynw,mysw,myne,myse,ierr,timestats,time_mptk1,mytime
      use mpi
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: s
      real, intent(inout), dimension(nk) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2

      integer k,nn,nr,nrb,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag1,tag2,tag3,tag4
      integer, dimension(mpi_status_size,8) :: status1
      logical, parameter :: Debug =.FALSE.
!-----

      nr = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1
!      !$acc update host(s)
!------------------------------------------------------------------

#ifndef _GPUDIRECT01
      if(Debug) print *,'getcorner_GPU: '
#endif
      tag1=5001

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT01
        !$acc host_data use_device(nw2)
        call mpi_irecv(nw2,nk,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(nw2,nk,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_nw = nr
      endif

!-----

      tag2=5002

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT01
        !$acc host_data use_device(sw2)
        call mpi_irecv(sw2,nk,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(sw2,nk,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_sw = nr
      endif

!-----

      tag3=5003

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT01
        !$acc host_data use_device(ne2)
        call mpi_irecv(ne2,nk,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(ne2,nk,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_ne = nr
      endif

!-----

      tag4=5004

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT01
        !$acc host_data use_device(se2)
        call mpi_irecv(se2,nk,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(se2,nk,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_se = nr
      endif

!------------------------------------------------------------------

      nrb = 4

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          se1(k)=s(ni,1,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT01
        !$acc host_data use_device(se1)
        call mpi_isend(se1,nk,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(se1)
        call mpi_isend(se1,nk,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          ne1(k)=s(ni,nj,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT01
        !$acc host_data use_device(ne1)
        call mpi_isend(ne1,nk,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(ne1)
        call mpi_isend(ne1,nk,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          sw1(k)=s(1,1,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT01
        !$acc host_data use_device(sw1)
        call mpi_isend(sw1,nk,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(sw1)
        call mpi_isend(sw1,nk,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          nw1(k)=s(1,nj,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT01
        !$acc host_data use_device(nw1)
        call mpi_isend(nw1,nk,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(nw1)
        call mpi_isend(nw1,nk,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_nw)then
#ifndef _GPUDIRECT01
        !$acc update device(nw2)
#endif
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          s(0,nj+1,k)=nw2(k)
        enddo
      elseif(index.eq.index_sw)then
#ifndef _GPUDIRECT01
        !$acc update device(sw2)
#endif
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          s(0,0,k)=sw2(k)
        enddo
      elseif(index.eq.index_ne)then
#ifndef _GPUDIRECT01
        !$acc update device(ne2)
#endif
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          s(ni+1,nj+1,k)=ne2(k)
        enddo
      elseif(index.eq.index_se)then
#ifndef _GPUDIRECT01
        !$acc update device(se2)
#endif
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          s(ni+1,0,k)=se2(k)
        enddo
      endif

      enddo

!-----

    nrb = nrb-4

    if( nrb.ge.1 )then
      call MPI_WAITALL(nrb,reqs(5:5+nrb-1),status1(1:mpi_status_size,5:5+nrb-1),ierr)
    endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()
      end subroutine getcorner_GPU


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getcornert_GPU(t,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe,ibs,ibn, &
          nkp1,mynw,mysw,myne,myse,ierr,timestats,time_mptk1,mytime
      use mpi
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: t
      real, intent(inout), dimension(nk+1) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2

      integer k,nn,nr,nrb,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag1,tag2,tag3,tag4
      integer, dimension(mpi_status_size,8) :: status1
      logical, parameter :: Debug =.FALSE.

!-----
#ifndef _GPUDIRECT02
      if(Debug) print *,'getcornert_GPU:'
#endif
      nr = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1
!-------------------------------------------------------------

      tag1=5001

      if(ibw.eq.0 .and. ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT02
        !$acc host_data use_device(nw2)
        call mpi_irecv(nw2,nkp1,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(nw2,nkp1,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_nw = nr
      endif

!-----

      tag2=5002

      if(ibw.eq.0 .and. ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT02
        !$acc host_data use_device(sw2)
        call mpi_irecv(sw2,nkp1,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(sw2,nkp1,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_sw = nr
      endif

!-----

      tag3=5003

      if(ibe.eq.0 .and. ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT02
        !$acc host_data use_device(ne2)
        call mpi_irecv(ne2,nkp1,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(ne2,nkp1,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_ne = nr
      endif

!-----

      tag4=5004

      if(ibe.eq.0 .and. ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT02
        !$acc host_data use_device(se2)
        call mpi_irecv(se2,nkp1,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(se2,nkp1,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_se = nr
      endif

!-------------------------------------------------------------

      nrb = 4

      if(ibe.eq.0 .and. ibs.eq.0)then
        !$omp parallel do default(shared) private(k)
        !$acc parallel loop default(present) private(k)
        do k=1,nkp1
          se1(k)=t(ni,1,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT02
        !$acc host_data use_device(se1)
        call mpi_isend(se1,nkp1,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(se1)
        call mpi_isend(se1,nkp1,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      if(ibe.eq.0 .and. ibn.eq.0)then
        !$omp parallel do default(shared) private(k)
        !$acc parallel loop default(present) private(k)
        do k=1,nkp1
          ne1(k)=t(ni,nj,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT02
        !$acc host_data use_device(ne1)
        call mpi_isend(ne1,nkp1,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(ne1)
        call mpi_isend(ne1,nkp1,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      if(ibw.eq.0 .and. ibs.eq.0)then
        !$omp parallel do default(shared) private(k)
        !$acc parallel loop default(present) private(k)
        do k=1,nkp1
          sw1(k)=t(1,1,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT02
        !$acc host_data use_device(sw1)
        call mpi_isend(sw1,nkp1,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(sw1)
        call mpi_isend(sw1,nkp1,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      if(ibw.eq.0 .and. ibn.eq.0)then
        !$omp parallel do default(shared) private(k)
        !$acc parallel loop default(present) private(k)
        do k=1,nkp1
          nw1(k)=t(1,nj,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT02
        !$acc host_data use_device(nw1)
        call mpi_isend(nw1,nkp1,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(nw1)
        call mpi_isend(nw1,nkp1,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_nw)then
#ifndef _GPUDIRECT02
        !$acc update device(nw2)
#endif
        !$omp parallel do default(shared) private(k)
        !$acc parallel loop default(present) private(k)
        do k=1,nkp1
          t(0,nj+1,k)=nw2(k)
        enddo
      elseif(index.eq.index_sw)then
#ifndef _GPUDIRECT02
        !$acc update device(sw2)
#endif
        !$omp parallel do default(shared) private(k)
        !$acc parallel loop default(present) private(k)
        do k=1,nkp1
          t(0,0,k)=sw2(k)
        enddo
      elseif(index.eq.index_ne)then
#ifndef _GPUDIRECT02
        !$acc update device(ne2)
#endif
        !$omp parallel do default(shared) private(k)
        !$acc parallel loop default(present) private(k)
        do k=1,nkp1
          t(ni+1,nj+1,k)=ne2(k)
        enddo
      elseif(index.eq.index_se)then
#ifndef _GPUDIRECT02
        !$acc update device(se2)
#endif
        !$omp parallel do default(shared) private(k)
        !$acc parallel loop default(present) private(k)
        do k=1,nkp1
          t(ni+1,0,k)=se2(k)
        enddo
      endif

      enddo
!-----

    nrb = nrb-4

    if( nrb.ge.1 )then
      call MPI_WAITALL(nrb,reqs(5:5+nrb-1),status1(1:mpi_status_size,5:5+nrb-1),ierr)
    endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()
      end subroutine getcornert_GPU

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getcorneru_GPU(u,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe,ibs,ibn, &
          nkp1,mynw,mysw,myne,myse,ierr,timestats,time_mptk1,mytime
      use mpi
      implicit none

      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: u
      real, intent(inout), dimension(nk) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2

      integer k,nn,nr,nrb,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag,count
      integer, dimension(mpi_status_size,8) :: status1
      logical, parameter :: Debug =.FALSE.

!-----
#ifndef _GPUDIRECT
      if(Debug) print *,'getcorneru_GPU:'
#endif
      nr = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1
!      !$acc update host(u)
!-----------------------------------------------------------------------

      tag=5001
      count=nk

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
!!$acc host_data use_device(nw2)
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
!!$acc end host_data
        index_nw = nr
      endif

!-----

      tag=5002
      count=nk

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
!!$acc host_data use_device(sw2)
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
!!$acc end host_data
        index_sw = nr
      endif

!-----

      tag=5003
      count=nk

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
!!$acc host_data use_device(ne2)
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
!!$acc end host_data
        index_ne = nr
      endif

!-----

      tag=5004
      count=nk

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
!!$acc host_data use_device(se2)
        call mpi_irecv(se2,count,MPI_REAL,myse,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
!!$acc end host_data
        index_se = nr
      endif

!-----------------------------------------------------------------------

      nrb = 4

      tag=5001
      count=nk

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          se1(k)=u(ni,1,k)
        enddo
        nrb = nrb + 1
        !$acc update host(se1)
!!$acc host_data use_device(se1)
        call mpi_isend(se1,count,MPI_REAL,myse,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
!!$acc end host_data
      endif

!-----

      tag=5002
      count=nk

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          ne1(k)=u(ni,nj,k)
        enddo
        nrb = nrb + 1
        !$acc update host(ne1)
!!$acc host_data use_device(ne1)
        call mpi_isend(ne1,count,MPI_REAL,myne,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
!!$acc end host_data
      endif

!-----

      tag=5003
      count=nk

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          sw1(k)=u(2,1,k)
        enddo
        nrb = nrb + 1
        !$acc update host(sw1)
!!$acc host_data use_device(sw1)
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
!!$acc end host_data
      endif

!-----

      tag=5004
      count=nk

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          nw1(k)=u(2,nj,k)
        enddo
        nrb = nrb + 1
        !$acc update host(nw1)
!!$acc host_data use_device(nw1)
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
!!$acc end host_data
      endif

!-----------------------------------------------------------------------

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_nw)then
        !$acc update device(nw2)
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          u(0,nj+1,k)=nw2(k)
        enddo
      elseif(index.eq.index_sw)then
        !$acc update device(sw2)  
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          u(0,0,k)=sw2(k)
        enddo
      elseif(index.eq.index_ne)then
        !$acc update device(ne2)
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          u(ni+2,nj+1,k)=ne2(k)
        enddo
      elseif(index.eq.index_se)then
        !$acc update device(se2)
!$acc parallel loop gang vector default(present) private(k)
!$omp parallel do default(shared)   &
!$omp private(k)
        do k=1,nk
          u(ni+2,0,k)=se2(k)
        enddo
      endif

      enddo

!-----

    nrb = nrb-4

    if( nrb.ge.1 )then
      call MPI_WAITALL(nrb,reqs(5:5+nrb-1),status1(1:mpi_status_size,5:5+nrb-1),ierr)
    endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()
      end subroutine getcorneru_GPU

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getcornerv_GPU(v,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe,ibs,ibn, &
          nkp1,mynw,mysw,myne,myse,ierr,timestats,time_mptk1,mytime
      use mpi
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: v
      real, intent(inout), dimension(nk) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2

      integer k,nn,nr,nrb,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag,count
      integer, dimension(mpi_status_size,8) :: status1
      logical, parameter :: Debug =.FALSE.

!-----

#ifndef _GPUDIRECT03
      if(Debug) print *,'getcornerv_GPU:'
#endif
      nr = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1
!      !$acc update host(v)
!-----------------------------------------------------------------------

      tag=5011
      count=nk

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT03
        !$acc host_data use_device(nw2)
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_nw = nr
      endif

!-----

      tag=5012
      count=nk

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT03
        !$acc host_data use_device(sw2)
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_sw = nr
      endif

!-----

      tag=5013
      count=nk

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT03
        !$acc host_data use_device(ne2)
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_ne = nr
      endif

!-----

      tag=5014
      count=nk

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT03
        !$acc host_data use_device(se2)
        call mpi_irecv(se2,count,MPI_REAL,myse,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(se2,count,MPI_REAL,myse,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_se = nr
      endif

!-----------------------------------------------------------------------

      nrb = 4

      tag=5011
      count=nk

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          se1(k)=v(ni,2,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT03
        !$acc host_data use_device(se1)
        call mpi_isend(se1,count,MPI_REAL,myse,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(se1)
        call mpi_isend(se1,count,MPI_REAL,myse,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      tag=5012
      count=nk

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          ne1(k)=v(ni,nj,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT03
        !$acc host_data use_device(ne1)
        call mpi_isend(ne1,count,MPI_REAL,myne,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(ne1)
        call mpi_isend(ne1,count,MPI_REAL,myne,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      tag=5013
      count=nk

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          sw1(k)=v(1,2,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT03
        !$acc host_data use_device(sw1)
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(sw1)
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      tag=5014
      count=nk

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          nw1(k)=v(1,nj,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT03
        !$acc host_data use_device(nw1)
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(nw1)
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----------------------------------------------------------------------

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_nw)then
#ifndef _GPUDIRECT03
        !$acc update device(nw2)
#endif
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          v(0,nj+2,k)=nw2(k)
        enddo
      elseif(index.eq.index_sw)then
#ifndef _GPUDIRECT03
        !$acc update device(sw2)
#endif
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          v(0,0,k)=sw2(k)
        enddo
      elseif(index.eq.index_ne)then
#ifndef _GPUDIRECT03
        !$acc update device(ne2)
#endif
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          v(ni+1,nj+2,k)=ne2(k)
        enddo
      elseif(index.eq.index_se)then
#ifndef _GPUDIRECT03
        !$acc update device(se2)
#endif
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          v(ni+1,0,k)=se2(k)
        enddo
      endif

      enddo

!-----

    nrb = nrb-4

    if( nrb.ge.1 )then
      call MPI_WAITALL(nrb,reqs(5:5+nrb-1),status1(1:mpi_status_size,5:5+nrb-1),ierr)
    endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()
      end subroutine getcornerv_GPU


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getcornerw_GPU(w,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe,ibs,ibn, &
          nkp1,mynw,mysw,myne,myse,ierr,timestats,time_mptk1,mytime
      use mpi
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: w
      real, intent(inout), dimension(nk) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2

      integer k,nn,nr,nrb,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag,count
      integer, dimension(mpi_status_size,8) :: status1
      logical, parameter :: Debug =.FALSE.

!-----
#ifndef _GPUDIRECT04
      if(Debug) print *,'getcornerw_GPU:'
#endif
      nr = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1
!-----------------------------------------------------------------------

      tag=5021
      count=nk

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT04
        !$acc host_data use_device(nw2)
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_nw = nr
      endif

!-----

      tag=5022
      count=nk

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT04
        !$acc host_data use_device(sw2)
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_sw = nr
      endif

!-----

      tag=5023
      count=nk

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT04
        !$acc host_data use_device(ne2)
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_ne = nr
      endif

!-----

      tag=5024
      count=nk

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT04
        !$acc host_data use_device(se2)
        call mpi_irecv(se2,count,MPI_REAL,myse,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(se2,count,MPI_REAL,myse,tag,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_se = nr
      endif

!-----------------------------------------------------------------------

      nrb = 4

      tag=5021
      count=nk

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          se1(k)=w(ni,1,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT04
        !$acc host_data use_device(se1)
        call mpi_isend(se1,count,MPI_REAL,myse,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(se1)
        call mpi_isend(se1,count,MPI_REAL,myse,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      tag=5022
      count=nk

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          ne1(k)=w(ni,nj,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT04
        !$acc host_data use_device(ne1)
        call mpi_isend(ne1,count,MPI_REAL,myne,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(ne1)
        call mpi_isend(ne1,count,MPI_REAL,myne,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      tag=5023
      count=nk

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          sw1(k)=w(1,1,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT04
        !$acc host_data use_device(sw1)
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(sw1)
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----

      tag=5024
      count=nk

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          nw1(k)=w(1,nj,k)
        enddo
        nrb = nrb + 1
#ifdef _GPUDIRECT04
        !$acc host_data use_device(nw1)
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
        !$acc end host_data
#else
        !$acc update host(nw1)
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
#endif
      endif

!-----------------------------------------------------------------------

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_nw)then
#ifndef _GPUDIRECT04
        !$acc update device(nw2)
#endif
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          w(0,nj+1,k)=nw2(k)
        enddo
      elseif(index.eq.index_sw)then
#ifndef _GPUDIRECT04
        !$acc update device(sw2)
#endif
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          w(0,0,k)=sw2(k)
        enddo
      elseif(index.eq.index_ne)then
#ifndef _GPUDIRECT04
        !$acc update device(ne2)
#endif
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          w(ni+1,nj+1,k)=ne2(k)
        enddo
      elseif(index.eq.index_se)then
#ifndef _GPUDIRECT04
        !$acc update device(se2)
#endif
        !$acc parallel loop gang vector default(present) private(k)
        !$omp parallel do default(shared) private(k)
        do k=1,nk
          w(ni+1,0,k)=se2(k)
        enddo
      endif

      enddo

!-----

    nrb = nrb-4

    if( nrb.ge.1 )then
      call MPI_WAITALL(nrb,reqs(5:5+nrb-1),status1(1:mpi_status_size,5:5+nrb-1),ierr)
    endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()
      end subroutine getcornerw_GPU


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine getcorner3(s,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe,ibs,ibn, &
          cmp,nkp1,mynw,mysw,myne,myse,ierr,timestats,time_mptk1,mytime
      use mpi
      implicit none
 
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: s
      real, intent(inout), dimension(cmp,cmp,nk) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
 
      integer :: i,j,k,nn,nr,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag1,tag2,tag3,tag4

!-----

      nr = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1

!------------------------------------------------------------------

      tag1=5001

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
        call mpi_irecv(nw2,cmp*cmp*nk,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        index_nw = nr
      endif

!-----

      tag2=5002

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
        call mpi_irecv(sw2,cmp*cmp*nk,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        index_sw = nr
      endif

!-----

      tag3=5003

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
        call mpi_irecv(ne2,cmp*cmp*nk,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        index_ne = nr
      endif

!-----

      tag4=5004

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
        call mpi_irecv(se2,cmp*cmp*nk,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        index_se = nr
      endif

!------------------------------------------------------------------

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          se1(i,j,k)=s(ni,1,k)
          se1(i,j,k)=s(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        call mpi_isend(se1,cmp*cmp*nk,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(5),ierr)
      endif
 
!-----

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          ne1(i,j,k)=s(ni,nj,k)
          ne1(i,j,k)=s(ni-cmp+i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        call mpi_isend(ne1,cmp*cmp*nk,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(6),ierr)
      endif
 
!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          sw1(i,j,k)=s(1,1,k)
          sw1(i,j,k)=s(i,j,k)
        enddo
        enddo
        enddo
        call mpi_isend(sw1,cmp*cmp*nk,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(7),ierr)
      endif
 
!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          nw1(i,j,k)=s(1,nj,k)
          nw1(i,j,k)=s(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        call mpi_isend(nw1,cmp*cmp*nk,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(8),ierr)
      endif
 
!-----

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_nw)then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          s(0,nj+1,k)=nw2(i,j,k)
          s(-cmp+i,nj+j,k)=nw2(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_sw)then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          s(0,0,k)=sw2(i,j,k)
          s(-cmp+i,-cmp+j,k)=sw2(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_ne)then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          s(ni+1,nj+1,k)=ne2(i,j,k)
          s(ni+i,nj+j,k)=ne2(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_se)then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          s(ni+1,0,k)=se2(i,j,k)
          s(ni+i,-cmp+j,k)=se2(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

!-----

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        call MPI_WAIT (reqs(5),MPI_STATUS_IGNORE,ierr)
      endif

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        call MPI_WAIT (reqs(6),MPI_STATUS_IGNORE,ierr)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        call MPI_WAIT (reqs(7),MPI_STATUS_IGNORE,ierr)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        call MPI_WAIT (reqs(8),MPI_STATUS_IGNORE,ierr)
      endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()

      end subroutine getcorner3

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine getcorner3_GPU(s,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe,ibs,ibn, &
          cmp,nkp1,mynw,mysw,myne,myse,ierr,timestats,time_mptk1,mytime
      use mpi
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: s
      real, intent(inout), dimension(cmp,cmp,nk) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2

      integer :: i,j,k,nn,nr,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag1,tag2,tag3,tag4
      logical, parameter :: Debug =.FALSE.

!-----
#ifndef _GPUDIRECT05
      if(Debug) print *,'getcorner3_GPU:'
#endif
      nr = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1
!      !$acc update host(s)
!------------------------------------------------------------------

      tag1=5001

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT05
        !$acc host_data use_device(nw2)
        call mpi_irecv(nw2,cmp*cmp*nk,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(nw2,cmp*cmp*nk,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_nw = nr
      endif

!-----

      tag2=5002

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT05
        !$acc host_data use_device(sw2)
        call mpi_irecv(sw2,cmp*cmp*nk,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(sw2,cmp*cmp*nk,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_sw = nr
      endif

!-----

      tag3=5003

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT05
        !$acc host_data use_device(ne2)
        call mpi_irecv(ne2,cmp*cmp*nk,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(ne2,cmp*cmp*nk,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_ne = nr
      endif

!-----

      tag4=5004

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT05
        !$acc host_data use_device(se2)
        call mpi_irecv(se2,cmp*cmp*nk,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(se2,cmp*cmp*nk,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_se = nr
      endif

!------------------------------------------------------------------

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
!$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          se1(i,j,k)=s(ni,1,k)
          se1(i,j,k)=s(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
#ifdef _GPUDIRECT05
        !$acc host_data use_device(se1)
        call mpi_isend(se1,cmp*cmp*nk,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(5),ierr)
        !$acc end host_data
#else
        !$acc update host(se1)
        call mpi_isend(se1,cmp*cmp*nk,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(5),ierr)
#endif
      endif

!-----

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
!$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          ne1(i,j,k)=s(ni,nj,k)
          ne1(i,j,k)=s(ni-cmp+i,nj-cmp+j,k)
        enddo
        enddo
        enddo
#ifdef _GPUDIRECT05
        !$acc host_data use_device(ne1)
        call mpi_isend(ne1,cmp*cmp*nk,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(6),ierr)
        !$acc end host_data
#else
        !$acc update host(ne1)
        call mpi_isend(ne1,cmp*cmp*nk,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(6),ierr)
#endif
      endif

!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
!$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          sw1(i,j,k)=s(1,1,k)
          sw1(i,j,k)=s(i,j,k)
        enddo
        enddo
        enddo
#ifdef _GPUDIRECT05
        !$acc host_data use_device(sw1)
        call mpi_isend(sw1,cmp*cmp*nk,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(7),ierr)
        !$acc end host_data
#else
        !$acc update host(sw1)
        call mpi_isend(sw1,cmp*cmp*nk,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(7),ierr)
#endif
      endif

!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
!$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          nw1(i,j,k)=s(1,nj,k)
          nw1(i,j,k)=s(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
#ifdef _GPUDIRECT05
        !$acc host_data use_device(nw1)
        call mpi_isend(nw1,cmp*cmp*nk,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(8),ierr)
        !$acc end host_data
#else
        !$acc update host(nw1)
        call mpi_isend(nw1,cmp*cmp*nk,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(8),ierr)
#endif
      endif

!-----

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_nw)then
#ifndef _GPUDIRECT05
        !$acc update device(nw2)
#endif
!$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          s(0,nj+1,k)=nw2(i,j,k)
          s(-cmp+i,nj+j,k)=nw2(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_sw)then
#ifndef _GPUDIRECT05
        !$acc update device(sw2)
#endif
!$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          s(0,0,k)=sw2(i,j,k)
          s(-cmp+i,-cmp+j,k)=sw2(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_ne)then
#ifndef _GPUDIRECT05
        !$acc update device(ne2)
#endif
!$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          s(ni+1,nj+1,k)=ne2(i,j,k)
          s(ni+i,nj+j,k)=ne2(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_se)then
#ifndef _GPUDIRECT05
        !$acc update device(se2)
#endif
!$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
!!!          s(ni+1,0,k)=se2(i,j,k)
          s(ni+i,-cmp+j,k)=se2(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

!-----
      !JMD REVISIT
      !JMD I am confused here.  Why are we updating the buffers here... I don't
      !JMD think this is needed
      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        call MPI_WAIT (reqs(5),MPI_STATUS_IGNORE,ierr)
        !$acc update device(se2)
      endif

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        call MPI_WAIT (reqs(6),MPI_STATUS_IGNORE,ierr)
        !$acc update device(ne2)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        call MPI_WAIT (reqs(7),MPI_STATUS_IGNORE,ierr)
        !$acc update device(sw2)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        call MPI_WAIT (reqs(8),MPI_STATUS_IGNORE,ierr)
        !$acc update device(nw2)
      endif

!-----
 
      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()
      end subroutine getcorner3_GPU


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_2we_start_GPU(s,west,newwest,east,neweast,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe,ibs,ibn, &
          cs2we,nf,nkp1,myeast,mywest,ierr,timestats,time_mps3,mytime
      use mpi
      implicit none

      real s(ib:ie,jb:je,kb:ke)
      real west(2,nj,nk),newwest(2,nj,nk)
      real east(2,nj,nk),neweast(2,nj,nk)
      integer reqs(4)

      integer i,j,k,nr
      integer tag1,tag2
      logical, parameter :: Debug =.FALSE.
      !$acc declare present(s) &
      !$acc present(west,newwest,east,neweast) 

!------------------------------------------------
#ifndef _GPUDIRECT06
      if(Debug) print *,'comm_2we_start_GPU:'
#endif
      nr = 0

      nf=nf+1
      tag1=nf
      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT06
        !$acc host_data use_device(neweast)
        call mpi_irecv(neweast,cs2we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(neweast,cs2we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT06
        !$acc host_data use_device(neweast)
        call mpi_irecv(newwest,cs2we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newwest,cs2we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!------------------------------------------------

      nr = 2

      ! send west
      if(ibw.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,2
          west(i,j,k)=s(i,j,k)
        enddo
        enddo
        enddo
        nr = nr+1
#ifdef _GPUDIRECT06
        !$acc host_data use_device(west)
        call mpi_isend(west,cs2we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(west)
        call mpi_isend(west,cs2we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send east
      if(ibe.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,2
          east(i,j,k)=s(ni-2+i,j,k)
        enddo
        enddo
        enddo
        nr = nr+1
#ifdef _GPUDIRECT06
        !$acc host_data use_device(east)
        call mpi_isend(east,cs2we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(east)
        call mpi_isend(east,cs2we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      if(timestats.ge.1) time_mps3=time_mps3+mytime()
!      !$acc update device(s)
      end subroutine comm_2we_start_GPU


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_2we_end_GPU(s,west,newwest,east,neweast,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe,ibs,ibn, &
          cs2we,nf,nkp1,myeast,mywest,ierr,timestats,time_mps4,mytime
      use mpi
      implicit none

      real s(ib:ie,jb:je,kb:ke)
      real west(2,nj,nk),newwest(2,nj,nk)
      real east(2,nj,nk),neweast(2,nj,nk)
      integer reqs(4)

      integer i,j,k,nn,nr,index
      integer :: index_east,index_west
      integer, dimension(mpi_status_size,8) :: status1
      !$acc declare present(s) &
      !$acc present(west,newwest,east,neweast) 

!-------------------------------------------------------------------

      index_east = -1
      index_west = -1
      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_east)then
#ifndef _GPUDIRECT06
        !$acc update device(neweast)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,2
          s(ni+i,j,k)=neweast(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_west)then
#ifndef _GPUDIRECT06
        !$acc update device(newwest)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,2
          s(i-2,j,k)=newwest(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(3:3+nr-1),status1(1:mpi_status_size,3:3+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mps4=time_mps4+mytime()
!      !$acc update device(s)

      end subroutine comm_2we_end_GPU


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_2sn_start_GPU(s,south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe,ibs,ibn, &
          nf,cs2sn,nkp1,mysouth,mynorth,ierr,timestats,time_mps3,mytime
      use mpi
      implicit none

      real s(ib:ie,jb:je,kb:ke)
      real south(ni,2,nk),newsouth(ni,2,nk)
      real north(ni,2,nk),newnorth(ni,2,nk)
      integer reqs(4)

      integer i,j,k,nr
      integer tag3,tag4
      logical, parameter :: Debug =.TRUE.
      !$acc declare present(s) &
      !$acc present(south,newsouth,north,newnorth)

#ifndef _GPUDIRECT07
      !if(Debug) print *,'comm_2sn_start_GPU:'
#endif
!----------
      nr = 0

      nf=nf+1
      tag3=nf
      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT07
        !$acc host_data use_device(newsouth)
        call mpi_irecv(newsouth,cs2sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newsouth,cs2sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT07
        !$acc host_data use_device(newnorth)
        call mpi_irecv(newnorth,cs2sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newnorth,cs2sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nr = 2

      ! send north
      if(ibn.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,2
        do i=1,ni
          north(i,j,k)=s(i,nj-2+j,k)
        enddo
        enddo
        enddo
        nr = nr+1

#ifdef _GPUDIRECT07
        !$acc host_data use_device(north)
        call mpi_isend(north,cs2sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(north)
        call mpi_isend(north,cs2sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send south
      if(ibs.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,2
        do i=1,ni
          south(i,j,k)=s(i,j,k)
        enddo
        enddo
        enddo
        nr = nr+1
#ifdef _GPUDIRECT07
        !$acc host_data use_device(south)
        call mpi_isend(south,cs2sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(south)
        call mpi_isend(south,cs2sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      if(timestats.ge.1) time_mps3=time_mps3+mytime()

      end subroutine comm_2sn_start_GPU


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_2sn_end_GPU(s,south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe,ibs,ibn, &
          nf,cs2sn,nkp1,mysouth,mynorth,ierr,timestats,time_mps4,mytime
      use mpi
      implicit none

      real s(ib:ie,jb:je,kb:ke)
      real south(ni,2,nk),newsouth(ni,2,nk)
      real north(ni,2,nk),newnorth(ni,2,nk)
      integer reqs(4)

      integer i,j,k,nn,nr,index
      integer :: index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1
      !$acc declare present(s) &
      !$acc present(south,newsouth,north,newnorth)

!----------

      index_south = -1
      index_north = -1
      nr = 0
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_south)then
#ifndef _GPUDIRECT07
        !$acc update device(newsouth)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,2
        do i=1,ni
          s(i,j-2,k)=newsouth(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_north)then
#ifndef _GPUDIRECT07
        !$acc update device(newnorth) 
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,2
        do i=1,ni
          s(i,nj+j,k)=newnorth(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

!----------

      nr = 0

      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(3:3+nr-1),status1(1:mpi_status_size,3:3+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mps4=time_mps4+mytime() 
      end subroutine comm_2sn_end_GPU


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine comm_3s_start(s,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibw,ibe,ibs,ibn, &
          myeast,mywest,mysouth,mynorth,nf,cs3we,cs3sn,timestats,time_mps1, &
          mytime,ierr
      use mpi
      implicit none
 
      real s(ib:ie,jb:je,kb:ke)
      real west(cmp,nj,nk),newwest(cmp,nj,nk)
      real east(cmp,nj,nk),neweast(cmp,nj,nk)
      real south(ni,cmp,nk),newsouth(ni,cmp,nk)
      real north(ni,cmp,nk),newnorth(ni,cmp,nk)
      integer reqs(8)
 
      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
 
!------------------------------------------------

      nr = 0

      nf=nf+1
      tag1=nf

      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
        call mpi_irecv(neweast,cs3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
        call mpi_irecv(newwest,cs3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      nf=nf+1
      tag3=nf

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
        call mpi_irecv(newsouth,cs3sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
        call mpi_irecv(newnorth,cs3sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4

      ! send west
      if(ibw.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          west(i,j,k)=s(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(west,cs3we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------

 
      ! send east
      if(ibe.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          east(i,j,k)=s(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(east,cs3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

 
      ! send north
      if(ibn.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          north(i,j,k)=s(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(north,cs3sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

 
      ! send south
      if(ibs.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          south(i,j,k)=s(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(south,cs3sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------
 
      if(timestats.ge.1) time_mps1=time_mps1+mytime()

      end subroutine comm_3s_start

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine comm_3s_start_GPU(s,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibw,ibe,ibs,ibn, &
          myeast,mywest,mysouth,mynorth,nf,cs3we,cs3sn,timestats,time_mps1, &
          mytime,ierr
      use mpi
      implicit none
 
      real s(ib:ie,jb:je,kb:ke)
      real west(cmp,nj,nk),newwest(cmp,nj,nk)
      real east(cmp,nj,nk),neweast(cmp,nj,nk)
      real south(ni,cmp,nk),newsouth(ni,cmp,nk)
      real north(ni,cmp,nk),newnorth(ni,cmp,nk)
      integer reqs(8)
 
      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
      logical, parameter :: Debug =.FALSE.
      !$acc declare present(s) &
      !$acc present(west,newwest,east,neweast) &
      !$acc present(south,newsouth,north,newnorth)
 
!------------------------------------------------
#ifndef _GPUDIRECT08
      if(Debug) print *,'comm_3s_start_GPU:'
#endif
      nr = 0

      nf=nf+1
      tag1=nf
      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT08
        !$acc host_data use_device(neweast)
        call mpi_irecv(neweast,cs3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(neweast,cs3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT08
        !$acc host_data use_device(newwest)
        call mpi_irecv(newwest,cs3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newwest,cs3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      nf=nf+1
      tag3=nf

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT08
        !$acc host_data use_device(newsouth)
        call mpi_irecv(newsouth,cs3sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newsouth,cs3sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT08
        !$acc host_data use_device(newnorth)
        call mpi_irecv(newnorth,cs3sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newnorth,cs3sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4

      ! send west
      if(ibw.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          west(i,j,k)=s(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT08
        !$acc host_data use_device(west) 
        call mpi_isend(west,cs3we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(west)
        call mpi_isend(west,cs3we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------

 
      ! send east
      if(ibe.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          east(i,j,k)=s(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT08
        !$acc host_data use_device(east) 
        call mpi_isend(east,cs3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(east)
        call mpi_isend(east,cs3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

 
      ! send north
      if(ibn.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          north(i,j,k)=s(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT08
        !$acc host_data use_device(north) 
        call mpi_isend(north,cs3sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(north)
        call mpi_isend(north,cs3sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

 
      ! send south
      if(ibs.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          south(i,j,k)=s(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT08
        !$acc host_data use_device(south) 
        call mpi_isend(south,cs3sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(south)
        call mpi_isend(south,cs3sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------
 
      if(timestats.ge.1) time_mps1=time_mps1+mytime()

      end subroutine comm_3s_start_GPU

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_3s_end(s,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibw,ibe,ibs,ibn, &
          wbc,ebc,nbc,sbc,myeast,mywest,mysouth,mynorth,nf,cs3we,cs3sn,timestats, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen, &
          time_bc,time_mps2,mytime,ierr
      use mpi
      implicit none
 
      real s(ib:ie,jb:je,kb:ke)
      real west(cmp,nj,nk),newwest(cmp,nj,nk)
      real east(cmp,nj,nk),neweast(cmp,nj,nk)
      real south(ni,cmp,nk),newsouth(ni,cmp,nk)
      real north(ni,cmp,nk),newnorth(ni,cmp,nk)
      integer reqs(8)
 
      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1

!-------------------------------------------------------------------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif

    nn = 1
    do while( nn .le. nr )
      call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
      nn = nn + 1
 
      if(index.eq.index_east)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          s(ni+i,j,k)=neweast(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_west)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          s(i-cmp,j,k)=newwest(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_south)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          s(i,j-cmp,k)=newsouth(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_north)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          s(i,nj+j,k)=newnorth(i,j,k)
        enddo
        enddo
        enddo
      endif

    enddo

      if(timestats.ge.1) time_mps2=time_mps2+mytime()
 
!----------
!  patch for corner
 
      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then
 
        if(ibw.eq.1)then

          if(p2tchsww)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(0,0,k)=s(1,0,k)
            enddo
          endif

          if(p2tchnww)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(0,nj+1,k)=s(1,nj+1,k)
            enddo
          endif

        endif

        if(ibe.eq.1)then
 
          if(p2tchsee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(ni+1,0,k)=s(ni,0,k)
            enddo
          endif
 
          if(p2tchnee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(ni+1,nj+1,k)=s(ni,nj+1,k)
            enddo
          endif
 
        endif
 
      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then
 
        if(ibs.eq.1)then
 
          if(p2tchsws)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(0,0,k)=s(0,1,k)
            enddo
          endif
 
          if(p2tchses)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(ni+1,0,k)=s(ni+1,1,k)
            enddo
          endif
 
        endif
 
        if(ibn.eq.1)then
 
          if(p2tchnwn)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(0,nj+1,k)=s(0,nj,k)
            enddo
          endif
 
          if(p2tchnen)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(ni+1,nj+1,k)=s(ni+1,nj,k)
            enddo
          endif
 
        endif
 
      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mps2=time_mps2+mytime()

!-----------------------------------------------------------
 
      end subroutine comm_3s_end


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_3s_end_GPU(s,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibw,ibe,ibs,ibn, &
          wbc,ebc,nbc,sbc,myeast,mywest,mysouth,mynorth,nf,cs3we,cs3sn,timestats, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen, &
          time_bc,time_mps2, mytime,ierr
      use mpi
      implicit none
 
      real s(ib:ie,jb:je,kb:ke)
      real west(cmp,nj,nk),newwest(cmp,nj,nk)
      real east(cmp,nj,nk),neweast(cmp,nj,nk)
      real south(ni,cmp,nk),newsouth(ni,cmp,nk)
      real north(ni,cmp,nk),newnorth(ni,cmp,nk)
      integer reqs(8)
 
      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1
      !$acc declare present(s) &
      !$acc present(west,newwest,east,neweast) &
      !$acc present(south,newsouth,north,newnorth)

!-------------------------------------------------------------------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1
      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif

    nn = 1
    do while( nn .le. nr )
      call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
      nn = nn + 1
 
      if(index.eq.index_east)then
#ifndef _GPUDIRECT08
        !$acc update device(neweast)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          s(ni+i,j,k)=neweast(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_west)then
#ifndef _GPUDIRECT08
        !$acc update device(newwest)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          s(i-cmp,j,k)=newwest(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_south)then
#ifndef _GPUDIRECT08
        !$acc update device(newsouth)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          s(i,j-cmp,k)=newsouth(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_north)then
#ifndef _GPUDIRECT08
        !$acc update device(newnorth)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          s(i,nj+j,k)=newnorth(i,j,k)
        enddo
        enddo
        enddo
      endif

    enddo

      if(timestats.ge.1) time_mps2=time_mps2+mytime()
 
!----------
!  patch for corner
 
      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then
 
        if(ibw.eq.1)then

          if(p2tchsww)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(0,0,k)=s(1,0,k)
            enddo
          endif

          if(p2tchnww)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(0,nj+1,k)=s(1,nj+1,k)
            enddo
          endif

        endif

        if(ibe.eq.1)then
 
          if(p2tchsee)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(ni+1,0,k)=s(ni,0,k)
            enddo
          endif
 
          if(p2tchnee)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(ni+1,nj+1,k)=s(ni,nj+1,k)
            enddo
          endif
 
        endif
 
      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then
 
        if(ibs.eq.1)then
 
          if(p2tchsws)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(0,0,k)=s(0,1,k)
            enddo
          endif
 
          if(p2tchses)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(ni+1,0,k)=s(ni+1,1,k)
            enddo
          endif
 
        endif
 
        if(ibn.eq.1)then
 
          if(p2tchnwn)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(0,nj+1,k)=s(0,nj,k)
            enddo
          endif
 
          if(p2tchnen)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(ni+1,nj+1,k)=s(ni+1,nj,k)
            enddo
          endif
 
        endif
 
      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif
      if(timestats.ge.1) time_mps2=time_mps2+mytime()

!-----------------------------------------------------------
 
      end subroutine comm_3s_end_GPU



!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      ! tk1
      subroutine comm_3t_start(t,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibw,ibe,ibs,ibn, &
          nf,ct3we,ct3sn,nkp1,timestats,time_mps1,mytime,ierr, &
          myeast,mywest,mynorth,mysouth
      use mpi
      implicit none

      real t(ib:ie,jb:je,kb:ke+1)
      real west(cmp,nj,nk+1),newwest(cmp,nj,nk+1)
      real east(cmp,nj,nk+1),neweast(cmp,nj,nk+1)
      real south(ni,cmp,nk+1),newsouth(ni,cmp,nk+1)
      real north(ni,cmp,nk+1),newnorth(ni,cmp,nk+1)
      integer reqs(8)

      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4

!------------------------------------------------

      nr = 0

      nf=nf+1
      tag1=nf

      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
        call mpi_irecv(neweast,ct3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
        call mpi_irecv(newwest,ct3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      nf=nf+1
      tag3=nf

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
        call mpi_irecv(newsouth,ct3sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
        call mpi_irecv(newnorth,ct3sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4

      ! send west
      if(ibw.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nkp1
        do j=1,nj
        do i=1,cmp
          west(i,j,k)=t(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(west,ct3we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      ! send east
      if(ibe.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nkp1
        do j=1,nj
        do i=1,cmp
          east(i,j,k)=t(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(east,ct3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      ! send north
      if(ibn.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,ni
          north(i,j,k)=t(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(north,ct3sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      ! send south
      if(ibs.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,ni
          south(i,j,k)=t(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(south,ct3sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      if(timestats.ge.1) time_mps1=time_mps1+mytime()

      end subroutine comm_3t_start

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      ! tk1
      subroutine comm_3t_start_GPU(t,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibw,ibe,ibs,ibn, &
          nf,ct3we,ct3sn,nkp1,timestats,time_mps1,mytime,ierr, &
          myeast,mywest,mynorth,mysouth
      use mpi
      implicit none

      real t(ib:ie,jb:je,kb:ke+1)
      real west(cmp,nj,nk+1),newwest(cmp,nj,nk+1)
      real east(cmp,nj,nk+1),neweast(cmp,nj,nk+1)
      real south(ni,cmp,nk+1),newsouth(ni,cmp,nk+1)
      real north(ni,cmp,nk+1),newnorth(ni,cmp,nk+1)
      integer reqs(8)

      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
      logical, parameter :: Debug =.FALSE.
      !$acc declare present(t) &
      !$acc present(west,newwest,east,neweast) &
      !$acc present(south,newsouth,north,newnorth)
!------------------------------------------------

#ifndef _GPUDIRECT09
      if(Debug) print *,'comm_3t_start_GPU:'
#endif
      nr = 0

      nf=nf+1
      tag1=nf

      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT09
        !$acc host_data use_device(neweast)
        call mpi_irecv(neweast,ct3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(neweast,ct3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT09
        !$acc host_data use_device(newwest)
        call mpi_irecv(newwest,ct3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newwest,ct3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag3=nf

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT09
        !$acc host_data use_device(newsouth)
        call mpi_irecv(newsouth,ct3sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newsouth,ct3sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT09
        !$acc host_data use_device(newnorth)
        call mpi_irecv(newnorth,ct3sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newnorth,ct3sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4

      ! send west
      if(ibw.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nkp1
        do j=1,nj
        do i=1,cmp
          west(i,j,k)=t(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT09
        !$acc host_data use_device(west)
        call mpi_isend(west,ct3we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(west)
        call mpi_isend(west,ct3we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send east
      if(ibe.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nkp1
        do j=1,nj
        do i=1,cmp
          east(i,j,k)=t(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT09
        !$acc host_data use_device(east)
        call mpi_isend(east,ct3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(east)
        call mpi_isend(east,ct3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send north
      if(ibn.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,ni
          north(i,j,k)=t(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT09
        !$acc host_data use_device(north)
        call mpi_isend(north,ct3sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(north)
        call mpi_isend(north,ct3sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send south
      if(ibs.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,ni
          south(i,j,k)=t(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT09
        !$acc host_data use_device(south)
        call mpi_isend(south,ct3sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(south)
        call mpi_isend(south,ct3sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      if(timestats.ge.1) time_mps1=time_mps1+mytime()
      end subroutine comm_3t_start_GPU


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_3t_end(t,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibw,ibe,ibs,ibn, &
          ierr,nkp1,ebc,wbc,sbc,nbc,timestats,time_mps2,time_bc,mytime, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none

      real t(ib:ie,jb:je,kb:ke+1)
      real west(cmp,nj,nk+1),newwest(cmp,nj,nk+1)
      real east(cmp,nj,nk+1),neweast(cmp,nj,nk+1)
      real south(ni,cmp,nk+1),newsouth(ni,cmp,nk+1)
      real north(ni,cmp,nk+1),newnorth(ni,cmp,nk+1)
      integer reqs(8)

      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1

!-------------------------------------------------------------------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_east)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nkp1
        do j=1,nj
        do i=1,cmp
          t(ni+i,j,k)=neweast(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_west)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nkp1
        do j=1,nj
        do i=1,cmp
          t(i-cmp,j,k)=newwest(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_south)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,ni
          t(i,j-cmp,k)=newsouth(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_north)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,ni
          t(i,nj+j,k)=newnorth(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

      if(timestats.ge.1) time_mps2=time_mps2+mytime()

!----------
!  patch for corner

     if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then

        if(ibw.eq.1)then

          if(p2tchsww)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nkp1
              t(0,0,k)=t(1,0,k)
            enddo
          endif

          if(p2tchnww)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nkp1
              t(0,nj+1,k)=t(1,nj+1,k)
            enddo
          endif

        endif

        if(ibe.eq.1)then

          if(p2tchsee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nkp1
              t(ni+1,0,k)=t(ni,0,k)
            enddo
          endif

          if(p2tchnee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nkp1
              t(ni+1,nj+1,k)=t(ni,nj+1,k)
            enddo
          endif

        endif

      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then

        if(ibs.eq.1)then

          if(p2tchsws)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nkp1
              t(0,0,k)=t(0,1,k)
            enddo
          endif

          if(p2tchses)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nkp1
              t(ni+1,0,k)=t(ni+1,1,k)
            enddo
          endif

        endif

        if(ibn.eq.1)then

          if(p2tchnwn)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nkp1
              t(0,nj+1,k)=t(0,nj,k)
            enddo
          endif

          if(p2tchnen)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nkp1
              t(ni+1,nj+1,k)=t(ni+1,nj,k)
            enddo
          endif

        endif

      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mps2=time_mps2+mytime()

      end subroutine comm_3t_end
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_3t_end_GPU(t,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibw,ibe,ibs,ibn, &
          ierr,nkp1,ebc,wbc,sbc,nbc,timestats,time_mps2,time_bc,mytime, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none

      real t(ib:ie,jb:je,kb:ke+1)
      real west(cmp,nj,nk+1),newwest(cmp,nj,nk+1)
      real east(cmp,nj,nk+1),neweast(cmp,nj,nk+1)
      real south(ni,cmp,nk+1),newsouth(ni,cmp,nk+1)
      real north(ni,cmp,nk+1),newnorth(ni,cmp,nk+1)
      integer reqs(8)

      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1
      !$acc declare present(t) &
      !$acc present(west,newwest,east,neweast) &
      !$acc present(south,newsouth,north,newnorth)

!-------------------------------------------------------------------


      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_east)then
#ifndef _GPUDIRECT09
        !$acc update device(neweast)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nkp1
        do j=1,nj
        do i=1,cmp
          t(ni+i,j,k)=neweast(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_west)then
#ifndef _GPUDIRECT09
        !$acc update device(newwest)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nkp1
        do j=1,nj
        do i=1,cmp
          t(i-cmp,j,k)=newwest(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_south)then
#ifndef _GPUDIRECT09
        !$acc update device(newsouth)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,ni
          t(i,j-cmp,k)=newsouth(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_north)then
#ifndef _GPUDIRECT09
        !$acc update device(newnorth)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,ni
          t(i,nj+j,k)=newnorth(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

      if(timestats.ge.1) time_mps2=time_mps2+mytime()

!----------
!  patch for corner

     if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then

        if(ibw.eq.1)then

          if(p2tchsww)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nkp1
              t(0,0,k)=t(1,0,k)
            enddo
          endif

          if(p2tchnww)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nkp1
              t(0,nj+1,k)=t(1,nj+1,k)
            enddo
          endif

        endif

        if(ibe.eq.1)then

          if(p2tchsee)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nkp1
              t(ni+1,0,k)=t(ni,0,k)
            enddo
          endif

          if(p2tchnee)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nkp1
              t(ni+1,nj+1,k)=t(ni,nj+1,k)
            enddo
          endif

        endif

      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then

        if(ibs.eq.1)then

          if(p2tchsws)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nkp1
              t(0,0,k)=t(0,1,k)
            enddo
          endif

          if(p2tchses)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nkp1
              t(ni+1,0,k)=t(ni+1,1,k)
            enddo
          endif

        endif

        if(ibn.eq.1)then

          if(p2tchnwn)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nkp1
              t(0,nj+1,k)=t(0,nj,k)
            enddo
          endif

          if(p2tchnen)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nkp1
              t(ni+1,nj+1,k)=t(ni+1,nj,k)
            enddo
          endif

        endif

      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mps2=time_mps2+mytime()
      end subroutine comm_3t_end_GPU

      ! tk2


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_3u_start(u,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibe,ibw,ibs,ibn, &
          timestats,mytime,nu,time_mpu1,cs3we,cu3sn,ierr,mynorth,mysouth,myeast,mywest
      use mpi
      implicit none
 
      real u(ib:ie+1,jb:je,kb:ke)
      real west(cmp,nj,nk),newwest(cmp,nj,nk)
      real east(cmp,nj,nk),neweast(cmp,nj,nk)
      real south(ni+1,cmp,nk),newsouth(ni+1,cmp,nk)
      real north(ni+1,cmp,nk),newnorth(ni+1,cmp,nk)
      integer reqs(8)
 
      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
 
!------------------------------------------------

      nr = 0

      nu=nu+1
      tag1=1000+nu

      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
        call mpi_irecv(neweast,cs3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      nu=nu+1
      tag2=1000+nu
 
      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
        call mpi_irecv(newwest,cs3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      nu=nu+1
      tag3=1000+nu

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
        call mpi_irecv(newnorth,cu3sn,MPI_REAL,mynorth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      nu=nu+1
      tag4=1000+nu

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
        call mpi_irecv(newsouth,cu3sn,MPI_REAL,mysouth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4
 
      ! send west
      if(ibw.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          west(i,j,k)=u(i+1,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(west,cs3we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------

      ! send east
      if(ibe.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          east(i,j,k)=u(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(east,cs3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------
 
      ! send south
      if(ibs.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni+1
          south(i,j,k)=u(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(south,cu3sn,MPI_REAL,mysouth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      ! send north
      if(ibn.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni+1
         north(i,j,k)=u(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(north,cu3sn,MPI_REAL,mynorth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------

      if(timestats.ge.1) time_mpu1=time_mpu1+mytime()
 
      end subroutine comm_3u_start

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_3u_start_GPU(u,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibe,ibw,ibs,ibn, &
          timestats,mytime,nu,time_mpu1,cs3we,cu3sn,ierr,mynorth,mysouth,myeast,mywest
      use mpi
      implicit none
 
      real u(ib:ie+1,jb:je,kb:ke)
      real west(cmp,nj,nk),newwest(cmp,nj,nk)
      real east(cmp,nj,nk),neweast(cmp,nj,nk)
      real south(ni+1,cmp,nk),newsouth(ni+1,cmp,nk)
      real north(ni+1,cmp,nk),newnorth(ni+1,cmp,nk)
      integer reqs(8)
 
      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
      logical, parameter :: Debug =.FALSE.
      !$acc declare present(u) &
      !$acc present(west,newwest,east,neweast) &
      !$acc present(south,newsouth,north,newnorth)
 
!------------------------------------------------

      nr = 0
#ifndef _GPUDIRECT10
      if(Debug) print *,'comm_3u_start_GPU'
#endif
      nu=nu+1
      tag1=1000+nu
      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT10
        !$acc host_data use_device(neweast)
        call mpi_irecv(neweast,cs3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(neweast,cs3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      nu=nu+1
      tag2=1000+nu
 
      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT10
        !$acc host_data use_device(newwest)
        call mpi_irecv(newwest,cs3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newwest,cs3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      nu=nu+1
      tag3=1000+nu

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT10
        !$acc host_data use_device(newnorth)
        call mpi_irecv(newnorth,cu3sn,MPI_REAL,mynorth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newnorth,cu3sn,MPI_REAL,mynorth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      nu=nu+1
      tag4=1000+nu

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT10
        !$acc host_data use_device(newsouth)
        call mpi_irecv(newsouth,cu3sn,MPI_REAL,mysouth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newsouth,cu3sn,MPI_REAL,mysouth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4
 
      ! send west
      if(ibw.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          west(i,j,k)=u(i+1,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT10
        !$acc host_data use_device(west)
        call mpi_isend(west,cs3we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(west)
        call mpi_isend(west,cs3we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------

      ! send east
      if(ibe.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          east(i,j,k)=u(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT10
        !$acc host_data use_device(east)
        call mpi_isend(east,cs3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(east)
        call mpi_isend(east,cs3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif

      endif

!----------
 
      ! send south
      if(ibs.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni+1
          south(i,j,k)=u(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT10
        !$acc host_data use_device(south)
        call mpi_isend(south,cu3sn,MPI_REAL,mysouth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(south)
        call mpi_isend(south,cu3sn,MPI_REAL,mysouth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      ! send north
      if(ibn.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni+1
          north(i,j,k)=u(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT10
        !$acc host_data use_device(north)
        call mpi_isend(north,cu3sn,MPI_REAL,mynorth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(north)
        call mpi_isend(north,cu3sn,MPI_REAL,mynorth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------

      if(timestats.ge.1) time_mpu1=time_mpu1+mytime()
 
      end subroutine comm_3u_start_GPU


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_3u_end(u,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibe,ibw,ibn,ibs, &
          ebc,wbc,sbc,nbc,ierr,timestats,time_mpu2,mytime,time_bc, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none
 
      real u(ib:ie+1,jb:je,kb:ke)
      real west(cmp,nj,nk),newwest(cmp,nj,nk)
      real east(cmp,nj,nk),neweast(cmp,nj,nk)
      real south(ni+1,cmp,nk),newsouth(ni+1,cmp,nk)
      real north(ni+1,cmp,nk),newnorth(ni+1,cmp,nk)
      integer reqs(8)
 
      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1

!----------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
 
      if(index.eq.index_east)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          u(ni+1+i,j,k)=neweast(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_west)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          u(i-cmp,j,k)=newwest(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_north)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni+1
          u(i,nj+j,k)=newnorth(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_south)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni+1
          u(i,j-cmp,k)=newsouth(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

      if(timestats.ge.1) time_mpu2=time_mpu2+mytime()

!----------
!  patch for corner
 
      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then
 
        if(ibw.eq.1)then
 
          if(p2tchsww)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              u(0,0,k)=u(1,0,k)
            enddo
          endif
 
          if(p2tchnww)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              u(0,nj+1,k)=u(1,nj+1,k)
            enddo
          endif
 
        endif
 
        if(ibe.eq.1)then
 
          if(p2tchsee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              u(ni+2,0,k)=u(ni+1,0,k)
            enddo
          endif
 
          if(p2tchnee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              u(ni+2,nj+1,k)=u(ni+1,nj+1,k)
            enddo
          endif
 
        endif
 
      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then
 
        if(ibs.eq.1)then
 
          if(p2tchsws)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              u(0,0,k)=u(0,1,k)
            enddo
          endif
 
          if(p2tchses)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              u(ni+2,0,k)=u(ni+2,1,k)
            enddo
          endif
 
        endif
 
        if(ibn.eq.1)then
 
          if(p2tchnwn)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              u(0,nj+1,k)=u(0,nj,k)
            enddo
          endif
 
          if(p2tchnen)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              u(ni+2,nj+1,k)=u(ni+2,nj,k)
            enddo
          endif
 
        endif
 
      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mpu2=time_mpu2+mytime()
 
!----------
 
      end subroutine comm_3u_end

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_3u_end_GPU(u,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibe,ibw,ibn,ibs, &
          ebc,wbc,sbc,nbc,ierr,timestats,time_mpu2,mytime,time_bc, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none
 
      real u(ib:ie+1,jb:je,kb:ke)
      real west(cmp,nj,nk),newwest(cmp,nj,nk)
      real east(cmp,nj,nk),neweast(cmp,nj,nk)
      real south(ni+1,cmp,nk),newsouth(ni+1,cmp,nk)
      real north(ni+1,cmp,nk),newnorth(ni+1,cmp,nk)
      integer reqs(8)
 
      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1
      !$acc declare present(u) &
      !$acc present(west,newwest,east,neweast) &
      !$acc present(south,newsouth,north,newnorth)

!----------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1
      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
 
      if(index.eq.index_east)then
#ifndef _GPUDIRECT10
        !$acc update device(neweast)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          u(ni+1+i,j,k)=neweast(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_west)then
#ifndef _GPUDIRECT10
        !$acc update device(newwest)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,cmp
          u(i-cmp,j,k)=newwest(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_north)then
#ifndef _GPUDIRECT10
        !$acc update device(newnorth)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni+1
          u(i,nj+j,k)=newnorth(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_south)then
#ifndef _GPUDIRECT10
        !$acc update device(newsouth)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni+1
          u(i,j-cmp,k)=newsouth(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

      if(timestats.ge.1) time_mpu2=time_mpu2+mytime()

!----------
!  patch for corner
 
      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then
 
        if(ibw.eq.1)then
 
          if(p2tchsww)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              u(0,0,k)=u(1,0,k)
            enddo
          endif
 
          if(p2tchnww)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              u(0,nj+1,k)=u(1,nj+1,k)
            enddo
          endif
 
        endif
 
        if(ibe.eq.1)then
 
          if(p2tchsee)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              u(ni+2,0,k)=u(ni+1,0,k)
            enddo
          endif
 
          if(p2tchnee)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              u(ni+2,nj+1,k)=u(ni+1,nj+1,k)
            enddo
          endif
 
        endif
 
      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then
 
        if(ibs.eq.1)then
 
          if(p2tchsws)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              u(0,0,k)=u(0,1,k)
            enddo
          endif
 
          if(p2tchses)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              u(ni+2,0,k)=u(ni+2,1,k)
            enddo
          endif
 
        endif
 
        if(ibn.eq.1)then
 
          if(p2tchnwn)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              u(0,nj+1,k)=u(0,nj,k)
            enddo
          endif
 
          if(p2tchnen)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              u(ni+2,nj+1,k)=u(ni+2,nj,k)
            enddo
          endif
 
        endif
 
      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mpu2=time_mpu2+mytime()
!----------
 
      end subroutine comm_3u_end_GPU


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_3v_start(v,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibe,ibw,ibs,ibn, &
          wbc,ebc,sbc,nbc,timestats,mytime,nv,time_mpv1,cv3we,cs3sn,ierr, &
          myeast,mywest,mysouth,mynorth
      use mpi
      implicit none
 
      real v(ib:ie,jb:je+1,kb:ke)
      real west(cmp,nj+1,nk),newwest(cmp,nj+1,nk)
      real east(cmp,nj+1,nk),neweast(cmp,nj+1,nk)
      real south(ni,cmp,nk),newsouth(ni,cmp,nk)
      real north(ni,cmp,nk),newnorth(ni,cmp,nk)
      integer reqs(8)
 
      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
 
!------------------------------------------------

      nr = 0

      nv=nv+1
      tag1=2000+nv

      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
        call mpi_irecv(neweast,cv3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      nv=nv+1
      tag2=2000+nv
 
      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
        call mpi_irecv(newwest,cv3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      nv=nv+1
      tag3=2000+nv
 
      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
        call mpi_irecv(newnorth,cs3sn,MPI_REAL,mynorth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      nv=nv+1
      tag4=2000+nv
 
      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
        call mpi_irecv(newsouth,cs3sn,MPI_REAL,mysouth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4
 
      ! send west
      if(ibw.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,cmp
          west(i,j,k)=v(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(west,cv3we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------

      if(ibe.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,cmp
          east(i,j,k)=v(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(east,cv3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      ! send south
      if(ibs.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          south(i,j,k)=v(i,j+1,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(south,cs3sn,MPI_REAL,mysouth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------

      ! send north
      if(ibn.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          north(i,j,k)=v(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(north,cs3sn,MPI_REAL,mynorth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------

      if(timestats.ge.1) time_mpv1=time_mpv1+mytime()
 
      end subroutine comm_3v_start

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_3v_start_GPU(v,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibe,ibw,ibs,ibn, &
          wbc,ebc,sbc,nbc,timestats,mytime,nv,time_mpv1,cv3we,cs3sn,ierr, &
          myeast,mywest,mysouth,mynorth
      use mpi
      implicit none
 
      real v(ib:ie,jb:je+1,kb:ke)
      real west(cmp,nj+1,nk),newwest(cmp,nj+1,nk)
      real east(cmp,nj+1,nk),neweast(cmp,nj+1,nk)
      real south(ni,cmp,nk),newsouth(ni,cmp,nk)
      real north(ni,cmp,nk),newnorth(ni,cmp,nk)
      integer reqs(8)
 
      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
      logical, parameter :: Debug =.FALSE.
      !$acc declare present(v) &
      !$acc present(west,newwest,east,neweast) &
      !$acc present(south,newsouth,north,newnorth)
 
!------------------------------------------------

      nr = 0

#ifndef _GPUDIRECT11
      if(Debug) print *,'comm_3v_start_GPU'
#endif
      nv=nv+1
      tag1=2000+nv
      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT11
        !$acc host_data use_device(neweast)
        call mpi_irecv(neweast,cv3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(neweast,cv3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      nv=nv+1
      tag2=2000+nv
 
      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT11
        !$acc host_data use_device(newwest)
        call mpi_irecv(newwest,cv3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newwest,cv3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      nv=nv+1
      tag3=2000+nv
 
      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT11
        !$acc host_data use_device(newnorth)
        call mpi_irecv(newnorth,cs3sn,MPI_REAL,mynorth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newnorth,cs3sn,MPI_REAL,mynorth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      nv=nv+1
      tag4=2000+nv
 
      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT11
        !$acc host_data use_device(newsouth)
        call mpi_irecv(newsouth,cs3sn,MPI_REAL,mysouth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newsouth,cs3sn,MPI_REAL,mysouth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4
 
      ! send west
      if(ibw.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,cmp
          west(i,j,k)=v(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT11
        !$acc host_data use_device(west)
        call mpi_isend(west,cv3we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(west)
        call mpi_isend(west,cv3we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------

      if(ibe.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,cmp
          east(i,j,k)=v(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT11
        !$acc host_data use_device(east)
        call mpi_isend(east,cv3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(east)
        call mpi_isend(east,cv3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send south
      if(ibs.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          south(i,j,k)=v(i,j+1,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT11
        !$acc host_data use_device(south)
        call mpi_isend(south,cs3sn,MPI_REAL,mysouth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(south)
        call mpi_isend(south,cs3sn,MPI_REAL,mysouth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------

      ! send north
      if(ibn.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          north(i,j,k)=v(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT11
        !$acc host_data use_device(north)
        call mpi_isend(north,cs3sn,MPI_REAL,mynorth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(north)
        call mpi_isend(north,cs3sn,MPI_REAL,mynorth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------

      if(timestats.ge.1) time_mpv1=time_mpv1+mytime()
 
      end subroutine comm_3v_start_GPU

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
      subroutine comm_3v_end(v,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibe,ibw,ibn,ibs, &
          ebc,wbc,sbc,nbc,ierr,timestats,time_mpv2,mytime,time_bc, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none
 
      real v(ib:ie,jb:je+1,kb:ke)
      real west(cmp,nj+1,nk),newwest(cmp,nj+1,nk)
      real east(cmp,nj+1,nk),neweast(cmp,nj+1,nk)
      real south(ni,cmp,nk),newsouth(ni,cmp,nk)
      real north(ni,cmp,nk),newnorth(ni,cmp,nk)
      integer reqs(8)
 
      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1
 
!--------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
 
      if(index.eq.index_east)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,cmp
          v(ni+i,j,k)=neweast(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_west)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,cmp
          v(i-cmp,j,k)=newwest(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_north)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          v(i,nj+1+j,k)=newnorth(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_south)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          v(i,j-cmp,k)=newsouth(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

      if(timestats.ge.1) time_mpv2=time_mpv2+mytime()

!----------
!  patch for corner
 
      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then
 
        if(ibw.eq.1)then
 
          if(p2tchsww)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              v(0,0,k)=v(1,0,k)
            enddo
          endif
 
          if(p2tchnww)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              v(0,nj+2,k)=v(1,nj+2,k)
            enddo
          endif
 
        endif
 
        if(ibe.eq.1)then
 
          if(p2tchsee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              v(ni+1,0,k)=v(ni,0,k)
            enddo
          endif
 
          if(p2tchnee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              v(ni+1,nj+2,k)=v(ni,nj+2,k)
            enddo
          endif
 
        endif
 
      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then
 
        if(ibs.eq.1)then
 
          if(p2tchsws)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              v(0,0,k)=v(0,1,k)
            enddo
          endif
 
          if(p2tchses)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              v(ni+1,0,k)=v(ni+1,1,k)
            enddo
          endif
 
        endif
 
        if(ibn.eq.1)then
 
          if(p2tchnwn)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              v(0,nj+2,k)=v(0,nj+1,k)
            enddo
          endif
 
          if(p2tchnen)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              v(ni+1,nj+2,k)=v(ni+1,nj+1,k)
            enddo
          endif
 
        endif
 
      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!--------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mpv2=time_mpv2+mytime()
 
!----------
 
      end subroutine comm_3v_end


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
      subroutine comm_3v_end_GPU(v,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibe,ibw,ibn,ibs, &
          ebc,wbc,sbc,nbc,ierr,timestats,time_mpv2,mytime,time_bc, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none
 
      real v(ib:ie,jb:je+1,kb:ke)
      real west(cmp,nj+1,nk),newwest(cmp,nj+1,nk)
      real east(cmp,nj+1,nk),neweast(cmp,nj+1,nk)
      real south(ni,cmp,nk),newsouth(ni,cmp,nk)
      real north(ni,cmp,nk),newnorth(ni,cmp,nk)
      integer reqs(8)
 
      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1
      !$acc declare present(v) &
      !$acc present(west,newwest,east,neweast) &
      !$acc present(south,newsouth,north,newnorth)
 
!--------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1
      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
 
      if(index.eq.index_east)then
#ifndef _GPUDIRECT11
        !$acc update device(neweast)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,cmp
          v(ni+i,j,k)=neweast(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_west)then
#ifndef _GPUDIRECT11
        !$acc update device(newwest)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,cmp
          v(i-cmp,j,k)=newwest(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_north)then
#ifndef _GPUDIRECT11
        !$acc update device(newnorth)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          v(i,nj+1+j,k)=newnorth(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_south)then
#ifndef _GPUDIRECT11
        !$acc update device(newsouth)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,ni
          v(i,j-cmp,k)=newsouth(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

      if(timestats.ge.1) time_mpv2=time_mpv2+mytime()

!----------
!  patch for corner
 
      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then
 
        if(ibw.eq.1)then
 
          if(p2tchsww)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              v(0,0,k)=v(1,0,k)
            enddo
          endif
 
          if(p2tchnww)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              v(0,nj+2,k)=v(1,nj+2,k)
            enddo
          endif
 
        endif
 
        if(ibe.eq.1)then
 
          if(p2tchsee)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              v(ni+1,0,k)=v(ni,0,k)
            enddo
          endif
 
          if(p2tchnee)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              v(ni+1,nj+2,k)=v(ni,nj+2,k)
            enddo
          endif
 
        endif
 
      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then
 
        if(ibs.eq.1)then
 
          if(p2tchsws)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              v(0,0,k)=v(0,1,k)
            enddo
          endif
 
          if(p2tchses)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              v(ni+1,0,k)=v(ni+1,1,k)
            enddo
          endif
 
        endif
 
        if(ibn.eq.1)then
 
          if(p2tchnwn)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              v(0,nj+2,k)=v(0,nj+1,k)
            enddo
          endif
 
          if(p2tchnen)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              v(ni+1,nj+2,k)=v(ni+1,nj+1,k)
            enddo
          endif
 
        endif
 
      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!--------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mpv2=time_mpv2+mytime()
!      !$acc update device(v) 
!----------
 
      end subroutine comm_3v_end_GPU


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_3w_start(w,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibe,ibw,ibs,ibn, &
          ierr,timestats,mytime,nw,time_mpw1,cw3we,cw3sn,mywest,myeast, & 
          mysouth,mynorth
      use mpi
      implicit none
 
      real w(ib:ie,jb:je,kb:ke+1)
      real west(cmp,nj,nk-1),newwest(cmp,nj,nk-1)
      real east(cmp,nj,nk-1),neweast(cmp,nj,nk-1)
      real south(ni,cmp,nk-1),newsouth(ni,cmp,nk-1)
      real north(ni,cmp,nk-1),newnorth(ni,cmp,nk-1)
      integer reqs(8)
 
      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
 
!------------------------------------------------

      nr = 0

      nw=nw+1
      tag1=3000+nw

      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
        call mpi_irecv(neweast,cw3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      nw=nw+1
      tag2=3000+nw
 
      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
        call mpi_irecv(newwest,cw3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------

      nw=nw+1
      tag3=3000+nw
 
      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
        call mpi_irecv(newnorth,cw3sn,MPI_REAL,mynorth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      nw=nw+1
      tag4=3000+nw
 
      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
        call mpi_irecv(newsouth,cw3sn,MPI_REAL,mysouth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4
 
      ! send west
      if(ibw.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,cmp
          west(i,j,k-1)=w(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(west,cw3we,MPI_REAL,mywest,tag1,    &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------

      ! send east
      if(ibe.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,cmp
          east(i,j,k-1)=w(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(east,cw3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      ! send south
      if(ibs.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=2,nk
        do j=1,cmp
        do i=1,ni
          south(i,j,k-1)=w(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(south,cw3sn,MPI_REAL,mysouth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------

      ! send north
      if(ibn.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=2,nk
        do j=1,cmp
        do i=1,ni
          north(i,j,k-1)=w(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(north,cw3sn,MPI_REAL,mynorth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------

      if(timestats.ge.1) time_mpw1=time_mpw1+mytime()
 
      end subroutine comm_3w_start

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_3w_start_GPU(w,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibe,ibw,ibs,ibn, &
          ierr,timestats,mytime,nw,time_mpw1,cw3we,cw3sn,mywest,myeast, &
          mysouth,mynorth
      use mpi
      implicit none
 
      real w(ib:ie,jb:je,kb:ke+1)
      real west(cmp,nj,nk-1),newwest(cmp,nj,nk-1)
      real east(cmp,nj,nk-1),neweast(cmp,nj,nk-1)
      real south(ni,cmp,nk-1),newsouth(ni,cmp,nk-1)
      real north(ni,cmp,nk-1),newnorth(ni,cmp,nk-1)
      integer reqs(8)
 
      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
      logical, parameter :: Debug =.FALSE.
      !$acc declare present(w) &
      !$acc present(west,newwest,east,neweast) &
      !$acc present(south,newsouth,north,newnorth)
 
!------------------------------------------------

      nr = 0
#ifndef _GPUDIRECT12
      if(Debug) print *,'comm_3w_start_GPU:'
#endif
      nw=nw+1
      tag1=3000+nw
      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT12
        !$acc host_data use_device(neweast)
        call mpi_irecv(neweast,cw3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(neweast,cw3we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      nw=nw+1
      tag2=3000+nw
 
      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT12
        !$acc host_data use_device(newwest)
        call mpi_irecv(newwest,cw3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newwest,cw3we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------

      nw=nw+1
      tag3=3000+nw
 
      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT12
        !$acc host_data use_device(newnorth)
        call mpi_irecv(newnorth,cw3sn,MPI_REAL,mynorth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newnorth,cw3sn,MPI_REAL,mynorth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      nw=nw+1
      tag4=3000+nw
 
      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT12
        !$acc host_data use_device(newsouth)
        call mpi_irecv(newsouth,cw3sn,MPI_REAL,mysouth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newsouth,cw3sn,MPI_REAL,mysouth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4
 
      ! send west
      if(ibw.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,cmp
          west(i,j,k-1)=w(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT12
        !$acc host_data use_device(west)
        call mpi_isend(west,cw3we,MPI_REAL,mywest,tag1,    &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(west)
        call mpi_isend(west,cw3we,MPI_REAL,mywest,tag1,    &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------

      ! send east
      if(ibe.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,cmp
          east(i,j,k-1)=w(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT12
        !$acc host_data use_device(east)
        call mpi_isend(east,cw3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(east)
        call mpi_isend(east,cw3we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send south
      if(ibs.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=2,nk
        do j=1,cmp
        do i=1,ni
          south(i,j,k-1)=w(i,j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT12
        !$acc host_data use_device(south)
        call mpi_isend(south,cw3sn,MPI_REAL,mysouth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(south)
        call mpi_isend(south,cw3sn,MPI_REAL,mysouth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------

      ! send north
      if(ibn.eq.0)then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=2,nk
        do j=1,cmp
        do i=1,ni
          north(i,j,k-1)=w(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT12
        !$acc host_data use_device(north)
        call mpi_isend(north,cw3sn,MPI_REAL,mynorth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(north)
        call mpi_isend(north,cw3sn,MPI_REAL,mynorth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------

      if(timestats.ge.1) time_mpw1=time_mpw1+mytime()
 
      end subroutine comm_3w_start_GPU

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_3w_end(w,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibe,ibw,ibn,ibs, &
          wbc,ebc,nbc,sbc,ierr,mytime,timestats,time_mpw2,time_bc, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none
 
      real w(ib:ie,jb:je,kb:ke+1)
      real west(cmp,nj,nk-1),newwest(cmp,nj,nk-1)
      real east(cmp,nj,nk-1),neweast(cmp,nj,nk-1)
      real south(ni,cmp,nk-1),newsouth(ni,cmp,nk-1)
      real north(ni,cmp,nk-1),newnorth(ni,cmp,nk-1)
      integer reqs(8)
 
      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1

!--------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
 
      if(index.eq.index_east)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,cmp
          w(ni+i,j,k)=neweast(i,j,k-1)
        enddo
        enddo
        enddo
      elseif(index.eq.index_west)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,cmp
          w(i-cmp,j,k)=newwest(i,j,k-1)
        enddo
        enddo
        enddo
      elseif(index.eq.index_north)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=2,nk
        do j=1,cmp
        do i=1,ni
          w(i,nj+j,k)=newnorth(i,j,k-1)
        enddo
        enddo
        enddo
      elseif(index.eq.index_south)then
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        do k=2,nk
        do j=1,cmp
        do i=1,ni
          w(i,j-cmp,k)=newsouth(i,j,k-1)
        enddo
        enddo
        enddo
      endif

      enddo
 
      if(timestats.ge.1) time_mpw2=time_mpw2+mytime()

!----------
!  patch for corner
 
      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then
 
        if(ibw.eq.1)then
 
          if(p2tchsww)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=2,nk
              w(0,0,k)=w(1,0,k)
            enddo
          endif
 
          if(p2tchnww)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=2,nk
              w(0,nj+1,k)=w(1,nj+1,k)
            enddo
          endif
 
        endif
 
        if(ibe.eq.1)then
 
          if(p2tchsee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=2,nk
              w(ni+1,0,k)=w(ni,0,k)
            enddo
          endif
 
          if(p2tchnee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=2,nk
              w(ni+1,nj+1,k)=w(ni,nj+1,k)
            enddo
          endif
 
        endif
 
      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then
 
        if(ibs.eq.1)then
 
          if(p2tchsws)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=2,nk
              w(0,0,k)=w(0,1,k)
            enddo
          endif
 
          if(p2tchses)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=2,nk
              w(ni+1,0,k)=w(ni+1,1,k)
            enddo
          endif
 
        endif
 
        if(ibn.eq.1)then
 
          if(p2tchnwn)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=2,nk
              w(0,nj+1,k)=w(0,nj,k)
            enddo
          endif
 
          if(p2tchnen)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=2,nk
              w(ni+1,nj+1,k)=w(ni+1,nj,k)
            enddo
          endif
 
        endif
 
      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!--------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mpw2=time_mpw2+mytime()
 
!----------
 
      end subroutine comm_3w_end

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_3w_end_GPU(w,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibe,ibw,ibn,ibs, &
          wbc,ebc,nbc,sbc,ierr,mytime,timestats,time_mpw2,time_bc, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none
 
      real w(ib:ie,jb:je,kb:ke+1)
      real west(cmp,nj,nk-1),newwest(cmp,nj,nk-1)
      real east(cmp,nj,nk-1),neweast(cmp,nj,nk-1)
      real south(ni,cmp,nk-1),newsouth(ni,cmp,nk-1)
      real north(ni,cmp,nk-1),newnorth(ni,cmp,nk-1)
      integer reqs(8)
 
      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1
      !$acc declare present(w) &
      !$acc present(west,newwest,east,neweast) &
      !$acc present(south,newsouth,north,newnorth)

!--------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1
      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
 
      if(index.eq.index_east)then
#ifndef _GPUDIRECT12
        !$acc update device(neweast)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,cmp
          w(ni+i,j,k)=neweast(i,j,k-1)
        enddo
        enddo
        enddo
      elseif(index.eq.index_west)then
#ifndef _GPUDIRECT12
        !$acc update device(newwest)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,cmp
          w(i-cmp,j,k)=newwest(i,j,k-1)
        enddo
        enddo
        enddo
      elseif(index.eq.index_north)then
#ifndef _GPUDIRECT12
        !$acc update device(newnorth)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=2,nk
        do j=1,cmp
        do i=1,ni
          w(i,nj+j,k)=newnorth(i,j,k-1)
        enddo
        enddo
        enddo
      elseif(index.eq.index_south)then
#ifndef _GPUDIRECT12
        !$acc update device(newsouth)
#endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=2,nk
        do j=1,cmp
        do i=1,ni
          w(i,j-cmp,k)=newsouth(i,j,k-1)
        enddo
        enddo
        enddo
      endif

      enddo
 
      if(timestats.ge.1) time_mpw2=time_mpw2+mytime()

!----------
!  patch for corner
 
      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then
 
        if(ibw.eq.1)then
 
          if(p2tchsww)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(0,0,k)=w(1,0,k)
            enddo
          endif
 
          if(p2tchnww)then
!$omp parallel do default(shared)   &
!$omp private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(0,nj+1,k)=w(1,nj+1,k)
            enddo
          endif
 
        endif
 
        if(ibe.eq.1)then
 
          if(p2tchsee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(ni+1,0,k)=w(ni,0,k)
            enddo
          endif
 
          if(p2tchnee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(ni+1,nj+1,k)=w(ni,nj+1,k)
            enddo
          endif
 
        endif
 
      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then
 
        if(ibs.eq.1)then
 
          if(p2tchsws)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(0,0,k)=w(0,1,k)
            enddo
          endif
 
          if(p2tchses)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(ni+1,0,k)=w(ni+1,1,k)
            enddo
          endif
 
        endif
 
        if(ibn.eq.1)then
 
          if(p2tchnwn)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(0,nj+1,k)=w(0,nj,k)
            enddo
          endif
 
          if(p2tchnen)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(ni+1,nj+1,k)=w(ni+1,nj,k)
            enddo
          endif
 
        endif
 
      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!--------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mpw2=time_mpw2+mytime()
!----------
 
      end subroutine comm_3w_end_GPU


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_1s2d_start(s,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,ni,nj,ibe,ibw,ibs,ibn,wbc,ebc,sbc,nbc, &
          nf,ierr,timestats,time_mps1,mytime,myeast,mywest,mysouth,mynorth
      use mpi
      implicit none
 
      real s(ib:ie,jb:je)
      real west(nj),newwest(nj)
      real east(nj),neweast(nj)
      real south(ni),newsouth(ni)
      real north(ni),newnorth(ni)
      integer reqs(8)
 
      integer i,j,nr
      integer tag1,tag2,tag3,tag4
 
!------------------------------------------------

      nr = 0

      nf=nf+1
      tag1=nf

      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
        call mpi_irecv(neweast,nj,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
        call mpi_irecv(newwest,nj,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      nf=nf+1
      tag3=nf

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
        call mpi_irecv(newsouth,ni,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
        call mpi_irecv(newnorth,ni,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4
 
      ! send west
      if(ibw.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(j)
        do j=1,nj
          west(j)=s(1,j)
        enddo
        nr = nr + 1
        call mpi_isend(west,nj,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      ! send east
      if(ibe.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(j)
        do j=1,nj
          east(j)=s(ni,j)
        enddo
        nr = nr + 1
        call mpi_isend(east,nj,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      ! send north
      if(ibn.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i)
        do i=1,ni
          north(i)=s(i,nj)
        enddo
        nr = nr + 1
        call mpi_isend(north,ni,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      ! send south
      if(ibs.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i)
        do i=1,ni
          south(i)=s(i,1)
        enddo
        nr = nr + 1
        call mpi_isend(south,ni,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------
 
      if(timestats.ge.1) time_mps1=time_mps1+mytime()
 
      end subroutine comm_1s2d_start


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_1s2d_start_GPU(s,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,ni,nj,ibe,ibw,ibs,ibn,wbc,ebc,sbc,nbc, &
          nf,ierr,timestats,time_mps1,mytime,myeast,mywest,mysouth,mynorth
      use mpi
      implicit none

      real s(ib:ie,jb:je)
      real west(nj),newwest(nj)
      real east(nj),neweast(nj)
      real south(ni),newsouth(ni)
      real north(ni),newnorth(ni)
      integer reqs(8)

      integer i,j,nr
      integer tag1,tag2,tag3,tag4
      logical, parameter :: Debug =.FALSE.

!------------------------------------------------

      nr = 0
#ifndef _GPUDIRECT13
      if(Debug) print *,'comm_1s2d_start_GPU:'
#endif
      nf=nf+1
      tag1=nf
      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT13
        !$acc host_data use_device(neweast)
        call mpi_irecv(neweast,nj,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(neweast,nj,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT13
        !$acc host_data use_device(newwest)
        call mpi_irecv(newwest,nj,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newwest,nj,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag3=nf

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT13
        !$acc host_data use_device(newsouth)
        call mpi_irecv(newsouth,ni,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newsouth,ni,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT13
        !$acc host_data use_device(newnorth)
        call mpi_irecv(newnorth,ni,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newnorth,ni,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4

      ! send west
      if(ibw.eq.0)then
        !$omp parallel do default(shared) private(j)
        !$acc parallel loop gang vector default(present) 
        do j=1,nj
          west(j)=s(1,j)
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT13
        !$acc host_data use_device(west)
        call mpi_isend(west,nj,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(west)
        call mpi_isend(west,nj,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send east
      if(ibe.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(j)
        !$acc parallel loop gang vector default(present)
        do j=1,nj
          east(j)=s(ni,j)
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT13
        !$acc host_data use_device(east)
        call mpi_isend(east,nj,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(east)
        call mpi_isend(east,nj,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send north
      if(ibn.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i)
        !$acc parallel loop gang vector default(present)
        do i=1,ni
          north(i)=s(i,nj)
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT13
        !$acc host_data use_device(north)
        call mpi_isend(north,ni,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(north)
        call mpi_isend(north,ni,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send south
      if(ibs.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i)
        !$acc parallel loop gang vector default(present)
        do i=1,ni
          south(i)=s(i,1)
        enddo
        nr = nr + 1

#ifdef _GPUDIRECT13
        !$acc host_data use_device(south)
        call mpi_isend(south,ni,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(south)
        call mpi_isend(south,ni,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      if(timestats.ge.1) time_mps1=time_mps1+mytime()

      end subroutine comm_1s2d_start_GPU


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_1s2d_end(s,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,ni,nj,ibe,ibw,ibs,ibn, &
          wbc,ebc,nbc,sbc,timestats,mytime,time_mps2,time_bc,ierr, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none
 
      real s(ib:ie,jb:je)
      real west(nj),newwest(nj)
      real east(nj),neweast(nj)
      real south(ni),newsouth(ni)
      real north(ni),newnorth(ni)
      integer reqs(8)
 
      integer i,j,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1

!---------------------------------------------------------------------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
 
      if(index.eq.index_east)then
        !$omp parallel do default(shared) private(j)
        do j=1,nj
          s(ni+1,j)=neweast(j)
        enddo
      elseif(index.eq.index_west)then
        !$omp parallel do default(shared) private(j)
        do j=1,nj
          s(0,j)=newwest(j)
        enddo
      elseif(index.eq.index_south)then
        !$omp parallel do default(shared) private(i)
        do i=1,ni
          s(i,0)=newsouth(i)
        enddo
      elseif(index.eq.index_north)then
        !$omp parallel do default(shared) private(i)
        do i=1,ni
          s(i,nj+1)=newnorth(i)
        enddo
      endif

      enddo

      if(timestats.ge.1) time_mps2=time_mps2+mytime()

!----------
!  patch for corner
 
      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then
 
        if(ibw.eq.1)then
 
          if(p2tchsww)then
              s(0,0)=s(1,0)
          endif
 
          if(p2tchnww)then
              s(0,nj+1)=s(1,nj+1)
          endif
 
        endif
 
        if(ibe.eq.1)then
 
          if(p2tchsee)then
              s(ni+1,0)=s(ni,0)
          endif
 
          if(p2tchnee)then
              s(ni+1,nj+1)=s(ni,nj+1)
          endif
 
        endif
 
      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then
 
        if(ibs.eq.1)then
 
          if(p2tchsws)then
              s(0,0)=s(0,1)
          endif
 
          if(p2tchses)then
              s(ni+1,0)=s(ni+1,1)
          endif
 
        endif
 
        if(ibn.eq.1)then
 
          if(p2tchnwn)then
              s(0,nj+1)=s(0,nj)
          endif
 
          if(p2tchnen)then
              s(ni+1,nj+1)=s(ni+1,nj)
          endif
 
        endif
 
      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mps2=time_mps2+mytime()
 
!----------
 
      end subroutine comm_1s2d_end


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_1s2d_end_GPU(s,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,ni,nj,ibe,ibw,ibs,ibn, &
          wbc,ebc,nbc,sbc,timestats,mytime,time_mps2,time_bc,ierr, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none

      real s(ib:ie,jb:je)
      real west(nj),newwest(nj)
      real east(nj),neweast(nj)
      real south(ni),newsouth(ni)
      real north(ni),newnorth(ni)
      integer reqs(8)

      integer i,j,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1

!---------------------------------------------------------------------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_east)then
#ifndef _GPUDIRECT13
        !$acc update device(neweast)
#endif
        !$omp parallel do default(shared) private(j)
        !$acc parallel loop default(present) private(j)
        do j=1,nj
          s(ni+1,j)=neweast(j)
        enddo
      elseif(index.eq.index_west)then
#ifndef _GPUDIRECT13
        !$acc update device(newwest)
#endif
        !$omp parallel do default(shared) private(j)
        !$acc parallel loop default(present) private(j)
        do j=1,nj
          s(0,j)=newwest(j)
        enddo
      elseif(index.eq.index_south)then
#ifndef _GPUDIRECT13
        !$acc update device(newsouth)
#endif
        !$omp parallel do default(shared) private(i)
        !$acc parallel loop default(present) private(i)
        do i=1,ni
          s(i,0)=newsouth(i)
        enddo
      elseif(index.eq.index_north)then
#ifndef _GPUDIRECT13
        !$acc update device(newnorth)
#endif
        !$omp parallel do default(shared) private(i)
        !$acc parallel loop default(present) private(i)
        do i=1,ni
          s(i,nj+1)=newnorth(i)
        enddo
      endif

      enddo

      if(timestats.ge.1) time_mps2=time_mps2+mytime()

      !$acc parallel default(present)
!----------
!  patch for corner
      !print *,'ibw: ',ibw
      !print *,'p2tchsww: ',p2tchsww
      !print *,'p2tchnww: ',p2tchnww
      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then

        if(ibw.eq.1)then

          if(p2tchsww)then
              s(0,0)=s(1,0)
          endif

          if(p2tchnww)then
              s(0,nj+1)=s(1,nj+1)
          endif

        endif

        if(ibe.eq.1)then

          if(p2tchsee)then
              s(ni+1,0)=s(ni,0)
          endif

          if(p2tchnee)then
              s(ni+1,nj+1)=s(ni,nj+1)
          endif

        endif

      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then

        if(ibs.eq.1)then

          if(p2tchsws)then
              s(0,0)=s(0,1)
          endif

          if(p2tchses)then
              s(ni+1,0)=s(ni+1,1)
          endif

        endif

        if(ibn.eq.1)then

          if(p2tchnwn)then
              s(0,nj+1)=s(0,nj)
          endif

          if(p2tchnen)then
              s(ni+1,nj+1)=s(ni+1,nj)
          endif

        endif

      endif
      !$acc end parallel

      if(timestats.ge.1) time_bc=time_bc+mytime()

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mps2=time_mps2+mytime()
      !stop 'comm_1s2d_end_GPU:'
!----------

      end subroutine comm_1s2d_end_GPU


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine comm_1s_start(s,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibe,ibw,ibs,ibn, &
          wbc,ebc,sbc,nbc,nf,cs1we,cs1sn,ierr,timestats,time_mps1,mytime, & 
          myeast,mywest,mysouth,mynorth
      use mpi
      implicit none
 
      real s(ib:ie,jb:je,kb:ke)
      real west(nj,nk),newwest(nj,nk)
      real east(nj,nk),neweast(nj,nk)
      real south(ni,nk),newsouth(ni,nk)
      real north(ni,nk),newnorth(ni,nk)
      integer reqs(8)
 
      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
 
!------------------------------------------------

      nr = 0

      nf=nf+1
      tag1=nf

      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
        call mpi_irecv(neweast,cs1we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
        call mpi_irecv(newwest,cs1we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      nf=nf+1
      tag3=nf

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
        call mpi_irecv(newsouth,cs1sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
        call mpi_irecv(newnorth,cs1sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4
 
      ! send west
      if(ibw.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(j,k)
        do k=1,nk
        do j=1,nj
          west(j,k)=s(1,j,k)
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(west,cs1we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      ! send east
      if(ibe.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(j,k)
        do k=1,nk
        do j=1,nj
          east(j,k)=s(ni,j,k)
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(east,cs1we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      ! send north
      if(ibn.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,k)
        do k=1,nk
        do i=1,ni
          north(i,k)=s(i,nj,k)
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(north,cs1sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif
 
!----------
 
      ! send south
      if(ibs.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,k)
        do k=1,nk
        do i=1,ni
          south(i,k)=s(i,1,k)
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(south,cs1sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------
 
      if(timestats.ge.1) time_mps1=time_mps1+mytime()
 
      end subroutine comm_1s_start


      subroutine comm_1s_start_GPU(s,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibe,ibw,ibs,ibn, &
          wbc,ebc,sbc,nbc,nf,cs1we,cs1sn,ierr,timestats,time_mps1,mytime, & 
          myeast,mywest,mysouth,mynorth
      use mpi
      implicit none
 
      real s(ib:ie,jb:je,kb:ke)
      real west(nj,nk),newwest(nj,nk)
      real east(nj,nk),neweast(nj,nk)
      real south(ni,nk),newsouth(ni,nk)
      real north(ni,nk),newnorth(ni,nk)
      integer reqs(8)
      !$acc declare present(s) &
      !$acc present(west,newwest,east,neweast) &
      !$acc present(south,newsouth,north,newnorth)
 
      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
      logical, parameter :: Debug =.FALSE.
 
!------------------------------------------------

      nr = 0
#ifndef _GPUDIRECT14
      if(Debug) print *,'comm_1s_start_GPU:'
#endif
      nf=nf+1
      tag1=nf
      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT14
        if(Debug) print *,'comm_1s_start_GPU: point #1'
        !$acc host_data use_device(neweast)
        call mpi_irecv(neweast,cs1we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(neweast,cs1we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT14
        if(Debug) print *,'comm_1s_start_GPU: point #2'
        !$acc host_data use_device(newwest)
        call mpi_irecv(newwest,cs1we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newwest,cs1we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag3=nf

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT14
        if(Debug) print *,'comm_1s_start_GPU: point #3'
        !$acc host_data use_device(newsouth)
        call mpi_irecv(newsouth,cs1sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newsouth,cs1sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT14
        if(Debug) print *,'comm_1s_start_GPU: point #4'
        !$acc host_data use_device(newnorth)
        call mpi_irecv(newnorth,cs1sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newnorth,cs1sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4
 
      ! send west
      if(ibw.eq.0)then
        !$omp parallel do default(shared) private(j,k)
        !$acc parallel loop gang vector default(present) collapse(2) private(j,k)
        do k=1,nk
        do j=1,nj
          west(j,k)=s(1,j,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT14
        if(Debug) print *,'comm_1s_start_GPU: point #5'
        !$acc host_data use_device(west)
        call mpi_isend(west,cs1we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(west)
        call mpi_isend(west,cs1we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      ! send east
      if(ibe.eq.0)then
        !$omp parallel do default(shared) private(j,k)
        !$acc parallel loop gang vector default(present) collapse(2) private(j,k)
        do k=1,nk
        do j=1,nj
          east(j,k)=s(ni,j,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT14
        if(Debug) print *,'comm_1s_start_GPU: point #6'
        !$acc host_data use_device(east)
        call mpi_isend(east,cs1we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(east)
        call mpi_isend(east,cs1we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      ! send north
      if(ibn.eq.0)then
        !$omp parallel do default(shared) private(i,k)
        !$acc parallel loop gang vector default(present) collapse(2) private(i,k)
        do k=1,nk
        do i=1,ni
          north(i,k)=s(i,nj,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT14
        if(Debug) print *,'comm_1s_start_GPU: point #7'
        !$acc host_data use_device(north)
        call mpi_isend(north,cs1sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(north)
        call mpi_isend(north,cs1sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      ! send south
      if(ibs.eq.0)then
        !$omp parallel do default(shared) private(i,k)
        !$acc parallel loop gang vector default(present) collapse(2) private(i,k)
        do k=1,nk
        do i=1,ni
          south(i,k)=s(i,1,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT14
        if(Debug) print *,'comm_1s_start_GPU: point #8'
        !$acc host_data use_device(south)
        call mpi_isend(south,cs1sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(south)
        call mpi_isend(south,cs1sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------
 
      if(timestats.ge.1) time_mps1=time_mps1+mytime()
      if(Debug) print *,'comm_1s_start_GPU: point #9'
 
      end subroutine comm_1s_start_GPU


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_1s_end(s,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibe,ibw,ibs,ibn, &
          ebc,wbc,sbc,nbc,timestats,mytime,ierr,time_bc,time_mps2, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none
 
      real s(ib:ie,jb:je,kb:ke)
      real west(nj,nk),newwest(nj,nk)
      real east(nj,nk),neweast(nj,nk)
      real south(ni,nk),newsouth(ni,nk)
      real north(ni,nk),newnorth(ni,nk)
      integer reqs(8)
 
      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1

!---------------------------------------------------------------------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
 
      if(index.eq.index_east)then
!$omp parallel do default(shared)   &
!$omp private(j,k)
        do k=1,nk
        do j=1,nj
          s(ni+1,j,k)=neweast(j,k)
        enddo
        enddo
      elseif(index.eq.index_west)then
!$omp parallel do default(shared)   &
!$omp private(j,k)
        do k=1,nk
        do j=1,nj
          s(0,j,k)=newwest(j,k)
        enddo
        enddo
      elseif(index.eq.index_south)then
!$omp parallel do default(shared)   &
!$omp private(i,k)
        do k=1,nk
        do i=1,ni
          s(i,0,k)=newsouth(i,k)
        enddo
        enddo
      elseif(index.eq.index_north)then
!$omp parallel do default(shared)   &
!$omp private(i,k)
        do k=1,nk
        do i=1,ni
          s(i,nj+1,k)=newnorth(i,k)
        enddo
        enddo
      endif

      enddo

      if(timestats.ge.1) time_mps2=time_mps2+mytime()

!----------
!  patch for corner
 
      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then
 
        if(ibw.eq.1)then
 
          if(p2tchsww)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(0,0,k)=s(1,0,k)
            enddo
          endif
 
          if(p2tchnww)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(0,nj+1,k)=s(1,nj+1,k)
            enddo
          endif
 
        endif
 
        if(ibe.eq.1)then
 
          if(p2tchsee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(ni+1,0,k)=s(ni,0,k)
            enddo
          endif
 
          if(p2tchnee)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(ni+1,nj+1,k)=s(ni,nj+1,k)
            enddo
          endif
 
        endif
 
      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then
 
        if(ibs.eq.1)then
 
          if(p2tchsws)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(0,0,k)=s(0,1,k)
            enddo
          endif
 
          if(p2tchses)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(ni+1,0,k)=s(ni+1,1,k)
            enddo
          endif
 
        endif
 
        if(ibn.eq.1)then
 
          if(p2tchnwn)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(0,nj+1,k)=s(0,nj,k)
            enddo
          endif
 
          if(p2tchnen)then
!$omp parallel do default(shared)   &
!$omp private(k)
            do k=1,nk
              s(ni+1,nj+1,k)=s(ni+1,nj,k)
            enddo
          endif
 
        endif
 
      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mps2=time_mps2+mytime()
 
!----------
 
      end subroutine comm_1s_end


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine comm_1s_end_GPU(s,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibe,ibw,ibs,ibn, &
          ebc,wbc,sbc,nbc,timestats,mytime,ierr,time_bc,time_mps2, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none
 
      real s(ib:ie,jb:je,kb:ke)
      real west(nj,nk),newwest(nj,nk)
      real east(nj,nk),neweast(nj,nk)
      real south(ni,nk),newsouth(ni,nk)
      real north(ni,nk),newnorth(ni,nk)
      integer reqs(8)
 
      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1
      logical, parameter :: Debug=.False.
      !$acc declare present(s) &
      !$acc present(west,newwest,east,neweast) &
      !$acc present(south,newsouth,north,newnorth)

!---------------------------------------------------------------------
! SM: GPU Direct back in
      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
 
      if(index.eq.index_east)then
#ifndef _GPUDIRECT14
        if(Debug) print *,'comm_1s_end_GPU: point #1'
        !$acc update device(neweast)
#endif
        !$omp parallel do default(shared) private(j,k)
        !$acc parallel loop gang vector default(present) collapse(2) private(j,k)
        do k=1,nk
        do j=1,nj
          s(ni+1,j,k)=neweast(j,k)
        enddo
        enddo
      elseif(index.eq.index_west)then
#ifndef _GPUDIRECT14
        if(Debug) print *,'comm_1s_end_GPU: point #2'
        !$acc update device(newwest)
#endif
        !$omp parallel do default(shared) private(j,k)
        !$acc parallel loop gang vector default(present) collapse(2) private(j,k)
        do k=1,nk
        do j=1,nj
          s(0,j,k)=newwest(j,k)
        enddo
        enddo
      elseif(index.eq.index_south)then
#ifndef _GPUDIRECT14
        if(Debug) print *,'comm_1s_end_GPU: point #3'
        !$acc update device(newsouth)
#endif
        !$omp parallel do default(shared) private(i,k)
        !$acc parallel loop gang vector default(present) collapse(2) private(i,k)
        do k=1,nk
        do i=1,ni
          s(i,0,k)=newsouth(i,k)
        enddo
        enddo
      elseif(index.eq.index_north)then
#ifndef _GPUDIRECT14
        if(Debug) print *,'comm_1s_end_GPU: point #4'
        !$acc update device(newnorth)
#endif
        !$omp parallel do default(shared) private(i,k)
        !$acc parallel loop gang vector default(present) collapse(2) private(i,k)
        do k=1,nk
        do i=1,ni
          s(i,nj+1,k)=newnorth(i,k)
        enddo
        enddo
      endif

      enddo

      if(timestats.ge.1) time_mps2=time_mps2+mytime()

!----------
!  patch for corner
 
      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then
 
        if(ibw.eq.1)then
 
          if(p2tchsww)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(0,0,k)=s(1,0,k)
            enddo
          endif
 
          if(p2tchnww)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(0,nj+1,k)=s(1,nj+1,k)
            enddo
          endif
 
        endif
 
        if(ibe.eq.1)then
 
          if(p2tchsee)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(ni+1,0,k)=s(ni,0,k)
            enddo
          endif
 
          if(p2tchnee)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(ni+1,nj+1,k)=s(ni,nj+1,k)
            enddo
          endif
 
        endif
 
      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then
 
        if(ibs.eq.1)then
 
          if(p2tchsws)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(0,0,k)=s(0,1,k)
            enddo
          endif
 
          if(p2tchses)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(ni+1,0,k)=s(ni+1,1,k)
            enddo
          endif
 
        endif
 
        if(ibn.eq.1)then
 
          if(p2tchnwn)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(0,nj+1,k)=s(0,nj,k)
            enddo
          endif
 
          if(p2tchnen)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              s(ni+1,nj+1,k)=s(ni+1,nj,k)
            enddo
          endif
 
        endif
 
      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif
      if(timestats.ge.1) time_mps2=time_mps2+mytime()
!----------
 
      end subroutine comm_1s_end_GPU


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_1p_start_GPU(s,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibe,ibw,ibs,ibn, &
          wbc,ebc,sbc,nbc,nf,ierr,timestats,time_mpp1,mytime, &
          cs1we,cs1sn,myeast,mywest,mysouth,mynorth
      use mpi
      implicit none

      real s(ib:ie,jb:je,kb:ke)
      real west(nj,nk),newwest(nj,nk)
      real east(nj,nk),neweast(nj,nk)
      real south(ni,nk),newsouth(ni,nk)
      real north(ni,nk),newnorth(ni,nk)
      integer reqs(8)

      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
      logical, parameter :: Debug = .FALSE.

!------------------------------------------------

      nr = 0
#ifndef _GPUDIRECT15
      if(Debug) print *,'comm_1p_start_GPU:'
#endif

      nf=nf+1
      tag1=nf
      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT15
        if(Debug) print *,'comm_1p_start_GPU point #1'
        !$acc host_data use_device(neweast)
        call mpi_irecv(neweast,cs1we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(neweast,cs1we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT15
        if(Debug) print *,'comm_1p_start_GPU point #2'
        !$acc host_data use_device(newwest)
        call mpi_irecv(newwest,cs1we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newwest,cs1we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag3=nf

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT15
        if(Debug) print *,'comm_1p_start_GPU point #3'
        !$acc host_data use_device(newsouth)
        call mpi_irecv(newsouth,cs1sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newsouth,cs1sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT15
        if(Debug) print *,'comm_1p_start_GPU point #4'
        !$acc host_data use_device(newnorth)
        call mpi_irecv(newnorth,cs1sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newnorth,cs1sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4

      ! send west
      if(ibw.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(j,k)
!$acc parallel loop gang vector default(present) collapse(2) private(j,k)
        do k=1,nk
        do j=1,nj
          west(j,k)=s(1,j,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT15
        if(Debug) print *,'comm_1p_start_GPU point #5'
        !$acc host_data use_device(west)
        call mpi_isend(west,cs1we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(west)
        call mpi_isend(west,cs1we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send east
      if(ibe.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(j,k)
!$acc parallel loop gang vector default(present) collapse(2) private(j,k)
        do k=1,nk
        do j=1,nj
          east(j,k)=s(ni,j,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT15
        if(Debug) print *,'comm_1p_start_GPU point #6'
        !$acc host_data use_device(east)
        call mpi_isend(east,cs1we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(east)
        call mpi_isend(east,cs1we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send north
      if(ibn.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,k)
!$acc parallel loop gang vector default(present) collapse(2) private(i,k)
        do k=1,nk
        do i=1,ni
          north(i,k)=s(i,nj,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT15
        if(Debug) print *,'comm_1p_start_GPU point #7'
        !$acc host_data use_device(north)
        call mpi_isend(north,cs1sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(north)
        call mpi_isend(north,cs1sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send south
      if(ibs.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,k)
!$acc parallel loop gang vector default(present) collapse(2) private(i,k)
        do k=1,nk
        do i=1,ni
          south(i,k)=s(i,1,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT15
        if(Debug) print *,'comm_1p_start_GPU point #8'
        !$acc host_data use_device(south)
        call mpi_isend(south,cs1sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(south)
        call mpi_isend(south,cs1sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      if(timestats.ge.1) time_mpp1=time_mpp1+mytime()
      if(Debug) print *,'comm_1p_start_GPU point #9'

      end subroutine comm_1p_start_GPU

 
      subroutine comm_1p_end_GPU(s,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibe,ibw,ibs,ibn, &
          ebc,wbc,sbc,nbc,timestats,mytime,time_bc,time_mpp2,ierr
      use mpi
      implicit none
 
      real s(ib:ie,jb:je,kb:ke)
      real west(nj,nk),newwest(nj,nk)
      real east(nj,nk),neweast(nj,nk)
      real south(ni,nk),newsouth(ni,nk)
      real north(ni,nk),newnorth(ni,nk)
      integer reqs(8)
 
      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1
      logical, parameter :: Debug = .FALSE.
      !$acc declare present(s) &
      !$acc present(west,newwest,east,neweast) &
      !$acc present(south,newsouth,north,newnorth)

!---------------------------------------------------------------------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1
      nr = 0

      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
 
      if(index.eq.index_east)then
#ifndef _GPUDIRECT15
        if(Debug) print *,'comm_1p_end_GPU: point #1'
        !$acc update device(neweast)
#endif
        !$omp parallel do default(shared) private(j,k)
        !$acc parallel loop gang vector default(present) collapse(2) private(j,k)
        do k=1,nk
        do j=1,nj
          s(ni+1,j,k)=neweast(j,k)
        enddo
        enddo
      elseif(index.eq.index_west)then
#ifndef _GPUDIRECT15
        if(Debug) print *,'comm_1p_end_GPU: point #2'
        !$acc update device(newwest)
#endif
        !$omp parallel do default(shared) private(j,k)
        !$acc parallel loop gang vector default(present) collapse(2) private(j,k)
        do k=1,nk
        do j=1,nj
          s(0,j,k)=newwest(j,k)
        enddo
        enddo
      elseif(index.eq.index_south)then
#ifndef _GPUDIRECT15
        if(Debug) print *,'comm_1p_end_GPU: point #3'
        !$acc update device(newsouth)
#endif
        !$omp parallel do default(shared) private(i,k)
        !$acc parallel loop gang vector default(present) collapse(2) private(i,k)
        do k=1,nk
        do i=1,ni
          s(i,0,k)=newsouth(i,k)
        enddo
        enddo
      elseif(index.eq.index_north)then
#ifndef _GPUDIRECT15
        if(Debug) print *,'comm_1p_end_GPU: point #4'
        !$acc update device(newnorth)
#endif
        !$omp parallel do default(shared) private(i,k)
        !$acc parallel loop gang vector default(present) collapse(2) private(i,k)
        do k=1,nk
        do i=1,ni
          s(i,nj+1,k)=newnorth(i,k)
        enddo
        enddo
      endif

      enddo

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mpp2=time_mpp2+mytime()
!----------
 
      end subroutine comm_1p_end_GPU


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_1t_start_GPU(t,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,nkp1,ibe,ibw,ibs,ibn, &
          wbc,ebc,sbc,nbc,ct1we,ct1sn,nf,ierr,timestats,time_mps1,mytime, &
          myeast,mywest,mysouth,mynorth
      use mpi
      implicit none

      real t(ib:ie,jb:je,kb:ke+1)
      real west(nj,nk+1),newwest(nj,nk+1)
      real east(nj,nk+1),neweast(nj,nk+1)
      real south(ni,nk+1),newsouth(ni,nk+1)
      real north(ni,nk+1),newnorth(ni,nk+1)
      integer reqs(8)

      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
      logical, parameter :: Debug=.FALSE.


!------------------------------------------------
     
      nr = 0
#ifndef _GPUDIRECT16
      if(Debug) print *,'comm_1t_start_GPU:'
#endif
      nf=nf+1
      tag1=nf


      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT16
        !$acc host_data use_device(neweast)
        call mpi_irecv(neweast,ct1we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(neweast,ct1we,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT16
        !$acc host_data use_device(newwest)
        call mpi_irecv(newwest,ct1we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newwest,ct1we,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag3=nf

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT16
        !$acc host_data use_device(newsouth)
        call mpi_irecv(newsouth,ct1sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newsouth,ct1sn,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT16
        !$acc host_data use_device(newnorth)
        call mpi_irecv(newnorth,ct1sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newnorth,ct1sn,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4

      ! send west
      if(ibw.eq.0)then
!$acc parallel loop gang vector collapse(2) default(present) private(j,k)
!$omp parallel do default(shared)   &
!$omp private(j,k)
        do k=1,nkp1
        do j=1,nj
          west(j,k)=t(1,j,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT16
        !$acc host_data use_device(west)
        call mpi_isend(west,ct1we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(west)
        call mpi_isend(west,ct1we,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send east
      if(ibe.eq.0)then
!$acc parallel loop gang vector collapse(2) default(present) private(j,k)
!$omp parallel do default(shared)   &
!$omp private(j,k)
        do k=1,nkp1
        do j=1,nj
          east(j,k)=t(ni,j,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT16
        !$acc host_data use_device(east)
        call mpi_isend(east,ct1we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(east)
        call mpi_isend(east,ct1we,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send north
      if(ibn.eq.0)then
!$acc parallel loop gang vector collapse(2) default(present) private(i,k)
!$omp parallel do default(shared)   &
!$omp private(i,k)
        do k=1,nkp1
        do i=1,ni
          north(i,k)=t(i,nj,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT16
        !$acc host_data use_device(north)
        call mpi_isend(north,ct1sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(north)
        call mpi_isend(north,ct1sn,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send south
      if(ibs.eq.0)then
!$acc parallel loop gang vector collapse(2) default(present) private(i,k)
!$omp parallel do default(shared)   &
!$omp private(i,k)
        do k=1,nkp1
        do i=1,ni
          south(i,k)=t(i,1,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT16
        !$acc host_data use_device(south)
        call mpi_isend(south,ct1sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(south)
        call mpi_isend(south,ct1sn,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      if(timestats.ge.1) time_mps1=time_mps1+mytime()
      end subroutine comm_1t_start_GPU


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_1t_end_GPU(t,west,newwest,east,neweast,   &
                               south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,nkp1,ibe,ibw,ibs,ibn, &
          wbc,ebc,sbc,nbc,mytime,ierr,timestats,time_mps2,time_bc, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none

      real t(ib:ie,jb:je,kb:ke+1)
      real west(nj,nk+1),newwest(nj,nk+1)
      real east(nj,nk+1),neweast(nj,nk+1)
      real south(ni,nk+1),newsouth(ni,nk+1)
      real north(ni,nk+1),newnorth(ni,nk+1)
      integer reqs(8)

      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1

!---------------------------------------------------------------------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif


      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_east)then
#ifndef _GPUDIRECT16
        !$acc update device(neweast)
#endif
!$omp parallel do default(shared)   &
!$omp private(j,k)
!$acc parallel loop gang vector default(present) collapse(2) private(j,k)
        do k=1,nkp1
        do j=1,nj
          t(ni+1,j,k)=neweast(j,k)
        enddo
        enddo
      elseif(index.eq.index_west)then
#ifndef _GPUDIRECT16
        !$acc update device(newwest)
#endif
!$omp parallel do default(shared)   &
!$omp private(j,k)
!$acc parallel loop gang vector default(present) collapse(2) private(j,k)
        do k=1,nkp1
        do j=1,nj
          t(0,j,k)=newwest(j,k)
        enddo
        enddo
      elseif(index.eq.index_south)then
#ifndef _GPUDIRECT16
        !$acc update device(newsouth)
#endif
!$omp parallel do default(shared)   &
!$omp private(i,k)
!$acc parallel loop gang vector default(present) collapse(2) private(i,k)
        do k=1,nkp1
        do i=1,ni
          t(i,0,k)=newsouth(i,k)
        enddo
        enddo
      elseif(index.eq.index_north)then
#ifndef _GPUDIRECT16
        !$acc update device(newnorth)
#endif
!$omp parallel do default(shared)   &
!$omp private(i,k)
!$acc parallel loop gang vector default(present) collapse(2) private(i,k)
        do k=1,nkp1
        do i=1,ni
          t(i,nj+1,k)=newnorth(i,k)
        enddo
        enddo
      endif

      enddo

      if(timestats.ge.1) time_mps2=time_mps2+mytime()

!----------
!  patch for corner

      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then

        if(ibw.eq.1)then

          if(p2tchsww)then
!$omp parallel do default(shared)   &
!$omp private(k)
!$acc parallel loop gang vector default(present)
            do k=1,nkp1
              t(0,0,k)=t(1,0,k)
            enddo
          endif

          if(p2tchnww)then
!$omp parallel do default(shared)   &
!$omp private(k)
!$acc parallel loop gang vector default(present) 
            do k=1,nkp1
              t(0,nj+1,k)=t(1,nj+1,k)
            enddo
          endif

        endif

        if(ibe.eq.1)then

          if(p2tchsee)then
!$omp parallel do default(shared)   &
!$omp private(k)
!$acc parallel loop gang vector default(present)
            do k=1,nkp1
              t(ni+1,0,k)=t(ni,0,k)
            enddo
          endif

          if(p2tchnee)then
!$omp parallel do default(shared)   &
!$omp private(k)
!$acc parallel loop gang vector default(present) 
            do k=1,nkp1
              t(ni+1,nj+1,k)=t(ni,nj+1,k)
            enddo
          endif

        endif

      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then

        if(ibs.eq.1)then

          if(p2tchsws)then
!$omp parallel do default(shared)   &
!$omp private(k)
!$acc parallel loop gang vector default(present) 
            do k=1,nkp1
              t(0,0,k)=t(0,1,k)
            enddo
          endif

          if(p2tchses)then
!$omp parallel do default(shared)   &
!$omp private(k)
!$acc parallel loop gang vector default(present)
            do k=1,nkp1
              t(ni+1,0,k)=t(ni+1,1,k)
            enddo
          endif

        endif

        if(ibn.eq.1)then

          if(p2tchnwn)then
!$omp parallel do default(shared)   &
!$omp private(k)
!$acc parallel loop gang vector default(present)
            do k=1,nkp1
              t(0,nj+1,k)=t(0,nj,k)
            enddo
          endif

          if(p2tchnen)then
!$omp parallel do default(shared)   &
!$omp private(k)
!$acc parallel loop gang vector default(present)
            do k=1,nkp1
              t(ni+1,nj+1,k)=t(ni+1,nj,k)
            enddo
          endif

        endif

      endif

      if(timestats.ge.1) time_bc=time_bc+mytime()

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mps2=time_mps2+mytime()

!----------
!      !$acc update device(t)
      end subroutine comm_1t_end_GPU

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_1w_start_GPU(w,ww1,ww2,we1,we2,   &
                                 ws1,ws2,wn1,wn2,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibe,ibw,ibs,ibn, &
          wbc,ebc,sbc,nbc,cw1we,cw1sn, &
          nw,ierr,timestats,time_mpw1,mytime,myeast,mywest,mysouth,mynorth
      use mpi
      implicit none
 
      real w(ib:ie,jb:je,kb:ke+1)
      real ww1(nj,nk-1),ww2(nj,nk-1)
      real we1(nj,nk-1),we2(nj,nk-1)
      real ws1(ni,nk-1),ws2(ni,nk-1)
      real wn1(ni,nk-1),wn2(ni,nk-1)
      integer reqs(8)
 
      integer i,j,k,nr
      integer tag1,tag2,tag3,tag4
      logical, parameter :: Debug = .FALSE.
      !$acc declare present(w) &
      !$acc present(ww1,ww2,we1,we2,ws1,ws2,wn1,wn2)

#ifndef _GPUDIRECT17
      if(Debug) print *,'comm_1w_start_GPU:'
#endif
      nr = 0
!-----

      nw=nw+1
      tag1=3000+nw
 
        ! receive east
        if(ibe.eq.0)then
          nr = nr+1
#ifdef _GPUDIRECT17
          !$acc host_data use_device(we2)
          call mpi_irecv(we2,cw1we,MPI_REAL,myeast,tag1,   &
                        MPI_COMM_WORLD,reqs(nr),ierr)
          !$acc end host_data
#else
          call mpi_irecv(we2,cw1we,MPI_REAL,myeast,tag1,   &
                        MPI_COMM_WORLD,reqs(nr),ierr)
#endif
        endif

!-----

      nw=nw+1
      tag2=3000+nw
 
        ! receive west
        if(ibw.eq.0)then
          nr = nr+1
#ifdef _GPUDIRECT17
          !$acc host_data use_device(ww2)
          call mpi_irecv(ww2,cw1we,MPI_REAL,mywest,tag2,   &
                        MPI_COMM_WORLD,reqs(nr),ierr)
          !$acc end host_data
#else
          call mpi_irecv(ww2,cw1we,MPI_REAL,mywest,tag2,   &
                        MPI_COMM_WORLD,reqs(nr),ierr)
#endif
        endif

!-----

      nw=nw+1
      tag3=3000+nw
 
        ! receive north
        if(ibn.eq.0)then
          nr = nr+1
#ifdef _GPUDIRECT17
          !$acc host_data use_device(wn2)
          call mpi_irecv(wn2,cw1sn,MPI_REAL,mynorth,tag3,   &
                        MPI_COMM_WORLD,reqs(nr),ierr)
          !$acc end host_data
#else
          call mpi_irecv(wn2,cw1sn,MPI_REAL,mynorth,tag3,   &
                        MPI_COMM_WORLD,reqs(nr),ierr)
#endif
        endif

!-----

      nw=nw+1
      tag4=3000+nw
 
        ! receive south
        if(ibs.eq.0)then
          nr = nr+1
#ifdef _GPUDIRECT17
          !$acc host_data use_device(ws2)
          call mpi_irecv(ws2,cw1sn,MPI_REAL,mysouth,tag4,   &
                        MPI_COMM_WORLD,reqs(nr),ierr)
          !$acc end host_data
#else
          call mpi_irecv(ws2,cw1sn,MPI_REAL,mysouth,tag4,   &
                        MPI_COMM_WORLD,reqs(nr),ierr)
#endif
        endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4

        ! send west
        if(ibw.eq.0)then
          !$omp parallel do default(shared) private(j,k)
          !$acc parallel loop gang vector default(present) collapse(2) private(j,k)
          do k=2,nk
          do j=1,nj
            ww1(j,k-1)=w(1,j,k)
          enddo
          enddo
          nr = nr+1
#ifdef _GPUDIRECT17
          !$acc host_data use_device(ww1)
          call mpi_isend(ww1,cw1we,MPI_REAL,mywest,tag1,   &
                         MPI_COMM_WORLD,reqs(nr),ierr)
          !$acc end host_data
#else
          !$acc update host(ww1)
          call mpi_isend(ww1,cw1we,MPI_REAL,mywest,tag1,   &
                         MPI_COMM_WORLD,reqs(nr),ierr)
#endif
        endif

        ! send east
        if(ibe.eq.0)then
          !$omp parallel do default(shared) private(j,k)
          !$acc parallel loop gang vector default(present) collapse(2) private(j,k)
          do k=2,nk
          do j=1,nj
            we1(j,k-1)=w(ni,j,k)
          enddo
          enddo
          nr = nr+1
#ifdef _GPUDIRECT17
          !$acc host_data use_device(we1)
          call mpi_isend(we1,cw1we,MPI_REAL,myeast,tag2,   &
                         MPI_COMM_WORLD,reqs(nr),ierr)
          !$acc end host_data
#else
          !$acc update host(we1)
          call mpi_isend(we1,cw1we,MPI_REAL,myeast,tag2,   &
                         MPI_COMM_WORLD,reqs(nr),ierr)
#endif
        endif

        ! send south
        if(ibs.eq.0)then
          !$omp parallel do default(shared) private(i,k)
          !$acc parallel loop gang vector default(present) collapse(2) private(i,k)
          do k=2,nk
          do i=1,ni
            ws1(i,k-1)=w(i,1,k)
          enddo
          enddo
          nr = nr+1
#ifdef _GPUDIRECT17
          !$acc host_data use_device(ws1)
          call mpi_isend(ws1,cw1sn,MPI_REAL,mysouth,tag3,   &
                         MPI_COMM_WORLD,reqs(nr),ierr)
          !$acc end host_data
#else
          !$acc update host(ws1)
          call mpi_isend(ws1,cw1sn,MPI_REAL,mysouth,tag3,   &
                         MPI_COMM_WORLD,reqs(nr),ierr)
#endif
        endif

        ! send north
        if(ibn.eq.0)then
          !$omp parallel do default(shared) private(i,k)
          !$acc parallel loop gang vector default(present) collapse(2) private(i,k)
          do k=2,nk
          do i=1,ni
            wn1(i,k-1)=w(i,nj,k)
          enddo
          enddo
          nr = nr+1
#ifdef _GPUDIRECT17
          !$acc host_data use_device(wn1)
          call mpi_isend(wn1,cw1sn,MPI_REAL,mynorth,tag4,   &
                         MPI_COMM_WORLD,reqs(nr),ierr)
          !$acc end host_data
#else
          !$acc update host(wn1)
          call mpi_isend(wn1,cw1sn,MPI_REAL,mynorth,tag4,   &
                         MPI_COMM_WORLD,reqs(nr),ierr)
#endif
        endif

!-----

      if(timestats.ge.1) time_mpw1=time_mpw1+mytime()
 
      end subroutine comm_1w_start_GPU

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine comm_1w_end_GPU(w,ww1,ww2,we1,we2,   &
                               ws1,ws2,wn1,wn2,reqs)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibe,ibw,ibn,ibs, &
          wbc,ebc,sbc,nbc,timestats,mytime,ierr,time_bc,time_mpw2, &
          p2tchsww,p2tchnww,p2tchsee,p2tchnee,p2tchsws,p2tchses,p2tchnwn,p2tchnen
      use mpi
      implicit none
 
      real w(ib:ie,jb:je,kb:ke+1)
      real ww1(nj,nk-1),ww2(nj,nk-1)
      real we1(nj,nk-1),we2(nj,nk-1)
      real ws1(ni,nk-1),ws2(ni,nk-1)
      real wn1(ni,nk-1),wn2(ni,nk-1)
      integer reqs(8)
 
      integer i,j,k,nn,nr,index
      integer :: index_east,index_west,index_south,index_north
      integer, dimension(mpi_status_size,8) :: status1
      !$acc declare present(w) &
      !$acc present(ww1,ww2,we1,we2,ws1,ws2,wn1,wn2)
 
!-----

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1
      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_east)then
#ifndef _GPUDIRECT17
        !$acc update device(we2)
#endif
        !$omp parallel do default(shared) private(j,k)
        !$acc parallel loop gang vector default(present) private(j,k)
        do k=2,nk
        do j=1,nj
          w(ni+1,j,k)=we2(j,k-1)
        enddo
        enddo
      elseif(index.eq.index_west)then
#ifndef _GPUDIRECT17
        !$acc update device(ww2)
#endif
        !$omp parallel do default(shared) private(j,k)
        !$acc parallel loop gang vector default(present) private(j,k)
        do k=2,nk
        do j=1,nj
          w(0,j,k)=ww2(j,k-1)
        enddo
        enddo
      elseif(index.eq.index_north)then
#ifndef _GPUDIRECT17
        !$acc update device(wn2)
#endif
        !$omp parallel do default(shared) private(i,k)
        !$acc parallel loop gang vector default(present) private(i,k)
        do k=2,nk
        do i=1,ni
          w(i,nj+1,k)=wn2(i,k-1)
        enddo
        enddo
      elseif(index.eq.index_south)then
#ifndef _GPUDIRECT17
        !$acc update device(ws2)
#endif
        !$omp parallel do default(shared) private(i,k)
        !$acc parallel loop gang vector default(present) private(i,k)
        do k=2,nk
        do i=1,ni
          w(i,0,k)=ws2(i,k-1)
        enddo
        enddo
      endif

      enddo

!-----

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

      if(timestats.ge.1) time_mpw2=time_mpw2+mytime()

!----------
!  patch for corner
 
      if( (ebc.eq.2.or.wbc.eq.2).and.(sbc.eq.1.or.nbc.eq.1) )then
 
        if(ibw.eq.1)then
 
          if(p2tchsww)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(0,0,k)=w(1,0,k)
            enddo
          endif
 
          if(p2tchnww)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(0,nj+1,k)=w(1,nj+1,k)
            enddo
          endif
 
        endif
 
        if(ibe.eq.1)then
 
          if(p2tchsee)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(ni+1,0,k)=w(ni,0,k)
            enddo
          endif
 
          if(p2tchnee)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(ni+1,nj+1,k)=w(ni,nj+1,k)
            enddo
          endif
 
        endif
 
      endif

      if( (ebc.eq.1.or.wbc.eq.1).and.(sbc.eq.2.or.nbc.eq.2) )then
 
        if(ibs.eq.1)then
 
          if(p2tchsws)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(0,0,k)=w(0,1,k)
            enddo
          endif
 
          if(p2tchses)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(ni+1,0,k)=w(ni+1,1,k)
            enddo
          endif
 
        endif
 
        if(ibn.eq.1)then
 
          if(p2tchnwn)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(0,nj+1,k)=w(0,nj,k)
            enddo
          endif
 
          if(p2tchnen)then
            !$omp parallel do default(shared) private(k)
            !$acc parallel loop gang vector default(present) private(k)
            do k=2,nk
              w(ni+1,nj+1,k)=w(ni+1,nj,k)
            enddo
          endif
 
        endif
 
      endif

!-----------------------------------------------------------
!  Mirror b.c. patch
!
!      if(ibn.eq.1)then
!!$omp parallel do default(shared)   &
!!$omp private(i,k)
!        do k=2,nk
!        do i=0,ni+1
!          w(i,nj+1,k)=w(i,nj  ,k)
!          w(i,nj+2,k)=w(i,nj-1,k)
!          w(i,nj+3,k)=w(i,nj-2,k)
!        enddo
!        enddo
!      endif
!
!      if(ibw.eq.1)then
!!$omp parallel do default(shared)   &
!!$omp private(i,k)
!        do k=2,nk
!        do j=0,nj+1
!          w(-2,j,k)=w(3,j,k)
!          w(-1,j,k)=w(2,j,k)
!          w( 0,j,k)=w(1,j,k)
!        enddo
!        enddo
!      endif
!
!-----------------------------------------------------------
 
      if(timestats.ge.1) time_bc=time_bc+mytime()
 
!     !$acc update device(w)
!----------

      end subroutine comm_1w_end_GPU


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_2d_start(s,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,ni,nj,cmp,ibe,ibw,ibs,ibn,wbc,ebc,sbc,nbc, &
          nf,ierr,timestats,time_mpq1,mytime,myeast,mywest,mysouth,mynorth
      use mpi
      implicit none

      real, dimension(ib:ie,jb:je) :: s
      real, dimension(cmp,nj) :: west,newwest,east,neweast
      real, dimension(ni,cmp) :: south,newsouth,north,newnorth
      integer reqs(8)

      integer i,j,nr
      integer tag1,tag2,tag3,tag4

!------------------------------------------------

      nr = 0

      nf=nf+1
      tag1=nf

      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
        call mpi_irecv(neweast,cmp*nj,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
        call mpi_irecv(newwest,cmp*nj,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      nf=nf+1
      tag3=nf

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
        call mpi_irecv(newsouth,ni*cmp,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
        call mpi_irecv(newnorth,ni*cmp,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4

      ! send west
      if(ibw.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=1,nj
        do i=1,cmp
          west(i,j)=s(i,j)
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(west,cmp*nj,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      ! send east
      if(ibe.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=1,nj
        do i=1,cmp
          east(i,j)=s(ni-cmp+i,j)
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(east,cmp*nj,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      ! send north
      if(ibn.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=1,cmp
        do i=1,ni
          north(i,j)=s(i,nj-cmp+j)
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(north,ni*cmp,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      ! send south
      if(ibs.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=1,cmp
        do i=1,ni
          south(i,j)=s(i,j)
        enddo
        enddo
        nr = nr + 1
        call mpi_isend(south,ni*cmp,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
      endif

!----------

      if(timestats.ge.1) time_mpq1=time_mpq1+mytime()

      end subroutine comm_2d_start

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_2d_start_GPU(s,west,newwest,east,neweast,   &
                                 south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,ni,nj,cmp,ibe,ibw,ibs,ibn,wbc,ebc,sbc,nbc, &
          nf,ierr,timestats,time_mpq1,mytime,myeast,mywest,mysouth,mynorth
      use mpi
      implicit none

      real, dimension(ib:ie,jb:je) :: s
      real, dimension(cmp,nj) :: west,newwest,east,neweast
      real, dimension(ni,cmp) :: south,newsouth,north,newnorth
      integer reqs(8)

      integer i,j,nr
      integer tag1,tag2,tag3,tag4
      logical, parameter :: Debug = .FALSE.

!------------------------------------------------
#ifndef _GPUDIRECT18
!print *, "comm_2d_start_GPU: Im called"
      if(Debug) print *,'comm_2d_start_GPU:'
#endif

      nr = 0

      nf=nf+1
      tag1=nf


      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT18
        !$acc host_data use_device(neweast)
        call mpi_irecv(neweast,cmp*nj,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(neweast,cmp*nj,MPI_REAL,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT18
        !$acc host_data use_device(newwest)
        call mpi_irecv(newwest,cmp*nj,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newwest,cmp*nj,MPI_REAL,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag3=nf

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT18
        !$acc host_data use_device(newsouth)
        call mpi_irecv(newsouth,ni*cmp,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newsouth,ni*cmp,MPI_REAL,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT18
        !$acc host_data use_device(newnorth)
        call mpi_irecv(newnorth,ni*cmp,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newnorth,ni*cmp,MPI_REAL,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4

      ! send west
      if(ibw.eq.0)then
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        !$omp parallel do default(shared) private(i,j)
        do j=1,nj
        do i=1,cmp
          west(i,j)=s(i,j)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT18
        !$acc host_data use_device(west)
        call mpi_isend(west,cmp*nj,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(west)
        call mpi_isend(west,cmp*nj,MPI_REAL,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send east
      if(ibe.eq.0)then
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        !$omp parallel do default(shared) private(i,j)
        do j=1,nj
        do i=1,cmp
          east(i,j)=s(ni-cmp+i,j)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT18
        !$acc host_data use_device(east)
        call mpi_isend(east,cmp*nj,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(east)
        call mpi_isend(east,cmp*nj,MPI_REAL,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send north
      if(ibn.eq.0)then
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        !$omp parallel do default(shared) private(i,j)
        do j=1,cmp
        do i=1,ni
          north(i,j)=s(i,nj-cmp+j)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT18
        !$acc host_data use_device(north)
        call mpi_isend(north,ni*cmp,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(north)
        call mpi_isend(north,ni*cmp,MPI_REAL,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      ! send south
      if(ibs.eq.0)then
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        !$omp parallel do default(shared) private(i,j)
        do j=1,cmp
        do i=1,ni
          south(i,j)=s(i,j)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT18
        !$acc host_data use_device(south)
        call mpi_isend(south,ni*cmp,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(south)
        call mpi_isend(south,ni*cmp,MPI_REAL,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      if(timestats.ge.1) time_mpq1=time_mpq1+mytime()
      end subroutine comm_2d_start_GPU



!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_2dew_end(s,west,newwest,east,neweast,reqs)
      use input, only : ib,ie,jb,je,ni,nj,cmp,ibe,ibw,ierr, &
          timestats,time_mpq2,mytime
      use mpi
      implicit none

      real, dimension(ib:ie,jb:je) :: s
      real, dimension(cmp,nj) :: west,newwest,east,neweast
      integer reqs(8)

      integer i,j,nn,nr,index

!-------------------------------------------------------------------

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
      endif
      if(ibw.eq.0)then
        nr = nr + 1
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
      enddo

      if(ibe.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=1,nj
        do i=1,cmp
          s(ni+i,j)=neweast(i,j)
        enddo
        enddo
      endif

      if(ibw.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=1,nj
        do i=1,cmp
          s(i-cmp,j)=newwest(i,j)
        enddo
        enddo
      endif

!----------

      if(timestats.ge.1) time_mpq2=time_mpq2+mytime()

      end subroutine comm_2dew_end

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_2dew_end_GPU(s,west,newwest,east,neweast,reqs)
      use input, only : ib,ie,jb,je,ni,nj,cmp,ibe,ibw,ierr, &
          timestats,time_mpq2,mytime
      use mpi
      implicit none

      real, dimension(ib:ie,jb:je) :: s
      real, dimension(cmp,nj) :: west,newwest,east,neweast
      integer reqs(8)
      logical, parameter :: Debug = .FALSE.
      integer i,j,nn,nr,index

!-------------------------------------------------------------------
#ifndef _GPUDIRECT18
if(Debug) print *, "comm_2dew_end_GPU: Im called"
#endif

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
      endif
      if(ibw.eq.0)then
        nr = nr + 1
      endif

      nn = 1

      !!$acc update host(s)
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
      enddo

      if(ibe.eq.0)then
#ifndef _GPUDIRECT18
        !$acc update device(neweast)
#endif
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        !$omp parallel do default(shared) private(i,j)
        do j=1,nj
        do i=1,cmp
          s(ni+i,j)=neweast(i,j)
        enddo
        enddo
      endif

      if(ibw.eq.0)then
#ifndef _GPUDIRECT18
        !$acc update device(newwest)
#endif
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        !$omp parallel do default(shared) private(i,j)
        do j=1,nj
        do i=1,cmp
          s(i-cmp,j)=newwest(i,j)
        enddo
        enddo
      endif

!----------

      if(timestats.ge.1) time_mpq2=time_mpq2+mytime()
      !!$acc update device(s)
      end subroutine comm_2dew_end_GPU



!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_2dns_end(s,south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,ni,nj,cmp,ibe,ibw,ibs,ibn, &
          wbc,ebc,sbc,nbc,ierr,timestats,time_mpq2,mytime
      use mpi
      implicit none

      real, dimension(ib:ie,jb:je) :: s
      real, dimension(ni,cmp) :: south,newsouth,north,newnorth
      integer reqs(8)

      integer i,j,nn,nr1,nr,index
      integer, dimension(mpi_status_size,8) :: status1

!-------------------------------------------------------------------

      nr1 = 0
      if(ibe.eq.0)then
        nr1 = nr1 + 1
      endif
      if(ibw.eq.0)then
        nr1 = nr1 + 1
      endif

      nr = 0
      if(ibs.eq.0)then
        nr = nr + 1
      endif
      if(ibn.eq.0)then
        nr = nr + 1
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(nr1+1:nr1+nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
      enddo

      if(ibs.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=1,cmp
        do i=1,ni
          s(i,j-cmp)=newsouth(i,j)
        enddo
        enddo
      endif

      if(ibn.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=1,cmp
        do i=1,ni
          s(i,nj+j)=newnorth(i,j)
        enddo
        enddo
      endif

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif

      if(ibe.eq.0)then
        nr = nr+1
      endif

      if(ibn.eq.0)then
        nr = nr+1
      endif

      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

!----------

      if(timestats.ge.1) time_mpq2=time_mpq2+mytime()

      end subroutine comm_2dns_end

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_2dns_end_GPU(s,south,newsouth,north,newnorth,reqs)
      use input, only : ib,ie,jb,je,ni,nj,cmp,ibe,ibw,ibs,ibn, &
          wbc,ebc,sbc,nbc,ierr,timestats,time_mpq2,mytime
      use mpi
      implicit none

      real, dimension(ib:ie,jb:je) :: s
      real, dimension(ni,cmp) :: south,newsouth,north,newnorth
      integer reqs(8)

      integer i,j,nn,nr1,nr,index
      integer, dimension(mpi_status_size,8) :: status1
      logical, parameter :: Debug = .FALSE.

!-------------------------------------------------------------------
#ifndef _GPUDIRECT18
if(Debug) print *, "comm_2dns_end_GPU: Im called"
#endif

      nr1 = 0
      if(ibe.eq.0)then
        nr1 = nr1 + 1
      endif
      if(ibw.eq.0)then
        nr1 = nr1 + 1
      endif

      nr = 0
      if(ibs.eq.0)then
        nr = nr + 1
      endif
      if(ibn.eq.0)then
        nr = nr + 1
      endif

      !!$acc update host(s)
      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(nr1+1:nr1+nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
      enddo

      if(ibs.eq.0)then
#ifndef _GPUDIRECT18
        !$acc update device(newsouth)
#endif
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        !$omp parallel do default(shared) private(i,j)
        do j=1,cmp
        do i=1,ni
          s(i,j-cmp)=newsouth(i,j)
        enddo
        enddo
      endif

      if(ibn.eq.0)then
#ifndef _GPUDIRECT18
        !$acc update device(newnorth)
#endif
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        !$omp parallel do default(shared) private(i,j)
        do j=1,cmp
        do i=1,ni
          s(i,nj+j)=newnorth(i,j)
        enddo
        enddo
      endif

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif

      if(ibe.eq.0)then
        nr = nr+1
      endif

      if(ibn.eq.0)then
        nr = nr+1
      endif

      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif

!----------

      if(timestats.ge.1) time_mpq2=time_mpq2+mytime()
!      !$acc update device(s)
      end subroutine comm_2dns_end_GPU



!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_2d_corner(s)
      use input, only : ib,ie,jb,je,ni,nj,ibe,ibw,ibs,ibn, &
          mynw,mysw,myne,myse,timestats,time_mptk1,mytime,ierr
      use mpi
      implicit none

      real, dimension(ib:ie,jb:je) :: s

      integer reqs(8)
      integer :: tag1,tag2,tag3,tag4,nr,nrb
      integer, dimension(mpi_status_size,8) :: status1

!------------------------------------------------

      nr = 0

!-----

      tag1=5061

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
        call mpi_irecv(s(0,nj+1),1,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
      endif

!-----

      tag2=5062

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
        call mpi_irecv(s(0,0),1,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
      endif

!-----

      tag3=5063

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
        call mpi_irecv(s(ni+1,nj+1),1,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
      endif

!-----

      tag4=5064

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
        call mpi_irecv(s(ni+1,0),1,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nrb = 4

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nrb = nrb + 1
        call mpi_isend(s(ni,1),1,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
      endif

!-----

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nrb = nrb + 1
        call mpi_isend(s(ni,nj),1,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
      endif

!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nrb = nrb + 1
        call mpi_isend(s(1,1),1,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
      endif

!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nrb = nrb + 1
        call mpi_isend(s(1,nj),1,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      if( nr.ge.1 )then
        call MPI_WAITALL(nr,reqs(1:nr),status1(1:mpi_status_size,1:nr),ierr)
      endif

!-----

    nrb = nrb-4

    if( nrb.ge.1 )then
      call MPI_WAITALL(nrb,reqs(5:5+nrb-1),status1(1:mpi_status_size,5:5+nrb-1),ierr)
    endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()

      end subroutine comm_2d_corner



!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_2d_corner_GPU(s)
      use input, only : ib,ie,jb,je,ni,nj,ibe,ibw,ibs,ibn, &
          mynw,mysw,myne,myse,timestats,time_mptk1,mytime,ierr
      use mpi
      implicit none

      real, dimension(ib:ie,jb:je) :: s

      integer reqs(8)
      integer :: tag1,tag2,tag3,tag4,nr,nrb
      integer, dimension(mpi_status_size,8) :: status1
      logical, parameter :: Debug = .FALSE.
!------------------------------------------------

      nr = 0

!-----
     if(Debug) print *,'comm_2d_corner_GPU:'
      tag1=5061
      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
        call mpi_irecv(s(0,nj+1),1,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
      endif

!-----

      tag2=5062

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
        call mpi_irecv(s(0,0),1,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
      endif

!-----

      tag3=5063

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
        call mpi_irecv(s(ni+1,nj+1),1,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
      endif

!-----

      tag4=5064

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
        call mpi_irecv(s(ni+1,0),1,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nrb = 4

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nrb = nrb + 1
        !$acc update host(s(ni,1))
        call mpi_isend(s(ni,1),1,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
      endif

!-----

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nrb = nrb + 1
        !$acc update host(s(ni,nj))
        call mpi_isend(s(ni,nj),1,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
      endif

!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nrb = nrb + 1
        !$acc update host(s(1,1))
        call mpi_isend(s(1,1),1,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
      endif

!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nrb = nrb + 1
         !$acc update host(s(1,nj))
        call mpi_isend(s(1,nj),1,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(nrb),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      if( nr.ge.1 )then
        call MPI_WAITALL(nr,reqs(1:nr),status1(1:mpi_status_size,1:nr),ierr)
      endif

!-----

    nrb = nrb-4

    if( nrb.ge.1 )then
      call MPI_WAITALL(nrb,reqs(5:5+nrb-1),status1(1:mpi_status_size,5:5+nrb-1),ierr)
    endif
#ifdef _OPENACC
    if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
      !$acc update device(s(ni+1,0))
    endif
    if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
      !$acc update device(s(ni+1,nj+1))
    endif
    if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
      !$acc update device(s(0,0))
    endif
    if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
      !$acc update device(s(0,nj+1))
    endif
#endif

!-----
      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()

      end subroutine comm_2d_corner_GPU

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine getcorner3_2d(s)
      use input, only : ib,ie,jb,je,ni,nj,cmp,ibe,ibw,ibs,ibn, &
          mynw,mysw,myne,myse,timestats,time_mptk1,mytime,ierr
      use mpi
      implicit none
 
      real, intent(inout), dimension(ib:ie,jb:je) :: s
 
      real, dimension(cmp,cmp) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      integer :: i,j,nn,nr,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag1,tag2,tag3,tag4

!-----

      nr = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1

!------------------------------------------------------------------

      tag1=5001

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
        call mpi_irecv(nw2,cmp*cmp,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        index_nw = nr
      endif

!-----

      tag2=5002

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
        call mpi_irecv(sw2,cmp*cmp,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        index_sw = nr
      endif

!-----

      tag3=5003

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
        call mpi_irecv(ne2,cmp*cmp,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        index_ne = nr
      endif

!-----

      tag4=5004

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
        call mpi_irecv(se2,cmp*cmp,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        index_se = nr
      endif

!------------------------------------------------------------------

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        do j=1,cmp
        do i=1,cmp
          se1(i,j)=s(ni-cmp+i,j)
        enddo
        enddo
        call mpi_isend(se1,cmp*cmp,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(5),ierr)
      endif
 
!-----

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        do j=1,cmp
        do i=1,cmp
          ne1(i,j)=s(ni-cmp+i,nj-cmp+j)
        enddo
        enddo
        call mpi_isend(ne1,cmp*cmp,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(6),ierr)
      endif
 
!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        do j=1,cmp
        do i=1,cmp
          sw1(i,j)=s(i,j)
        enddo
        enddo
        call mpi_isend(sw1,cmp*cmp,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(7),ierr)
      endif
 
!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        do j=1,cmp
        do i=1,cmp
          nw1(i,j)=s(i,nj-cmp+j)
        enddo
        enddo
        call mpi_isend(nw1,cmp*cmp,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(8),ierr)
      endif
 
!-----

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_nw)then
        do j=1,cmp
        do i=1,cmp
          s(-cmp+i,nj+j)=nw2(i,j)
        enddo
        enddo
      elseif(index.eq.index_sw)then
        do j=1,cmp
        do i=1,cmp
          s(-cmp+i,-cmp+j)=sw2(i,j)
        enddo
        enddo
      elseif(index.eq.index_ne)then
        do j=1,cmp
        do i=1,cmp
          s(ni+i,nj+j)=ne2(i,j)
        enddo
        enddo
      elseif(index.eq.index_se)then
        do j=1,cmp
        do i=1,cmp
          s(ni+i,-cmp+j)=se2(i,j)
        enddo
        enddo
      endif

      enddo

!-----

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        call MPI_WAIT (reqs(5),MPI_STATUS_IGNORE,ierr)
      endif

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        call MPI_WAIT (reqs(6),MPI_STATUS_IGNORE,ierr)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        call MPI_WAIT (reqs(7),MPI_STATUS_IGNORE,ierr)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        call MPI_WAIT (reqs(8),MPI_STATUS_IGNORE,ierr)
      endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()

      end subroutine getcorner3_2d

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 

      subroutine comm_all_s(s,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,  &
                              n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s)

      use input, only : ib,ie,jb,je,kb,ke,imp,jmp,kmp,cmp,rmp,kmt
      use bc_module, only: bcs2
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: s
      real, intent(inout), dimension(cmp,jmp,kmp)   :: sw31,sw32,se31,se32
      real, intent(inout), dimension(imp,cmp,kmp)   :: ss31,ss32,sn31,sn32
      real, intent(inout), dimension(cmp,cmp,kmt+1) :: n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2
      integer, intent(inout), dimension(rmp) :: reqs_s

      call comm_3s_start(s,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,reqs_s)
      call comm_3s_end(  s,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,reqs_s)
      call getcorner3(s,n3w1(1,1,1),n3w2(1,1,1),n3e1(1,1,1),n3e2(1,1,1),   &
                        s3w1(1,1,1),s3w2(1,1,1),s3e1(1,1,1),s3e2(1,1,1))
      call bcs2(s)

      end subroutine comm_all_s

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine comm_all_s_GPU(s,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,  &
                              n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s)

      use input, only : ib,ie,jb,je,kb,ke,imp,jmp,kmp,cmp,rmp,kmt
      use bc_module, only: bcs2_GPU
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: s
      real, intent(inout), dimension(cmp,jmp,kmp)   :: sw31,sw32,se31,se32
      real, intent(inout), dimension(imp,cmp,kmp)   :: ss31,ss32,sn31,sn32
      real, intent(inout), dimension(cmp,cmp,kmt+1) :: n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2
      integer, intent(inout), dimension(rmp) :: reqs_s
      logical, parameter :: Debug = .FALSE.

      if(Debug) print *,'comm_all_s_GPU:'

      !$acc data present (s,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32, &
      !$acc               n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s)

      call comm_3s_start_GPU(s,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,reqs_s)
      call comm_3s_end_GPU(  s,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,reqs_s)
      call getcorner3_GPU(s,n3w1(1,1,1),n3w2(1,1,1),n3e1(1,1,1),n3e2(1,1,1),   &
                        s3w1(1,1,1),s3w2(1,1,1),s3e1(1,1,1),s3e2(1,1,1))
      call bcs2_GPU(s)

      !$acc end data
      end subroutine comm_all_s_GPU

#endif
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine prepcorners_GPU(s,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,  &
                               pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p,comm)
      use input, only : ib,ie,jb,je,kb,ke,imp,jmp,kmp,cmp,rmp,kmt,ni,nj,nk, &
          tbc,bbc,cgs1,cgs2,cgs3,cgt1,cgt2,cgt3
      use bc_module, only: bcs_GPU,bcs2_GPU
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: s
      real, intent(inout), dimension(kmt) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      real, intent(inout), dimension(jmp,kmp) :: pw1,pw2,pe1,pe2
      real, intent(inout), dimension(imp,kmp) :: ps1,ps2,pn1,pn2
      integer, intent(inout), dimension(rmp) :: reqs_p
      integer, intent(in) :: comm

      integer :: i,j
      logical, parameter :: Debug = .FALSE.

      !$acc data present(s,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2, &
      !$acc              pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2)

      if(Debug) print *,'prepcorners_GPU'
!--------------------------------------------!
!  This subroutine is ONLY for parcel_interp !
!--------------------------------------------!

      IF( comm.eq.1 )THEN
        call bcs_GPU(s)
      ENDIF
#ifdef MPI
      if(Debug) print *,'prepcorners_GPU point #1'
      IF( comm.eq.1 )THEN
        call comm_1s_start_GPU(s,pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p)
        call comm_1s_end_GPU(  s,pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p)
      ENDIF
      if(Debug) print *,'prepcorners_GPU point #2'
      call getcorner_GPU(s,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1))
      if(Debug) print *,'prepcorners_GPU point #3'
      call bcs2_GPU(s)
      if(Debug) print *,'prepcorners_GPU point #4'
#endif

      IF( bbc.eq.1 .or. bbc.eq.2 .or. bbc.eq.3 )THEN
        ! extrapolate:
        !$omp parallel do default(shared) private(i,j)
        !$acc parallel loop gang vector collapse(2) default(present)
        do j=0,nj+1
        do i=0,ni+1
          s(i,j,0) = cgs1*s(i,j,1)+cgs2*s(i,j,2)+cgs3*s(i,j,3)
        enddo
        enddo
      ENDIF

      IF( tbc.eq.1 .or. tbc.eq.2 )THEN
        ! extrapolate:
        !$omp parallel do default(shared) private(i,j)
        !$acc parallel loop gang vector collapse(2) default(present) 
        do j=0,nj+1
        do i=0,ni+1
          s(i,j,nk+1) = cgt1*s(i,j,nk)+cgt2*s(i,j,nk-1)+cgt3*s(i,j,nk-2)
        enddo
        enddo
      ENDIF

      !$acc end data

      end subroutine prepcorners_GPU

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine prepcorners3_GPU(s,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,  &
                                n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s,comm)
      use input, only : ib,ie,jb,je,kb,ke,imp,jmp,kmp,cmp,rmp,kmt,ni,nj,nk, &
          tbc,bbc,cgs1,cgs2,cgs3,cgt1,cgt2,cgt3
      use bc_module, only: bcs_GPU
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: s
      real, intent(inout), dimension(cmp,jmp,kmp)   :: sw31,sw32,se31,se32
      real, intent(inout), dimension(imp,cmp,kmp)   :: ss31,ss32,sn31,sn32
      real, intent(inout), dimension(cmp,cmp,kmt+1) :: n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2
      integer, intent(inout), dimension(rmp) :: reqs_s
      integer, intent(in) :: comm
      logical, parameter :: Debug = .FALSE.
      integer :: i,j
      !$acc data present (s,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32, &
      !$acc               n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2, &
      !$acc               reqs_s)

if(Debug) print *, "prepcorners3_GPU: Im called"

      IF( comm.eq.1 )THEN
        call bcs_GPU(s)
#ifdef MPI
        call comm_all_s_GPU(s,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,  &
                          n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s)
#endif
      ENDIF

      IF( bbc.eq.1 .or. bbc.eq.2 .or. bbc.eq.3 )THEN
        ! extrapolate:
!$omp parallel do default(shared)  &
!$omp private(i,j)
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=jb,je
        do i=ib,ie
          s(i,j,0) = cgs1*s(i,j,1)+cgs2*s(i,j,2)+cgs3*s(i,j,3)
        enddo
        enddo
      ENDIF

      IF( tbc.eq.1 .or. tbc.eq.2 )THEN
        ! extrapolate:
!$omp parallel do default(shared)  &
!$omp private(i,j)
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=jb,je
        do i=ib,ie
          s(i,j,nk+1) = cgt1*s(i,j,nk)+cgt2*s(i,j,nk-1)+cgt3*s(i,j,nk-2)
        enddo
        enddo
      ENDIF

      !$acc end data

      end subroutine prepcorners3_GPU

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine prepcornert_GPU(t,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,  &
                               tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2,reqs_p,comm)
      use input, only : ib,ie,jb,je,kb,ke,imp,jmp,kmp,cmp,rmp,kmt,ni,nj,nk, &
          tbc,bbc,cgs1,cgs2,cgs3,cgt1,cgt2,cgt3
      use bc_module, only: bcw_GPU, bct2_GPU
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: t
      real, intent(inout), dimension(kmt) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      real, intent(inout), dimension(cmp,jmp,kmt) :: tkw1,tkw2,tke1,tke2
      real, intent(inout), dimension(imp,cmp,kmt) :: tks1,tks2,tkn1,tkn2
      integer, intent(inout), dimension(rmp) :: reqs_p
      integer, intent(in) :: comm
      logical, parameter :: Debug = .FALSE.
      integer :: i,j

!--------------------------------------------!
!  This subroutine is ONLY for parcel_interp !
!--------------------------------------------!

      !$acc data present (t,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2, &
      !$acc               tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2)

      if(Debug) print *,'prepcornert_GPU'
      IF( comm.eq.1 )THEN
        call bcw_GPU(t,0)
      ENDIF
#ifdef MPI
      IF( comm.eq.1 )THEN
        call comm_1t_start_GPU(t,tkw1(1,1,1),tkw2(1,1,1),tke1(1,1,1),tke2(1,1,1),tks1(1,1,1),tks2(1,1,1),tkn1(1,1,1),tkn2(1,1,1),reqs_p)
        call comm_1t_end_GPU(  t,tkw1(1,1,1),tkw2(1,1,1),tke1(1,1,1),tke2(1,1,1),tks1(1,1,1),tks2(1,1,1),tkn1(1,1,1),tkn2(1,1,1),reqs_p)
      ENDIF
      call getcornert_GPU(t,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      call bct2_GPU(t)
#endif

      !$acc end data

      end subroutine prepcornert_GPU

#ifdef MPI
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getcorneru3(u,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibw,ibe,ibn,ibs, &
          wbc,ebc,nbc,sbc,ierr,timestats,time_mptk1,mytime,mynw,mysw,myne,myse
      use mpi
      implicit none

      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: u
      real, intent(inout), dimension(cmp,cmp,nk) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2

      integer :: i,j,k,nn,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag1,tag2,tag3,tag4,count,nr1,nr2
      integer, dimension(mpi_status_size,8) :: status1

      count=cmp*cmp*nk
      nr1 = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1

!-----

      tag1=5031

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr1 = nr1+1
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        index_nw = nr1
      endif

!-----

      tag2=5032

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr1 = nr1+1
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        index_sw = nr1
      endif

!-----

      tag3=5033

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr1 = nr1+1
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        index_ne = nr1
      endif

!-----

      tag4=5034

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr1 = nr1+1
        call mpi_irecv(se2,count,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        index_se = nr1
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr2 = 0

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          se1(i,j,k)=u(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
        call mpi_isend(se1,count,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
      endif

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          ne1(i,j,k)=u(ni-cmp+i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
        call mpi_isend(ne1,count,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          sw1(i,j,k)=u(1+i,j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          nw1(i,j,k)=u(1+i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nn = 1
      do while( nn .le. nr1 )

        call MPI_WAITANY(nr1,reqs(1:nr1),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if( index.eq.index_nw )then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          u(-cmp+i,nj+j,k)=nw2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_sw )then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          u(-cmp+i,-cmp+j,k)=sw2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_ne )then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          u(ni+1+i,nj+j,k)=ne2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_se )then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          u(ni+1+i,-cmp+j,k)=se2(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

    if( nr2.ge.1 )then
      call MPI_WAITALL(nr2,reqs(5:5+nr2-1),status1(1:mpi_status_size,5:5+nr2-1),ierr)
    endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()

      end subroutine getcorneru3

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getcorneru3_GPU(u,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibw,ibe,ibn,ibs, &
          wbc,ebc,nbc,sbc,ierr,timestats,time_mptk1,mytime,mynw,mysw,myne,myse
      use mpi
      implicit none

      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: u
      real, intent(inout), dimension(cmp,cmp,nk) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2

      integer :: i,j,k,nn,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag1,tag2,tag3,tag4,count,nr1,nr2
      integer, dimension(mpi_status_size,8) :: status1
      logical, parameter :: Debug = .FALSE.

#ifndef _GPUDIRECT19
      if(Debug) print *,'getcorneru3_GPU:'
#endif
      count=cmp*cmp*nk
      nr1 = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1
!-----

      tag1=5031

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr1 = nr1+1
#ifdef _GPUDIRECT19
        !$acc host_data use_device(nw2)
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        !$acc end host_data
#else
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
#endif
        index_nw = nr1
      endif

!-----

      tag2=5032

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr1 = nr1+1
#ifdef _GPUDIRECT19
        !$acc host_data use_device(sw2)
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        !$acc end host_data
#else
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
#endif
        index_sw = nr1
      endif

!-----

      tag3=5033

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr1 = nr1+1
#ifdef _GPUDIRECT19
        !$acc host_data use_device(ne2)
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        !$acc end host_data
#else
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
#endif
        index_ne = nr1
      endif

!-----

      tag4=5034

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr1 = nr1+1
#ifdef _GPUDIRECT19
        !$acc host_data use_device(se2)
        call mpi_irecv(se2,count,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        !$acc end host_data
#else
        call mpi_irecv(se2,count,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
#endif
        index_se = nr1
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr2 = 0

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          se1(i,j,k)=u(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
#ifdef _GPUDIRECT19
        !$acc host_data use_device(se1)
        call mpi_isend(se1,count,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
        !$acc end host_data
#else
        !$acc update host(se1)
        call mpi_isend(se1,count,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
#endif
      endif

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          ne1(i,j,k)=u(ni-cmp+i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
#ifdef _GPUDIRECT19
        !$acc host_data use_device(ne1)
        call mpi_isend(ne1,count,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
        !$acc end host_data
#else
        !$acc update host(ne1)
        call mpi_isend(ne1,count,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
#endif
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          sw1(i,j,k)=u(1+i,j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
#ifdef _GPUDIRECT19
        !$acc host_data use_device(sw1)
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
        !$acc end host_data
#else
        !$acc update host(sw1)
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
#endif
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          nw1(i,j,k)=u(1+i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
#ifdef _GPUDIRECT19
        !$acc host_data use_device(nw1)
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
        !$acc end host_data
#else
        !$acc update host(nw1)
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nn = 1
      do while( nn .le. nr1 )

        call MPI_WAITANY(nr1,reqs(1:nr1),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if( index.eq.index_nw )then
#ifndef _GPUDIRECT19
        !$acc update device(nw2)
#endif
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          u(-cmp+i,nj+j,k)=nw2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_sw )then
#ifndef _GPUDIRECT19
        !$acc update device(sw2) 
#endif
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          u(-cmp+i,-cmp+j,k)=sw2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_ne )then
#ifndef _GPUDIRECT19
        !$acc update device(ne2)
#endif
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          u(ni+1+i,nj+j,k)=ne2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_se )then
#ifndef _GPUDIRECT19
        !$acc update device(se2)
#endif
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          u(ni+1+i,-cmp+j,k)=se2(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

    if( nr2.ge.1 )then
      call MPI_WAITALL(nr2,reqs(5:5+nr2-1),status1(1:mpi_status_size,5:5+nr2-1),ierr)
    endif

!-----
      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()

      end subroutine getcorneru3_GPU


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getcornerv3(v,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibw,ibe,ibn,ibs, &
          wbc,ebc,nbc,sbc,ierr,timestats,time_mptk1,mytime,mynw,mysw,myne,myse
      use mpi
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: v
      real, intent(inout), dimension(cmp,cmp,nk) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2

      integer :: i,j,k,nn,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag1,tag2,tag3,tag4,count,nr1,nr2
      integer, dimension(mpi_status_size,8) :: status1

      count=cmp*cmp*nk
      nr1 = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1

!-----

      tag1=5041

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr1 = nr1+1
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        index_nw = nr1
      endif

!-----

      tag2=5042

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr1 = nr1+1
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        index_sw = nr1
      endif

!-----

      tag3=5043

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr1 = nr1+1
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        index_ne = nr1
      endif

!-----

      tag4=5044

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr1 = nr1+1
        call mpi_irecv(se2,count,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        index_se = nr1
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr2 = 0

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          se1(i,j,k)=v(ni-cmp+i,1+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
        call mpi_isend(se1,count,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
      endif

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          ne1(i,j,k)=v(ni-cmp+i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
        call mpi_isend(ne1,count,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          sw1(i,j,k)=v(i,1+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          nw1(i,j,k)=v(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nn = 1
      do while( nn .le. nr1 )

        call MPI_WAITANY(nr1,reqs(1:nr1),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if( index.eq.index_nw )then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          v(-cmp+i,nj+1+j,k)=nw2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_sw )then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          v(-cmp+i,-cmp+j,k)=sw2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_ne )then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          v(ni+i,nj+1+j,k)=ne2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_se )then
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          v(ni+i,-cmp+j,k)=se2(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

    if( nr2.ge.1 )then
      call MPI_WAITALL(nr2,reqs(5:5+nr2-1),status1(1:mpi_status_size,5:5+nr2-1),ierr)
    endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()

      end subroutine getcornerv3

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getcornerv3_GPU(v,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,cmp,ibw,ibe,ibn,ibs, &
          wbc,ebc,nbc,sbc,ierr,timestats,time_mptk1,mytime,mynw,mysw,myne,myse
      use mpi
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: v
      real, intent(inout), dimension(cmp,cmp,nk) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2

      integer :: i,j,k,nn,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag1,tag2,tag3,tag4,count,nr1,nr2
      integer, dimension(mpi_status_size,8) :: status1
      logical, parameter :: Debug = .FALSE.

#ifndef _GPUDIRECT20
      if(Debug) print *,'getcornerv3_GPU:'
#endif
      count=cmp*cmp*nk
      nr1 = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1

!-----
      tag1=5041

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr1 = nr1+1
#ifdef _GPUDIRECT20
        !$acc host_data use_device(nw2)
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        !$acc end host_data
#else
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
#endif
        index_nw = nr1
      endif

!-----

      tag2=5042

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr1 = nr1+1
#ifdef _GPUDIRECT20
        !$acc host_data use_device(sw2)
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        !$acc end host_data
#else
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
#endif
        index_sw = nr1
      endif

!-----

      tag3=5043

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr1 = nr1+1
#ifdef _GPUDIRECT20
        !$acc host_data use_device(ne2)
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        !$acc end host_data
#else
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
#endif
        index_ne = nr1
      endif

!-----

      tag4=5044

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr1 = nr1+1
#ifdef _GPUDIRECT20
        !$acc host_data use_device(se2)
        call mpi_irecv(se2,count,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        !$acc end host_data
#else
        call mpi_irecv(se2,count,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
#endif
        index_se = nr1
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr2 = 0

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          se1(i,j,k)=v(ni-cmp+i,1+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
#ifdef _GPUDIRECT20
        !$acc host_data use_device(se1)
        call mpi_isend(se1,count,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
        !$acc end host_data
#else
        !$acc update host(se1)
        call mpi_isend(se1,count,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
#endif
      endif

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          ne1(i,j,k)=v(ni-cmp+i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
#ifdef _GPUDIRECT20
        !$acc host_data use_device(ne1)
        call mpi_isend(ne1,count,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
        !$acc end host_data
#else
        !$acc update host(ne1)
        call mpi_isend(ne1,count,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
#endif
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          sw1(i,j,k)=v(i,1+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
#ifdef _GPUDIRECT20
        !$acc host_data use_device(sw1)
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
        !$acc end host_data
#else
        !$acc update host(sw1)
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
#endif
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          nw1(i,j,k)=v(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
#ifdef _GPUDIRECT20
        !$acc host_data use_device(nw1)
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
        !$acc end host_data
#else
        !$acc update host(nw1)
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nn = 1
      do while( nn .le. nr1 )

        call MPI_WAITANY(nr1,reqs(1:nr1),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if( index.eq.index_nw )then
#ifndef _GPUDIRECT20
        !$acc update device(nw2)
#endif
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          v(-cmp+i,nj+1+j,k)=nw2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_sw )then
#ifndef _GPUDIRECT20
        !$acc update device(sw2)
#endif
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          v(-cmp+i,-cmp+j,k)=sw2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_ne )then
#ifndef _GPUDIRECT20
        !$acc update device(ne2)
#endif
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          v(ni+i,nj+1+j,k)=ne2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_se )then
#ifndef _GPUDIRECT20
        !$acc update device(se2)
#endif
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,cmp
        do i=1,cmp
          v(ni+i,-cmp+j,k)=se2(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

    if( nr2.ge.1 )then
      call MPI_WAITALL(nr2,reqs(5:5+nr2-1),status1(1:mpi_status_size,5:5+nr2-1),ierr)
    endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()

      end subroutine getcornerv3_GPU



!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getcornerw3(w,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,nkp1,cmp,ibw,ibe,ibn,ibs, &
          wbc,ebc,nbc,sbc,ierr,timestats,time_mptk1,mytime,mynw,mysw,myne,myse
      use mpi
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: w
      real, intent(inout), dimension(cmp,cmp,nk+1) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2

      integer :: i,j,k,nn,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag1,tag2,tag3,tag4,count,nr1,nr2
      integer, dimension(mpi_status_size,8) :: status1

      count=cmp*cmp*nkp1
      nr1 = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1

!-----

      tag1=5051

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr1 = nr1+1
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        index_nw = nr1
      endif

!-----

      tag2=5052

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr1 = nr1+1
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        index_sw = nr1
      endif

!-----

      tag3=5053

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr1 = nr1+1
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        index_ne = nr1
      endif

!-----

      tag4=5054

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr1 = nr1+1
        call mpi_irecv(se2,count,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        index_se = nr1
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr2 = 0

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          se1(i,j,k)=w(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
        call mpi_isend(se1,count,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
      endif

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          ne1(i,j,k)=w(ni-cmp+i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
        call mpi_isend(ne1,count,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          sw1(i,j,k)=w(i,j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          nw1(i,j,k)=w(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nn = 1
      do while( nn .le. nr1 )

        call MPI_WAITANY(nr1,reqs(1:nr1),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if( index.eq.index_nw )then
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          w(-cmp+i,nj+j,k)=nw2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_sw )then
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          w(-cmp+i,-cmp+j,k)=sw2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_ne )then
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          w(ni+i,nj+j,k)=ne2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_se )then
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          w(ni+i,-cmp+j,k)=se2(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

    if( nr2.ge.1 )then
      call MPI_WAITALL(nr2,reqs(5:5+nr2-1),status1(1:mpi_status_size,5:5+nr2-1),ierr)
    endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()

      end subroutine getcornerw3

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getcornerw3_GPU(w,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,nkp1,cmp,ibw,ibe,ibn,ibs, &
          wbc,ebc,nbc,sbc,ierr,timestats,time_mptk1,mytime,mynw,mysw,myne,myse
      use mpi
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: w
      real, intent(inout), dimension(cmp,cmp,nk+1) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2

      integer :: i,j,k,nn,index
      integer :: index_nw,index_sw,index_ne,index_se
      integer reqs(8)
      integer tag1,tag2,tag3,tag4,count,nr1,nr2
      integer, dimension(mpi_status_size,8) :: status1
      logical, parameter :: Debug = .FALSE.

#ifndef _GPUDIRECT21
      if(Debug) print *,'getcornerw3_GPU:'
#endif
      count=cmp*cmp*nkp1
      nr1 = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1

!-----
      tag1=5051

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr1 = nr1+1
#ifdef _GPUDIRECT21
        !$acc host_data use_device(nw2)
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        !$acc end host_data
#else
        call mpi_irecv(nw2,count,MPI_REAL,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
#endif
        index_nw = nr1
      endif

!-----

      tag2=5052

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr1 = nr1+1
#ifdef _GPUDIRECT21
        !$acc host_data use_device(sw2)
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        !$acc end host_data
#else
        call mpi_irecv(sw2,count,MPI_REAL,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
#endif
        index_sw = nr1
      endif

!-----

      tag3=5053

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr1 = nr1+1
#ifdef _GPUDIRECT21
        !$acc host_data use_device(ne2)
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        !$acc end host_data
#else
        call mpi_irecv(ne2,count,MPI_REAL,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
#endif
        index_ne = nr1
      endif

!-----

      tag4=5054

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr1 = nr1+1
#ifdef _GPUDIRECT21
        !$acc host_data use_device(se2)
        call mpi_irecv(se2,count,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
        !$acc end host_data
#else
        call mpi_irecv(se2,count,MPI_REAL,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr1),ierr)
#endif
        index_se = nr1
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr2 = 0

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          se1(i,j,k)=w(ni-cmp+i,j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
#ifdef _GPUDIRECT21
        !$acc host_data use_device(se1)
        call mpi_isend(se1,count,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
        !$acc end host_data
#else
        !$acc update host(se1)
        call mpi_isend(se1,count,MPI_REAL,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
#endif
      endif

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          ne1(i,j,k)=w(ni-cmp+i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
#ifdef _GPUDIRECT21
        !$acc host_data use_device(ne1)
        call mpi_isend(ne1,count,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
        !$acc end host_data
#else
        !$acc update host(ne1)
        call mpi_isend(ne1,count,MPI_REAL,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
#endif
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          sw1(i,j,k)=w(i,j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
#ifdef _GPUDIRECT21
        !$acc host_data use_device(sw1)
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
        !$acc end host_data
#else
        !$acc update host(sw1)
        call mpi_isend(sw1,count,MPI_REAL,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
#endif
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          nw1(i,j,k)=w(i,nj-cmp+j,k)
        enddo
        enddo
        enddo
        nr2 = nr2+1
#ifdef _GPUDIRECT21
        !$acc host_data use_device(nw1)
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
        !$acc end host_data
#else
        !$acc update host(nw1)
        call mpi_isend(nw1,count,MPI_REAL,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(4+nr2),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nn = 1
      do while( nn .le. nr1 )

        call MPI_WAITANY(nr1,reqs(1:nr1),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if( index.eq.index_nw )then
#ifndef _GPUDIRECT21
        !$acc update device(nw2)
#endif
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          w(-cmp+i,nj+j,k)=nw2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_sw )then
#ifndef _GPUDIRECT21
        !$acc update device(sw2)
#endif
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          w(-cmp+i,-cmp+j,k)=sw2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_ne )then
#ifndef _GPUDIRECT21
        !$acc update device(ne2)
#endif
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          w(ni+i,nj+j,k)=ne2(i,j,k)
        enddo
        enddo
        enddo
      elseif( index.eq.index_se )then
#ifndef _GPUDIRECT21
        !$acc update device(se2)
#endif
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nkp1
        do j=1,cmp
        do i=1,cmp
          w(ni+i,-cmp+j,k)=se2(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

    if( nr2.ge.1 )then
      call MPI_WAITALL(nr2,reqs(5:5+nr2-1),status1(1:mpi_status_size,5:5+nr2-1),ierr)
    endif

!-----

      if(timestats.ge.1) time_mptk1=time_mptk1+mytime()
      end subroutine getcornerw3_GPU


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine comm_1s_tend_halo_GPU(s)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,ibe,ibw,ibs,ibn, &
          wbc,ebc,sbc,nbc,nf,ierr,cs1we,cs1sn,timestats,time_mps2,mytime, &
          mynw,mysw,myne,myse,myeast,mywest,mynorth,mysouth
      use mpi
      implicit none
 
      double precision, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: s

      double precision, dimension(2,nj,nk) :: west,newwest,east,neweast
      double precision, dimension(ni,2,nk) :: south,newsouth,north,newnorth
      double precision, dimension(2,2,nk) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      integer reqs(8)
 
      integer :: i,j,k,nn,nr,index
      integer :: tag1,tag2,tag3,tag4
      integer :: index_east,index_west,index_south,index_north
      integer :: index_nw,index_sw,index_ne,index_se
      integer, dimension(mpi_status_size,8) :: status1
      logical, parameter :: Debug = .FALSE.
 
!------------------------------------------------
!$acc data create(west,newwest,east,neweast,south,newsouth,north,newnorth,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
#ifndef _GPUDIRECT22
      if(Debug) print *,'comm_1s_tend_halo_GPU:'
#endif

      nr = 0

      nf=nf+1
      tag1=nf

      ! receive east
      if(ibe.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT22
        !$acc host_data use_device(neweast)
        call mpi_irecv(neweast,2*cs1we,MPI_DOUBLE_PRECISION,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(neweast,2*cs1we,MPI_DOUBLE_PRECISION,myeast,tag1,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag2=nf

      ! receive west
      if(ibw.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT22
        !$acc host_data use_device(newwest)
        call mpi_irecv(newwest,2*cs1we,MPI_DOUBLE_PRECISION,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newwest,2*cs1we,MPI_DOUBLE_PRECISION,mywest,tag2,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag3=nf

      ! receive south
      if(ibs.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT22
        !$acc host_data use_device(newsouth)
        call mpi_irecv(newsouth,2*cs1sn,MPI_DOUBLE_PRECISION,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newsouth,2*cs1sn,MPI_DOUBLE_PRECISION,mysouth,tag3,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!----------

      nf=nf+1
      tag4=nf

      ! receive north
      if(ibn.eq.0)then
        nr = nr + 1
#ifdef _GPUDIRECT22
        !$acc host_data use_device(newnorth)
        call mpi_irecv(newnorth,2*cs1sn,MPI_DOUBLE_PRECISION,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(newnorth,2*cs1sn,MPI_DOUBLE_PRECISION,mynorth,tag4,   &
                      MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!------------------------------------------------
!------------------------------------------------
!------------------------------------------------

      nr = 4
 
      ! send west
      if(ibw.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(j,k)
!$acc parallel loop gang vector collapse(2) default(present) private(j,k)
        do k=1,nk
        do j=1,nj
          west(1,j,k)=s(-1,j,k)
          west(2,j,k)=s( 0,j,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT22
        !$acc host_data use_device(west)
        call mpi_isend(west,2*cs1we,MPI_DOUBLE_PRECISION,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(west)
        call mpi_isend(west,2*cs1we,MPI_DOUBLE_PRECISION,mywest,tag1,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      ! send east
      if(ibe.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(j,k)
!$acc parallel loop gang vector collapse(2) default(present) private(j,k)
        do k=1,nk
        do j=1,nj
          east(1,j,k)=s(ni+1,j,k)
          east(2,j,k)=s(ni+2,j,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT22
        !$acc host_data use_device(east)
        call mpi_isend(east,2*cs1we,MPI_DOUBLE_PRECISION,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(east) 
        call mpi_isend(east,2*cs1we,MPI_DOUBLE_PRECISION,myeast,tag2,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      ! send north
      if(ibn.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,k)
!$acc parallel loop gang vector collapse(2) default(present) private(i,k)
        do k=1,nk
        do i=1,ni
          north(i,1,k)=s(i,nj+1,k)
          north(i,2,k)=s(i,nj+2,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT22
        !$acc host_data use_device(north)
        call mpi_isend(north,2*cs1sn,MPI_DOUBLE_PRECISION,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(north)
        call mpi_isend(north,2*cs1sn,MPI_DOUBLE_PRECISION,mynorth,tag3,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif
 
!----------
 
      ! send south
      if(ibs.eq.0)then
!$omp parallel do default(shared)   &
!$omp private(i,k)
!$acc parallel loop gang vector collapse(2) default(present) private(i,k)
        do k=1,nk
        do i=1,ni
          south(i,1,k)=s(i,-1,k)
          south(i,2,k)=s(i, 0,k)
        enddo
        enddo
        nr = nr + 1
#ifdef _GPUDIRECT22
        !$acc host_data use_device(south)
        call mpi_isend(south,2*cs1sn,MPI_DOUBLE_PRECISION,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
        !$acc end host_data
#else
        !$acc update host(south)
        call mpi_isend(south,2*cs1sn,MPI_DOUBLE_PRECISION,mysouth,tag4,   &
                       MPI_COMM_WORLD,reqs(nr),ierr)
#endif
      endif

!---------------------------------------------------------------------

      index_east = -1
      index_west = -1
      index_south = -1
      index_north = -1

      nr = 0
      if(ibe.eq.0)then
        nr = nr + 1
        index_east = nr
      endif
      if(ibw.eq.0)then
        nr = nr + 1
        index_west = nr
      endif
      if(ibs.eq.0)then
        nr = nr + 1
        index_south = nr
      endif
      if(ibn.eq.0)then
        nr = nr + 1
        index_north = nr
      endif

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1
 
      if(index.eq.index_east)then
#ifndef _GPUDIRECT22
        !$acc update device(neweast)
#endif
!$omp parallel do default(shared)   &
!$omp private(j,k)
!$acc parallel loop gang vector collapse(2) default(present) private(j,k)
        do k=1,nk
        do j=1,nj
          s(ni-1,j,k)=s(ni-1,j,k)+neweast(1,j,k)
          s(ni  ,j,k)=s(ni  ,j,k)+neweast(2,j,k)
        enddo
        enddo
      elseif(index.eq.index_west)then
#ifndef _GPUDIRECT22
        !$acc update device(newwest)
#endif
!$omp parallel do default(shared)   &
!$omp private(j,k)
!$acc parallel loop gang vector collapse(2) default(present) private(j,k)
        do k=1,nk
        do j=1,nj
          s(1,j,k)=s(1,j,k)+newwest(1,j,k)
          s(2,j,k)=s(2,j,k)+newwest(2,j,k)
        enddo
        enddo
      elseif(index.eq.index_south)then
#ifndef _GPUDIRECT22
        !$acc update device(newsouth)
#endif
!$omp parallel do default(shared)   &
!$omp private(i,k)
!$acc parallel loop gang vector collapse(2) default(present) private(i,k)
        do k=1,nk
        do i=1,ni
          s(i,1,k)=s(i,1,k)+newsouth(i,1,k)
          s(i,2,k)=s(i,2,k)+newsouth(i,2,k)
        enddo
        enddo
      elseif(index.eq.index_north)then
#ifndef _GPUDIRECT22
        !$acc update device(newnorth)
#endif
!$omp parallel do default(shared)   &
!$omp private(i,k)
!$acc parallel loop gang vector collapse(2) default(present) private(i,k)
        do k=1,nk
        do i=1,ni
          s(i,nj-1,k)=s(i,nj-1,k)+newnorth(i,1,k)
          s(i,nj  ,k)=s(i,nj  ,k)+newnorth(i,2,k)
        enddo
        enddo
      endif

      enddo

!----------

      nr = 0

      if(ibw.eq.0)then
        nr = nr+1
      endif
      if(ibe.eq.0)then
        nr = nr+1
      endif
      if(ibn.eq.0)then
        nr = nr+1
      endif
      if(ibs.eq.0)then
        nr = nr+1
      endif

    if( nr.ge.1 )then
      call MPI_WAITALL(nr,reqs(5:5+nr-1),status1(1:mpi_status_size,5:5+nr-1),ierr)
    endif
      if(timestats.ge.1) time_mps2=time_mps2+mytime()
 
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  Now, get data from corners:

      nr = 0
      index_nw = -1
      index_sw = -1
      index_ne = -1
      index_se = -1

!------------------------------------------------------------------

      tag1=5001

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT22
        !$acc host_data use_device(nw2)
        call mpi_irecv(nw2,2*2*nk,MPI_DOUBLE_PRECISION,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(nw2,2*2*nk,MPI_DOUBLE_PRECISION,mynw,tag1,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_nw = nr
      endif

!-----

      tag2=5002

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT22
        !$acc host_data use_device(sw2)
        call mpi_irecv(sw2,2*2*nk,MPI_DOUBLE_PRECISION,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(sw2,2*2*nk,MPI_DOUBLE_PRECISION,mysw,tag2,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_sw = nr
      endif

!-----

      tag3=5003

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT22
        !$acc host_data use_device(ne2)
        call mpi_irecv(ne2,2*2*nk,MPI_DOUBLE_PRECISION,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(ne2,2*2*nk,MPI_DOUBLE_PRECISION,myne,tag3,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_ne = nr
      endif

!-----

      tag4=5004

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        nr = nr + 1
#ifdef _GPUDIRECT22
        !$acc host_data use_device(se2)
        call mpi_irecv(se2,2*2*nk,MPI_DOUBLE_PRECISION,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
        !$acc end host_data
#else
        call mpi_irecv(se2,2*2*nk,MPI_DOUBLE_PRECISION,myse,tag4,MPI_COMM_WORLD,   &
                       reqs(nr),ierr)
#endif
        index_se = nr
      endif

!------------------------------------------------------------------

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,2
        do i=1,2
          se1(i,j,k)=s(ni+i,-2+j,k)
        enddo
        enddo
        enddo

#ifdef _GPUDIRECT22
        !$acc host_data use_device(se1)
        call mpi_isend(se1,2*2*nk,MPI_DOUBLE_PRECISION,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(5),ierr)
        !$acc end host_data
#else
        !$acc update host(se1)
        call mpi_isend(se1,2*2*nk,MPI_DOUBLE_PRECISION,myse,tag1,MPI_COMM_WORLD,   &
                       reqs(5),ierr)
#endif
      endif
 
!-----

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,2
        do i=1,2
          ne1(i,j,k)=s(ni+i,nj+j,k)
        enddo
        enddo
        enddo
#ifdef _GPUDIRECT22
        !$acc host_data use_device(ne1)
        call mpi_isend(ne1,2*2*nk,MPI_DOUBLE_PRECISION,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(6),ierr)
        !$acc end host_data
#else
        !$acc update host(ne1)
        call mpi_isend(ne1,2*2*nk,MPI_DOUBLE_PRECISION,myne,tag2,MPI_COMM_WORLD,   &
                       reqs(6),ierr)
#endif
      endif
 
!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,2
        do i=1,2
          sw1(i,j,k)=s(-2+i,-2+j,k)
        enddo
        enddo
        enddo
#ifdef _GPUDIRECT22
        !$acc host_data use_device(sw1)
        call mpi_isend(sw1,2*2*nk,MPI_DOUBLE_PRECISION,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(7),ierr)
        !$acc end host_data
#else
        !$acc update host(sw1)
        call mpi_isend(sw1,2*2*nk,MPI_DOUBLE_PRECISION,mysw,tag3,MPI_COMM_WORLD,   &
                       reqs(7),ierr)
#endif
      endif
 
!-----

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,2
        do i=1,2
          nw1(i,j,k)=s(-2+i,nj+j,k)
        enddo
        enddo
        enddo
#ifdef _GPUDIRECT22
        !$acc host_data use_device(nw1)
        call mpi_isend(nw1,2*2*nk,MPI_DOUBLE_PRECISION,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(8),ierr)
        !$acc end host_data
#else
        !$acc update host(nw1)
        call mpi_isend(nw1,2*2*nk,MPI_DOUBLE_PRECISION,mynw,tag4,MPI_COMM_WORLD,   &
                       reqs(8),ierr)
#endif
      endif
 
!-----

      nn = 1
      do while( nn .le. nr )
        call MPI_WAITANY(nr,reqs(1:nr),index,MPI_STATUS_IGNORE,ierr)
        nn = nn + 1

      if(index.eq.index_nw)then
#ifndef _GPUDIRECT22
      !$acc update device(nw2)
#endif
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,2
        do i=1,2
          s(i,nj-2+j,k)=s(i,nj-2+j,k)+nw2(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_sw)then
#ifndef _GPUDIRECT22
      !$acc update device(sw2)
#endif
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,2
        do i=1,2
          s(i,j,k)=s(i,j,k)+sw2(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_ne)then
#ifndef _GPUDIRECT22
      !$acc update device(ne2)
#endif
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,2
        do i=1,2
          s(ni-2+i,nj-2+j,k)=s(ni-2+i,nj-2+j,k)+ne2(i,j,k)
        enddo
        enddo
        enddo
      elseif(index.eq.index_se)then
#ifndef _GPUDIRECT22
      !$acc update device(se2)
#endif
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,2
        do i=1,2
          s(ni-2+i,j,k)=s(ni-2+i,j,k)+se2(i,j,k)
        enddo
        enddo
        enddo
      endif

      enddo

!-----

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        call MPI_WAIT (reqs(5),MPI_STATUS_IGNORE,ierr)
      endif

      if((ibe.eq.0.or.ibe.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        call MPI_WAIT (reqs(6),MPI_STATUS_IGNORE,ierr)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibs.eq.0.or.ibs.eq.2))then
        call MPI_WAIT (reqs(7),MPI_STATUS_IGNORE,ierr)
      endif

      if((ibw.eq.0.or.ibw.eq.2) .and. (ibn.eq.0.or.ibn.eq.2))then
        call MPI_WAIT (reqs(8),MPI_STATUS_IGNORE,ierr)
      endif
!-----
!$acc end data 

      end subroutine comm_1s_tend_halo_GPU
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

#endif

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

#ifdef MPI

    subroutine comm_droplet_number(holes_ind,num1,num2,pdata, &
                                   pdata_locind,pdata_neighbor)

      use input, only : myid,nparcelsLocal,npvals,mynw,mysw,myne, &
                        myse,myeast,mywest,mynorth,mysouth,ierr, &
                        nparcelsActive
      use constants, only: undefined_index,num_nn,inorth,isouth, &
                           iwest,ieast,inw,ine,isw,ise
      use mpi

      implicit none

      integer, intent(inout), dimension(:) :: holes_ind                    ! location index in "pdata" that can 
                                                                           ! add a new droplet;
                                                                           ! not a fixed-size array but we do 
                                                                           ! not need to know its size
      integer, intent(out) :: num1(num_nn)                                 ! Number of droplets that will enter each 
                                                                           ! nearest neighbor at different directions
      integer, intent(out) :: num2(num_nn)                                 ! Number of droplets that will enter the 
                                                                           ! current MPI region from each nearest 
                                                                           ! neighbor at different directions
      real, intent(in), dimension(nparcelsLocal,npvals) :: pdata           ! droplet information
      integer, intent(in), dimension(nparcelsLocal,3) :: pdata_locind      ! x/y/z location index of each droplet
      integer, intent(in), dimension(nparcelsActive) :: pdata_neighbor     ! array to store the new MPI region 
                                                                           ! info for all the droplets

      ! Local variables

      integer :: n_recv,n_send
      integer :: reqs(16)
      integer :: tag_n,tag_s,tag_w,tag_e,tag_nw,tag_ne,tag_sw,tag_se,indx
      integer, dimension(mpi_status_size,16) :: status1
      integer :: i,n

      ! initiate some MPI index and tag values

      tag_n  = 1001
      tag_s  = 1002
      tag_w  = 1003
      tag_e  = 1004
      tag_nw = 1005
      tag_sw = 1006
      tag_ne = 1007
      tag_se = 1008

      do i = 1, num_nn 
         num1(i) = 0
      end do

      ! find out how many droplets will leave the 
      ! current MPI region and to which neighbor

      i = 1
      do n = 1, nparcelsLocal
         if ( (pdata_locind(n,1) .eq. undefined_index) .and. &
              (pdata_locind(n,2) .eq. undefined_index) ) then
            ! record the location index that can be used to store the new droplet
            ! information after the old drolpet leaves the current MPI region
            holes_ind(i) = n
            i = i + 1
            if ( n .le. nparcelsActive ) then
               if ( pdata_neighbor(n) .eq. mynorth ) then
                  num1(inorth) = num1(inorth) + 1 
               else if ( pdata_neighbor(n) .eq. mysouth ) then
                  num1(isouth) = num1(isouth) + 1 
               else if ( pdata_neighbor(n) .eq. mywest ) then
                  num1(iwest) = num1(iwest) + 1
               else if ( pdata_neighbor(n) .eq. myeast ) then
                  num1(ieast) = num1(ieast) + 1
               else if ( pdata_neighbor(n) .eq. mynw ) then
                  num1(inw) = num1(inw) + 1
               else if ( pdata_neighbor(n) .eq. myne ) then
                  num1(ine) = num1(ine) + 1
               else if ( pdata_neighbor(n) .eq. mysw ) then
                  num1(isw) = num1(isw) + 1
               else if ( pdata_neighbor(n) .eq. myse ) then
                  num1(ise) = num1(ise) + 1
               else
                  cycle  ! this is an existing "hole" of "pdata"
                         ! no action is needed
               end if
            end if
         end if
      end do

      ! initiate the MPI non-blocking receive interface to 
      ! know how many droplets from the nearest neighbors

      n_recv = 1
      call mpi_irecv(num2(inorth),1,MPI_INT,mynorth,tag_n,MPI_COMM_WORLD, &
                     reqs(n_recv),ierr)

      n_recv = n_recv + 1
      call mpi_irecv(num2(isouth),1,MPI_INT,mysouth,tag_s,MPI_COMM_WORLD, &
                     reqs(n_recv),ierr)

      n_recv = n_recv + 1
      call mpi_irecv(num2(iwest),1,MPI_INT,mywest,tag_w,MPI_COMM_WORLD, &
                     reqs(n_recv),ierr)

      n_recv = n_recv + 1
      call mpi_irecv(num2(ieast),1,MPI_INT,myeast,tag_e,MPI_COMM_WORLD, &
                     reqs(n_recv),ierr)

      n_recv = n_recv + 1
      call mpi_irecv(num2(inw),1,MPI_INT,mynw,tag_nw,MPI_COMM_WORLD, &
                     reqs(n_recv),ierr)

      n_recv = n_recv + 1
      call mpi_irecv(num2(ine),1,MPI_INT,myne,tag_ne,MPI_COMM_WORLD, &
                     reqs(n_recv),ierr)

      n_recv = n_recv + 1
      call mpi_irecv(num2(isw),1,MPI_INT,mysw,tag_sw,MPI_COMM_WORLD, &
                     reqs(n_recv),ierr)

      n_recv = n_recv + 1
      call mpi_irecv(num2(ise),1,MPI_INT,myse,tag_se,MPI_COMM_WORLD, &
                     reqs(n_recv),ierr)

      ! initiate the MPI non-blocking send interface to 
      ! send how many droplets entering the nearest neighbor 

      n_send = 9
      call mpi_isend(num1(inorth),1,MPI_INT,mynorth,tag_s,MPI_COMM_WORLD, &
                     reqs(n_send),ierr)
  
      n_send = n_send + 1
      call mpi_isend(num1(isouth),1,MPI_INT,mysouth,tag_n,MPI_COMM_WORLD, &
                     reqs(n_send),ierr)
  
      n_send = n_send + 1
      call mpi_isend(num1(iwest),1,MPI_INT,mywest,tag_e,MPI_COMM_WORLD, &
                     reqs(n_send),ierr)
  
      n_send = n_send + 1
      call mpi_isend(num1(ieast),1,MPI_INT,myeast,tag_w,MPI_COMM_WORLD, &
                     reqs(n_send),ierr)
  
      n_send = n_send + 1
      call mpi_isend(num1(inw),1,MPI_INT,mynw,tag_se,MPI_COMM_WORLD, &
                     reqs(n_send),ierr)
  
      n_send = n_send + 1
      call mpi_isend(num1(ine),1,MPI_INT,myne,tag_sw,MPI_COMM_WORLD, &
                     reqs(n_send),ierr)
  
      n_send = n_send + 1
      call mpi_isend(num1(isw),1,MPI_INT,mysw,tag_ne,MPI_COMM_WORLD, &
                     reqs(n_send),ierr)

      n_send = n_send + 1
      call mpi_isend(num1(ise),1,MPI_INT,myse,tag_nw,MPI_COMM_WORLD, &
                     reqs(n_send),ierr)

      ! make sure that all the non-blocking MPI operations are complete

      call mpi_waitall(16,reqs,status1,ierr)

      ! sanity check: if too many droplets enter the current MPI region
      !               and exceed the number of "holes", stop the program
      !               with an error message

      if ( sum(num2) .gt. size(holes_ind) ) then
          write(*,*) "Too many new droplets will enter the MPI rank: ", myid
          stop "Stop the program ..."
      end if

#if 0 
      ! sanity check: if the total number of leaving droplets matches
      !               the total number of entering droploets across 
      !               all the MPI ranks

      n_send = sum(num1)
      if ( myid == 0 ) then
         call mpi_reduce(MPI_IN_PLACE,n_send,1,MPI_INT, &
                         MPI_SUM,0,MPI_COMM_WORLD,ierr)
      else
         call mpi_reduce(n_send,n_send,1,MPI_INT, &
                         MPI_SUM,0,MPI_COMM_WORLD,ierr)
      end if

      n_recv = sum(num2)
      if ( myid == 0 ) then
         call mpi_reduce(MPI_IN_PLACE,n_recv,1,MPI_INT, &
                         MPI_SUM,0,MPI_COMM_WORLD,ierr)
      else
         call mpi_reduce(n_recv,n_recv,1,MPI_INT, &
                         MPI_SUM,0,MPI_COMM_WORLD,ierr)
      end if

      if ( myid == 0 ) then
         if ( n_send .ne. n_recv ) then
            write(*,*) "Unmatched total leaving droplets vs. total entering droplets..."
            write(*,*) "Total leaving droplets: ", n_send
            write(*,*) "Total entering droplets: ", n_recv
            stop "Stop the program ..."
         end if
      end if
#endif

    end subroutine comm_droplet_number

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

    subroutine comm_droplet_value(holes_ind,num1,num2,pdata,pdata_neighbor)

      use input, only : myid,nparcelsLocal,npvals,mynw,mysw,myne, &
                        myse,myeast,mywest,mynorth,mysouth,ierr, &
                        pract,nparcelsActive
      use constants, only: undefined_index,neg_huge,num_nn,inorth, &
                           isouth,iwest,ieast,inw,ine,isw,ise
      use mpi

      implicit none

      integer, intent(in), dimension(:) :: holes_ind                       ! location index in "pdata" that can 
                                                                           ! add a new droplet;
                                                                           ! not a fixed-size array but we do 
                                                                           ! not need to know its size
      integer, intent(in) :: num1(num_nn)                                  ! Number of droplets that will enter each 
                                                                           ! nearest neighbor at different directions
      integer, intent(in) :: num2(num_nn)                                  ! Number of droplets that will enter the 
                                                                           ! current MPI region from each nearest 
                                                                           ! neighbor at different directions
      real, intent(inout), dimension(nparcelsLocal,npvals) :: pdata        ! droplet information
      integer, intent(in), dimension(nparcelsActive) :: pdata_neighbor     ! array to store the new MPI region 
                                                                           ! info for all the droplets

      ! Local variables

      real, dimension(:,:), allocatable :: droplet_n1, droplet_s1, droplet_w1, &     ! information of droplets that will leave the 
                                           droplet_e1, droplet_nw1, droplet_ne1, &   ! current MPI region and enter the nearest neighbor;
                                           droplet_sw1, droplet_se1, &               ! 1 means sending array;
                                           droplet_n2, droplet_s2, droplet_w2, &     ! 2 means receiving array
                                           droplet_e2, droplet_nw2, droplet_ne2, &
                                           droplet_sw2, droplet_se2
      integer :: n_idx,s_idx,w_idx,e_idx,nw_idx,ne_idx,sw_idx,se_idx
      integer :: reqs(16),tag(num_nn,2)
      integer :: index_n,index_s,index_w,index_e,index_nw,index_ne, &
                 index_sw,index_se,n_recv,n_send
      integer, dimension(mpi_status_size,16) :: status1
      integer :: i,k,n,indx

      ! initialize some MPI related variables

      index_n  = undefined_index
      index_s  = undefined_index
      index_w  = undefined_index
      index_e  = undefined_index
      index_nw = undefined_index
      index_sw = undefined_index
      index_ne = undefined_index
      index_se = undefined_index

      do i = 1, num_nn
         tag(i,1) = undefined_index
         tag(i,2) = undefined_index
      end do 

      ! allocate spaces to store the droplets that will leave the current MPI region

      if ( num1(inorth) .ne. 0 ) allocate(droplet_n1(num1(inorth),npvals))
      if ( num1(isouth) .ne. 0 ) allocate(droplet_s1(num1(isouth),npvals))
      if ( num1(iwest) .ne. 0 ) allocate(droplet_w1(num1(iwest),npvals))
      if ( num1(ieast) .ne. 0 ) allocate(droplet_e1(num1(ieast),npvals))
      if ( num1(inw) .ne. 0 ) allocate(droplet_nw1(num1(inw),npvals))
      if ( num1(ine) .ne. 0 ) allocate(droplet_ne1(num1(ine),npvals))
      if ( num1(isw) .ne. 0 ) allocate(droplet_sw1(num1(isw),npvals))
      if ( num1(ise) .ne. 0 ) allocate(droplet_se1(num1(ise),npvals))

      ! extract the information of droplets that will leave the current MPI region 
      ! and reset the corresponding "pdata" record to a large negative value

      n_idx  = 1
      s_idx  = 1
      w_idx  = 1
      e_idx  = 1
      nw_idx = 1
      ne_idx = 1
      sw_idx = 1
      se_idx = 1

      do n = 1, nparcelsActive
         if ( pdata_neighbor(n) .eq. mynorth ) then
            droplet_n1(n_idx,:) = pdata(n,:)
            n_idx = n_idx + 1
            pdata(n,:) = neg_huge
         else if ( pdata_neighbor(n) .eq. mysouth ) then
            droplet_s1(s_idx,:) = pdata(n,:)
            s_idx = s_idx + 1
            pdata(n,:) = neg_huge
         else if ( pdata_neighbor(n) .eq. mywest ) then
            droplet_w1(w_idx,:) = pdata(n,:)
            w_idx = w_idx + 1
            pdata(n,:) = neg_huge
         else if ( pdata_neighbor(n) .eq. myeast ) then
            droplet_e1(e_idx,:) = pdata(n,:)
            e_idx = e_idx + 1
            pdata(n,:) = neg_huge
         else if ( pdata_neighbor(n) .eq. mynw ) then
            droplet_nw1(nw_idx,:) = pdata(n,:)
            nw_idx = nw_idx + 1
            pdata(n,:) = neg_huge
         else if ( pdata_neighbor(n) .eq. myne ) then
            droplet_ne1(ne_idx,:) = pdata(n,:)
            ne_idx = ne_idx + 1
            pdata(n,:) = neg_huge
         else if ( pdata_neighbor(n) .eq. mysw ) then
            droplet_sw1(sw_idx,:) = pdata(n,:)
            sw_idx = sw_idx + 1
            pdata(n,:) = neg_huge
         else if ( pdata_neighbor(n) .eq. myse ) then
            droplet_se1(se_idx,:) = pdata(n,:)
            se_idx = se_idx + 1
            pdata(n,:) = neg_huge
         else
            cycle   ! this droplet stays at the same MPI region or this is a "hole";
                    ! no action is needed
         end if
      end do

      ! allocate temporary arrays if there are new droplets from the nearest neighbor

      if ( num2(inorth) .ne. 0 ) allocate(droplet_n2(num2(inorth),npvals))
      if ( num2(isouth) .ne. 0 ) allocate(droplet_s2(num2(isouth),npvals))
      if ( num2(iwest) .ne. 0 ) allocate(droplet_w2(num2(iwest),npvals))
      if ( num2(ieast) .ne. 0 ) allocate(droplet_e2(num2(ieast),npvals))
      if ( num2(inw) .ne. 0 ) allocate(droplet_nw2(num2(inw),npvals))
      if ( num2(ine) .ne. 0 ) allocate(droplet_ne2(num2(ine),npvals))
      if ( num2(isw) .ne. 0 ) allocate(droplet_sw2(num2(isw),npvals))
      if ( num2(ise) .ne. 0 ) allocate(droplet_se2(num2(ise),npvals))

      ! initiate the MPI non-blocking receive interface to 
      ! obtain new droplet information from the nearest neighbors

      n_recv   = 0
      if ( num2(inorth) .ne. 0 ) then
         n_recv = n_recv + 1
         tag(inorth,2)  = 1000 + num2(inorth)
         call mpi_irecv(droplet_n2,num2(inorth)*npvals,MPI_REAL, &
                        mynorth,tag(inorth,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
         index_n = n_recv
      end if
      if ( num2(isouth) .ne. 0 ) then
         n_recv = n_recv + 1
         tag(isouth,2)  = 1000 + num2(isouth)
         call mpi_irecv(droplet_s2,num2(isouth)*npvals,MPI_REAL, &
                        mysouth,tag(isouth,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
         index_s = n_recv
      end if
      if ( num2(iwest) .ne. 0 ) then
         n_recv = n_recv + 1
         tag(iwest,2)  = 1000 + num2(iwest)
         call mpi_irecv(droplet_w2,num2(iwest)*npvals,MPI_REAL, &
                        mywest,tag(iwest,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
         index_w = n_recv
      end if
      if ( num2(ieast) .ne. 0 ) then
         n_recv = n_recv + 1
         tag(ieast,2)  = 1000 + num2(ieast)
         call mpi_irecv(droplet_e2,num2(ieast)*npvals,MPI_REAL, &
                        myeast,tag(ieast,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
         index_e = n_recv
      end if
      if ( num2(inw) .ne. 0 ) then
         n_recv = n_recv + 1
         tag(inw,2) = 1000 + num2(inw)
         call mpi_irecv(droplet_nw2,num2(inw)*npvals,MPI_REAL, &
                        mynw,tag(inw,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
         index_nw = n_recv
      end if
      if ( num2(ine) .ne. 0 ) then
         n_recv = n_recv + 1
         tag(ine,2) = 1000 + num2(ine)
         call mpi_irecv(droplet_ne2,num2(ine)*npvals,MPI_REAL, &
                        myne,tag(ine,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
         index_ne = n_recv
      end if
      if ( num2(isw) .ne. 0 ) then
         n_recv = n_recv + 1
         tag(isw,2) = 1000 + num2(isw)
         call mpi_irecv(droplet_sw2,num2(isw)*npvals,MPI_REAL, &
                        mysw,tag(isw,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
         index_sw = n_recv
      end if
      if ( num2(ise) .ne. 0 ) then
         n_recv = n_recv + 1
         tag(ise,2) = 1000 + num2(ise)
         call mpi_irecv(droplet_se2,num2(ise)*npvals,MPI_REAL, &
                        myse,tag(ise,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
         index_se = n_recv
      end if

      ! initiate the MPI non-blocking send interface to send the information of
      ! droplets that leave the current MPI region to the nearest neighbors 

      n_send = 8
      if ( num1(inorth) .ne. 0 ) then
         n_send = n_send + 1
         tag(isouth,1) = 1000 + num1(inorth)
         call mpi_isend(droplet_n1,num1(inorth)*npvals,MPI_REAL, &
                        mynorth,tag(isouth,1),MPI_COMM_WORLD,reqs(n_send),ierr)
      end if
      if ( num1(isouth) .ne. 0 ) then
         n_send = n_send + 1
         tag(inorth,1) = 1000 + num1(isouth)
         call mpi_isend(droplet_s1,num1(isouth)*npvals,MPI_REAL, &
                        mysouth,tag(inorth,1),MPI_COMM_WORLD,reqs(n_send),ierr)
      end if
      if ( num1(iwest) .ne. 0 ) then
         n_send = n_send + 1
         tag(ieast,1) = 1000 + num1(iwest)
         call mpi_isend(droplet_w1,num1(iwest)*npvals,MPI_REAL, &
                        mywest,tag(ieast,1),MPI_COMM_WORLD,reqs(n_send),ierr)
      end if
      if ( num1(ieast) .ne. 0 ) then
         n_send = n_send + 1
         tag(iwest,1) = 1000 + num1(ieast)
         call mpi_isend(droplet_e1,num1(ieast)*npvals,MPI_REAL, &
                        myeast,tag(iwest,1),MPI_COMM_WORLD,reqs(n_send),ierr)
      end if
      if ( num1(inw) .ne. 0 ) then
         n_send = n_send + 1
         tag(ise,1) = 1000 + num1(inw)
         call mpi_isend(droplet_nw1,num1(inw)*npvals,MPI_REAL, &
                        mynw,tag(ise,1),MPI_COMM_WORLD,reqs(n_send),ierr)
      end if
      if ( num1(ine) .ne. 0 ) then
         n_send = n_send + 1
         tag(isw,1) = 1000 + num1(ine)
         call mpi_isend(droplet_ne1,num1(ine)*npvals,MPI_REAL, &
                        myne,tag(isw,1),MPI_COMM_WORLD,reqs(n_send),ierr)
      end if
      if ( num1(isw) .ne. 0 ) then
         n_send = n_send + 1
         tag(ine,1) = 1000 + num1(isw)
         call mpi_isend(droplet_sw1,num1(isw)*npvals,MPI_REAL, &
                        mysw,tag(ine,1),MPI_COMM_WORLD,reqs(n_send),ierr)
      end if
      if ( num1(ise) .ne. 0 ) then
         n_send = n_send + 1
         tag(inw,1) = 1000 + num1(ise)
         call mpi_isend(droplet_se1,num1(ise)*npvals,MPI_REAL, &
                        myse,tag(inw,1),MPI_COMM_WORLD,reqs(n_send),ierr)
      end if

      ! update the "holes" in "pdata" with the new droplets that
      ! enter the current MPI region from the nearest neighbors

      n = 1
      k = 1
      do while( n .le. n_recv )
         call mpi_waitany(n_recv,reqs(1:n_recv),indx,MPI_STATUS_IGNORE,ierr)
         if ( indx .eq. index_n ) then
            do i = 1, num2(inorth) 
               pdata(holes_ind(k),:) = droplet_n2(i,:)
               k = k + 1
            end do
         else if ( indx .eq. index_s ) then
            do i = 1, num2(isouth)
               pdata(holes_ind(k),:) = droplet_s2(i,:)
               k = k + 1
            end do
         else if ( indx .eq. index_w ) then
            do i = 1, num2(iwest)
               pdata(holes_ind(k),:) = droplet_w2(i,:)
               k = k + 1
            end do
         else if ( indx .eq. index_e ) then
            do i = 1, num2(ieast)
               pdata(holes_ind(k),:) = droplet_e2(i,:)
               k = k + 1
            end do
         else if ( indx .eq. index_nw ) then
            do i = 1, num2(inw)
               pdata(holes_ind(k),:) = droplet_nw2(i,:)
               k = k + 1
            end do
         else if ( indx .eq. index_ne ) then
            do i = 1, num2(ine)
               pdata(holes_ind(k),:) = droplet_ne2(i,:)
               k = k + 1
            end do
         else if ( indx .eq. index_sw ) then
            do i = 1, num2(isw)
               pdata(holes_ind(k),:) = droplet_sw2(i,:)
               k = k + 1
            end do
         else if ( indx .eq. index_se ) then
            do i = 1, num2(ise)
               pdata(holes_ind(k),:) = droplet_se2(i,:)
               k = k + 1
            end do
         else
            write(*,*) indx," is not a nearest neighbor for myid = ",myid
            stop "Stop the program..."
         end if
         n = n + 1
      end do

      ! make sure that all the non-blocking MPI send operations are complete

      n_send = n_send - 8
      if ( n_send .ge. 1 ) then
         call mpi_waitall(n_send,reqs(9:9+n_send-1),status1(1:mpi_status_size,9:9+n_send-1),ierr)
      end if

      ! free up the memory for temporary variables
      if (allocated(droplet_n1))  deallocate(droplet_n1)
      if (allocated(droplet_n2))  deallocate(droplet_n2)
      if (allocated(droplet_s1))  deallocate(droplet_s1)
      if (allocated(droplet_s2))  deallocate(droplet_s2)
      if (allocated(droplet_w1))  deallocate(droplet_w1)
      if (allocated(droplet_w2))  deallocate(droplet_w2)
      if (allocated(droplet_e1))  deallocate(droplet_e1)
      if (allocated(droplet_e2))  deallocate(droplet_e2)
      if (allocated(droplet_ne1)) deallocate(droplet_ne1)
      if (allocated(droplet_ne2)) deallocate(droplet_ne2)
      if (allocated(droplet_nw1)) deallocate(droplet_nw1)
      if (allocated(droplet_nw2)) deallocate(droplet_nw2)
      if (allocated(droplet_se1)) deallocate(droplet_se1)
      if (allocated(droplet_se2)) deallocate(droplet_se2)
      if (allocated(droplet_sw1)) deallocate(droplet_sw1)
      if (allocated(droplet_sw2)) deallocate(droplet_sw2)

    end subroutine comm_droplet_value

#endif

  END MODULE comm_module
