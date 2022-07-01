#ifdef _B4B
#define _B4B01R
#define _B4B05R   
#endif
  MODULE simple_phys_module

  implicit none

  private
  public :: testcase_simple_phys,get_avg_uvt,get_avg_uvtq

!-------------------------------------------------------------------------------
!
!   Simple physics parameterizations for certain test cases
!
!     see references below for more details
!
!-------------------------------------------------------------------------------

    !  large-scale divergence:
    real, parameter :: bigd    =  3.75e-6

    !  parameters for simple radiation scheme:
    real, parameter :: qcrit   =  0.008
    real, parameter :: f0      =  70.0
    real, parameter :: f1      =  22.0
    real, parameter :: kappa   =  85.0
    real, parameter :: alphaz  =  1.0
    real, parameter :: rhoi    =  1.12


  CONTAINS

      subroutine testcase_simple_phys(mh,rho0,rr0,rf0,th0,u0,v0,     &
                   zh,zf,dum1,dum2,dum3,dum4,dum5,dum6,              &
                   ufrc,vfrc,thfrc,qvfrc,ug,vg,dvdr,                 &
                   uavg,vavg,thavg,qavg,cavg,                        &
                   ua,va,tha,qa,uten1,vten1,thten1,qten,             &
                   frad,zir,ruh,ruf,rvh,rvf)
      use input, only:ib,ie,jb,je,kb,ke,ibm,iem,jbm,jem,kbm,kem, &
          ibr,ier,jbr,jer,kbr,ker,numq,ni,nj,nk,rdz,hurr_rad,testcase, &
          umove,vmove,imoist,nqv
      use constants
      implicit none

      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: mh,rho0,rr0,rf0,th0
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: u0
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: v0
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2,dum3,dum4,dum5,dum6
      real, intent(inout), dimension(kb:ke) :: ufrc,vfrc
      real, intent(in), dimension(kb:ke) :: thfrc,qvfrc,ug,vg,dvdr
      real, intent(inout), dimension(kb:ke) :: uavg,vavg,thavg
      real, intent(inout), dimension(kb:ke,numq) :: qavg
      double precision, intent(inout), dimension(kb:ke,3+numq) :: cavg
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: tha
      real, intent(in), dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: qa
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: uten1
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: vten1
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: thten1
      real, intent(inout), dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: qten
      real, intent(inout), dimension(ibr:ier,jbr:jer,kbr:ker) :: frad
      real, intent(inout), dimension(ibr:ier,jbr:jer) :: zir
      real, intent(in), dimension(ib:ie) :: ruh
      real, intent(in), dimension(ib:ie+1) :: ruf
      real, intent(in), dimension(jb:je) :: rvh
      real, intent(in), dimension(jb:je+1) :: rvf

      integer :: i,j,k,n
      real :: tem
      !$acc declare present(rho0,zh,zf,qa) &
      !$acc present(ufrc,vfrc,thfrc,qvfrc) &
      !$acc present(dum1,dum2,dum3,dum4,frad,zir) &
      !$acc present(uten1,vten1,thten1,qten)

!-----------------------------------------------------------------------
!     nonprecipitating stratoCu  (Stevens et al, 2005, MWR)
!     drizzling stratoCu  (Ackerman et al, 2009, MWR)

      IF( testcase.eq.4 .or. testcase.eq.5 )THEN

        ! simple radiation (Stevens et al 2005)
        call     simplerad(rho0,zh,zf,dum1,dum2,dum3,dum4,qa,frad,zir)

        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          thten1(i,j,k) = thten1(i,j,k)-(frad(i,j,k+1)-frad(i,j,k))*rdz*mh(i,j,k)  &
                                       /(cp*rhoi)
        enddo
        enddo
        enddo

      ENDIF

!-----------------------------------------------------------------------
!     hurricane boundary layer (Bryan et al, 2017, BLM)

      IF( testcase.eq.6 .or. testcase.eq.10 )THEN

        ! get domain averages:
        call     get_avg_uvt(uavg,vavg,thavg,cavg,th0,ua,va,tha,ruh,ruf,rvh,rvf)

        tem = 1.0/hurr_rad

        !  Mesoscale Tendency terms:
        !  (radial advection and centrifugal accel terms)

        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector default(present) private(i,j,k)
        DO k=1,nk
          ! ghb, 210826: account for moving domain
          ufrc(k) = +(uavg(k)+umove)*(uavg(k)+umove)*tem  &
                    +(vavg(k)+vmove)*(v0(1,1,k)+vmove)*tem
          vfrc(k) = -(uavg(k)+umove)*dvdr(k)  &
                    -(uavg(k)+umove)*(v0(1,1,k)+vmove)*tem
        ENDDO
        !stop 'testcase_simple_phys: testcase={6 or 10}'
      ENDIF

!-----------------------------------------------------------------------
      !  Add tendencies:

    IF( testcase.ge.1 )THEN

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
      DO k=1,nk
        do j=1,nj
        do i=1,ni+1
          uten1(i,j,k) = uten1(i,j,k) + ufrc(k)
        enddo
        enddo
      ENDDO
      !
      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
      DO k=1,nk
        do j=1,nj+1
        do i=1,ni
          vten1(i,j,k) = vten1(i,j,k) + vfrc(k)
        enddo
        enddo
      ENDDO
      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
      DO k=1,nk
        do j=1,nj
        do i=1,ni
          thten1(i,j,k) = thten1(i,j,k) + thfrc(k)
        enddo
        enddo
      ENDDO
      IF( imoist.eq.1 )THEN
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        DO k=1,nk
          do j=1,nj
          do i=1,ni
            qten(i,j,k,nqv) = qten(i,j,k,nqv) + qvfrc(k)
          enddo
          enddo
        ENDDO
      ENDIF

    ENDIF

!-----------------------------------------------------------------------

      end subroutine testcase_simple_phys


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine get_avg_uvt(uavg,vavg,thavg,cavg,th0,ua,va,tha,ruh,ruf,rvh,rvf)
      use input, only: ib,ie,jb,je,kb,ke,numq,ni,nj,nk,dx,dy,ierr,maxx,minx,maxy,miny
#ifdef MPI
      use mpi
#endif
      implicit none

      real, intent(inout), dimension(kb:ke) :: uavg,vavg,thavg
      double precision, intent(inout), dimension(kb:ke,3+numq) :: cavg
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: th0
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: tha
      real, intent(in), dimension(ib:ie) :: ruh
      real, intent(in), dimension(ib:ie+1) :: ruf
      real, intent(in), dimension(jb:je) :: rvh
      real, intent(in), dimension(jb:je+1) :: rvf

      integer :: i,j,k,n
      real :: tem
      double precision :: temd
      double precision :: tmp1,tmp2,tmp3
      ! 180612:  area-weighted average

      tem = dx*dy

      !$acc parallel default(present) reduction(+:tmp1,tmp2,tmp3)
      ! Get domain-averages:
      !$acc loop gang
      do k=1,nk
         tmp1=0.0
         tmp2=0.0
         tmp3=0.0
        !$acc loop vector collapse(2) reduction(+:tmp1,tmp2,tmp3)
        do j=1,nj
        do i=1,ni
          tmp1 = tmp1 + ua(i,j,k)*tem*ruf(i)*rvh(j)
          tmp2 = tmp2 + va(i,j,k)*tem*ruh(i)*rvf(j)
          tmp3 = tmp3 + (th0(i,j,k)+tha(i,j,k))*tem*ruh(i)*rvh(j)
        enddo
        enddo
        cavg(k,1) = tmp1
        cavg(k,2) = tmp2
        cavg(k,3) = tmp3
      enddo
      !$acc end parallel

#ifdef MPI
      !$acc update host(cavg)
      call MPI_ALLREDUCE(MPI_IN_PLACE,cavg(kb,1),(ke-kb+1)*3       ,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      !$acc update device(cavg)
#endif

      temd = 1.0d0/( dble(maxx-minx)*dble(maxy-miny) )

      !$acc parallel default(present)
      !$acc loop gang vector
      do k=1,nk
        uavg(k)  = cavg(k,1)*temd
        vavg(k)  = cavg(k,2)*temd
        thavg(k) = cavg(k,3)*temd
      enddo
      !$acc end parallel 

      end subroutine get_avg_uvt


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine get_avg_uvtq(uavg,vavg,thavg,qavg,cavg,th0,ua,va,tha,qa,ruh,ruf,rvh,rvf)
      use input, only: ib,ie,jb,je,kb,ke,numq,ibm,iem,jbm,jem,kbm,kem,ni,nj,nk, &
          ierr,minx,maxx,miny,maxy,dx,dy
#ifdef MPI
      use mpi
#endif
      implicit none

      real, intent(inout), dimension(kb:ke) :: uavg,vavg,thavg
      real, intent(inout), dimension(kb:ke,numq) :: qavg
      double precision, intent(inout), dimension(kb:ke,3+numq) :: cavg
      real, intent(in), dimension(ib:ie,jb:je,kb:ke)   :: th0
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(in), dimension(ib:ie,jb:je,kb:ke)   :: tha
      real, intent(in), dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: qa
      real, intent(in), dimension(ib:ie) :: ruh
      real, intent(in), dimension(ib:ie+1) :: ruf
      real, intent(in), dimension(jb:je) :: rvh
      real, intent(in), dimension(jb:je+1) :: rvf

      integer :: i,j,k,n
      real :: tem
      double precision :: temd
      double precision :: cavg1,cavg2,cavg3,cavgt

      ! 180612:  area-weighted average

      !$acc parallel default(present) reduction(+:cavg1,cavg2,cavg3)
      !$acc loop gang reduction(+:cavg1,cavg2,cavg3)
      do k=1,nk
         cavg1 = 0.0
         cavg2 = 0.0
         cavg3 = 0.0
        !$acc loop vector collapse(2) reduction(+:cavg1,cavg2,cavg3)
        do j=1,nj
        do i=1,ni
          cavg1 = cavg1 + ua(i,j,k)*dx*dy*ruf(i)*rvh(j)
          cavg2 = cavg2 + va(i,j,k)*dx*dy*ruh(i)*rvf(j)
          cavg3 = cavg3 + (th0(i,j,k)+tha(i,j,k))*dx*dy*ruh(i)*rvh(j)
        enddo
        enddo
        cavg(k,1) = cavg1
        cavg(k,2) = cavg2
        cavg(k,3) = cavg3
      enddo
      !$acc end parallel

      !$acc parallel default(present) reduction(+:cavgt)
      !$acc loop gang collapse(2) reduction(+:cavgt)
      do k=1,nk
        do n=1,numq
          cavgt = 0.0
          !$acc loop vector collapse(2) reduction(+:cavgt)
          do j=1,nj
          do i=1,ni
            cavgt = cavgt + qa(i,j,k,n)*dx*dy*ruh(i)*rvh(j)
          enddo
          enddo
          cavg(k,3+n) = cavgt
        enddo
      enddo
      !$acc end parallel

#ifdef MPI
      !acc update host(cavg)
      call MPI_ALLREDUCE(MPI_IN_PLACE,cavg(kb,1),(ke-kb+1)*(3+numq),MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      !$acc update device(cavg) 
#endif

      temd = 1.0d0/( dble(maxx-minx)*dble(maxy-miny) )

      !$acc parallel default(present)
      !$acc loop gang vector
      do k=1,nk
        uavg(k)  = cavg(k,1)*temd
        vavg(k)  = cavg(k,2)*temd
        thavg(k) = cavg(k,3)*temd
        do n=1,numq
          qavg(k,n) = cavg(k,3+n)*temd
        enddo
      enddo
      !$acc end parallel

      end subroutine get_avg_uvtq

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine simplerad(rho0,zh,zf,dum1,dum2,qt  ,ql  ,qa,frad,zir)
      use input, only : ib,ie,jb,je,kb,ke,ibm,iem,jbm,jem,kbm,kem,numq, &
          ibr,ier,jbr,jer,kbr,ker,ni,nj,nk,nql1,nql2,nqv
      use constants
      implicit none

      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rho0,zh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2,qt,ql
      real, intent(in), dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: qa
      real, intent(inout), dimension(ibr:ier,jbr:jer,kbr:ker) :: frad
      real, intent(inout), dimension(ibr:ier,jbr:jer) :: zir

      integer :: i,j,k,n
      real :: fr1,fr2,fr3
      integer, dimension(ni,nj) :: kzi
      !$acc declare create(kzi) &
      !$acc present(rho0,zh,zf,qa) &
      !$acc present(dum1,dum2,ql,qt,zir,frad)

      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum2(i,j,1) = 0.0
        dum1(i,j,nk+1) = 0.0
        kzi(i,j) = 0  ! initilize this to zero 
      enddo
      enddo
      !$acc end parallel

      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,n)
      do k=1,nk
        ! qt = total water mixing ratio
        ! ql = liquid water mixing ratio
        !----
        do j=1,nj
        do i=1,ni
          ql(i,j,k) = 0.0
        enddo
        enddo
        !----
      enddo
      !$acc end parallel

      do n=nql1,nql2
      !$acc parallel default(present)
      !$acc loop gang vector collapse(3)
      do k=1,nk
        do j=1,nj
        do i=1,ni
          ql(i,j,k) = ql(i,j,k)+qa(i,j,k,n)
        enddo
        enddo
      enddo
      !$acc end parallel
      enddo

      !----

      do k=1,nk
        !$acc parallel default(present)
        !$acc loop gang vector collapse(2)
        do j=1,nj
        do i=1,ni
          qt(i,j,k) = qa(i,j,k,nqv)+ql(i,j,k)
          if( qt(i,j,k) .gt. qcrit ) kzi(i,j) = k
          dum2(i,j,k+1) = dum2(i,j,k) + kappa*rho0(i,j,k)*ql(i,j,k)*(zf(i,j,k+1)-zf(i,j,k))
        enddo
        enddo
        !$acc end parallel
      enddo

      !----

      ! interpolate:
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j,k)
      do j=1,nj
      do i=1,ni
        k = kzi(i,j)
        zir(i,j) = zh(i,j,k) +(zh(i,j,k+1)-zh(i,j,k))  &
                             *(      qcrit-qt(i,j,k))  &
                             /(qt(i,j,k+1)-qt(i,j,k))
      enddo
      enddo

!!!      if( myid.eq.0 )then
!!!        i = 1
!!!        j = 1
!!!        k = kzi(i,j)
!!!        print *
!!!        print *,zh(i,j,k),zir(i,j),zh(i,j,k+1)
!!!        print *,qt(i,j,k),qcrit,qt(i,j,k+1)
!!!        print *
!!!      endif

      do k=nk+1,1,-1
      !$acc parallel default(present)
      !$acc loop gang vector collapse(2)
      do j=1,nj
      do i=1,ni
        dum1(i,j,k) = dum1(i,j,min(nk+1,k+1))  &
                + kappa*rho0(i,j,k)*ql(i,j,k)*(zf(i,j,k+1)-zf(i,j,k))
        fr1 = f0*exp(-dum1(i,j,k))
        fr2 = f1*exp(-dum2(i,j,k))
        if( zf(i,j,k).lt.zir(i,j) )then
          fr3 = 0.0
        else
          fr3 = rhoi*cp*bigd*alphaz*( 0.25*((zf(i,j,k)-zir(i,j))**1.333333)  &
                                 +zir(i,j)*((zf(i,j,k)-zir(i,j))**0.333333)  )
        endif
        frad(i,j,k) = fr1+fr2+fr3
      enddo
      enddo
      !$acc end parallel
      enddo

      stop 'simplerad: at the end of the subroutine'
      end subroutine simplerad


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


  END MODULE simple_phys_module
