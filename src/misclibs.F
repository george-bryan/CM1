#ifdef _B4B


!JMD-expensive
#define _B4B04R
#define _B4B05R
#define _B4B06R
#define _B4B07R
#define _B4B08R
#define _B4B09R
#define _B4B10R
#endif
  MODULE misclibs

  implicit none

  CONTAINS

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getdiv(arh1,arh2,uh,vh,mh,u,v,w,dum1,dum2,dum3,div,  &
                        rds,rdsf,sigma,sigmaf,gz,rgzu,rgzv,dzdx,dzdy)
      use input, only: ib,ie,jb,je,kb,ke,itb,ite,jtb,jte,ni,nj,nk, &
          axisymm,terrain_flag,rdx,rdy,rdz,zt,rzt,smeps,timestats,mytime, &
          time_divx
      implicit none

      real, intent(in), dimension(ib:ie) :: arh1,arh2,uh
      real, intent(in), dimension(jb:je) :: vh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: mh
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: u
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: v
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: w
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2,dum3,div
      real, intent(in), dimension(kb:ke) :: rds,sigma
      real, intent(in), dimension(kb:ke+1) :: rdsf,sigmaf
      real, intent(in), dimension(itb:ite,jtb:jte) :: gz,rgzu,rgzv,dzdx,dzdy

      integer :: i,j,k
      real :: r1,r2
      !$acc declare present(arh1,arh2,uh,vh,mh,u,v,w) &
      !$acc present(dum1,dum2,dum3,div) &
      !$acc present(rds,sigma,rdsf,sigmaf) &
      !$acc present(gz,rgzu,rgzv,dzdx,dzdy)


      IF(.not.terrain_flag)THEN
        IF(axisymm.eq.0)THEN
          ! Cartesian without terrain:
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            div(i,j,k)=( (u(i+1,j,k)-u(i,j,k))*rdx*uh(i)        &
                        +(v(i,j+1,k)-v(i,j,k))*rdy*vh(j) )      &
                        +(w(i,j,k+1)-w(i,j,k))*rdz*mh(1,1,k)
            if(abs(div(i,j,k)).lt.smeps) div(i,j,k)=0.0
          enddo
          enddo
          enddo
        ELSE
          ! axisymmetric:
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            div(i,j,k)=(arh2(i)*u(i+1,j,k)-arh1(i)*u(i,j,k))*rdx*uh(i)   &
                      +(w(i,j,k+1)-w(i,j,k))*rdz*mh(1,1,k)
            if(abs(div(i,j,k)).lt.smeps) div(i,j,k)=0.0
          enddo
          enddo
          enddo
        ENDIF
      ELSE
          ! Cartesian with terrain:
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector private(i,j,k)
          DO k=1,nk
            do j=1,nj
            do i=1,ni+1
              dum1(i,j,k)=u(i,j,k)*rgzu(i,j)
            enddo
            enddo
            do j=1,nj+1
            do i=1,ni
              dum2(i,j,k)=v(i,j,k)*rgzv(i,j)
            enddo
            enddo
          ENDDO
          !$omp parallel do default(shared) private(i,j,k,r1,r2)
          !$acc parallel loop gang vector private(i,j,k,r1,r2)
          DO k=1,nk
            IF(k.eq.1)THEN
              do j=1,nj
              do i=1,ni
                dum3(i,j,1)=0.0
                dum3(i,j,nk+1)=0.0
              enddo
              enddo
            ELSE
              r2 = (sigmaf(k)-sigma(k-1))*rds(k)
              r1 = 1.0-r2
              r1 = 0.5*r1
              r2 = 0.5*r2
              do j=1,nj
              do i=1,ni
                dum3(i,j,k)=w(i,j,k)                                             &
                           +( ( r2*(dum1(i,j,k  )+dum1(i+1,j,k  ))               &
                               +r1*(dum1(i,j,k-1)+dum1(i+1,j,k-1)) )*dzdx(i,j)   &
                             +( r2*(dum2(i,j,k  )+dum2(i,j+1,k  ))               &
                               +r1*(dum2(i,j,k-1)+dum2(i,j+1,k-1)) )*dzdy(i,j)   &
                               )*(sigmaf(k)-zt)*gz(i,j)*rzt
              enddo
              enddo
            ENDIF
          ENDDO
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            div(i,j,k)=( (dum1(i+1,j,k)-dum1(i,j,k))*rdx*uh(i)        &
                        +(dum2(i,j+1,k)-dum2(i,j,k))*rdy*vh(j) )      &
                        +(dum3(i,j,k+1)-dum3(i,j,k))*rdsf(k)
            if(abs(div(i,j,k)).lt.smeps) div(i,j,k)=0.0
          enddo
          enddo
          enddo
      ENDIF
      if(timestats.ge.1) time_divx=time_divx+mytime()

      end subroutine getdiv


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getdivx(arh1,arh2,uh,vh,mh,rho0,rf0,rru,rrv,rrw,divx,  &
                         rds,rdsf,sigma,sigmaf,gz,rgzu,rgzv,dzdx,dzdy)
      use input, only: ib,ie,jb,je,kb,ke,itb,ite,jtb,jte,ni,nj,nk, &
          axisymm,terrain_flag,rdx,rdy,rdz,zt,rzt,smeps,timestats,mytime, &
          time_advs,time_divx

      implicit none

      real, intent(in), dimension(ib:ie) :: arh1,arh2,uh
      real, intent(in), dimension(jb:je) :: vh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: mh,rho0,rf0
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: rru
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: rrv
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: rrw
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: divx
      real, intent(in), dimension(kb:ke) :: rds,sigma
      real, intent(in), dimension(kb:ke+1) :: rdsf,sigmaf
      real, intent(in), dimension(itb:ite,jtb:jte) :: gz,rgzu,rgzv,dzdx,dzdy

      integer :: i,j,k
      real :: r1,r2,tem
      !$acc declare present(arh1,arh2,uh,vh,mh,rho0,rf0) &
      !$acc present(rru,rrv,rrw,divx) &
      !$acc present(rds,sigma,rdsf,sigmaf) &
      !$acc present(gz,rgzu,rgzv,dzdx,dzdy)

    IF(.not.terrain_flag)THEN
      ! without terrain:

      !$acc parallel loop gang vector collapse(2) private(i,j)
      do j=1,nj
      do i=1,ni
        rrw(i,j,   1) = 0.0
        rrw(i,j,nk+1) = 0.0
      enddo
      enddo
      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) private(i,j,k)
      DO k=1,nk
        do j=1,nj
        do i=1,ni+1
          rru(i,j,k)=rru(i,j,k)*rho0(1,1,k)
        enddo
        enddo
      ENDDO
      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) private(i,j,k)
      DO k=1,nk
        do j=1,nj+1
        do i=1,ni
          rrv(i,j,k)=rrv(i,j,k)*rho0(1,1,k)
        enddo
        enddo
      ENDDO
      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) private(i,j,k)
      DO k=1,nk
        do j=1,nj
        do i=1,ni
          rrw(i,j,k)=rrw(i,j,k)*rf0(1,1,k)
        enddo
        enddo
      ENDDO

    ELSE
      ! with terrain:

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector private(i,j,k)
      DO k=1,nk
        do j=1,nj
        do i=1,ni+1
          rru(i,j,k)=0.5*(rho0(i-1,j,k)+rho0(i,j,k))*rru(i,j,k)*rgzu(i,j)
        enddo
        enddo
        do j=1,nj+1
        do i=1,ni
          rrv(i,j,k)=0.5*(rho0(i,j-1,k)+rho0(i,j,k))*rrv(i,j,k)*rgzv(i,j)
        enddo
        enddo
      ENDDO

      !$omp parallel do default(shared) private(i,j,k,r1,r2)
      !$acc parallel loop gang vector private(i,j,k,r1,r2)
      DO k=1,nk
        IF(k.eq.1)THEN
          do j=1,nj
          do i=1,ni
            rrw(i,j,   1) = 0.0
            rrw(i,j,nk+1) = 0.0
          enddo
          enddo
        ELSE
          r2 = (sigmaf(k)-sigma(k-1))*rds(k)
          r1 = 1.0-r2
          r1 = 0.5*r1
          r2 = 0.5*r2
          do j=1,nj
          do i=1,ni
            rrw(i,j,k)=rf0(i,j,k)*rrw(i,j,k)                              &
                      +( ( r2*(rru(i,j,k  )+rru(i+1,j,k  ))               &
                          +r1*(rru(i,j,k-1)+rru(i+1,j,k-1)) )*dzdx(i,j)   &
                        +( r2*(rrv(i,j,k  )+rrv(i,j+1,k  ))               &
                          +r1*(rrv(i,j,k-1)+rrv(i,j+1,k-1)) )*dzdy(i,j)   &
                       )*(sigmaf(k)-zt)*gz(i,j)*rzt
          enddo
          enddo
        ENDIF
      ENDDO

    ENDIF
    if(timestats.ge.1) time_advs=time_advs+mytime()

      IF(.not.terrain_flag)THEN
        IF(axisymm.eq.0)THEN
          ! Cartesian without terrain:
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            divx(i,j,k)=( (rru(i+1,j,k)-rru(i,j,k))*rdx*uh(i)        &
                         +(rrv(i,j+1,k)-rrv(i,j,k))*rdy*vh(j) )      &
                         +(rrw(i,j,k+1)-rrw(i,j,k))*rdz*mh(1,1,k)
            if(abs(divx(i,j,k)).lt.smeps) divx(i,j,k)=0.0
          enddo
          enddo
          enddo
        ELSE
          ! axisymmetric:
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            divx(i,j,k)=(arh2(i)*rru(i+1,j,k)-arh1(i)*rru(i,j,k))*rdx*uh(i)   &
                       +(rrw(i,j,k+1)-rrw(i,j,k))*rdz*mh(1,1,k)
            if(abs(divx(i,j,k)).lt.smeps) divx(i,j,k)=0.0
          enddo
          enddo
          enddo
        ENDIF
      ELSE
          ! Cartesian with terrain:
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            divx(i,j,k)=( (rru(i+1,j,k)-rru(i,j,k))*rdx*uh(i)        &
                         +(rrv(i,j+1,k)-rrv(i,j,k))*rdy*vh(j) )      &
                         +(rrw(i,j,k+1)-rrw(i,j,k))*rdsf(k)
            if(abs(divx(i,j,k)).lt.smeps) divx(i,j,k)=0.0
          enddo
          enddo
          enddo
      ENDIF
      if(timestats.ge.1) time_divx=time_divx+mytime()

      end subroutine getdivx


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine randpert(xfref,yfref,xh,yh,zh,zf,tha)

      ! Generate random perturbations, add to tha (by default):

      use input, only : ib,ie,jb,je,kb,ke,nx,ny,ngxy,ni,nj,nk
      implicit none

      real, intent(in), dimension(1-ngxy:nx+ngxy+1) :: xfref
      real, intent(in), dimension(1-ngxy:ny+ngxy+1) :: yfref
      real, intent(in), dimension(ib:ie) :: xh
      real, intent(in), dimension(jb:je) :: yh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: tha

      integer :: i,j,k
      real :: rand,amplitude


      ! reinitialize pseudorandom number generator:
      call reinit_random_seed


      ! this is the amplitude of the theta perturbations
      ! (plus or minus this value in K)
      amplitude = 0.25


      ! random numbers added here
      ! (can be modified to only place perturbations in certain
      !  locations, but this default code simply puts them
      !  everywhere)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        call random_number(rand)
        tha(i,j,k) = tha(i,j,k)+amplitude*(2.0*rand-1.0)
      enddo
      enddo
      enddo

      end subroutine randpert


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine reinit_random_seed
      use input, only : myid
      implicit none

      integer :: k,n
      integer,dimension(8) :: values
      integer, dimension(:), allocatable :: sand
      real :: rand

      call date_and_time(values=values)
      call random_seed(size=k)
      k = max(3,k)
      allocate( sand(k) )
      sand(1) = myid
      sand(2) = values(7)
      sand(3) = values(8)
      call random_seed(put=sand(1:k))
      call random_number(rand)
      !print *,'  myid,rand = ',myid,rand
      deallocate( sand )

      end subroutine reinit_random_seed


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine convinitu(myid,ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe,   &
                           zdeep,lamx,lamy,xcent,ycent,aconv,    &
                           xf,yh,zh,u0,u3d)
      implicit none

      integer, intent(in) :: myid,ib,ie,jb,je,kb,ke,ni,nj,nk,ibw,ibe
      real, intent(in) :: zdeep,lamx,lamy,xcent,ycent,aconv
      real, intent(in), dimension(ib:ie+1) :: xf
      real, intent(in), dimension(jb:je) :: yh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in),    dimension(ib:ie+1,jb:je,kb:ke) :: u0
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: u3d

      integer :: i,j,k
      real :: term1,term2,term3,term4,umo
      !$acc declare present(xf,yh,zh,u0,u3d)

!!!      if(myid.eq.0) print *,'    convinitu '
      !$omp parallel do default(shared) private(i,j,k,term1,term2,term3,term4,umo)
      !$acc parallel loop gang vector private(i,j,k,term1,term2,term3,term4,umo)
      do k=1,nk
      do j=1,nj
      do i=1,ni+1
        term4 = (zdeep-0.5*(zh(i-1,j,k)+zh(i,j,k)))/zdeep
        if (term4 .gt. 0.0) then
          term1 = -(2.0*Aconv*(xf(i)-xcent))/(lamx**2)
          term2 = -((xf(i)-xcent)/lamx)**2
          term3 = -((yh(j)-ycent)/lamy)**2
          umo = term1*(exp(term2)*exp(term3))*term4
          if( abs(umo).gt.0.01 ) u3d(i,j,k) = u0(i,j,k)+umo
        endif
      enddo
      enddo
      enddo

      end subroutine convinitu


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine convinitv(myid,ib,ie,jb,je,kb,ke,ni,nj,nk,ibs,ibn,   &
                           zdeep,lamx,lamy,xcent,ycent,aconv,    &
                           xh,yf,zh,v0,v3d)
      implicit none

      integer, intent(in) :: myid,ib,ie,jb,je,kb,ke,ni,nj,nk,ibs,ibn
      real, intent(in) :: zdeep,lamx,lamy,xcent,ycent,aconv
      real, intent(in), dimension(ib:ie) :: xh
      real, intent(in), dimension(jb:je+1) :: yf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in),    dimension(ib:ie,jb:je+1,kb:ke) :: v0
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: v3d

      integer :: i,j,k
      real :: term1,term2,term3,term4,vmo
      !$acc declare present(xh,yf,zh,v0,v3d)

!!!      if(myid.eq.0) print *,'    convinitv '
      !$omp parallel do default(shared) private(i,j,k,term1,term2,term3,term4,vmo)
      !$acc parallel loop gang vector private(i,j,k,term1,term2,term3,term4,vmo)
      do k=1,nk
      do j=1,nj+1
      do i=1,ni
        term4 = (zdeep-0.5*(zh(i,j-1,k)+zh(i,j,k)))/zdeep
        if (term4 .gt. 0.0) then
          term1 = -(2.0*Aconv*(yf(j)-ycent))/(lamy**2)
          term2 = -((xh(i)-xcent)/lamx)**2
          term3 = -((yf(j)-ycent)/lamy)**2
          vmo = term1*(exp(term2)*exp(term3))*term4
          if( abs(vmo).gt.0.01 ) v3d(i,j,k) = v0(i,j,k)+vmo
        endif
      enddo
      enddo
      enddo

      end subroutine convinitv


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine get_wnudge(mtime,dtin,xh,yh,zf,wa,wndgten)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,mytime, &
          t1_wnudge,t2_wnudge,alpha_wnudge,xc_wnudge,yc_wnudge, &
          zc_wnudge,rxrwnudge,ryrwnudge,rzrwnudge,wmax_wnudge
      use constants , only : pi
      implicit none

      double precision, intent(in) :: mtime
      real, intent(in) :: dtin
      real, intent(in), dimension(ib:ie) :: xh
      real, intent(in), dimension(jb:je) :: yh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: wndgten

      integer :: i,j,k
      real :: beta,wmag,gamm,tem
      !$acc declare present(xh,yh,zf,wa,wndgten)

      !  updraft nudging scheme (Naylor and Gilmore, 2012, MWR, pgs 3699-3705)

      gamm = 1.0

      if(mtime.ge.t1_wnudge)THEN
        gamm = 1.0+(0.0-1.0)*(mtime-t1_wnudge)/(t2_wnudge-t1_wnudge)
      endif

!!!      if(myid.eq.0) print *,'    get_wnudge: mtime,gamm = ',mtime,gamm

      tem = alpha_wnudge * gamm

      !$omp parallel do default(shared) private(i,j,k,beta,wmag)
      !$acc parallel loop gang vector private(i,j,k,beta,wmag)
      do k=2,nk
      do j=1,nj
      do i=1,ni
        beta = sqrt( ((xh(i)-xc_wnudge)*rxrwnudge)**2       &
                    +((yh(j)-yc_wnudge)*ryrwnudge)**2       &
                    +((zf(i,j,k)-zc_wnudge)*rzrwnudge)**2)
        if(beta.lt.1.0)then
          wmag = wmax_wnudge*( cos(0.5*pi*beta)**2 )
          wndgten(i,j,k) = tem*max(wmag-wa(i,j,k),0.0)
        else
          wndgten(i,j,k) = 0.0
        endif
      enddo
      enddo
      enddo

      end subroutine get_wnudge


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getnewdt(ndt,dt,dtlast,adt,acfl,dbldt,                                 &
                          mtime,stattim,taptim,rsttim,prcltim,diagtim,azimavgtim,       &
                          dorestart,dowriteout,dostat,doprclout,dotdwrite,doazimwrite,  &
                          hifrqtim,dohifrqwrite,doinit)
      use input, only : nx,ny,cm1setup,myid,cflmax,ksmax,dtl,tapfrq,rstfrq,statfrq, &
          diagfrq,azimavgfrq,hifrqfrq,ierr,imoist,ptype,timestats,mytime,time_misc01, &
          time_microphy,dodomaindiag,doazimavg,dohifrq
      use goddard_module, only : consat2
      use lfoice_module, only : lfoice_init
#ifdef MPI
      use mpi
#endif
      implicit none

      integer, intent(inout) :: ndt
      real, intent(inout) :: dt,dtlast
      double precision, intent(inout) :: adt,acfl,dbldt
      double precision, intent(in) :: mtime,stattim,taptim,rsttim,prcltim,diagtim,azimavgtim
      logical, intent(in) :: dorestart,dowriteout,dostat,doprclout,dotdwrite,doazimwrite
      double precision, intent(in) :: hifrqtim
      logical, intent(in) :: dohifrqwrite
      logical, intent(in) :: doinit

      real :: tem,ks_limit
      double precision :: tout

      real, parameter  ::  cfl_limit   =  1.00    ! maximum CFL allowed  (actually a "target" value)
      real, parameter  ::  max_change  =  0.10    ! maximum (percentage) change in timestep

      if( cm1setup.ge.1 )then
        ! assume vertical component is handled implicitly
        if(nx.gt.3.and.ny.gt.3)then
          ! 3d:
          ks_limit = 0.18/2.0
        else
          ! 2d (including axisymm):
          ks_limit = 0.18
        endif
      endif

      dtlast = dt

      myid0:  &
      IF( myid.eq.0 )THEN
        ! only processor 0 does this:

        cflmax = max(cflmax,1.0e-10)
        !print *, 'getnewdt: cflmax:= ',cflmax
        !print *, 'getnewdt: ksmax:= ',ksmax
        IF( cflmax.gt.cfl_limit )THEN
          ! decrease timestep:
          dbldt = dbldt*(cfl_limit/cflmax)
        ELSE
          ! increase timestep:
          dbldt = dbldt*min( 1.0+max_change , cfl_limit/cflmax )
        ENDIF

        ! 180129:
        IF( cm1setup.ge.1 .and. (ksmax*dbldt/dt).gt.ks_limit )THEN
          ! decrease timestep:
          dbldt = min( dbldt , dt*ks_limit/max(1.0e-10,ksmax) )
        ENDIF

        ! don't allow dt to exceed twice initial timestep
        dbldt = min( dbldt , dble(2.0*dtl) )


        IF( .not. doinit )THEN

            IF( taptim.gt.0.0 )THEN
              ! ramp-down timestep when approaching output time
              if( dowriteout )then
                tout = ( ( taptim - mtime ) + tapfrq )
              else
                tout = ( taptim - mtime )
              endif
              if( tout.gt.(2.0*dbldt) .and. tout.le.(3.0*dbldt)  )then
                dbldt = tout/3.0
              elseif( tout.gt.dbldt .and. tout.le.(2.0*dbldt)  )then
                dbldt = tout/2.0
              elseif( tout.le.dbldt )then
                dbldt = tout
              endif
            ENDIF

            IF( rsttim.gt.0.0 )THEN
              ! ramp-down timestep when approaching restart time
              if( dorestart )then
                tout = ( ( rsttim - mtime ) + rstfrq )
              else
                tout = ( rsttim - mtime )
              endif
              if( tout.gt.(2.0*dbldt) .and. tout.le.(3.0*dbldt)  )then
                dbldt = tout/3.0
              elseif( tout.gt.dbldt .and. tout.le.(2.0*dbldt)  )then
                dbldt = tout/2.0
              elseif( tout.le.dbldt )then
                dbldt = tout
              endif
            ENDIF

            IF( stattim.gt.0.0 )THEN
              ! ramp-down timestep when approaching stat time
              if( dostat )then
                tout = ( ( stattim - mtime ) + statfrq )
              else
                tout = ( stattim - mtime )
              endif
              if( tout.gt.(2.0*dbldt) .and. tout.le.(3.0*dbldt)  )then
                dbldt = tout/3.0
              elseif( tout.gt.dbldt .and. tout.le.(2.0*dbldt)  )then
                dbldt = tout/2.0
              elseif( tout.le.dbldt )then
                dbldt = tout
              endif
            ENDIF

            IF( dodomaindiag .and. diagtim.gt.0.0 )THEN
              ! ramp-down timestep when approaching domaindiag time
              if( dotdwrite )then
                tout = ( ( diagtim - mtime ) + diagfrq )
              else
                tout = ( diagtim - mtime )
              endif
              if( tout.gt.(2.0*dbldt) .and. tout.le.(3.0*dbldt)  )then
                dbldt = tout/3.0
              elseif( tout.gt.dbldt .and. tout.le.(2.0*dbldt)  )then
                dbldt = tout/2.0
              elseif( tout.le.dbldt )then
                dbldt = tout
              endif
            ENDIF

            IF( doazimavg .and. azimavgtim.gt.0.0 )THEN
              ! ramp-down timestep when approaching azimavg time
              if( doazimwrite )then
                tout = ( ( azimavgtim - mtime ) + azimavgfrq )
              else
                tout = ( azimavgtim - mtime )
              endif
              if( tout.gt.(2.0*dbldt) .and. tout.le.(3.0*dbldt)  )then
                dbldt = tout/3.0
              elseif( tout.gt.dbldt .and. tout.le.(2.0*dbldt)  )then
                dbldt = tout/2.0
              elseif( tout.le.dbldt )then
                dbldt = tout
              endif
            ENDIF

            IF( dohifrq .and. hifrqtim.gt.0.0 )THEN
              ! ramp-down timestep when approaching hifrq time
              if( dohifrqwrite )then
                tout = ( ( hifrqtim - mtime ) + hifrqfrq )
              else
                tout = ( hifrqtim - mtime )
              endif
              if( tout.gt.(2.0*dbldt) .and. tout.le.(3.0*dbldt)  )then
                dbldt = tout/3.0
              elseif( tout.gt.dbldt .and. tout.le.(2.0*dbldt)  )then
                dbldt = tout/2.0
              elseif( tout.le.dbldt )then
                dbldt = tout
              endif
            ENDIF

        ENDIF

        ! end of processor 0 stuff
      ENDIF  myid0


      ! all processors:
#ifdef MPI
      call MPI_BCAST(dbldt ,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
#endif

      dt = dbldt
      call dtsmall(dt,dbldt)

      IF( ( dt.ne.dtlast ) .or. doinit )THEN
        IF( (imoist.eq.1).and.(ptype.eq.2) )then
          if(timestats.ge.1) time_misc01=time_misc01+mytime()
          call consat2(dt)
          if(timestats.ge.1) time_microphy=time_microphy+mytime()
        ENDIF
        IF( (imoist.eq.1).and.(ptype.eq.4) )then
          if(timestats.ge.1) time_misc01=time_misc01+mytime()
          call lfoice_init(dt)
          if(timestats.ge.1) time_microphy=time_microphy+mytime()
        ENDIF
      ENDIF

      tem = dt/dtlast
      cflmax = cflmax*tem
      ksmax = ksmax*tem

      ndt = ndt + 1
      adt = adt + dbldt
      acfl = acfl + cflmax

      if(timestats.ge.1) time_misc01=time_misc01+mytime()

      end subroutine getnewdt


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine dtsmall(dt,dbldt)
      use input, only : psolver,nsound,nx,ny,min_dx,min_dy,min_dz,csound, &
          adapt_dt,myid,ierr
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

      ! cm1r18:  moved this section of code from solve.F to misclibs.F

      real, intent(inout) :: dt
      double precision, intent(inout) :: dbldt

      real :: dtsm

      ! GHB:  this value is arbitrary ... could be probably be changed
!!!      integer, parameter :: max_nsound = 96
      integer, parameter :: max_nsound = 192

      IF( psolver.eq.1 .or. psolver.eq.4 .or. psolver.eq.5 )THEN

        dtsm = dt
        nsound = 1

      ELSE

        ! Algorithm to determine number of small steps:
        IF( psolver.eq.2 )THEN
          ! check dx,dy,dz:
          IF( ny.eq.1 )THEN
            ! 2D sims (x-z):
            dtsm = 0.60*min( min_dx , min_dz )/360.0
          ELSEIF( nx.eq.1 )THEN
            ! 2D sims (y-z):
            dtsm = 0.60*min( min_dy , min_dz )/360.0
          ELSE
            ! 3D sims:
            dtsm = 0.50*min( min_dx , min_dy , min_dz )/360.0
          ENDIF
        ELSEIF( psolver.eq.3 )THEN
          ! check dx,dy:
          IF( ny.eq.1 )THEN
            ! 2D sims (x-z):
            dtsm = 0.60*min_dx/360.0
          ELSEIF( nx.eq.1 )THEN
            ! 2D sims (y-z):
            dtsm = 0.60*min_dy/360.0
          ELSE
            ! 3D sims:
            dtsm = 0.60*min( min_dx , min_dy )/360.0
          ENDIF
        ELSEIF( psolver.eq.6 .or. psolver.eq.7 )THEN
          ! check dx,dy,dz:
          IF( ny.eq.1 )THEN
            ! 2D sims (x-z):
            dtsm = 0.60*min( min_dx , min_dz )/csound
          ELSEIF( nx.eq.1 )THEN
            ! 2D sims (y-z):
            dtsm = 0.60*min( min_dy , min_dz )/csound
          ELSE
            ! 3D sims:
            dtsm = 0.50*min( min_dx , min_dy , min_dz )/csound
          ENDIF
        ENDIF
        !print *,'dtsmall: min_{dx,dy,dz}: ', min_dx,min_dy,min_dz
        !print *,'dtsmall: dbldt: ',dbldt
        !print *,'dtsmall: dtsm: ',dtsm

        nsound = max( nint( dbldt/dtsm ) , 4 )
        if( mod(nsound,2).ne.0 ) nsound = nsound + 1
        if( dbldt/float(nsound).gt.dtsm ) nsound = nsound + 2
        !print *,'dtsmall: nsound: ',nsound

        if( nsound.gt.max_nsound )then
          ! GHB:  this is arbitrary ... could be changed
          if( adapt_dt.eq.1 )then
            nsound = max_nsound
            dbldt = nsound*dtsm
            dt = dbldt
          else
            if(myid.eq.0)then
            print *,'  -------------------------------- '
            print *
            print *,'  Limit for number of small steps exceeded: '
            print *
            print *,'      nsound      =  ',nsound
            print *,'      max_nsound  =  ',max_nsound
            print *
            print *,'  Time step (dtl) needs to be smaller '
            print *
            print *,'  -------------------------------- '
            endif
#ifdef MPI
            call MPI_BARRIER (MPI_COMM_WORLD,ierr)
#endif
            call stopcm1
          endif
        endif

      ENDIF

      end subroutine dtsmall


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getqli(q,ql,qi)
      use input,only : ib,ie,jb,je,kb,ke,ibm,iem,jbm,jem,kbm,kem, &
          numq,ni,nj,nk,nql1,nql2,nqs1,nqs2,iice,timestats,mytime, &
          time_misc02
      implicit none

      real, dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: q
      real, dimension(ib:ie,jb:je,kb:ke) :: ql,qi

      integer :: i,j,k,n

!$omp parallel do default(shared)  &
!$omp private(i,j,k,n)
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
    DO k=1,nk
      do j=1,nj
      do i=1,ni
        ql(i,j,k)=0.0
        qi(i,j,k)=0.0
      enddo
      enddo
    ENDDO

    IF( nql1.gt.0 )THEN
    DO n=nql1,nql2
      !$acc parallel default(present)
      !$acc loop gang vector collapse(3)
      do k=1,nk
        do j=1,nj
        do i=1,ni
          ql(i,j,k)=ql(i,j,k)+q(i,j,k,n)
        enddo
        enddo
      enddo
      !$acc end parallel
    ENDDO
    ENDIF

      IF(iice.eq.1)THEN
      DO n=nqs1,nqs2
        !$acc parallel default(present)
        !$acc loop gang vector collapse(3)
        do k=1,nk
          do j=1,nj
          do i=1,ni
            qi(i,j,k)=qi(i,j,k)+q(i,j,k,n)
          enddo
          enddo
        enddo
        !$acc end parallel
      ENDDO
      ENDIF

      if(timestats.ge.1) time_misc02=time_misc02+mytime()

      end subroutine getqli


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getcvm(cvm,q)
      use input, only :ib,ie,jb,je,kb,ke,ibm,iem,jbm,jem,kbm,kem,numq, &
          eqtset,imoist,ni,nj,nk,nqv,nql1,nql2,nqs1,nqs2,iice,timestats, &
          time_misc03,mytime
      use constants
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: cvm
      real, intent(in), dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: q

      integer :: i,j,k,n

!$omp parallel do default(shared)  &
!$omp private(i,j,k,n)
    DO k=1,nk

      IF( eqtset.le.1 .or. imoist.eq.0 )THEN

        do j=1,nj
        do i=1,ni
          cvm(i,j,k) = cv
        enddo
        enddo

      ELSE

        do j=1,nj
        do i=1,ni
          cvm(i,j,k) = cv+cvv*q(i,j,k,nqv)
        enddo
        enddo
      IF( nql1.gt.0 )THEN
        do n=nql1,nql2
          do j=1,nj
          do i=1,ni
            cvm(i,j,k)=cvm(i,j,k)+cpl*q(i,j,k,n)
          enddo
          enddo
        enddo
      ENDIF
        IF(iice.eq.1)THEN
          do n=nqs1,nqs2
          do j=1,nj
          do i=1,ni
            cvm(i,j,k)=cvm(i,j,k)+cpi*q(i,j,k,n)
          enddo
          enddo
          enddo
        ENDIF

      ENDIF

    ENDDO

      if(timestats.ge.1) time_misc03=time_misc03+mytime()

      end subroutine getcvm

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine getcvm_GPU(cvm,q)
      use input, only :ib,ie,jb,je,kb,ke,ibm,iem,jbm,jem,kbm,kem,numq, &
          eqtset,imoist,ni,nj,nk,nqv,nql1,nql2,nqs1,nqs2,iice,timestats, &
          time_misc03,mytime
      use constants
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: cvm
      real, intent(in), dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: q

      integer :: i,j,k,n


    IF( eqtset.le.1 .or. imoist.eq.0 )THEN

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
      DO k=1,nk
      do j=1,nj
      do i=1,ni
        cvm(i,j,k) = cv
      enddo
      enddo
      ENDDO

    ELSE

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
      DO k=1,nk
        do j=1,nj
        do i=1,ni
          cvm(i,j,k) = cv+cvv*q(i,j,k,nqv)
        enddo
        enddo
      ENDDO
      IF( nql1.gt.0 )THEN
        !$omp parallel do default(shared) private(i,j,k,n)
        do n=nql1,nql2
        !$acc parallel default(present)
        !$acc loop gang vector collapse(3)
        DO k=1,nk
          do j=1,nj
          do i=1,ni
            cvm(i,j,k)=cvm(i,j,k)+cpl*q(i,j,k,n)
          enddo
          enddo
        ENDDO
        !$acc end parallel
        enddo
      ENDIF
      IF(iice.eq.1)THEN
        !$omp parallel do default(shared) private(i,j,k,n)
        do n=nqs1,nqs2
        !$acc parallel default(present)
        !$acc loop gang vector collapse(3)
        DO k=1,nk
          do j=1,nj
          do i=1,ni
            cvm(i,j,k)=cvm(i,j,k)+cpi*q(i,j,k,n)
          enddo
          enddo
        ENDDO
        !$acc end parallel
        enddo
      ENDIF

    ENDIF


      if(timestats.ge.1) time_misc03=time_misc03+mytime()

      end subroutine getcvm_GPU


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine pdefq(rmax,asq,ruh,rvh,rmh,rho,q3d)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk, &
          dx,dy,dz,pdscheme,timestats,mytime,time_misc04
      implicit none

      real rmax
      double precision :: asq
      real, dimension(ib:ie) :: ruh
      real, dimension(jb:je) :: rvh
      real, dimension(ib:ie,jb:je,kb:ke) :: rmh
      real, dimension(ib:ie,jb:je,kb:ke) :: rho,q3d

      integer i,j,k
      double precision :: t1,t2,t3
      double precision :: a1,a2,tem
      double precision, dimension(nj) :: budj
      double precision, dimension(nk) :: budk

!----------------------------------------------------------------------

      tem = dx*dy*dz

      IF(pdscheme.eq.1)THEN

!$omp parallel do default(shared)   &
!$omp private(i,j,k,t1,t2,t3,a1,a2)
        do j=1,nj
        budj(j)=0.0d0
        do i=1,ni
          t1=0.0d0
          t2=0.0d0
          a1=0.0d0
          a2=0.0d0
          do k=1,nk
            t1=t1+rho(i,j,k)*q3d(i,j,k)
            a1=a1+rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
!!!            q3d(i,j,k)=max(0.0,q3d(i,j,k))
            if(q3d(i,j,k).lt.rmax) q3d(i,j,k)=0.0
            t2=t2+rho(i,j,k)*q3d(i,j,k)
          enddo
          t3=(t1+1.0d-20)/(t2+1.0d-20)
          if(t3.lt.0.0) t3=1.0d0
          do k=1,nk
            q3d(i,j,k)=t3*q3d(i,j,k)
            a2=a2+rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
          enddo
          budj(j)=budj(j)+a2-a1
        enddo
        enddo

        do j=1,nj
          asq=asq+budj(j)*tem
        enddo

      ELSE

!$omp parallel do default(shared)   &
!$omp private(i,j,k,a1,a2)
        do k=1,nk
        budk(k)=0.0d0
        do j=1,nj
        do i=1,ni
          a1=rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
!!!          q3d(i,j,k)=max(0.0,q3d(i,j,k))
          if(q3d(i,j,k).lt.rmax) q3d(i,j,k)=0.0
          a2=rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
          budk(k)=budk(k)+a2-a1
        enddo
        enddo
        enddo

        do k=1,nk
          asq=asq+budk(k)*tem
        enddo

      ENDIF

!----------------------------------------------------------------------

      if(timestats.ge.1) time_misc04=time_misc04+mytime()

      end subroutine pdefq

      subroutine pdefq_GPU(rmax,asq,ruh,rvh,rmh,rho,q3d)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk, &
          dx,dy,dz,pdscheme,timestats,mytime,time_misc23
      implicit none

      real rmax
      double precision, intent(inout)    :: asq
      real, intent(in), dimension(ib:ie) :: ruh
      real, intent(in), dimension(jb:je) :: rvh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rmh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rho
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: q3d

      integer i,j,k
      double precision :: t1,t2,t3
      double precision :: a1,a2,tem
      double precision :: budt
      !$acc declare present(ruh,rvh,rmh,rho,q3d)

!----------------------------------------------------------------------

      !print *,'pdefq_GPU: pdscheme: ',pdscheme
      !!$acc data create(budj,budk)
      !tem = dx*dy*dz
      IF(pdscheme.eq.1)THEN

        !$omp parallel do default(shared) private(i,j,k,t1,t2,t3,a1,a2)
        !$acc parallel default(present) private(i,j,k) &
        !$acc reduction(+:a1,t1,t2) reduction(+:a2) reduction(+:budt,asq)
        !$acc loop gang
        do j=1,nj
          budt = 0.0d0
          !$acc loop vector reduction(+:budt)
          do i=1,ni
            t1=0.0d0
            t2=0.0d0
            a1=0.0d0
            !$acc loop vector reduction(+:a1,t1,t2)
            do k=1,nk
              t1=t1+rho(i,j,k)*q3d(i,j,k)
              a1=a1+rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
!!!            q3d(i,j,k)=max(0.0,q3d(i,j,k))
              if(q3d(i,j,k).lt.rmax) q3d(i,j,k)=0.0
              t2=t2+rho(i,j,k)*q3d(i,j,k)
            enddo
            t3=(t1+1.0d-20)/(t2+1.0d-20)
            if(t3.lt.0.0) t3=1.0d0
            a2=0.0d0
            !$acc loop vector reduction(+:a2)
            do k=1,nk
              q3d(i,j,k)=t3*q3d(i,j,k)
              a2=a2+rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
            enddo
            budt = budt+a2-a1
          enddo
          asq=asq+budt*dx*dy*dz
        enddo
        !$acc end parallel

      ELSE

        !$omp parallel do default(shared) private(i,j,k,a1,a2)
        !$acc parallel default(present) private(i,j,k,a1,a2) reduction(+:budt,asq)
        !$acc loop gang
        do k=1,nk
          budt=0.0d0
          !$acc loop vector collapse(2) reduction(+:budt)
          do j=1,nj
          do i=1,ni
            a1=rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
!!!          q3d(i,j,k)=max(0.0,q3d(i,j,k))
            if(q3d(i,j,k).lt.rmax) q3d(i,j,k)=0.0
            a2=rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
            budt=budt+a2-a1
          enddo
          enddo
          asq=asq+budt*dx*dy*dz
        enddo
      !$acc end parallel

      ENDIF

!----------------------------------------------------------------------

      if(timestats.ge.1) time_misc23=time_misc23+mytime()

      end subroutine pdefq_GPU

      subroutine pdefq2_GPU(rmax,asq,ruh,rvh,rmh,rho,q3d)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk, &
          dx,dy,dz,pdscheme,timestats,mytime,time_misc23

      implicit none

      real rmax
      double precision, intent(inout)    :: asq
      real, intent(in), dimension(ib:ie) :: ruh
      real, intent(in), dimension(jb:je) :: rvh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rmh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rho
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: q3d

      integer i,j,k
      double precision :: t1,t2,t3
      double precision :: a1,a2,tem
      double precision, dimension(nj) :: budj
      double precision, dimension(nk) :: budk
      double precision :: budt
      !$acc declare present(ruh,rvh,rmh,rho,q3d)

!----------------------------------------------------------------------

      !print *,'pdefq2_GPU: pdscheme: ',pdscheme
      !tem = dx*dy*dz
      IF(pdscheme.eq.1)THEN

        !$omp parallel do default(shared) private(i,j,k,t1,t2,t3,a1,a2)
        !$acc parallel default(present) private(i,j,k) &
        !$acc reduction(+:a1,t1,t2) reduction(+:a2) reduction(+:asq)
        !$acc loop gang collapse(2)
        do j=1,nj
          do i=1,ni
            t1=0.0d0
            t2=0.0d0
            a1=0.0d0
            a2=0.0d0
            !$acc loop vector reduction(+:a1,t1,t2)
            do k=1,nk
              t1=t1+rho(i,j,k)*q3d(i,j,k)
              a1=a1+rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
!!!            q3d(i,j,k)=max(0.0,q3d(i,j,k))
              if(q3d(i,j,k).lt.rmax) q3d(i,j,k)=0.0
              t2=t2+rho(i,j,k)*q3d(i,j,k)
            enddo
            t3=(t1+1.0d-20)/(t2+1.0d-20)
            if(t3.lt.0.0) t3=1.0d0
            !$acc loop vector reduction(+:a2)
            do k=1,nk
              q3d(i,j,k)=t3*q3d(i,j,k)
              a2=a2+rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
            enddo
            asq=asq+(a2-a1)*dx*dy*dz
          enddo
        enddo
        !$acc end parallel

      ELSE

        !$omp parallel do default(shared) private(i,j,k,a1,a2)
        !$acc parallel default(present) private(i,j,k,a1,a2) reduction(+:budt,asq)
        !$acc loop gang reduction(+:asq)
        do k=1,nk
          budt=0.0d0
          !$acc loop vector collapse(2) reduction(+:budt)
          do j=1,nj
          do i=1,ni
            a1=rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
!!!          q3d(i,j,k)=max(0.0,q3d(i,j,k))
            if(q3d(i,j,k).lt.rmax) q3d(i,j,k)=0.0
            a2=rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
            budt=budt+a2-a1
          enddo
          enddo
          asq=asq+budt*dx*dy*dz
        enddo
        !$acc end parallel

      ENDIF

!----------------------------------------------------------------------

      if(timestats.ge.1) time_misc23=time_misc23+mytime()

      end subroutine pdefq2_GPU


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine pdefqtest(rmax,asq,ruh,rvh,rmh,rho,q3d,dum2d1,dum2d2,dum2d3,dum2d4,dum2d5)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,dx,dy,dz,pdscheme, &
          timestats,mytime,time_misc05
      implicit none

      real rmax
      double precision :: asq
      real, dimension(ib:ie) :: ruh
      real, dimension(jb:je) :: rvh
      real, dimension(ib:ie,jb:je,kb:ke) :: rmh
      real, dimension(ib:ie,jb:je,kb:ke) :: rho,q3d
      double precision, intent(inout), dimension(ib:ie,jb:je) :: dum2d1,dum2d2,dum2d3,dum2d4,dum2d5

      integer i,j,k
      double precision :: t1,t2,t3
      double precision :: a1,a2,tem
      double precision, dimension(nj) :: budj
      double precision, dimension(nk) :: budk
      double precision :: budt
      !$acc declare &
      !$acc present(ruh,rvh,rmh,rho,q3d) &
      !$acc present( dum2d1,dum2d2,dum2d3,dum2d4,dum2d5)
      !$acc data create(budk)

!----------------------------------------------------------------------

      tem = dx*dy*dz

      IF(pdscheme.eq.1)THEN

        
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=jb,je
        do i=ib,ie
          dum2d1(i,j) = 0.0
          dum2d2(i,j) = 0.0
          dum2d3(i,j) = 0.0
          dum2d4(i,j) = 0.0
          dum2d5(i,j) = 0.0
        enddo
        enddo

        !$acc parallel default(present) private(i,j,k)
        do k=1,nk
        !$acc loop gang vector collapse(2)
        do j=1,nj
        do i=1,ni
          ! t1
          dum2d1(i,j)=dum2d1(i,j)+rho(i,j,k)*q3d(i,j,k)
          ! a1
          dum2d2(i,j)=dum2d2(i,j)+rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
          if(q3d(i,j,k).lt.rmax) q3d(i,j,k)=0.0
          ! t2
          dum2d3(i,j)=dum2d3(i,j)+rho(i,j,k)*q3d(i,j,k)
        enddo
        enddo
        enddo
        !$acc end parallel


        k = 1
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          ! t3
          dum2d4(i,j)=(dum2d1(i,j)+1.0d-20)/(dum2d3(i,j)+1.0d-20)
          if(dum2d4(i,j).lt.0.0) dum2d4(i,j)=1.0d0
          q3d(i,j,k)=dum2d4(i,j)*q3d(i,j,k)
          ! a2
          dum2d5(i,j)=dum2d5(i,j)+rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
        enddo
        enddo

        !$acc parallel default(present) private(i,j,k)
        do k=2,nk
        !$acc loop gang vector collapse(2)
        do j=1,nj
        do i=1,ni
          q3d(i,j,k)=dum2d4(i,j)*q3d(i,j,k)
          ! a2
          dum2d5(i,j)=dum2d5(i,j)+rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
        enddo
        enddo
        enddo
        !$acc end parallel

        !$acc parallel loop vector collapse(2) default(present) private(i,j) reduction(+:asq)
        do j=1,nj
        do i=1,ni
          asq=asq+(dum2d5(i,j)-dum2d2(i,j))*tem
        enddo
        enddo

      ELSE

        !FIXME
        !$omp parallel do default(shared) private(i,j,k,a1,a2)
#ifdef _B4B05R
        !$acc update host(rho,q3d,ruh,rvh,rmh)
#else
        !$acc parallel default(present) private(k) reduction(+:budt) 
        !$acc loop gang 
#endif
        do k=1,nk
          budt = 0.0d0
          !$acc loop vector collapse(2) private(i,j,a1,a2) reduction(+:budt)
          do j=1,nj
          do i=1,ni
          a1=rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
!!!          q3d(i,j,k)=max(0.0,q3d(i,j,k))
          if(q3d(i,j,k).lt.rmax) q3d(i,j,k)=0.0
          a2=rho(i,j,k)*q3d(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
          !budk(k)=budk(k)+a2-a1
          budt=budt+(a2-a1)
          enddo
          enddo
          budk(k)=budt
        enddo
#ifdef _B4B05R
        !$acc update host(budk)
#else
        !$acc end parallel
        !$acc parallel loop vector default(present) private(k) reduction(+:asq)
#endif
        do k=1,nk
          asq=asq+budk(k)*tem
        enddo
#ifdef _B4B05R
        !$acc update device(asq)     
#endif

      ENDIF

!----------------------------------------------------------------------
      !$acc end data
      if(timestats.ge.1) time_misc05=time_misc05+mytime()

      end subroutine pdefqtest


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine calcprs(pi0,prs,pp3d)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,timestats, &
          mytime,time_prsrho
      use constants
      implicit none
 
      real, dimension(ib:ie,jb:je,kb:ke) :: pi0
      real, dimension(ib:ie,jb:je,kb:ke) :: prs,pp3d
 
      integer i,j,k
 
!----------------------------------------------------------------------
 
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        prs(i,j,k)=p00*((pi0(i,j,k)+pp3d(i,j,k))**cpdrd)
      enddo
      enddo
      enddo
 
!----------------------------------------------------------------------
 
      if(timestats.ge.1) time_prsrho=time_prsrho+mytime()
 
      end subroutine calcprs


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine calcrho(pi0,th0,rho,prs,pp3d,th3d,q3d)
      use input, only : ib,ie,jb,je,kb,ke,ibm,iem,jbm,jem, &
          kbm,kem,numq,ni,nj,nk,imoist,nqv,timestats,mytime, &
          time_prsrho
      use constants
      implicit none

      real, dimension(ib:ie,jb:je,kb:ke) :: pi0,th0
      real, dimension(ib:ie,jb:je,kb:ke) :: rho,prs,pp3d,th3d
      real, dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: q3d

      integer i,j,k

!----------------------------------------------------------------------

      IF(imoist.eq.1)THEN

!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          rho(i,j,k)=prs(i,j,k)                         &
             /( rd*(th0(i,j,k)+th3d(i,j,k))*(pi0(i,j,k)+pp3d(i,j,k))     &
                  *(1.0+max(0.0,q3d(i,j,k,nqv))*reps) )
        enddo
        enddo
        enddo

      ELSE

!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          rho(i,j,k)=prs(i,j,k)   &
             /(rd*(th0(i,j,k)+th3d(i,j,k))*(pi0(i,j,k)+pp3d(i,j,k)))
        enddo
        enddo
        enddo

      ENDIF

!----------------------------------------------------------------------

      if(timestats.ge.1) time_prsrho=time_prsrho+mytime()

      end subroutine calcrho


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine calcdbz(rho,qr,qs,qg,dbz)
      use input, only : ib,ie,jb,je,kb,ke,ptype,ni,nj,nk, &
          timestats,mytime,time_write,dowr,outfile
      use constants
      use goddard_module, only : ROQR,ROQG,ROQS,TNW,TNG,TNSS
      implicit none

      real, dimension(ib:ie,jb:je,kb:ke) :: rho,qr,qs,qg,dbz

      integer :: i,j,k
      real :: n0r,n0g,n0s,rhor,rhog,rhos,gamma,zer,zeg,zes
      real, parameter :: epp = 1.0e-8

      ! Reference:  Fovell and Ogura, 1988, JAS, pg 3850
      !             (and references therein)

  IF(ptype.eq.2)THEN

    rhor = 1000.0 * ROQR
    rhog = 1000.0 * ROQG
    rhos = 1000.0 * ROQS

    n0r = 1.0e8 * TNW
    n0g = 1.0e8 * TNG
    n0s = 1.0e8 * TNSS

!!!    print *,'  rhor,rhog,rhos = ',rhor,rhog,rhos
!!!    print *,'  n0r,n0g,n0s    = ',n0r,n0g,n0s

!$omp parallel do default(shared)  &
!$omp private(i,j,k,gamma,zer,zeg,zes)
    do k=1,nk
    do j=1,nj
    do i=1,ni

    if(qr(i,j,k).ge.epp)then
      !--- rain ---
      gamma=(3.14159*n0r*rhor/(rho(i,j,k)*qr(i,j,k)))**0.25
      zer=720.0*n0r*(gamma**(-7))
    else
      zer=0.0
    endif

    if(qg(i,j,k).ge.epp)then
      !--- graupel/hail ---
      gamma=(3.14159*n0g*rhog/(rho(i,j,k)*qg(i,j,k)))**0.25
      zeg=720.0*n0g*(gamma**(-7))*((rhog/rhor)**2)*0.224
    else
      zeg=0.0
    endif

    if(qs(i,j,k).ge.epp)then
      !--- snow ---
      gamma=(3.14159*n0s*rhos/(rho(i,j,k)*qs(i,j,k)))**0.25
      zes=720.0*n0s*(gamma**(-7))*((rhos/rhor)**2)*0.224
    else
      zes=0.0
    endif

      !--- dbz ---

    if( (zer+zeg+zes).gt.1.0e-18 )then
      dbz(i,j,k)=10.0*log10((zer+zeg+zes)*1.0e18)
    else
      dbz(i,j,k)=0.0
    endif

    enddo
    enddo
    enddo

  ELSE

    if(dowr) write(outfile,*)
    if(dowr) write(outfile,*) ' ptype = ',ptype
    if(dowr) write(outfile,*)
    if(dowr) write(outfile,*) ' calcdbz is not valid for this value of ptype'
    if(dowr) write(outfile,*)
    call stopcm1

  ENDIF

      if(timestats.ge.1) time_write=time_write+mytime()

      end subroutine calcdbz


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine calcuh(uf,vf,zh,zf,ua,va,wa,uh,zeta,dum1,dum2, &
                        zs,rgzu,rgzv,rds,sigma,rdsf,sigmaf)
      use input, only : ib,ie,jb,je,kb,ke,itb,ite,jtb,jte,ni,nj,nk, &
          rdx,rdy,cgs1,cgs2,cgs3,cgt1,cgt2,cgt3,zt,terrain_flag
      implicit none

      ! Subroutine to calculate vertically integrated updraft helicity
      ! Reference:  Kain et al, 2008, WAF, p 931

      ! note:  need zh,zf Above Ground Level

      real, intent(in), dimension(ib:ie+1) :: uf
      real, intent(in), dimension(jb:je+1) :: vf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, intent(inout), dimension(ib:ie,jb:je) :: uh
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: zeta,dum1,dum2
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(in), dimension(itb:ite,jtb:jte) :: rgzu,rgzv
      real, intent(in), dimension(kb:ke) :: rds,sigma
      real, intent(in), dimension(kb:ke+1) :: rdsf,sigmaf

      real, parameter :: zz0 = 2000.0     ! bottom of integration layer (m AGL)
      real, parameter :: zzt = 5000.0     ! top of integration layer (m AGL)

      integer :: i,j,k
      real :: r1,r2
      real :: wbar,zbar
      !!$acc declare &
      !!$acc present(uf,vf,zh,zf,ua,va,wa,uh,zeta,dum1,dum2, &
      !!$acc         zs,rgsu,rgzv,rds,sigma,rdsf,sigmaf)

  IF(.not.terrain_flag)THEN

    ! Cartesian grid, without terrain:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
    !$acc parallel loop gang vector collapse(3) default(present)
    DO k=1,nk
    DO j=1,nj+1
    DO i=1,ni+1
      zeta(i,j,k) = (va(i,j,k)-va(i-1,j,k))*rdx*uf(i)   &
                   -(ua(i,j,k)-ua(i,j-1,k))*rdy*vf(j)
    ENDDO
    ENDDO
    ENDDO

  ELSE

    ! Cartesian grid, with terrain:

        ! dum1 stores u at w-pts:
        ! dum2 stores v at w-pts:
!$omp parallel do default(shared)   &
!$omp private(i,j,k,r1,r2)
    !$acc parallel loop gang vector default(present) private(i,j,k,r1,r2)
        do j=0,nj+2
          ! lowest model level:
          do i=0,ni+2
            dum1(i,j,1) = cgs1*ua(i,j,1)+cgs2*ua(i,j,2)+cgs3*ua(i,j,3)
            dum2(i,j,1) = cgs1*va(i,j,1)+cgs2*va(i,j,2)+cgs3*va(i,j,3)
          enddo

          ! upper-most model level:
          !$acc loop
          do i=0,ni+2
            dum1(i,j,nk+1) = cgt1*ua(i,j,nk)+cgt2*ua(i,j,nk-1)+cgt3*ua(i,j,nk-2)
            dum2(i,j,nk+1) = cgt1*va(i,j,nk)+cgt2*va(i,j,nk-1)+cgt3*va(i,j,nk-2)
          enddo

          ! interior:
          !$acc loop
          do k=2,nk
          r2 = (sigmaf(k)-sigma(k-1))*rds(k)
          r1 = 1.0-r2
          !$acc loop
          do i=0,ni+2
            dum1(i,j,k) = r1*ua(i,j,k-1)+r2*ua(i,j,k)
            dum2(i,j,k) = r1*va(i,j,k-1)+r2*va(i,j,k)
          enddo
          enddo
        enddo
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,r1)
!$omp parallel do default(shared)  &
!$omp private(i,j,k,r1)
        do k=1,nk
          do j=1,nj+1
          do i=1,ni+1
            r1 = zt/(zt-0.25*((zs(i-1,j-1)+zs(i,j))+(zs(i-1,j)+zs(i,j-1))))
            zeta(i,j,k)=( r1*(va(i,j,k)*rgzv(i,j)-va(i-1,j,k)*rgzv(i-1,j))*rdx*uf(i)  &
                         +0.5*( (zt-sigmaf(k+1))*(dum2(i-1,j,k+1)+dum2(i,j,k+1))      &
                               -(zt-sigmaf(k  ))*(dum2(i-1,j,k  )+dum2(i,j,k  ))      &
                              )*rdsf(k)*r1*(rgzv(i,j)-rgzv(i-1,j))*rdx*uf(i) )        &
                       -( r1*(ua(i,j,k)*rgzu(i,j)-ua(i,j-1,k)*rgzu(i,j-1))*rdy*vf(j)  &
                         +0.5*( (zt-sigmaf(k+1))*(dum1(i,j-1,k+1)+dum1(i,j,k+1))      &
                               -(zt-sigmaf(k  ))*(dum1(i,j-1,k  )+dum1(i,j,k  ))      &
                              )*rdsf(k)*r1*(rgzu(i,j)-rgzu(i,j-1))*rdy*vf(j) )
          enddo
          enddo
        enddo

  ENDIF

!$acc parallel loop gang vector collapse(2) default(present) private(i,j,k,wbar,zbar)
!$omp parallel do default(shared)  &
!$omp private(i,j,k,wbar,zbar)
    DO j=1,nj
    DO i=1,ni
      uh(i,j) = 0.0
      !$acc loop
      DO k=1,nk
        IF( zh(i,j,k).ge.zz0 .and. zh(i,j,k).le.zzt )THEN
          ! note:  only consider cyclonically rotating updrafts
          !        (so, w and zeta must both be positive)
          wbar = max( 0.0 , 0.5*(wa(i,j,k)+wa(i,j,k+1)) )
          zbar = max( 0.0 , 0.25*(zeta(i,j,k)+zeta(i+1,j,k)   &
                                 +zeta(i,j+1,k)+zeta(i+1,j+1,k)) )
          uh(i,j) = uh(i,j) + (min(zf(i,j,k+1),zzt)-max(zf(i,j,k),zz0))*wbar*zbar
        ENDIF
      ENDDO
    ENDDO
    ENDDO

      end subroutine calcuh


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine calcvort(xh,xf,uf,vf,zh,mh,zf,mf,                                         &
                          zs,gz,gzu,gzv,rgz,rgzu,rgzv,gxu,gyv,rds,sigma,rdsf,sigmaf,       &
                          ua,va,wa,xvort,yvort,zvort,tem ,dum1,dum2,pv  ,th  ,th0,tha,rr,  &
                          ust,znt,u1,v1,s1)
      use input, only : ib,ie,jb,je,kb,ke,itb,ite,jtb,jte,ktb,kte,ni,nj,nk, &
          cgs1,cgs2,cgs3,cgt1,cgt2,cgt3,axisymm,rdx,rdy,rdz,bbc,tbc,imove, &
          umove,vmove,zt,dgs1,dgs2,dgs3,dgt1,dgt2,dgt3,output_pv,terrain_flag
      use constants
      implicit none

      ! Subroutine to calculate 3 components of vorticity
      ! at scalar points.

      ! cm1r19.6:  ua and va are now ground-relative winds when imove=1

      real, intent(in), dimension(ib:ie) :: xh
      real, intent(in), dimension(ib:ie+1) :: xf,uf
      real, intent(in), dimension(jb:je+1) :: vf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh,mh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf,mf
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(in), dimension(itb:ite,jtb:jte) :: gz,gzu,gzv,rgz,rgzu,rgzv
      real, intent(in), dimension(itb:ite,jtb:jte,ktb:kte) :: gxu,gyv
      real, intent(in), dimension(kb:ke) :: rds,sigma
      real, intent(in), dimension(kb:ke+1) :: rdsf,sigmaf
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: xvort,yvort,zvort,tem,dum1,dum2,pv,th
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: th0,tha,rr
      real, intent(in), dimension(ib:ie,jb:je) :: ust,znt,u1,v1,s1

      integer :: i,j,k
      real :: r1,r2

!-----------------------------------------------------------------------

      IF( output_pv.eq.1 )THEN
        do k=1,nk
        do j=1,nj
        do i=1,ni
          pv(i,j,k)=0.0
        enddo
        enddo
        enddo
      ENDIF

    IF( terrain_flag .or. output_pv.eq.1 )THEN
      ! dum1 stores w at scalar pts
      ! dum2 stores theta at w pts
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
      do j=0,nj+1
        do k=1,nk
        do i=0,ni+1
          dum1(i,j,k)=0.5*(wa(i,j,k)+wa(i,j,k+1))
          th(i,j,k)=th0(i,j,k)+tha(i,j,k)
        enddo
        enddo
        ! lowest model level:
        do i=0,ni+1
          dum2(i,j,1) = cgs1*th(i,j,1)+cgs2*th(i,j,2)+cgs3*th(i,j,3)
        enddo
        ! upper-most model level:
        do i=0,ni+1
          dum2(i,j,nk+1) = cgt1*th(i,j,nk)+cgt2*th(i,j,nk-1)+cgt3*th(i,j,nk-2)
        enddo
        ! interior:
        do k=2,nk
        r2 = (sigmaf(k)-sigma(k-1))*rds(k)
        r1 = 1.0-r2
        do i=0,ni+1
          dum2(i,j,k) = r1*th(i,j,k-1)+r2*th(i,j,k)
        enddo
        enddo
      enddo
    ENDIF

!-----------------------------------------------------------------------
! x-vort:

  tem=0.0
  if(axisymm.eq.0)then
    IF(.not.terrain_flag)THEN
      !cccccccccccccccccccccccccccccccccccccc
      ! Cartesian grid, without terrain:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=2,nk
      do j=1,nj+1
      do i=1,ni
        tem(i,j,k) = (wa(i,j,k)-wa(i,j-1,k))*rdy*vf(j)   &
                    -(va(i,j,k)-va(i,j,k-1))*rdz*0.5*(mf(i,j-1,k)+mf(i,j,k))
      enddo
      enddo
      enddo
      IF( bbc.eq.1 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj+1
        do i=1,ni
          tem(i,j,1)=tem(i,j,2)
        enddo
        enddo
      ELSEIF( bbc.eq.2 )THEN
      if( imove.eq.0 )then
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj+1
        do i=1,ni
          tem(i,j,1)=-2.0*va(i,j,1)*rdz*0.5*(mf(i,j-1,1)+mf(i,j,1))
        enddo
        enddo
      else
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj+1
        do i=1,ni
          tem(i,j,1)=-2.0*(va(i,j,1)+vmove)*rdz*0.5*(mf(i,j-1,1)+mf(i,j,1))
        enddo
        enddo
      endif
      ENDIF
      IF( tbc.eq.1 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj+1
        do i=1,ni
          tem(i,j,nk+1)=tem(i,j,nk)
        enddo
        enddo
      ELSEIF( tbc.eq.2 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj+1
        do i=1,ni
          tem(i,j,nk+1)=2.0*va(i,j,nk)*rdz*0.5*(mf(i,j-1,nk+1)+mf(i,j,nk+1))
        enddo
        enddo
      ENDIF
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        xvort(i,j,k) = 0.25*(tem(i,j,k)+tem(i,j+1,k)+tem(i,j,k+1)+tem(i,j+1,k+1))
      enddo
      enddo
      enddo
      !cccccccccccccccccccccccccccccccccccccc
    getpv11: IF( output_pv.eq.1 )THEN
      ! here, zvort array stores d(th)/dx
      do k=1,nk
      do j=1,nj
      do i=1,ni+1
        zvort(i,j,k) = (th(i,j,k)-th(i-1,j,k))*rdx*uf(i)
      enddo
      enddo
      enddo
      ! pv1:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        pv(i,j,k)=pv(i,j,k)+xvort(i,j,k)*0.5*( zvort(i,j,k)+zvort(i+1,j,k) )
      enddo
      enddo
      enddo
    ENDIF  getpv11
      !cccccccccccccccccccccccccccccccccccccc
    ELSE
      !cccccccccccccccccccccccccccccccccccccc
      ! Cartesian grid, with terrain:
      !   (dum1 stores w at scalar-pts:)
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=2,nk
      do j=1,nj+1
      do i=1,ni
        tem(i,j,k)=(-(va(i,j,k)-va(i,j,k-1))*rds(k)                                  &
                    +(wa(i,j,k)*rgz(i,j)-wa(i,j-1,k)*rgz(i,j-1))*rdy*vf(j)           &
                    +0.5*rds(k)*( (zt-sigma(k  ))*(dum1(i,j,k  )+dum1(i,j-1,k  ))    &
                                 -(zt-sigma(k-1))*(dum1(i,j,k-1)+dum1(i,j-1,k-1)) )  &
                               *(rgz(i,j)-rgz(i,j-1))*rdy*vf(j)                      &
                   )*0.5*( gz(i,j)+gz(i,j-1) )
      enddo
      enddo
      enddo
      IF( bbc.eq.1 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj+1
        do i=1,ni
          tem(i,j,1)=tem(i,j,2)
        enddo
        enddo
      ELSEIF( bbc.eq.2 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj+1
        do i=1,ni
          tem(i,j,1)=-2.0*va(i,j,1)*rds(2)*0.5*( gz(i,j-1)+gz(i,j) )
        enddo
        enddo
      ENDIF
      IF( tbc.eq.1 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj+1
        do i=1,ni
          tem(i,j,nk+1)=tem(i,j,nk)
        enddo
        enddo
      ELSEIF( tbc.eq.2 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj+1
        do i=1,ni
          tem(i,j,nk+1)=2.0*va(i,j,nk)*rdz*0.5*(mf(i,j-1,nk+1)+mf(i,j,nk+1))
        enddo
        enddo
      ENDIF
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        xvort(i,j,k) = 0.25*(tem(i,j,k)+tem(i,j+1,k)+tem(i,j,k+1)+tem(i,j+1,k+1))
      enddo
      enddo
      enddo
    getpv1: IF( output_pv.eq.1 )THEN
      !cccccccccccccccccccccccccccccccccccccc
      ! here, zvort array stores d(th)/dx
      do k=1,nk
      do j=1,nj
      do i=1,ni+1
        zvort(i,j,k) = gzu(i,j)*(th(i,j,k)*rgz(i,j)-th(i-1,j,k)*rgz(i-1,j))*rdx*uf(i)  &
               +0.5*( gxu(i,j,k+1)*(dum2(i,j,k+1)+dum2(i-1,j,k+1))                     &
                     -gxu(i,j,k  )*(dum2(i,j,k  )+dum2(i-1,j,k  )) )*rdsf(k)
      enddo
      enddo
      enddo
      ! pv1:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        pv(i,j,k)=pv(i,j,k)+xvort(i,j,k)*0.5*( zvort(i,j,k)+zvort(i+1,j,k) )
      enddo
      enddo
      enddo
      !cccccccccccccccccccccccccccccccccccccc
    ENDIF  getpv1
    ENDIF
  else
      !cccccccccccccccccccccccccccccccccccccc
      ! Axisymmetric grid:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=2,nk
      do j=1,nj
      do i=1,ni
        tem(i,j,k) = -(va(i,j,k)-va(i,j,k-1))*rdz*mf(1,1,k)
      enddo
      enddo
      enddo
      IF( bbc.eq.1 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni
          tem(i,j,1)=tem(i,j,2)
        enddo
        enddo
      ELSEIF( bbc.eq.2 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni
          tem(i,j,1)=-2.0*va(i,j,1)*rdz*mf(1,1,1)
        enddo
        enddo
      ENDIF
      IF( tbc.eq.1 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni
          tem(i,j,nk+1)=tem(i,j,nk)
        enddo
        enddo
      ELSEIF( tbc.eq.2 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni
          tem(i,j,nk+1)=2.0*va(i,j,nk)*rdz*mf(1,1,nk+1)
        enddo
        enddo
      ENDIF
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        xvort(i,j,k) = 0.5*(tem(i,j,k)+tem(i,j,k+1))
      enddo
      enddo
      enddo
      !cccccccccccccccccccccccccccccccccccccc
  endif

!-----------------------------------------------------------------------
! y-vort:

    tem=0.0
    IF(.not.terrain_flag)THEN
      !cccccccccccccccccccccccccccccccccccccc
      ! Cartesian grid, without terrain:
      ! and axisymmetric grid:

!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=2,nk
      do j=1,nj
      do i=1,ni+1
        tem(i,j,k) = (ua(i,j,k)-ua(i,j,k-1))*rdz*0.5*(mf(i-1,j,k)+mf(i,j,k))   &
                    -(wa(i,j,k)-wa(i-1,j,k))*rdx*uf(i)
      enddo
      enddo
      enddo
      IF( bbc.eq.1 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni+1
          tem(i,j,1)=tem(i,j,2)
        enddo
        enddo
      ELSEIF( bbc.eq.2 )THEN
      if( imove.eq.0 )then
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni+1
          tem(i,j,1)=2.0*ua(i,j,1)*rdz*0.5*(mf(i-1,j,1)+mf(i,j,1))
        enddo
        enddo
      else
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni+1
          tem(i,j,1)=2.0*(ua(i,j,1)+umove)*rdz*0.5*(mf(i-1,j,1)+mf(i,j,1))
        enddo
        enddo
      endif
      ENDIF
      IF( tbc.eq.1 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni+1
          tem(i,j,nk+1)=tem(i,j,nk)
        enddo
        enddo
      ELSEIF( tbc.eq.2 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni+1
          tem(i,j,nk+1)=-2.0*ua(i,j,nk)*rdz*0.5*(mf(i-1,j,nk+1)+mf(i,j,nk+1))
        enddo
        enddo
      ENDIF
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        yvort(i,j,k) = 0.25*(tem(i,j,k)+tem(i+1,j,k)+tem(i,j,k+1)+tem(i+1,j,k+1))
      enddo
      enddo
      enddo
      !cccccccccccccccccccccccccccccccccccccc
    getpv12:  IF( output_pv.eq.1 )THEN
      ! here, zvort array stores d(th)/dy
      do k=1,nk
      do j=1,nj+1
      do i=1,ni
        zvort(i,j,k) = (th(i,j,k)-th(i,j-1,k))*rdy*vf(j)
      enddo
      enddo
      enddo
      ! pv1:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        pv(i,j,k)=pv(i,j,k)+yvort(i,j,k)*0.5*( zvort(i,j,k)+zvort(i,j+1,k) )
      enddo
      enddo
      enddo
    ENDIF  getpv12
      !cccccccccccccccccccccccccccccccccccccc
    ELSE
      !cccccccccccccccccccccccccccccccccccccc
      ! Cartesian grid, with terrain:
      !   (dum1 stores w at scalar-pts:)
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=2,nk
      do j=1,nj
      do i=1,ni+1
        tem(i,j,k)=( (ua(i,j,k)-ua(i,j,k-1))*rds(k)                                  &
                    -(wa(i,j,k)*rgz(i,j)-wa(i-1,j,k)*rgz(i-1,j))*rdx*uf(i)           &
                    -0.5*rds(k)*( (zt-sigma(k  ))*(dum1(i,j,k  )+dum1(i-1,j,k  ))    &
                                 -(zt-sigma(k-1))*(dum1(i,j,k-1)+dum1(i-1,j,k-1)) )  &
                               *(rgz(i,j)-rgz(i-1,j))*rdx*uf(i)                      &
                   )*0.5*( gz(i,j)+gz(i-1,j) )
      enddo
      enddo
      enddo
      IF( bbc.eq.1 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni+1
          tem(i,j,1)=tem(i,j,2)
        enddo
        enddo
      ELSEIF( bbc.eq.2 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni+1
          tem(i,j,1)=2.0*ua(i,j,1)*rds(2)*0.5*( gz(i-1,j)+gz(i,j) )
        enddo
        enddo
      ENDIF
      IF( tbc.eq.1 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni+1
          tem(i,j,nk+1)=tem(i,j,nk)
        enddo
        enddo
      ELSEIF( tbc.eq.2 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni+1
          tem(i,j,nk+1)=-2.0*ua(i,j,nk)*rds(nk)*0.5*( gz(i-1,j)+gz(i,j) )
        enddo
        enddo
      ENDIF
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        yvort(i,j,k) = 0.25*(tem(i,j,k)+tem(i+1,j,k)+tem(i,j,k+1)+tem(i+1,j,k+1))
      enddo
      enddo
      enddo
    getpv2:  IF( output_pv.eq.1 )THEN
      !cccccccccccccccccccccccccccccccccccccc
      ! here, zvort array stores d(th)/dy
      do k=1,nk
      do j=1,nj+1
      do i=1,ni
        zvort(i,j,k) = gzv(i,j)*(th(i,j,k)*rgz(i,j)-th(i,j-1,k)*rgz(i,j-1))*rdy*vf(j)  &
               +0.5*( gyv(i,j,k+1)*(dum2(i,j,k+1)+dum2(i,j-1,k+1))                     &
                     -gyv(i,j,k  )*(dum2(i,j,k  )+dum2(i,j-1,k  )) )*rdsf(k)
      enddo
      enddo
      enddo
      ! pv1:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        pv(i,j,k)=pv(i,j,k)+yvort(i,j,k)*0.5*( zvort(i,j,k)+zvort(i,j+1,k) )
      enddo
      enddo
      enddo
    ENDIF  getpv2
      !cccccccccccccccccccccccccccccccccccccc
    ENDIF

!-----------------------------------------------------------------------
! z-vort:

    tem=0.0
    if(axisymm.eq.0)then
      IF(.not.terrain_flag)THEN
        !cccccccccccccccccccccccccccccccccccccc
        ! Cartesian grid, without terrain:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do k=1,nk
          do j=1,nj+1
          do i=1,ni+1
            tem(i,j,k) = (va(i,j,k)-va(i-1,j,k))*rdx*uf(i)   &
                        -(ua(i,j,k)-ua(i,j-1,k))*rdy*vf(j)
          enddo
          enddo
          do j=1,nj
          do i=1,ni
            zvort(i,j,k) = 0.25*(tem(i,j,k)+tem(i+1,j,k)+tem(i,j+1,k)+tem(i+1,j+1,k))
          enddo
          enddo
        enddo
        !cccccccccccccccccccccccccccccccccccccc
      ELSE
        !cccccccccccccccccccccccccccccccccccccc
        ! Cartesian grid, with terrain:
        ! dum1 stores u at w-pts:
        ! dum2 stores v at w-pts:
!$omp parallel do default(shared)   &
!$omp private(i,j,k,r1,r2)
        do j=0,nj+2
          ! lowest model level:
          do i=0,ni+2
            dum1(i,j,1) = cgs1*ua(i,j,1)+cgs2*ua(i,j,2)+cgs3*ua(i,j,3)
            dum2(i,j,1) = cgs1*va(i,j,1)+cgs2*va(i,j,2)+cgs3*va(i,j,3)
          enddo
          ! upper-most model level:
          do i=0,ni+2
            dum1(i,j,nk+1) = cgt1*ua(i,j,nk)+cgt2*ua(i,j,nk-1)+cgt3*ua(i,j,nk-2)
            dum2(i,j,nk+1) = cgt1*va(i,j,nk)+cgt2*va(i,j,nk-1)+cgt3*va(i,j,nk-2)
          enddo
          ! interior:
          do k=2,nk
          r2 = (sigmaf(k)-sigma(k-1))*rds(k)
          r1 = 1.0-r2
          do i=0,ni+2
            dum1(i,j,k) = r1*ua(i,j,k-1)+r2*ua(i,j,k)
            dum2(i,j,k) = r1*va(i,j,k-1)+r2*va(i,j,k)
          enddo
          enddo
        enddo
!$omp parallel do default(shared)  &
!$omp private(i,j,k,r1)
        do k=1,nk
          do j=1,nj+1
          do i=1,ni+1
            r1 = zt/(zt-0.25*((zs(i-1,j-1)+zs(i,j))+(zs(i-1,j)+zs(i,j-1))))
            tem(i,j,k)=r1*( (va(i,j,k)*rgzv(i,j)-va(i-1,j,k)*rgzv(i-1,j))*rdx*uf(i)  &
                           +0.5*( (zt-sigmaf(k+1))*(dum2(i-1,j,k+1)+dum2(i,j,k+1))   &
                                 -(zt-sigmaf(k  ))*(dum2(i-1,j,k  )+dum2(i,j,k  ))   &
                                )*rdsf(k)*(rgzv(i,j)-rgzv(i-1,j))*rdx*uf(i) )        &
                      -r1*( (ua(i,j,k)*rgzu(i,j)-ua(i,j-1,k)*rgzu(i,j-1))*rdy*vf(j)  &
                           +0.5*( (zt-sigmaf(k+1))*(dum1(i,j-1,k+1)+dum1(i,j,k+1))   &
                                 -(zt-sigmaf(k  ))*(dum1(i,j-1,k  )+dum1(i,j,k  ))   &
                                )*rdsf(k)*(rgzu(i,j)-rgzu(i,j-1))*rdy*vf(j) )
          enddo
          enddo
          do j=1,nj
          do i=1,ni
            zvort(i,j,k) = 0.25*(tem(i,j,k)+tem(i+1,j,k)+tem(i,j+1,k)+tem(i+1,j+1,k))
          enddo
          enddo
        enddo
        !cccccccccccccccccccccccccccccccccccccc
      ENDIF
    else
      !cccccccccccccccccccccccccccccccccccccc
      ! Axisymmetric grid:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do k=1,nk
        do j=1,nj
        tem(1,j,k) = 0.0
        do i=2,ni+1
          tem(i,j,k) = (va(i,j,k)*xh(i)-va(i-1,j,k)*xh(i-1))*rdx*uf(i)/xf(i)
        enddo
        enddo
        do j=1,nj
        do i=1,ni
          zvort(i,j,k) = 0.5*(tem(i,j,k)+tem(i+1,j,k))
        enddo
        enddo
      enddo
      !cccccccccccccccccccccccccccccccccccccc
    endif


    getpv3:  IF( output_pv.eq.1 )THEN
      ! now, dum1 stores dt/dz:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
      do j=1,nj
        do k=2,nk
        do i=1,ni
          dum1(i,j,k) = (th(i,j,k)-th(i,j,k-1))*rdz*mf(i,j,k)
        enddo
        enddo
        do i=1,ni
          dum1(i,j,1) = (dgs3*th(i,j,3)+dgs2*th(i,j,2)+dgs1*th(i,j,1))*rdz*mh(i,j,1)
        enddo
        do i=1,ni
          dum1(i,j,nk+1) = (dgt3*th(i,j,nk-2)+dgt2*th(i,j,nk-1)+dgt1*th(i,j,nk))*rdz*mh(i,j,nk)
        enddo
        ! pv:
        do k=1,nk
        do i=1,ni
          pv(i,j,k)=pv(i,j,k)+zvort(i,j,k)*0.5*(dum1(i,j,k)+dum1(i,j,k+1))
          pv(i,j,k)=pv(i,j,k)*rr(i,j,k)
        enddo
        enddo
      enddo
    ENDIF  getpv3

!-----------------------------------------------------------------------

      IF( bbc.eq.3 )THEN
        ! cm1r18:  use log-layer equation below
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do j=1,nj
        do i=1,ni
          xvort(i,j,1) = -(ust(i,j)/(karman*(zh(i,j,1)+znt(i,j))))*(v1(i,j)/max(s1(i,j),0.01))
          yvort(i,j,1) =  (ust(i,j)/(karman*(zh(i,j,1)+znt(i,j))))*(u1(i,j)/max(s1(i,j),0.01))
        enddo
        enddo
      ENDIF

!-----------------------------------------------------------------------

      end subroutine calcvort


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine calccpch(zh,zf,th0,qv0,cpc,cph,tha,qa)
      use input, only: ib,ie,jb,je,kb,ke,ibm,iem,jbm,jem,kbm,kem, &
          ni,nj,nk, numq,nqs1,nqs2,nql1,nql2,nqv,iice,imoist
      use constants
      implicit none

      real, intent(in),    dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in),    dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(in),    dimension(ib:ie,jb:je,kb:ke) :: th0,qv0
      real, intent(inout), dimension(ib:ie,jb:je) :: cpc,cph
      real, intent(in),    dimension(ib:ie,jb:je,kb:ke) :: tha
      real, intent(in),    dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: qa

      integer :: i,j,k,n
      real :: ql
      real, dimension(nk) :: bb

      ! defines top of cold pool / location to stop calculation of C
      real, parameter :: bcrit = -0.01

!$omp parallel do default(shared)  &
!$omp private(i,j,k,n,ql,bb)
    DO j=1,nj
    DO i=1,ni
      cpc(i,j) = 0.0
      cph(i,j) = 0.0
      bb = 0.0
      do k=1,nk
        bb(k) = g*tha(i,j,k)/th0(i,j,k)
      enddo
      if(imoist.eq.1)then
        do k=1,nk
          ql = 0.0
        IF( nql1.gt.0 )THEN
          do n=nql1,nql2
            ql=ql+qa(i,j,k,n)
          enddo
        ENDIF
          if(iice.eq.1)then
            do n=nqs1,nqs2
              ql=ql+qa(i,j,k,n)
            enddo
          endif
          bb(k) = bb(k) + g*( repsm1*(qa(i,j,k,nqv)-qv0(i,j,k)) - ql )
        enddo
      endif
    ! only calculate cpc/cph if surface B is less than bcrit
    IF( bb(1).lt.bcrit .and. tha(i,j,1).le.-1.0 )THEN
      cpc(i,j) = - 2.0*bb(1)*(zf(i,j,2)-zf(i,j,1))
      k = 2
      do while( bb(k).lt.bcrit .and. k.lt.nk )
        if( cpc(i,j).lt.0.0 ) cpc(i,j) = 0.0
        cpc(i,j) = cpc(i,j) - 2.0*bb(k)*(zf(i,j,k+1)-zf(i,j,k))
        k = k + 1
      enddo
      if( cpc(i,j).gt.0.0 )then
        cpc(i,j) = sqrt(cpc(i,j))
        if(k.eq.nk)then
          cph(i,j) = zf(i,j,nk+1)
        else
          cph(i,j) = zh(i,j,k-1) + (zh(i,j,k)-zh(i,j,k-1))*(bcrit-bb(k-1))   &
                                                          /(bb(k)-bb(k-1))
        endif
        ! account for terrain:
        cph(i,j) = cph(i,j) - zf(i,j,1)
      endif
    ENDIF
    ENDDO
    ENDDO

      end subroutine calccpch


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine calccref(cref,dbz)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,timestats,mytime,time_write
      use constants
      implicit none

      real, intent(inout), dimension(ib:ie,jb:je) :: cref
      real, intent(in),    dimension(ib:ie,jb:je,kb:ke) :: dbz

      integer :: i,j,k

      !$acc parallel default(present)
      !$omp parallel do default(shared)  &
      !$omp private(i,j)
      !$acc loop gang vector collapse(2)
      do j=1,nj
      do i=1,ni
        cref(i,j) = -1000.0
      enddo
      enddo

      
      !$acc loop gang
      do k=1,nk

      !$omp parallel do default(shared) private(i,j)
      !$acc loop vector collapse(2)
      do j=1,nj
      do i=1,ni
        cref(i,j)=max(cref(i,j),dbz(i,j,k))
      enddo
      enddo

      enddo
      !$acc end parallel


      if(timestats.ge.1) time_write=time_write+mytime()

      end subroutine calccref


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine calcthe(zh,pi0,th0,the,rh,prs,ppi,tha,qa)
      use input, only : ib,ie,jb,je,kb,ke,numq,ni,nj,nk,nqv, &
          timestats,mytime,time_stat
      use constants
      implicit none

      real, dimension(ib:ie,jb:je,kb:ke) :: zh,pi0,th0
      real, dimension(ib:ie,jb:je,kb:ke) :: the,rh,prs,ppi,tha
      real, dimension(ib:ie,jb:je,kb:ke,numq) :: qa

      integer i,j,k,n
      real tx,cpm
      real, parameter :: l0 = 2.555e6

! Reference:  Bryan, 2008, MWR, p. 5239

      !$omp parallel do default(shared) private(i,j,k,n,tx,cpm)
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j,k,tx,cpm)
      do j=1,nj
      do i=1,ni
      !$acc loop seq
      do k=1,nk
        if(zh(i,j,k).le.10000.)then
          tx=(th0(i,j,k)+tha(i,j,k))*(pi0(i,j,k)+ppi(i,j,k))
          cpm=cp
          the(i,j,k)=tx                                              &
            *((p00*(1.0+qa(i,j,k,nqv)*reps)/prs(i,j,k))**(rd/cpm))   &
            *(rh(i,j,k)**(-qa(i,j,k,nqv)*rv/cpm))                    &
            *exp(l0*qa(i,j,k,nqv)/(cpm*tx))
        else
          the(i,j,k)=the(i,j,k-1)
        endif
      enddo
      enddo
      enddo

      if(timestats.ge.1) time_stat=time_stat+mytime()

      end subroutine calcthe


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine cloud(nstat,rstat,zh,qci)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,stat_out,maxz, &
          ierr,myid,timestats,time_stat,mytime
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none
 
      integer nstat
      real, dimension(stat_out) :: rstat
      real, dimension(ib:ie,jb:je,kb:ke) :: zh
      real, dimension(ib:ie,jb:je,kb:ke) :: qci

      integer i,j,k
      real qcbot(nk),qctop(nk),bot,top,vart,varb

      !$acc data create(qcbot,qctop)
      !$acc parallel default(present) private(i,j,k) reduction(max:vart) reduction(min:varb)
      !$omp parallel do default(shared) private(i,j,k)
      !$acc loop gang
      do k=1,nk
        varb=maxz
        vart=0.0
        !$acc loop vector collapse(2) reduction(max:vart) reduction(min:varb)
        do j=1,nj
        do i=1,ni
          if(qci(i,j,k).ge.clwsat)then
            vart=max(vart,zh(i,j,k))
            varb=min(varb,zh(i,j,k))
          endif
        enddo
        enddo
        qctop(k) = vart
        qcbot(k) = varb
      enddo
      !$acc end parallel

      top=0.0
      bot=maxz
      !$acc parallel default(present) reduction(max:top) reduction(min:bot)
      !$acc loop gang vector reduction(max:top) reduction(min:bot)
      do k=1,nk
        top=max(top,qctop(k))
        bot=min(bot,qcbot(k))
      enddo
      !$acc end parallel

#ifdef MPI
      call MPI_REDUCE(bot,varb,1,MPI_REAL,MPI_MIN,0,MPI_COMM_WORLD,ierr)
      call MPI_REDUCE(top,vart,1,MPI_REAL,MPI_MAX,0,MPI_COMM_WORLD,ierr)
      bot=varb
      top=vart
      if(myid.eq.0)then
#endif

      if(bot.eq.maxz) bot=0.0

      write(6,100) 'QCTOP ',top,1,1,1,   &
                   'QCBOT ',bot,1,1,1
100   format(2x,'stat:: ',a6,':',1x,f13.6,i5,i5,i5,   &
             4x,a6,':',1x,f13.6,i5,i5,i5)
 
      nstat = nstat + 1
      rstat(nstat) = top
      nstat = nstat + 1
      rstat(nstat) = bot

#ifdef MPI
      endif
#endif

      if(timestats.ge.1) time_stat=time_stat+mytime()

      !$acc end data
 
      end subroutine cloud


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine vertvort(nstat,rstat,xh,xf,uf,vf,zh,zs,rgzu,rgzv,rds,sigma,rdsf,sigmaf,dum1,dum2,ua,va)
      use input, only : ib,ie,jb,je,kb,ke,itb,ite,jtb,jte,stat_out, &
          ni,nj,nk,cgs1,cgs2,cgs3,cgt1,cgt2,cgt3,axisymm,terrain_flag, &
          ierr,myid,timestats,time_stat,mytime,ibw,ibe,ibs,ibn,rdx,rdy,zt
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none
 
      integer nstat
      real, dimension(stat_out) :: rstat
      real, dimension(ib:ie) :: xh
      real, dimension(ib:ie+1) :: xf,uf
      real, dimension(jb:je+1) :: vf
      real, dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(in), dimension(itb:ite,jtb:jte) :: rgzu,rgzv
      real, intent(in), dimension(kb:ke) :: rds,sigma
      real, intent(in), dimension(kb:ke+1) :: rdsf,sigmaf
      real, dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2
      real, dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, dimension(ib:ie,jb:je+1,kb:ke) :: va

      integer i,j,k,n,n1km,n2km,n3km,n4km,n5km
      real vort,vmax,var
      real :: r1,r2
      character(len=6) :: text

!-----
!  note:  does not account for terrain

      n1km=nk+1
      n2km=nk+1
      n3km=nk+1
      n4km=nk+1
      n5km=nk+1

      do k=nk,1,-1
        if(zh(1,1,k).ge.1000.0) n1km=k
        if(zh(1,1,k).ge.2000.0) n2km=k
        if(zh(1,1,k).ge.3000.0) n3km=k
        if(zh(1,1,k).ge.4000.0) n4km=k
        if(zh(1,1,k).ge.5000.0) n5km=k
      enddo

      IF(terrain_flag)THEN
        ! dum1 stores u at w-pts:
        ! dum2 stores v at w-pts:
!$omp parallel do default(shared)   &
!$omp private(i,j,k,r1,r2)
!$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=0,nj+2
          ! lowest model level:
          do i=0,ni+2
            dum1(i,j,1) = cgs1*ua(i,j,1)+cgs2*ua(i,j,2)+cgs3*ua(i,j,3)
            dum2(i,j,1) = cgs1*va(i,j,1)+cgs2*va(i,j,2)+cgs3*va(i,j,3)
          enddo
        enddo

          ! upper-most model level:
!$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=0,nj+2
          do i=0,ni+2
            dum1(i,j,nk+1) = cgt1*ua(i,j,nk)+cgt2*ua(i,j,nk-1)+cgt3*ua(i,j,nk-2)
            dum2(i,j,nk+1) = cgt1*va(i,j,nk)+cgt2*va(i,j,nk-1)+cgt3*va(i,j,nk-2)
          enddo
        enddo

          ! interior:
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do j=0,nj+2
          do k=2,nk
          do i=0,ni+2
            r2 = (sigmaf(k)-sigma(k-1))*rds(k)
            r1 = 1.0-r2
            dum1(i,j,k) = r1*ua(i,j,k-1)+r2*ua(i,j,k)
            dum2(i,j,k) = r1*va(i,j,k-1)+r2*va(i,j,k)
          enddo
          enddo
        enddo
      ENDIF

      !FIXME-JMD inefficient loop 
      do n=1,6
        vmax = 0.0
        if(n.eq.1)then
          k=1
          text='VORSFC'
        elseif(n.eq.2)then
          k=n1km
          text='VOR1KM'
        elseif(n.eq.3)then
          k=n2km
          text='VOR2KM'
        elseif(n.eq.4)then
          k=n3km
          text='VOR3KM'
        elseif(n.eq.5)then
          k=n4km
          text='VOR4KM'
        elseif(n.eq.6)then
          k=n5km
          text='VOR5KM'
        endif
    kcheck:  IF( k.le.nk )THEN
        vmax=-9999999.
    IF( axisymm.eq.0 )THEN
      IF(.not.terrain_flag)THEN
        ! Cartesian grid, without terrain:
!$acc parallel loop gang vector collapse(2) default(present) private(i,j,vort) reduction(max:vmax)
        do j=1+ibs,nj+1-ibn
        do i=1+ibw,ni+1-ibe
          vort=(va(i,j,k)-va(i-1,j,k))*rdx*uf(i)   &
              -(ua(i,j,k)-ua(i,j-1,k))*rdy*vf(j)
          vmax=max(vmax,vort)
        enddo
        enddo
      ELSE
        ! Cartesian grid, with terrain:
!$acc parallel loop gang vector collapse(2) default(present) private(i,j,r1,vort) reduction(max:vmax)
        do j=1+ibs,nj+1-ibn
        do i=1+ibw,ni+1-ibe
          r1 = zt/(zt-0.25*((zs(i-1,j-1)+zs(i,j))+(zs(i-1,j)+zs(i,j-1))))
          vort=( r1*(va(i,j,k)*rgzv(i,j)-va(i-1,j,k)*rgzv(i-1,j))*rdx*uf(i)  &
                +0.5*( (zt-sigmaf(k+1))*(dum2(i-1,j,k+1)+dum2(i,j,k+1))      &
                      -(zt-sigmaf(k  ))*(dum2(i-1,j,k  )+dum2(i,j,k  ))      &
                     )*rdsf(k)*r1*(rgzv(i,j)-rgzv(i-1,j))*rdx*uf(i) )        &
              -( r1*(ua(i,j,k)*rgzu(i,j)-ua(i,j-1,k)*rgzu(i,j-1))*rdy*vf(j)  &
                +0.5*( (zt-sigmaf(k+1))*(dum1(i,j-1,k+1)+dum1(i,j,k+1))      &
                      -(zt-sigmaf(k  ))*(dum1(i,j-1,k  )+dum1(i,j,k  ))      &
                     )*rdsf(k)*r1*(rgzu(i,j)-rgzu(i,j-1))*rdy*vf(j) )
          vmax=max(vmax,vort)
        enddo
        enddo
      ENDIF
    ELSE
        ! axisymmetric grid
!$acc parallel loop gang vector collapse(2) default(present) private(i,j,vort) reduction(max:vmax)
        do j=1,nj+1
        do i=2,ni+1
          vort=(xh(i)*va(i,j,k)-xh(i-1)*va(i-1,j,k))*rdx*uf(i)/xf(i)
          vmax=max(vmax,vort)
        enddo
        enddo
    ENDIF
    ENDIF  kcheck
#ifdef MPI
        call MPI_REDUCE(vmax,var,1,MPI_REAL,MPI_MAX,0,   &
                        MPI_COMM_WORLD,ierr)
        vmax=var
        if(myid.eq.0)then
#endif
        write(6,100) text,vmax
        nstat = nstat + 1
        rstat(nstat) = vmax
#ifdef MPI
        endif
#endif
      ENDDO

100   format(2x,'stat:: ',a6,':',1x,e13.6)

      if(timestats.ge.1) time_stat=time_stat+mytime()
 
      end subroutine vertvort

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 
      subroutine calccfl(nstat,rstat,dt,acfl,uh,vh,mh,ua,va,wa,writeit)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,rdx,rdy,rdz,nx,ny, &
          stat_out,axisymm,myid,ierr,myi1,myj1,adapt_dt,stopit,timestats, &
          time_stat,mytime
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

      integer nstat
      real, dimension(stat_out) :: rstat
      real :: dt
      double precision :: acfl
      real, intent(in), dimension(ib:ie) :: uh
      real, intent(in), dimension(jb:je) :: vh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: mh
      real, dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, dimension(ib:ie,jb:je,kb:ke+1) :: wa
      integer :: writeit
 
      integer i,j,k
      integer imax,jmax,kmax
      integer imaxt(nk),jmaxt(nk),kmaxt(nk)
      real dtdx,dtdy,dtdz,cfl(nk),fmax
      real :: wsp
      integer :: loc
      real, dimension(2) :: mmax,nmax

!$acc data create(imaxt,jmaxt,kmaxt,cfl,mmax,nmax)

      dtdx=0.5*dt*rdx
      dtdy=0.5*dt*rdy
      dtdz=0.5*dt*rdz

      cfl = -1.0
      imaxt = 0
      jmaxt = 0
      kmaxt = 0

      if(nx.gt.1.and.ny.gt.1)then
!$omp parallel do default(shared)  &
!$omp private(i,j,k,wsp)
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,wsp)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          wsp = sqrt( ( ((ua(i,j,k)+ua(i+1,j,k))*dtdx*uh(i))**2     &
                       +((va(i,j,k)+va(i,j+1,k))*dtdy*vh(j))**2 )   &
                       +((wa(i,j,k)+wa(i,j,k+1))*dtdz*mh(i,j,k))**2 )
          if( wsp.gt.cfl(k) )then
            !$acc atomic write
            cfl(k) = wsp
            !$acc atomic write
            imaxt(k)=i
            !$acc atomic write
            jmaxt(k)=j
            !$acc atomic write
            kmaxt(k)=k
          endif
        enddo
        enddo
        enddo
      elseif(nx.gt.1)then
!$omp parallel do default(shared)  &
!$omp private(i,j,k,wsp)
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,wsp)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          wsp = sqrt( ((ua(i,j,k)+ua(i+1,j,k))*dtdx*uh(i))**2     &
                     +((wa(i,j,k)+wa(i,j,k+1))*dtdz*mh(i,j,k))**2 )
          if( wsp.gt.cfl(k) )then
            !$acc atomic write
            cfl(k) = wsp
            !$acc atomic write
            imaxt(k)=i
            !$acc atomic write
            jmaxt(k)=j
            !$acc atomic write
            kmaxt(k)=k
          endif
        enddo
        enddo
        enddo
      elseif(axisymm.eq.0.and.ny.gt.1)then
!$omp parallel do default(shared)  &
!$omp private(i,j,k,wsp)
!$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,wsp)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          wsp = sqrt( ((va(i,j,k)+va(i,j+1,k))*dtdy*vh(j))**2     &
                     +((wa(i,j,k)+wa(i,j,k+1))*dtdz*mh(i,j,k))**2 )
          if( wsp.gt.cfl(k) )then
            !$acc atomic write
            cfl(k) = wsp
            !$acc atomic write
            imaxt(k)=i
            !$acc atomic write
            jmaxt(k)=j
            !$acc atomic write
            kmaxt(k)=k
          endif
        enddo
        enddo
        enddo
      endif

      fmax=-99999999.
      imax=1
      jmax=1
      kmax=1
      !$acc parallel loop default(present) private(k)
      do k=1,nk
        if(cfl(k).gt.fmax)then
          !$acc atomic write
          fmax=cfl(k)
          !$acc atomic write
          imax=imaxt(k)
          !$acc atomic write
          jmax=jmaxt(k)
          !$acc atomic write
          kmax=kmaxt(k)
        endif
      enddo

#ifdef MPI
      mmax(1)=fmax
      mmax(2)=myid
      call MPI_ALLREDUCE(mmax,nmax,1,MPI_2REAL,MPI_MAXLOC,   &
                         MPI_COMM_WORLD,ierr)
      !!$acc update device(mmax)
      loc=nint(nmax(2))
      imax=imax+(myi1-1)
      jmax=jmax+(myj1-1)
      call MPI_BCAST(imax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(kmax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      !!$acc update device(imax,jmax,kmax)
      fmax=nmax(1)
      if(myid.eq.0)then
#endif

    IF(writeit.eq.1)THEN
      nstat = nstat + 1
      IF( adapt_dt.eq.1 )THEN
        write(6,100) 'CFLMAX',sngl(acfl),imax,jmax,kmax
        rstat(nstat) = sngl(acfl)
      ELSE
        write(6,100) 'CFLMAX',fmax,imax,jmax,kmax
        rstat(nstat) = fmax
      ENDIF
100   format(2x,'stat:: ',a6,':',1x,f13.6,i5,i5,i5)
    ENDIF

#ifdef MPI
      endif
#endif

!!!      cflmax = fmax

      print *,'calccfl: fmax is: ',fmax
      if(fmax.ge.1.50) stopit=.true.
 
      if(timestats.ge.1) time_stat=time_stat+mytime()

!$acc end data

      end subroutine calccfl

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

 
      subroutine calccfl_fold(nstat,rstat,dt,acfl,uh,vh,mh,ua,va,wa,writeit)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,rdx,rdy,rdz,nx,ny, &
          stat_out,axisymm,myid,ierr,myi1,myj1,adapt_dt,stopit,timestats, &
          time_stat,mytime

      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

      integer nstat
      real, dimension(stat_out) :: rstat
      real :: dt
      double precision :: acfl
      real, intent(in), dimension(ib:ie) :: uh
      real, intent(in), dimension(jb:je) :: vh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: mh
      real, dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, dimension(ib:ie,jb:je,kb:ke+1) :: wa
      integer :: writeit
 
      integer i,j,k,index
      integer imax,jmax,kmax
      integer imaxt(ni*nj),jmaxt(ni*nj),kmaxt(ni*nj)
      real dtdx,dtdy,dtdz,cfl(ni*nj),fmax
      real :: wsp
      integer :: loc
      integer RANGE, PAIRS
      real, dimension(2) :: mmax,nmax

!$acc data create(imaxt,jmaxt,kmaxt,cfl,mmax,nmax)

      dtdx=0.5*dt*rdx
      dtdy=0.5*dt*rdy
      dtdz=0.5*dt*rdz

      cfl = -1.0
      imaxt = 0
      jmaxt = 0
      kmaxt = 0

      if(nx.gt.1.and.ny.gt.1)then
!! Each thread runs a serial loop and finds the max from that "stripe".
!! We'd really want to split the second dimension between the parallel &
!! serial loops so we can fill the SM's with threads but not oversubscribe.
!$omp parallel do default(shared)  &
!$omp private(i,j,k,wsp)
!$acc parallel loop gang vector collapse(2) default(present) private(i,j,k,wsp)
        do j=1,nj
        do i=1,ni
!$acc loop seq private(index)
        do k=1,nk
          wsp = sqrt( ( ((ua(i,j,k)+ua(i+1,j,k))*dtdx*uh(i))**2     &
                       +((va(i,j,k)+va(i,j+1,k))*dtdy*vh(j))**2 )   &
                       +((wa(i,j,k)+wa(i,j,k+1))*dtdz*mh(i,j,k))**2 )
          index=(j-1)*ni+i
          if( wsp.gt.cfl(index) )then
            cfl(index) = wsp
            imaxt(index)=i
            jmaxt(index)=j
            kmaxt(index)=k
          endif
        enddo
        enddo
        enddo
      elseif(nx.gt.1)then
!$omp parallel do default(shared)  &
!$omp private(i,j,k,wsp)
!$acc parallel loop gang vector collapse(2) default(present) private(i,j,k,wsp)
        do j=1,nj
        do i=1,ni
!$acc loop seq private(index)
        do k=1,nk
          wsp = sqrt( ((ua(i,j,k)+ua(i+1,j,k))*dtdx*uh(i))**2     &
                     +((wa(i,j,k)+wa(i,j,k+1))*dtdz*mh(i,j,k))**2 )
          index=(j-1)*ni+i
          if( wsp.gt.cfl(index) )then
            cfl(index) = wsp
            imaxt(index)=i
            jmaxt(index)=j
            kmaxt(index)=k
          endif
        enddo
        enddo
        enddo
      elseif(axisymm.eq.0.and.ny.gt.1)then
!$omp parallel do default(shared)  &
!$omp private(i,j,k,wsp)
!$acc parallel loop gang vector collapse(2) default(present) private(i,j,k,wsp)
        do j=1,nj
        do i=1,ni
!$acc loop seq private(index)
        do k=1,nk
          wsp = sqrt( ((va(i,j,k)+va(i,j+1,k))*dtdy*vh(j))**2     &
                     +((wa(i,j,k)+wa(i,j,k+1))*dtdz*mh(i,j,k))**2 )
          index=(j-1)*ni+i
          if( wsp.gt.cfl(index) )then
            cfl(index) = wsp
            imaxt(index)=i
            jmaxt(index)=j
            kmaxt(index)=k
          endif
        enddo
        enddo
        enddo
      endif

!! Using a serial loop here so all the data is sync'd between consecutive kernel
!calls.
!! The RANGE = PAIRS or (PAIRS+1) as a way to manage odd numbers.
      PAIRS=ni*nj/2           !! Round down.
      RANGE=(ni*nj+1)/2       !! Round up.
      do while ( RANGE > 1 )
!! Now fold the 2-d array with half the threads each time.
!$acc parallel loop gang vector private(i)
        do i=1,PAIRS
          if ( cfl(i) .le. cfl(RANGE)) then
             cfl(i)=cfl(RANGE+i)
             imaxt(i)=imaxt(RANGE+i)
             jmaxt(i)=jmaxt(RANGE+i)
             kmaxt(i)=kmaxt(RANGE+i)
          endif
        enddo
        PAIRS=RANGE/2
        RANGE=(RANGE+1)/2
!! May need some kind of sync-barrier here.
      enddo

      !$acc update host (cfl,imaxt,jmaxt,kmaxt)

      fmax=cfl(1)
      imax=imaxt(1)
      jmax=jmaxt(1)
      kmax=kmaxt(1)

#ifdef MPI
      mmax(1)=fmax
      mmax(2)=myid
      call MPI_ALLREDUCE(mmax,nmax,1,MPI_2REAL,MPI_MAXLOC,   &
                         MPI_COMM_WORLD,ierr)
      !!$acc update device(mmax)
      loc=nint(nmax(2))
      imax=imax+(myi1-1)
      jmax=jmax+(myj1-1)
      call MPI_BCAST(imax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(kmax,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      !!$acc update device(imax,jmax,kmax)
      fmax=nmax(1)
      if(myid.eq.0)then
#endif

    IF(writeit.eq.1)THEN
      nstat = nstat + 1
      IF( adapt_dt.eq.1 )THEN
        write(6,100) 'CFLMAX',sngl(acfl),imax,jmax,kmax
        rstat(nstat) = sngl(acfl)
      ELSE
        write(6,100) 'CFLMAX',fmax,imax,jmax,kmax
        rstat(nstat) = fmax
      ENDIF
100   format(2x,'stat:: ',a6,':',1x,f13.6,i5,i5,i5)
    ENDIF

#ifdef MPI
      endif
#endif


      print *,'calccfl: fmax is: ',fmax
      if(fmax.ge.1.50) stopit=.true.
 
      if(timestats.ge.1) time_stat=time_stat+mytime()

!$acc end data

      end subroutine calccfl_fold


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine calccflquick(dt,uh,vh,mh,ua,va,wa,reqc)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,nx,ny,ierr, &
          rdx,rdy,rdz,axisymm,cflmax,stopit,timestats,time_cflq,mytime
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

      real, intent(in) :: dt
      real, intent(in), dimension(ib:ie) :: uh
      real, intent(in), dimension(jb:je) :: vh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: mh
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      integer, intent(inout) :: reqc
 
      integer :: i,j,k
      real :: dtdx,dtdy,dtdz,fmax,wsp,tem
      !$acc declare present(uh,vh,mh,ua,va,wa)
 
      dtdx=0.5*dt*rdx
      dtdy=0.5*dt*rdy
      dtdz=0.5*dt*rdz

      !$acc parallel private(i,j,k,wsp) reduction(max:fmax)
      fmax=-99999999.
      if(nx.gt.1.and.ny.gt.1)then
        !print *,'calcflquick: point #1'
        !$omp parallel do default(shared) private(i,j,k,wsp) reduction(max:fmax)
        !$acc loop collapse(3) private(i,j,k,wsp) reduction(max:fmax)
        do k=1,nk
          do j=1,nj
          do i=1,ni
          wsp = sqrt( ( ((ua(i,j,k)+ua(i+1,j,k))*dtdx*uh(i))**2     &
                       +((va(i,j,k)+va(i,j+1,k))*dtdy*vh(j))**2 )   &
                       +((wa(i,j,k)+wa(i,j,k+1))*dtdz*mh(i,j,k))**2 )
          fmax = max(fmax,wsp)
          ! if( abs(wsp).ge.1.30 ) print *,'  cfl,i,j,k = ',wsp,i,j,k
          enddo
          enddo
        enddo
      elseif(nx.gt.1)then
        !print *,'calcflquick: point #2'
        !$omp parallel do default(shared) private(i,j,k,wsp) reduction(max:fmax)
        !$acc loop collapse(3) private(i,j,k,wsp) reduction(max:fmax)
        do k=1,nk
          do j=1,nj
          do i=1,ni
          wsp = sqrt( ((ua(i,j,k)+ua(i+1,j,k))*dtdx*uh(i))**2     &
                     +((wa(i,j,k)+wa(i,j,k+1))*dtdz*mh(i,j,k))**2 )
          fmax = max(fmax,wsp )
!!!          if( wsp.ge.1.30 ) print *,'  cfl,myid,i,j,k = ',wsp,myid,i,j,k
          enddo
          enddo
        enddo
      elseif(axisymm.eq.0.and.ny.gt.1)then
        !print *,'calcflquick: point #3'
        !$omp parallel do default(shared) private(i,j,k,wsp) reduction(max:fmax)
        !$acc loop collapse(3) private(i,j,k,wsp) reduction(max:fmax)
        do k=1,nk
          do j=1,nj
          do i=1,ni
          wsp = sqrt( ((va(i,j,k)+va(i,j+1,k))*dtdy*vh(j))**2     &
                     +((wa(i,j,k)+wa(i,j,k+1))*dtdz*mh(i,j,k))**2 )
          fmax = max( fmax , wsp )
!!!          if( wsp.ge.1.30 ) print *,'  cfl,myid,i,j,k = ',wsp,myid,i,j,k
          enddo
          enddo
        enddo
      endif
      !print *,'calccflquick: ',fmax
      !$acc end parallel

      cflmax = fmax
      !print *,'calccflquick: afterloop: ',cflmax
#ifdef MPI
     call MPI_IALLREDUCE(MPI_IN_PLACE,cflmax,1,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,reqc,ierr)
!     call mpi_wait(reqc,mpi_status_ignore,ierr)
! interesting cflmax is never used.
#endif

      if(cflmax.ge.1.50) stopit=.true.


      if(timestats.ge.1) time_cflq=time_cflq+mytime()
 
      end subroutine calccflquick


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine calcksquick(dt,uh,vh,mf,kmh,kmv,khh,khv,reqk)
      use input, only : ib,ie,jb,je,kb,ke,ibc,iec,jbc,jec,kbc,kec,&
          ni,nj,nk,nx,ny,rdx,rdy,axisymm,ksmax,ierr,stopit,timestats, &
          time_stat,mytime
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

      real, intent(in) :: dt
      real, intent(in), dimension(ib:ie) :: uh
      real, intent(in), dimension(jb:je) :: vh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: mf
      real, intent(in), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv,khh,khv
      integer, intent(inout) :: reqk
 
      integer :: i,j,k
      real :: dtdx,dtdy,fmax,tem1,tem2,tem3
      real, dimension(nk) :: ks
      real :: kstmp


      !$acc data create(ks)
 
      dtdx=dt*rdx*rdx
      dtdy=dt*rdy*rdy

      if(nx.gt.1.and.ny.gt.1)then
        ! 3d:
        !$omp parallel do default(shared) private(i,j,k,tem1,tem2,tem3)
#ifdef _B4B04R
        !$acc update host(uh,vh,kmh,kmv,khh,khv)
#else
        !$acc parallel default(present) private(i,j,k,tem1,tem2,tem3) reduction(max:kstmp) 
#endif
        ks(1)=0.0
#ifndef _B4B04R
        !$acc loop gang 
#endif
        do k=2,nk
           kstmp = 0.0
#ifndef _B4B04R
           !$acc loop vector collapse(2) reduction(max:kstmp)
#endif
           do j=1,nj
           do i=1,ni
             tem1 = sqrt( (kmh(i,j,k)*dtdx*uh(i)*uh(i))**2 &
                      +(kmh(i,j,k)*dtdy*vh(j)*vh(j))**2 )
             tem2 = sqrt( (khh(i,j,k)*dtdx*uh(i)*uh(i))**2 &
                      +(khh(i,j,k)*dtdy*vh(j)*vh(j))**2 )
             tem3 = sqrt( (kmv(i,j,k)*dtdx*uh(i)*uh(i))**2 &
                      +(kmv(i,j,k)*dtdy*vh(j)*vh(j))**2 )
             kstmp = max( kstmp , tem1 , tem2 , tem3 )
          enddo
          enddo
          ks(k) = kstmp
        enddo
#ifdef _B4B04R
        !$acc update device(ks)
#else
        !$acc end parallel
#endif
      elseif(nx.gt.1)then
        ! 2d (including axisymm):
        !$omp parallel do default(shared) private(i,j,tem1,tem2,tem3)
#ifdef _B4B04R
        !$acc update host(uh,vh,kmh,kmv,khh,khv)
#else
        !$acc parallel default(present) private(i,j,k,tem1,tem2,tem3) reduction(max:kstmp) 
#endif
        ks(1)=0.0
#ifndef _B4B04R
        !$acc loop gang
#endif
        do k=2,nk
          kstmp = 0.0
#ifndef _B4B04R
          !$acc loop vector collapse(2) reduction(max:kstmp)
#endif
          do j=1,nj
          do i=1,ni
            tem1 = kmh(i,j,k)*dtdx*uh(i)*uh(i)
            tem2 = khh(i,j,k)*dtdx*uh(i)*uh(i)
            tem3 = kmv(i,j,k)*dtdx*uh(i)*uh(i)
            kstmp = max( kstmp , tem1 , tem2 , tem3 )
          enddo
          enddo
          ks(k) = kstmp
        enddo
#ifdef _B4B04R
        !$acc update device(ks)
#else
        !$acc end parallel
#endif
      elseif(axisymm.eq.0.and.ny.gt.1)then
        stop 1112
      endif

      fmax=-99999999.
      !$acc parallel default(present) reduction(max:fmax)
      !$acc loop gang vector reduction(max:fmax)
      do k=2,nk
         fmax = max( fmax , ks(k) )
      end do
      !$acc end parallel
      ksmax = fmax

      !$acc end data

#ifdef MPI
      call MPI_IALLREDUCE(MPI_IN_PLACE,ksmax,1,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,reqk,ierr)
#endif

      if(ksmax.ge.0.50) then 
         stopit=.true.
         stop 'calcksquick: end of subroutine'
      endif

      if(timestats.ge.1) time_stat=time_stat+mytime()
 
      end subroutine calcksquick


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine calcksmax(nstat,rstat,dt,uh,vh,mf,kmh,kmv,khh,khv)
      use input, only : ib,ie,jb,je,kb,ke,ibc,iec,jbc,jec,kbc,kec, &
          ni,nj,nk,rdx,rdy,rdz,stat_out,cm1setup,horizturb,ipbl, &
          myid,ierr,myi1,myj1,timestats,time_stat,mytime
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

      integer nstat
      real, dimension(stat_out) :: rstat
      real :: dt
      real, dimension(ib:ie) :: uh
      real, dimension(jb:je) :: vh
      real, dimension(ib:ie,jb:je,kb:ke+1) :: mf
      real, dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv,khh,khv

      integer i,j,k
      integer imaxh,jmaxh,kmaxh
      integer imaxv,jmaxv,kmaxv
      integer imaxth(nk),jmaxth(nk),kmaxth(nk)
      integer imaxtv(nk),jmaxtv(nk),kmaxtv(nk)
      real dtdx,dtdy,dtdz,tem,ksh(nk),ksv(nk),fhmax,fvmax
      integer :: loc
      real, dimension(2) :: mmax,nmax
      real maxksh,maxksv

!JMD-FIXME
!$acc data create (ksh,ksv,imaxth,jmaxth,kmaxth,imaxtv,jmaxtv,kmaxtv,mmax,nmax)

      dtdx=dt*rdx*rdx
      dtdy=dt*rdy*rdy
      dtdz=dt*rdz*rdz

!$omp parallel do default(shared)  &
!$omp private(i,j,k,tem)

!$acc parallel default(present) reduction(max:maxksh,maxksv)
!$acc loop gang reduction(max:maxksh,maxksv)
      do k=2,nk
         maxksh = -99999.0
         maxksv = -99999.0
!$acc loop vector collapse(2) reduction(max:maxksh,maxksv)
         do j=1,nj
            do i=1,ni
               tem = khh(i,j,k)*dtdx*uh(i)*uh(i)
               maxksh=max(tem,maxksh)

               tem = khh(i,j,k)*dtdy*vh(j)*vh(j)
               maxksh=max(tem,maxksh)

               tem = khv(i,j,k)*dtdz*mf(i,j,k)*mf(i,j,k)
               maxksv=max(tem,maxksv)
            end do
         end do
         ksh(k)=maxksh
         ksv(k)=maxksv
      end do
!$acc end parallel

!$omp parallel do default(shared)  &
!$omp private(i,j,k,tem)
!$acc parallel loop gang vector collapse(3) default(present)
      do k=2,nk
         do j=1,nj
            do i=1,ni
               tem = khh(i,j,k)*dtdx*uh(i)*uh(i)
               if( tem.eq.ksh(k) )then
                 !$acc atomic write
                 imaxth(k)=i
                 !$acc atomic write
                 jmaxth(k)=j
                 !$acc atomic write
                 kmaxth(k)=k
               end if
               tem = khh(i,j,k)*dtdy*vh(j)*vh(j)
               if( tem.eq.ksh(k) )then
                 !$acc atomic write
                 imaxth(k)=i
                 !$acc atomic write
                 jmaxth(k)=j
                 !$acc atomic write
                 kmaxth(k)=k
               end if
               tem = khv(i,j,k)*dtdz*mf(i,j,k)*mf(i,j,k)
               if( tem.eq.ksv(k) )then
                 !$acc atomic write
                 imaxtv(k)=i
                 !$acc atomic write
                 jmaxtv(k)=j
                 !$acc atomic write
                 kmaxtv(k)=k
               endif
            end do
         end do
      end do

      fhmax=-99999999.
      fvmax=-99999999.
      imaxh=1
      jmaxh=1
      kmaxh=1
      imaxv=1
      jmaxv=1
      kmaxv=1
      !$acc parallel loop seq default(present)
      do k=2,nk
         if(ksh(k).gt.fhmax)then
           !$acc atomic write
           fhmax=ksh(k)
           !$acc atomic write
           imaxh=imaxth(k)
           !$acc atomic write
           jmaxh=jmaxth(k)
           !$acc atomic write
           kmaxh=kmaxth(k)
         endif
         if(ksv(k).gt.fvmax)then
           !$acc atomic write
           fvmax=ksv(k)
           !$acc atomic write
           imaxv=imaxtv(k)
           !$acc atomic write
           jmaxv=jmaxtv(k)
           !$acc atomic write
           kmaxv=kmaxtv(k)
         endif
      enddo

      if( cm1setup.eq.2 .and. horizturb.eq.0 )then
        fhmax=0.0
        imaxh=0
        jmaxh=0
        kmaxh=0
      endif
      if( cm1setup.eq.2 .and. ipbl.ne.2 )then
        fvmax=0.0
        imaxv=0
        jmaxv=0
        kmaxv=0
      endif

#ifdef MPI
      mmax(1)=fhmax
      mmax(2)=myid
      call MPI_ALLREDUCE(mmax,nmax,1,MPI_2REAL,MPI_MAXLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmax(2))
      imaxh=imaxh+(myi1-1)
      jmaxh=jmaxh+(myj1-1)
      call MPI_BCAST(imaxh,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmaxh,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(kmaxh,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      fhmax=nmax(1)

      mmax(1)=fvmax
      mmax(2)=myid
      call MPI_ALLREDUCE(mmax,nmax,1,MPI_2REAL,MPI_MAXLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmax(2))
      imaxv=imaxv+(myi1-1)
      jmaxv=jmaxv+(myj1-1)
      call MPI_BCAST(imaxv,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmaxv,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(kmaxv,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      fvmax=nmax(1)
      if(myid.eq.0)then
#endif

      write(6,100) 'KSHMAX',fhmax,imaxh,jmaxh,kmaxh

      nstat = nstat + 1
      rstat(nstat) = fhmax

      write(6,100) 'KSVMAX',fvmax,imaxv,jmaxv,kmaxv

      nstat = nstat + 1
      rstat(nstat) = fvmax

100   format(2x,'stat:: ',a6,':',1x,g13.6,i5,i5,i5)

#ifdef MPI
      endif
#endif

      if(timestats.ge.1) time_stat=time_stat+mytime()

!$acc end data
!!$acc update device(nstat,rstat,dt,uh,vh,mf,kmh,kmv,khh,khv)
      end subroutine calcksmax


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine calcksmax_fold(nstat,rstat,dt,uh,vh,mf,kmh,kmv,khh,khv)
      use input, only : ib,ie,jb,je,kb,ke,ibc,iec,jbc,jec,kbc,kec, &
          ni,nj,nk,rdx,rdy,rdz,stat_out,cm1setup,horizturb,ipbl, &
          myid,ierr,myi1,myj1,timestats,time_stat,mytime
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

      integer nstat
      real, dimension(stat_out) :: rstat
      real :: dt
      real, dimension(ib:ie) :: uh
      real, dimension(jb:je) :: vh
      real, dimension(ib:ie,jb:je,kb:ke+1) :: mf
      real, dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv,khh,khv

      integer i,j,k,index
      integer imaxh,jmaxh,kmaxh
      integer imaxv,jmaxv,kmaxv
      integer imaxth(ni*nj),jmaxth(ni*nj),kmaxth(ni*nj)
      integer imaxtv(ni*nj),jmaxtv(ni*nj),kmaxtv(ni*nj)
      real dtdx,dtdy,dtdz,tem,ksh(ni*nj),ksv(ni*nj),fhmax,fvmax
      integer RANGE, PAIRS
      integer :: loc
      real, dimension(2) :: mmax,nmax

!$acc data create (ksh,ksv,imaxth,jmaxth,kmaxth,imaxtv,jmaxtv,kmaxtv,mmax,nmax)

      dtdx=dt*rdx*rdx
      dtdy=dt*rdy*rdy
      dtdz=dt*rdz*rdz

!$acc parallel loop gang vector collapse(2) default(present)
      do j=1,nj
      do i=1,ni
          index=(j-1)*ni+i
          ksh(index) = -99999.0
          ksv(index) = -99999.0
      enddo
      enddo

!$omp parallel do default(shared)  &
!$omp private(i,j,k,tem)

!$acc parallel default(present) 
!$acc loop vector collapse(2) 
      do j=1,nj
      do i=1,ni
!$acc loop seq private(index)
         do k=2,nk
            index=(j-1)*ni+i
            tem = khh(i,j,k)*dtdx*uh(i)*uh(i)
            ksh(index)=max(tem,ksh(index))

            tem = khh(i,j,k)*dtdy*vh(j)*vh(j)
            ksh(index)=max(tem,ksh(index))

            tem = khv(i,j,k)*dtdz*mf(i,j,k)*mf(i,j,k)
            ksv(index)=max(tem,ksv(index))
         end do
      end do
      enddo
!$acc end parallel

!$omp parallel do default(shared)  &
!$omp private(i,j,k,tem)
!$acc parallel loop gang vector collapse(2) default(present)
      do i=1,ni
         do j=1,nj
!$acc loop seq private(index)
            do k=2,nk
               index=(j-1)*ni+i
               tem = khh(i,j,k)*dtdx*uh(i)*uh(i)
               if( tem.eq.ksh(index) )then
                 imaxth(index)=i
                 jmaxth(index)=j
                 kmaxth(index)=k
               end if
               tem = khh(i,j,k)*dtdy*vh(j)*vh(j)
               if( tem.eq.ksh(index) )then
                 imaxth(index)=i
                 jmaxth(index)=j
                 kmaxth(index)=k
               end if
               tem = khv(i,j,k)*dtdz*mf(i,j,k)*mf(i,j,k)
               if( tem.eq.ksv(index) )then
                 imaxtv(index)=i
                 jmaxtv(index)=j
                 kmaxtv(index)=k
               endif
            end do
         end do
      end do

      fhmax=-99999999.
      fvmax=-99999999.
      imaxh=1
      jmaxh=1
      kmaxh=1
      imaxv=1
      jmaxv=1
      kmaxv=1

!! Using a serial loop here so all the data is sync'd between consecutive kernel
!calls.
!! The RANGE = PAIRS or (PAIRS+1) as a way to manage odd numbers.
      PAIRS=ni*nj/2           !! Round down.
      RANGE=(ni*nj+1)/2       !! Round up.
      do while ( RANGE > 1 )
!! Now fold the 2-d array with half the threads each time.
!$acc parallel loop gang vector private(i)
        do i=1,PAIRS
          if( ksh(i) .le. ksh(RANGE)) then
             ksh(i) = ksh(RANGE+1)
             imaxth(i) = imaxth(RANGE+1)
             jmaxth(i) = jmaxth(RANGE+1)
             kmaxth(i) = kmaxth(RANGE+1)
          endif
          if( ksv(i) .le. ksv(RANGE)) then
             ksv(i) = ksv(RANGE+1)
             imaxtv(i) = imaxtv(RANGE+1)
             jmaxtv(i) = jmaxtv(RANGE+1)
             kmaxtv(i) = kmaxtv(RANGE+1)
          endif
        enddo
        PAIRS=RANGE/2
        RANGE=(RANGE+1)/2
      enddo 

      !$acc update host (ksh,imaxth,jmaxth,kmaxth,ksv,imaxtv,jmaxtv,kmaxtv)

      fhmax=ksh(1)
      imaxh=imaxth(1)
      jmaxh=jmaxth(1)
      kmaxh=kmaxth(1)
      fvmax=ksv(1)
      imaxv=imaxtv(1)
      jmaxv=jmaxtv(1)
      kmaxv=kmaxtv(1)

      if( cm1setup.eq.2 .and. horizturb.eq.0 )then
        fhmax=0.0
        imaxh=0
        jmaxh=0
        kmaxh=0
      endif
      if( cm1setup.eq.2 .and. ipbl.ne.2 )then
        fvmax=0.0
        imaxv=0
        jmaxv=0
        kmaxv=0
      endif

#ifdef MPI
      mmax(1)=fhmax
      mmax(2)=myid
      call MPI_ALLREDUCE(mmax,nmax,1,MPI_2REAL,MPI_MAXLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmax(2))
      imaxh=imaxh+(myi1-1)
      jmaxh=jmaxh+(myj1-1)
      call MPI_BCAST(imaxh,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmaxh,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(kmaxh,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      fhmax=nmax(1)

      mmax(1)=fvmax
      mmax(2)=myid
      call MPI_ALLREDUCE(mmax,nmax,1,MPI_2REAL,MPI_MAXLOC,   &
                         MPI_COMM_WORLD,ierr)
      loc=nint(nmax(2))
      imaxv=imaxv+(myi1-1)
      jmaxv=jmaxv+(myj1-1)
      call MPI_BCAST(imaxv,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(jmaxv,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(kmaxv,1,MPI_INTEGER,loc,MPI_COMM_WORLD,ierr)
      fvmax=nmax(1)
      if(myid.eq.0)then
#endif

      write(6,100) 'KSHMAX',fhmax,imaxh,jmaxh,kmaxh

      nstat = nstat + 1
      rstat(nstat) = fhmax

      write(6,100) 'KSVMAX',fvmax,imaxv,jmaxv,kmaxv

      nstat = nstat + 1
      rstat(nstat) = fvmax

100   format(2x,'stat:: ',a6,':',1x,g13.6,i5,i5,i5)

#ifdef MPI
      endif
#endif

      if(timestats.ge.1) time_stat=time_stat+mytime()

!$acc end data
      end subroutine calcksmax_fold

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine getrmw(nstat,rstat,xh,zh,ua,va)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,stat_out, &
          timestats,time_stat,mytime
      implicit none

      integer, intent(inout) :: nstat
      real, intent(inout), dimension(stat_out) :: rstat
      real, intent(in), dimension(ib:ie) :: xh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va

      integer :: i,k,imax,jmax,kmax
      real :: wspd
      real :: rmax,zmax,vmax

      integer, dimension(nk) :: imaxt,kmaxt
      real, dimension(nk) :: rmaxt,zmaxt,vmaxt

      ! Note:  only called from axisymmetric simulation

!$acc data create(imaxt,kmaxt,rmaxt,zmaxt,vmaxt)
!$acc parallel loop gang vector default(present) private(k)
      do k=1,nk
        vmaxt(k) = 0.0
      enddo

      !FIXME
!$omp parallel do default(shared)  &
!$omp private(i,k,wspd)
!$acc parallel loop gang vector default(present)
      do k=1,nk
        !$acc loop seq
        do i=1,ni
          wspd = sqrt( (0.5*(ua(i,1,k)+ua(i+1,1,k)))**2 + va(i,1,k)**2 )
          IF( wspd.ge.vmaxt(k) )THEN
            vmaxt(k) = wspd
            rmaxt(k) = xh(i)
            zmaxt(k) = zh(i,1,k)
            imaxt(k) = i
            kmaxt(k) = k
          ENDIF
        enddo
      enddo
!$acc end parallel

      vmax = 0.0

!$acc serial default(present)
      do k=1,nk
        IF( vmaxt(k).ge.vmax )THEN
          vmax = vmaxt(k)
          rmax = rmaxt(k)
          zmax = zmaxt(k)
          imax = imaxt(k)
          kmax = kmaxt(k)
        ENDIF
      enddo
!$acc end serial

      jmax = 1

      write(6,131) 'RMW   ',rmax,imax,jmax,kmax
      write(6,131) 'ZMW   ',zmax,imax,jmax,kmax
131   format(2x,'stat:: ',a6,':',1x,f13.6,i5,i5,i5)

      nstat = nstat + 1
      rstat(nstat) = rmax
      nstat = nstat + 1
      rstat(nstat) = zmax

      if(timestats.ge.1) time_stat=time_stat+mytime()

!$acc end data

      end subroutine getrmw


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine calcmass(nstat,rstat,ruh,rvh,rmh,rho)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,dx,dy,dz, &
          myid,ierr,timestats,mytime,time_stat,stat_out
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

      integer nstat
      real, dimension(stat_out) :: rstat
      real, dimension(ib:ie) :: ruh
      real, dimension(jb:je) :: rvh
      real, dimension(ib:ie,jb:je,kb:ke) :: rmh
      real, dimension(ib:ie,jb:je,kb:ke) :: rho
 
      integer i,j,k
      double precision :: tmass,var
      double precision, dimension(nk) :: foo
      double precision :: foot

      !$acc data create(foo)

      !$omp parallel do default(shared) private(i,j,k)
#ifdef _B4B06R
      !$acc update host(rho,ruh,rvh,rmh)
#else
      !$acc parallel default(present) private(i,j,k) reduction(+:foot)
      !$acc loop gang
#endif
      do k=1,nk
        foot=0.0d0
#ifndef _B4B06R
        !$acc loop vector collapse(2) reduction(+:foot)
#endif
        do j=1,nj
        do i=1,ni
          foot=foot+rho(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
        enddo
        enddo
        foo(k)=foot
      enddo
#ifndef _B4B06R
      !$acc end parallel
#endif

      tmass=0.0d0 
#ifndef _B4B06R
      !$acc parallel default(present) private(k) reduction(+:tmass)
      !$acc loop gang vector reduction(+:tmass)
#endif
      do k=1,nk
        tmass=tmass+foo(k)
      enddo
#ifndef _B4B06R
      !$acc end parallel
#endif

      !$acc end data

#ifdef MPI
      var=0.0d0
      call MPI_REDUCE(tmass,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,   &
                      MPI_COMM_WORLD,ierr)
      tmass=var
      if(myid.eq.0)then
#endif

      tmass=tmass*(dx*dy*dz)
 
      write(6,100) 'TMASS ',tmass
100   format(2x,'stat:: ',a6,':',1x,e13.6)
 
      nstat = nstat + 1
      rstat(nstat) = tmass

#ifdef MPI
      endif
#endif
 
      if(timestats.ge.1) time_stat=time_stat+mytime()

 
      end subroutine calcmass


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine totmois(nstat,rstat,train,ruh,rvh,rmh,qv,ql,qi,rho)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,dx,dy,dz,ierr, &
          myid,timestats,time_stat,mytime,stat_out
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

      integer nstat
      real, dimension(stat_out) :: rstat
      double precision :: train
      real, dimension(ib:ie) :: ruh
      real, dimension(jb:je) :: rvh
      real, dimension(ib:ie,jb:je,kb:ke) :: rmh
      real, dimension(ib:ie,jb:je,kb:ke) :: qv,ql,qi,rho
 
      integer i,j,k
      double precision :: tmass,var
      double precision, dimension(nk) :: foo
      double precision :: foot

#ifdef _B4B07R
      !$acc update host(rho,qv,ql,qi,ruh,rvh,rmh)
#else
      !$acc data create(foo)

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel default(present) private(i,j,k) reduction(+:foot)
      !$acc loop gang
#endif
      do k=1,nk
        foot = 0.0d0   
#ifndef _B4B07R
        !$acc loop vector collapse(2) reduction(+:foot)
#endif
        do j=1,nj
        do i=1,ni
          foot=foot + rho(i,j,k)*(qv(i,j,k)+ql(i,j,k)+qi(i,j,k))*ruh(i)*rvh(j)*rmh(i,j,k)
        enddo
        enddo
        foo(k) = foot
      enddo
#ifndef _B4B07R
      !$acc end parallel
#endif
 
      tmass=0.0d0
#ifndef _B4B07R
      !$acc parallel default(present) reduction(+:tmass)
      !$acc loop gang vector reduction(+:tmass)
#endif
      do k=1,nk
        tmass=tmass+foo(k)
      enddo
#ifndef _B4B07R
      !$acc end parallel
#endif


#ifdef MPI
      var=0.0d0
      call MPI_REDUCE(tmass,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,   &
                      MPI_COMM_WORLD,ierr)
      tmass=var
      if(myid.eq.0)then
#endif

      ! cm1r18:  do not include rain:
      tmass=tmass*(dx*dy*dz)

      write(6,100) 'TMOIS ',tmass
100   format(2x,'stat:: ',a6,':',1x,e13.6)
 
      nstat = nstat + 1
      rstat(nstat) = tmass

#ifdef MPI
      endif
#endif
 
      if(timestats.ge.1) time_stat=time_stat+mytime()

#ifndef _B4B07R
!$acc end data
#endif
 
      end subroutine totmois


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine totq(nstat,rstat,ruh,rvh,rmh,q,rho,aname)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,dx,dy,dz, &
          ierr,timestats,mytime,time_stat,myid,stat_out
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

      integer nstat
      real, dimension(stat_out) :: rstat
      real, dimension(ib:ie) :: ruh
      real, dimension(jb:je) :: rvh
      real, dimension(ib:ie,jb:je,kb:ke) :: rmh
      real, dimension(ib:ie,jb:je,kb:ke) :: q,rho
      character(len=6) :: aname

      integer i,j,k
      double precision :: tmass,var
      double precision, dimension(nk) :: foo
      double precision :: foot

#ifdef _B4B08R
      !$acc update host(rho,q,ruh,rvh,rmh)
#else
      !$acc data create(foo)

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel default(present) private(i,j,k) reduction(+:foot)
      !$acc loop gang
#endif
      do k=1,nk
        foot=0.0d0
#ifndef _B4B08R
        !$acc loop vector collapse(2) reduction(+:foot)
#endif
        do j=1,nj
        do i=1,ni
          foot=foot+rho(i,j,k)*q(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
        enddo
        enddo
        foo(k)=foot
      enddo
#ifndef _B4B08R
      !$acc end parallel
#endif

      tmass=0.0d0
#ifndef _B4B08R
      !$acc parallel default(present) reduction(+:tmass)
      !$acc loop vector reduction(+:tmass)
#endif
      do k=1,nk
        tmass=tmass+foo(k)
      enddo
#ifndef _B4B08R
      !$acc end parallel
#endif

#ifdef MPI
      var=0.0d0
      call MPI_REDUCE(tmass,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,   &
                      MPI_COMM_WORLD,ierr)
      tmass=var
      if(myid.eq.0)then
#endif

      tmass=tmass*(dx*dy*dz)

      write(6,100) aname,tmass
100   format(2x,'stat:: ',a6,':',1x,e13.6)

      nstat = nstat + 1
      rstat(nstat) = tmass

#ifdef MPI
      endif
#endif

      if(timestats.ge.1) time_stat=time_stat+mytime()

#ifndef _B4B08R
!$acc end data
#endif

      end subroutine totq


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine calcener(nstat,rstat,ruh,rvh,zh,rmh,pi0,th0,rho,ua,va,wa,ppi,tha,   &
                          qv,ql,qi,vr)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,dx,dy,dz,umove,vmove, &
          stat_out,timestats,mytime,time_stat,ierr,myid
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

      integer nstat
      real, dimension(stat_out) :: rstat
      real, dimension(ib:ie) :: ruh
      real, dimension(jb:je) :: rvh
      real, dimension(ib:ie,jb:je,kb:ke) :: zh,rmh,pi0,th0
      real, dimension(ib:ie,jb:je,kb:ke) :: rho
      real, dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, dimension(ib:ie,jb:je,kb:ke) :: ppi,tha,qv,ql,qi,vr
 
      integer i,j,k
      double precision :: u,v,w,tmp,qtot,ek,ei,ep,et,le,var,tem
      double precision, dimension(nk) :: foo1,foo2,foo3,foo4
      double precision :: foo1t,foo2t,foo3t,foo4t

      ek=0.0d0
      ei=0.0d0
      ep=0.0d0
      le=0.0d0
      !$acc data create(foo1,foo2,foo3,foo4)
#ifdef _B4B
      !$acc update host(ruh,rvh,rmh,ua,va,wa,qv,ql,qi,rho,vr,pi0,ppi)
#else
      !$acc parallel default(present) private(i,j,k,u,v,w,tmp,qtot,tem) &
      !$acc reduction(+:foo1t,foo2t,foo3t,foo4t,ek,ei,ep,le)
#endif
      !$omp parallel do default(shared)  &
      !$omp private(i,j,k,u,v,w,tmp,qtot,tem)
#ifndef _B4B
      !$acc loop gang 
#endif
      do k=1,nk
        foo1t=0.0d0      ! = ek
        foo2t=0.0d0      ! = ei
        foo3t=0.0d0      ! = ep
        foo4t=0.0d0      ! = le
#ifndef _B4B
        !$acc loop vector collapse(2) reduction(+:foo1t,foo2t,foo3t,foo4t)
#endif
        do j=1,nj
        do i=1,ni
          tem=ruh(i)*rvh(j)*rmh(i,j,k)
          u=umove+0.5*(ua(i,j,k)+ua(i+1,j,k))
          v=vmove+0.5*(va(i,j,k)+va(i,j+1,k))
          w=0.5*(wa(i,j,k)+wa(i,j,k+1))
          qtot=qv(i,j,k)+ql(i,j,k)+qi(i,j,k)
          foo1t=foo1t+rho(i,j,k)*tem*(1.0+qtot)*0.5*(        &
                         0.5*( ua(i,j,k)**2 + ua(i+1,j,k)**2 )   &
                        +0.5*( va(i,j,k)**2 + va(i,j+1,k)**2 )   &
                        +0.5*( wa(i,j,k)**2 + wa(i,j,k+1)**2 ) ) &
               +ql(i,j,k)*rho(i,j,k)*tem*0.5*(vr(i,j,k)**2-2.0*w*vr(i,j,k))
          tmp=(th0(i,j,k)+tha(i,j,k))*(pi0(i,j,k)+ppi(i,j,k))
          foo2t=foo2t+rho(i,j,k)*tem*(cv+cvv*qv(i,j,k))*tmp
          foo3t=foo3t+rho(i,j,k)*tem*(1.0+qtot)*g*zh(i,j,k)
          foo4t=foo4t+rho(i,j,k)*tem*ql(i,j,k)*(cpl*tmp-lv1)   &
                         +rho(i,j,k)*tem*qi(i,j,k)*(cpi*tmp-ls1)
        enddo
        enddo
        foo1(k) = foo1t
        foo2(k) = foo2t
        foo3(k) = foo3t
        foo4(k) = foo4t
      enddo
 
#ifndef _B4B
      !$acc loop vector reduction(+:ek,ei,ep,le)
#endif
      do k=1,nk
        ek=ek+foo1(k)
        ei=ei+foo2(k)
        ep=ep+foo3(k)
        le=le+foo4(k)
      enddo
#ifndef _B4B
      !$acc end parallel
#endif
      !$acc end data

      ek=ek*(dx*dy*dz)
      ei=ei*(dx*dy*dz)
      ep=ep*(dx*dy*dz)
      le=le*(dx*dy*dz)

#ifdef MPI
      var=0.0d0
      call MPI_REDUCE(ek,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,   &
                      MPI_COMM_WORLD,ierr)
      ek=var
      var=0.0d0
      call MPI_REDUCE(ei,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,   &
                      MPI_COMM_WORLD,ierr)
      ei=var
      var=0.0d0
      call MPI_REDUCE(ep,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,   &
                      MPI_COMM_WORLD,ierr)
      ep=var
      var=0.0d0
      call MPI_REDUCE(le,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,   &
                      MPI_COMM_WORLD,ierr)
      le=var
      if(myid.eq.0)then
#endif

      et=ek+ei+ep+le
 
      write(6,100) 'TENERG',et
100   format(2x,'stat:: ',a6,':',1x,e13.6)

      nstat = nstat + 1
      rstat(nstat) = ek
      nstat = nstat + 1
      rstat(nstat) = ei
      nstat = nstat + 1
      rstat(nstat) = ep
      nstat = nstat + 1
      rstat(nstat) = le
      nstat = nstat + 1
      rstat(nstat) = et

#ifdef MPI
      endif
#endif
 
      if(timestats.ge.1) time_stat=time_stat+mytime()
 
      end subroutine calcener


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
 
      subroutine calcmoe(nstat,rstat,ruh,rvh,rmh,rho,ua,va,wa,qv,ql,qi,vr)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,dx,dy,dz,vmove,umove, &
          stat_out,myid,ierr,timestats,time_stat,mytime
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none
 
      integer nstat
      real, dimension(stat_out) :: rstat
      real, dimension(ib:ie) :: ruh
      real, dimension(jb:je) :: rvh
      real, dimension(ib:ie,jb:je,kb:ke) :: rmh
      real, dimension(ib:ie,jb:je,kb:ke) :: rho
      real, dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, dimension(ib:ie,jb:je,kb:ke) :: qv,ql,qi,vr
 
      integer i,j,k
      double precision :: tmu,tmv,tmw,qtot,var,tem
      double precision, dimension(nk) :: foo1,foo2,foo3
      double precision :: foot1,foot2,foot3
      double precision :: qltmp,vrtmp

      tmu=0.0d0
      tmv=0.0d0
      tmw=0.0d0
      qltmp=0.0d0
      vrtmp=0.0d0

#ifdef _B4B09R
      !$acc update host(qv,ql,qi,ruh,rvh,rmh,ua,va,wa,vr,rho)
#else
      !$acc parallel default(present) private(i,j,k,qtot,tem) reduction(+:tmu,tmv,tmw,qltmp,vrtmp)
#endif
      !$omp parallel do default(shared) private(i,j,k,qtot,tem)
#ifndef _B4B09R
      !$acc loop vector collapse(3) reduction(+:tmu,tmv,tmw,qltmp,vrtmp)
#endif
      do k=1,nk
        do j=1,nj
        do i=1,ni
          qtot=qv(i,j,k)+ql(i,j,k)+qi(i,j,k)
          tem=ruh(i)*rvh(j)*rmh(i,j,k)
          tmu=tmu  &
                +rho(i,j,k)*tem*(1.0+qtot)*( umove+0.5*(ua(i,j,k)+ua(i+1,j,k)) )
          tmv=tmv   &
                +rho(i,j,k)*tem*(1.0+qtot)*( vmove+0.5*(va(i,j,k)+va(i,j+1,k)) )
          tmw=tmw   &
                +rho(i,j,k)*tem*(1.0+qtot)*( 0.5*(wa(i,j,k)+wa(i,j,k+1)) )   &
                -rho(i,j,k)*tem*ql(i,j,k)*vr(i,j,k)
          qltmp = qltmp+ql(i,j,k)
          vrtmp = vrtmp+vr(i,j,k)
        enddo
        enddo
      enddo
#ifdef _B4B09R
      !!$acc update device(tmu,tmv,tmw)
#else
      !$acc end parallel
#endif

      tmu=tmu*(dx*dy*dz)
      tmv=tmv*(dx*dy*dz)
      tmw=tmw*(dx*dy*dz)
      vrtmp=vrtmp*(dx*dy*dz)
      qltmp=qltmp*(dx*dy*dz)
      

#ifdef MPI
      var=0.0d0
      call MPI_REDUCE(tmu,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,  &
                      MPI_COMM_WORLD,ierr)
      tmu=var

      var=0.0d0
      call MPI_REDUCE(tmv,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,  &
                      MPI_COMM_WORLD,ierr)
      tmv=var

      var=0.0d0
      call MPI_REDUCE(tmw,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,  &
                      MPI_COMM_WORLD,ierr)
      tmw=var

      var=0.0d0
      call MPI_REDUCE(qltmp,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,  &
                      MPI_COMM_WORLD,ierr)
      qltmp=var

      var=0.0d0
      call MPI_REDUCE(vrtmp,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,  &
                      MPI_COMM_WORLD,ierr)
      vrtmp=var

      if(myid.eq.0)then
#endif
 
      write(6,100) 'TMU   ',tmu
      write(6,100) 'TMV   ',tmv
      write(6,100) 'TMW   ',tmw
      write(6,100) 'QL   ',qltmp
      write(6,100) 'VR   ',vrtmp
100   format(2x,'stat:: ',a6,':',1x,e13.6)
 
      nstat = nstat + 1
      rstat(nstat) = tmu
      nstat = nstat + 1
      rstat(nstat) = tmv
      nstat = nstat + 1
      rstat(nstat) = tmw

#ifdef MPI
      endif
#endif
 
      if(timestats.ge.1) time_stat=time_stat+mytime()

!!$acc end data
 
      end subroutine calcmoe


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine tmf(nstat,rstat,ruh,rvh,rho,wa)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk,dx,dy, &
          myid,ierr,stat_out,timestats,mytime,time_stat
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

      integer nstat
      real, dimension(stat_out) :: rstat
      real, dimension(ib:ie) :: ruh
      real, dimension(jb:je) :: rvh
      real, dimension(ib:ie,jb:je,kb:ke) :: rho
      real, dimension(ib:ie,jb:je,kb:ke+1) :: wa

      integer i,j,k
      double precision :: tmfu,tmfd,mf,var
      double precision, dimension(nk) :: foo1,foo2
      double precision :: foo1t,foo2t

     !$acc data create(foo1,foo2)

     !$omp parallel do default(shared) private(i,j,k,mf)
#ifdef _B4B10R
     !$acc update host(wa,ruh,rvh)
#else
     !$acc parallel default(present)  private(i,j,k,mf) reduction(+:foo1t,foo2t) &
     !$acc reduction(+:tmfu,tmfd)
     !$acc loop gang 
#endif
      do k=1,nk
        foo1t = 0.0d0
        foo2t = 0.0d0
#ifndef _B4B10R
        !$acc loop vector collapse(2) reduction(+:foo1t,foo2t)
#endif
        do j=1,nj
        do i=1,ni
          mf=rho(i,j,k)*0.5*(wa(i,j,k)+wa(i,j,k+1))*ruh(i)*rvh(j)
          foo1t=foo1t+max(mf,0.0d0)
          foo2t=foo2t+min(mf,0.0d0)
        enddo
        enddo
        foo1(k) = foo1t
        foo2(k) = foo2t
      enddo

      tmfu=0.0d0
      tmfd=0.0d0
#ifndef _B4B10R
      !$acc loop vector reduction(+:tmfu,tmfd)
#endif
      do k=1,nk
        tmfu=tmfu+foo1(k)
        tmfd=tmfd+foo2(k)
      enddo
#ifdef _B4B10R
      !$acc update device(tmfu,tmfd)
#else
      !$acc end parallel
#endif

#ifdef MPI
      var=0.0d0
      call MPI_REDUCE(tmfu,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,  &
                      MPI_COMM_WORLD,ierr)
      tmfu=var
      var=0.0d0
      call MPI_REDUCE(tmfd,var,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,  &
                      MPI_COMM_WORLD,ierr)
      tmfd=var
      if(myid.eq.0)then
#endif
      tmfu=tmfu*dx*dy
      tmfd=tmfd*dx*dy

      write(6,100) 'TMFU  ',tmfu
      write(6,100) 'TMFD  ',tmfd
100   format(2x,'stat:: ',a6,':',1x,e13.6)

      nstat = nstat + 1
      rstat(nstat) = tmfu
      nstat = nstat + 1
      rstat(nstat) = tmfd

#ifdef MPI
      endif
#endif
      !$acc end data
      if(timestats.ge.1) time_stat=time_stat+mytime()


      end subroutine tmf


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine zinterp(sigma,zs,zh,dum1,dum2)
      use input, only : ib,ie,jb,je,kb,ke,ni,nj,nk
          
      use constants
      implicit none

      real, dimension(kb:ke) :: sigma
      real, dimension(ib:ie,jb:je) :: zs
      real, dimension(ib:ie,jb:je,kb:ke) :: zh,dum1,dum2

      integer i,j,k,kk,kup,kdn
      real, dimension(nk) :: zref

      do k=1,nk
!!!        zref(k)=(k*dz-0.5*dz)
        zref(k)=sigma(k)
      enddo

      do k=1,nk
      do j=1,nj
      do i=1,ni
        dum2(i,j,k)=dum1(i,j,k)
      enddo
      enddo
      enddo

      do k=1,nk
      do j=1,nj
      do i=1,ni
        if( (zref(k).lt.zh(i,j,1)).or.(zref(k).gt.zh(i,j,nk)) )then
          if( zref(k).gt.0.5*zh(i,j,1) .and. zref(k).gt.zs(i,j) )then
            ! 2nd-order extrapolation:
            dum1(i,j,k)=dum2(i,j,1)-(zh(i,j,1)-zref(k))                             &
                                   *(-3.0*dum2(i,j,1)+4.0*dum2(i,j,2)-dum2(i,j,3))  &
                                   *0.25/(zh(i,j,1)-zs(i,j))
          else
            dum1(i,j,k)=grads_undef
          endif
        elseif(zs(i,j).lt.0.1 .or. zref(k).eq.zh(i,j,1))then
          dum1(i,j,k)=dum2(i,j,k)
        else
          kup=0
          kdn=0
          do kk=1,nk
            if(zref(k).gt.zh(i,j,kk)) kdn=kk
          enddo
          kup=kdn+1
          if(kup.le.0.or.kdn.le.0.or.kup.ge.nk+1.or.kdn.ge.nk+1)then
            print *,kdn,kup
            print *,zs(i,j),zh(i,j,kdn),zref(k),zh(i,j,kup)
            print *,i,j,k
            call stopcm1
          endif
          dum1(i,j,k)=dum2(i,j,kdn)+(dum2(i,j,kup)-dum2(i,j,kdn))   &
                                   *(  zref(k  )  -zh(i,j,kdn))     &
                                   /(  zh(i,j,kup)-zh(i,j,kdn))
        endif
      enddo
      enddo
      enddo

      end subroutine zinterp


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine set_time_to_zero
      use input, only : time_sound,time_poiss,time_advs,time_phys_H2D,time_phys_D2H, &
          time_H2D,time_D2H,time_advu,time_advv,time_advw,time_buoyan,time_turb, &
          time_diffu,time_microphy,time_dbz,time_stat,time_cflq,time_bc,time_bcs, &
          time_bcs2,time_bcst,time_misc,time_misc01,time_misc02,time_misc03,time_misc04, &
          time_misc05,time_misc06,time_misc07,time_misc08,time_misc09,time_misc10, &
          time_misc11,time_misc12,time_misc13,time_misc14,time_misc15,time_misc16, &
          time_misc17,time_misc18,time_misc19,time_misc20,time_misc21,time_misc22, &
          time_misc23,time_integ,time_rdamp,time_divx,time_write,time_nvtx,time_restart, &
          time_ttend,time_cor,time_fall,time_satadj,time_sfcphys,time_parcels,time_parceli, &
          time_droplet,time_droplet_reduce,time_parceli_reduce,time_rad,time_pbl,time_swath, &
          time_pdef,time_prsrho,time_diag,time_azimavg,time_hifrq,time_ercyl,time_mpu1, &
          time_mpv1,time_mpw1,time_mpp1,time_mpu2,time_mpv2,time_mpw2,time_mpp2,time_mps1, &
          time_mps3,time_mpq1,time_mptk1,time_mptk2,time_mps2,time_mps4,time_mpq2,time_mpb, &
          time_dpc


      implicit none

      time_sound=0.0
      time_poiss=0.0
      time_advs=0.0
      time_phys_H2D=0.0
      time_phys_D2H=0.0
      time_H2D=0.0
      time_D2H=0.0
      time_advu=0.0
      time_advv=0.0
      time_advw=0.0
      time_buoyan=0.0
      time_turb=0.0
      time_diffu=0.0
      time_microphy=0.0
      time_dbz=0.0
      time_stat=0.0
      time_cflq=0.0
      time_bc=0.0
      time_bcs=0.0
      time_bcs2=0.0
      time_bcst=0.0
      time_misc=0.0
      time_misc01=0.0
      time_misc02=0.0
      time_misc03=0.0
      time_misc04=0.0
      time_misc05=0.0
      time_misc06=0.0
      time_misc07=0.0
      time_misc08=0.0
      time_misc09=0.0
      time_misc10=0.0
      time_misc11=0.0
      time_misc12=0.0
      time_misc13=0.0
      time_misc14=0.0
      time_misc15=0.0
      time_misc16=0.0
      time_misc17=0.0
      time_misc18=0.0
      time_misc19=0.0
      time_misc20=0.0
      time_misc21=0.0
      time_misc22=0.0
      time_misc23=0.0
      time_integ=0.0
      time_rdamp=0.0
      time_divx=0.0
      time_write=0.0
      time_nvtx=0.0
      time_restart=0.0
      time_ttend=0.0
      time_cor=0.0
      time_fall=0.0
      time_satadj=0.0
      time_sfcphys=0.0
      time_parcels=0.0
      time_parceli=0.0
      time_droplet=0.0
      time_droplet_reduce=0.0
      time_parceli_reduce=0.0
      time_rad=0.0
      time_pbl=0.0
      time_swath=0.0
      time_pdef=0.0
      time_prsrho=0.0
      time_diag=0.0
      time_azimavg=0.0
      time_hifrq=0.0
      time_ercyl=0.0
#ifdef MPI
      time_mpu1=0.0
      time_mpv1=0.0
      time_mpw1=0.0
      time_mpp1=0.0
      time_mpu2=0.0
      time_mpv2=0.0
      time_mpw2=0.0
      time_mpp2=0.0
      time_mps1=0.0
      time_mps3=0.0
      time_mpq1=0.0
      time_mptk1=0.0
      time_mptk2=0.0
      time_mps2=0.0
      time_mps4=0.0
      time_mpq2=0.0
      time_mpb=0.0
      time_dpc=0.0
#endif

      end subroutine set_time_to_zero


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


  END MODULE misclibs
