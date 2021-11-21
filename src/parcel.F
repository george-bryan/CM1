#define TROUBLE 0
#define PITER 1048576
#ifdef _B4B
#undef _B4B01F
#undef _B4B02F
#undef _B4B03F
#endif
  MODULE parcel_module

  implicit none

  private
  public :: droplet_driver,parcel_driver,parcel_interp,parcel_write,setup_parcel_vars,getparcelzs

  ! If we already know that a parcel stays on the same process 
  !    in the previous time step and it is still here, we start 
  !    the location search from the previous index and limit 
  !    the search range using the parameter below;
  ! This assumes that a parcel does not move too far and does
  !    not move between processes within this subroutine?
  integer, parameter :: location_offset = 3 
  integer, parameter :: neg_hundred = -100    ! define constant for initial value  

  CONTAINS

      subroutine droplet_driver(dt,xh,uh,ruh,xf,yh,vh,rvh,yf,zh,mh,rmh,zf,mf,zs,    &
                               sigma,sigmaf,znt,rho,ua,va,wa,pdata,                 &
                               th_in,qa,th0,pi0,ppa,prs,                            &
                               pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,                     &
                               nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,reqs_p,              &
                               sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,             &
                               n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s,      &
                               uten,vten,wten,thten,qten,pdata_locind)
      use input
      use constants
      use bc_module
      use comm_module
      use misclibs
      use cm1libs , only : rslf
#ifdef MPI
      use mpi
#endif
      implicit none

!-----------------------------------------------------------------------
!  This subroutine updates the parcel locations
!-----------------------------------------------------------------------

      real, intent(in) :: dt
      real, intent(in), dimension(ib:ie) :: xh,uh,ruh
      real, intent(in), dimension(ib:ie+1) :: xf
      real, intent(in), dimension(jb:je) :: yh,vh,rvh
      real, intent(in), dimension(jb:je+1) :: yf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: mh,rmh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf,mf
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(in), dimension(kb:ke) :: sigma
      real, intent(in), dimension(kb:ke+1) :: sigmaf
      real, intent(in), dimension(ib:ie,jb:je) :: znt
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: rho,prs
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: pi0,th0,th_in,ppa
      double precision, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: thten,qten
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke,numq) :: qa
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: ua,uten
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: va,vten
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: wa,wten
      real, intent(inout), dimension(nparcels,npvals) :: pdata

      real, intent(inout), dimension(jmp,kmp) :: pw1,pw2,pe1,pe2
      real, intent(inout), dimension(imp,kmp) :: ps1,ps2,pn1,pn2
      real, intent(inout), dimension(kmt) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      integer, intent(inout), dimension(rmp) :: reqs_p

      real, intent(inout), dimension(cmp,jmp,kmp)   :: sw31,sw32,se31,se32
      real, intent(inout), dimension(imp,cmp,kmp)   :: ss31,ss32,sn31,sn32
      real, intent(inout), dimension(cmp,cmp,kmt+1) :: n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2
      integer, intent(inout), dimension(rmp) :: reqs_s
      integer, intent(inout), dimension(nparcels,3) :: pdata_locind    ! x/y/z location index of each parcel

      !Need to compute the true temperature
      real, dimension(ib:ie,jb:je,kb:ke) :: ta

      integer :: n,np,i,j,k,iflag,jflag,kflag
      integer :: ix,iy,iz
      integer :: iflag_s,jflag_s,kflag_s
      real :: uval,vval,wval,rx,ry,rz,w1,w2,w3,w4,w5,w6,w7,w8,wsum
      real :: qval,tval,rhoval,prsval
      real :: rxu,ryv,rzw,rxs,rys,rzs
      real :: x3d,y3d,z3d
      integer :: nrkp
      real :: dt2,uu1,vv1,ww1
      real :: z0,rznt,var,rand
      real :: sigdot,sig1,zsp,sig3d
      real :: xrhs1,xrhs2,xrhs3,tprhs,tptmp
      real :: vtmp1,vtmp2,vtmp3
      real :: vrhs1,vrhs2,vrhs3
      real :: rprhs,rptmp,qstar,einf,lhv,qinf
      real :: k1xp1,k1xp2,k1xp3,k1vp1,k1vp2,k1vp3
      real :: k1tp,k2tp,k1rp,k2rp
      real :: taup,th_tmp,Rep,Nup,Shp,diffnorm
      real :: part_grav1,part_grav2,part_grav3,salinity
      real :: rhop,volp,rhop0,taup0,rp0,tp0,volp0
      real :: dvpdt1,dvpdt2,dvpdt3,drpdt,dtpdt,dmvdt
      real :: xv,yv,zv,dV,wtx,wty,wtz,wtt
      real :: esl

      logical, parameter :: debug = .false.

#ifdef _VERIFY_FIND_LOC
      integer :: tmp_flag
#endif

      !$acc declare present(rho,prs,qa,wa,sigma)

      !integer :: nip1

      !JMD no idea why these values hae not already been set previously
      !$acc update device(ib,jb,kb,ie,je,ke)
      !!$acc compare(wten,thten)
      !KLUDGE-JMD
      call reinit_random_seed
      !print *,'droplet_driver: point #1'
!----------------------------------------------------------------------
!  apply bottom/top boundary conditions:
!  [Note:  for u,v the array index (i,j,0) means the surface, ie z=0]
!     (for the parcel subroutines only!)
      !$acc data create(ta) copyin(ngz,ngxy)
      !print *,'droplet_driver: {ngxy,ngxy}: ',ngxy,ngz
!$omp parallel do default(shared) private(i,j)
!$acc parallel loop gang vector default(present) private(i,j)
  DO j=jb,je+1

    IF(bbc.eq.1)THEN
      ! free slip ... extrapolate:
      IF(j.le.je)THEN
      do i=ib,ie+1
        ua(i,j,0) = cgs1*ua(i,j,1)+cgs2*ua(i,j,2)+cgs3*ua(i,j,3)
      enddo
      ENDIF
      do i=ib,ie
        va(i,j,0) = cgs1*va(i,j,1)+cgs2*va(i,j,2)+cgs3*va(i,j,3)
      enddo
    ELSEIF(bbc.eq.2)THEN
      ! no slip:
      if( imove.eq.1 )then
        IF(j.le.je)THEN
        do i=ib,ie+1
          ua(i,j,0) = 0.0 - umove
        enddo
        ENDIF
        do i=ib,ie
          va(i,j,0) = 0.0 - vmove
        enddo
      else
        IF(j.le.je)THEN
        do i=ib,ie+1
          ua(i,j,0) = 0.0
        enddo
        ENDIF
        do i=ib,ie
          va(i,j,0) = 0.0
        enddo
      endif
    ELSEIF(bbc.eq.3)THEN
      ! u,v near sfc are determined below using log-layer equations
    ENDIF

!----------

    IF(tbc.eq.1)THEN
      ! free slip ... extrapolate:
      IF(j.le.je)THEN
      do i=ib,ie+1
        ua(i,j,nk+1) = cgt1*ua(i,j,nk)+cgt2*ua(i,j,nk-1)+cgt3*ua(i,j,nk-2)
      enddo
      ENDIF
      do i=ib,ie
        va(i,j,nk+1) = cgt1*va(i,j,nk)+cgt2*va(i,j,nk-1)+cgt3*va(i,j,nk-2)
      enddo
    ELSEIF(tbc.eq.2)THEN
      ! no slip:
      IF(j.le.je)THEN
      do i=ib,ie+1
        ua(i,j,nk+1) = 0.0
      enddo
      ENDIF
      do i=ib,ie
        va(i,j,nk+1) = 0.0
      enddo
    ENDIF

!----------

      IF(j.le.je)THEN
      do i=ib,ie
        wa(i,j,nk+1) = 0.0
      enddo
      ENDIF


    !NEED SCALARS
    

  ENDDO


  !!$acc compare(pi0,th_in,th0,ppa)
  !Compute temperature for interpolation
  !$acc parallel default(present) private(i,j,k)

   
  !JMD zero out the prevent PCAST errors
  !!$acc loop gang vector collapse(3)
  !do k=kb,ke
  !do j=jb,je
  !do i=ib,ie
  !   ta(i,j,k)=0.0
  !enddo
  !enddo
  !enddo

  !$acc loop gang vector collapse(3)
  do k=kb,ke
    do j=jb,je
    do i=ib,ie

      !th_tmp = (th0(i,j,k)+th_in(i,j,k))
      !ta(i,j,k) = th_tmp*(pi0(i,j,k)+ppa(i,j,k))
      ta(i,j,k) = (th0(i,j,k)+th_in(i,j,k))*(pi0(i,j,k)+ppa(i,j,k))

    enddo
    enddo
  enddo
  !$acc end parallel

  !!$acc compare(qa,ta,pi0,th_in,th0,ppa)
  !stop 'droplet_driver: after first compare'
  !DHR NEED TO CHECK IF THIS IS APPROPRIATE!
  ! GHB 210714:
    call prepcorners3_GPU( ta,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,  &
                          n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s,1)
    call prepcorners3_GPU(rho,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,  &
                          n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s,1)
    call prepcorners3_GPU(prs,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,  &
                          n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s,1)
    call prepcorners3_GPU(qa(ib,jb,kb,nqv),sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,  &
                          n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s,1)
    !$acc compare(rho)
    !!$acc compare(qa,ta,pi0,th_in,th0,ppa)
    !!$acc compare(sigma)
!  print *, 'droplet_driver: after call to prepcorners'



!----------------------------------------------------------------------
!  Loop through all parcels:  if you have it, update it's properties

    dt2 = dt/2.0
    part_grav1 = 0.0
    part_grav2 = 0.0
    part_grav3 = -0.84

#ifdef _B4B01F
    !$acc update &
    !$acc host(pdata,xf,yf,zf,zh,sigma,sigmaf, &
    !$acc      ua,va,wa,qa,ta,rho,prs,znt,qten,thten,zs)
#else
    !$acc compare(pdata)
    !JMD no idea why these values hae not already been set previously
    !$acc update device(ngxy,ngz)
    !JMD WARNING: Loop does not yet match CPU version.
    !$acc parallel &
    !$acc default(present) &
    !$acc private(i,j,k,rx,ry,rz,rxu,ryv,rxs,rys,rzs,rzw,x3d,y3d,z3d, &
    !$acc     w1,w2,w3,w4,w5,w6,w7,w8,sig3d,iflag,jflag,kflag,nrkp, &
    !$acc     uval,vval,wval,qval,tval,rhoval,prsval,sigdot,zsp,rznt,var, &
    !$acc     xrhs1,xrhs2,xrhs3,volp,rhop,rhop0,taup,taup0,diffnorm,Rep,Nup,Shp,vrhs1,vrhs2,vrhs3,qinf, &
    !$acc     lhv,qstar,rprhs,tprhs,vtmp1,vtmp2,vtmp3,k1xp1,k1xp2,k1xp3,k1vp1,k1vp2,k1vp3, &
    !$acc     dvpdt1,dvpdt2,dvpdt3,drpdt,dtpdt,dmvdt,dV,ix,iy,iz,xv,yv,zv,wtx,wty,wtz,wtt, &
    !$acc     sig1,k1tp,esl,tptmp,k1rp,rptmp,part_grav1,part_grav2,part_grav3,rp0,tp0,volp0)
    !$acc compare(ngxy,ngz)
    !$acc loop gang vector 
#endif
    nploop:  &
    DO np=1,nparcels

      x3d = pdata(np,prx)
      y3d = pdata(np,pry)
      if( .not. terrain_flag )then
        z3d = pdata(np,prz)
      else
        sig3d = pdata(np,prsig)
      endif

      iflag = neg_hundred
      jflag = neg_hundred
      kflag = 0

  ! cm1r19:  skip if we already know this processor doesnt have this parcel
  haveit1:  &
  IF( x3d.ge.xf(1) .and. x3d.le.xf(ni+1) .and.  &
      y3d.ge.yf(1) .and. y3d.le.yf(nj+1) )THEN

    IF(nx.eq.1)THEN
      iflag = 1
    ELSE
      ! cm1r19:
      call find_horizontal_location_index (pdata_locind(:,1), np, x3d, ib, ie+1, xf, ni+1, iflag) 
    ENDIF

    IF(axisymm.eq.1.or.ny.eq.1)THEN
      jflag = 1
    ELSE
      ! cm1r19:
      call find_horizontal_location_index (pdata_locind(:,2), np, y3d, jb, je+1, yf, nj+1, jflag)  
    ENDIF

  ELSE     ! re-initialize x/y/z location index array if a parcel is not on this process
    pdata_locind(np,1) = neg_hundred
    pdata_locind(np,2) = neg_hundred
    pdata_locind(np,3) = neg_hundred
  ENDIF  haveit1

#ifdef MPI
      ! check for conflict:
    IF( (iflag.ge.1.and.iflag.le.ni) .and.   &
        (jflag.ge.1.and.jflag.le.nj) )THEN
      IF( iflag.eq.ni .and. pdata(np,prx).eq.xf(iflag+1) .and. nodex.gt.1 .and.  myi.ne.nodex ) iflag = -1
      IF( jflag.eq.nj .and. pdata(np,pry).eq.yf(jflag+1) .and. nodey.gt.1 .and.  myj.ne.nodey ) jflag = -1
    ENDIF
#endif

      myparcel:  IF( (iflag.ge.1.and.iflag.le.ni) .and.   &
                     (jflag.ge.1.and.jflag.le.nj) )THEN

      kflag = 1
      if( .not. terrain_flag )then
        call find_vertical_location_index (pdata_locind(:,3), np, z3d, kb, ke+1, zf(iflag,jflag,:), kflag, .TRUE.)
      else
        call find_vertical_location_index (pdata_locind(:,3), np, sig3d, kb, ke+1, sigmaf, kflag, .TRUE.)
      endif
!      !Store these for two-way coupling
!      iflag_s=iflag
!      jflag_s=jflag
!      kflag_s=kflag
!
!      if( x3d.lt.xh(iflag) )then
!        iflag_s=iflag_s-1
!      endif
!      if( y3d.lt.yh(jflag) )then
!        jflag_s=jflag_s-1
!      endif
!      if( .not. terrain_flag )then
!        if( z3d.lt.zh(iflag,jflag,kflag) )then
!          kflag_s=kflag_s-1
!        endif
!      else
!        if( sig3d.lt.sigma(kflag) )then
!          kflag_s=kflag_s-1
!        endif
!      endif
      dropalive1:  &
      IF (pdata(np,pract).gt.0.0) THEN

      dvpdt1 = pdata(np,prvpx)
      dvpdt2 = pdata(np,prvpy)
      dvpdt3 = pdata(np,prvpz)
      dtpdt    = pdata(np,prtp)
      drpdt    = pdata(np,prrp)
      dmvdt    = rhow*4.0/3.0*pi*pdata(np,prrp)**3

      !HERE print *,'before rkloop np: ',np
      !$acc loop seq
      rkloop:  DO nrkp = 1,2

      IF( nrkp.eq.1 )THEN
        i=iflag
        j=jflag
      ELSE
        iflag = neg_hundred
        jflag = neg_hundred
        IF(nx.eq.1)THEN
          iflag = 1
        ELSE
          ! cm1r19:
          call find_horizontal_location_index (pdata_locind(:,1), np, x3d, ib, ie+1, xf, ni+2, iflag)
        ENDIF

        IF(axisymm.eq.1.or.ny.eq.1)THEN
          jflag = 1
        ELSE
          ! cm1r19:
          call find_horizontal_location_index (pdata_locind(:,2), np, y3d, jb, je+1, yf, nj+2, jflag)
        ENDIF
        i=iflag
        j=jflag
      ENDIF

        kflag = 1
        if( .not. terrain_flag )then
          call find_vertical_location_index (pdata_locind(:,3), np, z3d, kb, ke+1, zf(iflag,jflag,:), kflag, .FALSE.)
        else
          call find_vertical_location_index (pdata_locind(:,3), np, sig3d, kb, ke+1, sigmaf, kflag, .FALSE.)
        endif

!JMD-debug
!----------------------------------------------------------------------
!  Data on u points

        i=iflag
        j=jflag
        k=kflag

        if( y3d.lt.yh(j) )then
          j=j-1
        endif
        if( .not. terrain_flag )then
          if( z3d.lt.zh(iflag,jflag,k) )then
            k=k-1
          endif
          rz = ( z3d-zh(iflag,jflag,k) )/( zh(iflag,jflag,k+1)-zh(iflag,jflag,k) )
        else
          if( sig3d.lt.sigma(k) )then
            k=k-1
          endif
          rz = ( sig3d-sigma(k) )/( sigma(k+1)-sigma(k) )
        endif

        rx = ( x3d-xf(i) )/( xf(i+1)-xf(i) )
        ry = ( y3d-yh(j) )/( yh(j+1)-yh(j) )

        ! saveit:
        rxu = rx
        rys = ry
        rzs = rz


        !w1 = calcW1(rx,ry,rz)
        !w2 = calcW2(rx,ry,rz)
        !w3 = calcW3(rx,ry,rz)
        !w4 = calcW4(rx,ry,rz)
        !w5 = calcW5(rx,ry,rz)
        !w6 = calcW6(rx,ry,rz)
        !w7 = calcW7(rx,ry,rz)
        !w8 = calcW8(rx,ry,rz)

        w1 = (1.0-rx)*(1.0-ry)*(1.0-rz)
        w2 = rx*(1.0-ry)*(1.0-rz)
        w3 = (1.0-rx)*ry*(1.0-rz)
        w4 = (1.0-rx)*(1.0-ry)*rz
        w5 = rx*(1.0-ry)*rz
        w6 = (1.0-rx)*ry*rz
        w7 = rx*ry*(1.0-rz)
        w8 = rx*ry*rz
        if(np==TROUBLE) then 
          print *,'parcel: before uval: ',i,j,k
          print *,'parcel: ngxy,ngz: ',ngxy,ngz
          print *,'parcel: ni+1,nj,nk: ',nip1,nj,nk
        endif

        uval = tri_interp(nip1,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,ua)
        ! uval = ua(i  ,j  ,k  )*w1 &
        !      + ua(i+1,j  ,k  )*w2 &
        !      + ua(i  ,j+1,k  )*w3 &
        !      + ua(i  ,j  ,k+1)*w4 &
        !      + ua(i+1,j  ,k+1)*w5 &
        !      + ua(i  ,j+1,k+1)*w6 &
        !      + ua(i+1,j+1,k  )*w7 &
        !      + ua(i+1,j+1,k+1)*w8

!----------------------------------------------------------------------
!  Data on v points

        !print *,'parcel: data on v-points'
        i=iflag
        j=jflag
        k=kflag

        if( x3d.lt.xh(i) )then
          i=i-1
        endif
        if( .not. terrain_flag )then
          if( z3d.lt.zh(iflag,jflag,k) )then
            k=k-1
          endif
        else
          if( sig3d.lt.sigma(k) )then
            k=k-1
          endif
        endif

        rx = ( x3d-xh(i) )/( xh(i+1)-xh(i) )
        ry = ( y3d-yf(j) )/( yf(j+1)-yf(j) )
        rz = rzs

        ! saveit:
        rxs = rx
        ryv = ry

        !w1 = calcW1(rx,ry,rz)
        !w2 = calcW2(rx,ry,rz)
        !w3 = calcW3(rx,ry,rz)
        !w4 = calcW4(rx,ry,rz)
        !w5 = calcW5(rx,ry,rz)
        !w6 = calcW6(rx,ry,rz)
        !w7 = calcW7(rx,ry,rz)
        !!w8 = calcW8(rx,ry,rz)
        w1 = (1.0-rx)*(1.0-ry)*(1.0-rz)
        w2 = rx*(1.0-ry)*(1.0-rz)
        w3 = (1.0-rx)*ry*(1.0-rz)
        w4 = (1.0-rx)*(1.0-ry)*rz
        w5 = rx*(1.0-ry)*rz
        w6 = (1.0-rx)*ry*rz
        w7 = rx*ry*(1.0-rz)
        w8 = rx*ry*rz
        vval = tri_interp(ni,nj+1,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,va)
        ! vval = va(i  ,j  ,k  )*w1 &
        !      + va(i+1,j  ,k  )*w2 &
        !      + va(i  ,j+1,k  )*w3 &
        !      + va(i  ,j  ,k+1)*w4 &
        !      + va(i+1,j  ,k+1)*w5 &
        !      + va(i  ,j+1,k+1)*w6 &
        !      + va(i+1,j+1,k  )*w7 &
        !      + va(i+1,j+1,k+1)*w8


!----------------------------------------------------------------------
!  Data on w points

        i=iflag
        j=jflag
        k=kflag

        if( x3d.lt.xh(i) )then
          i=i-1
        endif
        if( y3d.lt.yh(j) )then
          j=j-1
        endif

        rx = rxs
        ry = rys
        if( .not. terrain_flag )then
          rz = ( z3d-zf(iflag,jflag,k) )/( zf(iflag,jflag,k+1)-zf(iflag,jflag,k) )
        else
          rz = ( sig3d-sigmaf(k) )/( sigmaf(k+1)-sigmaf(k) )
        endif

        ! saveit:
        rzw = rz

        !w1 = calcW1(rx,ry,rz)
        !w2 = calcW2(rx,ry,rz)
        !w3 = calcW3(rx,ry,rz)
        !w4 = calcW4(rx,ry,rz)
        !w5 = calcW5(rx,ry,rz)
        !w6 = calcW6(rx,ry,rz)
        !w7 = calcW7(rx,ry,rz)
        !w8 = calcW8(rx,ry,rz)
        w1 = (1.0-rx)*(1.0-ry)*(1.0-rz)
        w2 = rx*(1.0-ry)*(1.0-rz)
        w3 = (1.0-rx)*ry*(1.0-rz)
        w4 = (1.0-rx)*(1.0-ry)*rz
        w5 = rx*(1.0-ry)*rz
        w6 = (1.0-rx)*ry*rz
        w7 = rx*ry*(1.0-rz)
        w8 = rx*ry*rz
        wval = tri_interp(ni,nj,nkp1,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,wa)
        ! wval = wa(i  ,j  ,k  )*w1 &
        !      + wa(i+1,j  ,k  )*w2 &
        !      + wa(i  ,j+1,k  )*w3 &
        !      + wa(i  ,j  ,k+1)*w4 &
        !      + wa(i+1,j  ,k+1)*w5 &
        !      + wa(i  ,j+1,k+1)*w6 &
        !      + wa(i+1,j+1,k  )*w7 &
        !      + wa(i+1,j+1,k+1)*w8

        if( terrain_flag )then
          sigdot = tri_interp(ni,nj,nkp1,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,wa)
          ! sigdot = wa(i  ,j  ,k  )*w1 &
          !    + wa(i+1,j  ,k  )*w2 &
          !    + wa(i  ,j+1,k  )*w3 &
          !    + wa(i  ,j  ,k+1)*w4 &
          !    + wa(i+1,j  ,k+1)*w5 &
          !    + wa(i  ,j+1,k+1)*w6 &
          !    + wa(i+1,j+1,k  )*w7 &
          !    + wa(i+1,j+1,k+1)*w8
          zsp = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 0, 0, 0, 0,zs)
          z3d = zsp + sig3d*((zt-zsp)*rzt)
        endif


!----------------------------------------------------------------------
!  Data on scalar points

        i=iflag
        j=jflag
        k=kflag

        if( x3d.lt.xh(i) )then
          i=i-1
        endif
        if( y3d.lt.yh(j) )then
          j=j-1
        endif
        if( .not. terrain_flag )then
          if( z3d.lt.zh(iflag,jflag,k) )then
            k=k-1
          endif
        else
          if( z3d.lt.sigma(k) )then
            k=k-1
          endif
        endif


        rx = rxs
        ry = rys
        rz = rzs

        !w1 = calcW1(rx,ry,rz)
        !w2 = calcW2(rx,ry,rz)
        !w3 = calcW3(rx,ry,rz)
        !w4 = calcW4(rx,ry,rz)
        !w5 = calcW5(rx,ry,rz)
        !w6 = calcW6(rx,ry,rz)
        !w7 = calcW7(rx,ry,rz)
        !w8 = calcW8(rx,ry,rz)
        w1 = (1.0-rx)*(1.0-ry)*(1.0-rz)
        w2 = rx*(1.0-ry)*(1.0-rz)
        w3 = (1.0-rx)*ry*(1.0-rz)
        w4 = (1.0-rx)*(1.0-ry)*rz
        w5 = rx*(1.0-ry)*rz
        w6 = (1.0-rx)*ry*rz
        w7 = rx*ry*(1.0-rz)
        w8 = rx*ry*rz

      if(imoist.eq.1)then
        !!print *,'parcel: before qval'
        qval = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,qa(ib,jb,kb,nqv))
        ! qval = qa(i  ,j  ,k  ,nqv)*w1 &
        !      + qa(i+1,j  ,k  ,nqv)*w2 &
        !      + qa(i  ,j+1,k  ,nqv)*w3 &
        !      + qa(i  ,j  ,k+1,nqv)*w4 &
        !      + qa(i+1,j  ,k+1,nqv)*w5 &
        !      + qa(i  ,j+1,k+1,nqv)*w6 &
        !      + qa(i+1,j+1,k  ,nqv)*w7 &
        !      + qa(i+1,j+1,k+1,nqv)*w8
      endif
      !if(np==100) then 
      !  print *,'parcel: ngxy,ngz: ',ngxy,ngz
      !  print *,'parcel: before tval: ',i,j,k
      !endif
      tval   = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,ta)
      ! tval   = ta(i  ,j  ,k  )*w1 &
      !        + ta(i+1,j  ,k  )*w2 &
      !        + ta(i  ,j+1,k  )*w3 &
      !        + ta(i  ,j  ,k+1)*w4 &
      !        + ta(i+1,j  ,k+1)*w5 &
      !        + ta(i  ,j+1,k+1)*w6 &
      !        + ta(i+1,j+1,k  )*w7 &
      !        + ta(i+1,j+1,k+1)*w8
      if(nrkp==1 .and. np==TROUBLE) then 
        print *,'point #2: i,j,k: ',i,j,k
        print *,'point #2: ni,nj,nk: ',ni,nj,nk
        print *,'point #2: ib,jb,kb: ',ib,jb,kb
        print *,'point #2: ie,je,ke: ',ie,je,ke
        print *,'point #2: nqv: ',nqv
        print *,'point #2: rx,ry,rz: ',rx,ry,rz
        print *,'point #2: after tval: ',tval
        print *,'point #2: after qval: ',tval
      endif
      !if(np==100) then 
      !  print *,'parcel: before rhoval: ',i,j,k
      !endif
      rhoval = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,rho)
      ! rhoval = rho(i  ,j  ,k  )*w1 &
      !        + rho(i+1,j  ,k  )*w2 &
      !        + rho(i  ,j+1,k  )*w3 &
      !        + rho(i  ,j  ,k+1)*w4 &
      !        + rho(i+1,j  ,k+1)*w5 &
      !        + rho(i  ,j+1,k+1)*w6 &
      !        + rho(i+1,j+1,k  )*w7 &
      !        + rho(i+1,j+1,k+1)*w8
      !if(np==100) then 
      !  print *,'parcel: before prsval: ',i,j,k
      !endif
      prsval = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,prs)
      ! prsval = prs(i  ,j  ,k  )*w1 &
      !        + prs(i+1,j  ,k  )*w2 &
      !        + prs(i  ,j+1,k  )*w3 &
      !        + prs(i  ,j  ,k+1)*w4 &
      !        + prs(i+1,j  ,k+1)*w5 &
      !        + prs(i  ,j+1,k+1)*w6 &
      !        + prs(i+1,j+1,k  )*w7 &
      !        + prs(i+1,j+1,k+1)*w8


!----------------------------------------------------------------------
!  uv for parcels below lowest model level:

      IF( bbc.eq.3 )THEN
        ! semi-slip lower boundary condition:
        if( z3d.lt.zh(1,1,1) )then
          ! re-calculate velocities if parcel is below lowest model level:
          !------
          ! u at lowest model level:
          i=iflag
          j=jflag
          if( y3d.lt.yh(j) )then
            j=j-1
          endif
          uval = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 1, 0, 1, 0,ua(ib,jb,1))
          !------
          ! v at lowest model level:
          i=iflag
          j=jflag
          if( x3d.lt.xh(i) )then
            i=i-1
          endif
          vval = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 0, 1, 0, 1,va(ib,jb,1))
          !------
          ! z0:
          i=iflag
          j=jflag
          if( x3d.lt.xh(i) )then
            i=i-1
          endif
          if( y3d.lt.yh(j) )then
            j=j-1
          endif
          z0 = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 0, 0, 0, 0,znt)
          !------
          ! get u,v from (neutral) log-layer equation:
          rznt = 1.0/z0
          var = alog((z3d+z0)*rznt)/alog((zh(1,1,1)+z0)*rznt)
          if( imove.eq.1 )then
            uval = (uval+umove)*var - umove
            vval = (vval+vmove)*var - vmove
          else
            uval = uval*var
            vval = vval*var
          endif
        endif
      ENDIF

      if(np==TROUBLE) then 
        print *,'before rslf: tval: ',tval 
      endif
      qinf = rslf(prsval,tval)
      lhv=lv1-lv2*tval

!-----------------------------------------------------
!  Update droplet position, velocity, etc.
!-----------------------------------------------------
      if(np==TROUBLE) then 
        !print *,'before: pdata(np,prrp): ',pdata(np,prrp)
        !print *,'before: pdata(np,prms): ',pdata(np,prms)
        !print *,'before: pdata(np,prtp): ',pdata(np,prtp)
        print *,'after rslf: tval: ',tval 
        !print *,'prsval: ',prsval
        !print *,'lv1: ',lv1
        !print *,'lv2: ',lv2
        !print *,'ru: ',ru
        !print *,'ms: ',ms
        !print *,'mw: ',mw
        !print *,'ion: ',ion
        !print *,'os: ',os
        !print *,'surften: ',surften
      endif


      ! RK2 scheme:
      IF(nrkp.eq.1)THEN

        xrhs1 = pdata(np,prvpx)
        xrhs2 = pdata(np,prvpy)
        xrhs3 = pdata(np,prvpz)
        volp = 4.0/3.0*pi*pdata(np,prrp)**3
        rhop = (pdata(np,prms)+volp*rhow)/volp  !Density including the solute mass
        taup = rhop*(2.0*pdata(np,prrp))**2/18.0/rhoval/viscosity

!PREVIOUS

        rhop0 = rhop
        taup0 = taup
        rp0 = pdata(np,prrp)
        tp0 = pdata(np,prtp)
        volp0 = 4.0/3.0*pi*rp0**3

        volp = 4.0/3.0*pi*pdata(np,prrp)**3
        rhop = (pdata(np,prms)+volp*rhow)/volp  !Density including the solute mass
        taup = rhop*(2.0*pdata(np,prrp))**2/18.0/rhoval/viscosity

        diffnorm = sqrt( (uval-pdata(np,prvpx))**2+ &
                         (vval-pdata(np,prvpy))**2+ &
                         (wval-pdata(np,prvpz))**2)

        Rep = 2.0*pdata(np,prrp)*diffnorm/viscosity
        Nup = 2.0 + 0.6*sqrt(Rep)*pr_num**(1.0/3.0)
        Shp = 2.0 + 0.6*sqrt(Rep)*sc_num**(1.0/3.0)

        vrhs1 = 1.0/taup*(uval - pdata(np,prvpx)) + part_grav1
        vrhs2 = 1.0/taup*(vval - pdata(np,prvpy)) + part_grav2
        vrhs3 = 1.0/taup*(wval - pdata(np,prvpz)) + part_grav3

        ! einf = qinf*rhoval/mw*ru*tval

        qstar = qinf*exp(                                                   &
                     mw*lhv/ru*(1.0/tval - 1.0/pdata(np,prtp))              &
                    +2.0*ms*surften/(ru*rhow*pdata(np,prrp)*pdata(np,prtp)) &
                    -ion*os*pdata(np,prms)*mw/ms/(volp*rhop-pdata(np,prms)) &
                        )

        rprhs = Shp/9.0/sc_num*rhop/rhow*pdata(np,prrp)/taup*(qval-qstar)

        tprhs = Nup/3.0/pr_num*(cp/cpl)*(rhop/rhow)/taup*(tval-pdata(np,prtp))+ &
                3.0*lhv/cpl/pdata(np,prrp)*rprhs


        IF(nx.eq.1)THEN
          x3d=0.0
          vtmp1 = 0.0
        ELSE
          k1xp1 = dt*xrhs1
          x3d=pdata(np,prx)+k1xp1

          k1vp1 = dt*vrhs1
          vtmp1 = pdata(np,prvpx) + k1vp1

        ENDIF
        IF(axisymm.eq.1.or.ny.eq.1)THEN
          y3d=0.0
          vtmp2 = 0.0
        ELSE
          k1xp2 = dt*xrhs2
          y3d=pdata(np,pry)+k1xp2

          k1vp2 = dt*vrhs2
          vtmp2 = pdata(np,prvpy) + k1vp2
        ENDIF
        if( terrain_flag )then
          sig3d = pdata(np,prsig) + dt*sigdot
          sig1 = sigdot
        else
        
          k1xp3 = dt*xrhs3
          z3d = pdata(np,prz)+k1xp3

          ! GHB, 210713: check for top/bot bc on 1st RK step:
          !Wall BCs:
          if ( z3d .gt. maxz ) then
             z3d = maxz - (z3d - maxz)
          elseif ( z3d .lt. 0.0 ) then
             z3d = 1.0e-6  !Just temporary 
             pdata(np,pract) = -1.0  !Signal this as no longer alive
          endif

          k1vp3 = dt*vrhs3
          vtmp3 = pdata(np,prvpz) + k1vp3
        endif

        k1tp = dt*tprhs
        tptmp = pdata(np,prtp) + k1tp

        k1rp = dt*rprhs
        rptmp = pdata(np,prrp) + k1rp

        if(np==TROUBLE) then 
          print *,'dd: qstar: ',qstar
          print *,'dd: qinf: ',qinf
          print *,'dd: cp: ',cp
          print *,'dd: cpl: ',cpl
          print *,'dd: Nup: ',Nup
          print *,'dd: pr_num: ',pr_num
          print *,'dd: sc_num: ',sc_num
          !print *,'qval: ',qstar
          print *,'dd: rhop: ',rhop
          print *,'dd: rhow: ',rhow
          print *,'dd: taup: ',taup
          !print *,'tval: ',tval
          print *,'dd: tptmp: ',tptmp
          print *,'dd: lhv: ',lhv
          print *,'dd: rptmp: ',rptmp
          print *,'dd: rprhs: ',rprhs
          print *,'dd: tprhs: ',tprhs
        endif


      ELSE

        xrhs1 = vtmp1
        xrhs2 = vtmp2
        xrhs3 = vtmp3

        volp = 4.0/3.0*pi*rptmp**3
        rhop = (pdata(np,prms)+volp*rhow)/volp  !Density including the solute mass
        taup = rhop*(2.0*rptmp)**2/18.0/rhoval/viscosity

        diffnorm = sqrt((uval-vtmp1)**2+ &
                        (vval-vtmp2)**2+ &
                        (wval-vtmp3)**2)

        Rep = 2.0*rptmp*diffnorm/viscosity
        Nup = 2.0 + 0.6*sqrt(Rep)*pr_num**(1.0/3.0)
        Shp = 2.0 + 0.6*sqrt(Rep)*sc_num**(1.0/3.0)

        vrhs1 = 1.0/taup*(uval - vtmp1) + part_grav1
        vrhs2 = 1.0/taup*(vval - vtmp2) + part_grav2
        vrhs3 = 1.0/taup*(wval - vtmp3) + part_grav3

        qstar = qinf*exp(                                                   &
                     mw*lhv/ru*(1.0/tval - 1.0/tptmp)                       &
                    +2.0*ms*surften/(ru*rhow*rptmp*tptmp)                   &
                    -ion*os*pdata(np,prms)*mw/ms/(volp*rhop-pdata(np,prms)) &
                        )

        rprhs = Shp/9.0/sc_num*rhop/rhow*rptmp/taup*(qval-qstar)

        tprhs = Nup/3.0/pr_num*(cp/cpl)*(rhop/rhow)/taup*(tval-tptmp) + &
                3.0*lhv/cpl/rptmp*rprhs


        IF(nx.eq.1)THEN
          x3d=0.0
          pdata(np,prvpx) = 0.0
        ELSE
          !k2xp(1) = dt*xrhs(1)
          x3d=pdata(np,prx)+0.5*(k1xp1+dt*xrhs1)

          !k2vp(1) = dt*vrhs1
          pdata(np,prvpx) = pdata(np,prvpx) + 0.5*(k1vp1+dt*vrhs1)

        ENDIF
        IF(axisymm.eq.1.or.ny.eq.1)THEN
          y3d=0.0
          pdata(np,prvpy) = 0.0
        ELSE
          !k2xp(2) = dt*xrhs(2)
          y3d=pdata(np,pry)+0.5*(k1xp2+dt*xrhs2)

          !k2vp(2) = dt*vrhs2
          pdata(np,prvpy) = pdata(np,prvpy) + 0.5*(k1vp2+dt*vrhs2)
        ENDIF

        if( terrain_flag )then
          sig3d = pdata(np,prsig) + dt2*(sig1+sigdot)
          IF( sig3d.lt.0.0 )THEN
            print *,'  parcel is below surface:  np,x3d,y3d,sig3d = ',np,x3d,y3d,sig3d
            sig3d=1.0e-6
          ENDIF
          sig3d=min(sig3d,maxz)
        else
          !k2xp(3) = dt*xrhs(3)
          !JMD-BUG z3d = pdata(np,prz)+0.5*(k1xp(3)+k1xp(3))
          z3d = pdata(np,prz)+0.5*(k1xp3+dt*xrhs3)

          !k2vp(3) = dt*vrhs3
          pdata(np,prvpz) = pdata(np,prvpz) + 0.5*(k1vp3+dt*vrhs3)

          !Wall BCs:
          if ( z3d .gt. maxz ) then
             !Elastic collisions
             z3d = maxz - (z3d - maxz)
             pdata(np,prvpz) = -pdata(np,prvpz)
          elseif ( z3d .lt. 0.0 ) then
             !Elastic collisions:
             ! z3d = -z3d
             ! pdata(np,prvpz) = -pdata(np,prvpz)
             !Remove it
             z3d = 1.0e-6  !Temporary
             pdata(np,pract) = -1.0  !Signal this as no longer alive
          endif
        endif
        if(np==TROUBLE) then 
          print *,'dd: k1tp: ',k1tp
          print *,'dd: tprhs: ',tprhs
        endif
        pdata(np,prtp) = pdata(np,prtp) + 0.5*(k1tp+dt*tprhs)
        pdata(np,prrp) = pdata(np,prrp) + 0.5*(k1rp+dt*rprhs)

      ENDIF

      ENDDO  rkloop
      !HERE print *,'after rkloop np: ',np
      ENDIF dropalive1
      

    dropalive2:  &
    IF (pdata(np,pract).gt.0.0) THEN


!-----------------------------------------------------
!  Now perform the two-way coupling based on the changes in droplet momentum,
!  temp, and size
!-----------------------------------------------------

      ! GHB, 210714:
      ! get i,j,k for final parcel location:

        iflag = neg_hundred
        jflag = neg_hundred
        IF(nx.eq.1)THEN
          iflag = 1
        ELSE
          ! cm1r19:
          call find_horizontal_location_index (pdata_locind(:,1), np, x3d, ib, ie+1, xf, ni+2, iflag)
        ENDIF
        IF(axisymm.eq.1.or.ny.eq.1)THEN
          jflag = 1
        ELSE
          ! cm1r19:
          call find_horizontal_location_index (pdata_locind(:,2), np, y3d, jb, je+1, yf, nj+2, jflag)
          if(mod(np,PITER)==0) then
             print *,'after dowhile loop: ',jflag
          endif
          i=iflag
          j=jflag
        ENDIF

        !HERE print *,'droplet_driver: point #3'
#if 0
        IF(debug)THEN
        if( i.lt.0 .or. i.gt.(ni+1) .or. j.lt.0 .or. j.gt.(nj+1) )then
          print *,'  myid,i,j = ',myid,i,j
          print *,'  x,x1     = ',x3d,pdata(np,prx)
          print *,'  y,y1     = ',y3d,pdata(np,pry)
          do i=0,ni+1
            print *,i,abs(xh(i)-x3d),0.5*dx*ruh(i)
          enddo
          do j=0,nj+1
            print *,j,abs(yh(j)-y3d),0.5*dy*rvh(j)
          enddo
          print *,'  16665 '
          call stopcm1
        endif
        ENDIF
#endif

        !print *,'droplet_driver: point #4'
        kflag = 1
! JS - jflag could be negative somehow, which will break
!           the find_vertical_location_index subroutine;
!      use the original interface but with the pdata_locind(:,3) here
#ifdef _VERIFY_FIND_LOC
        tmp_flag = kflag
#endif
        kflag = max (kflag, pdata_locind(np,3) - location_offset - 1)
        if( .not. terrain_flag )then
          if ( z3d.lt.zf(iflag,jflag,kflag) ) then
            kflag = 1
          end if
          do while( z3d.gt.zf(iflag,jflag,kflag+1) )
            kflag = kflag+1
          enddo
#ifdef _VERIFY_FIND_LOC
          do while( z3d.gt.zf(iflag,jflag,tmp_flag+1) )
            tmp_flag = tmp_flag+1
          enddo
          if ( kflag .ne. tmp_flag ) then
            print *, "original search scheme finds z index = ", tmp_flag, ", new search scheme finds z index = ", kflag
            stop "Failed verification test: z index is not the same..."
          else
            print *, "Pass the verification test for z index..."
          end if
#endif
        else
          if ( sig3d.lt.sigmaf(kflag) ) then
            kflag = 1
          end if
          do while( sig3d.gt.sigmaf(kflag+1) )
            kflag = kflag+1
          enddo
#ifdef _VERIFY_FIND_LOC
          do while( sig3d.gt.sigmaf(tmp_flag+1) )
            tmp_flag = tmp_flag+1
          enddo
          if ( kflag .ne. tmp_flag ) then
            print *, "original search scheme finds z index = ", tmp_flag, ", new search scheme finds z index = ", kflag
            stop "Failed verification test: z index is not the same..."
          else
            print *, "Pass the verification test for z index..."
          end if
#endif
        endif

#if 0
        IF(debug)THEN
        if( kflag.le.0 .or. kflag.ge.(nk+1) )then
          print *,myid,nrkp
          print *,iflag,jflag,kflag
          print *,pdata(np,prx),pdata(np,pry),pdata(np,prz)
          print *,x3d,y3d,z3d
          print *,uval,vval,wval
          print *,zf(iflag,jflag,kflag),z3d,zf(iflag,jflag,kflag+1)
          print *,'  16667 '
          call stopcm1
        endif
        ENDIF
#endif

        !print *,'droplet_driver: point #5'
      !--------
      !PORTME
      !Recall that the "old" value is stored here
      dvpdt1 = (pdata(np,prvpx)-dvpdt1)/dt   !Remember to deal with gravity later!
      dvpdt2 = (pdata(np,prvpy)-dvpdt2)/dt
      dvpdt3 = (pdata(np,prvpz)-dvpdt3)/dt

      drpdt = (pdata(np,prrp)-drpdt)/dt
      !Only the sensible component of energy 
      !dtpdt = (pdata(np,prtp)-dtpdt)/dt - 3.0*lhv/cpl/rp0*drpdt
      dtpdt = Nup/3.0/pr_num*(cp/cpl)*(rhop0/rhow)/taup0*(tval-pdata(np,prtp))
      if(np==TROUBLE) then  
        print *,'dd: rhow: ',rhow
        print *,'dd: before dmvdt: ',dmvdt
        print *,'dd: pdata(np,prrp): ',pdata(np,prrp)
      endif
      dmvdt = (rhow*4.0/3.0*pi*pdata(np,prrp)**3-dmvdt)/dt

      !At this point, iflag,jflag,kflag is the node beneath droplet

      !Volume where the droplet finds itself
      dV = (xh(iflag+1)-xh(iflag))*   &
           (yh(jflag+1)-yh(jflag))*   &
           (zh(iflag,jflag,kflag+1)-zh(iflag,jflag,kflag))
       ! print *,'droplet_driver: point #6'

      if(mod(np,PITER)==0) then 
          print *,'{i,j,k}flag: ',iflag,jflag,kflag 
      endif
      !Loop over all nodes surrounding droplet
      !!$acc loop seq
      do i=0,1
      !!$acc loop seq
      do j=0,1
      !!$acc loop seq
      do k=0,1

        ix = iflag+i
        iy = jflag+j
        iz = kflag+k
        !print *,'point #1: ix,iy,iz: ',ix,iy,iz

        xv = xh(ix)
        yv = yh(iy)
        zv = zh(iflag,jflag,iz)

        wtx = 1.0 - abs(x3d-xv)/(xh(iflag+1)-xh(iflag))
        wty = 1.0 - abs(y3d-yv)/(yh(jflag+1)-yh(jflag))
        wtz = 1.0 - abs(z3d-zv)/(zh(iflag,jflag,kflag+1)-zh(iflag,jflag,kflag))
        wtt = wtx*wty*wtz

        !!$acc atomic write
        qten(ix,iy,iz) = qten(ix,iy,iz) - &
            dmvdt/rhoval/dV*wtt*pdata(np,prmult)

        !Ignoring the enthalpy contained in the vapor -- this has been shown to
        !be negligible
        !!$acc atomic write
        !thten(ix,iy,iz) = thten(ix,iy,iz) - &
        !    (rhow/rhoval)*(cpl/cp)*dtpdt/dV*wtt*pdata(np,prmult)
        thten(ix,iy,iz) = thten(ix,iy,iz) - &
            dtpdt*6.0*(rhow/rhop0)*(cpl/cp)*taup0*pi*rp0*viscosity*wtt/dV

#if 0
        if((np==TROUBLE)) then 
          print *,'dd2: {ix,iy,iz}: ',ix,iy,iz
          print *,'dd2: 2-way ',ix,iy,iz,rhow,rhoval,dtpdt,dv,wtt,pdata(np,prmult)
          print *,'dd2: 2-way wtx: ',ix,iy,iz,wtx
          print *,'dd2: 2-way wty: ',ix,iy,iz,wty
          print *,'dd2: 2-way wtz: ',ix,iy,iz,wtz
          print *,'dd2: thten: ',np, thten(ix,iy,iz)
        endif
        if((np==TROUBLE) .and. (i==0) .and. (j==0) .and. (k==0)) then 
            print *,'dd: wtt: ',wtt
            print *,'dd: dmvdt: ',dmvdt
        endif
#endif
!        if(np.eq.1) print *,'  thten,qvten    = ',thten(ix,iy,iz),qten(ix,iy,iz)
!        if(np.eq.1) print *,'    dmvdt,rhoval = ',dmvdt,rhoval
!        if(np.eq.1) print *,'    dv,wtt       = ',dv,wtt
!        if(np.eq.1) print *,'    rhow,cpl,cp  = ',rhow,cpl,cp
!        if(np.eq.1) print *,'    dtpdt        = ',dtpdt

      enddo
      enddo
      enddo
      !HERE print *,'after interpolation loops ',np

    ENDIF dropalive2
    !HERE print *,'after dropalive2' 


!-----------------------------------------------------
!  Account for boundary conditions (if necessary)
!-----------------------------------------------------

      !Now that the particles have been updated, do something with those which
      !have fallen out the bottom:
      if (pdata(np,pract).lt.0.0) then
            !Replace with a new droplet at random location
#if 0
              !JMD-KLUDGE 
              call random_number(rand)
              x3d = rand*maxx
              call random_number(rand)
              y3d = rand*maxy
              call random_number(rand)
              z3d = rand*maxz
#endif
              pdata(np,prvpx) = 0.0
              pdata(np,prvpy) = 0.0
              pdata(np,prvpz) = 0.0
              !Droplet size (radius)
              pdata(np,prrp) = 20.0e-6
  
              !Droplet solute mass
              salinity = 0.034
              pdata(np,prms) = salinity*rhow*4.0/3.0*pi*pdata(np,prrp)**3
              !Droplet temperature
              pdata(np,prtp) = 302.0
              !Droplet multiplicity
              pdata(np,prmult) = 1.0
              !Make it alive again
              pdata(np,pract) = 1.0
              
      end if

        ! New for cm1r17:  if parcel exits domain,
        ! just assume periodic lateral boundary conditions
        ! (no matter what actual settings are for wbc,ebc,sbc,nbc)

        if(x3d.lt.minx)then
          x3d=x3d+(maxx-minx)
          pdata_locind(np,1) = neg_hundred
        endif
        if(x3d.gt.maxx)then
          x3d=x3d-(maxx-minx)
          pdata_locind(np,1) = neg_hundred
        endif

        if( (y3d.gt.maxy).and.(axisymm.ne.1).and.(ny.ne.1) )then
          y3d=y3d-(maxy-miny)
          pdata_locind(np,2) = neg_hundred
        endif
        if( (y3d.lt.miny).and.(axisymm.ne.1).and.(ny.ne.1) )then
          y3d=y3d+(maxy-miny)
          pdata_locind(np,2) = neg_hundred
        endif

        pdata(np,prx)=x3d
        pdata(np,pry)=y3d
        if( .not. terrain_flag )then
          pdata(np,prz)=z3d
          pdata_locind(np,3) = neg_hundred
        else
          pdata(np,prsig)=sig3d
          pdata_locind(np,3) = neg_hundred
        endif

#ifdef MPI

      ELSE

        ! set to really small number (so we can use the allreduce command below)
        pdata(np,prx) = -1.0e30
        pdata(np,pry) = -1.0e30
        if( .not. terrain_flag )then
          pdata(np,prz) = -1.0e30
        else
          pdata(np,prsig) = -1.0e30
        endif
        pdata(np,prvpx) = -1.0e30
        pdata(np,prvpy) = -1.0e30
        pdata(np,prvpz) = -1.0e30
        pdata(np,prrp) = -1.0e30
        pdata(np,prtp) = -1.0e30
        pdata(np,prms) = -1.0e30
        pdata(np,prmult) = -1.0e30
        pdata(np,pract) = -1.0e30
#endif

      ENDIF  myparcel

    ENDDO  nploop
    !print *,'droplet_driver: after the nploop'
#ifdef _B4B01F
    !$acc update device(pdata,qten,thten)
#else
    !$acc end parallel
#endif
    if(timestats.ge.1) time_droplet=time_droplet+mytime()
!----------------------------------------------------------------------
!  communicate data  (for MPI runs)

     !$acc compare(pdata)
#ifdef MPI
      !print *,'droplet_driver: before call to MPI_ALLREDUCE'
      !$acc update host(pdata)
      !!$acc host_data use_device(pdata)
      if( .not. terrain_flag )then
        !call MPI_ALLREDUCE(MPI_IN_PLACE,pdata(1,1),npvars*nparcels,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,pdata,npvars*nparcels,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,ierr)
      else
        !JMD not clear why two MPI_ALLREDUCE are being performed here
        ! call MPI_ALLREDUCE(MPI_IN_PLACE,pdata(1,1),(npvars-1)*nparcels,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,ierr)
        ! call MPI_ALLREDUCE(MPI_IN_PLACE,pdata(1,prsig),nparcels,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,pdata,nparcels,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,ierr)
      endif
      !$acc update device(pdata)
      !$acc compare(pdata)
      !stop 'droplet_driver: after call to MPI_ALLREDUCE'
      if(timestats.ge.1) time_droplet_reduce=time_droplet_reduce+mytime()
     
#endif

!----------------------------------------------------------------------
!  get height ASL:

      if( terrain_flag )then
            call getparcelzs(xh,uh,ruh,xf,yh,vh,rvh,yf,zs,pdata)
            DO np=1,nparcels
              ! get z from sigma:
              ! (see Section 3 of "The governing equations for CM1", 
              !  http://www2.mmm.ucar.edu/people/bryan/cm1/cm1_equations.pdf)
              pdata(np,prz) = pdata(np,przs) + pdata(np,prsig)*((zt-pdata(np,przs))*rzt)
            ENDDO
      endif
    if(timestats.ge.1) time_droplet=time_droplet+mytime()

!
!----------------------------------------------------------------------
      !$acc end data
      !!$acc compare(pdata(1:50,1:npvals))
      !!$acc compare(pdata)
      !!$acc compare(qten,thten)
      !stop 'droplet_driver: at the end subroutine'

      end subroutine droplet_driver

      subroutine parcel_driver(dt,xh,uh,ruh,xf,yh,vh,rvh,yf,zh,mh,rmh,zf,mf,zs,    &
                               sigma,sigmaf,znt,rho,ua,va,wa,pdata)
      use input
      use constants
      use bc_module
      use comm_module
#ifdef MPI
      use mpi
#endif
      implicit none

!-----------------------------------------------------------------------
!  This subroutine updates the parcel locations
!-----------------------------------------------------------------------

      real, intent(in) :: dt
      real, intent(in), dimension(ib:ie) :: xh,uh,ruh
      real, intent(in), dimension(ib:ie+1) :: xf
      real, intent(in), dimension(jb:je) :: yh,vh,rvh
      real, intent(in), dimension(jb:je+1) :: yf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: mh,rmh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf,mf
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(in), dimension(kb:ke) :: sigma
      real, intent(in), dimension(kb:ke+1) :: sigmaf
      real, intent(in), dimension(ib:ie,jb:je) :: znt
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rho
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, intent(inout), dimension(nparcels,npvals) :: pdata

      integer :: n,np,i,j,k,iflag,jflag,kflag
      real :: uval,vval,wval,rx,ry,rz,w1,w2,w3,w4,w5,w6,w7,w8,wsum
      real :: rxu,ryv,rzw,rxs,rys,rzs
      real :: x3d,y3d,z3d
      integer :: nrkp
      real :: dt2,uu1,vv1,ww1
      real :: z0,rznt,var
      real :: sigdot,sig1,zsp,sig3d

      logical, parameter :: debug = .false.

!----------------------------------------------------------------------
!  apply bottom/top boundary conditions:
!  [Note:  for u,v the array index (i,j,0) means the surface, ie z=0]
!     (for the parcel subroutines only!)

!$omp parallel do default(shared)  &
!$omp private(i,j)
  DO j=jb,je+1

    IF(bbc.eq.1)THEN
      ! free slip ... extrapolate:
      IF(j.le.je)THEN
      do i=ib,ie+1
        ua(i,j,0) = cgs1*ua(i,j,1)+cgs2*ua(i,j,2)+cgs3*ua(i,j,3)
      enddo
      ENDIF
      do i=ib,ie
        va(i,j,0) = cgs1*va(i,j,1)+cgs2*va(i,j,2)+cgs3*va(i,j,3)
      enddo
    ELSEIF(bbc.eq.2)THEN
      ! no slip:
      if( imove.eq.1 )then
        IF(j.le.je)THEN
        do i=ib,ie+1
          ua(i,j,0) = 0.0 - umove
        enddo
        ENDIF
        do i=ib,ie
          va(i,j,0) = 0.0 - vmove
        enddo
      else
        IF(j.le.je)THEN
        do i=ib,ie+1
          ua(i,j,0) = 0.0
        enddo
        ENDIF
        do i=ib,ie
          va(i,j,0) = 0.0
        enddo
      endif
    ELSEIF(bbc.eq.3)THEN
      ! u,v near sfc are determined below using log-layer equations
    ENDIF

!----------

    IF(tbc.eq.1)THEN
      ! free slip ... extrapolate:
      IF(j.le.je)THEN
      do i=ib,ie+1
        ua(i,j,nk+1) = cgt1*ua(i,j,nk)+cgt2*ua(i,j,nk-1)+cgt3*ua(i,j,nk-2)
      enddo
      ENDIF
      do i=ib,ie
        va(i,j,nk+1) = cgt1*va(i,j,nk)+cgt2*va(i,j,nk-1)+cgt3*va(i,j,nk-2)
      enddo
    ELSEIF(tbc.eq.2)THEN
      ! no slip:
      IF(j.le.je)THEN
      do i=ib,ie+1
        ua(i,j,nk+1) = 0.0
      enddo
      ENDIF
      do i=ib,ie
        va(i,j,nk+1) = 0.0
      enddo
    ENDIF

!----------

      IF(j.le.je)THEN
      do i=ib,ie
        wa(i,j,nk+1) = 0.0
      enddo
      ENDIF

  ENDDO

!----------------------------------------------------------------------
!  Loop through all parcels:  if you have it, update it's location:

    dt2 = dt/2.0

    !$acc parallel loop gang vector default(present) 
    nploop:  &
    DO np=1,nparcels

      x3d = pdata(np,prx)
      y3d = pdata(np,pry)
      if( .not. terrain_flag )then
        z3d = pdata(np,prz)
      else
        sig3d = pdata(np,prsig)
      endif

      iflag = neg_hundred
      jflag = neg_hundred
      kflag = 0

  ! cm1r19:  skip if we already know this processor doesnt have this parcel
  haveit1:  &
  IF( x3d.ge.xf(1) .and. x3d.le.xf(ni+1) .and.  &
      y3d.ge.yf(1) .and. y3d.le.yf(nj+1) )THEN

    IF(nx.eq.1)THEN
      iflag = 1
    ELSE
      ! cm1r19:
      i = ni+1
      do while( iflag.lt.0 .and. i.gt.1 )
        i = i-1
        if( x3d.ge.xf(i) .and. x3d.le.xf(i+1) )then
          iflag = i
        endif
      enddo
    ENDIF

    IF(axisymm.eq.1.or.ny.eq.1)THEN
      jflag = 1
    ELSE
      ! cm1r19:
      j = nj+1
      do while( jflag.lt.0 .and. j.gt.1 )
        j = j-1
        if( y3d.ge.yf(j) .and. y3d.le.yf(j+1) )then
          jflag = j
        endif
      enddo
    ENDIF

  ENDIF  haveit1

#ifdef MPI
      ! check for conflict:
    IF( (iflag.ge.1.and.iflag.le.ni) .and.   &
        (jflag.ge.1.and.jflag.le.nj) )THEN
      IF( iflag.eq.ni .and. pdata(np,prx).eq.xf(iflag+1) .and. nodex.gt.1 .and.  myi.ne.nodex ) iflag = -1
      IF( jflag.eq.nj .and. pdata(np,pry).eq.yf(jflag+1) .and. nodey.gt.1 .and.  myj.ne.nodey ) jflag = -1
    ENDIF
#endif

      myparcel:  IF( (iflag.ge.1.and.iflag.le.ni) .and.   &
                     (jflag.ge.1.and.jflag.le.nj) )THEN

      rkloop:  DO nrkp = 1,2

      IF( nrkp.eq.1 )THEN
        i=iflag
        j=jflag
      ELSE
        iflag = neg_hundred
        jflag = neg_hundred
        IF(nx.eq.1)THEN
          iflag = 1
        ELSE
          ! cm1r19:
          i = ni+2
          do while( iflag.lt.0 .and. i.gt.0 )
            i = i-1
            if( x3d.ge.xf(i) .and. x3d.le.xf(i+1) )then
              iflag = i
            endif
          enddo
        ENDIF
        IF(axisymm.eq.1.or.ny.eq.1)THEN
          jflag = 1
        ELSE
          do j=0,nj+1
            if( y3d.ge.yf(j) .and. y3d.le.yf(j+1) ) jflag=j
          enddo
          ! cm1r19:
          j = nj+2
          do while( jflag.lt.0 .and. j.gt.0 )
            j = j-1
            if( y3d.ge.yf(j) .and. y3d.le.yf(j+1) )then
              jflag = j
            endif
          enddo
        ENDIF
        i=iflag
        j=jflag
      ENDIF

        IF(debug)THEN
        if( i.lt.0 .or. i.gt.(ni+1) .or. j.lt.0 .or. j.gt.(nj+1) )then
          print *,'  myid,i,j = ',myid,i,j
          print *,'  x,x1     = ',x3d,pdata(np,prx)
          print *,'  y,y1     = ',y3d,pdata(np,pry)
          do i=0,ni+1
            print *,i,abs(xh(i)-x3d),0.5*dx*ruh(i)
          enddo
          do j=0,nj+1
            print *,j,abs(yh(j)-y3d),0.5*dy*rvh(j)
          enddo
          call stopcm1
        endif
        ENDIF

        kflag = 1
        if( .not. terrain_flag )then
          do while( z3d.gt.zf(iflag,jflag,kflag+1) )
            kflag = kflag+1
          enddo
        else
          do while( sig3d.gt.sigmaf(kflag+1) )
            kflag = kflag+1
          enddo
        endif

        IF(debug)THEN
        if( kflag.le.0 .or. kflag.ge.(nk+1) )then
          print *,myid,nrkp
          print *,iflag,jflag,kflag
          print *,pdata(np,prx),pdata(np,pry),pdata(np,prz)
          print *,x3d,y3d,z3d
          print *,uval,vval,wval
          print *,zf(iflag,jflag,kflag),z3d,zf(iflag,jflag,kflag+1)
          print *,'  16667 '
          call stopcm1
        endif
        ENDIF

!----------------------------------------------------------------------
!  Data on u points

        i=iflag
        j=jflag
        k=kflag

        if( y3d.lt.yh(j) )then
          j=j-1
        endif
        if( .not. terrain_flag )then
          if( z3d.lt.zh(iflag,jflag,k) )then
            k=k-1
          endif
          rz = ( z3d-zh(iflag,jflag,k) )/( zh(iflag,jflag,k+1)-zh(iflag,jflag,k) )
        else
          if( sig3d.lt.sigma(k) )then
            k=k-1
          endif
          rz = ( sig3d-sigma(k) )/( sigma(k+1)-sigma(k) )
        endif

        rx = ( x3d-xf(i) )/( xf(i+1)-xf(i) )
        ry = ( y3d-yh(j) )/( yh(j+1)-yh(j) )

        ! saveit:
        rxu = rx
        rys = ry
        rzs = rz

        w1=(1.0-rx)*(1.0-ry)*(1.0-rz)
        w2=rx*(1.0-ry)*(1.0-rz)
        w3=(1.0-rx)*ry*(1.0-rz)
        w4=(1.0-rx)*(1.0-ry)*rz
        w5=rx*(1.0-ry)*rz
        w6=(1.0-rx)*ry*rz
        w7=rx*ry*(1.0-rz)
        w8=rx*ry*rz
        ! call calcWeights(w1,w2,w3,w4,w5,w6,w7,w8,rx,ry,rz)

        IF(debug)THEN
        wsum = w1+w2+w3+w4+w5+w6+w7+w8
        if( rx.lt.-0.0001 .or. rx.gt.1.0001 .or.  &
            ry.lt.-0.0001 .or. ry.gt.1.0001 .or.  &
            rz.lt.-0.0001 .or. rz.gt.1.0001 .or.  &
            wsum.le.0.99999 .or.                  &
            wsum.ge.1.00001 .or.                  &
            i.lt.0 .or. i.gt.(ni+1)   .or.        &
            j.lt.-1 .or. j.gt.(nj+1)   .or.       &
            k.lt.0 .or. k.gt.nk                   )then
          print *
          print *,'  13333a: '
          print *,'  np          = ',np
          print *,'  myid,i,j,k  = ',myid,i,j,k
          print *,'  rx,ry,rz    = ',rx,ry,rz
          print *,'  wsum        = ',wsum
          print *,'  xf1,x3d,xf2 = ',xf(i),x3d,xf(i+1)
          print *,'  yh1,y3d,yh2 = ',yh(j),y3d,yh(j+1)
          print *,'  zh1,z3d,zh2 = ',zh(iflag,jflag,k),z3d,zh(iflag,jflag,k+1)
          print *
          call stopcm1
        endif
        ENDIF

        uval = tri_interp(ie+1,je,ke,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,ua)

!----------------------------------------------------------------------
!  Data on v points

        i=iflag
        j=jflag
        k=kflag

        if( x3d.lt.xh(i) )then
          i=i-1
        endif
        if( .not. terrain_flag )then
          if( z3d.lt.zh(iflag,jflag,k) )then
            k=k-1
          endif
        else
          if( sig3d.lt.sigma(k) )then
            k=k-1
          endif
        endif

        rx = ( x3d-xh(i) )/( xh(i+1)-xh(i) )
        ry = ( y3d-yf(j) )/( yf(j+1)-yf(j) )
        rz = rzs

        ! saveit:
        rxs = rx
        ryv = ry

        ! w1=(1.0-rx)*(1.0-ry)*(1.0-rz)
        ! w2=rx*(1.0-ry)*(1.0-rz)
        ! w3=(1.0-rx)*ry*(1.0-rz)
        ! w4=(1.0-rx)*(1.0-ry)*rz
        ! w5=rx*(1.0-ry)*rz
        ! w6=(1.0-rx)*ry*rz
        ! w7=rx*ry*(1.0-rz)
        ! w8=rx*ry*rz
        call calcWeights(w1,w2,w3,w4,w5,w6,w7,w8,rx,ry,rz)

        IF(debug)THEN
        wsum = w1+w2+w3+w4+w5+w6+w7+w8
        if( rx.lt.-0.0001 .or. rx.gt.1.0001 .or.  &
            ry.lt.-0.0001 .or. ry.gt.1.0001 .or.  &
            rz.lt.-0.0001 .or. rz.gt.1.0001 .or.  &
            wsum.le.0.99999 .or.                  &
            wsum.ge.1.00001 .or.                  &
            i.lt.-1 .or. i.gt.(ni+1)   .or.       &
            j.lt.0 .or. j.gt.(nj+1)   .or.        &
            k.lt.0 .or. k.gt.nk                   )then
          print *
          print *,'  23333b: '
          print *,'  np          = ',np
          print *,'  myid,i,j,k  = ',myid,i,j,k
          print *,'  rx,ry,rz    = ',rx,ry,rz
          print *,'  wsum        = ',wsum
          print *,'  xh1,x3d,xh2 = ',xh(i),x3d,xh(i+1)
          print *,'  yf1,y3d,yh2 = ',yf(j),y3d,yf(j+1)
          print *,'  zh1,z3d,zh2 = ',zh(iflag,jflag,k),z3d,zh(iflag,jflag,k+1)
          print *
          call stopcm1
        endif
        ENDIF

        vval = tri_interp(ni,njp1,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,va)

!----------------------------------------------------------------------
!  Data on w points

        i=iflag
        j=jflag
        k=kflag

        if( x3d.lt.xh(i) )then
          i=i-1
        endif
        if( y3d.lt.yh(j) )then
          j=j-1
        endif

        rx = rxs
        ry = rys
        if( .not. terrain_flag )then
          rz = ( z3d-zf(iflag,jflag,k) )/( zf(iflag,jflag,k+1)-zf(iflag,jflag,k) )
        else
          rz = ( sig3d-sigmaf(k) )/( sigmaf(k+1)-sigmaf(k) )
        endif

        ! saveit:
        rzw = rz

        ! w1=(1.0-rx)*(1.0-ry)*(1.0-rz)
        ! w2=rx*(1.0-ry)*(1.0-rz)
        ! w3=(1.0-rx)*ry*(1.0-rz)
        ! w4=(1.0-rx)*(1.0-ry)*rz
        ! w5=rx*(1.0-ry)*rz
        ! w6=(1.0-rx)*ry*rz
        ! w7=rx*ry*(1.0-rz)
        ! w8=rx*ry*rz
        call calcWeights(w1,w2,w3,w4,w5,w6,w7,w8,rx,ry,rz)

        IF(debug)THEN
        wsum = w1+w2+w3+w4+w5+w6+w7+w8
        if( rx.lt.-0.0001 .or. rx.gt.1.0001 .or.  &
            ry.lt.-0.0001 .or. ry.gt.1.0001 .or.  &
            rz.lt.-0.0001 .or. rz.gt.1.0001 .or.  &
            wsum.le.0.99999 .or.                  &
            wsum.ge.1.00001 .or.                  &
            i.lt.-1 .or. i.gt.(ni+1)   .or.       &
            j.lt.-1 .or. j.gt.(nj+1)   .or.       &
            k.lt.1 .or. k.gt.nk                   )then
          print *
          print *,'  43333a: '
          print *,'  np          = ',np
          print *,'  myid,i,j,k  = ',myid,i,j,k
          print *,'  rx,ry,rz    = ',rx,ry,rz
          print *,'  wsum        = ',wsum
          print *,'  xh1,x3d,xh2 = ',xh(i),x3d,xh(i+1)
          print *,'  yh1,y3d,yh2 = ',yh(j),y3d,yh(j+1)
          print *,'  zh1,z3d,zh2 = ',zf(iflag,jflag,k),z3d,zf(iflag,jflag,k+1)
          print *
          call stopcm1
        endif
        ENDIF

        wval = tri_interp(ni,nj,nkp1,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,wa)

        if( terrain_flag )then
          sigdot =  tri_interp(ni,nj,nkp1,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,wa)
          zsp = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 0, 0, 0, 0,zs)
          z3d = zsp + sig3d*((zt-zsp)*rzt)
        endif

!----------------------------------------------------------------------
!  uv for parcels below lowest model level:

      IF( bbc.eq.3 )THEN
        ! semi-slip lower boundary condition:
        if( z3d.lt.zh(1,1,1) )then
          ! re-calculate velocities if parcel is below lowest model level:
          !------
          ! u at lowest model level:
          i=iflag
          j=jflag
          if( y3d.lt.yh(j) )then
            j=j-1
          endif
          uval = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 1, 0, 1, 0,ua(ib,jb,1))
          !------
          ! v at lowest model level:
          i=iflag
          j=jflag
          if( x3d.lt.xh(i) )then
            i=i-1
          endif
          vval = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 0, 1, 0, 1,va(ib,jb,1))
          !------
          ! z0:
          i=iflag
          j=jflag
          if( x3d.lt.xh(i) )then
            i=i-1
          endif
          if( y3d.lt.yh(j) )then
            j=j-1
          endif
          z0 = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 0, 0, 0, 0,znt)
          !------
          ! get u,v from (neutral) log-layer equation:
          rznt = 1.0/z0
          var = alog((z3d+z0)*rznt)/alog((zh(1,1,1)+z0)*rznt)
          if( imove.eq.1 )then
            uval = (uval+umove)*var - umove
            vval = (vval+vmove)*var - vmove
          else
            uval = uval*var
            vval = vval*var
          endif
        endif
      ENDIF

!-----------------------------------------------------
!  Update parcel positions:
!-----------------------------------------------------

      ! RK2 scheme:
      IF(nrkp.eq.1)THEN
        IF(nx.eq.1)THEN
          x3d=0.0
        ELSE
          x3d=pdata(np,prx)+dt*uval
        ENDIF
        IF(axisymm.eq.1.or.ny.eq.1)THEN
          y3d=0.0
        ELSE
          y3d=pdata(np,pry)+dt*vval
        ENDIF
        if( terrain_flag )then
          sig3d = pdata(np,prsig) + dt*sigdot
          sig1 = sigdot
        else
          z3d = pdata(np,prz)+dt*wval
          ww1=wval
        endif
        uu1=uval
        vv1=vval
      ELSE
        IF(nx.eq.1)THEN
          x3d=0.0
        ELSE
          x3d=pdata(np,prx)+dt2*(uu1+uval)
        ENDIF
        IF(axisymm.eq.1.or.ny.eq.1)THEN
          y3d=0.0
        ELSE
          y3d=pdata(np,pry)+dt2*(vv1+vval)
        ENDIF
        if( terrain_flag )then
          sig3d = pdata(np,prsig) + dt2*(sig1+sigdot)
          IF( sig3d.lt.0.0 )THEN
            print *,'  parcel is below surface:  np,x3d,y3d,sig3d = ',np,x3d,y3d,sig3d
            sig3d=1.0e-6
          ENDIF
          sig3d=min(sig3d,maxz)
        else
          z3d = pdata(np,prz)+dt2*(ww1+wval)
          IF( z3d.lt.0.0 )THEN
            print *,'  parcel is below surface:  np,x3d,y3d,z3d = ',np,x3d,y3d,z3d
            z3d=1.0e-6
          ENDIF
          z3d=min(z3d,maxz)
        endif
      ENDIF


      ENDDO  rkloop

!-----------------------------------------------------
!  Account for boundary conditions (if necessary)
!-----------------------------------------------------

        ! New for cm1r17:  if parcel exits domain,
        ! just assume periodic lateral boundary conditions
        ! (no matter what actual settings are for wbc,ebc,sbc,nbc)

        if(x3d.lt.minx)then
          x3d=x3d+(maxx-minx)
        endif
        if(x3d.gt.maxx)then
          x3d=x3d-(maxx-minx)
        endif

        if( (y3d.gt.maxy).and.(axisymm.ne.1).and.(ny.ne.1) )then
          y3d=y3d-(maxy-miny)
        endif
        if( (y3d.lt.miny).and.(axisymm.ne.1).and.(ny.ne.1) )then
          y3d=y3d+(maxy-miny)
        endif

        pdata(np,prx)=x3d
        pdata(np,pry)=y3d
        if( .not. terrain_flag )then
          pdata(np,prz)=z3d
        else
          pdata(np,prsig)=sig3d
        endif

#ifdef MPI

      ELSE

        ! set to really small number (so we can use the allreduce command below)
        pdata(np,prx) = -1.0e30
        pdata(np,pry) = -1.0e30
        if( .not. terrain_flag )then
          pdata(np,prz) = -1.0e30
        else
          pdata(np,prsig) = -1.0e30
        endif
#endif

      ENDIF  myparcel

    ENDDO  nploop
    if(timestats.ge.1) time_parcels=time_parcels+mytime()

!----------------------------------------------------------------------
!  communicate data  (for MPI runs)

#ifdef MPI
      !$acc update host(pdata)
      if( .not. terrain_flag )then
        call MPI_ALLREDUCE(MPI_IN_PLACE,pdata(1,1),npvars*nparcels,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,ierr)
      else
        call MPI_ALLREDUCE(MPI_IN_PLACE,pdata(1,1),(npvars-1)*nparcels,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,pdata(1,prsig),nparcels,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,ierr)
      endif
      !$acc update device(pdata)
#endif

!----------------------------------------------------------------------
!  get height ASL:

      if( terrain_flag )then
            call getparcelzs(xh,uh,ruh,xf,yh,vh,rvh,yf,zs,pdata)
            DO np=1,nparcels
              ! get z from sigma:
              ! (see Section 3 of "The governing equations for CM1", 
              !  http://www2.mmm.ucar.edu/people/bryan/cm1/cm1_equations.pdf)
              pdata(np,prz) = pdata(np,przs) + pdata(np,prsig)*((zt-pdata(np,przs))*rzt)
            ENDDO
      endif

!----------------------------------------------------------------------

      end subroutine parcel_driver


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine parcel_interp(dt,mtime,xh,uh,ruh,xf,uf,yh,vh,rvh,yf,vf, &
                               zh,mh,rmh,zf,mf,znt,ust,c1,c2,          &
                               zs,sigma,sigmaf,rds,gz,                 &
                               pi0,th0,thv0,qv0,qc0,qi0,rth0,          &
                               dum1,dum2,dum3,dum4,zv  ,qt  ,prs,rho,  &
                               dum7,dum8,buoy,vpg  ,                   &
                               u3d,v3d,w3d,pp3d,th   ,t     ,th3d,q3d, &
                               kmh,kmv,khh,khv,tke3d,pt3d,pdata,       &
                               tdiag,qdiag,                            &
                               pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,        &
                               nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,reqs_p, &
                               tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2)
      use input
      use constants
      use cm1libs , only : rslf,rsif
      use bc_module
      use comm_module
#ifdef MPI
      use mpi
#endif
      implicit none

!-----------------------------------------------------------------------
!  This subroutine interpolates model information to the parcel locations
!  (diagnostic only ... not used for model integration)
!-----------------------------------------------------------------------

      real, intent(in) :: dt
      double precision, intent(in) :: mtime
      real, intent(in), dimension(ib:ie) :: xh,uh,ruh
      real, intent(in), dimension(ib:ie+1) :: xf,uf
      real, intent(in), dimension(jb:je) :: yh,vh,rvh
      real, intent(in), dimension(jb:je+1) :: yf,vf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: mh,rmh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf,mf
      real, intent(in), dimension(ib:ie,jb:je) :: znt,ust
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: c1,c2
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(in), dimension(kb:ke) :: sigma
      real, intent(in), dimension(kb:ke+1) :: sigmaf
      real, intent(in), dimension(kb:ke) :: rds
      real, intent(in), dimension(itb:ite,jtb:jte) :: gz
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: pi0,th0,thv0,qv0,qc0,qi0,rth0
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2,dum3,dum4,zv,qt,prs,rho
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum7,dum8
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: u3d
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: v3d
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: w3d,buoy,vpg
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: pp3d,th3d
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: th,t
      real, intent(inout), dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: q3d
      real, intent(inout), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv,khh,khv
      real, intent(inout), dimension(ibt:iet,jbt:jet,kbt:ket) :: tke3d
      real, intent(inout), dimension(ibp:iep,jbp:jep,kbp:kep,npt) :: pt3d
      real, intent(inout), dimension(nparcels,npvals) :: pdata
      real, intent(inout) , dimension(ibdt:iedt,jbdt:jedt,kbdt:kedt,ntdiag) :: tdiag
      real, intent(inout) , dimension(ibdq:iedq,jbdq:jedq,kbdq:kedq,nqdiag) :: qdiag
      real, intent(inout), dimension(jmp,kmp) :: pw1,pw2,pe1,pe2
      real, intent(inout), dimension(imp,kmp) :: ps1,ps2,pn1,pn2
      real, intent(inout), dimension(kmt) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      integer, intent(inout), dimension(rmp) :: reqs_p
      real, intent(inout), dimension(cmp,jmp,kmt) :: tkw1,tkw2,tke1,tke2
      real, intent(inout), dimension(imp,cmp,kmt) :: tks1,tks2,tkn1,tkn2

      integer :: n,np,i,j,k,iflag,jflag,kflag
      real :: tem,tem1
      real :: uval,vval,wval,rx,ry,rz,w1,w2,w3,w4,w5,w6,w7,w8,wsum
      real :: rxu,ryv,rzw,rxs,rys,rzs
      real :: x3d,y3d,z3d,z0,rznt,var

      logical, parameter :: debug = .false.
!      !$acc update  &
!      !$acc host(xh,uh,ruh,xf,uf,yh,vh,rvh,yf,vf,zh,mh,rmh,zf,mf,znt,ust, &
!      !$acc      c1,c2,zs,sigma,sigmaf,rds,gz,pi0,th0,thv0,qv0,qc0,qi0,rth0, &
!      !$acc      dum1,dum2,dum3,dum4,zv,qt,prs,rho,dum7,dum8,u3d,v3d,w3d,buoy, &
!      !$acc      vpg,pp3d,th3d,th,t,q3d,kmh,kmv,khh,khv,tke3d,pt3d,pdata, &
!      !$acc      tdiag,qdiag)
!      !!$acc      pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,nw1,nw2,ne1,ne2, &
!      !!$acc      sw1,sw2,se1,se2,tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2)
      if(timestats.ge.1) time_phys_D2H=time_phys_D2H+mytime()

!----------------------------------------------------------------------
!  Get derived variables:

    IF(imoist.eq.1)THEN
      ! with moisture:

    !$omp parallel do default(shared) private(i,j,k)
    !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
    do k=1,nk
      do j=1,nj
      do i=1,ni
        qt(i,j,k)=q3d(i,j,k,nqv)
      enddo
      enddo
    enddo
    if(nql1.ge.1)then
      do n=nql1,nql2
      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
      do k=1,nk
        do j=1,nj
        do i=1,ni
          qt(i,j,k)=qt(i,j,k)+q3d(i,j,k,n)
        enddo
        enddo
      enddo
      enddo
    endif
    !$acc compare(qt)
      IF(iice.eq.1)THEN
        do n=nqs1,nqs2
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          qt(i,j,k)=qt(i,j,k)+q3d(i,j,k,n)
        enddo
        enddo
        enddo
    enddo
      ENDIF
      IF( prth.ge.1 .or. prt.ge.1 .or. prqsl.ge.1 .or. prqsi.ge.1 .or.  prvpg.ge.1 )THEN
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          th(i,j,k) = (th0(i,j,k)+th3d(i,j,k))
          t(i,j,k) = th(i,j,k)*(pi0(i,j,k)+pp3d(i,j,k))
        enddo
        enddo
        enddo
      ENDIF
      !$acc compare(th,t)
      IF( prb.ge.1 .or. prvpg.ge.1 )THEN
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum7(i,j,k) = g*( th3d(i,j,k)*rth0(i,j,k)             &
                           +repsm1*(q3d(i,j,k,nqv)-qv0(i,j,k))  &
                           -(qt(i,j,k)-q3d(i,j,k,nqv)-qc0(i,j,k)-qi0(i,j,k))   )
        enddo
        enddo
        enddo
        !$acc compare(dum7)
      ENDIF
      IF( prvpg.ge.1 )THEN
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum8(i,j,k) = th(i,j,k)*(1.0+reps*q3d(i,j,k,nqv))/(1.0+qt(i,j,k))
        enddo
        enddo
        enddo
        !$acc compare(dum8)
      ENDIF


    ELSE
      ! dry:


      IF( prth.ge.1 .or. prt.ge.1 .or. prvpg.ge.1 )THEN
       !$omp parallel do default(shared) private(i,j,k)
       !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
       do k=1,nk
        do j=1,nj
        do i=1,ni
          th(i,j,k)= (th0(i,j,k)+th3d(i,j,k))
          t(i,j,k) = th(i,j,k)*(pi0(i,j,k)+pp3d(i,j,k))
        enddo
        enddo
       enddo
       !$acc compare(th,t)
      ENDIF
      IF( prb.ge.1 .or. prvpg.ge.1 )THEN
       !$omp parallel do default(shared) private(i,j,k)
       !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
       do k=1,nk
        do j=1,nj
        do i=1,ni
          dum7(i,j,k) = g*( th3d(i,j,k)*rth0(i,j,k) )
        enddo
        enddo
       enddo
       !$acc compare(dum7)
      ENDIF
      IF( prvpg.ge.1 )THEN
       !$omp parallel do default(shared) private(i,j,k)
       !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
       do k=1,nk
        do j=1,nj
        do i=1,ni
          dum8(i,j,k) = th(i,j,k)
        enddo
        enddo
       enddo
       !$acc compare(dum8)
      ENDIF


    ENDIF


    IF( prb.ge.1 .or. prvpg.ge.1 )THEN
      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
      do k=2,nk
      do j=1,nj
      do i=1,ni
        buoy(i,j,k) = (c1(1,1,k)*dum7(i,j,k-1)+c2(1,1,k)*dum7(i,j,k))
      enddo
      enddo
      enddo
      if(timestats.ge.1) time_parceli=time_parceli+mytime()
      !!$acc update device(buoy)
      call    prepcornert_GPU(buoy,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,  &
                               tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2,reqs_p,1)
      !$acc compare(buoy)
      !!$acc update host(buoy)

      !$omp parallel do default(shared) private(i,j)
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=0,nj+1
      do i=0,ni+1
        buoy(i,j,1) = buoy(i,j,2)+(buoy(i,j,3)-buoy(i,j,2))  &
                                 *(  zf(i,j,1)-  zf(i,j,2))  &
                                 /(  zf(i,j,3)-  zf(i,j,2))
        buoy(i,j,nk+1) = buoy(i,j,nk)+(buoy(i,j,nk  )-buoy(i,j,nk-1))  &
                                     *(  zf(i,j,nk+1)-  zf(i,j,nk  ))  &
                                     /(  zf(i,j,nk  )-  zf(i,j,nk-1))
      enddo
      enddo
      !$acc compare(buoy)
    ENDIF
    IF( prvpg.ge.1 )THEN
    if( .not. terrain_flag )then
      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,tem1)
      do k=2,nk
      do j=1,nj
      do i=1,ni
        tem1 = rdz*cp*mf(1,1,k)
        vpg(i,j,k) = -tem1*(pp3d(i,j,k)-pp3d(i,j,k-1))  &
                          *(c2(1,1,k)*dum8(i,j,k)+c1(1,1,k)*dum8(i,j,k-1))
      enddo
      enddo
      enddo
      !$acc compare(vpg)
    else
      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,tem1)
      do k=2,nk
      do j=1,nj
      do i=1,ni
        tem1 = rds(k)*cp
        vpg(i,j,k) = -tem1*(pp3d(i,j,k)-pp3d(i,j,k-1))*gz(i,j)  &
                          *(c2(1,1,k)*dum8(i,j,k)+c1(1,1,k)*dum8(i,j,k-1))
      enddo
      enddo
      enddo
      !$acc compare(vpg)
    endif
      if(timestats.ge.1) time_parceli=time_parceli+mytime()
      !!$acc update device(vpg)
      call    prepcornert_GPU(vpg ,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,  &
                               tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2,reqs_p,1)
      !!$acc update host(vpg)
      ! cmr18:  at top/bottom boundaries, vpg + buoy = 0
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=0,nj+1
      do i=0,ni+1
        vpg(i,j,1) = -buoy(i,j,1)
        vpg(i,j,nk+1) = -buoy(i,j,nk+1)
      enddo
      enddo
    ENDIF

    if(timestats.ge.1) time_parceli=time_parceli+mytime()

!----------------------------------------------------------------------
!  get corner info for MPI runs
!  (may not parallelize correctly if this is not done)

#ifdef MPI
      !!$acc update device(u3d,v3d,w3d)
      call getcorneru_GPU(u3d,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1))
      call bcu2_GPU(u3d)
      call getcornerv_GPU(v3d,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1))
      call bcv2_GPU(v3d)
      call getcornerw_GPU(w3d,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1))
      call bcw2_GPU(w3d)
      !!$acc update host(u3d,v3d,w3d)
#endif
      !$acc compare(u3d,v3d,w3d)

!----------------------------------------------------------------------
!  apply bottom/top boundary conditions:
!  [Note:  for u,v,s the array index (i,j,0) means the surface, ie z=0]
!     (for the parcel subroutines only!)

  IF(bbc.eq.1)THEN
    !$omp parallel do default(shared) private(i,j)
    !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
    DO j=jb,je
      ! free slip ... extrapolate:
      do i=ib,ie+1
        u3d(i,j,0) = cgs1*u3d(i,j,1)+cgs2*u3d(i,j,2)+cgs3*u3d(i,j,3)
      enddo
    ENDDO
    !$omp parallel do default(shared) private(i,j)
    !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
    DO j=jb,je+1
      do i=ib,ie
        v3d(i,j,0) = cgs1*v3d(i,j,1)+cgs2*v3d(i,j,2)+cgs3*v3d(i,j,3)
      enddo
    ENDDO
    !$acc compare(u3d,v3d)
  ELSEIF(bbc.eq.2)THEN
    ! no slip:
    if( imove.eq.1 )then
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      DO j=jb,je
        do i=ib,ie+1
          u3d(i,j,0) = 0.0 - umove
        enddo
      ENDDO
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      DO j=jb,je+1
        do i=ib,ie
          v3d(i,j,0) = 0.0 - vmove
        enddo
       ENDDO
    !$acc compare(u3d,v3d)
    else
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      DO j=jb,je
        do i=ib,ie+1
          u3d(i,j,0) = 0.0
        enddo
      ENDDO
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      DO j=jb,je+1
        do i=ib,ie
          v3d(i,j,0) = 0.0
        enddo
       ENDDO
    endif
  ELSEIF(bbc.eq.3)THEN
      ! u,v near sfc are determined below using log-layer equations
  ENDIF

!----------

  IF(tbc.eq.1)THEN
    !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
    DO j=jb,je
      ! free slip ... extrapolate:
      do i=ib,ie+1
        u3d(i,j,nk+1) = cgt1*u3d(i,j,nk)+cgt2*u3d(i,j,nk-1)+cgt3*u3d(i,j,nk-2)
      enddo
    ENDDO
    !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
    DO j=jb,je+1
      do i=ib,ie
        v3d(i,j,nk+1) = cgt1*v3d(i,j,nk)+cgt2*v3d(i,j,nk-1)+cgt3*v3d(i,j,nk-2)
      enddo
    ENDDO
  ELSEIF(tbc.eq.2)THEN
    !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
    DO j=jb,je
      ! no slip:
      do i=ib,ie+1
        u3d(i,j,nk+1) = 0.0
      enddo
    ENDDO
    !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
    DO j=jb,je+1
      do i=ib,ie
        v3d(i,j,nk+1) = 0.0
      enddo
    ENDDO
  ENDIF
    !$acc compare(u3d,v3d)

!----------

  !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
  DO j=jb,je
    do i=ib,ie
      w3d(i,j,nk+1) = 0.0
    enddo
  ENDDO

      if(timestats.ge.1) time_parceli=time_parceli+mytime()

      !print *,'parcel_interp: point #1'
      if( prth.ge.1 )then
        !!$acc update device(th)
        call prepcorners_GPU(th ,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                             pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p,1)
        !!$acc update host(th)
        !$acc compare(th)
      endif
      !print *,'parcel_interp: point #2'
      if( prt.ge.1 )then
        !!$acc update device(t)
        call prepcorners_GPU(t  ,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                             pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p,1)
        !$acc compare(t)
        !!$acc update host(t)
      endif
      !print *,'parcel_interp: point #3'
      if( prprs.ge.1 )then
        !!$acc update device(prs)
        call prepcorners_GPU(prs,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                             pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p,1)
        !!$acc update host(prs)
        !$acc compare(prs)
      endif
      !print *,'parcel_interp: point #4'
      if( prrho.ge.1 )then
        !!$acc update device(rho)
        call prepcorners_GPU(rho,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                             pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p,1)
        !!$acc update host(rho)
        !$acc compare(rho)
      endif
      !print *,'parcel_interp: point #5'
      if(prpt1.ge.1)then
        !!$acc update device(pt3d)
        do n=1,npt
          call prepcorners_GPU(pt3d(ib,jb,kb,n),nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                                            pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p,0)
        enddo
        !!$acc update host(pt3d)
        !$acc compare(pt3d)
      endif
      !print *,'parcel_interp: point #6'
      !!$acc update device(q3d)
      if( prqv.ge.1 )then
        call prepcorners_GPU(q3d(ib,jb,kb,nqv),nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                                           pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p,0)
      endif
      !print *,'parcel_interp: point #7'
      if( prq1.ge.1 .or. prnc1.ge.1 )then
        do n = 1,numq
          call prepcorners_GPU(q3d(ib,jb,kb,n),nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                                           pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p,0)
        enddo
      endif
      !$acc compare(q3d)
      !!$acc update host(q3d)
      !print *,'parcel_interp: point #8'
      !!$acc update device(kmh,kmv)
      if( prkm.ge.1 )then
        call prepcornert_GPU(kmh,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                             tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2,reqs_p,0)
        call prepcornert_GPU(kmv,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                             tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2,reqs_p,0)
      endif
      !$acc compare(kmh,kmv)
      !!$acc update host(kmh,kmv)
      !print *,'parcel_interp: point #9'
      !!$acc update device(khh,khv)
      if( prkh.ge.1 )then
        call prepcornert_GPU(khh,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                             tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2,reqs_p,0)
        call prepcornert_GPU(khv,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                             tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2,reqs_p,0)
      endif
      !$acc compare(khh,khv)
      !!$acc update host(khh,khv)
      !print *,'parcel_interp: point #10'
      !!$acc update device(tke3d)
      if( prtke.ge.1 )then
        call prepcornert_GPU(tke3d,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                               tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2,reqs_p,0)
      endif
      !$acc compare(tke3d)
      !!$acc update host(tke3d)
      !print *,'parcel_interp: point #11'
      !!$acc update device(qdiag)
      if( prdbz.ge.1 )then
        call prepcorners_GPU(qdiag(ibdq,jbdq,kbdq,qd_dbz),  &
                             nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                             pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p,1)
      endif
      !!$acc update host(qdiag)
      !$acc compare(qdiag)

!----------------------------------------------------------------------

      !print *,'parcel_interp: point #12'
    IF( prqsl.ge.1 )THEN
#ifdef _B4B02F
      !$acc update host(prs,t)
#else
      !$acc parallel loop gang vector collapse(3) private(i,j,k)
#endif
      do k=1,nk
      do j=1,nj
      do i=1,ni
        dum1(i,j,k) = rslf( prs(i,j,k) , t(i,j,k) )
      enddo
      enddo
      enddo
#ifndef _B4B02F
      !$acc update device(dum1)
#endif
      if(timestats.ge.1) time_parceli=time_parceli+mytime()

      call prepcorners_GPU(dum1,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                            pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p,1)
      !!$acc update host(dum1)
    ENDIF
    IF( prqsi.ge.1 )THEN
#ifdef _B4B03F
      !$acc update host(prs,t)
#else
      !$acc parallel loop gang vector collapse(3) private(i,j,k)
#endif
      do k=1,nk
      do j=1,nj
      do i=1,ni
        dum2(i,j,k) = rsif( prs(i,j,k) , t(i,j,k) )
      enddo
      enddo
      enddo
#ifndef _B4B03F
      !$acc update device(dum2)
#endif
      if(timestats.ge.1) time_parceli=time_parceli+mytime()

      call prepcorners_GPU(dum2,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1), &
                            pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p,1)
      !!$acc update host(dum2)
    ENDIF
    !$acc compare(dum1,dum2)
    !  print *,'parcel_interp: point #13'

!----------------------------------------------------------------------
!  Get zvort at appropriate C-grid location:
!  (assuming no terrain)
!  cm1r18:  below lowest model level:
!           Use extrapolated velocities for bbc=1,2
!           Use log-layer equations for bbc=3 (see below)

    IF( przv.ge.1)THEN

      !$acc parallel loop gang vector collapse(3) private(i,j,k)
      do k=0,nk+1
      do j=1,nj+1
      do i=1,ni+1
        zv(i,j,k) = (v3d(i,j,k)-v3d(i-1,j,k))*rdx*uf(i)   &
                   -(u3d(i,j,k)-u3d(i,j-1,k))*rdy*vf(j)
      enddo
      enddo
      enddo

    ENDIF
    !$acc compare(zv)
    !HERE print *,'parcel_interp: before nploop2'
!----------------------------------------------------------------------
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!----------------------------------------------------------------------
!  Loop through all parcels:  if you have it, get interpolated info:
    !stop 'parcel_interp: before nploop2'
    !PORTME
    !$acc parallel loop gang vector default(present) &
    !$acc   private(x3d,y3d,z3d,i,j,k,n,iflag,jflag,kflag, &
    !$acc       rx,ry,rz,rxu,ryv,rzw,rxs,rys,rzs, &
    !$acc       w1,w2,w3,w4,w5,w6,w7,w8, &
    !$acc       uval,vval,wval,z0,var,rznt)
    nploop2:  &
    DO np=1,nparcels

      pdata(np,prtime) = mtime

      x3d = pdata(np,prx)
      y3d = pdata(np,pry)
      z3d = pdata(np,prz)

      iflag = neg_hundred
      jflag = neg_hundred
      kflag = 0

  ! cm1r19:  skip if we already know this processor doesnt have this parcel
  haveit2:  &
  IF( x3d.ge.xf(1) .and. x3d.le.xf(ni+1) .and.  &
      y3d.ge.yf(1) .and. y3d.le.yf(nj+1) )THEN

    IF(nx.eq.1)THEN
      iflag = 1
    ELSE
      ! cm1r19:
      i = ni+1
      do while( iflag.lt.0 .and. i.gt.1 )
        i = i-1
        if( x3d.ge.xf(i) .and. x3d.le.xf(i+1) )then
          iflag = i
        endif
      enddo
    ENDIF

    IF(axisymm.eq.1.or.ny.eq.1)THEN
      jflag = 1
    ELSE
      ! cm1r19:
      j = nj+1
      do while( jflag.lt.0 .and. j.gt.1 )
        j = j-1
        if( y3d.ge.yf(j) .and. y3d.le.yf(j+1) )then
          jflag = j
        endif
      enddo
    ENDIF

  ENDIF  haveit2

#ifdef MPI
      ! check for conflict:
    IF( (iflag.ge.1.and.iflag.le.ni) .and.   &
        (jflag.ge.1.and.jflag.le.nj) )THEN
      IF( iflag.eq.ni .and. pdata(np,prx).eq.xf(iflag+1) .and. nodex.gt.1 .and.  myi.ne.nodex ) iflag = -1
      IF( jflag.eq.nj .and. pdata(np,pry).eq.yf(jflag+1) .and. nodey.gt.1 .and.  myj.ne.nodey ) jflag = -1
    ENDIF
#endif

      myprcl:  IF( (iflag.ge.1.and.iflag.le.ni) .and.   &
                   (jflag.ge.1.and.jflag.le.nj) )THEN

        i=iflag
        j=jflag

        kflag = 1
        if( .not. terrain_flag )then
          do while( pdata(np,prz).ge.zf(iflag,jflag,kflag+1) )
            kflag = kflag+1
          enddo
        else
          do while( pdata(np,prsig).ge.sigmaf(kflag+1) )
            kflag = kflag+1
          enddo
        endif

        x3d = pdata(np,prx)
        y3d = pdata(np,pry)
        z3d = pdata(np,prz)

!----------------------------------------------------------------------
!  Data on u points

        i=iflag
        j=jflag
        k=kflag

        if( pdata(np,pry).lt.yh(j) )then
          j=j-1
        endif
        if( .not. terrain_flag )then
          if( pdata(np,prz).lt.zh(iflag,jflag,k) )then
            k=k-1
          endif
          rz = ( pdata(np,prz)-zh(iflag,jflag,k) )/( zh(iflag,jflag,k+1)-zh(iflag,jflag,k) )
        else
          if( pdata(np,prsig).lt.sigma(k) )then
            k=k-1
          endif
          rz = ( pdata(np,prsig)-sigma(k) )/( sigma(k+1)-sigma(k) )
        endif

        rx = ( pdata(np,prx)-xf(i) )/( xf(i+1)-xf(i) )
        ry = ( pdata(np,pry)-yh(j) )/( yh(j+1)-yh(j) )


        ! saveit:
        rxu = rx
        rys = ry
        rzs = rz

        w1=(1.0-rx)*(1.0-ry)*(1.0-rz)
        w2=rx*(1.0-ry)*(1.0-rz)
        w3=(1.0-rx)*ry*(1.0-rz)
        w4=(1.0-rx)*(1.0-ry)*rz
        w5=rx*(1.0-ry)*rz
        w6=(1.0-rx)*ry*rz
        w7=rx*ry*(1.0-rz)
        w8=rx*ry*rz

        IF(debug)THEN
        wsum = w1+w2+w3+w4+w5+w6+w7+w8
        if( rx.lt.-0.0001 .or. rx.gt.1.0001 .or.  &
            ry.lt.-0.0001 .or. ry.gt.1.0001 .or.  &
            rz.lt.-0.0001 .or. rz.gt.1.0001 .or.  &
            wsum.le.0.99999 .or.                  &
            wsum.ge.1.00001 .or.                  &
            i.lt.0 .or. i.gt.(ni+1)   .or.        &
            j.lt.-1 .or. j.gt.(nj+1)   .or.       &
            k.lt.0 .or. k.gt.nk                   )then
          print *
          print *,'  13333b: '
          print *,'  np          = ',np
          print *,'  myid,i,j,k  = ',myid,i,j,k
          print *,'  rx,ry,rz    = ',rx,ry,rz
          print *,'  wsum        = ',wsum
          print *,'  xf1,x3d,xf2 = ',xf(i),pdata(np,prx),xf(i+1)
          print *,'  yh1,y3d,yh2 = ',yh(j),pdata(np,pry),yh(j+1)
          print *,'  zh1,z3d,zh2 = ',zh(iflag,jflag,k),pdata(np,prz),zh(iflag,jflag,k+1)
          print *
          call stopcm1
        endif
        ENDIF

        uval =  tri_interp(nip1,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,u3d)

!----------------------------------------------------------------------
!  Data on v points

        i=iflag
        j=jflag
        k=kflag

        if( pdata(np,prx).lt.xh(i) )then
          i=i-1
        endif
        if( .not. terrain_flag )then
          if( pdata(np,prz).lt.zh(iflag,jflag,k) )then
            k=k-1
          endif
        else
          if( pdata(np,prsig).lt.sigma(k) )then
            k=k-1
          endif
        endif

        rx = ( pdata(np,prx)-xh(i) )/( xh(i+1)-xh(i) )
        ry = ( pdata(np,pry)-yf(j) )/( yf(j+1)-yf(j) )
        rz = rzs

        ! saveit:
        rxs = rx
        ryv = ry

        w1=(1.0-rx)*(1.0-ry)*(1.0-rz)
        w2=rx*(1.0-ry)*(1.0-rz)
        w3=(1.0-rx)*ry*(1.0-rz)
        w4=(1.0-rx)*(1.0-ry)*rz
        w5=rx*(1.0-ry)*rz
        w6=(1.0-rx)*ry*rz
        w7=rx*ry*(1.0-rz)
        w8=rx*ry*rz

        IF(debug)THEN
        wsum = w1+w2+w3+w4+w5+w6+w7+w8
        if( rx.lt.-0.0001 .or. rx.gt.1.0001 .or.  &
            ry.lt.-0.0001 .or. ry.gt.1.0001 .or.  &
            rz.lt.-0.0001 .or. rz.gt.1.0001 .or.  &
            wsum.le.0.99999 .or.                  &
            wsum.ge.1.00001 .or.                  &
            i.lt.-1 .or. i.gt.(ni+1)   .or.       &
            j.lt.0 .or. j.gt.(nj+1)   .or.        &
            k.lt.0 .or. k.gt.nk                   )then
          print *
          print *,'  23333a: '
          print *,'  np          = ',np
          print *,'  myid,i,j,k  = ',myid,i,j,k
          print *,'  rx,ry,rz    = ',rx,ry,rz
          print *,'  wsum        = ',wsum
          print *,'  xh1,x3d,xh2 = ',xh(i),pdata(np,prx),xh(i+1)
          print *,'  yf1,y3d,yh2 = ',yf(j),pdata(np,pry),yf(j+1)
          print *,'  zh1,z3d,zh2 = ',zh(iflag,jflag,k),pdata(np,prz),zh(iflag,jflag,k+1)
          print *
          call stopcm1
        endif
        ENDIF

        vval = tri_interp(ni,njp1,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,v3d)

!----------------------------------------------------------------------
!  Data on w points

        i=iflag
        j=jflag
        k=kflag

        if( pdata(np,prx).lt.xh(i) )then
          i=i-1
        endif
        if( pdata(np,pry).lt.yh(j) )then
          j=j-1
        endif

!!!        rx = ( pdata(np,prx)-xh(i) )/( xh(i+1)-xh(i) )
!!!        ry = ( pdata(np,pry)-yh(j) )/( yh(j+1)-yh(j) )
        rx = rxs
        ry = rys
        if( .not. terrain_flag )then
          rz = ( pdata(np,prz)-zf(iflag,jflag,k) )/( zf(iflag,jflag,k+1)-zf(iflag,jflag,k) )
        else
          rz = ( pdata(np,prsig)-sigmaf(k) )/( sigmaf(k+1)-sigmaf(k) )
        endif

        ! saveit:
        rzw = rz


        w1=(1.0-rx)*(1.0-ry)*(1.0-rz)
        w2=rx*(1.0-ry)*(1.0-rz)
        w3=(1.0-rx)*ry*(1.0-rz)
        w4=(1.0-rx)*(1.0-ry)*rz
        w5=rx*(1.0-ry)*rz
        w6=(1.0-rx)*ry*rz
        w7=rx*ry*(1.0-rz)
        w8=rx*ry*rz
        ! call calcWeights(w1,w2,w3,w4,w5,w6,w7,w8,rx,ry,rz)

        IF(debug)THEN
        wsum = w1+w2+w3+w4+w5+w6+w7+w8
        if( rx.lt.-0.0001 .or. rx.gt.1.0001 .or.  &
            ry.lt.-0.0001 .or. ry.gt.1.0001 .or.  &
            rz.lt.-0.0001 .or. rz.gt.1.0001 .or.  &
            wsum.le.0.99999 .or.                  &
            wsum.ge.1.00001 .or.                  &
            i.lt.-1 .or. i.gt.ni   .or.           &
            j.lt.-1 .or. j.gt.nj   .or.           &
            k.lt.1 .or. k.gt.nk                   )then
          print *
          print *,'  43333b: '
          print *,'  np          = ',np
          print *,'  myid,i,j,k  = ',myid,i,j,k
          print *,'  rx,ry,rz    = ',rx,ry,rz
          print *,'  wsum        = ',wsum
          print *,'  xh1,x3d,xh2 = ',xh(i),pdata(np,prx),xh(i+1)
          print *,'  yh1,y3d,yh2 = ',yh(j),pdata(np,pry),yh(j+1)
          print *,'  zh1,z3d,zh2 = ',zf(iflag,jflag,k),pdata(np,prz),zf(iflag,jflag,k+1)
          print *
          call stopcm1
        endif
        ENDIF

        wval = tri_interp(ni,nj,nkp1,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,w3d)
      if(prkm.ge.1)then
        pdata(np,prkm  ) = tri_interp(ni,nj,nkp1,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,kmh)
        pdata(np,prkm+1) = tri_interp(ni,nj,nkp1,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,kmv)
      endif
      if(prkh.ge.1)then
        pdata(np,prkh  ) = tri_interp(ni,nj,nkp1,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,khh)
        pdata(np,prkh+1) = tri_interp(ni,nj,nkp1,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,khv)
      endif
      if( prtke.ge.1 )then
        pdata(np,prtke) = tri_interp(ni,nj,nkp1,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,tke3d)
      endif
      if( prb.ge.1 )then
        pdata(np,prb) = tri_interp(ni,nj,nkp1,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,buoy)
      endif
      if( prvpg.ge.1 )then
        pdata(np,prvpg) = tri_interp(ni,nj,nkp1,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,vpg)
      endif

!----------------------------------------------------------------------
!  Data on scalar points

        i=iflag
        j=jflag
        k=kflag

        if( pdata(np,prx).lt.xh(i) )then
          i=i-1
        endif
        if( pdata(np,pry).lt.yh(j) )then
          j=j-1
        endif
        if( .not. terrain_flag )then
          if( pdata(np,prz).lt.zh(iflag,jflag,k) )then
            k=k-1
          endif
        else
          if( pdata(np,prsig).lt.sigma(k) )then
            k=k-1
          endif
        endif

        rx = rxs
        ry = rys
        rz = rzs

        ! w1=(1.0-rx)*(1.0-ry)*(1.0-rz)
        ! w2=rx*(1.0-ry)*(1.0-rz)
        ! w3=(1.0-rx)*ry*(1.0-rz)
        ! w4=(1.0-rx)*(1.0-ry)*rz
        ! w5=rx*(1.0-ry)*rz
        ! w6=(1.0-rx)*ry*rz
        ! w7=rx*ry*(1.0-rz)
        ! w8=rx*ry*rz
        call calcWeights(w1,w2,w3,w4,w5,w6,w7,w8,rx,ry,rz)

        IF(debug)THEN
        wsum = w1+w2+w3+w4+w5+w6+w7+w8
        if( rx.lt.-0.0001 .or. rx.gt.1.0001 .or.  &
            ry.lt.-0.0001 .or. ry.gt.1.0001 .or.  &
            rz.lt.-0.0001 .or. rz.gt.1.0001 .or.  &
            wsum.le.0.99999 .or.                  &
            wsum.ge.1.00001 .or.                  &
            i.lt.-1 .or. i.gt.ni   .or.           &
            j.lt.-1 .or. j.gt.nj   .or.           &
            k.lt.0 .or. k.gt.nk                   )then
          print *
          print *,'  15558: '
          print *,'  np          = ',np
          print *,'  myid,i,j,k  = ',myid,i,j,k
          print *,'  rx,ry,rz    = ',rx,ry,rz
          print *,'  wsum        = ',wsum
          print *
          call stopcm1
        endif
        ENDIF

      if(imoist.eq.1)then
        if(prdbz.ge.1)  &
        pdata(np,prdbz) = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,qdiag(ibdq,jbdq,kbdq,qd_dbz))
        if(prqv.ge.1)  &
        pdata(np,prqv) = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,q3d(ib,jb,kb,nqv))
        if( prq1.ge.1 .and. nql1.ge.1 )then
          do n=nql1,nql1+(prq2-prq1)
            pdata(np,prq1+(n-nql1)) = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,q3d(ib,jb,kb,n))
          enddo
        endif
        if(prnc1.ge.1)then
          do n=nnc1,nnc1+(prnc2-prnc1)
            pdata(np,prnc1+(n-nnc1)) = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,q3d(ib,jb,kb,n))
          enddo
        endif
        if( prqsl.ge.1 )  &
        pdata(np,prqsl) = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,dum1)
        if( prqsi.ge.1 )  &
        pdata(np,prqsi) = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,dum2)
      endif

        if( prth.ge.1 )  &
        pdata(np,prth) = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,th)
        if( prt.ge.1 )  &
        pdata(np,prt ) = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,t)
        if( prprs.ge.1 )  &
        pdata(np,prprs) = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,prs)
        if( prrho.ge.1 )  &
        pdata(np,prrho) = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,rho)

        if(prpt1.ge.1)then
          do n=1,npt
          pdata(np,prpt1+n-1) = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,pt3d(ib,jb,kb,n))
          enddo
        endif

!----------------------------------------------------------------------
!  Data on zvort points

      IF( przv.ge.1 )THEN

        i=iflag
        j=jflag
        k=kflag

        if( .not. terrain_flag )then
          if( pdata(np,prz).lt.zh(iflag,jflag,k) )then
            k=k-1
          endif
        else
          if( pdata(np,prsig).lt.sigma(k) )then
            k=k-1
          endif
        endif

        rx = rxu
        ry = ryv
        rz = rzs

        ! w1=(1.0-rx)*(1.0-ry)*(1.0-rz)
        ! w2=rx*(1.0-ry)*(1.0-rz)
        ! w3=(1.0-rx)*ry*(1.0-rz)
        ! w4=(1.0-rx)*(1.0-ry)*rz
        ! w5=rx*(1.0-ry)*rz
        ! w6=(1.0-rx)*ry*rz
        ! w7=rx*ry*(1.0-rz)
        ! w8=rx*ry*rz
        call calcWeights(w1,w2,w3,w4,w5,w6,w7,w8,rx,ry,rz)

        IF(debug)THEN
        wsum = w1+w2+w3+w4+w5+w6+w7+w8
        if( rx.lt.-0.0001 .or. rx.gt.1.0001 .or.  &
            ry.lt.-0.0001 .or. ry.gt.1.0001 .or.  &
            rz.lt.-0.0001 .or. rz.gt.1.0001 .or.  &
            wsum.le.0.99999 .or.                  &
            wsum.ge.1.00001 .or.                  &
            i.lt.1 .or. i.gt.(ni+1)   .or.        &
            j.lt.1 .or. j.gt.(nj+1)   .or.        &
            k.lt.0 .or. k.gt.nk                   )then
          print *
          print *,'  15559: '
          print *,'  np          = ',np
          print *,'  myid,i,j,k  = ',myid,i,j,k
          print *,'  rx,ry,rz    = ',rx,ry,rz
          print *,'  wsum        = ',wsum
          print *
          call stopcm1
        endif
        ENDIF

        pdata(np,przv) = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,zv)

      ENDIF

!----------------------------------------------------------------------
!  surface variables  and  uv for parcels below lowest model level:

      IF( prznt.ge.1 .or. prust.ge.1 .or. bbc.eq.3 )THEN
        i=iflag
        j=jflag
        if( x3d.lt.xh(i) )then
          i=i-1
        endif
        if( y3d.lt.yh(j) )then
          j=j-1
        endif
        z0 = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 0, 0, 0, 0,znt)
        if( prznt.ge.1 ) pdata(np,prznt) = z0
        if( prust.ge.1 )  &
        pdata(np,prust) = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 0, 0, 0, 0,ust)
      ENDIF

      IF( bbc.eq.3 )THEN
        ! semi-slip lower boundary condition:
        if( z3d.lt.zh(1,1,1) )then
          ! re-calculate velocities if parcel is below lowest model level:
          !------
          ! u at lowest model level:
          i=iflag
          j=jflag
          if( y3d.lt.yh(j) )then
            j=j-1
          endif
          uval = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 1, 0, 1, 0,u3d(ib,jb,1))
          !------
          ! v at lowest model level:
          i=iflag
          j=jflag
          if( x3d.lt.xh(i) )then
            i=i-1
          endif
          vval = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 0, 1, 0, 1,v3d(ib,jb,1))
          !------
          ! get u,v from (neutral) log-layer equation:
          rznt = 1.0/z0
          var = alog((z3d+z0)*rznt)/alog((zh(1,1,1)+z0)*rznt)
          uval = (uval+umove)*var
          vval = (vval+vmove)*var
          !------
          IF( przv.ge.1 )THEN
            do j=jflag-1,jflag+1
            do i=iflag  ,iflag+1
              z0 = 0.5*(znt(i-1,j)+znt(i,j))
              rznt = 1.0/z0
              dum3(i,j,1) = (u3d(i,j,1)+umove)*alog((z3d+z0)*rznt)/alog((zh(1,1,1)+z0)*rznt)
            enddo
            enddo
            do j=jflag  ,jflag+1
            do i=iflag-1,iflag+1
              z0 = 0.5*(znt(i,j-1)+znt(i,j))
              rznt = 1.0/z0
              dum4(i,j,1) = (v3d(i,j,1)+vmove)*alog((z3d+z0)*rznt)/alog((zh(1,1,1)+z0)*rznt)
            enddo
            enddo
            do j=jflag,jflag+1
            do i=iflag,iflag+1
              dum7(i,j,1) = (dum4(i,j,1)-dum4(i-1,j,1))*rdx*uf(i)   &
                           -(dum3(i,j,1)-dum3(i,j-1,1))*rdy*vf(j)
            enddo
            enddo
            i=iflag
            j=jflag
            pdata(np,przv) = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 1, 1, 0, 0,dum7(ib,jb,1))
          ENDIF
        endif
      ENDIF


!----------------------------------------------------------------------

        pdata(np,pru)=uval
        pdata(np,prv)=vval
        pdata(np,prw)=wval

#ifdef MPI
      ELSE

        ! set to really small number (so we can use the allreduce command below)
        do n=npvars+1,npvals
          pdata(np,n) = -1.0e30
        enddo
#endif

      ENDIF  myprcl

    ENDDO  nploop2
    !$acc compare(pdata)
    if(timestats.ge.1) time_parceli=time_parceli+mytime()

!----------------------------------------------------------------------
!  communicate data
#ifdef MPI
  !$acc update host(pdata)
  do n=npvars+1,npvals
    if( myid.eq.0 )then
      call MPI_REDUCE(MPI_IN_PLACE,pdata(1,n),nparcels,MPI_REAL,MPI_MAX,0,MPI_COMM_WORLD,ierr)
    else
      call MPI_REDUCE( pdata(1,n),pdata(1,n),nparcels,MPI_REAL,MPI_MAX,0,MPI_COMM_WORLD,ierr)
    endif
  enddo
  !$acc update device(pdata)
  if(timestats.ge.1) time_parceli_reduce=time_parceli_reduce+mytime()
#endif
!----------------------------------------------------------------------

      !stop 'parcel_interp: at the end of the subroutine'

      end subroutine parcel_interp


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine parcel_write(prec,rtime,qname,name_prcl,desc_prcl,unit_prcl,pdata,ploc)
      use input
#ifdef NETCDF
      use writeout_nc_module, only : writepdata_nc
#endif
      implicit none

      integer, intent(inout) :: prec
      real, intent(in) :: rtime
      character(len=3), intent(in), dimension(maxq) :: qname
      character(len=40), intent(in), dimension(maxvars) :: name_prcl,desc_prcl,unit_prcl
      real, intent(in), dimension(nparcels,npvals) :: pdata
      real, intent(inout), dimension(nparcels,3) :: ploc

      integer :: i,n,np

!----------------------------------------------------------------------
!  write out data
    !$acc update host(pdata)
    IF(myid.eq.0)THEN

      IF(output_format.eq.1)THEN
        ! GrADS format:

        ! write GrADS descriptor file:
        call write_prclctl(name_prcl,desc_prcl,unit_prcl,prec)

        do i=1,maxstring
          string(i:i) = ' '
        enddo

        string = 'cm1out_pdata.dat'

!!!        write(string(15:20),101) prec
!!!101     format(i6.6)

        if(dowr) write(outfile,*) string
        open(unit=61,file=string,form='unformatted',access='direct',   &
             recl=4*npvals*nparcels,status='unknown')

        if(dowr) write(outfile,*)
        if(dowr) write(outfile,*) '  pdata prec = ',prec

        write(61,rec=prec) ((pdata(np,n),np=1,nparcels),n=1,npvals)

        close(unit=61)

#ifdef NETCDF
      ELSEIF(output_format.eq.2)THEN

        call     writepdata_nc(prec,rtime,qname,name_prcl,desc_prcl,unit_prcl,pdata,ploc(1,1))

#endif
      ENDIF
      if(dowr) write(outfile,*)

    ENDIF   ! endif for myid=0

      return
      end subroutine parcel_write


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      real function tri_interp(iz,jz,kz,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,s)
      !$acc routine vector
      use input
      implicit none

      integer :: iz,jz,kz,i,j,k
      real :: w1,w2,w3,w4,w5,w6,w7,w8
      real, dimension(1-ngxy:iz+ngxy,1-ngxy:jz+ngxy,1-ngz:kz+ngz) :: s
      !real, dimension(ib:ie,jb:je,kb:ke) :: s
      ! The following can run as many as 8400 pracels
      !real, dimension(ib:iz,jb:jz,kb:kz) :: s

      tri_interp = s(i  ,j  ,k  )*w1    &
                 + s(i+1,j  ,k  )*w2    &
                 + s(i  ,j+1,k  )*w3    &
                 + s(i  ,j  ,k+1)*w4    &
                 + s(i+1,j  ,k+1)*w5    &
                 + s(i  ,j+1,k+1)*w6    &
                 + s(i+1,j+1,k  )*w7    &
                 + s(i+1,j+1,k+1)*w8

      end function tri_interp


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


    real function get2d(i,j,x3d,y3d,xh,xf,yh,yf,xs,ys,is,js,s)
    !$acc routine vector
    use input
    implicit none

    integer, intent(in) :: i,j
    real, intent(in) :: x3d,y3d
    real, intent(in), dimension(ib:ie) :: xh
    real, intent(in), dimension(ib:ie+1) :: xf
    real, intent(in), dimension(jb:je) :: yh
    real, intent(in), dimension(jb:je+1) :: yf

    ! 0 = scalar point
    ! 1 = velocity point
    integer, intent(in) :: xs,ys
    integer, intent(in) :: is,js

    real, intent(in), dimension(ib:ie+is,jb:je+js) :: s

    real :: wg1,wg2,wg3,wg4
    real :: x13,x23,x33,x43
    real :: w1,w2,w3,w7,rx,ry,rz

    logical, parameter :: debug = .false.

!-----------------------------------------------------------------------
      ! tri-linear interp:

      IF(xs.eq.1)THEN
        rx = ( x3d-xf(i) )/( xf(i+1)-xf(i) )
      ELSE
        rx = ( x3d-xh(i) )/( xh(i+1)-xh(i) )
      ENDIF

      IF(ys.eq.1)THEN
        ry = ( y3d-yf(j) )/( yf(j+1)-yf(j) )
      ELSE
        ry = ( y3d-yh(j) )/( yh(j+1)-yh(j) )
      ENDIF

        w1=(1.0-rx)*(1.0-ry)
        w2=rx*(1.0-ry)
        w3=(1.0-rx)*ry
        w7=rx*ry

      IF( debug )THEN
        if( rx.lt.-0.000001 .or. rx.gt.1.000001 .or.        &
            ry.lt.-0.000001 .or. ry.gt.1.000001 .or.        &
            (w1+w2+w3+w7).lt.0.999999 .or.  &
            (w1+w2+w3+w7).gt.1.000001       &
          )then
          print *,'  x3d,y3d     = ',x3d,y3d
          print *,'  i,j         = ',i,j
          print *,'  rx,ry       = ',rx,ry
          print *,'  w1,w2,w3,w7 = ',w1,w2,w3,w7
          print *,'  w1+w2+w3+w7 = ',w1+w2+w3+w7
          print *,' 22346 '
          call stopcm1
        endif
      ENDIF

      get2d =s(i  ,j  )*w1    &
           +s(i+1,j  )*w2    &
           +s(i  ,j+1)*w3    &
           +s(i+1,j+1)*w7

!-----------------------------------------------------------------------

    end function get2d


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getparcelzs(xh,uh,ruh,xf,yh,vh,rvh,yf,zs,pdata)
      use input
#ifdef MPI
      use mpi
#endif
      implicit none

      real, intent(in), dimension(ib:ie) :: xh,uh,ruh
      real, intent(in), dimension(ib:ie+1) :: xf
      real, intent(in), dimension(jb:je) :: yh,vh,rvh
      real, intent(in), dimension(jb:je+1) :: yf
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(inout), dimension(nparcels,npvals) :: pdata

      integer :: i,j,iflag,jflag,np
      real :: x3d,y3d

    zsnploop:  &
    DO np=1,nparcels

      x3d = pdata(np,prx)
      y3d = pdata(np,pry)

      iflag = neg_hundred
      jflag = neg_hundred

  ! cm1r19:  skip if we already know this processor doesnt have this parcel
  zshaveit1:  &
  IF( x3d.ge.xf(1) .and. x3d.le.xf(ni+1) .and.  &
      y3d.ge.yf(1) .and. y3d.le.yf(nj+1) )THEN

    IF(nx.eq.1)THEN
      iflag = 1
    ELSE
      ! cm1r19:
      i = ni+1
      do while( iflag.lt.0 .and. i.gt.1 )
        i = i-1
        if( x3d.ge.xf(i) .and. x3d.le.xf(i+1) )then
          iflag = i
        endif
      enddo
    ENDIF

    IF(axisymm.eq.1.or.ny.eq.1)THEN
      jflag = 1
    ELSE
      ! cm1r19:
      j = nj+1
      do while( jflag.lt.0 .and. j.gt.1 )
        j = j-1
        if( y3d.ge.yf(j) .and. y3d.le.yf(j+1) )then
          jflag = j
        endif
      enddo
    ENDIF

  ENDIF  zshaveit1

#ifdef MPI
      ! check for conflict:
    IF( (iflag.ge.1.and.iflag.le.ni) .and.   &
        (jflag.ge.1.and.jflag.le.nj) )THEN
      IF( iflag.eq.ni .and. pdata(np,prx).eq.xf(iflag+1) .and. nodex.gt.1 .and.  myi.ne.nodex ) iflag = -1
      IF( jflag.eq.nj .and. pdata(np,pry).eq.yf(jflag+1) .and. nodey.gt.1 .and.  myj.ne.nodey ) jflag = -1
    ENDIF
#endif

      zsmyparcel:  IF( (iflag.ge.1.and.iflag.le.ni) .and.   &
                       (jflag.ge.1.and.jflag.le.nj) )THEN

        i=iflag
        j=jflag

        if( x3d.lt.xh(i) )then
          i=i-1
        endif
        if( y3d.lt.yh(j) )then
          j=j-1
        endif

        pdata(np,przs) = get2d(i,j,x3d,y3d,xh,xf,yh,yf, 0, 0, 0, 0,zs)

      ELSE  zsmyparcel

        pdata(np,przs) = -1.0e30

      ENDIF  zsmyparcel

    ENDDO  zsnploop

#ifdef MPI
    call MPI_ALLREDUCE(MPI_IN_PLACE,pdata(1,przs) ,nparcels,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,ierr)
#endif

      end subroutine getparcelzs


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine setup_parcel_vars(name_prcl,desc_prcl,unit_prcl,qname)
      use input
      implicit none

      character(len=40), intent(inout), dimension(maxvars) :: name_prcl,desc_prcl,unit_prcl
      character(len=3), intent(in), dimension(maxq) :: qname

      integer :: n,n2
      character(len=8) :: text1
      character(len=30) :: text2

      prcl_out = 0

      prcl_out = prcl_out+1
      name_prcl(prcl_out) = 'x'
      desc_prcl(prcl_out) = 'x position'
      unit_prcl(prcl_out) = 'm'

      prcl_out = prcl_out+1
      name_prcl(prcl_out) = 'y'
      desc_prcl(prcl_out) = 'y position'
      unit_prcl(prcl_out) = 'm'

      prcl_out = prcl_out+1
      name_prcl(prcl_out) = 'z'
      desc_prcl(prcl_out) = 'z position (above sea level)'
      unit_prcl(prcl_out) = 'm'

      if (prcl_droplet) then
         prcl_out = prcl_out+1
         name_prcl(prcl_out) = 'vpx'
         desc_prcl(prcl_out) = 'u velocity of droplet'
         unit_prcl(prcl_out) = 'm/s'

         prcl_out = prcl_out+1
         name_prcl(prcl_out) = 'vpy'
         desc_prcl(prcl_out) = 'v velocity of droplet'
         unit_prcl(prcl_out) = 'm/s'

         prcl_out = prcl_out+1
         name_prcl(prcl_out) = 'vpz'
         desc_prcl(prcl_out) = 'w velocity of droplet'
         unit_prcl(prcl_out) = 'm/s'

         prcl_out = prcl_out+1
         name_prcl(prcl_out) = 'rp'
         desc_prcl(prcl_out) = 'radius of droplet'
         unit_prcl(prcl_out) = 'm'

         prcl_out = prcl_out+1
         name_prcl(prcl_out) = 'ms'
         desc_prcl(prcl_out) = 'solute mass in droplet'
         unit_prcl(prcl_out) = 'kg'

         prcl_out = prcl_out+1
         name_prcl(prcl_out) = 'tp'
         desc_prcl(prcl_out) = 'temperature of droplet'
         unit_prcl(prcl_out) = 'K'

         prcl_out = prcl_out+1
         name_prcl(prcl_out) = 'mult'
         desc_prcl(prcl_out) = 'droplet multiplicity'
         unit_prcl(prcl_out) = ' '

      endif

      prcl_out = prcl_out+1
      name_prcl(prcl_out) = 'u'
      desc_prcl(prcl_out) = 'u velocity'
      unit_prcl(prcl_out) = 'm/s'

      prcl_out = prcl_out+1
      name_prcl(prcl_out) = 'v'
      desc_prcl(prcl_out) = 'v velocity'
      unit_prcl(prcl_out) = 'm/s'

      prcl_out = prcl_out+1
      name_prcl(prcl_out) = 'w'
      desc_prcl(prcl_out) = 'w velocity'
      unit_prcl(prcl_out) = 'm/s'

      prcl_out = prcl_out+1
      name_prcl(prcl_out) = 'mtime'
      desc_prcl(prcl_out) = 'model time (seconds since beginning of simulation)'
      unit_prcl(prcl_out) = 's'

      if( prth.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'th'
        desc_prcl(prcl_out) = 'potential temperature'
        unit_prcl(prcl_out) = 'K'
      endif

      if( prt.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 't'
        desc_prcl(prcl_out) = 'temperature'
        unit_prcl(prcl_out) = 'K'
      endif

      if( prprs.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'prs'
        desc_prcl(prcl_out) = 'pressure'
        unit_prcl(prcl_out) = 'Pa'
      endif

      if(prpt1.ge.1)then
        do n=1,npt
          text1='pt      '
          if(n.le.9)then
            write(text1(3:3),155) n
155         format(i1.1)
          elseif(n.le.99)then
            write(text1(3:4),154) n
154         format(i2.2)
          else
            write(text1(3:5),153) n
153         format(i3.3)
          endif

          prcl_out = prcl_out+1
          name_prcl(prcl_out) = text1
          desc_prcl(prcl_out) = 'passive tracer mixing ratio'
          unit_prcl(prcl_out) = 'kg/kg'
        enddo
      endif

      if( prqv.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'qv'
        desc_prcl(prcl_out) = 'water vapor mixing ratio'
        unit_prcl(prcl_out) = 'kg/kg'
      endif

      if( prq1.ge.1 .and. nql1.ge.1 )then
        n2 = nql2
        if( iice.eq.1 ) n2 = nqs2
        do n=nql1,n2
          text1='        '
          text2='                              '
          write(text1(1:3),156) qname(n)
          write(text2(1:3),156) qname(n)
156       format(a3)

          prcl_out = prcl_out+1
          name_prcl(prcl_out) = text1
          desc_prcl(prcl_out) = text2
          unit_prcl(prcl_out) = 'kg/kg'
        enddo
      endif

      if(prnc1.ge.1)then
        do n=nnc1,nnc2
          text1='        '
          text2='                              '
          write(text1(1:3),156) qname(n)
          write(text2(1:3),156) qname(n)

          prcl_out = prcl_out+1
          name_prcl(prcl_out) = text1
          desc_prcl(prcl_out) = text2
          unit_prcl(prcl_out) = '1/kg'
        enddo
      endif


      if( prkm.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'kmh'
        desc_prcl(prcl_out) = 'horiz eddy viscosity for momentum'
        unit_prcl(prcl_out) = 'm^2/s'

        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'kmv'
        desc_prcl(prcl_out) = 'vert eddy viscosity for momentum'
        unit_prcl(prcl_out) = 'm^2/s'
      endif

      if( prkh.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'khh'
        desc_prcl(prcl_out) = 'horiz eddy diffusivity for scalars'
        unit_prcl(prcl_out) = 'm^2/s'

        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'khv'
        desc_prcl(prcl_out) = 'vert eddy diffusivity for scalars'
        unit_prcl(prcl_out) = 'm^2/s'
      endif

      if( prtke.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'tke'
        desc_prcl(prcl_out) = 'subgrid tke'
        unit_prcl(prcl_out) = 'm^2/s^2'
      endif

      if( prdbz.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'dbz'
        desc_prcl(prcl_out) = 'reflectivity'
        unit_prcl(prcl_out) = 'dBZ'
      endif

      if( prb.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'b'
        desc_prcl(prcl_out) = 'buoyancy'
        unit_prcl(prcl_out) = 'm/s/s'
      endif

      if( prvpg.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'vpg'
        desc_prcl(prcl_out) = 'vertical perturbation pressure gradient'
        unit_prcl(prcl_out) = 'm/s/s'
      endif

      if( przv.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'zvort'
        desc_prcl(prcl_out) = 'vertical vorticity'
        unit_prcl(prcl_out) = '1/s'
      endif

      if( prrho.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'rho'
        desc_prcl(prcl_out) = 'dry-air density'
        unit_prcl(prcl_out) = 'kg/m^3'
      endif

      if( prqsl.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'qsl'
        desc_prcl(prcl_out) = 'saturation mixing ratio wrt liquid'
        unit_prcl(prcl_out) = 'kg/kg'
      endif

      if( prqsi.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'qsi'
        desc_prcl(prcl_out) = 'saturation mixing ratio wrt ice'
        unit_prcl(prcl_out) = 'kg/kg'
      endif

      if( prznt.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'znt'
        desc_prcl(prcl_out) = 'surface roughness length'
        unit_prcl(prcl_out) = 'm'
      endif

      if( prust.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'ust'
        desc_prcl(prcl_out) = 'surface friction velocity'
        unit_prcl(prcl_out) = 'm/s'
      endif

      if( przs.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'zs'
        desc_prcl(prcl_out) = 'terrain height'
        unit_prcl(prcl_out) = 'm'
      endif

      if( prsig.ge.1 )then
        prcl_out = prcl_out+1
        name_prcl(prcl_out) = 'sigma'
        desc_prcl(prcl_out) = 'sigma (nondimensional height)'
        unit_prcl(prcl_out) = 'nondimensional'
      endif


!-----------------------------------------------------------------------

      end subroutine setup_parcel_vars


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine write_prclctl(name_prcl,desc_prcl,unit_prcl,prec)
      use input
      use constants , only : grads_undef
      implicit none

      character(len=40), intent(in), dimension(maxvars) :: name_prcl,desc_prcl,unit_prcl
      integer, intent(in) :: prec

      integer :: i,n,nn
      character(len=16) :: a16

      !---------------------------------------------------------------
      ! This subroutine writes the GrADS descriptor file for parcels
      !---------------------------------------------------------------

    idcheck:  &
    IF( myid.eq.0 )THEN

      do i=1,maxstring
        string(i:i) = ' '
      enddo

      string = 'cm1out_parcel.ctl'
      if(dowr) write(outfile,*) string
      open(unit=50,file=string,status='unknown')

      string = 'cm1out_pdata.dat'

      write(50,401) string
      write(50,402) trim(cm1version)
      write(50,403) grads_undef
      write(50,404) nparcels
      write(50,405)
      write(50,406)
      write(50,407) prec

      write(50,408) prcl_out

      DO n = 1 , prcl_out
        a16 = '                '
        nn = len(trim(unit_prcl(n)))
        write(a16(2:15),214) unit_prcl(n)
        write(a16(1:1),201 )       '('
        write(a16(nn+2:nn+2),201 ) ')'
        write(50,409) name_prcl(n),desc_prcl(n),a16
      ENDDO

      write(50,410)

      close(unit=50)

    ENDIF  idcheck

201   format(a1)
214   format(a14)

401   format('dset ^',a)
402   format('title CM1 parcel data output, using version ',a,'; time is generic, see variable mtime for actual times')
403   format('undef ',f10.1)
404   format('xdef ',i10,' linear 1 1')
405   format('ydef          1 linear 0 1')
406   format('zdef          1 linear 0 1')
407   format('tdef ',i10,' linear 00:00Z01JAN0001 1YR')
408   format('vars ',i6)
409   format(a12,' 1 99 ',a40,1x,a16)
410   format('endvars')

      end subroutine write_prclctl

      real function calcW1(rx,ry,rz)
      !$acc routine vector 
      real, intent(in) :: rx,ry,rz
        calcW1=(1.0-rx)*(1.0-ry)*(1.0-rz)
      end function calcW1

      real function calcW2(rx,ry,rz)
      !$acc routine vector 
      real, intent(in) :: rx,ry,rz
        calcW2=rx*(1.0-ry)*(1.0-rz)
      end function calcW2

      real function calcW3(rx,ry,rz)
      !$acc routine vector 
      real, intent(in) :: rx,ry,rz
        calcW3=(1.0-rx)*ry*(1.0-rz)
      end function calcW3

      real function calcW4(rx,ry,rz)
      !$acc routine vector 
      real, intent(in) :: rx,ry,rz
        calcW4=(1.0-rx)*(1.0-ry)*rz
      end function calcW4

      real function calcW5(rx,ry,rz)
      !$acc routine vector 
      real, intent(in) :: rx,ry,rz
        calcW5=rx*(1.0-ry)*rz
      end function calcW5

      real function calcW6(rx,ry,rz)
      !$acc routine vector 
      real, intent(in) :: rx,ry,rz
        calcW6=(1.0-rx)*ry*rz
      end function calcW6

      real function calcW7(rx,ry,rz)
      !$acc routine vector 
      real, intent(in) :: rx,ry,rz
        calcW7=rx*ry*(1.0-rz)
      end function calcW7

      real function calcW8(rx,ry,rz)
      !$acc routine vector 
      real, intent(in) :: rx,ry,rz
        calcW8=rx*ry*rz
      end function calcW8

      subroutine calcWeights(w1,w2,w3,w4,w5,w6,w7,w8,rx,ry,rz)
      !$acc routine vector 
        real, intent(in)  :: rx,ry,rz
        real, intent(inout) :: w1,w2,w3,w4,w5,w6,w7,w8

        w1=(1.0-rx)*(1.0-ry)*(1.0-rz)
        w2=rx*(1.0-ry)*(1.0-rz)
        w3=(1.0-rx)*ry*(1.0-rz)
        w4=(1.0-rx)*(1.0-ry)*rz
        w5=rx*(1.0-ry)*rz
        w6=(1.0-rx)*ry*rz
        w7=rx*ry*(1.0-rz)
        w8=rx*ry*rz
 
      end subroutine calcWeights

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ! These two subroutines are added by following John Dennis's suggestion
      !      to optimize the location search for a parcel in a process

      subroutine find_horizontal_location_index (loc_ind, par_id, loc, lb, ub, dsize, end_ind, ind)
      !$acc routine seq

      use input

      implicit none

      integer, dimension(nparcels), intent(inout) :: loc_ind    ! array to store x/y location index
      integer,                      intent(in)    :: par_id     ! parcel index in pdata
      real,                         intent(in)    :: loc        ! current x/y location of a parcel
      integer,                      intent(in)    :: lb, ub     ! lower and upper bounds of dsize
      real, dimension(lb:ub),       intent(in)    :: dsize      ! range of x/y dimension
      integer,                      intent(in)    :: end_ind    ! end index of x/y dimension
      integer,                      intent(inout) :: ind        ! return the x/y location index closest to 
                                                                ! the current parcel location

      ! local variable
      integer :: i
#ifdef _VERIFY_FIND_LOC
      integer :: ind_ori

      ind_ori = ind
#endif

      ! Sanity check:
      ! - If input x/y location is outside the x/y range, return directly
      if ( loc .lt. dsize(lb) .or. loc .gt. dsize(end_ind) ) return

      if ( loc_ind(par_id) .ne. neg_hundred ) then
         i = min (end_ind, loc_ind(par_id) + location_offset)
         do while( ind .lt. 0 .and. i .gt. loc_ind(par_id) - location_offset - 1 )
            i = i - 1
            if ( loc .ge. dsize(i) .and. loc .le. dsize(i+1) ) then
               ind = i
               loc_ind(par_id) = ind
            end if
         end do
         ! Sanity check: 
         ! - If a parcel moves too far and outside search range, 
         !      this scheme would fail;
         ! - Switch back to the original linear search scheme 
         if ( ind .lt. 0 ) then
            print *, 'Parcel x/y location searching fails...'
            print *, 'Switch back to the original search scheme...'
            i = end_ind
            do while( ind .lt. 0 .and. i .gt. 1 )
               i = i - 1
               if ( loc .ge. dsize(i) .and. loc .le. dsize(i+1) ) then
                  ind = i
                  loc_ind(par_id) = ind
               end if
            end do
         end if
#ifdef _VERIFY_FIND_LOC
         ind_ori = ind
         i = end_ind
         do while( ind_ori .lt. 0 .and. i .gt. 1 )
            i = i - 1
            if ( loc .ge. dsize(i) .and. loc .le. dsize(i+1) ) then
               ind_ori = i
            end if
         end do
         if ( ind_ori .ne. ind ) then
            print *, "original search scheme finds x/y index = ", ind_ori, ", new search scheme finds x/y index = ", ind 
            stop "Failed verification test: x/y index is not the same..."
         else
            print *, "Pass the verification test for x/y index..."
         end if
#endif
      else ! Always search from the end when:
           !    - it is the first time step
           !    - a parcel enters a new or different MPI process
         i = end_ind
         do while( ind .lt. 0 .and. i .gt. 1 )
            i = i - 1
            if ( loc .ge. dsize(i) .and. loc .le. dsize(i+1) ) then
               ind = i
               loc_ind(par_id) = ind
            end if
         end do
      end if

      end subroutine find_horizontal_location_index

      subroutine find_vertical_location_index (loc_ind, par_id, loc, lb, ub, dsize, ind, is_ge)
      !$acc routine seq

      use input

      implicit none

      integer, dimension(nparcels), intent(inout) :: loc_ind    ! array to store z location index
      integer,                      intent(in)    :: par_id     ! parcel index in pdata
      real,                         intent(in)    :: loc        ! current z location of a parcel
      integer,                      intent(in)    :: lb, ub     ! lower and upper bounds of dsize
      real, dimension(lb:ub),       intent(in)    :: dsize      ! range of z dimension
      integer,                      intent(inout) :: ind        ! return the z location index closest
                                                                ! to the current parcel location
      logical,                      intent(in)    :: is_ge      ! TRUE if use "ge"; FALSE if use "gt"

      ! Local variable
      integer :: i
#ifdef _VERIFY_FIND_LOC
      integer :: ind_ori

      ind_ori = ind
#endif

      ! Sanity check:
      ! - If input x/y location is outside the x/y range, return directly
      if ( loc .lt. dsize(lb) .or. loc .gt. dsize(ub) ) return

      if ( loc_ind(par_id) .ne. neg_hundred ) then 
         i = max (ind, loc_ind(par_id) - location_offset - 1)
         ! Sanity check: 
         ! - If a parcel falls too low and outside search range,
         !      this scheme would fail;
         ! - Reset the lower bound in this case
         if ( loc .lt. dsize(i) ) then
            print *, 'Parcel z location is outside search range...'
            print *, 'Search from the input index instead...'
            i = ind
         end if
         if ( is_ge ) then
            do while( loc .ge. dsize(i+1) )
               i = i + 1
            end do
         else
            do while( loc .gt. dsize(i+1) )
               i = i + 1
            end do
         end if
         ind = i
#ifdef _VERIFY_FIND_LOC
         if ( is_ge ) then
            do while( loc .ge. dsize(ind_ori+1) )
               ind_ori = ind_ori + 1
            end do
         else
            do while( loc .gt. dsize(ind_ori+1) )
               ind_ori = ind_ori + 1
            end do
         end if
         if ( ind_ori .ne. ind ) then
            print *, "original search scheme finds z index = ", ind_ori, ", new search scheme finds z index = ", ind
            stop "Failed verification test: z index is not the same..."
         else
            print *, "Pass the verification test for z index..."
         end if
#endif
      else ! Always search from the end when:
           !    - it is the first time step
           !    - a parcel enters a new or different MPI process
         if ( is_ge ) then
            do while ( loc .ge. dsize(ind+1) )
               ind = ind + 1
            end do
         else
            do while ( loc .gt. dsize(ind+1) )
               ind = ind + 1
            end do
         end if
      end if
      loc_ind(par_id) = ind 

      end subroutine find_vertical_location_index

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  END MODULE parcel_module
