#define TROUBLE 0
#define PITER 1048576
#ifdef _B4B
#undef _B4B01F
#undef _B4B02F
#undef _B4B03F
#endif
  MODULE droplet_module

  implicit none

  private
  public :: droplet_driver

  ! If we already know that a parcel stays on the same process 
  !    in the previous time step and it is still here, we start 
  !    the location search from the previous index and limit 
  !    the search range using the parameter below;
  ! This assumes that a parcel does not move too far and does
  !    not move between processes within this subroutine?
  integer, parameter :: location_offset = 3

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
      use parcel_module
      use cm1libs , only : eslf
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
      real :: x3d,y3d,z3d
      integer :: nrkp
      real :: dt2,uu1,vv1,ww1
      real :: sig3d,th_tmp,rand
      real :: dvpdt1,dvpdt2,dvpdt3,drpdt,dtpdt,dmvdt
      real :: Nup,rhop0,taup0,rp0,salinity,part_grav1,part_grav2,part_grav3,rhoval
      real :: xv,yv,zv,dV,wtx,wty,wtz,wtt
      real :: esl

      integer :: idropmethod

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
    !$acc     xrhs1,xrhs2,xrhs3,volp,rhop,rhop0,taup,taup0,diffnorm,Rep,Nup,Shp,vrhs1,vrhs2,vrhs3, &
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

      iflag = undefined_index
      jflag = undefined_index
      kflag = 0

  ! cm1r19:  skip if we already know this processor doesnt have this parcel
  haveit1:  &
  IF( x3d.ge.xf(1) .and. x3d.le.xf(ni+1) .and.  &
      y3d.ge.yf(1) .and. y3d.le.yf(nj+1) )THEN

    IF(nx.eq.1)THEN
      iflag = 1
    ELSE
      ! cm1r19:
      call find_horizontal_location_index (pdata_locind(np,1), x3d, ib, ie+1, xf, ni+1, iflag) 
    ENDIF

    IF(axisymm.eq.1.or.ny.eq.1)THEN
      jflag = 1
    ELSE
      ! cm1r19:
      call find_horizontal_location_index (pdata_locind(np,2), y3d, jb, je+1, yf, nj+1, jflag)  
    ENDIF

  ELSE     ! re-initialize x/y/z location index array if a parcel is not on this process
    pdata_locind(np,1) = undefined_index
    pdata_locind(np,2) = undefined_index
    pdata_locind(np,3) = undefined_index
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
        call find_vertical_location_index (pdata_locind(np,3), z3d, kb, ke+1, zf(iflag,jflag,:), kflag, .TRUE.)
      else
        call find_vertical_location_index (pdata_locind(np,3), sig3d, kb, ke+1, sigmaf, kflag, .TRUE.)
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


      !idropmethod = 0 !RK2 for droplet time integration
      idropmethod = 1 !Implicit backward Euler for droplet time integration

      dropmethod: IF ( idropmethod .eq. 0) THEN !RK2 for droplet time integration

      call rk2_integration(dt,np,pdata,pdata_locind,iflag,jflag,kflag,xf,xh,uh,ruh,yf,yh,vh,rvh,zf,zh,zs,znt,sigma,sigmaf,ua,va,wa,qa,ta,rho,prs,x3d,y3d,z3d,sig3d,rhoval,Nup,rhop0,taup0,rp0,part_grav1,part_grav2,part_grav3,debug)

      ELSEIF (idropmethod.eq.1) THEN  !Implicit backward Euler for the droplet integration

      call BE_integration(dt,np,pdata,pdata_locind,iflag,jflag,kflag,xf,xh,uh,ruh,yf,yh,vh,rvh,zf,zh,zs,znt,sigma,sigmaf,ua,va,wa,qa,ta,rho,prs,x3d,y3d,z3d,sig3d,rhoval,Nup,rhop0,taup0,rp0,part_grav1,part_grav2,part_grav3,debug)

      ENDIF dropmethod

      !write(*,'(a6,5e15.6)') 'DHR:',x3d,y3d,z3d,pdata(np,prtp),pdata(np,prrp)

      ENDIF dropalive1
      

    dropalive2:  &
    IF (pdata(np,pract).gt.0.0) THEN


!-----------------------------------------------------
!  Now perform the two-way coupling based on the changes in droplet momentum,
!  temp, and size
!-----------------------------------------------------

      ! GHB, 210714:
      ! get i,j,k for final parcel location:

        iflag = undefined_index
        jflag = undefined_index
        IF(nx.eq.1)THEN
          iflag = 1
        ELSE
          ! cm1r19:
          call find_horizontal_location_index (pdata_locind(np,1), x3d, ib, ie+1, xf, ni+2, iflag)
        ENDIF
        IF(axisymm.eq.1.or.ny.eq.1)THEN
          jflag = 1
        ELSE
          ! cm1r19:
          call find_horizontal_location_index (pdata_locind(np,2), y3d, jb, je+1, yf, nj+2, jflag)
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
!        the find_vertical_location_index subroutine and 
!        lead to invalid access to zf(iflag,jflag,kflag);
!        revert to the old implementation
        if( .not. terrain_flag )then
          do while( z3d.gt.zf(iflag,jflag,kflag+1) )
            kflag = kflag+1
          enddo
        else
          do while( sig3d.gt.sigmaf(kflag+1) )
            kflag = kflag+1
          enddo
        endif
        pdata_locind(np,3) = kflag
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
      dtpdt = Nup/3.0/pr_num*(cp/cpl)*(rhop0/rhow)/taup0*(dtpdt-pdata(np,prtp))
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
        !qten(ix,iy,iz) = qten(ix,iy,iz) - &
        !    dmvdt/rhoval/dV*wtt*pdata(np,prmult)

        !Ignoring the enthalpy contained in the vapor -- this has been shown to
        !be negligible
        !!$acc atomic write
        !thten(ix,iy,iz) = thten(ix,iy,iz) - &
        !    (rhow/rhoval)*(cpl/cp)*dtpdt/dV*wtt*pdata(np,prmult)

        !thten(ix,iy,iz) = thten(ix,iy,iz) - &
        !    dtpdt*6.0*(rhow/rhop0)*(cpl/cp)*taup0*pi*rp0*viscosity*wtt/dV

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
          pdata_locind(np,1) = undefined_index
        endif
        if(x3d.gt.maxx)then
          x3d=x3d-(maxx-minx)
          pdata_locind(np,1) = undefined_index
        endif

        if( (y3d.gt.maxy).and.(axisymm.ne.1).and.(ny.ne.1) )then
          y3d=y3d-(maxy-miny)
          pdata_locind(np,2) = undefined_index
        endif
        if( (y3d.lt.miny).and.(axisymm.ne.1).and.(ny.ne.1) )then
          y3d=y3d+(maxy-miny)
          pdata_locind(np,2) = undefined_index
        endif

        pdata(np,prx)=x3d
        pdata(np,pry)=y3d
        if( .not. terrain_flag )then
          pdata(np,prz)=z3d
          pdata_locind(np,3) = undefined_index
        else
          pdata(np,prsig)=sig3d
          pdata_locind(np,3) = undefined_index
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

      subroutine rk2_integration(dt,np,pdata,pdata_locind,iflag,jflag,kflag,xf,xh,uh,ruh,yf,yh,vh,rvh,zf,zh,zs,znt,sigma,sigmaf,ua,va,wa,qa,ta,rho,prs,x3d,y3d,z3d,sig3d,rhoval,Nup,rhop0,taup0,rp0,part_grav1,part_grav2,part_grav3,debug)
      use input
      use constants
      use bc_module
      use comm_module
      use misclibs
      use parcel_module
      use cm1libs , only : eslf
      implicit none

      integer, intent(inout) :: iflag,jflag,kflag
      integer, intent(in) :: np
      logical, intent(in) :: debug
      real, intent(in), dimension(ib:ie+1) :: xf
      real, intent(in), dimension(jb:je+1) :: yf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(in), dimension(ib:ie) :: xh,uh,ruh
      real, intent(in), dimension(jb:je) :: yh,vh,rvh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(kb:ke) :: sigma
      real, intent(in), dimension(kb:ke+1) :: sigmaf
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke,numq) :: qa
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: ta
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: rho,prs
      real, intent(in), dimension(ib:ie,jb:je) :: znt
      real, intent(inout) :: x3d,y3d,z3d,sig3d,rhoval
      real, intent(inout), dimension(nparcels,npvals) :: pdata
      integer, intent(inout), dimension(nparcels,3) :: pdata_locind

      real, intent(out) :: Nup,rhop0,taup0,rp0
      real, intent(in) :: dt,part_grav1,part_grav2,part_grav3

      !Common with BE_integration
      real :: dt2
      real :: uval,vval,wval
      real :: xrhs1,xrhs2,xrhs3,vrhs1,vrhs2,vrhs3
      real :: vtmp1,vtmp2,vtmp3
      real :: k1xp1,k1xp2,k1xp3,k1vp1,k1vp2,k1vp3
      real :: tprhs,tptmp
      real :: k1tp,k2tp,k1rp,k2rp
      real :: rprhs,rptmp,qstar,einf,lhv,estar
      real :: taup,Rep,Shp,diffnorm
      real :: rhop,volp,tp0,volp0
      real :: qval,tval,prsval
      real :: sig1,sigdot
      integer :: i,j,k

      !Unique to rk2_integration
      integer :: nrkp


      dt2 = dt/2.0      

      !HERE print *,'before rkloop np: ',np
      !$acc loop seq
      rkloop:  DO nrkp = 1,2


       call interpolate_to_parcel(np,nrkp,pdata_locind,iflag,jflag,kflag,x3d,y3d,z3d,sig3d,uval,vval,wval,tval,qval,rhoval,prsval,xh,xf,yh,yf,zh,zf,zs,znt,sigma,sigmaf,ua,va,wa,ta,qa,rho,prs,sigdot)

       qval = 0.008807098199749
       tval = 282.3
       uval = 0.0
       vval = 0.0
       wval = 0.0

      if(np==TROUBLE) then 
        print *,'before eslf: tval: ',tval 
      endif
      estar = eslf(prsval,tval)  !Saturation vapor pressure based on interpolated temp,pressure
      lhv=lv1-lv2*tval

      write(*,*) 'DHR1:',qval,tval,estar


!-----------------------------------------------------
!  Update droplet position, velocity, etc.
!-----------------------------------------------------
      if(np==TROUBLE) then 
        !print *,'before: pdata(np,prrp): ',pdata(np,prrp)
        !print *,'before: pdata(np,prms): ',pdata(np,prms)
        !print *,'before: pdata(np,prtp): ',pdata(np,prtp)
        print *,'after eslf: tval: ',tval 
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

        !Original, for calculating changes in momentum, mass, and energy
        rhop0 = rhop
        taup0 = taup
        rp0 = pdata(np,prrp)
        tp0 = pdata(np,prtp)
        volp0 = 4.0/3.0*pi*rp0**3

        diffnorm = sqrt( (uval-pdata(np,prvpx))**2+ &
                         (vval-pdata(np,prvpy))**2+ &
                         (wval-pdata(np,prvpz))**2)


        Rep = 2.0*pdata(np,prrp)*diffnorm/viscosity
        Nup = 2.0 + 0.6*sqrt(Rep)*pr_num**(1.0/3.0)
        Shp = 2.0 + 0.6*sqrt(Rep)*sc_num**(1.0/3.0)

        diffnorm = 0.0 !DHR
        Nup = 2.0 !DHR
        Shp = 2.0 !DHR

        vrhs1 = 1.0/taup*(uval - pdata(np,prvpx)) + part_grav1
        vrhs2 = 1.0/taup*(vval - pdata(np,prvpy)) + part_grav2
        vrhs3 = 1.0/taup*(wval - pdata(np,prvpz)) + part_grav3


        qstar = estar*mw/ru/pdata(np,prtp)/rhoval*exp(                        &
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

        diffnorm = 0.0 !DHR
        Nup = 2.0 !DHR
        Shp = 2.0 !DHR

        vrhs1 = 1.0/taup*(uval - vtmp1) + part_grav1
        vrhs2 = 1.0/taup*(vval - vtmp2) + part_grav2
        vrhs3 = 1.0/taup*(wval - vtmp3) + part_grav3

        qstar = estar*mw/ru/tptmp*exp(                             &
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

      end subroutine rk2_integration

      subroutine BE_integration(dt,np,pdata,pdata_locind,iflag,jflag,kflag,xf,xh,uh,ruh,yf,yh,vh,rvh,zf,zh,zs,znt,sigma,sigmaf,ua,va,wa,qa,ta,rho,prs,x3d,y3d,z3d,sig3d,rhoval,Nup,rhop0,taup0,rp0,part_grav1,part_grav2,part_grav3,debug)
      use input
      use constants
      use bc_module
      use comm_module
      use misclibs
      use parcel_module
      use cm1libs , only : eslf
      implicit none

      integer, intent(inout) :: iflag,jflag,kflag
      integer, intent(in) :: np
      logical, intent(in) :: debug
      real, intent(in), dimension(ib:ie+1) :: xf
      real, intent(in), dimension(jb:je+1) :: yf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(in), dimension(ib:ie) :: xh,uh,ruh
      real, intent(in), dimension(jb:je) :: yh,vh,rvh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(kb:ke) :: sigma
      real, intent(in), dimension(kb:ke+1) :: sigmaf
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke,numq) :: qa
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: ta
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: rho,prs
      real, intent(in), dimension(ib:ie,jb:je) :: znt
      real, intent(inout) :: x3d,y3d,z3d,sig3d,rhoval
      real, intent(inout), dimension(nparcels,npvals) :: pdata
      integer, intent(inout), dimension(nparcels,3) :: pdata_locind
      real, intent(out) :: Nup,rhop0,taup0,rp0
      real, intent(in) :: dt,part_grav1,part_grav2,part_grav3

      integer :: nrkp

      !Common with rk2_integration
      real :: dt2
      real :: uval,vval,wval
      real :: xrhs1,xrhs2,xrhs3,vrhs1,vrhs2,vrhs3
      real :: vtmp1,vtmp2,vtmp3
      real :: k1xp1,k1xp2,k1xp3,k1vp1,k1vp2,k1vp3
      real :: tprhs,tptmp
      real :: k1tp,k2tp,k1rp,k2rp
      real :: rprhs,rptmp,qstar,einf,estar
      real :: taup,Rep,Shp,diffnorm
      real :: rhop,volp,tp0,volp0
      real :: qval,tval,prsval
      real :: sig1,sigdot
      integer :: i,j,k

      !Unique to BE_integration:
      real :: taup_scale,dt_nondim
      real :: guess,rt_zeros(2),rt_start(2)
      integer :: mflag,flag


       call interpolate_to_parcel(np,1,pdata_locind,iflag,jflag,kflag,x3d,y3d,z3d,sig3d,uval,vval,wval,tval,qval,rhoval,prsval,xh,xf,yh,yf,zh,zf,zs,znt,sigma,sigmaf,ua,va,wa,ta,qa,rho,prs,sigdot)

       qval = 0.008807098199749
       tval = 282.3
       uval = 0.0
       vval = 0.0
       wval = 0.0

      if(np==TROUBLE) then 
        print *,'before eslf: tval: ',tval 
      endif
      estar = eslf(prsval,tval)  !Saturation humidity based on interpolated temp,pressure

      !write(*,*) 'DHR2:',pdata(np,prms),estar


!-----------------------------------------------------
!  Update droplet position, velocity, etc.
!-----------------------------------------------------

        xrhs1 = pdata(np,prvpx)
        xrhs2 = pdata(np,prvpy)
        xrhs3 = pdata(np,prvpz)
        volp = 4.0/3.0*pi*pdata(np,prrp)**3
        rhop = (pdata(np,prms)+volp*rhow)/volp  !Density including the solute mass
        taup = rhop*(2.0*pdata(np,prrp))**2/18.0/rhoval/viscosity

        !Original, for calculating changes in momentum, mass, and energy
        rhop0 = rhop
        taup0 = taup
        rp0 = pdata(np,prrp)
        tp0 = pdata(np,prtp)
        volp0 = 4.0/3.0*pi*rp0**3


        diffnorm = sqrt( (uval-pdata(np,prvpx))**2+ &
                         (vval-pdata(np,prvpy))**2+ &
                         (wval-pdata(np,prvpz))**2)

        Rep = 2.0*pdata(np,prrp)*diffnorm/viscosity
        Nup = 2.0 + 0.6*sqrt(Rep)*pr_num**(1.0/3.0)
        Shp = 2.0 + 0.6*sqrt(Rep)*sc_num**(1.0/3.0)

        diffnorm = 0.0 !DHR
        Nup = 2.0 !DHR
        Shp = 2.0 !DHR

        vrhs1 = 1.0/taup*(uval - pdata(np,prvpx)) + part_grav1
        vrhs2 = 1.0/taup*(vval - pdata(np,prvpy)) + part_grav2
        vrhs3 = 1.0/taup*(wval - pdata(np,prvpz)) + part_grav3

      !Position and velocity are straightforward:

      x3d = pdata(np,prx) + dt*pdata(np,prvpx)
      y3d = pdata(np,pry) + dt*pdata(np,prvpy)
      z3d = pdata(np,prz) + dt*pdata(np,prvpz)

      pdata(np,prvpx) = (pdata(np,prvpx) + dt*uval/taup + dt*part_grav1)/(1 + dt/taup)
      pdata(np,prvpy) = (pdata(np,prvpy) + dt*vval/taup + dt*part_grav2)/(1 + dt/taup)
      pdata(np,prvpz) = (pdata(np,prvpz) + dt*wval/taup + dt*part_grav3)/(1 + dt/taup)

      !Nonlinear solver for the radius and temperature:

      !Nondimensionalize parameters before the solver using 1-micron water droplet as ref
      taup_scale = rhow*(1.0e-6)**2/(18*rhoval*viscosity)

      dt_nondim = dt/taup_scale
      

      !write(*,'(a6,5e15.6)') 'DHR1:',x3d,y3d,z3d,pdata(np,prtp),pdata(np,prrp)
      !Gives initial guess into nonlinear solver
      !mflag = 0, has equilibrium radius; mflag = 1, no equilibrium (uses itself as initial guess)
      call rad_solver2(guess,mflag,prsval,rhoval,tval,qval,pdata(np,prtp),pdata(np,prms),pdata(np,prrp))
      !write(*,'(a6,5e15.6)') 'DHR2:',guess,prsval,rhoval,tval,qval

      if (mflag.eq.0) then
         rt_start(1) = guess/pdata(np,prrp)
         rt_start(2) = tval/pdata(np,prtp)
      else
         rt_start(1) = 1.0
         rt_start(2) = 1.0
      end if

      call gauss_newton_2d(diffnorm,dt_nondim,taup_scale,rt_start,rt_zeros,flag,prsval,rhoval,tval,qval,pdata(np,prtp),pdata(np,prms),pdata(np,prrp))
      !write(*,'(a6,2i,4e15.6)') 'DHR3:',myid,np,rt_start(1),rt_start(2),rt_zeros(1),rt_zeros(2)

      !Now redimsionalize
      pdata(np,prrp) = rt_zeros(1)*pdata(np,prrp)
      pdata(np,prtp) = rt_zeros(2)*pdata(np,prtp)



      end subroutine BE_integration

      subroutine interpolate_to_parcel(np,nrkp,pdata_locind,iflag,jflag,kflag,x3d,y3d,z3d,sig3d,uval,vval,wval,tval,qval,rhoval,prsval,xh,xf,yh,yf,zh,zf,zs,znt,sigma,sigmaf,ua,va,wa,ta,qa,rho,prs,sigdot)
      use input
      use constants
      use parcel_module
      use cm1libs , only : eslf
      implicit none

      real, intent(in), dimension(ib:ie+1) :: xf
      real, intent(in), dimension(jb:je+1) :: yf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(in), dimension(ib:ie) :: xh
      real, intent(in), dimension(jb:je) :: yh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(in), dimension(kb:ke) :: sigma
      real, intent(in), dimension(kb:ke+1) :: sigmaf
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke,numq) :: qa
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: ta
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: rho,prs
      real, intent(in), dimension(ib:ie,jb:je) :: znt
      integer, intent(inout), dimension(nparcels,3) :: pdata_locind    ! x/y/z location index of each parcel

      integer, intent(in) :: np,nrkp
      integer, intent(inout) :: iflag,jflag,kflag
      real, intent(inout) :: x3d,y3d,z3d,sig3d,uval,vval,wval,tval,qval,rhoval,prsval,sigdot
      real :: rx,ry,rz,w1,w2,w3,w4,w5,w6,w7,w8,wsum
      real :: rxu,ryv,rzw,rxs,rys,rzs
      real :: zsp,rznt,z0,var
      integer :: i,j,k

      IF( nrkp.eq.1 )THEN
        i=iflag
        j=jflag
      ELSE
        iflag = undefined_index
        jflag = undefined_index
        IF(nx.eq.1)THEN
          iflag = 1
        ELSE
          ! cm1r19:
          call find_horizontal_location_index (pdata_locind(np,1), x3d, ib, ie+1, xf, ni+2, iflag)
        ENDIF

        IF(axisymm.eq.1.or.ny.eq.1)THEN
          jflag = 1
        ELSE
          ! cm1r19:
          call find_horizontal_location_index (pdata_locind(np,2), y3d, jb, je+1, yf, nj+2, jflag)
        ENDIF
        i=iflag
        j=jflag
      ENDIF

        kflag = 1
! JS - jflag could be negative somehow, which will break
!        the find_vertical_location_index subroutine;
!        use the original interface but with the 
!        pdata_locind(:,3) here
!    - z3d could be NaN somehow, reset kflag = 1 to be 
!        consistent with the original implementation
#ifdef _VERIFY_FIND_LOC
        tmp_flag = kflag
#endif
        kflag = max (kflag, pdata_locind(np,3) - location_offset - 1)
        if ( z3d .ne. z3d ) kflag = 1
        if( .not. terrain_flag )then
          if ( z3d.lt.zf(iflag,jflag,kflag) ) then
            kflag = 1
          end if
          do while( z3d.gt.zf(iflag,jflag,kflag+1) )
            kflag = kflag+1
          enddo
          pdata_locind(np,3) = kflag
#ifdef _VERIFY_FIND_LOC
          do while( z3d.gt.zf(iflag,jflag,tmp_flag+1) )
            tmp_flag = tmp_flag+1
          enddo
          if ( kflag .ne. tmp_flag ) then
            print *, "z3d = ", z3d, ", old scheme finds ", zf(iflag,jflag,tmp_flag+1), ", new scheme finds ", zf(iflag,jflag,kflag+1)
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
          pdata_locind(np,3) = kflag
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
      if(np==TROUBLE) then 
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


      end subroutine interpolate_to_parcel


      subroutine find_horizontal_location_index (loc_ind, loc, lb, ub, dsize, end_ind, ind)
      !$acc routine seq

      use input
      use constants

      implicit none

      integer,                      intent(inout) :: loc_ind    ! x/y location index of a parcel
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

      if ( loc_ind .ne. undefined_index ) then
         i = min (end_ind, loc_ind + location_offset)
         do while( ind .lt. 0 .and. i .gt. loc_ind - location_offset - 1 )
            i = i - 1
            if ( loc .ge. dsize(i) .and. loc .le. dsize(i+1) ) then
               ind = i
               loc_ind = ind
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
                  loc_ind = ind
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
               loc_ind = ind
            end if
         end do
      end if

      end subroutine find_horizontal_location_index

      subroutine find_vertical_location_index (loc_ind, loc, lb, ub, dsize, ind, is_ge)
      !$acc routine seq

      use input
      use constants

      implicit none

      integer,                      intent(inout) :: loc_ind    ! z location index of a parcel
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

      if ( loc_ind .ne. undefined_index ) then 
         i = max (ind, loc_ind - location_offset - 1)
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
      loc_ind = ind 

      end subroutine find_vertical_location_index

      subroutine rad_solver2(guess,mflag,prsval,rhoval,tval,qval,Tp,m_s,radius)
      use constants
      use cm1libs , only : eslf
      implicit none

      real, intent(out) :: guess
      integer, intent(out) :: mflag
      real, intent(in) :: prsval,rhoval,tval,qval,Tp,m_s,radius
      real :: a, c, esa, Q, R, M, val, theta, S, T

      mflag = 0
      esa = eslf(prsval,tval)  !Saturation vapor pressure at droplet temperature

      a = -(2*mw*surften)/(ru*rhow*tval)/LOG((ru*tval*rhoval*qval)/(mw*esa))
      c = (ion*os*m_s*(mw/ms))/((4.0/3.0)*pi*rhow)/LOG((ru*tval*rhoval*qval)/(mw*esa))

      Q = (a**2.0)/9.0
      R = (2.0*a**3.0+27.0*c)/54.0
      M = R**2.0-Q**3.0
      val = (R**2.0)/(Q**3.0)

      if (M<0) then
        theta = acos(R/sqrt(Q**3.0))
        guess = -(2*sqrt(Q)*cos((theta-pi*2.0)/3.0))-a/3.0

        if (guess < 0) then
        guess = -(2*sqrt(Q)*cos((theta+pi*2.0)/3.0))-a/3.0
        end if

      else
        S = -(R/abs(R))*(abs(R)+sqrt(M))**(1.0/3.0)
        T = Q/S
        guess = S + T - a/3.0

        if (guess < 0) then
                guess = radius
                mflag = 1
        end if
      end if

      end subroutine rad_solver2

      subroutine gauss_newton_2d(diffnorm,dt_nondim,taup_scale,rt_start,rt_zeros,flag,prsval,rhoval,tval,qval,Tp,m_s,radius)
        implicit none

        real, intent(in) :: diffnorm,dt_nondim,taup_scale,rt_start(2)
        real, intent(in) :: prsval,rhoval,tval,qval,Tp,m_s,radius
        real, intent(out) :: rt_zeros(2)
        integer, intent(out) :: flag

        real :: error,fv1(2),fv2(2),v1(2),rel
        real :: diff, temp1(2), temp2(2), relax, coeff, correct(2)
        real, dimension(1:2, 1:2) :: J,fancy,inv,finalJ
        integer :: iterations,neg,counts

        iterations = 0
        flag = 0
        error = 1.e-8

        v1 = rt_start
        fv2 = (/1., 1./)
        coeff = 0.1
        do while ((sqrt(dot_product(fv2, fv2)) > error) .AND. (iterations<1000))


                iterations = iterations + 1
        !write(*,'(a6,i,4e15.6)') 'DHR4:',iterations,rt_zeros(1),rt_zeros(2),rt_start(1),rt_start(2)

                call ie_vrt_nd(diffnorm,v1(1),v1(2),fv1,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius)
        !write(*,'(a6,i,4e15.6)') 'DHR5:',iterations,v1(1),v1(2),fv1(1),fv1(2)
                call jacob_approx_2d(diffnorm,v1(1),v1(2),dt_nondim,J,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius)

                fancy = matmul(transpose(J),J)

                call inverse_finder_2d(fancy, inv)

                finalJ = matmul(inv, transpose(J))

                correct = matmul(finalJ,fv1)
                rt_zeros = v1 - correct

        !write(*,'(a8,i,4e15.6)') 'DHR5.2:',iterations,J(1,1),J(1,2),J(2,1),J(2,2)
        !write(*,'(a8,i,4e15.6)') 'DHR5.3:',iterations,inv(1,1),inv(1,2),inv(2,1),inv(2,2)
        !write(*,'(a8,i,4e15.6)') 'DHR5.4:',iterations,finalJ(1,1),finalJ(1,2),finalJ(2,1),finalJ(2,2)
        !write(*,'(a8,i,4e15.6)') 'DHR5.5:',iterations,correct(1),correct(2),rt_zeros(1),rt_zeros(2)

                call ie_vrt_nd(diffnorm,v1(1),v1(2),temp1,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius)

        !write(*,'(a6,i,6e15.6)') 'DHR6:',iterations,v1(1),v1(2),temp1(1),temp1(2),rt_zeros(1),rt_zeros(2)

                call ie_vrt_nd(diffnorm,rt_zeros(1),rt_zeros(2),temp2,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius)

        !write(*,'(a6,i,6e15.6)') 'DHR7:',iterations,v1(1),v1(2),temp2(1),temp2(2),rt_zeros(1),rt_zeros(2)

                diff = sqrt(dot_product(temp1,temp1))-sqrt(dot_product(temp2,temp2))

                if (sqrt(dot_product(correct,correct))<error) then
                        EXIT
                end if

                relax = 1.0
                counts = 0

               do while ((diff<0).OR.(rt_zeros(1)<0) .OR. (rt_zeros(2)<0) .OR. isnan(rt_zeros(1)))
                        counts = counts + 1
                        coeff = 0.5
                        relax = relax * coeff
                        rt_zeros = v1-matmul(finalJ,fv1)*relax
                        call ie_vrt_nd(diffnorm,rt_zeros(1),rt_zeros(2),temp2,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius)
        !write(*,'(a6,i,4e15.6)') 'DHR8:',counts,rt_zeros(1),rt_zeros(2),temp2(1),temp2(2)
                        diff = sqrt(dot_product(temp1,temp1))-sqrt(dot_product(temp2,temp2))

                        if (counts>10) EXIT
                end do

                v1 = rt_zeros

                call ie_vrt_nd(diffnorm,rt_zeros(1),rt_zeros(2),fv2,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius)
        end do
      if (iterations == 100) flag = 1
      if (isnan(rt_zeros(1)) .OR. rt_zeros(1)<0 .OR. isnan(rt_zeros(2)) .OR. rt_zeros(2)<0) flag = 1

      end subroutine gauss_newton_2d
      subroutine LV_solver(diffnorm,dt_nondim,taup_scale,rt_start,rt_zeros,flag,prsval,rhoval,tval,qval,Tp,m_s,radius)
        implicit none

        real, intent(in) :: diffnorm,dt_nondim,taup_scale,rt_start(2)
        real, intent(in) :: prsval,rhoval,tval,qval,Tp,m_s,radius
        real, intent(out) :: rt_zeros(2)
        integer, intent(out) :: flag

  end subroutine LV_solver

  subroutine inverse_finder_2d(C, invC)
        implicit none
        real :: det
        real, dimension(1:2, 1:2), intent(in) :: C
        real, dimension(1:2, 1:2), intent(out) :: invC

        det = C(1, 1) * C(2, 2) - C(1, 2) * C(2, 1)

        invC = reshape((/C(2, 2), -C(2,1), -C(1, 2), C(1, 1)/),shape(invC))
        invC = (1./det)*invC

  end subroutine inverse_finder_2d
  subroutine jacob_approx_2d(diffnorm,rnext,tnext,dt_nondim,J,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius)
        implicit none
        integer :: n

        real, intent(in) :: diffnorm,rnext,tnext,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius
        real, intent(out), dimension(1:2, 1:2) :: J
        real :: diff,v_output(3),rt_output(2),xper(2),fxper(2),ynext(2),xper2(2),fxper2(2)

        diff = 1E-5

        ynext(1) = rnext
        ynext(2) = tnext

      !write(*,'(a10,2e15.6)') 'DHR5.01:',rnext,tnext

        call ie_vrt_nd(diffnorm,rnext,tnext,rt_output,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius)

      !write(*,'(a10,2e15.6)') 'DHR5.02:',rt_output(1),rt_output(2)

        xper = ynext
        xper2 = ynext

        do n=1, 2
                xper(n) = xper(n) + diff
                xper2(n) = xper2(n) - diff
                call ie_vrt_nd(diffnorm,xper(1),xper(2),fxper,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius)

      !write(*,'(a10,4e15.6)') 'DHR5.03:',xper(1),xper(2),fxper(1),fxper(2)
                call ie_vrt_nd(diffnorm,xper2(1),xper2(2),fxper2,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius)
      !write(*,'(a10,4e15.6)') 'DHR5.04:',xper2(1),xper2(2),fxper2(1),fxper2(2)

                J(:, n) = (fxper-rt_output)/diff
      !write(*,'(a10,2e15.6)') 'DHR5.05:',fxper(1)-rt_output(1),fxper(2)-rt_output(2)
                xper(n) = ynext(n)
                xper2(n) = ynext(n)
      !write(*,'(a10,4e15.6)') 'DHR5.06:',xper(1),xper(2),xper2(1),xper2(2)
        end do

      !write(*,'(a8,4e15.6)') 'DHR5.1:',J(1,1),J(1,2),J(2,1),J(2,2)

  end subroutine jacob_approx_2d
  subroutine ie_vrt_nd(diffnorm,tempr,tempt,rt_output,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius)
      use constants
      use input
      use cm1libs , only : eslf
      implicit none

      real, intent(in) :: diffnorm,tempr,tempt,taup_scale,dt_nondim,prsval,rhoval,tval,qval,Tp,m_s,radius
      real, intent(out) :: rT_output(2)

      real :: esa,dnext,rhop,Rep,taup,rprime,Tprime,qstr,Shp,Nup,VolP,lhv
      real :: Tnext,rnext


        ! quantities come in already non-dimensionalized, so must be converted back;
        rnext = tempr*radius
        Tnext = tempt*Tp
        dnext = rnext*2.0

        esa = eslf(prsval,tval)
        VolP = (4.0/3.0)*pi*rnext**3
        rhop = (m_s + VolP*rhow)/VolP

        Rep = dnext*diffnorm/viscosity
        taup = (rhop*dnext**2)/(18.0*rhoval*viscosity)
        Nup = 2.0 + 0.6*sqrt(Rep)*pr_num**(1.0/3.0)
        Shp = 2.0 + 0.6*sqrt(Rep)*sc_num**(1.0/3.0)

        Nup = 2.0 !DHR
        Shp = 2.0 !DHR

        lhv=lv1-lv2*tval


        !!! Humidity !!!
        qstr = (mw/(ru*Tnext*rhoval))*esa*exp(((lhv*mw/ru)*((1.0/tval)-(1.0/Tnext))) + ((2.0*mw*surften)/(ru*rhow*rnext*Tnext)) - ((ion*os*m_s*(mw/ms))/(VolP*rhop-m_s)))
        !!!!!!!!!!!!!!!!!!

        !!! Radius !!!
        rprime = (1.0/9.0)*(Shp/sc_num)*(rhop/rhow)*(rnext/taup)*(qval - qstr)
        rprime = rprime*(taup_scale/radius)

        !!! Temperature !!!
        Tprime = -(1.0/3.0)*(Nup/pr_num)*(cp/cpl)*(rhop/rhow)/taup*(Tnext-tval) + 3.0*lhv/(rnext*cpl)*rprime*(radius/taup_scale)
        Tprime = Tprime*(taup_scale/Tp)

        rT_output(1) = rnext/radius - 1.0  - dt_nondim*rprime
        rT_output(2) = Tnext/Tp - 1.0  - dt_nondim*Tprime

  end subroutine ie_vrt_nd



!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  END MODULE droplet_module
