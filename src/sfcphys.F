  MODULE sfcphys_module

  implicit none

      real, parameter :: dcd1  =  1.0e-3
      real, parameter :: dcd2  =  2.4e-3
      real, parameter :: dwsp1 =  5.0
      real, parameter :: dwsp2 = 25.0
      real, parameter :: dfac = (dcd2-dcd1)/(dwsp2-dwsp1)

  CONTAINS

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getcecd(xh,u1,v1,s1,za,u10,v10,s10,xland,znt,ust,cd,ch,cq,avgsfcu,avgsfcv,avgsfcs,avgsfct)
      use input
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

      real, intent(in), dimension(ib:ie) :: xh
      real, intent(inout), dimension(ib:ie,jb:je) :: u1,v1,s1
      real, intent(in), dimension(ib:ie,jb:je) :: za
      real, intent(inout), dimension(ibl:iel,jbl:jel) :: u10,v10,s10
      real, intent(in), dimension(ib:ie,jb:je) :: xland
      real, intent(inout), dimension(ib:ie,jb:je) :: znt,ust,cd,ch,cq
      double precision, intent(inout) :: avgsfcu,avgsfcv,avgsfcs,avgsfct

      integer :: i,j,i1,i2,j1,j2,n,nmax
      real :: wsp,wlast,var,rznt,usave,vsave,ssave

!-----------------------------------------------------------------------
!
!  This subroutine determines several important variables at the surface
!  (eg, drag coefficient, roughness length, friction velocity).
!
!  Note:  This is a simple scheme that assumes neutral stability
!
!         For stability effects, use sfcmodel = 2 or 3
!
!  180213:  Rearrange into different sections (no need to iterate in most cases)
!
!-----------------------------------------------------------------------

!!!    print *,'  za = ',za(1,1),za(ni/2,1),za(ni,1)

    nmax = 0

    i1 = 1
    i2 = ni

    j1 = 1
    j2 = nj

!-----------------------------------------------------------------------

    if( use_avg_sfc )then

      i1 = 1
      i2 = 1
      j1 = 1
      j2 = 1

      usave = u1(1,1)
      vsave = v1(1,1)
      ssave = s1(1,1)

      u1(1,1) = avgsfcu
      v1(1,1) = avgsfcv
      s1(1,1) = avgsfcs

    endif

!-----------------------------------------------------------------------
!  cd,z0,ust:

  sfc_options:  &
  IF( set_znt.eq.1 )THEN
    ! specify z0 (roughness length):
    ! note:  set znt array in "init_surface.F"

    !$omp parallel do default(shared)   &
    !$omp private(i,j,wlast,n,var,rznt)
    do j=j1,j2
    do i=i1,i2
      rznt = 1.0/znt(i,j)
      cd(i,j) = ( karman/alog(10.0*rznt) )**2
      var = alog(10.0*rznt)/alog(za(i,j)*rznt)
      u10(i,j) = u1(i,j)*var
      v10(i,j) = v1(i,j)*var
      s10(i,j) = s1(i,j)*var
      ust(i,j) = max( s1(i,j)*karman/alog(za(i,j)*rznt) , 1.0e-6 )
    enddo
    enddo


  ELSEIF( set_ust.eq.1 )THEN  sfc_options
      ! specify ustar (friction velocity):

    !$omp parallel do default(shared)   &
    !$omp private(i,j,wlast,n,var,rznt)
    do j=j1,j2
    do i=i1,i2
      ust(i,j) = cnst_ust
      znt(i,j) = za(i,j)/(exp(max(1.0e-10,s1(i,j))*karman/max(1.0e-10,ust(i,j)))-1.0)
      rznt = 1.0/znt(i,j)
      cd(i,j) = ( karman/alog((10.0+znt(i,j))*rznt) )**2
      var = alog((10.0+znt(i,j))*rznt)/alog((za(i,j)+znt(i,j))*rznt)
      u10(i,j) = u1(i,j)*var
      v10(i,j) = v1(i,j)*var
      s10(i,j) = s1(i,j)*var
    enddo
    enddo

  ELSEIF( cecd.eq.1 )THEN  sfc_options
    ! specify Cd (drag coefficient):

    !$omp parallel do default(shared)   &
    !$omp private(i,j,wlast,n,var,rznt)
    do j=j1,j2
    do i=i1,i2
      cd(i,j) = max(1.0e-10,cnstcd)
      znt(i,j) = 10.0/(exp(karman/sqrt(cd(i,j)))-1.0)
      rznt = 1.0/znt(i,j)
      var = alog((10.0+znt(i,j))*rznt)/alog((za(i,j)+znt(i,j))*rznt)
      u10(i,j) = u1(i,j)*var
      v10(i,j) = v1(i,j)*var
      s10(i,j) = s1(i,j)*var
      ust(i,j) = max( s1(i,j)*karman/alog((za(i,j)+znt(i,j))*rznt) , 1.0e-6 )
    enddo
    enddo

  ELSE  sfc_options
    ! Cd,z0,ust are functions of 10-m windspeed over water ... need to iterate:

    !$omp parallel do default(shared)   &
    !$omp private(i,j,wlast,n,var,rznt)
    do j=j1,j2
    do i=i1,i2
      IF(xland(i,j).gt.1.5)THEN
        !-----------------------------------------
        ! water:  roughness length (z0) is a function of windspeed
        ! use last known z0 for first guess:
        rznt = 1.0/znt(i,j)
        var = alog((10.0+znt(i,j))*rznt)/alog((za(i,j)+znt(i,j))*rznt)
        s10(i,j) = s1(i,j)*var
        wlast = -1000.0
        n = 0
        do while( abs(s10(i,j)-wlast).gt.0.001 )
          n = n + 1
          wlast = s10(i,j)
          IF(cecd.eq.2)THEN
            ! Deacon's formula:  see Rotunno and Emanuel (1987, JAS, p. 547)
            cd(i,j) = 1.1e-3+(4.0e-5*s10(i,j))
          ELSEIF(cecd.eq.3)THEN
            ! based on Fairall et al (2003, JClim) at low wind speeds
            ! based on Donelan et al (2004, GRL) at high wind speeds
            cd(i,j) = dcd1+(s10(i,j)-dwsp1)*dfac
            cd(i,j) = min(cd(i,j),dcd2)
            cd(i,j) = max(cd(i,j),dcd1)
          ENDIF
          znt(i,j) = max( 1.0e-20 , 10.0/(exp(karman/sqrt(cd(i,j)))-1.0) )
          rznt = 1.0/znt(i,j)
          var = alog((10.0+znt(i,j))*rznt)/alog((za(i,j)+znt(i,j))*rznt)
          s10(i,j) = s1(i,j)*var
          if(n.gt.10) print *,'  getcecd:  myid,n,s10 = ',myid,n,s10(i,j)
          if(n.gt.20)then
            call stopcm1
          endif
        enddo
!!!        nmax = max(nmax,n)
        ! end water
        !-----------------------------------------
      ELSE
        !-----------------------------------------
        ! land:  roughness length (z0) is specified ... no need to iterate
        rznt = 1.0/znt(i,j)
        cd(i,j) = ( karman/alog((10.0+znt(i,j))*rznt) )**2
        var = alog((10.0+znt(i,j))*rznt)/alog((za(i,j)+znt(i,j))*rznt)
        s10(i,j) = s1(i,j)*var
        ! end land
        !-----------------------------------------
      ENDIF
      u10(i,j) = u1(i,j)*var
      v10(i,j) = v1(i,j)*var
      ust(i,j) = max( s1(i,j)*karman/alog((za(i,j)+znt(i,j))*rznt) , 1.0e-6 )
    enddo
    enddo

  ENDIF  sfc_options

!-----------------------------------------------------------------------
!  ch,cq:

    IF(isfcflx.eq.1)THEN
      ! surface fluxes of heat/moisture are included ... get exchange coefficients:

      !$omp parallel do default(shared)   &
      !$omp private(i,j,wlast,n,var,rznt)
      do j=j1,j2
      do i=i1,i2
        IF(xland(i,j).gt.1.5)THEN
          !-------
          ! water:
          IF(cecd.eq.1)THEN
            ! constant value (from namelist.input):
            ch(i,j) = cnstce
            cq(i,j) = cnstce
          ELSEIF(cecd.eq.2)THEN
            ! Deacon's formula:  see Rotunno and Emanuel (1987, JAS, p. 547)
            ch(i,j) = 1.1e-3+(4.0e-5*s10(i,j))
            cq(i,j) = 1.1e-3+(4.0e-5*s10(i,j))
          ELSEIF(cecd.eq.3)THEN
            ! Constant, based on Drennan et al. (2007, JAS, p. 1103)
            ch(i,j) = 1.20e-3
            cq(i,j) = 1.20e-3
          ENDIF
          ! end water
          !-------
        ELSE
          !-------
          ! land ... just set Ce to Cd (for now):
          ch(i,j) = cd(i,j)
          cq(i,j) = cd(i,j)
          ! end land
          !-------
        ENDIF
      enddo
      enddo

    ENDIF

!-----------------------------------------------------------------------

    if( use_avg_sfc )then

      do j=1,nj
      do i=1,ni
        u10(i,j) = u10(1,1)
        v10(i,j) = v10(1,1)
        s10(i,j) = s10(1,1)
        znt(i,j) = znt(1,1)
        ust(i,j) = ust(1,1)
        cd(i,j) = cd(1,1)
        ch(i,j) = ch(1,1)
        cq(i,j) = cq(1,1)
      enddo
      enddo

      u1(1,1) = usave
      v1(1,1) = vsave
      s1(1,1) = ssave

    endif

!-----------------------------------------------------------------------

!!!      print *,'  nmax = ',nmax

      if(timestats.ge.1) time_sfcphys=time_sfcphys+mytime()

      end subroutine getcecd


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine sfcflux(dt,ruh,xf,rvh,pi0s,ch,cq,pi0,thv0,th0,rf0,tsk,thflux,qvflux,mavail,   &
                         rho,rf,u1,v1,s1,ppi,tha,qva,qbsfc,psfc,u10,v10,s10,qsfc,znt,rtime)
      use input
      use constants
      use cm1libs , only : rslf
      implicit none

      real, intent(in) :: dt
      real, intent(in), dimension(ib:ie) :: ruh
      real, intent(in), dimension(ib:ie+1) :: xf
      real, intent(in), dimension(jb:je) :: rvh
      real, intent(in), dimension(ib:ie,jb:je) :: pi0s,ch,cq
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: pi0,thv0,th0,rf0
      real, intent(in), dimension(ib:ie,jb:je) :: tsk
      real, intent(inout), dimension(ib:ie,jb:je) :: psfc,thflux,qvflux
      real, intent(in), dimension(ibl:iel,jbl:jel) :: mavail
      real, intent(in), dimension(ib:ie,jb:je) :: u1,v1,s1
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rho,rf,ppi,tha
      real, intent(in), dimension(ibm:iem,jbm:jem,kbm:kem) :: qva
      double precision, intent(inout) :: qbsfc
      real, intent(inout), dimension(ibl:iel,jbl:jel) :: u10,v10,s10,qsfc
      real, intent(inout), dimension(ib:ie,jb:je) :: znt
      real, intent(in) :: rtime

      integer :: i,j
      real :: pisfc,qvsat,tem,shf
      double precision, dimension(nj) :: bud1
      real :: thmag,qvmag,trat1,trat2

!-----------------------------------------------------------------------
!
!  This subroutine calculates surface fluxes of heat and moisture.
!
!-----------------------------------------------------------------------

  !c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c!

  setting_flux:  &
  IF( set_flx.eq.1 )THEN

    ! specified heat fluxes:

    !$omp parallel do default(shared)   &
    !$omp private(i,j)
    DO j=1,nj
    do i=1,ni
      thflux(i,j) = cnst_shflx
    enddo
    ENDDO

    IF( imoist.eq.1 )THEN

      !$omp parallel do default(shared)   &
      !$omp private(i,j)
      DO j=1,nj
      do i=1,ni
        qvflux(i,j) = cnst_lhflx
      enddo
      ENDDO

    ENDIF

  !c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c!

  ELSE

    ! normal code:

!$omp parallel do default(shared)   &
!$omp private(i,j,pisfc,qvsat)
    DO j=1,nj

      !  sensible heat flux:
      do i=1,ni
        pisfc = (psfc(i,j)*rp00)**rovcp
        thflux(i,j)=ch(i,j)*s10(i,j)*(tsk(i,j)/pisfc-th0(i,j,1)-tha(i,j,1))
      enddo

      !  latent heat flux:
      IF(imoist.eq.1)THEN
        do i=1,ni
          qvsat=rslf(psfc(i,j),tsk(i,j))
          qsfc(i,j)=qvsat
          qvflux(i,j)=cq(i,j)*s10(i,j)*(qvsat-qva(i,j,1))*mavail(i,j)
        enddo
      ENDIF

    ENDDO

  ENDIF  setting_flux

  !c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c!

  !-------------------------------------------------------------------------
    ! for certain cases, turn off surface heat flux after specified time:

    IF( testcase.eq.2 )THEN

      ! shear-driven PBL: 
      ! heat flux for t < 3000 s to spin-up turbulence
      ! (Moeng and Sullivan, 1994, JAS)
      if( rtime.gt.3000.0 ) thflux = 0.0

    ELSEIF( testcase.eq.6 )THEN

      ! hurricane PBL  (heat flux for 3000 s to spin-up turbulence)
      ! (Bryan et al, 2017, BLM)
      if( rtime.gt.3000.0 ) thflux = 0.0

    ELSEIF( testcase.eq.7 )THEN

      ! Replace normal fluxes ... use lowest-model-level wind speed:
      ! (VanZanten et al 2011, JAMES)

      !$omp parallel do default(shared)   &
      !$omp private(i,j,pisfc,qvsat)
      DO j=1,nj
        do i=1,ni
          pisfc = (psfc(i,j)*rp00)**rovcp
          thflux(i,j) = 0.001094*s1(i,j)*(tsk(i,j)/pisfc-th0(i,j,1)-tha(i,j,1))
        enddo
        IF(imoist.eq.1)THEN
          do i=1,ni
            qvsat=rslf(psfc(i,j),tsk(i,j))
            qsfc(i,j)=qvsat
            qvflux(i,j) = 0.001133*s1(i,j)*(qvsat-qva(i,j,1))*mavail(i,j)
          enddo
        ENDIF
      ENDDO

    ELSEIF( testcase.eq.11 )THEN

      thmag = 0.1
      qvmag = 0.15e-3

      if( rtime.le.3600.0 ) thmag = 0.0
      if( rtime.le.7200.0 ) qvmag = 0.0

      trat1 = ( rtime - 3600.0 ) / ( timax - 3600.0 )
      trat2 = ( rtime - 7200.0 ) / ( timax - 7200.0 )

      ! SAS
      thflux = thmag*sin( pi*trat1 )
      qvflux = qvmag*sin( pi*trat2 )

    ENDIF

  !-------------------------------------------------------------------------

    IF(imoist.eq.1)THEN
      ! some budget calculations:
!$omp parallel do default(shared)   &
!$omp private(i,j)
      do j=1,nj
        bud1(j)=0.0d0
        do i=1,ni
          bud1(j)=bud1(j)+qvflux(i,j)*ruh(i)*rvh(j)*rf(i,j,1)
        enddo
      enddo
      tem = dt*dx*dy
      do j=1,nj
        qbsfc=qbsfc+bud1(j)*tem
      enddo
    ENDIF

!-----------------------------------------------------------------------

      if(timestats.ge.1) time_sfcphys=time_sfcphys+mytime()

      end subroutine sfcflux


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine sfcdiags(tsk,thflux,qvflux,cd,ch,cq,u1,v1,s1,wspd,        &
                          xland,psfc,qsfc,u10,v10,hfx,qfx,cda,znt,gz1oz0,  &
                          psim,psih,br,zol,mol,hpbl,dsxy,th2,t2,q2,fm,fh,  &
                          zs,za,pi0s,pi0,th0,ppi,tha,rho,rf,qa)
      use input
      use constants
      use cm1libs , only : rslf
      implicit none

      real, intent(in), dimension(ib:ie,jb:je) :: tsk,thflux,qvflux,   &
                                                  cd,ch,cq,u1,v1,s1,xland,psfc
      real, intent(inout), dimension(ibl:iel,jbl:jel) :: qsfc,u10,v10,hfx,qfx,wspd, &
                                    cda,gz1oz0,psim,psih,br,zol,mol,hpbl,dsxy,th2,t2,q2,fm,fh
      real, intent(inout), dimension(ib:ie,jb:je) :: znt
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(in), dimension(ib:ie,jb:je) :: za
      real, intent(in), dimension(ib:ie,jb:je) :: pi0s
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: pi0,th0,ppi,tha,rho,rf,qa

      integer :: i,j
      real :: pisfc,thgb,thx,thvx,tskv,govrth,dthvdz,vconv,vsgd,dthvm,   &
              val,fluxc,rznt

      REAL    , PARAMETER ::  VCONVC=1.
      REAL    , PARAMETER ::  CZO=0.0185
      REAL    , PARAMETER ::  OZO=1.59E-5
      real :: ep1


      EP1 = rv/rd - 1.0

      ! surface layer diagnostics:

!$omp parallel do default(shared)   &
!$omp private(i,j,pisfc,thgb,thx,thvx,tskv,govrth,dthvdz,vconv,vsgd,   &
!$omp dthvm,val,fluxc,rznt)
      do j=1,nj
      do i=1,ni
        pisfc = (psfc(i,j)*rp00)**rovcp
        thgb = tsk(i,j)/pisfc
        thx = th0(i,j,1)+tha(i,j,1)
        thvx = thx*(1.+EP1*qa(i,j,1))
        qsfc(i,j) = rslf(psfc(i,j),tsk(i,j))
        tskv = thgb*(1.0+ep1*qsfc(i,j))
        govrth = g/thx
        rznt = 1.0/znt(i,j)
        gz1oz0(i,j) = alog((za(i,j)+znt(i,j))*rznt)
        DTHVDZ = THVX-TSKV
        ! cm1r18:  use same formulation over land and water:
!!!        if (xland(i,j).lt.1.5) then
          ! land:
          fluxc = max(thflux(i,j) + ep1*tskv*qvflux(i,j),0.)
          VCONV = vconvc*(g/tsk(i,j)*hpbl(i,j)*fluxc)**.33
!!!        else
!!!          ! ocean:
!!!          IF(-DTHVDZ.GE.0)THEN
!!!            DTHVM=-DTHVDZ
!!!          ELSE
!!!            DTHVM=0.
!!!          ENDIF
!!!          VCONV = 2.*SQRT(DTHVM)
!!!        endif
! Mahrt and Sun low-res correction
        VSGD = 0.32 * (max(dsxy(i,j)/5000.-1.,0.))**.33
        wspd(i,j) = sqrt( s1(i,j)*s1(i,j) + vconv*vconv + vsgd*vsgd )
        wspd(i,j) = max(0.1,wspd(i,j))
        br(i,j) = govrth*za(i,j)*DTHVDZ/(wspd(i,j)**2)
        hfx(i,j) = thflux(i,j)*cp*rf(i,j,1)
        qfx(i,j) = qvflux(i,j)*rf(i,j,1)
        cda(i,j) = cd(i,j)
        ! impose neutral sfc layer:
        psim(i,j) = 0.0
        psih(i,j) = 0.0
        zol(i,j) = 0.0
        mol(i,j) = 0.0
        fm(i,j) = GZ1OZ0(i,j)
        fh(i,j) = GZ1OZ0(i,j)
        ! get 2-m th/q/t:
        val = alog((2.0+znt(i,j))*rznt)/alog((za(i,j)+znt(i,j))*rznt)
        th2(i,j) = thgb+(thx-thgb)*val
        q2(i,j) = qsfc(i,j)+(qa(i,j,1)-qsfc(i,j))*val
        t2(i,j) = th2(i,j)*pisfc
      enddo
      enddo

      if(timestats.ge.1) time_sfcphys=time_sfcphys+mytime()
      end subroutine sfcdiags


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine gethpbl(zh,th0,tha,qa,hpbl)
      use input
      use constants
      implicit none

      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh,th0,tha,qa
      real, intent(inout), dimension(ibl:iel,jbl:jel) :: hpbl

      integer :: i,j,kk
      real :: thx,thvx,thv,thvlast,thcrit,ep1

      EP1 = rv/rd - 1.0

      ! (NEEDED BY SFCLAY ... THIS IS A ROUGH ESTIMATE ONLY)
      ! (ONLY NEEDED WHEN IPBL=0)
      ! (USE WITH CAUTION)
      ! extraordinarily simple calculation:  define pbl depth as 
      ! level where thv is first greater than thv at lowest model level
      ! 110104:  add 0.5 K, for the sake of slightly stable PBLs

!$omp parallel do default(shared)   &
!$omp private(i,j,kk,thx,thvx,thv,thvlast,thcrit)
      do j=1,nj
      do i=1,ni
        hpbl(i,j) = 0.0
        kk = 1
        thx = th0(i,j,1)+tha(i,j,1)
        thvx = thx*(1.+EP1*qa(i,j,1))
        thvlast = thvx
        thcrit = thvx+0.5
        do while( hpbl(i,j).lt.1.0e-12 .and. kk.lt.nk )
          kk = kk + 1
          thv = (th0(i,j,kk)+tha(i,j,kk))*(1.0+EP1*qa(i,j,kk))
          if( thv.ge.thcrit )then
            hpbl(i,j) = zh(i,j,kk-1)+(zh(i,j,kk)-zh(i,j,kk-1))   &
                                    *(thcrit-thvlast)/(thv-thvlast)
          endif
          thvlast = thv
        enddo
        if( kk.gt.(nk-1) .or. hpbl(i,j).lt.1.0e-12 ) hpbl(i,j) = 0.0
      enddo
      enddo

      if(timestats.ge.1) time_sfcphys=time_sfcphys+mytime()
      end subroutine gethpbl


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine gethpbl2(psfc,qsfc,thflux,qvflux,ust,tsk,zh,th0,tha,qa,ua,va,thv1,thvsfc,govthv,brilast,hpbl,riout)
      use input
      use constants
      use cm1libs , only : rslf
      implicit none

      real, intent(in), dimension(ib:ie,jb:je) :: psfc
      real, intent(inout), dimension(ibl:iel,jbl:jel) :: qsfc
      real, intent(in), dimension(ib:ie,jb:je) :: thflux,qvflux,ust,tsk
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh,th0,tha,qa
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(inout), dimension(ib:ie,jb:je) :: thv1,thvsfc,govthv,brilast
      real, intent(inout), dimension(ibl:iel,jbl:jel) :: hpbl
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: riout

      integer :: i,j,k,ntot,nloop
      real :: thv,bri,uavg,vavg,thvflux,tmpprt,thf1,thf2,gamfac,ws
      logical :: doit

      ! critical (ie, threshold) bulk Richardson number:
      real, parameter :: bric = 0.125


      ! cm1r19.6: follows formulation in YSU scheme.

      if( imoist.eq.1 )then
        do j=1,nj
        do i=1,ni
          qsfc(i,j) = rslf(psfc(i,j),tsk(i,j))
        enddo
        enddo
      else
        do j=1,nj
        do i=1,ni
          qsfc(i,j) = 0.0
        enddo
        enddo
      endif

      do j=1,nj
      do i=1,ni
        thv = (th0(i,j,1)+tha(i,j,1))*(1.0+repsm1*qa(i,j,1))
        govthv(i,j) = g/thv
      enddo
      enddo

    hloop:  &
    DO nloop = 1 , 2

      k = 1

    if( nloop.eq.1 )then
      do j=1,nj
      do i=1,ni
        thv = (th0(i,j,1)+tha(i,j,1))*(1.0+repsm1*qa(i,j,1))
        uavg = 0.5*(ua(i,j,k)+ua(i+1,j,k))
        vavg = 0.5*(va(i,j,k)+va(i,j+1,k))
        thvsfc(i,j) = tsk(i,j)*((p00/psfc(i,j))**rovcp)*(1.0+repsm1*qsfc(i,j))
        brilast(i,j) = govthv(i,j)*zh(i,j,1)*(thv-thvsfc(i,j))/max( 1.0 , uavg**2 + vavg**2 )
        thv1(i,j) = thv
      enddo
      enddo
    else
      do j=1,nj
      do i=1,ni
        thv = (th0(i,j,1)+tha(i,j,1))*(1.0+repsm1*qa(i,j,1))
        uavg = 0.5*(ua(i,j,k)+ua(i+1,j,k))
        vavg = 0.5*(va(i,j,k)+va(i,j+1,k))
        thf1 = (1.0+repsm1*qa(i,j,1))
        thf2 = repsm1*(th0(i,j,1)+tha(i,j,1))
        thvflux = max( 0.0 , thf1*thflux(i,j) + thf2*qvflux(i,j) )
        ws = ( ust(i,j)**3 + 8.0*karman*0.5*govthv(i,j)*thvflux*hpbl(i,j) )**0.3333333
        ws = min( ws , ust(i,j)*16.0 )
        ws = max( ws , ust(i,j)/5.0 )
        ws = max( ws , 0.0001 )
        gamfac = 6.8/ws
        tmpprt = max( 0.0 , min( gamfac*thflux(i,j) , 3.0 )*thf1    &
                           +min( gamfac*qvflux(i,j) , 2.0e-3 )*thf2 )
        thv1(i,j) = thv + tmpprt
        brilast(i,j) = govthv(i,j)*zh(i,j,1)*(thv-thvsfc(i,j))/max( 1.0 , uavg**2 + vavg**2 )
        riout(i,j,1) = brilast(i,j)
        riout(i,j,nk) = thv1(i,j)
        riout(i,j,nk-1) = ws
      enddo
      enddo
    endif

      doit = .true.
      ntot = 0

      hpbl = -1.0

      do k=2,nk
        if( doit )then
          do j=1,nj
          do i=1,ni
            if( hpbl(i,j).lt.0.0 )then
              thv = (th0(i,j,k)+tha(i,j,k))*(1.0+repsm1*qa(i,j,k))
              uavg = 0.5*(ua(i,j,k)+ua(i+1,j,k))
              vavg = 0.5*(va(i,j,k)+va(i,j+1,k))
              bri = govthv(i,j)*zh(i,j,k)*(thv-thv1(i,j))/max( 1.0 , uavg**2 + vavg**2 )
              riout(i,j,k) = bri
              if( bri.gt.bric )then
                if( abs(bri-brilast(i,j)).lt.0.000001 .or. k.eq.1 )then
                  hpbl(i,j) = zh(i,j,k)
                else
                  hpbl(i,j) = zh(i,j,k-1)+(zh(i,j,k)-zh(i,j,k-1))  &
                                         *(bric-brilast(i,j))  &
                                         /(bri-brilast(i,j))
                endif
                ntot = ntot+1
              endif
              brilast(i,j) = bri
            endif
          enddo
          enddo
        endif
        if( ntot.eq.(ni*nj) ) doit = .false.
      enddo

    ENDDO  hloop

      if(timestats.ge.1) time_sfcphys=time_sfcphys+mytime()
      end subroutine gethpbl2


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine cm1most(u1,v1,s1,t1,tst,thflux,zol,mol,rmol,       &
                         phim,phih,psim,psih,za            ,        &
                         u10,v10,s10,xland,znt,rznt,ust,cd,ch,cq,   &
                         avgsfcu,avgsfcv,avgsfcs,avgsfct,rtime,     &
                         tsk,psfc,wspd,th0,tha)
      use input
      use constants
#ifdef MPI
      use mpi
#endif
      implicit none

    !  simple code for Monin-Obukhov Similarity Theory (MOST) in CM1

      real, intent(inout), dimension(ib:ie,jb:je) :: u1,v1,s1,t1,tst,thflux,zol,mol,rmol,phim,phih,psim,psih
      real, intent(in), dimension(ib:ie,jb:je) :: za
      real, intent(inout), dimension(ibl:iel,jbl:jel) :: u10,v10,s10,wspd
      real, intent(in), dimension(ib:ie,jb:je) :: xland
      real, intent(inout), dimension(ib:ie,jb:je) :: znt,rznt,ust,cd,ch,cq,tsk,psfc
      double precision, intent(inout) :: avgsfcu,avgsfcv,avgsfcs,avgsfct
      real, intent(in) :: rtime
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: th0
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: tha

      integer :: i,j,i1,i2,j1,j2,n,nmax,ntmp
      real :: wsp,var,varm,varh,usave,vsave,ssave,tsave,dt,tem
      real :: qstar,zeta,pisfc,x,y,kgt,gt,vconv
      real :: phim10,phih10,psim10,psih10,ln10dz0,lnzadz0
      real :: ustlast,zetalast

!-----------------------------------------------------------------------

!!!    print *,'  za = ',za(1,1),za(ni/2,1),za(ni,1)

    nmax = 0

    i1 = 1
    i2 = ni

    j1 = 1
    j2 = nj

!-----------------------------------------------------------------------

    if( use_avg_sfc )then

      i1 = 1
      i2 = 1
      j1 = 1
      j2 = 1

      usave = u1(1,1)
      vsave = v1(1,1)
      ssave = s1(1,1)
      tsave = t1(1,1)

      u1(1,1) = avgsfcu
      v1(1,1) = avgsfcv
      s1(1,1) = avgsfcs
      t1(1,1) = avgsfct

    endif

    if( testcase.eq.9 )then
      ! for stable boundary layer test case (Beare et al. 2006, BLM):
      do j=1,nj
      do i=1,ni
        tsk(i,j) = 265.0-0.25*(rtime/3600.0)
      enddo
      enddo
    endif

!-----------------------------------------------------------------------
!  v1.0: - assumes znt = z0t = constant
!        - not accounting for moisture

  IF( imoist.eq.0 )THEN

    ! assumptions:  - roughness length (znt) is known
    !               - temperature roughness length is same as znt
    !               - either surface temp (tsk) is known, or 
    !                 thflux is specified (testcase=1)
    !               - no terrain

    kgt = karman * g / th0(1,1,1)
    gt = g / th0(1,1,1)

    do j=j1,j2
    do i=i1,i2

      qstar = 0.0

      ! guess on z/L:
      zeta = 0.0
      zetalast = -1000.0

      ! assume znt,rznt are known (set in init_surface.F)
      lnzadz0 = alog(za(i,j)*rznt(i,j))

      ust(i,j) = 0.0
      ustlast = -1000.0

      n = 0
      ! iterate on z/L until ust converges:
    iterloop:  &
    do while( abs(ust(i,j)-ustlast).gt.0.0001 .or. n.lt.6 )
      n = n + 1
      ustlast = ust(i,j)
      zetalast = zeta
      call stabil_funcs(zeta,phim(i,j),phih(i,j),psim(i,j),psih(i,j))
      vconv = 0.07*( ( abs(gt*qstar*2.0*za(i,j)) )**0.33333333 )
      wspd(i,j) = s1(i,j)+vconv
      ust(i,j) = max( wspd(i,j)*karman/(lnzadz0-psim(i,j)) , 1.0e-8 )
      if( testcase.eq.1 )then
        ! specify for testcase 1:
        qstar = 0.24
        tst(i,j) = -qstar/ust(i,j)
        ! diagnose tsk:
        tsk(i,j) = t1(i,j)-tst(i,j)*rkarman*(lnzadz0-psih(i,j))
      else
        ! tsk is known:
        tst(i,j) = max( (t1(i,j)-tsk(i,j))*karman/(lnzadz0-psih(i,j)) , 1.0e-30 )
        qstar = -ust(i,j)*tst(i,j)
      endif
      rmol(i,j) = (kgt*tst(i,j))/(ust(i,j)**2)
      zol(i,j) = za(i,j)*rmol(i,j)
      ! new value of zeta:
      zeta = zetalast + 0.5*(zol(i,j)-zetalast)
      if(n.gt.90) print *,'  cm1most: ust,zeta,mol = ',n,ust(i,j),zeta,1.0/(sign( 1.0e-10 , rmol(i,j) )+rmol(i,j))
      if(n.gt.100)then
        call stopcm1
      endif
    enddo  iterloop

      nmax = max(nmax,n)
      zol(i,j) = zeta
      if( abs(rmol(i,j)).le.smeps )then
        mol(i,j) = sign( 1.0e10 , rmol(i,j) )
      else
        mol(i,j) = 1.0/rmol(i,j)
      endif
      thflux(i,j) = -ust(i,j)*tst(i,j)
      if( ust(i,j).lt.1.0e-10 ) stop 13131
      ! wind speed at 10 m:
      zeta = 10.0*rmol(i,j)
      call stabil_funcs(zeta,phim10,phih10,psim10,psih10)
      ln10dz0 = alog(10.0*rznt(i,j))
      var = (ln10dz0-psim10)/(lnzadz0-psim(i,j))
      u10(i,j) = u1(i,j)*var
      v10(i,j) = v1(i,j)*var
      s10(i,j) = s1(i,j)*var
      ! cd,ch valid at 10 m:
      varm = ln10dz0-psim10
      varh = ln10dz0-psih10
      cd(i,j) = (karman/varm)**2
      ch(i,j) = karman*karman/(varm*varh)
      dt = t1(i,j)-tsk(i,j)
    enddo
    enddo

!-----------------------------------------------------------------------

  ELSEIF( imoist.eq.1 )THEN

    stop 11211

  ENDIF

!-----------------------------------------------------------------------

    if( use_avg_sfc )then

      do j=1,nj
      do i=1,ni
        u10(i,j) = u10(1,1)
        v10(i,j) = v10(1,1)
        s10(i,j) = s10(1,1)
        znt(i,j) = znt(1,1)
        ust(i,j) = ust(1,1)
        cd(i,j) = cd(1,1)
        ch(i,j) = ch(1,1)
        cq(i,j) = cq(1,1)
        zol(i,j) = zol(1,1)
        mol(i,j) = mol(1,1)
        rmol(i,j) = rmol(1,1)
        phim(i,j) = phim(1,1)
        phih(i,j) = phih(1,1)
        psim(i,j) = psim(1,1)
        psih(i,j) = psih(1,1)
        tst(i,j) = tst(1,1)
        thflux(i,j) = thflux(1,1)
        tsk(i,j) = tsk(1,1)
        wspd(i,j) = wspd(1,1)
      enddo
      enddo

      u1(1,1) = usave
      v1(1,1) = vsave
      s1(1,1) = ssave
      t1(1,1) = tsave

      IF( nx.ge.3 .and. ny.ge.3 )THEN
        ! Moeng (1984), eqn 30
        tem = avgsfcs*dt
        if( abs(tem).gt.1.0e-10 )then
          tem = thflux(1,1)/tem
          do j=1,nj
          do i=1,ni
            thflux(i,j) = tem*( s1(i,j)*dt + avgsfcs*(t1(i,j)-avgsfct) )
          enddo
          enddo
        endif
      ENDIF

    endif

!-----------------------------------------------------------------------

#ifdef MPI
      ntmp = -1000
      call MPI_REDUCE(nmax,ntmp,1,MPI_INTEGER,MPI_MAX,0,MPI_COMM_WORLD,ierr)
      nmax = ntmp
#endif
!!!      if(myid.eq.0) print *,'  nmax = ',nmax
      nzeta = nmax

      if(timestats.ge.1) time_sfcphys=time_sfcphys+mytime()

      end subroutine cm1most


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine stabil_funcs(zeta,phim,phih,psim,psih)
      implicit none

      real, intent(in) :: zeta
      real, intent(out) :: phim,phih,psim,psih

      real :: x,y

      ! formulations from Hogstrom (1988, BLM, pg 55)
      if( zeta .lt. -1.0e-6 )then
        ! unstable:
        x = sqrt(sqrt(1.0-19.3*zeta))
        phim = 1.0/x
        psim = 2.0*alog(0.5*(1.0+x)) + alog(0.5*(1.0+x*x)) - 2.0*atan(x) + 1.570796327
        y = sqrt(1.0-12.0*zeta)
        phih = 1.0/y
        psih = 2.0*alog(0.5*(1.0+y))
      elseif( zeta .gt. 1.0e-6 )then
        ! stable:
        phim = 1.0 + 4.8*zeta
        phih = 1.0 + 7.8*zeta
        psim = -4.8*zeta
        psih = -7.8*zeta
      else
        ! neutral:
        phim = 1.0
        phih = 1.0
        psim = 0.0 
        psih = 0.0
      endif

      end subroutine stabil_funcs

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getavgsfc(u1,v1,s1,t1,avgsfcu,avgsfcv,avgsfcs,avgsfct)
      use input
#ifdef MPI
      use mpi
#endif
      implicit none

      real, intent(in), dimension(ib:ie,jb:je) :: u1,v1,s1,t1
      double precision, intent(inout) :: avgsfcu,avgsfcv,avgsfcs,avgsfct

      integer :: i,j
      double precision :: temd

!-----------------------------------------------------------------------

      avgsfcu = 0.0
      avgsfcv = 0.0
      avgsfcs = 0.0
      avgsfct = 0.0

      do j=1,nj
      do i=1,ni
        avgsfcu = avgsfcu + u1(i,j)
        avgsfcv = avgsfcv + v1(i,j)
        avgsfcs = avgsfcs + s1(i,j)
        avgsfct = avgsfct + t1(i,j)
      enddo
      enddo

#ifdef MPI
      call MPI_ALLREDUCE(MPI_IN_PLACE,avgsfcu,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,avgsfcv,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,avgsfcs,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,avgsfct,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif

      temd = 1.0/dble(nx*ny)
      avgsfcu = avgsfcu*temd
      avgsfcv = avgsfcv*temd
      avgsfcs = avgsfcs*temd
      avgsfct = avgsfct*temd

      if(timestats.ge.1) time_sfcphys=time_sfcphys+mytime()

      end subroutine getavgsfc


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  END MODULE sfcphys_module
