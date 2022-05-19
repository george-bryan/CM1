#ifdef _B4B

#define _B4B16R
#define _B4B19R
#define _B4B21R
#define _B4B22R
#define _B4B23R
#define _B4B24R
#define _B4B25R
#define _B4B26R
#define _B4B27RF
#define _B4B28R
#define _B4B29R
#define _B4B30
#else
!Appears to be a bug in the B4B28 loop
#undef _B4B28R
#endif
  MODULE domaindiag_module

  implicit none

  private
  public :: domaindiag

  integer, parameter :: varmax = 10000

  integer, parameter :: fnums = 67
  integer, parameter :: fnumw = 68

  integer :: ncid,wloop

  CONTAINS

!-------------------------------------------------------------------------------
!
!  domaindiag:  subroutine to calculate domain-wide diagnostics
!
!       Notes:  - reference profiles are domain-averages at height levels
!               - code currently does not account for terrain
!
!-------------------------------------------------------------------------------

    subroutine domaindiag(nstep,mtime,nwritet,trecs,trecw,qname,qunit,dt,dosfcflx, &
                   xh,rxh,arh1,arh2,uh,ruh,xf,rxf,arf1,arf2,uf,ruf,            &
                   yh,vh,rvh,yf,vf,rvf,                                        &
                   xfref,yfref,rds,sigma,rdsf,sigmaf,                          &
                   wprof,ufrc,vfrc,thfrc,qvfrc,ug,vg,dvdr,                     &
                   uavg,vavg,thavg,pavg,ulspg,vlspg,qavg,cavg,cloudvar,        &
                   tauh,taus,zh,mh,rmh,c1,c2,tauf,zf,mf,rmf,                   &
                   rho0s,pi0s,prs0s,rth0s,                                     &
                   pi0,rho0,prs0,thv0,th0,rth0,qv0,qc0,u0,v0,                  &
                   qi0,rr0,rf0,rrf0,                                           &
                   zs,gz,rgz,gzu,rgzu,gzv,rgzv,dzdx,dzdy,gx,gxu,gy,gyv,        &
                   tsk,znt,ust,stau,tst,qst,z0t,z0q,thflux,qvflux,             &
                   cd,ch,cq,u1,v1,s1,xland,psfc,tlh,prate,ustt,ut,vt,st,       &
                   dum1,dum2,dum3,dum4,dum5,dum6,th  ,ql  ,                    &
                   divx,rho,rr,rf,prs,t11,t12,t13,t22,t23,t33,                 &
                   m11,m12,m13,m22,m23,m33,                                    &
                   rru,u3d,utk ,utmp ,                                         &
                   rrv,v3d,vtk ,vtmp ,                                         &
                   rrw,w3d,wtk ,wtmp ,                                         &
                   pp3d,sadv,ppten ,qice,th3d,thv  ,thl   ,buoy  ,             &
                   q3d,qten,kmh,kmv,khh,khv,tkea,tke3d,tketen,                 &
                   nm,defv,defh,lenscl,dissten,epst,epsd1,epsd2,pt3d,          &
                   rain,hfx,qfx,u10,v10,s10,t2,q2,th2,mznt,                    &
                   hpbl,wspd,zol,mol,rmol,br,brcr,phim,phih,psim,psih,         &
                   thpten,qvpten,qcpten,qipten,upten,vpten,qnipten,qncpten,xkzh,xkzq,xkzm, &
                   qsfc,o30,zir,swten,lwten,swtenc,lwtenc,cldfra,              &
                   effc,effi,effs,effr,effg,effis,                             &
                   lwupt,lwuptc,lwdnt,lwdntc,lwupb,lwupbc,lwdnb,lwdnbc,        &
                   swupt,swuptc,swdnt,swdntc,swupb,swupbc,swdnb,swdnbc,        &
                   lwcf,swcf,                                                  &
                   tsq,qsq,cov,sh3d,el_pbl,qc_bl,qi_bl,cldfra_bl,              &
                   qWT,qSHEAR,qBUOY,qDISS,dqke,qke_adv,qke,                    &
                   edmf_a,edmf_w,edmf_qt,edmf_thl,edmf_ent,edmf_qc,            &
                   vdfg,maxmf,nupdraft,ktop_plume,                             &
                   thz0,qz0,uz0,vz0,tke_myj,el_myj,                            &
                   tdiag,qdiag,udiag,vdiag,wdiag,kdiag,out2d,out3d,            &
                   getdbz,getvt,                                               &
                   gamk,gamwall,kmw,u1b,v1b,l2p,s2p,s2b,t2pm1,t2pm2,t2pm3,t2pm4,cmz,csz,ce1z,ce2z, &
                   lsnudge_u,lsnudge_v,lsnudge_th,lsnudge_qv,                  &
                   sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,flag)
        ! end_domaindiag
    use input
    use constants
    use cm1libs , only : rslf,rsif
    use getcape_module
    use sfcphys_module , only : stabil_funcs
    use turb_module , only : getepst
    use lsnudge_module
#ifdef NETCDF
    use writeout_nc_module, only : writediag_nc
#endif
#ifdef MPI
    use mpi
#endif
    implicit none

    integer, intent(in) :: nstep
    double precision, intent(in) :: mtime
    integer, intent(in) :: nwritet
    integer, intent(inout) :: trecs,trecw
    character(len=3), intent(in), dimension(maxq) :: qname
    character(len=20), intent(in), dimension(maxq) :: qunit
    real, intent(inout) :: dt
    logical, intent(in) :: dosfcflx
    real, intent(in), dimension(ib:ie) :: xh,rxh,arh1,arh2,uh,ruh
    real, intent(in), dimension(ib:ie+1) :: xf,rxf,arf1,arf2,uf,ruf
    real, intent(in), dimension(jb:je) :: yh,vh,rvh
    real, intent(in), dimension(jb:je+1) :: yf,vf,rvf
    real, intent(in), dimension(1-ngxy:nx+ngxy+1) :: xfref
    real, intent(in), dimension(1-ngxy:ny+ngxy+1) :: yfref
    real, intent(in), dimension(kb:ke) :: rds,sigma
    real, intent(in), dimension(kb:ke+1) :: rdsf,sigmaf
    real, intent(inout), dimension(kb:ke) :: wprof,ufrc,vfrc,thfrc,qvfrc,ug,vg,dvdr,  &
                                             uavg,vavg,thavg,pavg,ulspg,vlspg
    real, intent(inout), dimension(kb:ke,numq) :: qavg
    double precision, intent(inout), dimension(kb:ke,3+numq) :: cavg
    logical, intent(in), dimension(maxq) :: cloudvar
    real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: tauh,taus,zh,mh,rmh,c1,c2
    real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: tauf,zf,mf,rmf
    real, intent(in), dimension(ib:ie,jb:je) :: rho0s,pi0s,prs0s,rth0s
    real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: pi0,rho0,prs0,thv0,th0,rth0,qv0,qc0
    real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: u0
    real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: v0
    real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: qi0,rr0,rf0,rrf0
    real, intent(in), dimension(ib:ie,jb:je) :: zs
    real, intent(in), dimension(itb:ite,jtb:jte) :: gz,rgz,gzu,rgzu,gzv,rgzv,dzdx,dzdy
    real, intent(in), dimension(itb:ite,jtb:jte,ktb:kte) :: gx,gxu,gy,gyv
    real, intent(in), dimension(ib:ie,jb:je) :: tsk,znt,ust,stau,tst,qst,z0t,z0q,thflux,qvflux,  &
                                                cd,ch,cq,u1,v1,s1,xland,psfc,tlh,prate,ustt,ut,vt,st


    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2,dum3,dum4,dum5,dum6,th,ql
    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: divx,rho,rr,rf,prs
    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: t11,t12,t13,t22,t23,t33
    real, intent(in), dimension(ibnba:ienba,jbnba:jenba,kbnba:kenba) :: m11,m12,m13,m22,m23,m33
    real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: rru,u3d,utk,utmp
    real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: rrv,v3d,vtk,vtmp
    real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: rrw,w3d,wtk,wtmp
    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: pp3d,sadv,ppten,qice
    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: th3d,thv,thl,buoy
    real, intent(inout), dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: q3d,qten
    real, intent(inout), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv,khh,khv
    real, intent(inout), dimension(ibt:iet,jbt:jet,kbt:ket) :: tkea,tke3d,tketen
    real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: nm,defv,defh,lenscl,dissten,epst,epsd1,epsd2
    real, intent(in), dimension(ibp:iep,jbp:jep,kbp:kep,npt) :: pt3d
    real, intent(in), dimension(ib:ie,jb:je,nrain) :: rain
    real, intent(in), dimension(ibl:iel,jbl:jel) :: hfx,qfx,u10,v10,s10,t2,q2,th2,mznt
    real, intent(in), dimension(ibl:iel,jbl:jel) :: hpbl,wspd,zol,mol,rmol,br,brcr,phim,phih,psim,psih,qsfc
    real, intent(in), dimension(ibb:ieb,jbb:jeb,kbb:keb) :: thpten,qvpten,qcpten,qipten,upten,vpten
    real, intent(in), dimension(ibb:ieb,jbb:jeb,kbb:keb) :: xkzh,xkzq,xkzm
    real, intent(in), dimension(ibr:ier,jbr:jer,kbr:ker) :: o30
    real, intent(in), dimension(ibr:ier,jbr:jer) :: zir
    real, intent(in), dimension(ibr:ier,jbr:jer,kbr:ker) :: swten,lwten,swtenc,lwtenc,cldfra
    real, intent(in), dimension(ibr:ier,jbr:jer,kbr:ker) :: effc,effi,effs,effr,effg,effis
    real, intent(in), dimension(ibr:ier,jbr:jer) :: lwupt,lwuptc,lwdnt,lwdntc,lwupb,lwupbc,lwdnb,lwdnbc
    real, intent(in), dimension(ibr:ier,jbr:jer) :: swupt,swuptc,swdnt,swdntc,swupb,swupbc,swdnb,swdnbc
    real, intent(inout), dimension(ibr:ier,jbr:jer) :: lwcf,swcf
    real, intent(inout), dimension(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) :: tsq,qsq,cov,sh3d,el_pbl,qc_bl,qi_bl,cldfra_bl, &
                                                                qnipten,qncpten,qWT,qSHEAR,qBUOY,qDISS,dqke,qke_adv,qke,  &
                                                                edmf_a,edmf_w,edmf_qt,edmf_thl,edmf_ent,edmf_qc
 
    real, intent(inout), dimension(ibmynn:iemynn,jbmynn:jemynn) :: vdfg,maxmf
    integer, intent(inout), dimension(ibmynn:iemynn,jbmynn:jemynn) :: nupdraft,ktop_plume
    real, intent(inout), dimension(ibmyj:iemyj,jbmyj:jemyj) :: thz0,qz0,uz0,vz0
    real, intent(inout), dimension(ibmyj:iemyj,jbmyj:jemyj,kbmyj:kemyj) :: tke_myj,el_myj
    logical, intent(in) :: getdbz,getvt
    real, intent(in), dimension(kb:ke) :: gamk,gamwall,kmw,u1b,v1b,l2p,s2p,s2b,t2pm1,t2pm2,t2pm3,t2pm4,cmz,csz,ce1z,ce2z
    real, intent(inout), dimension(lsnudge_maxlevels,lsnudge_nmax) :: lsnudge_u,lsnudge_v,lsnudge_th,lsnudge_qv
    real, intent(inout), dimension(cmp,jmp,kmp)   :: sw31,sw32,se31,se32
    real, intent(inout), dimension(imp,cmp,kmp)   :: ss31,ss32,sn31,sn32
    logical, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: flag
    real, intent(in) , dimension(ibdt:iedt,jbdt:jedt,kbdt:kedt,ntdiag) :: tdiag
    real, intent(in) , dimension(ibdq:iedq,jbdq:jedq,kbdq:kedq,nqdiag) :: qdiag
    real, intent(in) , dimension(ibdv:iedv,jbdv:jedv,kbdv:kedv,nudiag) :: udiag
    real, intent(in) , dimension(ibdv:iedv,jbdv:jedv,kbdv:kedv,nvdiag) :: vdiag
    real, intent(in) , dimension(ibdv:iedv,jbdv:jedv,kbdv:kedv,nwdiag) :: wdiag
    real, intent(in) , dimension(ibdk:iedk,jbdk:jedk,kbdk:kedk,nkdiag) :: kdiag
    real, intent(inout), dimension(ib2d:ie2d,jb2d:je2d,nout2d) :: out2d
    real, intent(in) , dimension(ib3d:ie3d,jb3d:je3d,kb3d:ke3d,nout3d) :: out3d

    !---------------------------------------------------------------------------

    integer :: i,j,k,n,nvar,nvar2d,nfile,ncb,nrb,nlb,nct,nrt,nlt,nrk,diffit,nloop,nn,ntmp,wloopmax
    integer :: qflag,pdef,pdefweno
    real :: zi,zimin,zimax,zitb,dtdz,tmax,ustbar,zntbar,molbar,tem,tem1,nmavg,tk
    real :: qx,px,tx,tlcl,ee,tlast,prinv,tphim,tphih,zeta,tpsim,tpsih
    real, dimension(:), allocatable :: pfoo,tfoo,qfoo
    real :: zlcl, zlfc, zel , psource , tsource , qvsource
    double precision :: savg2d,temd,cwp,rwp,lwp,qcfrac,qrfrac,qlfrac,qifrac,qtfrac,    &
                        zibar,qcb,qrb,qlb,qct,qrt,qlt,weps,thfavg,qvfavg,wstmp,wstar,  &
                        fbmin,zfbmin,bfoo
    double precision, dimension(nk+1) :: ravg,savg,tavg,uub,vvb,wwb,sfr,sfs,sfd,tmass,clw,cli,plw,pli
    double precision, dimension(nk+1) :: ufr,ufs,ufd,vfr,vfs,vfd,ptfr,ptfs,ptfd,qvfr,qvfs,qvfd
    real, dimension(0:nk+1) :: wavg,prsavg,piavg,thvavg,thlavg,rhoavg,rfavg,qcrit,  &
                               qvavg,qlavg,qtavg,wsp,wspa,stke,rtke,wvar,temavg,  &
                               u_hturb,u_vturb,u_hadv,u_vadv,u_pgrad,u_lsw,u_cor,u_hidiff,u_vidiff,u_rdamp,     &
                               v_hturb,v_vturb,v_hadv,v_vadv,v_pgrad,v_lsw,v_cor,v_hidiff,v_vidiff,v_rdamp,     &
                               w_hturb,w_vturb,w_hadv,w_vadv,w_pgrad,w_lsw,w_buoy,w_hidiff,w_vidiff,w_rdamp
    real, dimension(0:nk+1,npt) :: ptavg
    character(len=80), dimension(varmax) :: varname,vardesc,varunit
    integer, dimension(varmax) :: varlvls
    character(len=1),  dimension(varmax) :: vargrid
    character(len=80) :: a1,a2
    character(len=16) :: a16
    logical :: saveit,doit,do2

    real :: thvx
    real :: bri,ubar,vbar,thvflux,tmpprt,thf1,thf2,gamfac,ws
    real, dimension(ib:ie,jb:je) :: tmpqsfc,govthv,thvsfc,thv1
    double precision :: tmasst,clwt,clit,plwt,plit,tmpw,tmpi

    !$acc data create (ravg,savg,tavg,uub,vvb,wwb,sfr,sfs,sfd,   &
    !$acc              tmass,clw,cli,plw,pli,wavg,prsavg,piavg,  &
    !$acc              wvar,thvavg,rhoavg,rfavg,ufr,ufs,ufd,vfr, &
    !$acc              vfs,vfd,ptfr,ptfs,ptfd,qvfr,qvfs,qvfd,    &
    !$acc              temavg,qvavg,qlavg,qtavg,thlavg,wsp,wspa, &
    !$acc              stke,rtke,qcrit,u_hturb,u_vturb,u_hadv,   &
    !$acc              u_vadv,u_pgrad,u_cor,u_hidiff,u_vidiff,   &
    !$acc              u_rdamp,v_hturb,v_vturb,v_hadv,v_vadv,    &
    !$acc              v_pgrad,v_cor,v_hidiff,v_vidiff,v_rdamp,  &
    !$acc              w_hturb,w_vturb,w_hadv,w_vadv,w_pgrad,    &
    !$acc              w_buoy,w_hidiff,w_vidiff,w_rdamp)

    if( myid.eq.0 ) print *
    if( myid.eq.0 ) print *,' ..... begin domaindiag code ..... '
    if( myid.eq.0 ) print *

    !$acc parallel loop gang vector default(present) private(k)
    do k=kb,ke
      uavg(k) = 0.0
      vavg(k) = 0.0
    enddo

    !$acc parallel loop gang vector default(present) private(k)
    do k=0,nk+1
      wavg(k)     = 0.0
      prsavg(k)   = 0.0
      piavg(k)    = 0.0
      thvavg(k)   = 0.0
      thlavg(k)   = 0.0
      rhoavg(k)   = 0.0
      rfavg(k)    = 0.0
      qvavg(k)    = 0.0
      qlavg(k)    = 0.0
      qtavg(k)    = 0.0
      wsp(k)      = 0.0
      wspa(k)     = 0.0
      stke(k)     = 0.0
      rtke(k)     = 0.0
      wvar(k)     = 0.0
      u_hadv(k)   = 0.0
      v_hadv(k)   = 0.0
      w_hadv(k)   = 0.0
      u_hidiff(k) = 0.0
      v_hidiff(k) = 0.0
      w_hidiff(k) = 0.0
      v_vidiff(k) = 0.0
      u_vidiff(k) = 0.0
      w_vidiff(k) = 0.0
      temavg(k)   = 0.0
    enddo

    !$acc parallel loop gang vector default(present) private(k)
    do k=1,nk+1
      ptfr(k) = 0.0
      ptfs(k) = 0.0
      ptfd(k) = 0.0
      sfr(k)      = 0.0
      sfs(k)      = 0.0
      sfd(k)      = 0.0
    enddo

    ! by default, assume 3d and scalar levels:
    varlvls = nk
    vargrid = 's'

    thfavg = 0.0
    qvfavg = 0.0
    fbmin = 0.0
    zfbmin = 0.0
    wstar = 0.0

    ustbar = 0.0
    zntbar = 0.0
    molbar = 0.0

    !---------------------------------------------------------------------------
    ! open file:

      if( myid.eq.0 ) print *,'  nwritet = ',nwritet

      IF( output_format.eq.1 )THEN

        ! grads-format

        do i=1,maxstring
          string(i:i) = ' '
        enddo

        if( myid.eq.0 )then

          string = 'cm1out_diag_XXXXXX_s.dat'
          write(string(13:18),151) nwritet
          print *,string
          open(unit=fnums,file=string,form='unformatted',access='direct',recl=4)

          string = 'cm1out_diag_XXXXXX_w.dat'
          write(string(13:18),151) nwritet
          print *,string
          open(unit=fnumw,file=string,form='unformatted',access='direct',recl=4)

        endif

151     format(i6.6)

        wloopmax = 1

      ELSEIF( output_format.eq.2 )THEN

        ! netcdf-format
#ifdef NETCDF

        do i=1,maxstring
          string(i:i) = ' '
        enddo

        string = 'cm1out_diag_XXXXXX.nc'
        write(string(13:18),151) nwritet
        if( myid.eq.0 ) print *,string

        wloopmax = 2

#endif
      ELSE

        print *
        print *,'  output_format = ',output_format
        print *
        print *,'  Invalid value for output_format in domaindiag '
        print *
        call stopcm1

      ENDIF

        trecs = 1
        trecw = 1

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    ! misc prep:     th  =  potential temperature (theta)
    !                ql  =  total liquid water (usually qc+qr)

    IF( imoist.eq.0 )THEN
      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        ql(i,j,k)   = 0.0
        qice(i,j,k) = 0.0
      enddo
      enddo
      enddo
    ELSE
      do k=1,nk
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          ql(i,j,k) = 0.0
        enddo
        enddo
      IF( nql1.gt.0 )THEN
        do n=nql1,nql2
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          ql(i,j,k) = ql(i,j,k)+q3d(i,j,k,n)
        enddo
        enddo
        enddo
      ENDIF
        if( iice.eq.1 )then
          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            qice(i,j,k) = 0.0
          enddo
          enddo
          do n=nqs1,nqs2
          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            qice(i,j,k) = qice(i,j,k)+q3d(i,j,k,n)
          enddo
          enddo
          enddo
        endif
      enddo
    ENDIF

    IF( imoist.eq.1 )THEN

      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
      do k=1,nk
      do j=0,nj+1
      do i=0,ni+1
        th(i,j,k) = th0(i,j,k)+th3d(i,j,k)
        thv(i,j,k) = th(i,j,k)*(1.0+repsm1*q3d(i,j,k,nqv)-ql(i,j,k)-qice(i,j,k))
        dum1(i,j,k) = sqrt( (0.5*(u3d(i,j,k)+u3d(i+1,j,k))+umove)**2 &
                           +(0.5*(v3d(i,j,k)+v3d(i,j+1,k))+vmove)**2 )
      enddo
      enddo
      enddo

    ELSE

      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
      do k=1,nk
      do j=0,nj+1
      do i=0,ni+1
        th(i,j,k) = th0(i,j,k)+th3d(i,j,k)
        thv(i,j,k) = th(i,j,k)
        dum1(i,j,k) = sqrt( (0.5*(u3d(i,j,k)+u3d(i+1,j,k))+umove)**2 &
                           +(0.5*(v3d(i,j,k)+v3d(i,j+1,k))+vmove)**2 )
      enddo
      enddo
      enddo

    ENDIF

      !c-c-c-c-c-c-c-c-c-c

      call getavg3d(th,savg)
      !$acc parallel loop default(present) private(k)
      do k=1,nk
        thavg(k) = savg(k)
      enddo
      

      call getavg3d(thv,savg)
      !$acc parallel loop default(present) private(k)
      do k=1,nk
        thvavg(k) = savg(k)
      enddo

      call getavg3d(prs,savg)
      !$acc parallel loop default(present) private(k)
      do k=1,nk
        pavg(k) = savg(k)
      enddo

      call getavg3d(dum1,savg)
      !$acc parallel loop default(present) private(k)
      do k=1,nk
        wsp(k) = savg(k)
      enddo

      !c-c-c-c-c-c-c-c-c-c

  bigwloop:  &
  DO wloop = 1 , wloopmax

      if( myid.eq.0 ) print *,'  wloop = ',wloop

      nvar = 0

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  2d variables:

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'mtime'
        vardesc(nvar) = 'model time (since beginning of sim)'
        varunit(nvar) = 's'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'

        savg2d = mtime
        call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'nstep'
        vardesc(nvar) = 'time steps (since beginning of sim)'
        varunit(nvar) = 'unitless'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'

        savg2d = nstep
        call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'umove'
        vardesc(nvar) = 'umove'
        varunit(nvar) = 'm/s'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'

        savg2d = umove
        call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'vmove'
        vardesc(nvar) = 'vmove'
        varunit(nvar) = 'm/s'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'

        savg2d = vmove
        call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)


      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'ust'
      vardesc(nvar) = 'surface friction velocity'
      varunit(nvar) = 'm/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( ust  ,savg2d)
      ustbar = savg2d
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'stau'
      vardesc(nvar) = 'surface stress'
      varunit(nvar) = 'm2/s2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( stau  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

    IF( sfcmodel.eq.4 )THEN

      nvar = nvar+1
      varname(nvar) = 'znt'
      vardesc(nvar) = 'GFDL thermal roughness length (NOTE: unusual naming convention)'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( znt  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'mznt'
      vardesc(nvar) = 'GFDL surface roughness length (NOTE: unusual naming convention)'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( mznt  ,savg2d)
      zntbar = max( 1.0e-10 , savg2d )
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

    ELSE

      nvar = nvar+1
      varname(nvar) = 'znt'
      vardesc(nvar) = 'surface roughness length'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( znt  ,savg2d)
      zntbar = max( 1.0e-10 , savg2d )
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

    ENDIF

      !c-c-c-c-c-c-c-c-c-c

    IF( tbc.eq.3 )THEN

      nvar = nvar+1
      varname(nvar) = 'ustt'
      vardesc(nvar) = 'friction velocity on upper boundary'
      varunit(nvar) = 'm/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( ustt  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'ut'
      vardesc(nvar) = 'ut'
      varunit(nvar) = 'm/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( ut  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'vt'
      vardesc(nvar) = 'vt'
      varunit(nvar) = 'm/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( vt  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'st'
      vardesc(nvar) = 'st'
      varunit(nvar) = 'm/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( st  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

    ENDIF

      !c-c-c-c-c-c-c-c-c-c

    if( sfcmodel.eq.2 .or. sfcmodel.eq.3 )then

      nvar = nvar+1
      varname(nvar) = 'z0t'
      vardesc(nvar) = 'surface roughness length for temperature'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( z0t  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'z0q'
      vardesc(nvar) = 'surface roughness length for moisture'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( z0q  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

    endif

      !c-c-c-c-c-c-c-c-c-c

    if( sfcmodel.ge.1 )then
      nvar = nvar+1
      varname(nvar) = 'tsk'
      vardesc(nvar) = 'soil/ocean temperature'
      varunit(nvar) = 'K'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( tsk ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
    endif

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'psfc'
      vardesc(nvar) = 'pressure at surface'
      varunit(nvar) = 'Pa'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( psfc ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

    if( sfcmodel.ge.1 )then
      nvar = nvar+1
      varname(nvar) = 'qsfc'
      vardesc(nvar) = 'land/water water vapor mixing ratio'
      varunit(nvar) = 'g/g'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( qsfc ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
    endif

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'thflux'
      vardesc(nvar) = 'surface pot. temp. flux (Q*)'
      varunit(nvar) = 'K m/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(thflux,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      thfavg = savg2d

      !c-c-c-c-c-c-c-c-c-c

    IF( imoist.eq.1 )THEN
      nvar = nvar+1
      varname(nvar) = 'qvflux'
      vardesc(nvar) = 'surface water vapor flux'
      varunit(nvar) = 'g/g m/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(qvflux,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      qvfavg = savg2d
    ENDIF

      !c-c-c-c-c-c-c-c-c-c

    sfcmodel1:  &
    IF( sfcmodel.ge.1 )THEN

      nvar = nvar+1
      varname(nvar) = 'hfx'
      vardesc(nvar) = 'surface sensible heat flux'
      varunit(nvar) = 'W/m2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(hfx,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'qfx'
      vardesc(nvar) = 'surface latent heat flux'
      varunit(nvar) = 'W/m2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(qfx,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'u10'
    if( imove.eq.1 )then
      vardesc(nvar) = 'u component of 10m wind speed (ground-rel.)'
    else
      vardesc(nvar) = 'u component of 10m wind speed'
    endif
      varunit(nvar) = 'm/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( u10  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      if( myid.eq.0 ) print *,'  Metric: u component of 10m wind speed (m/s)= ',savg2d

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'v10'
    if( imove.eq.1 )then
      vardesc(nvar) = 'v component of 10m wind speed (ground-rel.)'
    else
      vardesc(nvar) = 'v component of 10m wind speed'
    endif
      varunit(nvar) = 'm/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( v10  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      if( myid.eq.0 ) print *,'  Metric: v component of 10m wind speed (m/s)= ',savg2d

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 's10'
    if( imove.eq.1 )then
      vardesc(nvar) = 'horiz wind speed at 10m (ground-rel.)'
    else
      vardesc(nvar) = 'horiz wind speed at 10m'
    endif
      varunit(nvar) = 'm/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( s10  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      if( myid.eq.0 ) print *,'  Metric: horiz wind speed at 10m (m/s)= ',savg2d

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 't2'
      vardesc(nvar) = 'diagnostic 2-m temperature'
      varunit(nvar) = 'K'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( t2  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'q2'
      vardesc(nvar) = 'diagnostic 2-m mixing ratio'
      varunit(nvar) = 'g/g'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( q2  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'th2'
      vardesc(nvar) = 'diagnostic 2-m potential temperature'
      varunit(nvar) = 'K'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( th2  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'cd'
      vardesc(nvar) = 'drag coefficient'
      varunit(nvar) = 'nondimensional'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( cd ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'ch'
      vardesc(nvar) = 'surface exchange coefficient for sensible heat'
      varunit(nvar) = 'nondimensional'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( ch ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'cq'
      vardesc(nvar) = 'surface exchange coefficient for moisture'
      varunit(nvar) = 'nondimensional'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( cq ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

    ENDIF  sfcmodel1

      !c-c-c-c-c-c-c-c-c-c

    IF( ipbl.ge.1 .or. sfcmodel.ge.1 )THEN
      nvar = nvar+1
      varname(nvar) = 'hpbl'
    if( ipbl.eq.1 )then
      vardesc(nvar) = 'PBL height (from YSU scheme)'
    elseif( ipbl.eq.3 )then
      vardesc(nvar) = 'PBL height (from GFSEDMF scheme)'
    elseif( ipbl.eq.4 .or. ipbl.eq.5 )then
      vardesc(nvar) = 'PBL height (from MYNN scheme)'
    elseif( ipbl.eq.6 )then
      vardesc(nvar) = 'PBL height (from MYJ scheme)'
    else
      vardesc(nvar) = 'estimated PBL height (based on bulk Ri)'
    endif
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( hpbl  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
    ENDIF

      !c-c-c-c-c-c-c-c-c-c

    sfcstuff:  &
    IF( sfcmodel.ge.1 )THEN


      !this causes a code bug
      nvar = nvar+1
      varname(nvar) = 'wspd'
      vardesc(nvar) = 'sfc layer wind speed (with gust)'
      varunit(nvar) = 'm/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( wspd  ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)


      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'zol'
      vardesc(nvar) = 'z/L (z over Monin-Obukhov length)'
      varunit(nvar) = 'nondimensional'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( zol ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'br'
      vardesc(nvar) = 'bulk Richardson number in surface layer'
      varunit(nvar) = 'nondimensional'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( br ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'brcr'
      vardesc(nvar) = 'critical bulk Richardson number in surface layer'
      varunit(nvar) = 'nondimensional'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( brcr ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c
      nvar = nvar+1
      varname(nvar) = 'phim'
      vardesc(nvar) = 'similarity nondimen wind shear at lowest model level'
      varunit(nvar) = 'nondimensional'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( phim ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'phih'
      vardesc(nvar) = 'similarity nondimen temp grad at lowest model level'
      varunit(nvar) = 'nondimensional'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( phih ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'psim'
      vardesc(nvar) = 'similarity stability function (momentum)'
      varunit(nvar) = 'nondimensional'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( psim ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'psih'
      vardesc(nvar) = 'similarity stability function (heat)'
      varunit(nvar) = 'nondimensional'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( psih ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'tst'
      vardesc(nvar) = 'theta-star (pot temp scaling parameter in similarity theory)'
      varunit(nvar) = 'nondimensional'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( tst ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

    IF( imoist.eq.1 )THEN
      nvar = nvar+1
      varname(nvar) = 'qst'
      vardesc(nvar) = 'q-star (water vapor scaling parameter in similarity theory)'
      varunit(nvar) = 'nondimensional'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( qst ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
    ENDIF

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'mol'
      vardesc(nvar) = 'Monin-Obukhov length (L)'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        if( abs(rmol(i,j)).le.1.0e-10 )then
          dum1(i,j,1) = sign( 1.0e10 , rmol(i,j) )
        else
          dum1(i,j,1) = 1.0/rmol(i,j)
        endif
      enddo
      enddo
      call getavg2d( dum1(ib,jb,1) ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      molbar = savg2d

      !c-c-c-c-c-c-c-c-c-c

    if( sfcmodel.eq.5 )then
      nvar = nvar+1
      varname(nvar) = 'nzeta'
      vardesc(nvar) = 'number of iterations in sfc layer code'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      savg2d = nzeta
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
    endif

      !c-c-c-c-c-c-c-c-c-c

    if( sfcmodel.eq.7 )then

      !----

      nvar = nvar+1
      varname(nvar) = 'thz0'
      vardesc(nvar) = 'thz0'
      varunit(nvar) = 'K'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum1(i,j,1) = thz0(i,j)
      enddo
      enddo
      call getavg2d( dum1(ib,jb,1) ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !----

      nvar = nvar+1
      varname(nvar) = 'qz0'
      vardesc(nvar) = 'qz0'
      varunit(nvar) = 'kg/kg'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum1(i,j,1) = qz0(i,j)
      enddo
      enddo
      call getavg2d( dum1(ib,jb,1) ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !----

      nvar = nvar+1
      varname(nvar) = 'uz0'
      vardesc(nvar) = 'uz0'
      varunit(nvar) = 'm/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum1(i,j,1) = uz0(i,j)
      enddo
      enddo
      call getavg2d( dum1(ib,jb,1) ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !----

      nvar = nvar+1
      varname(nvar) = 'vz0'
      vardesc(nvar) = 'vz0'
      varunit(nvar) = 'm/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum1(i,j,1) = vz0(i,j)
      enddo
      enddo
      call getavg2d( dum1(ib,jb,1) ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

    endif

      !c-c-c-c-c-c-c-c-c-c

    ENDIF  sfcstuff

      !c-c-c-c-c-c-c-c-c-c

    ziflag:  &
    IF( ( testcase.ge.1 .and. testcase.le.7 ) .or. testcase.eq.9 .or. testcase.eq.10 .or. testcase.eq.11 )THEN

        zimin =  1.0e30
        zimax = -1.0e30
        !$acc parallel default(present) reduction(min:zimin) reduction(max:zimax)
        !$acc loop gang vector collapse(2)
        do j=1,nj
        do i=1,ni
          zi = 0.0
          tmax = 0.0
          !$acc loop seq
          do k=nk,2,-1
            dtdz = (thv(i,j,k)-thv(i,j,k-1))*rdz*mf(i,j,k)
            if( dtdz .ge. tmax )then
              tmax = dtdz
              zi = zf(i,j,k)
            endif
          enddo
          dum1(i,j,1) = zi
          zimax = max( zimax , zi )
          zimin = min( zimin , zi )
        enddo
        enddo
        !$acc end parallel

#ifdef MPI
      call MPI_ALLREDUCE(mpi_in_place,zimin,1,MPI_REAL,MPI_MIN,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(mpi_in_place,zimax,1,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,ierr)
#endif

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'zi'
      vardesc(nvar) = 'estimate of boundary-layer depth (max gradient method)'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d( dum1(ib,jb,1) ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      if( myid.eq.0 ) print *,'  Metric: boundary-layer depth (m)= ',savg2d
      zibar = savg2d

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'zimin'
      vardesc(nvar) = 'minimum value of zi'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      if( myid.eq.0 ) print *,'  Metric: boundary-layer depth min (m)= ',zimin
      savg2d = zimin
      if( myid.eq.0 ) print *,'  Metric: boundary-layer depth min (m)= ',zimin
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      if( myid.eq.0 ) print *,'  Metric: boundary-layer depth min (m)= ',savg2d

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'zimax'
      vardesc(nvar) = 'maximum value of zi'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      savg2d = zimax
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      if( myid.eq.0 ) print *,'  Metric: boundary-layer depth max (m)= ',savg2d

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'zivar'
      vardesc(nvar) = 'variance of zi'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum1(i,j,2) = (dum1(i,j,1)-zibar)**2
      enddo
      enddo
      call getavg2d( dum1(ib,jb,2) ,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      if( myid.eq.0 ) print *,'  Metric: boundary-layer depth variance (m)= ',savg2d

    ENDIF  ziflag
    !stop 'domaindiag: after boundary-layer depth diagnostics'

      !c-c-c-c-c-c-c-c-c-c

    IF( ( testcase.ge.1 .and. testcase.le.7 ) .or. testcase.eq.9 .or. testcase.eq.10 )THEN

      nvar = nvar+1
      varname(nvar) = 'zitb'
      vardesc(nvar) = 'zi, max gradient method using thetav-bar'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      !$acc parallel default(present)
      tmax = -1.0e30
      zi = 0.0

      !$acc loop seq
      do k=nk,2,-1
        dtdz = (thvavg(k)-thvavg(k-1))*rdz*mf(1,1,k)
        if( dtdz .ge. tmax )then
          tmax = dtdz
          zi = zf(1,1,k)
        endif
      enddo

      savg2d = zi
      zitb = zi
      !$acc end parallel

      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

    ENDIF

      !c-c-c-c-c-c-c-c-c-c

    IF( ( testcase.ge.1 .and. testcase.le.7 ) .or. testcase.eq.9 .or. testcase.eq.10 )THEN

        nvar = nvar+1
        varname(nvar) = 'zwspmax'
        vardesc(nvar) = 'level of max mean (ground-rel) wind speed'
        varunit(nvar) = 'm'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'

        !$acc parallel default(present)
        tmax = -1.0e30

        !$acc loop seq
        do k=nk,1,-1
          if( wsp(k) .ge. tmax )then
            tmax = wsp(k)
            savg2d = zh(1,1,k)
          endif
        enddo
        !$acc end parallel

        call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

    ENDIF

      !c-c-c-c-c-c-c-c-c-c

    IF( imoist.eq.1 )THEN

      nvar = nvar+1
      varname(nvar) = 'rain'
      vardesc(nvar) = 'domain-averaged accum. rain at sfc'
      varunit(nvar) = 'cm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(rain(ib,jb,1),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

    ENDIF


      !c-c-c-c-c-c-c-c-c-c


    IF( imoist.eq.1 )THEN

      nvar = nvar+1
      varname(nvar) = 'prate'
      vardesc(nvar) = 'domain-averaged surface precipitation rate'
      varunit(nvar) = 'kg/m2/s'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(prate(ib,jb),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      if( myid.eq.0 ) print *,'  Metric: preciptation rate (mm/day) = ',86400.0*savg2d

    ENDIF


      !c-c-c-c-c-c-c-c-c-c

    IF( imoist.eq.1 )THEN
      ! water paths, precipitable water:

      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
      do k=1,5
      do j=1,nj
      do i=1,ni
        dum1(i,j,k) = 0.0
        dum2(i,j,k) = 0.0
      enddo
      enddo
      enddo

      do k=1,nk
        if( nqc.ge.1 )then
          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            dum1(i,j,1) = dum1(i,j,1) + rho(i,j,k)*q3d(i,j,k,nqc)*dz*rmh(i,j,k)
          enddo
          enddo
        endif
        if( nqr.ge.1 )then
          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            dum1(i,j,2) = dum1(i,j,2) + rho(i,j,k)*q3d(i,j,k,nqr)*dz*rmh(i,j,k)
          enddo
          enddo
        endif
          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          dum1(i,j,3) = dum1(i,j,3) + rho(i,j,k)*ql(i,j,k)*dz*rmh(i,j,k)
        enddo
        enddo
        if( nqv.ge.1 )then
          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
                                                                            ! 1000 kg/m3
            dum1(i,j,4) = dum1(i,j,4) + rho(i,j,k)*q3d(i,j,k,nqv)*dz*rmh(i,j,k)/1000.0
          enddo
          enddo
        endif
      enddo
      !$acc parallel default(present) private(i,j)
      !$acc loop seq
      do k=1,nk
        !$acc loop gang vector collapse(2)
        do j=1,nj
        do i=1,ni
          dum1(i,j,5) = dum1(i,j,5) + rho(i,j,k)*rslf(prs(i,j,k),th(i,j,k)*(pi0(i,j,k)+pp3d(i,j,k)))*dz*rmh(i,j,k)
        enddo
        enddo
      enddo
      !$acc end parallel

      do k=1,nk
        IF( iice.eq.1 )THEN
          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            dum2(i,j,1) = dum2(i,j,1) + rho(i,j,k)*qice(i,j,k)*dz*rmh(i,j,k)
          enddo
          enddo
          if( nqi.ge.1 )then
           !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
            do j=1,nj
            do i=1,ni
              dum2(i,j,2) = dum2(i,j,2) + rho(i,j,k)*q3d(i,j,k,nqi)*dz*rmh(i,j,k)
            enddo
            enddo
          endif
          if( nqs.ge.1 )then
          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
            do j=1,nj
            do i=1,ni
              dum2(i,j,3) = dum2(i,j,3) + rho(i,j,k)*q3d(i,j,k,nqs)*dz*rmh(i,j,k)
            enddo
            enddo
          endif
          if( nqg.ge.1 )then
          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
            do j=1,nj
            do i=1,ni
              dum2(i,j,4) = dum2(i,j,4) + rho(i,j,k)*q3d(i,j,k,nqg)*dz*rmh(i,j,k)
            enddo
            enddo
          endif
          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            dum2(i,j,5) = dum2(i,j,5) + rho(i,j,k)*(ql(i,j,k)+qice(i,j,k))*dz*rmh(i,j,k)
          enddo
          enddo
        ENDIF
      enddo


      nvar = nvar+1
      varname(nvar) = 'cwp'
      vardesc(nvar) = 'cloud water path'
      varunit(nvar) = 'kg/(m^2)'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(dum1(ib,jb,1),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      cwp = savg2d

      nvar = nvar+1
      varname(nvar) = 'rwp'
      vardesc(nvar) = 'rain water path'
      varunit(nvar) = 'kg/(m^2)'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(dum1(ib,jb,2),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      rwp = savg2d

      nvar = nvar+1
      varname(nvar) = 'lwp'
      vardesc(nvar) = 'liquid water path (qc+qr)'
      varunit(nvar) = 'kg/(m^2)'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(dum1(ib,jb,3),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      lwp = savg2d
      if( myid.eq.0 ) print *,'  Metric: LWP (g/m2) = ',1000.0*lwp

      nvar = nvar+1
      varname(nvar) = 'pwat'
      vardesc(nvar) = 'precipitable water'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(dum1(ib,jb,4),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      !stop 'after call to getavg2d(pwat) '

      nvar = nvar+1
      varname(nvar) = 'spwr'
      vardesc(nvar) = 'saturated water vapor path'
      varunit(nvar) = 'kg/(m^2)'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(dum1(ib,jb,5),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      !stop 'after call to getavg2d(spwr) '

      nvar = nvar+1
      varname(nvar) = 'fwp'
      vardesc(nvar) = 'frozen water (i.e., ice+snow+graup) path'
      varunit(nvar) = 'kg/(m^2)'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(dum2(ib,jb,1),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      cwp = savg2d

      nvar = nvar+1
      varname(nvar) = 'iwp'
      vardesc(nvar) = 'ice crystal water path'
      varunit(nvar) = 'kg/(m^2)'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(dum2(ib,jb,2),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      rwp = savg2d

      nvar = nvar+1
      varname(nvar) = 'swp'
      vardesc(nvar) = 'snow water path'
      varunit(nvar) = 'kg/(m^2)'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(dum2(ib,jb,3),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      lwp = savg2d

      nvar = nvar+1
      varname(nvar) = 'gwp'
      vardesc(nvar) = 'graupel/hail water path'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(dum2(ib,jb,4),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'tcwp'
      vardesc(nvar) = 'total condensate water path'
      varunit(nvar) = 'kg/(m^2)'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      call getavg2d(dum2(ib,jb,5),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'cwpvar'
      vardesc(nvar) = 'cloud water path variance'
      varunit(nvar) = 'kg^2/(m^4)'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum2(i,j,1) = (dum1(i,j,1)-cwp)**2
      enddo
      enddo
      call getavg2d(dum2(ib,jb,1),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'rwpvar'
      vardesc(nvar) = 'rain water path variance'
      varunit(nvar) = 'kg^2/(m^4)'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum2(i,j,2) = (dum1(i,j,2)-rwp)**2
      enddo
      enddo
      call getavg2d(dum2(ib,jb,2),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'lwpvar'
      vardesc(nvar) = 'liquid water path (qc+qr) variance'
      varunit(nvar) = 'kg^2/(m^4)'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum2(i,j,3) = (dum1(i,j,3)-lwp)**2
      enddo
      enddo
      call getavg2d(dum2(ib,jb,3),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

    ENDIF

      !c-c-c-c-c-c-c-c-c-c

    IF( imoist.eq.1 )THEN
      ! fraction of cloudy/rainy grid points:

      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum1(i,j,1) = 0.0
        dum1(i,j,2) = 0.0
        dum1(i,j,3) = 0.0
        dum1(i,j,4) = 0.0
        dum1(i,j,5) = 0.0
      enddo
      enddo

      !$acc parallel loop gang vector default(present) private(i,j,k)
      do k=1,nk
        ! use minimum of 1.0e-5 and 1% of saturation wrt liquid (using last known t,p)
        qcrit(k) = min( 1.0e-5 , 0.01*rslf(pavg(k),thavg(k)*((pavg(k)*rp00)**rovcp)) )
      enddo

        ! use minimum of 1.0e-5 and 1% of saturation wrt liquid (using last known t,p)
        !qcrit(k) = min( 1.0e-5 , 0.01*rslf(pavg(k),thavg(k)*((pavg(k)*rp00)**rovcp)) )
        if( nqc.ge.1 )then
          !$acc parallel default(present)
          !$acc loop gang vector collapse(3)
          do k=nk,1,-1
             do j=1,nj
                do i=1,ni
                   if( q3d(i,j,k,nqc).ge.qcrit(k) ) dum1(i,j,1) = zh(i,j,k)
                end do
             end do
          end do
          !$acc end parallel
        endif
        if( nqr.ge.1 )then
          !$acc parallel default(present)
          !$acc loop gang vector collapse(3)
          do k=nk,1,-1
             do j=1,nj
                do i=1,ni
                   if( q3d(i,j,k,nqr).ge.qcrit(k) ) dum1(i,j,2) = zh(i,j,k)
                end do
             end do
          end do
          !$acc end parallel
        endif
        !$acc parallel default(present)
        !$acc loop gang vector collapse(3)
        do k=nk,1,-1
           do j=1,nj
              do i=1,ni
                 if( ql(i,j,k).ge.qcrit(k) ) dum1(i,j,3) = zh(i,j,k)
              end do
           end do
        end do
        !$acc end parallel
        if( iice.eq.1 )then
        !$acc parallel default(present)
        !$acc loop gang vector collapse(3)
        do k=nk,1,-1
           do j=1,nj
              do i=1,ni
                 if( qice(i,j,k).ge.qcrit(k) ) dum1(i,j,4) = zh(i,j,k)
              end do
           end do
        end do
        !$acc end parallel
        endif
        !$acc parallel default(present)
        !$acc loop gang vector collapse(3)
        do k=nk,1,-1
           do j=1,nj
              do i=1,ni
                 if( (ql(i,j,k)+qice(i,j,k)).ge.qcrit(k) ) dum1(i,j,5) = zh(i,j,k)
              end do
           end do
        end do
        !$acc end parallel

      qcfrac = 0.0
      qrfrac = 0.0
      qlfrac = 0.0
      qifrac = 0.0
      qtfrac = 0.0
      !$acc parallel default(present) reduction(+:qcfrac,qrfrac,qlfrac,qifrac,qtfrac)
      !$acc loop gang vector collapse(2) reduction(+:qcfrac,qrfrac,qlfrac,qifrac,qtfrac)
      do j=1,nj
         do i=1,ni
            if( dum1(i,j,1).gt.0.01*zh(1,1,1) ) qcfrac = qcfrac+1.0
            if( dum1(i,j,2).gt.0.01*zh(1,1,1) ) qrfrac = qrfrac+1.0
            if( dum1(i,j,3).gt.0.01*zh(1,1,1) ) qlfrac = qlfrac+1.0
            if( dum1(i,j,4).gt.0.01*zh(1,1,1) ) qifrac = qifrac+1.0
            if( dum1(i,j,5).gt.0.01*zh(1,1,1) ) qtfrac = qtfrac+1.0
         end do
      end do
      !$acc end parallel

#ifdef MPI
      call MPI_ALLREDUCE(MPI_IN_PLACE,qcfrac,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,qrfrac,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,qlfrac,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,qifrac,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,qtfrac,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif

      temd = 1.0d0/dble(nx*ny)

      qcfrac = qcfrac*temd
      qrfrac = qrfrac*temd
      qlfrac = qlfrac*temd
      qifrac = qifrac*temd
      qtfrac = qtfrac*temd

      nvar = nvar+1
      varname(nvar) = 'qcfrac_col'
      vardesc(nvar) = 'fraction of columns with cloud water'
      varunit(nvar) = 'dimensionless'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      savg2d = qcfrac
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'qrfrac_col'
      vardesc(nvar) = 'fraction of columns with rain water'
      varunit(nvar) = 'dimensionless'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      savg2d = qrfrac
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'qlfrac_col'
      vardesc(nvar) = 'fraction of columns with liquid (qc+qr)'
      varunit(nvar) = 'dimensionless'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      savg2d = qlfrac
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      if( myid.eq.0 ) print *,'  Metric: cloud fraction (%) = ',100.0*qlfrac

      nvar = nvar+1
      varname(nvar) = 'qifrac_col'
      vardesc(nvar) = 'fraction of columns with ice (qs+qg+qi)'
      varunit(nvar) = 'dimensionless'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      savg2d = qifrac
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'qtfrac_col'
      vardesc(nvar) = 'fraction of columns with liquid+ice (qc+qr+qs+qg+qi)'
      varunit(nvar) = 'dimensionless'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      savg2d = qtfrac
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      !$acc parallel default(present) private(i,j) reduction(+:qcb,ncb,qrb,nrb,qlb,nlb)
      qcb = 0.0
      qrb = 0.0
      qlb = 0.0

      ncb = 0
      nrb = 0
      nlb = 0

      !$acc loop gang vector collapse(2) &
      !$acc    reduction(+:qcb,ncb,qrb,nrb,qlb,nlb) 
      do j=1,nj
      do i=1,ni
        if( dum1(i,j,1).gt.0.01*zh(1,1,1) )then
          qcb = qcb+dum1(i,j,1)
          ncb = ncb+1
        endif
        if( dum1(i,j,2).gt.0.01*zh(1,1,1) )then
          qrb = qrb+dum1(i,j,2)
          nrb = nrb+1
        endif
        if( dum1(i,j,3).gt.0.01*zh(1,1,1) )then
          qlb = qlb+dum1(i,j,3)
          nlb = nlb+1
        endif
      enddo
      enddo
      !$acc end parallel

#ifdef MPI
      call MPI_ALLREDUCE(MPI_IN_PLACE,qcb,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,qrb,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,qlb,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,ncb,1,MPI_INTEGER         ,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,nrb,1,MPI_INTEGER         ,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,nlb,1,MPI_INTEGER         ,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif

      qcb = qcb/dble(max(1,ncb))
      qrb = qrb/dble(max(1,nrb))
      qlb = qlb/dble(max(1,nlb))

      nvar = nvar+1
      varname(nvar) = 'qcb'
      vardesc(nvar) = 'avg height of qc base'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      savg2d = qcb
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'qrb'
      vardesc(nvar) = 'avg height of qr base'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      savg2d = qrb
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'qlb'
      vardesc(nvar) = 'avg height of ql (=qc+qr) base'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      savg2d = qlb
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'qcbvar'
      vardesc(nvar) = 'variance of height of qc base'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum2(i,j,1) = (dum1(i,j,1)-qcb)**2
      enddo
      enddo
      call getavg2d(dum2(ib,jb,1),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'qrbvar'
      vardesc(nvar) = 'variance of height of qr base'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum2(i,j,2) = (dum1(i,j,2)-qrb)**2
      enddo
      enddo
      call getavg2d(dum2(ib,jb,2),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'qlbvar'
      vardesc(nvar) = 'variance of height of ql (=qc+qr)base'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum2(i,j,3) = (dum1(i,j,3)-qlb)**2
      enddo
      enddo

      call getavg2d(dum2(ib,jb,3),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

    ENDIF

      !c-c-c-c-c-c-c-c-c-c

    IF( imoist.eq.1 )THEN
      ! cloud top heights:

      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum1(i,j,1) = 0.0
        dum1(i,j,2) = 0.0
        dum1(i,j,3) = 0.0
      enddo
      enddo

        !PORTME
        !$acc parallel  default(present) private(i,j)
        if( nqc.ge.1 )then
          !$acc loop seq
          do k=1,nk
          !$acc loop gang vector collapse(2)
          do j=1,nj
          do i=1,ni
            if( q3d(i,j,k,nqc).ge.qcrit(k) ) dum1(i,j,1) = zh(i,j,k)
          enddo
          enddo
          enddo
        endif
        if( nqr.ge.1 )then
          !$acc loop seq
          do k=1,nk
          !$acc loop gang vector collapse(2)
          do j=1,nj
          do i=1,ni
            if( q3d(i,j,k,nqr).ge.qcrit(k) ) dum1(i,j,2) = zh(i,j,k)
          enddo
          enddo
          enddo
        endif
        !$acc loop seq
        do k=1,nk
          !$acc loop gang vector collapse(2)
          do j=1,nj
          do i=1,ni
            if( ql(i,j,k).ge.qcrit(k) ) dum1(i,j,3) = zh(i,j,k)
          enddo
          enddo
        enddo
        !$acc end parallel
      !$acc parallel default(present) private(i,j) reduction(+:qct,nct,qrt,nrt,qlt,nlt)
      qct = 0.0
      qrt = 0.0
      qlt = 0.0

      nct = 0
      nrt = 0
      nlt = 0

      !$acc loop vector collapse(2) &
      !$acc reduction(+:qct,nct,qrt,nrt,qlt,nlt)
      do j=1,nj
      do i=1,ni
        if( dum1(i,j,1).gt.0.01*zh(1,1,1) )then
          qct = qct+dum1(i,j,1)
          nct = nct+1
        endif
        if( dum1(i,j,2).gt.0.01*zh(1,1,1) )then
          qrt = qrt+dum1(i,j,2)
          nrt = nrt+1
        endif
        if( dum1(i,j,3).gt.0.01*zh(1,1,1) )then
          qlt = qlt+dum1(i,j,3)
          nlt = nlt+1
        endif
      enddo
      enddo
      !$acc end parallel

#ifdef MPI
      call MPI_ALLREDUCE(MPI_IN_PLACE,qct,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,qrt,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,qlt,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,nct,1,MPI_INTEGER         ,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,nrt,1,MPI_INTEGER         ,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,nlt,1,MPI_INTEGER         ,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif
      qct = qct/dble(max(1,nct))
      qrt = qrt/dble(max(1,nrt))
      qlt = qlt/dble(max(1,nlt))

      nvar = nvar+1
      varname(nvar) = 'qct'
      vardesc(nvar) = 'avg height of qc top'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      savg2d = qct
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !stop 'domaindiag: after calculation of qct'

      nvar = nvar+1
      varname(nvar) = 'qrt'
      vardesc(nvar) = 'avg height of qr top'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      savg2d = qrt
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'qlt'
      vardesc(nvar) = 'avg height of ql (=qc+qr) top'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'
      savg2d = qlt
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'qctvar'
      vardesc(nvar) = 'variance of height of qc top'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum2(i,j,1) = (dum1(i,j,1)-qct)**2
      enddo
      enddo

      call getavg2d(dum2(ib,jb,1),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'qrtvar'
      vardesc(nvar) = 'variance of height of qr top'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum2(i,j,2) = (dum1(i,j,2)-qrt)**2
      enddo
      enddo

      call getavg2d(dum2(ib,jb,2),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'qltvar'
      vardesc(nvar) = 'variance of height of ql (=qc+qr) top'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum2(i,j,3) = (dum1(i,j,3)-qlt)**2
      enddo
      enddo

      call getavg2d(dum2(ib,jb,3),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

    ENDIF

      !c-c-c-c-c-c-c-c-c-c

    IF( testcase.eq.4 .or. testcase.eq.5 )THEN

      nvar = nvar+1
      varname(nvar) = 'zir'
      vardesc(nvar) = 'estimate of b.l. depth for simple rad. scheme'
      varunit(nvar) = 'm'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

     
      call getavg2d( zir ,savg2d)
      zibar = savg2d
      call write2d(zibar,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'zirvar'
      vardesc(nvar) = 'variance of zir'
      varunit(nvar) = 'm^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
      do j=1,nj
      do i=1,ni
        dum2(i,j,1) = (zir(i,j)-zibar)**2
      enddo
      enddo

      call getavg2d(dum2(ib,jb,1),savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

    ENDIF

    !stop 'domaindiag: after calculation of zirvar'

      !c-c-c-c-c-c-c-c-c-c

    IF( radopt.eq.2 )THEN

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'lwupt'
      vardesc(nvar) = 'lw flux, upward, top of atmosphere (OLR)'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(lwupt,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'lwdnt'
      vardesc(nvar) = 'lw flux, downward, top of atmosphere'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(lwdnt,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'lwupb'
      vardesc(nvar) = 'lw flux, upward, bottom of atmosphere'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(lwupb,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'lwdnb'
      vardesc(nvar) = 'lw flux, downward, bottom of atmosphere'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(lwdnb,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'swupt'
      vardesc(nvar) = 'sw flux, upward, top of atmosphere'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(swupt,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'swdnt'
      vardesc(nvar) = 'sw flux, downward, top of atmosphere'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(swdnt,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'swupb'
      vardesc(nvar) = 'sw flux, upward, bottom of atmosphere'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(swupb,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'swdnb'
      vardesc(nvar) = 'sw flux, downward, bottom of atmosphere'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(swdnb,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'lwuptc'
      vardesc(nvar) = 'lw flux, upward, top of atmosphere - clear sky'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(lwuptc,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'lwdntc'
      vardesc(nvar) = 'lw flux, downward, top of atmosphere - clear sky'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(lwdntc,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'lwupbc'
      vardesc(nvar) = 'lw flux, upward, bottom of atmosphere - clear sky'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(lwupbc,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'lwdnbc'
      vardesc(nvar) = 'lw flux, downward, bottom of atmosphere - clear sky'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(lwdnbc,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'swuptc'
      vardesc(nvar) = 'sw flux, upward, top of atmosphere - clear sky'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(swuptc,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'swdntc'
      vardesc(nvar) = 'sw flux, downward, top of atmosphere - clear sky'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(swdntc,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'swupbc'
      vardesc(nvar) = 'sw flux, upward, bottom of atmosphere - clear sky'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(swupbc,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'swdnbc'
      vardesc(nvar) = 'sw flux, downward, bottom of atmosphere - clear sky'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(swdnbc,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'lwcf'
      vardesc(nvar) = 'longwave cloud forcing at top-of-atmosphere'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(lwcf,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      nvar = nvar+1
      varname(nvar) = 'swcf'
      vardesc(nvar) = 'shortwave cloud forcing at top-of-atmosphere'
      varunit(nvar) = 'W/m^2'
      varlvls(nvar) = 0
      vargrid(nvar) = '2'

      call getavg2d(swcf,savg2d)
      call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

    ENDIF
    !stop 'domaindiag: after calculation of swcf'

!BLOCK02
#if 0
      !c-c-c-c-c-c-c-c-c-c
      !PORTME
      doit = .true.
      IF( doit )THEN
        IF( imoist.eq.1 )THEN

          ! cape, cin, lfc, lcl:

          !$omp parallel do default(shared)  &
          !$omp private(i,j,k,pfoo,tfoo,qfoo,zel,psource,tsource,qvsource)
          DO j=1,nj
          DO i=1,ni

            allocate( pfoo(nk+1) )
            allocate( tfoo(nk+1) )
            allocate( qfoo(nk+1) )

            do k=1,nk
              pfoo(k+1) = 0.01*prs(i,j,k)
              tfoo(k+1) = (th0(i,j,k)+th3d(i,j,k))*(pi0(i,j,k)+pp3d(i,j,k)) - 273.15
              qfoo(k+1) = q3d(i,j,k,nqv)
            enddo

            pfoo(1) = cgs1*pfoo(2)+cgs2*pfoo(3)+cgs3*pfoo(4)
            tfoo(1) = cgs1*tfoo(2)+cgs2*tfoo(3)+cgs3*tfoo(4)
            qfoo(1) = cgs1*qfoo(2)+cgs2*qfoo(3)+cgs3*qfoo(4)

            ! dum1(1) = cape
            ! dum1(2) = cin
            ! dum2(1) = lcl
            ! dum2(2) = lfc

            call getcape( 3 , nk+1 , pfoo , tfoo , qfoo , dum1(i,j,1) , dum1(i,j,2) ,   &
                          dum2(i,j,1), dum2(i,j,2), zel , psource , tsource , qvsource )

            deallocate( pfoo )
            deallocate( tfoo )
            deallocate( qfoo )

          ENDDO
          ENDDO

          nvar = nvar+1
          varname(nvar) = 'cape'
          vardesc(nvar) = 'convective available potential energy'
          varunit(nvar) = 'J/kg'
          varlvls(nvar) = 0
          vargrid(nvar) = '2'
          call getavg2d(dum1(ib,jb,1),savg2d)
          call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

          nvar = nvar+1
          varname(nvar) = 'cin'
          vardesc(nvar) = 'convective inhibition'
          varunit(nvar) = 'J/kg'
          varlvls(nvar) = 0
          vargrid(nvar) = '2'
          call getavg2d(dum1(ib,jb,2),savg2d)
          call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

          nvar = nvar+1
          varname(nvar) = 'lcl'
          vardesc(nvar) = 'lifted condensation level'
          varunit(nvar) = 'm'
          varlvls(nvar) = 0
          vargrid(nvar) = '2'
          call getavg2d(dum2(ib,jb,1),savg2d)
          call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

          nvar = nvar+1
          varname(nvar) = 'lfc'
          vardesc(nvar) = 'level of free convection'
          varunit(nvar) = 'm'
          varlvls(nvar) = 0
          vargrid(nvar) = '2'
          call getavg2d(dum2(ib,jb,2),savg2d)
          call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        ENDIF
      ENDIF
!END-BLOCK02
#endif
     !stop 'domaindiag: after BLOCK02'

      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 .and. th0(1,1,nk)*pi0(1,1,nk).lt.to )THEN

        !$omp parallel do default(shared) private(i,j,k,tx,tlast)
        !$acc parallel loop gang vector collapse(2) default(present) 
        do j=1,nj
        do i=1,ni
          tx = 1.0e30
          k = 1
          !!$acc loop seq
          do while( tx.gt.273.15 .and. k.lt.nk )
            tlast = tx
            k = k + 1
            tx = (th0(i,j,k)+th3d(i,j,k))*(pi0(i,j,k)+pp3d(i,j,k))
          enddo
          dum1(i,j,1) = zh(i,j,k-1)+(zh(i,j,k)-zh(i,j,k-1))  &
                                   *(273.15-tlast)  &
                                   /(tx-tlast)
          dum1(i,j,2) = prs(i,j,k-1)+(prs(i,j,k)-prs(i,j,k-1))  &
                                    *(273.15-tlast)  &
                                    /(tx-tlast)
        enddo
        enddo

        nvar = nvar+1
        varname(nvar) = 'zmelt'
        vardesc(nvar) = 'height of melting level'
        varunit(nvar) = 'm'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'
        call getavg2d(dum1(ib,jb,1),savg2d)
        call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'pmelt'
        vardesc(nvar) = 'pressure of melting level'
        varunit(nvar) = 'Pa'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'
        call getavg2d(dum1(ib,jb,2),savg2d)
        call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF


      !c-c-c-c-c-c-c-c-c-c

      nvar2d = nvar

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    ! 3d vars:  averages

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'u'
        vardesc(nvar) = 'u component of velocity (grid-relative)'
        varunit(nvar) = 'm/s'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = u3d(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)

        !print *,'after calculation of uavg'
        !! save uavg:
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          uavg(k) = savg(k)
        enddo
        !stop 'after calclation of uavg'
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'uinterp'
        vardesc(nvar) = 'u component of velocity (grid-relative)'
        varunit(nvar) = 'm/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = 0.5*(u3d(i,j,k-1)+u3d(i,j,k))
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !stop 'domaindiag: after uinterp'
      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'v'
        vardesc(nvar) = 'v component of velocity (grid-relative)'
        varunit(nvar) = 'm/s'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = v3d(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)

        ! save vavg:
        !$acc parallel loop gang vector default(present) 
        do k=1,nk
          vavg(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'vinterp'
        vardesc(nvar) = 'v component of velocity (grid-relative)'
        varunit(nvar) = 'm/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector default(present) collapse(3) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = 0.5*(v3d(i,j,k-1)+v3d(i,j,k))
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'w'
        vardesc(nvar) = 'vertical velocity'
        varunit(nvar) = 'm/s'
        vargrid(nvar) = 'w'
        call getavg3d(w3d(ib,jb,kb),savg)

        !! save wavg:
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          wavg(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !stop 'domaindiag: after calculation of wavg'


      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'th'
        vardesc(nvar) = 'potential temperature'
        varunit(nvar) = 'K'
        call getavg3d(th,savg)

        ! save thavg:
        !$acc parallel loop default(present) private(k)
        do k=1,nk
          thavg(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)


      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'thinterp'
        vardesc(nvar) = 'potential temperature'
        varunit(nvar) = 'K'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = 0.5*( (th0(i,j,k-1)+th0(i,j,k)) &
                             +(th3d(i,j,k-1)+th3d(i,j,k)) )
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        !$acc parallel
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !stop 'domaindiag: after thinterp'
      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 't'
        vardesc(nvar) = 'temperature'
        varunit(nvar) = 'K'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = th(i,j,k)*(pi0(i,j,k)+pp3d(i,j,k))
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'prs'
        vardesc(nvar) = 'pressure'
        varunit(nvar) = 'Pa'
        call getavg3d(prs,savg)

        ! save prsavg:
        !$acc parallel loop private(k)
        do k=1,nk
          prsavg(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'prsinterp'
        vardesc(nvar) = 'pressure'
        varunit(nvar) = 'Pa'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = 0.5*(prs(i,j,k-1)+prs(i,j,k))
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel 
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'rho'
        vardesc(nvar) = 'dry air density'
        varunit(nvar) = 'kg/m3'

        
        call getavg3d(rho,savg)

        ! save rhoavg:
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          rhoavg(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'rhointerp'
        vardesc(nvar) = 'dry air density'
        varunit(nvar) = 'kg/m3'
        vargrid(nvar) = 'w'

        call getavg3d(rf,savg,nkp1)

        ! save rfavg:
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          rfavg(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !stop 'domaindiag: after rhointerp'
      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'ppi'
        vardesc(nvar) = 'nondimensional pressure'
        varunit(nvar) = 'nondimensional'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = pi0(i,j,k)+pp3d(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)

        ! save piavg:
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          piavg(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 )THEN
        ! mixing ratios:

        DO n=1,numq

          nvar = nvar+1
          varname(nvar) = qname(n)
          vardesc(nvar) = qname(n)
          varunit(nvar) = qunit(n)


          call getavg3d(q3d(ib,jb,kb,n),savg)

          if( n.eq.nqv )then
            ! save qvavg:
            !$acc parallel loop gang vector default(present) private(k)
            do k=1,nk
              qvavg(k) = savg(k)
            enddo
          endif
          call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        ENDDO

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      ! specific humidities:
      doit = .true.
      IF( doit .and. imoist.eq.1 )THEN
        DO n=1,numq

          do2 = .false.

          if( qname(n).eq.'qv ' )then
            do2 = .true.
            nvar = nvar+1
            varname(nvar) = 'shv'
            vardesc(nvar) = 'specific humidity (water vapor)'
            varunit(nvar) = 'g/g'
          endif
          if( qname(n).eq.'qc ' )then
            do2 = .true.
            nvar = nvar+1
            varname(nvar) = 'shc'
            vardesc(nvar) = 'specific humidity (cloud water)'
            varunit(nvar) = 'g/g'
          endif
          if( qname(n).eq.'qr ' )then
            do2 = .true.
            nvar = nvar+1
            varname(nvar) = 'shr'
            vardesc(nvar) = 'specific humidity (rain water)'
            varunit(nvar) = 'g/g'
          endif
          if( qname(n).eq.'qi ' )then
            do2 = .true.
            nvar = nvar+1
            varname(nvar) = 'shi'
            vardesc(nvar) = 'specific humidity (cloud ice)'
            varunit(nvar) = 'g/g'
          endif
          if( qname(n).eq.'qs ' )then
            do2 = .true.
            nvar = nvar+1
            varname(nvar) = 'shs'
            vardesc(nvar) = 'specific humidity (snow)'
            varunit(nvar) = 'g/g'
          endif
          if( qname(n).eq.'qg ' )then
            do2 = .true.
            nvar = nvar+1
            varname(nvar) = 'shg'
            vardesc(nvar) = 'specific humidity (graupel)'
            varunit(nvar) = 'g/g'
          endif

          IF( do2 )THEN

            !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              dum1(i,j,k) = q3d(i,j,k,n)/(1.0+q3d(i,j,k,n))
            enddo
            enddo
            enddo

            call getavg3d(dum1,savg)
            call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

          ENDIF

        ENDDO

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'qliq'
        vardesc(nvar) = 'liquid water mixing ratio'
        varunit(nvar) = 'g/g'
        call getavg3d(ql,savg)

        ! save qlavg:
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          qlavg(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF
      !stop 'domaindiag: after qliq'

      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'qice'
        vardesc(nvar) = 'ice mixing ratio (qs+qg+qi)'
        varunit(nvar) = 'g/g'
        call getavg3d(qice,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'qt'
        vardesc(nvar) = 'total water mixing ratio'
        varunit(nvar) = 'g/g'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = q3d(i,j,k,nqv)+ql(i,j,k)+qice(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)

        ! save qtavg:
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          qtavg(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 .and. output_dbz.eq.1 .and. qd_dbz.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'dbz'
        vardesc(nvar) = 'reflectivity'
        varunit(nvar) = 'dBZ'
        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_dbz),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 .and. ptype.eq.5 .and. getvt )THEN

      if( qd_vtc.ge.1 )then
        nvar = nvar+1
        varname(nvar) = 'fc'
        vardesc(nvar) = 'fall velocity of qc'
        varunit(nvar) = 'm/s'
        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_vtc),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      if( qd_vtr.ge.1 )then
        nvar = nvar+1
        varname(nvar) = 'fr'
        vardesc(nvar) = 'fall velocity of qr'
        varunit(nvar) = 'm/s'
        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_vtr),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      if( qd_vti.ge.1 )then
        nvar = nvar+1
        varname(nvar) = 'fi'
        vardesc(nvar) = 'fall velocity of qi'
        varunit(nvar) = 'm/s'
        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_vti),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      if( qd_vts.ge.1 )then
        nvar = nvar+1
        varname(nvar) = 'fs'
        vardesc(nvar) = 'fall velocity of qs'
        varunit(nvar) = 'm/s'
        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_vts),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      if( qd_vtg.ge.1 )then
        nvar = nvar+1
        varname(nvar) = 'fg'
        vardesc(nvar) = 'fall velocity of qg'
        varunit(nvar) = 'm/s'
        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_vtg),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 .and. ptype.eq.5 .and. getvt )THEN

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum2(i,j,k) = 0.0
        enddo
        enddo
        enddo

      if( qd_vtc.ge.1 )then
        nvar = nvar+1
        varname(nvar) = 'precc'
        vardesc(nvar) = 'precip flux for qc'
        varunit(nvar) = 'kg m-2 s-1'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = rho(i,j,k)*qdiag(i,j,k,qd_vtc)*q3d(i,j,k,nqc)
          dum2(i,j,k) = dum2(i,j,k)+dum1(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      if( qd_vtr.ge.1 )then
        nvar = nvar+1
        varname(nvar) = 'precr'
        vardesc(nvar) = 'precip flux for qr'
        varunit(nvar) = 'kg m-2 s-1'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = rho(i,j,k)*qdiag(i,j,k,qd_vtr)*q3d(i,j,k,nqr)
          dum2(i,j,k) = dum2(i,j,k)+dum1(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      if( qd_vti.ge.1 )then
        nvar = nvar+1
        varname(nvar) = 'preci'
        vardesc(nvar) = 'precip flux for qi'
        varunit(nvar) = 'kg m-2 s-1'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = rho(i,j,k)*qdiag(i,j,k,qd_vti)*q3d(i,j,k,nqi)
          dum2(i,j,k) = dum2(i,j,k)+dum1(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      if( qd_vts.ge.1 )then
        nvar = nvar+1
        varname(nvar) = 'precs'
        vardesc(nvar) = 'precip flux for qs'
        varunit(nvar) = 'kg m-2 s-1'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = rho(i,j,k)*qdiag(i,j,k,qd_vts)*q3d(i,j,k,nqs)
          dum2(i,j,k) = dum2(i,j,k)+dum1(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      if( qd_vtg.ge.1 )then
        nvar = nvar+1
        varname(nvar) = 'precg'
        vardesc(nvar) = 'precip flux for qg'
        varunit(nvar) = 'kg m-2 s-1'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = rho(i,j,k)*qdiag(i,j,k,qd_vtg)*q3d(i,j,k,nqg)
          dum2(i,j,k) = dum2(i,j,k)+dum1(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

        nvar = nvar+1
        varname(nvar) = 'prec'
        vardesc(nvar) = 'total precip flux'
        varunit(nvar) = 'kg m-2 s-1'
        call getavg3d(dum2,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

    IF( imoist.eq.1 )THEN
      ! fraction of cloudy/rainy grid points:

!!!        nvar = nvar+1
!!!        varname(nvar) = 'qcrit'
!!!        vardesc(nvar) = 'qcrit'
!!!        varunit(nvar) = 'kg/kg'
!!!        do k=1,nk
!!!          savg(k) = qcrit(k)
!!!        enddo
!!!        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        dum1(i,j,k) = 0.0
        dum2(i,j,k) = 0.0
        dum3(i,j,k) = 0.0
        dum4(i,j,k) = 0.0
        dum5(i,j,k) = 0.0
      enddo
      enddo
      enddo

      do k=1,nk
        if( nqc.ge.1 )then
          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            if( q3d(i,j,k,nqc).ge.qcrit(k) ) dum1(i,j,k) = dum1(i,j,k)+1.0
          enddo
          enddo
        endif
        if( nqr.ge.1 )then
          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            if( q3d(i,j,k,nqr).ge.qcrit(k) ) dum2(i,j,k) = dum2(i,j,k)+1.0
          enddo
          enddo
        endif
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          if( ql(i,j,k).ge.qcrit(k) ) dum3(i,j,k) = dum3(i,j,k)+1.0
        enddo
        enddo
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          if( qice(i,j,k).ge.qcrit(k) ) dum4(i,j,k) = dum4(i,j,k)+1.0
        enddo
        enddo
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          if( (ql(i,j,k)+qice(i,j,k)).ge.qcrit(k) ) dum5(i,j,k) = dum5(i,j,k)+1.0
        enddo
        enddo
      enddo

        nvar = nvar+1
        varname(nvar) = 'qcfrac'
        vardesc(nvar) = 'cloud water fraction'
        varunit(nvar) = 'dimensionless'
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qrfrac'
        vardesc(nvar) = 'rain water fraction'
        varunit(nvar) = 'dimensionless'
        call getavg3d(dum2,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qlfrac'
        vardesc(nvar) = 'liquid water fraction (qc+qr)'
        varunit(nvar) = 'dimensionless'
        call getavg3d(dum3,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qifrac'
        vardesc(nvar) = 'ice water fraction (qs+qg+qi)'
        varunit(nvar) = 'dimensionless'
        call getavg3d(dum4,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qtfrac'
        vardesc(nvar) = 'total water fraction (qc+qr+qs+qg+qi)'
        varunit(nvar) = 'dimensionless'
        call getavg3d(dum5,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

    ENDIF

      !c-c-c-c-c-c-c-c-c-c


    IF( imoist.eq.1 )THEN

      ! mass fractions:

      !$acc parallel loop gang vector default(present) private(k)
      do k=1,nk+1
        tmass(k) = 0.0
        clw(k)   = 0.0
        cli(k)   = 0.0
        plw(k)   = 0.0
        pli(k)   = 0.0
      ENDDO

      !$acc parallel default(present) private(i,j,k) reduction(+:tmasst)
      !$acc loop gang
      DO k=1,nk
        tmasst = 0.0d0
        !$acc loop vector reduction(+:tmasst)
        do j=1,nj
        do i=1,ni
          tmasst = tmasst+rho(i,j,k)*(1.0+q3d(i,j,k,nqv)+ql(i,j,k)+qice(i,j,k))
        enddo
        enddo
        tmass(k) = tmasst
      ENDDO
      !$acc end parallel

#if 0
      !$acc parallel default(present) private(i,j,k,n,qflag) reduction(+:tmpw,tmpi)
#if 1
        !$acc loop seq
        DO n=nql1,nql2
          DO k=1,nk
            tmpw=0.0d0
            do j=1,nj
            do i=1,ni
              tmpw = tmpw+rho(i,j,k)*q3d(i,j,k,n)
            enddo
            enddo
            if(cloudvar(n)) then 
               clw(k)=tmpw
            else
               plw(k)=tmpw
            endif
          ENDDO
        ENDDO
        !$acc loop seq
        DO n=nqs1,nqs2
          DO k=1,nk
            tmpi=0.0d0
            do j=1,nj
            do i=1,ni
              tmpi = tmpi+rho(i,j,k)*q3d(i,j,k,n)
            enddo
            enddo
            if(cloudvar(n)) then 
               cli(k)=tmpi
            else
               pli(k)=tmpi
            endif
          ENDDO
        ENDDO

#else
        !$acc loop seq
        DO n=1,numq
          qflag = 0
          if( qflag.eq.0 .and. cloudvar(n) .and. (n.ge.nql1) .and. (n.le.nql2) )then
            qflag = 1
            !$acc loop gang
            DO k=1,nk
              clwt=0.0d0
              !$acc loop vector collapse(2) reduction(+:clwt)
              do j=1,nj
              do i=1,ni
                clwt = clwt+rho(i,j,k)*q3d(i,j,k,n)
              enddo
              enddo
              clw(k)=clwt
            ENDDO
          endif
          if( qflag.eq.0 .and. cloudvar(n) .and. (n.ge.nqs1) .and. (n.le.nqs2) )then
            qflag = 1
            !$acc loop gang
            DO k=1,nk
              clit=0.0d0
              !$acc loop vector collapse(2) reduction(+:clit)
              do j=1,nj
              do i=1,ni
                clit = clit+rho(i,j,k)*q3d(i,j,k,n)
              enddo
              enddo
              cli(k)=clit
            ENDDO
          endif
          if( qflag.eq.0 .and. (n.ge.nql1) .and. (n.le.nql2) )then
            qflag = 1
            !$acc loop gang
            DO k=1,nk
              plwt=0.0d0
              !$acc loop vector collapse(2) reduction(+:plwt)
              do j=1,nj
              do i=1,ni
                plwt = plwt+rho(i,j,k)*q3d(i,j,k,n)
              enddo
              enddo
              plw(k)=plwt
            ENDDO
          endif
          if( qflag.eq.0 .and. (n.ge.nqs1) .and. (n.le.nqs2) )then
            qflag = 1
            !$acc loop gang
            DO k=1,nk
              plit=0.0d0
              !$acc loop vector collapse(2) reduction(+:plit)
              do j=1,nj
              do i=1,ni
                plit = plit+rho(i,j,k)*q3d(i,j,k,n)
              enddo
              enddo
              pli(k)=plit
            ENDDO
          endif
        ENDDO
#endif
      !$acc end parallel
#endif

#ifdef MPI
      !!$acc host_data use_device(tmass,clw,cli,plw,pli)
      !$acc update host(tmass,clw,cli,plw,pli)
      call MPI_ALLREDUCE(MPI_IN_PLACE,tmass,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,clw  ,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,cli  ,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,plw  ,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,pli  ,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      !$acc update device(tmass,clw,cli,plw,pli)
      !!$acc end host_data
#endif
      !$acc parallel loop gang vector default(present) private(k)
      do k=1,nk
        clw(k) = clw(k)/tmass(k)
        cli(k) = cli(k)/tmass(k)
        plw(k) = plw(k)/tmass(k)
        pli(k) = pli(k)/tmass(k)
      enddo

      nvar = nvar+1
      varname(nvar) = 'clw'
      vardesc(nvar) = 'mass fraction of cloud liquid water'
      varunit(nvar) = 'kg/kg'
      call write3d(clw,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'cli'
      vardesc(nvar) = 'mass fraction of cloud ice'
      varunit(nvar) = 'kg/kg'
      call write3d(cli,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'plw'
      vardesc(nvar) = 'mass fraction of precipitating liquid water'
      varunit(nvar) = 'kg/kg'
      call write3d(plw,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      nvar = nvar+1
      varname(nvar) = 'pli'
      vardesc(nvar) = 'mass fraction of preciptating ice'
      varunit(nvar) = 'kg/kg'
      call write3d(pli,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

    ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'thv'
        vardesc(nvar) = 'virtual potential temperature'
        varunit(nvar) = 'K'

        call getavg3d(thv,savg)

        ! save thvavg:
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          thvavg(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      if( ipbl.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'brz'
        vardesc(nvar) = 'bulk Richardson number (as function of height)'
        varunit(nvar) = 'n.d.'

      if( imoist.eq.1 )then
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          tmpqsfc(i,j) = rslf(psfc(i,j),tsk(i,j))
        enddo
        enddo
        !...
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum2(i,j,k) = q3d(i,j,k,nqv)
        enddo
        enddo
        enddo
      else
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          tmpqsfc(i,j) = 0.0
        enddo
        enddo
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk 
        do j=1,nj
        do i=1,ni
        dum2(i,j,k) = 0.0
        enddo
        enddo
        enddo
      endif

      !$acc parallel loop gang vector collapse(2) default(present) private(i,j,thvx)
      do j=1,nj
      do i=1,ni
        thvx = (th0(i,j,1)+th3d(i,j,1))*(1.0+repsm1*dum2(i,j,1))
        govthv(i,j) = g/thvx
      enddo
      enddo

    hloop:  &
    DO nloop = 1 , 2

      k = 1

    !PORTME
    if( nloop.eq.1 )then
      !$acc parallel loop gang vector collapse(2) default(present) private(i,j,thvx)
      do j=1,nj
      do i=1,ni
        thvx = (th0(i,j,1)+th3d(i,j,1))*(1.0+repsm1*dum2(i,j,1))
        thvsfc(i,j) = tsk(i,j)*((p00/psfc(i,j))**rovcp)*(1.0+repsm1*tmpqsfc(i,j))
        thv1(i,j) = thvx
      enddo
      enddo
    else
      !$acc parallel loop gang vector collapse(2) default(present) &
      !$acc private(i,j,thvx,thf1,thf2,thvflux,ws,gamfac,tmpprt)
      do j=1,nj
      do i=1,ni
        thvx = (th0(i,j,1)+th3d(i,j,1))*(1.0+repsm1*dum2(i,j,1))
        thf1 = (1.0+repsm1*dum2(i,j,1))
        thf2 = repsm1*(th0(i,j,1)+th3d(i,j,1))
        thvflux = max( 0.0 , thf1*thflux(i,j) + thf2*qvflux(i,j) )
        ws = ( ust(i,j)**3 + 8.0*karman*0.5*govthv(i,j)*thvflux*hpbl(i,j) )**0.3333333
        ws = min( ws , ust(i,j)*16.0 )
        ws = max( ws , ust(i,j)/5.0 )
        ws = max( ws , 0.0001 )
        gamfac = 6.8/ws
        tmpprt = max( 0.0 , min( gamfac*thflux(i,j) , 3.0 )*thf1    &
                           +min( gamfac*qvflux(i,j) , 2.0e-3 )*thf2 )
        thv1(i,j) = thvx + tmpprt
      enddo
      enddo
    endif

      !$acc parallel loop gang vector collapse(3) default(present) &
      !$acc private(i,j,k,thvx,ubar,vbar)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        thvx = (th0(i,j,k)+th3d(i,j,k))*(1.0+repsm1*dum2(i,j,k))
        ubar = 0.5*(u3d(i,j,k)+u3d(i+1,j,k))
        vbar = 0.5*(v3d(i,j,k)+v3d(i,j+1,k))
        dum1(i,j,k) = govthv(i,j)*zh(i,j,k)*(thvx-thv1(i,j))/max( 1.0 , ubar**2 + vbar**2 )
      enddo
      enddo
      enddo

    ENDDO  hloop

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif


      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'thl'
        vardesc(nvar) = 'liquid-water potential temp (simple def.)'
        varunit(nvar) = 'K'

        ! (use simple definition ... for now)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          thl(i,j,k) = th(i,j,k)*( 1.0-xlv*ql(i,j,k)/(cp*th(i,j,k)*(pi0(i,j,k)+pp3d(i,j,k))) )
        enddo
        enddo
        enddo

        call getavg3d(thl,savg)

        ! save thlavg:
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          thlavg(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'the'
        vardesc(nvar) = 'equivalent potential temperature'
        varunit(nvar) = 'K'

      if( ptype.eq.0 )then
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,tx,qx,px,tlcl,ee)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          tx = (th0(i,j,k)+th3d(i,j,k))*(pi0(i,j,k)+pp3d(i,j,k))
          qx = q3d(i,j,k,nqv)
          px = prs(i,j,k)
            ee=0.01*px*qx/(eps+qx)
            tlcl=55.0+2840.0/(3.5*alog(tx)-log(1.0e-20+ee)-4.805)
          dum1(i,j,k) = tx*((p00/px)**(0.2854*(1.0-0.28*qx)))   &
                          *exp(((3376.0/tlcl)-2.54)*qx*(1.0+0.81*qx))
        enddo
        enddo
        enddo
      else
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,tx,qx,px,tlcl,ee)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          tx = (th0(i,j,k)+th3d(i,j,k))*(pi0(i,j,k)+pp3d(i,j,k))
          qx = q3d(i,j,k,nqv)
          px = prs(i,j,k)
          if(q3d(i,j,k,nqc).ge.clwsat)then
            tlcl=tx
          else
            ee=0.01*px*qx/(eps+qx)
            tlcl=55.0+2840.0/(3.5*alog(tx)-log(1.0e-20+ee)-4.805)
          endif
          dum1(i,j,k) = tx*((p00/px)**(0.2854*(1.0-0.28*qx)))   &
                          *exp(((3376.0/tlcl)-2.54)*qx*(1.0+0.81*qx))
        enddo
        enddo
        enddo
      endif

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'rh'
        vardesc(nvar) = 'relative humidity (wrt liquid)'
        varunit(nvar) = 'nondimensional'
     
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = q3d(i,j,k,nqv)/rslf(prs(i,j,k),th(i,j,k)*(pi0(i,j,k)+pp3d(i,j,k)))
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 .and. iice.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'rhi'
        vardesc(nvar) = 'relative humidity (wrt ice)'
        varunit(nvar) = 'nondimensional'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = q3d(i,j,k,nqv)/rsif(prs(i,j,k),th(i,j,k)*(pi0(i,j,k)+pp3d(i,j,k)))
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( cm1setup.eq.1 .and. iusetke )THEN

        nvar = nvar+1
        varname(nvar) = 'stke'
        vardesc(nvar) = 'subgrid turbulence kinetic energy'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk+1
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = tke3d(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
!!!        savg(1) = grads_undef
!!!        savg(nk+1) = grads_undef

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          stke(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( cm1setup.eq.1 .and. iusetke )THEN
        ! tke interpolated to s points:

        nvar = nvar+1
        varname(nvar) = 'stkeinterp'
        vardesc(nvar) = 'subgrid turbulence kinetic energy'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 's'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = 0.5*(tke3d(i,j,k)+tke3d(i,j,k+1))
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( cm1setup.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'dissten'
        vardesc(nvar) = 'parameterized dissipation rate'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dissten(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF


      !c-c-c-c-c-c-c-c-c-c

      IF( cm1setup.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'epst'
        vardesc(nvar) = 'epst'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = epst(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( cm1setup.eq.1 )THEN

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k) 
        do k = kb,ke
        do j = ib,je
        do i = ib,ie
          wtk(i,j,k) = 0.0
          wtmp(i,j,k) = 0.0
        enddo
        enddo
        enddo
        
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j) 
        do j = ib,je
        do i = ib,ie
          wtmp(i,j,ke+1) = 0.0
        enddo
        enddo

          if(timestats.ge.1) time_diag = time_diag+mytime()
          if( sgsmodel.eq.5 .or. sgsmodel.eq.6 )then
              ! use mij:
            call getepst(xh,rxh,uh,xf,rxf,uf,vh,vf,mh,c1,c2,mf,u3d,v3d,w3d,  &
                         m11,m12,m13,m22,m23,m33,rf,                         &
                         dum1,dum2,rrw,wtk,wtmp)
          else
              ! use tij:
            call getepst(xh,rxh,uh,xf,rxf,uf,vh,vf,mh,c1,c2,mf,u3d,v3d,w3d,  &
                         t11,t12,t13,t22,t23,t33,rf,                         &
                         dum1,dum2,rrw,wtk,wtmp)
          endif

      !...............

        nvar = nvar+1
        varname(nvar) = 'epstp'
        vardesc(nvar) = 'positive part of epst'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        call getavg3d(wtk(ib,jb,kb),savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !...............

        nvar = nvar+1
        varname(nvar) = 'epstn'
        vardesc(nvar) = 'negative part of epst'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        call getavg3d(wtmp(ib,jb,kb),savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( cm1setup.eq.1 .and. dot2p )THEN

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=kb,ke
        do j=jb,je
        do i=ib,ie
          wtk(i,j,k) = 0.0
          wtmp(i,j,k) = 0.0
        enddo
        enddo
        enddo

        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=jb,je
        do i=ib,ie
          wtmp(i,j,ke+1) = 0.0
        enddo
        enddo

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk+1
        do j=0,nj+2
        do i=0,ni+2
          dum1(i,j,k) = 0.0
          dum2(i,j,k) = 0.0
          dum3(i,j,k) = kmw(k)*(u1b(k)-u1b(k-1))*rdz*mf(1,1,k)
          dum4(i,j,k) = 0.0
          dum5(i,j,k) = kmw(k)*(v1b(k)-v1b(k-1))*rdz*mf(1,1,k)
          dum6(i,j,k) = 0.0
        enddo
        enddo
        enddo

            if(timestats.ge.1) time_diag = time_diag+mytime()
            call getepst(xh,rxh,uh,xf,rxf,uf,vh,vf,mh,c1,c2,mf,u3d,v3d,w3d,  &
                         dum1,dum2,dum3,dum4,dum5,dum6,rf,                         &
                         th  ,ql  ,rrw,wtk,wtmp)

      !...............

        nvar = nvar+1
        varname(nvar) = 'epsw'
        vardesc(nvar) = 'epsw'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        call getavg3d(rrw(ib,jb,kb),savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !...............

        nvar = nvar+1
        varname(nvar) = 'epswp'
        vardesc(nvar) = 'positive part of epsw'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        call getavg3d(wtk(ib,jb,kb),savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !...............

        nvar = nvar+1
        varname(nvar) = 'epswn'
        vardesc(nvar) = 'negative part of epsw'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        call getavg3d(wtmp(ib,jb,kb),savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( cm1setup.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'epsd1'
        vardesc(nvar) = 'epsd1'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = epsd1(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( cm1setup.ge.1 )THEN
        nvar = nvar+1
        varname(nvar) = 'epsd2'
        vardesc(nvar) = 'epsd2'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = epsd2(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      ipbl1:  &
      IF( ipbl.eq.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'xkzh'
        vardesc(nvar) = 'eddy diffusivity for heat (from YSU)'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = xkzh(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'xkzq'
        vardesc(nvar) = 'eddy diffusivity for moisture (from YSU)'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = xkzq(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'xkzm'
        vardesc(nvar) = 'eddy viscosity (from YSU)'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = xkzm(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF  ipbl1

      !c-c-c-c-c-c-c-c-c-c

      ipbl3:  &
      IF( ipbl.eq.3 )THEN

        nvar = nvar+1
        varname(nvar) = 'dkt3d'
        vardesc(nvar) = 'Thermal Diffusivity (from GFSEDMF)'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = xkzh(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'dku3d'
        vardesc(nvar) = 'Momentum Diffusivity (from GFSEDMF)'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = xkzm(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF  ipbl3

!BLOCK
      !c-c-c-c-c-c-c-c-c-c

      ipbl4:  &
      IF( ipbl.eq.4 .or. ipbl.eq.5 )THEN

        nvar = nvar+1
        varname(nvar) = 'qke'
        vardesc(nvar) = 'twice TKE from MYNN'
        varunit(nvar) = 'm2/s2'
        call getavg3d(qke,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qwt'
        vardesc(nvar) = 'TKE vertical transport (MYNN)'
        varunit(nvar) = 'm2/s3'
        call getavg3d(qwt,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qshear'
        vardesc(nvar) = 'TKE Production - shear (MYNN)'
        varunit(nvar) = 'm2/s3'
        call getavg3d(qshear,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qbuoy'
        vardesc(nvar) = 'TKE Production - buoyancy (MYNN)'
        varunit(nvar) = 'm2/s3'
        call getavg3d(qbuoy,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qdiss'
        vardesc(nvar) = 'TKE dissipation (MYNN)'
        varunit(nvar) = 'm2/s3'
        call getavg3d(qdiss,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'dqke'
        vardesc(nvar) = 'TKE change (MYNN)'
        varunit(nvar) = 'm2/s3'
        call getavg3d(dqke,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qke_adv'
        vardesc(nvar) = 'advection of twice TKE from MYNN'
        varunit(nvar) = 'm2/s3'
        call getavg3d(qke_adv,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'sh3d'
        vardesc(nvar) = 'Stability function for heat (MYNN)'
        varunit(nvar) = ' '
        call getavg3d(sh3d,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'tsq'
        vardesc(nvar) = 'liquid water pottemp variance (MYNN)'
        varunit(nvar) = 'K2'
        call getavg3d(tsq,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qsq'
        vardesc(nvar) = 'liquid water variance (MYNN)'
        varunit(nvar) = '(kg/kg)**2'
        call getavg3d(qsq,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'cov'
        vardesc(nvar) = 'liquid water-liquid water pottemp covariance (MYNN)'
        varunit(nvar) = 'K kg/kg'
        call getavg3d(cov,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'exch_h'
        vardesc(nvar) = 'Thermal Diffusivity (from MYNN)'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = xkzh(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'exch_m'
        vardesc(nvar) = 'Momentum Diffusivity (from MYNN)'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = xkzm(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'el_pbl'
        vardesc(nvar) = 'Length scale from PBL'
        varunit(nvar) = 'm'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = el_pbl(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

   !..................................

      ENDIF  ipbl4

      !c-c-c-c-c-c-c-c-c-c

      ipbl6:  &
      IF( ipbl.eq.6 )THEN

        nvar = nvar+1
        varname(nvar) = 'tke_myj'
        vardesc(nvar) = 'TKE from PBL'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk+1
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = tke_myj(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'el_myj'
        vardesc(nvar) = 'Length scale from PBL'
        varunit(nvar) = 'm'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk+1
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = el_myj(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'exch_h'
        vardesc(nvar) = 'Thermal Diffusivity (from MYJ)'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = xkzh(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'exch_m'
        vardesc(nvar) = 'Momentum Diffusivity (from MYJ)'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = xkzm(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF  ipbl6

      !c-c-c-c-c-c-c-c-c-c

      IF( ipbl.eq.1 .or. ipbl.eq.3 .or. ipbl.eq.4 .or. ipbl.eq.5 .or. ipbl.eq.6 )THEN
      if( horizturb.eq.1 )then
        nvar = nvar+1
        varname(nvar) = 'kmh'
        vardesc(nvar) = 'eddy viscosity in horiz direction'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = kmh(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif
      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( sgsmodel.ge.1 .or. output_nm.eq.1 .or. ipbl.ge.1 )THEN
        nvar = nvar+1
        varname(nvar) = 'nm'
        vardesc(nvar) = 'squared Brunt-Vaisala frequency'
        varunit(nvar) = 's-2'
        vargrid(nvar) = 'w'

        call getavg3d(nm(ib,jb,kb),savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( cm1setup.ge.1 .or. output_def.eq.1 .or. ipbl.ge.1 .or. horizturb.eq.1 )THEN
        nvar = nvar+1
        varname(nvar) = 'defh'
        vardesc(nvar) = 'horizontal deformation'
        varunit(nvar) = 's-2'
        vargrid(nvar) = 'w'

        call getavg3d(defh(ib,jb,kb),savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'defv'
        vardesc(nvar) = 'vertical deformation'
        varunit(nvar) = 's-2'
        vargrid(nvar) = 'w'

        call getavg3d(defv(ib,jb,kb),savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      ENDIF

      !c-c-c-c-c-c-c-c-c-c

    turbcheck:  &
    IF( sgsmodel.ge.1 .or. ipbl.eq.2 )THEN

      if( iusekm.eq.1 )then
        nvar = nvar+1
        varname(nvar) = 'kmh'
        vardesc(nvar) = 'eddy viscosity in horiz direction'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk+1
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = kmh(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      !c-c-c-c-c-c-c-c-c-c

      if( iusekm.eq.1 )then
        nvar = nvar+1
        varname(nvar) = 'kmv'
        vardesc(nvar) = 'eddy viscosity in vert direction'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk+1
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = kmv(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      !c-c-c-c-c-c-c-c-c-c

      if( iusekh.eq.1 )then
        nvar = nvar+1
        varname(nvar) = 'khh'
        vardesc(nvar) = 'eddy diffusivity in horiz direction'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk+1
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = khh(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      !c-c-c-c-c-c-c-c-c-c

      if( iusekh.eq.1 )then
        nvar = nvar+1
        varname(nvar) = 'khv'
        vardesc(nvar) = 'eddy diffusivity in vert direction'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk+1
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = khv(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      !c-c-c-c-c-c-c-c-c-c

      if( iusekh.eq.1 .and. iusekm.eq.1 )then
        nvar = nvar+1
        varname(nvar) = 'pr'
        vardesc(nvar) = 'subgrid Prandtl number'
        varunit(nvar) = 'nondimensional'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = (1.0e-20+kmv(i,j,k))/(1.0e-20+khv(i,j,k))
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'rinum'
        vardesc(nvar) = 'Richardson number in subgrid model'
        varunit(nvar) = 'nondimensional'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          dum1(i,j,1) = 0.0
          dum1(i,j,nk+1) = 0.0
        enddo
        enddo

        IF(tconfig.eq.1)THEN
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,prinv)
          do k=2,nk
          do j=1,nj
          do i=1,ni
            prinv = (1.0e-20+khv(i,j,k))/(1.0e-20+kmv(i,j,k))
            dum1(i,j,k) = nm(i,j,k)*prinv/(1.0e-20+defv(i,j,k)+defh(i,j,k))
          enddo
          enddo
          enddo
        ELSEIF(tconfig.eq.2)THEN
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,prinv)
          do k=2,nk
          do j=1,nj
          do i=1,ni
            prinv = (1.0e-20+khv(i,j,k))/(1.0e-20+kmv(i,j,k))
            dum1(i,j,k) = nm(i,j,k)*prinv/(1.0e-20+defv(i,j,k))
          enddo
          enddo
          enddo
        ENDIF

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'grdscl'
        vardesc(nvar) = 'grid scale'
        varunit(nvar) = 'm'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = ( ((dx*ruh(i))*(dy*rvh(j)))*(dz*rmf(i,j,k)) )**0.33333333
          ! cm1r17:  wall condition near surface
          dum1(i,j,k) = sqrt(1.0/( 1.0/(dum1(i,j,k)**2)                                  &
                                    +1.0/((karman*((zf(i,j,k)-zf(i,j,1))+znt(i,j))*rcs)**2)  &
                                 ) )
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      IF( cm1setup.eq.1 .and. iusetke )THEN

        nvar = nvar+1
        varname(nvar) = 'lenscl'
        vardesc(nvar) = 'length scale in subgrid turbulence model'
        varunit(nvar) = 'm'
        vargrid(nvar) = 'w'

        call getavg3d(lenscl,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

    ENDIF  turbcheck

      !c-c-c-c-c-c-c-c-c-c

      t2pstuff:  &
      IF( dot2p )THEN

        !-----

        nvar = nvar+1
        varname(nvar) = 'gamk'
        vardesc(nvar) = 'gamk'
        varunit(nvar) = 'unitless'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = gamk(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 'gamwall'
        vardesc(nvar) = 'gamwall'
        varunit(nvar) = 'unitless'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = gamwall(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 'kmw'
        vardesc(nvar) = 'kmw'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = kmw(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 'u1b'
        vardesc(nvar) = 'u1b'
        varunit(nvar) = 'm/s'
        vargrid(nvar) = 's'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = u1b(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 'v1b'
        vardesc(nvar) = 'v1b'
        varunit(nvar) = 'm/s'
        vargrid(nvar) = 's'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = v1b(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 'l2p'
        vardesc(nvar) = 'l2p'
        varunit(nvar) = 'm'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = l2p(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 't2pm1'
        vardesc(nvar) = 't2pm1'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = t2pm1(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 't2pm2'
        vardesc(nvar) = 't2pm2'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = t2pm2(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 't2pm3'
        vardesc(nvar) = 't2pm3'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = t2pm3(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 't2pm4'
        vardesc(nvar) = 't2pm4'
        varunit(nvar) = 'm2/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = t2pm4(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

      ENDIF  t2pstuff

      !c-c-c-c-c-c-c-c-c-c

      IF( sgsmodel.ge.1 .and. sgsmodel.le.6 )THEN

        !-----

        nvar = nvar+1
        varname(nvar) = 'cmz'
        vardesc(nvar) = 'cmz'
        varunit(nvar) = 'unitless'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = cmz(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 'csz'
        vardesc(nvar) = 'csz'
        varunit(nvar) = 'unitless'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = csz(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 'ce1z'
        vardesc(nvar) = 'ce1z'
        varunit(nvar) = 'unitless'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = ce1z(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 'ce2z'
        vardesc(nvar) = 'ce2z'
        varunit(nvar) = 'unitless'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          savg(k) = ce2z(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wsp'
        vardesc(nvar) = 'horizontal wind speed (ground-relative)'
        varunit(nvar) = 'm/s'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = sqrt( (0.5*(u3d(i,j,k)+u3d(i+1,j,k))+umove)**2 &
                             +(0.5*(v3d(i,j,k)+v3d(i,j+1,k))+vmove)**2 )
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wspinterp'
        vardesc(nvar) = 'horizontal wind speed (ground-relative)'
        varunit(nvar) = 'm/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum2(i,j,k) = 0.5*(dum1(i,j,k-1)+dum1(i,j,k))
        enddo
        enddo
        enddo

        call getavg3d(dum2,savg)
        !$acc parallel 
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wspan'
        vardesc(nvar) = 'analytic wind speed (neutral)'
        varunit(nvar) = 'm/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = (ustbar/karman)*alog( (zh(1,1,k)+zntbar)/zntbar )
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wspa'
        vardesc(nvar) = 'analytic wind speed'
        varunit(nvar) = 'm/s'

        !$acc parallel loop gang vector default(present) private(k,zeta,tpsim)
        do k=1,nk
          if( abs(molbar).gt.1.0e-6 )then
            zeta = zh(1,1,k)/molbar
            call stabil_funcs(zeta,tphim,tphih,tpsim,tpsih)
          else
            tpsim = 0.0
          endif
          savg(k) = (ustbar/karman)*( alog(zh(1,1,k)/zntbar) - tpsim )
          wspa(k) = savg(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'sdwsp'
        vardesc(nvar) = 'standard deviation of horizontal wind speed'
        varunit(nvar) = 'm/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = ( dum1(i,j,k) - wsp(k) )**2
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

    IF( iptra.eq.1 )THEN

      !$acc parallel loop gang vector collapse(2) default(present)
      do n=1,npt
      do k=0,nk+1
         ptavg(k,n) = 0.0
      enddo
      enddo

      DO n=1,npt

        nvar = nvar+1
        a1 = 'pt                                                                              '
        if(n.le.9)then
          write(a1(3:3),155) n
155       format(i1.1)
        elseif(n.le.99)then
          write(a1(3:4),154) n
154       format(i2.2)
        else
          write(a1(3:5),153) n
153       format(i3.3)
        endif
        varname(nvar) = a1
        vardesc(nvar) = 'passive tracer mixing ratio'
        varunit(nvar) = 'g/g'

        call getavg3d(pt3d(ib,jb,kb,n),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          ptavg(k,n) = savg(k)
        enddo

      ENDDO

    ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( testcase.eq.4 .or. testcase.eq.5 )THEN

        ! note:  o30 array stores frad

        nvar = nvar+1
        varname(nvar) = 'frad'
        vardesc(nvar) = 'radiative flux'
        varunit(nvar) = 'W/m2'
        call getavg3d(o30(ibr,jbr,kbr),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        ! note:  o30 array stores frad

        nvar = nvar+1
        varname(nvar) = 'thrad'
        vardesc(nvar) = 'pot temp tendency from radiation scheme'
        varunit(nvar) = 'K/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = -(o30(i,j,k+1)-o30(i,j,k))*rdz*mh(i,j,k)  &
                         /(cp*rho0(i,j,k))
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( radopt.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'swten'
        vardesc(nvar) = 'temperature tendency, sw radiation'
        varunit(nvar) = 'K/s'
        call getavg3d(swten(ibr,jbr,kbr),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'lwten'
        vardesc(nvar) = 'temperature tendency, sw radiation'
        varunit(nvar) = 'K/s'
        call getavg3d(lwten(ibr,jbr,kbr),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'swtenc'
        vardesc(nvar) = 'temperature tendency, sw radiation (clear sky)'
        varunit(nvar) = 'K/s'
        call getavg3d(swtenc(ibr,jbr,kbr),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'lwtenc'
        vardesc(nvar) = 'temperature tendency, sw radiation (clear sky)'
        varunit(nvar) = 'K/s'
        call getavg3d(lwtenc(ibr,jbr,kbr),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'cldfra'
        vardesc(nvar) = 'cloud fraction from radiation scheme'
        varunit(nvar) = 'nondimensional'
        call getavg3d(cldfra(ibr,jbr,kbr),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      doit = .true.
      IF( doit )THEN
      IF( radopt.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'effc'
        vardesc(nvar) = 'cloud droplet effective radius'
        varunit(nvar) = 'micron'
        call getavg3d(effc(ibr,jbr,kbr),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'effr'
        vardesc(nvar) = 'rain effective radius'
        varunit(nvar) = 'micron'
        call getavg3d(effr(ibr,jbr,kbr),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'effs'
        vardesc(nvar) = 'snow effective radius'
        varunit(nvar) = 'micron'
        call getavg3d(effs(ibr,jbr,kbr),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'effi'
        vardesc(nvar) = 'cloud ice effective radius'
        varunit(nvar) = 'micron'
        call getavg3d(effi(ibr,jbr,kbr),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'effg'
        vardesc(nvar) = 'graupel/hail effective radius'
        varunit(nvar) = 'micron'
        call getavg3d(effg(ibr,jbr,kbr),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'effis'
        vardesc(nvar) = 'effis'
        varunit(nvar) = 'micron'
        call getavg3d(effis(ibr,jbr,kbr),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF
      ENDIF

      !c-c-c-c-c-c-c-c-c-c

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    ! 1d (z) diagnostics:

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'ndws'
        vardesc(nvar) = 'nondimensional wind shear'
        varunit(nvar) = 'nondimensinal'
        vargrid(nvar) = 'w'

        !PORTME
        !savg = 0.0

        !$acc parallel default(present) private(k)
        savg(1) = grads_undef
        !$acc loop gang vector
        do k=2,nk
!!!          savg(k) = (wsp(k)-wsp(k-1))*rdz*mf(1,1,k)*karman*zf(1,1,k)/ustbar
          ! 180212:
          savg(k) = (wsp(k)-wsp(k-1))/(wspa(k)-wspa(k-1))
        enddo
        savg(nk+1) = grads_undef
        !$acc end parallel

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'dtdz'
        vardesc(nvar) = 'vertical gradient of avg theta'
        varunit(nvar) = 'K/m'
        vargrid(nvar) = 'w'

        !savg = 0.0

        
        !$acc parallel default(present) private(k)
        savg(1) = grads_undef
        !$acc loop gang vector
        do k=2,nk
          savg(k) = (thavg(k)-thavg(k-1))*rdz*mf(1,1,k)
        enddo
        savg(nk+1) = grads_undef
        !$acc end parallel

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'dudz'
        vardesc(nvar) = 'vertical gradient of avg u'
        varunit(nvar) = '1/s'
        vargrid(nvar) = 'w'

        !PORTME
        !savg = 0.0

        !$acc parallel default(present) private(k)
        savg(1) = grads_undef
        !$acc loop gang vector
        do k=2,nk
          savg(k) = (uavg(k)-uavg(k-1))*rdz*mf(1,1,k)
        enddo
        savg(nk+1) = grads_undef
        !$acc end parallel

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'dvdz'
        vardesc(nvar) = 'vertical gradient of avg v'
        varunit(nvar) = '1/s'
        vargrid(nvar) = 'w'

        !savg = 0.0

        !$acc parallel default(present) private(k)
        savg(1) = grads_undef
        !$acc loop gang vector
        do k=2,nk
          savg(k) = (vavg(k)-vavg(k-1))*rdz*mf(1,1,k)
        enddo
        savg(nk+1) = grads_undef
        !$acc end parallel

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'dsdz'
        vardesc(nvar) = 'vertical gradient of avg horiz wind speed'
        varunit(nvar) = '1/s'
        vargrid(nvar) = 'w'

        !JMD savg = 0.0

        !$acc parallel default(present) private(k)
        savg(1) = grads_undef
        !$acc loop gang vector private(k)
        do k=2,nk
          savg(k) = (wsp(k)-wsp(k-1))*rdz*mf(1,1,k)
        enddo
        savg(nk+1) = grads_undef
        !$acc end parallel

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 )THEN
        nvar = nvar+1
        varname(nvar) = 'dqvdz'
        vardesc(nvar) = 'vertical gradient of avg qv'
        varunit(nvar) = 'g/g/m'
        vargrid(nvar) = 'w'

        !JMD savg = 0.0

        !$acc parallel default(present) private(k)
        savg(1) = grads_undef
        !$acc loop gang vector private(k)
        do k=2,nk
          savg(k) = (qvavg(k)-qvavg(k-1))*rdz*mf(1,1,k)
        enddo
        savg(nk+1) = grads_undef
        !$acc end parallel

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      ENDIF

      !c-c-c-c-c-c-c-c-c-c

!!!      IF( testcase.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'ug'
        vardesc(nvar) = 'u-component of geostrophic wind'
        varunit(nvar) = 'm/s'
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = ug(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'vg'
        vardesc(nvar) = 'v-component of geostrophic wind'
        varunit(nvar) = 'm/s'
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = vg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)


        nvar = nvar+1
        varname(nvar) = 'u0'
        vardesc(nvar) = 'base-state u'
        varunit(nvar) = 'm/s'
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = u0(1,1,k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'v0'
        vardesc(nvar) = 'base-state v'
        varunit(nvar) = 'm/s'
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = v0(1,1,k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

!!!      ENDIF


      IF( testcase.eq.6 .or. testcase.eq.10 )THEN

        nvar = nvar+1
        varname(nvar) = 'dvdr'
        vardesc(nvar) = 'radial gradient of V'
        varunit(nvar) = '1/s'
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = dvdr(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      lsn:  &
      IF( do_lsnudge )THEN

        !-----

        nvar = nvar+1
        varname(nvar) = 'lsnudge_u'
        vardesc(nvar) = 'lsnudge_u'
        varunit(nvar) = 'm/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = lsnudge_u(k,lsnudge_count)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 'lsnudge_v'
        vardesc(nvar) = 'lsnudge_v'
        varunit(nvar) = 'm/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = lsnudge_v(k,lsnudge_count)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 'lsnudge_th'
        vardesc(nvar) = 'lsnudge_th'
        varunit(nvar) = 'K'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = lsnudge_th(k,lsnudge_count)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

        nvar = nvar+1
        varname(nvar) = 'lsnudge_qv'
        vardesc(nvar) = 'lsnudge_qv'
        varunit(nvar) = 'kg/kg'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = lsnudge_qv(k,lsnudge_count)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !-----

      ENDIF  lsn

    
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    ! 3d vars:  2nd-order and 3rd-order calcs AT SCALAR POINTS:
    !                dum4  =  u-prime
    !                dum5  =  v-prime
    !                dum6  =  w-prime

        !PORTME
    !dum4 = 0.0
    !dum5 = 0.0
    !dum6 = 0.0

        !PORTME
    ! BUG:
    ! wavg(k+1) is never set
    !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
    do k=1,nk
    do j=1,nj
    do i=1,ni
      dum4(i,j,k) = 0.5*( (u3d(i,j,k)-uavg(k)) + (u3d(i+1,j,k)-uavg(k) ) )
      dum5(i,j,k) = 0.5*( (v3d(i,j,k)-vavg(k)) + (v3d(i,j+1,k)-vavg(k) ) )
      dum6(i,j,k) = 0.5*( (w3d(i,j,k)-wavg(k)) + (w3d(i,j,k+1)-wavg(k+1)) )
    enddo
    enddo
    enddo

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'upup'
        vardesc(nvar) = '< u-prime u-prime >'
        varunit(nvar) = 'm2/s2'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum4(i,j,k)*dum4(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'vpvp'
        vardesc(nvar) = '< v-prime v-prime >'
        varunit(nvar) = 'm2/s2'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum5(i,j,k)*dum5(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wpwp'
        vardesc(nvar) = '< w-prime w-prime >'
        varunit(nvar) = 'm2/s2'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum6(i,j,k)*dum6(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'upvp'
        vardesc(nvar) = '< u-prime v-prime >'
        varunit(nvar) = 'm2/s2'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum4(i,j,k)*dum5(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'upwp'
        vardesc(nvar) = '< u-prime w-prime >'
        varunit(nvar) = 'm2/s2'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum4(i,j,k)*dum6(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'vpwp'
        vardesc(nvar) = '< v-prime w-prime >'
        varunit(nvar) = 'm2/s2'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum5(i,j,k)*dum6(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'thvarr'
        vardesc(nvar) = 'pot temp variance, resolved'
        varunit(nvar) = 'K^2'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = (th(i,j,k)-thavg(k))**2
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'thvarrinterp'
        vardesc(nvar) = 'pot temp variance, resolved'
        varunit(nvar) = 'K^2'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
           dum1(i,j,1) = 0.0
        enddo
        enddo

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = ( 0.5*( (th(i,j,k  )-thavg(k  ))   &
                               +(th(i,j,k-1)-thavg(k-1)) ) )**2
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        !$acc parallel
        savg(1) = grads_undef
        savg(nk+1) = grads_undef
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'uptp'
        vardesc(nvar) = '< u-prime t-prime >'
        varunit(nvar) = 'K m/s'
        vargrid(nvar) = 's'

        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
           dum1(i,j,1) = 0.0
        enddo
        enddo

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = (th(i,j,k)-thavg(k))*dum4(i,j,k)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'vptp'
        vardesc(nvar) = '< v-prime t-prime >'
        varunit(nvar) = 'K m/s'
        vargrid(nvar) = 's'

        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
           dum1(i,j,1) = 0.0
        enddo
        enddo

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = (th(i,j,k)-thavg(k))*dum5(i,j,k)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)


      !c-c-c-c-c-c-c-c-c-c

      IF( imoist.eq.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'thvvarr'
        vardesc(nvar) = 'resolved virtual pot temp variance'
        varunit(nvar) = 'K^2'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = (thv(i,j,k)-thvavg(k))**2
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'thlvarr'
        vardesc(nvar) = 'resolved liquid-water pot temp variance'
        varunit(nvar) = 'K^2'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = (thl(i,j,k)-thlavg(k))**2
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'qvvarr'
        vardesc(nvar) = 'resolved qv variance'
        varunit(nvar) = 'g2/g2'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = (q3d(i,j,k,nqv)-qvavg(k))**2
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'qlvarr'
        vardesc(nvar) = 'resolved ql variance'
        varunit(nvar) = 'g2/g2'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = (ql(i,j,k)-qlavg(k))**2
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'qtvarr'
        vardesc(nvar) = 'resolved qt variance'
        varunit(nvar) = 'g2/g2'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = (q3d(i,j,k,nqv)+ql(i,j,k)-qtavg(k))**2
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wpwptp'
        vardesc(nvar) = '< w-prime w-prime theta-prime >'
        varunit(nvar) = 'K m2/s2'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum6(i,j,k)*dum6(i,j,k)*(th(i,j,k)-thavg(k))
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wptptp'
        vardesc(nvar) = '< w-prime theta-prime theta-prime >'
        varunit(nvar) = 'K^2 m/s'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum6(i,j,k)*(th(i,j,k)-thavg(k))**2
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c


    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    ! 3d vars:  2nd-order and 3rd-order calcs AT W POINTS:
    !                dum4  =  u-prime
    !                dum5  =  v-prime
    !                dum6  =  w-prime

    !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
    do j=1,nj
    do i=1,ni
      dum1(i,j,1)    = 0.0
      dum4(i,j,1)    = 0.0
      dum5(i,j,1)    = 0.0
      dum6(i,j,1)    = 0.0
      !dum1(i,j,nk+1) = 0.0
      !dum4(i,j,nk+1) = 0.0
      !dum5(i,j,nk+1) = 0.0
      !dum6(i,j,nk+1) = 0.0
    enddo
    enddo
    !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
    do k=2,nk
    do j=1,nj
    do i=1,ni
      dum4(i,j,k) = 0.25*( (u3d(i  ,j,k  )-uavg(k  )) + (u3d(i  ,j,k-1)-uavg(k-1))   &
                          +(u3d(i+1,j,k  )-uavg(k  )) + (u3d(i+1,j,k-1)-uavg(k-1)) )
      dum5(i,j,k) = 0.25*( (v3d(i,j  ,k  )-vavg(k  )) + (v3d(i,j  ,k-1)-vavg(k-1))   &
                          +(v3d(i,j+1,k  )-vavg(k  )) + (v3d(i,j+1,k-1)-vavg(k-1)) )
      dum6(i,j,k) = w3d(i,j,k)-wavg(k)
    enddo
    enddo
    enddo

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'upup_w'
        vardesc(nvar) = '< u-prime u-prime >'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum4(i,j,k)*dum4(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'vpvp_w'
        vardesc(nvar) = '< v-prime v-prime >'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum5(i,j,k)*dum5(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wpwp_w'
        vardesc(nvar) = '< w-prime w-prime >'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum6(i,j,k)*dum6(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=2,nk
          wvar(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'upvp_w'
        vardesc(nvar) = '< u-prime v-prime >'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum4(i,j,k)*dum5(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

!HERE
      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'upwp_w'
        vardesc(nvar) = '< u-prime w-prime >'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum4(i,j,k)*dum6(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'vpwp_w'
        vardesc(nvar) = '< v-prime w-prime >'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum5(i,j,k)*dum6(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wpwpwp'
        vardesc(nvar) = '< w-prime w-prime w-prime >'
        varunit(nvar) = 'm3/s3'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum6(i,j,k)**3
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wpupup'
        vardesc(nvar) = '< w-prime u-prime u-prime >'
        varunit(nvar) = 'm3/s3'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum6(i,j,k)*dum4(i,j,k)*dum4(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wpvpvp'
        vardesc(nvar) = '< w-prime v-prime v-prime >'
        varunit(nvar) = 'm3/s3'
        vargrid(nvar) = 'w'
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = dum6(i,j,k)*dum5(i,j,k)*dum5(i,j,k)
        enddo
        enddo
        enddo
        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c
#if 0
        call     momentum_flux(u3d,v3d,w3d,t13,t23,m13,m23,rf,rf0,c1,c2,dum3,dum4,dum5,ufr,ufs,ufd,vfr,vfs,vfd)

        nvar = nvar+1
        varname(nvar) = 'ufr'
        vardesc(nvar) = 'vertical flux of u, resolved'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        call write3d(ufr,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)


        nvar = nvar+1
        varname(nvar) = 'ufs'
        vardesc(nvar) = 'vertical flux of u, subgrid'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        call write3d(ufs,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'ufd'
        vardesc(nvar) = 'vertical flux of u, diffusion'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        call write3d(ufd,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      IF( dot2p )THEN
        nvar = nvar+1
        varname(nvar) = 'ufw'
        vardesc(nvar) = 'vertical flux of u, wall model'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        !$acc parallel default(present) private(k)
        savg(1) = 0.0
        !$acc loop gang vector
        do k=2,ntwk
          savg(k) = -kmw(k)*(u1b(k)-u1b(k-1))*rdz*mf(1,1,k)
        enddo
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      ENDIF

      !..................

        nvar = nvar+1
        varname(nvar) = 'vfr'
        vardesc(nvar) = 'vertical flux of v, resolved'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        call write3d(vfr,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'vfs'
        vardesc(nvar) = 'vertical flux of v, subgrid'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        call write3d(vfs,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'vfd'
        vardesc(nvar) = 'vertical flux of v, diffusion'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        call write3d(vfd,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      IF( dot2p )THEN
        nvar = nvar+1
        varname(nvar) = 'vfw'
        vardesc(nvar) = 'vertical flux of v, wall model'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        !$acc parallel default(present) private(k)
        savg(1) = 0.0
        !$acc loop gang vector
        do k=2,ntwk
          savg(k) = -kmw(k)*(v1b(k)-v1b(k-1))*rdz*mf(1,1,k)
        enddo
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      ENDIF


      !c-c-c-c-c-c-c-c-c-c
        ! theta fluxes:

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=0,nj+1
        do i=0,ni+1
!!!          dum2(i,j,k)=th0(i,j,k)+th3d(i,j,k)
          dum2(i,j,k)=(th0(i,j,k)-th0r)+th3d(i,j,k)
        enddo
        enddo
        enddo

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          temavg(k) = thavg(k)-th0r
        enddo


        pdef = 0
        pdefweno = 0
        weps = 10.0*epsilon

        call     scalar_flux(weps,c1,c2,mf,rf0,dum2,dum3,dum4,dum5,w3d,khv,thflux,wavg,temavg,ptfr,ptfs,ptfd,dosfcflx,1,pdef,pdefweno)

        
        nvar = nvar+1
        varname(nvar) = 'thfr'
        vardesc(nvar) = 'vertical flux of theta, resolved'
        varunit(nvar) = 'K m/s'
        vargrid(nvar) = 'w'
        call write3d(ptfr,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'thfs'
        vardesc(nvar) = 'vertical flux of theta, subgrid'
        varunit(nvar) = 'K m/s'
        vargrid(nvar) = 'w'
        call write3d(ptfs,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'thfd'
        vardesc(nvar) = 'vertical flux of theta, diffusion'
        varunit(nvar) = 'K m/s'
        vargrid(nvar) = 'w'
        call write3d(ptfd,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)


      IF( imoist.eq.0 )THEN

        call     getwstar(zf,thv0,rth0s,ptfr,ptfs,ptfd,thfavg,qvfavg,wstar,fbmin,zfbmin)

      ENDIF

!BLOCK06
      !c-c-c-c-c-c-c-c-c-c
        ! theta variance

      IF( cm1setup.eq.1 .and. iusetke )THEN

        nvar = nvar+1
        varname(nvar) = 'thvars'
        vardesc(nvar) = 'pot temp variance, subgrid'
        varunit(nvar) = 'K^2'
        vargrid(nvar) = 'w'

        !$acc parallel default(present) private(i,j,k)
        !$acc loop gang vector collapse(3) 
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = 0.0
        enddo
        enddo
        enddo

        !$acc loop gang vector collapse(3) 
        do k=2,nk
        do j=1,nj
        do i=1,ni
          if( tkea(i,j,k).ge.1.0e-6 )then
            dum1(i,j,k) = 2.0*lenscl(i,j,k)*khv(i,j,k)                        &
                             *( ((th(i,j,k)-th(i,j,k-1))*rdz*mf(i,j,k))**2 )  &
                             /(2.02*sqrt(max(1.0e-6,tkea(i,j,k))))
          endif
        enddo
        enddo
        enddo
        !$acc end parallel

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c
        ! thetav fluxes:

      IF( imoist.eq.1 )THEN

        ! surface flux:
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          dum6(i,j,1) = (1.0+repsm1*q3d(i,j,1,nqv))*thflux(i,j)  &
                       +repsm1*th(i,j,1)*qvflux(i,j)
        enddo
        enddo

        pdef = 0
        pdefweno = 0
        weps = 10.0*epsilon

        !PORTME
        call     scalar_flux(weps,c1,c2,mf,rf0,thv,dum3,dum4,dum5,w3d,khv,dum6(ib,jb,1),wavg,thvavg,sfr,sfs,sfd,dosfcflx,1,pdef,pdefweno)

        nvar = nvar+1
        varname(nvar) = 'thvfr'
        vardesc(nvar) = 'vertical flux of theta-v, resolved'
        varunit(nvar) = 'K m/s'
        vargrid(nvar) = 'w'
        call write3d(sfr,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'thvfs'
        vardesc(nvar) = 'vertical flux of theta-v, subgrid'
        varunit(nvar) = 'K m/s'
        vargrid(nvar) = 'w'
        call write3d(sfs,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'thvfd'
        vardesc(nvar) = 'vertical flux of theta-v, diffusion'
        varunit(nvar) = 'K m/s'
        vargrid(nvar) = 'w'
        call write3d(sfd,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        call     getwstar(zf,thv0,rth0s,sfr,sfs,sfd,thfavg,qvfavg,wstar,fbmin,zfbmin)

      ENDIF


      !c-c-c-c-c-c-c-c-c-c
        ! thetal fluxes:

      IF( imoist.eq.1 )THEN

        ! surface flux:
        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          dum6(i,j,1) = thflux(i,j)
        enddo
        enddo

        pdef = 0
        pdefweno = 0
        weps = 10.0*epsilon

        !PORTME
        call     scalar_flux(weps,c1,c2,mf,rf0,thl,dum3,dum4,dum5,w3d,khv,dum6(ib,jb,1),wavg,thlavg,sfr,sfs,sfd,dosfcflx,1,pdef,pdefweno)

        nvar = nvar+1
        varname(nvar) = 'thlfr'
        vardesc(nvar) = 'vertical flux of theta-l, resolved'
        varunit(nvar) = 'K m/s'
        vargrid(nvar) = 'w'
        call write3d(sfr,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'thlfs'
        vardesc(nvar) = 'vertical flux of theta-l, subgrid'
        varunit(nvar) = 'K m/s'
        vargrid(nvar) = 'w'
        call write3d(sfs,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'thlfd'
        vardesc(nvar) = 'vertical flux of theta-l, diffusion'
        varunit(nvar) = 'K m/s'
        vargrid(nvar) = 'w'
        call write3d(sfd,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF


      !c-c-c-c-c-c-c-c-c-c
        ! qv fluxes:

      IF( imoist.eq.1 )THEN

        pdef = 1
        pdefweno = 0
        weps = 0.01*epsilon

        !PORTME
        call     scalar_flux(weps,c1,c2,mf,rf0,q3d(ib,jb,kb,nqv),dum3,dum4,dum5,w3d,khv,qvflux,wavg,qvavg,qvfr,qvfs,qvfd,dosfcflx,1,pdef,pdefweno)

        nvar = nvar+1
        varname(nvar) = 'qvfr'
        vardesc(nvar) = 'vertical flux of qv, resolved'
        varunit(nvar) = 'g/g m/s'
        vargrid(nvar) = 'w'
        call write3d(qvfr,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qvfs'
        vardesc(nvar) = 'vertical flux of qv, subgrid'
        varunit(nvar) = 'g/g m/s'
        vargrid(nvar) = 'w'
        call write3d(qvfs,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qvfd'
        vardesc(nvar) = 'vertical flux of qv, diffusion'
        varunit(nvar) = 'g/g m/s'
        vargrid(nvar) = 'w'
        call write3d(qvfd,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c
        ! ql fluxes:

      IF( imoist.eq.1 )THEN

        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          dum6(i,j,1) = 0.0
        enddo
        enddo

        pdef = 1
        pdefweno = 1
        weps = 0.01*epsilon

        !PORTME
        call     scalar_flux(weps,c1,c2,mf,rf0,ql,dum3,dum4,dum5,w3d,khv,dum6(ib,jb,1),wavg,qlavg,sfr,sfs,sfd,dosfcflx,0,pdef,pdefweno)

        nvar = nvar+1
        varname(nvar) = 'qlfr'
        vardesc(nvar) = 'vertical flux of ql, resolved'
        varunit(nvar) = 'g/g m/s'
        vargrid(nvar) = 'w'
        call write3d(sfr,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qlfs'
        vardesc(nvar) = 'vertical flux of ql, subgrid'
        varunit(nvar) = 'g/g m/s'
        vargrid(nvar) = 'w'
        call write3d(sfs,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qlfd'
        vardesc(nvar) = 'vertical flux of ql, diffusion'
        varunit(nvar) = 'g/g m/s'
        vargrid(nvar) = 'w'
        call write3d(sfd,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c
        ! qt fluxes:

      IF( imoist.eq.1 )THEN

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum2(i,j,k) = q3d(i,j,k,nqv)+ql(i,j,k)
        enddo
        enddo
        enddo

        pdef = 1
        pdefweno = 1
        weps = 0.01*epsilon

        !PORTME
        call     scalar_flux(weps,c1,c2,mf,rf0,dum2,dum3,dum4,dum5,w3d,khv,qvflux,wavg,qtavg,sfr,sfs,sfd,dosfcflx,1,pdef,pdefweno)

        nvar = nvar+1
        varname(nvar) = 'qtfr'
        vardesc(nvar) = 'vertical flux of qt, resolved'
        varunit(nvar) = 'g/g m/s'
        vargrid(nvar) = 'w'
        call write3d(sfr,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qtfs'
        vardesc(nvar) = 'vertical flux of qt, subgrid'
        varunit(nvar) = 'g/g m/s'
        vargrid(nvar) = 'w'
        call write3d(sfs,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar = nvar+1
        varname(nvar) = 'qtfd'
        vardesc(nvar) = 'vertical flux of qt, diffusion'
        varunit(nvar) = 'g/g m/s'
        vargrid(nvar) = 'w'
        call write3d(sfd,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c
        ! pt fluxes:

      IF( iptra.eq.1 )THEN
      DO n=1,npt

        if( pdtra.eq.1 )then
          pdef = 1
        else
          pdef = 0
        endif
        pdefweno = 1
        weps = 1.0*epsilon

        !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
        do j=1,nj
        do i=1,ni
          dum6(i,j,1) = 0.0
        enddo
        enddo

        !PORTME
        call     scalar_flux(weps,c1,c2,mf,rf0,pt3d(ib,jb,kb,n),dum3,dum4,dum5,w3d,khv,dum6(ib,jb,1),wavg,ptavg(0,n),sfr,sfs,sfd,dosfcflx,1,pdef,pdefweno)

        a1 = 'ptfr                                                                            '
        if(n.le.9)then
          write(a1(5:5),155) n
        elseif(n.le.99)then
          write(a1(5:6),154) n
        else
          write(a1(5:7),153) n
        endif

        nvar = nvar+1
        varname(nvar) = a1
        vardesc(nvar) = 'vertical flux of pt, resolved'
        varunit(nvar) = 'g/g m/s'
        vargrid(nvar) = 'w'
        call write3d(sfr,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        a1 = 'ptfs                                                                            '
        if(n.le.9)then
          write(a1(5:5),155) n
        elseif(n.le.99)then
          write(a1(5:6),154) n
        else
          write(a1(5:7),153) n
        endif

        nvar = nvar+1
        varname(nvar) = a1
        vardesc(nvar) = 'vertical flux of pt, subgrid'
        varunit(nvar) = 'g/g m/s'
        vargrid(nvar) = 'w'
        call write3d(sfs,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        a1 = 'ptfd                                                                            '
        if(n.le.9)then
          write(a1(5:5),155) n
        elseif(n.le.99)then
          write(a1(5:6),154) n
        else
          write(a1(5:7),153) n
        endif

        nvar = nvar+1
        varname(nvar) = a1
        vardesc(nvar) = 'vertical flux of pt, diffusion'
        varunit(nvar) = 'g/g m/s'
        vargrid(nvar) = 'w'
        call write3d(sfd,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDDO
      ENDIF
      !c-c-c-c-c-c-c-c-c-c

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    ! subgrid TKE budget:

    sgsm:  &
    IF( cm1setup.eq.1 .and. iusetke )THEN
      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'stkeb_shear'
        vardesc(nvar) = 'subgrid tke budget: shear term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            dum1(i,j,1) = ust(i,j)*ust(i,j)*ust(i,j)/(karman*znt(i,j))
          enddo
          enddo


          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=2,nk
          do j=1,nj
          do i=1,ni
            dum1(i,j,k) = kmv(i,j,k)*max(0.0,(defv(i,j,k)+defh(i,j,k)))
          enddo
          enddo
          enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'stkeb_buoy'
        vardesc(nvar) = 'subgrid tke budget: buoyancy term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            dum1(i,j,1) = g*( thflux(i,j)*rth0s(i,j) + repsm1*qvflux(i,j) )
          enddo
          enddo

          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=2,nk
          do j=1,nj
          do i=1,ni
            dum1(i,j,k) = -khv(i,j,k)*nm(i,j,k)
          enddo
          enddo
          enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'stkeb_diss'
        vardesc(nvar) = 'subgrid tke budget: dissipation term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
!!!            dum1(i,j,1) = -ust(i,j)*ust(i,j)*ust(i,j)/(karman*znt(i,j))  &
!!!                          -g*( thflux(i,j)*rth0s(i,j) + repsm1*qvflux(i,j) )
            dum1(i,j,1) = 0.0
            dum1(i,j,nk+1) = 0.0
          enddo
          enddo

          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=2,nk
          do j=1,nj
          do i=1,ni
            dum1(i,j,k) = -dissten(i,j,k)
          enddo
          enddo
          enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      if( kd_adv.ge.1 )then
        nvar = nvar+1
        varname(nvar) = 'stkeb_adv'
        vardesc(nvar) = 'subgrid tke budget: advection term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            dum1(i,j,k) = kdiag(i,j,k,kd_adv)
          enddo
          enddo
          enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      !c-c-c-c-c-c-c-c-c-c

      if( kd_turb.ge.1 )then
        nvar = nvar+1
        varname(nvar) = 'stkeb_diff'
        vardesc(nvar) = 'subgrid tke budget: diffusion term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            dum1(i,j,k) = kdiag(i,j,k,kd_turb)
          enddo
          enddo
          enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'stkef'
        vardesc(nvar) = 'param. vert flux of subgrid tke'
        varunit(nvar) = 'm3/s3'

        !dum1 = 0.0

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = -0.5*(kmv(i,j,k)+kmv(i,j,k+1))*(tke3d(i,j,k+1)-tke3d(i,j,k))*rdz*mh(i,j,k)*rho(i,j,k)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

    ENDIF  sgsm

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  theta tendencies:

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'ptb_vturbr'
        vardesc(nvar) = 'theta budget: vertical turbulence (resolved)'
        varunit(nvar) = 'K/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = -(rfavg(k+1)*ptfr(k+1)-rfavg(k)*ptfr(k))*rdz*mh(1,1,k)/rhoavg(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'ptb_vturbs'
        vardesc(nvar) = 'theta budget: vertical turbulence (subgrid)'
        varunit(nvar) = 'K/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = -(rfavg(k+1)*ptfs(k+1)-rfavg(k)*ptfs(k))*rdz*mh(1,1,k)/rhoavg(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'ptb_vturbd'
        vardesc(nvar) = 'theta budget: vertical implicit diffusion'
        varunit(nvar) = 'K/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = -(rfavg(k+1)*ptfd(k+1)-rfavg(k)*ptfd(k))*rdz*mh(1,1,k)/rhoavg(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      if( td_hadv.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_hadv'
      if( hadvordrs.eq.3 .or. hadvordrs.eq.5 .or. hadvordrs.eq.7 .or. hadvordrs.eq.9 .or. advwenos.ge.1 )then
        vardesc(nvar) = 'theta budget: horizontal advection (non-diff component)'
      else
        vardesc(nvar) = 'theta budget: horizontal advection'
      endif
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_hadv),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_vadv.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_vadv'
      if( vadvordrs.eq.3 .or. vadvordrs.eq.5 .or. vadvordrs.eq.7 .or. vadvordrs.eq.9 .or. advwenos.ge.1 )then
        vardesc(nvar) = 'theta budget: vertical advection (non-diff component)'
      else
        vardesc(nvar) = 'theta budget: vertical advection'
      endif
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_vadv),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_hidiff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_hidiff'
        vardesc(nvar) = 'theta budget: horiz implicit diffusion'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_hidiff),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_vidiff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_vidiff'
        vardesc(nvar) = 'theta budget: vert implicit diffusion'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_vidiff),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_hediff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_hediff'
        vardesc(nvar) = 'theta budget: horiz explicit diffusion'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_hediff),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_vediff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_vediff'
        vardesc(nvar) = 'theta budget: vertical explicit diffusion'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_vediff),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_hturb.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_hturb'
        vardesc(nvar) = 'theta budget: horizontal parameterized turbulence'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_hturb),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_vturb.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_vturb'
        vardesc(nvar) = 'theta budget: vert parameterized turbulence'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_vturb),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_mp.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_mp'
        vardesc(nvar) = 'theta budget: microphysics'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_mp),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_rdamp.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_rdamp'
        vardesc(nvar) = 'theta budget: Rayleigh damper'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_rdamp),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_nudge.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_nudge'
        vardesc(nvar) = 'theta budget: nudging'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_nudge),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_rad.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_rad'
        vardesc(nvar) = 'theta budget: radiation scheme'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_rad),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_div.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_div'
        vardesc(nvar) = 'theta budget: moist divergence term'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_div),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_diss.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_diss'
      if( ipbl.eq.3 .or. ipbl.eq.4 .or. ipbl.eq.5 )then
        vardesc(nvar) = 'theta budget: diss. heating (from PBL)'
      else
        vardesc(nvar) = 'theta budget: dissipative heating'
      endif
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_diss),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_pbl.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_pbl'
      if( ipbl.eq.3 .or. ipbl.eq.4 .or. ipbl.eq.5 )then
        vardesc(nvar) = 'theta budget: PBL scheme (excluding diss. heating)'
      else
        vardesc(nvar) = 'theta budget: PBL scheme'
      endif
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_pbl),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_lsw.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ptb_lsw'
        vardesc(nvar) = 'theta budget: large-scale vertical advection'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_lsw),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_cond.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'tt_cond'
        vardesc(nvar) = 'theta tendency: condensation'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_cond),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_evac.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'tt_evac'
        vardesc(nvar) = 'theta tendency: cloudwater evaporation'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_evac),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_evar.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'tt_evar'
        vardesc(nvar) = 'theta budget: rainwater evaporation'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_evar),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_dep.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'tt_dep'
        vardesc(nvar) = 'theta tendency: deposition'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_dep),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_subl.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'tt_subl'
        vardesc(nvar) = 'theta tendency: sublimation'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_subl),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_melt.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'tt_melt'
        vardesc(nvar) = 'theta tendency: melting'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_melt),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_frz.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'tt_frz'
        vardesc(nvar) = 'theta tendency: freezing'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_frz),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( td_efall.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'td_efall'
        vardesc(nvar) = 'temp. tendency: energy fallout terms'
        varunit(nvar) = 'K/s'

        call getavg3d(tdiag(ibdt,jbdt,kbdt,td_efall),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      IF( testcase.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'ptb_frc'
        vardesc(nvar) = 'theta budget: idealized forcing'
        varunit(nvar) = 'K/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = thfrc(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  qv tendencies:

    imoist1:  &
    IF( imoist.eq.1 )THEN

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'qvb_vturbr'
        vardesc(nvar) = 'qv budget: vertical turbulence (resolved)'
        varunit(nvar) = 'g/g/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = -(rfavg(k+1)*qvfr(k+1)-rfavg(k)*qvfr(k))*rdz*mh(1,1,k)/rhoavg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'qvb_vturbs'
        vardesc(nvar) = 'qv budget: vertical turbulence (subgrid)'
        varunit(nvar) = 'g/g/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = -(rfavg(k+1)*qvfs(k+1)-rfavg(k)*qvfs(k))*rdz*mh(1,1,k)/rhoavg(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'qvb_vturbd'
        vardesc(nvar) = 'qv budget: vertical implicit diffusion'
        varunit(nvar) = 'g/g/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = -(rfavg(k+1)*qvfd(k+1)-rfavg(k)*qvfd(k))*rdz*mh(1,1,k)/rhoavg(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

      if( qd_hadv.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_hadv'
      if( hadvordrs.eq.3 .or. hadvordrs.eq.5 .or. hadvordrs.eq.7 .or. hadvordrs.eq.9 .or. advwenos.ge.1 )then
        vardesc(nvar) = 'qv budget: horizontal advection (non-diff component)'
      else
        vardesc(nvar) = 'qv budget: horizontal advection'
      endif
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_hadv),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_vadv.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_vadv'
      if( vadvordrs.eq.3 .or. vadvordrs.eq.5 .or. vadvordrs.eq.7 .or. vadvordrs.eq.9 .or. advwenos.ge.1 )then
        vardesc(nvar) = 'qv budget: vertical advection (non-diff component)'
      else
        vardesc(nvar) = 'qv budget: vertical advection'
      endif
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_vadv),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_hturb.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_hturb'
        vardesc(nvar) = 'qv budget: horizontal parameterized turbulence'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_hturb),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_vturb.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_vturb'
        vardesc(nvar) = 'qv budget: vertical parameterized turbulence'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_vturb),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_mp.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_mp'
        vardesc(nvar) = 'qv budget: microphysics tendency'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_mp),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_nudge.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_nudge'
        vardesc(nvar) = 'qv budget: nudging'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_nudge),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_pbl.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_pbl'
        vardesc(nvar) = 'qv budget: PBL scheme'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_pbl),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_lsw.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_lsw'
        vardesc(nvar) = 'qv budget: large-scale vertical advection'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_lsw),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_cond.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_cond'
        vardesc(nvar) = 'qv tendency: condensation'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_cond),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_evac.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_evac'
        vardesc(nvar) = 'qv tendency: cloudwater evaporation'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_evac),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_evar.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_evar'
        vardesc(nvar) = 'qv tendency: rainwater evaporation'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_evar),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_dep.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_dep'
        vardesc(nvar) = 'qv tendency: deposition'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_dep),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_subl.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_subl'
        vardesc(nvar) = 'qv tendency: sublimation'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_subl),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_hidiff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_hidiff'
        vardesc(nvar) = 'qv budget: horizontal implicit diffusion'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_hidiff),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_vidiff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_vidiff'
        vardesc(nvar) = 'qv budget: vertical implicit diffusion'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdq,jbdq,kbdq,qd_vidiff),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_hediff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_hediff'
        vardesc(nvar) = 'qv budget: horizontal explicit diffusion'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdt,jbdt,kbdt,qd_hediff),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( qd_vediff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'qvb_vediff'
        vardesc(nvar) = 'qv budget: vertical explicit diffusion'
        varunit(nvar) = 'g/g/s'

        call getavg3d(qdiag(ibdt,jbdt,kbdt,qd_vediff),savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      IF( testcase.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'qvb_frc'
        vardesc(nvar) = 'qv budget: idealized forcing'
        varunit(nvar) = 'g/g/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = qvfrc(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

    ENDIF  imoist1
    !stop 'domaindiag: after qvb_frc'

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  u tendencies:

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'ub_vturbr'
        vardesc(nvar) = 'u budget: vertical turbulence (resolved)'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = -(rfavg(k+1)*ufr(k+1)-rfavg(k)*ufr(k))*rdz*mh(1,1,k)/rhoavg(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

       stop 'domaindiag: ub_vturbr'
      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'ub_vturbs'
        vardesc(nvar) = 'u budget: vertical turbulence (subgrid)'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = -(rfavg(k+1)*ufs(k+1)-rfavg(k)*ufs(k))*rdz*mh(1,1,k)/rhoavg(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'ub_vturbd'
        vardesc(nvar) = 'u budget: vertical implicit diffusion'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = -(rfavg(k+1)*ufd(k+1)-rfavg(k)*ufd(k))*rdz*mh(1,1,k)/rhoavg(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c
#endif

      if( ud_hadv.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_hadv'
      if( hadvordrv.eq.3 .or. hadvordrv.eq.5 .or. hadvordrv.eq.7 .or. hadvordrv.eq.9 .or. advwenov.ge.1 )then
        vardesc(nvar) = 'u budget: horizontal advection (non-diff component)'
      else
        vardesc(nvar) = 'u budget: horizontal advection'
      endif
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_hadv)
        enddo
        enddo
        enddo

        
        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          u_hadv(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif
      !stop 'domaindiag: ub_hadv'

      !c-c-c-c-c-c-c-c-c-c

      if( ud_vadv.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_vadv'
      if( vadvordrv.eq.3 .or. vadvordrv.eq.5 .or. vadvordrv.eq.7 .or. vadvordrv.eq.9 .or. advwenov.ge.1 )then
        vardesc(nvar) = 'u budget: vertical advection (non-diff component)'
      else
        vardesc(nvar) = 'u budget: vertical advection'
      endif
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_vadv)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          u_vadv(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif
      !stop 'domaindiag: ub_vadv'

      !c-c-c-c-c-c-c-c-c-c

      if( ud_hidiff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_hidiff'
        vardesc(nvar) = 'u budget: horizontal implicit diffusion'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_hidiff)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          u_hidiff(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_vidiff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_vidiff'
        vardesc(nvar) = 'u budget: vertical implicit diffusion'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_vidiff)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          u_vidiff(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_hediff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_hediff'
        vardesc(nvar) = 'u budget: horizontal explicit diffusion'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_hediff)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_vediff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_vediff'
        vardesc(nvar) = 'u budget: vertical explicit diffusion'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_vediff)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_hturb.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_hturb'
        vardesc(nvar) = 'u budget: horizontal parameterized turbulence'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_hturb)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          u_hturb(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_vturb.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_vturb'
        vardesc(nvar) = 'u budget: vertical parameterized turbulence'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_vturb)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          u_vturb(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_pgrad.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_pgrad'
        vardesc(nvar) = 'u budget: pressure gradient'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_pgrad)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          u_pgrad(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_rdamp.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_rdamp'
        vardesc(nvar) = 'u budget: Rayleigh damping'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_rdamp)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          u_rdamp(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_cor.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_cor'
        vardesc(nvar) = 'u budget: Coriolis term'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_cor)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          u_cor(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_cent.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_cent'
        vardesc(nvar) = 'u budget: centrifugal acceleration'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_cent)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_pbl.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_pbl'
        vardesc(nvar) = 'u budget: PBL scheme'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_pbl)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_lsw.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'ub_lsw'
        vardesc(nvar) = 'u budget: large-scale vertical advection'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = udiag(i,j,k,ud_lsw)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          u_lsw(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      IF( lspgrad.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'ub_lspg'
        vardesc(nvar) = 'u budget: large-scale pressure gradient'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = ulspg(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

      IF( testcase.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'ub_frc'
        vardesc(nvar) = 'u budget: idealized forcing'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = ufrc(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

#if 0
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  v tendencies:
      !c-c-c-c-c-c-c-c-c-c

    
        nvar = nvar+1
        varname(nvar) = 'vb_vturbr'
        vardesc(nvar) = 'v budget: vertical turbulence (resolved)'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = -(rfavg(k+1)*vfr(k+1)-rfavg(k)*vfr(k))*rdz*mh(1,1,k)/rhoavg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'vb_vturbs'
        vardesc(nvar) = 'v budget: vertical turbulence (subgrid)'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = -(rfavg(k+1)*vfs(k+1)-rfavg(k)*vfs(k))*rdz*mh(1,1,k)/rhoavg(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'vb_vturbd'
        vardesc(nvar) = 'v budget: vertical implicit diffusion'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = -(rfavg(k+1)*vfd(k+1)-rfavg(k)*vfd(k))*rdz*mh(1,1,k)/rhoavg(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

#endif

      !c-c-c-c-c-c-c-c-c-c

      if( vd_hadv.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_hadv'
      if( hadvordrv.eq.3 .or. hadvordrv.eq.5 .or. hadvordrv.eq.7 .or. hadvordrv.eq.9 .or. advwenov.ge.1 )then
        vardesc(nvar) = 'v budget: horizontal advection (non-diff component)'
      else
        vardesc(nvar) = 'v budget: horizontal advection'
      endif
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_hadv)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          v_hadv(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif


      !c-c-c-c-c-c-c-c-c-c

      if( vd_vadv.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_vadv'
      if( vadvordrv.eq.3 .or. vadvordrv.eq.5 .or. vadvordrv.eq.7 .or. vadvordrv.eq.9 .or. advwenov.ge.1 )then
        vardesc(nvar) = 'v budget: vertical advection (non-diff component)'
      else
        vardesc(nvar) = 'v budget: vertical advection'
      endif
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_vadv)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel default(present) private(k)
        v_vadv(0) = 0.0
        v_vadv(nkp1) = 0.0
        !$acc loop gang vector
        do k=1,nk
          v_vadv(k) = savg(k)
        enddo
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif


      !c-c-c-c-c-c-c-c-c-c

      if( vd_hidiff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_hidiff'
        vardesc(nvar) = 'v budget: horizontal implicit diffusion'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_hidiff)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel default(present) private(k)
        v_hidiff(0) = 0.0
        v_hidiff(nkp1) = 0.0
        !$acc loop gang vector
        do k=1,nk
          v_hidiff(k) = savg(k)
        enddo
        !$acc end parallel
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

!HERE
      !c-c-c-c-c-c-c-c-c-c

      if( vd_vidiff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_vidiff'
        vardesc(nvar) = 'v budget: vertical implicit diffusion'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_vidiff)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          v_vidiff(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( vd_hediff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_hediff'
        vardesc(nvar) = 'v budget: horizontal explicit diffusion'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_hediff)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( vd_vediff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_vediff'
        vardesc(nvar) = 'v budget: vertical explicit diffusion'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_vediff)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( vd_hturb.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_hturb'
        vardesc(nvar) = 'v budget: horizontal parameterized turbulence'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_hturb)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          v_hturb(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( vd_vturb.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_vturb'
        vardesc(nvar) = 'v budget: vertical parameterized turbulence'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_vturb)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          v_vturb(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( vd_pgrad.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_pgrad'
        vardesc(nvar) = 'v budget: pressure gradient'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_pgrad)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          v_pgrad(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( vd_rdamp.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_rdamp'
        vardesc(nvar) = 'v budget: Rayleigh damping'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_rdamp)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          v_rdamp(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( vd_cor.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_cor'
        vardesc(nvar) = 'v budget: Coriolis term'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_cor)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          v_cor(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( vd_cent.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_cent'
        vardesc(nvar) = 'v budget: centrifugal acceleration'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_cent)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c


      if( vd_pbl.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_pbl'
        vardesc(nvar) = 'v budget: PBL scheme'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_pbl)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( vd_lsw.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vb_lsw'
        vardesc(nvar) = 'v budget: large-scale vertical advection'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vdiag(i,j,k,vd_lsw)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          v_lsw(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif


      IF( lspgrad.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'vb_lspg'
        vardesc(nvar) = 'v budget: large-scale pressure gradient'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = vlspg(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF
      !c-c-c-c-c-c-c-c-c-c

      !c-c-c-c-c-c-c-c-c-c

      IF( testcase.ge.1 )THEN

        nvar = nvar+1
        varname(nvar) = 'vb_frc'
        vardesc(nvar) = 'v budget: idealized forcing'
        varunit(nvar) = 'm/s/s'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = vfrc(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  w tendencies:

      !c-c-c-c-c-c-c-c-c-c

      if( wd_hadv.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'wb_hadv'
      if( hadvordrv.eq.3 .or. hadvordrv.eq.5 .or. hadvordrv.eq.7 .or. hadvordrv.eq.9 .or. advwenov.ge.1 )then
        vardesc(nvar) = 'w budget: horizontal advection (non-diff component)'
      else
        vardesc(nvar) = 'w budget: horizontal advection'
      endif
        varunit(nvar) = 'm/s/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = wdiag(i,j,k,wd_hadv)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          w_hadv(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( wd_vadv.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'wb_vadv'
      if( vadvordrv.eq.3 .or. vadvordrv.eq.5 .or. vadvordrv.eq.7 .or. vadvordrv.eq.9 .or. advwenov.ge.1 )then
        vardesc(nvar) = 'w budget: vertical advection (non-diff component)'
      else
        vardesc(nvar) = 'w budget: vertical advection'
      endif
        varunit(nvar) = 'm/s/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = wdiag(i,j,k,wd_vadv)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          w_vadv(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif


      !c-c-c-c-c-c-c-c-c-c

      if( wd_hturb.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'wb_hturb'
        vardesc(nvar) = 'w budget: horizontal parameterized turbulence'
        varunit(nvar) = 'm/s/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = wdiag(i,j,k,wd_hturb)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          w_hturb(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( wd_vturb.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'wb_vturb'
        vardesc(nvar) = 'w budget: vertical parameterized turbulence'
        varunit(nvar) = 'm/s/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = wdiag(i,j,k,wd_vturb)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          w_vturb(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c
      w_pgrad=0.0
      if( wd_pgrad.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'wb_pgrad'
        vardesc(nvar) = 'w budget: pressure gradient'
        varunit(nvar) = 'm/s/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = wdiag(i,j,k,wd_pgrad)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=2,nk
          w_pgrad(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( wd_rdamp.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'wb_rdamp'
        vardesc(nvar) = 'w budget: Rayleigh damping'
        varunit(nvar) = 'm/s/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = wdiag(i,j,k,wd_rdamp)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          w_rdamp(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !$acc parallel loop gang vector default(present) private(k)
      do k=0,nk+1
        w_buoy(k) = 0.0
      enddo
      !c-c-c-c-c-c-c-c-c-c

      if( wd_buoy.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'wb_buoy'
        vardesc(nvar) = 'w budget: buoyancy'
        varunit(nvar) = 'm/s/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = wdiag(i,j,k,wd_buoy)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=2,nk
          w_buoy(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
      endif

      !c-c-c-c-c-c-c-c-c-c

      if( wd_hidiff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'wb_hidiff'
        vardesc(nvar) = 'w budget: horizontal implicit diffusion'
        varunit(nvar) = 'm/s/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = wdiag(i,j,k,wd_hidiff)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          w_hidiff(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( wd_vidiff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'wb_vidiff'
        vardesc(nvar) = 'w budget: vertical implicit diffusion'
        varunit(nvar) = 'm/s/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = wdiag(i,j,k,wd_vidiff)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          w_vidiff(k) = savg(k)
        enddo
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( wd_hediff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'wb_hediff'
        vardesc(nvar) = 'w budget: horizontal explicit diffusion'
        varunit(nvar) = 'm/s/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = wdiag(i,j,k,wd_hediff)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( wd_vediff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'wb_vediff'
        vardesc(nvar) = 'w budget: vertical explicit diffusion'
        varunit(nvar) = 'm/s/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = wdiag(i,j,k,wd_vediff)
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

!HERE
      !c-c-c-c-c-c-c-c-c-c

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  resolved tke tendencies:

      !c-c-c-c-c-c-c-c-c-c
      ! get u,v,w perts for rtke calcs:

        !!$acc data copyin(udiag,vdiag,wdiag,utk,vtk,wtk) create(savg) &
        !!$acc copy(dum1,dum2,dum3) copyout(uavg,vavg,wavg,tavg,ravg)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,ni+1
          dum1(i,j,k) = udiag(i,j,k,nutk)
          dum2(i,j,k) = vdiag(i,j,k,nvtk)
          dum3(i,j,k) = wdiag(i,j,k,nwtk)
        enddo
        enddo
        enddo

        !MARK
        !$acc parallel loop gang vector default(present) private(k)
        do k=kb,ke
          uavg(k) = 0.0
          vavg(k) = 0.0
          wavg(k) = 0.0
        enddo

        call getavg3d(dum1,savg)
        ! save uavg:
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          uavg(k) = savg(k)
        enddo

        call getavg3d(dum2,savg)
        ! save vavg:
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          vavg(k) = savg(k)
        enddo

        call getavg3d(dum3,savg)
        ! save wavg:
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          wavg(k) = savg(k)
        enddo


        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,ni+1
          utk(i,j,k) = dum1(i,j,k)-uavg(k)
          vtk(i,j,k) = dum2(i,j,k)-vavg(k)
          wtk(i,j,k) = dum3(i,j,k)-wavg(k)
        enddo
        enddo
        enddo

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'rtke'
        vardesc(nvar) = 'resolved tke'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 's'

     
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          ravg(k) = 0.0
          tavg(k) = 0.0
        enddo

        !-----

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = utk(i,j,k)**2
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + 0.5*savg(k)
        enddo
        !$acc parallel loop gang vector default(present) private(k)
        do k=2,nk
          ravg(k) = ravg(k) + 0.5*( 0.5*(savg(k-1)+savg(k)) )
        enddo

        !-----

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = vtk(i,j,k)**2
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + 0.5*savg(k)
        enddo
        !$acc parallel loop gang vector default(present) private(k)
        do k=2,nk
          ravg(k) = ravg(k) + 0.5*( 0.5*(savg(k-1)+savg(k)) )
        enddo

        !-----

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = wtk(i,j,k)**2
        enddo
        enddo
        enddo

        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + 0.5*( 0.5*(savg(k)+savg(k+1)) )
          ravg(k) = ravg(k) + savg(k)
        enddo

        !-----

        call write3d(tavg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !......
        nvar = nvar+1
        varname(nvar) = 'rtke_w'
        vardesc(nvar) = 'resolved tke'
        varunit(nvar) = 'm2/s2'
        vargrid(nvar) = 'w'
        !$acc parallel
        ravg(1) = grads_undef
        ravg(nk+1) = grads_undef
        !$acc end parallel
        !ISSUE
        call write3d(ravg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......

      !c-c-c-c-c-c-c-c-c-c

      if( wd_buoy.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'rtkeb_buoy'
        vardesc(nvar) = 'resolved tke budget: buoyancy term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'

        call rtkeb_addw(dum1,wtk,wdiag,wd_buoy,w_buoy)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = 0.5*(savg(k)+savg(k+1))
          wwb(k) = savg(k)
        enddo

        call write3d(tavg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !......
        nvar = nvar+1
        varname(nvar) = 'wwb_buoy'
        vardesc(nvar) = '<ww> budget: buoyancy'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_pgrad.ge.1 .and. vd_pgrad.ge.1 .and. wd_pgrad.ge.1)then

        nvar = nvar+1
        varname(nvar) = 'rtkeb_pgrad'
        vardesc(nvar) = 'resolved tke budget: pressure term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          tavg(k) = 0.0
          uub(k)  = 0.0
          vvb(k)  = 0.0
          wwb(k)  = 0.0
        enddo

        call rtkeb_addu(dum1,utk,udiag,ud_pgrad,u_pgrad)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          uub(k) = uub(k) + savg(k)
        enddo
    
        call rtkeb_addv(dum1,vtk,vdiag,vd_pgrad,v_pgrad)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          vvb(k) = vvb(k) + savg(k)
        enddo

        call rtkeb_addw(dum1,wtk,wdiag,wd_pgrad,w_pgrad)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + 0.5*(savg(k)+savg(k+1))
          wwb(k) = wwb(k) + savg(k)
        enddo

        call write3d(tavg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !......
        nvar = nvar+1
        varname(nvar) = 'uub_pgrad'
        vardesc(nvar) = '<uu> budget: pressure-gradient term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'
        call write3d(uub,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......
        nvar = nvar+1
        varname(nvar) = 'vvb_pgrad'
        vardesc(nvar) = '<vv> budget: pressure-gradient term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'
        call write3d(vvb,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......
        nvar = nvar+1
        varname(nvar) = 'wwb_pgrad'
        vardesc(nvar) = '<ww> budget: pressure-gradient term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'
        call write3d(wwb,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......

      endif

!BLOCK06b
      !c-c-c-c-c-c-c-c-c-c

      if( ud_hadv.ge.1 .and. vd_hadv.ge.1 .and. wd_hadv.ge.1 .and.  &
          ud_vadv.ge.1 .and. vd_vadv.ge.1 .and. wd_vadv.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'rtkeb_adv'
        vardesc(nvar) = 'resolved tke budget: advection term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          tavg(k) = 0.0
          uub(k)  = 0.0
          vvb(k)  = 0.0
          wwb(k)  = 0.0
        enddo

        call rtkeb_addu(dum1,utk,udiag,ud_hadv,u_hadv)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          uub(k) = uub(k) + savg(k)
        enddo

        call rtkeb_addv(dum1,vtk,vdiag,vd_hadv,v_hadv)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          vvb(k) = vvb(k) + savg(k)
        enddo

        call rtkeb_addw(dum1,wtk,wdiag,wd_hadv,w_hadv)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + 0.5*(savg(k)+savg(k+1))
          wwb(k) = wwb(k) + savg(k)
        enddo


        call rtkeb_addu(dum1,utk,udiag,ud_vadv,u_vadv)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          uub(k) = uub(k) + savg(k)
        enddo

        call rtkeb_addv(dum1,vtk,vdiag,vd_vadv,v_vadv)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          vvb(k) = vvb(k) + savg(k)
        enddo

        call rtkeb_addw(dum1,wtk,wdiag,wd_vadv,w_vadv)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + 0.5*(savg(k)+savg(k+1))
          wwb(k) = wwb(k) + savg(k)
        enddo


        call write3d(tavg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !......
        nvar = nvar+1
        varname(nvar) = 'uub_adv'
        vardesc(nvar) = '<uu> budget: advection term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'
        call write3d(uub,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......
        nvar = nvar+1
        varname(nvar) = 'vvb_adv'
        vardesc(nvar) = '<vv> budget: advection term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'
        call write3d(vvb,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......
        nvar = nvar+1
        varname(nvar) = 'wwb_adv'
        vardesc(nvar) = '<ww> budget: advection term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'
        call write3d(wwb,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......

      endif


      !c-c-c-c-c-c-c-c-c-c

      if( ud_hidiff.ge.1 .and. vd_hidiff.ge.1 .and. wd_hidiff.ge.1 .and.  &
          ud_vidiff.ge.1 .and. vd_vidiff.ge.1 .and. wd_vidiff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'rtkeb_idiff'
        vardesc(nvar) = 'resolved tke budget: implicit diffusion'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'

        !$acc parallel loop gang vector default(present) private(k)
        do i=1,nk+1
          tavg(k) = 0.0
          uub(k)  = 0.0
          vvb(k)  = 0.0
          wwb(k)  = 0.0
        enddo

        call rtkeb_addu(dum1,utk,udiag,ud_hidiff,u_hidiff)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          uub(k) = uub(k) + savg(k)
        enddo

        
        call rtkeb_addv(dum1,vtk,vdiag,vd_hidiff,v_hidiff)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          vvb(k)  = vvb(k) + savg(k)
        enddo

        call rtkeb_addw(dum1,wtk,wdiag,wd_hidiff,w_hidiff)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + 0.5*(savg(k)+savg(k+1))
          wwb(k) = wwb(k) + savg(k)
        enddo


        call rtkeb_addu(dum1,utk,udiag,ud_vidiff,u_vidiff)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          uub(k)  = uub(k) + savg(k)
        enddo

        call rtkeb_addv(dum1,vtk,vdiag,vd_vidiff,v_vidiff)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          vvb(k)  = vvb(k) + savg(k)
        enddo

        call rtkeb_addw(dum1,wtk,wdiag,wd_vidiff,w_vidiff)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + 0.5*(savg(k)+savg(k+1))
          wwb(k) = wwb(k) + savg(k)
        enddo


        call write3d(tavg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !......
        nvar = nvar+1
        varname(nvar) = 'uub_idiff'
        vardesc(nvar) = '<uu> budget: implicit diffusion term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'
        call write3d(uub,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......
        nvar = nvar+1
        varname(nvar) = 'vvb_idiff'
        vardesc(nvar) = '<vv> budget: implicit diffusion term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'
        call write3d(vvb,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......
        nvar = nvar+1
        varname(nvar) = 'wwb_idiff'
        vardesc(nvar) = '<ww> budget: implicit diffusion term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'
        call write3d(wwb,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......

      endif


      !c-c-c-c-c-c-c-c-c-c

!!!      if( ud_lsw.ge.1 .and. vd_lsw.ge.1 .and. wd_lsw.ge.1 )then
      if( ud_lsw.ge.1 .and. vd_lsw.ge.1 )then

!!!        nvar = nvar+1
!!!        varname(nvar) = 'rtkeb_lsw'
!!!        vardesc(nvar) = 'resolved tke budget: large-scale w'
!!!        varunit(nvar) = 'm2/s3'
!!!        vargrid(nvar) = 's'
!!!
!!!        call getavg3d(dum1,savg)
!!!        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_hturb.ge.1 .and. vd_hturb.ge.1 .and. wd_hturb.ge.1 .and.  &
          ud_vturb.ge.1 .and. vd_vturb.ge.1 .and. wd_vturb.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'rtkeb_turb'
        vardesc(nvar) = 'resolved tke budget: subgrid turbulence'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          tavg(k) = 0.0
          uub(k)  = 0.0
          vvb(k)  = 0.0
          wwb(k)  = 0.0
        enddo

        call rtkeb_addu(dum1,utk,udiag,ud_hturb,u_hturb)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          uub(k) = uub(k) + savg(k)
        enddo

        call rtkeb_addv(dum1,vtk,vdiag,vd_hturb,v_hturb)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          vvb(k) = vvb(k) + savg(k)
        enddo

        call rtkeb_addw(dum1,wtk,wdiag,wd_hturb,w_hturb)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + 0.5*(savg(k)+savg(k+1))
          wwb(k) = wwb(k) + savg(k)
        enddo


        call rtkeb_addu(dum1,utk,udiag,ud_vturb,u_vturb)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          uub(k) = uub(k) + savg(k)
        enddo

        call rtkeb_addv(dum1,vtk,vdiag,vd_vturb,v_vturb)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          vvb(k) = vvb(k) + savg(k)
        enddo

        call rtkeb_addw(dum1,wtk,wdiag,wd_vturb,w_vturb)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + 0.5*(savg(k)+savg(k+1))
          wwb(k) = wwb(k) + savg(k)
        enddo


        call write3d(tavg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !......
        nvar = nvar+1
        varname(nvar) = 'uub_turb'
        vardesc(nvar) = '<uu> budget: subgrid turbulence term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'
        call write3d(uub,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......
        nvar = nvar+1
        varname(nvar) = 'vvb_turb'
        vardesc(nvar) = '<vv> budget: subgrid turbulence term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'
        call write3d(vvb,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......
        nvar = nvar+1
        varname(nvar) = 'wwb_turb'
        vardesc(nvar) = '<ww> budget: subgrid turbulence term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'
        call write3d(wwb,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_cor.ge.1 .and. vd_cor.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'rtkeb_cor'
        vardesc(nvar) = 'resolved tke budget: Coriolis accel (dummy check)'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          tavg(k) = 0.0
          uub(k)  = 0.0
          vvb(k)  = 0.0
          wwb(k)  = 0.0
        enddo

        call rtkeb_addu(dum1,utk,udiag,ud_cor,u_cor)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          uub(k) = uub(k) + savg(k)
        enddo

        call rtkeb_addv(dum1,vtk,vdiag,vd_cor,v_cor)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          vvb(k) = vvb(k) + savg(k)
        enddo


        call write3d(tavg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !......
        nvar = nvar+1
        varname(nvar) = 'uub_cor'
        vardesc(nvar) = '<uu> budget: Coriolis term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'
        call write3d(uub,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......
        nvar = nvar+1
        varname(nvar) = 'vvb_cor'
        vardesc(nvar) = '<vv> budget: Coriolis term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'
        call write3d(vvb,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_rdamp.ge.1 .and. vd_rdamp.ge.1 .and. wd_rdamp.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'rtkeb_rdamp'
        vardesc(nvar) = 'resolved tke budget: rdamp'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk+1
          tavg(k) = 0.0
          uub(k) = 0.0
          vvb(k) = 0.0
          wwb(k) = 0.0
        enddo

        call rtkeb_addu(dum1,utk,udiag,ud_rdamp,u_rdamp)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          uub(k) = uub(k) + savg(k)
        enddo

        call rtkeb_addv(dum1,vtk,vdiag,vd_rdamp,v_rdamp)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + savg(k)
          vvb(k) = vvb(k) + savg(k)
        enddo

        call rtkeb_addw(dum1,wtk,wdiag,wd_rdamp,w_rdamp)
        call getavg3d(dum1,savg)
        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          tavg(k) = tavg(k) + 0.5*(savg(k)+savg(k+1))
          wwb(k) = wwb(k) + savg(k)
        enddo


        call write3d(tavg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        !......
        nvar = nvar+1
        varname(nvar) = 'uub_rdamp'
        vardesc(nvar) = '<uu> budget: rdamp term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'
        call write3d(uub,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......
        nvar = nvar+1
        varname(nvar) = 'vvb_rdamp'
        vardesc(nvar) = '<vv> budget: rdamp term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 's'
        call write3d(vvb,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......
        nvar = nvar+1
        varname(nvar) = 'wwb_rdamp'
        vardesc(nvar) = '<ww> budget: rdamp term'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'
        call write3d(wwb,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !......

      endif

      !c-c-c-c-c-c-c-c-c-c

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      !c-c-c-c-c-c-c-c-c-c

      if( ud_hadv.ge.1 .and. wd_hadv.ge.1 .and.  &
          ud_vadv.ge.1 .and. wd_vadv.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'uwb_adv'
        vardesc(nvar) = '<uw> budget: advection terms'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) 
        do k=kb,ke
        do j=jb,je
        do i=ib,ie
          dum1(i,j,k) = 0.0
        enddo
        enddo
        enddo
        call   uwb_term(dum1,utk,wtk,udiag,wdiag,ud_hadv,wd_hadv,u_hadv,w_hadv)
        call   uwb_term(dum1,utk,wtk,udiag,wdiag,ud_vadv,wd_vadv,u_vadv,w_vadv)

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_hidiff.ge.1 .and. wd_hidiff.ge.1 .and.  &
          ud_vidiff.ge.1 .and. wd_vidiff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'uwb_idiff'
        vardesc(nvar) = '<uw> budget: implicit diffusion'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) 
        do k=kb,ke
        do j=jb,je
        do i=ib,ie
          dum1(i,j,k) = 0.0
        enddo
        enddo
        enddo
        call   uwb_term(dum1,utk,wtk,udiag,wdiag,ud_hidiff,wd_hidiff,u_hidiff,w_hidiff)
        call   uwb_term(dum1,utk,wtk,udiag,wdiag,ud_vidiff,wd_vidiff,u_vidiff,w_vidiff)

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_hturb.ge.1 .and. wd_hturb.ge.1 .and.  &
          ud_vturb.ge.1 .and. wd_vturb.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'uwb_turb'
        vardesc(nvar) = '<uw> budget: subgrid turbulence'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present)
        do k=kb,ke
        do j=jb,je
        do i=ib,ie
          dum1(i,j,k) = 0.0
        enddo
        enddo
        enddo
        call   uwb_term(dum1,utk,wtk,udiag,wdiag,ud_hturb,wd_hturb,u_hturb,w_hturb)
        call   uwb_term(dum1,utk,wtk,udiag,wdiag,ud_vturb,wd_vturb,u_vturb,w_vturb)

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_pgrad.ge.1 .and. wd_pgrad.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'uwb_prs'
        vardesc(nvar) = '<uw> budget: pressure terms'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present)
        do k=kb,ke
        do j=jb,je
        do i=ib,ie
          dum1(i,j,k) = 0.0
        enddo
        enddo
        enddo
        call   uwb_term(dum1,utk,wtk,udiag,wdiag,ud_pgrad,wd_pgrad,u_pgrad,w_pgrad)

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( ud_cor.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'uwb_cor'
        vardesc(nvar) = '<uw> budget: Coriolis'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            dum1(i,j,1) = 0.0
            dum1(i,j,nk+1) = 0.0
          enddo
          enddo

          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=2,nk
          do j=1,nj
          do i=1,ni
            dum1(i,j,k) =                                             &
              wtk(i,j,k)*0.25*( (udiag(i  ,j,k  ,ud_cor)-u_cor(k  ))  &
                               +(udiag(i+1,j,k  ,ud_cor)-u_cor(k  ))  &
                               +(udiag(i  ,j,k-1,ud_cor)-u_cor(k-1))  &
                               +(udiag(i+1,j,k-1,ud_cor)-u_cor(k-1)) )
          enddo
          enddo
          enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( wd_buoy.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'uwb_buoy'
        vardesc(nvar) = '<uw> budget: buoyancy'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            dum1(i,j,1) = 0.0
            dum1(i,j,nk+1) = 0.0
          enddo
          enddo

          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=2,nk
          do j=1,nj
          do i=1,ni
            dum1(i,j,k) = +(wdiag(i,j,k,wd_buoy)-w_buoy(k)) &
                          *0.25*( utk(i  ,j,k  )+utk(i+1,j,k  ) &
                                 +utk(i  ,j,k-1)+utk(i+1,j,k-1) )
          enddo
          enddo
          enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      !c-c-c-c-c-c-c-c-c-c

      if( vd_hadv.ge.1 .and. wd_hadv.ge.1 .and.  &
          vd_vadv.ge.1 .and. wd_vadv.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vwb_adv'
        vardesc(nvar) = '<vw> budget: advection terms'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) 
        do k=kb,ke
        do j=jb,je
        do i=ib,ie
          dum1(i,j,k) = 0.0
        enddo
        enddo
        enddo
        call   vwb_term(dum1,vtk,wtk,vdiag,wdiag,vd_hadv,wd_hadv,v_hadv,w_hadv)
        call   vwb_term(dum1,vtk,wtk,vdiag,wdiag,vd_vadv,wd_vadv,v_vadv,w_vadv)

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( vd_hidiff.ge.1 .and. wd_hidiff.ge.1 .and.  &
          vd_vidiff.ge.1 .and. wd_vidiff.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vwb_idiff'
        vardesc(nvar) = '<vw> budget: implicit diffusion'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) 
        do k=kb,ke
        do j=jb,je
        do i=ib,ie
          dum1(i,j,k) = 0.0
        enddo
        enddo
        enddo
        call   vwb_term(dum1,vtk,wtk,vdiag,wdiag,vd_hidiff,wd_hidiff,v_hidiff,w_hidiff)
        call   vwb_term(dum1,vtk,wtk,vdiag,wdiag,vd_vidiff,wd_vidiff,v_vidiff,w_vidiff)

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif
      ! stop 'domaindiag: after vwb_idiff'

      !c-c-c-c-c-c-c-c-c-c

      if( vd_hturb.ge.1 .and. wd_hturb.ge.1 .and.  &
          vd_vturb.ge.1 .and. wd_vturb.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vwb_turb'
        vardesc(nvar) = '<vw> budget: subgrid turbulence'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present) 
        do k=kb,ke
        do j=jb,je
        do i=ib,ie
          dum1(i,j,k) = 0.0
        enddo
        enddo
        enddo
        call   vwb_term(dum1,vtk,wtk,vdiag,wdiag,vd_hturb,wd_hturb,v_hturb,w_hturb)
        call   vwb_term(dum1,vtk,wtk,vdiag,wdiag,vd_vturb,wd_vturb,v_vturb,w_vturb)

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( vd_pgrad.ge.1 .and. wd_pgrad.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vwb_prs'
        vardesc(nvar) = '<vw> budget: pressure terms'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector collapse(3) default(present)
        do k=kb,ke
        do j=jb,je
        do i=ib,ie
          dum1(i,j,k) = 0.0
        enddo
        enddo
        enddo
        call   vwb_term(dum1,vtk,wtk,vdiag,wdiag,vd_pgrad,wd_pgrad,v_pgrad,w_pgrad)

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( vd_cor.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vwb_cor'
        vardesc(nvar) = '<vw> budget: Coriolis'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            dum1(i,j,1) = 0.0
            dum1(i,j,nk+1) = 0.0
          enddo
          enddo

          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=2,nk
          do j=1,nj
          do i=1,ni
            dum1(i,j,k) =                                             &
              wtk(i,j,k)*0.25*( (vdiag(i,j  ,k  ,vd_cor)-v_cor(k  ))  &
                               +(vdiag(i,j+1,k  ,vd_cor)-v_cor(k  ))  &
                               +(vdiag(i,j  ,k-1,vd_cor)-v_cor(k-1))  &
                               +(vdiag(i,j+1,k-1,vd_cor)-v_cor(k-1)) )
          enddo
          enddo
          enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

      if( wd_buoy.ge.1 )then

        nvar = nvar+1
        varname(nvar) = 'vwb_buoy'
        vardesc(nvar) = '<vw> budget: buoyancy'
        varunit(nvar) = 'm2/s3'
        vargrid(nvar) = 'w'

          !$acc parallel loop gang vector collapse(2) default(present) private(i,j)
          do j=1,nj
          do i=1,ni
            dum1(i,j,1) = 0.0
            dum1(i,j,nk+1) = 0.0
          enddo
          enddo

          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=2,nk
          do j=1,nj
          do i=1,ni
            dum1(i,j,k) = +(wdiag(i,j,k,wd_buoy)-w_buoy(k)) &
                          *0.25*( vtk(i,j,k  )+vtk(i,j+1,k  ) &
                                 +vtk(i,j,k-1)+vtk(i,j+1,k-1) )
          enddo
          enddo
          enddo

        call getavg3d(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      endif

      !c-c-c-c-c-c-c-c-c-c

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      !c-c-c-c-c-c-c-c-c-c

!!!      IF( dosub )THEN

        nvar = nvar+1
        varname(nvar) = 'wprof'
        vardesc(nvar) = 'large-scale vertical velocity profile'
        varunit(nvar) = 'm/s'
        vargrid(nvar) = 'w'

        !$acc parallel loop gang vector default(present) private(k)
        do k=1,nk
          savg(k) = wprof(k)
        enddo

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

!!!      ENDIF

      !c-c-c-c-c-c-c-c-c-c

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wmax'
        vardesc(nvar) = 'maximum w'
        varunit(nvar) = 'm/s'
        vargrid(nvar) = 'w'

        call getmax(w3d(ib,jb,kb),savg,nkp1)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wmin'
        vardesc(nvar) = 'minimum w'
        varunit(nvar) = 'm/s'
        vargrid(nvar) = 'w'

        call getmin(w3d(ib,jb,kb),savg,nkp1)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

#if 0
      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'tmfu'
        vardesc(nvar) = 'total upward max flux'
        varunit(nvar) = 'kg m/s'
        vargrid(nvar) = 's'

        !$acc parallel default(present) reduction(+:temd)
        !$acc loop gang
        do k=1,nk
          temd=0.0d0
          !$acc loop vector collapse(2)
          do j=1,nj
          do i=1,ni
            !savg(k)=savg(k)+rho(i,j,k)*0.5*max(0.0,(w3d(i,j,k)+w3d(i,j,k+1)))*ruh(i)*rvh(j)
            temd=temd+rho(i,j,k)*0.5*max(0.0,(w3d(i,j,k)+w3d(i,j,k+1)))*ruh(i)*rvh(j)
          enddo
          enddo
          savg(k)=temd*dx*dy
        enddo
        !$acc end parallel

#ifdef MPI
        !!$acc host_data use_device(savg)
        !$acc update host(savg)
        call MPI_ALLREDUCE(MPI_IN_PLACE,savg,nk,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        !$acc update device(savg)
        !!$acc end host_data
#endif

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'tmfd'
        vardesc(nvar) = 'total downward mass flux'
        varunit(nvar) = 'kg m/s'
        vargrid(nvar) = 's'

        !$acc parallel default(present) reduction(+:temd)
        !$acc loop gang
        do k=1,nk
          temd=0.0d0
          !$acc loop vector collapse(2)
          do j=1,nj
          do i=1,ni
            !savg(k)=savg(k)+rho(i,j,k)*0.5*min(0.0,(w3d(i,j,k)+w3d(i,j,k+1)))*ruh(i)*rvh(j)
            temd=temd+rho(i,j,k)*0.5*min(0.0,(w3d(i,j,k)+w3d(i,j,k+1)))*ruh(i)*rvh(j)
          enddo
          enddo
          savg(k)=temd*dx*dy
        enddo
        !$acc end parallel

#ifdef MPI
        !!$acc host_data use_device(savg)
        !$acc update host(savg)
        call MPI_ALLREDUCE(MPI_IN_PLACE,savg,nk,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        !$acc update device(savg)
        !!$acc end host_data
#endif

        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
#endif

      !c-c-c-c-c-c-c-c-c-c

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k) = sqrt( (umove+0.5*(u3d(i,j,k)+u3d(i+1,j,k)))**2     &
                             +(vmove+0.5*(v3d(i,j,k)+v3d(i,j+1,k)))**2 )
        enddo
        enddo
        enddo

        nvar = nvar+1
        varname(nvar) = 'wspmax'
        vardesc(nvar) = 'maximum (grnd-rel) horiz wind speed'
        varunit(nvar) = 'm/s'
        vargrid(nvar) = 's'
        !print *,'domaindiag: wspmax'
        call getmax(dum1,savg)
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'wspmin'
        vardesc(nvar) = 'minimum (grnd-rel) horiz wind speed'
        varunit(nvar) = 'm/s'
        vargrid(nvar) = 's'

        !print *,'domaindiag: wspmin'
        call getmin(dum1,savg)
        !print *,'domaindiag: wspmin: after getmin'
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        !print *,'domaindiag: wspmin: after write3d'

      !c-c-c-c-c-c-c-c-c-c

      IF( .not. terrain_flag )THEN

        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,ni+1
          dum1(i,j,k) = (v3d(i,j,k)-v3d(i-1,j,k))*rdx*uf(i)   &
                       -(u3d(i,j,k)-u3d(i,j-1,k))*rdy*vf(j)
        enddo
        enddo
        enddo

        nvar = nvar+1
        varname(nvar) = 'zetamax'
        vardesc(nvar) = 'maximum vertical vorticity'
        varunit(nvar) = '1/s'
        vargrid(nvar) = 's'

        
        !print *,'domaindiag: zetamax before getmax'
        call getmax(dum1,savg)
        !print *,'domaindiag: zetamax after getmax'
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar = nvar+1
        varname(nvar) = 'zetamin'
        vardesc(nvar) = 'minimum vertical vorticity'
        varunit(nvar) = '1/s'
        vargrid(nvar) = 's'

        !print *,'domaindiag: zetamin before getmin'
        call getmin(dum1,savg)
        !print *,'domaindiag: zetamin after getmin'
        call write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      ENDIF

      !c-c-c-c-c-c-c-c-c-c

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  more 2d vars

      !c-c-c-c-c-c-c-c-c-c
      !print *,'domaindiag: before int_tkes'

    IF( cm1setup.eq.1 .and. iusetke )THEN

        ! vertically integrate TKE:

        nvar2d = nvar2d+1
        nvar = nvar+1
        varname(nvar) = 'int_tkes'
        vardesc(nvar) = 'vertically integrated subgrid tke'
        varunit(nvar) = 'kg/s2'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'
        savg2d = 0.0
        !$acc parallel default(present) reduction(+:savg2d)
        !$acc loop gang vector reduction(+:savg2d)
        do k=2,nk
          savg2d = savg2d+rf0(1,1,k)*stke(k)*dz*rmf(1,1,k)
        enddo
        !$acc end parallel
        call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar2d = nvar2d+1
        nvar = nvar+1
        varname(nvar) = 'int_tker'
        vardesc(nvar) = 'vertically integrated resolved tke'
        varunit(nvar) = 'kg/s2'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'
        savg2d = 0.0
        !$acc parallel loop gang vector default(present) reduction(+:savg2d)
        do k=2,nk
          savg2d = savg2d+rf0(1,1,k)*rtke(k)*dz*rmf(1,1,k)
        enddo
        call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

        nvar2d = nvar2d+1
        nvar = nvar+1
        varname(nvar) = 'int_tket'
        vardesc(nvar) = 'vertically integrated total tke'
        varunit(nvar) = 'kg/s2'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'
        savg2d = 0.0
        !$acc parallel loop gang vector default(present) reduction(+:savg2d)
        do k=2,nk
          savg2d = savg2d+rf0(1,1,k)*(stke(k)+rtke(k))*dz*rmf(1,1,k)
        enddo
        call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

    ENDIF

      !c-c-c-c-c-c-c-c-c-c

        nvar2d = nvar2d+1
        nvar = nvar+1
        varname(nvar) = 'wvar_max'
        vardesc(nvar) = 'maximum vertical velocity variance'
        varunit(nvar) = 'm2/s2'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'

        savg2d = 0.0
        !$acc parallel loop gang vector default(present) reduction(max:savg2d) 
        do k=2,nk
          savg2d = max( savg2d , dble(wvar(k)) )
        enddo

        call write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
        if( myid.eq.0 ) print *,'  Metric: max w variance (m2/s2) = ',savg2d

      !c-c-c-c-c-c-c-c-c-c

        nvar2d = nvar2d+1
        nvar = nvar+1
        varname(nvar) = 'wstar'
        vardesc(nvar) = 'wstar (vert. integr. buoy flux, r+s)'
        varunit(nvar) = 'm/s'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'

        call write2d(wstar,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar2d = nvar2d+1
        nvar = nvar+1
        varname(nvar) = 'fbmin'
        vardesc(nvar) = 'minimum value of buoyancy flux'
        varunit(nvar) = 'm2/s3'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'

        call write2d(fbmin,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

        nvar2d = nvar2d+1
        nvar = nvar+1
        varname(nvar) = 'zfbmin'
        vardesc(nvar) = 'level of minimum buoyancy flux'
        varunit(nvar) = 'm'
        varlvls(nvar) = 0
        vargrid(nvar) = '2'

        call write2d(zfbmin,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)

      !c-c-c-c-c-c-c-c-c-c

  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
  !ccccccc    all done    ccccccccccccccccccccccccccccccccccccccccccccccccccccc!
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
  ! close output file:

    id0:  IF( myid.eq.0 )THEN

!-------------------------------------------------------------------
!  grads format

    of2:  &
    IF( output_format.eq.1 )THEN

        ! close binary output files:
        close(unit=fnums)
        close(unit=fnumw)

      ! write descriptor files:
        do nfile=1,2
          do i=1,maxstring
            string(i:i) = ' '
          enddo
          if( nfile.eq.1 )then
            string = 'cm1out_diag_s.ctl'
          elseif( nfile.eq.2 )then
            string = 'cm1out_diag_w.ctl'
          endif
          open(unit=66,file=string)
          do i=1,maxstring
            string(i:i) = ' '
          enddo
          if( nfile.eq.1 )then
            string = 'cm1out_diag_%t6_s.dat'
          elseif( nfile.eq.2 )then
            string = 'cm1out_diag_%t6_w.dat'
          endif
          write(66,101) string
          write(66,102)
          if( nfile.eq.1 )then
            write(66,103) trim(cm1version)
          elseif( nfile.eq.2 )then
            write(66,203) trim(cm1version)
          endif
          write(66,104) grads_undef
          write(66,105)
          write(66,106)
          if( nfile.eq.1 )then
            write(66,107) nk
            !$acc update host(zh(1,1,1:nk))
            do k=1,nk
              write(66,217) 0.001*zh(1,1,k)
            enddo
          elseif( nfile.eq.2 )then
            write(66,107) nk+1
            !$acc update host(zf(1,1,1:nk))
            do k=1,nk+1
              write(66,217) 0.001*zf(1,1,k)
            enddo
          endif
          write(66,108) nwritet
          ntmp = 0
          if( nfile.eq.1 )then
            do n=1,nvar
              if( vargrid(n).eq.'s' .or. vargrid(n).eq.'2' ) ntmp = ntmp+1
            enddo
          else
            do n=1,nvar
              if( vargrid(n).eq.'w' .or. vargrid(n).eq.'2' ) ntmp = ntmp+1
            enddo
          endif
          write(66,109) ntmp
          do n=1,nvar
            doit = .false.
            if( vargrid(n).eq.'2' ) doit = .true.
            if( nfile.eq.1 .and. vargrid(n).eq.'s' ) doit = .true.
            if( nfile.eq.2 .and. vargrid(n).eq.'w' ) doit = .true.
            if( doit )then
              a1 = varname(n)
              a2 = vardesc(n)
              !---
              a16 = '                '
              nn = len(trim(varunit(n)))
              write(a16(2:15),314) varunit(n)
              write(a16(1:1),301 )       '('
              write(a16(nn+2:nn+2),301 ) ')'
              !---
              if(     nfile.eq.1 )then
                write(66,110) a1(1:12),varlvls(n),a2(1:40),a16
              elseif( nfile.eq.2 )then
                if( varlvls(n).eq.0 )then
                  write(66,110) a1(1:12), varlvls(n)   ,a2(1:40),a16
                else
                  write(66,110) a1(1:12),(varlvls(n)+1),a2(1:40),a16
                endif
              endif
            endif
          enddo
          write(66,111)
          close(unit=66)
        enddo

 301    format(a1)
 314    format(a14)

 101    format('dset ^',a)
 102    format('options template')
 103    format('title CM1 domain-wide diagnostics at scalar levels, using version ',a,'; most variables herein are domain averages; z dim is km')
 203    format('title CM1 domain-wide diagnostics at w levels, using version ',a,'; most variables herein are domain averages; z dim is km')
 104    format('undef ',f10.1)
 105    format('xdef 1 linear 0 1')
 106    format('ydef 1 linear 0 1')
 107    format('zdef ',i6,' levels')
 217    format(2x,f16.9)
 108    format('tdef ',i10,' linear 00:00Z01JAN0001 1YR')
 109    format('vars ',i6)
 110    format(a12,2x,i6,' 99 ',a40,1x,a16)
 111    format('endvars')

!-------------------------------------------------------------------
! netcdf format

#ifdef NETCDF
    ELSEIF( output_format.eq.2 )THEN  of2

        !PORTME
      call       writediag_nc(wloop,varmax,ncid,varname,vardesc,varunit,varlvls,vargrid,nvar,mtime,zh,zf,dum1(ib,jb,kb),dum2(ib,jb,kb))

#endif
!-------------------------------------------------------------------

    ENDIF  of2

    ENDIF  id0

  ENDDO  bigwloop

    !$acc end data

    if( myid.eq.0 ) print *
    if( myid.eq.0 ) print *,' ..... end domaindiag code ..... '
    if( myid.eq.0 ) print *
    !stop 'domaindiag: at the end of the subroutine'

    end subroutine domaindiag


    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine momentum_flux(u3d,v3d,w3d,t13,t23,m13,m23,rf,rf0,c1,c2,dum3,dum4,dum5,ufr,ufs,ufd,vfr,vfs,vfd)
      use input
      use constants
      use interp_routines
#ifdef MPI
      use mpi
#endif
      implicit none

      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: u3d
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: v3d
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: w3d
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: t13,t23,rf,rf0,c1,c2
      real, intent(in), dimension(ibnba:ienba,jbnba:jenba,kbnba:kenba) :: m13,m23
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke)   :: dum3,dum4,dum5
      double precision, intent(inout), dimension(nk+1) :: ufr,ufs,ufd,vfr,vfs,vfd

      integer :: i,j,k
      real :: mfr,mfs,mfd,wbar,wpbar,cc1,cc2
      double precision :: temd,weps
      double precision :: ubart
      double precision, dimension(nk) :: ubar,vbar
      double precision :: vbart,vfrt,vfdt,ufst,vfst,ufrt,ufdt

      !$acc data create(ubar,vbar)
      weps = 100.0*epsilon

    !-------------------------------------------------------
    !  u component:

      !$acc parallel default(present) private(i,j,k,cc1,cc2)
      !$acc loop gang vector 
      do k=1,nk+1
        ufr(k) = 0.0
        ufs(k) = 0.0
        ufd(k) = 0.0
      enddo

      ! baseline algorithm:
      !$acc loop gang vector collapse(3) 
      do k=2,nk
      do j=1,nj
      do i=1,ni
        cc2 = 0.5*(c2(i-1,j,k)+c2(i,j,k))
        cc1 = 1.0-cc2
        dum3(i,j,k) = (cc1*u3d(i,j,k-1)+cc2*u3d(i,j,k))
        dum4(i,j,k) = dum3(i,j,k)
      enddo
      enddo
      enddo
      !$acc end parallel

    IF( advwenov.ge.1 )THEN

      if(     weno_order.eq.5 )then
        call vinterp_weno5(  2 ,ni+1,nj,nk,c1,c2,w3d,dum3, u3d ,  0 ,0,weps)
        call vinterp_flx6(   2 ,ni+1,nj,nk,c1,c2,w3d,dum4, u3d )
      endif

    ELSE

        !buh3
      if(     vadvordrs.eq.5 )then
        call vinterp_flx5(   2 ,ni+1,nj,nk,c1,c2,w3d,dum3, u3d )
        call vinterp_flx6(   2 ,ni+1,nj,nk,c1,c2,w3d,dum4, u3d )
      elseif( vadvordrs.eq.6 )then
        call vinterp_flx6(   2 ,ni+1,nj,nk,c1,c2,w3d,dum3, u3d )
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum4(i,j,k) = dum3(i,j,k)
        enddo
        enddo
        enddo
      endif

    ENDIF

  !----------------------


#ifdef _B4B16R
    !$acc update host(dum3)
#else
    !$acc parallel default(present) private(i,j,k) reduction(+:ubart)
    !$acc loop gang vector
#endif
    do k=1,nk
      ubar(k) = 0.0
    enddo

#ifndef _B4B16R
    !$acc loop gang 
#endif
    do k=2,nk
      ubart = 0.0d0
#ifndef _B4B16R
      !$acc loop vector collapse(2) reduction(+:ubart)
#endif
      do j=1,nj
      do i=1,ni
        ubart = ubart+dum3(i,j,k)
      enddo
      enddo
      ubar(k) = ubart
    enddo
#ifdef _B4B16R
    !$acc update device(ubar)
#else
    !$acc end parallel
#endif
        !buh4

#ifdef MPI
    !$acc update host(ubar)
    call MPI_ALLREDUCE(MPI_IN_PLACE,ubar,nk,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
    !$acc update device(ubar)
#endif

    temd = 1.0d0/dble(nx*ny)

    !$acc parallel default(present)
    !$acc loop gang vector
    do k=2,nk
      ubar(k)  = ubar(k)*temd
    enddo
    !$acc end parallel

  !----------------------

      !$omp parallel default(shared) private(i,j,k,wbar) reduction(+:ufrt,ufdt)
      !$acc parallel default(present) reduction(+:ufrt,ufdt)
      !$acc loop gang 
      do k=2,nk
        ufrt=0.0d0
        ufdt=0.0d0
        !$acc loop vector collapse(2) reduction(+:ufrt,ufdt)
        do j=1,nj
        do i=1,ni
          wbar = 0.5*(w3d(i,j,k)+w3d(i-1,j,k))
          ufrt = ufrt + wbar*(dum4(i,j,k)-ubar(k))
          ufdt = ufdt + wbar*(dum3(i,j,k)-dum4(i,j,k))
        !buh5
        enddo
        enddo
        ufr(k) =ufrt
        ufd(k) =ufdt
      enddo
      !$acc end parallel

    !-------------------------------------------------------
    !  v component:
      !$acc parallel loop gang vector default(present)
      do k=1,nk+1
        vfr(k) = 0.0
        vfs(k) = 0.0
        vfd(k) = 0.0
      enddo

      ! baseline algorithm:
      !$acc parallel loop gang vector collapse(3) default(present)
      do k=2,nk
      do j=1,nj
      do i=1,ni
        cc2 = 0.5*(c2(i,j-1,k)+c2(i,j,k))
        cc1 = 1.0-cc2
        dum3(i,j,k) = (cc1*v3d(i,j,k-1)+cc2*v3d(i,j,k))
        dum4(i,j,k) = dum3(i,j,k)
      enddo
      enddo
      enddo

    IF( advwenov.ge.1 )THEN

      if(     weno_order.eq.5 )then
        print *,'momentum_flux: calling vinterp_{weno5,flx6}'
        call vinterp_weno5(  3 ,ni,nj+1,nk,c1,c2,w3d,dum3, v3d ,  0 ,0,weps)
 
        call vinterp_flx6(   3 ,ni,nj+1,nk,c1,c2,w3d,dum4, v3d )
      endif

    ELSE

        !buh3
      if(     vadvordrs.eq.5 )then
        print *,'momentum_flux: calling vinterp_{flx5,flx6}'
        call vinterp_flx5(   3 ,ni,nj+1,nk,c1,c2,w3d,dum3, v3d )
        call vinterp_flx6(   3 ,ni,nj+1,nk,c1,c2,w3d,dum4, v3d )
      elseif( vadvordrs.eq.6 )then
        print *,'momentum_flux: calling vinterp_flx6'
        call vinterp_flx6(   3 ,ni,nj+1,nk,c1,c2,w3d,dum3, v3d )
        !$acc parallel loop gang vector collapse(3) default(present)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum4(i,j,k) = dum3(i,j,k)
        enddo
        enddo
        enddo
      endif

    ENDIF

  !----------------------

    
#ifndef _B4B21R
    !$acc parallel default(present) private(i,j,k) reduction(+:vbart) reduction(+:vfrt,vfdt)
    !$acc loop gang vector
#endif
    do k=1,nk
      vbar(k) = 0.0
    enddo

#ifdef _B4B21R
    !$acc update host(dum3)
#else
    !$acc loop gang
#endif
    do k=2,nk
      vbart = 0.0d0
#ifndef _B4B21R
      !$acc loop vector collapse(2) reduction(+:vbart)  
#endif
      do j=1,nj
      do i=1,ni
        vbart = vbart+dum3(i,j,k)
      enddo
      enddo
      vbar(k) = vbart
    enddo
#ifdef _B4B21R
    !$acc update device(vbar)
#else
    !$acc end parallel
#endif
        !buh4
    !stop 'domaindiag: point #1'

#ifdef MPI
    !$acc update host(vbar)
    call MPI_ALLREDUCE(MPI_IN_PLACE,vbar,nk,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
    !$acc update device(vbar)
#endif

    temd = 1.0d0/dble(nx*ny)

    !$acc parallel default(present)
    !$acc loop gang vector
    do k=2,nk
      vbar(k)  = vbar(k)*temd
    enddo
    !$acc end parallel

  !----------------------

      !$omp parallel do default(shared) private(i,j,k,wbar)
      !$acc parallel default(present) reduction(+:vfrt,vfdt)
      !$acc loop gang
      do k=2,nk
        vfrt = 0.0d0
        vfdt = 0.0d0
        !$acc loop vector collapse(2) reduction(+:vfrt,vfdt)
        do j=1,nj
        do i=1,ni
          wbar = 0.5*(w3d(i,j,k)+w3d(i,j-1,k))
          vfrt = vfrt + wbar*(dum4(i,j,k)-vbar(k))
          vfdt = vfdt+ wbar*(dum3(i,j,k)-dum4(i,j,k))
        !buh5
        enddo
        enddo
        vfr(k) = vfrt
        vfd(k) = vfdt
      enddo
      !$acc end parallel

    !-------------------------------------------------------

#ifdef _B4B23R
      !$acc update host(rf,m13,m23,t13,t23)
#else
      !$acc parallel default(present) private(i,j,k) reduction(+:ufst,vfst)
#endif
      if( cm1setup.ge.1 .or. ipbl.eq.2 )then
        if( sgsmodel.eq.5 .or. sgsmodel.eq.6 )then
          ! use mij:
          !$omp parallel do default(shared) private(i,j,k) reduction(+:ufst,vfst)
          !$acc loop gang
          do k=1,nk+1
            ufst = 0.0d0
            vfst = 0.0d0
            !$acc loop vector collapse(2) reduction(+:ufst,vfst)
            do j=1,nj
            do i=1,ni
              ufst = ufst - m13(i,j,k)/(0.5*(rf(i,j,k)+rf(i-1,j,k)))
              vfst = vfst - m23(i,j,k)/(0.5*(rf(i,j,k)+rf(i,j-1,k)))
            enddo
            enddo
            ufs(k) = ufst
            vfs(k) = vfst
          enddo
        else
          ! use tij:
          !$omp parallel do default(shared) private(i,j,k)
          !$acc loop gang
          do k=1,nk+1
            ufst = 0.0d0
            vfst = 0.0d0
            !$acc loop vector collapse(2) reduction(+:ufst,vfst)
            do j=1,nj
            do i=1,ni
              ufst = ufst- t13(i,j,k)/(0.5*(rf(i,j,k)+rf(i-1,j,k)))
              vfst = vfst- t23(i,j,k)/(0.5*(rf(i,j,k)+rf(i,j-1,k)))
            enddo
            enddo
            ufs(k) = ufst
            vfs(k) = vfst
          enddo
        endif
      else
        !$omp parallel do default(shared) private(i,j,k)
        !$acc loop gang vector 
        do k=1,nk+1
          ufs(k) = 0.0
          vfs(k) = 0.0
        enddo
      endif
      !$acc end parallel

    !-------------------------------------------------------

#ifdef MPI
    !$acc update host(ufr,ufs,ufd,vfr,vfs,vfd)
    call MPI_ALLREDUCE(MPI_IN_PLACE,ufr,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
    call MPI_ALLREDUCE(MPI_IN_PLACE,ufs,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
    call MPI_ALLREDUCE(MPI_IN_PLACE,ufd,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)

    call MPI_ALLREDUCE(MPI_IN_PLACE,vfr,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
    call MPI_ALLREDUCE(MPI_IN_PLACE,vfs,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
    call MPI_ALLREDUCE(MPI_IN_PLACE,vfd,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
    !$acc update device(ufr,ufs,ufd,vfr,vfs,vfd)
#endif

    temd = 1.0d0/dble(nx*ny)

    !$acc parallel default(present)
    !$acc loop gang vector
    do k=1,nk+1
      ufr(k)  = ufr(k)*temd
      ufs(k)  = ufs(k)*temd
      ufd(k)  = ufd(k)*temd

      vfr(k)  = vfr(k)*temd
      vfs(k)  = vfs(k)*temd
      vfd(k)  = vfd(k)*temd
    enddo
    !$acc end parallel

      !$acc end data

      stop 'momentum_flux: at the end'
      end subroutine momentum_flux


    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine scalar_flux(weps,c1,c2,mf,rf0,s   ,dum3,dum4,dum5,w3d,khv,sflux ,wavg,savg  ,sfr,sfs,sfd,dosfcflx,iflux,pdef,pdefweno)
      use input
      use constants
      use interp_routines
#ifdef MPI
      use mpi
#endif
      implicit none

      double precision, intent(in) :: weps
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: c1,c2,rf0
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: mf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke)   :: s
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke)   :: dum3,dum4,dum5
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: w3d
      real, intent(in), dimension(ibc:iec,jbc:jec,kbc:kec) :: khv
      real, intent(in), dimension(ib:ie,jb:je) :: sflux
      real, intent(in), dimension(0:nk+1) :: wavg,savg
      double precision, intent(inout), dimension(nk+1) :: sfr,sfs,sfd
      logical, intent(in) :: dosfcflx
      integer, intent(in) :: iflux,pdef,pdefweno

      integer :: i,j,k
      real :: wbar
      double precision :: temd
      double precision, dimension(nk) :: sbar
      double precision :: sbart,sfrt,sfst,sfst1,sfst2,sfdt

      !$acc data create(sbar) 

      !$acc parallel default(present) private(i,j,k)
      !$acc loop gang vector
      do k=1,nk+1
        sfr(k) = 0.0
        sfs(k) = 0.0
        sfd(k) = 0.0
      enddo

      ! baseline algorithm:
      !$acc loop gang vector collapse(3)
      do k=2,nk
      do j=1,nj
      do i=1,ni
        dum3(i,j,k) = (c1(i,j,k)*s(i,j,k-1)+c2(i,j,k)*s(i,j,k))
        dum4(i,j,k) = dum3(i,j,k)
      enddo
      enddo
      enddo
      !$acc end parallel


    IF( advwenos.ge.1 )THEN

      if(     weno_order.eq.5 )then
        call vinterp_weno5(  1 ,ni,nj,nk,c1,c2,w3d,dum3,  s ,pdef,pdefweno,weps)
        call vinterp_flx6(   1 ,ni,nj,nk,c1,c2,w3d,dum4,  s )
      endif

    ELSE

        !buh3
      if(     vadvordrs.eq.5 )then
        call vinterp_flx5(   1 ,ni,nj,nk  ,c1,c2,w3d,dum3, s )
        call vinterp_flx6(   1 ,ni,nj,nk  ,c1,c2,w3d,dum4, s )
      elseif( vadvordrs.eq.6 )then
        call vinterp_flx6(   1 ,ni,nj,nk  ,c1,c2,w3d,dum3, s )
        !$acc parallel loop gang vector collapse(3)  default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum4(i,j,k) = dum3(i,j,k)
        enddo
        enddo
        enddo
      endif

    ENDIF


  !----------------------

    !$acc parallel default(present) reduction(+:sbart)
    !$acc loop vector gang 
    do k=1,nk+1
      sbar(k) = 0.0
    enddo

    !$acc loop gang
    do k=2,nk
      sbart=0.d0
      !$acc loop vector collapse(2) reduction(+:sbart)
      do j=1,nj
      do i=1,ni
        sbart = sbart+dum3(i,j,k)
      enddo
      enddo
      sbar(k) = sbart
    enddo
    !$acc end parallel
        !buh4

#ifdef MPI
    !$acc update host(sbar)
    call MPI_ALLREDUCE(MPI_IN_PLACE,sbar(2),(nk-1),MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
    !$acc update device(sbar)
#endif

    temd = 1.0d0/dble(nx*ny)

    !$acc parallel default(present)
    !$acc loop gang vector 
    do k=2,nk
      sbar(k)  = sbar(k)*temd
    enddo
    !$acc end parallel

  !----------------------

    !$acc parallel default(present) private(i,j,k) reduction(+:sfrt,sfdt,sfst)
    !$omp parallel do default(shared) private(i,j,k)
    !$acc loop gang 
    do k=2,nk
      sfrt=0.0d0
      sfdt=0.0d0
      !$acc loop vector collapse(2) reduction(+:sfrt,sfdt)
      do j=1,nj
      do i=1,ni
        sfrt = sfrt + w3d(i,j,k)*(dum4(i,j,k)-sbar(k))
        sfdt = sfdt + w3d(i,j,k)*(dum3(i,j,k)-dum4(i,j,k))
        !buh5
      enddo
      enddo
      sfr(k) = sfrt
      sfd(k) = sfdt
    enddo

    if( cm1setup.ge.1 .or. ipbl.eq.2 .or. cm1setup.eq.3 )then
      !$acc loop gang 
      do k=2,nk
        sfst=0.0d0
        !$acc loop vector collapse(2) reduction(+:sfst)
        do j=1,nj
        do i=1,ni
          sfst = sfst - khv(i,j,k)*(s(i,j,k)-s(i,j,k-1))*rdz*mf(i,j,k)
        enddo
        enddo
        sfs(k) = sfst
      enddo
    else
      !$acc loop gang vector
      do k=2,nk
        sfs(k) = 0.0d0
      enddo
    endif
    !$acc end parallel

      !-----------------------------
      ! 171122:  boundary conditions

      !$acc parallel default(present) private(i,j) reduction(+:sfst,sfst1,sfst2)
      IF( cm1setup.eq.1 .or. cm1setup.eq.2 )THEN
        sfcfl:  &
        IF( iflux.eq.1 .and. dosfcflx )THEN
          sfst = 0.0d0
          !$acc loop gang vector collapse(2) reduction(+:sfst)
          do j=1,nj
          do i=1,ni
            sfst = sfst + sflux(i,j)
          enddo
          enddo

          sfs(1) = sfst

        ELSE  sfcfl

          IF( bcturbs.eq.1 )THEN
            sfs(1) = 0.0
            sfs(nk+1) = 0.0
          ELSEIF( bcturbs.eq.2 )THEN
            sfs(1) = sfs(2)
            sfs(nk+1) = sfs(nk)
          ENDIF

        ENDIF  sfcfl

      ELSEIF( cm1setup.eq.3 )THEN

          if(bc_temp.eq.1)then
            ! specified theta at boundary

            !$acc loop gang vector collapse(2) reduction(-:sfst1,sfst2) 
            do j=1,nj
            do i=1,ni
              sfst1 = sfst1-(viscosity/pr_num)*2.0*(savg(1)-ptc_bot)*rdz*mf(1,1,1)
              sfst2 = sfst2-(viscosity/pr_num)*2.0*(ptc_top-savg(nk))*rdz*mf(1,1,nk+1)
            enddo
            enddo

            sfs(1)=sfst1
            sfs(nk+1)=sfst2

          elseif(bc_temp.eq.2)then
            ! specified flux at boundary

            !$acc loop gang vector collapse(2) reduction(+:sfst1,sfst2) 
            do j=1,nj
            do i=1,ni
              sfst1 = sfst1 + (viscosity/pr_num)*ptc_bot
              sfst2 = sfst2 + (viscosity/pr_num)*ptc_top
            enddo
            enddo

            sfs(1)=sfst1
            sfs(nk+1)=sfst2

          endif

      ENDIF
      !$acc end parallel

#ifdef MPI
      !$acc update host(sfr,sfs,sfd)
      call MPI_ALLREDUCE(MPI_IN_PLACE,sfr,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,sfs,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,sfd,nk+1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      !$acc update device(sfr,sfs,sfd)
#endif

      temd = 1.0d0/dble(nx*ny)

      !$omp parallel do default(shared) private(k)
      !$acc parallel default(present)
      !$acc loop gang vector 
      do k=1,nk+1
        sfr(k)  = sfr(k)*temd
        sfs(k)  = sfs(k)*temd
        sfd(k)  = sfd(k)*temd
      enddo
      !$acc end parallel

      !$acc end data

      end subroutine scalar_flux


    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getwstar(zf,thv0,rth0s,sfr,sfs,sfd,thfavg,qvfavg,wstar,fbmin,zfbmin)
      use input
      use constants
      implicit none

      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: thv0
      real, intent(in), dimension(ib:ie,jb:je) :: rth0s
      double precision, intent(in), dimension(nk+1) :: sfr,sfs,sfd
      double precision, intent(in)    :: thfavg,qvfavg
      double precision, intent(inout) :: wstar,fbmin,zfbmin

      integer :: k
      real :: fb1,fb2

        !$acc parallel default(present) private(k,fb1,fb2) reduction(+:wstar)
        wstar = 0.0
        fbmin = 1.0e30
        zfbmin = 0.0

        !$acc loop vector reduction(+:wstar)
        do k=1,nk
          if( k.eq.1 )then
            fb1 = g*( thfavg*rth0s(1,1) + repsm1*qvfavg )
          else
            fb1 = (sfr(k  )+sfs(k  )+sfd(k  ))*g/(0.5*(thv0(1,1,k)+thv0(1,1,k-1)))
          endif
          if( k.eq.nk )then
            fb2 = 0.0
          else
            fb2 = (sfr(k+1)+sfs(k+1)+sfd(k+1))*g/(0.5*(thv0(1,1,k)+thv0(1,1,k+1)))
          endif

          wstar = wstar + (zf(1,1,k+1)-zf(1,1,k))*0.5*(fb1+fb2)

          if( fb1.lt.fbmin )then
            !$acc atomic write
            fbmin = fb1
            !$acc atomic write
            zfbmin = zf(1,1,k)
          endif

        enddo
        
        wstar = 2.5*(max(0.0d0,wstar)**(1.0/3.0))
        !$acc end parallel

      end subroutine getwstar


    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


    subroutine getavg2d(s2d,savg2d)
    use input
#ifdef MPI
    use mpi
#endif
    implicit none

    real, intent(in), dimension(ib:ie,jb:je) :: s2d
    double precision, intent(inout) :: savg2d
    !$acc declare present(s2d)


    integer :: i,j

    savg2d = 0.0
    !$acc parallel default(present) private(i,j) reduction(+:savg2d)
    !$acc loop gang vector collapse(2) reduction(+:savg2d)
    do j=1,nj
    do i=1,ni
      savg2d = savg2d+s2d(i,j)
    enddo
    enddo
    !$acc end parallel

#ifdef MPI
    call MPI_ALLREDUCE(MPI_IN_PLACE,savg2d,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif

    savg2d = savg2d/dble(nx*ny)

    end subroutine getavg2d



    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



    subroutine getavg3d(s,savg,nkm)
    use input
#ifdef MPI
    use mpi
#endif
    implicit none

    real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: s
    double precision, intent(out), dimension(nk+1) :: savg
    integer, intent(in), optional :: nkm
    !$acc declare present(s,savg,nkm)

    integer :: i,j,k,k2
    real :: stmp
    double precision :: temd
    

!!!    k2 = nk
    k2 = nk+1

    IF( present(nkm) )THEN
      k2 = min(nkm,nk+1)
    ENDIF

    !$acc parallel default(present) reduction(+:stmp)
    !$acc loop 
    do k=1,nk+1
      savg(k) = 0.0
    enddo

    !$acc loop gang
    do k=1,k2
      stmp = 0.0
      !$acc loop vector collapse(2) reduction(+:stmp)
      do j=1,nj
      do i=1,ni
        stmp = stmp+s(i,j,k)
      enddo
      enddo
      savg(k)=stmp
    enddo
    !$acc end parallel

#ifdef MPI
    !$acc update host(savg)
    call MPI_ALLREDUCE(MPI_IN_PLACE,savg(1),k2,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
    !$acc update device(savg)
#endif

    !$acc parallel loop gang vector default(present)
    do k=1,k2
      savg(k) = savg(k)/dble(nx*ny)
    enddo

    end subroutine getavg3d



    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


    subroutine getmax(s,savg,nkm)
    use input
#ifdef MPI
    use mpi
#endif
    implicit none

    real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: s
    double precision, dimension(nk+1) :: savg
    integer, intent(in), optional :: nkm

    integer :: i,j,k,k2
    double precision :: temd
    !$acc declare present(s,savg)


    k2 = nk

    IF( present(nkm) )THEN
      k2 = min(nkm,nk+1)
    ENDIF
    !$acc parallel default(present) private(i,j,k) reduction(max:temd)
    !$acc loop gang vector
    do k=1,nk+1
      savg(k) = -1.0e30
    enddo

    !$acc loop gang
    do k=1,k2
      temd = -1.0e30
      !$acc loop vector collapse(2) reduction(max:temd)
      do j=1,nj
      do i=1,ni
        !savg(k) = max(savg(k),s(i,j,k))
        temd = max(temd,s(i,j,k))
      enddo
      enddo
      savg(k)=temd
    enddo
    !$acc end parallel
#ifdef MPI
    !$acc update host(savg)
    call MPI_ALLREDUCE(MPI_IN_PLACE,savg,k2,MPI_DOUBLE_PRECISION,MPI_MAX,MPI_COMM_WORLD,ierr)
    !$acc update device(savg)
#endif

    end subroutine getmax


    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


    subroutine getmin(s,savg,nkm)
    use input
#ifdef MPI
    use mpi
#endif
    implicit none

    real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: s
    double precision, dimension(nk+1) :: savg
    integer, intent(in), optional :: nkm

    integer :: i,j,k,k2
    double precision :: temd
    !$acc declare present(s,savg)


    k2 = nk

    IF( present(nkm) )THEN
      k2 = min(nkm,nk+1)
    ENDIF
    !$acc parallel default(present) reduction(min:temd)
    !$acc loop gang vector
    do k=1,nk+1
      savg(k) = 1.0e30
    enddo

    !$acc loop gang
    do k=1,k2
      temd = 1.0e30
      !$acc loop vector collapse(2) reduction(min:temd)
      do j=1,nj
      do i=1,ni
        !savg(k) = min(savg(k),s(i,j,k))
        temd = min(temd,s(i,j,k))
      enddo
      enddo
      savg(k) = temd
    enddo
    !$acc end parallel

#ifdef MPI
    !$acc update host(savg)
    call MPI_ALLREDUCE(MPI_IN_PLACE,savg,k2,MPI_DOUBLE_PRECISION,MPI_MIN,MPI_COMM_WORLD,ierr)
    !$acc update device(savg)
#endif

    end subroutine getmin


    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


    subroutine write2d(savg2d,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
    use input
#ifdef NETCDF
    use writeout_nc_module, only : disp_err
    use netcdf
#endif
    implicit none

    double precision, intent(in) :: savg2d
    integer, intent(inout) :: trecs,trecw
    integer, intent(in) :: nvar
    character(len=80), intent(in), dimension(varmax) :: varname,vardesc,varunit
    character(len=1),  dimension(varmax) :: vargrid

    integer :: varid,status
    real :: var
      
  if( output_format.eq.1 .or. ( output_format.eq.2 .and. wloop.eq.2 ) )then

    if( myid.eq.0 )then

      print *,nvar,trim(varname(nvar))

      if( output_format.eq.1 )then
        ! grads-format file:

        ! note:  all 2d vars go into both scalar and w output files

        write(fnums,rec=trecs) sngl(savg2d)
        trecs = trecs+1

        write(fnumw,rec=trecw) sngl(savg2d)
        trecw = trecw+1

#ifdef NETCDF
      elseif( output_format.eq.2 )then

        status = nf90_inq_varid(ncid,trim(varname(nvar)),varid)
        if(status.ne.nf90_noerr)then
          print *,'  Error in write2d, varname = ',varname(nvar)
          print *,nf90_strerror(status)
          call stopcm1
        endif
        var = savg2d
        call disp_err( nf90_put_var(ncid,varid,var) , .true. )

#endif
      endif

    endif

  endif

    end subroutine write2d


    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


    subroutine write3d(savg,trecs,trecw,vargrid,nvar,varname,vardesc,varunit)
    use input
#ifdef NETCDF
    use writeout_nc_module, only : disp_err
    use netcdf
#endif
    implicit none

    double precision, intent(in), dimension(nk+1) :: savg
    integer, intent(inout) :: trecs,trecw
    integer, intent(in) :: nvar
    character(len=80), intent(in), dimension(varmax) :: varname,vardesc,varunit
    character(len=1),  dimension(varmax) :: vargrid

    integer :: k,varid,status,kmax
    real, dimension(1,1,1,1) :: var
      
  !$acc update host(savg)
  if( output_format.eq.1 .or. ( output_format.eq.2 .and. wloop.eq.2 ) )then

    if( myid.eq.0 )then

      print *,nvar,vargrid(nvar),' ',trim(varname(nvar))

      of1:  &
      if( output_format.eq.1 )then
        ! grads-format file:

      if( vargrid(nvar).eq.'s' )then
        do k=1,nk
          write(fnums,rec=trecs) sngl(savg(k))
          trecs = trecs+1
        enddo
      elseif( vargrid(nvar).eq.'w' )then
        do k=1,nk+1
          write(fnumw,rec=trecw) sngl(savg(k))
          trecw = trecw+1
        enddo
      else
        print *,' 67832 '
        call stopcm1
      endif

#ifdef NETCDF
      elseif( output_format.eq.2 )then  of1

        status = nf90_inq_varid(ncid,trim(varname(nvar)),varid)
        if(status.ne.nf90_noerr)then
          print *,'  Error in write3d, varname = ',varname(nvar)
          print *,nf90_strerror(status)
          call stopcm1
        endif

        if( vargrid(nvar).eq.'s' )then
          kmax = nk
        elseif( vargrid(nvar).eq.'w' )then
          kmax = nk+1
        else
          print *,' 67833 '
          call stopcm1
        endif

        do k=1,kmax
          var = savg(k)
          call disp_err( nf90_put_var(ncid,varid,var,(/1,1,k,1/),(/1,1,1,1/)) , .true. )
        enddo

#endif
      endif  of1

    endif

  endif

    end subroutine write3d


    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

    subroutine rtkeb_addu(rtkeb,utk,udiag,udn,uavg)
    use input
    implicit none

    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: rtkeb
    real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: utk
    real, intent(in) , dimension(ibdv:iedv,jbdv:jedv,kbdv:kedv,nudiag) :: udiag
    integer, intent(in) :: udn
    real, dimension(0:nk+1) :: uavg

    integer :: i,j,k
    !$acc declare present(rtkeb,utk,udiag,uavg)

    !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
    do k=1,nk
    do j=1,nj
    do i=1,ni
!!!      rtkeb(i,j,k) = rtkeb(i,j,k) + 0.25*( ( utk(i  ,j,k  )*(udiag(i  ,j,k  ,udn)-savg(k  ))  &
!!!                                            +utk(i+1,j,k  )*(udiag(i+1,j,k  ,udn)-savg(k  )) ) &
!!!                                          +( utk(i  ,j,k-1)*(udiag(i  ,j,k-1,udn)-savg(k-1))  &
!!!                                            +utk(i+1,j,k-1)*(udiag(i+1,j,k-1,udn)-savg(k-1)) ) )
!!!      rtkeb(i,j,k) = rtkeb(i,j,k) + 0.5*( ( utk(i  ,j,k)*(udiag(i  ,j,k,udn)-savg(k))  &
!!!                                           +utk(i+1,j,k)*(udiag(i+1,j,k,udn)-savg(k)) ) )
!!!      rtkeb(i,j,k) = rtkeb(i,j,k) + 0.5*( utk(i  ,j,k)+utk(i+1,j,k) )  &
!!!                                   *0.5*( (udiag(i  ,j,k,udn)-savg(k))+(udiag(i+1,j,k,udn)-savg(k)) )
      rtkeb(i,j,k) = utk(i,j,k)*(udiag(i,j,k,udn)-uavg(k))
    enddo
    enddo
    enddo


    if( myid.eq.0 ) print *,'  rtkeb_addu,udn = ',udn

    end subroutine rtkeb_addu

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

    subroutine rtkeb_addv(rtkeb,vtk,vdiag,vdn,vavg)
    use input
    implicit none

    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: rtkeb
    real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: vtk
    real, intent(in) , dimension(ibdv:iedv,jbdv:jedv,kbdv:kedv,nvdiag) :: vdiag
    integer, intent(in) :: vdn
    real, dimension(0:nk+1) :: vavg

    integer :: i,j,k
    !$acc declare present(rtkeb,vtk,vdiag,vavg)

    !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
    do k=1,nk
    do j=1,nj
    do i=1,ni
!!!      rtkeb(i,j,k) = rtkeb(i,j,k) + 0.25*( ( vtk(i,j  ,k  )*(vdiag(i,j  ,k  ,vdn)-savg(k  ))  &
!!!                                            +vtk(i,j+1,k  )*(vdiag(i,j+1,k  ,vdn)-savg(k  )) ) &
!!!                                          +( vtk(i,j  ,k-1)*(vdiag(i,j  ,k-1,vdn)-savg(k-1))  &
!!!                                            +vtk(i,j+1,k-1)*(vdiag(i,j+1,k-1,vdn)-savg(k-1)) ) )
!!!      rtkeb(i,j,k) = rtkeb(i,j,k) + 0.5*( ( vtk(i,j  ,k)*(vdiag(i,j  ,k,vdn)-savg(k))  &
!!!                                           +vtk(i,j+1,k)*(vdiag(i,j+1,k,vdn)-savg(k)) ) )
!!!      rtkeb(i,j,k) = rtkeb(i,j,k) + 0.5*( vtk(i,j  ,k)+vtk(i,j+1,k) ) &
!!!                                   *0.5*( (vdiag(i,j  ,k,vdn)-savg(k)) + (vdiag(i,j+1,k,vdn)-savg(k)) )
      rtkeb(i,j,k) = vtk(i,j,k)*(vdiag(i,j,k,vdn)-vavg(k))
    enddo
    enddo
    enddo

    if( myid.eq.0 ) print *,'  rtkeb_addv,vdn = ',vdn

    end subroutine rtkeb_addv

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

    subroutine rtkeb_addw(rtkeb,wtk,wdiag,wdn,wavg)
    use input
    implicit none

    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: rtkeb
    real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: wtk
    real, intent(in) , dimension(ibdv:iedv,jbdv:jedv,kbdv:kedv,nwdiag) :: wdiag
    integer, intent(in) :: wdn
    real, dimension(0:nk+1) :: wavg

    integer :: i,j,k
    !$acc declare present(rtkeb,wtk,wdiag,wavg)

    !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
    do k=1,nk
    do j=1,nj
    do i=1,ni
!!!      rtkeb(i,j,k) = rtkeb(i,j,k) + wtk(i,j,k)*(wdiag(i,j,k,wdn)-savg(k))
!!!      rtkeb(i,j,k) = rtkeb(i,j,k) + 0.5*( wtk(i,j,k  )*(wdiag(i,j,k  ,wdn)-savg(k  )) &
!!!                                         +wtk(i,j,k+1)*(wdiag(i,j,k+1,wdn)-savg(k+1)) )
!!!      rtkeb(i,j,k) = rtkeb(i,j,k) + 0.5*( wtk(i,j,k)+wtk(i,j,k+1) )  &
!!!                                   *0.5*( wdiag(i,j,k,wdn)-savg(k) + wdiag(i,j,k+1,wdn)-savg(k+1) )
      rtkeb(i,j,k) = wtk(i,j,k)*(wdiag(i,j,k,wdn)-wavg(k))
    enddo
    enddo
    enddo

    if( myid.eq.0 ) print *,'  rtkeb_addw,wdn = ',wdn

    end subroutine rtkeb_addw

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

    subroutine uwb_term(uwb,utk,wtk,udiag,wdiag,udn,wdn,u_avg,w_avg)
    use input
    implicit none

    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: uwb
    real, intent(in),    dimension(ib:ie+1,jb:je,kb:ke) :: utk
    real, intent(in),    dimension(ib:ie,jb:je,kb:ke+1) :: wtk
    real, intent(in),    dimension(ibdv:iedv,jbdv:jedv,kbdv:kedv,nudiag) :: udiag
    real, intent(in),    dimension(ibdv:iedv,jbdv:jedv,kbdv:kedv,nwdiag) :: wdiag
    integer, intent(in) :: udn,wdn
    real, intent(in), dimension(0:nk+1) :: u_avg,w_avg

    integer :: i,j,k
    !$acc declare present(uwb,utk,wtk,udiag,wdiag,u_avg,w_avg)

    !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
    do k=2,nk
    do j=1,nj
    do i=1,ni
      uwb(i,j,k) = uwb(i,j,k)                                                 &
             +wtk(i,j,k)*0.25*( (udiag(i  ,j,k  ,udn)-u_avg(k  ))             &
                               +(udiag(i+1,j,k  ,udn)-u_avg(k  ))             &
                               +(udiag(i  ,j,k-1,udn)-u_avg(k-1))             &
                               +(udiag(i+1,j,k-1,udn)-u_avg(k-1)) )           &
             +(wdiag(i,j,k,wdn)-w_avg(k))*0.25*( utk(i,j,k  )+utk(i+1,j,k  )  &
                                                +utk(i,j,k-1)+utk(i+1,j,k-1) )
    enddo
    enddo
    enddo

    if( myid.eq.0 ) print *,'  uwb: udn,wdn = ',udn,wdn

    end subroutine uwb_term

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

    subroutine vwb_term(vwb,vtk,wtk,vdiag,wdiag,vdn,wdn,v_avg,w_avg)
    use input
    implicit none

    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: vwb
    real, intent(in),    dimension(ib:ie,jb:je+1,kb:ke) :: vtk
    real, intent(in),    dimension(ib:ie,jb:je,kb:ke+1) :: wtk
    real, intent(in),    dimension(ibdv:iedv,jbdv:jedv,kbdv:kedv,nvdiag) :: vdiag
    real, intent(in),    dimension(ibdv:iedv,jbdv:jedv,kbdv:kedv,nwdiag) :: wdiag
    integer, intent(in) :: vdn,wdn
    real, intent(in), dimension(0:nk+1) :: v_avg,w_avg

    integer :: i,j,k
    !$acc declare present(vwb,vtk,wtk,vdiag,wdiag,v_avg,w_avg)

    !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
    do k=2,nk
    do j=1,nj
    do i=1,ni
      vwb(i,j,k) = vwb(i,j,k)                                                 &
             +wtk(i,j,k)*0.25*( (vdiag(i,j  ,k  ,vdn)-v_avg(k  ))             &
                               +(vdiag(i,j+1,k  ,vdn)-v_avg(k  ))             &
                               +(vdiag(i,j  ,k-1,vdn)-v_avg(k-1))             &
                               +(vdiag(i,j+1,k-1,vdn)-v_avg(k-1)) )           &
             +(wdiag(i,j,k,wdn)-w_avg(k))*0.25*( vtk(i,j,k  )+vtk(i,j+1,k  )  &
                                                +vtk(i,j,k-1)+vtk(i,j+1,k-1) )
    enddo
    enddo
    enddo

    if( myid.eq.0 ) print *,'  vwb: vdn,wdn = ',vdn,wdn

    end subroutine vwb_term

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  END MODULE domaindiag_module
