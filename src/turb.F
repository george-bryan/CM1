  MODULE turb_module

  implicit none

  private
  public :: sfc_and_turb,getepst,getepsd,tkebc,ramp_up_turb

  real :: turbfac,t2pfac
  logical :: ramp_up_turb

  CONTAINS

      subroutine sfc_and_turb(getsfc,getpbl,nstep,dt,dosfcflx,cloudvar,qbudget,    &
                   avgsfcu,avgsfcv,avgsfcs,avgsfct,                  &
                   xh,rxh,arh1,arh2,uh,ruh,xf,rxf,arf1,arf2,uf,ruf,  &
                   yh,vh,rvh,yf,vf,rvf,                              &
                   rds,sigma,rdsf,sigmaf,zh,mh,rmh,c1,c2,zf,mf,rmf,  &
                   pi0s,rth0s,pi0,rho0,prs0,thv0,th0,qv0,rr0,rf0,rrf0, &
                   zs,gz,rgz,gzu,rgzu,gzv,rgzv,gx,gxu,gy,gyv,        &
                   tsk,thflux,qvflux,cd,ch,cq,u1,v1,s1,t1,tlh,f2d,ustt,ut,vt,st,   &
                   dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9,     &
                   divx,rho,rr,rf,prs,                               &
                   t11,t12,t13,t22,t23,t33,                          &
                   m11,m12,m13,m22,m23,m33,                          &
                   u0,rru,ua ,ugr ,fsu  ,                            &
                   v0,rrv,va ,vgr ,fsv  ,                            &
                   rrw,wa ,dumw1,dumw2,                              &
                   ppi ,ppten,sten,sadv,                             &
                   tha ,thten,thten1,thterm,qa ,                     &
                   kmh,kmv,khh,khv,tkea,tke3d,                       &
                   nm,defv,defh,lenscl,dissten,epst,epsd1,epsd2,radsw,radswnet,radlwin, &
                   thpten,qvpten,qcpten,qipten,upten,vpten,qnipten,qncpten,qrpten,qspten,qgpten, &
                   xkzh,xkzq,xkzm,                                   &
                   tsq,qsq,cov,sh3d,el_pbl,qc_bl,qi_bl,cldfra_bl,            &
                   qWT,qSHEAR,qBUOY,qDISS,dqke,qke_adv,qke,qke3d,            &
                   edmf_a,edmf_w,edmf_qt,edmf_thl,edmf_ent,edmf_qc,          &
                   sub_thl3D,sub_sqv3D,det_thl3D,det_sqv3D,                  &
                   vdfg,maxmf,nupdraft,ktop_plume,                           &
                   lwten,swten,tdiag,                                &
                   lu_index,kpbl2d,psfc,u10,v10,s10,hfx,qfx,         &
                   xland,znt,rznt,ust,stau,tst,qst,z0t,z0q,          &
                   hpbl,wspd,phim,phih,psim,psih,psiq,gz1oz0,br,brcr, &
                   CHS,CHS2,CQS2,CPMM,ZOL,MAVAIL,                    &
                   MOL,RMOL,REGIME,LH,FLHC,FLQC,QGH,                 &
                   CK,CKA,CDA,USTM,QSFC,T2,Q2,TH2,EMISS,THC,ALBD,    &
                   gsw,glw,chklowq,capg,snowc,snowh,qcg,dsxy,wstar,delta,prkpp,fm,fh,  &
                   charn,msang,scurx,scury,zkmax,cd_out,ch_out,wscale,wscaleu, &
                   zntmp,mznt,swspd,smois,taux,tauy,hpbl2d,evap2d,heat2d,  &
                   mixht,akhs,akms,ct,snow,sice,thz0,qz0,uz0,vz0,u10e,v10e,th10,q10,tshltr,qshltr,pshltr,z0base,zntmyj,lowlyr,ivgtyp,tke_myj,el_myj,tmp_myj,  &
                   num_soil_layers,slab_zs,slab_dzs,tslb,tmn,        &
                   tml,t0ml,hml,h0ml,huml,hvml,tmoml,                &
                   cavg,uavg,vavg,savg,gamk,gamwall,kmw,u1b,v1b,l2p,s2p,s2b,t2pm1,t2pm2,t2pm3,t2pm4,cmz,csz,ce1z,ce2z,u2pt,v2pt, &
                   bndy,kbdy,                                        &
                   reqs_u,reqs_v,reqs_w,reqs_s,reqs_p,reqs_x,        &
                   nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,                  &
                   pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,                  &
                   vw1,vw2,ve1,ve2,vs1,vs2,vn1,vn2,                  &
                   uw31,uw32,ue31,ue32,us31,us32,un31,un32,          &
                   sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,          &
                   kw1,kw2,ke1,ke2,ks1,ks2,kn1,kn2,reqk,             &
                   iamsat,out2d,out3d,rtime,update_sfc,dotbud,dotdwrite,restarted,  &
                   dowriteout,doazimwrite)
      ! end_sfc_and_turb
      use input
      use constants
      use bc_module
      use comm_module
      use misclibs , only : calcksquick
      use sfcphys_module
      use module_sf_sfclay
      use module_sf_sfclayrev
      use module_sf_slab
      use module_sf_oml
      use module_bl_ysu
      use module_bl_gfsedmf , only : bl_gfsedmf
      use module_sf_gfdl , only : sf_gfdl
      use cm1libs , only : rslf,rsif
      use module_sf_mynn , only : sfclay_mynn
      use module_bl_mynn , only : mynn_bl_driver
      use module_bl_myjpbl , only : myjpbl
      use module_sf_myjsfc , only : myjsfc
      use turbnba_module
      use ib_module
#ifdef MPI
      use mpi
#endif
      implicit none

!-----------------------------------------------------------------------
! Arrays and variables passed into solve

      logical, intent(in) :: getsfc,getpbl
      integer, intent(in) :: nstep
      real, intent(inout) :: dt
      logical, intent(in) :: dosfcflx
      logical, intent(in), dimension(maxq) :: cloudvar
      double precision, intent(inout), dimension(nbudget) :: qbudget
      double precision, intent(inout) :: avgsfcu,avgsfcv,avgsfcs,avgsfct
      real, intent(in), dimension(ib:ie) :: xh,rxh,arh1,arh2,uh,ruh
      real, intent(in), dimension(ib:ie+1) :: xf,rxf,arf1,arf2,uf,ruf
      real, intent(in), dimension(jb:je) :: yh,vh,rvh
      real, intent(in), dimension(jb:je+1) :: yf,vf,rvf
      real, intent(in), dimension(kb:ke) :: rds,sigma
      real, intent(in), dimension(kb:ke+1) :: rdsf,sigmaf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh,mh,rmh,c1,c2
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf,mf,rmf
      real, intent(in), dimension(ib:ie,jb:je) :: pi0s,rth0s
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: pi0,rho0,prs0,thv0,th0,qv0,rr0,rf0,rrf0
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(in), dimension(itb:ite,jtb:jte) :: gz,rgz,gzu,rgzu,gzv,rgzv
      real, intent(in), dimension(itb:ite,jtb:jte,ktb:kte) :: gx,gxu,gy,gyv
      real, intent(inout), dimension(ib:ie,jb:je) :: tsk,znt,rznt,zntmp,ust,stau,tst,qst,z0t,z0q,thflux,qvflux,  &
                                                     cd,ch,cq,u1,v1,s1,t1,psfc,tlh,ustt,ut,vt,st
      real, intent(in),    dimension(ib:ie,jb:je) :: xland,f2d
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9,divx,rho,rr,rf,prs
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: t11,t12,t13,t22,t23,t33
      real, intent(inout), dimension(ibnba:ienba,jbnba:jenba,kbnba:kenba) :: m11,m12,m13,m22,m23,m33
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: u0
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: rru,ua,ugr,fsu
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: v0
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: rrv,va,vgr,fsv
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: rrw,wa,dumw1,dumw2
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: ppi,ppten,sten,sadv
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: tha,thten,thten1,thterm
      real, intent(inout), dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: qa
      real, intent(inout), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv,khh,khv
      real, intent(inout), dimension(ibt:iet,jbt:jet,kbt:ket) :: tkea,tke3d
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: nm,defv,defh,lenscl,dissten,epst,epsd1,epsd2
      real, intent(inout), dimension(ni,nj) :: radsw,radswnet,radlwin
      real, intent(inout), dimension(ibb:ieb,jbb:jeb,kbb:keb) :: thpten,qvpten,qcpten,qipten,upten,vpten,qnipten,qncpten,qrpten,qspten,qgpten
      real, intent(inout), dimension(ibb:ieb,jbb:jeb,kbb:keb) :: xkzh,xkzq,xkzm
      real, intent(inout), dimension(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) :: tsq,qsq,cov,sh3d,el_pbl,qc_bl,qi_bl,cldfra_bl, &
           qWT,qSHEAR,qBUOY,qDISS,dqke,qke_adv,qke,qke3d,edmf_a,edmf_w,edmf_qt,edmf_thl,edmf_ent,edmf_qc,  &
           sub_thl3D,sub_sqv3D,det_thl3D,det_sqv3D
      real, intent(inout), dimension(ibmynn:iemynn,jbmynn:jemynn) :: vdfg,maxmf
      integer, intent(inout), dimension(ibmynn:iemynn,jbmynn:jemynn) :: nupdraft,ktop_plume
      real, intent(inout), dimension(ibr:ier,jbr:jer,kbr:ker) :: swten,lwten
      real, intent(inout) , dimension(ibdt:iedt,jbdt:jedt,kbdt:kedt,ntdiag) :: tdiag
      integer, intent(inout), dimension(ibl:iel,jbl:jel) :: lu_index,kpbl2d
      real, intent(inout), dimension(ibl:iel,jbl:jel) :: u10,v10,s10,hfx,qfx, &
                                      hpbl,wspd,phim,phih,psim,psih,psiq,gz1oz0,br,brcr, &
                                      CHS,CHS2,CQS2,CPMM,ZOL,MAVAIL,          &
                                      MOL,RMOL,REGIME,LH,FLHC,FLQC,QGH,       &
                                      CK,CKA,CDA,USTM,QSFC,T2,Q2,TH2,EMISS,THC,ALBD,   &
                                      gsw,glw,chklowq,capg,snowc,snowh,qcg,dsxy,wstar,delta,prkpp,fm,fh
      real, intent(inout), dimension(ibl:iel,jbl:jel) :: charn,msang,scurx,scury,zkmax,cd_out,ch_out,wscale,wscaleu
      real, intent(inout), dimension(ibl:iel,jbl:jel) :: mznt,swspd,smois,taux,tauy,hpbl2d,evap2d,heat2d
      real, intent(inout), dimension(ibmyj:iemyj,jbmyj:jemyj) :: mixht,akhs,akms,ct,snow,sice,thz0,qz0,uz0,vz0,u10e,v10e,th10,q10,tshltr,qshltr,pshltr,z0base,zntmyj
      integer, intent(inout), dimension(ibmyj:iemyj,jbmyj:jemyj) :: lowlyr,ivgtyp
      real, intent(inout), dimension(ibmyj:iemyj,jbmyj:jemyj,kbmyj:kemyj) :: tke_myj,el_myj
      real, intent(inout), dimension(ibmyj:iemyj,kbmyj:kemyj,nmyj) :: tmp_myj
      integer, intent(in) :: num_soil_layers
      real, intent(in), dimension(num_soil_layers) :: slab_zs,slab_dzs
      real, intent(inout), dimension(ibl:iel,jbl:jel,num_soil_layers) :: tslb
      real, intent(inout), dimension(ibl:iel,jbl:jel) :: tmn,tml,t0ml,hml,h0ml,huml,hvml,tmoml
      double precision, intent(inout), dimension(kb:ke,3+numq) :: cavg
      real, intent(inout), dimension(kb:ke) :: uavg,vavg,savg,gamk,gamwall,kmw,u1b,v1b,l2p,s2p,s2b,t2pm1,t2pm2,t2pm3,t2pm4,cmz,csz,ce1z,ce2z
      real, intent(inout), dimension(ib2pt:ie2pt,jb2pt:je2pt,kb2pt:ke2pt) :: u2pt,v2pt
      integer, intent(inout), dimension(rmp) :: reqs_u,reqs_v,reqs_w,reqs_s,reqs_p,reqs_x
      real, intent(inout), dimension(kmt) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      real, intent(inout), dimension(jmp,kmp) :: pw1,pw2,pe1,pe2
      real, intent(inout), dimension(imp,kmp) :: ps1,ps2,pn1,pn2
      real, intent(inout), dimension(jmp,kmp) :: vw1,vw2,ve1,ve2
      real, intent(inout), dimension(imp,kmp) :: vs1,vs2,vn1,vn2
      real, intent(inout), dimension(cmp,jmp,kmp)   :: uw31,uw32,ue31,ue32
      real, intent(inout), dimension(imp+1,cmp,kmp) :: us31,us32,un31,un32
      real, intent(inout), dimension(cmp,jmp,kmp)   :: sw31,sw32,se31,se32
      real, intent(inout), dimension(imp,cmp,kmp)   :: ss31,ss32,sn31,sn32
      real, intent(inout), dimension(jmp,kmt,4)     :: kw1,kw2,ke1,ke2
      real, intent(inout), dimension(imp,kmt,4)     :: ks1,ks2,kn1,kn2
      integer, intent(inout) :: reqk
      logical, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: iamsat
      real, intent(inout), dimension(ib2d:ie2d,jb2d:je2d,nout2d) :: out2d
      real, intent(inout), dimension(ib3d:ie3d,jb3d:je3d,kb3d:ke3d,nout3d) :: out3d

      logical, intent(in), dimension(ibib:ieib,jbib:jeib,kbib:keib) :: bndy
      integer, intent(in), dimension(ibib:ieib,jbib:jeib) :: kbdy

      real, intent(in) :: rtime
      logical, intent(in) :: update_sfc,dotbud,dotdwrite,restarted,dowriteout,doazimwrite

!-----------------------------------------------------------------------

      integer :: i,j,k,k2,n,kval
      integer :: isfflx,ifsnow,ysu_topdown_pblmix
      real :: ep1,ep2,rovg,dtmin,dz1
      real :: SVP1,SVP2,SVP3,SVPT0,p1000mb,eomeg,stbolt,tem,tem1,tem2,tem3

      logical :: flag_qi
      integer :: p_qi,p_first_scalar
      logical :: disheat,usemyjpbl
      real :: gfs_alpha,var_ric,coef_ric_l,coef_ric_s
      real :: qx
      real :: dtdz
      real :: tmp11,tmp22,tmp33,tmp12,tmp13,tmp23,rcoef

      real, parameter  ::  oml_relaxation_time  =  -1.0

      ! hwrf vars:
      integer :: ntsflg,ens_random_seed,icoef_sf,iwavecpl
      real :: sfenth,ens_Cdamp
      logical :: lcurr_sf,pert_cd
      integer, dimension(ibl:iel,jbl:jel) :: isltyp
      real, dimension(nk) :: zh1d
      real, dimension(nk+1) :: zf1d
      real :: ens_pblamp
      logical :: pert_pbl

      ! mynn:
      real, dimension(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) :: RQNWFABLTEN,RQNIFABLTEN
      logical :: FLAG_QC,FLAG_QNC,FLAG_QNI,FLAG_QNWFA,FLAG_QNIFA

!-----------------------------------------------------------------------

      turbfac = 1.0
      t2pfac = 1.0

    facheck:  &
    IF( cm1setup.eq.1 .and. sgsmodel.ge.1 )THEN


      if( ramp_up_turb )then
        ! ramp up sgs turbulence near beginning of simulation:
        turbfac = (rtime-0.0)/(1800.0)
        turbfac = max(0.0,turbfac)
        turbfac = min(1.0,turbfac)
        if(myid.eq.0.and.turbfac.lt.0.999) print *,'  turbfac = ',turbfac

        ! update turbulence model "constants":
        do k=1,nk+1
          cmz(k) = max( 0.001 , turbfac*c_m )
          ce1z(k) = cmz(k) * c_l * c_l * ( 1.0 / ri_c - 1.0 )
          ce2z(k) = max( 0.0 , cmz(k) * pi * pi - ce1z(k) )
          csz(k) = ( cmz(k) * cmz(k) * cmz(k) / ( ce1z(k) + ce2z(k) ) )**0.25   ! Smagorinsky constant
        enddo
      endif

      if( dot2p )then
        ! ramp up 2-part turbulence model near beginning of simulation:
        t2pfac = (rtime-0.0)/(1800.0)
        t2pfac = max(0.0,t2pfac)
        t2pfac = min(1.0,t2pfac)
        if(myid.eq.0.and.t2pfac.lt.0.999) print *,'  t2pfac = ',t2pfac
      endif

    ENDIF  facheck

      IF( cm1setup.ge.1 .or. ipbl.ge.1 .or. horizturb.eq.1 .or. idiss.eq.1 .or. output_dissten.eq.1 )THEN

        ! cm1r17:  dissten is defined on w (full) levels:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
        do k=1,nk+1
        do j=1,nj
        do i=1,ni
          dissten(i,j,k)=0.0
        enddo
        enddo
        enddo

      ENDIF

      do j=1,nj
      do i=1,ni
        psfc(i,j) = cgs1*prs(i,j,1)+cgs2*prs(i,j,2)+cgs3*prs(i,j,3)
      enddo
      enddo

      IF( imove.eq.1 )THEN
        !$omp parallel do default(shared)   &
        !$omp private(i,j,k)
        do k=1,nk
        do j=jb,je
        do i=ib,ie
          ! get ground-relative winds:
          ugr(i,j,k) = ua(i,j,k)+umove
          vgr(i,j,k) = va(i,j,k)+vmove
        enddo
        enddo
        enddo
      ELSE
        IF( pertflx.eq.1 )THEN
          !$omp parallel do default(shared)   &
          !$omp private(i,j,k)
          do k=1,nk
          do j=jb,je
          do i=ib,ie
            ugr(i,j,k) = ua(i,j,k)-u0(i,j,k)
            vgr(i,j,k) = va(i,j,k)-v0(i,j,k)
          enddo
          enddo
          enddo
        ELSE
          !$omp parallel do default(shared)   &
          !$omp private(i,j,k)
          do k=1,nk
          do j=jb,je
          do i=ib,ie
            ugr(i,j,k) = ua(i,j,k)
            vgr(i,j,k) = va(i,j,k)
          enddo
          enddo
          enddo
        ENDIF
      ENDIF


      IF( (sfcmodel.eq.2) .or. (sfcmodel.eq.3) .or. (sfcmodel.eq.4) .or. (sfcmodel.eq.6) .or. (sfcmodel.eq.7) .or. use_pbl .or. (oceanmodel.eq.2) )THEN

        ! variables for wrf physics:

        if( sfcmodel.eq.6 )then
          k2 = 2
          if( myid.eq.0 .and. nstep.eq.1 ) print *,'  k2 = ',k2
        else
          k2 = 1
        endif

!$omp parallel do default(shared)   &
!$omp private(i,j,k)
        DO j=1,nj
          do k=1,k2
          do i=1,ni
            ! use ground-relative winds:
            dum1(i,j,k)=0.5*(ugr(i,j,k)+ugr(i+1,j,k))
            dum2(i,j,k)=0.5*(vgr(i,j,k)+vgr(i,j+1,k))
            dum3(i,j,k)=th0(i,j,k)+tha(i,j,k)
            dum7(i,j,k)=pi0(i,j,k)+ppi(i,j,k)
            dum4(i,j,k)=dum3(i,j,k)*dum7(i,j,k)
            thten1(i,j,k)=dum7(i,j,k)
          enddo
          enddo
          if( sfcmodel.eq.6 )then
          if( imoist.eq.1 )then
            do k=1,k2
            do i=1,ni
              sten(i,j,k) = qa(i,j,k,nqc)
            enddo
            enddo
          else
            do k=1,k2
            do i=1,ni
              sten(i,j,k) = 0.0
            enddo
            enddo
          endif
          endif
          do k=1,2
          do i=1,ni
            dum5(i,j,k) = dz*rmh(i,j,k)
          enddo
          enddo
          k = 2
          do i=1,ni
            dum6(i,j,k) = c1(i,j,k)*prs(i,j,k-1)+c2(i,j,k)*prs(i,j,k)
          enddo
          ! surface:
          do i=1,ni
            dum6(i,j,1) = psfc(i,j)
          enddo
        ENDDO

        ! dum1 = u at scalars
        ! dum2 = v at scalars
        ! dum3 = th
        ! dum4 = t
        ! dum5 = dz between full levels (m)
        ! dum6 = p3di (p8w)
        ! dum7 = pi

        isfflx = 1
        SVP1=0.6112
        SVP2=17.67
        SVP3=29.65
        SVPT0=273.15
        p1000mb      = 100000.
        EOMEG=7.2921E-5
        STBOLT=5.67051E-8
        ep1 = rv/rd - 1.0
        ep2 = rd/rv
        rovg = rd/g

        ! note:  for RRTMG (radopt=2) gsw and glw arrays are already filled !
        IF( radopt.eq.1 )THEN
!$omp parallel do default(shared)   &
!$omp private(i,j)
          do j=1,nj
          do i=1,ni
            gsw(i,j)=radswnet(i,j)
            glw(i,j)=radlwin(i,j)
          enddo
          enddo
        ELSEIF( radopt.eq.0 )THEN
!$omp parallel do default(shared)   &
!$omp private(i,j)
          do j=1,nj
          do i=1,ni
            gsw(i,j)=0.0
            glw(i,j)=0.0
          enddo
          enddo
        ENDIF

      ENDIF


      !-------------------
      ! now, divx stores qv
      !      dum7 stores ql+qi

      IF(imoist.eq.1)THEN
        !$omp parallel do default(shared)   &
        !$omp private(i,j,k,n)
          DO j=1,nj
            do k=1,nk
            do i=1,ni
              divx(i,j,k) = qa(i,j,k,nqv)
              dum7(i,j,k) = 0.0
            enddo
            enddo
            do n=nql1,nql2
              do i=1,ni
                dum7(i,j,k)=dum7(i,j,k)+qa(i,j,k,n)
              enddo
            enddo
            IF(iice.eq.1)THEN
              do n=nqs1,nqs2
                do i=1,ni
                  dum7(i,j,k)=dum7(i,j,k)+qa(i,j,k,n)
                enddo
              enddo
            ENDIF
          ENDDO
      ELSE
        !$omp parallel do default(shared)   &
        !$omp private(i,j,k,n)
          DO j=1,nj
            do k=1,nk
            do i=1,ni
              divx(i,j,k) = 0.0
              dum7(i,j,k) = 0.0
            enddo
            enddo
          ENDDO
      ENDIF

!-----------------------------------------------------------------------
!  hwrf parameters:

      if( sfcmodel.eq.4 )then
        ! do land sfc temperature prediction if ntsflg=1
        ntsflg = 1
        ! sea spray parameter:
        sfenth = 0.0
        ! randomly perturb Cd:
        pert_cd = .false.
        ens_random_seed = 99
        ens_cdamp = 0.2
        ! Option for exchange coefficients in the surface flux scheme:
        icoef_sf = 6
        ! Option for activate coupling to sea surface wave model:
        iwavecpl = 0
        ! Option to include ocean currents in the surface flux calculations:
        lcurr_sf = .false.
        ! water:
        isltyp = 14
      endif

      if( ipbl.eq.3 )then
        p_qi = nqi
        p_first_scalar = 1
        disheat = .true.
        gfs_alpha = -1.0
        !  Flag for using variable Ric
        var_ric = 1.0
        coef_ric_l = 0.16
        coef_ric_s = 0.25
        ens_pblamp = 0.2
        pert_pbl = .false.
      endif

!-----------------------------------------------------------------------

    if(timestats.ge.1) time_turb=time_turb+mytime()

    dosfc:  IF( getsfc )THEN

      bbc3:  IF( bbc.eq.3 )THEN

        !-------------------------------
        ! u1 is u at k=kval (lowest model level by default)
        ! v1 is v at k=kval
        ! s1 is horizontal wind speed at k=kval
        ! (all defined at the scalar point of the staggered grid)
        ! for pertflx=1, account for domain (i.e., surface) motion 
        !                in calculation of wind speed

        kval = 1

        !$omp parallel do default(shared)   &
        !$omp private(i,j)
          do j=1,nj
          do i=1,ni
            ! use ground-relative winds:
            u1(i,j) = 0.5*( ugr(i,j,kval) + ugr(i+1,j,kval) )
            v1(i,j) = 0.5*( vgr(i,j,kval) + vgr(i,j+1,kval) )
            s1(i,j) = sqrt(u1(i,j)**2+v1(i,j)**2)
            t1(i,j) = th0(i,j,kval)+tha(i,j,kval)
            ppten(i,j,1) = zh(i,j,kval)
          enddo
          enddo

          IF( terrain_flag )THEN
            !$omp parallel do default(shared)   &
            !$omp private(i,j)
            do j=1,nj
            do i=1,ni
              ppten(i,j,1) = zh(i,j,kval)-zs(i,j)
            enddo
            enddo
          ENDIF

        !-------------------------------
        ! NOTE:
        ! divx stores qv
        ! dum7 stores ql+qi
        ! ppten(k=1) stores height of first model level above surface

        IF( testcase.ge.1 .and. testcase.le.7 )THEN

          ! max gradient method (for simple LES simulations only):

          !$omp parallel do default(shared)   &
          !$omp private(i,j,k,dtdz)
          DO j=1,nj

            do i=1,ni
              dum8(i,j,1) = -1.0e30
            enddo

            do k=nk,2,-1
            do i=1,ni
              dtdz = ( (th0(i,j,k  )+tha(i,j,k  ))*(1.0+repsm1*divx(i,j,k  )-dum7(i,j,k  ))  &
                      -(th0(i,j,k-1)+tha(i,j,k-1))*(1.0+repsm1*divx(i,j,k-1)-dum7(i,j,k-1)) )*rdz*mf(i,j,k)
              if( dtdz .ge. dum8(i,j,1) )then
                dum8(i,j,1) = dtdz
                hpbl(i,j) = zf(i,j,k)
              endif
            enddo
            enddo

          ENDDO

        ELSE

          ! bulk-Ri method:

          IF( ipbl.eq.2 )THEN
            ! (note: for ipbl=1,3,4,5 pbl depth is calculated within the PBL subroutine)
            call gethpbl2(psfc,qsfc,thflux,qvflux,ust,tsk,zh,th0,tha,divx,ugr,vgr,dum8(ib,jb,1),dum8(ib,jb,2),dum8(ib,jb,3),dum8(ib,jb,4),hpbl,thten)
          ENDIF

        ENDIF


        IF( terrain_flag )THEN
          !$omp parallel do default(shared)   &
          !$omp private(i,j)
          do j=1,nj
          do i=1,ni
            hpbl(i,j) = hpbl(i,j)-zs(i,j)
          enddo
          enddo
         ENDIF

        IF( use_avg_sfc .and. ( sfcmodel.eq.1 .or. sfcmodel.eq.5 ) )THEN
            call getavgsfc(u1,v1,s1,t1,avgsfcu,avgsfcv,avgsfcs,avgsfct)
        ENDIF

        IF( sfcmodel.eq.1 )THEN

          call getcecd(xh,u1,v1,s1,ppten(ib,jb,1),u10,v10,s10,xland,znt,ust,cd,ch,cq,avgsfcu,avgsfcv,avgsfcs,avgsfct)

          if( tbc.eq.3 )then
            do j=1,nj
            do i=1,ni
              ut(i,j) = 0.5*( ugr(i,j,nk) + ugr(i+1,j,nk) )
              vt(i,j) = 0.5*( vgr(i,j,nk) + vgr(i,j+1,nk) )
              st(i,j) = sqrt(ut(i,j)**2+vt(i,j)**2)
              ustt(i,j) = max( st(i,j)*karman/alog((zf(i,j,nk+1)-zh(i,j,nk))/cnst_znt) , 1.0e-6 )
            enddo
            enddo
          endif

          if(isfcflx.eq.1)then
            call sfcflux(dt,ruh,xf,rvh,pi0s,ch,cq,pi0,thv0,th0,rf0,tsk,thflux,qvflux,mavail, &
                         rho,rf,u1,v1,s1,ppi,tha,divx, &
                         qbudget(8),psfc,u10,v10,s10,qsfc,znt,rtime)
          endif

          ! get sfc diagnostics needed by pbl scheme:
          call sfcdiags(tsk,thflux,qvflux,cd,ch,cq,u1,v1,s1,wspd,        &
                        xland,psfc,qsfc,u10,v10,hfx,qfx,cda,znt,gz1oz0,  &
                        psim,psih,br,zol,mol,hpbl,dsxy,th2,t2,q2,fm,fh,  &
                        zs,ppten(ib,jb,1),pi0s,pi0,th0,ppi,tha,rho,rf,divx)
          if( use_pbl )then
            !$omp parallel do default(shared)   &
            !$omp private(i,j)
            do j=1,nj
            do i=1,ni
              CPMM(i,j)=CP*(1.0+0.8*divx(i,j,1))                                   
              hfx(i,j) = thflux(i,j)*CPMM(i,j)*rf(i,j,1)
              qfx(i,j) = qvflux(i,j)*rf(i,j,1)
            enddo
            enddo
          endif

        ENDIF

        IF( sfcmodel.eq.5 )THEN

          call   cm1most(u1,v1,s1,t1,tst,thflux,zol,mol,rmol,       &
                         phim,phih,psim,psih,ppten(ib,jb,1),        &
                         u10,v10,s10,xland,znt,rznt,ust,cd,ch,cq,   &
                         avgsfcu,avgsfcv,avgsfcs,avgsfct,rtime,     &
                         tsk,psfc,wspd,th0,tha)

        ENDIF


        IF( (sfcmodel.eq.2) .or. (sfcmodel.eq.3) .or. (sfcmodel.eq.4) .or. (sfcmodel.eq.6) .or. (sfcmodel.eq.7) )THEN
          ! surface layer:
        if( sfcmodel.eq.2 )then
          call SFCLAY(dum1,dum2,dum4,divx,prs,dum5,      &
                       CP,G,ROVCP,RD,XLV,lv1,lv2,PSFC,CHS,CHS2,CQS2,CPMM, &
                       ZNT,UST,hpbl,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                       FM,FH,                                        &
                       XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                       U10,V10,TH2,T2,Q2,rf(ib,jb,1),                &
                       GZ1OZ0,WSPD,BR,ISFFLX,dsxy,                   &
                       SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                       KARMAN,EOMEG,STBOLT,                          &
                       P1000mb,                                      &
                       1  ,ni+1 , 1  ,nj+1 , 1  ,nk+1 ,              &
                       ib ,ie , jb ,je , kb ,ke ,                    &
                       1  ,ni , 1  ,nj , 1  ,nk ,                    &
                       ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,z0t,z0q   )
        elseif( sfcmodel.eq.3 )then
          call SFCLAYREV(dum1,dum2,dum4,divx,prs,dum5,   &
                       CP,G,ROVCP,RD,XLV,lv1,lv2,PSFC,CHS,CHS2,CQS2,CPMM, &
                       ZNT,UST,hpbl,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
                       FM,FH,                                        &
                       XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,RMOL, &
                       U10,V10,TH2,T2,Q2,rf(ib,jb,1),                &
                       GZ1OZ0,WSPD,BR,ISFFLX,dsxy,                   &
                       SVP1,SVP2,SVP3,SVPT0,EP1,EP2,                 &
                       KARMAN,EOMEG,STBOLT,                          &
                       P1000mb,                                      &
                       1  ,ni+1 , 1  ,nj+1 , 1  ,nk+1 ,              &
                       ib ,ie , jb ,je , kb ,ke ,                    &
                       1  ,ni , 1  ,nj , 1  ,nk ,                    &
                       ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,tst,qst,psiq,z0t,z0q)
        elseif( sfcmodel.eq.4 )then
          cd_out = 0.0
          ch_out = 0.0
          mol = 0.0
          zol = 0.0
          out2d = 0.0
          do j=1,nj
          do i=1,ni
            zkmax(i,j) = zh(i,j,1)
          enddo
          enddo
          call SF_GFDL(U3D=dum1,V3D=dum2,T3D=dum4,QV3D=divx,P3D=prs,                           &
                     CP=cp,ROVCP=rovcp,R=rd,XLV=xlv,PSFC=psfc,                                 &
                     CHS=chs,CHS2=chs2,CQS2=cqs2, CPM=cpmm,                                    &
                     DT=dt, SMOIS=smois,num_soil_layers=1,ISLTYP=ISLTYP,ZNT=znt,               &
                     MZNT=mznt,                                                                &
                     UST=ust,PSIM=psim,PSIH=psih,                                              &
                     XLAND=xland,HFX=hfx,QFX=qfx,TAUX=taux,TAUY=tauy,LH=lh,GSW=gsw,GLW=glw,    &
                     TSK=tsk,FLHC=flhc,FLQC=flqc,                                              &
                     QGH=qgh,QSFC=qsfc,U10=u10,V10=v10,                                        &
                     ICOEF_SF=icoef_sf,IWAVECPL=iwavecpl,LCURR_SF=lcurr_sf,                    &
                     CHARN=charn,MSANG=msang,SCURX=scurx, SCURY=scury,                         &
                     pert_Cd=pert_cd, ens_random_seed=ens_random_seed, ens_Cdamp=ens_cdamp,    &
                     GZ1OZ0=gz1oz0,WSPD=swspd,BR=br,ZKMAX=zkmax, ISFFLX=isfflx,                &
                     EP1=ep1,EP2=ep2,KARMAN=karman,NTSFLG=ntsflg,SFENTH=sfenth,                &
                     Cd_out=cd_out,Ch_out=ch_out,mol_out=mol,zol_out=zol,z0t_out=z0t,          &
                     fm_out=fm,fh_out=fh,                                                      &
                  ids=1  ,ide=ni+1 , jds= 1 ,jde=nj+1 , kds=1  ,kde=nk+1 ,                     &
                  ims=ib ,ime=ie   , jms=jb ,jme=je   , kms=kb ,kme=ke ,                       &
                  its=1  ,ite=ni   , jts=1  ,jte=nj   , kts=1  ,kte=nk  )
          do j=1,nj
          do i=1,ni
            ust(i,j) = max(ust(i,j),1.0e-8)
            wspd(i,j) = swspd(i,j)
          enddo
          enddo
        elseif( sfcmodel.eq.6 )then
          call SFCLAY_mynn(                                                                                     &
                     U3D=dum1,V3D=dum2,T3D=dum4,QV3D=divx,P3D=prs,dz8w=dum5,                                    &
                     CP=cp,G=g,ROVCP=rovcp,R=rd,XLV=xlv,PSFCPA=psfc,CHS=chs,CHS2=chs2,CQS2=cqs2,CPM=cpmm,       &
                     ZNT=znt,UST=ust,PBLH=hpbl,MAVAIL=mavail,ZOL=zol,MOL=mol,REGIME=regime,PSIM=psim,PSIH=psih, &
                     XLAND=xland,HFX=hfx,QFX=qfx,LH=lh,TSK=tsk,FLHC=flhc,FLQC=flqc,QGH=qgh,QSFC=qsfc,RMOL=rmol, &
                     U10=u10,V10=v10,TH2=th2,T2=t2,Q2=q2,SNOWH=snowh,                                           &
                     GZ1OZ0=gz1oz0,WSPD=wspd,BR=br,ISFFLX=isfflx,DX=dx,                                         &
                     SVP1=svp1,SVP2=svp2,SVP3=svp3,SVPT0=svpt0,EP1=ep1,EP2=ep2,                                 &
                     KARMAN=karman,itimestep=nstep,ch=ch,th3d=dum3,pi3d=thten1,qc3d=sten,rho3d=rho,qcg=qcg,     &
                     spp_pbl=spp_pbl,                                                                           &
                     ids=1  ,ide=ni+1 , jds= 1 ,jde=nj+1 , kds=1  ,kde=nk+1 ,                                   &
                     ims=ib ,ime=ie   , jms=jb ,jme=je   , kms=kb ,kme=ke ,                                     &
                     its=1  ,ite=ni   , jts=1  ,jte=nj   , kts=1  ,kte=nk  ,                                    &
                     ustm=ustm,ck=ck,cka=cka,cd=cd,cda=cda,isftcflx=isftcflx,iz0tlnd=iz0tlnd)

        elseif( sfcmodel.eq.7 )then
          ! 7 = MYJ sfclayer

          if( ipbl.eq.6 )then
            usemyjpbl = .true.
          else
            usemyjpbl = .false.
          endif

        do j=1,nj

          do k=1,nk
          do i=1,ni
            tmp_myj(i,k, 1) = dz*rmh(i,j,k)  ! dz
            tmp_myj(i,k, 2) = prs(i,j,k)     ! pmid
            tmp_myj(i,k, 4) = (th0(i,j,k)+tha(i,j,k))  ! th
            tmp_myj(i,k, 5) = (th0(i,j,k)+tha(i,j,k))*(pi0(i,j,k)+ppi(i,j,k))  ! t
            tmp_myj(i,k, 6) = 0.0
            tmp_myj(i,k, 7) = 0.0
            tmp_myj(i,k, 8) = 0.5*(ugr(i,j,k)+ugr(i+1,j,k))  ! u
            tmp_myj(i,k, 9) = 0.5*(vgr(i,j,k)+vgr(i,j+1,k))  ! v
            tmp_myj(i,k,10) = tke_myj(i,j,k)  ! tke_myj
          enddo
          enddo
          ! pint:
          do k=2,nk
            tmp_myj(i,k,3) = c1(i,j,k)*prs(i,j,k-1)+c2(i,j,k)*prs(i,j,k)
          enddo
          do i=1,ni
            tmp_myj(i,1,3) = psfc(i,j)
            tmp_myj(i,nk+1,3) = cgt1*prs(i,j,nk)+cgt2*prs(i,j,nk-1)+cgt3*prs(i,j,nk-2)
          enddo
          if( imoist.eq.1 .and. nqv.ge.1 )then
            do k=1,nk
            do i=1,ni
              tmp_myj(i,k, 6) = qa(i,j,k,nqv)
            enddo
            enddo
          endif
          if( imoist.eq.1 .and. nqc.ge.1 )then
            do k=1,nk
            do i=1,ni
              tmp_myj(i,k, 7) = qa(i,j,k,nqc)
            enddo
            enddo
          endif

          call   MYJSFC(ITIMESTEP=nstep,  &
                        HT=zs(ib,j),  &
                        DZ=tmp_myj(ibmyj,kbmyj,1),                               &
                        PMID=tmp_myj(ibmyj,kbmyj,2),  &
                        PINT=tmp_myj(ibmyj,kbmyj,3),  &
                        TH=tmp_myj(ibmyj,kbmyj,4),  &
                        T=tmp_myj(ibmyj,kbmyj,5),  &
                        QV=tmp_myj(ibmyj,kbmyj,6),  &
                        QC=tmp_myj(ibmyj,kbmyj,7),  &
                        U=tmp_myj(ibmyj,kbmyj,8),  &
                        V=tmp_myj(ibmyj,kbmyj,9),  &
                        Q2=tmp_myj(ibmyj,kbmyj,10)                    &
                       ,TSK=tsk(ib,j),QSFC=qsfc(ib,j),THZ0=thz0(ib,j),QZ0=qz0(ib,j),UZ0=uz0(ib,j),VZ0=vz0(ib,j)                      &
                       ,LOWLYR=lowlyr(ib,j),XLAND=xland(ib,j),IVGTYP=ivgtyp(ib,j),ISURBAN=0,IZ0TLND=iz0tlnd            &
                       ,USTAR=ust(ib,j),ZNT=zntmyj(ib,j),Z0BASE=z0base(ib,j),PBLH=hpbl(ib,j),MAVAIL=mavail(ib,j),RMOL=rmol(ib,j)              &
                       ,AKHS=akhs(ib,j),AKMS=akms(ib,j)                                      &
                       ,RIB=br(ib,j)                                             &
                       ,CHS=chs(ib,j),CHS2=chs2(ib,j),CQS2=cqs2(ib,j),HFX=hfx(ib,j),QFX=qfx(ib,j),FLX_LH=lh(ib,j),FLHC=flhc(ib,j),FLQC=flqc(ib,j)         &
                       ,QGH=qgh(ib,j),CPM=cpmm(ib,j),CT=ct(ib,j)                                     &
                       ,U10=u10(ib,j),V10=v10(ib,j),T02=t2(ib,j),TH02=th2(ib,j),TSHLTR=tshltr(ib,j),TH10=th10(ib,j),Q02=q2(ib,j),QSHLTR=qshltr(ib,j),Q10=q10(ib,j),PSHLTR=pshltr(ib,j)          &
                       ,P1000mb=p1000mb,U10E=u10e(ib,j),V10E=v10e(ib,j),                             &
                        psim=psim(ib,j),psih=psih(ib,j),fm=fm(ib,j),fhh=fh(ib,j),z0out=znt(ib,j),usemyjpbl=usemyjpbl,rhosfc=rf(ib,j,1),isftcflx=isftcflx,     &
                     ids=1  ,ide=ni+1 , jds= 1 ,jde= 1   , kds=1  ,kde=nk+1 ,                                   &
                     ims=ib ,ime=ie   , jms= 1 ,jme= 1   , kms=kb ,kme=ke ,                                     &
                     its=1  ,ite=ni   , jts= 1 ,jte= 1   , kts=1  ,kte=nk  )
          do i=1,ni
            br(i,j) = max( -10.0 , br(i,j) )
            br(i,j) = min(  10.0 , br(i,j) )
            gz1oz0(I,J)=ALOG(ppten(i,j,1)/znt(i,j))
          enddo
        enddo
        endif


          ifsnow = 0
          dtmin = dt/60.0

        IF( update_sfc )THEN
          ! slab scheme (MM5/WRF):
          call SLAB(dum4,divx,prs,FLHC,FLQC,                        &
                       PSFC,XLAND,TMN,HFX,QFX,LH,TSK,QSFC,CHKLOWQ,  &
                       GSW,GLW,CAPG,THC,SNOWC,EMISS,MAVAIL,         &
                       DT,ROVCP,XLV,lv1,lv2,DTMIN,IFSNOW,           &
                       SVP1,SVP2,SVP3,SVPT0,EP2,                    &
                       KARMAN,EOMEG,STBOLT,                         &
                       TSLB,slab_ZS,slab_DZS,num_soil_layers, .true. ,       &
                       P1000mb,                                     &
                         1, ni+1,   1, nj+1,   1, nk+1,             &
                        ib, ie,  jb, je,  kb, ke,                   &
                         1, ni,   1, nj,   1, nk                    )
        ELSE
        if( sfcmodel.ne.7 )then
          ! dont update tsk, but diagnose qsfc:
          !  ?  ... should this be land pts only? !
          do j=1,nj
          do i=1,ni
            qx = divx(i,j,1)
            if ( FLQC(i,j) .ne. 0.) then
               QSFC(i,j)=QX+QFX(i,j)/FLQC(i,j)
            else
               QSFC(i,j) = QX
            end if
            CHKLOWQ(i,j)=MAVAIL(i,j)
          enddo
          enddo
        endif
        ENDIF

          ! put WRF parameters into CM1 arrays:
          if( sfcmodel.eq.2 .or. sfcmodel.eq.3 .or. sfcmodel.eq.6 .or. sfcmodel.eq.7 )then
            !$omp parallel do default(shared)   &
            !$omp private(i,j)
            do j=1,nj
            do i=1,ni
              ch(i,j) = chs2(i,j)
              cq(i,j) = cqs2(i,j)
              s10(i,j) = sqrt( u10(i,j)**2 + v10(i,j)**2 )
            enddo
            enddo
          endif
          if( sfcmodel.eq.4 )then
            do j=1,nj
            do i=1,ni
              cd(i,j) = cd_out(i,j)
              ch(i,j) = ch_out(i,j)
              cq(i,j) = ch_out(i,j)
              s10(i,j) = sqrt( u10(i,j)**2 + v10(i,j)**2 )
              if( abs(mol(i,j)).le.smeps )then
                rmol(i,j) = sign( 1.0e20 , mol(i,j) )
              else
                rmol(i,j) = 1.0/mol(i,j)
              endif
            enddo
            enddo
          endif
          if( sfcmodel.eq.6 )then
            !$omp parallel do default(shared)   &
            !$omp private(i,j)
            do j=1,nj
            do i=1,ni
              ch(i,j) = ck(i,j)
              cq(i,j) = ck(i,j)
            enddo
            enddo
          endif
          if( sfcmodel.eq.7 )then
            !$omp parallel do default(shared)   &
            !$omp private(i,j)
            do j=1,nj
            do i=1,ni
              ustm(i,j) = ust(i,j)
              ! from wrf surface_driver:
              wspd(i,j) = MAX(SQRT(dum1(i,j,1)**2+dum2(i,j,1)**2),0.001)
!!!              ch(i,j) = chs(i,j)
!!!              cq(i,j) = akhs(i,j)
!!!              cd(i,j) = akms(i,j)
              if( abs(rmol(i,j)).le.smeps )then
                mol(i,j) = sign( 1.0e20 , rmol(i,j) )
              else
                mol(i,j) = 1.0/rmol(i,j)
              endif
              ! wspd:
              tem = max( 0.1 , ust(i,j)*rkarman*fm(i,j) )
              wspd(i,j) = max( wspd(i,j) , tem )
!!!              cd(i,j) = ust(i,j)*ust(i,j)/max(1.0e-20,tem*tem)
              cd(i,j) = ust(i,j)*ust(i,j)/max(1.0e-20,s10(i,j)*s10(i,j))
            enddo
            enddo
          endif
          IF( dosfcflx .or. output_sfcflx.eq.1 )THEN
            !$omp parallel do default(shared)   &
            !$omp private(i,j)
            do j=1,nj
            do i=1,ni
              thflux(i,j) = hfx(i,j)/(CPMM(i,j)*rf(i,j,1))
              qvflux(i,j) = qfx(i,j)/rf(i,j,1)
            enddo
            enddo
          ENDIF

        ENDIF

      ENDIF  bbc3
      if(timestats.ge.1) time_sfcphys=time_sfcphys+mytime()

    ELSE

      if(dowr) write(outfile,*)
      if(dowr) write(outfile,*) '  ... skipping sfc stuff ... '
      if(dowr) write(outfile,*)

    ENDIF  dosfc

!---------------------------------------------
! bc/comms (very important):

  bbc3b:  &
  IF( bbc.eq.3 )THEN
    !-------------!
    call bc2d(ust)
#ifdef MPI
    call comm_1s2d_start(ust,uw31(1,1,1),uw32(1,1,1),ue31(1,1,1),ue32(1,1,1),  &
                             us31(1,1,1),us32(1,1,1),un31(1,1,1),un32(1,1,1),reqs_s)
#endif
    call bc2d(u1)
#ifdef MPI
    call comm_1s2d_start(u1 ,uw31(1,1,2),uw32(1,1,2),ue31(1,1,2),ue32(1,1,2),  &
                             us31(1,1,2),us32(1,1,2),un31(1,1,2),un32(1,1,2),reqs_u)
#endif
    call bc2d(v1)
#ifdef MPI
    call comm_1s2d_start(v1 ,uw31(1,1,3),uw32(1,1,3),ue31(1,1,3),ue32(1,1,3),  &
                             us31(1,1,3),us32(1,1,3),un31(1,1,3),un32(1,1,3),reqs_v)
#endif
    call bc2d(s1)
#ifdef MPI
    call comm_1s2d_start(s1 ,uw31(1,1,4),uw32(1,1,4),ue31(1,1,4),ue32(1,1,4),  &
                             us31(1,1,4),us32(1,1,4),un31(1,1,4),un32(1,1,4),reqs_w)
#endif
    call bc2d(znt)
#ifdef MPI
    call comm_1s2d_start(znt,uw31(1,1,5),uw32(1,1,5),ue31(1,1,5),ue32(1,1,5),  &
                             us31(1,1,5),us32(1,1,5),un31(1,1,5),un32(1,1,5),reqs_p)
#endif
  if( sfcmodel.eq.4 )then
    call bc2d(mznt)
#ifdef MPI
    call comm_1s2d_start(mznt,uw31(1,1,6),uw32(1,1,6),ue31(1,1,6),ue32(1,1,6),  &
                              us31(1,1,6),us32(1,1,6),un31(1,1,6),un32(1,1,6),reqs_x)
#endif
  endif
    !-------------!

#ifdef MPI
    !-------------!
    call comm_1s2d_end(ust,uw31(1,1,1),uw32(1,1,1),ue31(1,1,1),ue32(1,1,1),  &
                           us31(1,1,1),us32(1,1,1),un31(1,1,1),un32(1,1,1),reqs_s)
    call bcs2_2d(ust)

    call comm_1s2d_end(u1 ,uw31(1,1,2),uw32(1,1,2),ue31(1,1,2),ue32(1,1,2),  &
                           us31(1,1,2),us32(1,1,2),un31(1,1,2),un32(1,1,2),reqs_u)
    call bcs2_2d(u1 )

    call comm_1s2d_end(v1 ,uw31(1,1,3),uw32(1,1,3),ue31(1,1,3),ue32(1,1,3),  &
                           us31(1,1,3),us32(1,1,3),un31(1,1,3),un32(1,1,3),reqs_v)
    call bcs2_2d(v1 )

    call comm_1s2d_end(s1 ,uw31(1,1,4),uw32(1,1,4),ue31(1,1,4),ue32(1,1,4),  &
                           us31(1,1,4),us32(1,1,4),un31(1,1,4),un32(1,1,4),reqs_w)
    call bcs2_2d(s1 )

    call comm_1s2d_end(znt,uw31(1,1,5),uw32(1,1,5),ue31(1,1,5),ue32(1,1,5),  &
                           us31(1,1,5),us32(1,1,5),un31(1,1,5),un32(1,1,5),reqs_p)
    call bcs2_2d(znt)

  if( sfcmodel.eq.4 )then
    call comm_1s2d_end(mznt,uw31(1,1,6),uw32(1,1,6),ue31(1,1,6),ue32(1,1,6),  &
                            us31(1,1,6),us32(1,1,6),un31(1,1,6),un32(1,1,6),reqs_x)
    call bcs2_2d(mznt)
  endif
    !-------------!

    !-------------!
    call comm_2d_corner(ust)
    call comm_2d_corner(u1)
    call comm_2d_corner(v1)
    call comm_2d_corner(s1)
    call comm_2d_corner(znt)
  if( sfcmodel.eq.4 )then
    call comm_2d_corner(mznt)
  endif
    !-------------!
#endif

    ! account for annoying GFDL naming convention:
    IF( sfcmodel.eq.4 )THEN
      do j=0,nj+1
      do i=0,ni+1
        zntmp(i,j) = mznt(i,j)
      enddo
      enddo
    ELSE
      do j=0,nj+1
      do i=0,ni+1
        zntmp(i,j) = znt(i,j)
      enddo
      enddo
    ENDIF
    if(timestats.ge.1) time_sfcphys=time_sfcphys+mytime()

  ENDIF  bbc3b

!-------------------------------------------------------------------

  tbc3b:  &
  IF( tbc.eq.3 )THEN
    !-------------!
    call bc2d(ustt)
#ifdef MPI
    call comm_1s2d_start(ustt,uw31(1,1,1),uw32(1,1,1),ue31(1,1,1),ue32(1,1,1),  &
                              us31(1,1,1),us32(1,1,1),un31(1,1,1),un32(1,1,1),reqs_s)
#endif
    call bc2d(ut)
#ifdef MPI
    call comm_1s2d_start(ut ,uw31(1,1,2),uw32(1,1,2),ue31(1,1,2),ue32(1,1,2),  &
                             us31(1,1,2),us32(1,1,2),un31(1,1,2),un32(1,1,2),reqs_u)
#endif
    call bc2d(vt)
#ifdef MPI
    call comm_1s2d_start(vt ,uw31(1,1,3),uw32(1,1,3),ue31(1,1,3),ue32(1,1,3),  &
                             us31(1,1,3),us32(1,1,3),un31(1,1,3),un32(1,1,3),reqs_v)
#endif
    call bc2d(st)
#ifdef MPI
    call comm_1s2d_start(st ,uw31(1,1,4),uw32(1,1,4),ue31(1,1,4),ue32(1,1,4),  &
                             us31(1,1,4),us32(1,1,4),un31(1,1,4),un32(1,1,4),reqs_w)
#endif
    !-------------!

#ifdef MPI
    !-------------!
    call comm_1s2d_end(ustt,uw31(1,1,1),uw32(1,1,1),ue31(1,1,1),ue32(1,1,1),  &
                            us31(1,1,1),us32(1,1,1),un31(1,1,1),un32(1,1,1),reqs_s)
    call bcs2_2d(ustt)

    call comm_1s2d_end(ut ,uw31(1,1,2),uw32(1,1,2),ue31(1,1,2),ue32(1,1,2),  &
                           us31(1,1,2),us32(1,1,2),un31(1,1,2),un32(1,1,2),reqs_u)
    call bcs2_2d(ut )

    call comm_1s2d_end(vt ,uw31(1,1,3),uw32(1,1,3),ue31(1,1,3),ue32(1,1,3),  &
                           us31(1,1,3),us32(1,1,3),un31(1,1,3),un32(1,1,3),reqs_v)
    call bcs2_2d(vt )

    call comm_1s2d_end(st ,uw31(1,1,4),uw32(1,1,4),ue31(1,1,4),ue32(1,1,4),  &
                           us31(1,1,4),us32(1,1,4),un31(1,1,4),un32(1,1,4),reqs_w)
    call bcs2_2d(st )

    !-------------!

    !-------------!
    call comm_2d_corner(ustt)
    call comm_2d_corner(ut)
    call comm_2d_corner(vt)
    call comm_2d_corner(st)
    !-------------!
#endif

  ENDIF  tbc3b

!-------------------------------------------------------------------
! simple ocean mixed layer model based Pollard, Rhines and Thompson (1973)
!   (from WRF)

    IF(oceanmodel.eq.2)THEN
    IF( update_sfc )THEN
      if( getsfc )then

        CALL oceanml(tml,t0ml,hml,h0ml,huml,hvml,ust,dum1,dum2, &
                     tmoml,f2d,g,oml_gamma,                     &
                    OML_RELAXATION_TIME,                        &
                     xland,hfx,lh,tsk,gsw,glw,emiss,            &
                     dt,STBOLT,                                 &
                       1, ni+1,   1, nj+1,   1, nk+1,           &
                      ib, ie,  jb, je,  kb, ke,                 &
                       1, ni,   1, nj,   1, nk                  )

        if(timestats.ge.1) time_sfcphys=time_sfcphys+mytime()

      endif
    ENDIF
    ENDIF

!---------------------------------------------

      IF( sgsmodel.ge.1 .or. output_nm.eq.1 .or. ipbl.ge.1 )THEN
        ! squared Brunt-Vaisala frequency:
        iamsat = .false.
        call calcnm(c1,c2,mf,pi0,thv0,th0,cloudvar,nm,dum1,dum2,dum3,dum4,dum5,dum6,   &
                    prs,ppi,tha,qa,iamsat)
      ENDIF

      IF( cm1setup.ge.1 .or. output_def.eq.1 .or. ipbl.ge.1 .or. horizturb.eq.1 )THEN
        ! deformation:
        call calcdef(    rds,sigma,rdsf,sigmaf,zs,gz,rgz,gzu,rgzu,gzv,rgzv,                &
                     xh,rxh,arh1,arh2,uh,xf,rxf,arf1,arf2,uf,vh,vf,mh,c1,c2,mf,defv,defh,  &
                     dum1,dum2,ua,va,wa,t11,t12,t13,t22,t23,t33,gx,gy,rho,rr,rf)

      ENDIF

!--------------------------------------
!  LES subgrid models:

    les_sgs:  &
    IF( cm1setup.eq.1 )THEN

      IF( dot2p )THEN
        ! get gamk for two-part model:
        call     getgamk(dt,rtime,xf,rxf,c1,c2,zh,mh,zf,mf,rf0,rr0,rho0,rrf0,u0,v0,            &
                         uavg,vavg,savg,l2p,kmw,gamwall,gamk,s2p,s2b,t2pm1,t2pm2,t2pm3,t2pm4,cavg,  &
                         ua,va,wa,t11,t12,t13,t22,t23,t33,dum1,dum2,dum3,dum4,                 &
                         ust,zntmp,stau,rho,rr,rf,kmh,kmv,                                     &
                         ugr ,fsu  ,u2pt,vgr ,fsv  ,v2pt,uf,vf,arf1,arf2,                      &
                         u1,v1,s1,u1b,v1b,                                                     &
                         avgsfcu,avgsfcv,avgsfcs,                                              &
                         sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,reqs_s)
        ! update turbulence model "constants":
        do k=1,nk+1
          cmz(k) = gamk(k)*c_m
          ce1z(k) = cmz(k) * c_l * c_l * ( 1.0 / ri_c - 1.0 )
          ce2z(k) = max( 0.0 , cmz(k) * pi * pi - ce1z(k) )
          csz(k) = ( cmz(k) * cmz(k) * cmz(k) / ( ce1z(k) + ce2z(k) ) )**0.25   ! Smagorinsky constant
        enddo
      ENDIF

      sgsoption:  &
      IF( sgsmodel.eq.1 .or. sgsmodel.eq.3 .or. sgsmodel.eq.4 .or. sgsmodel.eq.5 )THEN

         ! Deardorff TKE:
        call tkekm(nstep,rtime,dt,ruh,rvh,rmh,zf,mf,rmf,zntmp,ust,ustt,rf,cmz,csz,ce1z,ce2z, &
                   nm,defv,defh,dum1,dum2,dum3,dum4,dum5,             &
                   kmh,kmv,khh,khv,tkea,lenscl,dissten,out3d,         &
                   nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,                   &
                   kw1(1,1,1),kw2(1,1,1),ke1(1,1,1),ke2(1,1,1),       &
                   ks1(1,1,1),ks2(1,1,1),kn1(1,1,1),kn2(1,1,1),       &
                   kw1(1,1,2),kw2(1,1,2),ke1(1,1,2),ke2(1,1,2),       &
                   ks1(1,1,2),ks2(1,1,2),kn1(1,1,2),kn2(1,1,2),       &
                   kw1(1,1,3),kw2(1,1,3),ke1(1,1,3),ke2(1,1,3),       &
                   ks1(1,1,3),ks2(1,1,3),kn1(1,1,3),kn2(1,1,3),       &
                   kw1(1,1,4),kw2(1,1,4),ke1(1,1,4),ke2(1,1,4),       &
                   ks1(1,1,4),ks2(1,1,4),kn1(1,1,4),kn2(1,1,4))

        if( do_ib )then
          do j=0,nj+2
          do i=0,ni+2
            if( kbdy(i,j).gt.1 )then
              tkea(i,j,1) = 0.0
               kmv(i,j,1) = 0.0
               kmh(i,j,1) = 0.0
               khv(i,j,1) = 0.0
               khh(i,j,1) = 0.0
            endif
          enddo
          enddo
        endif

        call     tkebc(tkea,kmh,kmv,khh,khv,zntmp,ust,ustt,          &
                       kw1(1,1,1),kw2(1,1,1),ke1(1,1,1),ke2(1,1,1),  &
                       ks1(1,1,1),ks2(1,1,1),kn1(1,1,1),kn2(1,1,1),  &
                       kw1(1,1,2),kw2(1,1,2),ke1(1,1,2),ke2(1,1,2),  &
                       ks1(1,1,2),ks2(1,1,2),kn1(1,1,2),kn2(1,1,2))

        do j=0,nj+1
        do i=0,ni+1
          tke3d(i,j,1) = tkea(i,j,1)
          tke3d(i,j,nk+1) = tkea(i,j,nk+1)
        enddo
        enddo

        IF( sgsmodel.eq.5 )THEN
          ! Nonlinear Backscatter and Anisotropy (NBA) model:
          ! TKE version:
          call   turbnba(nstep,uh,ruh,uf,ruf,vh,rvh,vf,rvf,mh,rmh,mf,rmf,zf,c1,c2,rho,rf,zntmp,ust,  &
                         dum1,dum2,dum3,dum4,dum5,dum6,            &
                         dum7,dum8,thten,thten1,ppten ,divx,       &
                         m11,m12,m13,m22,m23,m33,                  &
                         cmz,csz,ce1z,ce2z,                        &
                         ua ,va ,wa ,tkea ,nm,                     &
                         kw1,kw2,ke1,ke2,ks1,ks2,kn1,kn2,reqs_s,   &
                         nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
        ENDIF

      ELSEIF(sgsmodel.eq.2)THEN

         ! Smagorinsky:
        call turbsmag(nstep,rtime,dt,dosfcflx,ruh,rvh,rmh,mf,rmf,th0,thflux,qvflux,rth0s,rf, &
                      nm,defv,defh,dum4,dum5,thten1,zf,zntmp,ust,        &
                      cmz,csz,ce1z,ce2z,                               &
                      kmh,kmv,khh,khv,lenscl,dissten,out3d,            &
                      nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,                 &
                      kw1(1,1,1),kw2(1,1,1),ke1(1,1,1),ke2(1,1,1),     &
                      ks1(1,1,1),ks2(1,1,1),kn1(1,1,1),kn2(1,1,1),     &
                      kw1(1,1,2),kw2(1,1,2),ke1(1,1,2),ke2(1,1,2),     &
                      ks1(1,1,2),ks2(1,1,2),kn1(1,1,2),kn2(1,1,2))

      ELSEIF( sgsmodel.eq.6 )THEN

        ! Nonlinear Backscatter and Anisotropy (NBA) model:
        ! Smagorinsky version:
        call     turbnba2(nstep,uh,ruh,uf,ruf,vh,rvh,vf,rvf,mh,rmh,mf,rmf,zf,c1,c2,rho,rf,zntmp,ust,  &
                         dum1,dum2,dum3,dum4,dum5,dum6,            &
                         dum7,dum8,thten,thten1,ppten ,divx,       &
                         m11,m12,m13,m22,m23,m33,                  &
                         cmz,csz,ce1z,ce2z,                        &
                         ua ,va ,wa ,tkea ,nm,                     &
                         kw1,kw2,ke1,ke2,ks1,ks2,kn1,kn2,reqs_s,   &
                         nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)

      ELSEIF( sgsmodel.eq.0 )THEN

        ! for LES modeling without a subgrid turbulence model:
        ! (sometimes called implicit les, or ILES)

        !$omp parallel do default(shared)  &
        !$omp private(i,j,k)
        do k=2,nk+1
        do j=0,nj+1
        do i=0,ni+1
          t11(i,j,k) = 0.0
          t22(i,j,k) = 0.0
          t33(i,j,k) = 0.0
          t12(i,j,k) = 0.0
          t13(i,j,k) = 0.0
          t23(i,j,k) = 0.0
        enddo
        enddo
        enddo

      ELSE

        print *,'  72383 '
        call stopcm1

      ENDIF  sgsoption

      !-------------

      ift2p:  &
      IF( dot2p )THEN
        ! Use 2-part subgrid turbulence model:

        rstrt:  &
        if( restarted )then

          if(dowr) write(outfile,*)
          if(dowr) write(outfile,*) '  ... skipping t2p stuff ... '
          if(dowr) write(outfile,*)

        else

          if( sgsmodel.eq.3 )then
            call  t2psmm(dt,rtime,xf,rxf,c1,c2,                                                &
                         zh,mh,zf,mf,rf0,rr0,rho0,rrf0,u0,v0,                                  &
                         uavg,vavg,savg,l2p,kmw,gamk,gamwall,s2p,s2b,t2pm1,t2pm2,t2pm3,t2pm4,cavg,  &
                         ua,va,wa,t11,t12,t13,t22,t23,t33,                                     &
                         dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,                              &
                         ust,zntmp,stau,rho,rr,rf,kmv,kmh,                                     &
                         ugr ,vgr ,uf,vf,arf1,arf2,                                            &
                         u1,v1,s1,u1b,v1b,avgsfcu,avgsfcv,avgsfcs,                             &
                         reqs_s,uw31,uw32,ue31,ue32,us31,us32,un31,un32)
          elseif( sgsmodel.eq.4 )then
            call t2pcode(dt,rtime,xf,rxf,c1,c2,zh,mh,zf,mf,rf0,rr0,rho0,rrf0,u0,v0,            &
                         uavg,vavg,savg,l2p,kmw,gamwall,gamk,s2p,s2b,t2pm1,t2pm2,t2pm3,t2pm4,cavg,  &
                         ua,va,wa,t11,t12,t13,t22,t23,t33,dum1,dum2,dum3,dum4,                 &
                         ust,zntmp,stau,rho,rr,rf,kmh,kmv,                                     &
                         ugr ,fsu  ,u2pt,vgr ,fsv  ,v2pt,uf,vf,arf1,arf2,                      &
                         u1,v1,s1,u1b,v1b,                                                     &
                         avgsfcu,avgsfcv,avgsfcs,                                              &
                         sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,reqs_s)
          else
            stop 54321
          endif

        endif  rstrt

      ENDIF  ift2p

      !-------------

    ENDIF  les_sgs

!--------------------------------------
!  Simple Smagorinsky-like turbulence schemes:

      IF( cm1setup.eq.2 .and. ipbl.eq.2 )THEN
        call turbparam_vert(nstep,zf,dt,dosfcflx,ruh,rvh,rmh,mf,rmf,th0,thflux,qvflux,rth0s,rf, &
                      nm,defv,defh,dum4,kmv,khv,dissten,out3d,zs,zntmp,ust,xland, &
                      nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,                 &
                      kw1(1,1,1),kw2(1,1,1),ke1(1,1,1),ke2(1,1,1),     &
                      ks1(1,1,1),ks2(1,1,1),kn1(1,1,1),kn2(1,1,1),     &
                      kw1(1,1,2),kw2(1,1,2),ke1(1,1,2),ke2(1,1,2),     &
                      ks1(1,1,2),ks2(1,1,2),kn1(1,1,2),kn2(1,1,2))
      ENDIF

      IF( cm1setup.eq.2 .and. horizturb.eq.1 )THEN
        call turbparam_horiz(nstep,zf,dt,dosfcflx,ruh,rvh,rmh,mf,rmf,th0,thflux,qvflux,rth0s,rf, &
                      nm,defv,defh,dum4,kmh,khh,dissten,out3d,zs,zntmp,ust,xland,psfc,tlh, &
                      nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,                 &
                      kw1(1,1,1),kw2(1,1,1),ke1(1,1,1),ke2(1,1,1),     &
                      ks1(1,1,1),ks2(1,1,1),kn1(1,1,1),kn2(1,1,1),     &
                      kw1(1,1,2),kw2(1,1,2),ke1(1,1,2),ke2(1,1,2),     &
                      ks1(1,1,2),ks2(1,1,2),kn1(1,1,2),kn2(1,1,2))
      ENDIF

!--------------------------------------
!   DNS (constant viscosity)

      IF( cm1setup.eq.3 )THEN

        tem = viscosity/pr_num

        !$omp parallel do default(shared)  &
        !$omp private(i,j,k)
        do k=1,nk+1
        do j=0,nj+1
        do i=0,ni+1
          kmh(i,j,k) = viscosity
          kmv(i,j,k) = viscosity
          khh(i,j,k) = tem
          khv(i,j,k) = tem
        enddo
        enddo
        enddo

      ENDIF

!--------------------------------------
!  Check for numerical stability:

      if( cm1setup.ge.1 .and. adapt_dt.eq.1 )then
        call calcksquick(dt,uh,vh,mf,kmh,kmv,khh,khv,reqk)
      endif

!--------------------------------------

    tau:  &
    IF( cm1setup.eq.1 .or. ipbl.eq.2 .or. horizturb.eq.1 .or. cm1setup.eq.3 )THEN

      if( do_ib )then
#ifdef MPI
        call getcorneru(ua,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1))
        call bcu2(ua)
        call getcornerv(va,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1))
        call bcv2(va)
        call getcornerw(wa,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1))
        call bcw2(wa)
#endif
      endif

        !  now, get turbulent stresses:
        call     gettau(xf,rxf,arf1,arf2,ust,stau,u1,v1,s1,ustt,ut,vt,st,rf,mf,dum1,dum2,dum3,dum4,dum5,dum6, &
                        kmh,kmv,t11,t12,t13,t22,t23,t33,ua,ugr,va,vgr,wa,avgsfcu,avgsfcv,avgsfcs,bndy,kbdy)

        if( do_ib )then
          call drag_obstacles(xh,yh,zh,zf,rho,rf,dum1,dum2,dum3,dum4,dum5,dum6,t11,t12,t13,t22,t23,t33,ua,va,wa,kbdy)
        endif

      IF( sgsmodel.eq.5 .or. sgsmodel.eq.6 )THEN
        do j=1,nj+1
        do i=1,ni+1
          m13(i,j,nk+1) = m13(i,j,nk)
          m23(i,j,nk+1) = m23(i,j,nk)
        enddo
        enddo
      ENDIF

      IF( bbc.eq.3 .and. ( sgsmodel.eq.5 .or. sgsmodel.eq.6 ) )THEN
          ! for NBA scheme, set m13 and m23 at boundaries:
        do j=1,nj+1
        do i=1,ni+1
          m13(i,j,1) = t13(i,j,1)
          m23(i,j,1) = t23(i,j,1)
        enddo
        enddo
      ENDIF
      IF( tbc.eq.3 .and. ( sgsmodel.eq.5 .or. sgsmodel.eq.6 ) )THEN
          ! for NBA scheme, set m13 and m23 at boundaries:
        do j=1,nj+1
        do i=1,ni+1
          m13(i,j,nk+1) = t13(i,j,nk+1)
          m23(i,j,nk+1) = t23(i,j,nk+1)
        enddo
        enddo
      ENDIF

    ENDIF  tau

    IF( cm1setup.eq.1 .and. sgsmodel.ge.1 )THEN

      if( sgsmodel.ge.1 .or. sgsmodel.le.4 )then
              ! use tij arrays:
            call getepst(xh,rxh,uh,xf,rxf,uf,vh,vf,mh,c1,c2,mf,ua ,va ,wa ,  &
                         t11,t12,t13,t22,t23,t33,rf,                         &
                         dum1,dum2,epst)
      elseif( sgsmodel.eq.5 .or. sgsmodel.eq.6 )then
              ! use mij arrays:
            call getepst(xh,rxh,uh,xf,rxf,uf,vh,vf,mh,c1,c2,mf,ua ,va ,wa ,  &
                         m11,m12,m13,m22,m23,m33,rf,                         &
                         dum1,dum2,epst)
      else
        print *,'  93872 '
        call stopcm1
      endif

          IF( dodomaindiag .and. dotdwrite )then
            call getepsd(xh,rxh,uh,xf,rxf,uf,vh,vf,mh,c1,c2,mf,ua ,va ,wa ,  &
                         dum1,dum2,epsd1,epsd2,rho0,rf0,rrf0,rru,rrv,rrw,dt)
          ENDIF

    ENDIF


!-------------------------------------------------------------------
!  dissip rate for DNS:

        IF( cm1setup.eq.3 )THEN

          rcoef = 1.0/viscosity

          if( axisymm.eq.0 )then

            ! Cartesian grid:

            !$omp parallel do default(shared)  &
            !$omp private(i,j,k,tmp11,tmp22,tmp33,tmp12,tmp13,tmp23)
            DO j=1,nj
              do k=2,nk
              do i=1,ni
                tmp11=( c1(i,j,k)*t11(i,j,k-1)**2 + c2(i,j,k)*t11(i,j,k)**2 )
                tmp22=( c1(i,j,k)*t22(i,j,k-1)**2 + c2(i,j,k)*t22(i,j,k)**2 )
                tmp33=( c1(i,j,k)*t33(i,j,k-1)**2 + c2(i,j,k)*t33(i,j,k)**2 )
                tmp12=0.25*( c1(i,j,k)*( ( t12(i,j  ,k-1)**2 + t12(i+1,j+1,k-1)**2 )     &
                                       + ( t12(i,j+1,k-1)**2 + t12(i+1,j  ,k-1)**2 ) )   &
                            +c2(i,j,k)*( ( t12(i,j  ,k  )**2 + t12(i+1,j+1,k  )**2 )     &
                                       + ( t12(i,j+1,k  )**2 + t12(i+1,j  ,k  )**2 ) ) ) 
                tmp13=0.5*( t13(i,j,k)**2 + t13(i+1,j,k)**2 )
                tmp23=0.5*( t23(i,j,k)**2 + t23(i,j+1,k)**2 )
                dissten(i,j,k)= rcoef*( ( 2.0*( tmp33 ) + ( tmp13 + tmp23 )               &
                                         +2.0*( tmp11 + tmp22 ) + tmp12 )/(rf(i,j,k)**2) )
              enddo
              enddo
            ENDDO

          elseif( axisymm.eq.1 )then

            ! axisymmetric grid:

            !$omp parallel do default(shared)  &
            !$omp private(i,j,k,tmp11,tmp22,tmp33,tmp12,tmp13,tmp23)
            do k=2,nk
            do j=1,nj
            do i=1,ni
              tmp11=( c1(1,1,k)*t11(i,j,k-1)**2 + c2(1,1,k)*t11(i,j,k)**2 )
              tmp33=( c1(1,1,k)*t33(i,j,k-1)**2 + c2(1,1,k)*t33(i,j,k)**2 )
              tmp12=0.5*( c1(1,1,k)*( t12(i,j  ,k-1)**2 + t12(i+1,j  ,k-1)**2 ) &
                         +c2(1,1,k)*( t12(i,j  ,k  )**2 + t12(i+1,j  ,k  )**2 ) ) 
              tmp13=0.5*( t13(i,j,k)**2 + t13(i+1,j,k)**2 )
              tmp23=      t23(i,j,k)**2
              dissten(i,j,k)= rcoef*( ( 2.0*( tmp33 ) + ( tmp13 + tmp23 )               &
                                       +2.0*( tmp11 + tmp22 ) + tmp12 )/(rf(i,j,k)**2) )
            enddo
            enddo
            enddo

          endif

        ENDIF

      if(timestats.ge.1) time_turb=time_turb+mytime()

!-------------------------------------------------------------------
!  other PBL schemes:

    doingpbl:  &
    IF( cm1setup.eq.2 .and. ipbl.ge.1 )THEN

    dopbl:  &
    IF( getpbl )THEN

      !c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c!

      pblopt:  &
      if( use_pbl )then

        !$omp parallel do default(shared)   &
        !$omp private(i,j,k)
        DO k=1,nk
          ! store qi in dum8:
          if( nqi.ge.1 )then
            do j=1,nj
            do i=1,ni
              dum8(i,j,k) = qa(i,j,k,nqi)
            enddo
            enddo
          else
            do j=1,nj
            do i=1,ni
              dum8(i,j,k) = 0.0
            enddo
            enddo
          endif
          IF(output_km.eq.1.or.output_kh.eq.1)THEN
            do j=1,nj
            do i=1,ni
              thten1(i,j,k)=0.0
              thten(i,j,k)=0.0
            enddo
            enddo
          ENDIF
        ENDDO

        ! here, ppten stores qc:

        if( nqc.ge.1 )then
          !$omp parallel do default(shared)   &
          !$omp private(i,j,k)
          DO k=1,nk
          do j=1,nj
          do i=1,ni
            ppten(i,j,k) = qa(i,j,k,nqc)
          enddo
          enddo
          ENDDO
        else
          !$omp parallel do default(shared)   &
          !$omp private(i,j,k)
          DO k=1,nk
          do j=1,nj
          do i=1,ni
            ppten(i,j,k) = 0.0
          enddo
          enddo
          ENDDO
        endif

        !$omp parallel do default(shared)   &
        !$omp private(i,j,k)
        DO j=1,nj
          do k=1,nk
          do i=1,ni
            ! ground-relative winds:
            dum1(i,j,k)=0.5*(ugr(i,j,k)+ugr(i+1,j,k))
            dum2(i,j,k)=0.5*(vgr(i,j,k)+vgr(i,j+1,k))
            dum3(i,j,k)=th0(i,j,k)+tha(i,j,k)
            dum7(i,j,k)=pi0(i,j,k)+ppi(i,j,k)
            dum4(i,j,k)=dum3(i,j,k)*dum7(i,j,k)
          enddo
          enddo
          do k=1,nk
          do i=1,ni
            dum5(i,j,k) = dz*rmh(i,j,k)
          enddo
          enddo
          do k=2,nk
          do i=1,ni
            dum6(i,j,k) = c1(i,j,k)*prs(i,j,k-1)+c2(i,j,k)*prs(i,j,k)
          enddo
          enddo
          ! surface:
          do i=1,ni
            dum6(i,j,1) = psfc(i,j)
          enddo
          ! top of model:
          do i=1,ni
            dum6(i,j,nk+1)= cgt1*prs(i,j,nk)+cgt2*prs(i,j,nk-1)+cgt3*prs(i,j,nk-2)
          enddo
        ENDDO

        ! dum1 = u at scalars
        ! dum2 = v at scalars
        ! dum3 = th
        ! dum4 = t
        ! dum5 = dz
        ! dum6 = p3di (p8w)
        ! dum7 = pi

        if( sfcmodel.eq.4 )then
          do j=1,nj
          do i=1,ni
            wspd(i,j) = swspd(i,j)
          enddo
          enddo
        endif

        if( iice.eq.1 )then
          flag_qi = .true.
        else
          flag_qi = .false.
        endif

      doysu:  &
      if(ipbl.eq.1)then
        ! YSU PBL:

        ysu_topdown_pblmix = 0
        if( ysu_topdown_pblmix .eq. 1 )then
          if( radopt.ge.1 )then
            do k=1,nk
            do j=1,nj
            do i=1,ni
              t33(i,j,k) = lwten(i,j,k)+swten(i,j,k)
            enddo
            enddo
            enddo
          else
            do k=1,nk
            do j=1,nj
            do i=1,ni
              t33(i,j,k) = 0.0
            enddo
            enddo
            enddo
          endif
        endif
        call ysu(u3d=dum1,v3d=dum2,th3d=dum3,t3d=dum4,qv3d=divx,                    &
                  qc3d=ppten,qi3d=dum8,p3d=prs,p3di=dum6,pi3d=dum7,                 &
                  rublten=upten,rvblten=vpten,rthblten=thpten,                      &
                  rqvblten=qvpten,rqcblten=qcpten,rqiblten=qipten,flag_qi=flag_qi,  &
                  cp=cp,g=g,rovcp=rovcp,rd=rd,rovg=rovg,ep1=ep1,ep2=ep2,            &
                  karman=karman,xlv=xlv,lv1=lv1,lv2=lv2,rv=rv,                      &
                  dz8w=dum5 ,psfc=psfc,                                             &
                  znt=zntmp,ust=ust,hpbl=hpbl,psim=fm,psih=fh,                      &
                  xland=xland,hfx=hfx,qfx=qfx,wspd=wspd,br=br,brcr=brcr,            &
                  dt=dt,kpbl2d=kpbl2d,                                              &
                  exch_h=thten1,exch_m=thten,                                       &
                  xkzh=xkzh,xkzq=xkzq,xkzm=xkzm,                                    &
                  wstar=wstar,delta=delta,prkpp=prkpp,                              &
                  u10=u10,v10=v10,stau=stau,                                        &
                  uoce=huml,voce=hvml,                                              &
                  rthraten=t33,ysu_topdown_pblmix=ysu_topdown_pblmix,               &
                  iamsat=iamsat,                                                    &
                  ids=1  ,ide=ni+1 , jds= 1 ,jde=nj+1 , kds=1  ,kde=nk+1 ,          &
                  ims=ib ,ime=ie   , jms=jb ,jme=je   , kms=kb ,kme=ke ,            &
                  its=1  ,ite=ni   , jts=1  ,jte=nj   , kts=1  ,kte=nk ,            &
                  regime=regime)

        IF( output_km.eq.1 .or. output_kh.eq.1 .or. dodomaindiag )THEN
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
          do k=1,nk+1
          do j=1,nj
          do i=1,ni
            khv(i,j,k) = thten1(i,j,k)
            kmv(i,j,k) = thten(i,j,k)
          enddo
          enddo
          enddo
        ENDIF

      endif  doysu

      !c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c!

      dogfsedmf:  &
      if( ipbl.eq.3 )then
        ! HWRF PBL  (aka, GFS-EDMF)

        if( radopt.ge.1 )then
          do k=1,nk
          do j=1,nj
          do i=1,ni
            t33(i,j,k) = lwten(i,j,k)+swten(i,j,k)
          enddo
          enddo
          enddo
        else
          do k=1,nk
          do j=1,nj
          do i=1,ni
            t33(i,j,k) = 0.0
          enddo
          enddo
          enddo
        endif

        ep1 = rv/rd - 1.0
        ep2 = rd/rv
        rovg = rd/g

        do k=1,nk
          zh1d(k) = zh(1,1,k)
        enddo
        do k=1,nk+1
          zf1d(k) = zf(1,1,k)
        enddo

        if( sfcmodel.ne.4 )then
          do j=1,nj
          do i=1,ni
            mznt(i,j) = znt(i,j)
          enddo
          enddo
        endif

        call  BL_GFSEDMF(U3D=dum1,V3D=dum2,TH3D=dum3,T3D=dum4,QV3D=divx,            &
                  QC3D=ppten,QI3D=dum8,P3D=prs,PI3D=dum7,                           &
                  RUBLTEN=upten,RVBLTEN=vpten,RTHBLTEN=thpten,                      &
                  RQVBLTEN=qvpten,RQCBLTEN=qcpten,RQIBLTEN=qipten,                  & 
                  CP=cp,G=g,ROVCP=rovcp,R=rd,ROVG=rovg,                             &
                  P_QI=p_qi,P_FIRST_SCALAR=p_first_scalar,                          &
                  dz8w=dum5,z=zf(ib,jb,kb),PSFC=psfc,                               &
                  UST=ust,PBL=hpbl,PSIM=psim,PSIH=psih,                             &
                  HFX=hfx,QFX=qfx,TSK=tsk,GZ1OZ0=gz1oz0,WSPD=wspd,BR=br,            &
                  DT=dt,KPBL2D=kpbl2d,EP1=ep1,KARMAN=karman,                        &
                  DISHEAT=disheat,                                                  &
                  RTHRATEN=t33,                                                     &
                  HPBL2D=hpbl2d, EVAP2D=evap2d, HEAT2D=heat2d,                      &
                  U10=u10,V10=v10,ZNT=mznt,                                         &
                  DKU3D=xkzm,DKT3D=xkzh,                                            & 
                  VAR_RIC=var_ric,coef_ric_l=coef_ric_l,coef_ric_s=coef_ric_s,      &
                  alpha=gfs_alpha,xland=xland, pert_pbl=pert_pbl,                   &
                  ens_random_seed=ens_random_seed, ens_pblamp=ens_pblamp,           &
                  brcr=brcr,wscale=wscale,wscaleu=wscaleu,prkpp=prkpp,              &
                  stau=stau,dissten=dissten(ib,jb,kb),dheat=thten,                  &
                  ids=1  ,ide=ni+1 , jds= 1 ,jde=nj+1 , kds=1  ,kde=nk+1 ,          &
                  ims=ib ,ime=ie   , jms=jb ,jme=je   , kms=kb ,kme=ke ,            &
                  its=1  ,ite=ni   , jts=1  ,jte=nj   , kts=1  ,kte=nk              )
        IF( dotbud .and. td_diss.ge.1 )THEN
          !$omp parallel do default(shared)  &
          !$omp private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            ! dissipative heating rate:
            tdiag(i,j,k,td_diss) = thten(i,j,k)
          enddo
          enddo
          enddo
        ENDIF
        IF( dotbud .and. td_pbl.ge.1 )THEN
          !$omp parallel do default(shared)  &
          !$omp private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            ! for budget, subtract dissip heating from PBL tendency:
            tdiag(i,j,k,td_pbl) = thpten(i,j,k)-thten(i,j,k)
          enddo
          enddo
          enddo
        ENDIF

      endif  dogfsedmf

      !c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c!

      domynn:  &
      if( ipbl.eq.4 .or. ipbl.eq.5 )then
        ! MYNN PBL:

        if( radopt.ge.1 )then
          do k=1,nk
          do j=1,nj
          do i=1,ni
            t33(i,j,k) = lwten(i,j,k)+swten(i,j,k)
          enddo
          enddo
          enddo
        else
          do k=1,nk
          do j=1,nj
          do i=1,ni
            t33(i,j,k) = 0.0
          enddo
          enddo
          enddo
        endif

        do k=1,nk
        do j=1,nj
        do i=1,ni
          thten(i,j,k) = 0.5*(wa(i,j,k)+wa(i,j,k+1))
        enddo
        enddo
        enddo

        IF( imoist.eq.1 )THEN
          if( nnci.ge.1 )then
            do k=1,nk
            do j=1,nj
            do i=1,ni
              sadv(i,j,k) = qa(i,j,k,nnci)
            enddo
            enddo
            enddo
          else
            do k=1,nk
            do j=1,nj
            do i=1,ni
              sadv(i,j,k) = 0.0
            enddo
            enddo
            enddo
          endif
          if( nncc.ge.1 )then
            do k=1,nk
            do j=1,nj
            do i=1,ni
              thterm(i,j,k) = qa(i,j,k,nncc)
            enddo
            enddo
            enddo
          else
            do k=1,nk
            do j=1,nj
            do i=1,ni
              thterm(i,j,k) = 0.0
            enddo
            enddo
            enddo
          endif
        ELSE
          do k=1,nk
          do j=1,nj
          do i=1,ni
            sadv(i,j,k) = 0.0
            thterm(i,j,k) = 0.0
          enddo
          enddo
          enddo
        ENDIF

        FLAG_QC = .true.
        FLAG_QI = .true.
        if( ptype.eq.3 .or. ptype.eq.5 )then
          FLAG_QNI = .true.
        else
          FLAG_QNI = .false.
        endif
        FLAG_QNC = .false.
        FLAG_QNWFA = .false.
        FLAG_QNIFA = .false.

        IF( nstep.eq.0 )THEN
          initflag=1
          if( myid.eq.0 ) print *,'  mynn_bl_driver initflag = ',initflag
        ELSE
          initflag=0
        ENDIF

        call mynn_bl_driver(                                                         &
        initflag=initflag,grav_settling=grav_settling,                               &
        restart=.false.,cycling=.false.,                                             &
        delt=dt,dz=dum5,dx=dx,znt=zntmp,                                             &
        u=dum1,v=dum2,w=thten,th=dum3,qv=divx,qc=ppten,qi=dum8,qnc=thterm,qni=sadv,  &
        p=prs,exner=dum7,rho=rho,T3D=dum4,                                           &
        xland=xland,ts=tsk,qsfc=qsfc,qcg=qcg,ps=psfc,                                &
        ust=ust,ch=ch,hfx=hfx,qfx=qfx,rmol=rmol,wspd=wspd,                           &
        uoce=huml,voce=hvml,                                                         & !ocean current
        vdfg=vdfg,                                                                   & !Katata-added for fog dep
        Qke=qke,                                                                     &
        qke_adv=qke3d,bl_mynn_tkeadvect=bl_mynn_tkeadvect,                           & !ACF for QKE advection,  GHB: Note that qke_adv is really qke updated after advection (200812)
        Tsq=tsq,Qsq=qsq,Cov=cov,                                                     &
        RUBLTEN=upten,RVBLTEN=vpten,RTHBLTEN=thpten,                                 &
        RQVBLTEN=qvpten,RQCBLTEN=qcpten,RQIBLTEN=qipten,                             &
        RQNCBLTEN=qncpten,RQNIBLTEN=qnipten,                                         &
        RQNWFABLTEN=RQNWFABLTEN,RQNIFABLTEN=RQNIFABLTEN,                             &
        exch_h=xkzh,exch_m=xkzm,                                                     &
        Pblh=hpbl,kpbl=kpbl2d,                                                       &
        el_pbl=el_pbl,                                                               &
        dqke=dqke,qWT=qWT,qSHEAR=qSHEAR,qBUOY=qBUOY,qDISS=qDISS,                     & !JOE-TKE BUDGET
        wstar=wstar,delta=delta,                                                     & !JOE-added for grims
        bl_mynn_tkebudget=bl_mynn_tkebudget,                                         &
        bl_mynn_cloudpdf=bl_mynn_cloudpdf,Sh3D=sh3d,                                 &
        bl_mynn_mixlength=bl_mynn_mixlength,                                         &
        icloud_bl=icloud_bl,qc_bl=qc_bl,qi_bl=qi_bl,cldfra_bl=cldfra_bl,             &
        bl_mynn_edmf=bl_mynn_edmf,                                                   &
        bl_mynn_edmf_mom=bl_mynn_edmf_mom,bl_mynn_edmf_tke=bl_mynn_edmf_tke,         &
        bl_mynn_mixscalars=bl_mynn_mixscalars,                                       &
        bl_mynn_output=bl_mynn_output,                                               &
        bl_mynn_cloudmix=bl_mynn_cloudmix,bl_mynn_mixqt=bl_mynn_mixqt,               &
        edmf_a=edmf_a,edmf_w=edmf_w,edmf_qt=edmf_qt,                                 &
        edmf_thl=edmf_thl,edmf_ent=edmf_ent,edmf_qc=edmf_qc,                         &
        sub_thl3D=sub_thl3D,sub_sqv3D=sub_sqv3D,                                     &
        det_thl3D=det_thl3D,det_sqv3D=det_sqv3D,                                     &
        nupdraft=nupdraft,maxMF=maxmf,ktop_plume=ktop_plume,                         &
        spp_pbl=spp_pbl,                                                             &
        RTHRATEN=t33,dheat3d=thten1,                                                 &
        FLAG_QC=FLAG_QC,FLAG_QI=FLAG_QI,FLAG_QNC=FLAG_QNC,                           &
        FLAG_QNI=FLAG_QNI,FLAG_QNWFA=FLAG_QNWFA,FLAG_QNIFA=FLAG_QNIFA                &
                 ,ids=1  ,ide=ni+1 , jds= 1 ,jde=nj+1 , kds=1  ,kde=nk+1 ,           &
                  ims=ib ,ime=ie   , jms=jb ,jme=je   , kms=kb ,kme=ke ,             &
                  its=1  ,ite=ni   , jts=1  ,jte=nj   , kts=1  ,kte=nk )

      ! Diagnostics !
        IF( dotbud .and. td_diss.ge.1 )THEN
          !$omp parallel do default(shared)  &
          !$omp private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            ! dissipative heating rate:
            tdiag(i,j,k,td_diss) = thten1(i,j,k)
          enddo
          enddo
          enddo
        ENDIF
        IF( dotbud .and. td_pbl.ge.1 )THEN
          !$omp parallel do default(shared)  &
          !$omp private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            ! for budget, subtract dissip heating from PBL tendency:
            tdiag(i,j,k,td_pbl) = thpten(i,j,k)-thten1(i,j,k)
          enddo
          enddo
          enddo
        ENDIF
      ! Diagnostics !

      endif  domynn

      !c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c!

      domyj:  &
      if( ipbl.eq.6 )then
        ! MYJ PBL:

        ! dum1 = u at scalars
        ! dum2 = v at scalars
        ! dum3 = th
        ! dum4 = t
        ! dum5 = dz
        ! dum6 = p3di (p8w)
        ! dum7 = pi

        ! divx  = qv
        ! ppten = qc
        ! dum8  = qi

        ! thten1 = exch_h
        ! thten  = exch_m


      myjloop:  &
      do j=1,nj
        do k=1,nk
        do i=1,ni
          tmp_myj(i,k, 1) = 0.0
          tmp_myj(i,k, 2) = 0.0
          tmp_myj(i,k, 3) = 0.0
          tmp_myj(i,k, 4) = tke_myj(i,j,k)
          tmp_myj(i,k, 5) = el_myj(i,j,k)
          tmp_myj(i,k, 6) = dum5(i,j,k)
          tmp_myj(i,k, 7) = prs(i,j,k)
          tmp_myj(i,k, 9) = dum3(i,j,k)
          tmp_myj(i,k,10) = dum4(i,j,k)
          tmp_myj(i,k,11) = dum7(i,j,k)
          tmp_myj(i,k,12) = divx(i,j,k)
          tmp_myj(i,k,13) = ppten(i,j,k)
          tmp_myj(i,k,14) = dum8(i,j,k)
          tmp_myj(i,k,15) = dum1(i,j,k)
          tmp_myj(i,k,16) = dum2(i,j,k)
          tmp_myj(i,k,17) = rho(i,j,k)
          tmp_myj(i,k,19) = upten(i,j,k)
          tmp_myj(i,k,20) = vpten(i,j,k)
          tmp_myj(i,k,21) = thpten(i,j,k)
          tmp_myj(i,k,22) = qvpten(i,j,k)
          tmp_myj(i,k,23) = qcpten(i,j,k)
          tmp_myj(i,k,24) = qipten(i,j,k)
          tmp_myj(i,k,25) = qspten(i,j,k)
          tmp_myj(i,k,26) = qrpten(i,j,k)
          tmp_myj(i,k,27) = qgpten(i,j,k)
        enddo
        enddo
        do k=1,nk+1
        do i=1,ni
          tmp_myj(i,k, 8) = dum6(i,j,k)
          tmp_myj(i,k,18) = 0.0
          tmp_myj(i,k,28) = 0.0
        enddo
        enddo
        call     MYJPBL(dt=DT,STEPBL=1,HT=zs(ib,j),DZ=tmp_myj(ibmyj,kbmyj,6)                     &
                       ,PMID=tmp_myj(ibmyj,kbmyj,7),PINT=tmp_myj(ibmyj,kbmyj,8),TH=tmp_myj(ibmyj,kbmyj,9),T=tmp_myj(ibmyj,kbmyj,10),EXNER=tmp_myj(ibmyj,kbmyj,11),QV=tmp_myj(ibmyj,kbmyj,12),QCW=tmp_myj(ibmyj,kbmyj,13),QCI=tmp_myj(ibmyj,kbmyj,14),QCS=tmp_myj(ibmyj,kbmyj,1),QCR=tmp_myj(ibmyj,kbmyj,2),QCG=tmp_myj(ibmyj,kbmyj,3)    &
                       ,U=tmp_myj(ibmyj,kbmyj,15),V=tmp_myj(ibmyj,kbmyj,16),RHO=tmp_myj(ibmyj,kbmyj,17),TSK=tsk(ib,j),QSFC=qsfc(ib,j),CHKLOWQ=chklowq(ib,j),THZ0=thz0(ib,j),QZ0=qz0(ib,j),UZ0=uz0(ib,j),VZ0=vz0(ib,j)      &
                       ,LOWLYR=lowlyr(ib,j),XLAND=xland(ib,j),SICE=sice(ib,j),SNOW=snow(ib,j)                         &
                       ,TKE_MYJ=tmp_myj(ibmyj,kbmyj,4),EXCH_H=tmp_myj(ibmyj,kbmyj,18),EXCH_M=tmp_myj(ibmyj,kbmyj,28),USTAR=ust(ib,j),ZNT=znt(ib,j),EL_MYJ=tmp_myj(ibmyj,kbmyj,5),PBLH=hpbl(ib,j),KPBL=kpbl2d(ib,j),CT=ct(ib,j)   &
                       ,AKHS=akhs(ib,j),AKMS=akms(ib,j),ELFLX=lh(ib,j),MIXHT=mixht(ib,j)                          &
                       ,RUBLTEN=tmp_myj(ibmyj,kbmyj,19),RVBLTEN=tmp_myj(ibmyj,kbmyj,20),RTHBLTEN=tmp_myj(ibmyj,kbmyj,21),RQVBLTEN=tmp_myj(ibmyj,kbmyj,22),RQCBLTEN=tmp_myj(ibmyj,kbmyj,23)     &
                       ,RQIBLTEN=tmp_myj(ibmyj,kbmyj,24),RQSBLTEN=tmp_myj(ibmyj,kbmyj,25),RQRBLTEN=tmp_myj(ibmyj,kbmyj,26),RQGBLTEN=tmp_myj(ibmyj,kbmyj,27),           &
                  ids=1  ,ide=ni+1 , jds=1  ,jde=1    , kds=1  ,kde=nk+1 ,          &
                  ims=ibmyj ,ime=iemyj   , jms=1  ,jme=1    , kms=kbmyj ,kme=kemyj ,            &
                  its=1  ,ite=ni   , jts=1  ,jte=1    , kts=1  ,kte=nk )
        do k=1,nk
        do i=1,ni
          tke_myj(i,j,k) = tmp_myj(i,k,4)
          el_myj(i,j,k) = tmp_myj(i,k,5)
          xkzh(i,j,k+1) = tmp_myj(i,k,18)
          xkzm(i,j,k+1) = tmp_myj(i,k,28)
          upten(i,j,k) = tmp_myj(i,k,19)
          vpten(i,j,k) = tmp_myj(i,k,20)
          thpten(i,j,k) = tmp_myj(i,k,21)
          qvpten(i,j,k) = tmp_myj(i,k,22)
          qcpten(i,j,k) = tmp_myj(i,k,23)
          qipten(i,j,k) = tmp_myj(i,k,24)
          qspten(i,j,k) = tmp_myj(i,k,25)
          qrpten(i,j,k) = tmp_myj(i,k,26)
          qgpten(i,j,k) = tmp_myj(i,k,27)
        enddo
        enddo
      enddo  myjloop


      endif  domyj

      !c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c!

      endif  pblopt

    ELSE

      if(dowr) write(outfile,*)
      if(dowr) write(outfile,*) '  ... skipping pbl calcs ... '
      if(dowr) write(outfile,*)

    ENDIF  dopbl

        if(timestats.ge.1) time_pbl=time_pbl+mytime()


      ysudiss:  &
      if( ipbl.eq.1 )then
        ! Dissipative heating from ysu scheme:

        IF( idiss.eq.1 .or. output_dissten.eq.1 )THEN
          !$omp parallel do default(shared)  &
          !$omp private(i,j)
          do j=1,nj
          do i=1,ni
            ! assume t13,t23 are zero at top of domain:
            !   dum3 = t13
            !   dum4 = t23
            dum3(i,j,nk+1) = 0.0
            dum4(i,j,nk+1) = 0.0
          enddo
          enddo
          !$omp parallel do default(shared)  &
          !$omp private(i,j,k,tem1,tem2)
          do k=nk,2,-1
          do j=1,nj
          do i=1,ni
            tem1 = dz*rmh(i,j,k)
            tem2 = rdz*mf(i,j,k)
            ! get stresses from u,v tendencies:
            dum3(i,j,k) = dum3(i,j,k+1)-upten(i,j,k)*tem1
            dum4(i,j,k) = dum4(i,j,k+1)-vpten(i,j,k)*tem1
            ! NOTE:  dissten is defined at w points:
            dissten(i,j,k) = dissten(i,j,k)                                 &
                          +( dum3(i,j,k)*( dum1(i,j,k)-dum1(i,j,k-1) )*tem2 &
                            +dum4(i,j,k)*( dum2(i,j,k)-dum2(i,j,k-1) )*tem2 )
          enddo
          enddo
          enddo

        ENDIF

      endif  ysudiss

        if(timestats.ge.1) time_pbl=time_pbl+mytime()

      !c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c!

      if( use_pbl )then
        call bcs(upten)
#ifdef MPI
        call comm_1s_start(upten,pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_s)
#endif
        call bcs(vpten)
#ifdef MPI
        call comm_1s_start(vpten,vw1,vw2,ve1,ve2,vs1,vs2,vn1,vn2,reqs_p)
#endif
        IF( axisymm.eq.1 )THEN
          ! cm1r19 bug fix:
          !$omp parallel do default(shared)   &
          !$omp private(i,k)
          do k=1,nk
            upten(0,1,k) = -upten(1,1,k)
            upten(ni+1,1,k) = upten(ni,1,k)
            do i=1,ni
              vpten(i,0,k) = vpten(i,1,k)
              vpten(i,2,k) = vpten(i,1,k)
            enddo
          enddo
        ENDIF
#ifdef MPI
        call comm_1s_end(upten,pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_s)
        call comm_1s_end(vpten,vw1,vw2,ve1,ve2,vs1,vs2,vn1,vn2,reqs_p)
#endif
      endif

    ENDIF  doingpbl

!-------------------------------------------------------------------


      end subroutine sfc_and_turb

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine tkekm(nstep,rtime,dt,ruh,rvh,rmh,zf,mf,rmf,zntmp,ust,ustt,rf,cmz,csz,ce1z,ce2z, &
                         nm,defv,defh,tk,dum2  ,lenh,grdscl,rgrdscl,        &
                         kmh,kmv,khh,khv,tkea,lenscl,dissten,out3d,         &
                         nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,                   &
                         khcw1,khcw2,khce1,khce2,khcs1,khcs2,khcn1,khcn2,   &
                         khdw1,khdw2,khde1,khde2,khds1,khds2,khdn1,khdn2,   &
                         kvcw1,kvcw2,kvce1,kvce2,kvcs1,kvcs2,kvcn1,kvcn2,   &
                         kvdw1,kvdw2,kvde1,kvde2,kvds1,kvds2,kvdn1,kvdn2)
      use input
      use constants
      use bc_module
      use comm_module
      implicit none

      integer, intent(in) :: nstep
      real, intent(in) :: rtime,dt
      real, intent(in), dimension(ib:ie) :: ruh
      real, intent(in), dimension(jb:je) :: rvh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rmh
      real, intent(in),dimension(ib:ie,jb:je,kb:ke+1) :: zf,mf,rmf
      real, intent(in), dimension(ib:ie,jb:je) :: zntmp,ust,ustt
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rf
      real, intent(in), dimension(kb:ke) :: cmz,csz,ce1z,ce2z
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: nm,defv,defh
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: tk,dum2,lenh,grdscl,rgrdscl
      real, intent(inout), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv,khh,khv
      real, intent(inout), dimension(ibt:iet,jbt:jet,kbt:ket) :: tkea
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: lenscl,dissten
      real, intent(inout) , dimension(ib3d:ie3d,jb3d:je3d,kb3d:ke3d,nout3d) :: out3d
      real, intent(inout), dimension(kmt) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      real, intent(inout), dimension(jmp,kmt) :: khcw1,khcw2,khce1,khce2
      real, intent(inout), dimension(imp,kmt) :: khcs1,khcs2,khcn1,khcn2
      real, intent(inout), dimension(jmp,kmt) :: khdw1,khdw2,khde1,khde2
      real, intent(inout), dimension(imp,kmt) :: khds1,khds2,khdn1,khdn2
      real, intent(inout), dimension(jmp,kmt) :: kvcw1,kvcw2,kvce1,kvce2
      real, intent(inout), dimension(imp,kmt) :: kvcs1,kvcs2,kvcn1,kvcn2
      real, intent(inout), dimension(jmp,kmt) :: kvdw1,kvdw2,kvde1,kvde2
      real, intent(inout), dimension(imp,kmt) :: kvds1,kvds2,kvdn1,kvdn2

!----------------------------------------

      integer :: i,j,k
      real :: prinv,tem1,tem2

      real, parameter :: tke_min         =  1.0e-10
      real, parameter :: nm_min          =  1.0e-6
      real, parameter :: small_len_frac  =  0.001

#ifdef MPI
      integer reqs_khc(8)
      integer reqs_kvc(8)
      integer reqs_khd(8)
      integer reqs_kvd(8)
#endif

!------------------------------------------------------------------
!  Get length scales:

    !  get grid scale
    IF(tconfig.eq.1)THEN

      ! single length scale:  appropriate if dx,dy are nearly the same as dz

      !$omp parallel do default(shared)   &
      !$omp private(i,j,k,prinv)
      do k=2,nk
        rcs = 1.0/max(1.0e-10,csz(k))
      do j=1,nj
      do i=1,ni
        grdscl(i,j,k)=( ((dx*ruh(i))*(dy*rvh(j)))*(dz*rmf(i,j,k)) )**0.33333333
        ! cm1r17:  wall condition near surface
        ! cm1r20.1: revisit near-sfc grid scale at a later date
!!!        grdscl(i,j,k) = sqrt(1.0/( 1.0/(grdscl(i,j,k)**2)                                  &
!!!                                  +1.0/((karman*((zf(i,j,k)-zf(i,j,1))+zntmp(i,j))*rcs)**2)  &
!!!                               ) )
        rgrdscl(i,j,k)=1.0/grdscl(i,j,k)

      ! Get turbulence length scale

        if( tkea(i,j,k).le.tke_min )then
          ! 170718:
          tk(i,j,k) = tke_min
          lenscl(i,j,k) = small_len_frac*grdscl(i,j,k)
        else
          tk(i,j,k)=tkea(i,j,k)
          lenscl(i,j,k)=grdscl(i,j,k)
          if(nm(i,j,k).gt.nm_min)then
            lenscl(i,j,k)=c_l*sqrt(tk(i,j,k)/nm(i,j,k))
            lenscl(i,j,k)=min(lenscl(i,j,k),grdscl(i,j,k))
            lenscl(i,j,k)=max(lenscl(i,j,k),small_len_frac*grdscl(i,j,k))
          endif 
        endif

    !  Dissipation

        dissten(i,j,k) = dissten(i,j,k)                           &
                     +(ce1z(k)+ce2z(k)*lenscl(i,j,k)*rgrdscl(i,j,k))    &
                     *tk(i,j,k)*sqrt(tk(i,j,k))/lenscl(i,j,k)

    !  Get km, kh

        kmh(i,j,k)=cmz(k)*sqrt(tk(i,j,k))*lenscl(i,j,k)
        kmv(i,j,k)=kmh(i,j,k)
        prinv=3.00
        if(nm(i,j,k).gt.nm_min)then
          prinv=min(1.0+2.00*lenscl(i,j,k)*rgrdscl(i,j,k),3.00)
        endif
        khh(i,j,k)=kmh(i,j,k)*prinv
        khv(i,j,k)=khh(i,j,k)

      enddo
      enddo
      enddo

    ELSEIF(tconfig.eq.2)THEN

      ! two length scales:  one for horizontal, one for vertical

      !$omp parallel do default(shared)   &
      !$omp private(i,j,k,prinv)
      do k=2,nk
        rcs = 1.0/max(1.0e-10,csz(k))
      do j=1,nj
      do i=1,ni

        lenh(i,j,k)=sqrt( (dx*ruh(i))*(dy*rvh(j)) )
        grdscl(i,j,k)=dz*rmf(i,j,k)
        ! cm1r17:  wall condition near surface
        ! cm1r20.1: revisit near-sfc grid scale at a later date
!!!        grdscl(i,j,k) = sqrt(1.0/( 1.0/(grdscl(i,j,k)**2)                                  &
!!!                                  +1.0/((karman*((zf(i,j,k)-zf(i,j,1))+zntmp(i,j))*rcs)**2)  &
!!!                               ) )
        rgrdscl(i,j,k)=1.0/grdscl(i,j,k)

      ! Get turbulence length scale

        if( tkea(i,j,k).le.tke_min )then
          ! 170718:
          tk(i,j,k) = tke_min
          lenscl(i,j,k) = small_len_frac*grdscl(i,j,k)
        else
          tk(i,j,k)=tkea(i,j,k)
          lenscl(i,j,k)=grdscl(i,j,k)
          if(nm(i,j,k).gt.nm_min)then
            lenscl(i,j,k)=c_l*sqrt(tk(i,j,k)/nm(i,j,k))
            lenscl(i,j,k)=min(lenscl(i,j,k),grdscl(i,j,k))
            lenscl(i,j,k)=max(lenscl(i,j,k),small_len_frac*grdscl(i,j,k))
          endif 
        endif

    !  Dissipation

        dissten(i,j,k) = dissten(i,j,k)                           &
                     +(ce1z(k)+ce2z(k)*lenscl(i,j,k)*rgrdscl(i,j,k))    &
                     *tk(i,j,k)*sqrt(tk(i,j,k))/lenscl(i,j,k)

    !  Get km, kh

        kmh(i,j,k)=cmz(k)*sqrt(tk(i,j,k))*lenh(i,j,k)
        kmv(i,j,k)=cmz(k)*sqrt(tk(i,j,k))*lenscl(i,j,k)
        prinv=3.00
        if(nm(i,j,k).gt.nm_min)then
          prinv=min(1.0+2.00*lenscl(i,j,k)*rgrdscl(i,j,k),3.00)
        endif
        khh(i,j,k)=kmh(i,j,k)*prinv
        khv(i,j,k)=kmv(i,j,k)*prinv

      enddo
      enddo
      enddo

    ENDIF


    if( nstep.eq.0 .and. myid.eq.0 )then
      print *
      print *,'  zf,grdscl:'
      i = 1
      j = 1
      do k=2,nk
        print *,k,(zf(i,j,k)-zf(i,j,1)),grdscl(i,j,k)
      enddo
    endif

      if(timestats.ge.1) time_turb=time_turb+mytime()

!------------------------------------------------------------
! Set values at boundaries, start comms:

      call bcw(kmh,1)
#ifdef MPI
      call comm_1t_start(kmh,khcw1,khcw2,khce1,khce2,   &
                             khcs1,khcs2,khcn1,khcn2,reqs_khc)
#endif

      call bcw(kmv,1)
#ifdef MPI
      call comm_1t_start(kmv,kvcw1,kvcw2,kvce1,kvce2,   &
                             kvcs1,kvcs2,kvcn1,kvcn2,reqs_kvc)
#endif

      call bcw(khh,1)
#ifdef MPI
      call comm_1t_start(khh,khdw1,khdw2,khde1,khde2,   &
                             khds1,khds2,khdn1,khdn2,reqs_khd)
#endif

      call bcw(khv,1)
#ifdef MPI
      call comm_1t_start(khv,kvdw1,kvdw2,kvde1,kvde2,   &
                             kvds1,kvds2,kvdn1,kvdn2,reqs_kvd)
#endif

!--------------------------------------------------------------
!  Finish comms:
#ifdef MPI
      call comm_1t_end(kmh,khcw1,khcw2,khce1,khce2,   &
                           khcs1,khcs2,khcn1,khcn2,reqs_khc)
      call bct2(kmh)
      call comm_1t_end(kmv,kvcw1,kvcw2,kvce1,kvce2,   &
                           kvcs1,kvcs2,kvcn1,kvcn2,reqs_kvc)
      call bct2(kmv)
      call comm_1t_end(khh,khdw1,khdw2,khde1,khde2,   &
                           khds1,khds2,khdn1,khdn2,reqs_khd)
      call comm_1t_end(khv,kvdw1,kvdw2,kvde1,kvde2,   &
                           kvds1,kvds2,kvdn1,kvdn2,reqs_kvd)
      call getcornert(kmh,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      call getcornert(kmv,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
#endif

!--------------------------------------------------------------
!  finished
      
      return
      end subroutine tkekm


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine tkebc(tkea,kmh,kmv,khh,khv,zntmp,ust,ustt,              &
                       khcw1,khcw2,khce1,khce2,khcs1,khcs2,khcn1,khcn2,  &
                       kvcw1,kvcw2,kvce1,kvce2,kvcs1,kvcs2,kvcn1,kvcn2)
      use input
      use constants
      use bc_module
      use comm_module
      implicit none

      real, intent(inout), dimension(ibt:iet,jbt:jet,kbt:ket) :: tkea
      real, intent(inout), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv,khh,khv
      real, intent(in), dimension(ib:ie,jb:je) :: zntmp,ust,ustt
      real, intent(inout), dimension(jmp,kmt) :: khcw1,khcw2,khce1,khce2
      real, intent(inout), dimension(imp,kmt) :: khcs1,khcs2,khcn1,khcn2
      real, intent(inout), dimension(jmp,kmt) :: kvcw1,kvcw2,kvce1,kvce2
      real, intent(inout), dimension(imp,kmt) :: kvcs1,kvcs2,kvcn1,kvcn2

      integer :: i,j
#ifdef MPI
      integer, dimension(8) :: reqs_khc,reqs_kvc
#endif

!--------------------------------------------------------------
!  lower surface:

      IF( bbc.eq.2 .or. bbc.eq.3 )THEN
        ! no slip (fix/check this later)
        ! semi-slip

        !$omp parallel do default(shared)   &
        !$omp private(i,j)
        do j=1,nj
        do i=1,ni
          ! Pope, pg 288:
!!!          tkea(i,j,1) = 3.33*ust(i,j)*ust(i,j)
          tkea(i,j,1) = 0.0
          kmh(i,j,1) = karman*zntmp(i,j)*ust(i,j)
        enddo
        enddo

      ELSE
        ! free slip

        !$omp parallel do default(shared)   &
        !$omp private(i,j)
        do j=1,nj
        do i=1,ni
          tkea(i,j,1) = 0.0
          kmh(i,j,1) = 0.0
        enddo
        enddo

      ENDIF

        if(timestats.ge.1) time_turb=time_turb+mytime()

        !-----
        call bc2d(tkea(ibt,jbt,1))
#ifdef MPI
        call comm_1s2d_start(tkea(ibt,jbt,1),kvcw1(1,1),kvcw2(1,1),kvce1(1,1),kvce2(1,1),   &
                                             kvcs1(1,1),kvcs2(1,1),kvcn1(1,1),kvcn2(1,1),reqs_kvc)
#endif
        call bc2d(kmh(ibc,jbc,1))
#ifdef MPI
        call comm_1s2d_start(kmh(ibc,jbc,1),khcw1(1,1),khcw2(1,1),khce1(1,1),khce2(1,1),   &
                                            khcs1(1,1),khcs2(1,1),khcn1(1,1),khcn2(1,1),reqs_khc)
        !-----
        call comm_1s2d_end(tkea(ibt,jbt,1),kvcw1(1,1),kvcw2(1,1),kvce1(1,1),kvce2(1,1),   &
                                           kvcs1(1,1),kvcs2(1,1),kvcn1(1,1),kvcn2(1,1),reqs_kvc)
        call bcs2_2d(tkea(ibt,jbt,1))
        call comm_1s2d_end(kmh(ibc,jbc,1),khcw1(1,1),khcw2(1,1),khce1(1,1),khce2(1,1),   &
                                          khcs1(1,1),khcs2(1,1),khcn1(1,1),khcn2(1,1),reqs_khc)
        call bcs2_2d(kmh(ibc,jbc,1))
        !-----
        call comm_2d_corner(tkea(ibt,jbt,1))
        call comm_2d_corner(kmh(ibc,jbc,1))
        !-----
#endif

        !$omp parallel do default(shared)   &
        !$omp private(i,j)
        do j=0,nj+1
        do i=0,ni+1
          kmv(i,j,1) = kmh(i,j,1)
          khv(i,j,1) = kmv(i,j,1)*(khv(i,j,2)/(1.0e-10+kmv(i,j,2)))
          khh(i,j,1) = kmh(i,j,1)*(khh(i,j,2)/(1.0e-10+kmh(i,j,2)))
        enddo
        enddo

        if(timestats.ge.1) time_turb=time_turb+mytime()

!--------------------------------------------------------------
!  cm1r20: upper surface

      IF( tbc.eq.3 )THEN

        !$omp parallel do default(shared)   &
        !$omp private(i,j)
        do j=1,nj
        do i=1,ni
          ! Pope, pg 288:
!!!          tkea(i,j,nk+1) = 3.33*ustt(i,j)*ustt(i,j)
          tkea(i,j,nk+1) = 0.0
          kmh(i,j,nk+1) = karman*zntmp(i,j)*ustt(i,j)
        enddo
        enddo

        if(timestats.ge.1) time_turb=time_turb+mytime()

        !-----
        call bc2d(tkea(ibt,jbt,nk+1))
#ifdef MPI
        call comm_1s2d_start(tkea(ibt,jbt,nk+1),kvcw1(1,1),kvcw2(1,1),kvce1(1,1),kvce2(1,1),   &
                                                kvcs1(1,1),kvcs2(1,1),kvcn1(1,1),kvcn2(1,1),reqs_kvc)
#endif
        call bc2d(kmh(ibc,jbc,nk+1))
#ifdef MPI
        call comm_1s2d_start(kmh(ibc,jbc,nk+1),khcw1(1,1),khcw2(1,1),khce1(1,1),khce2(1,1),   &
                                               khcs1(1,1),khcs2(1,1),khcn1(1,1),khcn2(1,1),reqs_khc)
        !-----
        call comm_1s2d_end(tkea(ibt,jbt,nk+1),kvcw1(1,1),kvcw2(1,1),kvce1(1,1),kvce2(1,1),   &
                                              kvcs1(1,1),kvcs2(1,1),kvcn1(1,1),kvcn2(1,1),reqs_kvc)
        call bcs2_2d(tkea(ibt,jbt,nk+1))
        call comm_1s2d_end(kmh(ibc,jbc,nk+1),khcw1(1,1),khcw2(1,1),khce1(1,1),khce2(1,1),   &
                                             khcs1(1,1),khcs2(1,1),khcn1(1,1),khcn2(1,1),reqs_khc)
        call bcs2_2d(kmh(ibc,jbc,nk+1))
        !-----
        call comm_2d_corner(tkea(ibt,jbt,nk+1))
        call comm_2d_corner(kmh(ibc,jbc,nk+1))
        !-----
#endif

        !$omp parallel do default(shared)   &
        !$omp private(i,j)
        do j=0,nj+1
        do i=0,ni+1
          kmv(i,j,nk+1) = kmh(i,j,nk+1)
          khv(i,j,nk+1) = kmv(i,j,nk+1)*(khv(i,j,nk)/(1.0e-10+kmv(i,j,nk)))
          khh(i,j,nk+1) = kmh(i,j,nk+1)*(khh(i,j,nk)/(1.0e-10+kmh(i,j,nk)))
        enddo
        enddo

        if(timestats.ge.1) time_turb=time_turb+mytime()

      ENDIF


      return
      end subroutine tkebc


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine turbsmag(nstep,rtime,dt,dosfcflx,ruh,rvh,rmh,mf,rmf,th0,thflux,qvflux,rth0s,rf, &
                          nm,defv,defh,dum4  ,grdscl,lenh,zf,zntmp,ust,      &
                          cmz,csz,ce1z,ce2z,                               &
                          kmh,kmv,khh,khv,lenscl,dissten,out3d,            &
                          nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,                 &
                          khcw1,khcw2,khce1,khce2,khcs1,khcs2,khcn1,khcn2, &
                          kvcw1,kvcw2,kvce1,kvce2,kvcs1,kvcs2,kvcn1,kvcn2)
      use input
      use constants
      use bc_module
      use comm_module
      implicit none

      integer, intent(in) :: nstep
      real, intent(in) :: rtime,dt
      logical, intent(in) :: dosfcflx
      real, intent(in), dimension(ib:ie) :: ruh
      real, intent(in), dimension(jb:je) :: rvh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rmh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: mf,rmf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: th0
      real, intent(in), dimension(ib:ie,jb:je) :: thflux,qvflux,rth0s
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: nm,defv,defh
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum4,grdscl,lenh
      real, intent(in), dimension(ib:ie,jb:je) :: zntmp,ust
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(in), dimension(kb:ke) :: cmz,csz,ce1z,ce2z
      real, intent(inout), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv,khh,khv
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: lenscl,dissten
      real, intent(inout) , dimension(ib3d:ie3d,jb3d:je3d,kb3d:ke3d,nout3d) :: out3d
      real, intent(inout), dimension(kmt) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      real, intent(inout), dimension(jmp,kmt) :: khcw1,khcw2,khce1,khce2
      real, intent(inout), dimension(imp,kmt) :: khcs1,khcs2,khcn1,khcn2
      real, intent(inout), dimension(jmp,kmt) :: kvcw1,kvcw2,kvce1,kvce2
      real, intent(inout), dimension(imp,kmt) :: kvcs1,kvcs2,kvcn1,kvcn2

      integer i,j,k
      real :: tem,temx,temy

#ifdef MPI
      integer reqs_khc(8)
      integer reqs_kvc(8)
#endif

!!!      real, parameter :: cs      = 0.18
!!!      real, parameter :: csinv   = 1.0/cs
      real, parameter :: prandtl = 1.0/3.00
      real, parameter :: prinv   = 1.0/prandtl
      real, parameter :: dmin    = 1.0e-10

!-----------------------------------------------------------------------

      temx = 0.125*dx*dx/dt
      temy = 0.125*dy*dy/dt

!$omp parallel do default(shared)   &
!$omp private(i,j,k)
    jloop:  DO j=1,nj

    IF(tconfig.eq.1)THEN
      ! single length scale:  appropriate if dx,dy are nearly the same as dz

      do k=2,nk
      do i=1,ni
        grdscl(i,j,k)=( ((dx*ruh(i))*(dy*rvh(j)))*(dz*rmf(i,j,k)) )**0.33333333
           ! cm1r20.1: revisit near-sfc grid scale at a later date
!!!        grdscl(i,j,k) = sqrt(1.0/( 1.0/(grdscl(i,j,k)**2)                                    &
!!!                                  +1.0/((karman*((zf(i,j,k)-zf(i,j,1))+zntmp(i,j))*csinv)**2)  &
!!!                               ) )
      enddo
      enddo

    ELSEIF(tconfig.eq.2)THEN
      ! two length scales:  one for horizontal, one for vertical

      do i=1,ni
        tem=sqrt( (dx*ruh(i))*(dy*rvh(j)) )
        do k=2,nk
          lenh(i,j,k)=tem
        enddo
      enddo

      do k=2,nk
      do i=1,ni
        grdscl(i,j,k)=dz*rmf(i,j,k)
           ! cm1r20.1: revisit near-sfc grid scale at a later date
!!!        grdscl(i,j,k) = sqrt(1.0/( 1.0/(grdscl(i,j,k)**2)                                    &
!!!                                  +1.0/((karman*((zf(i,j,k)-zf(i,j,1))+zntmp(i,j))*csinv)**2)  &
!!!                               ) )
      enddo
      enddo

    ENDIF

!-----------------------------------------------------------------------

    IF(tconfig.eq.1)THEN

      do k=2,nk
      do i=1,ni
        kmh(i,j,k)=((csz(k)*grdscl(i,j,k))**2)     &
                 *sqrt( max(defv(i,j,k)+defh(i,j,k)-nm(i,j,k)*prinv,dmin) )
!!!        kmh(i,j,k) = min( kmh(i,j,k) , temx*ruh(i)*ruh(i)   &
!!!                                     , temy*rvh(j)*rvh(j) )
        kmv(i,j,k)=kmh(i,j,k)
      enddo
      enddo

    ELSEIF(tconfig.eq.2)THEN

      do k=2,nk
      do i=1,ni
        kmh(i,j,k)=((csz(k)*lenh(i,j,k))**2)     &
                 *sqrt( max(defh(i,j,k),dmin) )
!!!        kmh(i,j,k) = min( kmh(i,j,k) , temx*ruh(i)*ruh(i)   &
!!!                                     , temy*rvh(j)*rvh(j) )
        kmv(i,j,k)=((csz(k)*grdscl(i,j,k))**2)     &
                 *sqrt( max(defv(i,j,k)-nm(i,j,k)*prinv,dmin) )
      enddo
      enddo

    ENDIF

    ENDDO  jloop

    if( nstep.eq.0 .and. myid.eq.0 )then
      print *
!!!      print *,'  cs,csinv = ',cs,csinv
      print *,'  zf,grdscl:'
      i = 1
      j = 1
      do k=2,nk
        print *,k,(zf(i,j,k)-zf(i,j,1)),grdscl(i,j,k)
      enddo
    endif

!--------------------------------------------------------------

      if(timestats.ge.1) time_turb=time_turb+mytime()
      call bcw(kmh,1)
      call bcw(kmv,1)
#ifdef MPI
      call comm_1t_start(kmh,khcw1,khcw2,khce1,khce2,   &
                             khcs1,khcs2,khcn1,khcn2,reqs_khc)
      call comm_1t_start(kmv,kvcw1,kvcw2,kvce1,kvce2,   &
                             kvcs1,kvcs2,kvcn1,kvcn2,reqs_kvc)
      call comm_1t_end(kmh,khcw1,khcw2,khce1,khce2,   &
                           khcs1,khcs2,khcn1,khcn2,reqs_khc)
      call comm_1t_end(kmv,kvcw1,kvcw2,kvce1,kvce2,   &
                           kvcs1,kvcs2,kvcn1,kvcn2,reqs_kvc)
      call getcornert(kmh,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      call getcornert(kmv,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      call bct2(kmh)
      call bct2(kmv)
#endif

!--------------------------------------------------------------

!$omp parallel do default(shared)   &
!$omp private(i,j,k)
    do j=0,nj+1

      do k=1,nk+1
      do i=0,ni+1
        khh(i,j,k)=kmh(i,j,k)*prinv
        ! limit for numerical stability:
!!!        khh(i,j,k) = min( khh(i,j,k) , temx*ruh(i)*ruh(i)   &
!!!                                     , temy*rvh(j)*rvh(j) )
        khv(i,j,k)=kmv(i,j,k)*prinv
      enddo
      enddo

!!!    IF( idiss.eq.1 .or. output_dissten.eq.1 )THEN
    IF( j.ge.1 .and. j.le.nj )THEN
    IF( tconfig.eq.1 )THEN
      do k=2,nk
      do i=1,ni
        dissten(i,j,k) = dissten(i,j,k) + (kmv(i,j,k)**3)/((csz(k)*grdscl(i,j,k))**4)
      enddo
      enddo
    ELSEIF( tconfig.eq.2 )THEN
      do k=2,nk
      do i=1,ni
        dissten(i,j,k) = dissten(i,j,k) + (kmv(i,j,k)**3)/((csz(k)*grdscl(i,j,k))**4)    &
                                        + (kmh(i,j,k)**3)/((csz(k)*lenh(i,j,k))**4)
      enddo
      enddo
    ENDIF
    ENDIF
!!!    ENDIF

    enddo

!--------------------------------------------------------------
!  cm1r18: surface

      IF( bbc.eq.3 )THEN

!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=1,nj
        do i=1,ni
          kmv(i,j,1) = karman*zntmp(i,j)*ust(i,j)
        enddo
        enddo

        call bc2d(kmv(ibc,jbc,1))
#ifdef MPI
        call comm_1s2d_start(kmv(ibc,jbc,1),khcw1(1,1),khcw2(1,1),khce1(1,1),khce2(1,1),   &
                                            khcs1(1,1),khcs2(1,1),khcn1(1,1),khcn2(1,1),reqs_khc)
        call comm_1s2d_end(kmv(ibc,jbc,1),khcw1(1,1),khcw2(1,1),khce1(1,1),khce2(1,1),   &
                                          khcs1(1,1),khcs2(1,1),khcn1(1,1),khcn2(1,1),reqs_khc)
        call comm_2d_corner(kmv(ibc,jbc,1))
        call bcs2_2d(kmv(ibc,jbc,1))
#endif

      IF( tconfig.eq.1 )THEN
!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=0,nj+1
        do i=0,ni+1
          kmh(i,j,1) = kmv(i,j,1)
          khv(i,j,1) = kmv(i,j,1)*prinv
          khh(i,j,1) = khv(i,j,1)
        enddo
        enddo
      ELSEIF( tconfig.eq.2 )THEN
!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=0,nj+1
        do i=0,ni+1
          khv(i,j,1) = kmv(i,j,1)*prinv
        enddo
        enddo
      ENDIF

      ENDIF

!--------------------------------------------------------------

      if(timestats.ge.1) time_turb=time_turb+mytime()

      return
      end subroutine turbsmag


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine turbparam_horiz(nstep,zf,dt,dosfcflx,ruh,rvh,rmh,mf,rmf,th0,thflux,qvflux,rth0s,rf, &
                          nm,defv,defh,lvz,kmh,khh,dissten,out3d,zs,zntmp,ust,xland,psfc,tlh,  &
                          nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,                         &
                          khcw1,khcw2,khce1,khce2,khcs1,khcs2,khcn1,khcn2,         &
                          kvcw1,kvcw2,kvce1,kvce2,kvcs1,kvcs2,kvcn1,kvcn2)
      use input
      use constants
      use bc_module
      use comm_module
      implicit none

      integer, intent(in) :: nstep
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(in) :: dt
      logical, intent(in) :: dosfcflx
      real, intent(in), dimension(ib:ie) :: ruh
      real, intent(in), dimension(jb:je) :: rvh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rmh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: mf,rmf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: th0
      real, intent(in), dimension(ib:ie,jb:je) :: thflux,qvflux,rth0s
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: nm,defv,defh
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: lvz
      real, intent(inout), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,khh
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: dissten
      real, intent(inout) , dimension(ib3d:ie3d,jb3d:je3d,kb3d:ke3d,nout3d) :: out3d
      real, intent(in), dimension(ib:ie,jb:je) :: zs,zntmp,ust,xland,psfc
      real, intent(inout), dimension(ib:ie,jb:je) :: tlh
      real, intent(inout), dimension(kmt) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      real, intent(inout), dimension(jmp,kmt) :: khcw1,khcw2,khce1,khce2
      real, intent(inout), dimension(imp,kmt) :: khcs1,khcs2,khcn1,khcn2
      real, intent(inout), dimension(jmp,kmt) :: kvcw1,kvcw2,kvce1,kvce2
      real, intent(inout), dimension(imp,kmt) :: kvcs1,kvcs2,kvcn1,kvcn2

      integer i,j,k
      real :: tem,tem1,temx,temy

#ifdef MPI
      integer reqs_khc(8)
      integer reqs_kvc(8)
#endif

      real, parameter :: prandtl = 1.0
      real, parameter :: prinv   = 1.0/prandtl
      real, parameter :: dmin    = 1.0e-10

!--------------------------------------------------------------
!  Smagorinsky-type scheme for parameterized turbulence:
!--------------------------------------------------------------
!  Interior:

  lhcheck:  &
  IF( l_h.gt.1.0e-12 .or. lhref1.gt.1.0e-12 .or. lhref2.gt.1.0e-12 )THEN

    if(ny.eq.1)then
      temx =  0.250*dx*dx/dt
      temy = 1000.0*dy*dy/dt
    elseif(nx.eq.1)then
      temx = 1000.0*dx*dx/dt
      temy =  0.250*dy*dy/dt
    else
      temx =  0.125*dx*dx/dt
      temy =  0.125*dy*dy/dt
    endif

    ! cm1r18:
    ! Over water, make tlh a function of surface pressure.
    !   (designed for hurricanes)
    ! Over land, simply set to tlh to l_h.
!$omp parallel do default(shared)   &
!$omp private(i,j)
    do j=1,nj
    do i=1,ni
      IF( (xland(i,j).gt.1.5) .and. (zs(i,j).lt.1.0) )THEN
        ! over water (sea level only):
        tlh(i,j) = lhref2+(lhref1-lhref2)   &
                      *(psfc(i,j)-90000.0)  &
                      /( 101500.0-90000.0)
      ELSE
        ! all other cases:
        tlh(i,j) = l_h
      ENDIF
    enddo
    enddo
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
    do k=2,nk
    do j=1,nj
    do i=1,ni
      kmh(i,j,k)=(tlh(i,j)**2)*sqrt( max(defh(i,j,k),dmin) )
!!!      kmh(i,j,k) = min( kmh(i,j,k) , temx*ruh(i)*ruh(i) , temy*rvh(j)*rvh(j) )
    enddo
    enddo
    enddo

!--------------------------------------------------------------
! boundary conditions:

      if(timestats.ge.1) time_turb=time_turb+mytime()

      call bcw(kmh,1)
#ifdef MPI
      call comm_1t_start(kmh,khcw1,khcw2,khce1,khce2,   &
                             khcs1,khcs2,khcn1,khcn2,reqs_khc)
      call comm_1t_end(kmh,khcw1,khcw2,khce1,khce2,   &
                           khcs1,khcs2,khcn1,khcn2,reqs_khc)
      call getcornert(kmh,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      call bct2(kmh)
#endif

        ! Extrapolate:
        do j=0,nj+1
        do i=0,ni+1
          kmh(i,j,1) = 2.0*kmh(i,j,2) - kmh(i,j,3)
        enddo
        enddo

!--------------------------------------------------------------
!  calculate Kh
!  and also limit horizontal coeffs for numerical stability:

!$omp parallel do default(shared)   &
!$omp private(i,j,k,tem1)
    do j=0,nj+1

      do k=1,nk+1
      do i=0,ni+1
        khh(i,j,k)=kmh(i,j,k)*prinv
!!!        khh(i,j,k) = min( khh(i,j,k) , temx*ruh(i)*ruh(i) , temy*rvh(j)*rvh(j) )
      enddo
      enddo

    IF( idiss.eq.1 .or. output_dissten.eq.1 )THEN
    IF( j.ge.1 .and. j.le.nj )THEN
      do k=2,nk
      do i=1,ni
        dissten(i,j,k) = dissten(i,j,k) + (kmh(i,j,k)**3)/(tlh(i,j)**4)
      enddo
      enddo
    ENDIF
    ENDIF

    enddo

  ENDIF  lhcheck

!--------------------------------------------------------------

      if(timestats.ge.1) time_turb=time_turb+mytime()

      return
      end subroutine turbparam_horiz


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine turbparam_vert(nstep,zf,dt,dosfcflx,ruh,rvh,rmh,mf,rmf,th0,thflux,qvflux,rth0s,rf, &
                          nm,defv,defh,lvz,kmv,khv,dissten,out3d,zs,zntmp,ust,xland,  &
                          nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,                         &
                          khcw1,khcw2,khce1,khce2,khcs1,khcs2,khcn1,khcn2,         &
                          kvcw1,kvcw2,kvce1,kvce2,kvcs1,kvcs2,kvcn1,kvcn2)
      use input
      use constants
      use bc_module
      use comm_module
      implicit none

      integer, intent(in) :: nstep
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(in) :: dt
      logical, intent(in) :: dosfcflx
      real, intent(in), dimension(ib:ie) :: ruh
      real, intent(in), dimension(jb:je) :: rvh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rmh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: mf,rmf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: th0
      real, intent(in), dimension(ib:ie,jb:je) :: thflux,qvflux,rth0s
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: nm,defv,defh
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: lvz
      real, intent(inout), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmv,khv
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: dissten
      real, intent(inout) , dimension(ib3d:ie3d,jb3d:je3d,kb3d:ke3d,nout3d) :: out3d
      real, intent(in), dimension(ib:ie,jb:je) :: zs,zntmp,ust,xland
      real, intent(inout), dimension(kmt) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      real, intent(inout), dimension(jmp,kmt) :: khcw1,khcw2,khce1,khce2
      real, intent(inout), dimension(imp,kmt) :: khcs1,khcs2,khcn1,khcn2
      real, intent(inout), dimension(jmp,kmt) :: kvcw1,kvcw2,kvce1,kvce2
      real, intent(inout), dimension(imp,kmt) :: kvcs1,kvcs2,kvcn1,kvcn2

      integer i,j,k
      real :: rlinf,tem,tem1

#ifdef MPI
      integer reqs_khc(8)
      integer reqs_kvc(8)
#endif

      real, parameter :: prandtl = 1.0
      real, parameter :: prinv   = 1.0/prandtl
      real, parameter :: dmin    = 1.0e-10

!--------------------------------------------------------------
!  Smagorinsky-type scheme for parameterized turbulence:
!--------------------------------------------------------------
!  Interior:

  linfcheck:  &
  IF( l_inf.gt.1.0e-12 )THEN

!!!    tem = 1.0/(1.0e-6+l_inf)
    rlinf = (1.0e-6+l_inf)**(-2)

!$omp parallel do default(shared)   &
!$omp private(i,j,k)
    do k=2,nk
    do j=1,nj
    do i=1,ni
      lvz(i,j,k)=sqrt( ( rlinf + (karman*((zf(i,j,k)-zf(i,j,1))+zntmp(i,j)))**(-2) )**(-1) )
      kmv(i,j,k)=(lvz(i,j,k)**2)*sqrt( max(defv(i,j,k)-nm(i,j,k)*prinv,dmin) )
    enddo
    enddo
    enddo

!--------------------------------------------------------------
! boundary conditions:

      if(timestats.ge.1) time_turb=time_turb+mytime()

      call bcw(kmv,1)
#ifdef MPI
      call comm_1t_start(kmv,kvcw1,kvcw2,kvce1,kvce2,   &
                             kvcs1,kvcs2,kvcn1,kvcn2,reqs_kvc)
      call comm_1t_end(kmv,kvcw1,kvcw2,kvce1,kvce2,   &
                           kvcs1,kvcs2,kvcn1,kvcn2,reqs_kvc)
      call getcornert(kmv,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2)
      call bct2(kmv)
#endif

!--------------------------------------------------------------
!  calculate Kh

!$omp parallel do default(shared)   &
!$omp private(i,j,k,tem1)
    do j=0,nj+1

      do k=1,nk+1
      do i=0,ni+1
        khv(i,j,k)=kmv(i,j,k)*prinv
      enddo
      enddo

    IF( idiss.eq.1 .or. output_dissten.eq.1 )THEN
    IF( j.ge.1 .and. j.le.nj )THEN
      do k=2,nk
      do i=1,ni
        dissten(i,j,k) = dissten(i,j,k) + (kmv(i,j,k)**3)/(lvz(i,j,k)**4)
      enddo
      enddo
    ENDIF
    ENDIF

    enddo

!--------------------------------------------------------------
!  cm1r18: surface

      IF( bbc.eq.3 )THEN

        do j=1,nj
        do i=1,ni
          kmv(i,j,1) = karman*zntmp(i,j)*ust(i,j)
        enddo
        enddo

        call bc2d(kmv(ibc,jbc,1))
#ifdef MPI
        call comm_1s2d_start(kmv(ibc,jbc,1),khcw1(1,1),khcw2(1,1),khce1(1,1),khce2(1,1),   &
                                            khcs1(1,1),khcs2(1,1),khcn1(1,1),khcn2(1,1),reqs_khc)
        call comm_1s2d_end(kmv(ibc,jbc,1),khcw1(1,1),khcw2(1,1),khce1(1,1),khce2(1,1),   &
                                          khcs1(1,1),khcs2(1,1),khcn1(1,1),khcn2(1,1),reqs_khc)
        call comm_2d_corner(kmv(ibc,jbc,1))
        call bcs2_2d(kmv(ibc,jbc,1))
#endif

        do j=0,nj+1
        do i=0,ni+1
          khv(i,j,1) = kmv(i,j,1)*prinv
        enddo
        enddo

      ENDIF

!--------------------------------------------------------------

  ENDIF linfcheck

      if(nstep.eq.1.and.myid.eq.0)then
        print *,'  k,zf,lvz:  znt = ',zntmp(1,1)
        do k=2,nk
          print *,k,(zf(1,1,k)-zf(1,1,1)),lvz(1,1,k)
        enddo
      endif

      if(timestats.ge.1) time_turb=time_turb+mytime()

      return
      end subroutine turbparam_vert


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      ! gettau
      subroutine gettau(xf,rxf,arf1,arf2,ust,stau,u1,v1,s1,ustt,ut,vt,st,rf,mf,dum1,dum2,dum3,dum4,dum5,dum6, &
                        kmh,kmv,t11,t12,t13,t22,t23,t33,ua,ugr,va,vgr,wa,avgsfcu,avgsfcv,avgsfcs,bndy,kbdy)
      use input
      use constants
      use ib_module
      implicit none
      
      real, intent(in), dimension(ib:ie+1) :: xf,rxf,arf1,arf2
      real, intent(in), dimension(ib:ie,jb:je) :: ust,u1,v1,s1,ustt,ut,vt,st
      real, intent(inout), dimension(ib:ie,jb:je) :: stau
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: mf
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2,dum3,dum4,dum5,dum6
      real, intent(in), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: t11,t12,t13,t22,t23,t33
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua,ugr
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va,vgr
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      double precision, intent(in) :: avgsfcu,avgsfcv,avgsfcs
      logical, intent(in), dimension(ibib:ieib,jbib:jeib,kbib:keib) :: bndy
      integer, intent(in), dimension(ibib:ieib,jbib:jeib) :: kbdy
        
      integer i,j,k,kk
      real :: tem,top_cd,side_cd
      logical :: doit

!----------------------------------------------------------------------
!
!  This subroutine calculates the subgrid stress terms.
!
!    t_ij  =  2 * rho * K * S_ij
!
!  NOTE:  upon entering this subroutine, the t_ij arrays must already 
!         contain rho * S_ij  (see calcdef subroutine)
!
!  Since cm1r18, surface stress (ie, surface drag) is incorporated into
!  the stress arrays here.
!
!  Note:  Turbulent viscosities are defined on w points.
!
!  Note:  For axisymmetric simulations, t11 and t12 herein are 
!         actually not stresses:  the actual stresses are
!         combined in a convienent form for the sake of flux-form
!         calculations in the turbu and turbv subroutines.
!         Also note that t22 is never calculated.
!         So, if you need the actual stress components for something, 
!         beware that you will need to re-calculate t11,t12,t22.
!
!----------------------------------------------------------------------

  IF(axisymm.eq.0)THEN

    ! Cartesian grid:

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    do k=1,nk
      do j=0,nj+1
      do i=0,ni+1
        !  2.0 * 0.5 = 1.0
        tem = (kmh(i,j,k)+kmh(i,j,k+1))
        t11(i,j,k)=t11(i,j,k)*tem
        t22(i,j,k)=t22(i,j,k)*tem
        t33(i,j,k)=t33(i,j,k)*tem
        !  2.0 * 0.125 = 0.25
        t12(i,j,k)=t12(i,j,k)*0.25                                            &
     *( ( (kmh(i-1,j-1,k  )+kmh(i,j,k  ))+(kmh(i-1,j,k  )+kmh(i,j-1,k  )) )   &
       +( (kmh(i-1,j-1,k+1)+kmh(i,j,k+1))+(kmh(i-1,j,k+1)+kmh(i,j-1,k+1)) ) )
      enddo
      enddo
    ENDDO

          !-----
          ! lateral boundary conditions:
          if(wbc.eq.3.and.ibw.eq.1)then
            ! free slip b.c.
            do k=1,nk
            do j=1,nj+1
              t12(1,j,k) = t12(2,j,k)
            enddo
            enddo
          endif
          if(ebc.eq.3.and.ibe.eq.1)then
            ! free slip b.c.
            do k=1,nk
            do j=1,nj+1
              t12(ni+1,j,k) = t12(ni,j,k)
            enddo
            enddo
          endif
          !-----
          !-----
          if(sbc.eq.3.and.ibs.eq.1)then
            ! free slip b.c.
            do k=1,nk
            do i=1,ni+1
              t12(i,1,k) = t12(i,2,k)
            enddo
            enddo
          endif
          if(nbc.eq.3.and.ibn.eq.1)then
            ! free slip b.c.
            do k=1,nk
            do i=1,ni+1
              t12(i,nj+1,k) = t12(i,nj,k)
            enddo
            enddo
          endif
          !-----

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    do k=2,nk
      do j=1,nj+1
      do i=1,ni+1
        !  2.0 x 0.5 = 1.0
        t13(i,j,k)=t13(i,j,k)*( kmv(i-1,j,k)+kmv(i,j,k) )
        t23(i,j,k)=t23(i,j,k)*( kmv(i,j-1,k)+kmv(i,j,k) )
      enddo
      enddo
    enddo
            !-----
            ! lateral boundary conditions:
            if(wbc.eq.3.and.ibw.eq.1)then
              ! free slip b.c.
              do k=2,nk
              do j=1,nj
                t13(1,j,k) = t13(2,j,k)
              enddo
              enddo
            endif
            if(ebc.eq.3.and.ibe.eq.1)then
              ! free slip b.c.
              do k=2,nk
              do j=1,nj
                t13(ni+1,j,k) = t13(ni,j,k)
              enddo
              enddo
            endif
            !-----
            !-----
            if(sbc.eq.3.and.ibs.eq.1)then
              ! free slip b.c.
              do k=2,nk
              do i=1,ni
                t23(i,1,k) = t23(i,2,k)
              enddo
              enddo
            endif
            if(nbc.eq.3.and.ibn.eq.1)then
              ! free slip b.c.
              do k=2,nk
              do i=1,ni
                t23(i,nj+1,k) = t23(i,nj,k)
              enddo
              enddo
            endif
            !-----

!------------------------------------

  ELSE

    ! axisymmetric grid:

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    DO k=1,nk
      do j=1,nj
      do i=1,ni+1
        !  2.0 * 0.5 = 1.0
        tem = (kmh(i,j,k)+kmh(i,j,k+1))
        t11(i,j,k)=t11(i,j,k)*tem
        t33(i,j,k)=t33(i,j,k)*tem
        !  2.0 * 0.25  =  0.5
        t12(i,j,k)=0.5*t12(i,j,k)*( arf2(i)*(kmh(i  ,j,k+1)+kmh(i  ,j,k)) &
                                   +arf1(i)*(kmh(i-1,j,k+1)+kmh(i-1,j,k)) )
      enddo
      enddo
    ENDDO

          !-----
          ! lateral boundary conditions:
          j = 1
          if(wbc.eq.3)then
            ! free slip b.c.
            do k=1,nk
!!!              t12(1,j,k) = t12(2,j,k)
              t12(1,j,k) = 0.0
            enddo
          endif
          if(ebc.eq.3)then
            ! free slip b.c.
            do k=1,nk
              t12(ni+1,j,k) = t12(ni,j,k)
            enddo
          endif
          !-----

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    DO k=2,nk
      do j=1,nj
      do i=1,ni+1
        !  2.0 * 0.5  =  1.0
        t13(i,j,k)=t13(i,j,k)*( arf1(i)*kmv(i-1,j,k)+arf2(i)*kmv(i,j,k) )
        t23(i,j,k)=2.0*t23(i,j,k)*kmv(i,j,k)
      enddo
      enddo
    ENDDO

  ENDIF

!------------------------------------------------------------------
!  open boundary conditions:

    IF( wbc.eq.2 .or. ebc.eq.2 .or. sbc.eq.2 .or. nbc.eq.2 )THEN
      DO k=1,nk
        !-----
        IF( wbc.eq.2 .and. ibw.eq.1 )THEN
          do j=0,nj+1
            t11(0,j,k) = t11(1,j,k)
          enddo
        ENDIF
        IF( ebc.eq.2 .and. ibe.eq.1 )THEN
          do j=0,nj+1
            t11(ni+1,j,k) = t11(ni,j,k)
          enddo
        ENDIF
        !-----
        !ccccc
        !-----
        IF( sbc.eq.2 .and. ibs.eq.1 )THEN
          do i=0,ni+1
            t22(i,0,k) = t22(i,1,k)
          enddo
        ENDIF
        IF( nbc.eq.2 .and. ibn.eq.1 )THEN
          do i=0,ni+1
            t22(i,nj+1,k) = t22(i,nj,k)
          enddo
        ENDIF
        !-----
        !ccccc
        !-----
        IF( wbc.eq.2 .and. ibw.eq.1 )THEN
          do j=1,nj+1
            t12(1,j,k) = t12(2,j,k)
          enddo
        ENDIF
        IF( ebc.eq.2 .and. ibe.eq.1 )THEN
          do j=1,nj+1
            t12(ni+1,j,k) = t12(ni,j,k)
          enddo
        ENDIF
        !-----
        IF( sbc.eq.2 .and. ibs.eq.1 )THEN
          do i=1,ni+1
            t12(i,1,k) = t12(i,2,k)
          enddo
        ENDIF
        IF( nbc.eq.2 .and. ibn.eq.1 )THEN
          do i=1,ni+1
            t12(i,nj+1,k) = t12(i,nj,k)
          enddo
        ENDIF
        !-----
        ! corner points:
        !-----
        IF( sbc.eq.2 .and. ibs.eq.1 .and. &
            wbc.eq.2 .and. ibw.eq.1 )THEN
          t12(1,1,k) = t12(2,2,k)
        ENDIF
        IF( sbc.eq.2 .and. ibs.eq.1 .and. &
            ebc.eq.2 .and. ibe.eq.1 )THEN
          t12(ni+1,1,k) = t12(ni,2,k)
        ENDIF
        IF( nbc.eq.2 .and. ibn.eq.1 .and. &
            wbc.eq.2 .and. ibw.eq.1 )THEN
          t12(1,nj+1,k) = t12(2,nj,k)
        ENDIF
        IF( nbc.eq.2 .and. ibn.eq.1 .and. &
            ebc.eq.2 .and. ibe.eq.1 )THEN
          t12(ni+1,nj+1,k) = t12(ni,nj,k)
        ENDIF
        !-----
      ENDDO
    ENDIF

!--------------------------------------------------------------
!  lower boundary conditions

            call sfcstress(xf,rxf,arf1,arf2,ust,stau,u1,v1,s1,rf,mf,dum1,dum2,  &
                        kmh,kmv,t13,t23,ua,ugr,va,vgr,avgsfcu,avgsfcv,avgsfcs)

!--------------------------------------------------------------
!  upper boundary conditions

    IF(tbc.eq.1)THEN
      ! free slip:

!$omp parallel do default(shared)   &
!$omp private(i,j)
      do j=1,nj+1
      do i=1,ni+1
        t13(i,j,nk+1)=t13(i,j,nk)
        t23(i,j,nk+1)=t23(i,j,nk)
      enddo
      enddo

    ELSEIF(tbc.eq.2)THEN
      ! no slip:

      IF(axisymm.eq.0)THEN

!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=1,nj+1
        do i=1,ni+1
          t13(i,j,nk+1) = -2.0*ugr(i,j,nk)*rdz*0.5*( mf(i-1,j,nk+1)+ mf(i,j,nk+1))  &
                                              *0.5*( rf(i-1,j,nk+1)+ rf(i,j,nk+1))  &
                                              *0.5*(kmv(i-1,j,nk  )+kmv(i,j,nk  ))
          t23(i,j,nk+1) = -2.0*vgr(i,j,nk)*rdz*0.5*( mf(i,j-1,nk+1)+ mf(i,j,nk+1))  &
                                              *0.5*( rf(i,j-1,nk+1)+ rf(i,j,nk+1))  &
                                              *0.5*(kmv(i,j-1,nk  )+kmv(i,j,nk  ))
        enddo
        enddo

      ELSE

!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=1,nj+1
        do i=1,ni+1
          t13(i,j,nk+1) = -2.0*ugr(i,j,nk)*rdz*mf(1,1,nk+1)                                       &
                                              *0.5*(arf1(i)*rf(i-1,j,nk+1)+arf2(i)*rf(i,j,nk+1))  &
                                              *0.5*(arf1(i)*kmv(i-1,j,nk)+arf2(i)*kmv(i,j,nk))
          t23(i,j,nk+1) = -2.0*vgr(i,j,nk)*rdz*mf(i,j,nk+1)  &
                                              *rf(i,j,nk+1)  &
                                              *kmv(i,j,nk)
        enddo
        enddo

      ENDIF

    ELSEIF(tbc.eq.3)THEN
      !--------------------------------------------------------!
      !-------- (this is where "drag" is set for tbc=3) -------!

      do j=1,nj+1
      do i=1,ni+1
        t13(i,j,nk+1) = -0.25*( (ustt(i-1,j)**2)*(ut(i-1,j)/max(st(i-1,j),0.01))    &
                               +(ustt(i  ,j)**2)*(ut(i  ,j)/max(st(i  ,j),0.01)) )  &
                             *( rf(i-1,j,nk+1)+rf(i,j,nk+1) )
        t23(i,j,nk+1) = -0.25*( (ustt(i,j-1)**2)*(vt(i,j-1)/max(st(i,j-1),0.01))    &
                               +(ustt(i,j  )**2)*(vt(i,j  )/max(st(i,j  ),0.01)) )  &
                             *( rf(i,j-1,nk+1)+rf(i,j,nk+1) )
      enddo
      enddo

    ENDIF

!--------------------------------------------------------------

    IF( axisymm.eq.1 )THEN
      ! lateral boundary condition:
      !$omp parallel do default(shared)   &
      !$omp private(k)
      do k=0,nk+1
        t13(1,1,k)=0.0
      enddo
    ENDIF

!--------------------------------------------------------------
!  finished

      if(timestats.ge.1) time_turb=time_turb+mytime()
 
      return
      end subroutine gettau
      ! gettau


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine sfcstress(xf,rxf,arf1,arf2,ust,stau,u1,v1,s1,rf,mf,dum1,dum2,  &
                        kmh,kmv,t13,t23,ua,ugr,va,vgr,avgsfcu,avgsfcv,avgsfcs)
      use input
      use constants
      implicit none
      
      real, intent(in), dimension(ib:ie+1) :: xf,rxf,arf1,arf2
      real, intent(in), dimension(ib:ie,jb:je) :: ust,u1,v1,s1
      real, intent(inout), dimension(ib:ie,jb:je) :: stau
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: mf
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2
      real, intent(in), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: t13,t23
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua,ugr
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va,vgr
      double precision, intent(in) :: avgsfcu,avgsfcv,avgsfcs
        
      integer :: i,j,k

    IF(bbc.eq.1)THEN
      ! free slip:

!$omp parallel do default(shared)   &
!$omp private(i,j)
      do j=1,nj+1
      do i=1,ni+1
        t13(i,j,1)=t13(i,j,2)
        t23(i,j,1)=t23(i,j,2)
      enddo
      enddo

    ELSEIF(bbc.eq.2)THEN
      ! no slip:

      IF(axisymm.eq.0)THEN

!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=1,nj+1
        do i=1,ni+1
          t13(i,j,1) = 2.0*ugr(i,j,1)*rdz*0.5*( mf(i-1,j,1)+ mf(i,j,1))  &
                                         *0.5*( rf(i-1,j,1)+ rf(i,j,1))  &
                                         *0.5*(kmv(i-1,j,2)+kmv(i,j,2))
          t23(i,j,1) = 2.0*vgr(i,j,1)*rdz*0.5*( mf(i,j-1,1)+ mf(i,j,1))  &
                                         *0.5*( rf(i,j-1,1)+ rf(i,j,1))  &
                                         *0.5*(kmv(i,j-1,2)+kmv(i,j,2))
        enddo
        enddo

      ELSE

!$omp parallel do default(shared)   &
!$omp private(i,j)
        do j=1,nj+1
        do i=1,ni+1
          t13(i,j,1)=2.0*ugr(i,j,1)*rdz*mf(1,1,1)                                    &
                                       *0.5*(arf1(i)*rf(i-1,j,1)+arf2(i)*rf(i,j,1))  &
                                       *0.5*( arf1(i)*kmv(i-1,j,2)+arf2(i)*kmv(i,j,2) )
          t23(i,j,1)=2.0*vgr(i,j,1)*rdz*mf(i,j,1)  &
                                       *rf(i,j,1)  &
                                       *kmv(i,j,2)
        enddo
        enddo

      ENDIF

    ELSEIF(bbc.eq.3)THEN
      !--------------------------------------------------------!
      !--------  surface stress for semi-slip lower bc --------!
      !-------- (this is where "drag" is set for bbc=3) -------!

      IF(axisymm.eq.0)THEN
        ! Cartesian grid:

        avgsfc:  &
        IF( ( .not. use_avg_sfc ) .or. ( sfcmodel.eq.2 .or. sfcmodel.eq.3 ) )THEN

          !$omp parallel do default(shared)   &
          !$omp private(i,j)
          do j=1,nj+1
          do i=1,ni+1
            t13(i,j,1) = 0.25*( (ust(i-1,j)**2)*(u1(i-1,j)/max(s1(i-1,j),0.01))    &
                               +(ust(i  ,j)**2)*(u1(i  ,j)/max(s1(i  ,j),0.01)) )  &
                             *( rf(i-1,j,1)+rf(i,j,1) )
            t23(i,j,1) = 0.25*( (ust(i,j-1)**2)*(v1(i,j-1)/max(s1(i,j-1),0.01))    &
                               +(ust(i,j  )**2)*(v1(i,j  )/max(s1(i,j  ),0.01)) )  &
                             *( rf(i,j-1,1)+rf(i,j,1) )
          enddo
          enddo

        ELSE  avgsfc

          ! Moeng (1984):

          do j=0,nj+1
          do i=0,ni+1
              dum1(i,j,1) = ust(i,j)*ust(i,j)        &
                           *( s1(i,j)*avgsfcu           &
                             +avgsfcs*(u1(i,j)-avgsfcu) )  &
                           /( max(0.0001,avgsfcs*sqrt( avgsfcu**2 + avgsfcv**2 )) )
              dum2(i,j,1) = ust(i,j)*ust(i,j)        &
                           *( s1(i,j)*avgsfcv           &
                             +avgsfcs*(v1(i,j)-avgsfcv) )  &
                           /( max(0.0001,avgsfcs*sqrt( avgsfcu**2 + avgsfcv**2 )) )
          enddo
          enddo
          do j=1,nj+1
          do i=1,ni+1
            t13(i,j,1) = 0.25*( dum1(i-1,j,1)+dum1(i,j,1) )  &
                             *( rf(i-1,j,1)+rf(i,j,1) )
            t23(i,j,1) = 0.25*( dum2(i,j-1,1)+dum2(i,j,1) )  &
                             *( rf(i,j-1,1)+rf(i,j,1) )
          enddo
          enddo

        ENDIF  avgsfc

      ELSE

        ! axisymmetric grid:
!$omp parallel do default(shared)   &
!$omp private(i,j)
      do j=1,nj+1
      do i=1,ni+1
        t13(i,j,1) = 0.25*( arf1(i)*(ust(i-1,j)**2)*(u1(i-1,j)/max(s1(i-1,j),0.01))    &
                           +arf2(i)*(ust(i  ,j)**2)*(u1(i  ,j)/max(s1(i  ,j),0.01)) )  &
                         *(arf1(i)*rf(i-1,j,1)+arf2(i)*rf(i,j,1))
        t23(i,j,1) = rf(i,j,1)*(ust(i,j)**2)*(v1(i,j)/max(s1(i,j),0.01))
      enddo
      enddo

      ENDIF

      IF( testcase.eq.7 )THEN

        ! Replace normal sfc stress ... use lowest-model-level wind speed
        ! (VanZanten et al 2011, JAMES)

        !$omp parallel do default(shared)   &
        !$omp private(i,j)
        do j=1,nj+1
        do i=1,ni+1
          t13(i,j,1) =     ( 0.001229 * 0.5*(s1(i-1,j)+s1(i,j)) * ugr(i,j,1) )  &
                      *0.5*( rf(i-1,j,1)+rf(i,j,1) )
          t23(i,j,1) =     ( 0.001229 * 0.5*(s1(i,j-1)+s1(i,j)) * vgr(i,j,1) )  &
                      *0.5*( rf(i,j-1,1)+rf(i,j,1) )
        enddo
        enddo

      ENDIF

    ENDIF

    do j=1,nj
    do i=1,ni
      stau(i,j) = sqrt( (0.5*(t13(i,j,1)+t13(i+1,j,1)))**2  &
                       +(0.5*(t23(i,j,1)+t23(i,j+1,1)))**2  &
                      )/rf(i,j,1)
    enddo
    enddo

      end subroutine sfcstress


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      ! calcdef
      subroutine calcdef(rds,sigma,rdsf,sigmaf,zs,gz,rgz,gzu,rgzu,gzv,rgzv,                &
                     xh,rxh,arh1,arh2,uh,xf,rxf,arf1,arf2,uf,vh,vf,mh,c1,c2,mf,defv,defh,  &
                     dum1,dum2,ua,va,wa,s11,s12,s13,s22,s23,s33,gx,gy,rho,rr,rf)
      use input
      use constants
      implicit none

      real, intent(in), dimension(kb:ke) :: rds,sigma
      real, intent(in), dimension(kb:ke+1) :: rdsf,sigmaf
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(in), dimension(itb:ite,jtb:jte) :: gz,rgz,gzu,rgzu,gzv,rgzv
      real, intent(in), dimension(ib:ie) :: xh,rxh,arh1,arh2,uh
      real, intent(in), dimension(ib:ie+1) :: xf,rxf,arf1,arf2,uf
      real, intent(in), dimension(jb:je) :: vh
      real, intent(in), dimension(jb:je+1) :: vf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: mh,c1,c2
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: mf
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: defv,defh
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: s11,s12,s13,s22,s23,s33
      real, intent(in), dimension(itb:ite,jtb:jte,ktb:kte) :: gx,gy
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rho,rr,rf
        
      integer :: i,j,k
      real :: r1,r2,r3,r4
      real :: tmp11,tmp22,tmp33,tmp12,tmp13,tmp23,rrf
      real :: temz

!----------------------------------------------------------------------
!
!  This subroutine calculates the strain rate terms
!
!    S_ij  =  0.5 * ( d(u_i)/d(x_j) + d(u_j)/d(x_i) )
!
!  (note: multiplied by density herein)
!  and then uses these variables to calculate deformation.
!
!  Note:
!  Since cm1r18, surface stress (ie, surface drag) is no longer 
!  calculated in this subroutine.  See gettau subroutine instead.
!
!  Note:  For axisymmetric simulations, s11 and s12 herein are 
!         actually not rate-of-strain components:  the actual 
!         components have been combined mathematically in a 
!         way to be consistent with the flux-form calculations 
!         in the turbu and turbv subroutines.
!         Also note that s22 is never calculated.
!         So, if you need the actual strain components for something, 
!         beware that you will need to re-calculate s11,s12,s22.
!
!----------------------------------------------------------------------

  IF(.not.terrain_flag)THEN

  IF( axisymm.eq.0 )THEN
    ! Cartesian without terrain:

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,temz)
    DO k=1,nk

      temz = rdz*mh(1,1,k)

      do j=0,nj+1
      do i=0,ni+1 
        s11(i,j,k)=rho(i,j,k)*(ua(i+1,j,k)-ua(i,j,k))*rdx*uh(i)
        s22(i,j,k)=rho(i,j,k)*(va(i,j+1,k)-va(i,j,k))*rdy*vh(j)
        s33(i,j,k)=rho(i,j,k)*(wa(i,j,k+1)-wa(i,j,k))*temz
        s12(i,j,k)=0.5*( (ua(i,j,k)-ua(i,j-1,k))*rdy*vf(j)   &
                        +(va(i,j,k)-va(i-1,j,k))*rdx*uf(i) ) &
              *0.25*( (rho(i-1,j-1,k)+rho(i,j,k))+(rho(i-1,j,k)+rho(i,j-1,k)) )
      enddo
      enddo       

    ENDDO

          !-----
          ! lateral boundary conditions:
          if(wbc.eq.3.and.ibw.eq.1)then
            ! free slip b.c.
            do k=1,nk
            do j=1,nj+1
              s12(1,j,k) = s12(2,j,k)
            enddo
            enddo
          elseif(wbc.eq.4.and.ibw.eq.1)then
            ! no slip b.c.
            i = 1
            do k=1,nk
            do j=1,nj+1
              s12(1,j,k) = 2.0*va(1,j,k)*rdx*uf(1)   &
                   *0.25*( (rho(i-1,j-1,k)+rho(i,j,k))+(rho(i-1,j,k)+rho(i,j-1,k)) )
            enddo
            enddo
          endif
          if(ebc.eq.3.and.ibe.eq.1)then
            ! free slip b.c.
            do k=1,nk
            do j=1,nj+1
              s12(ni+1,j,k) = s12(ni,j,k)
            enddo
            enddo
          elseif(ebc.eq.4.and.ibe.eq.1)then
            ! no slip b.c.
            i = ni+1
            do k=1,nk
            do j=1,nj+1
              s12(ni+1,j,k) = -2.0*va(ni,j,k)*rdx*uf(ni+1)   &
                   *0.25*( (rho(i-1,j-1,k)+rho(i,j,k))+(rho(i-1,j,k)+rho(i,j-1,k)) )
            enddo
            enddo
          endif
          !-----
          !-----
          if(sbc.eq.3.and.ibs.eq.1)then
            ! free slip b.c.
            do k=1,nk
            do i=1,ni+1
              s12(i,1,k) = s12(i,2,k)
            enddo
            enddo
          elseif(sbc.eq.4.and.ibs.eq.1)then
            ! no slip b.c.
            j = 1
            do k=1,nk
            do i=1,ni+1
              s12(i,1,k) = 2.0*ua(i,1,k)*rdy*vf(1)   &
                   *0.25*( (rho(i-1,j-1,k)+rho(i,j,k))+(rho(i-1,j,k)+rho(i,j-1,k)) )
            enddo
            enddo
          endif
          if(nbc.eq.3.and.ibn.eq.1)then
            ! free slip b.c.
            do k=1,nk
            do i=1,ni+1
              s12(i,nj+1,k) = s12(i,nj,k)
            enddo
            enddo
          elseif(nbc.eq.4.and.ibn.eq.1)then
            ! no slip b.c.
            j = nj+1
            do k=1,nk
            do i=1,ni+1
              s12(i,nj+1,k) = -2.0*ua(i,nj,k)*rdy*vf(nj+1)   &
                   *0.25*( (rho(i-1,j-1,k)+rho(i,j,k))+(rho(i-1,j,k)+rho(i,j-1,k)) )
            enddo
            enddo
          endif
          !-----

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,temz)
    DO k=2,nk

      do j=1,nj+1
      do i=1,ni+1 
        s13(i,j,k)=0.5*( (wa(i,j,k)-wa(i-1,j,k))*rdx*uf(i)   &
                        +(ua(i,j,k)-ua(i,j,k-1))*rdz*0.5*(mf(i-1,j,k)+mf(i,j,k))  &
                       )*0.5*( rf(i-1,j,k)+rf(i,j,k) )
        s23(i,j,k)=0.5*( (wa(i,j,k)-wa(i,j-1,k))*rdy*vf(j)   &
                        +(va(i,j,k)-va(i,j,k-1))*rdz*0.5*(mf(i,j-1,k)+mf(i,j,k))  &
                       )*0.5*( rf(i,j-1,k)+rf(i,j,k) )
      enddo
      enddo       

    ENDDO

            !-----
            ! lateral boundary conditions:
            if(wbc.eq.3.and.ibw.eq.1)then
              ! free slip b.c.
              do k=2,nk
              do j=1,nj
                s13(1,j,k) = s13(2,j,k)
              enddo
              enddo
            elseif(wbc.eq.4.and.ibw.eq.1)then
              ! no slip b.c.
              do k=2,nk
              do j=1,nj
                s13(1,j,k) = 2.0*wa(1,j,k)*rdx*uf(1)
              enddo
              enddo
            endif
            if(ebc.eq.3.and.ibe.eq.1)then
              ! free slip b.c.
              do k=2,nk
              do j=1,nj
                s13(ni+1,j,k) = s13(ni,j,k)
              enddo
              enddo
            elseif(ebc.eq.4.and.ibe.eq.1)then
              ! no slip b.c.
              do k=2,nk
              do j=1,nj
                s13(ni+1,j,k) = -2.0*wa(ni,j,k)*rdx*uf(ni+1)
              enddo
              enddo
            endif
            !-----

            !-----
            if(sbc.eq.3.and.ibs.eq.1)then
              ! free slip b.c.
              do k=2,nk
              do i=1,ni
                s23(i,1,k) = s23(i,2,k)
              enddo
              enddo
            elseif(sbc.eq.4.and.ibs.eq.1)then
              ! no slip b.c.
              do k=2,nk
              do i=1,ni
                s23(i,1,k) = 2.0*wa(i,1,k)*rdy*vf(1)
              enddo
              enddo
            endif
            if(nbc.eq.3.and.ibn.eq.1)then
              ! free slip b.c.
              do k=2,nk
              do i=1,ni
                s23(i,nj+1,k) = s23(i,nj,k)
              enddo
              enddo
            elseif(nbc.eq.4.and.ibn.eq.1)then
              ! no slip b.c.
              do k=2,nk
              do i=1,ni
                s23(i,nj+1,k) = -2.0*wa(i,nj,k)*rdy*vf(nj+1)
              enddo
              enddo
            endif
            !-----

!-------------------------------------------------------------------------------

  ELSE
    ! axisymmetric:

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k)
    DO k=1,nk

      do j=1,nj
      do i=1,ni+1
        s11(i,j,k)=rho(i,j,k)*(ua(i+1,j,k)*arf1(i+1)-ua(i,j,k)*arf2(i))*rdx*uh(i)
        s33(i,j,k)=rho(i,j,k)*(wa(i,j,k+1)-wa(i,j,k))*rdz*mh(1,1,k)
        !  0.5 * 0.5  =  0.25
        s12(i,j,k)=0.25*(arf1(i)*rho(i-1,j,k)+arf2(i)*rho(i,j,k))   &
                       *(arh1(i)*va(i,j,k)-arh2(i-1)*va(i-1,j,k))*rdx*uf(i)
      enddo
      enddo

    ENDDO

          !-----
          ! lateral boundary conditions:
          j = 1
          if(wbc.eq.3)then
            ! free slip b.c.
            do k=1,nk
!!!              s12(1,j,k) = s12(2,j,k)
              s12(1,j,k) = 0.0
            enddo
          elseif(wbc.eq.4)then
            ! no slip b.c.
            i = 1
            do k=1,nk
              s12(1,j,k) = 2.0*va(1,j,k)*rdx*uf(1)   &
                        *0.5*(arf1(i)*rho(i-1,j,k)+arf2(i)*rho(i,j,k))
            enddo
          endif
          if(ebc.eq.3)then
            ! free slip b.c.
            do k=1,nk
              s12(ni+1,j,k) = s12(ni,j,k)
            enddo
          elseif(ebc.eq.4)then
            ! no slip b.c.
            i = ni+1
            do k=1,nk
              s12(ni+1,j,k) = -2.0*va(ni,j,k)*rdx*uf(ni+1)   &
                        *0.5*(arf1(i)*rho(i-1,j,k)+arf2(i)*rho(i,j,k))
            enddo
          endif
          !-----

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k)
    DO k=2,nk

      do j=1,nj
      do i=1,ni+1
        !  0.5 * 0.5  =  0.25
        s13(i,j,k)=0.25*(arf1(i)*rf(i-1,j,k)+arf2(i)*rf(i,j,k))  &
                       *( (ua(i,j,k)-ua(i,j,k-1))*rdz*mf(1,1,k)  &
                         +(wa(i,j,k)-wa(i-1,j,k))*rdx*uf(i) )
        s23(i,j,k)=0.5*rf(i,j,k)*(va(i,j,k)-va(i,j,k-1))*rdz*mf(1,1,k)
      enddo
      enddo

    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
!  Cartesian with terrain:

  ELSE

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

!$omp parallel do default(shared)   &
!$omp private(i,j,k,r1)
    DO k=1,nk
      do j=0,nj+1
      do i=0,ni+1 
        s11(i,j,k)=gz(i,j)*(ua(i+1,j,k)*rgzu(i+1,j)-ua(i,j,k)*rgzu(i,j))*rdx*uh(i) &
                  +( gx(i,j,k+1)*(dum1(i,j,k+1)+dum1(i+1,j,k+1))      &
                    -gx(i,j,k  )*(dum1(i,j,k  )+dum1(i+1,j,k  ))      &
                   )*0.5*rdsf(k)
        s11(i,j,k)=s11(i,j,k)*rho(i,j,k)
        s22(i,j,k)=gz(i,j)*(va(i,j+1,k)*rgzv(i,j+1)-va(i,j,k)*rgzv(i,j))*rdy*vh(j) &
                  +( gy(i,j,k+1)*(dum2(i,j,k+1)+dum2(i,j+1,k+1))      &
                    -gy(i,j,k  )*(dum2(i,j,k  )+dum2(i,j+1,k  ))      &
                   )*0.5*rdsf(k)
        s22(i,j,k)=s22(i,j,k)*rho(i,j,k)
        s33(i,j,k)=(wa(i,j,k+1)-wa(i,j,k))*rdsf(k)*gz(i,j)
        s33(i,j,k)=s33(i,j,k)*rho(i,j,k)
      enddo
      enddo
      do j=1,nj+1 
      do i=1,ni+1
        r1 = 0.25*( ( rho(i-1,j-1,k)*gz(i-1,j-1)   &
                     +rho(i  ,j  ,k)*gz(i  ,j  ) ) &
                   +( rho(i-1,j  ,k)*gz(i-1,j  )   &
                     +rho(i  ,j-1,k)*gz(i  ,j-1) ) )
        s12(i,j,k)=0.5*(                                                         &
                   ( r1*(ua(i,j,k)*rgzu(i,j)-ua(i,j-1,k)*rgzu(i,j-1))*rdy*vf(j)  &
                    +0.5*( (zt-sigmaf(k+1))*(dum1(i,j-1,k+1)+dum1(i,j,k+1))      &
                          -(zt-sigmaf(k  ))*(dum1(i,j-1,k  )+dum1(i,j,k  ))      &
                         )*rdsf(k)*r1*(rgzu(i,j)-rgzu(i,j-1))*rdy*vf(j) )        &
                  +( r1*(va(i,j,k)*rgzv(i,j)-va(i-1,j,k)*rgzv(i-1,j))*rdx*uf(i)  &
                    +0.5*( (zt-sigmaf(k+1))*(dum2(i-1,j,k+1)+dum2(i,j,k+1))      &
                          -(zt-sigmaf(k  ))*(dum2(i-1,j,k  )+dum2(i,j,k  ))      &
                         )*rdsf(k)*r1*(rgzv(i,j)-rgzv(i-1,j))*rdx*uf(i) )    )
      enddo
      enddo       
    ENDDO

    ! now, dum1 stores w at scalar-pts:
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
    DO k=1,nk
      do j=0,nj+1
      do i=0,ni+1
        dum1(i,j,k)=0.5*(wa(i,j,k)+wa(i,j,k+1))
      enddo
      enddo
    ENDDO
!$omp parallel do default(shared)   &
!$omp private(i,j,k)
    DO k=2,nk
      do j=1,nj
      do i=1,ni+1
        s13(i,j,k)=0.5*(                                                              &
                   (ua(i,j,k)-ua(i,j,k-1))*rds(k)                                     &
                  +(wa(i,j,k)*rgz(i,j)-wa(i-1,j,k)*rgz(i-1,j))*rdx*uf(i)              &
                  +0.5*rds(k)*( (zt-sigma(k  ))*(dum1(i,j,k  )+dum1(i-1,j,k  ))       &
                               -(zt-sigma(k-1))*(dum1(i,j,k-1)+dum1(i-1,j,k-1)) )     &
                             *(rgz(i,j)-rgz(i-1,j))*rdx*uf(i)                         )
        s13(i,j,k)=s13(i,j,k)*0.5*( gz(i-1,j)*rf(i-1,j,k)+gz(i,j)*rf(i,j,k) )
      enddo
      enddo
      do j=1,nj+1   
      do i=1,ni
        s23(i,j,k)=0.5*(                                                              &
                   (va(i,j,k)-va(i,j,k-1))*rds(k)                                     &
                  +(wa(i,j,k)*rgz(i,j)-wa(i,j-1,k)*rgz(i,j-1))*rdy*vf(j)              &
                  +0.5*rds(k)*( (zt-sigma(k  ))*(dum1(i,j,k  )+dum1(i,j-1,k  ))       &
                               -(zt-sigma(k-1))*(dum1(i,j,k-1)+dum1(i,j-1,k-1)) )     &
                             *(rgz(i,j)-rgz(i,j-1))*rdy*vf(j)                         )
        s23(i,j,k)=s23(i,j,k)*0.5*( gz(i,j-1)*rf(i,j-1,k)+gz(i,j)*rf(i,j,k) )
      enddo
      enddo
    ENDDO

  ENDIF

!  end of calculations for terrain
!-------------------------------------------------------------------------------
!  open boundary conditions:

    IF( wbc.eq.2 .or. ebc.eq.2 .or. sbc.eq.2 .or. nbc.eq.2 )THEN
      DO k=1,nk
        !-----
        IF( wbc.eq.2 .and. ibw.eq.1 )THEN
          do j=0,nj+1
            s11(0,j,k) = s11(1,j,k)
          enddo
        ENDIF
        IF( ebc.eq.2 .and. ibe.eq.1 )THEN
          do j=0,nj+1
            s11(ni+1,j,k) = s11(ni,j,k)
          enddo
        ENDIF
        !-----
        !ccccc
        !-----
        IF( sbc.eq.2 .and. ibs.eq.1 )THEN
          do i=0,ni+1
            s22(i,0,k) = s22(i,1,k)
          enddo
        ENDIF
        IF( nbc.eq.2 .and. ibn.eq.1 )THEN
          do i=0,ni+1
            s22(i,nj+1,k) = s22(i,nj,k)
          enddo
        ENDIF
        !-----
        !ccccc
        !-----
        IF( wbc.eq.2 .and. ibw.eq.1 )THEN
          do j=1,nj+1
            s12(1,j,k) = s12(2,j,k)
          enddo
        ENDIF
        IF( ebc.eq.2 .and. ibe.eq.1 )THEN
          do j=1,nj+1
            s12(ni+1,j,k) = s12(ni,j,k)
          enddo
        ENDIF
        !-----
        IF( sbc.eq.2 .and. ibs.eq.1 )THEN
          do i=1,ni+1
            s12(i,1,k) = s12(i,2,k)
          enddo
        ENDIF
        IF( nbc.eq.2 .and. ibn.eq.1 )THEN
          do i=1,ni+1
            s12(i,nj+1,k) = s12(i,nj,k)
          enddo
        ENDIF
        !-----
        ! corner points:
        !-----
        IF( sbc.eq.2 .and. ibs.eq.1 .and. &
            wbc.eq.2 .and. ibw.eq.1 )THEN
          s12(1,1,k) = s12(2,2,k)
        ENDIF
        IF( sbc.eq.2 .and. ibs.eq.1 .and. &
            ebc.eq.2 .and. ibe.eq.1 )THEN
          s12(ni+1,1,k) = s12(ni,2,k)
        ENDIF
        IF( nbc.eq.2 .and. ibn.eq.1 .and. &
            wbc.eq.2 .and. ibw.eq.1 )THEN
          s12(1,nj+1,k) = s12(2,nj,k)
        ENDIF
        IF( nbc.eq.2 .and. ibn.eq.1 .and. &
            ebc.eq.2 .and. ibe.eq.1 )THEN
          s12(ni+1,nj+1,k) = s12(ni,nj,k)
        ENDIF
        !-----
      ENDDO
    ENDIF

!----------------------------------------------------------------------
!  if l_h or l_v is zero, set appropriate terms to zero:
!    (just to be sure)

!    IF( horizturb.eq.1 .or. ipbl.eq.2 )THEN
!    IF( l_h*lhref1*lhref2.lt.1.0e-12 )THEN
!!$omp parallel do default(shared)   &
!!$omp private(i,j,k)
!      do k=0,nk+1
!      do j=0,nj+1
!      do i=0,ni+1
!        s11(i,j,k) = 0.0
!        s12(i,j,k) = 0.0
!        s33(i,j,k) = 0.0
!        s22(i,j,k) = 0.0
!      enddo
!      enddo
!      enddo
!    ENDIF
!    ENDIF

!    IF( horizturb.eq.1 .or. ipbl.eq.2 )THEN
!    IF( l_inf.lt.tsmall )THEN
!!$omp parallel do default(shared)   &
!!$omp private(i,j,k)
!      do k=0,nk+1
!      do j=0,nj+1
!      do i=0,ni+1
!        s13(i,j,k) = 0.0
!        s23(i,j,k) = 0.0
!      enddo
!      enddo
!      enddo
!    ENDIF
!    ENDIF

!--------------------------------------------------------------

    IF( axisymm.eq.1 )THEN
      ! lateral boundary condition:
!$omp parallel do default(shared)   &
!$omp private(k)
      do k=0,nk+1
        s13(1,1,k)=0.0
      enddo
    ENDIF

!----------------------------------------------------------------------
!  calculate deformation:
!  Note:  deformation is defined at w points.

    IF(axisymm.eq.0)THEN
      ! Cartesian domain:

      ! Def = 2.0 * S_ij * S_ij
      !
      !     = 2.0 * (  S11*S11 + S12*S12 + S13*S13 
      !              + S21*S21 + S22*S22 + S23*S23 
      !              + S31*S31 + S32*S32 + S33*S33 )
      !
      !     =   2.0*( S11*S11 + S22*S22 + S33*S33 )
      !       + 4.0*( S12*S12 + S13*S13 + S23*S23 )

!$omp parallel do default(shared)   &
!$omp private(i,j,k,tmp11,tmp22,tmp33,tmp12,tmp13,tmp23,rrf)
      do k=2,nk
      do j=1,nj
      do i=1,ni

        tmp11=( c1(i,j,k)*s11(i,j,k-1)**2 + c2(i,j,k)*s11(i,j,k)**2 )
        tmp22=( c1(i,j,k)*s22(i,j,k-1)**2 + c2(i,j,k)*s22(i,j,k)**2 )
        tmp33=( c1(i,j,k)*s33(i,j,k-1)**2 + c2(i,j,k)*s33(i,j,k)**2 )

        tmp12=0.25*( c1(i,j,k)*( ( s12(i,j  ,k-1)**2 + s12(i+1,j+1,k-1)**2 )     &
                               + ( s12(i,j+1,k-1)**2 + s12(i+1,j  ,k-1)**2 ) )   &
                    +c2(i,j,k)*( ( s12(i,j  ,k  )**2 + s12(i+1,j+1,k  )**2 )     &
                               + ( s12(i,j+1,k  )**2 + s12(i+1,j  ,k  )**2 ) ) )

        tmp13=0.5*( s13(i,j,k)**2 + s13(i+1,j,k)**2 )

        tmp23=0.5*( s23(i,j,k)**2 + s23(i,j+1,k)**2 )

        rrf = 1.0/(rf(i,j,k)**2)

        defv(i,j,k)= 4.0*( tmp13 + tmp23 )*rrf

        defh(i,j,k) = ( 2.0*( ( tmp11 + tmp22 ) + tmp33 ) + 4.0*tmp12 )*rrf

      enddo
      enddo
      enddo

!--------------------------------------------
    ELSE
      ! axisymmetric domain:

!$omp parallel do default(shared)   &
!$omp private(i,j,k,tmp11,tmp22,tmp33,tmp12,tmp13,tmp23,rrf,r1,r2,r3,r4)
      do k=2,nk
      do j=1,nj
      do i=1,ni

        tmp11=( c1(1,1,k)*(s11(i,j,k-1)**2) + c2(1,1,k)*(s11(i,j,k)**2) )
        tmp33=( c1(1,1,k)*(s33(i,j,k-1)**2) + c2(1,1,k)*(s33(i,j,k)**2) )

        tmp12=0.5*(  c1(1,1,k)*( s12(i,j  ,k-1)**2 + s12(i+1,j  ,k-1)**2 )     &
                   + c2(1,1,k)*( s12(i,j  ,k  )**2 + s12(i+1,j  ,k  )**2 ) )

        tmp13=0.5*( s13(i,j,k)**2 + s13(i+1,j,k)**2 )

        tmp23=      s23(i,j,k)**2

        rrf = 1.0/(rf(i,j,k)**2)

        defv(i,j,k)= 4.0*( tmp13 + tmp23 )*rrf

        defh(i,j,k) = ( 2.0*( tmp11 + tmp33 ) + 4.0*tmp12 )*rrf

      enddo
      enddo
      enddo

    ENDIF  ! endif for axisymm


!--------------------------------------------------------------
!  finished

      if(timestats.ge.1) time_turb=time_turb+mytime()

      return
      end subroutine calcdef
      ! calcdef


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine calcnm(c1,c2,mf,pi0,thv0,th0,cloudvar,nm,t,qt,thv,cloud,rh,qvci,   &
                        prs,pp,th,qa,iamsat)
      use input
      use constants
      use cm1libs , only : rslf,rsif
      implicit none

      logical, intent(in), dimension(maxq) :: cloudvar
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: c1,c2
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: mf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: pi0,thv0,th0
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: nm
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: t,qt,thv,cloud,rh,qvci
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: prs,pp,th
      real, intent(in), dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: qa
      logical, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: iamsat

      integer :: i,j,k,n
      real :: pavg,tavg,qtavg,qvs,lh,cpml,gamma,drdt
      real :: nml,nmi,ff,qvsl,qvsi,qvciavg,qv,ql,qi

      real, parameter :: nmsat = 1.0e-10

!----------------------------------------------------------------------
!  Dry nm

    IF(imoist.eq.0)then

      !$omp parallel do default(shared)  &
      !$omp private(i,j,k)
      do k=2,nk
      do j=1,nj
      do i=1,ni
        nm(i,j,k)=alog( (th0(i,j,k)+th(i,j,k))/(th0(i,j,k-1)+th(i,j,k-1)) ) &
                    *g*rdz*mf(i,j,k)
      enddo
      enddo
      enddo

      !$omp parallel do default(shared)  &
      !$omp private(i,j)
      do j=1,nj
      do i=1,ni
        nm(i,j,   1)=0.0
        nm(i,j,nk+1)=0.0
      enddo
      enddo

!-----------------------------------------------------------------------
!  Moist nm

    ELSE

      !$omp parallel do default(shared)  &
      !$omp private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        qvci(i,j,k) = qa(i,j,k,nqv) + qa(i,j,k,nqc)
        qt(i,j,k) = 0.0
      enddo
      enddo
      enddo

      IF( iice.eq.1 )THEN
      !$omp parallel do default(shared)  &
      !$omp private(i,j,k)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        qvci(i,j,k) = qvci(i,j,k) + qa(i,j,k,nqi)
      enddo
      enddo
      enddo
      ENDIF

      DO n=1,numq
        IF( (n.eq.nqv) .or.                                 &
            (n.ge.nql1.and.n.le.nql2) .or.                  &
            (n.ge.nqs1.and.n.le.nqs2.and.iice.eq.1) )THEN
          !$omp parallel do default(shared)  &
          !$omp private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            qt(i,j,k)=qt(i,j,k)+qa(i,j,k,n)
          enddo
          enddo
          enddo
        ENDIF
      ENDDO

      k = 1

      !$omp parallel do default(shared)  &
      !$omp private(i,j)
      do j=1,nj
      do i=1,ni
        t(i,j,k)=(th0(i,j,k)+th(i,j,k))*(pi0(i,j,k)+pp(i,j,k))
        thv(i,j,k)=(th0(i,j,k)+th(i,j,k))*(1.0+reps*qa(i,j,k,nqv))   &
                                         /(1.0+qt(i,j,k))
      enddo
      enddo

    icecheck:  &
    IF( iice.eq.1 )THEN

      do k=2,nk
      do j=1,nj
      do i=1,ni
        t(i,j,k)=(th0(i,j,k)+th(i,j,k))*(pi0(i,j,k)+pp(i,j,k))
        thv(i,j,k)=(th0(i,j,k)+th(i,j,k))*(1.0+reps*qa(i,j,k,nqv))   &
                                         /(1.0+qt(i,j,k))
        ! subsaturated formulation:
        nm(i,j,k)=g*alog(thv(i,j,k)/thv(i,j,k-1))*rdz*mf(i,j,k)

        ! saturated formulation (if necessary):
        pavg = c1(i,j,k)*prs(i,j,k-1)+c2(i,j,k)*prs(i,j,k)
        tavg =   c1(i,j,k)*t(i,j,k-1)+  c2(i,j,k)*t(i,j,k)
        qtavg=  c1(i,j,k)*qt(i,j,k-1)+ c2(i,j,k)*qt(i,j,k)
        qvciavg = c1(i,j,k)*qvci(i,j,k-1)+ c2(i,j,k)*qvci(i,j,k)

        if( tavg.ge.273.15 )then
          qvs = rslf(pavg,tavg)
          ql = max( qvciavg - qvs , 0.0 )
          if( ql.gt.nmsat )then
            iamsat(i,j,k) = .true.
            qv = max( qvciavg - ql , 0.0 )
            cpml=cp+cpv*qv+cpl*ql
            lh=lv1-lv2*tavg
            drdt=17.67*(273.15-29.65)*qvs/((tavg-29.65)**2)
            gamma=g*(1.0+qtavg)*(1.0+lh*qvs/(rd*tavg))/(cpml+lh*drdt)
            nm(i,j,k)=g*( ( alog(t(i,j,k)/t(i,j,k-1))*rdz*mf(i,j,k)      &
                              +gamma/tavg )*(1.0+tavg*drdt/(eps+qvs))   &
                           -alog((1.0+qt(i,j,k))/(1.0+qt(i,j,k-1)))*rdz*mf(i,j,k) )
          endif
        elseif( tavg.le.233.15 )then
          qvs = rsif(pavg,tavg)
          qi = max( qvciavg - qvs , 0.0 )
          if( qi.gt.nmsat )then
            iamsat(i,j,k) = .true.
            qv = max( qvciavg - qi , 0.0 )
            cpml=cp+cpv*qv+cpi*qi
            lh=ls1-ls2*tavg
            drdt=21.8745584*(273.15-7.66)*qvs/((tavg-7.66)**2)
            gamma=g*(1.0+qtavg)*(1.0+lh*qvs/(rd*tavg))/(cpml+lh*drdt)
            nm(i,j,k)=g*( ( alog(t(i,j,k)/t(i,j,k-1))*rdz*mf(i,j,k)      &
                              +gamma/tavg )*(1.0+tavg*drdt/(eps+qvs))   &
                           -alog((1.0+qt(i,j,k))/(1.0+qt(i,j,k-1)))*rdz*mf(i,j,k) )
          endif
        else
          ff = (tavg-233.15)/(273.15-233.15)
          qvsl = rslf(pavg,tavg)
          qvsi = rsif(pavg,tavg)
          qvs = ff*qvsl + (1.0-ff)*qvsi
          qi = max( 0.0 , (1.0-ff)*(qvciavg-qvs) )
          ql = max( 0.0 , ff*(qvciavg-qvs) )
          if( (ql+qi).gt.nmsat )then
            iamsat(i,j,k) = .true.
            qv = max( 0.0 , qvciavg - ql - qi )
            cpml=cp+cpv*qv+cpl*ql+cpi*qi

            lh=lv1-lv2*tavg
            drdt=17.67*(273.15-29.65)*qvsl/((tavg-29.65)**2)
            gamma=g*(1.0+qtavg)*(1.0+lh*qvsl/(rd*tavg))/(cpml+lh*drdt)
            nml=g*( ( alog(t(i,j,k)/t(i,j,k-1))*rdz*mf(i,j,k)      &
                              +gamma/tavg )*(1.0+tavg*drdt/(eps+qvsl))   &
                           -alog((1.0+qt(i,j,k))/(1.0+qt(i,j,k-1)))*rdz*mf(i,j,k) )

            lh=ls1-ls2*tavg
            drdt=21.8745584*(273.15-7.66)*qvsi/((tavg-7.66)**2)
            gamma=g*(1.0+qtavg)*(1.0+lh*qvsi/(rd*tavg))/(cpml+lh*drdt)
            nmi=g*( ( alog(t(i,j,k)/t(i,j,k-1))*rdz*mf(i,j,k)      &
                              +gamma/tavg )*(1.0+tavg*drdt/(eps+qvsi))   &
                           -alog((1.0+qt(i,j,k))/(1.0+qt(i,j,k-1)))*rdz*mf(i,j,k) )

            nm(i,j,k) = ff*nml + (1.0-ff)*nmi
          endif
        endif
      enddo
      enddo
      enddo

    ELSE  icecheck

      ! liquid only:
      do k=2,nk
      do j=1,nj
      do i=1,ni
        t(i,j,k)=(th0(i,j,k)+th(i,j,k))*(pi0(i,j,k)+pp(i,j,k))
        thv(i,j,k)=(th0(i,j,k)+th(i,j,k))*(1.0+reps*qa(i,j,k,nqv))   &
                                         /(1.0+qt(i,j,k))
        ! subsaturated formulation:
        nm(i,j,k)=g*alog(thv(i,j,k)/thv(i,j,k-1))*rdz*mf(i,j,k)

        ! saturated formulation (if necessary):
        pavg = c1(i,j,k)*prs(i,j,k-1)+c2(i,j,k)*prs(i,j,k)
        tavg =   c1(i,j,k)*t(i,j,k-1)+  c2(i,j,k)*t(i,j,k)
        qtavg=  c1(i,j,k)*qt(i,j,k-1)+ c2(i,j,k)*qt(i,j,k)
        qvciavg = c1(i,j,k)*qvci(i,j,k-1)+ c2(i,j,k)*qvci(i,j,k)

          qvs = rslf(pavg,tavg)
          ql = max( qvciavg - qvs , 0.0 )
          if( ql.gt.nmsat )then
            iamsat(i,j,k) = .true.
            qv = max( qvciavg - ql , 0.0 )
            cpml=cp+cpv*qv+cpl*ql
            lh=lv1-lv2*tavg
            drdt=17.67*(273.15-29.65)*qvs/((tavg-29.65)**2)
            gamma=g*(1.0+qtavg)*(1.0+lh*qvs/(rd*tavg))/(cpml+lh*drdt)
            nm(i,j,k)=g*( ( alog(t(i,j,k)/t(i,j,k-1))*rdz*mf(i,j,k)      &
                              +gamma/tavg )*(1.0+tavg*drdt/(eps+qvs))   &
                           -alog((1.0+qt(i,j,k))/(1.0+qt(i,j,k-1)))*rdz*mf(i,j,k) )
          endif
      enddo
      enddo
      enddo

    ENDIF  icecheck

      !$omp parallel do default(shared)  &
      !$omp private(i,j)
      do j=1,nj
      do i=1,ni
        nm(i,j,   1)=0.0
        nm(i,j,nk+1)=0.0
      enddo
      enddo

    ENDIF    ! endif for imoist

!----------------------------------------------------------------------

      if(timestats.ge.1) time_turb=time_turb+mytime()

      return
      end subroutine calcnm


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine  t2psmm(dt,rtime,xf,rxf,c1,c2,                                   &
                         zh,mh,zf,mf,rf0,rr0,rho0,rrf0,u0,v0,                     &
                         uavg,vavg,savg,l2p,kmw,gamk,gamwall,s2p,s2b,t2pm1,t2pm2,t2pm3,t2pm4,cavg,  &
                         ua,va,wa,t11,t12,t13,t22,t23,t33,                        &
                         dum1,dum2,s13 ,s23 ,up  ,vp  ,fru ,frv ,                 &
                         ust,zntmp,stau,rho,rr,rf,kmv,kmh,                        &
                         ugr ,vgr ,uf,vf,arf1,arf2,                               &
                         u1,v1,s1,u1b,v1b,avgsfcu,avgsfcv,avgsfcs,                &
                         reqs_s,uw31,uw32,ue31,ue32,us31,us32,un31,un32)

      use input
      use constants , only : karman
#ifdef MPI
      use mpi
#endif
      implicit none

      real, intent(in) :: dt,rtime
      real, intent(in), dimension(ib:ie+1) :: xf,rxf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: c1,c2,zh,mh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf,mf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rf0,rr0,rho0,rrf0
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: u0
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: v0
      real, intent(inout), dimension(kb:ke) :: uavg,vavg,savg,l2p,kmw,gamk,s2p,s2b,t2pm1,t2pm2,t2pm3,t2pm4
      real, intent(in), dimension(kb:ke) :: gamwall
      double precision, intent(inout), dimension(kb:ke,3+numq) :: cavg
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: ugr
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: vgr
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: t11,t12,t13,t22,t23,t33
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2,s13,s23,up,vp,fru,frv
      real, intent(in), dimension(ib:ie,jb:je) :: ust,zntmp
      real, intent(inout), dimension(ib:ie,jb:je) :: stau
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rho,rr,rf
      real, intent(in), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmv,kmh
      real, intent(in), dimension(ib:ie+1) :: uf
      real, intent(in), dimension(jb:je+1) :: vf
      real, intent(in), dimension(ib:ie+1) :: arf1,arf2
      real, intent(in), dimension(ib:ie,jb:je) :: u1,v1,s1
      real, intent(inout), dimension(kb:ke) :: u1b,v1b
      double precision, intent(inout) :: avgsfcu,avgsfcv,avgsfcs
      integer, intent(inout), dimension(rmp) :: reqs_s
      real, intent(inout), dimension(cmp,jmp,kmp)   :: uw31,uw32,ue31,ue32
      real, intent(inout), dimension(imp+1,cmp,kmp) :: us31,us32,un31,un32

      integer :: i,j,k,n
      real :: tmp11,tmp22,tmp33,tmp12,tmp13,tmp23,wbar,rratio,fstar,tem,temx,temy
      real :: d2pm1,d2pm2,d2pm3,d2pm4,d2pm5,ueo,veo,dsdz,rdsdz,dsdzu,dsdzv,dudz,dvdz,s13b,s23b
      double precision :: temd,teme,savgt,t13avg,t23avg,d13avg,d23avg,s13avg,s23avg,z13avg,z23avg,  &
                          ustbar,zntbar,kavgt,rustbar,ubar,vbar,ufrc,vfrc,ravg,rfavg,rflast,spavg

      real, dimension(kb:ke) :: wspa
      double precision, dimension(kb:ke) :: shravg

    !-------------------------------------------

    doingt2p:  &
    if( t2pfac.ge.0.001 )then

        kmw = 0.0

    !-------------------------------------------
    !  Get domain average profiles:

      uavg = 0.0
      vavg = 0.0

      do k=1,nk
        !----
        cavg(k,1) = 0.0
        cavg(k,2) = 0.0
        !----
        do j=1,nj
        do i=1,ni
          cavg(k,1) = cavg(k,1) + ugr(i,j,k)
          cavg(k,2) = cavg(k,2) + vgr(i,j,k)
        enddo
        enddo
        !----
      enddo

#ifdef MPI
      call MPI_ALLREDUCE(MPI_IN_PLACE,cavg(1,1),nk,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,cavg(1,2),nk,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif

      temd = 1.0/dble(nx*ny)

      do k=1,nk
        uavg(k)  = cavg(k,1)*temd
        vavg(k)  = cavg(k,2)*temd
        savg(k)  = max( sqrt( uavg(k)**2 + vavg(k)**2 ) , 1.0e-10 )
        u1b(k) = uavg(k)
        v1b(k) = vavg(k)
      enddo

    !-------------------------------------------
    !  surface vars:

            call sfcstress(xf,rxf,arf1,arf2,ust,stau,u1,v1,s1,rf,mf,dum1,dum2,  &
                        kmh,kmv,s13,s23,ua,ugr,va,vgr,avgsfcu,avgsfcv,avgsfcs)

        ustbar = 0.0
        zntbar = 0.0

        do j=1,nj
        do i=1,ni
          ustbar = ustbar + sqrt(sqrt( (0.5*(s13(i,j,1)+s13(i+1,j,1)))**2  &
                                      +(0.5*(s23(i,j,1)+s23(i,j+1,1)))**2  &
                                     )/rf(i,j,1))
          zntbar = zntbar + zntmp(i,j)
        enddo
        enddo

#ifdef MPI
        call MPI_ALLREDUCE(MPI_IN_PLACE,ustbar,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,zntbar,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif

        teme = 1.0/dble(nx*ny)

        ustbar = ustbar*teme
        zntbar = zntbar*teme

        if(myid.eq.0) print *,'  zntbar,ustbar,ust = ',zntbar,ustbar,ust(1,1)

        do k=1,(ntwk+1)
          wspa(k) = (ustbar/karman)*alog( sngl(zh(1,1,k)/zntbar) )
        enddo

    !-------------------------------------------
    !  Get new value of gamma:

    do k=2,ntwk

      spavg = 0.0
      s13b = 0.5*(uavg(k)-uavg(k-1))*rdz*mf(1,1,k)
      s23b = 0.5*(vavg(k)-vavg(k-1))*rdz*mf(1,1,k)

      do j=1,nj
      do i=1,ni

        tmp11=( c1(i,j,k)*(t11(i,j,k-1)**2) + c2(i,j,k)*(t11(i,j,k)**2) )
        tmp22=( c1(i,j,k)*(t22(i,j,k-1)**2) + c2(i,j,k)*(t22(i,j,k)**2) )
        tmp33=( c1(i,j,k)*(t33(i,j,k-1)**2) + c2(i,j,k)*(t33(i,j,k)**2) )

        tmp12=0.25*( c1(i,j,k)*( ( (t12(i  ,j  ,k-1)**2)     &
                                 + (t12(i+1,j+1,k-1)**2) )   &
                               + ( (t12(i  ,j+1,k-1)**2)     &
                                 + (t12(i+1,j  ,k-1)**2) ) ) &
                    +c2(i,j,k)*( ( (t12(i  ,j  ,k  )**2)     &
                                 + (t12(i+1,j+1,k  )**2) )   &
                               + ( (t12(i  ,j+1,k  )**2)     &
                                 + (t12(i+1,j  ,k  )**2) ) ) )

        tmp13=0.5*( (t13(i,j,k)-s13b)**2 + (t13(i+1,j,k)-s13b)**2 )

        tmp23=0.5*( (t23(i,j,k)-s23b)**2 + (t23(i,j+1,k)-s23b)**2 )

        spavg = spavg + ( 4.0*( tmp12 + ( tmp13 + tmp23 ) ) + 2.0*( ( tmp11 + tmp22 ) + tmp33 ) )/(rf(i,j,k)**2)

      enddo
      enddo

#ifdef MPI
      call MPI_ALLREDUCE(MPI_IN_PLACE,spavg,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif

      spavg = sqrt( spavg*teme )

      shravg(k) = sqrt( 4.0*( s13b**2 + s23b**2 ) )

      gamk(k) = spavg/( smeps + (spavg+gamwall(k)*shravg(k)) )

      s2p(k) = spavg
      s2b(k) = shravg(k)

    enddo

    gamk(1) = gamk(2)

    !-------------------------------------------


      rustbar = 1.0/max( 0.0001 , ustbar)

      kloops94:  &
      DO k = 2 , 2

        ubar = 0.0
        vbar = 0.0
        t13avg = 0.0
        t23avg = 0.0
        d13avg = 0.0
        d23avg = 0.0
        s13avg = 0.0
        s23avg = 0.0
        kavgt = 0.0

        temx = 0.0
        temy = 0.0

        ! Note:  here, we assume wavg = 0

      IF( k.eq.2 )THEN
        ! Get avg u,v at w level 2:
        ! (use same algorithms as in adv_routines)
        do j=1,nj
        do i=1,ni
          !-----
          wbar = 0.5*(wa(i,j,k)+wa(i-1,j,k))
          IF( wbar.ge.0.0 )THEN
            up(i,j,k) = ( c1(1,1,k)*ua(i,j,k-1) &
                         +c2(1,1,k)*ua(i,j,k  ) )
          ELSE
            up(i,j,k) = flx3(ua(i,j,k+1),ua(i,j,k),ua(i,j,k-1))
          ENDIF
          ubar = ubar+up(i,j,k)
          !-----
          wbar = 0.5*(wa(i,j,k)+wa(i,j-1,k))
          IF( wbar.ge.0.0 )THEN
            vp(i,j,k) = ( c1(1,1,k)*va(i,j,k-1) &
                         +c2(1,1,k)*va(i,j,k  ) )
          ELSE
            vp(i,j,k) = flx3(va(i,j,k+1),va(i,j,k),va(i,j,k-1))
          ENDIF
          vbar = vbar+vp(i,j,k)
          !-----
        enddo
        enddo
      ELSE
        stop 87632
      ENDIF

#ifdef MPI
        call MPI_ALLREDUCE(MPI_IN_PLACE,ubar,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,vbar,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif

        ! avg u,v at w level 2:
        ubar = ubar*teme
        vbar = vbar*teme

      IF( k.eq.2 )THEN
        ! Get <u'w'> and <v'w'> at w level 2:
        ! (use same algorithms as in adv_routines)
        do j=1,nj
        do i=1,ni
          !-----
          ueo = ( c1(1,1,k)*ua(i,j,k-1)+c2(1,1,k)*ua(i,j,k  ) )
          wbar = rf0(1,1,k)*0.5*(wa(i,j,k)+wa(i-1,j,k))
          fru(i,j,k) = wbar*(ueo-ubar)
          IF( wbar.lt.0.0 )THEN
            fru(i,j,k) = fru(i,j,k) + wbar*(up(i,j,k)-ueo)
          ENDIF
          !-----
          veo = ( c1(1,1,k)*va(i,j,k-1)+c2(1,1,k)*va(i,j,k  ) )
          wbar = rf0(1,1,k)*0.5*(wa(i,j,k)+wa(i,j-1,k))
          frv(i,j,k) = wbar*(veo-vbar)
          IF( wbar.lt.0.0 )THEN
            frv(i,j,k) = frv(i,j,k) + wbar*(vp(i,j,k)-veo)
          ENDIF
          !-----
          t13avg = t13avg + fru(i,j,k)
          t23avg = t23avg + frv(i,j,k)
        enddo
        enddo
      ELSE
        stop 32987
      ENDIF

        do j=1,nj
        do i=1,ni
!!!          kavgt = kavgt + gamk(k)*kmv(i,j,k)
          kavgt = kavgt + kmv(i,j,k)
        enddo
        enddo

#ifdef MPI
        call MPI_ALLREDUCE(MPI_IN_PLACE,t13avg,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,t23avg,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,kavgt ,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif

        t13avg = t13avg*teme
        t23avg = t23avg*teme
        kavgt = kavgt*teme

        !-----
        ! Get kmw:

        rdsdz = 1.0/max( 1.0e-20 , (wspa(k)-wspa(k-1))*rdz*mf(1,1,k) )

        d2pm1 = -kavgt
        d2pm2 = -rdsdz*sqrt( t13avg**2 + t23avg**2 )
        d2pm3 =  0.0
        d2pm4 =  0.0

!!!        kmw(k) = t2pfac*max( 0.0 , ustbar*karman*zf(1,1,2) + d2pm1 + d2pm2 )
        kmw(k) = t2pfac*max( 0.0 , ustbar*ustbar*rdsdz + d2pm1 + d2pm2 )

        kmw(k) = min( kmw(k) , ustbar*karman*zf(1,1,2) )

        t2pm1(k) = d2pm2
        t2pm2(k) = d2pm1
!!!        t2pm3(k) = ustbar*karman*zf(1,1,2)
        t2pm3(k) = ustbar*ustbar*rdsdz

      ENDDO  kloops94

        l2p(1) = zntbar
        l2p(2) = sqrt( kmw(2)*karman*zf(1,1,2)*rustbar )

        do k=3,ntwk
          l2p(k) = l2p(2)
        enddo

        do k=3,ntwk
!!!          kmw(k) = gamwall(k)*l2p(k)*l2p(k)*shravg(k)
          kmw(k) = l2p(k)*l2p(k)*shravg(k)
        enddo


    !-------------------------------------------

    endif  doingt2p

      if(timestats.ge.1) time_turb=time_turb+mytime()

      end subroutine t2psmm


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine t2pcode(dt,rtime,xf,rxf,c1,c2,zh,mh,zf,mf,rf0,rr0,rho0,rrf0,u0,v0,            &
                         uavg,vavg,savg,l2p,kmw,gamwall,gamk,s2p,s2b,t2pm1,t2pm2,t2pm3,t2pm4,cavg,  &
                         ua,va,wa,t11,t12,t13,t22,t23,t33,dum1,dum2,s13 ,s23 ,                 &
                         ust,zntmp,stau,rho,rr,rf,kmh,kmv,                                     &
                         ugr ,fsu  ,u2pt,vgr ,fsv  ,v2pt,uf,vf,arf1,arf2,                      &
                         u1,v1,s1,u1b,v1b,                                                     &
                         avgsfcu,avgsfcv,avgsfcs,                                              &
                         sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,reqs_s)

      use input
      use constants , only : karman
#ifdef MPI
      use mpi
#endif
      implicit none

      real, intent(in) :: dt,rtime
      real, intent(in), dimension(ib:ie+1) :: xf,rxf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: c1,c2,zh,mh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf,mf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rf0,rr0,rho0,rrf0
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: u0
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: v0
      real, intent(inout), dimension(kb:ke) :: uavg,vavg,savg,l2p,kmw,t2pm1,t2pm2,t2pm3,t2pm4
      real, intent(in), dimension(kb:ke) :: gamwall
      real, intent(inout), dimension(kb:ke) :: gamk,s2p,s2b
      double precision, intent(inout), dimension(kb:ke,3+numq) :: cavg
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: ugr,fsu
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: vgr,fsv
      real, intent(in), dimension(ib2pt:ie2pt,jb2pt:je2pt,kb2pt:ke2pt) :: u2pt,v2pt
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: t11,t12,t13,t22,t23,t33
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2,s13,s23
      real, intent(in), dimension(ib:ie,jb:je) :: ust,zntmp
      real, intent(inout), dimension(ib:ie,jb:je) :: stau
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rho,rr,rf
      real, intent(in), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv
      real, intent(in), dimension(ib:ie+1) :: uf
      real, intent(in), dimension(jb:je+1) :: vf
      real, intent(in), dimension(ib:ie+1) :: arf1,arf2
      real, intent(in), dimension(ib:ie,jb:je) :: u1,v1,s1
      real, intent(inout), dimension(kb:ke) :: u1b,v1b
      double precision, intent(in) :: avgsfcu,avgsfcv,avgsfcs
      real, intent(inout), dimension(cmp,jmp,kmp)   :: sw31,sw32,se31,se32
      real, intent(inout), dimension(imp,cmp,kmp)   :: ss31,ss32,sn31,sn32
      integer, intent(inout), dimension(rmp) :: reqs_s

      integer :: i,j,k,n
      real :: tmp11,tmp22,tmp33,tmp12,tmp13,tmp23,wbar,rratio,fstar,tem,temx,temy
      real :: d2pm1,d2pm2,d2pm3,d2pm4,d2pm5,ueo,veo,dsdz,dsdzu,dsdzv,dudz,dvdz,up,vp,s13b,s23b
      double precision :: temd,teme,savgt,t13avg,t23avg,d13avg,d23avg,s13avg,s23avg,z13avg,z23avg,  &
                          ustbar,zntbar,kavgt,rustbar,ubar,vbar,ufrc,vfrc,ravg,rfavg,rflast,shravg,spavg

      real, dimension(kb:ke) :: wspa

    !-------------------------------------------

      kmw = 0.0
      l2p = 0.0
      wspa = 0.0

    doingt2p:  &
    if( t2pfac.ge.0.001 )then

      if( myid.eq.0 ) print *,'  t2pfac = ',t2pfac

    !-------------------------------------------

        call     sfcstress(xf,rxf,arf1,arf2,ust,stau,u1,v1,s1,rf,mf,dum1,dum2,  &
                        kmh,kmv,s13,s23,ua,ugr,va,vgr,avgsfcu,avgsfcv,avgsfcs)

        do j=1,nj+1
        do i=1,ni+1
          fsu(i,j,1) = s13(i,j,1)
          fsv(i,j,1) = s23(i,j,1)
        enddo
        enddo

    !-------------------------------------------
    !  surface vars:

        ustbar = 0.0
        zntbar = 0.0

        do j=1,nj
        do i=1,ni
!!!          ustbar = ustbar + ust(i,j)
          ustbar = ustbar + sqrt(sqrt( (0.5*(s13(i,j,1)+s13(i+1,j,1)))**2  &
                                      +(0.5*(s23(i,j,1)+s23(i,j+1,1)))**2  &
                                     )/rf(i,j,1))
          zntbar = zntbar + zntmp(i,j)
        enddo
        enddo

#ifdef MPI
        call MPI_ALLREDUCE(MPI_IN_PLACE,ustbar,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,zntbar,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif

        teme = 1.0/dble(nx*ny)

        ustbar = ustbar*teme
        zntbar = zntbar*teme

        do k=1,(ntwk+1)
          wspa(k) = (ustbar/karman)*alog( sngl(zh(1,1,k)/zntbar) )
        enddo

    !-------------------------------------------

      ! new formulation (accounts for all forcing terms in u,v equations)

      kloop:  &
      DO k=1,(ntwk-1)

        s13avg = 0.0
        s23avg = 0.0
        t13avg = 0.0
        t23avg = 0.0
        d13avg = 0.0
        d23avg = 0.0
        rfavg = 0.0

!!!        tem = 0.25*gamk(k+1)
        tem = 0.25

        s13b = 0.5*(uavg(k)-uavg(k-1))*rdz*mf(1,1,k)
        s23b = 0.5*(vavg(k)-vavg(k-1))*rdz*mf(1,1,k)

        do j=1,nj
        do i=1,ni

          fsu(i,j,k+1) = ( (wa(i,j,k+1)-wa(i-1,j,k+1))*rdx*uf(i)        &
                          +(ua(i,j,k+1)-ua(i,j,k))*rdz*mf(1,1,k+1) )  &
                        *( kmv(i-1,j,k+1)+kmv(i,j,k+1) )*( rf(i-1,j,k+1)+rf(i,j,k+1) )*tem
          fsv(i,j,k+1) = ( (wa(i,j,k+1)-wa(i,j-1,k+1))*rdy*vf(j)        &
                          +(va(i,j,k+1)-va(i,j,k))*rdz*mf(1,1,k+1) )  &
                        *( kmv(i,j-1,k+1)+kmv(i,j,k+1) )*( rf(i,j-1,k+1)+rf(i,j,k+1) )*tem

          temx = rdz*mh(1,1,k)/(0.5*(rho(i-1,j,k)+rho(i,j,k)))
          temy = rdz*mh(1,1,k)/(0.5*(rho(i,j-1,k)+rho(i,j,k)))

          ! standard part of subgrid model:
          s13avg = s13avg + (fsu(i,j,k+1)-fsu(i,j,k))*temx
          s23avg = s23avg + (fsv(i,j,k+1)-fsv(i,j,k))*temy

          ! everything else in governing equations (other than subgrid turbulence):
          t13avg = t13avg + u2pt(i,j,k)
          t23avg = t23avg + v2pt(i,j,k)

          ! known part of 2p term:
          d13avg = d13avg + (0.0-s13b*kmw(k))*( rf(i-1,j,k)+rf(i,j,k) )*temx
          d23avg = d23avg + (0.0-s23b*kmw(k))*( rf(i,j-1,k)+rf(i,j,k) )*temy

          rfavg = rfavg + rf(i,j,k+1)*rr(i,j,k)

        enddo
        enddo

#ifdef MPI
        call MPI_ALLREDUCE(MPI_IN_PLACE,s13avg,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,s23avg,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,t13avg,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,t23avg,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,d13avg,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,d23avg,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
        call MPI_ALLREDUCE(MPI_IN_PLACE,rfavg ,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif

        s13avg = s13avg*teme
        s23avg = s23avg*teme
        t13avg = t13avg*teme
        t23avg = t23avg*teme
        d13avg = d13avg*teme
        d23avg = d23avg*teme
        rfavg = rfavg*teme

    !-------------------------------------------
    !  Get kmw:

        d2pm1 =  ( uavg(k)*t13avg + vavg(k)*t23avg )/savg(k)
        d2pm2 =  ( uavg(k)*s13avg + vavg(k)*s23avg )/savg(k)

      if( k.eq.1 )then
        d2pm3 = 0.0
      else
        d2pm3 =  ( uavg(k)*d13avg + vavg(k)*d23avg )/savg(k)
      endif

        dsdz = (wspa(k+1)-wspa(k))*rdz*mf(1,1,k+1)
        tem = t2pfac*gamwall(k+1)/( rdz*mh(1,1,k)*max(1.0e-12,dsdz)*rfavg )

        ! 180903:
        kmw(k+1) = max( 0.0 , ( 0.0 - d2pm1 - d2pm2 - d2pm3 )*tem )

        t2pm1(k+1) = -d2pm1*tem
        t2pm2(k+1) = -d2pm2*tem
        t2pm3(k+1) = -d2pm3*tem

      ENDDO  kloop

        ! diagnostic only:
        l2p(1) = zntbar
        do k=2,ntwk
          l2p(k) = sqrt( kmw(k)*karman*(zf(1,1,k)-zntbar)/max(0.0001,ustbar) )
        enddo

    !-------------------------------------------

    endif  doingt2p

      if(timestats.ge.1) time_turb=time_turb+mytime()

      end subroutine t2pcode


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine getgamk(dt,rtime,xf,rxf,c1,c2,zh,mh,zf,mf,rf0,rr0,rho0,rrf0,u0,v0,            &
                         uavg,vavg,savg,l2p,kmw,gamwall,gamk,s2p,s2b,t2pm1,t2pm2,t2pm3,t2pm4,cavg,  &
                         ua,va,wa,t11,t12,t13,t22,t23,t33,dum1,dum2,s13 ,s23 ,                 &
                         ust,zntmp,stau,rho,rr,rf,kmh,kmv,                                     &
                         ugr ,fsu  ,u2pt,vgr ,fsv  ,v2pt,uf,vf,arf1,arf2,                      &
                         u1,v1,s1,u1b,v1b,                                                     &
                         avgsfcu,avgsfcv,avgsfcs,                                              &
                         sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,reqs_s)

      use input
      use constants , only : karman
#ifdef MPI
      use mpi
#endif
      implicit none

      real, intent(in) :: dt,rtime
      real, intent(in), dimension(ib:ie+1) :: xf,rxf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: c1,c2,zh,mh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf,mf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rf0,rr0,rho0,rrf0
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: u0
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: v0
      real, intent(inout), dimension(kb:ke) :: uavg,vavg,savg,l2p,kmw,t2pm1,t2pm2,t2pm3,t2pm4
      real, intent(in), dimension(kb:ke) :: gamwall
      real, intent(inout), dimension(kb:ke) :: gamk,s2p,s2b
      double precision, intent(inout), dimension(kb:ke,3+numq) :: cavg
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: ugr,fsu
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: vgr,fsv
      real, intent(in), dimension(ib2pt:ie2pt,jb2pt:je2pt,kb2pt:ke2pt) :: u2pt,v2pt
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: t11,t12,t13,t22,t23,t33
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2,s13,s23
      real, intent(in), dimension(ib:ie,jb:je) :: ust,zntmp
      real, intent(inout), dimension(ib:ie,jb:je) :: stau
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rho,rr,rf
      real, intent(in), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv
      real, intent(in), dimension(ib:ie+1) :: uf
      real, intent(in), dimension(jb:je+1) :: vf
      real, intent(in), dimension(ib:ie+1) :: arf1,arf2
      real, intent(in), dimension(ib:ie,jb:je) :: u1,v1,s1
      real, intent(inout), dimension(kb:ke) :: u1b,v1b
      double precision, intent(in) :: avgsfcu,avgsfcv,avgsfcs
      real, intent(inout), dimension(cmp,jmp,kmp)   :: sw31,sw32,se31,se32
      real, intent(inout), dimension(imp,cmp,kmp)   :: ss31,ss32,sn31,sn32
      integer, intent(inout), dimension(rmp) :: reqs_s

      integer :: i,j,k,n
      real :: tmp11,tmp22,tmp33,tmp12,tmp13,tmp23,wbar,rratio,fstar,tem,temx,temy
      real :: d2pm1,d2pm2,d2pm3,d2pm4,d2pm5,ueo,veo,dsdz,dsdzu,dsdzv,dudz,dvdz,up,vp,s13b,s23b
      double precision :: temd,teme,savgt,t13avg,t23avg,d13avg,d23avg,s13avg,s23avg,z13avg,z23avg,  &
                          ustbar,zntbar,kavgt,rustbar,ubar,vbar,ufrc,vfrc,ravg,rfavg,rflast,shravg,spavg

      real, dimension(kb:ke) :: wspa


    !-------------------------------------------

      u1b = 0.0
      v1b = 0.0
      savg = 0.0

    doinggamk:  &
    if( t2pfac.ge.0.001 )then

    !-------------------------------------------
    !  Get domain average profiles:

      uavg = 0.0
      vavg = 0.0

      do k=1,nk
        !----
        cavg(k,1) = 0.0
        cavg(k,2) = 0.0
        !----
        do j=1,nj
        do i=1,ni
          cavg(k,1) = cavg(k,1) + ugr(i,j,k)
          cavg(k,2) = cavg(k,2) + vgr(i,j,k)
        enddo
        enddo
        !----
      enddo

#ifdef MPI
      call MPI_ALLREDUCE(MPI_IN_PLACE,cavg(1,1),nk,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(MPI_IN_PLACE,cavg(1,2),nk,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif

      temd = 1.0/dble(nx*ny)

      do k=1,nk
        uavg(k)  = cavg(k,1)*temd
        vavg(k)  = cavg(k,2)*temd
        savg(k)  = max( sqrt( uavg(k)**2 + vavg(k)**2 ) , 1.0e-10 )
        u1b(k) = uavg(k)
        v1b(k) = vavg(k)
      enddo

      teme = 1.0/dble(nx*ny)

    !-------------------------------------------
    !  Get new value of gamma:

    do k=2,ntwk

      spavg = 0.0
      s13b = 0.5*(uavg(k)-uavg(k-1))*rdz*mf(1,1,k)
      s23b = 0.5*(vavg(k)-vavg(k-1))*rdz*mf(1,1,k)

      do j=1,nj
      do i=1,ni

        tmp11=( c1(i,j,k)*(t11(i,j,k-1)**2) + c2(i,j,k)*(t11(i,j,k)**2) )
        tmp22=( c1(i,j,k)*(t22(i,j,k-1)**2) + c2(i,j,k)*(t22(i,j,k)**2) )
        tmp33=( c1(i,j,k)*(t33(i,j,k-1)**2) + c2(i,j,k)*(t33(i,j,k)**2) )

        tmp12=0.25*( c1(i,j,k)*( ( (t12(i  ,j  ,k-1)**2)     &
                                 + (t12(i+1,j+1,k-1)**2) )   &
                               + ( (t12(i  ,j+1,k-1)**2)     &
                                 + (t12(i+1,j  ,k-1)**2) ) ) &
                    +c2(i,j,k)*( ( (t12(i  ,j  ,k  )**2)     &
                                 + (t12(i+1,j+1,k  )**2) )   &
                               + ( (t12(i  ,j+1,k  )**2)     &
                                 + (t12(i+1,j  ,k  )**2) ) ) )

        tmp13=0.5*( (t13(i,j,k)-s13b)**2 + (t13(i+1,j,k)-s13b)**2 )

        tmp23=0.5*( (t23(i,j,k)-s23b)**2 + (t23(i,j+1,k)-s23b)**2 )

        spavg = spavg + ( 4.0*( tmp12 + ( tmp13 + tmp23 ) ) + 2.0*( ( tmp11 + tmp22 ) + tmp33 ) )/(rf(i,j,k)**2)

      enddo
      enddo

#ifdef MPI
      call MPI_ALLREDUCE(MPI_IN_PLACE,spavg,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
#endif

      spavg = sqrt( spavg*teme )

      shravg = sqrt( 4.0*( s13b**2 + s23b**2 ) )

      gamk(k) = spavg/( smeps + (spavg+gamwall(k)*shravg) )

      s2p(k) = spavg
      s2b(k) = shravg

    enddo

    gamk(1) = gamk(2)

    endif  doinggamk

      if(timestats.ge.1) time_turb=time_turb+mytime()

      end subroutine getgamk

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      !dir$ attributes forceinline :: flx3
      real function flx3(s1,s2,s3)
      implicit none

      real, intent(in) :: s1,s2,s3

      ! 3rd-order flux (eg, Wicker and Skamarock, 2002, MWR)

      flx3 = (  (-1.0/6.0)*s1  &
               +( 5.0/6.0)*s2  &
               +( 2.0/6.0)*s3  )

      end function flx3

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      !dir$ attributes forceinline :: flx4
      real function flx4(s1,s2,s3,s4)
      implicit none

      real, intent(in) :: s1,s2,s3,s4

      ! 4th-order flux (eg, Wicker and Skamarock, 2002, MWR)

      flx4 = (  (7.0/12.0)*(s3+s2)  &
               -(1.0/12.0)*(s4+s1)  )

      end function flx4

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      !dir$ attributes forceinline :: flx5
      real function flx5(s1,s2,s3,s4,s5)
      implicit none

      real, intent(in) :: s1,s2,s3,s4,s5

      ! 5th-order flux (eg, Wicker and Skamarock, 2002, MWR)

      flx5 = (  (  2.0/60.0)*s1  &
               +(-13.0/60.0)*s2  &
               +( 47.0/60.0)*s3  &
               +( 27.0/60.0)*s4  &
               +( -3.0/60.0)*s5  )

      end function flx5

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      !dir$ attributes forceinline :: flx6
      real function flx6(s1,s2,s3,s4,s5,s6)
      implicit none

      real, intent(in) :: s1,s2,s3,s4,s5,s6

      ! 6th-order flux (eg, Wicker and Skamarock, 2002, MWR)

      flx6 = (  (37.0/60.0)*(s4+s3)  &
               +(-8.0/60.0)*(s5+s2)  &
               +( 1.0/60.0)*(s6+s1)  )

      end function flx6


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getepsd(xh,rxh,uh,xf,rxf,uf,vh,vf,mh,c1,c2,mf,ua ,va ,wa ,  &
                         dum1,dum2,epsd1,epsd2,rho0,rf0,rrf0,rru,rrv,rrw,dt)
      use input
      use constants
      implicit none

      real, intent(in), dimension(ib:ie) :: xh,rxh,uh
      real, intent(in), dimension(ib:ie+1) :: xf,rxf,uf
      real, intent(in), dimension(jb:je) :: vh
      real, intent(in), dimension(jb:je+1) :: vf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: mh,c1,c2
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: mf
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: epsd1,epsd2
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: rho0,rf0,rrf0
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: rru
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: rrv
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: rrw
      real, intent(in) :: dt

      integer :: i,j,k
      real :: ubar,vbar,wbar
      real :: tem
      real :: coef

      real, parameter :: onedtwelve = 1.0/12.0
      real, parameter :: onedsixty  = 1.0/60.0

    IF(.not.terrain_flag)THEN
      ! without terrain:

      !$omp parallel do default(shared)  &
      !$omp private(i,j,k,tem)
      DO k=1,nk
        tem = rho0(1,1,k)
        do j=0,nj+2
        do i=0,ni+2
          rru(i,j,k)=tem*ua(i,j,k)
          rrv(i,j,k)=tem*va(i,j,k)
        enddo
        enddo
        IF(k.eq.1)THEN
          do j=0,nj+1
          do i=0,ni+1
            rrw(i,j,   1) = 0.0
            rrw(i,j,nk+1) = 0.0
          enddo
          enddo
        ELSE
          tem = rf0(1,1,k)
          do j=0,nj+1
          do i=0,ni+1
            rrw(i,j,k)=tem*wa(i,j,k)
          enddo
          enddo
        ENDIF
      ENDDO

    ELSE
      ! with terrain:
      stop 45454
    ENDIF

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  acheck:  IF( axisymm.eq.0 )THEN

!----------
!  u-x:
!  v-y:

  IF( hadvordrv.eq.5 )THEN
    do k=1,nk
    do j=1,nj
    do i=1,ni
      ubar = 0.5*(rru(i,j,k)+rru(i+1,j,k))
      dum1(i,j,k) = (ua(i+1,j,k)-ua(i,j,k))*rdx*uh(i)*(  &
                      ubar*sign(1.0,ubar)               &
             *( 10.0*(ua(i+1,j,k)-ua(i  ,j,k))          &
                -5.0*(ua(i+2,j,k)-ua(i-1,j,k))          &
                    +(ua(i+3,j,k)-ua(i-2,j,k)) )*onedsixty )
      vbar = 0.5*(rrv(i,j,k)+rrv(i,j+1,k))
      dum2(i,j,k) = (va(i,j+1,k)-va(i,j,k))*rdy*vh(j)*(  &
                      vbar*sign(1.0,vbar)               &
             *( 10.0*(va(i,j+1,k)-va(i,j  ,k))          &
                -5.0*(va(i,j+2,k)-va(i,j-1,k))          &
                    +(va(i,j+3,k)-va(i,j-2,k)) )*onedsixty )
    enddo
    enddo
    enddo
  ELSEIF( idiff.ge.1 .and. difforder.eq.6 )THEN
    coef = kdiff6/64.0/dt
    do k=1,nk
    do j=1,nj
    do i=1,ni
      dum1(i,j,k) = coef*( 10.0*(ua(i+1,j,k)-ua(i  ,j,k))     &
                           -5.0*(ua(i+2,j,k)-ua(i-1,j,k))     &
                               +(ua(i+3,j,k)-ua(i-2,j,k)) )   &
                        *(ua(i+1,j,k)-ua(i,j,k))
      dum2(i,j,k) = coef*( 10.0*(va(i,j+1,k)-va(i,j  ,k))     &
                           -5.0*(va(i,j+2,k)-va(i,j-1,k))     &
                               +(va(i,j+3,k)-va(i,j-2,k)) )   &
                        *(va(i,j+1,k)-va(i,j,k))
    enddo
    enddo
    enddo
  ELSE
    dum1 = 0.0
    dum2 = 0.0
  ENDIF

    do k=2,nk
    do j=1,nj
    do i=1,ni
      tem          = ( c1(1,1,k)*(dum1(i,j,k-1)+dum2(i,j,k-1)) &
                      +c2(1,1,k)*(dum1(i,j,k  )+dum2(i,j,k  )) )
      epsd1(i,j,k) = max( 0.0 , tem )
      epsd2(i,j,k) = min( 0.0 , tem )
    enddo
    enddo
    enddo

!----------
!  u-y:
!  v-x:

  IF( hadvordrv.eq.5 )THEN
    do k=1,nk
    do j=1,nj+1
    do i=1,ni+1
      vbar = 0.5*(rrv(i,j,k)+rrv(i-1,j,k))
      ubar = 0.5*(rru(i,j,k)+rru(i,j-1,k))
      dum1(i,j,k) = (ua(i,j,k)-ua(i,j-1,k))*rdy*vf(j)*(  &
                      vbar*sign(1.0,vbar)               &
             *( 10.0*(ua(i,j  ,k)-ua(i,j-1,k))          &
                -5.0*(ua(i,j+1,k)-ua(i,j-2,k))          &
                    +(ua(i,j+2,k)-ua(i,j-3,k)) )*onedsixty ) &
                  + (va(i,j,k)-va(i-1,j,k))*rdx*uf(i)*(  &
                      ubar*sign(1.0,ubar)               &
             *( 10.0*(va(i  ,j,k)-va(i-1,j,k))          &
                -5.0*(va(i+1,j,k)-va(i-2,j,k))          &
                    +(va(i+2,j,k)-va(i-3,j,k)) )*onedsixty )
    enddo
    enddo
    enddo
  ELSEIF( idiff.ge.1 .and. difforder.eq.6 )THEN
    coef = kdiff6/64.0/dt
    do k=1,nk
    do j=1,nj+1
    do i=1,ni+1
      dum1(i,j,k) = coef*(ua(i,j,k)-ua(i,j-1,k))*(            &
                         ( 10.0*(ua(i,j  ,k)-ua(i,j-1,k))     &
                           -5.0*(ua(i,j+1,k)-ua(i,j-2,k))     &
                               +(ua(i,j+2,k)-ua(i,j-3,k)) ) ) &
                  + coef*(va(i,j,k)-va(i-1,j,k))*(            &
                         ( 10.0*(va(i  ,j,k)-va(i-1,j,k))     &
                           -5.0*(va(i+1,j,k)-va(i-2,j,k))     &
                               +(va(i+2,j,k)-va(i-3,j,k)) ) )
    enddo
    enddo
    enddo
  ELSE
    dum1 = 0.0
    dum2 = 0.0
  ENDIF

    do k=2,nk
    do j=1,nj
    do i=1,ni
      tem          =                0.25*(                                                   &
         ( c2(1,1,k)*((dum1(i,j,k  )+dum1(i+1,j+1,k  ))+(dum1(i,j+1,k  )+dum1(i+1,j,k  )))   &
          +c1(1,1,k)*((dum1(i,j,k-1)+dum1(i+1,j+1,k-1))+(dum1(i,j+1,k-1)+dum1(i+1,j,k-1))) ) )
      epsd1(i,j,k) = epsd1(i,j,k) + max( 0.0 , tem )
      epsd2(i,j,k) = epsd2(i,j,k) + min( 0.0 , tem )
    enddo
    enddo
    enddo

!----------
!  w-x:

  IF( hadvordrv.eq.5 )THEN
    do k=2,nk
    do j=1,nj
    do i=1,ni+1
      ubar = c2(1,1,k)*rru(i,j,k)+c1(1,1,k)*rru(i,j,k-1)
      dum1(i,j,k) = (wa(i,j,k)-wa(i-1,j,k))*rdx*uf(i)*(  &
                      ubar*sign(1.0,ubar)               &
             *( 10.0*(wa(i  ,j,k)-wa(i-1,j,k))          &
                -5.0*(wa(i+1,j,k)-wa(i-2,j,k))          &
                    +(wa(i+2,j,k)-wa(i-3,j,k)) )*onedsixty )
    enddo
    enddo
    enddo
  ELSEIF( idiff.ge.1 .and. difforder.eq.6 )THEN
    coef = kdiff6/64.0/dt
    do k=2,nk
    do j=1,nj
    do i=1,ni+1
      dum1(i,j,k) = coef*(wa(i,j,k)-wa(i-1,j,k))*(    &
                   ( 10.0*(wa(i  ,j,k)-wa(i-1,j,k))   &
                     -5.0*(wa(i+1,j,k)-wa(i-2,j,k))   &
                         +(wa(i+2,j,k)-wa(i-3,j,k)) ) )
    enddo
    enddo
    enddo
  ELSE
    dum1 = 0.0
  ENDIF

!----------
!  w-y:

  IF( hadvordrv.eq.5 )THEN
    do k=2,nk
    do j=1,nj+1
    do i=1,ni
      vbar = c2(1,1,k)*rrv(i,j,k)+c1(1,1,k)*rrv(i,j,k-1)
      dum2(i,j,k) = (wa(i,j,k)-wa(i,j-1,k))*rdy*vf(j)*(  &
                      vbar*sign(1.0,vbar)               &
             *( 10.0*(wa(i,j  ,k)-wa(i,j-1,k))          &
                -5.0*(wa(i,j+1,k)-wa(i,j-2,k))          &
                    +(wa(i,j+2,k)-wa(i,j-3,k)) )*onedsixty )
    enddo
    enddo
    enddo
  ELSEIF( idiff.ge.1 .and. difforder.eq.6 )THEN
    coef = kdiff6/64.0/dt
    do k=2,nk
    do j=1,nj+1
    do i=1,ni
      dum2(i,j,k) = coef*(wa(i,j,k)-wa(i,j-1,k))*(    &
                   ( 10.0*(wa(i,j  ,k)-wa(i,j-1,k))   &
                     -5.0*(wa(i,j+1,k)-wa(i,j-2,k))   &
                         +(wa(i,j+2,k)-wa(i,j-3,k)) ) )
    enddo
    enddo
    enddo
  ELSE
    dum2 = 0.0
  ENDIF

!----------

  ELSE

    stop 23232

  ENDIF    acheck

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  vertical terms:

!----------
! u-z:

    do k=4,nk-2
    do j=1,nj
    do i=1,ni+1
      wbar = 0.5*(rrw(i-1,j,k)+rrw(i,j,k))
      if( wbar.ge.0.0 )then
        dum1(i,j,k) = dum1(i,j,k) + (ua(i,j,k)-ua(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx5(ua(i,j,k-3),ua(i,j,k-2),ua(i,j,k-1),ua(i,j,k  ),ua(i,j,k+1))    &
             -flx6(ua(i,j,k-3),ua(i,j,k-2),ua(i,j,k-1),ua(i,j,k  ),ua(i,j,k+1),ua(i,j,k+2))  )
      else
        dum1(i,j,k) = dum1(i,j,k) + (ua(i,j,k)-ua(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx5(ua(i,j,k+2),ua(i,j,k+1),ua(i,j,k  ),ua(i,j,k-1),ua(i,j,k-2))    &
             -flx6(ua(i,j,k-3),ua(i,j,k-2),ua(i,j,k-1),ua(i,j,k  ),ua(i,j,k+1),ua(i,j,k+2))  )
      endif
    enddo
    enddo
    enddo

    k = 3
    do j=1,nj
    do i=1,ni+1
      wbar = 0.5*(rrw(i-1,j,k)+rrw(i,j,k))
      if( wbar.ge.0.0 )then
        dum1(i,j,k) = dum1(i,j,k) + (ua(i,j,k)-ua(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx3(ua(i,j,k-2),ua(i,j,k-1),ua(i,j,k  ))   &
             -flx4(ua(i,j,k-2),ua(i,j,k-1),ua(i,j,k  ),ua(i,j,k+1))  )
      else
        dum1(i,j,k) = dum1(i,j,k) + (ua(i,j,k)-ua(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx5(ua(i,j,k+2),ua(i,j,k+1),ua(i,j,k  ),ua(i,j,k-1),ua(i,j,k-2))    &
             -flx4(ua(i,j,k-2),ua(i,j,k-1),ua(i,j,k  ),ua(i,j,k+1))  )
      endif
    enddo
    enddo

    k = nk-1
    do j=1,nj
    do i=1,ni+1
      wbar = 0.5*(rrw(i-1,j,k)+rrw(i,j,k))
      if( wbar.ge.0.0 )then
        dum1(i,j,k) = dum1(i,j,k) + (ua(i,j,k)-ua(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx5(ua(i,j,k-3),ua(i,j,k-2),ua(i,j,k-1),ua(i,j,k  ),ua(i,j,k+1))    &
             -flx4(ua(i,j,k-2),ua(i,j,k-1),ua(i,j,k  ),ua(i,j,k+1))  )
      else
        dum1(i,j,k) = dum1(i,j,k) + (ua(i,j,k)-ua(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx3(ua(i,j,k+1),ua(i,j,k),ua(i,j,k-1))    &
             -flx4(ua(i,j,k-2),ua(i,j,k-1),ua(i,j,k  ),ua(i,j,k+1))  )
      endif
    enddo
    enddo

    k = 2
    do j=1,nj
    do i=1,ni+1
      wbar = 0.5*(rrw(i-1,j,k)+rrw(i,j,k))
      if( wbar.lt.0.0 )then
        dum1(i,j,k) = dum1(i,j,k) + (ua(i,j,k)-ua(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx3(ua(i,j,k+1),ua(i,j,k),ua(i,j,k-1))   &
             -( c1(1,1,k)*ua(i,j,k-1)+c2(1,1,k)*ua(i,j,k  ) ) )
      else
        dum1(i,j,k) = 0.0
      endif
    enddo
    enddo

    k = nk
    do j=1,nj
    do i=1,ni+1
      wbar = 0.5*(rrw(i-1,j,k)+rrw(i,j,k))
      if( wbar.gt.0.0 )then
        dum1(i,j,k) = dum1(i,j,k) + (ua(i,j,k)-ua(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx3(ua(i,j,k-2),ua(i,j,k-1),ua(i,j,k  ))  &
             -( c1(1,1,k)*ua(i,j,k-1)+c2(1,1,k)*ua(i,j,k  ) ) )
      else
        dum1(i,j,k) = 0.0
      endif
    enddo
    enddo

!----------
! v-z:

    do k=4,nk-2
    do j=1,nj+1
    do i=1,ni
      wbar = 0.5*(rrw(i,j-1,k)+rrw(i,j,k))
      if( wbar.ge.0.0 )then
        dum2(i,j,k) = dum2(i,j,k) + (va(i,j,k)-va(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx5(va(i,j,k-3),va(i,j,k-2),va(i,j,k-1),va(i,j,k  ),va(i,j,k+1))    &
             -flx6(va(i,j,k-3),va(i,j,k-2),va(i,j,k-1),va(i,j,k  ),va(i,j,k+1),va(i,j,k+2))  )
      else
        dum2(i,j,k) = dum2(i,j,k) + (va(i,j,k)-va(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx5(va(i,j,k+2),va(i,j,k+1),va(i,j,k  ),va(i,j,k-1),va(i,j,k-2))    &
             -flx6(va(i,j,k-3),va(i,j,k-2),va(i,j,k-1),va(i,j,k  ),va(i,j,k+1),va(i,j,k+2))  )
      endif
    enddo
    enddo
    enddo

    k = 3
    do j=1,nj+1
    do i=1,ni
      wbar = 0.5*(rrw(i,j-1,k)+rrw(i,j,k))
      if( wbar.ge.0.0 )then
        dum2(i,j,k) = dum2(i,j,k) + (va(i,j,k)-va(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx3(va(i,j,k-2),va(i,j,k-1),va(i,j,k  ))   &
             -flx4(va(i,j,k-2),va(i,j,k-1),va(i,j,k  ),va(i,j,k+1))  )
      else
        dum2(i,j,k) = dum2(i,j,k) + (va(i,j,k)-va(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx5(va(i,j,k+2),va(i,j,k+1),va(i,j,k  ),va(i,j,k-1),va(i,j,k-2))    &
             -flx4(va(i,j,k-2),va(i,j,k-1),va(i,j,k  ),va(i,j,k+1))  )
      endif
    enddo
    enddo

    k = nk-1
    do j=1,nj+1
    do i=1,ni
      wbar = 0.5*(rrw(i,j-1,k)+rrw(i,j,k))
      if( wbar.ge.0.0 )then
        dum2(i,j,k) = dum2(i,j,k) + (va(i,j,k)-va(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx5(va(i,j,k-3),va(i,j,k-2),va(i,j,k-1),va(i,j,k  ),va(i,j,k+1))    &
             -flx4(va(i,j,k-2),va(i,j,k-1),va(i,j,k  ),va(i,j,k+1))  )
      else
        dum2(i,j,k) = dum2(i,j,k) + (va(i,j,k)-va(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx3(va(i,j,k+1),va(i,j,k),va(i,j,k-1))    &
             -flx4(va(i,j,k-2),va(i,j,k-1),va(i,j,k  ),va(i,j,k+1))  )
      endif
    enddo
    enddo

    k = 2
    do j=1,nj+1
    do i=1,ni
      wbar = 0.5*(rrw(i,j-1,k)+rrw(i,j,k))
      if( wbar.lt.0.0 )then
        dum2(i,j,k) = dum2(i,j,k) + (va(i,j,k)-va(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx3(va(i,j,k+1),va(i,j,k),va(i,j,k-1))   &
             -( c1(1,1,k)*va(i,j,k-1)+c2(1,1,k)*va(i,j,k  ) ) )
      else
        dum2(i,j,k) = 0.0
      endif
    enddo
    enddo

    k = nk
    do j=1,nj+1
    do i=1,ni
      wbar = 0.5*(rrw(i,j-1,k)+rrw(i,j,k))
      if( wbar.gt.0.0 )then
        dum2(i,j,k) = dum2(i,j,k) + (va(i,j,k)-va(i,j,k-1))*rdz*mf(1,1,k)*wbar*(   &
              flx3(va(i,j,k-2),va(i,j,k-1),va(i,j,k  ))  &
             -( c1(1,1,k)*va(i,j,k-1)+c2(1,1,k)*va(i,j,k  ) ) )
      else
        dum2(i,j,k) = 0.0
      endif
    enddo
    enddo

!----------

    do k=2,nk
    do j=1,nj
    do i=1,ni
      tem          = 0.5*( (dum1(i,j,k)+dum1(i+1,j,k)) &
                          +(dum2(i,j,k)+dum2(i,j+1,k)) )
      epsd1(i,j,k) = epsd1(i,j,k) + max( 0.0 , tem )
      epsd2(i,j,k) = epsd2(i,j,k) + min( 0.0 , tem )
    enddo
    enddo
    enddo

!----------
! w-z:

  acheck2:  IF( axisymm.eq.0 )THEN

    do k=3,nk-2
    do j=1,nj
    do i=1,ni
      wbar = 0.5*(rrw(i,j,k)+rrw(i,j,k+1))
      if( wbar.ge.0.0 )then
        dum1(i,j,k) = (wa(i,j,k+1)-wa(i,j,k))*rdz*mh(1,1,k)*wbar*(  &
            flx5(wa(i,j,k-2),wa(i,j,k-1),wa(i,j,k  ),wa(i,j,k+1),wa(i,j,k+2))  &
           -flx6(wa(i,j,k-2),wa(i,j,k-1),wa(i,j,k  ),wa(i,j,k+1),wa(i,j,k+2),wa(i,j,k+3)) )
      else
        dum1(i,j,k) = (wa(i,j,k+1)-wa(i,j,k))*rdz*mh(1,1,k)*wbar*(  &
            flx5(wa(i,j,k+3),wa(i,j,k+2),wa(i,j,k+1),wa(i,j,k  ),wa(i,j,k-1))  &
           -flx6(wa(i,j,k-2),wa(i,j,k-1),wa(i,j,k  ),wa(i,j,k+1),wa(i,j,k+2),wa(i,j,k+3)) )
      endif
    enddo
    enddo
    enddo

    k = 2
    do j=1,nj
    do i=1,ni
      wbar = 0.5*(rrw(i,j,k)+rrw(i,j,k+1))
      if( wbar.ge.0.0 )then
        dum1(i,j,k) = (wa(i,j,k+1)-wa(i,j,k))*rdz*mh(1,1,k)*wbar*(  &
            flx3(wa(i,j,k-1),wa(i,j,k  ),wa(i,j,k+1))  &
           -flx4(wa(i,j,k-1),wa(i,j,k  ),wa(i,j,k+1),wa(i,j,k+2)) )
      else
        dum1(i,j,k) = (wa(i,j,k+1)-wa(i,j,k))*rdz*mh(1,1,k)*wbar*(  &
            flx5(wa(i,j,k+3),wa(i,j,k+2),wa(i,j,k+1),wa(i,j,k  ),wa(i,j,k-1))  &
           -flx4(wa(i,j,k-1),wa(i,j,k  ),wa(i,j,k+1),wa(i,j,k+2)) )
      endif
    enddo
    enddo

    k = nk-1
    do j=1,nj
    do i=1,ni
      wbar = 0.5*(rrw(i,j,k)+rrw(i,j,k+1))
      if( wbar.ge.0.0 )then
        dum1(i,j,k) = (wa(i,j,k+1)-wa(i,j,k))*rdz*mh(1,1,k)*wbar*(  &
            flx5(wa(i,j,k-2),wa(i,j,k-1),wa(i,j,k  ),wa(i,j,k+1),wa(i,j,k+2))  &
           -flx4(wa(i,j,k-1),wa(i,j,k  ),wa(i,j,k+1),wa(i,j,k+2)) )
      else
        dum1(i,j,k) = (wa(i,j,k+1)-wa(i,j,k))*rdz*mh(1,1,k)*wbar*(  &
            flx3(wa(i,j,k+2),wa(i,j,k+1),wa(i,j,k  ))  &
           -flx4(wa(i,j,k-1),wa(i,j,k  ),wa(i,j,k+1),wa(i,j,k+2)) )
      endif
    enddo
    enddo

    k = 1
    do j=1,nj
    do i=1,ni
      wbar = 0.5*(rrw(i,j,k)+rrw(i,j,k+1))
      if( wbar.lt.0.0 )then
        dum1(i,j,k) = (wa(i,j,k+1)-wa(i,j,k))*rdz*mh(1,1,k)*wbar*(  &
                         flx3(wa(i,j,k+2),wa(i,j,k+1),wa(i,j,k  )) &
                        -0.5*(wa(i,j,k)+wa(i,j,k+1)) )
      else
        dum1(i,j,k) = 0.0
      endif
    enddo
    enddo

    k = nk
    do j=1,nj
    do i=1,ni
      wbar = 0.5*(rrw(i,j,k)+rrw(i,j,k+1))
      if( wbar.gt.0.0 )then
        dum1(i,j,k) = (wa(i,j,k+1)-wa(i,j,k))*rdz*mh(1,1,k)*wbar*(  &
                         flx3(wa(i,j,k-1),wa(i,j,k  ),wa(i,j,k+1)) &
                        -0.5*(wa(i,j,k)+wa(i,j,k+1)) )
      else
        dum1(i,j,k) = 0.0
      endif
    enddo
    enddo

  ELSE

    dum1 = 0.0

  ENDIF   acheck2

!----------

    do k=2,nk
    do j=1,nj
    do i=1,ni
      tem          =                (c1(1,1,k)*dum1(i,j,k-1)+c2(1,1,k)*dum1(i,j,k))
      epsd1(i,j,k) = epsd1(i,j,k) + max( 0.0 , tem )
      epsd2(i,j,k) = epsd2(i,j,k) + min( 0.0 , tem )
      !-----
      epsd1(i,j,k) = epsd1(i,j,k)*rrf0(1,1,k)
      epsd2(i,j,k) = epsd2(i,j,k)*rrf0(1,1,k)
    enddo
    enddo
    enddo

!--------------------------------------------------------------
!  finished

      if(timestats.ge.1) time_diag=time_diag+mytime()

      end subroutine getepsd


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine getepst(xh,rxh,uh,xf,rxf,uf,vh,vf,mh,c1,c2,mf,ua ,va ,wa ,  &
                         t11,t12,t13,t22,t23,t33,rf,                         &
                         dum1,dum2,epst,epstp,epstn)
      use input
      use constants
      implicit none

      real, intent(in), dimension(ib:ie) :: xh,rxh,uh
      real, intent(in), dimension(ib:ie+1) :: xf,rxf,uf
      real, intent(in), dimension(jb:je) :: vh
      real, intent(in), dimension(jb:je+1) :: vf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: mh,c1,c2
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: mf
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: ua
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: va
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: wa
      real, intent(in),    dimension(ib:ie,jb:je,kb:ke) :: t11,t12,t13,t22,t23,t33,rf
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: epst
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1), optional :: epstp,epstn

      integer :: i,j,k
      real :: rrf,tem

!--------------------------------------------------------------
!  horizontal terms, Cartesian grid:

!----------
!  u-x:
!  v-y:

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    do k=1,nk
    do j=1,nj
    do i=1,ni
      dum1(i,j,k) = (ua(i+1,j,k)-ua(i,j,k))*rdx*uh(i)*t11(i,j,k)
      dum2(i,j,k) = (va(i,j+1,k)-va(i,j,k))*rdy*vh(j)*t22(i,j,k)
    enddo
    enddo
    enddo

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    do k=2,nk
    do j=1,nj
    do i=1,ni
      tem          = ( c1(1,1,k)*(dum1(i,j,k-1)+dum2(i,j,k-1)) &
                      +c2(1,1,k)*(dum1(i,j,k  )+dum2(i,j,k  )) )
      epst(i,j,k) = tem
    enddo
    enddo
    enddo

    if( present(epstp) .and. present(epstn) )then
    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    do k=2,nk
    do j=1,nj
    do i=1,ni
      tem          = ( c1(1,1,k)*(dum1(i,j,k-1)+dum2(i,j,k-1)) &
                      +c2(1,1,k)*(dum1(i,j,k  )+dum2(i,j,k  )) )
      epstp(i,j,k) = max( 0.0 , tem )
      epstn(i,j,k) = min( 0.0 , tem )
    enddo
    enddo
    enddo
    endif

!----------
!  u-y:
!  v-x:

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    do k=1,nk
    do j=1,nj+1
    do i=1,ni+1
      dum1(i,j,k) = t12(i,j,k)*( (ua(i,j,k)-ua(i,j-1,k))*rdy*vf(j) &
                                +(va(i,j,k)-va(i-1,j,k))*rdx*uf(i) )
    enddo
    enddo
    enddo

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    do k=2,nk
    do j=1,nj
    do i=1,ni
      tem          =                0.25*(                                                   &
         ( c2(1,1,k)*((dum1(i,j,k  )+dum1(i+1,j+1,k  ))+(dum1(i,j+1,k  )+dum1(i+1,j,k  )))   &
          +c1(1,1,k)*((dum1(i,j,k-1)+dum1(i+1,j+1,k-1))+(dum1(i,j+1,k-1)+dum1(i+1,j,k-1))) ) )
      epst(i,j,k) = epst(i,j,k) + tem
    enddo
    enddo
    enddo

    if( present(epstp) .and. present(epstn) )then
    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    do k=2,nk
    do j=1,nj
    do i=1,ni
      tem          =                0.25*(                                                   &
         ( c2(1,1,k)*((dum1(i,j,k  )+dum1(i+1,j+1,k  ))+(dum1(i,j+1,k  )+dum1(i+1,j,k  )))   &
          +c1(1,1,k)*((dum1(i,j,k-1)+dum1(i+1,j+1,k-1))+(dum1(i,j+1,k-1)+dum1(i+1,j,k-1))) ) )
      epstp(i,j,k) = epstp(i,j,k) + max( 0.0 , tem )
      epstn(i,j,k) = epstn(i,j,k) + min( 0.0 , tem )
    enddo
    enddo
    enddo
    endif

!----------
!  w-x:
!  w-y:
!  u-z:
!  v-z:

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    do k=2,nk
    do j=1,nj+1
    do i=1,ni+1
      dum1(i,j,k) = ( (wa(i,j,k)-wa(i-1,j,k))*rdx*uf(i)     &
                     +(ua(i,j,k)-ua(i,j,k-1))*rdz*mf(1,1,k) )*t13(i,j,k)
      dum2(i,j,k) = ( (wa(i,j,k)-wa(i,j-1,k))*rdy*vf(j)     &
                     +(va(i,j,k)-va(i,j,k-1))*rdz*mf(1,1,k) )*t23(i,j,k)
    enddo
    enddo
    enddo

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    do k=2,nk
    do j=1,nj
    do i=1,ni
      tem          = 0.5*( (dum1(i,j,k)+dum1(i+1,j,k)) &
                          +(dum2(i,j,k)+dum2(i,j+1,k)) )
      epst(i,j,k) = epst(i,j,k) + tem
    enddo
    enddo
    enddo

    if( present(epstp) .and. present(epstn) )then
    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    do k=2,nk
    do j=1,nj
    do i=1,ni
      tem          = 0.5*( (dum1(i,j,k)+dum1(i+1,j,k)) &
                          +(dum2(i,j,k)+dum2(i,j+1,k)) )
      epstp(i,j,k) = epstp(i,j,k) + max( 0.0 , tem )
      epstn(i,j,k) = epstn(i,j,k) + min( 0.0 , tem )
    enddo
    enddo
    enddo
    endif

!----------
! w-z:

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem)
    do k=1,nk
    do j=1,nj
    do i=1,ni
      dum1(i,j,k) = (wa(i,j,k+1)-wa(i,j,k))*rdz*mh(1,1,k)*t33(i,j,k)
    enddo
    enddo
    enddo

    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem,rrf)
    do k=2,nk
    do j=1,nj
    do i=1,ni
      tem          =                (c1(1,1,k)*dum1(i,j,k-1)+c2(1,1,k)*dum1(i,j,k))
      epst(i,j,k) = epst(i,j,k) + tem
      !-------------------------
      rrf = 1.0/rf(i,j,k)
      epst(i,j,k) = epst(i,j,k)*rrf
    enddo
    enddo
    enddo

    if( present(epstp) .and. present(epstn) )then
    !$omp parallel do default(shared)   &
    !$omp private(i,j,k,tem,rrf)
    do k=2,nk
    do j=1,nj
    do i=1,ni
      tem          =                (c1(1,1,k)*dum1(i,j,k-1)+c2(1,1,k)*dum1(i,j,k))
      epstp(i,j,k) = epstp(i,j,k) + max( 0.0 , tem )
      epstn(i,j,k) = epstn(i,j,k) + min( 0.0 , tem )
      !-------------------------
      rrf = 1.0/rf(i,j,k)
      epstp(i,j,k) = epstp(i,j,k)*rrf
      epstn(i,j,k) = epstn(i,j,k)*rrf
    enddo
    enddo
    enddo
    endif

    if(timestats.ge.1) time_turb=time_turb+mytime()

!--------------------------------------------------------------
!  finished

      if(timestats.ge.1) time_turb=time_turb+mytime()

      end subroutine getepst


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  END MODULE turb_module
