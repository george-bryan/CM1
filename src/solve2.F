  MODULE solve2_module

        ! solve2: RK loop and pressure solver
        ! (advection, buoyancy, pressure gradient)

!-----------------------------------------------------------------------------
!
!  CM1 Numerical Model, Release 20.2  (cm1r20.2)
!  18 October 2020
!  https://www2.mmm.ucar.edu/people/bryan/cm1/
!
!  (c)2020 - University Corporation for Atmospheric Research 
!
!-----------------------------------------------------------------------------
!  Quick Index:
!    ua/u3d     = velocity in x-direction (m/s)  (grid-relative)
!    va/v3d     = velocity in y-direction (m/s)  (grid-relative)
!       Note: when imove=1, ground-relative winds are umove+ua, umove+u3d,
!                                                     vmove+va, vmove+v3d.
!    wa/w3d     = velocity in z-direction (m/s)
!    tha/th3d   = perturbation potential temperature (K)
!    ppi/pp3d   = perturbation nondimensional pressure ("Exner function")
!    qa/q3d     = mixing ratios of moisture (kg/kg)
!    tkea/tke3d = SUBGRID turbulence kinetic energy (m^2/s^2)
!    kmh/kmv    = turbulent diffusion coefficients for momentum (m^2/s)
!    khh/khv    = turbulent diffusion coefficients for scalars (m^2/s)
!                 (h = horizontal, v = vertical)
!    prs        = pressure (Pa)
!    rho        = density (kg/m^3)
!
!    th0,pi0,prs0,etc = base-state arrays
!
!    xh         = x (m) at scalar points
!    xf         = x (m) at u points
!    yh         = y (m) at scalar points
!    yf         = y (m) at v points
!    zh         = z (m above sea level) of scalar points (aka, "half levels")
!    zf         = z (m above sea level) of w points (aka, "full levels")
!
!    For the axisymmetric model (axisymm=1), xh and xf are radius (m).
!
!  See "The governing equations for CM1" for more details:
!        https://www2.mmm.ucar.edu/people/bryan/cm1/cm1_equations.pdf
!-----------------------------------------------------------------------------
!  Some notes:
!
!  - Upon entering solve, the arrays ending in "a" (eg, ua,wa,tha,qa,etc)
!    are equivalent to the arrays ending in "3d" (eg, u3d,w3d,th3d,q3d,etc).
!  - The purpose of solve is to update the variables from time "t" to time
!    "t+dt".  Values at time "t+dt" are stored in the "3d" arrays.
!  - The "ghost zones" (boundaries beyond the computational subdomain) are
!    filled out completely (3 rows/columns) for the "3d" arrays.  To save 
!    unnecessary computations, starting with cm1r15 the "ghost zones" of 
!    the "a" arrays are only filled out to 1 row/column.  Hence, if you 
!    need to do calculations that use a large stencil, you must use the 
!    "3d" arrays (not the "a") arrays.
!  - Arrays named "ten" store tendencies.  Those ending "ten1" store
!    pre-RK tendencies that are calculated once and then held fixed during
!    the RK (Runge-Kutta) sub-steps. 
!  - CM1 uses a low-storage three-step Runge-Kutta scheme.  See Wicker
!    and Skamarock (2002, MWR, p 2088) for more information.
!  - CM1 uses a staggered C grid.  Hence, u arrays have one more grid point
!    in the i direction, v arrays have one more grid point in the j 
!    direction, and w arrays have one more grid point in the k direction
!    (compared to scalar arrays).
!  - CM1 assumes the subgrid turbulence parameters (tke,km,kh) are located
!    at the w points. 
!-----------------------------------------------------------------------------

  implicit none

  private
  public :: solve2

  CONTAINS

      subroutine solve2(nstep,rbufsz,num_soil_layers,                  &
                   dt,dtlast,mtime,dbldt,mass1,mass2,                 &
                   dosfcflx,cloudvar,rhovar,qmag,bud,bud2,qbudget,asq,bsq, &
                   xh,rxh,arh1,arh2,uh,ruh,xf,rxf,arf1,arf2,uf,ruf,   &
                   yh,vh,rvh,yf,vf,rvf,                               &
                   xfref,xhref,yfref,yhref,dumk1,dumk2,rds,sigma,rdsf,sigmaf, &
                   tauh,taus,zh,mh,rmh,c1,c2,tauf,zf,mf,rmf,         &
                   rho0s,pi0s,prs0s,rth0s,                           &
                   wprof,ufrc,vfrc,thfrc,qvfrc,ug,vg,dvdr,           &
                   uavg,vavg,thavg,pavg,ulspg,vlspg,qavg,cavg,       &
                   pi0,rho0,prs0,thv0,th0,rth0,qv0,qc0,              &
                   qi0,rr0,rf0,rrf0,thrd,                            &
                   zs,gz,rgz,gzu,rgzu,gzv,rgzv,dzdx,dzdy,gx,gxu,gy,gyv, &
                   rain,sws,svs,sps,srs,sgs,sus,shs,                 &
                   tsk,thflux,qvflux,cd,ch,cq,u1,v1,s1,tlh,f2d,prate, &
                   radbcw,radbce,radbcs,radbcn,dtu,dtu0,dtv,dtv0,    &
                   dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9,     &
                   divx,rho,rr,rf,prs,                               &
                   t11,t12,t13,t22,t23,t33,                          &
                   m11,m12,m13,m22,m23,m33,                          &
                   u0,rru,ua,u3d,uten,uten1,                         &
                   v0,rrv,va,v3d,vten,vten1,                         &
                   rrw,wa,w3d,wten,wten1,                            &
                   ppi,pp3d,ppten,sten,sadv,ppx,phi1,phi2,           &
                   tha,th3d,thten,thten1,thterm,                     &
                   qpten,qtten,qvten,qcten,qa,q3d,qten,              &
                   kmh,kmv,khh,khv,tkea,tke3d,tketen,                &
                   nm,defv,defh,lenscl,dissten,epst,epsd1,epsd2,     &
                   qke_adv,qke,qke3d,                                &
                   thpten,qvpten,qcpten,qipten,upten,vpten,qnipten,qncpten,o30,zir,  &
                   swten,lwten,effc,effi,effs,effr,effg,effis,       &
                   lu_index,kpbl2d,psfc,u10,v10,s10,hfx,qfx,xland,znt,ust,  &
                   hpbl,wspd,psim,psih,gz1oz0,br,                    &
                   CHS,CHS2,CQS2,CPMM,ZOL,MAVAIL,                    &
                   MOL,RMOL,REGIME,LH,FLHC,FLQC,QGH,                 &
                   CK,CKA,CDA,USTM,QSFC,T2,Q2,TH2,EMISS,THC,ALBD,    &
                   gsw,glw,chklowq,capg,snowc,dsxy,wstar,delta,fm,fh,zntmp,  &
                   slab_zs,slab_dzs,tslb,tmn,tml,t0ml,hml,h0ml,huml,hvml,tmoml,       &
                   pta,pt3d,ptten,pdata,                             &
                   cfb,cfa,cfc,ad1,ad2,pdt,lgbth,lgbph,rhs,trans,flag,  &
                   gamk,gamwall,kmw,u1b,v1b,u2pt,v2pt,               &
                   reqs_u,reqs_v,reqs_w,reqs_s,reqs_p,reqs_p2,reqs_p3, &
                   reqs_x,reqs_y,reqs_z,reqs_tk,reqs_q,reqs_t,       &
                   nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,                  &
                   n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,          &
                   ww1,ww2,we1,we2,ws1,ws2,wn1,wn2,                  &
                   pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,                  &
                   p2w1,p2w2,p2e1,p2e2,p2s1,p2s2,p2n1,p2n2,          &
                   p3w1,p3w2,p3e1,p3e2,p3s1,p3s2,p3n1,p3n2,          &
                   vw1,vw2,ve1,ve2,vs1,vs2,vn1,vn2,                  &
                   zw1,zw2,ze1,ze2,zs1,zs2,zn1,zn2,                  &
                   uw31,uw32,ue31,ue32,us31,us32,un31,un32,          &
                   vw31,vw32,ve31,ve32,vs31,vs32,vn31,vn32,          &
                   ww31,ww32,we31,we32,ws31,ws32,wn31,wn32,          &
                   sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,          &
                   rw31,rw32,re31,re32,rs31,rs32,rn31,rn32,          &
                   qw31,qw32,qe31,qe32,qs31,qs32,qn31,qn32,          &
                   tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2,          &
                   kw1,kw2,ke1,ke2,ks1,ks2,kn1,kn2,                  &
                   tw1,tw2,te1,te2,ts1,ts2,tn1,tn2,                  &
                   dat1,dat2,dat3,reqt,reqc,                         &
                   tdiag,qdiag,udiag,vdiag,wdiag,kdiag,pdiag,        &
                   out2d,out3d,                                      &
                   bndy,kbdy,hflxw,hflxe,hflxs,hflxn,                &
                   urecyw,vrecyw,wrecyw,trecyw,                      &
                   urecys,vrecys,wrecys,trecys,                      &
                   dowriteout,dorad,getdbz,getvt,dotdwrite,          &
                   dotbud,doqbud,doubud,dovbud,dowbud,               &
                   doazimwrite,dorestart)
        ! end_solve2
      use input, only : ib,ie,jb,je,kb,ke,itb,ite,jtb,jte,ktb,kte,ibb2,ibe2,jbb2,jbe2, &
          kbb2,kbe2,ibph,ieph,jbph,jeph,kbph,keph,ibnba,ienba,jbnba,jenba,kbnba,kenba, &
          nx,ny,nz,ni,nj,nk,ibm,iem,jbm,jem,kbm,kem,ibc,iec,jbc,jec,kbc,kec,ibt,iet,jbt, &
          jet,kbt,ket,ibb,ieb,jbb,jeb,kbb,keb,ibr,ier,jbr,jer,kbr,ker,maxq,nbudget,numq,ngxy, &
          nrain,npt,nparcelsLocal,npvals,ibl,iel,jbl,jel,ibp,iep,jbp,jep,kbp,kep,ib2pt,ie2pt, &
          jb2pt,je2pt,kb2pt,ke2pt,ipb,ipe,jpb,jpe,kpb,kpe,rmp,cmp,kmt,imp,jmp,kmp,d3i,d3j, &
          d3t,d3n,d2i,d2j,ibdt,iedt,jbdt,jedt,kbdt,kedt,ntdiag,ibdq,iedq,jbdq,jedq,kbdq, &
          kedq,nqdiag,ibdv,iedv,jbdv,jedv,kbdv,kedv,nudiag,nvdiag,nwdiag,ibdk,iedk,jbdk, &
          jedk,kbdk,kedk,nkdiag,ibdp,iedp,jbdp,jedp,kbdp,kedp,npdiag,ib2d,ie2d,jb2d,je2d, &
          nout2d,ib3d,ie3d,jb3d,je3d,kb3d,ke3d,nout3d,ibmynn,iemynn,jbmynn,jemynn,kbmynn,kemynn, &
          qp,nf,nu,nv,nw,testcase,myid,timestats,hadvordrv,vadvordrv,hadvordrs,vadvordrs, &
          psolver,dolsw,nqc,nqi,nnci,nncc,zt,rzt,rdx,rdy,rdz,smeps,advwenov,advwenos, &
          time_misc13,time_misc14,time_misc15,time_misc16,time_misc17,time_misc18,time_misc19, &
          time_misc20,time_misc21,time_misc22,time_buoyan,time_advs,time_divx,time_cor, &
          time_advu,time_integ,time_prsrho,time_cflq,mytime,cm1setup,iusetke,sgsmodel,doimpl, &
          iinit,ptype,wbc,ebc,sbc,nbc,icor,betaplane,nudgeobc,alphobc,iprcl,apmasscon, &
          ibw,ibe,ibs,ibn,imoist,irdamp,hrdamp,irbc,rdalpha,iptra,lspgrad,ierr,fcor,vmove, &
          umove,hurr_rad,difforder,idiff,nqv,eqtset,idiss,rterm,iice,nql1,nql2,nqs1,nqs2, &
          axisymm,dx,dy,dz,bbc,terrain_flag,radopt,use_pbl,tsmall,qsmall,dohturb,dovturb, &
          xc_uforce,xr_uforce,zr_uforce,alpha_uforce,t1_uforce,t2_uforce,stat_qsrc, &
          bl_mynn_tkeadvect,cflmax,stopit,pdtra, &
          kd_turb,ud_rdamp,vd_rdamp,wd_rdamp,td_rdamp,td_hediff,td_vediff,qd_hediff,qd_vediff, &
          td_diss,td_rad,td_pbl,td_hturb,td_vturb,qd_hturb,qd_vturb,ud_pbl,vd_pbl,qd_pbl, &
          td_nudge,qd_nudge,ud_cor,td_hadv,td_vadv,td_lsw,td_hidiff,td_vidiff,qd_hadv,qd_vadv, &
          qd_lsw,qd_hidiff,qd_vidiff,vd_cor,ud_cent,vd_cent,vd_hadv,wd_buoy,ud_pgrad,vd_pgrad, &
          wd_pgrad,td_div,kd_adv
      use constants
      use bc_module, only: bc2d_GPU, bcs_GPU, bcu_GPU, bcv_GPU, bcw_GPU, bcwsfc, &
                           radbcew4, radbcns4, bcs2_gpu 
      use comm_module
      use adv_module
      use maxmin_module
      use adv_routines , only : movesfc
      use sound_module
      use sounde_module
      use soundns_module
      use soundcb_module
      use anelp_module
      use misclibs
      use module_mp_nssl_2mom, only : zscale, nssl_2mom_driver
      use ib_module
      use eddy_recycle
#ifdef MPI
      use mpi
#endif
      implicit none

!-----------------------------------------------------------------------
! Arrays and variables passed into solve

      integer, intent(in) :: nstep
      integer, intent(in) :: rbufsz,num_soil_layers
      real, intent(inout) :: dt,dtlast
      double precision, intent(in   ) :: mtime
      double precision, intent(inout) :: dbldt
      double precision, intent(in   ) :: mass1
      double precision, intent(inout) :: mass2
      logical, intent(in) :: dosfcflx
      logical, intent(in), dimension(maxq) :: cloudvar,rhovar
      real, dimension(maxq) :: qmag
      double precision, intent(inout), dimension(nk) :: bud
      double precision, intent(inout), dimension(nj) :: bud2
      double precision, intent(inout), dimension(nbudget) :: qbudget
      double precision, intent(inout), dimension(numq) :: asq,bsq
      real, intent(in), dimension(ib:ie) :: xh,rxh,arh1,arh2,uh,ruh
      real, intent(in), dimension(ib:ie+1) :: xf,rxf,arf1,arf2,uf,ruf
      real, intent(in), dimension(jb:je) :: yh,vh,rvh
      real, intent(in), dimension(jb:je+1) :: yf,vf,rvf
      real, intent(in), dimension(1-ngxy:nx+ngxy+1) :: xfref,xhref
      real, intent(in), dimension(1-ngxy:ny+ngxy+1) :: yfref,yhref
      double precision, intent(inout), dimension(kb:ke) :: dumk1,dumk2
      real, intent(in), dimension(kb:ke) :: rds,sigma
      real, intent(in), dimension(kb:ke+1) :: rdsf,sigmaf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: tauh,taus,zh,mh,rmh,c1,c2
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: tauf,zf,mf,rmf
      real, intent(in), dimension(ib:ie,jb:je) :: rho0s,pi0s,prs0s,rth0s
      real, intent(in),    dimension(kb:ke) :: wprof
      real, intent(inout), dimension(kb:ke) :: ufrc,vfrc,thfrc,qvfrc,ug,vg,dvdr,  &
                                               uavg,vavg,thavg,pavg,ulspg,vlspg
      real, intent(inout), dimension(kb:ke,numq) :: qavg
      double precision, intent(inout), dimension(kb:ke,3+numq) :: cavg
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: pi0,rho0,prs0,thv0,th0,rth0,qv0,qc0
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: qi0,rr0,rf0,rrf0
      real, intent(in), dimension(ibb2:ibe2,jbb2:jbe2,kbb2:kbe2) :: thrd
      real, intent(in), dimension(ib:ie,jb:je) :: zs
      real, intent(in), dimension(itb:ite,jtb:jte) :: gz,rgz,gzu,rgzu,gzv,rgzv,dzdx,dzdy
      real, intent(in), dimension(itb:ite,jtb:jte,ktb:kte) :: gx,gxu,gy,gyv
      real, intent(inout), dimension(ib:ie,jb:je,nrain) :: rain,sws,svs,sps,srs,sgs,sus,shs
      real, intent(inout), dimension(ib:ie,jb:je) :: tsk,znt,zntmp,ust,thflux,qvflux,cd,ch,cq,u1,v1,s1,psfc,tlh
      real, intent(in),    dimension(ib:ie,jb:je) :: xland,f2d
      real, intent(inout), dimension(ib:ie,jb:je) :: prate
      real, intent(inout), dimension(jb:je,kb:ke) :: radbcw,radbce
      real, intent(inout), dimension(ib:ie,kb:ke) :: radbcs,radbcn
      real, intent(inout), dimension(ib:ie,jb:je) :: dtu,dtv
      real, intent(in),    dimension(ib:ie,jb:je) :: dtu0,dtv0
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: divx,rho,rr,rf,prs
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: t11,t12,t13,t22,t23,t33
      real, intent(in), dimension(ibnba:ienba,jbnba:jenba,kbnba:kenba) :: m11,m12,m13,m22,m23,m33
      real, intent(in), dimension(ib:ie+1,jb:je,kb:ke) :: u0
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: rru,ua,u3d,uten,uten1
      real, intent(in), dimension(ib:ie,jb:je+1,kb:ke) :: v0
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: rrv,va,v3d,vten,vten1
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: rrw,wa,w3d,wten,wten1
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: ppi,pp3d,ppten,sten,sadv,ppx
      real, intent(inout), dimension(ibph:ieph,jbph:jeph,kbph:keph) :: phi1,phi2
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: tha,th3d,thten,thten1,thterm
      real, intent(inout), dimension(ibm:iem,jbm:jem,kbm:kem) :: qpten,qtten,qvten,qcten
      real, intent(inout), dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: qa,q3d,qten
      real, intent(inout), dimension(ibc:iec,jbc:jec,kbc:kec) :: kmh,kmv,khh,khv
      real, intent(inout), dimension(ibt:iet,jbt:jet,kbt:ket) :: tkea,tke3d,tketen
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: nm,defv,defh,lenscl,dissten,epst,epsd1,epsd2
      real, intent(inout), dimension(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) :: qke_adv,qke,qke3d
      real, intent(inout), dimension(ibb:ieb,jbb:jeb,kbb:keb) :: thpten,qvpten,qcpten,qipten,upten,vpten,qnipten,qncpten
      real, intent(inout), dimension(ibr:ier,jbr:jer,kbr:ker) :: o30
      real, intent(inout), dimension(ibr:ier,jbr:jer) :: zir
      real, intent(inout), dimension(ibr:ier,jbr:jer,kbr:ker) :: swten,lwten
      real, intent(inout), dimension(ibr:ier,jbr:jer,kbr:ker) :: effc,effi,effs,effr,effg,effis
      integer, intent(inout), dimension(ibl:iel,jbl:jel) :: lu_index
      integer, intent(inout), dimension(ibl:iel,jbl:jel) :: kpbl2d
      real, intent(inout), dimension(ibl:iel,jbl:jel) :: u10,v10,s10,hfx,qfx, &
                                      hpbl,wspd,psim,psih,gz1oz0,br,          &
                                      CHS,CHS2,CQS2,CPMM,ZOL,MAVAIL,          &
                                      MOL,RMOL,REGIME,LH,FLHC,FLQC,QGH,       &
                                      CK,CKA,CDA,USTM,QSFC,T2,Q2,TH2,EMISS,THC,ALBD,   &
                                      gsw,glw,chklowq,capg,snowc,dsxy,wstar,delta,fm,fh
      real, intent(in), dimension(num_soil_layers) :: slab_zs,slab_dzs
      real, intent(inout), dimension(ibl:iel,jbl:jel,num_soil_layers) :: tslb
      real, intent(inout), dimension(ibl:iel,jbl:jel) :: tmn,tml,t0ml,hml,h0ml,huml,hvml,tmoml
      real, intent(inout), dimension(ibp:iep,jbp:jep,kbp:kep,npt) :: pta,pt3d,ptten
      real, intent(inout), dimension(nparcelsLocal,npvals) :: pdata
      real, intent(in), dimension(ipb:ipe,jpb:jpe,kpb:kpe) :: cfb
      real, intent(in), dimension(kpb:kpe) :: cfa,cfc,ad1,ad2
      complex, intent(inout), dimension(ipb:ipe,jpb:jpe,kpb:kpe) :: pdt,lgbth,lgbph
      complex, intent(inout), dimension(ipb:ipe,jpb:jpe) :: rhs,trans
      logical, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: flag
      real, intent(in), dimension(kb:ke) :: gamk,gamwall,kmw,u1b,v1b
      real, intent(inout), dimension(ib2pt:ie2pt,jb2pt:je2pt,kb2pt:ke2pt) :: u2pt,v2pt
      integer, intent(inout), dimension(rmp) :: reqs_u,reqs_v,reqs_w,reqs_s,reqs_p,reqs_p2,reqs_p3,reqs_x,reqs_y,reqs_z,reqs_tk
      integer, intent(inout), dimension(rmp,numq) :: reqs_q
      integer, intent(inout), dimension(rmp,npt) :: reqs_t
      real, intent(inout), dimension(kmt) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      real, intent(inout), dimension(cmp,cmp,kmt+1) :: n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2
      real, intent(inout), dimension(jmp,kmp-1) :: ww1,ww2,we1,we2
      real, intent(inout), dimension(imp,kmp-1) :: ws1,ws2,wn1,wn2
      real, intent(inout), dimension(jmp,kmp) :: pw1,pw2,pe1,pe2
      real, intent(inout), dimension(imp,kmp) :: ps1,ps2,pn1,pn2
      real, intent(inout), dimension(jmp,kmp) :: p2w1,p2w2,p2e1,p2e2
      real, intent(inout), dimension(imp,kmp) :: p2s1,p2s2,p2n1,p2n2
      real, intent(inout), dimension(jmp,kmp) :: p3w1,p3w2,p3e1,p3e2
      real, intent(inout), dimension(imp,kmp) :: p3s1,p3s2,p3n1,p3n2
      real, intent(inout), dimension(jmp,kmp) :: vw1,vw2,ve1,ve2
      real, intent(inout), dimension(imp,kmp) :: vs1,vs2,vn1,vn2
      real, intent(inout), dimension(jmp,kmp) :: zw1,zw2,ze1,ze2
      real, intent(inout), dimension(imp,kmp) :: zs1,zs2,zn1,zn2
      real, intent(inout), dimension(cmp,jmp,kmp)   :: uw31,uw32,ue31,ue32
      real, intent(inout), dimension(imp+1,cmp,kmp) :: us31,us32,un31,un32
      real, intent(inout), dimension(cmp,jmp+1,kmp) :: vw31,vw32,ve31,ve32
      real, intent(inout), dimension(imp,cmp,kmp)   :: vs31,vs32,vn31,vn32
      real, intent(inout), dimension(cmp,jmp,kmp-1) :: ww31,ww32,we31,we32
      real, intent(inout), dimension(imp,cmp,kmp-1) :: ws31,ws32,wn31,wn32
      real, intent(inout), dimension(cmp,jmp,kmp)   :: sw31,sw32,se31,se32
      real, intent(inout), dimension(imp,cmp,kmp)   :: ss31,ss32,sn31,sn32
      real, intent(inout), dimension(cmp,jmp,kmp)   :: rw31,rw32,re31,re32
      real, intent(inout), dimension(imp,cmp,kmp)   :: rs31,rs32,rn31,rn32
      real, intent(inout), dimension(cmp,jmp,kmp,numq) :: qw31,qw32,qe31,qe32
      real, intent(inout), dimension(imp,cmp,kmp,numq) :: qs31,qs32,qn31,qn32
      real, intent(inout), dimension(cmp,jmp,kmt)   :: tkw1,tkw2,tke1,tke2
      real, intent(inout), dimension(imp,cmp,kmt)   :: tks1,tks2,tkn1,tkn2
      real, intent(inout), dimension(jmp,kmt,4)     :: kw1,kw2,ke1,ke2
      real, intent(inout), dimension(imp,kmt,4)     :: ks1,ks2,kn1,kn2
      real, intent(inout), dimension(cmp,jmp,kmp,npt) :: tw1,tw2,te1,te2
      real, intent(inout), dimension(imp,cmp,kmp,npt) :: ts1,ts2,tn1,tn2
      real, intent(inout), dimension(d3i,d3j) :: dat1
      real, intent(inout), dimension(d2i,d2j) :: dat2
      real, intent(inout), dimension(d3i,d3j,d3n) :: dat3
      integer, intent(inout), dimension(d3t) :: reqt
      integer, intent(inout) :: reqc
      real, intent(inout) , dimension(ibdt:iedt,jbdt:jedt,kbdt:kedt,ntdiag) :: tdiag
      real, intent(inout) , dimension(ibdq:iedq,jbdq:jedq,kbdq:kedq,nqdiag) :: qdiag
      real, intent(inout) , dimension(ibdv:iedv,jbdv:jedv,kbdv:kedv,nudiag) :: udiag
      real, intent(inout) , dimension(ibdv:iedv,jbdv:jedv,kbdv:kedv,nvdiag) :: vdiag
      real, intent(inout) , dimension(ibdv:iedv,jbdv:jedv,kbdv:kedv,nwdiag) :: wdiag
      real, intent(inout) , dimension(ibdk:iedk,jbdk:jedk,kbdk:kedk,nkdiag) :: kdiag
      real, intent(inout) , dimension(ibdp:iedp,jbdp:jedp,kbdp:kedp,npdiag) :: pdiag
      real, intent(inout) , dimension(ib2d:ie2d,jb2d:je2d,nout2d) :: out2d
      real, intent(inout) , dimension(ib3d:ie3d,jb3d:je3d,kb3d:ke3d,nout3d) :: out3d

      logical, intent(in), dimension(ibib:ieib,jbib:jeib,kbib:keib) :: bndy
      integer, intent(in), dimension(ibib:ieib,jbib:jeib) :: kbdy
      integer, intent(in), dimension(ibib:ieib,jbib:jeib,kmaxib) :: hflxw,hflxe,hflxs,hflxn

      real, intent(inout), dimension(irecywe,jrecywe,krecy) :: urecyw,vrecyw,wrecyw,trecyw
      real, intent(inout), dimension(irecysn,jrecysn,krecy) :: urecys,vrecys,wrecys,trecys

      logical, intent(in) :: dowriteout,dorad,dotdwrite,doazimwrite,dorestart
      logical, intent(inout) :: getdbz,getvt,dotbud,doqbud,doubud,dovbud,dowbud

!-----------------------------------------------------------------------
! Arrays and variables defined inside solve

      integer :: i,j,k,n,nrk,bflag,pdef,diffit,k1,k2,ii
      integer :: has_reqc,has_reqi,has_reqs,do_radar_ref,ke_diag

      real :: delqv,delpi,delth,delt,fac,epsd,dheat,dz1,xs
      real :: foo1,foo2
      real :: dttmp,rtime,rdt,tem,tem0,tem1,tem2,thrad,prad
      real :: r1,r2,tnew,pnew,pinew,thnew,qvnew
      real :: gamm,aiu
      real :: qv,qli,cpli,cpm,cvm,qmax
      real :: tn,qn,nudgefac,taunudge

      real :: umod,uref,zref,oldval,newval

      double precision :: weps,afoo,bfoo,p0,p2
      real(kind=qp) :: tmp1,tmp2
      real(kind=qp) :: temq1,temq2

      logical :: get_time_avg,dopf

      double precision, dimension(nk) :: dumk3,dumk4

#ifdef MPI
      integer :: reqp1,reqp2
#endif
      character(len=6) :: text1,text2

       !$acc declare create(dumk3,dumk4)
       !$acc declare &
       !$acc present(xh,rxh,arh1,arh2,uh,ruh,arf1,arf2) &
       !$acc present(xf,yh,vh,rvh,uf,vf,mf) &
       !$acc present(u0,v0) &
       !$acc present(rds,sigma,rdsf,sigmaf) &
       !$acc present(mh,rmh,zh,c1,c2,wprof) &
       !$acc present(gz,rgz,gzv,gzu,rgzu,rgzv) &
       !$acc present(f2d) &
       !$acc present(gxu,gyv,dzdx,dzdy) &
       !$acc present(pi0,rho0,prs0,th0,rth0,qv0,qc0,qi0,rr0,rf0,rrf0) &
       !$acc present(dumk1,dumk2) &
       !$acc present(radbcw,radbce,radbcs,radbcn,dtu,dtv,dtu0,dtv0) &
       !$acc present(t11,t12,t13,t22,t23) &
       !$acc present(dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9) &
       !$acc present(divx,rho,rr,prs) &
       !$acc present(rru,ua,u3d,uten,uten1) &
       !$acc present(rrv,va,v3d,vten,vten1) &
       !$acc present(rrw,wa,w3d,wten,wten1) &
       !$acc present(ppi,pp3d,ppten,sten,sadv,ppx) &
       !$acc present(tha,th3d,thten,thten1,thterm) &
       !$acc present(qpten,qtten,qvten,qcten,qa,q3d,qten) &
       !$acc present(qke_adv,qke,qke3d) &
       !$acc present(tkea,tke3d,tketen) &
       !$acc present(pta,pt3d,ptten) &
       !$acc present(pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2) &
       !$acc present(p2w1,p2w2,p2e1,p2e2,p2s1,p2s2,p2n1,p2n2) &
       !$acc present(zw1,zw2,ze1,ze2,zs1,zs2,zn1,zn2) &
       !$acc present(vw1,vw2,ve1,ve2,vs1,vs2,vn1,vn2) &
       !$acc present(ww1,ww2,we1,we2,ws1,ws2,wn1,wn2) &
       !$acc present(uw31,uw32,ue31,ue32,us31,us32,un31,un32) &
       !$acc present(vw31,vw32,ve31,ve32,vs31,vs32,vn31,vn32) &
       !$acc present(ww31,ww32,we31,we32,ws31,ws32,wn31,wn32) &
       !$acc present(rw31,rw32,re31,re32,rs31,rs32,rn31,rn32) &
       !$acc present(qw31,qw32,qe31,qe32,qs31,qs32,qn31,qn32) &
       !$acc present(sw31,sw32,se31,se32,ss31,ss32,sn31,sn32) &
       !$acc present(tw1,tw2,te1,te2,ts1,ts2,tn1,tn2) &
       !$acc present(tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2) &
       !$acc present(flag) &
       !$acc present(tdiag,qdiag,udiag,vdiag,wdiag,kdiag) &
       !$acc present(bndy,kbdy)

!--------------------------------------------------------------------
#ifdef MPI
      nf=0
      nu=0
      nv=0
      nw=0
#endif

      afoo=0.0d0
      bfoo=0.0d0

      if(timestats.ge.1) time_misc13=time_misc13+mytime()


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CC   Begin RK section   CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      ! time at end of full timestep:
      rtime=sngl(mtime+dt)

!--------------------------------------------------------------------
! RK3 begin

      rkloop:  &
      DO NRK=1,nrkmax

        dttmp=dt/float(nrkmax+1-nrk)

!--------------------------------------------------------------------
!  Calculate misc. variables
!
!    These arrays store variables that are used later in the
!    SOUND subroutine.  Do not modify t11 or t22 until after sound!
!
!    dum1 = vapor
!    dum2 = all liquid
!    dum3 = all solid
!    t11 = theta_rho
!    t22 = ppterm
!    dum8 = buoyancy at s pts
     !KLUDGE this to eliminated PCAST ERRORS
     !$acc parallel loop gang vector collapse(3) default(present)
     do k=kb,ke
     do j=jb,je
     do i=ib,ie
        dum2(i,j,k) = 0.0
     enddo
     enddo
     enddo

        IF( imoist.eq.1 )THEN
          ! moist:

        ifql:  &
        if( nql1.le.0 )then

          !$acc parallel loop gang vector collapse(3) default(present) 
          do k=1,nk
          do j=1,nj
          do i=1,ni
            dum2(i,j,k)=0.0
          enddo
          enddo
          enddo

        else  ifql

          !$acc parallel loop gang vector collapse(3) default(present) 
          do k=1,nk
          do j=1,nj
          do i=1,ni
            dum2(i,j,k)=q3d(i,j,k,nql1)
          enddo
          enddo
          enddo

          do n=nql1+1,nql2
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            dum2(i,j,k)=dum2(i,j,k)+q3d(i,j,k,n)
          enddo
          enddo
          enddo
          enddo

        endif  ifql


          IF(iice.eq.1)THEN

            !$acc parallel loop gang vector collapse(3) default(present) 
            do k=1,nk
            do j=1,nj
            do i=1,ni
              dum3(i,j,k)=q3d(i,j,k,nqs1)
            enddo
            enddo
            enddo

            do n=nqs1+1,nqs2
            !$acc parallel default(present)
            !$acc loop gang vector collapse(3)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              dum3(i,j,k)=dum3(i,j,k)+q3d(i,j,k,n)
            enddo
            enddo
            enddo
            !$acc end parallel
            enddo

          ELSE

            !$acc parallel loop gang vector collapse(3) default(present)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              dum3(i,j,k)=0.0
            enddo
            enddo
            enddo

          ENDIF
          IF(eqtset.eq.2)THEN

            !$acc parallel loop gang vector collapse(3) default(present) 
            do k=1,nk
            do j=1,nj
            do i=1,ni
              qv=max(q3d(i,j,k,nqv),0.0)
              qli=max(0.0,dum2(i,j,k)+dum3(i,j,k))
              cpli=cpl*max(0.0,dum2(i,j,k))+cpi*max(0.0,dum3(i,j,k))
              dum8(i,j,k) = g*( th3d(i,j,k)*rth0(i,j,k) + repsm1*(qv-qv0(i,j,k)) - (qli-qc0(i,j,k)-qi0(i,j,k)) )
              t11(i,j,k)=(th0(i,j,k)+th3d(i,j,k))*(1.0+reps*qv)/(1.0+qv+qli)
          ! terms in theta and pi equations for proper mass/energy conservation
          ! Reference:  Bryan and Fritsch (2002, MWR), Bryan and Morrison (2012, MWR)
              cpm=cp+cpv*qv+cpli
              cvm=1.0/(cv+cvv*qv+cpli)
              thterm(i,j,k)=(th0(i,j,k)+th3d(i,j,k))*( rd+rv*qv-rovcp*cpm )*cvm
              t22(i,j,k)=(pi0(i,j,k)+pp3d(i,j,k))*rovcp*cpm*cvm
            enddo
            enddo
            enddo

          ELSEIF(eqtset.eq.1)THEN

            !print *,'before eqtset==1 loop'
            !$acc parallel loop gang vector collapse(3) default(present) 
            do k=1,nk
            do j=1,nj
            do i=1,ni
              qv=max(q3d(i,j,k,nqv),0.0)
              qli=max(0.0,dum2(i,j,k)+dum3(i,j,k))
              dum8(i,j,k) = g*( th3d(i,j,k)*rth0(i,j,k) + repsm1*(qv-qv0(i,j,k)) - (qli-qc0(i,j,k)-qi0(i,j,k)) )
              t11(i,j,k)=(th0(i,j,k)+th3d(i,j,k))*(1.0+reps*qv)/(1.0+qv+qli)
              t22(i,j,k)=(pi0(i,j,k)+pp3d(i,j,k))*rddcv
            enddo
            enddo
            enddo

          ENDIF

        ELSE
          ! dry:
          !$acc parallel loop gang vector collapse(3) default(present) 
          do k=1,nk
          do j=1,nj
          do i=1,ni
            dum8(i,j,k) = g*th3d(i,j,k)*rth0(i,j,k)
            t11(i,j,k)=th0(i,j,k)+th3d(i,j,k)
            t22(i,j,k)=(pi0(i,j,k)+pp3d(i,j,k))*rddcv
          enddo
          enddo
          enddo
        ENDIF
        if(timestats.ge.1) time_buoyan=time_buoyan+mytime()

!--------------------------------------------------------------------
        !print *,'solve2: before call to bcs_GPU(t11) #1'
        call bcs_GPU(t11)
        !print *,'solve2: after call to bcs_GPU(t11) #1'
#ifdef MPI
        call comm_1s_start_GPU(t11,p2w1,p2w2,p2e1,p2e2,p2s1,p2s2,p2n1,p2n2,reqs_p2)
#endif

!--------------------------------------------------------------------
!  Set RK tendency arrays:

        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) 
        do k=1,nk
        do j=1,nj+1
        do i=1,ni+1
          uten(i,j,k)=uten1(i,j,k)
          vten(i,j,k)=vten1(i,j,k)
          wten(i,j,k)=wten1(i,j,k)
        enddo
        enddo
        enddo
        if(timestats.ge.1) time_misc14=time_misc14+mytime()
        !print *,'solve2: point #2'
!--------------------------------------------------------------------
        IF(nrk.ge.2)THEN
#ifdef MPI
          call comm_3u_end_GPU(u3d,uw31,uw32,ue31,ue32,   &
                               us31,us32,un31,un32,reqs_u)
          call comm_3v_end_GPU(v3d,vw31,vw32,ve31,ve32,   &
                               vs31,vs32,vn31,vn32,reqs_v)
          call comm_3w_end_GPU(w3d,ww31,ww32,we31,we32,   &
                               ws31,ws32,wn31,wn32,reqs_w)
#endif
          if(terrain_flag)then
            call bcwsfc(gz,dzdx,dzdy,u3d,v3d,w3d)
            call bc2d_GPU(w3d(ib,jb,1))
          endif
        ENDIF
        !print *,'solve2: point #27'
!--------------------------------------------------------------------
!  Get rru,rrv,rrw,divx
!  (NOTE:  do not change these arrays until after small steps)
    IF(.not.terrain_flag)THEN
      ! without terrain:

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present) 
      DO k=1,nk
        do j=0,nj+1
        do i=0,ni+2
          rru(i,j,k)=rho0(1,1,k)*u3d(i,j,k)
        enddo
        enddo
      ENDDO

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present) 
      DO k=1,nk
        do j=0,nj+2
        do i=0,ni+1
          rrv(i,j,k)=rho0(1,1,k)*v3d(i,j,k)
        enddo
        enddo
      ENDDO

      !$omp parallel do default(shared) private(i,j)
      !$acc parallel loop gang vector collapse(2) default(present)
      do j=0,nj+1
      do i=0,ni+1
        rrw(i,j,   1) = 0.0
        rrw(i,j,nk+1) = 0.0
      enddo
      enddo

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present)
      DO k=2,nk
        do j=0,nj+1
        do i=0,ni+1
          rrw(i,j,k)=rf0(1,1,k)*w3d(i,j,k)
        enddo
        enddo
      ENDDO

    ELSE
      ! with terrain:
      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present)
      DO k=1,nk
        do j=0,nj+1
        do i=0,ni+2
          rru(i,j,k)=0.5*(rho0(i-1,j,k)+rho0(i,j,k))*u3d(i,j,k)*rgzu(i,j)
        enddo
        enddo
      ENDDO

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present)
      DO k=1,nk
        do j=0,nj+2
        do i=0,ni+1
          rrv(i,j,k)=0.5*(rho0(i,j-1,k)+rho0(i,j,k))*v3d(i,j,k)*rgzv(i,j)
        enddo
        enddo
      ENDDO

      !$omp parallel do default(shared) private(i,j)
      !$acc parallel loop gang vector collapse(2) default(present)
      do j=0,nj+1
      do i=0,ni+1
        rrw(i,j,   1) = 0.0
        rrw(i,j,nk+1) = 0.0
      enddo
      enddo

      !$omp parallel do default(shared) private(i,j,k,r1,r2)
      !$acc parallel loop gang vector default(present)
      DO k=1,nk
          r2 = (sigmaf(k)-sigma(k-1))*rds(k)
          r1 = 1.0-r2
          r1 = 0.5*r1
          r2 = 0.5*r2
          do j=0,nj+1
          do i=0,ni+1
            rrw(i,j,k)=rf0(i,j,k)*w3d(i,j,k)                              &
                      +( ( r2*(rru(i,j,k  )+rru(i+1,j,k  ))               &
                          +r1*(rru(i,j,k-1)+rru(i+1,j,k-1)) )*dzdx(i,j)   &
                        +( r2*(rrv(i,j,k  )+rrv(i,j+1,k  ))               &
                          +r1*(rrv(i,j,k-1)+rrv(i,j+1,k-1)) )*dzdy(i,j)   &
                       )*(sigmaf(k)-zt)*gz(i,j)*rzt
          enddo
          enddo
      ENDDO

    ENDIF
    if(timestats.ge.1) time_advs=time_advs+mytime()

        !print *,'solve2: point #4'
        IF(terrain_flag)THEN
          call bcw_GPU(rrw,0)
#ifdef MPI
          call comm_1w_start_GPU(rrw,ww1,ww2,we1,we2,   &
                                 ws1,ws2,wn1,wn2,reqs_w)
          call comm_1w_end_GPU(rrw,ww1,ww2,we1,we2,   &
                               ws1,ws2,wn1,wn2,reqs_w)
#endif
        ENDIF
        !print *,'solve2: point #5'

      IF(.not.terrain_flag)THEN
        IF(axisymm.eq.0)THEN
          ! Cartesian without terrain:
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=0,nj+1
          do i=0,ni+1
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
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=0,nj+1
          do i=0,ni+1
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
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=0,nj+1
          do i=0,ni+1
            divx(i,j,k)=( (rru(i+1,j,k)-rru(i,j,k))*rdx*uh(i)        &
                         +(rrv(i,j+1,k)-rrv(i,j,k))*rdy*vh(j) )      &
                         +(rrw(i,j,k+1)-rrw(i,j,k))*rdsf(k)
            if(abs(divx(i,j,k)).lt.smeps) divx(i,j,k)=0.0
          enddo
          enddo
          enddo
      ENDIF
      if(timestats.ge.1) time_divx=time_divx+mytime()

!--------------------------------------------------------------------
!  Coriolis terms:

      IF( icor.eq.1 )THEN

        IF(axisymm.eq.0)THEN

          ! for Cartesian grid:

          if( betaplane.eq.0 )then
            ! f plane:
            !$omp parallel do default(shared) private(i,j,k)
            !$acc parallel loop gang vector collapse(3) default(present)
            do k=1,nk
            do j=1,nj+1
            do i=1,ni+1
              uten(i,j,k)=uten(i,j,k)+fcor*( 0.25*( (v3d(i  ,j,k)+v3d(i  ,j+1,k)) &
                                                   +(v3d(i-1,j,k)+v3d(i-1,j+1,k)) ) + vmove )
              vten(i,j,k)=vten(i,j,k)-fcor*( 0.25*( (u3d(i,j  ,k)+u3d(i+1,j  ,k)) &
                                                   +(u3d(i,j-1,k)+u3d(i+1,j-1,k)) ) + umove )
            enddo
            enddo
            enddo

          elseif( betaplane.eq.1 )then

            !$omp parallel do default(shared) private(i,j,k)
            !$acc parallel loop gang vector collapse(3) default(present)
            do k=1,nk
            do j=1,nj+1
            do i=1,ni+1
              ! beta plane:
              uten(i,j,k)=uten(i,j,k)+0.125*(f2d(i,j)+f2d(i-1,j))           &
                                           *( (v3d(i  ,j,k)+v3d(i  ,j+1,k)) &
                                             +(v3d(i-1,j,k)+v3d(i-1,j+1,k)) )
              vten(i,j,k)=vten(i,j,k)-0.125*(f2d(i,j)+f2d(i,j-1))           &
                                           *( (u3d(i,j  ,k)+u3d(i+1,j  ,k)) &
                                             +(u3d(i,j-1,k)+u3d(i+1,j-1,k)) )
            enddo
            enddo
            enddo

          endif

        ELSE

            ! for axisymmetric grid:
            ! note for axisymmetric grid: since cm1r18, Coriolis term for v is included in advvaxi
            !$omp parallel do default(shared) private(i,j,k)
            !$acc parallel loop gang vector collapse(3) default(present)
            do k=1,nk
            do j=1,nj
            do i=1,ni+1
              uten(i,j,k)=uten(i,j,k)+fcor*0.5*(v3d(i,j,k)+v3d(i-1,j,k))
            enddo
            enddo
            enddo

        ENDIF


          !........ budget (infrequent) ........!
          IF( doubud .and. ud_cor.ge.1 .and. nrk.eq.nrkmax )THEN
            !$omp parallel do default(shared) private(i,j,k)
            !$acc parallel loop gang vector collapse(3) default(present)
            do k=1,nk
            do j=1,nj
            do i=1,ni+1
              udiag(i,j,k,ud_cor) = uten(i,j,k)-uten1(i,j,k)
            enddo
            enddo
            enddo
          ENDIF
          IF( dovbud .and. vd_cor.ge.1 .and. nrk.eq.nrkmax )THEN
            !$omp parallel do default(shared) private(i,j,k)
            !$acc parallel loop gang vector collapse(3) default(present)
            do k=1,nk
            do j=1,nj+1
            do i=1,ni
              vdiag(i,j,k,vd_cor) = vten(i,j,k)-vten1(i,j,k)
            enddo
            enddo
            enddo
          ENDIF
        !........ end budget ........!

        if(timestats.ge.1) time_cor=time_cor+mytime()

      ENDIF

!--------------------------------------------------------------------
!  U-equation


        if( nudgeobc.eq.1 .and. wbc.eq.2 .and. ibw.eq.1 )then
          ! 190315: nudge inflow point back towards base state:
          tem = 1.0/alphobc
          !$acc parallel loop gang vector collapse(2) default(present)
          do k=1,nk
          do j=1,nj
            if( u3d(1,j,k).gt.0.0 )then
              uten(1,j,k) = uten(1,j,k)-(u3d(1,j,k)-u0(1,j,k))*tem
            endif
          enddo
          enddo
        endif
        if( nudgeobc.eq.1 .and. ebc.eq.2 .and. ibe.eq.1 )then
          ! 190315: nudge inflow point back towards base state:
          tem = 1.0/alphobc
          !$acc parallel loop gang vector collapse(2) default(present)
          do k=1,nk
          do j=1,nj
            if( u3d(ni+1,j,k).lt.0.0 )then
              uten(ni+1,j,k) = uten(ni+1,j,k)-(u3d(ni+1,j,k)-u0(ni+1,j,k))*tem
            endif
          enddo
          enddo
        endif



        ! inertial term for axisymmetric grid:
        if(axisymm.eq.1)then

          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=1,nj
          do i=1,ni+1
            dum1(i,j,k)=(v3d(i,j,k)**2)*rxh(i)
          enddo
          enddo
          enddo

          if(ebc.eq.3)then
            !$omp parallel do default(shared) private(i,j,k)
            !$acc parallel loop gang vector collapse(2) default(present)
            do k=1,nk
            do j=1,nj
              dum1(ni+1,j,k) = -dum1(ni,j,k)
            enddo
            enddo
          endif

          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=1,nj
          do i=2,ni+1
            uten(i,j,k)=uten(i,j,k)+0.5*(dum1(i-1,j,k)+dum1(i,j,k))
          enddo
          enddo
          enddo

          IF( doubud .and. nrk.eq.nrkmax )THEN
            !$omp parallel do default(shared) private(i,j,k)
            !$acc parallel loop gang vector collapse(3) default(present)
            do k=1,nk
            do j=1,nj
            do i=2,ni+1
              udiag(i,j,k,ud_cent) = 0.5*(dum1(i-1,j,k)+dum1(i,j,k))
            enddo
            enddo
            enddo
          ENDIF

          if(timestats.ge.1) time_advu=time_advu+mytime()

        endif

!--------------------------------------------------------------------
!  V-equation


        if( nudgeobc.eq.1 .and. sbc.eq.2 .and. ibs.eq.1 )then
          ! 190315: nudge inflow point back towards base state:
          tem = 1.0/alphobc
          !$acc parallel loop gang vector collapse(2) default(present)
          do k=1,nk
          do i=1,ni
            if( v3d(i,1,k).gt.0.0 )then
              vten(i,1,k) = vten(i,1,k)-(v3d(i,1,k)-v0(i,1,k))*tem
            endif
          enddo
          enddo
        endif
        if( nudgeobc.eq.1 .and. nbc.eq.2 .and. ibn.eq.1 )then
          ! 190315: nudge inflow point back towards base state:
          tem = 1.0/alphobc
          !$acc parallel loop gang vector collapse(2) default(present)
          do k=1,nk
          do i=1,ni
            if( v3d(i,nj+1,k).lt.0.0 )then
              vten(i,nj+1,k) = vten(i,nj+1,k)-(v3d(i,nj+1,k)-v0(i,nj+1,k))*tem
            endif
          enddo
          enddo
        endif



!!!        ! since cm1r17, this term is included in advvaxi
!!!        if(axisymm.eq.1)then
!!!          ! for axisymmetric grid:
!!!
!!!!$omp parallel do default(shared)  &
!!!!$omp private(i,j,k)
!!!          do k=1,nk
!!!          do j=1,nj
!!!          do i=1,ni
!!!            vten(i,j,k)=vten(i,j,k)-(v3d(i,j,k)*rxh(i))*0.5*(xf(i)*u3d(i,j,k)+xf(i+1)*u3d(i+1,j,k))*rxh(i)
!!!          enddo
!!!          enddo
!!!          enddo
!!!
!!!        endif




        IF( dovbud )THEN
        IF( axisymm.eq.1 .and. nrk.eq.nrkmax )THEN
          !  Diagnostics for axisymm:
          !$omp parallel do default(shared) private(i,j,k,tem1,tem2)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=1,1
          do i=1,ni
            ! estimate Coriolis:
            tem1 = -fcor*0.5*(xf(i)*u3d(i,j,k)+xf(i+1)*u3d(i+1,j,k))*rxh(i)
            ! estimate centrifugal accel:
            tem2 = -(v3d(i,j,k)*rxh(i))*0.5*(xf(i)*u3d(i,j,k)+xf(i+1)*u3d(i+1,j,k))*rxh(i)

            vdiag(i,j,k,vd_cor)  = tem1
            vdiag(i,j,k,vd_cent) = tem2
            vdiag(i,j,k,vd_hadv) = vdiag(i,j,k,vd_hadv) - tem1 - tem2

            vdiag(i,2,k,vd_cor)  = vdiag(i,1,k,vd_cor)
            vdiag(i,2,k,vd_cent) = vdiag(i,1,k,vd_cent)
            vdiag(i,2,k,vd_hadv) = vdiag(i,1,k,vd_hadv)
          enddo
          enddo
          enddo
        ENDIF
        ENDIF

!--------------------------------------------------------------------
        !print *,'solve2: point #6'
#ifdef MPI
        IF(nrk.ge.2)THEN
          call comm_1s_end_GPU(rho,pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_s)
          call getcorner_GPU(rho,nw1(1),nw2(1),ne1(1),ne2(1),sw1(1),sw2(1),se1(1),se2(1))
          call bcs2_GPU(rho)
        ENDIF
#endif
        !print *,'solve2: point #7'
!--------------------------------------------------------------------
!  advection:
          call advu(nrk   ,arh1,arh2,uh,xf,rxf,arf1,arf2,uf,vh,gz,rgz,gzu,mh,rho0,rr0,rf0,rrf0,dum1,dum2,dum3,dum4,dum5,dum6,dum7,divx, &
                     rru,u3d,uten,rrv,rrw,rdsf,c1,c2,rho,dttmp,doubud,udiag,wprof,bndy,kbdy,hflxw,hflxe,hflxs,hflxn,vf,mf,v3d,w3d)
          call advv(nrk   ,xh,rxh,arh1,arh2,uh,xf,vh,vf,gz,rgz,gzv,mh,rho0,rr0,rf0,rrf0,dum1,dum2,dum3,dum4,dum5,dum6,dum7,divx, &
                     rru,rrv,v3d,vten,rrw,rdsf,c1,c2,rho,dttmp,dovbud,vdiag,wprof,bndy,kbdy,hflxw,hflxe,hflxs,hflxn,uf,mf,u3d,w3d)
          call   advw(nrk   ,xh,rxh,arh1,arh2,uh,xf,vh,gz,rgz,mh,mf,rho0,rr0,rf0,rrf0,  &
                      dum1,dum2,dum3,dum4,dum5,dum6,dum7,divx,                       &
                      rru,rrv,rrw,w3d  ,wten,rds,rdsf,c1,c2,rho,dttmp,               &
                      dowbud ,wdiag,hadvordrv,vadvordrv,advwenov,bndy,kbdy,uf,vf,u3d,v3d,hflxw,hflxe,hflxs,hflxn)

!--------------------------------------------------------------------
!  Buoyancy

        ! dum8 stores buoyancy at s pts:
 
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          wten(i,j,k)=wten(i,j,k)+(c1(i,j,k)*dum8(i,j,k-1)+c2(i,j,k)*dum8(i,j,k))
        enddo
        enddo
        enddo

        if( dowbud .and. nrk.eq.nrkmax )then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=2,nk
          do j=1,nj
          do i=1,ni
            wdiag(i,j,k,wd_buoy) = (c1(i,j,k)*dum8(i,j,k-1)+c2(i,j,k)*dum8(i,j,k))
          enddo
          enddo
          enddo
        endif

        if(timestats.ge.1) time_buoyan=time_buoyan+mytime()

!--------------------------------------------------------------------

        IF( (doubud.or.dovbud.or.dowbud) .and. nrk.eq.nrkmax )THEN
          ! bug fix, 170725
          ! save velocity tendencies before pgrad calculations:
          if( ud_pgrad.ge.1 )then
            !$omp parallel do default(shared) private(i,j,k)
            !$acc parallel loop gang vector collapse(3) default(present)
            do k=1,nk
            do j=1,nj+1
            do i=1,ni+1
              udiag(i,j,k,ud_pgrad) = uten(i,j,k)
            enddo
            enddo
            enddo
          endif
          if( vd_pgrad.ge.1 )then
            !$omp parallel do default(shared) private(i,j,k)
            !$acc parallel loop gang vector collapse(3) default(present)
            do k=1,nk
            do j=1,nj+1
            do i=1,ni+1
              vdiag(i,j,k,vd_pgrad) = vten(i,j,k)
            enddo
            enddo
            enddo
          endif
          if( wd_pgrad.ge.1 )then
            !$omp parallel do default(shared) private(i,j,k)
            !$acc parallel loop gang vector collapse(3) default(present)
            do k=1,nk
            do j=1,nj+1
            do i=1,ni+1
              wdiag(i,j,k,wd_pgrad) = wten(i,j,k)
            enddo
            enddo
            enddo
          endif
          if(timestats.ge.1) time_misc15=time_misc15+mytime()
        ENDIF

!--------------------------------------------------------------------
!  cm1r19 terrain modification:
!  note:  this is part of horiz pressure gradient

        ! dum6 stores buoyancy:

        !print *,'solve2: point #8'
      termod1:  &
      IF( terrain_flag )THEN
        !call flush(6)
        !print *,'solve2: before bcs_GPU #2'
        call bcs_GPU(dum6)
#ifdef MPI
        call comm_1s_start_GPU(dum6,zw1,zw2,ze1,ze2,zs1,zs2,zn1,zn2,reqs_z)
        call comm_1s_end_GPU(  dum6,zw1,zw2,ze1,ze2,zs1,zs2,zn1,zn2,reqs_z)
#endif
  
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present)
        do k=2,nk
         do j=0,nj+1
          do i=0,ni+1
            dum1(i,j,k) = c1(i,j,k)*dum6(i,j,k-1)+c2(i,j,k)*dum6(i,j,k)
          enddo
          enddo
        enddo

        !$omp parallel do default(shared) private(i,j)
        !$acc parallel loop gang vector collapse(2) default(present)
        do j=0,nj+1
          do i=0,ni+1
            dum1(i,j,1) = 0.0
            dum1(i,j,nk+1) = 0.0
          enddo
        enddo

        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present)
        do k=1,nk
          ! x-dir
          do j=1,nj
          do i=1+ibw,ni+1-ibe
            uten(i,j,k) = uten(i,j,k) + ( 0.125*( (dum1(i,j,k+1)+dum1(i-1,j,k+1))    &
                                                 +(dum1(i,j,k  )+dum1(i-1,j,k  )) )  &
                                          -0.25*(dum6(i,j,k)+dum6(i-1,j,k))          &
                                        )*(gxu(i,j,k)+gxu(i,j,k+1))
          enddo
          enddo
        enddo
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present)
        do k=1,nk
          ! y-dir
          do j=1+ibs,nj+1-ibn
          do i=1,ni
            vten(i,j,k) = vten(i,j,k) + ( 0.125*( (dum1(i,j,k+1)+dum1(i,j-1,k+1))    &
                                                 +(dum1(i,j,k  )+dum1(i,j-1,k  )) )  &
                                          -0.25*(dum6(i,j,k)+dum6(i,j-1,k))          &
                                        )*(gyv(i,j,k)+gyv(i,j,k+1))
          enddo
          enddo
        enddo

      ENDIF  termod1
        !print *,'solve2: point #9'

!--------------------------------------------------------------------
!  Pressure equation

#ifdef MPI
      IF(nrk.ge.2)THEN
        call comm_1s_end_GPU(pp3d,vw1,vw2,ve1,ve2,vs1,vs2,vn1,vn2,reqs_x)
      ENDIF
#endif
      !print *,'solve2: point #10'

      IF( psolver.le.3 )THEN

        IF(.not.terrain_flag)THEN
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=0,nj+1
          do i=0,ni+1
            sadv(i,j,k)=pi0(1,1,k)+pp3d(i,j,k)
          enddo
          enddo
          enddo
        ELSE
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=0,nj+1
          do i=0,ni+1
            sadv(i,j,k)=pi0(i,j,k)+pp3d(i,j,k)
          enddo
          enddo
          enddo
        ENDIF
        if(timestats.ge.1) time_misc16=time_misc16+mytime()

      if( psolver.eq.1 )then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop vector gang default(present)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          sten(i,j,k)=ppten(i,j,k)
        enddo
        enddo
        enddo
        weps = epsilon
        diffit = 0
        call advs(nrk,0,0,bfoo,xh,rxh,arh1,arh2,uh,ruh,xf,vh,rvh,gz,rgz,mh,rmh,           &
                   rho0,rr0,rf0,rrf0,dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,divx,        &
                   rru,rrv,rrw,ppi,sadv,sten ,0,0,dttmp,weps,                             &
                   flag,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,rdsf,c1,c2,rho,rr,diffit, &
                   .false.,ibdt,iedt,jbdt,jedt,kbdt,kedt,ntdiag,tdiag,1,1,1,              &
                   1,1,1,wprof,dumk1,dumk2,2,2,kbdy,bndy,hflxw,hflxe,hflxs,hflxn,out3d)
      endif

      ENDIF

!--------------------------------------------------------------------
#ifdef MPI
        call comm_1s_end_GPU(t11,p2w1,p2w2,p2e1,p2e2,p2s1,p2s2,p2n1,p2n2,reqs_p2)
#endif
!--------------------------------------------------------------------
!  call sound

        get_time_avg = .false.


        IF(psolver.eq.1)THEN

#ifdef _OPENACC
          stop 'ERROR: soundns not ported to GPU'
#endif
          call   soundns(xh,rxh,arh1,arh2,uh,xf,uf,yh,vh,yf,vf,           &
                         zh,mh,c1,c2,mf,zf,pi0,thv0,rr0,rf0,              &
                         rds,sigma,rdsf,sigmaf,                           &
                         zs,gz,rgz,gzu,rgzu,gzv,rgzv,                     &
                         dzdx,dzdy,gx,gxu,gy,gyv,                         &
                         radbcw,radbce,radbcs,radbcn,dtu,dtu0,dtv,dtv0,   &
                         dum1,dum2,dum3,dum4,                             &
                         u0,ua,u3d,uten,                                  &
                         v0,va,v3d,vten,                                  &
                         wa,w3d,wten,                                     &
                         ppi,pp3d,sten ,t11,   t22,dttmp,nrk,rtime,mtime, &
                         bndy,kbdy)

        ELSEIF( psolver.eq.2 .or. psolver.eq.7 )THEN
          get_time_avg = .true.
          call   sounde(dt,xh,arh1,arh2,uh,ruh,xf,uf,yh,vh,rvh,yf,vf,     &
                        rds,sigma,rdsf,sigmaf,zh,mh,rmh,c1,c2,mf,zf,      &
                        pi0,rho0,rr0,rf0,rrf0,th0,rth0,thv0,zs,           &
                        gz,rgz,gzu,rgzu,gzv,rgzv,                         &
                        dzdx,dzdy,gx,gxu,gy,gyv,                          &
                        radbcw,radbce,radbcs,radbcn,dtu,dtu0,dtv,dtv0,    &
                        dum1,dum2,dum3,dum4,dum5,dum6,                    &
                        dum7,dum8,t12,t13,t23,t33,                        &
                        u0,rru,ua,u3d,uten,                               &
                        v0,rrv,va,v3d,vten,                               &
                        rrw,wa,w3d,wten,                                  &
                        ppi,pp3d,sadv ,ppten,ppx,                         &
                        t11,t22   ,nrk,dttmp,rtime,mtime,get_time_avg,    &
                        bndy,kbdy,                                        &
                        pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p)
        ELSEIF(psolver.eq.3)THEN
          get_time_avg = .true.
          call   sound( dt,xh,arh1,arh2,uh,ruh,xf,uf,yh,vh,rvh,yf,vf,     &
                        rds,sigma,rdsf,sigmaf,zh,mh,rmh,c1,c2,mf,zf,      &
                        pi0,rho0,rr0,rf0,rrf0,th0,rth0,zs,                &
                        gz,rgz,gzu,rgzu,gzv,rgzv,                         &
                        dzdx,dzdy,gx,gxu,gy,gyv,                          &
                        radbcw,radbce,radbcs,radbcn,dtu,dtu0,dtv,dtv0,    &
                        dum1,dum2,dum3,dum4,dum5,dum6,                    &
                        dum7,dum8,t12,t13,t23,dum9,                       &
                        u0,rru,ua,u3d,uten,                               &
                        v0,rrv,va,v3d,vten,                               &
                        rrw,wa,w3d,wten,                                  &
                        ppi,pp3d,sadv ,ppten,ppx,                         &
                        t11,t22   ,nrk,dttmp,rtime,mtime,get_time_avg,    &
                        bndy,kbdy,                                        &
                        pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p)
        ELSEIF(psolver.eq.4.or.psolver.eq.5)THEN
          ! anelastic/incompressible solver:

#ifdef _OPENACC
          stop 'ERROR: anelp not ported to GPU'
#endif
          call   anelp(xh,uh,ruh,xf,uf,yh,vh,rvh,yf,vf,             &
                       zh,mh,rmh,mf,rmf,zf,pi0,thv0,rho0,prs0,rf0,  &
                       rds,sigma,rdsf,sigmaf,                       &
                       gz,rgz,gzu,rgzu,gzv,rgzv,                    &
                       dzdx,dzdy,gx,gxu,gy,gyv,                     &
                       radbcw,radbce,radbcs,radbcn,                 &
                       dum1,dum2,dum3,dum4,divx,                    &
                       u0,ua,u3d,uten,                              &
                       v0,va,v3d,vten,                              &
                       wa,w3d,wten,                                 &
                       ppi,pp3d,phi1,phi2,cfb,cfa,cfc,              &
                       ad1,ad2,pdt,lgbth,lgbph,rhs,trans,dttmp,nrk,rtime,mtime)

        ELSEIF(psolver.eq.6)THEN

          get_time_avg = .true.
          call   soundcb(dt,xh,arh1,arh2,uh,ruh,xf,uf,yh,vh,rvh,yf,vf,    &
                        rds,sigma,rdsf,sigmaf,zh,mh,rmh,c1,c2,mf,zf,      &
                        pi0,rho0,rr0,rf0,rrf0,th0,rth0,zs,                &
                        gz,rgz,gzu,rgzu,gzv,rgzv,                         &
                        dzdx,dzdy,gx,gxu,gy,gyv,                          &
                        radbcw,radbce,radbcs,radbcn,                      &
                        dum1,dum2,dum3,dum4,dum5,dum6,                    &
                        dum7,dum8,t12,t13,t23,t33,                        &
                        u0,rru,ua,u3d,uten,                               &
                        v0,rrv,va,v3d,vten,                               &
                        rrw,wa,w3d,wten,                                  &
                        ppi,pp3d,sadv ,ppten,ppx,phi1,phi2,               &
                        t11,t22   ,nrk,dttmp,rtime,mtime,get_time_avg,    &
                        bndy,kbdy,                                        &
                        pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_p)

        ENDIF

!--------------------------------------------------------------------
!  Update v for axisymmetric model simulations:

        IF(axisymm.eq.1)THEN

          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            v3d(i,j,k)=va(i,j,k)+dttmp*vten(i,j,k)
          enddo
          enddo
          enddo
          if(timestats.ge.1) time_misc17=time_misc17+mytime()

        ENDIF

!--------------------------------------------------------------------
!  Diagnostics:

      IF( doubud .and. nrk.eq.nrkmax )THEN
        ! pressure gradient accel:
        rdt = 1.0/dt
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present)
        do k=1,nk
        do j=1,nj
        do i=1,ni+1
          udiag(i,j,k,ud_pgrad) = (u3d(i,j,k)-ua(i,j,k))*rdt - udiag(i,j,k,ud_pgrad)
        enddo
        enddo
        enddo
      ENDIF

      IF( dovbud .and. nrk.eq.nrkmax )THEN
        rdt = 1.0/dt
        IF( axisymm.eq.1 )THEN
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=1,2
          do i=1,ni
            ! pressure gradient accel:
            vdiag(i,j,k,vd_pgrad) = 0.0
          enddo
          enddo
          enddo
        ELSE
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=1,nj+1
          do i=1,ni
            ! pressure gradient accel:
            vdiag(i,j,k,vd_pgrad) = (v3d(i,j,k)-va(i,j,k))*rdt - vdiag(i,j,k,vd_pgrad)
          enddo
          enddo
          enddo
        ENDIF
      ENDIF

      IF( dowbud .and. nrk.eq.nrkmax )THEN
        ! pressure gradient accel:
        rdt = 1.0/dt
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present)
        do k=2,nk
        do j=1,nj
        do i=1,ni
          wdiag(i,j,k,wd_pgrad) = (w3d(i,j,k)-wa(i,j,k))*rdt - wdiag(i,j,k,wd_pgrad)
        enddo
        enddo
        enddo
      ENDIF

!--------------------------------------------------------------------
!  on final RK step, get max CFL

        IF( nrk.eq.nrkmax )THEN
          !print *,'solve2: before call to calccflquick'
          call calccflquick(dt,uh,vh,mh,u3d,v3d,w3d,reqc)
        ENDIF

!--------------------------------------------------------------------
!  radbc

        if(irbc.eq.4)then

          if(ibw.eq.1 .or. ibe.eq.1)then
            call radbcew4(ruf,radbcw,radbce,ua,u3d,dttmp)
          endif

          if(ibs.eq.1 .or. ibn.eq.1)then
            call radbcns4(rvf,radbcs,radbcn,va,v3d,dttmp)
          endif

        endif

!--------------------------------------------------------------------
!  For Bryan-Fritsch equation set, compute 3d divergence.
!     Store in T11 array.

    IF( imoist.eq.1 .and. eqtset.eq.2 )THEN
      if( get_time_avg )then
        ! cm1r19:  rru,rrv,rrw store small-step-avg velocities
        call     getdiv(arh1,arh2,uh,vh,mh,rru,rrv,rrw,dum1,dum2,dum3,t11,  &
                        rds,rdsf,sigma,sigmaf,gz,rgzu,rgzv,dzdx,dzdy)
      else
        call     getdiv(arh1,arh2,uh,vh,mh,u3d,v3d,w3d,dum1,dum2,dum3,t11,  &
                        rds,rdsf,sigma,sigmaf,gz,rgzu,rgzv,dzdx,dzdy)
      endif
    ENDIF

!--------------------------------------------------------------------

      if( iprcl.eq.1 .and. nrk.eq.nrkmax )then
        ! save time-averaged velocities for parcel driver:
        !$acc parallel loop gang vector collapse(3) default(present)
        do k=1,nk+1
        do j=1,nj+1
        do i=1,ni+1
          uten1(i,j,k) = rru(i,j,k)
          vten1(i,j,k) = rrv(i,j,k)
          wten1(i,j,k) = rrw(i,j,k)
        enddo
        enddo
        enddo
      endif

      if( get_time_avg )then
        ! cm1r19:  rru,rrv,rrw store small-step-avg velocities
        call     getdivx(arh1,arh2,uh,vh,mh,rho0,rf0,rru,rrv,rrw,divx,  &
                         rds,rdsf,sigma,sigmaf,gz,rgzu,rgzv,dzdx,dzdy)
      endif

!--------------------------------------------------------------------
!  THETA-equation

        !print *,'solve2: point #13'
#ifdef MPI
        IF(nrk.ge.2)THEN
          call comm_3s_end_GPU(th3d,rw31,rw32,re31,re32,   &
                                rs31,rs32,rn31,rn32,reqs_y)
        ENDIF
#endif
        !print *,'solve2: point #14'

        ! note: t11 stores 3d divergence

        IF( imoist.eq.1 .and. eqtset.eq.2 )THEN
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            thten(i,j,k)=thten1(i,j,k)-t11(i,j,k)*thterm(i,j,k)
          enddo
          enddo
          enddo
          if( dotbud .and. td_div.ge.1 )then
            !$omp parallel do default(shared) private(i,j,k)
            !$acc parallel loop gang vector collapse(3) default(present)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              tdiag(i,j,k,td_div) = -t11(i,j,k)*thterm(i,j,k)
            enddo
            enddo
            enddo
          endif
        ELSE
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            thten(i,j,k)=thten1(i,j,k)
          enddo
          enddo
          enddo
        ENDIF

        IF(.not.terrain_flag)THEN
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=jb,je
          do i=ib,ie
            sadv(i,j,k)=(th0(1,1,k)-th0r)+th3d(i,j,k)
          enddo
          enddo
          enddo
        ELSE
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=jb,je
          do i=ib,ie
            sadv(i,j,k)=(th0(i,j,k)-th0r)+th3d(i,j,k)
          enddo
          enddo
          enddo
        ENDIF

      if(timestats.ge.1) time_misc18=time_misc18+mytime()

        weps = 10.0*epsilon
        diffit = 0
        if( idiff.eq.1 .and. difforder.eq.6 ) diffit = 1
        call advs(nrk,1,0,bfoo,xh,rxh,arh1,arh2,uh,ruh,xf,vh,rvh,gz,rgz,mh,rmh,           &
                   rho0,rr0,rf0,rrf0,dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,divx,        &
                   rru,rrv,rrw,tha,sadv,thten,0,0,dttmp,weps,                             &
                   flag,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,rdsf,c1,c2,rho,rr,diffit, &
                   dotbud,ibdt,iedt,jbdt,jedt,kbdt,kedt,ntdiag,tdiag,td_hadv,td_vadv,td_lsw, &
                   td_hidiff,td_vidiff,td_hediff,wprof,dumk1,dumk2,hadvordrs,vadvordrs,kbdy,bndy,hflxw,hflxe,hflxs,hflxn,out3d)
      !print *,'solve2: point #14.1'

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        th3d(i,j,k) = tha(i,j,k)+dttmp*thten(i,j,k)
        if(abs(th3d(i,j,k)).lt.smeps) th3d(i,j,k)=0.0
      enddo
      enddo
      enddo
      if(timestats.ge.1) time_integ=time_integ+mytime()


!--------------------------------------------------------------------
!  Moisture:

  IF(imoist.eq.1)THEN
    
    DO n=1,numq

      ! t33 = dummy

      bflag=0
      if(stat_qsrc.eq.1 .and. nrk.eq.nrkmax) bflag=1

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        sten(i,j,k)=qten(i,j,k,n)
      enddo
      enddo
      enddo
      if(timestats.ge.1) time_misc19=time_misc19+mytime()

      if(nrk.eq.nrkmax)then
        pdef = 1
      else
        pdef = 0
      endif

        !print *,'solve2: point #15'
#ifdef MPI
      if( nrk.ge.2 )then
        call comm_3s_end_GPU(q3d(ib,jb,kb,n)  &
                       ,qw31(1,1,1,n),qw32(1,1,1,n),qe31(1,1,1,n),qe32(1,1,1,n)     &
                       ,qs31(1,1,1,n),qs32(1,1,1,n),qn31(1,1,1,n),qn32(1,1,1,n)     &
                       ,reqs_q(1,n) )
      endif
#endif
        !print *,'solve2: point #16'

      ! Note: epsilon = 1.0e-18
!!!      weps = 0.01*epsilon
!!!      IF( idm.eq.1 .and. n.ge.nnc1 .and. n .le. nnc2 ) weps = 1.0e5*epsilon
!!!      IF( idmplus.eq.1 .and. n.ge.nzl1 .and. n .le. nzl2 ) weps = 1.d-30/zscale

      ! cm1r20.1: use qmag array (defined in param.F)
      weps = qmag(n)*epsilon

      diffit = 0
      if( idiff.eq.1 .and. difforder.eq.6 ) diffit = 1

    IF( n.eq.nqv )THEN
      call advs(nrk,1,bflag,bsq(n),xh,rxh,arh1,arh2,uh,ruh,xf,vh,rvh,gz,rgz,mh,rmh,     &
                 rho0,rr0,rf0,rrf0,dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,divx,        &
                 rru,rrv,rrw,qa(ib,jb,kb,n),q3d(ib,jb,kb,n),sten,pdef,0,dttmp,weps,     &
                 flag,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,rdsf,c1,c2,rho,rr,diffit, &
                 doqbud ,ibdq,iedq,jbdq,jedq,kbdq,kedq,nqdiag,qdiag,qd_hadv,qd_vadv,qd_lsw, &
                 qd_hidiff,qd_vidiff,qd_hediff,wprof,dumk1,dumk2,hadvordrs,vadvordrs,kbdy,bndy,hflxw,hflxe,hflxs,hflxn,out3d)
    ELSE
      call advs(nrk,1,bflag,bsq(n),xh,rxh,arh1,arh2,uh,ruh,xf,vh,rvh,gz,rgz,mh,rmh,     &
                 rho0,rr0,rf0,rrf0,dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,divx,        &
                 rru,rrv,rrw,qa(ib,jb,kb,n),q3d(ib,jb,kb,n),sten,pdef,1,dttmp,weps,     &
                 flag,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,rdsf,c1,c2,rho,rr,diffit, &
                 .false.,ibdq,iedq,jbdq,jedq,kbdq,kedq,nqdiag,qdiag,1,1,1,              &
                 1,1,1,wprof,dumk1,dumk2,hadvordrs,vadvordrs,kbdy,bndy,hflxw,hflxe,hflxs,hflxn,out3d)
    ENDIF

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        q3d(i,j,k,n) = qa(i,j,k,n)+dttmp*sten(i,j,k)
        if( abs(q3d(i,j,k,n)).lt.smeps ) q3d(i,j,k,n) = 0.0
      enddo
      enddo
      enddo
      if(timestats.ge.1) time_integ=time_integ+mytime()

    ENDDO   ! enddo for n loop
  ENDIF    ! endif for imoist=1

!--------------------------------------------------------------------
!  Get pressure
!  Get density

    dopf = .false.

    pscheck:  &
    IF(psolver.eq.4.or.psolver.eq.5.or.psolver.eq.6.or.psolver.eq.7)THEN

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        prs(i,j,k)=prs0(i,j,k)
        rho(i,j,k)=rho0(i,j,k)
        rr(i,j,k)=rr0(i,j,k)
      enddo
      enddo
      enddo
      if(timestats.ge.1) time_prsrho=time_prsrho+mytime()
      
    ELSE

      IF(imoist.eq.1)THEN

        IF(nrk.eq.nrkmax.and.eqtset.eq.2)THEN
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            ! subtract-off estimated diabatic terms used during RK steps:
            ! also, save values before calculating microphysics:
            pp3d(i,j,k)=pp3d(i,j,k)-dt*qpten(i,j,k)
            qpten(i,j,k)=pp3d(i,j,k)
            th3d(i,j,k)=th3d(i,j,k)-dt*qtten(i,j,k)
            qtten(i,j,k)=th3d(i,j,k)
            q3d(i,j,k,nqv)=q3d(i,j,k,nqv)-dt*qvten(i,j,k)
            qvten(i,j,k)=q3d(i,j,k,nqv)
            q3d(i,j,k,nqc)=q3d(i,j,k,nqc)-dt*qcten(i,j,k)
            qcten(i,j,k)=q3d(i,j,k,nqc)
          enddo
          enddo
          enddo
        ENDIF

        IF( nrk.eq.nrkmax .or. (idiff.ge.1 .and. difforder.eq.6) )THEN
          !$acc parallel loop gang vector collapse(3) default(present)
          !$omp parallel do default(shared) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            prs(i,j,k)=p00*((pi0(i,j,k)+pp3d(i,j,k))**cpdrd)
            rho(i,j,k)=prs(i,j,k)                         &
               /( (th0(i,j,k)+th3d(i,j,k))*(pi0(i,j,k)+pp3d(i,j,k))     &
                 *(rd+max(0.0,q3d(i,j,k,nqv))*rv) )
          enddo
          enddo
          enddo
        ENDIF

      ELSE

        IF( nrk.eq.nrkmax .or. (idiff.ge.1 .and. difforder.eq.6) )THEN
          !$acc parallel loop gang vector collapse(3) default(present)
          !$omp parallel do default(shared) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            prs(i,j,k)=p00*((pi0(i,j,k)+pp3d(i,j,k))**cpdrd)
            rho(i,j,k)=prs(i,j,k)   &
               /(rd*(th0(i,j,k)+th3d(i,j,k))*(pi0(i,j,k)+pp3d(i,j,k)))
          enddo
          enddo
          enddo
        ENDIF

      ENDIF


      !-----------------------------------------------
      pmod1:  &
      IF( apmasscon.eq.1 .and. nrk.eq.nrkmax )THEN
        ! cm1r18:  adjust average pressure perturbation to ensure 
        !          conservation of total dry-air mass

        IF( axisymm.eq.0 )THEN
          !$acc parallel default(present) reduction(+:tmp1,tmp2)
          !$acc loop gang
          do k=1,nk
            tmp1=0
            tmp2=0
            !$omp parallel do default(shared) private(i,j) reduction(+:tmp1,tmp2)
            !$acc loop vector collapse(2) reduction(+:tmp1,tmp2)
            do j=1,nj
            do i=1,ni
              tmp1 = tmp1 + rho(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)
              tmp2 = tmp2 + (pi0(i,j,k)+pp3d(i,j,k))
            enddo
            enddo
            dumk3(k) = tmp1
            dumk4(k) = tmp2
          enddo
          !$acc end parallel
        ELSEIF( axisymm.eq.1 )THEN
          !$acc parallel default(present) reduction(+:tmp1,tmp2)
          !$acc loop gang
          do k=1,nk
            tmp1=0
            tmp2=0
            !$omp parallel do default(shared) private(i,j) reduction(+:tmp1,tmp2)
            !$acc loop vector collapse(2) reduction(+:tmp1,tmp2)
            do j=1,nj
            do i=1,ni
              tmp1 = tmp1 + rho(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)*pi*(xf(i+1)**2-xf(i)**2)
              tmp2 = tmp2 + (pi0(i,j,k)+pp3d(i,j,k))
            enddo
            enddo
            dumk3(k) = tmp1
            dumk4(k) = tmp2
          enddo
          !$acc end parallel
        ENDIF

#ifdef MPI
        !$acc update host(dumk3,dumk4)
        call MPI_IALLREDUCE(mpi_in_place,dumk3,nk,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,reqp1,ierr)
        call MPI_IALLREDUCE(mpi_in_place,dumk4,nk,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,reqp2,ierr)
#endif
        !print *,'solve2: point #18'

        dopf = .true.

      ENDIF  pmod1

      if(timestats.ge.1) time_prsrho=time_prsrho+mytime()

    ENDIF  pscheck

!--------------------------------------------------------------------
!  bcs and comms:

      !print *,'solve2: point #19'

      call bcu_GPU(u3d)
#ifdef MPI
      call comm_3u_start_GPU(u3d,uw31,uw32,ue31,ue32,   &
                             us31,us32,un31,un32,reqs_u)
#endif
      call bcv_GPU(v3d)
#ifdef MPI
      call comm_3v_start_GPU(v3d,vw31,vw32,ve31,ve32,   &
                             vs31,vs32,vn31,vn32,reqs_v)
#endif
      call bcw_GPU(w3d,1)
      if(terrain_flag) then 
        call bcwsfc(gz,dzdx,dzdy,u3d,v3d,w3d)
      endif
#ifdef MPI
      call comm_3w_start_GPU(w3d,ww31,ww32,we31,we32,   &
                             ws31,ws32,wn31,wn32,reqs_w)
#endif
      IF(nrk.lt.nrkmax)THEN
        !call flush(6)
        !print *,'solve2: before bcs_GPU #3'
        call bcs_GPU(rho)
#ifdef MPI
        call comm_1s_start_GPU(rho,pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,reqs_s)
#endif
        !call flush(6)
        !print *,'solve2: before bcs_GPU #4'
        call bcs_GPU(pp3d)
#ifdef MPI
        call comm_1s_start_GPU(pp3d,vw1,vw2,ve1,ve2,vs1,vs2,vn1,vn2,reqs_x)
#endif
        !call flush(6)
        !print *,'solve2: before bcs_GPU #5'
        call bcs_GPU(th3d)
#ifdef MPI
        call comm_3s_start_GPU(th3d,rw31,rw32,re31,re32,   &
                                rs31,rs32,rn31,rn32,reqs_y)
#endif
        IF(imoist.eq.1)THEN
          !print *,'numq: ',numq
        !call flush(6)
        !print *,'solve2: before bcs_GPU #6'
          do n=1,numq
            call bcs_GPU(q3d(ib,jb,kb,n))
#ifdef MPI
            call comm_3s_start_GPU(q3d(ib,jb,kb,n)  &
                       ,qw31(1,1,1,n),qw32(1,1,1,n),qe31(1,1,1,n),qe32(1,1,1,n)     &
                       ,qs31(1,1,1,n),qs32(1,1,1,n),qn31(1,1,1,n),qn32(1,1,1,n)     &
                       ,reqs_q(1,n) )
#endif
          enddo
        ENDIF
      ENDIF
      !print *,'solve2: point #20'

!--------------------------------------------------------------------
!  TKE advection
 
        IF( cm1setup.eq.1 .and. iusetke )THEN

          ! use wten for tke tendency, step tke forward:
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) 
          do k=2,nk
          do j=1,nj
          do i=1,ni
            wten(i,j,k)=tketen(i,j,k)
          enddo
          enddo
          enddo
          if(timestats.ge.1) time_misc20=time_misc20+mytime()

#ifdef MPI
        IF(nrk.ge.2)THEN
          call comm_3t_end_GPU(tke3d,tkw1,tkw2,tke1,tke2,   &
                                 tks1,tks2,tkn1,tkn2,reqs_tk)
        ENDIF
#endif
      !print *,'solve2: point #20.1'

        if( dotdwrite .and. kd_adv.ge.1 )then
        if( nrk.eq.nrkmax )then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) 
          do k=1,nk+1
          do j=1,nj
          do i=1,ni
            kdiag(i,j,k,kd_adv) = wten(i,j,k)
          enddo
          enddo
          enddo
        endif
        endif

          call   advw(nrk   ,xh,rxh,arh1,arh2,uh,xf,vh,gz,rgz,mh,mf,rho0,rr0,rf0,rrf0,  &
                      dum1,dum2,dum3,dum4,dum5,dum6,dum7,divx,                       &
                      rru,rrv,rrw,tke3d,wten,rds,rdsf,c1,c2,rho,dttmp,               &
                      .false.,wdiag,hadvordrs,vadvordrs,advwenos,bndy,kbdy,uf,vf,u3d,v3d,hflxw,hflxe,hflxs,hflxn)

        if( dotdwrite .and. kd_adv.ge.1 )then
        if( nrk.eq.nrkmax )then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) 
          do k=1,nk+1
          do j=1,nj
          do i=1,ni
            kdiag(i,j,k,kd_adv) = wten(i,j,k)-kdiag(i,j,k,kd_adv)
          enddo
          enddo
          enddo
        endif
        endif

        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) 
        do k=2,nk
          do j=1,nj
          do i=1,ni
            tke3d(i,j,k)=tkea(i,j,k)+dttmp*wten(i,j,k)
            if(tke3d(i,j,k).lt.1.0e-10) tke3d(i,j,k)=0.0
          enddo
          enddo
        enddo
        if(timestats.ge.1) time_integ=time_integ+mytime()
        IF( do_ib )THEN
          call zero_out_w(bndy,kbdy,tke3d)
        ENDIF

          call bcw_GPU(tke3d,1)
#ifdef MPI
          call comm_3t_start_GPU(tke3d,tkw1,tkw2,tke1,tke2,   &
                                   tks1,tks2,tkn1,tkn2,reqs_tk)
#endif

        ENDIF
        !print *,'solve2: point #21'

!--------------------------------------------------------------------
!  qke advection (for PBL schemes with TKE)

    IF( cm1setup.eq.2 .and. iusetke )THEN

      doadv:  &
      if( bl_mynn_tkeadvect )then
        ! with tke advection:

        IF( nrk.eq.1 )THEN
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) 
          do k=1,nk
          do j=1,nj
          do i=1,ni
            qke3d(i,j,k)=qke(i,j,k)
            qke_adv(i,j,k)=0.0
          enddo
          enddo
          enddo
        ELSE
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) 
          do k=1,nk
          do j=1,nj
          do i=1,ni
            qke_adv(i,j,k)=0.0
          enddo
          enddo
          enddo
        ENDIF
        if(timestats.ge.1) time_misc21=time_misc21+mytime()

        !call flush(6)
        !print *,'solve2: before bcs_GPU #7'
        call bcs_GPU(qke3d)
        !print *,'solve2: point #22'
#ifdef MPI
        call comm_3s_start_GPU(qke3d,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,reqs_tk)
        call comm_3s_end_GPU(  qke3d,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,reqs_tk)
#endif
        !print *,'solve2: point #22'
        weps = 10.0*epsilon
        diffit = 0
        if( idiff.eq.1 .and. difforder.eq.6 ) diffit = 1
        call advs(nrk,1,0,bfoo,xh,rxh,arh1,arh2,uh,ruh,xf,vh,rvh,gz,rgz,mh,rmh,           &
                   rho0,rr0,rf0,rrf0,dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,divx,        &
                   rru,rrv,rrw,qke,qke3d,qke_adv,1,1,dttmp,weps,                          &
                   flag,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,rdsf,c1,c2,rho,rr,diffit, &
                 .false.,ibdq,iedq,jbdq,jedq,kbdq,kedq,nqdiag,qdiag,1,1,1,              &
                 1,1,1,wprof,dumk1,dumk2,hadvordrs,vadvordrs,kbdy,bndy,hflxw,hflxe,hflxs,hflxn,out3d)
      !print *,'solve2: after advs'

        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          qke3d(i,j,k)=qke(i,j,k)+dttmp*qke_adv(i,j,k)
          if(qke3d(i,j,k).lt.1.0e-10) qke3d(i,j,k)=0.0
        enddo
        enddo
        enddo
        if(timestats.ge.1) time_integ=time_integ+mytime()

      else
        ! without tke advection:

        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          qke_adv(i,j,k) = 0.0
          qke3d(i,j,k) = qke(i,j,k)
        enddo
        enddo
        enddo

      endif  doadv

    ENDIF

!--------------------------------------------------------------------
!  Passive Tracers

    if(iptra.eq.1)then

      if( nrk.eq.nrkmax .and. pdtra.eq.1 )then
        pdef = 1
      else
        pdef = 0
      endif

    DO n=1,npt

      ! t33 = dummy

      bflag=0
      if(stat_qsrc.eq.1 .and. nrk.eq.nrkmax) bflag=1

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        sten(i,j,k)=ptten(i,j,k,n)
      enddo
      enddo
      enddo
      if(timestats.ge.1) time_misc22=time_misc22+mytime()


        !print *,'solve2: point #23'
#ifdef MPI
          IF(nrk.ge.2)THEN
            call comm_3s_end_GPU(pt3d(ib,jb,kb,n),                           &
                  tw1(1,1,1,n),tw2(1,1,1,n),te1(1,1,1,n),te2(1,1,1,n),   &
                  ts1(1,1,1,n),ts2(1,1,1,n),tn1(1,1,1,n),tn2(1,1,1,n),   &
                  reqs_t(1,n))
          ENDIF
#endif
        !print *,'solve2: point #24'

      weps = 1.0*epsilon
      diffit = 0
      if( idiff.eq.1 .and. difforder.eq.6 ) diffit = 1
      call advs(nrk,1,0,bfoo,xh,rxh,arh1,arh2,uh,ruh,xf,vh,rvh,gz,rgz,mh,rmh,       &
                 rho0,rr0,rf0,rrf0,dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,divx,        &
                 rru,rrv,rrw,pta(ib,jb,kb,n),pt3d(ib,jb,kb,n),sten,pdef,1,dttmp,weps,   &
                 flag,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,rdsf,c1,c2,rho,rr,diffit, &
                 .false.,ibdq,iedq,jbdq,jedq,kbdq,kedq,nqdiag,qdiag,1,1,1,              &
                 1,1,1,wprof,dumk1,dumk2,hadvordrs,vadvordrs,kbdy,bndy,hflxw,hflxe,hflxs,hflxn,out3d)
      print *,'solve2: after advs'

      !$omp parallel do default(shared) private(i,j,k)
      !$acc parallel loop gang vector collapse(3) default(present)
      do k=1,nk
      do j=1,nj
      do i=1,ni
        pt3d(i,j,k,n)=pta(i,j,k,n)+dttmp*sten(i,j,k)
      enddo
      enddo
      enddo
      if(timestats.ge.1) time_integ=time_integ+mytime()

        !print *,'solve2: point #25'
      IF(nrk.le.2)THEN
        !call flush(6)
        !print *,'solve2: before bcs_GPU #8'
        call bcs_GPU(pt3d(ib,jb,kb,n))
#ifdef MPI
        call comm_3s_start_GPU(pt3d(ib,jb,kb,n)   &
                     ,tw1(1,1,1,n),tw2(1,1,1,n),te1(1,1,1,n),te2(1,1,1,n)     &
                     ,ts1(1,1,1,n),ts2(1,1,1,n),tn1(1,1,1,n),tn2(1,1,1,n)     &
                     ,reqs_t(1,n) )
#endif
      ENDIF
        !print *,'solve2: point #26'

    ENDDO
    endif

!--------------------------------------------------------------------
!  Finish pressure
!  Finish density

      pscheck2:  &
      IF( dopf )THEN

#ifdef MPI
        call mpi_wait(reqp1,mpi_status_ignore,ierr)
        call mpi_wait(reqp2,mpi_status_ignore,ierr)
        !$acc update device(dumk3,dumk4)
#endif
        temq1 = 0.0
        temq2 = 0.0
        !$acc parallel loop gang vector default(present) reduction(+:temq1,temq2)
        do k=1,nk
          temq1 = temq1 + dumk3(k)
          temq2 = temq2 + dumk4(k)
        enddo

        mass2 = temq1
        p2 = temq2

        tem = ( (mass1/mass2)**rddcv - 1.0d0 )*p2/(nx*ny*nz)

        IF( imoist.eq.1 )THEN
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) 
          do k=1,nk
          do j=1,nj
          do i=1,ni
            pp3d(i,j,k) = pp3d(i,j,k) + tem
            prs(i,j,k)=p00*((pi0(i,j,k)+pp3d(i,j,k))**cpdrd)
            rho(i,j,k)=prs(i,j,k)                         &
               /( (th0(i,j,k)+th3d(i,j,k))*(pi0(i,j,k)+pp3d(i,j,k))     &
                 *(rd+max(0.0,q3d(i,j,k,nqv))*rv) )
          enddo
          enddo
          enddo
        ELSE
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) 
          do k=1,nk
          do j=1,nj
          do i=1,ni
            pp3d(i,j,k) = pp3d(i,j,k) + tem
            prs(i,j,k)=p00*((pi0(i,j,k)+pp3d(i,j,k))**cpdrd)
            rho(i,j,k)=prs(i,j,k)   &
               /(rd*(th0(i,j,k)+th3d(i,j,k))*(pi0(i,j,k)+pp3d(i,j,k)))
          enddo
          enddo
          enddo
        ENDIF

      ENDIF  pscheck2

      if(timestats.ge.1) time_prsrho=time_prsrho+mytime()

!--------------------------------------------------------------------

      IF( idiff.ge.1 .and. difforder.eq.6 .and. nrk.lt.nrkmax )THEN
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) 
        do k=1,nk
        do j=1,nj
        do i=1,ni
          rr(i,j,k) = 1.0/rho(i,j,k)
        enddo
        enddo
        enddo
        if(timestats.ge.1) time_prsrho=time_prsrho+mytime()
      ENDIF

!--------------------------------------------------------------------
! RK loop end

      ENDDO  rkloop

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CC   End of RK section   CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


!--------------------------------------------------------------------
!  Final step for Passive Tracers
!  (using final value of rho)

    if(iptra.eq.1)then
        !call flush(6)
        !print *,'solve2: before bcs_GPU #9'
      DO n=1,npt
        if( pdtra.eq.1 ) then 
            call pdefq2_GPU(0.0,afoo,ruh,rvh,rmh,rho,pt3d(ib,jb,kb,n))
        endif
        call bcs_GPU(pt3d(ib,jb,kb,n))
#ifdef MPI
        call comm_3s_start_GPU(pt3d(ib,jb,kb,n)   &
                     ,tw1(1,1,1,n),tw2(1,1,1,n),te1(1,1,1,n),te2(1,1,1,n)     &
                     ,ts1(1,1,1,n),ts2(1,1,1,n),tn1(1,1,1,n),tn2(1,1,1,n)     &
                     ,reqs_t(1,n) )
#endif
      ENDDO
    endif

!--------------------------------------------------------------------

#ifdef MPI
      ! finish comms for cflquick:
      call mpi_wait(reqc,mpi_status_ignore,ierr)
      if(myid.eq.0) print *,'solve2: cflmax is: ',cflmax
      if(cflmax.ge.1.50) stopit=.true.
      if(timestats.ge.1) time_cflq=time_cflq+mytime()
#endif

!--------------------------------------------------------------------

    ! NOTE:

    ! cm1r20.1:  - moved microphysics to mp_driver (which is called from cm1.F)
    !            - moved message passing and equate to solve_finish.F

      end subroutine solve2


  END MODULE solve2_module
