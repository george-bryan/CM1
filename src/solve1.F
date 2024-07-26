  MODULE solve1_module

        ! solve1: pre-RK terms
        ! (tendencies held fixed during Runge-Kutta integration and acoustic sub-stepping)

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
  public :: solve1

  CONTAINS

      subroutine solve1(nstep,rbufsz,num_soil_layers,                  &
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
                   dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,          &
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
                   lsnudge_u,lsnudge_v,lsnudge_th,lsnudge_qv,        &
                   dowriteout,dorad,getdbz,getvt,dotdwrite,          &
                   dotbud,doqbud,doubud,dovbud,dowbud,donudge,       &
                   doazimwrite,dorestart)
        ! end_solve1
      use input, only : ib,ie,jb,je,kb,ke,itb,ite,jtb,jte,ktb,kte,ibb2,ibe2,jbb2,jbe2, &
          kbb2,kbe2,ibph,ieph,jbph,jeph,kbph,keph,ibnba,ienba,jbnba,jenba,kbnba,kenba, &
          nx,ny,nz,ni,nj,nk,ibm,iem,jbm,jem,kbm,kem,ibc,iec,jbc,jec,kbc,kec,ibt,iet,jbt, &
          jet,kbt,ket,ibb,ieb,jbb,jeb,kbb,keb,ibr,ier,jbr,jer,kbr,ker,maxq,nbudget,numq,ngxy, &
          nrain,npt,nparcelsLocal,npvals,ibl,iel,jbl,jel,ibp,iep,jbp,jep,kbp,kep,ib2pt,ie2pt, &
          jb2pt,je2pt,kb2pt,ke2pt,ipb,ipe,jpb,jpe,kpb,kpe,rmp,cmp,kmt,imp,jmp,kmp,d3i,d3j, &
          d3t,d3n,d2i,d2j,ibdt,iedt,jbdt,jedt,kbdt,kedt,ntdiag,ibdq,iedq,jbdq,jedq,kbdq, &
          kedq,nqdiag,ibdv,iedv,jbdv,jedv,kbdv,kedv,nudiag,nvdiag,nwdiag,ibdk,iedk,jbdk, &
          jedk,kbdk,kedk,nkdiag,ibdp,iedp,jbdp,jedp,kbdp,kedp,npdiag,ib2d,ie2d,jb2d,je2d, &
          nout2d,ib3d,ie3d,jb3d,je3d,kb3d,ke3d,nout3d,nf,nu,nv,nw,testcase,myid,timestats, &
          time_misc06,time_misc07,time_misc08,time_misc09,time_misc10,time_misc11,time_misc12, &
          time_rdamp,time_turb,time_ercyl,time_integ,psolver,dolsw,nqc,nqi,nnci,nncc, &
          time_rad,time_pbl,mytime,cm1setup,iusetke,sgsmodel,doimpl,iinit,ptype, &
          ibw,ibe,ibs,ibn,imoist,irdamp,hrdamp,irbc,rdalpha,iptra,lspgrad,ierr,fcor,vmove, &
          umove,hurr_rad,difforder,idiff,nqv,eqtset,idiss,rterm,iice,nql1,nql2,nqs1,nqs2, &
          axisymm,dx,dy,dz,rdz,bbc,terrain_flag,radopt,use_pbl,tsmall,qsmall,dohturb,dovturb, &
          xc_uforce,xr_uforce,zr_uforce,alpha_uforce,t1_uforce,t2_uforce,    &
          kd_turb,ud_rdamp,vd_rdamp,wd_rdamp,td_rdamp,td_hediff,td_vediff,qd_hediff,qd_vediff, &
          td_diss,td_rad,td_pbl,td_hturb,td_vturb,qd_hturb,qd_vturb,ud_pbl,vd_pbl,qd_pbl, &
          td_nudge,qd_nudge

      use constants
      use bc_module, only: radbcew, radbcns 
      use comm_module
      use diff2_module
      use turbtend_module
      use misclibs
      use simple_phys_module, only : testcase_simple_phys,get_avg_uvt,get_avg_uvtq
      use ib_module
      use eddy_recycle
      use lsnudge_module
#ifdef MPI
      use mpi
#endif
      implicit none

!-----------------------------------------------------------------------
! Arrays and variables passed into solve

      integer, intent(in) :: nstep
      integer, intent(in) :: rbufsz,num_soil_layers
!      integer :: jmd
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
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8
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

      real, intent(inout), dimension(lsnudge_maxlevels,lsnudge_nmax) :: lsnudge_u,lsnudge_v,lsnudge_th,lsnudge_qv

      logical, intent(in) :: dowriteout,dorad,dotdwrite,doazimwrite,dorestart
      logical, intent(inout) :: getdbz,getvt,dotbud,doqbud,doubud,dovbud,dowbud,donudge


!-----------------------------------------------------------------------
! Arrays and variables defined inside solve

      integer :: i,j,k,n,nrk,bflag,pdef,diffit,k1,k2,ii,ip
      integer :: has_reqc,has_reqi,has_reqs,do_radar_ref,ke_diag

      real :: delqv,delpi,delth,delt,fac,epsd,dheat,dz1,xs
      real :: foo1,foo2
      real :: dttmp,rtime,rdt,tem,tem0,tem1,tem2,thrad,prad
      real :: r1,r2,tnew,pnew,pinew,thnew,qvnew
      real :: gamm,aiu
      real :: qv,qli,cpli,cpm,cvm,qmax
      real :: tn,qn,nudgefac,taunudge,timenudge
      real :: qtmpreal

      real :: umod,uref,zref,oldval,newval

      double precision :: weps,afoo,bfoo,p0,p2
      double precision :: budtmp
      double precision :: qtmp

      double precision :: budarray(1:ni,1:nj)

      logical :: get_time_avg,tqnudge,dopf
      logical :: do_adapt_lspg

#ifdef MPI
      integer :: reqp1,reqp2
#endif
      !$acc declare &
      !$acc present(xh,rxh,arh1,arh2,uh,ruh) &
      !$acc present(xf,rxf,arf1,arf2,uf,ruf) &
      !$acc present(yh,vh,rvh) &
      !$acc present(yf,vf,rvf) &
      !$acc present(rds,sigma,rdsf,sigmaf) &
      !$acc present(zh,mh,rmh) &
      !$acc present(zf,mf,rmf) &
      !$acc present(zs,gz,rgz,gzu,rgzu,gzv,rgzv) &
      !$acc present(gx,gxu,gy,gyv) &
      !$acc present(pi0,rho0,th0,rr0,rf0) &
      !$acc present(u0,v0) &
      !$acc present(m11,m12,m13,m22,m23,m33) &
      !$acc present(kmw,u1b,v1b) &
      !$acc present(tauh,tauf,taus) &
      !$acc present(bud2) &
      !$acc present(ufrc,vfrc,thfrc,qvfrc) &
      !$acc present(t11,t12,t13,t22,t23,t33) &
      !$acc present(thflux,qvflux) &
      !$acc present(radbcw,radbce,radbcs,radbcn) &
      !$acc present(dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8) &
      !$acc present(rho,rr,rf) &
      !$acc present(ua,uten1) &
      !$acc present(va,vten1) &
      !$acc present(wa,wten1) &
      !$acc present(o30,zir) &
      !$acc present(ppi,ppten,sten,sadv) &
      !$acc present(tha,thten,thten1) &
      !$acc present(qpten,qtten,qvten,qcten) &
      !$acc present(qa,qten) &
      !$acc present(kmh,kmv,khh,khv,tkea,tketen,nm,dissten,epst) &
      !$acc present(pta,ptten,u2pt,v2pt) &
      !$acc present(tdiag,qdiag,udiag,vdiag,wdiag,kdiag) &
      !$acc present(uavg,vavg,thavg,qavg,cavg)

!--------------------------------------------------------------------

#ifdef MPI
      nf=0
      nu=0
      nv=0
      nw=0
#endif

      afoo=0.0d0
      bfoo=0.0d0


      if( testcase.eq.10 )then
        tqnudge = .true.
      else
        tqnudge = .false.
      endif


      timenudge = mtime+dt
      donudge = .false.
      lsnudgefac = 0.0

      if( do_lsnudge )then
        if( (mtime+dt).gt.lsnudge_start .and. (mtime+dt).lt.lsnudge_end )then

          donudge = .true.
          lsnudgefac = 1.0

          if( donudge .and. timenudge.gt.lsnudge_time2(lsnudge_count) )then

            ! update which profile we are nudging towards:
            lsnudge_count = lsnudge_count + 1

            if( myid.eq.0 ) print *,'  lsnudge times:  time1,timenudge,time2 = ',lsnudge_time1(lsnudge_count),timenudge,lsnudge_time2(lsnudge_count)
          endif

          ! ramp-up nudging starting at lsnudge_start:
          lsnudgefac = (timenudge-lsnudge_time1(lsnudge_count))/nudge_ramp_time
          lsnudgefac = max( lsnudgefac , 0.0 )
          lsnudgefac = min( lsnudgefac , 1.0 )
          if( lsnudgefac.gt.0.001 .and. lsnudgefac.lt.0.999 .and. myid.eq.0 ) print *,'  lsnudgefac = ',lsnudgefac

        endif
      endif

      if(timestats.ge.1) time_misc06=time_misc06+mytime()

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cc   subgrid turbulence schemes  cccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!--------------------------------------------------------------------
!  get RHS for tke scheme:

      IF( cm1setup.eq.1 .and. iusetke )THEN

        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        DO k=2,nk
          !  Buoyancy, Dissipation, and mean-gradient (ie, shear) terms:
          do j=1,nj
          do i=1,ni
            tketen(i,j,k) = -khv(i,j,k)*nm(i,j,k)  &
                            -dissten(i,j,k)  &
                            +epst(i,j,k)
          enddo
          enddo
        ENDDO
        if(timestats.ge.1) time_turb=time_turb+mytime()

        if( dotdwrite .and. kd_turb.ge.1 )then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk+1
          do j=1,nj
          do i=1,ni
            kdiag(i,j,k,kd_turb) = tketen(i,j,k)
          enddo
          enddo
          enddo
        endif

        call turbt(dt,xh,rxh,uh,xf,uf,vh,vf,mh,mf,rho,rr,rf,  &
                   rds,sigma,gz,rgz,gzu,rgzu,gzv,rgzv,        &
                   dum1,dum2,dum3,dum4,dum5,sten,tkea,tketen,kmh,kmv)

        if( dotdwrite .and. kd_turb.ge.1 )then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk+1
          do j=1,nj
          do i=1,ni
            kdiag(i,j,k,kd_turb) = tketen(i,j,k)-kdiag(i,j,k,kd_turb)
          enddo
          enddo
          enddo
        endif

      ENDIF


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CC   Pre-RK calculations   CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

    IF( irdamp.eq.2 .or. hrdamp.eq.2 .or. tqnudge .or. do_lsnudge )THEN
      if( imoist.eq.1 )then
        call     get_avg_uvtq(uavg,vavg,thavg,qavg,cavg,th0,ua,va,tha,qa,ruh,ruf,rvh,rvf)
      else
        call     get_avg_uvt(uavg,vavg,thavg,cavg,th0,ua,va,tha,ruh,ruf,rvh,rvf)
      endif
    ENDIF

!--------------------------------------------------------------------
!  radbc
 
      if(irbc.eq.1)then
        if(ibw.eq.1 .or. ibe.eq.1) call radbcew(radbcw,radbce,ua)
 
        if(ibs.eq.1 .or. ibn.eq.1) call radbcns(radbcs,radbcn,va)
      endif

!--------------------------------------------------------------------
!  Rayleigh damping and set tendency arrays:

      IF( irdamp.eq.1 .or. hrdamp.eq.1 )THEN
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,ni+1
          uten1(i,j,k) = -rdalpha*0.5*(tauh(i-1,j,k)+tauh(i,j,k))*(ua(i,j,k)-u0(i,j,k))
          vten1(i,j,k) = -rdalpha*0.5*(tauh(i,j-1,k)+tauh(i,j,k))*(va(i,j,k)-v0(i,j,k))
          wten1(i,j,k) = -rdalpha*tauf(i,j,k)*wa(i,j,k)
          thten1(i,j,k) = -rdalpha*taus(i,j,k)*tha(i,j,k)
        enddo
        enddo
        enddo
      ELSEIF( irdamp.eq.2 .or. hrdamp.eq.2 )THEN
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,ni+1
          uten1(i,j,k) = -rdalpha*0.5*(tauh(i-1,j,k)+tauh(i,j,k))*(ua(i,j,k)-uavg(k))
          vten1(i,j,k) = -rdalpha*0.5*(tauh(i,j-1,k)+tauh(i,j,k))*(va(i,j,k)-vavg(k))
          wten1(i,j,k) = -rdalpha*tauf(i,j,k)*wa(i,j,k)
          thten1(i,j,k) = -rdalpha*taus(i,j,k)*((th0(i,j,k)-thavg(k))+tha(i,j,k))
        enddo
        enddo
        enddo
      ELSE
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,ni+1
          uten1(i,j,k) = 0.0
          vten1(i,j,k) = 0.0
          wten1(i,j,k) = 0.0
          thten1(i,j,k) = 0.0
        enddo
        enddo
        enddo
      ENDIF

      if(timestats.ge.1) time_rdamp=time_rdamp+mytime()

      IF( imoist.eq.1 )THEN
        ! moisture:
        !$omp parallel do default(shared) private(i,j,k,n)
        !$acc parallel loop gang vector collapse(4) default(present) private(i,j,k,n)
        do n=1,numq
          do k=1,nk
          do j=1,nj
          do i=1,ni
            qten(i,j,k,n)=0.0
          enddo
          enddo
          enddo
        enddo
        if(timestats.ge.1) time_misc07=time_misc07+mytime()
      ENDIF

      IF(iptra.eq.1)THEN
        ! passive tracers:
        !$omp parallel do default(shared) private(i,j,k,n)
        !$acc parallel loop gang vector collapse(4) default(present) private(i,j,k,n)
        do n=1,npt
          do k=1,nk
          do j=1,nj
          do i=1,ni
            ptten(i,j,k,n)=0.0
          enddo
          enddo
          enddo
        enddo
      ENDIF

    ! Diagnostics (infrequent):

      
      if( doubud .and. ud_rdamp.ge.1 )then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni+1
          udiag(i,j,k,ud_rdamp) = uten1(i,j,k)
        enddo
        enddo
        enddo
      endif
      if( dovbud .and. vd_rdamp.ge.1 )then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,ni
          vdiag(i,j,k,vd_rdamp) = vten1(i,j,k)
        enddo
        enddo
        enddo
      endif
      if( dowbud .and. wd_rdamp.ge.1 )then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk+1
        do j=1,nj
        do i=1,ni
          wdiag(i,j,k,wd_rdamp) = wten1(i,j,k)
        enddo
        enddo
        enddo
      endif
      if( dotbud .and. td_rdamp.ge.1 )then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          tdiag(i,j,k,td_rdamp) = thten1(i,j,k)
        enddo
        enddo
        enddo
        !615 $acc end data
      endif
      if(timestats.ge.1) time_rdamp=time_rdamp+mytime()

!--------------------------------------------------------------------
!  large-scale pressure gradient:

      IF( lspgrad.ge.1 )THEN

        ! Include a large-scale pressure gradient:

          do_adapt_lspg = .false.

          IF( do_adapt_lspg .and. lspgrad.eq.4 )THEN
!!!            stop 3333

          if( mtime.gt.1800.0 .and. mod(mtime,300.0).lt.0.01 )then

            call get_avg_uvt(uavg,vavg,thavg,cavg,th0,ua,va,tha,ruh,ruf,rvh,rvf)

            ! 200529: modify lspg to match reference wind
            if( myid.eq.0 )then

!!!              zref = 240.0
!!!              uref = 16.7303

              if( testcase.eq.12 )then
                ! Martinuzzi and Tropea (1993, JFE) wind tunnel test case:
                zref = 500.0
                uref = 11.096
              endif

              !$acc update host(zh)
              k = 1
              do while( zh(1,1,k).lt.zref )
                k = k+1
              enddo

              k1 = k-1
              k2 = k

              !$acc update host(uavg)
              umod = uavg(k1)+(uavg(k2)-uavg(k1))*(zref-zh(1,1,k2))/(zh(1,1,k1)-zh(1,1,k2))

              oldval = ulspg(1)
              newval = oldval*uref/umod

              print *,'  Get lspg:  ',mtime
              print *,'     old u,pg',umod,oldval
              print *,'     new u,pg',uref,newval
            endif

#ifdef MPI
            call MPI_BCAST( newval  ,1,MPI_REAL   ,0,MPI_COMM_WORLD,ierr)
#endif

            !$acc parallel loop gang vector default(present)
            do k=kb,ke
              ulspg(k) = newval
            enddo
   
          endif
          ENDIF

        if(     lspgrad.eq.1 )then

          !$acc parallel loop gang vector default(present)
          do k=kb,ke
            ulspg(k) = 0.0
            vlspg(k) = 0.0
          enddo

          !---------------------------------------------------------------!
          ! Large-scale pressure gradient based on geostropic balance,
          ! using base-state wind profiles:
          !---------------------------------------------------------------!
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj+1
          do i=1,ni+1
            ! 170728 bug fix:  account for grid staggering:
            ! 180618:  when imove=1, need to add vmove to base state
            uten1(i,j,k) = uten1(i,j,k)-fcor*( 0.25*( (v0(i  ,j,k)+v0(i  ,j+1,k))   &
                                                     +(v0(i-1,j,k)+v0(i-1,j+1,k)) ) + vmove )
            vten1(i,j,k) = vten1(i,j,k)+fcor*( 0.25*( (u0(i,j  ,k)+u0(i+1,j  ,k))   &
                                                     +(u0(i,j-1,k)+u0(i+1,j-1,k)) ) + umove )
          enddo
          enddo
          enddo
          !698 $acc end data

        elseif( lspgrad.eq.2 )then

          !---------------------------------------------------------------!
          ! Large-scale pressure gradient based on geostropic balance,
          ! using ug,vg arrays:
          !---------------------------------------------------------------!
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector default(present) private(k)
          do k=1,nk
            ulspg(k) = -fcor*(vg(k)+vmove)
            vlspg(k) =  fcor*(ug(k)+umove)
          enddo
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
            do j=1,nj+1
            do i=1,ni+1
              uten1(i,j,k) = uten1(i,j,k)+ulspg(k)
              vten1(i,j,k) = vten1(i,j,k)+vlspg(k)
            enddo
            enddo
          enddo

        elseif( lspgrad.eq.3 )then

          !---------------------------------------------------------------!
          ! Large-scale pressure gradient based on gradient-wind balance,
          ! (Bryan et al, 2017, BLM, eqn 10)
          ! using base-state wind profiles:
          !---------------------------------------------------------------!
          !JMD FIXME
          !$omp parallel do default(shared) private(i,j,k,tem)
          !$acc parallel loop gang vector default(present) private(i,j,k,tem)
          do k=1,nk
            ! ghb, 210826: account for moving domain
            ulspg(k) = -fcor*(v0(1,1,k)+vmove)-(v0(1,1,k)+vmove)*(v0(1,1,k)+vmove)/hurr_rad
            vlspg(k) =  0.0
            do j=1,nj
            do i=1,ni+1
              uten1(i,j,k) = uten1(i,j,k)+ulspg(k)
            enddo
            enddo
          enddo

        elseif( lspgrad.eq.4 )then

          !---------------------------------------------------------------!
          ! Specified large-scale pressure gradient:
          ! (user must set ulspg and vlspg in base.F)
          !---------------------------------------------------------------!
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
            do j=1,nj+1
            do i=1,ni+1
              uten1(i,j,k) = uten1(i,j,k)+ulspg(k)
              vten1(i,j,k) = vten1(i,j,k)+vlspg(k)
            enddo
            enddo
          enddo

        endif
        if(timestats.ge.1) time_misc08=time_misc08+mytime()

      ENDIF

!--------------------------------------------------------------------
!  2nd-order diffusion, fixed viscosity (eg, DNS and idealized test):

      IF( difforder.eq.2 )THEN
#ifdef _OPENACC
        print *,'WARNING: solve1: diffusion operator not support with OPENACC'
#endif
        idiffge1:  &
        if(idiff.ge.1)then

          call diff2u(rxh,arh1,arh2,uh,xf,arf1,arf2,uf,vh,vf,mh,mf,  &
                      dum1,dum2,dum3,dum4,uten1,ust,rho,rr,rf,divx,t11,t12,t13, &
                      doubud,udiag)
          call diff2v(xh,arh1,arh2,uh,rxf,arf1,arf2,uf,vh,vf,mh,mf,  &
                      dum1,dum2,dum3,dum4,vten1,ust,rho,rr,rf,divx,t22,t12,t23, &
                      dovbud,vdiag)
          call diff2w(rxh,arh1,arh2,uh,xf,arf1,arf2,uf,vh,vf,mh,mf,  &
                      dum1,dum2,dum3,dum4,wten1,rho,rr,rf,divx,t33,t13,t23,  &
                      dowbud,wdiag)

        endif  idiffge1

        idiffeq1:  &
        if(idiff.eq.1)then

          call diff2s(rxh,arh1,arh2,uh,xf,arf1,arf2,uf,vh,vf,mh,mf,  &
                      dum1,dum2,dum3,dum4,tha,thten1,rho,rr,rf,  &
                      dotbud,ibdt,iedt,jbdt,jedt,kbdt,kedt,ntdiag,tdiag,td_hediff,td_vediff)

          IF( imoist.eq.1 )THEN
            ! moisture:
            do n=1,numq
              if( n.eq.nqv )then
                call diff2s(rxh,arh1,arh2,uh,xf,arf1,arf2,uf,vh,vf,mh,mf,  &
                            dum1,dum2,dum3,dum4,qa(ib,jb,kb,n),qten(ib,jb,kb,n),rho,rr,rf,  &
                            doqbud,ibdq,iedq,jbdq,jedq,kbdq,kedq,nqdiag,qdiag,qd_hediff,qd_vediff)
              else
                call diff2s(rxh,arh1,arh2,uh,xf,arf1,arf2,uf,vh,vf,mh,mf,  &
                            dum1,dum2,dum3,dum4,qa(ib,jb,kb,n),qten(ib,jb,kb,n),rho,rr,rf,  &
                            .false.,ibdq,iedq,jbdq,jedq,kbdq,kedq,nqdiag,qdiag,1,1)
              endif
            enddo
          ENDIF

          IF(iptra.eq.1)THEN
            ! passive tracers:
            do n=1,npt
              call diff2s(rxh,arh1,arh2,uh,xf,arf1,arf2,uf,vh,vf,mh,mf,  &
                          dum1,dum2,dum3,dum4,pta(ib,jb,kb,n),ptten(ib,jb,kb,n),rho,rr,rf,  &
                          .false.,ibdq,iedq,jbdq,jedq,kbdq,kedq,nqdiag,qdiag,1,1)
            enddo
          ENDIF

        endif  idiffeq1

      ENDIF

!--------------------------------------------------------------------
!  tendencies specific to THETA:


      !----- cvm (if needed) -----!

      IF( eqtset.eq.2 .and. imoist.eq.1 .and. (idiss.eq.1.or.rterm.eq.1) )THEN
        ! for energy-conserving moist thermodynamics:
        ! store cvm in dum1:
        ! store ql  in dum2:
        ! store qi  in dum3:
        IF( nql1.ge.1 )THEN

          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            dum2(i,j,k)=qa(i,j,k,nql1)
          enddo
          enddo
          enddo

          do n=nql1+1,nql2
            !$omp parallel do default(shared)  &
            !$omp private(i,j,k)
            !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              dum2(i,j,k)=dum2(i,j,k)+qa(i,j,k,n)
            enddo
            enddo
            enddo
            !$acc end parallel
          enddo

        ELSE

          !$omp parallel do default(shared)  &
          !$omp private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            dum2(i,j,k)=0.0
          enddo
          enddo
          enddo

        ENDIF

        IF(iice.eq.1)THEN

          !$omp parallel do default(shared)  &
          !$omp private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            dum3(i,j,k)=qa(i,j,k,nqs1)
          enddo
          enddo
          enddo

          do n=nqs1+1,nqs2
            !$omp parallel do default(shared)  &
            !$omp private(i,j,k)
            !$acc parallel loop gang vector collapse(3) default(present)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              dum3(i,j,k)=dum3(i,j,k)+qa(i,j,k,n)
            enddo
            enddo
            enddo
            !$acc end parallel
          enddo

        ELSE

          !$omp parallel do default(shared)  &
          !$omp private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            dum3(i,j,k)=0.0
          enddo
          enddo
          enddo

        ENDIF

        !$omp parallel do default(shared)  &
        !$omp private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present)
        DO k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k)=cv+cvv*qa(i,j,k,nqv)+cpl*dum2(i,j,k)+cpi*dum3(i,j,k)
        enddo
        enddo
        ENDDO

      ELSE
        !$omp parallel do default(shared) private(i,j,k,n)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,n)
        DO k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k)=cv
        enddo
        enddo
        ENDDO
      ENDIF

      !----- store appropriate rho for budget calculations in dum2 -----!
      IF(axisymm.eq.1)THEN
       ! for axisymmetric grid:
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum2(i,j,k) = rho(i,j,k)*pi*(xf(i+1)**2-xf(i)**2)/(dx*dy)
        enddo
        enddo
        enddo
      ELSE
       ! for Cartesian grid:
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum2(i,j,k) = rho(i,j,k)
        enddo
        enddo
        enddo
      ENDIF

      !-------------------------------------------------------------
      
      !  budget calculations:
      if(dosfcflx.and.imoist.eq.1)then
        !JMD-FIXME review the indexing in this loop 
        qtmp =0.0
        !$acc data create(budarray)
        !$omp parallel do default(shared) private(i,j,delpi,delth,delqv,delt,n)
        !$acc parallel loop gang vector collapse(2) default(present) &
        !$acc private(i,j,delpi,delth,delqv,delt,n)
        do j=1,nj
        do i=1,ni
          delth = rf0(i,j,1)*rr0(i,j,1)*rdz*mh(i,j,1)*thflux(i,j)
          delqv = rf0(i,j,1)*rr0(i,j,1)*rdz*mh(i,j,1)*qvflux(i,j)
          delpi = rddcv*(pi0(i,j,1)+ppi(i,j,1))*(           &
                               delqv/( eps+qa(i,j,1,nqv)) &
                               +delth/(th0(i,j,1)+tha(i,j,1))  )
          delt = (pi0(i,j,1)+ppi(i,j,1))*delth   &
                +(th0(i,j,1)+tha(i,j,1))*delpi
          budarray(i,j) = dum2(i,j,1)*ruh(i)*rvh(j)*rmh(i,j,1)*(           &
                   cv*delt                                                   &
                 + cvv*qa(i,j,1,nqv)*delt                                    &
                 + cvv*(pi0(i,j,1)+ppi(i,j,1))*(th0(i,j,1)+tha(i,j,1))*delqv &
                 + g*zh(i,j,1)*delqv   )

          if( nql1.ge.1 )then
            !$acc loop seq
            do n=nql1,nql2
              budarray(i,j) = budarray(i,j) + dum2(i,j,1)*ruh(i)*rvh(j)*rmh(i,j,1)*cpl*qa(i,j,1,n)*delt
            enddo
          endif
          if(iice.eq.1)then
            !$acc loop seq
            do n=nqs1,nqs2
              !bud2(j) = bud2(j) + dum2(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)*cpi*qa(i,j,k,n)*delt
              budarray(i,j) = budarray(i,j) + dum2(i,j,1)*ruh(i)*rvh(j)*rmh(i,j,1)*cpi*qa(i,j,1,n)*delt
            enddo
          endif
        enddo
        enddo

        !$acc parallel default(present) reduction(+:budtmp,qtmp)
        !$acc loop gang reduction(+:budtmp) reduction(+:qtmp)
        do j=1,nj
          budtmp=0.0
          !$acc loop vector reduction(+:budtmp)
          do i=1,ni
            budtmp = budtmp + budarray(i,j) 
          enddo
          bud2(j)=budtmp
          qtmp = qtmp + dt*dx*dy*dz*bud2(j)
        enddo
        !$acc end parallel
        qbudget(9) = qtmp

        !$acc end data
        if(timestats.ge.1) time_misc09=time_misc09+mytime()
      endif

      !---- Dissipative heating term:

      IF(idiss.eq.1)THEN
        IF( dotbud .and. td_diss.ge.1 )THEN
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang collapse(3) vector default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            tdiag(i,j,k,td_diss) = thten1(i,j,k)
          enddo
          enddo
          enddo
        ENDIF
        ! note:  dissten array stores epsilon (dissipation rate) at w points
        if( bbc.eq.3 )then
          k1 = 2
        else
          k1 = 1
        endif
        if(imoist.eq.1.and.eqtset.eq.2)then
          ! moist, new equations:
          !$omp parallel do default(shared) private(i,j,k,epsd,dheat)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,epsd,dheat)
          do k=k1,nk
          do j=1,nj
          do i=1,ni
            epsd = 0.5*(dissten(i,j,k)+dissten(i,j,k+1))
            dheat=epsd/( cpdcv*dum1(i,j,k)*(pi0(i,j,k)+ppi(i,j,k)) )
            thten1(i,j,k)=thten1(i,j,k)+dheat
          enddo
          enddo
          enddo
          if( bbc.eq.3 )then
            k = 1
            !$omp parallel do default(shared) private(i,j,dz1,epsd,dheat)
            !$acc parallel loop gang vector collapse(2) default(present) private(i,j,dz1,epsd,dheat)
            do j=1,nj
            do i=1,ni
              dz1 = zf(i,j,2)-zf(i,j,1)
              epsd = (ust(i,j)**3)*alog((dz1+zntmp(i,j))/zntmp(i,j))/(karman*dz1)
              dheat=epsd/( cpdcv*dum1(i,j,k)*(pi0(i,j,k)+ppi(i,j,k)) )
              thten1(i,j,k)=thten1(i,j,k)+dheat
            enddo
            enddo
          endif
        else
          ! traditional cloud-modeling equations (also dry equations):
          !$omp parallel do default(shared) private(i,j,k,dz1,epsd,dheat)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,dz1,epsd,dheat)
          do k=k1,nk
          do j=1,nj
          do i=1,ni
            epsd = 0.5*(dissten(i,j,k)+dissten(i,j,k+1))
            dheat=epsd/( cp*(pi0(i,j,k)+ppi(i,j,k)) )
            thten1(i,j,k)=thten1(i,j,k)+dheat
          enddo
          enddo
          enddo
          if( bbc.eq.3 )then
            k = 1
            !$omp parallel do default(shared) private(i,j,dz1,epsd,dheat)
            !$acc parallel loop gang vector collapse(2) default(present) private(i,j,dz1,epsd,dheat)
            do j=1,nj
            do i=1,ni
              dz1 = zf(i,j,2)-zf(i,j,1)
              epsd = (ust(i,j)**3)*alog((dz1+zntmp(i,j))/zntmp(i,j))/(karman*dz1)
              dheat=epsd/( cp*(pi0(i,j,k)+ppi(i,j,k)) )
              thten1(i,j,k)=thten1(i,j,k)+dheat
            enddo
            enddo
          endif
        endif
        IF( dotbud .and. td_diss.ge.1 )THEN
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            tdiag(i,j,k,td_diss) = thten1(i,j,k)-tdiag(i,j,k,td_diss)
          enddo
          enddo
          enddo
        ENDIF
      ENDIF

      !---- Rotunno-Emanuel "radiation" term
      !---- (currently capped at 2 K/day ... see RE87 p 546)

      IF(rterm.eq.1)THEN
        if( dotbud .and. td_rad.ge.1 )then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            tdiag(i,j,k,td_rad) = thten1(i,j,k)
          enddo
          enddo
          enddo
        endif
        tem0 = dt*dx*dy*dz
        !FIXME
        !$omp parallel do default(shared) private(i,j,k,thrad,prad)
        !$acc parallel default(present) private(i,j,k,thrad,prad) reduction(+:budtmp)
        !$acc loop gang
        do k=1,nk
          budtmp=0.0d0
          !$acc loop vector collapse(2) reduction(+:budtmp)
          do j=1,nj
          do i=1,ni
            ! NOTE:  thrad is a POTENTIAL TEMPERATURE tendency
            thrad = -tha(i,j,k)/(12.0*3600.0)
            if( tha(i,j,k).gt. 1.0 ) thrad = -1.0/(12.0*3600.0)
            if( tha(i,j,k).lt.-1.0 ) thrad =  1.0/(12.0*3600.0)
            thten1(i,j,k)=thten1(i,j,k)+thrad
            ! associated pressure tendency:
            prad = (pi0(i,j,k)+ppi(i,j,k))*rddcv*thrad/(th0(i,j,k)+tha(i,j,k))
            ! budget:
            budtmp = budtmp + dum1(i,j,k)*dum2(i,j,k)*ruh(i)*rvh(j)*rmh(i,j,k)*( &
                            thrad*(pi0(i,j,k)+ppi(i,j,k))    &
                           + prad*(th0(i,j,k)+tha(i,j,k)) )
          enddo
          enddo
          bud(k) = budtmp
        enddo
        !$acc end parallel
        !FIXME
        budtmp=qbudget(10)

        !$acc parallel loop gang vector default(present) private(k) reduction(+:budtmp)
        do k=1,nk
          budtmp = budtmp + tem0*bud(k)
        enddo

        qbudget(10)=budtmp

        if( dotbud .and. td_rad.ge.1 )then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            tdiag(i,j,k,td_rad) = thten1(i,j,k)-tdiag(i,j,k,td_rad)
          enddo
          enddo
          enddo
        endif
      ENDIF
      if(timestats.ge.1) time_misc10=time_misc10+mytime()

      IF( radopt.ge.1 )THEN
        ! Radiation tendencies:

        ! Notes:
        ! use sadv to store total potential temperature:
        ! TEMPERATURE tendencies from radiation scheme
        ! are stored in lwten and swten

        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=0,nj+1
        do i=0,ni+1
          sadv(i,j,k)=th0(i,j,k)+tha(i,j,k)
        enddo
        enddo
        enddo

        if( dotbud .and. td_rad.ge.1 )then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            tdiag(i,j,k,td_rad) = thten1(i,j,k)
          enddo
          enddo
          enddo
        endif
        IF( eqtset.eq.1 )THEN
          ! traditional equation set:
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            ! cm1r17:  swten and lwten now store TEMPERATURE tendencies:
            thten1(i,j,k) = thten1(i,j,k) + (swten(i,j,k)+lwten(i,j,k))/(pi0(i,j,k)+ppi(i,j,k))
          enddo
          enddo
          enddo
        ELSEIF( eqtset.eq.2 )THEN
          ! Bryan-Fritsch equation set:
          rdt = 1.0/dt
          !$omp parallel do default(shared) private(i,j,k,tnew,pnew,thnew)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,tnew,pnew,thnew)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            ! cm1r17:  swten and lwten now store TEMPERATURE tendencies:
            ! NOTE:  sadv stores theta (see above)
            tnew = sadv(i,j,k)*(pi0(i,j,k)+ppi(i,j,k)) + dt*(swten(i,j,k)+lwten(i,j,k))
            pnew = rho(i,j,k)*(rd+rv*qa(i,j,k,nqv))*tnew
            thnew = tnew/((pnew*rp00)**rovcp)
            thten1(i,j,k) = thten1(i,j,k) + (thnew-sadv(i,j,k))*rdt
          enddo
          enddo
          enddo
        ENDIF
        if( dotbud .and. td_rad.ge.1 )then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            tdiag(i,j,k,td_rad) = thten1(i,j,k)-tdiag(i,j,k,td_rad)
          enddo
          enddo
          enddo
        endif
        if(timestats.ge.1) time_rad=time_rad+mytime()
      ENDIF

      IF( use_pbl )THEN
        ! PBL tendencies:

        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          thten1(i,j,k) = thten1(i,j,k) + thpten(i,j,k)
        enddo
        enddo
        enddo
        if( dotbud .and. td_pbl.ge.1 )then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            tdiag(i,j,k,td_pbl) = thpten(i,j,k)
          enddo
          enddo
          enddo
        endif
        if(timestats.ge.1) time_pbl=time_pbl+mytime()

      ENDIF

!--------------------------------------------------------------------
!  subgrid turbulence tendencies:

      doturb:  &
      IF( dohturb .or. dovturb )THEN

        if( sgsmodel.eq.5 .or. sgsmodel.eq.6 )then
          ! NBA sgs turbulence models:
          call turbu(dt,xh,ruh,xf,rxf,arf1,arf2,uf,vh,mh,mf,rmf,rho,rf,  &
                     zs,gz,rgz,gzu,gzv,rds,sigma,rdsf,sigmaf,gxu,     &
                     dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,ua,uten1,wa,m11,m12,m13,m22,kmv, &
                     kmw,u1b,u2pt,doubud,udiag)
          call turbv(dt,xh,rxh,arh1,arh2,uh,xf,rvh,vf,mh,mf,rho,rr,rf,   &
                     zs,gz,rgz,gzu,gzv,rds,sigma,rdsf,sigmaf,gyv,  &
                     dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,va,vten1,wa,m12,m22,m23,kmv, &
                     kmw,v1b,v2pt,dovbud,vdiag)

          call turbw(dt,xh,rxh,arh1,arh2,uh,xf,vh,mh,mf,rho,rf,gz,rgzu,rgzv,rds,sigma,   &
                     dum1,dum2,dum3,dum4,dum5,dum6,wa,wten1,m13,m23,m33,m22,kmh,  &
                     dowbud,wdiag)

        else
          ! all other sgs turbulence models:

          call turbu(dt,xh,ruh,xf,rxf,arf1,arf2,uf,vh,mh,mf,rmf,rho,rf,  &
                     zs,gz,rgz,gzu,gzv,rds,sigma,rdsf,sigmaf,gxu,     &
                     dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,ua,uten1,wa,t11,t12,t13,t22,kmv, &
                     kmw,u1b,u2pt,doubud,udiag)
          call turbv(dt,xh,rxh,arh1,arh2,uh,xf,rvh,vf,mh,mf,rho,rr,rf,   &
                     zs,gz,rgz,gzu,gzv,rds,sigma,rdsf,sigmaf,gyv,  &
                     dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,va,vten1,wa,t12,t22,t23,kmv, &
                     kmw,v1b,v2pt,dovbud,vdiag)

          call turbw(dt,xh,rxh,arh1,arh2,uh,xf,vh,mh,mf,rho,rf,gz,rgzu,rgzv,rds,sigma,   &
                     dum1,dum2,dum3,dum4,dum5,dum6,wa,wten1,t13,t23,t33,t22,kmh,  &
                     dowbud,wdiag)


        endif
        if( doimpl.eq.1 .and. dovturb )then
          !  Arrays for vimpl turbs:
          !    NOTE:  do not change dum7,dum8 from here to RK loop
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            dum7(i,j,k) = khv(i,j,k  )*mf(i,j,k  )*rf(i,j,k  )*mh(i,j,k)*rr(i,j,k)
            dum8(i,j,k) = khv(i,j,k+1)*mf(i,j,k+1)*rf(i,j,k+1)*mh(i,j,k)*rr(i,j,k)
          enddo
          enddo
          enddo
          !1238 $acc end data
        endif

        ! cm1r18: subtract th0r from theta (as in advection scheme)
        !         (reduces roundoff error)
        IF(.not.terrain_flag)THEN
          !$omp parallel do default(shared) private(i,j,k,tem)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=0,nj+1
          do i=0,ni+1
            sadv(i,j,k)=(th0(1,1,k)-th0r)+tha(i,j,k)
          enddo
          enddo
          enddo
        ELSE
          !$omp parallel do default(shared) private(i,j,k,tem)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=0,nj+1
          do i=0,ni+1
            sadv(i,j,k)=(th0(i,j,k)-th0r)+tha(i,j,k)
          enddo
          enddo
          enddo
        ENDIF
        call turbs(1,dt,dosfcflx,xh,rxh,arh1,arh2,uh,xf,arf1,arf2,uf,vh,vf,thflux,   &
                   rds,sigma,rdsf,sigmaf,mh,mf,gz,rgz,gzu,rgzu,gzv,rgzv,gx,gxu,gy,gyv, &
                   dum1,dum2,dum3,dum4,dum5,sten,rho,rr,rf,sadv,thten1,khh,khv,dum7,dum8, &
                   dotbud,ibdt,iedt,jbdt,jedt,kbdt,kedt,ntdiag,tdiag,td_hturb,td_vturb,1,pi0,prs0)

        IF( imoist.eq.1 )THEN
          ! moisture:
          do n=1,numq
            if( n.eq.nqv )then
              call turbs(1,dt,dosfcflx,xh,rxh,arh1,arh2,uh,xf,arf1,arf2,uf,vh,vf,qvflux,   &
                         rds,sigma,rdsf,sigmaf,mh,mf,gz,rgz,gzu,rgzu,gzv,rgzv,gx,gxu,gy,gyv, &
                         dum1,dum2,dum3,dum4,dum5,sten,rho,rr,rf,qa(ib,jb,kb,n),qten(ib,jb,kb,n),khh,khv,dum7,dum8, &
                         doqbud,ibdq,iedq,jbdq,jedq,kbdq,kedq,nqdiag,qdiag,qd_hturb,qd_vturb,2,pi0,prs0)
            else
              call turbs(0,dt,dosfcflx,xh,rxh,arh1,arh2,uh,xf,arf1,arf2,uf,vh,vf,qvflux,   &
                         rds,sigma,rdsf,sigmaf,mh,mf,gz,rgz,gzu,rgzu,gzv,rgzv,gx,gxu,gy,gyv, &
                         dum1,dum2,dum3,dum4,dum5,sten,rho,rr,rf,qa(ib,jb,kb,n),qten(ib,jb,kb,n),khh,khv,dum7,dum8, &
                         .false.,ibdq,iedq,jbdq,jedq,kbdq,kedq,nqdiag,qdiag,1,1,0,pi0,prs0)
            endif
          enddo
        ENDIF

        IF(iptra.eq.1)THEN
          ! passive tracers:
          do n=1,npt
            call turbs(0,dt,dosfcflx,xh,rxh,arh1,arh2,uh,xf,arf1,arf2,uf,vh,vf,qvflux,   &
                       rds,sigma,rdsf,sigmaf,mh,mf,gz,rgz,gzu,rgzu,gzv,rgzv,gx,gxu,gy,gyv, &
                       dum1,dum2,dum3,dum4,dum5,sten,rho,rr,rf,pta(ib,jb,kb,n),ptten(ib,jb,kb,n),khh,khv,dum7,dum8, &
                       .false.,ibdq,iedq,jbdq,jedq,kbdq,kedq,nqdiag,qdiag,1,1,0,pi0,prs0)
          enddo
        ENDIF

      ENDIF  doturb

!--------------------------------------------------------------------
!  U-equation

      IF( iinit.eq.10 .and. mtime.lt.t2_uforce )THEN
        ! u-forcing for squall-line initialization:
        ! (Morrison et al, 2015, JAS, pg 315)
        gamm = 1.0
        if(mtime.ge.t1_uforce)THEN
          gamm = 1.0+(0.0-1.0)*(mtime-t1_uforce)/(t2_uforce-t1_uforce)
        endif
        if(myid.eq.0) print *,'  mtime,gamm = ',mtime,gamm
        !$omp parallel do default(shared) private(i,j,k,aiu)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,aiu)
        do k=1,nk
        do j=1,nj
        do i=1,ni+1
          if( abs(xf(i)-xc_uforce).lt.xr_uforce .and. abs(zf(i,j,k)-zs(i,j)).lt.zr_uforce )then
            aiu = alpha_uforce*cos(0.5*pi*(xf(i)-xc_uforce)/xr_uforce)   &
                              *((cosh(2.5*(zf(i,j,k)-zs(i,j))/zr_uforce))**(-2))
            uten1(i,j,k)=uten1(i,j,k)+gamm*aiu
          endif
        enddo
        enddo
        enddo
      ENDIF
      if(timestats.ge.1) time_rdamp=time_rdamp+mytime()

      if( use_pbl )then
        if( axisymm.eq.1 )then
          ip = 2
        else
          ip = 1
        endif
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=ip,ni+1
          uten1(i,j,k) = uten1(i,j,k) + 0.5*( upten(i-1,j,k)+ upten(i,j,k))
        enddo
        enddo
        enddo
        IF( doubud .and. ud_pbl.ge.1 )then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=ip,ni+1
            udiag(i,j,k,ud_pbl) = 0.5*( upten(i-1,j,k)+ upten(i,j,k))
          enddo
          enddo
          enddo
        ENDIF
        if(timestats.ge.1) time_pbl=time_pbl+mytime()
      endif

!--------------------------------------------------------------------
!  V-equation

      if( use_pbl )then
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,ni
          vten1(i,j,k) = vten1(i,j,k) + 0.5*( vpten(i,j-1,k)+ vpten(i,j,k))
        enddo
        enddo
        enddo
        IF( dovbud .and. vd_pbl.ge.1 )then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj+1
          do i=1,ni
            vdiag(i,j,k,vd_pbl) = 0.5*( vpten(i,j-1,k)+ vpten(i,j,k))
          enddo
          enddo
          enddo
        ENDIF
        if(timestats.ge.1) time_pbl=time_pbl+mytime()
      endif
 
!-------------------------------------------------------------------
!  Moisture

      if(imoist.eq.1)then
            if( use_pbl )then
              !$omp parallel do default(shared) private(i,j,k)
              !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
              do k=1,nk
              do j=1,nj
              do i=1,ni
                qten(i,j,k,nqv) = qten(i,j,k,nqv) + qvpten(i,j,k)
              enddo
              enddo
              enddo
              if( doqbud .and. qd_pbl.ge.1 )then
                !$omp parallel do default(shared) private(i,j,k)
                !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
                do k=1,nk
                do j=1,nj
                do i=1,ni
                  qdiag(i,j,k,qd_pbl) = qvpten(i,j,k)
                enddo
                enddo
                enddo
              endif
              if(timestats.ge.1) time_pbl=time_pbl+mytime()
            endif
        IF( use_pbl )THEN
        if(nqc.ge.1)then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            qten(i,j,k,nqc) = qten(i,j,k,nqc) + qcpten(i,j,k)
          enddo
          enddo
          enddo
        endif
        if(nqi.ge.1)then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            qten(i,j,k,nqi) = qten(i,j,k,nqi) + qipten(i,j,k)
          enddo
          enddo
          enddo
        endif
        if(nnci.ge.1)then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            qten(i,j,k,nnci) = qten(i,j,k,nnci) + qnipten(i,j,k)
          enddo
          enddo
          enddo
        endif
        if(nncc.ge.1)then
          !$omp parallel do default(shared) private(i,j,k)
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            qten(i,j,k,nncc) = qten(i,j,k,nncc) + qncpten(i,j,k)
          enddo
          enddo
          enddo
        endif
          if(timestats.ge.1) time_pbl=time_pbl+mytime()
        ENDIF
      endif

!--------------------------------------------------------------------

    IF( tqnudge )THEN

      if( imoist.eq.0 .and. td_nudge.ge.1   &
          .and. dotbud )then
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,ni+1
          tdiag(i,j,k,td_nudge) = thten1(i,j,k)
        enddo
        enddo
        enddo
      elseif( imoist.eq.1 .and. td_nudge.ge.1   &
          .and. dotbud .and. doqbud .and. qd_nudge.ge.1 )then
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,ni+1
          tdiag(i,j,k,td_nudge) = thten1(i,j,k)
          qdiag(i,j,k,qd_nudge) = qten(i,j,k,nqv)
        enddo
        enddo
        enddo
      endif

      ! ramp-up nudging at start of simulation:
      nudgefac = (mtime-0.0)/(900.0-0.0)
      nudgefac = max(0.0,nudgefac)
      nudgefac = min(1.0,nudgefac)

      ! nudging time scale (seconds):
      taunudge = 20.0

      if( abs(nudgefac).gt.0.001 )then
        ! make sure subsidence is off:
        dolsw = .false.
        if( myid.eq.0 ) print *,'  nudging with nudgefac,taunudge = ',nudgefac,taunudge
        tem = nudgefac/taunudge
        if( imoist.eq.0 )then
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,tn)
          do k=1,nk
            do j=1,nj+1
            do i=1,ni+1
              tn = -(thavg(k)-th0(1,1,k))*tem
              thten1(i,j,k) = thten1(i,j,k) + tn
            enddo
            enddo
          enddo
        else
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,tn,qn)
          do k=1,nk
            do j=1,nj+1
            do i=1,ni+1
              tn = -(thavg(k)-th0(1,1,k))*tem
              qn = -(qavg(k,nqv)-qv0(1,1,k))*tem
              thten1(i,j,k) = thten1(i,j,k) + tn
              qten(i,j,k,nqv) = qten(i,j,k,nqv) + qn
            enddo
            enddo
          enddo
        endif
      else
        if(myid.eq.0) print *,'  no nudging (yet) '
      endif

      if( imoist.eq.0 .and. td_nudge.ge.1   &
          .and. dotbud )then
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,ni+1
          tdiag(i,j,k,td_nudge) = thten1(i,j,k)-tdiag(i,j,k,td_nudge)
        enddo
        enddo
        enddo
      elseif( imoist.eq.1 .and. td_nudge.ge.1   &
          .and. dotbud .and. doqbud .and. qd_nudge.ge.1 )then
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj+1
        do i=1,ni+1
          tdiag(i,j,k,td_nudge) = thten1(i,j,k)-tdiag(i,j,k,td_nudge)
          qdiag(i,j,k,qd_nudge) = qten(i,j,k,nqv)-qdiag(i,j,k,qd_nudge)
        enddo
        enddo
        enddo
      endif

      if( terrain_flag ) stop 82701

      if(timestats.ge.1) time_misc11=time_misc11+mytime()

    ENDIF

!--------------------------------------------------------------------
!  TENDENCIES for pre-configured cases:
!    (new for cm1r19)

      IF( testcase.ge.1 )THEN
        call     testcase_simple_phys(mh,rho0,rr0,rf0,th0,u0,v0,     &
                   zh,zf,dum1,dum2,dum3,dum4,dum5,dum6,              &
                   ufrc,vfrc,thfrc,qvfrc,ug,vg,dvdr,                 &
                   uavg,vavg,thavg,qavg,cavg,                        &
                   ua,va,tha,qa,uten1,vten1,thten1,qten,             &
                   o30 ,zir,ruh,ruf,rvh,rvf)
        if(timestats.ge.1) time_misc12=time_misc12+mytime()
        
      ENDIF
!-------------------------------------------------------------------
!   Eddy recycler

      doeddyrec:  &
      IF( do_recycle )THEN
#ifdef _OPENACC
        print *,'WARNING: solve1: do_eddy_{recyr,recys} code not ported to OPENACC'
#endif
        if( do_recycle_w )then
          call   do_eddy_recyw(dt,xh,xf,yh,yf,zh,zf,u3d,v3d,w3d,uten1,vten1,wten1,urecyw,vrecyw,wrecyw,trecyw,out3d)
        endif
        if( do_recycle_s )then
          call   do_eddy_recys(dt,xh,xf,yh,yf,zh,zf,u3d,v3d,w3d,uten1,vten1,wten1,urecys,vrecys,wrecys,trecys,out3d)
        endif

        if(timestats.ge.1) time_ercyl=time_ercyl+mytime()

      ENDIF  doeddyrec

 
!--------------------------------------------------------------------


    IF( do_lsnudge .and. timenudge.gt.lsnudge_start )THEN
      if( donudge .and. do_lsnudge_u )then
        if( myid.eq.0 ) print *,'  applying large-scale nudging of u, lsnudge_count = ',lsnudge_count
        rdt = 1.0/dt
        !$acc parallel loop gang vector default(present) private(i,j,k,tem1)
        do k=1,nk
          tem1 = -lsnudgefac*( (umove+uavg(k))-lsnudge_u(k,lsnudge_count) )/(lsnudge_tau)
          ! apply nudging tendency:
          do j=1,nj
          do i=1,ni+1
            uten1(i,j,k) = uten1(i,j,k) + tem1
          enddo
          enddo
        enddo
      endif
      if( donudge .and. do_lsnudge_v )then
        if( myid.eq.0 ) print *,'  applying large-scale nudging of v, lsnudge_count = ',lsnudge_count
        rdt = 1.0/dt
        !$acc parallel loop gang vector default(present) private(i,j,k,tem2)
        do k=1,nk
          tem2 = -lsnudgefac*( (vmove+vavg(k))-lsnudge_v(k,lsnudge_count) )/(lsnudge_tau)
          ! apply nudging tendency:
          do j=1,nj+1
          do i=1,ni
            vten1(i,j,k) = vten1(i,j,k) + tem2
          enddo
          enddo
        enddo
      endif
      if( donudge .and. do_lsnudge_th )then
        if( myid.eq.0 ) print *,'  applying large-scale nudging of theta, lsnudge_count = ',lsnudge_count
        rdt = 1.0/dt
        !$acc parallel loop gang vector default(present) private(i,j,k,tem1)
        do k=1,nk
          tem1 = -lsnudgefac*( thavg(k)-lsnudge_th(k,lsnudge_count) )/(lsnudge_tau)
          ! apply nudging tendency:
          do j=1,nj
          do i=1,ni
            thten1(i,j,k) = thten1(i,j,k) + tem1
          enddo
          enddo
        enddo
      endif
      if( donudge .and. do_lsnudge_qv .and. imoist.eq.1 )then
        if( myid.eq.0 ) print *,'  applying large-scale nudging of qv, lsnudge_count = ',lsnudge_count
        rdt = 1.0/dt
        !$acc parallel loop gang vector default(present) private(i,j,k,tem1)
        do k=1,nk
          tem1 = -lsnudgefac*( qavg(k,nqv)-lsnudge_qv(k,lsnudge_count) )/(lsnudge_tau)
          ! apply nudging tendency:
          do j=1,nj
          do i=1,ni
            qten(i,j,k,nqv) = qten(i,j,k,nqv) + tem1
          enddo
          enddo
        enddo
      endif
    ENDIF


!-------------------------------------------------------------------
!    NOTE:  now ok to change dum7,dum8
!-------------------------------------------------------------------
!  contribution to pressure tendency from potential temperature:
!  (for mass conservation)
!  plus, some other stuff:

    ps6:  &
    IF( psolver.le.3 )THEN

      IF(eqtset.eq.1)THEN
        ! traditional cloud modeling:
        !$omp parallel do default(shared) private(i,j,k)
        !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          ppten(i,j,k)=0.0
        enddo
        enddo
        enddo
      ELSE
        ! mass-conserving pressure eqt:  different sections for moist/dry cases:
        rdt = 1.0/dt
        tem = 0.0001*tsmall
        IF(imoist.eq.1)THEN
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,pinew,thnew,qvnew)
          !$omp parallel do default(shared) private(i,j,k,pinew,thnew,qvnew)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            ! cm1r17:
            ! note:  nothing in pre-RK section should modify rho
            !jmd  = (k-kb)*((ie-ib+1)*(je-jb+1)) + (j-jb)*(ie-ib+1) + (i-ib)
            IF( abs(dt*thten1(i,j,k)).gt.tem .or.  &
                abs(dt*qten(i,j,k,nqv)).gt.qsmall )THEN
              thnew = tha(i,j,k)+dt*thten1(i,j,k)
              qvnew = qa(i,j,k,nqv)+dt*qten(i,j,k,nqv)
              pinew = (rho(i,j,k)*(th0(i,j,k)+thnew)*(rd+rv*qvnew)*rp00)**rddcv - pi0(i,j,k)
              ppten(i,j,k) = (pinew-ppi(i,j,k))*rdt
            ELSE
              ppten(i,j,k) = 0.0
            ENDIF
          enddo
          enddo
          enddo
          if( ptype.ge.1 )then
            ! use diabatic tendencies from last timestep as a good estimate:
            !$omp parallel do default(shared)  &
            !$omp private(i,j,k,tnew,pnew,pinew,thnew,qvnew)
            !$acc parallel loop gang vector default(present) collapse(3)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              ppten(i,j,k)=ppten(i,j,k)+qpten(i,j,k)
              thten1(i,j,k)=thten1(i,j,k)+qtten(i,j,k)
              qten(i,j,k,nqv)=qten(i,j,k,nqv)+qvten(i,j,k)
              qten(i,j,k,nqc)=qten(i,j,k,nqc)+qcten(i,j,k)
            enddo
            enddo
            enddo
          endif
        ELSE
          !$acc parallel loop gang vector collapse(3) default(present) private(i,j,k,pinew,thnew)
          !$omp parallel do default(shared) private(i,j,k,pinew,thnew,qvnew)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            !-----
            ! cm1r17:
            ! note:  nothing in pre-RK section should modify rho
            IF( abs(dt*thten1(i,j,k)).gt.tem )THEN
              thnew = tha(i,j,k)+dt*thten1(i,j,k)
              pinew = (rho(i,j,k)*(th0(i,j,k)+thnew)*rd*rp00)**rddcv - pi0(i,j,k)
              ppten(i,j,k) = (pinew-ppi(i,j,k))*rdt
            ELSE
              ppten(i,j,k)=0.0
            ENDIF
            !-----
          enddo
          enddo
          enddo
        ENDIF  ! endif for moist/dry
      ENDIF    ! endif for eqtset 1/2

    ENDIF  ps6

        if(timestats.ge.1) time_integ=time_integ+mytime()

!--------------------------------------------------------------------

    ! NOTE:

    ! cm1r20.1:  - moved microphysics to mp_driver (which is called from cm1.F)
    !            - moved message passing and equate to solve_finish.F

      !!$acc end data

      end subroutine solve1


  END MODULE solve1_module
