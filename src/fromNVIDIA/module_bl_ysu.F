!WRF:model_layer:physics
!
!
!
!
!
!
!
module module_bl_ysu
contains
!
!
!-------------------------------------------------------------------------------
!
   subroutine ysu(u3d,v3d,th3d,t3d,qv3d,qc3d,qi3d,p3d,p3di,pi3d,               &
                  rublten,rvblten,rthblten,                                    &
                  rqvblten,rqcblten,rqiblten,flag_qi,                          &
                  cp,g,rovcp,rd,rovg,ep1,ep2,karman,xlv,rv,                    &
                  dz8w,psfc,                                                   &
                  znu,znw,mut,p_top,                                           &
                  znt,ust,hpbl,psim,psih,                                      &
                  xland,hfx,qfx,wspd,br,                                       &
                  dt,kpbl2d,                                                   &
                  exch_h,                                                      &
                  wstar,delta,                                                 &
                  u10,v10,                                                     &
                  uoce,voce,                                                   &        
                  rthraten,ysu_topdown_pblmix,                                 &
                  ctopo,ctopo2,                                                &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte,                                   &
                !optional
                  regime                                           )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!-- u3d         3d u-velocity interpolated to theta points (m/s)
!-- v3d         3d v-velocity interpolated to theta points (m/s)
!-- th3d        3d potential temperature (k)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- qc3d        3d cloud mixing ratio (kg/kg)
!-- qi3d        3d ice mixing ratio (kg/kg)
!               (note: if P_QI<PARAM_FIRST_SCALAR this should be zero filled)
!-- p3d         3d pressure (pa)
!-- p3di        3d pressure (pa) at interface level
!-- pi3d        3d exner function (dimensionless)
!-- rr3d        3d dry air density (kg/m^3)
!-- rublten     u tendency due to
!               pbl parameterization (m/s/s)
!-- rvblten     v tendency due to
!               pbl parameterization (m/s/s)
!-- rthblten    theta tendency due to
!               pbl parameterization (K/s)
!-- rqvblten    qv tendency due to
!               pbl parameterization (kg/kg/s)
!-- rqcblten    qc tendency due to
!               pbl parameterization (kg/kg/s)
!-- rqiblten    qi tendency due to
!               pbl parameterization (kg/kg/s)
!-- cp          heat capacity at constant pressure for dry air (j/kg/k)
!-- g           acceleration due to gravity (m/s^2)
!-- rovcp       r/cp
!-- rd          gas constant for dry air (j/kg/k)
!-- rovg        r/g
!-- dz8w        dz between full levels (m)
!-- xlv         latent heat of vaporization (j/kg)
!-- rv          gas constant for water vapor (j/kg/k)
!-- psfc        pressure at the surface (pa)
!-- znt         roughness length (m)
!-- ust         u* in similarity theory (m/s)
!-- hpbl        pbl height (m)
!-- psim        similarity stability function for momentum
!-- psih        similarity stability function for heat
!-- xland       land mask (1 for land, 2 for water)
!-- hfx         upward heat flux at the surface (w/m^2)
!-- qfx         upward moisture flux at the surface (kg/m^2/s)
!-- wspd        wind speed at lowest model level (m/s)
!-- u10         u-wind speed at 10 m (m/s)
!-- v10         v-wind speed at 10 m (m/s)
!-- uoce        sea surface zonal currents (m s-1)
!-- voce        sea surface meridional currents (m s-1)
!-- br          bulk richardson number in surface layer
!-- dt          time step (s)
!-- rvovrd      r_v divided by r_d (dimensionless)
!-- ep1         constant for virtual temperature (r_v/r_d - 1)
!-- ep2         constant for specific humidity calculation
!-- karman      von karman constant
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!-------------------------------------------------------------------------------
!
   integer,parameter ::  ndiff = 3
   real,parameter    ::  rcl = 1.0
!
   integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte

   integer,  intent(in)      ::      ysu_topdown_pblmix
!
   real,     intent(in   )   ::      dt,cp,g,rovcp,rovg,rd,xlv,rv
!
   real,     intent(in )     ::      ep1,ep2,karman
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                          qv3d, &
                                                                         qc3d, &
                                                                         qi3d, &
                                                                          p3d, &
                                                                         pi3d, &
                                                                         th3d, &
                                                                          t3d, &
                                                                         dz8w, &
                                                                     rthraten
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                          p3di
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::                                       rublten, &
                                                                      rvblten, &
                                                                     rthblten, &
                                                                     rqvblten, &
                                                                     rqcblten
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::                                        exch_h
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::                                         wstar
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::                                         delta
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::                                           u10, &
                                                                          v10
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                          uoce, &
                                                                         voce
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                         xland, &
                                                                          hfx, &
                                                                          qfx, &
                                                                           br, &
                                                                         psfc
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                                &
                                                                         psim, &
                                                                         psih
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::                                           znt, &
                                                                          ust, &
                                                                         hpbl, &
                                                                          wspd
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                           u3d, &
                                                                          v3d
!
   integer,  dimension( ims:ime, jms:jme )                                   , &
             intent(out  )   ::                                        kpbl2d
   logical,  intent(in)      ::                                       flag_qi
!
!optional
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             optional                                                        , &
             intent(inout)   ::                                        regime
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             optional                                                        , &
             intent(inout)   ::                                       rqiblten
!
   real,     dimension( kms:kme )                                            , &
             optional                                                        , &
             intent(in   )   ::                                           znu, &
                                                                          znw
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             optional                                                        , &
             intent(in   )   ::                                           mut
!
   real,     optional, intent(in   )   ::                               p_top
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             optional                                                        , &
             intent(in   )   ::                                         ctopo, &
                                                                       ctopo2
!local
   integer ::  i,j,k
   real,     dimension( its:ite, kts:kte*ndiff,jts:jte )  ::         rqvbl2dt, &
                                                                         qv2d
   real,     dimension( its:ite, kts:kte,jts:jte )  ::                    pdh
   real,     dimension( its:ite, kts:kte+1,jts:jte )  ::                 pdhi
   LOGICAL :: present_rqiblten
   real    :: p_top_
! Commented by ARomanenko as unused   
!   real,     dimension( its:ite,jts:jte )          ::                          &
!                                                                        dusfc, &
!                                                                        dvsfc, &
!                                                                        dtsfc, &
!                                                                        dqsfc
!
   present_rqiblten = present(rqiblten)
!$acc data pcopy(u10,v10,u3d,v3d,t3d,psih,psim,xland,hfx,qfx,br,psfc) &
!$acc      pcopy(ctopo,ctopo2,uoce,voce,znt,ust,hpbl,wspd) &
!$acc      pcopy(rublten,rvblten,rthblten,rthraten,wstar,delta,regime) &
!$acc      pcopy(kpbl2d,dz8w,p3di,pi3d,mut,znu,znw,p3d,qv3d,qc3d,qi3d) &
!$acc      pcopy(exch_h,rqvblten,rqcblten,rqiblten) &
!$acc      create(rqvbl2dt,qv2d,pdh,pdhi) 

!$acc kernels async
   qv2d(its:ite,:,:) = 0.0
!$acc end kernels
   if(present(mut))then
     p_top_ = p_top
!$acc kernels async
     do j = jts,jte
!
! For ARW we will replace p and p8w with dry hydrostatic pressure
!
        do k = kts,kte+1
          do i = its,ite
             if(k.le.kte)pdh(i,k,j) = mut(i,j)*znu(k) + p_top_
             pdhi(i,k,j) = mut(i,j)*znw(k) + p_top_
          enddo
        enddo
      enddo
!$acc end kernels
   else
!$acc kernels async
      do j = jts,jte
        do k = kts,kte+1
          do i = its,ite
            if(k.le.kte)pdh(i,k,j) = p3d(i,k,j)
            pdhi(i,k,j) = p3di(i,k,j)
          enddo
        enddo
     enddo
!$acc end kernels
   endif
!$acc kernels async
   do j = jts,jte
      do k = kts,kte
        do i = its,ite
          qv2d(i,k,j) = qv3d(i,k,j)
          qv2d(i,k+kte,j) = qc3d(i,k,j)
          if(present_rqiblten) qv2d(i,k+kte+kte,j) = qi3d(i,k,j)
        enddo
      enddo
   end do
!$acc end kernels      
!$acc wait
      call ysu2d(ux=u3d,vx=v3d                       &
              ,tx=t3d                                               &
              ,qx=qv2d                                                &
              ,p2d=pdh,p2di=pdhi                             &
              ,pi2d=pi3d                                           &
              ,utnp=rublten,vtnp=rvblten                 &
              ,ttnp=rthblten,qtnp=rqvbl2dt,ndiff=ndiff     &
              ,cp=cp,g=g,rovcp=rovcp,rd=rd,rovg=rovg                           &    
              ,xlv=xlv,rv=rv                                                   &
              ,ep1=ep1,ep2=ep2,karman=karman                                   &
              ,dz8w2d=dz8w                                          &
              ,psfcpa=psfc,znt=znt,ust=ust                &
              ,hpbl=hpbl                                                &
              ,regime=regime,psim=psim                           &
              ,psih=psih,xland=xland                             &
              ,hfx=hfx,qfx=qfx                                   &
              ,wspd=wspd,br=br                                   &
!              ,dusfc=dusfc,dvsfc=dvsfc,dtsfc=dtsfc,dqsfc=dqsfc                 &
              ,dt=dt,rcl=1.0,kpbl=kpbl2d                              &
              ,exch_hx=exch_h                                       &
              ,wstar=wstar                                              &
              ,delta=delta                                              &
              ,u10=u10,v10=v10                                   &
              ,uox=uoce,vox=voce                                 &
              ,rthraten=rthraten,p2diORG=p3di            &
              ,ysu_topdown_pblmix=ysu_topdown_pblmix                           &
              ,ctopo=ctopo,ctopo2=ctopo2                         &
              ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde               &
              ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme               &
              ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )
   
!$acc kernels
   do j = jts,jte
     do k = kts,kte
       do i = its,ite
         rthblten(i,k,j) = rthblten(i,k,j)/pi3d(i,k,j)
         rqvblten(i,k,j) = rqvbl2dt(i,k,j)
         rqcblten(i,k,j) = rqvbl2dt(i,k+kte,j)
         if(present_rqiblten) rqiblten(i,k,j) = rqvbl2dt(i,k+kte+kte,j)
       enddo
     enddo
!
   enddo
!$acc end kernels   
!$acc end data   
!
   end subroutine ysu
!
!-------------------------------------------------------------------------------
!
   subroutine ysu2d(ux,vx,tx,qx,p2d,p2di,pi2d,                               &
                  utnp,vtnp,ttnp,qtnp,ndiff,                                   &
                  cp,g,rovcp,rd,rovg,ep1,ep2,karman,xlv,rv,                    &
                  dz8w2d,psfcpa,                                               &
                  znt,ust,hpbl,psim,psih,                                      &
                  xland,hfx,qfx,wspd,br,                                       &
!                  dusfc,dvsfc,dtsfc,dqsfc,                                     &
                  dt,rcl,kpbl,                                               &
                  exch_hx,                                                     &
                  wstar,delta,                                                 &
                  u10,v10,                                                     &
                  uox,vox,                                                     &
                  rthraten,p2diORG,                                            &
                  ysu_topdown_pblmix,                                          &
                  ctopo,ctopo2,                                                &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte,                                   &
                !optional
                  regime )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
!     this code is a revised vertical diffusion package ("ysupbl")
!     with a nonlocal turbulent mixing in the pbl after "mrfpbl".
!     the ysupbl (hong et al. 2006) is based on the study of noh
!     et al.(2003) and accumulated realism of the behavior of the
!     troen and mahrt (1986) concept implemented by hong and pan(1996).
!     the major ingredient of the ysupbl is the inclusion of an explicit
!     treatment of the entrainment processes at the entrainment layer.
!     this routine uses an implicit approach for vertical flux
!     divergence and does not require "miter" timesteps.
!     it includes vertical diffusion in the stable atmosphere
!     and moist vertical diffusion in clouds.
!
!     mrfpbl:
!     coded by song-you hong (ncep), implemented by jimy dudhia (ncar)
!              fall 1996
!
!     ysupbl:
!     coded by song-you hong (yonsei university) and implemented by
!              song-you hong (yonsei university) and jimy dudhia (ncar)
!              summer 2002
!
!     further modifications :
!              an enhanced stable layer mixing, april 2008
!              ==> increase pbl height when sfc is stable (hong 2010)
!              pressure-level diffusion, april 2009
!              ==> negligible differences
!              implicit forcing for momentum with clean up, july 2009
!              ==> prevents model blowup when sfc layer is too low
!              incresea of lamda, maximum (30, 0.1 x del z) feb 2010
!              ==> prevents model blowup when delz is extremely large
!              revised prandtl number at surface, peggy lemone, feb 2010
!              ==> increase kh, decrease mixing due to counter-gradient term
!              revised thermal, shin et al. mon. wea. rev. , songyou hong, aug 2011
!              ==> reduce the thermal strength when z1 < 0.1 h
!              revised prandtl number for free convection, dudhia, mar 2012
!              ==> pr0 = 1 + bke (=0.272) when newtral, kh is reduced
!              minimum kzo = 0.01, lo = min (30m,delz), hong, mar 2012
!              ==> weaker mixing when stable, and les resolution in vertical
!              gz1oz0 is removed, and phim phih are ln(z1/z0)-phim,h, hong, mar 2012
!              ==> consider thermal z0 when differs from mechanical z0
!              a bug fix in wscale computation in stable bl, sukanta basu, jun 2012
!              ==> wscale becomes small with height, and less mixing in stable bl
!
!     references:
!
!        hong (2010) quart. j. roy. met. soc
!        hong, noh, and dudhia (2006), mon. wea. rev.
!        hong and pan (1996), mon. wea. rev.
!        noh, chun, hong, and raasch (2003), boundary layer met.
!        troen and mahrt (1986), boundary layer met.
!
!-------------------------------------------------------------------------------
!
   real,parameter    ::  xkzmin = 0.01,xkzmax = 1000.,rimin = -100.
   real,parameter    ::  rlam = 30.,prmin = 0.25,prmax = 4.
   real,parameter    ::  brcr_ub = 0.0,brcr_sb = 0.25,cori = 1.e-4
   real,parameter    ::  afac = 6.8,bfac = 6.8,pfac = 2.0,pfac_q = 2.0
   real,parameter    ::  phifac = 8.,sfcfrac = 0.1
   real,parameter    ::  d1 = 0.02, d2 = 0.05, d3 = 0.001
   real,parameter    ::  h1 = 0.33333335, h2 = 0.6666667
   real,parameter    ::  ckz = 0.001,zfmin = 1.e-8,aphi5 = 5.,aphi16 = 16.
   real,parameter    ::  tmin=1.e-2
   real,parameter    ::  gamcrt = 3.,gamcrq = 2.e-3
   real,parameter    ::  xka = 2.4e-5
   integer,parameter ::  imvdif = 1
!
   integer,  intent(in   )   ::     ids,ide, jds,jde, kds,kde,                 &
                                    ims,ime, jms,jme, kms,kme,                 &
                                    its,ite, jts,jte, kts,kte,                 &
                                    ndiff

   integer,  intent(in)      ::     ysu_topdown_pblmix
!
   real,     intent(in   )   ::     dt,rcl,cp,g,rovcp,rovg,rd,xlv,rv
!
   real,     intent(in )     ::     ep1,ep2,karman
!
   real,     dimension( ims:ime, kms:kme, jms:jme ),                           &
             intent(in)      ::                                        dz8w2d, &
                                                                         pi2d, &
                                                                      p2diorg
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                            tx
   real,     dimension( its:ite, kts:kte*ndiff,jts:jte )                             , &
             intent(in   )   ::                                            qx
!
   real,     dimension( ims:ime, kms:kme,jms:jme )                           , &
             intent(inout)   ::                                          utnp, &
                                                                         vtnp, &
                                                                         ttnp
   real,     dimension( its:ite, kts:kte*ndiff,jts:jte )                     , &
             intent(inout)   ::                                          qtnp
!
   real,     dimension( its:ite, kts:kte+1,jts:jte )                                 , &
             intent(in   )   ::                                          p2di
!
   real,     dimension( its:ite, kts:kte,jts:jte )                                   , &
             intent(in   )   ::                                           p2d
!
!
   real,     dimension( ims:ime, jms:jme )                                            , &
             intent(inout)   ::                                           ust, &
                                                                         hpbl, &
                                                                          znt
   real,     dimension( ims:ime ,jms:jme)                                            , &
             intent(in   )   ::                                         xland, &
                                                                          hfx, &
                                                                          qfx
!
   real,     dimension( ims:ime,jms:jme ), intent(inout)   ::                    wspd
   real,     dimension( ims:ime,jms:jme ), intent(in  )    ::                      br
!
   real,     dimension( ims:ime,jms:jme ), intent(in   )   ::            psim, &
                                                                         psih
!
   real,     dimension( ims:ime,jms:jme ), intent(in   )   ::                  psfcpa
   integer,  dimension( ims:ime,jms:jme ), intent(out  )   ::                  kpbl
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                            ux, &
                                                                           vx, &
                                                                     rthraten
   real,     dimension( ims:ime, jms:jme )                                            , &
             optional                                                        , &
             intent(in   )   ::                                         ctopo, &
                                                                       ctopo2
   real,     dimension( ims:ime,jms:jme )                                            , &
             optional                                                        , &
             intent(inout)   ::                                        regime
!
! local vars
!
!   real,     dimension( its:ite,jts:jte )            ::                           hol
   real,     dimension( its:ite, kts:kte+1,jts:jte ) ::                            zq
!
   real,     dimension( its:ite, kts:kte,jts:jte )   ::                        &
                                                               thx,thvx,thlix, &
                                                                          del, &
                                                                          dza, &
!                                                                          dzq, &
                                                                         xkzo, &
                                                                           za
!$acc declare device_resident(zq,thx,thvx,thlix,del,dza,xkzo,za)
   real,    dimension( its:ite,jts:jte )             ::                                &
                                                                         rhox, &
                                                                       govrth, &
                                                                  zl1,thermal, &
                                                                       wscale, &
                                                                  hgamt,hgamq, &
                                                                    brdn,brup, &
                                                                    phim,phih, &
                                                              prpbl,thermalli !, &
!                                                                        wspd1
!$acc declare device_resident(rhox,govrth,zl1,thermal,wscale,hgamt,hgamq,&
!$acc           brdn,brup,phim,phih,prpbl,thermalli)
!   real,    dimension( its:ite,jts:jte )     ::                                &
!                                                                  dusfc,dvsfc, &
!                                                                  dtsfc,dqsfc, &
!
   real,    dimension( its:ite, kts:kte, jms:jme )    ::                     xkzm,xkzh!, &
   real,    dimension( its:ite, kts:kte, jms:jme )    ::                f1,f2, &
                                                                        r1,r2, &
                                                                        ad,au, &
                                                                           cu, &
                                                                           al!!&
   real,    dimension( its:ite, kts:kte, jms:jme )    ::    rhox2,xkzq!, &
   real ::                                                        hgamt2,zfac
!$acc declare device_resident(xkzm,xkzh,f1,f2,r1,r2,ad,au,cu,al,rhox2,xkzq,hgamt2,zfac)
!
!jdf added exch_hx
!
   real,    dimension( ims:ime, kms:kme, jms:jme )                           , &
            intent(inout)   ::                                        exch_hx
!
   real,    dimension( ims:ime,jms:jme )                                     , &
            intent(inout)    ::                                           u10, &
                                                                          v10
   real,    dimension( ims:ime,jms:jme )                                     , & 
            intent(in  )    ::                                            uox, &
                                                                          vox
   real,    dimension( its:ite,jts:jte )    ::                                 &
                                                                         brcr, &
                                                                        sflux, &
                                                                         zol1, &
                                                                    brcr_sbro
   real,    dimension( its:ite, kts:kte, ndiff,jms:jme)  ::                     r3,f3
!$acc declare device_resident(brcr,sflux,zol1,brcr_sbro,r3,f3)
   integer, dimension( its:ite,jts:jte )             ::              kpblold
!
   logical, dimension( its:ite,jts:jte )     ::                        pblflg, &
                                                                       sfcflg, &
                                                                       stable, &
                                                                 cloudflg
!$acc declare device_resident(kpblold,pblflg,sfcflg,stable,cloudflg)   
   logical                                   ::                     definebrup                                                                 
!
   integer ::  n,i,k,l,ic,is,kk
   integer ::  klpbl, ktrace1, ktrace2, ktrace3
!
!
   real    ::  dt2,rdt,spdk2,fm,fh,hol1,gamfac,vpert,prnum,prnum0
   real    ::  ss,ri,qmean,tmean,alph,chi,zk,rl2,dk,sri
   real    ::  brint,dtodsd,dtodsu,rdz,dsdzt,dsdzq,dsdz2,rlamdz
   real    ::  utend,vtend,ttend,qtend
   real    ::  dtstep,govrthv,hgamq_
   real    ::  cont, conq, conw, conwrc
!

   real    ::                         wscalek,wscalek2
   real, dimension( ims:ime,jms:jme )              ::                           wstar
   real, dimension( ims:ime,jms:jme )              ::                           delta
   real, dimension( its:ite, kts:kte,jts:jte )     ::                     xkzml,xkzhl, &
                                                               zfacent,entfac
!$acc declare device_resident(xkzml,xkzhl,zfacent,entfac)                                                               
   real, dimension( its:ite,jts:jte )      ::                            ust3, &
                                                                       wstar3, &
                                                                     wstar3_2, &
                                                                  hgamu,hgamv, &
                                                                       we, &
                                                                       bfxpbl, &
                                                                hfxpbl,qfxpbl, &
                                                                ufxpbl,vfxpbl!, &
                                                                     !   dthvx
!$acc declare device_resident(ust3,wstar3,wstar3_2,hgamu,hgamv,we, &
!$acc      bfxpbl,hfxpbl,qfxpbl,ufxpbl,vfxpbl )   
   real    ::  prnumfac,bfx0,hfx0,qfx0,delb,dux,dvx,                           &
               dsdzu,dsdzv,wm3,dthx,dqx,wspd1,wspd10,ross,tem1,dsig,tvcon,conpr,     &
               prfac,prfac2,phim8z,wm2,dthvx,radsum,tmp1,templ,rvls,temps,ent_eff, &
                              rcldb,bruptmp,radflux
   integer :: j               
!
!-------------------------------------------------------------------------------
!



!$acc data present(dz8w2d) present(pi2d) &
!$acc      present(qx) &
!$acc      present(utnp,vtnp,ttnp,qtnp,rthraten) &
!$acc      present(p2di) &
!$acc      present(p2d)  &
!$acc      present(ust,hpbl,znt) &
!$acc      present(xland,hfx,qfx) &
!$acc      present(wspd,p2diorg) & 
!$acc      present(br,psim,psih) &
!$acc      present(psfcpa) &
!$acc      present(ux,vx,tx) &
!$acc      present(ctopo,ctopo2) &
!$acc      present(regime,delta,wstar,kpbl) &
!$acc      present(exch_hx) &
!$acc      present(u10,v10) &
!$acc      present(uox,vox) !&
!!$acc      create(zq,thx,thvx, del, dza, xkzo, za,thlix ) & 
!!$acc      create(rhox, govrth, zl1,thermal, wscale, hgamt, brdn,brup) &
!!$acc      create(phim,phih,cloudflg,kpblold,thermalli,rhox2,prpbl) & 
!!$acc      create(xkzm,xkzh, f1,f2, r1,r2, ad,au, cu, al, xkzq) & 
!!$acc      create(brcr, sflux, zol1, brcr_sbro, r3,f3,  pblflg, sfcflg, stable) &
!!$acc      create(xkzml,xkzhl, zfacent,entfac, ust3) & 
!!$acc      create(wstar3,wstar3_2, hgamu,hgamv, we, bfxpbl, hfxpbl,qfxpbl, ufxpbl,vfxpbl) 
!-----------------------------------------------------------------------------------------------
!present


   klpbl = kte
!
   cont=cp/g
   conq=xlv/g
   conw=1./g
   conwrc = conw*sqrt(rcl)
   conpr = bfac*karman*sfcfrac
!
!  k-start index for tracer diffusion
!
   ktrace1 = 0
   ktrace2 = 0 + kte
   ktrace3 = 0 + kte*2
   
!
!$acc kernels
   do j = jts,jte
   do k = kts,kte
     do i = its,ite
       thx(i,k,j) = tx(i,k,j)/pi2d(i,k,j)
       thlix(i,k,j) = (tx(i,k,j)-xlv*qx(i,ktrace2+k,j)/cp-2.834E6*qx(i,ktrace3+k,j)/cp)/pi2d(i,k,j)
#ifndef _OPENACC       
     enddo
   enddo
!
   do k = kts,kte
     do i = its,ite
#endif     
       tvcon = (1.+ep1*qx(i,k,j))
       thvx(i,k,j) = thx(i,k,j)*tvcon
     enddo
   enddo
!   
   do i = its,ite
     tvcon = (1.+ep1*qx(i,1,j))
     rhox(i,j) = psfcpa(i,j)/(rd*tx(i,1,j)*tvcon)
     govrth(i,j) = g/thx(i,1,j)
#ifndef _OPENACC   
   enddo
!
!-----compute the height of full- and half-sigma levels above ground
!     level, and the layer thicknesses.
!
   do i = its,ite
#endif   
     zq(i,1,j) = 0.
   enddo
   enddo
!
   do j = jts,jte
   do k = kts,kte
     do i = its,ite
       zq(i,k+1,j) = dz8w2d(i,k,j)+zq(i,k,j)
       tvcon = (1.+ep1*qx(i,k,j))
       rhox2(i,k,j) = p2d(i,k,j)/(rd*tx(i,k,j)*tvcon)
     enddo
   enddo
   enddo
!
   do j = jts,jte
   do k = kts,kte
     do i = its,ite
       za(i,k,j) = 0.5*(zq(i,k,j)+zq(i,k+1,j))
!       dzq(i,k,j) = zq(i,k+1)-zq(i,k)
       del(i,k,j) = p2di(i,k,j)-p2di(i,k+1,j)
     enddo
   enddo
!
   do i = its,ite
     dza(i,1,j) = za(i,1,j)
   enddo
!
   do k = kts+1,kte
     do i = its,ite
       dza(i,k,j) = za(i,k,j)-za(i,k-1,j)
     enddo
   enddo

!-----initialize vertical tendencies and
!
   utnp(its:ite,kms:kme,j) = 0.
   vtnp(its:ite,kms:kme,j) = 0.
   ttnp(its:ite,kms:kme,j) = 0.
   qtnp(its:ite,kts:kte*ndiff,j) = 0.

   
!
!   do i = its,ite
     ! wspd1(i,j) = sqrt( (ux(i,1,j)-uox(i,j))*(ux(i,1,j)-uox(i,j)) + (vx(i,1,j)-vox(i,j))*(vx(i,1,j)-vox(i,j)) )+1.e-9
!   enddo
   enddo
!$acc end kernels
!
!---- compute vertical diffusion
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     compute preliminary variables
!
   dtstep = dt
   dt2 = 2.*dtstep
   rdt = 1./dt2
!
!$acc kernels   
   do j = jts,jte
   do i = its,ite
     bfxpbl(i,j) = 0.0
     hfxpbl(i,j) = 0.0
     qfxpbl(i,j) = 0.0
     ufxpbl(i,j) = 0.0
     vfxpbl(i,j) = 0.0
     hgamu(i,j)  = 0.0
     hgamv(i,j)  = 0.0
     delta(i,j)  = 0.0
     wstar3_2(i,j) =  0.0
   enddo
   enddo
!$acc end kernels
!
!$acc kernels   
!   do j = jts,jte
!   do k = kts,klpbl
!     do i = its,ite
!       wscalek(i,k,j) = 0.0
!       wscalek2(i,k,j) = 0.0
!     enddo
!   enddo
!   enddo
!
   do j = jts,jte
!   do k = kts,klpbl
!     do i = its,ite
!       zfac(i,k) = 0.0
!     enddo
!   enddo
   do k = kts,klpbl-1
     do i = its,ite
       xkzo(i,k,j) = ckz*dza(i,k+1,j)
     enddo
   enddo
   enddo
!$acc end kernels   

!$acc kernels
!
!   do i = its,ite
!     dusfc(i,j) = 0.
!     dvsfc(i,j) = 0.
!     dtsfc(i,j) = 0.
!     dqsfc(i,j) = 0.
!   enddo
!
   do j = jts,jte
   do i = its,ite
     hgamt(i,j)  = 0.
     wscale(i,j) = 0.
     kpbl(i,j)   = 1
     hpbl(i,j)   = zq(i,1,j)
     zl1(i,j)    = za(i,1,j)
     thermal(i,j)= thvx(i,1,j)
     thermalli(i,j) = thlix(i,1,j)
     pblflg(i,j) = .true.
     sfcflg(i,j) = .true.
     sflux(i,j) = hfx(i,j)/rhox(i,j)/cp + qfx(i,j)/rhox(i,j)*ep1*thx(i,1,j)
     if(br(i,j).gt.0.0) sfcflg(i,j) = .false.
   enddo
   enddo
!
!     compute the first guess of pbl height
!

   do j = jts,jte
   do i = its,ite
     stable(i,j) = .false.
     brup(i,j) = br(i,j)
     brcr(i,j) = brcr_ub
   enddo
   enddo
!$acc end kernels

!$acc kernels
   do j = jts,jte
   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i,j))then
         brdn(i,j) = brup(i,j)
         spdk2   = max(ux(i,k,j)**2+vx(i,k,j)**2,1.)
         brup(i,j) = (thvx(i,k,j)-thermal(i,j))*(g*za(i,k,j)/thvx(i,1,j))/spdk2
         kpbl(i,j) = k
         stable(i,j) = brup(i,j).gt.brcr(i,j)
       endif
     enddo
   enddo
!
   do i = its,ite
     k = kpbl(i,j)
     if(brdn(i,j).ge.brcr(i,j))then
       brint = 0.
     elseif(brup(i,j).le.brcr(i,j))then
       brint = 1.
     else
       brint = (brcr(i,j)-brdn(i,j))/(brup(i,j)-brdn(i,j))
     endif
     hpbl(i,j) = za(i,k-1,j)+brint*(za(i,k,j)-za(i,k-1,j))
     if(hpbl(i,j).lt.zq(i,2,j)) kpbl(i,j) = 1
     if(kpbl(i,j).le.1) pblflg(i,j) = .false.
   enddo
   enddo
!$acc end kernels   

!
!$acc kernels
   do j = jts,jte
   do i = its,ite
     fm = psim(i,j)
     fh = psih(i,j)
     zol1(i,j) = max(br(i,j)*fm*fm/fh,rimin)
     if(sfcflg(i,j))then
       zol1(i,j) = min(zol1(i,j),-zfmin)
     else
       zol1(i,j) = max(zol1(i,j),zfmin)
     endif
     hol1 = zol1(i,j)*hpbl(i,j)/zl1(i,j)*sfcfrac
     if(sfcflg(i,j))then
       phim(i,j) = (1.-aphi16*hol1)**(-1./4.)
       phih(i,j) = (1.-aphi16*hol1)**(-1./2.)
       bfx0 = max(sflux(i,j),0.)
       hfx0 = max(hfx(i,j)/rhox(i,j)/cp,0.)
       qfx0 = max(ep1*thx(i,1,j)*qfx(i,j)/rhox(i,j),0.)
       wstar3(i,j) = (govrth(i,j)*bfx0*hpbl(i,j))
       wstar(i,j) = (wstar3(i,j))**h1
     else
       phim(i,j) = (1.+aphi5*hol1)
       phih(i,j) = phim(i,j)
       wstar(i,j)  = 0.
       wstar3(i,j) = 0.
     endif
     ust3(i,j)   = ust(i,j)**3.
     wscale(i,j) = (ust3(i,j)+phifac*karman*wstar3(i,j)*0.5)**h1
     wscale(i,j) = min(wscale(i,j),ust(i,j)*aphi16)
     wscale(i,j) = max(wscale(i,j),ust(i,j)/aphi5)
   enddo
   enddo

  
!
!     compute the surface variables for pbl height estimation
!     under unstable conditions
!

   do j = jts,jte
   do i = its,ite
     if(sfcflg(i,j).and.sflux(i,j).gt.0.0)then
       gamfac   = bfac/rhox(i,j)/wscale(i,j)
       hgamt(i,j) = min(gamfac*hfx(i,j)/cp,gamcrt)
       hgamq_ = min(gamfac*qfx(i,j),gamcrq)
       vpert = (hgamt(i,j)+ep1*thx(i,1,j)*hgamq_)/bfac*afac
       thermal(i,j) = thermal(i,j)+max(vpert,0.)*min(za(i,1,j)/(sfcfrac*hpbl(i,j)),1.0)
       thermalli(i,j)= thermalli(i,j)+max(vpert,0.)*min(za(i,1,j)/(sfcfrac*hpbl(i,j)),1.0)
       hgamt(i,j) = max(hgamt(i,j),0.0)
!       hgamq(i,j) = max(hgamq(i,j),0.0)
       brint    = -15.9*ust(i,j)*ust(i,j)/wspd(i,j)*wstar3(i,j)/(wscale(i,j)**4.)
       hgamu(i,j) = brint*ux(i,1,j)
       hgamv(i,j) = brint*vx(i,1,j)
     else
       pblflg(i,j) = .false.
     endif
   enddo
   enddo
!$acc end kernels

!
!     enhance the pbl height by considering the thermal
!

!$acc kernels
   do j = jts,jte
   do i = its,ite
     if(pblflg(i,j))then
       kpbl(i,j) = 1
       hpbl(i,j) = zq(i,1,j)
       stable(i,j) = .false.
       brup(i,j) = br(i,j)
       brcr(i,j) = brcr_ub
     endif
   enddo
   enddo
!$acc end kernels   
!

!$acc kernels
   do j = jts,jte
   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i,j).and.pblflg(i,j))then
         brdn(i,j) = brup(i,j)
         spdk2   = max(ux(i,k,j)**2+vx(i,k,j)**2,1.)
         brup(i,j) = (thvx(i,k,j)-thermal(i,j))*(g*za(i,k,j)/thvx(i,1,j))/spdk2
         kpbl(i,j) = k
         stable(i,j) = brup(i,j).gt.brcr(i,j)
       endif
     enddo
   enddo
   enddo
!$acc end kernels
!
!     enhance pbl by theta-li
!
   if (ysu_topdown_pblmix.eq.1)then
!$acc kernels
     do j = jts,jte
     do i = its,ite
        kpblold(i,j) = kpbl(i,j)
        definebrup=.false.
!$acc loop seq
        do k = kpblold(i,j), kte-1
           spdk2   = max(ux(i,k,j)**2+vx(i,k,j)**2,1.)
           bruptmp = (thlix(i,k,j)-thermalli(i,j))*(g*za(i,k,j)/thlix(i,1,j))/spdk2
           stable(i,j) = bruptmp.ge.brcr(i,j)
           if (definebrup) then
           kpbl(i,j) = k
           brup(i,j) = bruptmp
           definebrup=.false.
           endif
           if (.not.stable(i,j)) then !overwrite brup brdn values
           brdn(i,j)=bruptmp
           definebrup=.true.
           pblflg(i,j)=.true.
           endif
        enddo
     enddo
     enddo
!$acc end kernels
   endif


!$acc kernels
   do j = jts,jte
   do i = its,ite
     if(pblflg(i,j)) then
       k = kpbl(i,j)
       if(brdn(i,j).ge.brcr(i,j))then
         brint = 0.
       elseif(brup(i,j).le.brcr(i,j))then
         brint = 1.
       else
         brint = (brcr(i,j)-brdn(i,j))/(brup(i,j)-brdn(i,j))
       endif
       hpbl(i,j) = za(i,k-1,j)+brint*(za(i,k,j)-za(i,k-1,j))
       if(hpbl(i,j).lt.zq(i,2,j)) kpbl(i,j) = 1
       if(kpbl(i,j).le.1) pblflg(i,j) = .false.
     endif
   enddo
   enddo
!$acc end kernels   
!
!     stable boundary layer
!
!$acc kernels
   do j = jts,jte
   do i = its,ite
     if((.not.sfcflg(i,j)).and.hpbl(i,j).lt.zq(i,2,j)) then
       brup(i,j) = br(i,j)
       stable(i,j) = .false.
     else
       stable(i,j) = .true.
     endif
   enddo
   enddo
!$acc end kernels
!
!$acc kernels
   do j = jts,jte
   do i = its,ite
     if((.not.stable(i,j)).and.((xland(i,j)-1.5).ge.0))then
       wspd10 = u10(i,j)*u10(i,j) + v10(i,j)*v10(i,j)
       wspd10 = sqrt(wspd10)
       ross = wspd10 / (cori*znt(i,j))
       brcr_sbro(i,j) = min(0.16*(1.e-7*ross)**(-0.18),.3)
     endif
   enddo
!
   do i = its,ite
     if(.not.stable(i,j))then
       if((xland(i,j)-1.5).ge.0)then
         brcr(i,j) = brcr_sbro(i,j)
       else
         brcr(i,j) = brcr_sb
       endif
     endif
   enddo
   enddo
!$acc end kernels
!
!$acc kernels
   do j = jts,jte
   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i,j))then
         brdn(i,j) = brup(i,j)
         spdk2   = max(ux(i,k,j)**2+vx(i,k,j)**2,1.)
         brup(i,j) = (thvx(i,k,j)-thermal(i,j))*(g*za(i,k,j)/thvx(i,1,j))/spdk2
         kpbl(i,j) = k
         stable(i,j) = brup(i,j).gt.brcr(i,j)
       endif
     enddo
   enddo
   enddo
!$acc end kernels
!

!$acc kernels
   do j = jts,jte
   do i = its,ite
     if((.not.sfcflg(i,j)).and.hpbl(i,j).lt.zq(i,2,j)) then
       k = kpbl(i,j)
       if(brdn(i,j).ge.brcr(i,j))then
         brint = 0.
       elseif(brup(i,j).le.brcr(i,j))then
         brint = 1.
       else
         brint = (brcr(i,j)-brdn(i,j))/(brup(i,j)-brdn(i,j))
       endif
       hpbl(i,j) = za(i,k-1,j)+brint*(za(i,k,j)-za(i,k-1,j))
       if(hpbl(i,j).lt.zq(i,2,j)) kpbl(i,j) = 1
       if(kpbl(i,j).le.1) pblflg(i,j) = .false.
     endif
   enddo
   enddo
!$acc end kernels
   
!
!     estimate the entrainment parameters
!

!$acc kernels
   do j = jts,jte
   do i = its,ite
     cloudflg(i,j)=.false.
     if(pblflg(i,j)) then
       k = kpbl(i,j) - 1
!       prpbl(i,j) = 1.0
       wm3       = wstar3(i,j) + 5. * ust3(i,j)
       wm2    = wm3**h2
       bfxpbl(i,j) = -0.15*thvx(i,1,j)/g*wm3/hpbl(i,j)
       dthvx  = max(thvx(i,k+1,j)-thvx(i,k,j),tmin)
       dthx  = max(thx(i,k+1,j)-thx(i,k,j),tmin)
!---------------------
       we(i,j) = max(bfxpbl(i,j)/dthvx,-sqrt(wm2))
       if((qx(i,ktrace2+k,j)+qx(i,ktrace3+k,j)).gt.0.01e-3.and.ysu_topdown_pblmix.eq.1)then
           if ( kpbl(i,j) .ge. 2) then
                cloudflg(i,j)=.true. 
                templ=thlix(i,k,j)*(p2di(i,k+1,j)/100000)**rovcp
                !rvls is ws at full level
                rvls=100.*6.112*EXP(17.67*(templ-273.16)/(templ-29.65))*(ep2/p2di(i,k+1,j))
                temps=templ + ((qx(i,k,j)+qx(i,ktrace2+k,j))-rvls)/(cp/xlv  + &
                ep2*xlv*rvls/(rd*templ**2))
                rvls=100.*6.112*EXP(17.67*(temps-273.15)/(temps-29.65))*(ep2/p2di(i,k+1,j))
                rcldb=max((qx(i,k,j)+qx(i,ktrace2+k,j))-rvls,0.)
                !entrainment efficiency
                dthvx  = (thlix(i,k+2,j)+thx(i,k+2,j)*ep1*(qx(i,k+2,j)+qx(i,ktrace2+k+2,j))) &
                          - (thlix(i,k,j) + thx(i,k,j)  *ep1*(qx(i,k,j) +qx(i,ktrace2+k,j)))
                dthvx  = max(dthvx,0.1)
                tmp1      = xlv/cp * rcldb/(pi2d(i,k,j)*dthvx)
                ent_eff   = 0.2 * 8. * tmp1 +0.2

                radsum=0.
                do kk = 1,kpbl(i,j)-1
                   radflux=rthraten(i,kk,j)*pi2d(i,kk,j) !converts theta/s to temp/s
                   radflux=radflux*cp/g*(p2diORG(i,kk,j)-p2diORG(i,kk+1,j)) !  converts temp/s to W/m^2
                   if (radflux < 0.0 ) radsum=abs(radflux)+radsum
                enddo
                radsum=max(radsum,0.0)

                !recompute entrainment from sfc thermals
!                bfx0 = max(max(sflux(i,j),0.0)-radsum/rhox2(i,k,j)/cp,0.)
                bfx0 = max(sflux(i,j),0.0)
                wm3 = (govrth(i,j)*bfx0*hpbl(i,j))+5. * ust3(i,j)
                wm2    = wm3**h2
                bfxpbl(i,j) = -0.15*thvx(i,1,j)/g*wm3/hpbl(i,j)
                dthvx  = max(thvx(i,k+1,j)-thvx(i,k,j),tmin)
                we(i,j) = max(bfxpbl(i,j)/dthvx,-sqrt(wm2))

                !entrainment from PBL top thermals
!                bfx0 = max(radsum/rhox2(i,k,j)/cp-max(sflux(i,j),0.0),0.)
                bfx0 = max(radsum/rhox2(i,k,j)/cp,0.)
                wm3       = (g/thvx(i,k,j)*bfx0*hpbl(i,j)) ! this is wstar3(i)
                wm2    = wm2+wm3**h2
                bfxpbl(i,j) = - ent_eff * bfx0
                dthvx  = max(thvx(i,k+1,j)-thvx(i,k,j),0.1)
                we(i,j) = we(i,j) + max(bfxpbl(i,j)/dthvx,-sqrt(wm3**h2))

                !wstar3_2
                bfx0 = max(radsum/rhox2(i,k,j)/cp,0.)
                wstar3_2(i,j) =  (g/thvx(i,k,j)*bfx0*hpbl(i,j))
                !recompute hgamt 
                wscale(i,j) = (ust3(i,j)+phifac*karman*(wstar3(i,j)+wstar3_2(i,j))*0.5)**h1
                wscale(i,j) = min(wscale(i,j),ust(i,j)*aphi16)
                wscale(i,j) = max(wscale(i,j),ust(i,j)/aphi5)
                gamfac   = bfac/rhox(i,j)/wscale(i,j)
                hgamt(i,j) = min(gamfac*hfx(i,j)/cp,gamcrt)
!                hgamq(i,j) = min(gamfac*qfx(i,j),gamcrq)
                gamfac   = bfac/rhox2(i,k,j)/wscale(i,j)
                hgamt2 = min(gamfac*radsum/cp,gamcrt)
                hgamt(i,j) = max(hgamt(i,j),0.0) + max(hgamt2,0.0)
                brint    = -15.9*ust(i,j)*ust(i,j)/wspd(i,j)*(wstar3(i,j)+wstar3_2(i,j))/(wscale(i,j)**4.)
                hgamu(i,j) = brint*ux(i,1,j)
                hgamv(i,j) = brint*vx(i,1,j)
           endif
       endif
       prpbl(i,j) = 1.0
!---------------------
       dqx   = min(qx(i,k+1,j)-qx(i,k,j),0.0)
!       we(i,j) = max(bfxpbl(i,j)/dthvx,-sqrt(wm2))
       hfxpbl(i,j) = we(i,j)*dthx
       qfxpbl(i,j) = we(i,j)*dqx
!
       dux = ux(i,k+1,j)-ux(i,k,j)
       dvx = vx(i,k+1,j)-vx(i,k,j)
       if(dux.gt.tmin) then
         ufxpbl(i,j) = max(prpbl(i,j)*we(i,j)*dux,-ust(i,j)*ust(i,j))
       elseif(dux.lt.-tmin) then
         ufxpbl(i,j) = min(prpbl(i,j)*we(i,j)*dux,ust(i,j)*ust(i,j))
       else
         ufxpbl(i,j) = 0.0
       endif
       if(dvx.gt.tmin) then
         vfxpbl(i,j) = max(prpbl(i,j)*we(i,j)*dvx,-ust(i,j)*ust(i,j))
       elseif(dvx.lt.-tmin) then
         vfxpbl(i,j) = min(prpbl(i,j)*we(i,j)*dvx,ust(i,j)*ust(i,j))
       else
         vfxpbl(i,j) = 0.0
       endif
       delb  = govrth(i,j)*d3*hpbl(i,j)
       delta(i,j) = min(d1*hpbl(i,j) + d2*wm2/delb,100.)
     endif
   enddo
   enddo
!$acc end kernels
!

!$acc kernels
   do j = jts,jte
   do k = kts,klpbl
     do i = its,ite
       if(pblflg(i,j).and.k.ge.kpbl(i,j))then
         entfac(i,k,j) = ((zq(i,k+1,j)-hpbl(i,j))/delta(i,j))**2.
       else
         entfac(i,k,j) = 1.e30
       endif
     enddo
   enddo
   enddo
!$acc end kernels
!
!     compute diffusion coefficients below pbl
!

!$acc kernels
   do j = jts,jte
   do k = kts,klpbl
     do i = its,ite
       if(k.lt.kpbl(i,j)) then
         zfac = min(max((1.-(zq(i,k+1,j)-zl1(i,j))/(hpbl(i,j)-zl1(i,j))),zfmin),1.)
         zfacent(i,k,j) = (1.-zfac)**3.
         wscalek = (ust3(i,j)+phifac*karman*wstar3(i,j)*(1.-zfac))**h1
         wscalek2 = (phifac*karman*wstar3_2(i,j)*(zfac))**h1
         if(sfcflg(i,j)) then
           prfac = conpr
           prfac2 = 15.9*(wstar3(i,j)+wstar3_2(i,j))/ust3(i,j)/(1.+4.*karman*(wstar3(i,j)+wstar3_2(i,j))/ust3(i,j))
           prnumfac = -3.*(max(zq(i,k+1,j)-sfcfrac*hpbl(i,j),0.))**2./hpbl(i,j)**2.
         else
           prfac = 0.
           prfac2 = 0.
           prnumfac = 0.
           phim8z = 1.+aphi5*zol1(i,j)*zq(i,k+1,j)/zl1(i,j)
           wscalek = ust(i,j)/phim8z
           wscalek = max(wscalek,0.001)
         endif
         prnum0 = (phih(i,j)/phim(i,j)+prfac)
         prnum0 = max(min(prnum0,prmax),prmin)
         xkzm(i,k,j) = wscalek*karman*zq(i,k+1,j)*zfac**pfac+ &
                       wscalek2*karman*(hpbl(i,j)-zq(i,k+1,j))*(1-zfac)**pfac
         !Do not include xkzm at kpbl-1 since it changes entrainment
         if (k.eq.kpbl(i,j)-1.and.cloudflg(i,j).and.we(i,j).lt.0.0) then
           xkzm(i,k,j) = 0.0
         endif
         prnum =  1. + (prnum0-1.)*exp(prnumfac)
         xkzq(i,k,j) = xkzm(i,k,j)/prnum*zfac**(pfac_q-pfac)
         prnum0 = prnum0/(1.+prfac2*karman*sfcfrac)
         prnum =  1. + (prnum0-1.)*exp(prnumfac)
         xkzh(i,k,j) = xkzm(i,k,j)/prnum
         xkzm(i,k,j) = min(xkzm(i,k,j),xkzmax)
         xkzm(i,k,j) = max(xkzm(i,k,j),xkzo(i,k,j))
         xkzh(i,k,j) = min(xkzh(i,k,j),xkzmax)
         xkzh(i,k,j) = max(xkzh(i,k,j),xkzo(i,k,j))
         xkzq(i,k,j) = min(xkzq(i,k,j),xkzmax)
         xkzq(i,k,j) = max(xkzq(i,k,j),xkzo(i,k,j))
       endif
     enddo
   enddo
   enddo
   
!$acc end kernels
!
!     compute diffusion coefficients over pbl (free atmosphere)
!


!$acc kernels   

   do j = jts,jte
   do k = kts,kte-1
     do i = its,ite
       if(k.ge.kpbl(i,j)) then
         ss = ((ux(i,k+1,j)-ux(i,k,j))*(ux(i,k+1,j)-ux(i,k,j))                         &
              +(vx(i,k+1,j)-vx(i,k,j))*(vx(i,k+1,j)-vx(i,k,j)))                        &
              /(dza(i,k+1,j)*dza(i,k+1,j))+1.e-9
         govrthv = g/(0.5*(thvx(i,k+1,j)+thvx(i,k,j)))
         ri = govrthv*(thvx(i,k+1,j)-thvx(i,k,j))/(ss*dza(i,k+1,j))
         if(imvdif.eq.1.and.ndiff.ge.3)then
           if((qx(i,ktrace2+k,j)+qx(i,ktrace3+k,j)).gt.0.01e-3.and.(qx(i           &
             ,ktrace2+k+1,j)+qx(i,ktrace3+k+1,j)).gt.0.01e-3)then
!      in cloud
             qmean = 0.5*(qx(i,k,j)+qx(i,k+1,j))
             tmean = 0.5*(tx(i,k,j)+tx(i,k+1,j))
             alph  = xlv*qmean/rd/tmean
             chi   = xlv*xlv*qmean/cp/rv/tmean/tmean
             ri    = (1.+alph)*(ri-g*g/ss/tmean/cp*((chi-alph)/(1.+chi)))
           endif
         endif
         zk = karman*zq(i,k+1,j)
         rlamdz = min(max(0.1*dza(i,k+1,j),rlam),300.)
         rlamdz = min(dza(i,k+1,j),rlamdz)
         rl2 = (zk*rlamdz/(rlamdz+zk))**2
         dk = rl2*sqrt(ss)
         if(ri.lt.0.)then
! unstable regime
           ri = max(ri, rimin)
           sri = sqrt(-ri)
           xkzm(i,k,j) = dk*(1+8.*(-ri)/(1+1.746*sri))
           xkzh(i,k,j) = dk*(1+8.*(-ri)/(1+1.286*sri))
         else
! stable regime
           xkzh(i,k,j) = dk/(1+5.*ri)**2
           prnum = 1.0+2.1*ri
           prnum = min(prnum,prmax)
           xkzm(i,k,j) = xkzh(i,k,j)*prnum
         endif
!
         xkzm(i,k,j) = min(xkzm(i,k,j),xkzmax)
         xkzm(i,k,j) = max(xkzm(i,k,j),xkzo(i,k,j))
         xkzh(i,k,j) = min(xkzh(i,k,j),xkzmax)
         xkzh(i,k,j) = max(xkzh(i,k,j),xkzo(i,k,j))
         xkzml(i,k,j) = xkzm(i,k,j)
         xkzhl(i,k,j) = xkzh(i,k,j)
       endif
     enddo
   enddo
   enddo
!$acc end kernels

!
!     compute tridiagonal matrix elements for heat
!
!$acc kernels   
   do j = jts,jte
   do k = kts,kte
     do i = its,ite
       au(i,k,j) = 0.
       al(i,k,j) = 0.
       ad(i,k,j) = 0.
       f1(i,k,j) = 0.
     enddo
   enddo
!
   do i = its,ite
     ad(i,1,j) = 1.
     f1(i,1,j) = thx(i,1,j)-300.+hfx(i,j)/cont/del(i,1,j)*dt2
   enddo
!
   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k,j)
       dtodsu = dt2/del(i,k+1,j)
       dsig   = p2d(i,k,j)-p2d(i,k+1,j)
       rdz    = 1./dza(i,k+1,j)
       tem1   = dsig*xkzh(i,k,j)*rdz
       if(pblflg(i,j).and.k.lt.kpbl(i,j)) then
         dsdzt = tem1*(-hgamt(i,j)/hpbl(i,j)-hfxpbl(i,j)*zfacent(i,k,j)/xkzh(i,k,j))
         f1(i,k,j)   = f1(i,k,j)+dtodsd*dsdzt
         f1(i,k+1,j) = thx(i,k+1,j)-300.-dtodsu*dsdzt
       elseif(pblflg(i,j).and.k.ge.kpbl(i,j).and.entfac(i,k,j).lt.4.6) then
         xkzh(i,k,j) = -we(i,j)*dza(i,kpbl(i,j),j)*exp(-entfac(i,k,j))
         xkzh(i,k,j) = sqrt(xkzh(i,k,j)*xkzhl(i,k,j))
         xkzh(i,k,j) = min(xkzh(i,k,j),xkzmax)
         xkzh(i,k,j) = max(xkzh(i,k,j),xkzo(i,k,j))
         f1(i,k+1,j) = thx(i,k+1,j)-300.
       else
         f1(i,k+1,j) = thx(i,k+1,j)-300.
       endif
       tem1   = dsig*xkzh(i,k,j)*rdz
       dsdz2     = tem1*rdz
       au(i,k,j)   = -dtodsd*dsdz2
       al(i,k,j)   = -dtodsu*dsdz2
       ad(i,k,j)   = ad(i,k,j)-au(i,k,j)
       ad(i,k+1,j) = 1.-al(i,k,j)
       exch_hx(i,k+1,j) = xkzh(i,k,j)
     enddo
   enddo
   
!
! copies here to avoid duplicate input args for tridin
!
   do k = kts,kte
     do i = its,ite
       cu(i,k,j) = au(i,k,j)
       r1(i,k,j) = f1(i,k,j)
     enddo
   enddo
   enddo
!$acc end kernels
   
!
!   do j = jts,jte
!   call tridin_ysu(al(its,kts,j),ad(its,kts,j),cu(its,kts,j),r1(its,kts,j),au(its,kts,j),f1(its,kts,j),its,ite,kts,kte,1)
   call tridin_ysu(al,ad,cu,r1,au,f1,its,ite,kts,kte,1,jts,jte,jms,jme)
!   enddo
   
!$acc kernels   
   do j = jts,jte
   
!
!     recover tendencies of heat
!
   do k = kte,kts,-1
     do i = its,ite
       ttend = (f1(i,k,j)-thx(i,k,j)+300.)*rdt*pi2d(i,k,j)
       ttnp(i,k,j) = ttnp(i,k,j)+ttend
!       dtsfc(i,j) = dtsfc(i,j)+ttend*cont*del(i,k)/pi2d(i,k)
     enddo
   enddo
!
!     compute tridiagonal matrix elements for moisture, clouds, and gases
!
   do k = kts,kte
     do i = its,ite
       au(i,k,j) = 0.
       al(i,k,j) = 0.
       ad(i,k,j) = 0.
     enddo
   enddo
!
   do ic = 1,ndiff
     do k = kts,kte
       do i = its,ite
         f3(i,k,ic,j) = 0.
       enddo
     enddo
   enddo
!
   do i = its,ite
     ad(i,1,j) = 1.
     f3(i,1,1,j) = qx(i,1,j)+qfx(i,j)*g/del(i,1,j)*dt2
   enddo
!
   if(ndiff.ge.2) then
     do ic = 2,ndiff
       is = (ic-1) * kte
       do i = its,ite
         f3(i,1,ic,j) = qx(i,1+is,j)
       enddo
     enddo
   endif
!
   do k = kts,kte-1
     do i = its,ite
       if(k.ge.kpbl(i,j)) then
         xkzq(i,k,j) = xkzh(i,k,j)
       endif
     enddo
   enddo
!
   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k,j)
       dtodsu = dt2/del(i,k+1,j)
       dsig   = p2d(i,k,j)-p2d(i,k+1,j)
       rdz    = 1./dza(i,k+1,j)
       tem1   = dsig*xkzq(i,k,j)*rdz
       if(pblflg(i,j).and.k.lt.kpbl(i,j)) then
         dsdzq = tem1*(-qfxpbl(i,j)*zfacent(i,k,j)/xkzq(i,k,j))
         f3(i,k,1,j) = f3(i,k,1,j)+dtodsd*dsdzq
         f3(i,k+1,1,j) = qx(i,k+1,j)-dtodsu*dsdzq
       elseif(pblflg(i,j).and.k.ge.kpbl(i,j).and.entfac(i,k,j).lt.4.6) then
         xkzq(i,k,j) = -we(i,j)*dza(i,kpbl(i,j),j)*exp(-entfac(i,k,j))
         xkzq(i,k,j) = sqrt(xkzq(i,k,j)*xkzhl(i,k,j))
         xkzq(i,k,j) = min(xkzq(i,k,j),xkzmax)
         xkzq(i,k,j) = max(xkzq(i,k,j),xkzo(i,k,j))
         f3(i,k+1,1,j) = qx(i,k+1,j)
       else
         f3(i,k+1,1,j) = qx(i,k+1,j)
       endif
       tem1   = dsig*xkzq(i,k,j)*rdz
       dsdz2     = tem1*rdz
       au(i,k,j)   = -dtodsd*dsdz2
       al(i,k,j)   = -dtodsu*dsdz2
       ad(i,k,j)   = ad(i,k,j)-au(i,k,j)
       ad(i,k+1,j) = 1.-al(i,k,j)
!      exch_hx(i,k+1) = xkzh(i,k)
     enddo
   enddo


!
   if(ndiff.ge.2) then
     do ic = 2,ndiff
       is = (ic-1) * kte
       do k = kts,kte-1
         do i = its,ite
           f3(i,k+1,ic,j) = qx(i,k+1+is,j)
         enddo
       enddo
     enddo
   endif
!
! copies here to avoid duplicate input args for tridin
!
   do k = kts,kte
     do i = its,ite
       cu(i,k,j) = au(i,k,j)
     enddo
   enddo
!
   do ic = 1,ndiff
     do k = kts,kte
       do i = its,ite
         r3(i,k,ic,j) = f3(i,k,ic,j)
       enddo
     enddo
   enddo
   enddo
!$acc end kernels
!
!     solve tridiagonal problem for moisture, clouds, and gases
!
!   do j = jts,jte
   call tridin_ysu(al,ad,cu,r3,au,f3,its,ite,kts,kte,ndiff,jts,jte,jms,jme)
!   call tridin_ysu(al(its,kts,j),ad(its,kts,j),cu(its,kts,j),r3(its,kts,1,j),au(its,kts,j),f3(its,kts,1,j),its,ite,kts,kte,ndiff)
!   enddo
!$acc kernels   
   
   do j = jts,jte
!
!     recover tendencies of heat and moisture
!
   do k = kte,kts,-1
     do i = its,ite
       qtend = (f3(i,k,1,j)-qx(i,k,j))*rdt
       qtnp(i,k,j) = qtnp(i,k,j)+qtend
!       dqsfc(i,j) = dqsfc(i,j)+qtend*conq*del(i,k)
     enddo
   enddo
!
   if(ndiff.ge.2) then
     do ic = 2,ndiff
       is = (ic-1) * kte
       do k = kte,kts,-1
         do i = its,ite
           qtend = (f3(i,k,ic,j)-qx(i,k+is,j))*rdt
           qtnp(i,k+is,j) = qtnp(i,k+is,j)+qtend
         enddo
       enddo
     enddo
   endif
!
!     compute tridiagonal matrix elements for momentum
!
   do i = its,ite
     do k = kts,kte
       au(i,k,j) = 0.
       al(i,k,j) = 0.
       ad(i,k,j) = 0.
       f1(i,k,j) = 0.
       f2(i,k,j) = 0.
     enddo
   enddo
!
   do i = its,ite
! paj: ctopo=1 if topo_wind=0 (default)
! mchen add this line to make sure NMM can still work with YSU PBL
     wspd1 = sqrt( (ux(i,1,j)-uox(i,j))*(ux(i,1,j)-uox(i,j)) + (vx(i,1,j)-vox(i,j))*(vx(i,1,j)-vox(i,j)) )+1.e-9
     if(present(ctopo)) then
       ad(i,1,j) = 1.+ctopo(i,j)*ust(i,j)**2/wspd1*rhox(i,j)*g/del(i,1,j)*dt2         &
        *(wspd1/wspd(i,j))**2
     else
       ad(i,1,j) = 1.+ust(i,j)**2/wspd1*rhox(i,j)*g/del(i,1,j)*dt2                  &
        *(wspd1/wspd(i,j))**2
     endif
     f1(i,1,j) = ux(i,1,j)+uox(i,j)*ust(i,j)**2*g/del(i,1,j)*dt2/wspd1
     f2(i,1,j) = vx(i,1,j)+vox(i,j)*ust(i,j)**2*g/del(i,1,j)*dt2/wspd1
   enddo
!

   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k,j)
       dtodsu = dt2/del(i,k+1,j)
       dsig   = p2d(i,k,j)-p2d(i,k+1,j)
       rdz    = 1./dza(i,k+1,j)
       tem1   = dsig*xkzm(i,k,j)*rdz
       !print*,'flags ',pblflg(i),k,kpbl(i),(pblflg(i).and.k.lt.kpbl(i))
       if(pblflg(i,j).and.k.lt.kpbl(i,j))then
         !print*,'tem1,hgamu(i),hpbl(i),ufxpbl(i),zfacent(i,k),xkzm(i,k) ',&
         !         tem1,hgamu(i),hpbl(i),ufxpbl(i),zfacent(i,k),xkzm(i,k)
         dsdzu     = tem1*(-hgamu(i,j)/hpbl(i,j)-ufxpbl(i,j)*zfacent(i,k,j)/xkzm(i,k,j))
         dsdzv     = tem1*(-hgamv(i,j)/hpbl(i,j)-vfxpbl(i,j)*zfacent(i,k,j)/xkzm(i,k,j))
         f1(i,k,j)   = f1(i,k,j)+dtodsd*dsdzu
         !print*,i,k+1,'f1(i,k+1),dtodsd,dsdzu ',f1(i,k+1),dtodsd,dsdzu
         f1(i,k+1,j) = ux(i,k+1,j)-dtodsu*dsdzu
         f2(i,k,j)   = f2(i,k,j)+dtodsd*dsdzv
         f2(i,k+1,j) = vx(i,k+1,j)-dtodsu*dsdzv
       elseif(pblflg(i,j).and.k.ge.kpbl(i,j).and.entfac(i,k,j).lt.4.6) then
         xkzm(i,k,j) = prpbl(i,j)*xkzh(i,k,j)
         xkzm(i,k,j) = sqrt(xkzm(i,k,j)*xkzml(i,k,j))
         xkzm(i,k,j) = min(xkzm(i,k,j),xkzmax)
         xkzm(i,k,j) = max(xkzm(i,k,j),xkzo(i,k,j))
         f1(i,k+1,j) = ux(i,k+1,j)
         f2(i,k+1,j) = vx(i,k+1,j)
         !print*,'elseif ',i,k+1,f1(i,k+1),f2(i,k+1)
       else
         f1(i,k+1,j) = ux(i,k+1,j)
         f2(i,k+1,j) = vx(i,k+1,j)
         !print*,'else ',i,k+1,f1(i,k+1),f2(i,k+1)
       endif
       tem1   = dsig*xkzm(i,k,j)*rdz
       dsdz2     = tem1*rdz
       au(i,k,j)   = -dtodsd*dsdz2
       al(i,k,j)   = -dtodsu*dsdz2
       ad(i,k,j)   = ad(i,k,j)-au(i,k,j)
       ad(i,k+1,j) = 1.-al(i,k,j)
       !print*,i,k,f1(i,k+1),f2(i,k+1)
     enddo
   enddo
   
   
!
! copies here to avoid duplicate input args for tridin
!
   do k = kts,kte
     do i = its,ite
       cu(i,k,j) = au(i,k,j)
       r1(i,k,j) = f1(i,k,j)
       r2(i,k,j) = f2(i,k,j)
     enddo
   enddo
   enddo
!$acc end kernels
   
!
!     solve tridiagonal problem for momentum
!
!   do j = jts,jte
!   call tridi1n(al(its,kts,j),ad(its,kts,j),cu(its,kts,j),r1(its,kts,j),r2(its,kts,j), &
!      au(its,kts,j),f1(its,kts,j),f2(its,kts,j),its,ite,kts,kte,1)
   call tridi1n(al,ad,cu,r1,r2,au,f1,f2,its,ite,kts,kte,1,jts,jte,jms,jme)
!   enddo
!
!     recover tendencies of momentum
!
!
!$acc kernels


   do j = jts,jte
   do k = kte,kts,-1
     do i = its,ite
       utend = (f1(i,k,j)-ux(i,k,j))*rdt
       vtend = (f2(i,k,j)-vx(i,k,j))*rdt
       utnp(i,k,j) = utnp(i,k,j)+utend
       vtnp(i,k,j) = vtnp(i,k,j)+vtend
!       dusfc(i,j) = dusfc(i,j) + utend*conwrc*del(i,k)
!       dvsfc(i,j) = dvsfc(i,j) + vtend*conwrc*del(i,k)
     enddo
   enddo
   end do ! j loop
!$acc end kernels

!
! paj: ctopo2=1 if topo_wind=0 (default)
!
   if(present(ctopo).and.present(ctopo2)) then ! mchen for NMM

!$acc kernels 
   do j = jts,jte
   do i = its,ite
       u10(i,j) = ctopo2(i,j)*u10(i,j)+(1-ctopo2(i,j))*ux(i,1,j)
       v10(i,j) = ctopo2(i,j)*v10(i,j)+(1-ctopo2(i,j))*vx(i,1,j)
   enddo
   enddo
!$acc end kernels
   endif !mchen
!
!---- end of vertical diffusion
!
!!$acc kernels
!   do i = its,ite
!     kpbl1d(i,j) = kpbl(i,j)
!   enddo
!!$acc end kernels
!$acc end data   
   
!
!
   end subroutine ysu2d
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine tridi1n(cl,cm,cu,r1,r2,au,f1,f2,its,ite,kts,kte,nt,jts,jte,jms,jme)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   integer, intent(in )      ::     its,ite, kts,kte, nt, jts,jte,jms,jme
!
   real, dimension( its:ite, kts+1:kte+1, jms:jme )                                   , &
         intent(in   )  ::                                                 cl
!
   real, dimension( its:ite, kts:kte, jms:jme )                                       , &
         intent(in   )  ::                                                 cm, &
                                                                           r1
   real, dimension( its:ite, kts:kte,nt,jms:jme )                                    , &
         intent(in   )  ::                                                 r2
!
   real, dimension( its:ite, kts:kte, jms:jme )                                       , &
         intent(inout)  ::                                                 au, &
                                                                           cu, &
                                                                           f1
   real, dimension( its:ite, kts:kte,nt, jms:jme )                                    , &
         intent(inout)  ::                                                 f2
!
   real    :: fk
   integer :: i,k,l,n,it,j
!
!-------------------------------------------------------------------------------
!
!$acc data present(cl,cm,r1,r2) &
!$acc      present(au,cu,f1,f2)

!$acc kernels

   l = ite
   n = kte
!
   do j = jts,jte
   do i = its,l
     fk = 1./cm(i,1,j)
     au(i,1,j) = fk*cu(i,1,j)
     f1(i,1,j) = fk*r1(i,1,j)
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./cm(i,1,j)
       f2(i,1,it,j) = fk*r2(i,1,it,j)
     enddo
   enddo
!
   do k = kts+1,n-1
     do i = its,l
       fk = 1./(cm(i,k,j)-cl(i,k,j)*au(i,k-1,j))
       au(i,k,j) = fk*cu(i,k,j)
       f1(i,k,j) = fk*(r1(i,k,j)-cl(i,k,j)*f1(i,k-1,j))
     enddo
   enddo
!
   do it = 1,nt
     do k = kts+1,n-1
       do i = its,l
         fk = 1./(cm(i,k,j)-cl(i,k,j)*au(i,k-1,j))
         f2(i,k,it,j) = fk*(r2(i,k,it,j)-cl(i,k,j)*f2(i,k-1,it,j))
       enddo
     enddo
   enddo
!
   do i = its,l
     fk = 1./(cm(i,n,j)-cl(i,n,j)*au(i,n-1,j))
     f1(i,n,j) = fk*(r1(i,n,j)-cl(i,n,j)*f1(i,n-1,j))
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./(cm(i,n,j)-cl(i,n,j)*au(i,n-1,j))
       f2(i,n,it,j) = fk*(r2(i,n,it,j)-cl(i,n,j)*f2(i,n-1,it,j))
     enddo
   enddo
!
   do k = n-1,kts,-1
     do i = its,l
       f1(i,k,j) = f1(i,k,j)-au(i,k,j)*f1(i,k+1,j)
     enddo
   enddo
!
   do it = 1,nt
     do k = n-1,kts,-1
       do i = its,l
         f2(i,k,it,j) = f2(i,k,it,j)-au(i,k,j)*f2(i,k+1,it,j)
       enddo
     enddo
   enddo
   enddo
!$acc end kernels
!$acc end data   
!
   end subroutine tridi1n
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine tridin_ysu(cl,cm,cu,r2,au,f2,its,ite,kts,kte,nt,jts,jte,jms,jme)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   integer, intent(in )      ::     its,ite, kts,kte, nt,jts,jte,jms,jme
!
   real, dimension( its:ite, kts+1:kte+1, jms:jme )                                   , &
         intent(in   )  ::                                                 cl
!
   real, dimension( its:ite, kts:kte, jms:jme )                                       , &
         intent(in   )  ::                                                 cm
   real, dimension( its:ite, kts:kte,nt, jms:jme )                                    , &
         intent(in   )  ::                                                 r2
!
   real, dimension( its:ite, kts:kte, jms:jme )                                       , &
         intent(inout)  ::                                                 au, &
                                                                           cu
   real, dimension( its:ite, kts:kte,nt, jms:jme )                                    , &
         intent(inout)  ::                                                 f2
!
   real    :: fk
   integer :: i,k,l,n,it,j
!
!-------------------------------------------------------------------------------


!$acc data present(au,cu,f2, cl,cm,r2) 

!$acc kernels
!
   l = ite
   n = kte
!
   do j = jts,jte
   do it = 1,nt
     do i = its,l
       fk = 1./cm(i,1,j)
       au(i,1,j) = fk*cu(i,1,j)
       f2(i,1,it,j) = fk*r2(i,1,it,j)
     enddo
   enddo
!
   do it = 1,nt
     do k = kts+1,n-1
       do i = its,l
         fk = 1./(cm(i,k,j)-cl(i,k,j)*au(i,k-1,j))
         au(i,k,j) = fk*cu(i,k,j)
         f2(i,k,it,j) = fk*(r2(i,k,it,j)-cl(i,k,j)*f2(i,k-1,it,j))
       enddo
     enddo
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./(cm(i,n,j)-cl(i,n,j)*au(i,n-1,j))
       f2(i,n,it,j) = fk*(r2(i,n,it,j)-cl(i,n,j)*f2(i,n-1,it,j))
     enddo
   enddo
!
   do it = 1,nt
     do k = n-1,kts,-1
       do i = its,l
         f2(i,k,it,j) = f2(i,k,it,j)-au(i,k,j)*f2(i,k+1,it,j)
       enddo
     enddo
   enddo
   enddo 
!$acc end kernels
!$acc end data   
!
   end subroutine tridin_ysu
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine ysuinit(rublten,rvblten,rthblten,rqvblten,                       &
                      rqcblten,rqiblten,p_qi,p_first_scalar,                   &
                      restart, allowed_to_read,                                &
                      ids, ide, jds, jde, kds, kde,                            &
                      ims, ime, jms, jme, kms, kme,                            &
                      its, ite, jts, jte, kts, kte                 )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   logical , intent(in)          :: restart, allowed_to_read
   integer , intent(in)          ::  ids, ide, jds, jde, kds, kde,             &
                                     ims, ime, jms, jme, kms, kme,             &
                                     its, ite, jts, jte, kts, kte
   integer , intent(in)          ::  p_qi,p_first_scalar
   real , dimension( ims:ime , kms:kme , jms:jme ), intent(out) ::             &
                                                                      rublten, &
                                                                      rvblten, &
                                                                     rthblten, &
                                                                     rqvblten, &
                                                                     rqcblten, &
                                                                     rqiblten
   integer :: i, j, k, itf, jtf, ktf
!
   jtf = min0(jte,jde-1)
   ktf = min0(kte,kde-1)
   itf = min0(ite,ide-1)
!
   if(.not.restart)then
     do j = jts,jtf
       do k = kts,ktf
         do i = its,itf
            rublten(i,k,j) = 0.
            rvblten(i,k,j) = 0.
            rthblten(i,k,j) = 0.
            rqvblten(i,k,j) = 0.
            rqcblten(i,k,j) = 0.
         enddo
       enddo
     enddo
   endif
!
   if (p_qi .ge. p_first_scalar .and. .not.restart) then
     do j = jts,jtf
       do k = kts,ktf
         do i = its,itf
           rqiblten(i,k,j) = 0.
         enddo
       enddo
     enddo
   endif
!
   end subroutine ysuinit
!-------------------------------------------------------------------------------
end module module_bl_ysu
!-------------------------------------------------------------------------------
