
      program cm1

!-----------------------------------------------------------------------------
!
!  CM1 Numerical Model, Release 20.2  (cm1r20.2)
!  18 October 2020
!  https://www2.mmm.ucar.edu/people/bryan/cm1/
!
!  (c)2020 - University Corporation for Atmospheric Research 
!
!-----------------------------------------------------------------------------
!
!  Please see documentation at the top of the "solve.F" file.
!
!  See also documentation at the cm1 website, such as:
!
!    "The governing equations for CM1"
!        https://www2.mmm.ucar.edu/people/bryan/cm1/cm1_equations.pdf
!
!-----------------------------------------------------------------------------

      use input
      use constants
      use param_module
      use base_module
      use init3d_module
      use misclibs
      use solve1_module
      use solve2_module
      use solve3_module
      use mp_driver_module
      use diff2_module
      use turb_module
      use statpack_module
      use writeout_module
      use restart_module
      use radiation_module, only : radiation_driver
      use radtrns3d_module, only : nrad2d,n2d_radiat,n3d_radiat
      use domaindiag_module, only : domaindiag
      use azimavg_module
      use hifrq_module, only : writeout_hifrq
      use parcel_module
      use init_physics_module
      use init_surface_module
      use ib_module
      use eddy_recycle
      use lsnudge_module
#ifdef MPI
      use mpi
#endif
      implicit none

      integer :: nstep,nstep0
      integer :: srec,sirec,urec,vrec,wrec,nrec,mrec,prec,trecs,trecw,arecs,arecw
      integer :: nrst,nstatout,nwrite,nwritet,nwritea,nwriteh
      integer :: rbufsz,num_soil_layers,ndt
      real :: dt,dtlast
      double precision :: mtime,stattim,taptim,rsttim,radtim,prcltim,diagtim,azimavgtim,hifrqtim
      double precision :: adt,acfl,dbldt
      double precision :: mass1,mass2
      double precision :: avgsfcu,avgsfcv,avgsfcs,avgsfct
      logical :: dosfcflx
      logical, dimension(maxq) :: cloudvar,rhovar
      real, dimension(maxq) :: qmag
      character(len=3), dimension(maxq) :: qname
      character(len=20), dimension(maxq) :: qunit
      character(len=6), dimension(maxq) :: budname
      character(len=60), dimension(maxvars) :: desc_output
      character(len=40), dimension(maxvars) :: name_output,unit_output
      character(len=1),  dimension(maxvars) :: grid_output
      logical, dimension(maxvars) :: cmpr_output
      character(len=40), dimension(maxvars) :: name_stat,desc_stat,unit_stat
      character(len=40), dimension(maxvars) :: name_prcl,desc_prcl,unit_prcl
      double precision, dimension(:), allocatable :: bud,bud2
      double precision, dimension(:), allocatable :: qbudget
      double precision, dimension(:), allocatable :: asq,bsq
      real, dimension(:), allocatable :: xh,rxh,arh1,arh2,uh,ruh
      real, dimension(:), allocatable :: xf,rxf,arf1,arf2,uf,ruf
      real, dimension(:), allocatable :: yh,vh,rvh
      real, dimension(:), allocatable :: yf,vf,rvf
      real, dimension(:), allocatable :: xfref,xhref,yfref,yhref
      double precision, dimension(:), allocatable :: dumk1,dumk2
      double precision, dimension(:,:), allocatable :: dum2d1,dum2d2,dum2d3,dum2d4,dum2d5
      real, dimension(:), allocatable :: rds,sigma,rdsf,sigmaf
      real, dimension(:), allocatable :: wprof,ufrc,vfrc,thfrc,qvfrc,ug,vg,dvdr,  &
                                         uavg,vavg,savg,thavg,pavg,ulspg,vlspg
      real, dimension(:,:), allocatable :: qavg
      double precision, dimension(:,:), allocatable :: cavg
      real, dimension(:,:,:), allocatable :: tauh,taus,zh,mh,rmh,c1,c2
      real, dimension(:,:,:), allocatable :: tauf,zf,mf,rmf
      real, dimension(:), allocatable :: rstat
      real, dimension(:,:), allocatable :: rho0s,pi0s,prs0s,rth0s
      real, dimension(:,:,:), allocatable :: pi0,rho0,prs0,thv0,th0,rth0,qv0
      real, dimension(:,:,:), allocatable :: qc0,qi0,rr0,rf0,rrf0,u0,v0,thrd
      real, dimension(:,:,:), allocatable :: dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9
      real, dimension(:,:), allocatable :: zs,gz,rgz,gzu,rgzu,gzv,rgzv,dzdx,dzdy
      real, dimension(:,:,:), allocatable :: gx,gxu,gy,gyv
      real, dimension(:,:,:), allocatable :: rain,sws,svs,sps,srs,sgs,sus,shs
      real, dimension(:,:), allocatable :: tsk,znt,rznt,zntmp,ust,stau,tst,qst,z0t,z0q,thflux,qvflux,  &
                                           cd,ch,cq,u1,v1,s1,t1,xland,psfc,tlh,f2d,psmth,prate,ustt,ut,vt,st
      real, dimension(:,:), allocatable :: radbcw,radbce
      real, dimension(:,:), allocatable :: radbcs,radbcn
      real, dimension(:,:), allocatable :: dtu,dtu0,dtv,dtv0
      real, dimension(:,:,:), allocatable :: divx,rho,rr,rf,prs
      real, dimension(:,:,:), allocatable :: t11,t12,t13,t22,t23,t33
      real, dimension(:,:,:), allocatable :: m11,m12,m13,m22,m23,m33
      real, dimension(:,:,:), allocatable :: rru,ua,u3d,uten,uten1
      real, dimension(:,:,:), allocatable :: rrv,va,v3d,vten,vten1
      real, dimension(:,:,:), allocatable :: rrw,wa,w3d,wten,wten1
      real, dimension(:,:,:), allocatable :: ppi,pp3d,ppten,sten,sadv,ppx,phi1,phi2
      real, dimension(:,:,:), allocatable :: tha,th3d,thten,thten1,thterm
      real, dimension(:,:,:), allocatable :: qpten,qtten,qvten,qcten
      real, dimension(:,:,:,:), allocatable :: qa,q3d,qten
      real, dimension(:,:,:), allocatable :: p3a
      real, dimension(:,:,:,:), allocatable :: p3o
      real, dimension(:,:,:), allocatable :: kmh,kmv,khh,khv
      real, dimension(:,:,:), allocatable :: tkea,tke3d,tketen
      real, dimension(:,:,:), allocatable :: nm,defv,defh,lenscl,dissten,epst,epsd1,epsd2
      real, dimension(:,:,:), allocatable :: thpten,qvpten,qcpten,qipten,upten,vpten,qnipten,qncpten,qrpten,qspten,qgpten
      real, dimension(:,:,:), allocatable :: xkzh,xkzq,xkzm
      real, dimension(:,:,:), allocatable :: tsq,qsq,cov,sh3d,el_pbl,qc_bl,qi_bl,cldfra_bl,  &
                                      qWT,qSHEAR,qBUOY,qDISS,dqke,qke_adv,qke,qke3d,   &
                                      edmf_a,edmf_w,edmf_qt,edmf_thl,edmf_ent,edmf_qc, &
                                      sub_thl3D,sub_sqv3D,det_thl3D,det_sqv3D
      real, dimension(:,:), allocatable :: vdfg,maxmf
      integer, dimension(:,:), allocatable :: nupdraft,ktop_plume
      real, dimension(:,:,:), allocatable :: swten,lwten,swtenc,lwtenc,cldfra,o30
      real, dimension(:,:), allocatable :: zir,radsw,rnflx,radswnet,radlwin,dsr,olr
      real, dimension(:,:,:), allocatable :: rad2d,effc,effi,effs,effr,effg,effis
      real, dimension(:,:), allocatable :: lwupt,lwuptc,lwdnt,lwdntc,lwupb,lwupbc,lwdnb,lwdnbc
      real, dimension(:,:), allocatable :: swupt,swuptc,swdnt,swdntc,swupb,swupbc,swdnb,swdnbc
      real, dimension(:,:), allocatable :: lwcf,swcf,coszr
      real, dimension(:,:), allocatable :: xice,xsnow,xlat,xlong,coszen,swddir,swddni,swddif,hrang
      integer, dimension(:,:,:), allocatable :: cldfra1_flag
      integer, dimension(:,:), allocatable :: lu_index,kpbl2d
      real, dimension(:,:), allocatable :: u10,v10,s10,hfx,qfx,               &
                                      hpbl,wspd,phim,phih,psim,psih,psiq,gz1oz0,br,brcr, &
                                      CHS,CHS2,CQS2,CPMM,ZOL,MAVAIL,          &
                                      MOL,RMOL,REGIME,LH,FLHC,FLQC,QGH,       &
                                      CK,CKA,CDA,USTM,QSFC,T2,Q2,TH2,EMISS,THC,ALBD,   &
                                      gsw,glw,chklowq,capg,snowc,snowh,qcg,dsxy,wstar,delta,prkpp,fm,fh
      real, dimension(:,:), allocatable :: charn,msang,scurx,scury,zkmax,cd_out,ch_out,wscale,wscaleu
      real, dimension(:,:), allocatable :: mznt,swspd,smois,taux,tauy,hpbl2d,evap2d,heat2d

      ! for myj:
      real, dimension(:,:), allocatable :: mixht,akhs,akms,elflx,ct,snow,sice,thz0,qz0,uz0,vz0,u10e,v10e,th10,q10,tshltr,qshltr,pshltr,z0base,zntmyj
      integer, dimension(:,:), allocatable :: lowlyr,ivgtyp
      real, dimension(:,:,:), allocatable :: tke_myj,el_myj
      real, dimension(:,:,:), allocatable :: tmp_myj

      real, dimension(:), allocatable :: slab_zs,slab_dzs
      real, dimension(:,:,:), allocatable :: tslb
      real, dimension(:,:), allocatable :: tmn,tml,t0ml,hml,h0ml,huml,hvml,tmoml
      real, dimension(:,:,:,:),  allocatable :: pta,pt3d,ptten
      real, dimension(:,:), allocatable :: dat1,dat2
      real, dimension(:,:,:), allocatable :: dat3
      integer, dimension(:), allocatable :: reqt
      integer :: reqc,reqk
      real, dimension(:,:), allocatable :: pdata,ploc
      logical, dimension(:,:,:), allocatable :: flag

!--- arrays for MPI ---
      integer, dimension(:), allocatable :: myi1p,myi2p,myj1p,myj2p
      integer, dimension(:), allocatable :: reqs_u,reqs_v,reqs_w,reqs_s,reqs_p,reqs_p2,reqs_p3,reqs_x,reqs_y,reqs_z,reqs_tk
      integer, dimension(:,:),  allocatable :: reqs_q,reqs_t
      real, dimension(:), allocatable :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      real, dimension(:,:,:), allocatable :: n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2
      real, dimension(:,:), allocatable :: ww1,ww2,we1,we2
      real, dimension(:,:), allocatable :: ws1,ws2,wn1,wn2
      real, dimension(:,:), allocatable :: pw1,pw2,pe1,pe2
      real, dimension(:,:), allocatable :: ps1,ps2,pn1,pn2
      real, dimension(:,:), allocatable :: p2w1,p2w2,p2e1,p2e2
      real, dimension(:,:), allocatable :: p2s1,p2s2,p2n1,p2n2
      real, dimension(:,:), allocatable :: p3w1,p3w2,p3e1,p3e2
      real, dimension(:,:), allocatable :: p3s1,p3s2,p3n1,p3n2
      real, dimension(:,:), allocatable :: vw1,vw2,ve1,ve2
      real, dimension(:,:), allocatable :: vs1,vs2,vn1,vn2
      real, dimension(:,:), allocatable :: zw1,zw2,ze1,ze2
      real, dimension(:,:), allocatable :: zs1,zs2,zn1,zn2
      real, dimension(:,:,:), allocatable :: uw31,uw32,ue31,ue32
      real, dimension(:,:,:), allocatable :: us31,us32,un31,un32
      real, dimension(:,:,:), allocatable :: vw31,vw32,ve31,ve32
      real, dimension(:,:,:), allocatable :: vs31,vs32,vn31,vn32
      real, dimension(:,:,:), allocatable :: ww31,ww32,we31,we32
      real, dimension(:,:,:), allocatable :: ws31,ws32,wn31,wn32
      real, dimension(:,:,:), allocatable :: sw31,sw32,se31,se32
      real, dimension(:,:,:), allocatable :: ss31,ss32,sn31,sn32
      real, dimension(:,:,:), allocatable :: rw31,rw32,re31,re32
      real, dimension(:,:,:), allocatable :: rs31,rs32,rn31,rn32
      real, dimension(:,:,:,:), allocatable :: qw31,qw32,qe31,qe32
      real, dimension(:,:,:,:), allocatable :: qs31,qs32,qn31,qn32
      real, dimension(:,:,:), allocatable :: tkw1,tkw2,tke1,tke2
      real, dimension(:,:,:), allocatable :: tks1,tks2,tkn1,tkn2
      real, dimension(:,:,:), allocatable :: kw1,kw2,ke1,ke2
      real, dimension(:,:,:), allocatable :: ks1,ks2,kn1,kn2
      real, dimension(:,:,:,:), allocatable :: tw1,tw2,te1,te2
      real, dimension(:,:,:,:), allocatable :: ts1,ts2,tn1,tn2

      ! arrays for elliptic solver:
      real, dimension(:,:,:),    allocatable :: cfb
      real, dimension(:),        allocatable :: cfa,cfc,d1,d2
      complex, dimension(:,:,:), allocatable :: pdt,lgbth,lgbph
      complex, dimension(:,:),   allocatable :: rhs,trans

      ! diagnostic arrays:
      real, dimension(:,:,:,:), allocatable :: tdiag,qdiag,udiag,vdiag,wdiag,kdiag,pdiag

      ! miscellaneous output:
      real, dimension(:,:,:),   allocatable :: out2d
      real, dimension(:,:,:,:), allocatable :: out3d

      ! t2p arrays:
      real, dimension(:), allocatable :: gamk,gamwall,kmw,u1b,v1b,l2p,s2p,s2b,t2pm1,t2pm2,t2pm3,t2pm4,cmz,csz,ce1z,ce2z
      real, dimension(:,:,:), allocatable :: u2pt,v2pt

      ! cm1ib arrays:
      logical, dimension(:,:,:), allocatable :: bndy
      integer, dimension(:,:), allocatable :: kbdy
      integer, dimension(:,:,:), allocatable :: hflxw,hflxe,hflxs,hflxn

      logical :: dorestart,dowriteout,dostat,doprclout,dotdwrite,doazimwrite,dohifrqwrite

      ! eddy recycling:
      real, dimension(:,:,:), allocatable :: urecyw,vrecyw,wrecyw,trecyw
      real, dimension(:,:,:), allocatable :: urecys,vrecys,wrecys,trecys

      ! for large-scale nudging:
      real, dimension(:,:), allocatable :: lsnudge_u,lsnudge_v,lsnudge_th,lsnudge_qv

!-----

      integer count,rate,maxr
      real rtime,xtime,time_solve,time_solve0
      real steptime1,steptime2
      integer :: i,j,k,n,nn,fnum,frec
      real :: sum,tem0,tem
      logical :: getsfc,getpbl,update_sfc,startup,restarted,restart_prcl,reset
      logical :: dosolve,dorad,getdbz,getvt,doit,dotbud,doqbud,doubud,dovbud,dowbud,donudge

      integer :: icrs,icenter,jcenter
      real :: xcenter,ycenter

      double precision :: adaptmovetim
      integer :: mvrec,nwritemv

#ifdef MPI
      integer :: reqs,rc,ii,jj,id,itmp1,itmp2,jtmp1,jtmp2
      integer, dimension(MPI_STATUS_SIZE) :: status
      real :: mp_total,minval,temx,temy,temni,temnj
      double precision :: tstart,tend
      integer, dimension(:), allocatable :: isum,jsum
#endif

!----------------------------------------------------------------------

      nstep = 0
      nstep0 = 0
      mtime = 0.0d0
      srec=1
      sirec=1
      urec=1
      vrec=1
      wrec=1
      nrec=1
      mrec=1
      prec=1
      trecs=1
      trecw=1
      arecs=1
      arecw=1
      nrst=1
      nstatout=1
      nwrite = 1
      nwritet = 1
      nwritea = 1
      nwriteh = 1
      outfile=6
      startup = .true.
      stopit = .false.
      smeps = 1.0e-30
      tsmall = 0.0001
      ! (should be same as qsmall in morrison scheme)
      qsmall = 1.0e-14
#ifdef DP
      smeps = 1.0e-60
      tsmall = 0.000000000001
      qsmall = 1.0e-24
#endif
      qmag = 0.01
      np3a = 1
      np3o = 1
      cflmax = 0.0
      ksmax = 0.0
      ndt = 0
      adt = 0.0
      acfl = 0.0
      mass1 = 0.0
      mass2 = 0.0
      avgsfcu = 0.0
      avgsfcv = 0.0
      avgsfcs = 0.0
      avgsfct = 0.0
      nudgeobc = 0
      alphobc = 60.0
      diagtim = 0.0

      getsfc = .true.
      getpbl = .true.
      restarted = .false.
      restart_prcl = .false.
      restart_format = 1
      restart_filetype = 1
      restart_reset_frqtim = .true.
      run_time = -999.0
      hurr_rad = 1.0e30
      dolsw = .false.
      cmpr_output = .false.
      dohifrq = .false.
      iusetke = .false.
      ramp_up_turb = .false.
      do_adapt_move = .false.

      dorestart = .false.
      dowriteout = .false.
      dostat = .false.
      doprclout = .false.
      dotdwrite = .false.
      doazimwrite = .false.
      dohifrqwrite = .false.
      dotbud = .true.
      doqbud = .false.
      doubud = .false.
      dovbud = .false.
      dowbud = .false.

      adaptmovetim = 0.0
      nwritemv = 1
      mvrec = 1

      cm1version = 'cm1r20.2'

!----------------------------------------------------------------------
!  For arbitrary 3d output array:  
!
!    nout3d is the total number of output variables.
!
!    User must then "populate" the out3d array.  (That is, you must
!    fill in the out3d(i,j,k,1) and out3d(i,j,k,2) etc arrays with the
!    information you wish to write out.)
!
!    No other changes are necessary:  output file will contain the 
!    variables "out1", "out2", "out3", ... etc.

      nout3d =   0

!----------------------------------------------------------------------
!  For arbitrary 2d output array:  
!
!    nout2d is the total number of output variables.
!
!    User must then "populate" the out2d array.  (That is, you must
!    fill in the out2d(i,j,1) and out3d(i,j,2) etc arrays with the
!    information you wish to write out.)
!
!    No other changes are necessary:  output file will contain the 
!    variables "out2d1", "out2d2", "out2d3", ... etc.

      nout2d =   0

!----------------------------------------------------------------------
!  Initialize MPI

      myid=0
      numprocs=1

#ifdef MPI
      call MPI_INIT( ierr )
      call MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )
      call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr )
#endif

!----------------------------------------------------------------------

      if( myid.eq.0 )then
        print *
        print *,'|---------------------------------------------------------------|'
        print *,'|                                                               |'
        print *,'|          You are using CM1, Release 20.2 (cm1r20.2)           |'
        print *,'|                                                               |'
        print *,'|---------------------------------------------------------------|'

#ifdef MPI
        print *,'|                                                               |'
        print *,'|    compiled using distributed-memory parallelization (MPI)    |'
        print *,'|                                                               |'
        print *,'|---------------------------------------------------------------|'
        print *
#endif

#ifdef OPENMP
        print *,'|                                                               |'
        print *,'|     compiled using shared-memory parallelization (OpenMP)     |'
        print *,'|                                                               |'
        print *,'|---------------------------------------------------------------|'
        print *
#endif

        print *
      endif

!----------------------------------------------------------------------

      ! This initializes timer
      time_last = 0.0
      call system_clock(count,rate,maxr)
      clock_rate = 1.0/rate
      xtime = mytime()

      call set_time_to_zero()

!----------------------------------------------------------------------
!  Get domain dimensions, allocate some arrays, then call PARAM

      open(unit=20,file='namelist.input',form='formatted',status='old',    &
           access='sequential',err=8778)
      read(20,nml=param0)
      read(20,nml=param2)
      read(20,nml=param8)
      close(unit=20)

      ! note:  read remainder of namelist sections in param.F !

!----------------------------------------------------------------------

      IF( procfiles )THEN
#ifdef MPI
        outfile=10
#endif
        dowr = .true.
      ELSE
        dowr = .false.
      ENDIF

      IF( myid.eq.0 ) dowr = .true.

!----------------------------------------------------------------------
#ifdef MPI
  !......
  ! cm1r20.1:
  ! determine values for nodex and nodey:
    if(myid.eq.0)then
      minval = 1.0e30
      open(unit=10,file='proc.info',status='unknown')
      write(10,*)
    IF( nx.eq.1 .and. ny.eq.1 )THEN
      print *
      print *,'  nx = ',nx
      print *,'  ny = ',ny
      print *
      print *,'  Cannot use MPI when nx = ny = 1 '
      print *
      call stopcm1
    ELSEIF( nx.le.3 )THEN
      nodex = 1
      nodey = numprocs
    ELSEIF( ny.le.3 )THEN
      nodex = numprocs
      nodey = 1
    ELSE
      do n = numprocs , 1 , -1
        temy = float(n)
        temx = float(numprocs)/temy
        temni = float(nx)/temx
        temnj = float(ny)/temy
!!!        write(10,*) temx,temy,temx*temy,numprocs,temx/temy

        ! target: ni = 2 x nj
        tem0 = abs(temni/temnj-2.0)

        write(10,*) temx,temy,temni,temnj,temni/temnj
        ! only consider cases in which temx,temy are both integers:
        if( abs(float(int(temx))-temx).lt.smeps )then
          write(10,*) '      temx,temy   = ',temx,temy
          write(10,*) '      temni,temnj = ',temni,temnj
          if( tem0.lt.minval )then
            minval = tem0
            nodex = temx
            nodey = temy
            write(10,*) '        minval = ',minval
          endif
        endif
      enddo
      write(10,*)
      write(10,*) '  nodex,nodey = ',nodex,nodey,nodex*nodey,numprocs
      write(10,*)
    ENDIF
    endif

    ! just to be sure:
    nodex  = max(1,nodex)
    nodey  = max(1,nodey)
    ppnode = max(1,ppnode)

    call MPI_BCAST(nodex,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MPI_BCAST(nodey,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

  !......
#else
      ! serial (i.e. single-processor) run:
      nodex = 1
      nodey = 1
      ppnode = 1
#endif

      ni = nx / nodex
      nj = ny / nodey
      nk = nz
      nkp1 = nk+1

      nimax = ni
      njmax = nj

      myi=1
      myj=1

      myi1 =  1
      myi2 = nx
      myj1 =  1
      myj2 = ny

      allocate( myi1p(numprocs) )
      myi1p = 1
      allocate( myi2p(numprocs) )
      myi2p = nx
      allocate( myj1p(numprocs) )
      myj1p = 1
      allocate( myj2p(numprocs) )
      myj2p = ny

#ifdef MPI
!-----------------------------------------------
!  Determine number of grid points for each processore.
!
!  cm1r20:  each processor is no longer required
!           to have the same number of grid points

      myj = myid / nodex + 1
      myi = myid - (myj-1)*nodex  + 1

      allocate( isum(nodex) )
      isum = 0
      allocate( jsum(nodey) )
      jsum = 0

      ! determine number of "i" points for each processor:

      ni = 0
      id = 0

      do i = 1,nx
        id = id+1
        isum(id) = isum(id)+1
        if( id.eq.myi ) ni = ni+1
        if( id.eq.nodex ) id = 0
      enddo

      ! determine number of "j" points for each processor:

      nj = 0
      id = 0

      do j = 1,ny
        id = id+1
        jsum(id) = jsum(id)+1
        if( id.eq.myj ) nj = nj+1
        if( id.eq.nodey ) id = 0
      enddo

!......

      ! first gridpoint for each processor (from the total grid):

      myi1 = 1
      do i=1,(myi-1)
        myi1 = myi1+isum(i)
      enddo
      myi2 = myi1+ni-1

      myj1 = 1
      do j=1,(myj-1)
        myj1 = myj1+jsum(j)
      enddo
      myj2 = myj1+nj-1

!!!      write(10,*) '  myid,myi1,myi2,myj1,myj2 = ',myid,myi1,myi2,myj1,myj

!......

      ! write processor info:

      n = 0

      if( myid.eq.0 )then
        write(10,*)
        write(10,*) '  n,i,j,ni,nj:'
        do j = 1,nodey
        do i = 1,nodex
          if( i.eq.1 .and. j.eq.1 )then
            write(10,*) n,i,j,ni,nj
          else
            call mpi_recv(ii   ,1,mpi_integer,n,1,MPI_COMM_WORLD,status,ierr)
            call mpi_recv(jj   ,1,mpi_integer,n,2,MPI_COMM_WORLD,status,ierr)
            write(10,*) n,i,j,ii,jj
          endif
          n = n+1
        enddo
        enddo
      else
        call mpi_send(ni  ,1,mpi_integer,0,1,MPI_COMM_WORLD,ierr)
        call mpi_send(nj  ,1,mpi_integer,0,2,MPI_COMM_WORLD,ierr)
      endif

!......

      ! write more processor info:

      n = 0

      if( myid.eq.0 )then
        write(10,*)
        write(10,*) '  n,myi1,myi2,myj1,myj2:'
        do j = 1,nodey
        do i = 1,nodex
          if( i.eq.1 .and. j.eq.1 )then
            write(10,*) n,myi1,myi2,myj1,myj2
            myi1p(1) = myi1
            myi2p(1) = myi2
            myj1p(1) = myj1
            myj2p(1) = myj2
          else
            call mpi_recv(itmp1,1,mpi_integer,n,3,MPI_COMM_WORLD,status,ierr)
            call mpi_recv(itmp2,1,mpi_integer,n,4,MPI_COMM_WORLD,status,ierr)
            call mpi_recv(jtmp1,1,mpi_integer,n,5,MPI_COMM_WORLD,status,ierr)
            call mpi_recv(jtmp2,1,mpi_integer,n,6,MPI_COMM_WORLD,status,ierr)
            write(10,*) n,itmp1,itmp2,jtmp1,jtmp2
            myi1p(n+1) = itmp1
            myi2p(n+1) = itmp2
            myj1p(n+1) = jtmp1
            myj2p(n+1) = jtmp2
          endif
          n = n+1
        enddo
        enddo
        write(10,*)
      else
        call mpi_send(myi1,1,mpi_integer,0,3,MPI_COMM_WORLD,ierr)
        call mpi_send(myi2,1,mpi_integer,0,4,MPI_COMM_WORLD,ierr)
        call mpi_send(myj1,1,mpi_integer,0,5,MPI_COMM_WORLD,ierr)
        call mpi_send(myj2,1,mpi_integer,0,6,MPI_COMM_WORLD,ierr)
      endif

      call MPI_BCAST(myi1p,numprocs,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(myi2p,numprocs,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(myj1p,numprocs,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      call MPI_BCAST(myj2p,numprocs,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

      nimax = 0
      do n=1,numprocs
        nimax = max( nimax , myi2p(n)-myi1p(n)+1 )
      enddo

      njmax = 0
      do n=1,numprocs
        njmax = max( njmax , myj2p(n)-myj1p(n)+1 )
      enddo

!......

      deallocate( isum )
      deallocate( jsum )

      close(unit=10)

!!!      call MPI_BARRIER (MPI_COMM_WORLD,ierr)

!-----------------------------------------------
#endif


      call wenocheck

      ! number of 'ghost' points in the horizontal directions:
      ngxy  = 2
      if( hadvordrs.eq.3 .or. hadvordrv.eq.3 .or. hadvordrs.eq.4 .or. hadvordrv.eq.4 )   ngxy = 2
      if( hadvordrs.eq.5 .or. hadvordrv.eq.5 .or. hadvordrs.eq.6 .or. hadvordrv.eq.6 )   ngxy = 3
      if( hadvordrs.eq.7 .or. hadvordrv.eq.7 .or. hadvordrs.eq.8 .or. hadvordrv.eq.8 )   ngxy = 4
      if( hadvordrs.eq.9 .or. hadvordrv.eq.9 .or. hadvordrs.eq.10 .or. hadvordrv.eq.10 ) ngxy = 5

      if( advwenos.ge.1 .or. advwenov.ge.1 )then
        if( weno_order.eq.3 ) ngxy = max(ngxy,2)
        if( weno_order.eq.5 ) ngxy = max(ngxy,3)
        if( weno_order.eq.7 ) ngxy = max(ngxy,4)
        if( weno_order.eq.9 ) ngxy = max(ngxy,5)
      endif

      ! number of 'ghost' points in the vertical direction:
      ngz   = 1

!---------------------------------------------------------------------
!      For ZVD:
!      ngz   = 3
!      IF( ngz.eq.3 )THEN
!        kb =  1 - ngz
!        ke = nk + ngz
!      ENDIF
!---------------------------------------------------------------------

      ib =  1 - ngxy
      ie = ni + ngxy
      jb =  1 - ngxy
      je = nj + ngxy
      kb =  1 - ngz
      ke = nk + ngz

      allocate(    xh(ib:ie) )
      xh = 0.0
      allocate(   rxh(ib:ie) )
      rxh = 0.0
      allocate(  arh1(ib:ie) )
      arh1 = 0.0
      allocate(  arh2(ib:ie) )
      arh2 = 0.0
      allocate(    uh(ib:ie) )
      uh = 0.0
      allocate(   ruh(ib:ie) )
      ruh = 0.0
      allocate(    xf(ib:ie+1) )
      xf = 0.0
      allocate(   rxf(ib:ie+1) )
      rxf = 0.0
      allocate(  arf1(ib:ie+1) )
      arf1 = 0.0
      allocate(  arf2(ib:ie+1) )
      arf2 = 0.0
      allocate(    uf(ib:ie+1) )
      uf = 0.0
      allocate(   ruf(ib:ie+1) )
      ruf = 0.0
      allocate(    yh(jb:je) )
      yh = 0.0
      allocate(    vh(jb:je) )
      vh = 0.0
      allocate(   rvh(jb:je) )
      rvh = 0.0
      allocate(    yf(jb:je+1) )
      yf = 0.0
      allocate(    vf(jb:je+1) )
      vf = 0.0
      allocate(   rvf(jb:je+1) )
      rvf = 0.0
      allocate( xfref(1-ngxy:nx+ngxy+1) )
      xfref = 0.0
      allocate( xhref(1-ngxy:nx+ngxy+1) )
      xhref = 0.0
      allocate( yfref(1-ngxy:ny+ngxy+1) )
      yfref = 0.0
      allocate( yhref(1-ngxy:ny+ngxy+1) )
      yhref = 0.0
      allocate( dumk1(kb:ke) )
      dumk1 = 0.0
      allocate( dumk2(kb:ke) )
      dumk2 = 0.0
      allocate( dum2d1(ib:ie,jb:je) )
      dum2d1 = 0.0
      allocate( dum2d2(ib:ie,jb:je) )
      dum2d2 = 0.0
      allocate( dum2d3(ib:ie,jb:je) )
      dum2d3 = 0.0
      allocate( dum2d4(ib:ie,jb:je) )
      dum2d4 = 0.0
      allocate( dum2d5(ib:ie,jb:je) )
      dum2d5 = 0.0
      allocate(   rds(kb:ke) )
      rds = 0.0
      allocate( sigma(kb:ke) )
      sigma = 0.0
      allocate(   rdsf(kb:ke+1) )
      rdsf = 0.0
      allocate( sigmaf(kb:ke+1) )
      sigmaf = 0.0
      allocate(  tauh(ib:ie,jb:je,kb:ke) )
      tauh = 0.0
      allocate(  taus(ib:ie,jb:je,kb:ke) )
      taus = 0.0
      allocate(    zh(ib:ie,jb:je,kb:ke) )
      zh = 0.0
      allocate(    mh(ib:ie,jb:je,kb:ke) )
      mh = 0.0
      allocate(   rmh(ib:ie,jb:je,kb:ke) )
      rmh = 0.0
      allocate(    c1(ib:ie,jb:je,kb:ke) )
      c1 = 0.0
      allocate(    c2(ib:ie,jb:je,kb:ke) )
      c2 = 0.0
      allocate(  tauf(ib:ie,jb:je,kb:ke+1) )
      tauf = 0.0
      allocate(    mf(ib:ie,jb:je,kb:ke+1) )
      mf = 0.0
      allocate(   rmf(ib:ie,jb:je,kb:ke+1) )
      rmf = 0.0
      allocate(    f2d(ib:ie,jb:je) )
      f2d = 0.0
      allocate(  psmth(ib:ie,jb:je) )
      psmth = 0.0
      allocate(  prate(ib:ie,jb:je) )
      prate = 0.0

      allocate( gamk(kb:ke) )
      gamk = 0.0
      allocate( gamwall(kb:ke) )
      gamwall = 0.0

      if(terrain_flag)then
        itb=ib
        ite=ie
        jtb=jb
        jte=je
        ktb=kb
        kte=ke
      else
        itb=1
        ite=1
        jtb=1
        jte=1
        ktb=1
        kte=1
      endif

      allocate(   zs(ib:ie,jb:je) )
      zs = 0.0
      allocate(   gz(itb:ite,jtb:jte) )
      gz = 0.0
      allocate(  rgz(itb:ite,jtb:jte) )
      rgz = 0.0
      allocate(  gzu(itb:ite,jtb:jte) )
      gzu = 0.0
      allocate( rgzu(itb:ite,jtb:jte) )
      rgzu = 0.0
      allocate(  gzv(itb:ite,jtb:jte) )
      gzv = 0.0
      allocate( rgzv(itb:ite,jtb:jte) )
      rgzv = 0.0
      allocate( dzdx(itb:ite,jtb:jte) )
      dzdx = 0.0
      allocate( dzdy(itb:ite,jtb:jte) )
      dzdy = 0.0
      allocate(   gx(itb:ite,jtb:jte,ktb:kte) )
      gx = 0.0
      allocate(  gxu(itb:ite,jtb:jte,ktb:kte) )
      gxu = 0.0
      allocate(   gy(itb:ite,jtb:jte,ktb:kte) )
      gy = 0.0
      allocate(  gyv(itb:ite,jtb:jte,ktb:kte) )
      gyv = 0.0
      allocate(   zf(ib:ie,jb:je,kb:ke+1) )
      zf = 0.0

      allocate( dtu(ib:ie,jb:je) )
      dtu = 1.0
      allocate( dtu0(ib:ie,jb:je) )
      dtu0 = 1.0
      allocate( dtv(ib:ie,jb:je) )
      dtv = 1.0
      allocate( dtv0(ib:ie,jb:je) )
      dtv0 = 1.0

      allocate( cmz(kb:ke) )
      cmz = 0.0
      allocate( csz(kb:ke) )
      csz = 0.0
      allocate( ce1z(kb:ke) )
      ce1z = 0.0
      allocate( ce2z(kb:ke) )
      ce2z = 0.0

!------
! allocate the MPI arrays

#ifdef MPI
      imp = max(1,ni)
      jmp = max(1,nj)
      kmp = max(2,nk)
      kmt = max(2,nk+1)
      rmp = 8
      cmp = ngxy
#else
      imp = 1
      jmp = 1
      kmp = 2
      kmt = 2
      rmp = 1
      cmp = 1
#endif

      allocate( reqs_u(rmp) )
      reqs_u = 0
      allocate( reqs_v(rmp) )
      reqs_v = 0
      allocate( reqs_w(rmp) )
      reqs_w = 0
      allocate( reqs_s(rmp) )
      reqs_s = 0
      allocate( reqs_p(rmp) )
      reqs_p = 0
      allocate( reqs_p2(rmp) )
      reqs_p2 = 0
      allocate( reqs_p3(rmp) )
      reqs_p3 = 0
      allocate( reqs_x(rmp) )
      reqs_x = 0
      allocate( reqs_y(rmp) )
      reqs_y = 0
      allocate( reqs_z(rmp) )
      reqs_z = 0
      allocate( reqs_tk(rmp) )
      reqs_tk = 0

      allocate( nw1(kmt) )
      nw1 = 0.0
      allocate( nw2(kmt) )
      nw2 = 0.0
      allocate( ne1(kmt) )
      ne1 = 0.0
      allocate( ne2(kmt) )
      ne2 = 0.0
      allocate( sw1(kmt) )
      sw1 = 0.0
      allocate( sw2(kmt) )
      sw2 = 0.0
      allocate( se1(kmt) )
      se1 = 0.0
      allocate( se2(kmt) )
      se2 = 0.0

      allocate( n3w1(cmp,cmp,kmt+1) )
      n3w1 = 0.0
      allocate( n3w2(cmp,cmp,kmt+1) )
      n3w2 = 0.0
      allocate( n3e1(cmp,cmp,kmt+1) )
      n3e1 = 0.0
      allocate( n3e2(cmp,cmp,kmt+1) )
      n3e2 = 0.0
      allocate( s3w1(cmp,cmp,kmt+1) )
      s3w1 = 0.0
      allocate( s3w2(cmp,cmp,kmt+1) )
      s3w2 = 0.0
      allocate( s3e1(cmp,cmp,kmt+1) )
      s3e1 = 0.0
      allocate( s3e2(cmp,cmp,kmt+1) )
      s3e2 = 0.0

      allocate( ww1(jmp,kmp-1) )
      ww1 = 0.0
      allocate( ww2(jmp,kmp-1) )
      ww2 = 0.0
      allocate( we1(jmp,kmp-1) )
      we1 = 0.0
      allocate( we2(jmp,kmp-1) )
      we2 = 0.0
      allocate( ws1(imp,kmp-1) )
      ws1 = 0.0
      allocate( ws2(imp,kmp-1) )
      ws2 = 0.0
      allocate( wn1(imp,kmp-1) )
      wn1 = 0.0
      allocate( wn2(imp,kmp-1) )
      wn2 = 0.0

      allocate( pw1(jmp,kmp) )
      pw1 = 0.0
      allocate( pw2(jmp,kmp) )
      pw2 = 0.0
      allocate( pe1(jmp,kmp) )
      pe1 = 0.0
      allocate( pe2(jmp,kmp) )
      pe2 = 0.0
      allocate( ps1(imp,kmp) )
      ps1 = 0.0
      allocate( ps2(imp,kmp) )
      ps2 = 0.0
      allocate( pn1(imp,kmp) )
      pn1 = 0.0
      allocate( pn2(imp,kmp) )
      pn2 = 0.0

      allocate( p2w1(jmp,kmp) )
      p2w1 = 0.0
      allocate( p2w2(jmp,kmp) )
      p2w2 = 0.0
      allocate( p2e1(jmp,kmp) )
      p2e1 = 0.0
      allocate( p2e2(jmp,kmp) )
      p2e2 = 0.0
      allocate( p2s1(imp,kmp) )
      p2s1 = 0.0
      allocate( p2s2(imp,kmp) )
      p2s2 = 0.0
      allocate( p2n1(imp,kmp) )
      p2n1 = 0.0
      allocate( p2n2(imp,kmp) )
      p2n2 = 0.0

      allocate( p3w1(jmp,kmp) )
      p3w1 = 0.0
      allocate( p3w2(jmp,kmp) )
      p3w2 = 0.0
      allocate( p3e1(jmp,kmp) )
      p3e1 = 0.0
      allocate( p3e2(jmp,kmp) )
      p3e2 = 0.0
      allocate( p3s1(imp,kmp) )
      p3s1 = 0.0
      allocate( p3s2(imp,kmp) )
      p3s2 = 0.0
      allocate( p3n1(imp,kmp) )
      p3n1 = 0.0
      allocate( p3n2(imp,kmp) )
      p3n2 = 0.0

      allocate( vw1(jmp,kmp) )
      vw1 = 0.0
      allocate( vw2(jmp,kmp) )
      vw2 = 0.0
      allocate( ve1(jmp,kmp) )
      ve1 = 0.0
      allocate( ve2(jmp,kmp) )
      ve2 = 0.0
      allocate( vs1(imp,kmp) )
      vs1 = 0.0
      allocate( vs2(imp,kmp) )
      vs2 = 0.0
      allocate( vn1(imp,kmp) )
      vn1 = 0.0
      allocate( vn2(imp,kmp) )
      vn2 = 0.0

      allocate( zw1(jmp,kmp) )
      zw1 = 0.0
      allocate( zw2(jmp,kmp) )
      zw2 = 0.0
      allocate( ze1(jmp,kmp) )
      ze1 = 0.0
      allocate( ze2(jmp,kmp) )
      ze2 = 0.0
      allocate( zs1(imp,kmp) )
      zs1 = 0.0
      allocate( zs2(imp,kmp) )
      zs2 = 0.0
      allocate( zn1(imp,kmp) )
      zn1 = 0.0
      allocate( zn2(imp,kmp) )
      zn2 = 0.0

      allocate( uw31(cmp,jmp,kmp) )
      uw31 = 0.0
      allocate( uw32(cmp,jmp,kmp) )
      uw32 = 0.0
      allocate( ue31(cmp,jmp,kmp) )
      ue31 = 0.0
      allocate( ue32(cmp,jmp,kmp) )
      ue32 = 0.0
      allocate( us31(imp+1,cmp,kmp) )
      us31 = 0.0
      allocate( us32(imp+1,cmp,kmp) )
      us32 = 0.0
      allocate( un31(imp+1,cmp,kmp) )
      un31 = 0.0
      allocate( un32(imp+1,cmp,kmp) )
      un32 = 0.0

      allocate( vw31(cmp,jmp+1,kmp) )
      vw31 = 0.0
      allocate( vw32(cmp,jmp+1,kmp) )
      vw32 = 0.0
      allocate( ve31(cmp,jmp+1,kmp) )
      ve31 = 0.0
      allocate( ve32(cmp,jmp+1,kmp) )
      ve32 = 0.0
      allocate( vs31(imp,cmp,kmp) )
      vs31 = 0.0
      allocate( vs32(imp,cmp,kmp) )
      vs32 = 0.0
      allocate( vn31(imp,cmp,kmp) )
      vn31 = 0.0
      allocate( vn32(imp,cmp,kmp) )
      vn32 = 0.0

      allocate( ww31(cmp,jmp,kmp-1) )
      ww31 = 0.0
      allocate( ww32(cmp,jmp,kmp-1) )
      ww32 = 0.0
      allocate( we31(cmp,jmp,kmp-1) )
      we31 = 0.0
      allocate( we32(cmp,jmp,kmp-1) )
      we32 = 0.0
      allocate( ws31(imp,cmp,kmp-1) )
      ws31 = 0.0
      allocate( ws32(imp,cmp,kmp-1) )
      ws32 = 0.0
      allocate( wn31(imp,cmp,kmp-1) )
      wn31 = 0.0
      allocate( wn32(imp,cmp,kmp-1) )
      wn32 = 0.0

      allocate( sw31(cmp,jmp,kmp) )
      sw31 = 0.0
      allocate( sw32(cmp,jmp,kmp) )
      sw32 = 0.0
      allocate( se31(cmp,jmp,kmp) )
      se31 = 0.0
      allocate( se32(cmp,jmp,kmp) )
      se32 = 0.0
      allocate( ss31(imp,cmp,kmp) )
      ss31 = 0.0
      allocate( ss32(imp,cmp,kmp) )
      ss32 = 0.0
      allocate( sn31(imp,cmp,kmp) )
      sn31 = 0.0
      allocate( sn32(imp,cmp,kmp) )
      sn32 = 0.0

      allocate( rw31(cmp,jmp,kmp) )
      rw31 = 0.0
      allocate( rw32(cmp,jmp,kmp) )
      rw32 = 0.0
      allocate( re31(cmp,jmp,kmp) )
      re31 = 0.0
      allocate( re32(cmp,jmp,kmp) )
      re32 = 0.0
      allocate( rs31(imp,cmp,kmp) )
      rs31 = 0.0
      allocate( rs32(imp,cmp,kmp) )
      rs32 = 0.0
      allocate( rn31(imp,cmp,kmp) )
      rn31 = 0.0
      allocate( rn32(imp,cmp,kmp) )
      rn32 = 0.0

      allocate( tkw1(cmp,jmp,kmt) )
      tkw1 = 0.0
      allocate( tkw2(cmp,jmp,kmt) )
      tkw2 = 0.0
      allocate( tke1(cmp,jmp,kmt) )
      tke1 = 0.0
      allocate( tke2(cmp,jmp,kmt) )
      tke2 = 0.0
      allocate( tks1(imp,cmp,kmt) )
      tks1 = 0.0
      allocate( tks2(imp,cmp,kmt) )
      tks2 = 0.0
      allocate( tkn1(imp,cmp,kmt) )
      tkn1 = 0.0
      allocate( tkn2(imp,cmp,kmt) )
      tkn2 = 0.0

      allocate( kw1(jmp,kmt,4) )
      kw1 = 0.0
      allocate( kw2(jmp,kmt,4) )
      kw2 = 0.0
      allocate( ke1(jmp,kmt,4) )
      ke1 = 0.0
      allocate( ke2(jmp,kmt,4) )
      ke2 = 0.0
      allocate( ks1(imp,kmt,4) )
      ks1 = 0.0
      allocate( ks2(imp,kmt,4) )
      ks2 = 0.0
      allocate( kn1(imp,kmt,4) )
      kn1 = 0.0
      allocate( kn2(imp,kmt,4) )
      kn2 = 0.0

      call       param(dt,dtlast,stattim,taptim,rsttim,radtim,prcltim,  &
                       cloudvar,rhovar,qmag,qname,qunit,budname,        &
                       xh,rxh,arh1,arh2,uh,ruh,xf,rxf,arf1,arf2,uf,ruf, &
                       yh,vh,rvh,yf,vf,rvf,xfref,xhref,yfref,yhref,     &
                       rds,sigma,rdsf,sigmaf,tauh,taus,                 &
                       zh,mh,rmh,c1 ,c2 ,tauf,zf,mf,rmf,f2d,dtu0,dtv0,  &
                       gamk,gamwall,cmz,csz,ce1z,ce2z,                  &
                       zs,gz,rgz,gzu,rgzu,gzv,rgzv,dzdx,dzdy,gx,gxu,gy,gyv,  &
                       reqs_u,reqs_v,reqs_s,reqs_p,                     &
                       nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,                 &
                       n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,         &
                       sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,         &
                       uw31,uw32,ue31,ue32,us31,us32,un31,un32,         &
                       vw31,vw32,ve31,ve32,vs31,vs32,vn31,vn32,         &
                       ww31,ww32,we31,we32,ws31,ws32,wn31,wn32)

!----------------------------------------------------------------------

      dbldt = dble(dt)

      allocate( reqs_q(rmp,numq) )
      reqs_q = 0
      allocate( reqs_t(rmp,npt) )
      reqs_t = 0

      allocate( qw31(cmp,jmp,kmp,numq) )
      qw31 = 0.0
      allocate( qw32(cmp,jmp,kmp,numq) )
      qw32 = 0.0
      allocate( qe31(cmp,jmp,kmp,numq) )
      qe31 = 0.0
      allocate( qe32(cmp,jmp,kmp,numq) )
      qe32 = 0.0
      allocate( qs31(imp,cmp,kmp,numq) )
      qs31 = 0.0
      allocate( qs32(imp,cmp,kmp,numq) )
      qs32 = 0.0
      allocate( qn31(imp,cmp,kmp,numq) )
      qn31 = 0.0
      allocate( qn32(imp,cmp,kmp,numq) )
      qn32 = 0.0

      allocate( tw1(cmp,jmp,kmp,npt) )
      tw1 = 0.0
      allocate( tw2(cmp,jmp,kmp,npt) )
      tw2 = 0.0
      allocate( te1(cmp,jmp,kmp,npt) )
      te1 = 0.0
      allocate( te2(cmp,jmp,kmp,npt) )
      te2 = 0.0
      allocate( ts1(imp,cmp,kmp,npt) )
      ts1 = 0.0
      allocate( ts2(imp,cmp,kmp,npt) )
      ts2 = 0.0
      allocate( tn1(imp,cmp,kmp,npt) )
      tn1 = 0.0
      allocate( tn2(imp,cmp,kmp,npt) )
      tn2 = 0.0


      ! 1d arrays for test cases:
      allocate( wprof(kb:ke) )
      wprof = 0.0
      allocate( ufrc(kb:ke) )
      ufrc = 0.0
      allocate( vfrc(kb:ke) )
      vfrc = 0.0
      allocate( thfrc(kb:ke) )
      thfrc = 0.0
      allocate( qvfrc(kb:ke) )
      qvfrc = 0.0
      allocate( ug(kb:ke) )
      ug = 0.0
      allocate( vg(kb:ke) )
      vg = 0.0
      allocate( dvdr(kb:ke) )
      dvdr = 0.0

      allocate( uavg(kb:ke) )
      uavg = 0.0
      allocate( vavg(kb:ke) )
      vavg = 0.0
      allocate( savg(kb:ke) )
      savg = 0.0
      allocate( thavg(kb:ke) )
      thavg = 0.0
      allocate( pavg(kb:ke) )
      pavg = 0.0

      allocate( ulspg(kb:ke) )
      ulspg = 0.0
      allocate( vlspg(kb:ke) )
      vlspg = 0.0

      allocate( qavg(kb:ke,numq) )
      qavg = 0.0

      allocate( cavg(kb:ke,3+numq) )
      cavg = 0.0

      allocate( kmw(kb:ke) )
      kmw = 0.0
      allocate( u1b(kb:ke) )
      u1b = 0.0
      allocate( v1b(kb:ke) )
      v1b = 0.0
      allocate( l2p(kb:ke) )
      l2p = 0.0
      allocate( s2p(kb:ke) )
      s2p = 0.0
      allocate( s2b(kb:ke) )
      s2b = 0.0
      allocate( t2pm1(kb:ke) )
      t2pm1 = 0.0
      allocate( t2pm2(kb:ke) )
      t2pm2 = 0.0
      allocate( t2pm3(kb:ke) )
      t2pm3 = 0.0
      allocate( t2pm4(kb:ke) )
      t2pm4 = 0.0

      allocate( u2pt(ib2pt:ie2pt,jb2pt:je2pt,kb2pt:ke2pt) )
      u2pt = 0.0
      allocate( v2pt(ib2pt:ie2pt,jb2pt:je2pt,kb2pt:ke2pt) )
      v2pt = 0.0

!----------------------------------------------------------------------
!  allocate the base state arrays, then call BASE

      allocate( rho0s(ib:ie,jb:je) )
      rho0s = 0.0
      allocate(  pi0s(ib:ie,jb:je) )
      pi0s = 0.0
      allocate( prs0s(ib:ie,jb:je) )
      prs0s = 0.0
      allocate( rth0s(ib:ie,jb:je) )
      rth0s = 0.0
      allocate(  pi0(ib:ie,jb:je,kb:ke) )
      pi0 = 0.0
      allocate( rho0(ib:ie,jb:je,kb:ke) )
      rho0 = 0.0
      allocate( prs0(ib:ie,jb:je,kb:ke) )
      prs0 = 0.0
      allocate( thv0(ib:ie,jb:je,kb:ke) )
      thv0 = 0.0
      allocate(  th0(ib:ie,jb:je,kb:ke) )
      th0 = 0.0
      allocate( rth0(ib:ie,jb:je,kb:ke) )
      rth0 = 0.0
      allocate(  qv0(ib:ie,jb:je,kb:ke) )
      qv0 = 0.0
      allocate(  qc0(ib:ie,jb:je,kb:ke) )
      qc0 = 0.0
      allocate(  qi0(ib:ie,jb:je,kb:ke) )
      qi0 = 0.0
      allocate(  rr0(ib:ie,jb:je,kb:ke) )
      rr0 = 0.0
      allocate(  rf0(ib:ie,jb:je,kb:ke) )
      rf0 = 0.0
      allocate( rrf0(ib:ie,jb:je,kb:ke) )
      rrf0 = 0.0
      allocate(   u0(ib:ie+1,jb:je,kb:ke) )
      u0 = 0.0
      allocate(   v0(ib:ie,jb:je+1,kb:ke) )
      v0 = 0.0

      allocate( thrd(ibb2:ibe2,jbb2:jbe2,kbb2:kbe2) )
      thrd = 0.0

      allocate( dum1(ib:ie,jb:je,kb:ke) )
      dum1 = 0.0
      allocate( dum2(ib:ie,jb:je,kb:ke) )
      dum2 = 0.0
      allocate( dum3(ib:ie,jb:je,kb:ke) )
      dum3 = 0.0
      allocate( dum4(ib:ie,jb:je,kb:ke) )
      dum4 = 0.0
      allocate( dum5(ib:ie,jb:je,kb:ke) )
      dum5 = 0.0
      allocate( dum6(ib:ie,jb:je,kb:ke) )
      dum6 = 0.0
      allocate( dum7(ib:ie,jb:je,kb:ke) )
      dum7 = 0.0
      allocate( dum8(ib:ie,jb:je,kb:ke) )
      dum8 = 0.0
      allocate( dum9(ib:ie,jb:je,kb:ke) )
      dum9 = 0.0

      allocate( out2d(ib2d:ie2d,jb2d:je2d,nout2d) )
      out2d = 0.0

      allocate( out3d(ib3d:ie3d,jb3d:je3d,kb3d:ke3d,nout3d) )
      out3d = 0.0

      call       base(zh,mh,rmh,c1,c2,zf,mf,rho0s,pi0s,prs0s,rth0s,         &
                      wprof,ufrc,vfrc,thfrc,qvfrc,ug,vg,dvdr,               &
                      uavg,vavg,thavg,pavg,qavg,ulspg,vlspg,                &
                      pi0,prs0,rho0,thv0,th0,rth0,qv0,u0,v0,thrd,           &
                      qc0,qi0,rr0,rf0,rrf0,dum1,dum2,                       &
                      reqs_u,reqs_v,reqs_s,nw1,nw2,ne1,ne2,sw1,sw2,se1,se2, &
                      n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,              &
                      uw31,uw32,ue31,ue32,us31,us32,un31,un32,              &
                      vw31,vw32,ve31,ve32,vs31,vs32,vn31,vn32,              &
                      sw31,sw32,se31,se32,ss31,ss32,sn31,sn32)


!----------------------------------------------------------------------
!  Now, allocate the mother lode, then call INIT3D

      allocate(   rain(ib:ie,jb:je,nrain) )
      rain = 0.0
      allocate(    sws(ib:ie,jb:je,nrain) )
      sws = 0.0
      allocate(    svs(ib:ie,jb:je,nrain) )
      svs = 0.0
      allocate(    sps(ib:ie,jb:je,nrain) )
      sps = 0.0
      allocate(    srs(ib:ie,jb:je,nrain) )
      srs = 0.0
      allocate(    sgs(ib:ie,jb:je,nrain) )
      sgs = 0.0
      allocate(    sus(ib:ie,jb:je,nrain) )
      sus = 0.0
      allocate(    shs(ib:ie,jb:je,nrain) )
      shs = 0.0

      allocate(    tsk(ib:ie,jb:je) )
      tsk = 0.0
      allocate(    znt(ib:ie,jb:je) )
      znt = 0.0
      allocate(   rznt(ib:ie,jb:je) )
      rznt = 0.0
      allocate(  zntmp(ib:ie,jb:je) )
      zntmp = 0.0
      allocate(    ust(ib:ie,jb:je) )
      ust = 0.0
      allocate(   stau(ib:ie,jb:je) )
      stau = 0.0
      allocate(    tst(ib:ie,jb:je) )
      tst = 0.0
      allocate(    qst(ib:ie,jb:je) )
      qst = 0.0
      allocate(    z0t(ib:ie,jb:je) )
      z0t = 0.0
      allocate(    z0q(ib:ie,jb:je) )
      z0q = 0.0
      allocate( thflux(ib:ie,jb:je) )
      thflux = 0.0
      allocate( qvflux(ib:ie,jb:je) )
      qvflux = 0.0
      allocate(     cd(ib:ie,jb:je) )
      cd = 0.0
      allocate(     ch(ib:ie,jb:je) )
      ch = 0.0
      allocate(     cq(ib:ie,jb:je) )
      cq = 0.0
      allocate(     u1(ib:ie,jb:je) )
      u1 = 0.0
      allocate(     v1(ib:ie,jb:je) )
      v1 = 0.0
      allocate(     s1(ib:ie,jb:je) )
      s1 = 0.0
      allocate(     t1(ib:ie,jb:je) )
      t1 = 0.0
      allocate(  xland(ib:ie,jb:je) )
      xland = 0.0
      allocate(   psfc(ib:ie,jb:je) )
      psfc = 0.0
      allocate(    tlh(ib:ie,jb:je) )
      tlh = l_h
      allocate(   ustt(ib:ie,jb:je) )
      ustt = 0.0
      allocate(     ut(ib:ie,jb:je) )
      ut = 0.0
      allocate(     vt(ib:ie,jb:je) )
      vt = 0.0
      allocate(     st(ib:ie,jb:je) )
      st = 0.0

      allocate( radbcw(jb:je,kb:ke) )
      radbcw = 0.0
      allocate( radbce(jb:je,kb:ke) )
      radbce = 0.0
      allocate( radbcs(ib:ie,kb:ke) )
      radbcs = 0.0
      allocate( radbcn(ib:ie,kb:ke) )
      radbcn = 0.0

      allocate( divx(ib:ie,jb:je,kb:ke) )
      divx = 0.0
      allocate(  rho(ib:ie,jb:je,kb:ke) )
      rho = 0.0
      allocate(   rr(ib:ie,jb:je,kb:ke) )
      rr = 0.0
      allocate(   rf(ib:ie,jb:je,kb:ke) )
      rf = 0.0
      allocate(  prs(ib:ie,jb:je,kb:ke) )
      prs = 0.0
      allocate(  t11(ib:ie,jb:je,kb:ke) )
      t11 = 0.0
      allocate(  t12(ib:ie,jb:je,kb:ke) )
      t12 = 0.0
      allocate(  t13(ib:ie,jb:je,kb:ke) )
      t13 = 0.0
      allocate(  t22(ib:ie,jb:je,kb:ke) )
      t22 = 0.0
      allocate(  t23(ib:ie,jb:je,kb:ke) )
      t23 = 0.0
      allocate(  t33(ib:ie,jb:je,kb:ke) )
      t33 = 0.0

      allocate( m11(ibnba:ienba,jbnba:jenba,kbnba:kenba) )
      m11 = 0.0
      allocate( m12(ibnba:ienba,jbnba:jenba,kbnba:kenba) )
      m12 = 0.0
      allocate( m13(ibnba:ienba,jbnba:jenba,kbnba:kenba) )
      m13 = 0.0
      allocate( m22(ibnba:ienba,jbnba:jenba,kbnba:kenba) )
      m22 = 0.0
      allocate( m23(ibnba:ienba,jbnba:jenba,kbnba:kenba) )
      m23 = 0.0
      allocate( m33(ibnba:ienba,jbnba:jenba,kbnba:kenba) )
      m33 = 0.0

      allocate(   rru(ib:ie+1,jb:je,kb:ke) )
      rru = 0.0
      allocate(    ua(ib:ie+1,jb:je,kb:ke) )
      ua = 0.0
      allocate(   u3d(ib:ie+1,jb:je,kb:ke) )
      u3d = 0.0
      allocate(  uten(ib:ie+1,jb:je,kb:ke) )
      uten = 0.0
      allocate( uten1(ib:ie+1,jb:je,kb:ke) )
      uten1 = 0.0

      allocate(   rrv(ib:ie,jb:je+1,kb:ke) )
      rrv = 0.0
      allocate(    va(ib:ie,jb:je+1,kb:ke) )
      va = 0.0
      allocate(   v3d(ib:ie,jb:je+1,kb:ke) )
      v3d = 0.0
      allocate(  vten(ib:ie,jb:je+1,kb:ke) )
      vten = 0.0
      allocate( vten1(ib:ie,jb:je+1,kb:ke) )
      vten1 = 0.0

      allocate(   rrw(ib:ie,jb:je,kb:ke+1) )
      rrw = 0.0
      allocate(    wa(ib:ie,jb:je,kb:ke+1) )
      wa = 0.0
      allocate(   w3d(ib:ie,jb:je,kb:ke+1) )
      w3d = 0.0
      allocate(  wten(ib:ie,jb:je,kb:ke+1) )
      wten = 0.0
      allocate( wten1(ib:ie,jb:je,kb:ke+1) )
      wten1 = 0.0

      allocate(   ppi(ib:ie,jb:je,kb:ke) )
      ppi = 0.0
      allocate(  pp3d(ib:ie,jb:je,kb:ke) )
      pp3d = 0.0
      allocate( ppten(ib:ie,jb:je,kb:ke) )
      ppten = 0.0
      allocate(  sten(ib:ie,jb:je,kb:ke) )
      sten = 0.0
      allocate(  sadv(ib:ie,jb:je,kb:ke) )
      sadv = 0.0
      allocate(   ppx(ib:ie,jb:je,kb:ke) )
      ppx = 0.0

      allocate(  phi1(ibph:ieph,jbph:jeph,kbph:keph) )
      phi1 = 0.0
      allocate(  phi2(ibph:ieph,jbph:jeph,kbph:keph) )
      phi2 = 0.0

      allocate(   tha(ib:ie,jb:je,kb:ke) )
      tha = 0.0
      allocate(  th3d(ib:ie,jb:je,kb:ke) )
      th3d = 0.0
      allocate( thten(ib:ie,jb:je,kb:ke) )
      thten = 0.0
      allocate(thten1(ib:ie,jb:je,kb:ke) )
      thten1 = 0.0
      allocate(thterm(ib:ie,jb:je,kb:ke) )
      thterm = 0.0

      allocate( qpten(ibm:iem,jbm:jem,kbm:kem) )
      qpten = 0.0
      allocate( qtten(ibm:iem,jbm:jem,kbm:kem) )
      qtten = 0.0
      allocate( qvten(ibm:iem,jbm:jem,kbm:kem) )
      qvten = 0.0
      allocate( qcten(ibm:iem,jbm:jem,kbm:kem) )
      qcten = 0.0

      allocate(   bud(nk) )
      bud = 0.0
      allocate(  bud2(nj) )
      bud2 = 0.0
      allocate( qbudget(nbudget) )
      qbudget = 0.0
      allocate(    asq(numq) )
      asq = 0.0
      allocate(    bsq(numq) )
      bsq = 0.0

      allocate(     qa(ibm:iem,jbm:jem,kbm:kem,numq) )
      qa = 0.0
      allocate(    q3d(ibm:iem,jbm:jem,kbm:kem,numq) )
      q3d = 0.0
      allocate(   qten(ibm:iem,jbm:jem,kbm:kem,numq) )
      qten = 0.0

      allocate( p3a(ibp3:iep3,kbp3:kep3,np3a) )
      p3a = 0.0
      allocate( p3o(ibp3:iep3,jbp3:jep3,kbp3:kep3,np3o) )
      p3o = 0.0

      allocate(    kmh(ibc:iec,jbc:jec,kbc:kec) )
      kmh = 0.0
      allocate(    kmv(ibc:iec,jbc:jec,kbc:kec) )
      kmv = 0.0
      allocate(    khh(ibc:iec,jbc:jec,kbc:kec) )
      khh = 0.0
      allocate(    khv(ibc:iec,jbc:jec,kbc:kec) )
      khv = 0.0
      allocate(   tkea(ibt:iet,jbt:jet,kbt:ket) )
      tkea = 0.0
      allocate(  tke3d(ibt:iet,jbt:jet,kbt:ket) )
      tke3d = 0.0
      allocate( tketen(ibt:iet,jbt:jet,kbt:ket) )
      tketen = 0.0

      allocate(      nm(ib:ie,jb:je,kb:ke+1) )
      nm = 0.0
      allocate(    defv(ib:ie,jb:je,kb:ke+1) )
      defv = 0.0
      allocate(    defh(ib:ie,jb:je,kb:ke+1) )
      defh = 0.0
      allocate(  lenscl(ib:ie,jb:je,kb:ke+1) )
      lenscl = 0.0
      allocate( dissten(ib:ie,jb:je,kb:ke+1) )
      dissten = 0.0
      allocate(    epst(ib:ie,jb:je,kb:ke+1) )
      epst = 0.0
      allocate(   epsd1(ib:ie,jb:je,kb:ke+1) )
      epsd1 = 0.0
      allocate(   epsd2(ib:ie,jb:je,kb:ke+1) )
      epsd2 = 0.0

      allocate( thpten(ibb:ieb,jbb:jeb,kbb:keb) )
      thpten = 0.0
      allocate( qvpten(ibb:ieb,jbb:jeb,kbb:keb) )
      qvpten = 0.0
      allocate( qcpten(ibb:ieb,jbb:jeb,kbb:keb) )
      qcpten = 0.0
      allocate( qipten(ibb:ieb,jbb:jeb,kbb:keb) )
      qipten = 0.0
      allocate(  upten(ibb:ieb,jbb:jeb,kbb:keb) )
      upten = 0.0
      allocate(  vpten(ibb:ieb,jbb:jeb,kbb:keb) )
      vpten = 0.0
      allocate( qnipten(ibb:ieb,jbb:jeb,kbb:keb) )
      qnipten = 0.0
      allocate( qncpten(ibb:ieb,jbb:jeb,kbb:keb) )
      qncpten = 0.0
      allocate( qrpten(ibb:ieb,jbb:jeb,kbb:keb) )
      qrpten = 0.0
      allocate( qspten(ibb:ieb,jbb:jeb,kbb:keb) )
      qspten = 0.0
      allocate( qgpten(ibb:ieb,jbb:jeb,kbb:keb) )
      qgpten = 0.0

      allocate(   xkzh(ibb:ieb,jbb:jeb,kbb:keb) )
      xkzh = 0.0
      allocate(   xkzq(ibb:ieb,jbb:jeb,kbb:keb) )
      xkzq = 0.0
      allocate(   xkzm(ibb:ieb,jbb:jeb,kbb:keb) )
      xkzm = 0.0

      ! MYNN:
      allocate(       tsq(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      tsq = 0.0
      allocate(       qsq(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      qsq = 0.0
      allocate(       cov(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      cov = 0.0
      allocate(      sh3d(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      sh3d = 0.0
      allocate(    el_pbl(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      el_pbl = 0.0
      allocate(     qc_bl(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      qc_bl = 0.0
      allocate(     qi_bl(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      qi_bl = 0.0
      allocate( cldfra_bl(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      cldfra_bl = 0.0
      allocate(       qwt(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      qwt = 0.0
      allocate(    qshear(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      qshear = 0.0
      allocate(     qbuoy(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      qbuoy = 0.0
      allocate(     qdiss(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      qdiss = 0.0
      allocate(      dqke(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      dqke = 0.0
      allocate(   qke_adv(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      qke_adv = 0.0
      allocate(       qke(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      qke = 0.0
      allocate(     qke3d(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      qke3d = 0.0
      allocate(    edmf_a(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      edmf_a = 0.0
      allocate(    edmf_w(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      edmf_w = 0.0
      allocate(   edmf_qt(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      edmf_qt = 0.0
      allocate(  edmf_thl(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      edmf_thl = 0.0
      allocate(  edmf_ent(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      edmf_ent = 0.0
      allocate(   edmf_qc(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      edmf_qc = 0.0
      allocate( sub_thl3D(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      sub_thl3D = 0.0
      allocate( sub_sqv3D(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      sub_sqv3D = 0.0
      allocate( det_thl3D(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      det_thl3D = 0.0
      allocate( det_sqv3D(ibmynn:iemynn,jbmynn:jemynn,kbmynn:kemynn) )
      det_sqv3D = 0.0

      allocate( vdfg(ibmynn:iemynn,jbmynn:jemynn) )
      vdfg = 0.0
      allocate( maxmf(ibmynn:iemynn,jbmynn:jemynn) )
      maxmf = 0.0

      allocate( nupdraft(ibmynn:iemynn,jbmynn:jemynn) )
      nupdraft = 0
      allocate( ktop_plume(ibmynn:iemynn,jbmynn:jemynn) )
      ktop_plume = 0

      !-----------------
      ! begin radiation

      allocate( swten(ibr:ier,jbr:jer,kbr:ker) )
      swten = 0.0
      allocate( lwten(ibr:ier,jbr:jer,kbr:ker) )
      lwten = 0.0
      allocate( swtenc(ibr:ier,jbr:jer,kbr:ker) )
      swtenc = 0.0
      allocate( lwtenc(ibr:ier,jbr:jer,kbr:ker) )
      lwtenc = 0.0
      allocate(cldfra(ibr:ier,jbr:jer,kbr:ker) )
      cldfra = 0.0
      allocate(   o30(ibr:ier,jbr:jer,kbr:ker) )
      o30 = 0.0
      allocate(   zir(ibr:ier,jbr:jer) )
      zir = 0.0

      IF( radopt .eq. 1 )THEN
        nir = 1
        njr = 1
        nkr = nk+3
        rbufsz = n2d_radiat*nir*njr + n3d_radiat*nir*njr*nkr
      ELSE
        nir = 1
        njr = 1
        nkr = 1
        rbufsz = 1
      ENDIF

      allocate(    radsw(ni,nj) )
      radsw = 0.0
      allocate(    rnflx(ni,nj) )
      rnflx = 0.0
      allocate( radswnet(ni,nj) )
      radswnet = 0.0
      allocate(  radlwin(ni,nj) )
      radlwin = 0.0
      allocate(      dsr(ni,nj) )
      dsr = 0.0
      allocate(      olr(ni,nj) )
      olr = 0.0

      allocate(    rad2d(ni,nj,nrad2d) )
      rad2d = 0.0

      allocate(  effc(ibr:ier,jbr:jer,kbr:ker) )
      effc = 25.0
      allocate(  effi(ibr:ier,jbr:jer,kbr:ker) )
      effi = 25.0
      allocate(  effs(ibr:ier,jbr:jer,kbr:ker) )
      effs = 25.0
      allocate(  effr(ibr:ier,jbr:jer,kbr:ker) )
      effr = 25.0
      allocate(  effg(ibr:ier,jbr:jer,kbr:ker) )
      effg = 25.0
      allocate( effis(ibr:ier,jbr:jer,kbr:ker) )
      effis = 25.0

      allocate( lwupt(ibr:ier,jbr:jer) )
      lwupt = 0.0
      allocate( lwuptc(ibr:ier,jbr:jer) )
      lwuptc = 0.0
      allocate( lwdnt(ibr:ier,jbr:jer) )
      lwdnt = 0.0
      allocate( lwdntc(ibr:ier,jbr:jer) )
      lwdntc = 0.0
      allocate( lwupb(ibr:ier,jbr:jer) )
      lwupb = 0.0
      allocate( lwupbc(ibr:ier,jbr:jer) )
      lwupbc = 0.0
      allocate( lwdnb(ibr:ier,jbr:jer) )
      lwdnb = 0.0
      allocate( lwdnbc(ibr:ier,jbr:jer) )
      lwdnbc = 0.0

      allocate( swupt(ibr:ier,jbr:jer) )
      swupt = 0.0
      allocate( swuptc(ibr:ier,jbr:jer) )
      swuptc = 0.0
      allocate( swdnt(ibr:ier,jbr:jer) )
      swdnt = 0.0
      allocate( swdntc(ibr:ier,jbr:jer) )
      swdntc = 0.0
      allocate( swupb(ibr:ier,jbr:jer) )
      swupb = 0.0
      allocate( swupbc(ibr:ier,jbr:jer) )
      swupbc = 0.0
      allocate( swdnb(ibr:ier,jbr:jer) )
      swdnb = 0.0
      allocate( swdnbc(ibr:ier,jbr:jer) )
      swdnbc = 0.0

      allocate(   lwcf(ibr:ier,jbr:jer) )
      lwcf = 0.0
      allocate(   swcf(ibr:ier,jbr:jer) )
      swcf = 0.0
      allocate(  coszr(ibr:ier,jbr:jer) )
      coszr = 0.0

      allocate( xice(ibr:ier,jbr:jer) )
      xice = 0.0
      allocate( xsnow(ibr:ier,jbr:jer) )
      xsnow = 0.0
      allocate( xlat(ibr:ier,jbr:jer) )
      xlat = 0.0
      allocate( xlong(ibr:ier,jbr:jer) )
      xlong = 0.0
      allocate( coszen(ibr:ier,jbr:jer) )
      coszen = 0.0
      allocate( swddir(ibr:ier,jbr:jer) )
      swddir = 0.0
      allocate( swddni(ibr:ier,jbr:jer) )
      swddni = 0.0
      allocate( swddif(ibr:ier,jbr:jer) )
      swddif = 0.0
      allocate( hrang(ibr:ier,jbr:jer) )
      hrang = 0.0

      allocate( cldfra1_flag(ibr:ier,jbr:jer,kbr:ker) )
      cldfra1_flag = 0

      if(dowr) write(outfile,*) '  rbufsz,nrad2d = ',rbufsz,nrad2d


      ! end radiation
      !-----------------

      allocate( lu_index(ibl:iel,jbl:jel) )
      lu_index = 0
      allocate(   kpbl2d(ibl:iel,jbl:jel) )
      kpbl2d = 0
      allocate(      u10(ibl:iel,jbl:jel) )
      u10 = 0.0
      allocate(      v10(ibl:iel,jbl:jel) )
      v10 = 0.0
      allocate(      s10(ibl:iel,jbl:jel) )
      s10 = 0.0
      allocate(      hfx(ibl:iel,jbl:jel) )
      hfx = 0.0
      allocate(      qfx(ibl:iel,jbl:jel) )
      qfx = 0.0
      allocate(     hpbl(ibl:iel,jbl:jel) )
      hpbl = 100.0
      allocate(     wspd(ibl:iel,jbl:jel) )
      wspd = 0.0
      allocate(     phim(ibl:iel,jbl:jel) )
      phim = 0.0
      allocate(     phih(ibl:iel,jbl:jel) )
      phih = 0.0
      allocate(     psim(ibl:iel,jbl:jel) )
      psim = 0.0
      allocate(     psih(ibl:iel,jbl:jel) )
      psih = 0.0
      allocate(     psiq(ibl:iel,jbl:jel) )
      psiq = 0.0
      allocate(   gz1oz0(ibl:iel,jbl:jel) )
      gz1oz0 = 0.0
      allocate(       br(ibl:iel,jbl:jel) )
      br = 0.0
      allocate(     brcr(ibl:iel,jbl:jel) )
      brcr = 0.0
      allocate(      chs(ibl:iel,jbl:jel) )
      chs = 0.0
      allocate(     chs2(ibl:iel,jbl:jel) )
      chs2 = 0.0
      allocate(     cqs2(ibl:iel,jbl:jel) )
      cqs2 = 0.0
      allocate(     cpmm(ibl:iel,jbl:jel) )
      cpmm = 0.0
      allocate(      zol(ibl:iel,jbl:jel) )
      zol = 0.0
      allocate(   mavail(ibl:iel,jbl:jel) )
      mavail = 0.0
      allocate(      mol(ibl:iel,jbl:jel) )
      mol = 0.0
      allocate(     rmol(ibl:iel,jbl:jel) )
      rmol = 0.0
      allocate(   regime(ibl:iel,jbl:jel) )
      regime = 0.0
      allocate(       lh(ibl:iel,jbl:jel) )
      lh = 0.0
      allocate(     flhc(ibl:iel,jbl:jel) )
      flhc = 0.0
      allocate(     flqc(ibl:iel,jbl:jel) )
      flqc = 0.0
      allocate(      qgh(ibl:iel,jbl:jel) )
      qgh = 0.0
      allocate(       ck(ibl:iel,jbl:jel) )
      ck = 0.0
      allocate(      cka(ibl:iel,jbl:jel) )
      cka = 0.0
      allocate(      cda(ibl:iel,jbl:jel) )
      cda = 0.0
      allocate(     ustm(ibl:iel,jbl:jel) )
      ustm = 0.0
      allocate(     qsfc(ibl:iel,jbl:jel) )
      qsfc = 0.0
      allocate(       t2(ibl:iel,jbl:jel) )
      t2 = 0.0
      allocate(       q2(ibl:iel,jbl:jel) )
      q2 = 0.0
      allocate(      th2(ibl:iel,jbl:jel) )
      th2 = 0.0
      allocate(    emiss(ibl:iel,jbl:jel) )
      emiss = 0.0
      allocate(      thc(ibl:iel,jbl:jel) )
      thc = 0.0
      allocate(     albd(ibl:iel,jbl:jel) )
      albd = 0.0
      allocate(      gsw(ibl:iel,jbl:jel) )
      gsw = 0.0
      allocate(      glw(ibl:iel,jbl:jel) )
      glw = 0.0
      allocate(  chklowq(ibl:iel,jbl:jel) )
      chklowq = 0.0
      allocate(     capg(ibl:iel,jbl:jel) )
      capg = 0.0
      allocate(    snowc(ibl:iel,jbl:jel) )
      snowc = 0.0
      allocate(    snowh(ibl:iel,jbl:jel) )
      snowh = 0.0
      allocate(      qcg(ibl:iel,jbl:jel) )
      qcg = 0.0
      allocate(     dsxy(ibl:iel,jbl:jel) )
      dsxy = 0.0
      allocate(    wstar(ibl:iel,jbl:jel) )
      wstar = 0.0
      allocate(    delta(ibl:iel,jbl:jel) )
      delta = 0.0
      allocate(    prkpp(ibl:iel,jbl:jel) )
      prkpp = 0.0
      allocate(       fm(ibl:iel,jbl:jel) )
      fm = 0.0
      allocate(       fh(ibl:iel,jbl:jel) )
      fh = 0.0

      ! for hwrf sfc:
      allocate(    charn(ibl:iel,jbl:jel) )
      charn = 0.0185
      allocate(    msang(ibl:iel,jbl:jel) )
      msang = 0.0
      allocate(    scurx(ibl:iel,jbl:jel) )
      scurx = 0.0
      allocate(    scury(ibl:iel,jbl:jel) )
      scury = 0.0
      allocate(    zkmax(ibl:iel,jbl:jel) )
      zkmax = 0.0
      allocate(   cd_out(ibl:iel,jbl:jel) )
      cd_out = 0.0
      allocate(   ch_out(ibl:iel,jbl:jel) )
      ch_out = 0.0
      allocate(   wscale(ibl:iel,jbl:jel) )
      wscale = 0.0
      allocate(  wscaleu(ibl:iel,jbl:jel) )
      wscaleu = 0.0
      allocate(     mznt(ibl:iel,jbl:jel) )
      mznt = 0.0
      allocate(    swspd(ibl:iel,jbl:jel) )
      swspd = 0.0
      allocate(    smois(ibl:iel,jbl:jel) )
      smois = 0.0
      allocate(     taux(ibl:iel,jbl:jel) )
      taux = 0.0
      allocate(     tauy(ibl:iel,jbl:jel) )
      tauy = 0.0
      allocate(   hpbl2d(ibl:iel,jbl:jel) )
      hpbl2d = 0.0
      allocate(   evap2d(ibl:iel,jbl:jel) )
      evap2d = 0.0
      allocate(   heat2d(ibl:iel,jbl:jel) )
      heat2d = 0.0

      allocate(  mixht(ibmyj:iemyj,jbmyj:jemyj) )
      mixht = 0.0
      allocate(   akhs(ibmyj:iemyj,jbmyj:jemyj) )
      akhs = 0.0
      allocate(   akms(ibmyj:iemyj,jbmyj:jemyj) )
      akms = 0.0
      allocate(  elflx(ibmyj:iemyj,jbmyj:jemyj) )
      elflx = 0.0
      allocate(     ct(ibmyj:iemyj,jbmyj:jemyj) )
      ct = 0.0
      allocate(   snow(ibmyj:iemyj,jbmyj:jemyj) )
      snow = 0.0
      allocate(   sice(ibmyj:iemyj,jbmyj:jemyj) )
      sice = 0.0
      allocate(   thz0(ibmyj:iemyj,jbmyj:jemyj) )
      thz0 = 0.0
      allocate(    qz0(ibmyj:iemyj,jbmyj:jemyj) )
      qz0 = 0.0
      allocate(    uz0(ibmyj:iemyj,jbmyj:jemyj) )
      uz0 = 0.0
      allocate(    vz0(ibmyj:iemyj,jbmyj:jemyj) )
      vz0 = 0.0
      allocate(   u10e(ibmyj:iemyj,jbmyj:jemyj) )
      u10e = 0.0
      allocate(   v10e(ibmyj:iemyj,jbmyj:jemyj) )
      v10e = 0.0
      allocate(   th10(ibmyj:iemyj,jbmyj:jemyj) )
      th10 = 0.0
      allocate(    q10(ibmyj:iemyj,jbmyj:jemyj) )
      q10 = 0.0
      allocate( tshltr(ibmyj:iemyj,jbmyj:jemyj) )
      tshltr = 0.0
      allocate( qshltr(ibmyj:iemyj,jbmyj:jemyj) )
      qshltr = 0.0
      allocate( pshltr(ibmyj:iemyj,jbmyj:jemyj) )
      pshltr = 0.0
      allocate( z0base(ibmyj:iemyj,jbmyj:jemyj) )
      z0base = 0.0
      allocate( zntmyj(ibmyj:iemyj,jbmyj:jemyj) )
      zntmyj = 0.0

      allocate( lowlyr(ibmyj:iemyj,jbmyj:jemyj) )
      lowlyr = 0
      allocate( ivgtyp(ibmyj:iemyj,jbmyj:jemyj) )
      ivgtyp = 0

      allocate( tke_myj(ibmyj:iemyj,jbmyj:jemyj,kbmyj:kemyj) )
      tke_myj = 0.01
      allocate(  el_myj(ibmyj:iemyj,jbmyj:jemyj,kbmyj:kemyj) )
      el_myj = 0.0

      nmyj = 28

      allocate( tmp_myj(ibmyj:iemyj,kbmyj:kemyj,nmyj) )
      tmp_myj = 0.0

      ! start with very small, but non-zero, numbers:
      tem = 1.0e-6
      znt = tem
      rznt = 1.0/tem
      zntmp = tem
      ust = 1.0e-6
      ! to prevent divide-by-zeros on init for some combinations of namelist params:
      tsk  = 300.0
      psfc = 100000.0
      qsfc = 0.00001

      num_soil_layers = 5
      allocate(  slab_zs(num_soil_layers) )
      slab_zs = 0.0
      allocate( slab_dzs(num_soil_layers) )
      slab_dzs = 0.0
      allocate(  tslb(ibl:iel,jbl:jel,num_soil_layers) )
      tslb = 0.0
      allocate(   tmn(ibl:iel,jbl:jel) )
      tmn = 0.0

      ! arrays for oml model:
      allocate(   tml(ibl:iel,jbl:jel) )
      tml = 0.0
      allocate(  t0ml(ibl:iel,jbl:jel) )
      t0ml = 0.0
      allocate(   hml(ibl:iel,jbl:jel) )
      hml = 0.0
      allocate(  h0ml(ibl:iel,jbl:jel) )
      h0ml = 0.0
      allocate(  huml(ibl:iel,jbl:jel) )
      huml = 0.0
      allocate(  hvml(ibl:iel,jbl:jel) )
      hvml = 0.0
      allocate( tmoml(ibl:iel,jbl:jel) )
      tmoml = 0.0

      allocate(    pta(ibp:iep,jbp:jep,kbp:kep,npt) )
      pta = 0.0
      allocate(   pt3d(ibp:iep,jbp:jep,kbp:kep,npt) )
      pt3d = 0.0
      allocate(  ptten(ibp:iep,jbp:jep,kbp:kep,npt) )
      ptten = 0.0

!----------------------------------------------------------------------
!  arrays for cm1ib:

      allocate( bndy(ibib:ieib,jbib:jeib,kbib:keib) )
      bndy = .false.

      allocate( kbdy(ibib:ieib,jbib:jeib) )
      kbdy = 1

      IF( do_ib )THEN

        call   ib_init(xh,yh,xf,yf,sigma,sigmaf,zs,zh,bndy,kbdy,out3d,            &
                       ww31(1,1,1),ww32(1,1,1),we31(1,1,1),we32(1,1,1),           &
                       ws31(1,1,1),ws32(1,1,1),wn31(1,1,1),wn32(1,1,1),reqs_p)

      ENDIF

      allocate( hflxw(ibib:ieib,jbib:jeib,kmaxib) )
      hflxw = 0
      allocate( hflxe(ibib:ieib,jbib:jeib,kmaxib) )
      hflxe = 0
      allocate( hflxs(ibib:ieib,jbib:jeib,kmaxib) )
      hflxs = 0
      allocate( hflxn(ibib:ieib,jbib:jeib,kmaxib) )
      hflxn = 0

      IF( do_ib )THEN

        call   ib_flx_init(bndy,hflxw,hflxe,hflxs,hflxn,dum1,dum2,     &
                           uw31,uw32,ue31,ue32,us31,us32,un31,un32,    &
                           vw31,vw32,ve31,ve32,vs31,vs32,vn31,vn32,reqs_u,reqs_v)

      ENDIF

!----------------------------------------------------------------------
!  allocate arrays and initialize some variables for large-scale nudging:

      IF( do_lsnudge )THEN
        lsnudge_maxlevels = max( nk , 1000 )
      ELSE
        lsnudge_maxlevels = 1
      ENDIF

      lsnudge_unit = 74
      lsnudge_count = 0
      lsnudge_time1 = 0.0
      lsnudge_time2 = 0.0
      lsnudgefac = 0.0

      allocate( lsnudge_u(lsnudge_maxlevels,lsnudge_nmax) )
      lsnudge_u = 0.0
      allocate( lsnudge_v(lsnudge_maxlevels,lsnudge_nmax) )
      lsnudge_v = 0.0
      allocate( lsnudge_th(lsnudge_maxlevels,lsnudge_nmax) )
      lsnudge_th = 0.0
      allocate( lsnudge_qv(lsnudge_maxlevels,lsnudge_nmax) )
      lsnudge_qv = 0.0

!----------------------------------------------------------------------

      allocate( dat1(d3i,d3j) )
      dat1 = 0.0
      allocate( dat2(d2i,d2j) )
      dat2 = 0.0
      allocate( dat3(d3i,d3j,d3n) )
      dat3 = 0.0
      allocate( reqt(d3t) )
      reqt = 0


      allocate(  pdata(nparcels,npvals) )
      pdata = 0.0
      allocate(   ploc(nparcels,  3   ) )
      ploc = 0.0

      allocate( flag(ib:ie,jb:je,kb:ke) )
      flag = .false.

      allocate(    cfb(ipb:ipe,jpb:jpe,kpb:kpe) )
      cfb = 0.0
      allocate(    cfa(kpb:kpe) )
      cfa = 0.0
      allocate(    cfc(kpb:kpe) )
      cfc = 0.0
      allocate(     d1(kpb:kpe) )
      d1 = 0.0
      allocate(     d2(kpb:kpe) )
      d2 = 0.0
      allocate(    pdt(ipb:ipe,jpb:jpe,kpb:kpe) )
      pdt = 0.0
      allocate(  lgbth(ipb:ipe,jpb:jpe,kpb:kpe) )
      lgbth = 0.0
      allocate(  lgbph(ipb:ipe,jpb:jpe,kpb:kpe) )
      lgbph = 0.0
      allocate(    rhs(ipb:ipe,jpb:jpe) )
      rhs = 0.0
      allocate(  trans(ipb:ipe,jpb:jpe) )
      trans = 0.0

!----------------------------------------------------------------------

      call init3d(xh,rxh,uh,ruh,xf,rxf,uf,ruf,yh,vh,rvh,yf,vf,rvf,  &
                  xfref,yfref,sigma,c1,c2,gz,zs,                    &
                  zh,mh,rmh,zf,mf,rmf,rho0s,pi0s,prs0s,             &
                  pi0,prs0,rho0,thv0,th0,rth0,qv0,                  &
                  u0,v0,qc0,qi0,rr0,rf0,rrf0,                       &
                  rain,sws,svs,sps,srs,sgs,sus,shs,                 &
                  thflux,qvflux,cd,ch,cq,f2d,                       &
                  dum1,dum2,dum3,dum4,divx,rho,prs,                 &
                  t11,t12,t13,t22,t23,t33,                          &
                  rru,ua,u3d,uten,uten1,rrv,va,v3d,vten,vten1,      &
                  rrw,wa,w3d,wten,wten1,ppi,pp3d,ppten,sten,        &
                  tha,th3d,thten,thten1,qa,q3d,qten,                &
                  kmh,kmv,khh,khv,tkea,tke3d,tketen,                &
                  pta,pt3d,ptten,                                   &
                  pdata,cfb,cfa,cfc,d1,d2,pdt,lgbth,lgbph,rhs,trans)


!----------------------------------------------------------------------

      if( ibalance.eq.2 .and. psolver.ne.4 .and. psolver.ne.5 .and. (.not.pdcomp) )then
        deallocate( cfb )
        deallocate( cfa )
        deallocate( cfc )
        deallocate( d1 )
        deallocate( d2 )
        deallocate( pdt )
        deallocate( lgbth )
        deallocate( lgbph )
        deallocate( rhs )
        deallocate( trans )
        ipb = 1
        ipe = 1
        jpb = 1
        jpe = 1
        kpb = 1
        kpe = 1
        allocate(    cfb(ipb:ipe,jpb:jpe,kpb:kpe) )
        cfb = 0.0
        allocate(    cfa(kpb:kpe) )
        cfa = 0.0
        allocate(    cfc(kpb:kpe) )
        cfc = 0.0
        allocate(     d1(kpb:kpe) )
        d1 = 0.0
        allocate(     d2(kpb:kpe) )
        d2 = 0.0
        allocate(    pdt(ipb:ipe,jpb:jpe,kpb:kpe) )
        pdt = 0.0
        allocate(  lgbth(ipb:ipe,jpb:jpe,kpb:kpe) )
        lgbth = 0.0
        allocate(  lgbph(ipb:ipe,jpb:jpe,kpb:kpe) )
        lgbph = 0.0
        allocate(    rhs(ipb:ipe,jpb:jpe) )
        rhs = 0.0
        allocate(  trans(ipb:ipe,jpb:jpe) )
        trans = 0.0
      endif

      if( myid.eq.0 ) print *
      if( myid.eq.0 ) print *,'  allocating diagnostic arrays: '
      if( myid.eq.0 ) print *

      allocate( tdiag(ibdt:iedt,jbdt:jedt,kbdt:kedt,ntdiag) )
      tdiag = 0.0
      allocate( qdiag(ibdq:iedq,jbdq:jedq,kbdq:kedq,nqdiag) )
      qdiag = 0.0
      allocate( udiag(ibdv:iedv,jbdv:jedv,kbdv:kedv,nudiag) )
      udiag = 0.0
      allocate( vdiag(ibdv:iedv,jbdv:jedv,kbdv:kedv,nvdiag) )
      vdiag = 0.0
      allocate( wdiag(ibdv:iedv,jbdv:jedv,kbdv:kedv,nwdiag) )
      wdiag = 0.0
      allocate( kdiag(ibdk:iedk,jbdk:jedk,kbdk:kedk,nkdiag) )
      kdiag = 0.0
      allocate( pdiag(ibdp:iedp,jbdp:jedp,kbdp:kedp,npdiag) )
      pdiag = 0.0

!----------------------------------------------------------------------

      nwritea = 1

      icrs = 1
      icenter = nx/2 + 1
      jcenter = ny/2 + 1
      xcenter = minx + 0.5*(maxx-minx)
      ycenter = miny + 0.5*(maxy-miny)

      IF( doazimavg )THEN

        ! check that ddr is positive and non-zero
        if( ddr.lt.smeps )then
          if(myid.eq.0)then
          print *
          print *,'  ddr  = ',ddr
          print *
          print *,'  Stopping cm1 ... '
          print *
          endif
#ifdef MPI
          call MPI_BARRIER (MPI_COMM_WORLD,ierr)
#endif
          call stopcm1
        endif

        icrs = nint( rlen / ddr )

        ! check that icrs is greater than
        if( icrs.le.0 )then
          if(myid.eq.0)then
          print *
          print *,'  icrs = ',icrs
          print *
          print *,'  Stopping cm1 ... '
          print *
          endif
#ifdef MPI
          call MPI_BARRIER (MPI_COMM_WORLD,ierr)
#endif
          call stopcm1
        endif
        ! initial values for cyclone center
        ! (assumed center of domain)
        if( myid.eq.0 ) print *
        if( myid.eq.0 ) print *,'  ddr,rlen,icrs = ',ddr,rlen,icrs
        if( myid.eq.0 ) print *
      ENDIF

!----------------------------------------------------------------------
!  Eddy recycling:

      ! 200711: moved code to param.F

      allocate( urecyw(irecywe,jrecywe,krecy) )
      urecyw = 0.0
      allocate( vrecyw(irecywe,jrecywe,krecy) )
      vrecyw = 0.0
      allocate( wrecyw(irecywe,jrecywe,krecy) )
      wrecyw = 0.0
      allocate( trecyw(irecywe,jrecywe,krecy) )
      trecyw = 0.0

      allocate( urecys(irecysn,jrecysn,krecy) )
      urecys = 0.0
      allocate( vrecys(irecysn,jrecysn,krecy) )
      vrecys = 0.0
      allocate( wrecys(irecysn,jrecysn,krecy) )
      wrecys = 0.0
      allocate( trecys(irecysn,jrecysn,krecy) )
      trecys = 0.0

!----------------------------------------------------------------------
!  Prepare for I/O:

      cmpr_output = .true.


      call       setup_stat_vars(name_stat,desc_stat,unit_stat,  &
                                 qname,qunit,budname)

      if(dowr) write(outfile,*) 'stat_out = ',stat_out
      if(dowr) write(outfile,*)
      allocate( rstat(stat_out) )
      rstat = 0.0


      if( iprcl.eq.1 )  &
      call       setup_parcel_vars(name_prcl,desc_prcl,unit_prcl,qname)



!----------------------------------------------------------------------
!  New for cm1r19.6  (not thoroughly tested)

      dohifrq = .false.     ! do high-frequency output?

      hifrqfrq =  3600.0    ! frequency (seconds)

!----------------------------------------------------------------------

      call init_physics(prs0,rf0,dum1,dum2,dum3,u0,ua,v0,va,o30,   &
                             lu_index,xland,emiss,thc,albd,znt,mavail,tsk,u1,v1,s1, &
                             zh,u10,v10,wspd,lowlyr,ivgtyp,ust,sice)

      call init_surface(num_soil_layers,dosfcflx,xh,ruh,xf,yh,rvh,yf,   &
                        lu_index,xland,tsk,slab_zs,slab_dzs,tslb, &
                        emiss,thc,albd,znt,rznt,mavail,dsxy,prs0s,prs0,   &
                        tmn,tml,t0ml,hml,h0ml,huml,hvml,tmoml,z0base)

      IF(irst.eq.1)THEN

        startup = .false.
        restarted = .true.
        call     read_restart(nstep,srec,sirec,urec,vrec,wrec,nrec,mrec,prec,      &
                              trecs,trecw,arecs,arecw,                             &
                              nwrite,nwritet,nwritea,nwriteh,nrst,nstatout,        &
                              num_soil_layers,nrad2d,                              &
                              avgsfcu,avgsfcv,avgsfcs,avgsfct,                     &
                              dt,dtlast,mtime,ndt,adt,acfl,dbldt,mass1,            &
                              stattim,taptim,rsttim,radtim,prcltim,                &
                              qbudget,asq,bsq,qname,                               &
                              xfref,yfref,zh,zf,sigma,sigmaf,zs,                   &
                              th0,prs0,pi0,rho0,qv0,u0,v0,                         &
                              rain,sws,svs,sps,srs,sgs,sus,shs,                    &
                              tsk,znt,ust,cd,ch,cq,u1,v1,s1,t1,thflux,qvflux,      &
                              prate,ustt,ut,vt,st,                                 &
                              radbcw,radbce,radbcs,radbcn,                         &
                              rho,prs,ua,va,wa,ppi,tha,qa,tkea,                    &
                              swten,lwten,radsw,rnflx,radswnet,radlwin,rad2d,      &
                              effc,effi,effs,effr,effg,effis,                      &
                              lu_index,kpbl2d,psfc,u10,v10,s10,hfx,qfx,xland,      &
                              hpbl,wspd,psim,psih,gz1oz0,br,                       &
                              CHS,CHS2,CQS2,CPMM,ZOL,MAVAIL,                       &
                              MOL,RMOL,REGIME,LH,FLHC,FLQC,QGH,                    &
                              CK,CKA,CDA,USTM,QSFC,T2,Q2,TH2,EMISS,THC,ALBD,       &
                              gsw,glw,chklowq,capg,snowc,fm,fh,mznt,swspd,wstar,delta,tslb,    &
                              tmn,tml,t0ml,hml,h0ml,huml,hvml,tmoml,               &
                              qpten,qtten,qvten,qcten,pta,pdata,ploc,ppx,          &
                              tdiag,qdiag,phi1,phi2,                               &
                   tsq,qsq,cov,sh3d,el_pbl,qc_bl,qi_bl,cldfra_bl,            &
                   qWT,qSHEAR,qBUOY,qDISS,dqke,qke_adv,qke,qke3d,            &
                   edmf_a,edmf_w,edmf_qt,edmf_thl,edmf_ent,edmf_qc,          &
                   sub_thl3D,sub_sqv3D,det_thl3D,det_sqv3D,                  &
                   vdfg,maxmf,nupdraft,ktop_plume,                           &
                              tke_myj,el_myj,mixht,akhs,akms,elflx,ct,snow,sice,thz0,qz0,uz0,vz0,th10,q10,z0base,zntmyj,lowlyr,ivgtyp, &
                              thpten,qvpten,qcpten,qipten,upten,vpten,qnipten,qncpten, &
                              icenter,jcenter,xcenter,ycenter,adaptmovetim,mvrec,nwritemv,ug,vg, &
                              gamk,kmw,u1b,v1b,cmz,csz,ce1z,ce2z,                  &
                              dum1,dat1,dat2,dat3,reqt,myi1p,myi2p,myj1p,myj2p,restarted,restart_prcl)
          ! end_read_restart
        !  In case user wants to change values on a restart:
        IF( restart_reset_frqtim )THEN 
          if( statfrq.gt.1.0e-6 ) stattim = mtime + statfrq
          if(  tapfrq.gt.1.0e-6 ) taptim  = mtime + tapfrq
          if(  rstfrq.gt.1.0e-6 ) rsttim  = mtime + rstfrq
        ENDIF
        if(dowr) write(outfile,*)
        if(dowr) write(outfile,*) '  Using the following: '
        if(dowr) write(outfile,*)
        if(dowr) write(outfile,*) '   stattim = ',stattim
        if(dowr) write(outfile,*) '   taptim  = ',taptim
        if(dowr) write(outfile,*) '   rsttim  = ',rsttim
        if(dowr) write(outfile,*) '   radtim  = ',radtim
        if(dowr) write(outfile,*)

        ! Experimental ... not thoroughly tested !
!!!        doit = .false.
!!!        IF( doit )THEN
!!!          call change_uvmove(u0,ua,u3d,v0,va,v3d,20.0,3.0)
!!!        ENDIF
        ! Experimental ... not thoroughly tested !


        ! Experimental (cm1r19.8) !
        ! Insert random perturbations to potential temperature.
        ! (see subroutine in misclibs.F for more details)
!!!        call randpert(xfref,yfref,xh,yh,zh,zf,tha)

      ELSE

        restarted = .false.
        restart_prcl = .false.

      ENDIF


      IF( dodomaindiag )THEN
        IF( restarted .and. diagfrq.gt.1.0e-6 )THEN
          diagtim = mtime + diagfrq
          if(dowr) write(outfile,*)
          if(dowr) write(outfile,*) '   diagtim = ',diagtim
          if(dowr) write(outfile,*)
        ELSE
          diagtim = 0.0
        ENDIF
      ELSE
        diagtim = 1.0d60
      ENDIF

      IF( doazimavg )THEN
        IF( restarted .and. azimavgfrq.gt.1.0e-6 )THEN
          azimavgtim = mtime + azimavgfrq
          if(dowr) write(outfile,*)
          if(dowr) write(outfile,*) '   azimavgtim = ',azimavgtim
          if(dowr) write(outfile,*)
        ELSE
          azimavgtim = 0.0
        ENDIF
      ELSE
        azimavgtim = 1.0d60
      ENDIF

      IF( dohifrq )THEN
        IF( restarted .and. hifrqfrq.ge.1.0e-6 )THEN
          hifrqtim = mtime + hifrqfrq
          if(dowr) write(outfile,*)
          if(dowr) write(outfile,*) '   hifrqtim = ',hifrqtim
          if(dowr) write(outfile,*)
        ELSE
          hifrqtim = 0.0
        ENDIF
      ELSE
        hifrqtim = 1.0d60
      ENDIF


      IF( run_time .gt. 0.0 )THEN
        timax = mtime + run_time
        if(dowr) write(outfile,*)
        if(dowr) write(outfile,*) '  Detected positive value for run_time. '
        if(dowr) write(outfile,*) '     Using the following values: '
        if(dowr) write(outfile,*) '        run_time = ',run_time
        if(dowr) write(outfile,*) '        timax    = ',timax
        if(dowr) write(outfile,*)
      ENDIF

      call       getset(restarted,mass1,uh,ruh,vh,rvh,xh,yh,xf,yf,         &
                        zs,gz,sigma,sigmaf,rmh,mf,dzdx,dzdy,dumk1,dumk2,   &
                        pi0,th0,rho0,prs0,ust,u1,v1,s1,u0,v0,rth0,         &
                        zh,c1,c2,zf,rr,rf,rho,prs,dum1,dum2,               &
                        ua,u3d,va,v3d,wa,w3d,ppi,pp3d,ppx,phi1,phi2,       &
                        tha,th3d,qa,q3d,tkea,tke3d,pta,pt3d,pdata,         &
                        bndy,kbdy,                                         &
                        reqs_u,reqs_v,reqs_w,reqs_s,reqs_p,reqs_tk,        &
                        nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,                   &
                        pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,                   &
                        uw31,uw32,ue31,ue32,us31,us32,un31,un32,           &
                        vw31,vw32,ve31,ve32,vs31,vs32,vn31,vn32,           &
                        ww31,ww32,we31,we32,ws31,ws32,wn31,wn32,           &
                        sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,           &
                        tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2)


!----------------------------------------------------------------------
!  get info for large-scale nudging:
!  (190724, GHB: moved this after read_restart)

      IF( do_lsnudge )THEN

        if(dowr) write(outfile,*)
        if(dowr) write(outfile,*)
        if(dowr) write(outfile,*) '  LSNUDGE is active '

        if( myid.eq.0 )then
          print *
          print *,'  do_lsnudge_u   = ',do_lsnudge_u
          print *,'  do_lsnudge_v   = ',do_lsnudge_v
          print *,'  do_lsnudge_th  = ',do_lsnudge_th
          print *,'  do_lsnudge_qv  = ',do_lsnudge_qv
          print *
        endif

        call   read_lsnudge(lsnudge_u,lsnudge_v,lsnudge_th,lsnudge_qv,sngl(mtime),zh)

        ! 190407:  moved this from lsnudge_module 
!!!        lsnudge_tau    =  var1   ! time scale (seconds) for damping
!!!        lsnudge_start  =  var2   ! time (seconds) to begin large-scale nudging
!!!        lsnudge_end    =  var3   ! time (seconds) to end large-scale nudging

        if( myid.eq.0 ) print *
        if( myid.eq.0 ) print *,'  lsnudge_tau    =  ',lsnudge_tau
        if( myid.eq.0 ) print *,'  lsnudge_start  =  ',lsnudge_start
        if( myid.eq.0 ) print *,'  lsnudge_end    =  ',lsnudge_end
        if( myid.eq.0 ) print *

      ENDIF

!----------------------------------------------------------------------
!  All done with initialization and setup.  A few more odds and ends ....

      reqc = 0
      reqk = 0

      if( adapt_dt.eq.1 )then
        dt = dbldt
        if( .not. restarted )then
          call calccflquick(dt,uh,vh,mh,u3d,v3d,w3d,reqc)
#ifdef MPI
          call mpi_wait(reqc,mpi_status_ignore,ierr)
          if(cflmax.ge.1.50) stopit=.true.
          if(timestats.ge.1) time_cflq=time_cflq+mytime()
#endif
          if( cm1setup.ge.1 )then
            call calcksquick(dt,uh,vh,mf,kmh,kmv,khh,khv,reqk)
#ifdef MPI
            call mpi_wait(reqk,mpi_status_ignore,ierr)
            if(timestats.ge.1) time_cflq=time_cflq+mytime()
#endif
          endif
          stopit = .false.
          ndt  = 0
          adt  = 0.0
          acfl = 0.0
          call   getnewdt(ndt,dt,dtlast,adt,acfl,dbldt,                                 &
                          mtime,stattim,taptim,rsttim,prcltim,diagtim,azimavgtim,       &
                          dorestart,dowriteout,dostat,doprclout,dotdwrite,doazimwrite,  &
                          hifrqtim,dohifrqwrite,.true.)
        endif
      endif


      ! cm1r19:  nsound is always determined diagnostically
      call dtsmall(dt,dbldt)


      if(dowr) write(outfile,*)
      if(dowr) write(outfile,*) '-------------Done with Preprocessors-----------'
      if(dowr) write(outfile,*)


!----------------------------------------------------------------------

#ifdef MPI
      call MPI_BARRIER (MPI_COMM_WORLD,ierr)

      if(myid.eq.0)then
        tstart=mpi_wtime()
      endif
#endif

      call set_time_to_zero()

!----------------------------------------------------------------------
!  Time loop

      if(timestats.ge.1)then
        steptime1 = 0.0
        steptime2 = 0.0
      endif


      nstep0 = nstep


      IF( startup )THEN
        ! starting from t=0:  skip solve and write initial conditions
        dosolve     = .false.
        dorad       = .true.
        dostat      = .true.
        dowriteout  = .true.
        doprclout   = .true.
        dotdwrite   = .true.
        doazimwrite = .true.
        dohifrqwrite = .true.
      ELSE
        ! call solve
        dosolve     = .true.
        dorad       = .false.
        dostat      = .false.
        dowriteout  = .false.
        doprclout   = .false.
        dotdwrite   = .false.
        doazimwrite = .false.
        dohifrqwrite = .false.
        if( .not. restart_prcl ) doprclout = .true.
      ENDIF


      IF( abs(rstfrq).lt.1.0e-10 )THEN
        ! 150820:  Write restart file if rstfrq=0 and stop
        dorestart = .true.
        dosolve = .false.
        nrst = 0
      ELSE
        dorestart = .false.
      ENDIF

      IF( restarted )THEN
        dostat = .true.
        dosolve = .false.
        if( doazimavg .and. nwritea.eq.1 ) doazimwrite = .true.
        if( dodomaindiag .and. nwritet.eq.1 ) dotdwrite = .true.
        if( dohifrq .and. nwriteh.eq.1 ) dohifrqwrite = .true.
      ENDIF

      getvt = .false.
      IF( dowriteout .and. output_fallvel.eq.1 ) getvt = .true.
      IF( imoist.eq.1 .and. dodomaindiag .and. ptype.eq.5 ) getvt = .true.
      IF( imoist.eq.1 .and. dodomaindiag .and. testcase.eq.5 ) getvt = .true.


    !c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c!
    timeloop:  &
    DO WHILE( mtime.lt.timax )

      ifsolve:  &
      if( dosolve )then

        ! call solve, integrate model one time step:
        nstep = nstep + 1


        ! check if writeout will be called at end of timestep:
        !   (needed for some diagnostic calculations in solve)
        dowriteout = .false.
        if( (mtime+dbldt).ge.(taptim-0.1*dt) .or. tapfrq.lt.0.0 ) dowriteout = .true.
        IF( myid.eq.0 .and. dowriteout ) print *,'  dowriteout = ',dowriteout


        dostat = .false.
        if( (mtime+dbldt).ge.(stattim-0.1*dt) .or. statfrq.lt.0.0 )  dostat     =  .true.


        dorestart  =  .false.
        if( (mtime+dbldt).ge.(rsttim-0.1*dt) .and. rstfrq.gt.0.0 ) dorestart  =  .true.
        IF( myid.eq.0 .and. dorestart ) print *,'  dorestart = ',dorestart


        doprclout  =  .false.
        if( iprcl.eq.1 )then
          if( (mtime+dbldt).ge.(prcltim-0.1*dt) .or. prclfrq.lt.0.0 ) doprclout  =  .true.
        endif


        dorad = .false.
        IF( radopt.ge.1 )THEN
          ! use time at end of timestep:
          IF( (mtime+dbldt).ge.(radtim-0.1*dt) ) dorad = .true.
          IF( myid.eq.0 .and. dorad ) print *,'  dorad = ',dorad
        ENDIF


        getvt = .false.
        IF( dodomaindiag )THEN
        IF( imoist.eq.1 .and. (ptype.eq.5.or.testcase.eq.5) )THEN
          if( (mtime+dbldt).ge.(diagtim-0.1*dt) ) getvt = .true.
          if( getvt .and. myid.eq.0 ) print *,'  getvt = ',getvt
        ENDIF
        ENDIF


        getdbz = .false.
        IF(output_dbz.eq.1)THEN
          if( ((mtime+dbldt).ge.(taptim-0.1*dt)) .or. tapfrq.le.0.0 )then
            getdbz = .true.
          endif
          if( dodomaindiag )then
            if( (mtime+dbldt).ge.(diagtim-0.1*dt) .or. diagfrq.le.0.0 )then
              getdbz = .true.
            endif
          endif
          if( doazimavg )then
            if( (mtime+dbldt).ge.(azimavgtim-0.1*dt) .or. azimavgfrq.le.0.0 )then
              getdbz = .true.
            endif
          endif
          if( dohifrq )then
            if( (mtime+dbldt).ge.(hifrqtim-0.1*dt) .or. hifrqfrq.le.0.0 )then
              getdbz = .true.
            endif
          endif
        ENDIF
        IF( restart_file_dbz )THEN
          if( ((mtime+dbldt).ge.(rsttim-0.1*dt)) .and. rstfrq.gt.0.0001 )then
            getdbz = .true.
          endif
        ENDIF
        IF( iprcl.eq.1 .and. prcl_dbz.eq.1 )THEN
          if( ((mtime+dbldt).ge.(prcltim-0.1*dt)) .or. prclfrq.le.0.0 )then
            getdbz = .true.
          endif
        ENDIF
        if(getdbz)then
          if(dowr) write(outfile,*) '  Getting dbz ... '
        endif


        IF( dodomaindiag )THEN
          if( (mtime+dbldt).ge.(diagtim-0.1*dt) .or. diagfrq.le.0.0 )then
            dotdwrite = .true.
          endif
        ENDIF

        IF( doazimavg )THEN
          if( (mtime+dbldt).ge.(azimavgtim-0.1*dt) .or. azimavgfrq.le.0.0 )then
            doazimwrite = .true.
          endif
        ENDIF

        IF( dohifrq )THEN
          if( (mtime+dbldt).ge.(hifrqtim-0.1*dt) .or. hifrqfrq.le.0.0 )then
            dohifrqwrite = .true.
          endif
        ENDIF

        dotbud = .false.
        doqbud = .false.
        doubud = .false.
        dovbud = .false.
        dowbud = .false.

        IF( dowriteout .or. dotdwrite )THEN
          if( output_thbudget.eq.1 .or. dodomaindiag )THEN
            dotbud = .true.
            if(myid.eq.0) print *,'  dotbud = ',dotbud
          endif
          if( output_qvbudget.eq.1 .or. dodomaindiag )THEN
            doqbud = .true.
            if(myid.eq.0) print *,'  doqbud = ',doqbud
          endif
          if( output_ubudget.eq.1 .or. dodomaindiag )THEN
            doubud = .true.
            if(myid.eq.0) print *,'  doubud = ',doubud
          endif
          if( output_vbudget.eq.1 .or. dodomaindiag )THEN
            dovbud = .true.
            if(myid.eq.0) print *,'  dovbud = ',dovbud
          endif
          if( output_wbudget.eq.1 .or. dodomaindiag )THEN
            dowbud = .true.
            if(myid.eq.0) print *,'  dowbud = ',dowbud
          endif
        ENDIF

        donudge = .false.

        if( adapt_dt.eq.1 .and. myid.eq.0 ) write(6,122) cflmax,ksmax,dt,nsound
122     format(1x,'cflmax,ksmax,dt,nsound:',2x,f6.4,2x,f6.4,2x,f9.4,2x,i3)

        if(timestats.ge.1) time_misc=time_misc+mytime()


        ! solve1: pre-RK terms
        ! (tendencies held fixed during Runge-Kutta integration and acoustic sub-stepping)
        call     solve1(nstep,rbufsz,num_soil_layers,                  &
                   dt,dtlast,mtime,dbldt,mass1,mass2,                 &
                   dosfcflx,cloudvar,rhovar,qmag,bud,bud2,qbudget,asq,bsq, &
                   xh,rxh,arh1,arh2,uh,ruh,xf,rxf,arf1,arf2,uf,ruf,   &
                   yh,vh,rvh,yf,vf,rvf,                               &
                   xfref,xhref,yfref,yhref,dumk1,dumk2,rds,sigma,rdsf,sigmaf, &
                   tauh,taus,zh,mh,rmh,c1,c2,tauf,zf,mf,rmf,          &
                   rho0s,pi0s,prs0s,rth0s,                            &
                   wprof,ufrc,vfrc,thfrc,qvfrc,ug,vg,dvdr,            &
                   uavg,vavg,thavg,pavg,ulspg,vlspg,qavg,cavg,        &
                   pi0,rho0,prs0,thv0,th0,rth0,qv0,qc0,               &
                   qi0,rr0,rf0,rrf0,thrd,                             &
                   zs,gz,rgz,gzu,rgzu,gzv,rgzv,dzdx,dzdy,gx,gxu,gy,gyv, &
                   rain,sws,svs,sps,srs,sgs,sus,shs,                  &
                   tsk,thflux,qvflux,cd,ch,cq,u1,v1,s1,tlh,f2d,prate, &
                   radbcw,radbce,radbcs,radbcn,dtu,dtu0,dtv,dtv0,     &
                   dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,           &
                   divx,rho,rr,rf,prs,                                &
                   t11,t12,t13,t22,t23,t33,                           &
                   m11,m12,m13,m22,m23,m33,                           &
                   u0,rru,ua,u3d,uten,uten1,                          &
                   v0,rrv,va,v3d,vten,vten1,                          &
                   rrw,wa,w3d,wten,wten1,                             &
                   ppi,pp3d,ppten,sten,sadv,ppx,phi1,phi2,            &
                   tha,th3d,thten,thten1,thterm,                      &
                   qpten,qtten,qvten,qcten,qa,q3d,qten,               &
                   kmh,kmv,khh,khv,tkea,tke3d,tketen,                 &
                   nm,defv,defh,lenscl,dissten,epst,epsd1,epsd2,      &
                   thpten,qvpten,qcpten,qipten,upten,vpten,qnipten,qncpten,o30,zir,   &
                   swten,lwten,effc,effi,effs,effr,effg,effis,        &
                   lu_index,kpbl2d,psfc,u10,v10,s10,hfx,qfx,xland,znt,ust,   &
                   hpbl,wspd,psim,psih,gz1oz0,br,                     &
                   CHS,CHS2,CQS2,CPMM,ZOL,MAVAIL,                     &
                   MOL,RMOL,REGIME,LH,FLHC,FLQC,QGH,                  &
                   CK,CKA,CDA,USTM,QSFC,T2,Q2,TH2,EMISS,THC,ALBD,     &
                   gsw,glw,chklowq,capg,snowc,dsxy,wstar,delta,fm,fh,zntmp,   &
                   slab_zs,slab_dzs,tslb,tmn,tml,t0ml,hml,h0ml,huml,hvml,tmoml,        &
                   pta,pt3d,ptten,pdata,                              &
                   cfb,cfa,cfc, d1, d2,pdt,lgbth,lgbph,rhs,trans,flag,  &
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


        ! solve2: RK loop and pressure solver
        ! (advection, buoyancy, pressure gradient)
        call     solve2(nstep,rbufsz,num_soil_layers,                  &
                   dt,dtlast,mtime,dbldt,mass1,mass2,                 &
                   dosfcflx,cloudvar,rhovar,qmag,bud,bud2,qbudget,asq,bsq, &
                   xh,rxh,arh1,arh2,uh,ruh,xf,rxf,arf1,arf2,uf,ruf,   &
                   yh,vh,rvh,yf,vf,rvf,                               &
                   xfref,xhref,yfref,yhref,dumk1,dumk2,rds,sigma,rdsf,sigmaf, &
                   tauh,taus,zh,mh,rmh,c1,c2,tauf,zf,mf,rmf,          &
                   rho0s,pi0s,prs0s,rth0s,                            &
                   wprof,ufrc,vfrc,thfrc,qvfrc,ug,vg,dvdr,            &
                   uavg,vavg,thavg,pavg,ulspg,vlspg,qavg,cavg,        &
                   pi0,rho0,prs0,thv0,th0,rth0,qv0,qc0,               &
                   qi0,rr0,rf0,rrf0,thrd,                             &
                   zs,gz,rgz,gzu,rgzu,gzv,rgzv,dzdx,dzdy,gx,gxu,gy,gyv, &
                   rain,sws,svs,sps,srs,sgs,sus,shs,                  &
                   tsk,thflux,qvflux,cd,ch,cq,u1,v1,s1,tlh,f2d,prate, &
                   radbcw,radbce,radbcs,radbcn,dtu,dtu0,dtv,dtv0,     &
                   dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9,      &
                   divx,rho,rr,rf,prs,                                &
                   t11,t12,t13,t22,t23,t33,                           &
                   m11,m12,m13,m22,m23,m33,                           &
                   u0,rru,ua,u3d,uten,uten1,                          &
                   v0,rrv,va,v3d,vten,vten1,                          &
                   rrw,wa,w3d,wten,wten1,                             &
                   ppi,pp3d,ppten,sten,sadv,ppx,phi1,phi2,            &
                   tha,th3d,thten,thten1,thterm,                      &
                   qpten,qtten,qvten,qcten,qa,q3d,qten,               &
                   kmh,kmv,khh,khv,tkea,tke3d,tketen,                 &
                   nm,defv,defh,lenscl,dissten,epst,epsd1,epsd2,      &
                   qke_adv,qke,qke3d,                                 &
                   thpten,qvpten,qcpten,qipten,upten,vpten,qnipten,qncpten,o30,zir,   &
                   swten,lwten,effc,effi,effs,effr,effg,effis,        &
                   lu_index,kpbl2d,psfc,u10,v10,s10,hfx,qfx,xland,znt,ust,   &
                   hpbl,wspd,psim,psih,gz1oz0,br,                     &
                   CHS,CHS2,CQS2,CPMM,ZOL,MAVAIL,                     &
                   MOL,RMOL,REGIME,LH,FLHC,FLQC,QGH,                  &
                   CK,CKA,CDA,USTM,QSFC,T2,Q2,TH2,EMISS,THC,ALBD,     &
                   gsw,glw,chklowq,capg,snowc,dsxy,wstar,delta,fm,fh,zntmp,   &
                   slab_zs,slab_dzs,tslb,tmn,tml,t0ml,hml,h0ml,huml,hvml,tmoml,        &
                   pta,pt3d,ptten,pdata,                              &
                   cfb,cfa,cfc, d1, d2,pdt,lgbth,lgbph,rhs,trans,flag,  &
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


      IF(imoist.eq.1)THEN

        ! microphysics:

        ! cm1r20:  move calls to microphysics scheme into mp_driver.F

        call   mp_driver(nstep,dt,qbudget,asq,bsq,xh,ruh,xf,ruf,yh,rvh,yf,rvf,  &
                         mh,rmh,c1,c2,zh,mf,rmf,zf,rain,prate,pi0,th0,rho0,prs0,qv0,   &
                         rho,prs,dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,  &
                         w3d,ppi,pp3d,ppten,sten,tha,th3d,thten,qa,q3d,qten,   &
                         p3a,p3o,dum2d1,dum2d2,dum2d3,dum2d4,dum2d5,  &
                         effc,effi,effs,effr,effg,effis,    &
                         tdiag,qdiag,out2d,out3d,  &
                         dowriteout,dorad,dotdwrite,doazimwrite,dorestart,  &
                         getdbz,getvt,dotbud,doqbud)

      ENDIF


        ! solve3: finish primary dynamical solver
        ! (finish comms, update arrays, move surface)
        call     solve3(nstep,rbufsz,num_soil_layers,                  &
                   dt,dtlast,mtime,dbldt,mass1,mass2,                 &
                   dosfcflx,cloudvar,rhovar,bud,bud2,qbudget,asq,bsq, &
                   xh,rxh,arh1,arh2,uh,ruh,xf,rxf,arf1,arf2,uf,ruf,   &
                   yh,vh,rvh,yf,vf,rvf,                               &
                   xfref,yfref,dumk1,dumk2,rds,sigma,rdsf,sigmaf,     &
                   tauh,taus,zh,mh,rmh,c1,c2,tauf,zf,mf,rmf,          &
                   rho0s,pi0s,prs0s,rth0s,                            &
                   wprof,ufrc,vfrc,thfrc,qvfrc,ug,vg,dvdr,            &
                   uavg,vavg,thavg,pavg,ulspg,vlspg,qavg,cavg,        &
                   pi0,rho0,prs0,thv0,th0,rth0,qv0,qc0,               &
                   qi0,rr0,rf0,rrf0,thrd,                             &
                   zs,gz,rgz,gzu,rgzu,gzv,rgzv,dzdx,dzdy,gx,gxu,gy,gyv, &
                   rain,sws,svs,sps,srs,sgs,sus,shs,                  &
                   tsk,thflux,qvflux,cd,ch,cq,u1,v1,s1,tlh,f2d,prate, &
                   radbcw,radbce,radbcs,radbcn,dtu,dtu0,dtv,dtv0,     &
                   dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,           &
                   divx,rho,rr,rf,prs,                                &
                   t11,t12,t13,t22,t23,t33,                           &
                   m11,m12,m13,m22,m23,m33,                           &
                   u0,rru,ua,u3d,uten,uten1,                          &
                   v0,rrv,va,v3d,vten,vten1,                          &
                   rrw,wa,w3d,wten,wten1,                             &
                   ppi,pp3d,ppten,sten,sadv,ppx,phi1,phi2,            &
                   tha,th3d,thten,thten1,thterm,                      &
                   qpten,qtten,qvten,qcten,qa,q3d,qten,               &
                   kmh,kmv,khh,khv,tkea,tke3d,tketen,                 &
                   nm,defv,defh,lenscl,dissten,epst,epsd1,epsd2,      &
                   thpten,qvpten,qcpten,qipten,upten,vpten,qnipten,qncpten,o30,zir,   &
                   swten,lwten,effc,effi,effs,effr,effg,effis,        &
                   lu_index,kpbl2d,psfc,u10,v10,s10,hfx,qfx,xland,znt,ust,   &
                   hpbl,wspd,psim,psih,gz1oz0,br,                     &
                   CHS,CHS2,CQS2,CPMM,ZOL,MAVAIL,                     &
                   MOL,RMOL,REGIME,LH,FLHC,FLQC,QGH,                  &
                   CK,CKA,CDA,USTM,QSFC,T2,Q2,TH2,EMISS,THC,ALBD,     &
                   gsw,glw,chklowq,capg,snowc,dsxy,wstar,delta,fm,fh,zntmp,   &
                   slab_zs,slab_dzs,tslb,tmn,tml,t0ml,hml,h0ml,huml,hvml,tmoml,        &
                   pta,pt3d,ptten,pdata,                              &
                   cfb,cfa,cfc, d1, d2,pdt,lgbth,lgbph,rhs,trans,flag,  &
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
                   lsnudge_u,lsnudge_v,lsnudge_th,lsnudge_qv,        &
                   bndy,kbdy,hflxw,hflxe,hflxs,hflxn,                &
                   dowriteout,dorad,getdbz,getvt,dotdwrite,          &
                   dotbud,doqbud,doubud,dovbud,dowbud,donudge,       &
                   doazimwrite,dorestart)
        ! end_solve3


        !--------------------------------
        ! Step model time forward one dt
        !   (NOTE:  do not change mtime anywhere else!)

        mtime = mtime + dbldt

        !--------------------------------

        if( stopit )then
          dostat      =  .true.
          dowriteout  =  .true.
        endif

        !--------------------------------

      endif  ifsolve


      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      !cc   Adaptive domain-moving option for TCs  ccccccccccccccccccccccccccc
      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ! Experimental (seems to work well, though)
        !
        ! Uses a smoothed field of pressure at the lowest model level to determine
        ! the position of a TC.

      IF( do_adapt_move )THEN

        IF( mtime.ge.adaptmovetim )then

          call   update_adapt_move(nstep,mtime,xh,yh,xfref,yfref,ug,vg,psmth,     &
                                   dum1,pp3d,u0,ua,u3d,v0,va,v3d,mvrec,nwritemv,  &
                                   icenter,jcenter,xcenter,ycenter)

          nwritemv = nwritemv+1
          adaptmovetim = adaptmovetim + adapt_move_frq

        ENDIF

      ENDIF


      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      !cc   radiation  ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      IF( radopt.ge.1 .and. dorad )THEN

        call     radiation_driver(mtime,radtim,dt,rbufsz,xh,yh,xf,yf,zf,rmh,c1,c2,     &
                   swten,lwten,swtenc,lwtenc,cldfra,o30,                               &
                   radsw,rnflx,radswnet,radlwin,dsr,olr,rad2d,                         &
                   effc,effi,effs,effr,effg,effis,                                     &
                   lwupt,lwuptc,lwdnt,lwdntc,lwupb,lwupbc,lwdnb,lwdnbc,                &
                   swupt,swuptc,swdnt,swdntc,swupb,swupbc,swdnb,swdnbc,                &
                   lwcf,swcf,coszr,                                                    &
                   xice,xsnow,xlat,xlong,coszen,swddir,swddni,swddif,hrang,            &
                   cldfra1_flag,dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,               &
                   prs0,pi0,th0,prs,ppi,tha,rho,qa,                                    &
                   rth0s,prs0s,rho0s,tsk,albd,glw,gsw,emiss,xland,nstep,               &
                   qc_bl,qi_bl,cldfra_bl)

        doit = .true.
        do while( doit )
          radtim = radtim+dtrad
          if( radtim.gt.mtime )then
            doit = .false.
          endif
        enddo
        if(timestats.ge.1) time_rad=time_rad+mytime()

      ENDIF


      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      !cc   Prepare turbulence vars for next time step   cccccccccccccccccc
      !cc     (new since cm1r17)                         cccccccccccccccccc
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      rtime=sngl(mtime)

      getsfc = .true.
      if( restarted ) getsfc = .false.

      getpbl = .true.
      if( restarted .and. ipbl.ge.1 ) getpbl = .false.

      update_sfc = .true.
      if( startup .or. restarted ) update_sfc = .false.

      call sfc_and_turb(getsfc,getpbl,nstep,dt,dosfcflx,cloudvar,qbudget,   &
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
                   u0,rru,u3d,uten,uten1,                            &
                   v0,rrv,v3d,vten,vten1,                            &
                   rrw,w3d,wten ,wten1,                              &
                   pp3d,ppten,sten,sadv,                             &
                   th3d,thten,thten1,thterm,q3d,                     &
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
                   flag  ,out2d,out3d,rtime,update_sfc,dotbud,dotdwrite,restarted,  &
                   dowriteout,doazimwrite)
        ! end_sfc_and_turb


      IF( (idiff.ge.1).and.(difforder.eq.2) )THEN
        !  get stress terms for explicit diffusion scheme:
        call diff2def(uh,arh1,arh2,uf,arf1,arf2,vh,vf,mh,c1,c2,mf,ust,zntmp,u1,v1,s1,  &
                      divx,rho,rr,rf,t11,t12,t13,t22,t23,t33,u3d,v3d,w3d,dissten)
      ENDIF

#ifdef MPI
      if( cm1setup.ge.1 .and. adapt_dt.eq.1 )then
        call mpi_wait(reqk,mpi_status_ignore,ierr)
        if(timestats.ge.1) time_cflq=time_cflq+mytime()
      endif
#endif

      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!


        !--------------------------------
        !  Adaptive timestepping:
        !   (assumes cflmax and ksmax have already been calculated)

        IF( adapt_dt.eq.1 .and. dosolve )THEN

          call   getnewdt(ndt,dt,dtlast,adt,acfl,dbldt,                                 &
                          mtime,stattim,taptim,rsttim,prcltim,diagtim,azimavgtim,       &
                          dorestart,dowriteout,dostat,doprclout,dotdwrite,doazimwrite,  &
                          hifrqtim,dohifrqwrite,.false.)

        ENDIF


      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!


      rtime=sngl(mtime)
      reset = .false.


      if( dostat )then
        if( startup )then
          if(dowr) write(outfile,*)
          if(dowr) write(outfile,*) '  initial conditions:'
          if(dowr) write(outfile,*)
        endif
        IF(axisymm.eq.0)THEN
          ! for Cartesian grid:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            ppten(i,j,k)=rho(i,j,k)
          enddo
          enddo
          enddo
        ELSE
          ! for axisymmetric grid:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            ppten(i,j,k) = rho(i,j,k)*pi*(xf(i+1)**2-xf(i)**2)/(dx*dy)
          enddo
          enddo
          enddo
        ENDIF
        call     statpack(mtime,nrec,ndt,dt,dtlast,rtime,adt,acfl,cloudvar,   &
                          qname,budname,qbudget,asq,bsq,                      &
                          name_stat,desc_stat,unit_stat,                      &
                          xh,rxh,uh,ruh,xf,uf,yh,vh,rvh,vf,zh,mh,rmh,zf,mf,   &
                          zs,rgzu,rgzv,rds,sigma,rdsf,sigmaf,                 &
                          rstat,pi0,rho0,thv0,th0,qv0,u0,v0,                  &
                          dum1,dum2,dum3,dum4,dum5,ppten,prs,                 &
                          ua,va,wa,ppi,tha,qa,qten,kmh,kmv,khh,khv,tkea,qke,  &
                          tke_myj,xkzh,xkzq,xkzm,                             &
                          pta,u10,v10,hpbl,prate,reset,nstatout)
        nrec = nrec + 1
        nstatout = nstatout + 1
        if( statfrq.gt.0.0 .and. ( .not. restarted ) )then
          doit = .true.
          do while( doit )
            stattim = stattim+statfrq
            if( stattim.gt.mtime )then
              doit = .false.
            endif
          enddo
        endif
        if(myid.eq.0) print *,'  next stattim = ',stattim
        if(timestats.ge.1) time_stat=time_stat+mytime()
      endif


      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!


        if(myid.eq.0)then
          if(timeformat.eq.1)then
            write(6,110) nstep,rtime,' sec '
          elseif(timeformat.eq.2)then
            write(6,110) nstep,rtime/60.0,' min '
          elseif(timeformat.eq.3)then
            write(6,110) nstep,rtime/3600.0,' hour'
          elseif(timeformat.eq.4)then
            write(6,110) nstep,rtime/86400.0,' day '
          else
            write(6,110) nstep,rtime,' sec'
          endif
110       format(2x,i12,4x,f18.6,a5)
        endif


      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!


      if( dowriteout )then
      otype:  &
      IF(output_format.eq.1.or.output_format.eq.2)THEN
        nn = 1
        if(terrain_flag .and. output_interp.eq.1) nn = 2
        DO n=1,nn
          if(n.eq.1)then
            fnum = 51
            frec = srec
          else
            fnum = 71
            frec = sirec
          endif
          IF( stopit )THEN
            ! diag code does not account for stopit, so just set arrays to zero
            ! to avoid confusion
            tdiag = 0.0
            qdiag = 0.0
            udiag = 0.0
            vdiag = 0.0
            wdiag = 0.0
            kdiag = 0.0
          ENDIF
          call          writeout(frec,urec,vrec,wrec,mrec,rtime,dt,fnum,nwrite,qname,qunit,    &
                        nstep,mtime,adt,ndt,                                                   &
                        name_output,desc_output,unit_output,grid_output,cmpr_output,           &
                        xh,xf,uf,yh,yf,vf,xfref,yfref,                                         &
                        rds,sigma,rdsf,sigmaf,zh,zf,mf,gx,gy,wprof,                            &
                        pi0,prs0,rho0,rr0,rf0,rrf0,th0,qv0,u0,v0,thv0,rth0,qc0,qi0,            &
                        zs,rgzu,rgzv,rain,sws,svs,sps,srs,sgs,sus,shs,thflux,qvflux,psfc,      &
                        rxh,arh1,arh2,uh,ruh,rxf,arf1,arf2,vh,rvh,mh,rmh,rmf,rr,rf,            &
                        gz,rgz,gzu,gzv,gxu,gyv,dzdx,dzdy,c1,c2,                                &
                        cd,ch,cq,tlh,f2d,psmth,prate,ustt,                                     &
                        dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9,                          &
                        t11,t12,t13,t22,t23,t33,rho,prs,divx,                                  &
                        rru,u3d,uten,uten1,rrv,v3d,vten,vten1,rrw,w3d,wten,pp3d,th3d,phi2,     &
                        sadv,thten,nm,defv,defh,lenscl,dissten,epst,epsd1,epsd2,               &
                        thpten,qvpten,qcpten,qipten,upten,vpten,qnipten,qncpten,xkzh,xkzq,xkzm, &
                        lu_index,xland,mavail,tsk,tmn,tml,hml,huml,hvml,hfx,qfx,gsw,glw,tslb,  &
                        q3d,p3a,p3o,kmh,kmv,khh,khv,tke3d,swten,lwten,cldfra,                  &
                        radsw,rnflx,radswnet,radlwin,dsr,olr,pta,                              &
                        effc,effi,effs,effr,effg,effis,                                        &
                        lwupt,lwdnt,lwupb,lwdnb,                                               &
                        swupt,swdnt,swupb,swdnb,lwcf,swcf,coszen,                              &
                        num_soil_layers,u10,v10,s10,t2,q2,znt,ust,stau,tst,qst,z0t,z0q,u1,v1,s1,     &
                        hpbl,zol,mol,rmol,br,brcr,wscale,wscaleu,phim,phih,psim,psih,psiq,wspd,qsfc,wstar,delta,prkpp,fm,fh, &
                        mznt,taux,tauy,                                                        &
                        z0base,thz0,qz0,uz0,vz0,u10e,v10e,tke_myj,el_myj,  &
                        tsq,qsq,cov,sh3d,el_pbl,qc_bl,qi_bl,cldfra_bl,                         &
                        qWT,qSHEAR,qBUOY,qDISS,dqke,qke_adv,qke,                               &
                        edmf_a,edmf_w,edmf_qt,edmf_thl,edmf_ent,edmf_qc,                       &
                        vdfg,maxmf,nupdraft,ktop_plume,                                        &
                        dat1,dat2,dat3,reqt,thten1(ib,jb,1),dumk1,dumk2,                       &
                        tdiag,qdiag,udiag,vdiag,wdiag,pdiag,out2d,out3d,                       &
                        bndy,kbdy,hflxw,hflxe,hflxs,hflxn,                                     &
                        nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,myi1p,myi2p,myj1p,myj2p)
                        ! end_writeout
          if(n.eq.1)then
            srec = frec
          else
            sirec = frec
          endif
        ENDDO
      ELSE  otype
        print *,'  09832 '
        call stopcm1
      ENDIF  otype
        nwrite=nwrite+1
        if(tapfrq.gt.0.0)then
          doit = .true.
          do while( doit )
            taptim = taptim+tapfrq
            if( taptim.gt.mtime )then
              doit = .false.
            endif
          enddo
        endif
        if(myid.eq.0) print *,'  next taptim = ',taptim
        if(timestats.ge.1) time_write=time_write+mytime()
      endif


      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!


      if(iprcl.eq.1)then
      IF( doprclout )THEN
        call     parcel_interp(dt,mtime,xh,uh,ruh,xf,uf,yh,vh,rvh,yf,vf, &
                               zh,mh,rmh,zf,mf,zntmp,ust,c1,c2,        &
                               zs,sigma,sigmaf,rds,gz,                 &
                               pi0,th0,thv0,qv0,qc0,qi0,rth0,          &
                               dum1,dum2,dum3,dum4,dum5,dum6,prs,rho,  &
                               dum7,dum8,wten,wten1,                   &
                               u3d,v3d,w3d,pp3d,thten,thten1,th3d,q3d, &
                               kmh,kmv,khh,khv,tke3d,pt3d,pdata,       &
                               tdiag,qdiag,                            &
                               pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,        &
                               nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,reqs_p, &
                               tkw1,tkw2,tke1,tke2,tks1,tks2,tkn1,tkn2)
        call     parcel_write(prec,rtime,qname,name_prcl,desc_prcl,unit_prcl,pdata,ploc)
        prec = prec+1
        if(prclfrq.gt.0.0)then
          doit = .true.
          do while( doit )
            prcltim = prcltim+prclfrq
            if( prcltim.gt.mtime )then
              doit = .false.
            endif
          enddo
        endif
        if(myid.eq.0) print *,'  next prcltim = ',prcltim
        if(timestats.ge.1) time_parcels=time_parcels+mytime()
      ELSE
        if( startup )then
          if(myid.eq.0) print *
          if(myid.eq.0) print *,'  NOTE:  skipping parcel_write '
          if(myid.eq.0) print *
        endif
      ENDIF
      endif


      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!

    IF( dodomaindiag )THEN
      if( dotdwrite )then

        call   domaindiag(nstep,mtime,nwritet,trecs,trecw,qname,qunit,dt,dosfcflx, &
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
                   dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,                    &
                   divx,rho,rr,rf,prs,t11,t12,t13,t22,t23,t33,                 &
                   m11,m12,m13,m22,m23,m33,                                    &
                   rru,u3d,uten,uten1,                                         &
                   rrv,v3d,vten,vten1,                                         &
                   rrw,w3d,wten,wten1,                                         &
                   pp3d,sadv,ppten ,sten,th3d,thten,thten1,thterm,             &
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
        nwritet = nwritet+1
        if(diagfrq.gt.0.0)then
          doit = .true.
          do while( doit )
            diagtim = diagtim+diagfrq
            if( diagtim.gt.mtime )then
              doit = .false.
            endif
          enddo
        endif
        if(myid.eq.0) print *,'  next diagtim = ',diagtim
        if(timestats.ge.1) time_diag = time_diag+mytime()

      endif
    ENDIF


      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!


    IF( doazimavg )THEN
      if( doazimwrite )then

        call   azimavg(nstep,mtime,nwritea,arecs,arecw,qname,dt,dosfcflx,      &
                   icrs,icenter,jcenter,xcenter,ycenter,                       &
                   xh,rxh,arh1,arh2,uh,ruh,xf,rxf,arf1,arf2,uf,ruf,            &
                   yh,vh,rvh,yf,vf,rvf,                                        &
                   xfref,yfref,rds,sigma,rdsf,sigmaf,                          &
                   tauh,taus,zh,mh,rmh,c1,c2,tauf,zf,mf,rmf,                   &
                   rho0s,pi0s,prs0s,rth0s,                                     &
                   pi0,rho0,prs0,thv0,th0,rth0,qv0,qc0,u0,v0,                  &
                   qi0,rr0,rf0,rrf0,                                           &
                   zs,gz,rgz,gzu,rgzu,gzv,rgzv,dzdx,dzdy,gx,gxu,gy,gyv,        &
                   tsk,znt,ust,thflux,qvflux,cd,ch,cq,u1,v1,s1,xland,psfc,tlh, &
                   dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,                    &
                   divx,rho,rf,prs,t11,t12,t13,t22,t23,t33,                    &
                   rru,u3d,uten,uten1,                                         &
                   rrv,v3d,vten,vten1,                                         &
                   rrw,w3d,wten,wten1,                                         &
                   pp3d,ppten,sten,th3d,sadv,thten,thten1,                     &
                   q3d,qten,kmh,kmv,khh,khv,tkea,tke3d,tketen,                 &
                   nm,defv,defh,lenscl,dissten,                                &
                   thpten,qvpten,qcpten,qipten,upten,vpten,xkzh,xkzq,xkzm,     &
                   rain,u10,v10,s10,br,brcr,hpbl,prate,                        &
                   swten,lwten,cldfra,qke,qke_adv,                             &
                   tdiag,qdiag,udiag,vdiag,wdiag,pdiag,out2d,out3d,getdbz,getvt, &
                   dat1(1,1),dat2(1,1),dat3(1,1,1),                            &
                   sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,flag)
        ! end_azimavg
        nwritea = nwritea+1
        if(azimavgfrq.gt.0.0)then
          doit = .true.
          do while( doit )
            azimavgtim = azimavgtim+azimavgfrq
            if( azimavgtim.gt.mtime )then
              doit = .false.
            endif
          enddo
        endif
        if(myid.eq.0) print *,'  next azimavgtim = ',azimavgtim
        if(timestats.ge.1) time_azimavg = time_azimavg+mytime()

      endif
    ENDIF


      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!


    IF( dohifrq )THEN
      if( dohifrqwrite )then

        call   writeout_hifrq(                                                 &
                   nstep,mtime,nwriteh,qname,dt,dosfcflx,                      &
                   icrs,icenter,jcenter,xcenter,ycenter,                       &
                   xh,rxh,arh1,arh2,uh,ruh,xf,rxf,arf1,arf2,uf,ruf,            &
                   yh,vh,rvh,yf,vf,rvf,                                        &
                   xfref,xhref,yfref,yhref,rds,sigma,rdsf,sigmaf,              &
                   tauh,taus,zh,mh,rmh,c1,c2,tauf,zf,mf,rmf,                   &
                   rho0s,pi0s,prs0s,rth0s,                                     &
                   pi0,rho0,prs0,thv0,th0,rth0,qv0,qc0,u0,v0,                  &
                   qi0,rr0,rf0,rrf0,                                           &
                   zs,gz,rgz,gzu,rgzu,gzv,rgzv,dzdx,dzdy,gx,gxu,gy,gyv,        &
                   tsk,znt,ust,stau,thflux,qvflux,cd,ch,cq,u1,v1,s1,xland,psfc,tlh, &
                   dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,                    &
                   divx,rho,rr,rf,prs,t11,t12,t13,t22,t23,t33,                 &
                   rru,u3d,uten,uten1,                                         &
                   rrv,v3d,vten,vten1,                                         &
                   rrw,w3d,wten,wten1,                                         &
                   pp3d,ppten,sten,th3d,sadv,thten,thten1,                     &
                   q3d,qten,kmh,kmv,khh,khv,tkea,tke3d,tketen,                 &
                   nm,defv,defh,dissten,                                       &
                   thpten,qvpten,qcpten,qipten,upten,vpten,xkzh,xkzq,xkzm,     &
                   rain,u10,v10,s10,br,brcr,hpbl,t2,q2,hfx,qfx,prate,          &
                   swten,lwten,cldfra,                                         &
                   lwupt,lwuptc,lwdnt,lwdntc,lwupb,lwupbc,lwdnb,lwdnbc,        &
                   swupt,swuptc,swdnt,swdntc,swupb,swupbc,swdnb,swdnbc,        &
                   lwcf,swcf,wprof,dumk1,dumk2,                                &
                   tdiag,qdiag,udiag,vdiag,wdiag,pdiag,out2d,out3d,getdbz,getvt, &
                   myi1p,myi2p,myj1p,myj2p,kbdy,bndy,                          &
                   sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,flag,               &
                   dat1(1,1),dat2(1,1),dat3(1,1,1),reqt,reqs_s)
        ! end hifrq

        nwriteh = nwriteh+1
        if(hifrqfrq.gt.0.0)then
          doit = .true.
          do while( doit )
            hifrqtim = hifrqtim+hifrqfrq
            if( hifrqtim.gt.mtime )then
              doit = .false.
            endif
          enddo
        endif
        if(myid.eq.0) print *,'  next hifrqtim = ',hifrqtim
        if(timestats.ge.1) time_hifrq = time_hifrq+mytime()

      endif
    ENDIF


      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!


      if( dorestart )then
        if(rstfrq.gt.0.0)then
          doit = .true.
          do while( doit )
            rsttim = rsttim+rstfrq
            if( rsttim.gt.mtime )then
              doit = .false.
            endif
          enddo
        endif
        call     write_restart(nstep,srec,sirec,urec,vrec,wrec,nrec,mrec,prec,      &
                               trecs,trecw,arecs,arecw,                             &
                               nwrite,nwritet,nwritea,nwriteh,nrst,nstatout,        &
                               num_soil_layers,nrad2d,                              &
                               avgsfcu,avgsfcv,avgsfcs,avgsfct,                     &
                               dt,dtlast,mtime,ndt,adt,acfl,dbldt,mass1,            &
                               stattim,taptim,rsttim,radtim,prcltim,                &
                               qbudget,asq,bsq,qname,                               &
                               xfref,yfref,zh,zf,sigma,sigmaf,zs,                   &
                               th0,prs0,pi0,rho0,qv0,u0,v0,                         &
                               rain,sws,svs,sps,srs,sgs,sus,shs,                    &
                               tsk,znt,ust,cd,ch,cq,u1,v1,s1,t1,thflux,qvflux,      &
                               prate,ustt,ut,vt,st,                                 &
                               radbcw,radbce,radbcs,radbcn,                         &
                               rho,prs,ua,va,wa,ppi,tha,qa,tkea,                    &
                               swten,lwten,radsw,rnflx,radswnet,radlwin,rad2d,      &
                               effc,effi,effs,effr,effg,effis,                      &
                               lu_index,kpbl2d,psfc,u10,v10,s10,hfx,qfx,xland,      &
                               hpbl,wspd,psim,psih,gz1oz0,br,                       &
                               CHS,CHS2,CQS2,CPMM,ZOL,MAVAIL,                       &
                               MOL,RMOL,REGIME,LH,FLHC,FLQC,QGH,                    &
                               CK,CKA,CDA,USTM,QSFC,T2,Q2,TH2,EMISS,THC,ALBD,       &
                               gsw,glw,chklowq,capg,snowc,fm,fh,mznt,swspd,wstar,delta,tslb,    &
                               tmn,tml,t0ml,hml,h0ml,huml,hvml,tmoml,               &
                               qpten,qtten,qvten,qcten,pta,pdata,ploc,ppx,          &
                               tdiag,qdiag,phi1,phi2,                               &
                   tsq,qsq,cov,sh3d,el_pbl,qc_bl,qi_bl,cldfra_bl,            &
                   qWT,qSHEAR,qBUOY,qDISS,dqke,qke_adv,qke,qke3d,            &
                   edmf_a,edmf_w,edmf_qt,edmf_thl,edmf_ent,edmf_qc,          &
                   sub_thl3D,sub_sqv3D,det_thl3D,det_sqv3D,                  &
                   vdfg,maxmf,nupdraft,ktop_plume,                           &
                               tke_myj,el_myj,mixht,akhs,akms,elflx,ct,snow,sice,thz0,qz0,uz0,vz0,th10,q10,z0base,zntmyj,lowlyr,ivgtyp, &
                               thpten,qvpten,qcpten,qipten,upten,vpten,qnipten,qncpten, &
                               icenter,jcenter,xcenter,ycenter,adaptmovetim,mvrec,nwritemv,ug,vg, &
                               gamk,kmw,u1b,v1b,cmz,csz,ce1z,ce2z,                  &
                               dum1,dat1,dat2,dat3,reqt,myi1p,myi2p,myj1p,myj2p)
          ! end_write_restart
        if(myid.eq.0) print *,'  next rsttim = ',rsttim
        IF( nrst.eq.0 )THEN
          ! 150820:  Write restart file if rstfrq=0 and stop
          !          (useful for ensemble DA)
          if(dowr) write(outfile,*)
          if(dowr) write(outfile,*) '  Detected rstfrq = 0 '
          if(dowr) write(outfile,*) '  ... writing restart file ... '
          if(dowr) write(outfile,*)
          if(dowr) write(outfile,*) '  ... stopping ... '
          if(dowr) write(outfile,*)
#ifdef MPI
          call MPI_BARRIER (MPI_COMM_WORLD,ierr)
#endif
          stop 55556
        ENDIF
        nrst = nrst+1
        if(timestats.ge.1) time_restart=time_restart+mytime()
      endif


      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!
      !cccccccccccccccccccccccccccccccccccccccccccccccccccccccc!

        if(timestats.eq.2)then
          steptime2=time_sound+time_poiss+time_buoyan+time_turb+            &
                    time_diffu+time_microphy+time_stat+time_cflq+           &
                    time_bc+time_misc+time_integ+time_rdamp+time_divx+      &
                    time_write+time_restart+time_ttend+time_cor+time_fall+  &
                    time_satadj+time_dbz+time_sfcphys+time_parcels+         &
                    time_rad+time_pbl+time_swath+time_pdef+time_prsrho+     &
                    time_diag+time_azimavg+time_hifrq+time_ercyl+           &
#ifdef MPI
                    time_mpu1+time_mpv1+time_mpw1+time_mpp1+                &
                    time_mpu2+time_mpv2+time_mpw2+time_mpp2+                &
                    time_mps1+time_mps3+time_mpq1+time_mptk1+                         &
                    time_mps2+time_mps4+time_mpq2+time_mptk2+time_mpb+                &
#endif
                    time_advs+time_advu+time_advv+time_advw
          write(6,157) nstep,steptime2-steptime1
157       format('    timing for time step ',i12,':',f12.4,' s')
          steptime1 = steptime2
        endif

        !--------------------------------------------------------------------

        if( startup )then
          if(iconly.eq.1)then
            if(dowr) write(outfile,*)
            if(dowr) write(outfile,*) '  User has requested initial conditions only'
            if(dowr) write(outfile,*) '     (iconly = 1)'
            if(dowr) write(outfile,*) '  ... stopping ... '
            if(dowr) write(outfile,*)
#ifdef MPI
            call MPI_BARRIER (MPI_COMM_WORLD,ierr)
#endif
            stop 55555
          endif
        endif

        !--------------------------------------------------------------------
        ! Time step complete.  Odds and ends:

        startup = .false.
        dorestart = .false.

        dosolve     = .true.
        dorad       = .false.
        dostat      = .false.
        dowriteout  = .false.
        doprclout   = .false.
        dotdwrite   = .false.
        doazimwrite = .false.
        dohifrqwrite = .false.

      !-------------------------------------


      IF( adapt_dt.eq.1 .and. reset )THEN
        ndt  = 0
        adt  = 0.0
        acfl = 0.0
      ENDIF

        if( convinit.eq.1 )then
          if( mtime.gt.convtime ) convinit = 0
        endif

      if(stopit)then
        if(myid.eq.0)then
          print *
          print *,' Courant number has exceeded 1.5 '
          print *
          print *,' Stopping model .... '
          print *
        endif
#ifdef MPI
        call MPI_BARRIER (MPI_COMM_WORLD,ierr)
#endif
        call stopcm1
      endif


      !-------------------------------------


      startup = .false.
      restarted = .false.


    ENDDO  timeloop
    !c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c-c!


!----------------------------------------------------------------------

#ifdef MPI
      call MPI_BARRIER (MPI_COMM_WORLD,ierr)
      if(timestats.ge.1) time_mpb=time_mpb+mytime()

      if(myid.eq.0.and.procfiles)then
        tend=mpi_wtime()
        print *
        print *,'Total time (s): ',tend-tstart
        print *
      endif

#endif
!----------------------------------------------------------------------

    IF(timestats.ge.1)THEN

#ifdef MPI
      ! for MPI runs without procfiles, average the timestat terms:
      IF(.not.procfiles)THEN
        sum = 0.0
        call MPI_REDUCE(time_sound   ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_sound = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_poiss   ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_poiss = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_advs    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_advs = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_advu    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_advu = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_advv    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_advv = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_advw    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_advw = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_divx    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_divx = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_buoyan  ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_buoyan = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_turb    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_turb = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_sfcphys ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_sfcphys = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_ttend   ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_ttend = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_cor     ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_cor = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_diffu   ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_diffu = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_rdamp   ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_rdamp = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_microphy,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_microphy = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_satadj  ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_satadj = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_dbz,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_dbz = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_fall    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_fall = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_rad     ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_rad = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_pbl     ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_pbl = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_stat    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_stat = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_cflq    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_cflq = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_bc      ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_bc = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_integ   ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_integ = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_write   ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_write = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_restart ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_restart = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_misc    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_misc = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_swath   ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_swath = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_pdef    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_pdef = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_prsrho  ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_prsrho = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_diag,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_diag = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_azimavg ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_azimavg = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_hifrq ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_hifrq = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_ercyl ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_ercyl = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_parcels ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_parcels = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mpu1    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mpu1 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mpv1    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mpv1 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mpw1    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mpw1 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mpp1    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mpp1 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mpu2    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mpu2 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mpv2    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mpv2 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mpw2    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mpw2 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mpp2    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mpp2 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mps1    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mps1 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mps3    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mps3 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mpq1    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mpq1 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mps2    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mps2 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mps4    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mps4 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mpq2    ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mpq2 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mptk1   ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mptk1 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mptk2   ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mptk2 = sum/float(numprocs)
        sum = 0.0
        call MPI_REDUCE(time_mpb     ,sum,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
        time_mpb = sum/float(numprocs)
      ENDIF
#endif

      time_solve=time_sound+time_poiss+time_buoyan+time_turb+             &
                  time_diffu+time_microphy+time_stat+time_cflq+           &
                  time_bc+time_misc+time_integ+time_rdamp+time_divx+      &
                  time_write+time_restart+time_ttend+time_cor+time_fall+  &
                  time_satadj+time_dbz+time_sfcphys+time_parcels+         &
                  time_rad+time_pbl+time_swath+time_pdef+time_prsrho+     &
                  time_diag+time_azimavg+time_hifrq+time_ercyl+           &
#ifdef MPI
                  time_mpu1+time_mpv1+time_mpw1+time_mpp1+                &
                  time_mpu2+time_mpv2+time_mpw2+time_mpp2+                &
                  time_mps1+time_mps3+time_mpq1+time_mptk1+                         &
                  time_mps2+time_mps4+time_mpq2+time_mptk2+time_mpb+                &
#endif
                  time_advs+time_advu+time_advv+time_advw
      time_solve0 = time_solve

#ifdef MPI
      mp_total=time_mpu1+time_mpv1+time_mpw1+time_mpp1+                   &
               time_mpu2+time_mpv2+time_mpw2+time_mpp2+                   &
               time_mps1+time_mps3+time_mpq1+time_mptk1+                            &
               time_mps2+time_mps4+time_mpq2+time_mptk2+time_mpb
#endif

      if(dowr) write(outfile,*)
      if(dowr) write(outfile,*) 'Total time: ',time_solve
      if(dowr) write(outfile,*)
      time_solve=0.01*time_solve
      if(time_solve.lt.0.0001) time_solve=1.

    IF(dowr)THEN
      write(outfile,100) 'sound   ',time_sound,time_sound/time_solve
      write(outfile,100) 'poiss   ',time_poiss,time_poiss/time_solve
      write(outfile,100) 'advs    ',time_advs,time_advs/time_solve
      write(outfile,100) 'advu    ',time_advu,time_advu/time_solve
      write(outfile,100) 'advv    ',time_advv,time_advv/time_solve
      write(outfile,100) 'advw    ',time_advw,time_advw/time_solve
      write(outfile,100) 'divx    ',time_divx,time_divx/time_solve
      write(outfile,100) 'buoyan  ',time_buoyan,time_buoyan/time_solve
      write(outfile,100) 'turb    ',time_turb,time_turb/time_solve
      write(outfile,100) 'sfcphys ',time_sfcphys,time_sfcphys/time_solve
      write(outfile,100) 'ttend   ',time_ttend,time_ttend/time_solve
      write(outfile,100) 'cor     ',time_cor,time_cor/time_solve
      write(outfile,100) 'diffu   ',time_diffu,time_diffu/time_solve
      write(outfile,100) 'rdamp   ',time_rdamp,time_rdamp/time_solve
      write(outfile,100) 'microphy',time_microphy,time_microphy/time_solve
      write(outfile,100) 'satadj  ',time_satadj,time_satadj/time_solve
      write(outfile,100) 'dbz     ',time_dbz,time_dbz/time_solve
      write(outfile,100) 'fallout ',time_fall,time_fall/time_solve
      write(outfile,100) 'radiatio',time_rad,time_rad/time_solve
      write(outfile,100) 'pbl     ',time_pbl,time_pbl/time_solve
      write(outfile,100) 'stat    ',time_stat,time_stat/time_solve
      write(outfile,100) 'cflq    ',time_cflq,time_cflq/time_solve
      write(outfile,100) 'bc      ',time_bc,time_bc/time_solve
      write(outfile,100) 'integ   ',time_integ,time_integ/time_solve
      write(outfile,100) 'write   ',time_write,time_write/time_solve
      write(outfile,100) 'restart ',time_restart,time_restart/time_solve
      write(outfile,100) 'misc    ',time_misc,time_misc/time_solve
      write(outfile,100) 'swaths  ',time_swath,time_swath/time_solve
      write(outfile,100) 'pdef    ',time_pdef,time_pdef/time_solve
      write(outfile,100) 'prsrho  ',time_prsrho,time_prsrho/time_solve
      write(outfile,100) 'parcels ',time_parcels,time_parcels/time_solve
      if( dodomaindiag )  &
      write(outfile,100) 'domndiag',time_diag,time_diag/time_solve
      if( doazimavg )  &
      write(outfile,100) 'azimavg ',time_azimavg,time_azimavg/time_solve
      if( dohifrq )  &
      write(outfile,100) 'hifrq   ',time_hifrq,time_hifrq/time_solve
      if( do_recycle )  &
      write(outfile,100) 'recycle ',time_ercyl,time_ercyl/time_solve
#ifdef MPI
      write(outfile,100) 'mp_total',mp_total,mp_total/time_solve
      write(outfile,*)
      write(outfile,100) 'mpu1    ',time_mpu1,time_mpu1/time_solve
      write(outfile,100) 'mpu2    ',time_mpu2,time_mpu2/time_solve
      write(outfile,100) 'mpv1    ',time_mpv1,time_mpv1/time_solve
      write(outfile,100) 'mpv2    ',time_mpv2,time_mpv2/time_solve
      write(outfile,100) 'mpw1    ',time_mpw1,time_mpw1/time_solve
      write(outfile,100) 'mpw2    ',time_mpw2,time_mpw2/time_solve
      write(outfile,100) 'mpp1    ',time_mpp1,time_mpp1/time_solve
      write(outfile,100) 'mpp2    ',time_mpp2,time_mpp2/time_solve
      write(outfile,100) 'mps1    ',time_mps1,time_mps1/time_solve
      write(outfile,100) 'mps2    ',time_mps2,time_mps2/time_solve
      write(outfile,100) 'mps3    ',time_mps3,time_mps3/time_solve
      write(outfile,100) 'mps4    ',time_mps4,time_mps4/time_solve
      write(outfile,100) 'mpq1    ',time_mpq1,time_mpq1/time_solve
      write(outfile,100) 'mpq2    ',time_mpq2,time_mpq2/time_solve
      write(outfile,100) 'mptk1   ',time_mptk1,time_mptk1/time_solve
      write(outfile,100) 'mptk2   ',time_mptk2,time_mptk2/time_solve
      write(outfile,100) 'mpb     ',time_mpb,time_mpb/time_solve
#endif
      write(outfile,*)
    ENDIF

100   format(3x,a8,' :  ',f10.2,2x,f6.2,'%')

#ifdef MPI
      if(myid.eq.0) print *
      time_solve0 = max( time_solve0 , 1.0e-20 )
      if(myid.eq.0) print *,'  MN = ',float(nx*ny*nz)*float(nstep-nstep0)/((time_solve0/3600.0)*float(numprocs))
      if(myid.eq.0) print *
      call MPI_BARRIER (MPI_COMM_WORLD,ierr)
#endif

    ENDIF

!  End time loop
!----------------------------------------------------------------------

      close(unit=51)
      close(unit=52)
      close(unit=53)
      close(unit=54)
      close(unit=60)

!----------------------------------------------------------------------

#ifdef MPI
      if(myid.eq.0)then
        print *,'Program terminated normally:  myid=',myid
        do n=1,(numprocs-1)
          call MPI_IRECV(doit,1,mpi_logical,n,999,MPI_COMM_WORLD,reqs,ierr)
          call MPI_WAIT(reqs,status,ierr)
          print *,'Program terminated normally:  myid=',n
        enddo
        call MPI_FINALIZE(rc)
      else
        doit = .true.
        call MPI_ISEND(doit,1,mpi_logical,0,999,MPI_COMM_WORLD,reqs,ierr)
        call MPI_WAIT(reqs,status,ierr)
        call MPI_FINALIZE(rc)
      endif
#else
      print *,'Program terminated normally'
#endif

      stop

8778  print *
      print *,'  8778: error opening namelist.input '
      print *,'    ... stopping cm1 ... '
      print *
      call stopcm1

8800  print *
      print *,'  8800: error reading param0 section of namelist.input '
      print *,'    ... stopping cm1 ... '
      call stopcm1

8802  print *
      print *,'  8802: error reading param2 section of namelist.input '
      print *,'    ... stopping cm1 ... '
      call stopcm1

8808  print *
      print *,'  8808: error reading param8 section of namelist.input '
      print *,'    ... stopping cm1 ... '
      call stopcm1

      end program cm1

