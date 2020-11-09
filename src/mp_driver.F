  MODULE mp_driver_module

             !------  Microphysics Driver ------!

  implicit none

  private
  public :: mp_driver

  CONTAINS

  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


    subroutine mp_driver(nstep,dt,qbudget,asq,bsq,xh,ruh,xf,ruf,yh,rvh,yf,rvf,  &
                         mh,rmh,c1,c2,zh,mf,rmf,zf,rain,prate,pi0,th0,rho0,prs0,qv0,   &
                         rho,prs,dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,  &
                         w3d,ppi,pp3d,ppten,sten,tha,th3d,thten,qa,q3d,qten,  &
                         p3a,p3o,dum2d1,dum2d2,dum2d3,dum2d4,dum2d5,  &
                         effc,effi,effs,effr,effg,effis,    &
                         tdiag,qdiag,out2d,out3d,  &
                         dowriteout,dorad,dotdwrite,doazimwrite,dorestart,  &
                         getdbz,getvt,dotbud,doqbud)

    use input
    use constants
    use misclibs
    use kessler_module
    use goddard_module, only : goddard,satadj_ice,consat2
    use module_mp_thompson , only : mp_gt_driver
    use lfoice_module, only : lfo_ice_drive,lfoice_init
    use module_mp_graupel , only : mp_graupel
    use module_mp_nssl_2mom, only : nssl_2mom_driver
    use module_mp_p3, only : mp_p3_wrapper_wrf,mp_p3_wrapper_wrf_2cat
    use module_mp_jensen_ishmael , only : mp_jensen_ishmael

    implicit none

    integer, intent(in) :: nstep
    real, intent(inout) :: dt
    double precision, intent(inout), dimension(nbudget) :: qbudget
    double precision, intent(inout), dimension(numq) :: asq,bsq
    real, intent(in), dimension(ib:ie) :: xh,ruh
    real, intent(in), dimension(ib:ie+1) :: xf,ruf
    real, intent(in), dimension(jb:je) :: yh,rvh
    real, intent(in), dimension(jb:je+1) :: yf,rvf
    real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: mh,rmh,c1,c2,zh
    real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: mf,rmf,zf
    real, intent(inout), dimension(ib:ie,jb:je,nrain) :: rain
    real, intent(inout), dimension(ib:ie,jb:je) :: prate
    real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: pi0,th0,rho0,prs0,qv0
    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: rho,prs
    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8
    real, intent(in),    dimension(ib:ie,jb:je,kb:ke+1) :: w3d
    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: ppi,pp3d,ppten,sten
    real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: tha,th3d,thten
    real, intent(inout), dimension(ibm:iem,jbm:jem,kbm:kem,numq) :: qa,q3d,qten
    real, intent(inout), dimension(ibp3:iep3,kbp3:kep3,np3a) :: p3a
    real, intent(inout), dimension(ibp3:iep3,jbp3:jep3,kbp3:kep3,np3o) :: p3o
      double precision, intent(inout), dimension(ib:ie,jb:je) :: dum2d1,dum2d2,dum2d3,dum2d4,dum2d5
    real, intent(inout), dimension(ibr:ier,jbr:jer,kbr:ker) :: effc,effi,effs,effr,effg,effis
    real, intent(inout) , dimension(ibdt:iedt,jbdt:jedt,kbdt:kedt,ntdiag) :: tdiag
    real, intent(inout) , dimension(ibdq:iedq,jbdq:jedq,kbdq:kedq,nqdiag) :: qdiag
    real, intent(inout) , dimension(ib2d:ie2d,jb2d:je2d,nout2d) :: out2d
    real, intent(inout) , dimension(ib3d:ie3d,jb3d:je3d,kb3d:ke3d,nout3d) :: out3d
    logical, intent(in) :: dowriteout,dorad,dotdwrite,doazimwrite,dorestart
    logical, intent(inout) :: getdbz,getvt
    logical, intent(in) :: dotbud,doqbud

    !........

    integer :: i,j,k,n
    integer :: has_reqc,has_reqi,has_reqs,do_radar_ref
    real :: rdt

        if( stopit ) getdbz = .true.

        IF( efall.eq.1 ) getvt = .true.
        IF( dowriteout .and. output_fallvel.eq.1 ) getvt = .true.

        ! dum1 = T
        ! dum3 = appropriate rho for budget calculations
        ! store copy of T in thten array:

        !$omp parallel do default(shared)  &
        !$omp private(i,j,k,n)
        do k=1,nk
        do j=1,nj
        do i=1,ni
          dum1(i,j,k)=(th0(i,j,k)+th3d(i,j,k))*(pi0(i,j,k)+pp3d(i,j,k))
          thten(i,j,k)=dum1(i,j,k)
          qten(i,j,k,nqv)=q3d(i,j,k,nqv)
        enddo
        enddo
        enddo

          if( getdbz .and. qd_dbz.ge.1 )then
            !$omp parallel do default(shared)  &
            !$omp private(i,j,k,n)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              qdiag(i,j,k,qd_dbz)=0.0
            enddo
            enddo
            enddo
          endif

          IF(axisymm.eq.0)THEN
            ! for Cartesian grid:
            !$omp parallel do default(shared)  &
            !$omp private(i,j,k,n)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              dum3(i,j,k)=rho(i,j,k)
            enddo
            enddo
            enddo
          ELSE
            ! for axisymmetric grid:
            !$omp parallel do default(shared)  &
            !$omp private(i,j,k,n)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              dum3(i,j,k) = rho(i,j,k)*pi*(xf(i+1)**2-xf(i)**2)/(dx*dy)
            enddo
            enddo
            enddo
          ENDIF

          if( dotbud .and. td_mp.ge.1 )then
            !$omp parallel do default(shared)  &
            !$omp private(i,j,k,n)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              tdiag(i,j,k,td_mp) = th3d(i,j,k)
              dum5(i,j,k) = pi0(i,j,k)+pp3d(i,j,k)
            enddo
            enddo
            enddo
          endif
          if( doqbud .and. qd_mp.ge.1 )then
            !$omp parallel do default(shared)  &
            !$omp private(i,j,k,n)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              qdiag(i,j,k,qd_mp) = q3d(i,j,k,nqv)
            enddo
            enddo
            enddo
          endif


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  NOTES:
!
!           dum1   is   T
!           dum3   is   rho for budget calculations
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccc   Kessler scheme   cccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ifptype:  &
        IF(ptype.eq.1)THEN

          call pdefq(    0.0,asq(1),ruh,rvh,rmh,rho,q3d(ib,jb,kb,1))
          call pdefq( qsmall,asq(2),ruh,rvh,rmh,rho,q3d(ib,jb,kb,2))
          call pdefq( qsmall,asq(3),ruh,rvh,rmh,rho,q3d(ib,jb,kb,3))
          call k_fallout(rho,q3d(ib,jb,kb,3),qten(ib,jb,kb,3))
          call geterain(dt,cpl,lv1,qbudget(7),ruh,rvh,dum1,dum3,q3d(ib,jb,kb,3),qten(ib,jb,kb,3))
          if(efall.ge.1)then
            call getcvm(dum2,q3d)
            call getefall(1,cpl,mf,dum1,dum2,dum4,q3d(ib,jb,kb,3),qten(ib,jb,kb,3))
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
            do k=1,nk-1
            do j=1,nj
            do i=1,ni
              if( abs(dt*dum4(i,j,k)).ge.tsmall )then
                dum1(i,j,k) = dum1(i,j,k) + dt*dum4(i,j,k)
                prs(i,j,k)=rho(i,j,k)*rd*dum1(i,j,k)*(1.0+q3d(i,j,k,nqv)*reps)
                pp3d(i,j,k)=(prs(i,j,k)*rp00)**rovcp - pi0(i,j,k)
                th3d(i,j,k)=dum1(i,j,k)/(pi0(i,j,k)+pp3d(i,j,k)) - th0(i,j,k)
              endif
            enddo
            enddo
            enddo
            if( dotbud .and. td_efall.ge.1 )then
              !$omp parallel do default(shared)  &
              !$omp private(i,j,k)
              do k=1,nk-1
              do j=1,nj
              do i=1,ni
                tdiag(i,j,k,td_efall) = dum4(i,j,k)
              enddo
              enddo
              enddo
            endif
          endif
          call fallout(dt,qbudget(6),ruh,rvh,zh,mh,mf,rain,prate,dum3,rho,   &
                       q3d(ib,jb,kb,3),qten(ib,jb,kb,3))
          call kessler(dt,qbudget(3),qbudget(4),qbudget(5),ruh,rvh,rmh,pi0,th0,dum1,   &
                       rho,dum3,pp3d,th3d,prs,                            &
                       q3d(ib,jb,kb,nqv),q3d(ib,jb,kb,2),q3d(ib,jb,kb,3))
          call satadj(4,dt,qbudget(1),qbudget(2),ruh,rvh,rmh,pi0,th0,   &
                      rho,dum3,pp3d,prs,th3d,q3d)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccc   Goddard LFO scheme   cccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ELSEIF(ptype.eq.2)THEN

          call pdefq(    0.0,asq(1),ruh,rvh,rmh,rho,q3d(ib,jb,kb,1))
          call pdefq( qsmall,asq(2),ruh,rvh,rmh,rho,q3d(ib,jb,kb,2))
          call pdefq( qsmall,asq(3),ruh,rvh,rmh,rho,q3d(ib,jb,kb,3))
          call pdefq( qsmall,asq(4),ruh,rvh,rmh,rho,q3d(ib,jb,kb,4))
          call pdefq( qsmall,asq(5),ruh,rvh,rmh,rho,q3d(ib,jb,kb,5))
          call pdefq( qsmall,asq(6),ruh,rvh,rmh,rho,q3d(ib,jb,kb,6))
          call goddard(dt,qbudget(3),qbudget(4),qbudget(5),ruh,rvh,rmh,pi0,th0,             &
                       rho,dum3,prs,pp3d,th3d,                            &
     q3d(ib,jb,kb,1), q3d(ib,jb,kb,2),q3d(ib,jb,kb,3),qten(ib,jb,kb,3),   &
     q3d(ib,jb,kb,4),qten(ib,jb,kb,4),q3d(ib,jb,kb,5),qten(ib,jb,kb,5),   &
     q3d(ib,jb,kb,6),qten(ib,jb,kb,6))
          call satadj_ice(4,dt,qbudget(1),qbudget(2),ruh,rvh,rmh,pi0,th0,     &
                          rho,dum3,pp3d,prs,th3d,                     &
              q3d(ib,jb,kb,1),q3d(ib,jb,kb,2),q3d(ib,jb,kb,3),   &
              q3d(ib,jb,kb,4),q3d(ib,jb,kb,5),q3d(ib,jb,kb,6))
          call geterain(dt,cpl,lv1,qbudget(7),ruh,rvh,dum1,dum3,q3d(ib,jb,kb,3),qten(ib,jb,kb,3))
          call geterain(dt,cpi,ls1,qbudget(7),ruh,rvh,dum1,dum3,q3d(ib,jb,kb,4),qten(ib,jb,kb,4))
          call geterain(dt,cpi,ls1,qbudget(7),ruh,rvh,dum1,dum3,q3d(ib,jb,kb,5),qten(ib,jb,kb,5))
          call geterain(dt,cpi,ls1,qbudget(7),ruh,rvh,dum1,dum3,q3d(ib,jb,kb,6),qten(ib,jb,kb,6))
          if(efall.ge.1)then
            call getcvm(dum2,q3d)
            call getefall(1,cpl,mf,dum1,dum2,dum4,q3d(ib,jb,kb,3),qten(ib,jb,kb,3))
            call getefall(0,cpi,mf,dum1,dum2,dum4,q3d(ib,jb,kb,4),qten(ib,jb,kb,4))
            call getefall(0,cpi,mf,dum1,dum2,dum4,q3d(ib,jb,kb,5),qten(ib,jb,kb,5))
            call getefall(0,cpi,mf,dum1,dum2,dum4,q3d(ib,jb,kb,6),qten(ib,jb,kb,6))
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
            do k=1,nk-1
            do j=1,nj
            do i=1,ni
              if( abs(dt*dum4(i,j,k)).ge.tsmall )then
                dum1(i,j,k) = dum1(i,j,k) + dt*dum4(i,j,k)
                prs(i,j,k)=rho(i,j,k)*rd*dum1(i,j,k)*(1.0+q3d(i,j,k,nqv)*reps)
                pp3d(i,j,k)=(prs(i,j,k)*rp00)**rovcp - pi0(i,j,k)
                th3d(i,j,k)=dum1(i,j,k)/(pi0(i,j,k)+pp3d(i,j,k)) - th0(i,j,k)
              endif
            enddo
            enddo
            enddo
            if( dotbud .and. td_efall.ge.1 )then
              !$omp parallel do default(shared)  &
              !$omp private(i,j,k)
              do k=1,nk-1
              do j=1,nj
              do i=1,ni
                tdiag(i,j,k,td_efall) = dum4(i,j,k)
              enddo
              enddo
              enddo
            endif
          endif
          call fallout(dt,qbudget(6),ruh,rvh,zh,mh,mf,rain,prate,dum3,rho,   &
                       q3d(ib,jb,kb,3),qten(ib,jb,kb,3))
          call fallout(dt,qbudget(6),ruh,rvh,zh,mh,mf,rain,prate,dum3,rho,   &
                       q3d(ib,jb,kb,4),qten(ib,jb,kb,4))
          call fallout(dt,qbudget(6),ruh,rvh,zh,mh,mf,rain,prate,dum3,rho,   &
                       q3d(ib,jb,kb,5),qten(ib,jb,kb,5))
          call fallout(dt,qbudget(6),ruh,rvh,zh,mh,mf,rain,prate,dum3,rho,   &
                       q3d(ib,jb,kb,6),qten(ib,jb,kb,6))
          if(getdbz) call calcdbz(rho,q3d(ib,jb,kb,3),q3d(ib,jb,kb,5),q3d(ib,jb,kb,6),  &
                                  qdiag(ibdq,jbdq,kbdq,qd_dbz))

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccc   Thompson scheme   ccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ELSEIF(ptype.eq.3)THEN

          call pdefq(    0.0,asq(1),ruh,rvh,rmh,rho,q3d(ib,jb,kb,1))
          call pdefq( qsmall,asq(2),ruh,rvh,rmh,rho,q3d(ib,jb,kb,2))
          call pdefq( qsmall,asq(3),ruh,rvh,rmh,rho,q3d(ib,jb,kb,3))
          call pdefq( qsmall,asq(4),ruh,rvh,rmh,rho,q3d(ib,jb,kb,4))
          call pdefq( qsmall,asq(5),ruh,rvh,rmh,rho,q3d(ib,jb,kb,5))
          call pdefq( qsmall,asq(6),ruh,rvh,rmh,rho,q3d(ib,jb,kb,6))
!!!          call pdefq(    1.0,asq(7),ruh,rvh,rmh,rho,q3d(ib,jb,kb,7))
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            ! cm1r17:  to make things easier to understand, use same arrays 
            !          that are used for morrison code:
            ! dum1 = T  (this should have been calculated already)
            ! dum2 = pi (nondimensional pressure)
            ! dum4 = dz
            ! thten = copy of T  (this should have been calculated already)
            dum2(i,j,k)=pi0(i,j,k)+pp3d(i,j,k)
            dum4(i,j,k)=dz*rmh(i,j,k)
          enddo
          enddo
          enddo

          if( radopt.eq.2 )then
            has_reqc = 1
            has_reqi = 1
            has_reqs = 1
          else
            has_reqc = 0
            has_reqi = 0
            has_reqs = 0
          endif
          do_radar_ref = 1

          call   mp_gt_driver(qv=q3d(ib,jb,kb,1), &
                              qc=q3d(ib,jb,kb,2), &
                              qr=q3d(ib,jb,kb,3), &
                              qi=q3d(ib,jb,kb,4), &
                              qs=q3d(ib,jb,kb,5), &
                              qg=q3d(ib,jb,kb,6), &
                              ni=q3d(ib,jb,kb,7), &
                              nr=q3d(ib,jb,kb,8), &
                  t3d=dum1, pii=dum2, p=prs, w=w3d, dz=dum4, dt_in=dt, itimestep=nstep,                &
                  RAINNC=rain(ib,jb,1), RAINNCV=dum5(ib,jb,1), SR=dum5(ib,jb,2),                       &
                  refl_10cm=qdiag(ibdq,jbdq,kbdq,qd_dbz), diagflag=getdbz, do_radar_ref=do_radar_ref,  &
                  re_cloud=dum6, re_ice=dum7, re_snow=dum8,                  &
                  has_reqc=has_reqc, has_reqi=has_reqi, has_reqs=has_reqs,   &
                  nrain=nrain,dx=dx, dy=dy, cm1dz=dz,                        &
                  tcond=qbudget(1),tevac=qbudget(2),                         &
                  tevar=qbudget(5),train=qbudget(6),                         &
                  ruh=ruh,rvh=rvh,rmh=rmh,rr=dum3,rain=rain,prate=prate,     &
                  ib3d=ib3d,ie3d=ie3d,jb3d=jb3d,je3d=je3d,kb3d=kb3d,ke3d=ke3d, &
                  nout3d=nout3d,out3d=out3d,eqtset=eqtset,                   &
                  ids=1  ,ide=ni+1 , jds= 1 ,jde=nj+1 , kds=1  ,kde=nk+1 ,   &
                  ims=ib ,ime=ie   , jms=jb ,jme=je   , kms=kb ,kme=ke ,     &
                  its=1  ,ite=ni   , jts=1  ,jte=nj   , kts=1  ,kte=nk )

          ! Get final values for th3d,pp3d,prs:
          ! Note:  dum1 stores temperature, thten stores old temperature:
          IF( eqtset.eq.2 )THEN
            !$omp parallel do default(shared)  &
            !$omp private(i,j,k)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              if( abs(dum1(i,j,k)-thten(i,j,k)).ge.tsmall .or.  &
                  abs(q3d(i,j,k,nqv)-qten(i,j,k,nqv)).ge.qsmall )then
                prs(i,j,k)=rho(i,j,k)*(rd+rv*q3d(i,j,k,nqv))*dum1(i,j,k)
                pp3d(i,j,k)=(prs(i,j,k)*rp00)**rovcp - pi0(i,j,k)
                th3d(i,j,k)=dum1(i,j,k)/(pi0(i,j,k)+pp3d(i,j,k)) - th0(i,j,k)
              endif
            enddo
            enddo
            enddo
          ELSE
            !$omp parallel do default(shared)  &
            !$omp private(i,j,k)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              if( abs(dum1(i,j,k)-thten(i,j,k)).ge.tsmall .or.  &
                  abs(q3d(i,j,k,nqv)-qten(i,j,k,nqv)).ge.qsmall )then
                th3d(i,j,k)=dum1(i,j,k)/(pi0(i,j,k)+pp3d(i,j,k)) - th0(i,j,k)
                rho(i,j,k)=prs(i,j,k)/(rd*dum1(i,j,k)*(1.0+q3d(i,j,k,nqv)*reps))
              endif
            enddo
            enddo
            enddo
          ENDIF
          if( radopt.eq.2 )then
            !$omp parallel do default(shared)  &
            !$omp private(i,j,k)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              effc(i,j,k) = dum6(i,j,k)
              effi(i,j,k) = dum7(i,j,k)
              effs(i,j,k) = dum8(i,j,k)
            enddo
            enddo
            enddo
          endif
          if(timestats.ge.1) time_microphy=time_microphy+mytime()

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccc   GSR LFO scheme   cccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ELSEIF(ptype.eq.4)THEN

          call pdefq(    0.0,asq(1),ruh,rvh,rmh,rho,q3d(ib,jb,kb,1))
          call pdefq( qsmall,asq(2),ruh,rvh,rmh,rho,q3d(ib,jb,kb,2))
          call pdefq( qsmall,asq(3),ruh,rvh,rmh,rho,q3d(ib,jb,kb,3))
          call pdefq( qsmall,asq(4),ruh,rvh,rmh,rho,q3d(ib,jb,kb,4))
          call pdefq( qsmall,asq(5),ruh,rvh,rmh,rho,q3d(ib,jb,kb,5))
          call pdefq( qsmall,asq(6),ruh,rvh,rmh,rho,q3d(ib,jb,kb,6))
          call lfo_ice_drive(dt, mf, pi0, prs0, pp3d, prs, th0, th3d,    &
                             qv0, rho0, q3d, qten, dum1)
          do n=2,numq
            call fallout(dt,qbudget(6),ruh,rvh,zh,mh,mf,rain,prate,dum3,rho,   &
                         q3d(ib,jb,kb,n),qten(ib,jb,kb,n))
          enddo

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccc   Morrison scheme   cccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ELSEIF(ptype.eq.5)THEN

!          call pdefq(    0.0,asq(1),ruh,rvh,rmh,rho,q3d(ib,jb,kb,1))
!          call pdefq( qsmall,asq(2),ruh,rvh,rmh,rho,q3d(ib,jb,kb,2))
!          call pdefq( qsmall,asq(3),ruh,rvh,rmh,rho,q3d(ib,jb,kb,3))
!          call pdefq( qsmall,asq(4),ruh,rvh,rmh,rho,q3d(ib,jb,kb,4))
!          call pdefq( qsmall,asq(5),ruh,rvh,rmh,rho,q3d(ib,jb,kb,5))
!          call pdefq( qsmall,asq(6),ruh,rvh,rmh,rho,q3d(ib,jb,kb,6))
          call pdefqtest(    0.0,asq(1),ruh,rvh,rmh,rho,q3d(ib,jb,kb,1),dum2d1,dum2d2,dum2d3,dum2d4,dum2d5)
          call pdefqtest( qsmall,asq(2),ruh,rvh,rmh,rho,q3d(ib,jb,kb,2),dum2d1,dum2d2,dum2d3,dum2d4,dum2d5)
          call pdefqtest( qsmall,asq(3),ruh,rvh,rmh,rho,q3d(ib,jb,kb,3),dum2d1,dum2d2,dum2d3,dum2d4,dum2d5)
          call pdefqtest( qsmall,asq(4),ruh,rvh,rmh,rho,q3d(ib,jb,kb,4),dum2d1,dum2d2,dum2d3,dum2d4,dum2d5)
          call pdefqtest( qsmall,asq(5),ruh,rvh,rmh,rho,q3d(ib,jb,kb,5),dum2d1,dum2d2,dum2d3,dum2d4,dum2d5)
          call pdefqtest( qsmall,asq(6),ruh,rvh,rmh,rho,q3d(ib,jb,kb,6),dum2d1,dum2d2,dum2d3,dum2d4,dum2d5)
!!!          call pdefq(    1.0,asq(7),ruh,rvh,rmh,rho,q3d(ib,jb,kb,7))
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            ! dum1 = T  (this should have been calculated already)
            ! dum4 = dz
            ! thten = copy of T  (this should have been calculated already)
            dum4(i,j,k)=dz*rmh(i,j,k)
          enddo
          enddo
          enddo
          IF(numq.eq.11)THEN
            ! ppten stores ncc:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              ppten(i,j,k) = q3d(i,j,k,11)
            enddo
            enddo
            enddo
          ENDIF
          ! cm1r17:  get fall velocities (store in qten array)
          call MP_GRAUPEL(nstep,dum1,dum5,                            &
                          q3d(ib,jb,kb, 1),q3d(ib,jb,kb, 2),q3d(ib,jb,kb, 3), &
                          q3d(ib,jb,kb, 4),q3d(ib,jb,kb, 5),q3d(ib,jb,kb, 6), &
                          q3d(ib,jb,kb, 7),q3d(ib,jb,kb, 8),q3d(ib,jb,kb, 9), &
                          q3d(ib,jb,kb,10),ppten,                             &
                               prs,rho,dt,dum4,w3d,rain,prate,                &
                          effc,effi,effs,effr,effg,effis,                     &
                          qbudget(1),qbudget(2),qbudget(5),qbudget(6),        &
                          ruh,rvh,rmh,dum3,getdbz,                            &
                          qten(ib,jb,kb,nqc),qten(ib,jb,kb,nqr),qten(ib,jb,kb,nqi),  &
                          qten(ib,jb,kb,nqs),qten(ib,jb,kb,nqg),getvt,dorad,  &
                          dotbud,doqbud,tdiag,qdiag,out3d)
          IF(numq.eq.11)THEN
            ! ppten stores ncc:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              q3d(i,j,k,11) = ppten(i,j,k)
            enddo
            enddo
            enddo
          ENDIF
          if(timestats.ge.1) time_microphy=time_microphy+mytime()
          IF(efall.eq.1)THEN
            ! dum1 = T
            ! dum2 = cvm
            ! dum4 = T tendency
            call getcvm(dum2,q3d)
            call getefall(1,cpl,mf,dum1,dum2,dum4,q3d(ib,jb,kb,nqc),qten(ib,jb,kb,nqc))
            call getefall(0,cpl,mf,dum1,dum2,dum4,q3d(ib,jb,kb,nqr),qten(ib,jb,kb,nqr))
            call getefall(0,cpi,mf,dum1,dum2,dum4,q3d(ib,jb,kb,nqi),qten(ib,jb,kb,nqi))
            call getefall(0,cpi,mf,dum1,dum2,dum4,q3d(ib,jb,kb,nqs),qten(ib,jb,kb,nqs))
            call getefall(0,cpi,mf,dum1,dum2,dum4,q3d(ib,jb,kb,nqg),qten(ib,jb,kb,nqg))
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
            do k=1,nk-1
            do j=1,nj
            do i=1,ni
              dum1(i,j,k) = dum1(i,j,k) + dt*dum4(i,j,k)
            enddo
            enddo
            enddo
            if( dotbud .and. td_efall.ge.1 )then
              !$omp parallel do default(shared)  &
              !$omp private(i,j,k)
              do k=1,nk-1
              do j=1,nj
              do i=1,ni
                tdiag(i,j,k,td_efall) = dum4(i,j,k)
              enddo
              enddo
              enddo
            endif
          ENDIF
          ! Get final values for th3d,pp3d,prs:
          ! Note:  dum1 stores temperature, thten stores old temperature:
          IF( eqtset.eq.2 )THEN
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              if( abs(dum1(i,j,k)-thten(i,j,k)).ge.tsmall .or.  &
                  abs(q3d(i,j,k,nqv)-qten(i,j,k,nqv)).ge.qsmall )then
                prs(i,j,k)=rho(i,j,k)*(rd+rv*q3d(i,j,k,nqv))*dum1(i,j,k)
                pp3d(i,j,k)=(prs(i,j,k)*rp00)**rovcp - pi0(i,j,k)
                th3d(i,j,k)=dum1(i,j,k)/(pi0(i,j,k)+pp3d(i,j,k)) - th0(i,j,k)
              endif
            enddo
            enddo
            enddo
          ELSE
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              if( abs(dum1(i,j,k)-thten(i,j,k)).ge.tsmall .or.  &
                  abs(q3d(i,j,k,nqv)-qten(i,j,k,nqv)).ge.qsmall )then
                th3d(i,j,k)=dum1(i,j,k)/(pi0(i,j,k)+pp3d(i,j,k)) - th0(i,j,k)
                rho(i,j,k)=prs(i,j,k)/(rd*dum1(i,j,k)*(1.0+q3d(i,j,k,nqv)*reps))
              endif
            enddo
            enddo
            enddo
          ENDIF

          IF( getvt )THEN
          IF( dowriteout .or. dotdwrite .or. doazimwrite .or. dorestart )THEN
            !$omp parallel do default(shared)  &
            !$omp private(i,j,k)
            do k=1,nk
              if( qd_vtc.ge.1 )then
                do j=1,nj
                do i=1,ni
                  qdiag(i,j,k,qd_vtc) = qten(i,j,k,nqc)
                enddo
                enddo
              endif
              if( qd_vtr.ge.1 )then
                do j=1,nj
                do i=1,ni
                  qdiag(i,j,k,qd_vtr) = qten(i,j,k,nqr)
                enddo
                enddo
              endif
              if( qd_vts.ge.1 )then
                do j=1,nj
                do i=1,ni
                  qdiag(i,j,k,qd_vts) = qten(i,j,k,nqs)
                enddo
                enddo
              endif
              if( qd_vtg.ge.1 )then
                do j=1,nj
                do i=1,ni
                  qdiag(i,j,k,qd_vtg) = qten(i,j,k,nqg)
                enddo
                enddo
              endif
              if( qd_vti.ge.1 )then
                do j=1,nj
                do i=1,ni
                  qdiag(i,j,k,qd_vti) = qten(i,j,k,nqi)
                enddo
                enddo
              endif
            enddo
          ENDIF
          ENDIF

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccc   RE87-type scheme   cccccccccccccccccccccccccccccccccccccccccccccc
!ccc   also:  reversible moist thermo. if v_t = 0   cccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ELSEIF(ptype.eq.6)THEN

          call pdefq(    0.0,asq(1),ruh,rvh,rmh,rho,q3d(ib,jb,kb,1))
          call pdefq( qsmall,asq(2),ruh,rvh,rmh,rho,q3d(ib,jb,kb,2))
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            if(q3d(i,j,k,2).gt.0.001)then
              qten(i,j,k,2) = v_t
            else
              qten(i,j,k,2) = 0.0
            endif
          enddo
          enddo
          enddo
          call geterain(dt,cpl,lv1,qbudget(7),ruh,rvh,dum1,dum3,q3d(ib,jb,kb,2),qten(ib,jb,kb,2))
          if( efall.ge.1 .and. v_t.gt.1.0e-6 )then
            call getcvm(dum2,q3d)
            call getefall(1,cpl,mf,dum1,dum2,dum4,q3d(ib,jb,kb,2),qten(ib,jb,kb,2))
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
            do k=1,nk-1
            do j=1,nj
            do i=1,ni
              if( abs(dt*dum4(i,j,k)).ge.tsmall )then
                dum1(i,j,k) = dum1(i,j,k) + dt*dum4(i,j,k)
                prs(i,j,k)=rho(i,j,k)*rd*dum1(i,j,k)*(1.0+q3d(i,j,k,nqv)*reps)
                pp3d(i,j,k)=(prs(i,j,k)*rp00)**rovcp - pi0(i,j,k)
                th3d(i,j,k)=dum1(i,j,k)/(pi0(i,j,k)+pp3d(i,j,k)) - th0(i,j,k)
              endif
            enddo
            enddo
            enddo
            if( dotbud .and. td_efall.ge.1 )then
              !$omp parallel do default(shared)  &
              !$omp private(i,j,k)
              do k=1,nk-1
              do j=1,nj
              do i=1,ni
                tdiag(i,j,k,td_efall) = dum4(i,j,k)
              enddo
              enddo
              enddo
            endif
          endif
          call fallout(dt,qbudget(6),ruh,rvh,zh,mh,mf,rain,prate,dum3,rho,   &
                       q3d(ib,jb,kb,2),qten(ib,jb,kb,2))
          call satadj(4,dt,qbudget(1),qbudget(2),ruh,rvh,rmh,pi0,th0,   &
                      rho,dum3,pp3d,prs,th3d,q3d)

          IF( v_t.lt.(-0.0001) )THEN
            ! pseudoadiabatic approach of Bryan and Rotunno (2009, JAS, pg 3046)
            !$omp parallel do default(shared)  &
            !$omp private(i,j,k)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              q3d(i,j,k,nqc) = min( q3d(i,j,k,nqc) , 0.0001 )
            enddo
            enddo
            enddo
          ENDIF

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  Ziegler/Mansell (NSSL) two-moment scheme
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ELSEIF( ptype.ge.26 .and. ptype.le.28 )THEN

          IF ( ptype .eq. 26 ) THEN
            j = 13
          ELSEIF ( ptype .eq. 27 ) THEN
            j = 16
          ELSEIF ( ptype .eq. 28) THEN ! single moment
            j = 6
          ENDIF
          DO i = 1,j
            call pdefq(0.0,asq(i),ruh,rvh,rmh,rho,q3d(ib,jb,kb,i))
          ENDDO

!$omp parallel do default(shared)  &
!$omp private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            dum1(i,j,k) = pi0(i,j,k)+pp3d(i,j,k)
            dum2(i,j,k) = dz*rmh(i,j,k)
            dum4(i,j,k) = th0(i,j,k)+th3d(i,j,k)
            ! store old theta in thten array:
            thten(i,j,k)=dum4(i,j,k)
          enddo
          enddo
          enddo
          
          IF ( ptype .eq. 26 ) THEN  ! graupel only
             call nssl_2mom_driver(                          &
                               th  = dum4,                   &
                               qv  = q3d(ib,jb,kb, 1),       &
                               qc  = q3d(ib,jb,kb, 2),       &
                               qr  = q3d(ib,jb,kb, 3),       &
                               qi  = q3d(ib,jb,kb, 4),       &
                               qs  = q3d(ib,jb,kb, 5),       &
                               qh  = q3d(ib,jb,kb, 6),       &
                               cn  = q3d(ib,jb,kb, 7),       &
                               ccw = q3d(ib,jb,kb, 8),       &
                               crw = q3d(ib,jb,kb, 9),       &
                               cci = q3d(ib,jb,kb, 10),      &
                               csw = q3d(ib,jb,kb, 11),      &
                               chw = q3d(ib,jb,kb, 12),      &
                               vhw = q3d(ib,jb,kb, 13),      &
                               pii = dum1,                   &
                               p   =  prs,                   &
                               w   =  w3d,                   &
                               dn  =  rho,                   &
                               dz  =  dum2,                  &
                               dtp = dt,                     &
                               itimestep = nstep,            &
                              RAIN = rain,                   &
                              nrain = nrain,                 &
                              prate = prate,                 &
                              dbz = qdiag(ibdq,jbdq,kbdq,qd_dbz), &
                              ruh = ruh, rvh = rvh, rmh = rmh, &
                              dx = dx, dy = dy,              &
                              tcond = qbudget(1),            &
                              tevac = qbudget(2),            &
                              tevar = qbudget(5),            &
                              train = qbudget(6),            &
                              rr    = dum3,                  &
                              diagflag = getdbz,                  &
                  ib3d=ib3d,ie3d=ie3d,jb3d=jb3d,je3d=je3d,kb3d=kb3d,ke3d=ke3d, &
                  nout3d=nout3d,out3d=out3d,                                 &
                              ims = ib ,ime = ie , jms = jb ,jme = je, kms = kb,kme = ke,  &  
                              its = 1 ,ite = ni, jts = 1,jte = nj, kts = 1,kte = nk)
         ELSEIF ( ptype .eq. 27 ) THEN
             call nssl_2mom_driver(                          &
                               th  = dum4,                   &
                               qv  = q3d(ib,jb,kb, 1),       &
                               qc  = q3d(ib,jb,kb, 2),       &
                               qr  = q3d(ib,jb,kb, 3),       &
                               qi  = q3d(ib,jb,kb, 4),       &
                               qs  = q3d(ib,jb,kb, 5),       &
                               qh  = q3d(ib,jb,kb, 6),       &
                               qhl = q3d(ib,jb,kb, 7),       &
                               cn  = q3d(ib,jb,kb, 8),       &
                               ccw = q3d(ib,jb,kb, 9),       &
                               crw = q3d(ib,jb,kb,10),       &
                               cci = q3d(ib,jb,kb, 11),      &
                               csw = q3d(ib,jb,kb, 12),      &
                               chw = q3d(ib,jb,kb, 13),      &
                               chl = q3d(ib,jb,kb, 14),      &
                               vhw = q3d(ib,jb,kb, 15),      &
                               vhl = q3d(ib,jb,kb, 16),      &
                               pii = dum1,                   &
                               p   =  prs,                   &
                               w   =  w3d,                   &
                               dn  =  rho,                   &
                               dz  =  dum2,                  &
                               dtp = dt,                     &
                               itimestep = nstep,            &
                              RAIN = rain,                   &
                              nrain = nrain,                 &
                              prate = prate,                 &
                              dbz = qdiag(ibdq,jbdq,kbdq,qd_dbz), &
                              ruh = ruh, rvh = rvh, rmh = rmh, &
                              dx = dx, dy = dy,              &
                              tcond = qbudget(1),            &
                              tevac = qbudget(2),            &
                              tevar = qbudget(5),            &
                              train = qbudget(6),            &
                              rr    = dum3,                  &
                              diagflag = getdbz,             &
                  ib3d=ib3d,ie3d=ie3d,jb3d=jb3d,je3d=je3d,kb3d=kb3d,ke3d=ke3d, &
                  nout3d=nout3d,out3d=out3d,                                 &
                              ims = ib ,ime = ie , jms = jb ,jme = je, kms = kb,kme = ke,  &  
                              its = 1 ,ite = ni, jts = 1,jte = nj, kts = 1,kte = nk)
          ELSEIF ( ptype .eq. 28 ) THEN  ! single moment
             call nssl_2mom_driver(                          &
                               th  = dum4,                   &
                               qv  = q3d(ib,jb,kb, 1),       &
                               qc  = q3d(ib,jb,kb, 2),       &
                               qr  = q3d(ib,jb,kb, 3),       &
                               qi  = q3d(ib,jb,kb, 4),       &
                               qs  = q3d(ib,jb,kb, 5),       &
                               qh  = q3d(ib,jb,kb, 6),       &
                               pii = dum1,                   &
                               p   =  prs,                   &
                               w   =  w3d,                   &
                               dn  =  rho,                   &
                               dz  =  dum2,                  &
                               dtp = dt,                     &
                               itimestep = nstep,            &
                              RAIN = rain,                   &
                              nrain = nrain,                 &
                              prate = prate,                 &
                              dbz = qdiag(ibdq,jbdq,kbdq,qd_dbz), &
                              ruh = ruh, rvh = rvh, rmh = rmh, &
                              dx = dx, dy = dy,              &
                              tcond = qbudget(1),            &
                              tevac = qbudget(2),            &
                              tevar = qbudget(5),            &
                              train = qbudget(6),            &
                              rr    = dum3,                  &
                              diagflag = getdbz,                  &
                  ib3d=ib3d,ie3d=ie3d,jb3d=jb3d,je3d=je3d,kb3d=kb3d,ke3d=ke3d, &
                  nout3d=nout3d,out3d=out3d,                                 &
                              ims = ib ,ime = ie , jms = jb ,jme = je, kms = kb,kme = ke,  &  
                              its = 1 ,ite = ni, jts = 1,jte = nj, kts = 1,kte = nk)

          ENDIF

        IF(eqtset.eq.2)THEN
          ! for mass conservation:
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            if( abs(dum4(i,j,k)-thten(i,j,k)).ge.tsmall .or.  &
                abs(q3d(i,j,k,nqv)-qten(i,j,k,nqv)).ge.qsmall )then
              prs(i,j,k) = rho(i,j,k)*rd*dum4(i,j,k)*dum1(i,j,k)*(1.0+q3d(i,j,k,nqv)*reps)
              pp3d(i,j,k) = (prs(i,j,k)*rp00)**rovcp - pi0(i,j,k)
              th3d(i,j,k) = dum4(i,j,k)*dum1(i,j,k)/(pi0(i,j,k)+pp3d(i,j,k)) - th0(i,j,k)
            endif
          enddo
          enddo
          enddo
        ELSE
          ! traditional thermodynamics:  p,pi remain unchanged
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
          do k=1,nk
          do j=1,nj
          do i=1,ni
            if( abs(dum4(i,j,k)-thten(i,j,k)).ge.tsmall .or.  &
                abs(q3d(i,j,k,nqv)-qten(i,j,k,nqv)).ge.qsmall )then
              th3d(i,j,k)= dum4(i,j,k) - th0(i,j,k)
              rho(i,j,k)=prs(i,j,k)/(rd*dum4(i,j,k)*dum1(i,j,k)*(1.0+q3d(i,j,k,nqv)*reps))
            endif
          enddo
          enddo
          enddo
        ENDIF

          if(timestats.ge.1) time_microphy=time_microphy+mytime()


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccc     P3 scheme      cccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ELSEIF( ptype.eq.50 .or. ptype.eq.51 )THEN

          rdt = 1.0/dt

          p3jloop:  &
          DO j=1,nj

            do k=1,nk
            do i=1,ni
              p3a(i,k, 1) = th0(i,j,k)+th3d(i,j,k)
              p3a(i,k, 2) = q3d(i,j,k,nqv)
              thten(i,j,k)    = p3a(i,k, 1)
              qten(i,j,k,nqv) = p3a(i,k, 2)
              p3a(i,k, 3) = q3d(i,j,k,nqc)
              p3a(i,k, 4) = q3d(i,j,k,nqr)
              p3a(i,k, 5) = q3d(i,j,k,6)
              p3a(i,k, 6) = th0(i,j,k)+tha(i,j,k)
              p3a(i,k, 7) = qa(i,j,k,nqv)
              p3a(i,k, 8) = pi0(i,j,k)+pp3d(i,j,k)
              p3a(i,k, 9) = prs(i,j,k)
              p3a(i,k,10) = dz*rmh(i,j,k)
              p3a(i,k,11) = 0.5*(w3d(i,j,k)+w3d(i,j,k+1))
              p3a(i,k,18) = q3d(i,j,k,nqi)
              p3a(i,k,19) = q3d(i,j,k,5)
              p3a(i,k,20) = q3d(i,j,k,7)
              p3a(i,k,21) = q3d(i,j,k,8)
            enddo
            enddo

            do i=1,ni
              p3a(i,1,22) = 0.0
            enddo

     if( ptype.eq.50 )then
       call mp_p3_wrapper_wrf( th_3d=p3a(1,1,1),  &
                               qv_3d=p3a(1,1,2),  &
                               qc_3d=p3a(1,1,3),  &
                               qr_3d=p3a(1,1,4),  &
                               qnr_3d=p3a(1,1,5),                            &
                              th_old_3d=p3a(1,1,6),  &
                              qv_old_3d=p3a(1,1,7),                                       &
                              pii=p3a(1,1,8),  &
                              p=p3a(1,1,9),  &
                              dz=p3a(1,1,10),  &
                              w=p3a(1,1,11),    &
                              dt=dt,  &
                              itimestep=nstep,                                   &
                              rainnc=p3a(1,1,22),    &
                              rainncv=dum1(ib,jb,2),  &
                              sr=dum1(ib,jb,3),  &
                              snownc=dum1(ib,jb,4),  &
                              snowncv=dum1(ib,jb,5),  &
                              n_iceCat=1,                 &
                              ids=1, ide=ni, jds=1, jde=1, kds=1, kde=nk ,                             &
                              ims=1, ime=ni, jms=1, jme=1, kms=1, kme=nk ,                             &
                              its=1, ite=ni, jts=1, jte=1, kts=1, kte=nk ,                             &
                              diag_zdbz_3d=p3a(1,1,12),  &
                              diag_effc_3d=p3a(1,1,13),  &
                              diag_effi_3d=p3a(1,1,14),                    &
                              diag_vmi_3d=p3a(1,1,15),  &
                              diag_di_3d=p3a(1,1,16),  &
                              diag_rhopo_3d=p3a(1,1,17),                      &
                              qi1_3d=p3a(1,1,18),  &
                              qni1_3d=p3a(1,1,19),  &
                              qir1_3d=p3a(1,1,20),  &
                              qib1_3d=p3a(1,1,21) )
     elseif( ptype.eq.51 )then
        do k=1,nk
        do i=1,ni
          p3a(i,k,23) = q3d(i,j,k,9)
        enddo
        enddo
       call mp_p3_wrapper_wrf( th_3d=p3a(1,1,1),  &
                               qv_3d=p3a(1,1,2),  &
                               qc_3d=p3a(1,1,3),  &
                               qr_3d=p3a(1,1,4),  &
                               qnr_3d=p3a(1,1,5),                            &
                              th_old_3d=p3a(1,1,6),  &
                              qv_old_3d=p3a(1,1,7),                                       &
                              pii=p3a(1,1,8),  &
                              p=p3a(1,1,9),  &
                              dz=p3a(1,1,10),  &
                              w=p3a(1,1,11),    &
                              dt=dt,  &
                              itimestep=nstep,                                   &
                              rainnc=p3a(1,1,22),    &
                              rainncv=dum1(ib,jb,2),  &
                              sr=dum1(ib,jb,3),  &
                              snownc=dum1(ib,jb,4),  &
                              snowncv=dum1(ib,jb,5),  &
                              n_iceCat=1,                 &
                              ids=1, ide=ni, jds=1, jde=1, kds=1, kde=nk ,                             &
                              ims=1, ime=ni, jms=1, jme=1, kms=1, kme=nk ,                             &
                              its=1, ite=ni, jts=1, jte=1, kts=1, kte=nk ,                             &
                              diag_zdbz_3d=p3a(1,1,12),  &
                              diag_effc_3d=p3a(1,1,13),  &
                              diag_effi_3d=p3a(1,1,14),                    &
                              diag_vmi_3d=p3a(1,1,15),  &
                              diag_di_3d=p3a(1,1,16),  &
                              diag_rhopo_3d=p3a(1,1,17),                      &
                              qi1_3d=p3a(1,1,18),  &
                              qni1_3d=p3a(1,1,19),  &
                              qir1_3d=p3a(1,1,20),  &
                              qib1_3d=p3a(1,1,21), &
                              nc_3d=p3a(1,1,23) )
        do k=1,nk
        do i=1,ni
          q3d(i,j,k,9) = p3a(i,k,23)
        enddo
        enddo
     endif

            if( axisymm.eq.1 )then
              do i=1,ni
                prate(i,j) = p3a(i,1,22)*rdt
                qbudget(6) = qbudget(6) + p3a(i,1,22)*ruh(i)*rvh(j)*dx*dy*dum3(i,j,1)/rho(i,j,1)
              enddo
            else
              do i=1,ni
                prate(i,j) = p3a(i,1,22)*rdt
                qbudget(6) = qbudget(6) + p3a(i,1,22)*ruh(i)*rvh(j)*dx*dy
              enddo
            endif

            do n=1,nrain
            do i=1,ni
              ! convert from mm to cm:
              rain(i,j,n) = rain(i,j,n) + 0.1*p3a(i,1,22)
            enddo
            enddo

            do k=1,nk
            do i=1,ni
              if( abs(p3a(i,k,1)-thten(i,j,k)).ge.tsmall .or.  &
                  abs(p3a(i,k,2)-qten(i,j,k,nqv)).ge.qsmall )then
                pp3d(i,j,k)=(rho(i,j,k)*(rd+rv*p3a(i,k,2))*p3a(i,k,1)*rp00)**rddcv
                prs(i,j,k)=p00*(pp3d(i,j,k)**cpdrd)
                pp3d(i,j,k)=pp3d(i,j,k)-pi0(i,j,k)
                th3d(i,j,k)=th3d(i,j,k)+(p3a(i,k,1)-thten(i,j,k))
              endif
              q3d(i,j,k,nqv) = p3a(i,k, 2)
              q3d(i,j,k,nqc) = p3a(i,k, 3)
              q3d(i,j,k,nqr) = p3a(i,k, 4)
              q3d(i,j,k, 6 ) = p3a(i,k, 5)
              q3d(i,j,k,nqi) = p3a(i,k,18)
              q3d(i,j,k, 5 ) = p3a(i,k,19)
              q3d(i,j,k, 7 ) = p3a(i,k,20)
              q3d(i,j,k, 8 ) = p3a(i,k,21)
            enddo
            enddo

            IF( getdbz .and. qd_dbz.ge.1 )THEN
              do k=1,nk
              do i=1,ni
                qdiag(i,j,k,qd_dbz) = p3a(i,k,12)
              enddo
              enddo
            ENDIF

            IF( dorad )THEN
              do k=1,nk
              do i=1,ni
                effc(i,j,k) = p3a(i,k,13)
                effi(i,j,k) = p3a(i,k,14)
              enddo
              enddo
            ENDIF

            IF( dowriteout .or. getdbz )THEN
              do k=1,nk
              do i=1,ni
                p3o(i,j,k,1) = p3a(i,k,15)
                p3o(i,j,k,2) = p3a(i,k,16)
                p3o(i,j,k,3) = p3a(i,k,17)
              enddo
              enddo
            ENDIF

          ENDDO  p3jloop

          if(timestats.ge.1) time_microphy=time_microphy+mytime()

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccc     P3 scheme, 2 ice     cccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ELSEIF( ptype.eq.52 )THEN

          rdt = 1.0/dt

          p3jloop2:  &
          DO j=1,nj

            do k=1,nk
            do i=1,ni
              p3a(i,k, 1) = th0(i,j,k)+th3d(i,j,k)         ! th_3d
              p3a(i,k, 2) = q3d(i,j,k,nqv)                 ! qv_3d
              thten(i,j,k)    = p3a(i,k, 1)
              qten(i,j,k,nqv) = p3a(i,k, 2)
              p3a(i,k, 3) = q3d(i,j,k,nqc)                 ! qc_3d
              p3a(i,k, 4) = q3d(i,j,k,nqr)                 ! qr_3d
              p3a(i,k, 5) = q3d(i,j,k,7)                   ! qnr_3d
              p3a(i,k, 6) = th0(i,j,k)+tha(i,j,k)          ! th_old
              p3a(i,k, 7) = qa(i,j,k,nqv)                  ! qv_old
              p3a(i,k, 8) = pi0(i,j,k)+pp3d(i,j,k)         ! pii
              p3a(i,k, 9) = prs(i,j,k)                     ! p
              p3a(i,k,10) = dz*rmh(i,j,k)                  ! dz
              p3a(i,k,11) = 0.5*(w3d(i,j,k)+w3d(i,j,k+1))  ! w
              p3a(i,k,18) = q3d(i,j,k,nqi)                 ! qi1_3d
              p3a(i,k,19) = q3d(i,j,k,8)                   ! qni1_3d
              p3a(i,k,20) = q3d(i,j,k,9)                   ! qir1_3d
              p3a(i,k,21) = q3d(i,j,k,10)                  ! qib1_3d
              p3a(i,k,23) = q3d(i,j,k, 5)                  ! qi2_3d
              p3a(i,k,24) = q3d(i,j,k,12)                  ! qir2_3d
              p3a(i,k,25) = q3d(i,j,k,11)                  ! qni2_3d
              p3a(i,k,26) = q3d(i,j,k,13)                  ! qib2_3d
              p3a(i,k,27) = q3d(i,j,k, 6)                  ! nc_3d
            enddo
            enddo

            do i=1,ni
              p3a(i,1,22) = 0.0
            enddo

       call mp_p3_wrapper_wrf_2cat( th_3d=p3a(1,1,1),  &
                               qv_3d=p3a(1,1,2),  &
                               qc_3d=p3a(1,1,3),  &
                               qr_3d=p3a(1,1,4),  &
                               qnr_3d=p3a(1,1,5),                            &
                              th_old_3d=p3a(1,1,6),  &
                              qv_old_3d=p3a(1,1,7),                                       &
                              pii=p3a(1,1,8),  &
                              p=p3a(1,1,9),  &
                              dz=p3a(1,1,10),  &
                              w=p3a(1,1,11),    &
                              dt=dt,  &
                              itimestep=nstep,                                   &
                              rainnc=p3a(1,1,22),    &
                              rainncv=dum1(ib,jb,2),  &
                              sr=dum1(ib,jb,3),  &
                              snownc=dum1(ib,jb,4),  &
                              snowncv=dum1(ib,jb,5),  &
                              n_iceCat=2,                 &
                              ids=1, ide=ni, jds=1, jde=1, kds=1, kde=nk ,             &
                              ims=1, ime=ni, jms=1, jme=1, kms=1, kme=nk ,             &
                              its=1, ite=ni, jts=1, jte=1, kts=1, kte=nk ,             &
                              diag_zdbz_3d=p3a(1,1,12),  &
                              diag_effc_3d=p3a(1,1,13),  &
                              diag_effi_3d=p3a(1,1,14),                    &
                              diag_vmi_3d=p3a(1,1,15),  &
                              diag_di_3d=p3a(1,1,16),  &
                              diag_rhopo_3d=p3a(1,1,17),                      &
                              qi1_3d=p3a(1,1,18),  &
                              qni1_3d=p3a(1,1,19),  &
                              qir1_3d=p3a(1,1,20),  &
                              qib1_3d=p3a(1,1,21),  &
                     QI2_3d=p3a(1,1,23),                  &
                     QIR2_3d=p3a(1,1,24),                  &
                     QNI2_3d=p3a(1,1,25),                 &
                     QIB2_3d=p3a(1,1,26),               &
                     nc_3d=p3a(1,1,27),                     &
                  diag_vmi2_3d=p3a(1,1,28),                                  &
                  diag_di2_3d=p3a(1,1,29),                                   &
                  diag_rhopo2_3d=p3a(1,1,30)    )

            if( axisymm.eq.1 )then
              do i=1,ni
                prate(i,j) = p3a(i,1,22)*rdt
                qbudget(6) = qbudget(6) + p3a(i,1,22)*ruh(i)*rvh(j)*dx*dy*dum3(i,j,1)/rho(i,j,1)
              enddo
            else
              do i=1,ni
                prate(i,j) = p3a(i,1,22)*rdt
                qbudget(6) = qbudget(6) + p3a(i,1,22)*ruh(i)*rvh(j)*dx*dy
              enddo
            endif

            do n=1,nrain
            do i=1,ni
              ! convert from mm to cm:
              rain(i,j,n) = rain(i,j,n) + 0.1*p3a(i,1,22)
            enddo
            enddo

            do k=1,nk
            do i=1,ni
              if( abs(p3a(i,k,1)-thten(i,j,k)).ge.tsmall .or.  &
                  abs(p3a(i,k,2)-qten(i,j,k,nqv)).ge.qsmall )then
                pp3d(i,j,k)=(rho(i,j,k)*(rd+rv*p3a(i,k,2))*p3a(i,k,1)*rp00)**rddcv
                prs(i,j,k)=p00*(pp3d(i,j,k)**cpdrd)
                pp3d(i,j,k)=pp3d(i,j,k)-pi0(i,j,k)
                th3d(i,j,k)=th3d(i,j,k)+(p3a(i,k,1)-thten(i,j,k))
              endif
              q3d(i,j,k,nqv) = p3a(i,k, 2)
              q3d(i,j,k,nqc) = p3a(i,k, 3)    ! qc_3d
              q3d(i,j,k,nqr) = p3a(i,k, 4)    ! qr_3d
              q3d(i,j,k, 7 ) = p3a(i,k, 5)    ! qnr_3d
              q3d(i,j,k,nqi) = p3a(i,k,18)    ! qi1_3d
              q3d(i,j,k, 8 ) = p3a(i,k,19)    ! qni1_3d
              q3d(i,j,k, 9 ) = p3a(i,k,20)    ! qir1_3d
              q3d(i,j,k,10 ) = p3a(i,k,21)    ! qib1_3d
              q3d(i,j,k, 5 ) = p3a(i,k,23)    ! qi2_3d
              q3d(i,j,k,12 ) = p3a(i,k,24)    ! qir2_3d
              q3d(i,j,k,11 ) = p3a(i,k,25)    ! qni2_3d
              q3d(i,j,k,13 ) = p3a(i,k,26)    ! qib2_3d
              q3d(i,j,k, 6 ) = p3a(i,k,27)    ! nc_3d
            enddo
            enddo

            IF( getdbz .and. qd_dbz.ge.1 )THEN
              do k=1,nk
              do i=1,ni
                qdiag(i,j,k,qd_dbz) = p3a(i,k,12)
              enddo
              enddo
            ENDIF

            IF( dorad )THEN
              do k=1,nk
              do i=1,ni
                effc(i,j,k) = p3a(i,k,13)
                effi(i,j,k) = p3a(i,k,14)
              enddo
              enddo
            ENDIF

            IF( dowriteout .or. getdbz )THEN
              do k=1,nk
              do i=1,ni
                p3o(i,j,k,1) = p3a(i,k,15)
                p3o(i,j,k,2) = p3a(i,k,16)
                p3o(i,j,k,3) = p3a(i,k,17)
              enddo
              enddo
              if( ptype.eq.52 )then
                do k=1,nk
                do i=1,ni
                  p3o(i,j,k,4) = p3a(i,k,28)
                  p3o(i,j,k,5) = p3a(i,k,29)
                  p3o(i,j,k,6) = p3a(i,k,30)
                enddo
                enddo
              endif
            ENDIF

          ENDDO  p3jloop2

          if(timestats.ge.1) time_microphy=time_microphy+mytime()


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccc   Jensen scheme   ccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ELSEIF(ptype.eq.55)THEN

          call pdefq(    0.0,asq(1),ruh,rvh,rmh,rho,q3d(ib,jb,kb,1))
          call pdefq( qsmall,asq(2),ruh,rvh,rmh,rho,q3d(ib,jb,kb,2))
          call pdefq( qsmall,asq(3),ruh,rvh,rmh,rho,q3d(ib,jb,kb,3))
          call pdefq( qsmall,asq(4),ruh,rvh,rmh,rho,q3d(ib,jb,kb,4))
          call pdefq( qsmall,asq(5),ruh,rvh,rmh,rho,q3d(ib,jb,kb,5))
          call pdefq( qsmall,asq(6),ruh,rvh,rmh,rho,q3d(ib,jb,kb,6))

          do k=1,nk
          do j=1,nj
          do i=1,ni
            ! cm1r17:  to make things easier to understand, use same arrays 
            !          that are used for morrison code:
            ! dum1 = T  (this should have been calculated already)
            ! dum2 = pi (nondimensional pressure)
            ! dum4 = dz
            ! thten = copy of T  (this should have been calculated already)
            dum2(i,j,k)=pi0(i,j,k)+pp3d(i,j,k)
            dum4(i,j,k)=dz*rmh(i,j,k)
          enddo
          enddo
          enddo

!          write(6,*) 'got to call'
!          write(6,*) nstep, dt, prs(1,1,1), dum4(1,1,1), dum1(1,1,1)
!          write(6,*) q3d(ib,jb,kb,1),q3d(ib,jb,kb,2), q3d(ib,jb,kb,3), q3d(ib,jb,kb,4), &
!               q3d(ib,jb,kb,5), q3d(ib,jb,kb,6), q3d(ib,jb,kb,7), q3d(ib,jb,kb,8), &
!               q3d(ib,jb,kb,9), q3d(ib,jb,kb,10), q3d(ib,jb,kb,11), q3d(ib,jb,kb,12), &
!               q3d(ib,jb,kb,13), q3d(ib,jb,kb,14), q3d(ib,jb,kb,15), q3d(ib,jb,kb,16)

!          write(6,*) ni, nk, nj, ib, ie, jb, je, kb, ke, rain(ib,jb,1),dum5(ib,jb,1)
!          write(6,*) nrain, dx, dy, dz, qbudget(1), qbudget(2), qbudget(5), qbudget(6)
!          write(6,*)  ruh(1), rvh(1), rmh(1,1,1), dum3(1,1,1), rain(1,1,1), prate(1,1)

!               ib3d=ib3d,ie3d=ie3d,jb3d=jb3d,je3d=je3d,kb3d=kb3d,ke3d=ke3d, &
!                    nout3d=nout3d,out3d=out3d,eqtset=eqtset)
               
          call   mp_jensen_ishmael( &
          itimestep=nstep, dt_in=dt, p=prs, dz=dum4, t3d=dum1, &
               qv=q3d(ib,jb,kb,1), &
               qc=q3d(ib,jb,kb,2), &
               qr=q3d(ib,jb,kb,3), &
               nr=q3d(ib,jb,kb,7), &
               qi1=q3d(ib,jb,kb,4), &
               ni1=q3d(ib,jb,kb,8), &
               ai1=q3d(ib,jb,kb,11), &
               ci1=q3d(ib,jb,kb,14), &
               qi2=q3d(ib,jb,kb,5), &
               ni2=q3d(ib,jb,kb,9), &
               ai2=q3d(ib,jb,kb,12), &
               ci2=q3d(ib,jb,kb,15), &
               qi3=q3d(ib,jb,kb,6), &
               ni3=q3d(ib,jb,kb,10), &
               ai3=q3d(ib,jb,kb,13), &
               ci3=q3d(ib,jb,kb,16), &
               ids=1  ,ide=ni+1 , jds= 1 ,jde=nj+1 , kds=1  ,kde=nk+1 ,   &
               ims=ib ,ime=ie   , jms=jb ,jme=je   , kms=kb ,kme=ke ,     &
               its=1  ,ite=ni   , jts=1  ,jte=nj   , kts=1  ,kte=nk ,     &
               RAINNC=rain(ib,jb,1), RAINNCV=dum5(ib,jb,1), &
               diag_effc3d=effc(ibr,jbr,kbr), diag_effi3d=effi(ibr,jbr,kbr), diag_dbz3d=dum8,  &
       diag_vmi3d_1=p3o(1,1,1, 1), diag_di3d_1=p3o(1,1,1, 2), diag_rhopo3d_1=p3o(1,1,1, 3), diag_phii3d_1=p3o(1,1,1, 4), &
       diag_vmi3d_2=p3o(1,1,1, 5), diag_di3d_2=p3o(1,1,1, 6), diag_rhopo3d_2=p3o(1,1,1, 7), diag_phii3d_2=p3o(1,1,1, 8), &
       diag_vmi3d_3=p3o(1,1,1, 9), diag_di3d_3=p3o(1,1,1,10), diag_rhopo3d_3=p3o(1,1,1,11), diag_phii3d_3=p3o(1,1,1,12), &
               nrain=nrain,dx=dx, dy=dy, cm1dz=dz,                        &
               tcond=qbudget(1),tevac=qbudget(2),                         &
               tevar=qbudget(5),train=qbudget(6),                         &
               ruh=ruh,rvh=rvh,rmh=rmh,rr=dum3,rain=rain,prate=prate,     &
               ib3d=ib3d,ie3d=ie3d,jb3d=jb3d,je3d=je3d,kb3d=kb3d,ke3d=ke3d,eqtset=eqtset, &
               nout3d=nout3d,out3d=out3d,dowr=dowr,outfile=outfile,getdbz=getdbz,dorad=dorad)

            do k=1,nk
            do j=1,nj
            do i=1,ni
              if( abs(dum1(i,j,k)-thten(i,j,k)).ge.tsmall .or.  &
                  abs(q3d(i,j,k,nqv)-qten(i,j,k,nqv)).ge.qsmall )then
                 th3d(i,j,k)=dum1(i,j,k)/(pi0(i,j,k)+pp3d(i,j,k)) - th0(i,j,k)
                 rho(i,j,k)=prs(i,j,k)/(rd*dum1(i,j,k)*(1.0+q3d(i,j,k,nqv)*reps))
              endif
            enddo
            enddo
            enddo

            if( getdbz )then
              do k=1,nk
              do j=1,nj
              do i=1,ni
                qdiag(i,j,k,qd_dbz) = dum8(i,j,k)
              enddo
              enddo
              enddo
            endif


!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  insert new microphysics schemes here
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!        ELSEIF(ptype.eq.8)THEN
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! otherwise, stop for undefined ptype

        ELSE

          print *,'  Undefined ptype!'
          call stopcm1

        ENDIF  ifptype

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CC   END microphysics   CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


          if( dotbud .and. td_mp.ge.1 )then
            rdt = 1.0/dt
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              tdiag(i,j,k,td_mp) = (th3d(i,j,k)-tdiag(i,j,k,td_mp))*rdt
            enddo
            enddo
            enddo
          endif

          if( doqbud .and. qd_mp.ge.1 )then
            rdt = 1.0/dt
!$omp parallel do default(shared)  &
!$omp private(i,j,k)
            do k=1,nk
            do j=1,nj
            do i=1,ni
              qdiag(i,j,k,qd_mp) = (q3d(i,j,k,nqv)-qdiag(i,j,k,qd_mp))*rdt
            enddo
            enddo
            enddo
          endif

        if(timestats.ge.1) time_microphy=time_microphy+mytime()


    end subroutine mp_driver

  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


  END MODULE mp_driver_module
