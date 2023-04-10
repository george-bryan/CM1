#define TROUBLE 0
#define PITER 1048576
#ifdef _B4B
#undef _B4B01F
#endif
  MODULE droplet_module

  implicit none

  private
  public :: droplet_driver,droplet_diag

  ! If we already know that a parcel stays on the same process 
  !    in the previous time step and it is still here, we start 
  !    the location search from the previous index and limit 
  !    the search range using the parameter below;
  ! This assumes that a parcel does not move too far and does
  !    not move between processes within this subroutine?
  integer, parameter :: location_offset = 16

  ! set this variable to ".true." to rebounce the particles
  ! falling off the bottom (i.e., conserve the total droplet number)
  logical, parameter :: use_wall_bc = .true.

  !For testing: set this parameter to ".false." to make droplet injection deterministic (not random)
  logical, parameter :: random_droplets = .true.

  CONTAINS

      subroutine droplet_driver(dt,mtime,xh,uh,ruh,xf,yh,vh,rvh,yf,zh,mh,rmh,zf,mf,zs,    &
                               sigma,sigmaf,znt,rho,ua,va,wa,s10,pdata,                   &
                               th_in,qa,th0,pi0,ppa,prs,                                  &
                               pw1,pw2,pe1,pe2,ps1,ps2,pn1,pn2,                           &
                               nw1,nw2,ne1,ne2,sw1,sw2,se1,se2,reqs_p,                    &
                               sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,                   &
                               n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s,            &
                               uten,vten,wten,thten,qten,pdata_locind)
      use input, only : ib,ie,jb,je,kb,ke,ibl,iel,jbl,jel,numq,ni,nj,nk,imp,jmp,kmp,rmp,cmp, &
          kmt,npvals,npvars,cgs1,cgs2,cgs3,cgt1,cgt2,cgt3,bbc,tbc,imove,zt,rzt, &
          umove,vmove,nqv,terrain_flag,nx,ny,axisymm,nodex,nodey,myi,myj,viscosity, &
          prx,pry,prz,prsig,pract,prvpx,prvpy,prvpz,prtp,prrp,prmult,prms,przs,pru,prv,prw,prt,prqv,prprs,prrho, &
          timestats,time_droplet,time_droplet_inject, mytime,ierr,time_droplet_reduce,maxx,maxy,maxz, &
          ierr,mynw,mysw,myne,myse,mynorth,mysouth,myeast,mywest,myid,nparcelsLocal,nparcelsLocalActive, &
          time_phys_H2D,time_phys_D2H
      use constants
      use comm_module
      use comm_droplet_module
      use misclibs
      use parcel_module
      use cm1libs , only : eslf
#ifdef MPI
      use mpi
#endif
      use MersenneTwister_mod

      implicit none

!-----------------------------------------------------------------------
!  This subroutine updates the parcel locations
!-----------------------------------------------------------------------

      real, intent(in) :: dt
      double precision, intent(in) :: mtime
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
      real, intent(in), dimension(ibl:iel,jbl:jel) :: s10
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: rho,prs
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: pi0,th0,th_in,ppa
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: thten,qten
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke,numq) :: qa
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: ua,uten
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: va,vten
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: wa,wten
      real, intent(inout), dimension(nparcelsLocal,npvals) :: pdata

      real, intent(inout), dimension(jmp,kmp) :: pw1,pw2,pe1,pe2
      real, intent(inout), dimension(imp,kmp) :: ps1,ps2,pn1,pn2
      real, intent(inout), dimension(kmt) :: nw1,nw2,ne1,ne2,sw1,sw2,se1,se2
      integer, intent(inout), dimension(rmp) :: reqs_p

      real, intent(inout), dimension(cmp,jmp,kmp)   :: sw31,sw32,se31,se32
      real, intent(inout), dimension(imp,cmp,kmp)   :: ss31,ss32,sn31,sn32
      real, intent(inout), dimension(cmp,cmp,kmt+1) :: n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2
      integer, intent(inout), dimension(rmp) :: reqs_s
      integer, intent(inout), dimension(nparcelsLocal,3) :: pdata_locind    ! x/y/z location index of each parcel

      !Need to compute the true temperature
      real, dimension(ib:ie,jb:je,kb:ke) :: ta

      integer :: n,np,i,j,k,iflag,jflag,kflag
      integer :: ix,iy,iz
      integer :: num100,num1000
      integer, dimension(2) :: numvec
      real :: x3d,y3d,z3d
      integer :: nrkp
      real :: dt2,uu1,vv1,ww1
      real :: sig3d,th_tmp,rand,s10_xym
      real :: dvpdt1,dvpdt2,dvpdt3,drpdt,dtpdt,dmpdt, &
              rp0,taup0,rhop0,x3d0,y3d0,z3d0,sig3d0

      real :: Nup,salinity,part_grav1,part_grav2,part_grav3
      real :: xv,yv,zv,dV,wtx,wty,wtz,wtt
      real :: esl

      real :: uval,vval,wval,qval,prsval,tval,rhoval
      real :: sigdot

      logical, parameter :: debug = .false.

      integer :: values(8),local_seed

      type(randomNumberSequence) :: randomNumbers
!      real, dimension(kb:ke+1) :: zf_tmp

      integer :: num_fallout,idx,np_tmp                        !droplets which fell out of domain
      integer, dimension(:), allocatable :: holes_fallout_ind  !their location index in pdata prior to removal

#ifdef MPI 
      integer :: num_holes                                 ! how many new droplets could be added to "pdata"
      integer, dimension(:), allocatable :: holes_ind      ! location index in "pdata" that can add a new droplet
      integer :: Depart(num_nn)                            ! Number of droplets that will enter each nearest 
                                                           ! neighbor at different directions
      integer :: Arrive(num_nn)                            ! Number of droplets that will enter the current
                                                           ! MPI region from each nearest neighbor
      integer, dimension(nparcelsLocal) :: pdata_neighbor  ! array to store the new MPI region info for all the droplets
#endif
      integer :: neighbor                                  ! indicate which MPI region a droplet stays after this time step 

#ifdef _VERIFY_FIND_LOC
      integer :: tmp_flag
#endif

      call date_and_time(values=values)
      ! 1) To match the initial and restart runs for total droplet number,
      !    use the following form to calculate the seed value so that it is 
      !    deterministic but varies between different MPI ranks and time steps
      ! 2) Otherwise, use David's original form with date and time to achieve
      !    more randomness but unmatched results between initial and restart runs
      local_seed = int(mtime) * 1000 + myid          ! -(myid+values(8)+values(7)+values(6))
      randomNumbers = new_RandomNumberSequence(seed = local_seed)

      num_fallout = 0  !Reset the number of droplets that have fallen out of physical domain

!----------------------------------------------------------------------
!  apply bottom/top boundary conditions:
!  [Note:  for u,v the array index (i,j,0) means the surface, ie z=0]
!     (for the parcel subroutines only!)

#ifdef MPI
      !$acc data create  (ta) &
      !$acc      copyout (pdata_neighbor) &
      !$acc      copyin  (randomNumbers,randomNumbers%state)

      !$acc parallel loop gang vector default(present)
      do np = 1, nparcelsLocalActive
         pdata_neighbor(np) = undefined_index
      end do
      !$acc end parallel
#else
      !$acc data create  (ta) &
      !$acc      copyin  (randomNumbers,randomNumbers%state)
#endif

    IF(bbc.eq.1)THEN
      ! free slip ... extrapolate:
      !$acc parallel loop gang vector collapse(2) default(present)
      do j=jb,je
         do i=ib,ie+1
            ua(i,j,0) = cgs1*ua(i,j,1)+cgs2*ua(i,j,2)+cgs3*ua(i,j,3)
         end do
      end do
      !$acc end parallel
      !$acc parallel loop gang vector collapse(2) default(present)
      do j=jb,je+1
         do i=ib,ie
            va(i,j,0) = cgs1*va(i,j,1)+cgs2*va(i,j,2)+cgs3*va(i,j,3)
         end do
      end do
      !$acc end parallel
    ELSEIF(bbc.eq.2)THEN
      ! no slip:
      if( imove.eq.1 )then
        !$acc parallel loop gang vector collapse(2) default(present)
        do j=jb,je
           do i=ib,ie+1
              ua(i,j,0) = 0.0 - umove
           end do
        end do
        !$acc end parallel
        !$acc parallel loop gang vector collapse(2) default(present)
        do j=jb,je+1
           do i=ib,ie
              va(i,j,0) = 0.0 - vmove
           end do
        end do
        !$acc end parallel
      else
        !$acc parallel loop gang vector collapse(2) default(present)
        do j=jb,je
           do i=ib,ie+1
              ua(i,j,0) = 0.0
           end do
        end do
        !$acc end parallel
        !$acc parallel loop gang vector collapse(2) default(present)
        do j=jb,je+1
           do i=ib,ie
              va(i,j,0) = 0.0
           end do
        end do
        !$acc end parallel
      endif
    ELSEIF(bbc.eq.3)THEN
      ! u,v near sfc are determined below using log-layer equations
    ENDIF

!----------

    IF(tbc.eq.1)THEN
      ! free slip ... extrapolate:
      !$acc parallel loop gang vector collapse(2) default(present)
      do j=jb,je
         do i=ib,ie+1
            ua(i,j,nk+1) = cgt1*ua(i,j,nk)+cgt2*ua(i,j,nk-1)+cgt3*ua(i,j,nk-2)
         end do
      end do
      !$acc end parallel
      !$acc parallel loop gang vector collapse(2) default(present)
      do j=jb,je+1
         do i=ib,ie
            va(i,j,nk+1) = cgt1*va(i,j,nk)+cgt2*va(i,j,nk-1)+cgt3*va(i,j,nk-2)
         end do
      end do
      !$acc end parallel
    ELSEIF(tbc.eq.2)THEN
      ! no slip:
      !$acc parallel loop gang vector collapse(2) default(present)
      do j=jb,je
         do i=ib,ie+1
            ua(i,j,nk+1) = 0.0
         end do
      end do
      !$acc end parallel
      !$acc parallel loop gang vector collapse(2) default(present)
      do j=jb,je+1
         do i=ib,ie
            va(i,j,nk+1) = 0.0
         end do
      end do
      !$acc end parallel
    ENDIF

!----------

    !$acc parallel loop gang vector collapse(2) default(present)
    do j=jb,je
       do i=ib,ie
          wa(i,j,nk+1) = 0.0
       end do
    end do
    !$acc end parallel

  !Compute temperature for interpolation

  !$acc parallel default(present) private(i,j,k)
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

  ! GHB 210714:
    call prepcorners3_GPU( ta,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,  &
                          n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s,1)
    call prepcorners3_GPU(rho,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,  &
                          n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s,1)
    call prepcorners3_GPU(prs,sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,  &
                          n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s,1)
    call prepcorners3_GPU(qa(ib,jb,kb,nqv),sw31,sw32,se31,se32,ss31,ss32,sn31,sn32,  &
                          n3w1,n3w2,n3e1,n3e2,s3w1,s3w2,s3e1,s3e2,reqs_s,1)

!----------------------------------------------------------------------
!  Loop through all parcels:  if you have it, update it's properties

    dt2 = dt/2.0
    part_grav1 = 0.0
    part_grav2 = 0.0
    part_grav3 = -9.81

    num100 = 0
    num1000 = 0

    !print *,'HOST: nparcelsLocalActive: ',nparcelsLocalActive
    !!$acc parallel
    !print *,'DEVICE: nparcelsLocalActive: ',nparcelsLocalActive
    !!$acc end parallel
#ifdef _B4B01F
    !$acc update &
    !$acc host(pdata,xf,yf,zf,zh,sigma,sigmaf, &
    !$acc      ua,va,wa,qa,ta,rho,prs,znt,qten,thten,zs)
#else
    !JMD WARNING: Loop does not yet match CPU version.
    !$acc parallel default(present)
    !$acc loop gang vector reduction(+:num_fallout)
#endif
    nploop:  &
    DO np=1,nparcelsLocalActive

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

#ifdef MPI
! JS: comment out this conflict check for the new MPI interface;
!     otherwise there will be a droplet leak

      ! check for conflict:
!    IF( (iflag.ge.1.and.iflag.le.ni) .and.   &
!        (jflag.ge.1.and.jflag.le.nj) )THEN
!      IF( iflag.eq.ni .and. pdata(np,prx).eq.xf(iflag+1) .and. nodex.gt.1 .and.  myi.ne.nodex ) iflag = -1
!      IF( jflag.eq.nj .and. pdata(np,pry).eq.yf(jflag+1) .and. nodey.gt.1 .and.  myj.ne.nodey ) jflag = -1
!    ENDIF
#endif

      kflag = 1
      if( .not. terrain_flag )then
        !JS-KLUDGE: Somehow I have to use the zf_tmp variable 
        !           for the find_vertical* subroutine,
        !           otherwise the GPU code breaks for nparcels > ~2000
!        !$acc loop seq
!        do k = kb, ke+1
!           zf_tmp(k) = zf(iflag,jflag,k)
!        end do
         do while( z3d.gt.zf(iflag,jflag,kflag+1) )
            kflag = kflag+1
         enddo
         pdata_locind(np,3) = kflag
!         call find_vertical_location_index (pdata_locind(np,3), z3d, kb, ke+1, zf_tmp, kflag, .TRUE.)
      else
         call find_vertical_location_index (pdata_locind(np,3), sig3d, kb, ke+1, sigmaf, kflag, .TRUE.)
      endif

      dvpdt1 = pdata(np,prvpx)
      dvpdt2 = pdata(np,prvpy)
      dvpdt3 = pdata(np,prvpz)
      dtpdt  = pdata(np,prtp)
      drpdt  = pdata(np,prrp)
      dmpdt  = rhow*4.0/3.0*pi*pdata(np,prrp)**3
      
      !Store its old position before getting updated:
      x3d0   = x3d
      y3d0   = y3d
      z3d0   = z3d 
      sig3d0 = sig3d

      call interpolate_to_parcel(np,iflag,jflag,kflag,x3d,y3d,z3d,sig3d, &
                                 uval,vval,wval,tval,qval,rhoval,prsval, &
                                 xh,xf,yh,yf,zh,zf,zs,znt,sigma,sigmaf, &
                                 ua,va,wa,ta,qa,rho,prs,sigdot)


      call BE_integration(dt,np,pdata,pdata_locind,xf,yf,x3d,y3d,z3d,sig3d, &
                          uval,vval,wval,qval,rhoval,prsval,tval,Nup,rhop0, &
                          taup0,rp0,part_grav1,part_grav2,part_grav3,debug, &
                          num100,num1000,neighbor,num_fallout)


#ifdef MPI
      pdata_neighbor(np) = neighbor 
#endif

!-----------------------------------------------------
!  Now perform the two-way coupling based on the changes in droplet momentum,
!  temp, and size
!-----------------------------------------------------

      !Recall that the "old" values are stored in the "d/dt" variables
      dvpdt1 = (pdata(np,prvpx)-dvpdt1)/dt   !Remember to deal with gravity later!
      dvpdt2 = (pdata(np,prvpy)-dvpdt2)/dt
      dvpdt3 = (pdata(np,prvpz)-dvpdt3)/dt
      drpdt  = (pdata(np,prrp)-drpdt)/dt
      dtpdt  = (pdata(np,prtp)-dtpdt)/dt
      dmpdt  = (rhow*4.0/3.0*pi*pdata(np,prrp)**3-dmpdt)/dt


      !Project the feedback onto the grid, using ORIGINAL location
      call project_feedback(iflag,jflag,kflag,x3d0,y3d0,z3d0,sig3d0, &
                            rhop0,taup0,rp0,tval,uten,vten,wten,qten,thten, &
                            xh,xf,yh,yf,zh,zf,sigma,sigmaf, &
                            sigdot,dvpdt1,dvpdt2,dvpdt3,drpdt,dtpdt,dmpdt, &
                            rhoval,part_grav1,part_grav2,part_grav3,pdata,np)

    ENDDO  nploop
    !$acc end parallel

    !$acc end data
    if(timestats.ge.1) time_droplet=time_droplet+mytime()

    !$acc update host(pdata,pdata_locind)

    if(timestats.ge.1) time_phys_D2H=time_phys_D2H+mytime()

    numvec(1) = num100
    numvec(2) = num1000

#ifdef MPI

    if ( myid == 0 ) then
       call mpi_reduce(mpi_in_place,numvec,2,mpi_int,mpi_sum,0,mpi_comm_world,ierr)
    else
       call mpi_reduce(numvec,numvec,2,mpi_int,mpi_sum,0,mpi_comm_world,ierr)
    end if

#endif

    if (myid==0) write(*,*) 'num100,num1000 = ',numvec(1),numvec(2)


!----------------------------------------------------------------------
!  remove the "holes" due to droplets which fell out: collapse pdata and adjust nparcelsLocalActive
!  num_fallout already contains the number which have fallen out

   allocate(holes_fallout_ind(num_fallout))

   !List the spots in pdata which have "dead" particles
   idx=1
   do np=1,nparcelsLocalActive
      if (pdata(np,pract) .lt. 0.0) then  !If it has been flagged as falling out
         holes_fallout_ind(idx) = np
         idx=idx+1
      end if
   end do

   !Now transfer these to the end of the pdata list
   !Simply move the last particle in the list, pdata(nparcelsLocalActive,:), to a hole and update nparcelsLocalActive
   do np=num_fallout,1,-1
      if (holes_fallout_ind(np) .eq. nparcelsLocalActive) then
         nparcelsLocalActive = nparcelsLocalActive-1  !If the dead particle is at end, simply reduce nparcelsLocalActive
      else
         pdata(holes_fallout_ind(np),:) = pdata(nparcelsLocalActive,:)
         pdata_locind(holes_fallout_ind(np),:) = pdata_locind(nparcelsLocalActive,:)
         pdata_neighbor(holes_fallout_ind(np)) = pdata_neighbor(nparcelsLocalActive)
         nparcelsLocalActive = nparcelslocalActive-1
      end if
      ! reset all the information for this "dead" droplet
      pdata_locind(nparcelsLocalActive+1,:) = undefined_index
      pdata(nparcelsLocalActive+1,:) = neg_huge
      pdata_neighbor(nparcelsLocalActive+1) = undefined_index
   end do

   deallocate(holes_fallout_ind)

    if(timestats.ge.1) time_droplet=time_droplet+mytime()

!----------------------------------------------------------------------
!  communicate data  (for MPI runs)

#ifdef MPI

    ! JS: send/receive the droplet information through MPI;
    !     all the calculations below are done on CPU

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Step 1: Count the "num_holes", which is the number of "holes" in "pdata" !
    !         at the beginning of this time step + the number of droplets      !
    !         leaving this MPI region at this time step                        !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    num_holes = 0
    do np = 1, nparcelsLocal
       if ( (pdata_locind(np,1) .eq. undefined_index) .and. &
            (pdata_locind(np,2) .eq. undefined_index) ) then
          num_holes = num_holes + 1
       end if
    end do

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Step 2: Allocate "hole" index array and initialize it ! 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    allocate(holes_ind(num_holes))
    do np = 1, num_holes
       holes_ind(np) = undefined_index
    end do

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Step 3: Exchange the number of droplets that will enter/leave !
    !         the current MPI region with its nearest neighbor      !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call comm_droplet_number(holes_ind,Depart,Arrive,pdata, &
                             pdata_locind,pdata_neighbor)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Step 4: Exchange the detailed information of the droplets that will  !
    !         enter/leave the current MPI region with its nearest neighbor !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call comm_droplet_value(holes_ind,Depart,Arrive,pdata, &
                            pdata_locind,pdata_neighbor)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Step 5: Update the count of new active droplets with contiguous memory !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    nparcelsLocalActive = nparcelsLocalActive + sum(Arrive) - sum(Depart)

    if(timestats.ge.1) time_droplet_reduce=time_droplet_reduce+mytime()

    ! free up the memory for temporary variables

    deallocate(holes_ind)

#endif


!----------------------------------------------------------------------
!  get height ASL:

      if( terrain_flag )then
            call getparcelzs(xh,uh,ruh,xf,yh,vh,rvh,yf,zs,pdata)
            DO np=1,nparcelsLocalActive
              ! get z from sigma:
              ! (see Section 3 of "The governing equations for CM1", 
              !  http://www2.mmm.ucar.edu/people/bryan/cm1/cm1_equations.pdf)
              pdata(np,prz) = pdata(np,przs) + pdata(np,prsig)*((zt-pdata(np,przs))*rzt)
            ENDDO
      endif

    if(timestats.ge.1) time_droplet=time_droplet+mytime()

!----------------------------------------------------------------------
! Final thing: Inject droplets which will be active on the next call of droplet_driver
! Requires inputting the current 10-meter wind speed
   
   if ( .not. use_wall_bc ) then  
      s10_xym = 0.0
      !$acc parallel loop gang vector reduction(+:s10_xym)
      do j=1,nj
         do i=1,ni
            s10_xym = s10_xym + s10(i,j)
         end do
      end do
      !$acc end parallel
#ifdef MPI
      call mpi_allreduce(mpi_in_place,s10_xym,1,mpi_real,mpi_sum,mpi_comm_world,ierr)
#endif
      s10_xym = s10_xym/real(nx*ny)
      if (myid==0) write(*,*) 'DHR U10',s10_xym
      call droplet_injection(pdata,xf,yf,dt,s10_xym,randomNumbers)
#ifdef MPI
      call mpi_allreduce(nparcelsLocalActive,np_tmp,1,mpi_int,mpi_sum,mpi_comm_world,ierr)
#endif
      if (myid==0) then 
         write(*,'(a8,3i)')  'DHR1:',myid,np_tmp,nparcelsLocalActive
      end if
   end if

   if(timestats.ge.1) time_droplet_inject=time_droplet_inject+mytime()
  
   !$acc update device(pdata,pdata_locind)
   if(timestats.ge.1) time_phys_H2D=time_phys_H2D+mytime()
   

   end subroutine droplet_driver

!----------------------------------------------------------------------

   subroutine BE_integration(dt,np,pdata,pdata_locind,xf,yf,x3d,y3d, &
                                z3d,sig3d,uval,vval,wval,qval,rhoval, &
                                prsval,tval,Nup,rhop0,taup0,rp0, &
                                part_grav1,part_grav2,part_grav3, &
                                debug,num100,num1000,neighbor,num_fallout)
      !$acc routine seq
      use input, only : ib,ie,jb,je,kb,ke,numq,npvals,nx,ny,viscosity, &
          pr_num,sc_num,axisymm,terrain_flag,maxz,minx,maxx,miny,maxy, &
          prvpx,prvpy,prvpz,prrp,prms,prtp,prx,pry,prsig,prz,pru,prv,prw,prt,prqv,prprs,prrho,pract, &
          ni,nj,mywest,mysw,mynw,myeast,myse,myne,mysouth,mynorth,nparcelsLocal,myid,dx,dy
      use constants
      use comm_module
      use misclibs
      use parcel_module
      use cm1libs , only : eslf
      use mpi

      implicit none

      real, intent(in), dimension(ib:ie+1) :: xf
      real, intent(in), dimension(jb:je+1) :: yf
      integer, intent(inout) :: num100,num1000
      integer, intent(in) :: np
      logical, intent(in) :: debug
      real, intent(inout) :: x3d,y3d,z3d,sig3d
      real, intent(in) :: uval,vval,wval,qval,rhoval,prsval,tval
      real, intent(inout), dimension(nparcelsLocal,npvals) :: pdata
      integer, intent(inout), dimension(nparcelsLocal,3) :: pdata_locind
      real, intent(out) :: Nup
      real, intent(inout) :: rhop0,taup0,rp0
      real, intent(in) :: dt,part_grav1,part_grav2,part_grav3
      integer, intent(out), optional :: neighbor  ! indicate which MPI region a droplet 
                                                  ! stays after this time step 

      real :: dt2
      real :: xrhs1,xrhs2,xrhs3,vrhs1,vrhs2,vrhs3
      real :: vtmp1,vtmp2,vtmp3
      real :: k1xp1,k1xp2,k1xp3,k1vp1,k1vp2,k1vp3
      real :: tprhs,tptmp
      real :: k1tp,k2tp,k1rp,k2rp
      real :: rprhs,rptmp,qstar,einf,estar
      real :: taup,Rep,Shp,diffnorm
      real :: rhop,volp,tp0,volp0
      real :: sig1
      real :: top,bot
      integer :: i,j,k
      real :: taup_scale,dt_nondim
      double precision :: rt_zeros(2),rt_start(2)
      real :: guess
      integer :: mflag,flag

      integer :: num_fallout


      !Store interpolated quantities for the sake of statistics
      pdata(np,pru) = uval
      pdata(np,prv) = vval
      pdata(np,prw) = wval
  
      pdata(np,prt) = tval
      pdata(np,prqv) = qval
      pdata(np,prprs) = prsval
      pdata(np,prrho) = rhoval


      estar = eslf(prsval,tval)  !Saturation humidity based on interpolated temp,pressure

!-----------------------------------------------------
!  Update droplet position, velocity, etc.
!-----------------------------------------------------

      xrhs1 = pdata(np,prvpx)
      xrhs2 = pdata(np,prvpy)
      xrhs3 = pdata(np,prvpz)
      volp  = 4.0/3.0*pi*pdata(np,prrp)**3
      rhop  = (pdata(np,prms)+volp*rhow)/volp  !Density including the solute mass
      taup  = rhop*(2.0*pdata(np,prrp))**2/18.0/rhoval/viscosity

      !Original, for calculating changes in momentum, mass, and energy
      rhop0 = rhop
      taup0 = taup
      rp0   = pdata(np,prrp)
      tp0   = pdata(np,prtp)
      volp0 = 4.0/3.0*pi*rp0**3


      diffnorm = sqrt( (uval-pdata(np,prvpx))**2+ &
                       (vval-pdata(np,prvpy))**2+ &
                       (wval-pdata(np,prvpz))**2)

      Rep = 2.0*pdata(np,prrp)*diffnorm/viscosity
      Nup = 2.0 + 0.6*sqrt(Rep)*pr_num**(1.0/3.0)
      Shp = 2.0 + 0.6*sqrt(Rep)*sc_num**(1.0/3.0)

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
      
      guess = 0.0


      !Gives initial guess into nonlinear solver
      !mflag = 0, has equilibrium radius; mflag = 1, no equilibrium (uses itself as initial guess)
      call rad_solver2(guess,mflag,prsval,rhoval,tval,qval,pdata(np,prtp),pdata(np,prms),pdata(np,prrp),np)

      !Nondimensionalize the guesses
      if (mflag.eq.0) then
         rt_start(1) = guess/dble(pdata(np,prrp))
         rt_start(2) = dble(tval)/dble(pdata(np,prtp))
      else
         rt_start(1) = 1.0
         rt_start(2) = 1.0
      end if


      !Start using the standard Gauss-Newton algorithm
      call gauss_newton_2d(dble(diffnorm),dble(dt_nondim),dble(taup_scale),rt_start,rt_zeros,flag,dble(prsval),dble(rhoval),dble(tval),dble(qval),dble(pdata(np,prtp)),dble(pdata(np,prms)),dble(pdata(np,prrp)),np)

      !If Gauss-Newton failed to converge, flag is set to 1 and try Levenberg-Marquardt algorithm
      if (flag==1) then
         num100 = num100+1
         call LV_solver(dble(diffnorm),dble(dt_nondim),dble(taup_scale),rt_start,rt_zeros,flag,dble(prsval),dble(rhoval),dble(tval),dble(qval),dble(pdata(np,prtp)),dble(pdata(np,prms)),dble(pdata(np,prrp)),np)
      end if

      !In rare cases both nonlinear solvers fail to converge
      if (flag==1)  then

       num1000 = num1000+1 

       !If this happens keep radius unchanged and temp equal to surrounding
       rt_zeros(1) = 1.0
       rt_zeros(2) = tval/pdata(np,prtp)

      end if

      !Now redimsionalize
      pdata(np,prrp) = rt_zeros(1)*pdata(np,prrp)
      pdata(np,prtp) = rt_zeros(2)*pdata(np,prtp)

      !Finally, take care of particle BCs

      !Wall BCs:
      top = maxz-1.0e-4
      bot = 1.0e-4
      if ( z3d .gt. top ) then
         z3d = top - (z3d - top)
         pdata(np,prvpz) = -pdata(np,prvpz)
         pdata_locind(np,3) = undefined_index
      elseif ( z3d .lt. bot ) then
         if ( use_wall_bc ) then
            ! use Wall BC
            z3d = bot + (bot-z3d)
            pdata(np,prvpz) = -pdata(np,prvpz)
         else
            ! fall off the bottom and "die"
            z3d = 1.0e-6            !Just temporary 
            pdata(np,pract) = -1.0  !Signal this as no longer alive
            num_fallout = num_fallout + 1
         end if 
         pdata_locind(np,3) = undefined_index
      endif

      ! New for cm1r17:  if parcel exits domain,
      ! just assume periodic lateral boundary conditions
      ! (no matter what actual settings are for wbc,ebc,sbc,nbc)

#ifdef MPI


      ! determine which new MPI region this droplet 
      ! enters or stays for the next time step

      if ( (x3d .ge. xf(1)) .and. (x3d .le. xf(ni+1)) .and.  &
           (y3d .ge. yf(1)) .and. (y3d .le. yf(nj+1)) ) then
           neighbor = undefined_index   ! undefined_index means this droplet
                                        ! stays within the same MPI region
      else if ( (x3d .lt. xf(1)) .and. (y3d .ge. yf(1)) .and. (y3d .le. yf(nj+1)) ) then
           if ( x3d .lt. xf(1)-dx*ni ) then
              stop "Jump over the west nearest neighbor"
           end if
           neighbor = mywest
      else if ( (x3d .lt. xf(1)) .and. (y3d .lt. yf(1)) ) then
           if ( (x3d .lt. xf(1)-dx*ni) .or. (y3d .lt. yf(1)-dy*nj) ) then
              stop "Jump over the southwest nearest neighbor"
           end if
           neighbor = mysw
      else if ( (x3d .lt. xf(1)) .and. (y3d .gt. yf(nj+1)) ) then
           if ( (x3d .lt. xf(1)-dx*ni) .or. (y3d .gt. yf(nj+1)+dy*nj) ) then
              stop "Jump over the northwest nearest neighbor"
           end if
           neighbor = mynw
      else if ( (x3d .gt. xf(ni+1)) .and. (y3d .ge. yf(1)) .and. (y3d .le. yf(nj+1)) ) then
           if ( x3d .gt. xf(ni+1)+dx*ni ) then
              stop "Jump over the east nearest neighbor"
           end if
           neighbor = myeast
      else if ( (x3d .gt. xf(ni+1)) .and. (y3d .lt. yf(1)) ) then
           if ( (x3d .gt. xf(ni+1)+dx*ni) .or. (y3d .lt. yf(1)-dy*nj) ) then
              stop "Jump over the southeast nearest neighbor"
           end if
           neighbor = myse
      else if ( (x3d .gt. xf(ni+1)) .and. (y3d .gt. yf(nj+1)) ) then
           if ( (x3d .gt. xf(ni+1)+dx*ni) .or. (y3d .gt. yf(nj+1)+dy*nj) ) then
              stop "Jump over the northeast nearest neighbor"
           end if
           neighbor = myne
      else if ( (x3d .ge. xf(1)) .and. (x3d .le. xf(ni+1)) .and. (y3d .lt. yf(1)) ) then
           if ( y3d .lt. yf(1)-dy*nj ) then
              stop "Jump over the south nearest neighbor"
           end if
           neighbor = mysouth
      else if ( (x3d .ge. xf(1)) .and. (x3d .le. xf(ni+1)) .and. (y3d .gt. yf(nj+1)) ) then
           if ( y3d .gt. yf(nj+1)+dy*nj ) then
              stop "Jump over the north nearest neighbor"
           end if
           neighbor = mynorth
      else
           stop "This section should be never entered!"
      end if

      if ( neighbor .ne. undefined_index ) then
         pdata_locind(np,1) = undefined_index
         pdata_locind(np,2) = undefined_index
         pdata_locind(np,3) = undefined_index
      end if

#endif

      if ( x3d .lt. minx ) then
         x3d = x3d + ( maxx - minx )
         pdata_locind(np,1) = undefined_index
      end if
      if ( x3d .gt. maxx ) then
         x3d = x3d - ( maxx - minx )
         pdata_locind(np,1) = undefined_index
      end if

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
      else
        pdata(np,prsig)=sig3d
      endif

      ! If this droplet falls outside the bottom boundary,
      ! it "dies" and should not enter any other MPI rank
      ! even if its x/y location is valid
      if (pdata(np,pract) .lt. 0.0) then
         neighbor = undefined_index
         pdata_locind(np,1) = undefined_index
         pdata_locind(np,2) = undefined_index
      end if

      if (pdata_locind(np,1) .eq. undefined_index .or. &
          pdata_locind(np,2) .eq. undefined_index) then
          pdata_locind(np,3) = undefined_index
      end if

      end subroutine BE_integration

      subroutine interpolate_to_parcel(np,iflag,jflag,kflag,x3d,y3d,z3d,sig3d,uval,vval,wval,tval,qval,rhoval,prsval,xh,xf,yh,yf,zh,zf,zs,znt,sigma,sigmaf,ua,va,wa,ta,qa,rho,prs,sigdot)
      !$acc routine seq
      use input, only : ib,ie,jb,je,kb,ke,numq,nparcelsLocal,nx,ny, &
                        axisymm,terrain_flag,ni,nj,nip1,nk,nkp1, &
                        zt,rzt,imoist,nqv,bbc,imove,umove,vmove
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

      integer, intent(in) :: np
      integer, intent(in) :: iflag,jflag,kflag
      real, intent(inout) :: x3d,y3d,z3d,sig3d,uval,vval,wval,tval,qval,rhoval,prsval,sigdot
      real :: rx,ry,rz,w1,w2,w3,w4,w5,w6,w7,w8,wsum
      real :: rxu,ryv,rzw,rxs,rys,rzs
      real :: zsp,rznt,z0,var
      integer :: i,j,k


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
      rhoval = tri_interp(ni,nj,nk,i,j,k,w1,w2,w3,w4,w5,w6,w7,w8,rho)
      ! rhoval = rho(i  ,j  ,k  )*w1 &
      !        + rho(i+1,j  ,k  )*w2 &
      !        + rho(i  ,j+1,k  )*w3 &
      !        + rho(i  ,j  ,k+1)*w4 &
      !        + rho(i+1,j  ,k+1)*w5 &
      !        + rho(i  ,j+1,k+1)*w6 &
      !        + rho(i+1,j+1,k  )*w7 &
      !        + rho(i+1,j+1,k+1)*w8
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

      subroutine project_feedback(iflag,jflag,kflag,x3d,y3d,z3d,sig3d, &
                                  rhop0,taup0,rp0,tval,uten,vten,wten, &
                                  qten,thten,xh,xf,yh,yf,zh,zf,sigma,  &
                                  sigmaf,sigdot,dvpdt1,dvpdt2,dvpdt3,  &
                                  drpdt,dtpdt,dmpdt,rhoval,part_grav1, &
                                  part_grav2,part_grav3,pdata,np)
      !$acc routine seq
      use input, only : ib,ie,jb,je,kb,ke,numq,nparcelsLocal,nx,ny, &
                        axisymm,terrain_flag,ni,nj,nip1,nk,nkp1, &
                        zt,rzt,imoist,nqv,bbc,imove,umove,vmove, &
                        npvals,nparcelsLocal,prrp,prms,prmult, &
                        pract,prt,viscosity,nparcelsLocalActive
      use constants
      use parcel_module
      use cm1libs , only : eslf
      implicit none

      real, intent(in) :: dvpdt1,dvpdt2,dvpdt3,drpdt,dtpdt,dmpdt
      real, intent(in), dimension(nparcelsLocal,npvals) :: pdata
      real, intent(in) :: part_grav1,part_grav2,part_grav3

      real, intent(in), dimension(ib:ie+1) :: xf
      real, intent(in), dimension(jb:je+1) :: yf
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf
      real, intent(in), dimension(ib:ie) :: xh
      real, intent(in), dimension(jb:je) :: yh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(kb:ke) :: sigma
      real, intent(in), dimension(kb:ke+1) :: sigmaf
      real, intent(inout), dimension(ib:ie+1,jb:je,kb:ke) :: uten
      real, intent(inout), dimension(ib:ie,jb:je+1,kb:ke) :: vten
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke+1) :: wten
      real, intent(inout), dimension(ib:ie,jb:je,kb:ke) :: thten,qten

      integer, intent(in) :: np
      integer, intent(in) :: iflag,jflag,kflag
      real, intent(in) :: x3d,y3d,z3d,sig3d,rhop0,taup0,rp0,tval,rhoval
      real :: rx,ry,rz,w1,w2,w3,w4,w5,w6,w7,w8,wsum
      real :: rxu,ryv,rzw,rxs,rys,rzs
      real :: zsp,rznt,z0,var,lhv
      real :: partmass,dV,volp,sigdot,tmpval
      integer :: i,j,k,dum

!----------------------------------------------------------------------
!  Project to u points

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


      w1 = (1.0-rx)*(1.0-ry)*(1.0-rz)
      w2 = rx*(1.0-ry)*(1.0-rz)
      w3 = (1.0-rx)*ry*(1.0-rz)
      w4 = (1.0-rx)*(1.0-ry)*rz
      w5 = rx*(1.0-ry)*rz
      w6 = (1.0-rx)*ry*rz
      w7 = rx*ry*(1.0-rz)
      w8 = rx*ry*rz

      if (.not. terrain_flag) then
         dV = (xf(i+1)-xf(i))*(yh(j+1)-yh(j))*(zh(iflag,jflag,k+1)-zh(iflag,jflag,k))
      else
         dV = (xf(i+1)-xf(i))*(yh(j+1)-yh(j))*(sigma(k+1)-sigma(k))
      end if
      
      volp = (4.0/3.0)*pi*pdata(np,prrp)**3
      partmass = pdata(np,prms) + VolP*rhow

      !$acc atomic update
      uten(i,j,k)       = uten(i,j,k)       - partmass/rhoval*(dvpdt1-part_grav1)*w1/dV*pdata(np,prmult)
      !$acc atomic update
      uten(i+1,j,k)     = uten(i+1,j,k)     - partmass/rhoval*(dvpdt1-part_grav1)*w2/dV*pdata(np,prmult)
      !$acc atomic update
      uten(i,j+1,k)     = uten(i,j+1,k)     - partmass/rhoval*(dvpdt1-part_grav1)*w3/dV*pdata(np,prmult)
      !$acc atomic update
      uten(i,j,k+1)     = uten(i,j,k+1)     - partmass/rhoval*(dvpdt1-part_grav1)*w4/dV*pdata(np,prmult)
      !$acc atomic update
      uten(i+1,j,k+1)   = uten(i+1,j,k+1)   - partmass/rhoval*(dvpdt1-part_grav1)*w5/dV*pdata(np,prmult)
      !$acc atomic update
      uten(i,j+1,k+1)   = uten(i,j+1,k+1)   - partmass/rhoval*(dvpdt1-part_grav1)*w6/dV*pdata(np,prmult)
      !$acc atomic update
      uten(i+1,j+1,k)   = uten(i+1,j+1,k)   - partmass/rhoval*(dvpdt1-part_grav1)*w7/dV*pdata(np,prmult)
      !$acc atomic update
      uten(i+1,j+1,k+1) = uten(i+1,j+1,k+1) - partmass/rhoval*(dvpdt1-part_grav1)*w8/dV*pdata(np,prmult)

      !What interpolation is doing, for reference
      ! uval = ua(i  ,j  ,k  )*w1 &
      !      + ua(i+1,j  ,k  )*w2 &
      !      + ua(i  ,j+1,k  )*w3 &
      !      + ua(i  ,j  ,k+1)*w4 &
      !      + ua(i+1,j  ,k+1)*w5 &
      !      + ua(i  ,j+1,k+1)*w6 &
      !      + ua(i+1,j+1,k  )*w7 &
      !      + ua(i+1,j+1,k+1)*w8

!----------------------------------------------------------------------
!  Project to v points

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

      if (.not. terrain_flag) then
         dV = (xh(i+1)-xh(i))*(yf(j+1)-yf(j))*(zh(iflag,jflag,k+1)-zh(iflag,jflag,k))
      else
         dV = (xh(i+1)-xh(i))*(yf(j+1)-yf(j))*(sigma(k+1)-sigma(k))
      end if

      w1 = (1.0-rx)*(1.0-ry)*(1.0-rz)
      w2 = rx*(1.0-ry)*(1.0-rz)
      w3 = (1.0-rx)*ry*(1.0-rz)
      w4 = (1.0-rx)*(1.0-ry)*rz
      w5 = rx*(1.0-ry)*rz
      w6 = (1.0-rx)*ry*rz
      w7 = rx*ry*(1.0-rz)
      w8 = rx*ry*rz

      !$acc atomic update
      vten(i,j,k)       = vten(i,j,k)       - partmass/rhoval*(dvpdt2-part_grav2)*w1/dV*pdata(np,prmult)
      !$acc atomic update
      vten(i+1,j,k)     = vten(i+1,j,k)     - partmass/rhoval*(dvpdt2-part_grav2)*w2/dV*pdata(np,prmult)
      !$acc atomic update
      vten(i,j+1,k)     = vten(i,j+1,k)     - partmass/rhoval*(dvpdt2-part_grav2)*w3/dV*pdata(np,prmult)
      !$acc atomic update
      vten(i,j,k+1)     = vten(i,j,k+1)     - partmass/rhoval*(dvpdt2-part_grav2)*w4/dV*pdata(np,prmult)
      !$acc atomic update
      vten(i+1,j,k+1)   = vten(i+1,j,k+1)   - partmass/rhoval*(dvpdt2-part_grav2)*w5/dV*pdata(np,prmult)
      !$acc atomic update
      vten(i,j+1,k+1)   = vten(i,j+1,k+1)   - partmass/rhoval*(dvpdt2-part_grav2)*w6/dV*pdata(np,prmult)
      !$acc atomic update
      vten(i+1,j+1,k)   = vten(i+1,j+1,k)   - partmass/rhoval*(dvpdt2-part_grav2)*w7/dV*pdata(np,prmult)
      !$acc atomic update
      vten(i+1,j+1,k+1) = vten(i+1,j+1,k+1) - partmass/rhoval*(dvpdt2-part_grav2)*w8/dV*pdata(np,prmult)

      ! vval = va(i  ,j  ,k  )*w1 &
      !      + va(i+1,j  ,k  )*w2 &
      !      + va(i  ,j+1,k  )*w3 &
      !      + va(i  ,j  ,k+1)*w4 &
      !      + va(i+1,j  ,k+1)*w5 &
      !      + va(i  ,j+1,k+1)*w6 &
      !      + va(i+1,j+1,k  )*w7 &
      !      + va(i+1,j+1,k+1)*w8

!----------------------------------------------------------------------
!  Project to w points

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

      if (.not. terrain_flag) then
         dV = (xh(i+1)-xh(i))*(yh(j+1)-yh(j))*(zf(iflag,jflag,k+1)-zf(iflag,jflag,k))
      else
         dV = (xh(i+1)-xh(i))*(yh(j+1)-yh(j))*(sigmaf(k+1)-sigmaf(k))
      end if

      w1 = (1.0-rx)*(1.0-ry)*(1.0-rz)
      w2 = rx*(1.0-ry)*(1.0-rz)
      w3 = (1.0-rx)*ry*(1.0-rz)
      w4 = (1.0-rx)*(1.0-ry)*rz
      w5 = rx*(1.0-ry)*rz
      w6 = (1.0-rx)*ry*rz
      w7 = rx*ry*(1.0-rz)
      w8 = rx*ry*rz

      !$acc atomic update
      wten(i,j,k)       = wten(i,j,k)       - partmass/rhoval*(dvpdt3-part_grav3)*w1/dV*pdata(np,prmult)
      !$acc atomic update
      wten(i+1,j,k)     = wten(i+1,j,k)     - partmass/rhoval*(dvpdt3-part_grav3)*w2/dV*pdata(np,prmult)
      !$acc atomic update
      wten(i,j+1,k)     = wten(i,j+1,k)     - partmass/rhoval*(dvpdt3-part_grav3)*w3/dV*pdata(np,prmult)
      !$acc atomic update
      wten(i,j,k+1)     = wten(i,j,k+1)     - partmass/rhoval*(dvpdt3-part_grav3)*w4/dV*pdata(np,prmult)
      !$acc atomic update
      wten(i+1,j,k+1)   = wten(i+1,j,k+1)   - partmass/rhoval*(dvpdt3-part_grav3)*w5/dV*pdata(np,prmult)
      !$acc atomic update
      wten(i,j+1,k+1)   = wten(i,j+1,k+1)   - partmass/rhoval*(dvpdt3-part_grav3)*w6/dV*pdata(np,prmult)
      !$acc atomic update
      wten(i+1,j+1,k)   = wten(i+1,j+1,k)   - partmass/rhoval*(dvpdt3-part_grav3)*w7/dV*pdata(np,prmult)
      !$acc atomic update
      wten(i+1,j+1,k+1) = wten(i+1,j+1,k+1) - partmass/rhoval*(dvpdt3-part_grav3)*w8/dV*pdata(np,prmult)

      ! wval = wa(i  ,j  ,k  )*w1 &
      !      + wa(i+1,j  ,k  )*w2 &
      !      + wa(i  ,j+1,k  )*w3 &
      !      + wa(i  ,j  ,k+1)*w4 &
      !      + wa(i+1,j  ,k+1)*w5 &
      !      + wa(i  ,j+1,k+1)*w6 &
      !      + wa(i+1,j+1,k  )*w7 &
      !      + wa(i+1,j+1,k+1)*w8

!----------------------------------------------------------------------
!  Project to scalar points

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

      if (.not. terrain_flag) then
         dV = (xh(i+1)-xh(i))*(yh(j+1)-yh(j))*(zh(iflag,jflag,k+1)-zh(iflag,jflag,k))
      else
         dV = (xh(i+1)-xh(i))*(yh(j+1)-yh(j))*(sigma(k+1)-sigma(k))
      end if

      w1 = (1.0-rx)*(1.0-ry)*(1.0-rz)
      w2 = rx*(1.0-ry)*(1.0-rz)
      w3 = (1.0-rx)*ry*(1.0-rz)
      w4 = (1.0-rx)*(1.0-ry)*rz
      w5 = rx*(1.0-ry)*rz
      w6 = (1.0-rx)*ry*rz
      w7 = rx*ry*(1.0-rz)
      w8 = rx*ry*rz

      if(imoist.eq.1)then
        !$acc atomic update
        qten(i,j,k)       = qten(i,j,k)       - dmpdt/rhoval/dV*w1*pdata(np,prmult)
        !$acc atomic update
        qten(i+1,j,k)     = qten(i+1,j,k)     - dmpdt/rhoval/dV*w2*pdata(np,prmult)
        !$acc atomic update
        qten(i,j+1,k)     = qten(i,j+1,k)     - dmpdt/rhoval/dV*w3*pdata(np,prmult)
        !$acc atomic update
        qten(i,j,k+1)     = qten(i,j,k+1)     - dmpdt/rhoval/dV*w4*pdata(np,prmult)
        !$acc atomic update
        qten(i+1,j,k+1)   = qten(i+1,j,k+1)   - dmpdt/rhoval/dV*w5*pdata(np,prmult)
        !$acc atomic update
        qten(i,j+1,k+1)   = qten(i,j+1,k+1)   - dmpdt/rhoval/dV*w6*pdata(np,prmult)
        !$acc atomic update
        qten(i+1,j+1,k)   = qten(i+1,j+1,k)   - dmpdt/rhoval/dV*w7*pdata(np,prmult)
        !$acc atomic update
        qten(i+1,j+1,k+1) = qten(i+1,j+1,k+1) - dmpdt/rhoval/dV*w8*pdata(np,prmult)

        ! qval = qa(i  ,j  ,k  ,nqv)*w1 &
        !      + qa(i+1,j  ,k  ,nqv)*w2 &
        !      + qa(i  ,j+1,k  ,nqv)*w3 &
        !      + qa(i  ,j  ,k+1,nqv)*w4 &
        !      + qa(i+1,j  ,k+1,nqv)*w5 &
        !      + qa(i  ,j+1,k+1,nqv)*w6 &
        !      + qa(i+1,j+1,k  ,nqv)*w7 &
        !      + qa(i+1,j+1,k+1,nqv)*w8
      endif

      lhv=lv1-lv2*pdata(np,prt)

      !$acc atomic update
      thten(i,j,k)       = thten(i,j,k) - (dtpdt - 3*lhv/cpl/rp0*drpdt)*6.0*(rhow/rhop0)*(cpl/cp)*taup0*pi*rp0*viscosity*w1/dV*pdata(np,prmult)

      !$acc atomic update
      thten(i+1,j,k)     = thten(i+1,j,k) - (dtpdt - 3*lhv/cpl/rp0*drpdt)*6.0*(rhow/rhop0)*(cpl/cp)*taup0*pi*rp0*viscosity*w2/dV*pdata(np,prmult)

      !$acc atomic update
      thten(i,j+1,k)     = thten(i,j+1,k) - (dtpdt - 3*lhv/cpl/rp0*drpdt)*6.0*(rhow/rhop0)*(cpl/cp)*taup0*pi*rp0*viscosity*w3/dV*pdata(np,prmult)

      !$acc atomic update
      thten(i,j,k+1)     = thten(i,j,k+1) - (dtpdt - 3*lhv/cpl/rp0*drpdt)*6.0*(rhow/rhop0)*(cpl/cp)*taup0*pi*rp0*viscosity*w4/dV*pdata(np,prmult)

      !$acc atomic update
      thten(i+1,j,k+1)   = thten(i+1,j,k+1) - (dtpdt - 3*lhv/cpl/rp0*drpdt)*6.0*(rhow/rhop0)*(cpl/cp)*taup0*pi*rp0*viscosity*w5/dV*pdata(np,prmult)

      !$acc atomic update
      thten(i,j+1,k+1)   = thten(i,j+1,k+1) - (dtpdt - 3*lhv/cpl/rp0*drpdt)*6.0*(rhow/rhop0)*(cpl/cp)*taup0*pi*rp0*viscosity*w6/dV*pdata(np,prmult)

      !$acc atomic update
      thten(i+1,j+1,k)   = thten(i+1,j+1,k) - (dtpdt - 3*lhv/cpl/rp0*drpdt)*6.0*(rhow/rhop0)*(cpl/cp)*taup0*pi*rp0*viscosity*w7/dV*pdata(np,prmult)

      !$acc atomic update
      thten(i+1,j+1,k+1) = thten(i+1,j+1,k+1) - (dtpdt - 3*lhv/cpl/rp0*drpdt)*6.0*(rhow/rhop0)*(cpl/cp)*taup0*pi*rp0*viscosity*w8/dV*pdata(np,prmult)

      ! tval   = ta(i  ,j  ,k  )*w1 &
      !        + ta(i+1,j  ,k  )*w2 &
      !        + ta(i  ,j+1,k  )*w3 &
      !        + ta(i  ,j  ,k+1)*w4 &
      !        + ta(i+1,j  ,k+1)*w5 &
      !        + ta(i  ,j+1,k+1)*w6 &
      !        + ta(i+1,j+1,k  )*w7 &
      !        + ta(i+1,j+1,k+1)*w8

      end subroutine project_feedback

      subroutine droplet_diag(rtime,nrec,zh,zf,pdata,pdata_locind)

      use input
      !use input, only : nparcels,npvals,prx,pry,prz,prsig,pract,prvpx,prvpy,prvpz,prtp,prrp,prmult,prms,przs,pru,prv,prw,prt,prqv,prprs,prrho
#ifdef MPI
      use mpi
#endif
#ifdef NETCDF
      use writeout_nc_module, only : writedropdiag_nc
#endif
      implicit none

      integer, intent(inout) :: nrec
      real, intent(in) :: rtime
      real, intent(in), dimension(nparcelsLocal,npvals) :: pdata
      integer, intent(inout), dimension(nparcelsLocal,3) :: pdata_locind    ! x/y/z location index of each parcel
      real, intent(in), dimension(ib:ie,jb:je,kb:ke) :: zh
      real, intent(in), dimension(ib:ie,jb:je,kb:ke+1) :: zf

      integer :: np,iz,k
      integer :: kflag
      real :: dumzh(kb:ke),dumzf(kb:ke+1),dzf,xl,yl

      integer :: tnumpart,tnumdrop,tnumaerosol
      real :: radavg,radsqr,radmin,radmax,Tpavg,Tpmin,Tpmax,Tpsqr,Tfavg,qfavg,prsavg,rhoavg
      real :: partnum(nk),numconc(nk)
#ifdef MPI
      real, dimension(11) :: droplet_diag0     ! temporary array for MPI reduce operation
                                               ! for the 0th-order diagnostic except rtime
#endif

      !Compute the various types of droplet statistics

      !$acc data create (dumzh,dumzf,partnum,numconc)

      !!!!!!! 0th order  !!!!!!!!

      tnumpart = 0
      radavg = 0.0
      radsqr = 0.0
      Tpavg = 0.0
      Tpsqr = 0.0
      Tfavg = 0.0
      qfavg = 0.0
      prsavg = 0.0
      rhoavg = 0.0
      radmin = 1000.0
      radmax = -1000.0
      Tpmin = 1000.0
      Tpmax = -1000.0

      !$acc parallel loop gang vector default(present) &
      !$acc          reduction(+:tnumpart,radavg,radsqr,Tpavg,Tpsqr,Tfavg,qfavg,prsavg,rhoavg) &
      !$acc          reduction(max:radmax,Tpmax) &
      !$acc          reduction(min:radmin,Tpmin)
      do np=1,nparcelsLocalActive
         tnumpart = tnumpart + 1
         radavg = radavg + pdata(np,prrp)
         radsqr = radsqr + pdata(np,prrp)**2
         Tpavg = Tpavg + pdata(np,prtp)
         Tpsqr = Tpsqr + pdata(np,prtp)**2 
         Tfavg = Tfavg + pdata(np,prt)
         qfavg = qfavg + pdata(np,prqv)
         prsavg = prsavg + pdata(np,prprs)
         rhoavg = rhoavg + pdata(np,prrho)

         radmin = min(radmin, pdata(np,prrp))
         radmax = max(radmax, pdata(np,prrp))
         Tpmin = min(Tpmin, pdata(np,prtp))
         Tpmax = max(Tpmax, pdata(np,prtp))
      end do
      !$acc end parallel

#ifdef MPI
      droplet_diag0(1) = real(tnumpart)
      droplet_diag0(2) = radavg
      droplet_diag0(3) = Tpavg
      droplet_diag0(4) = Tfavg
      droplet_diag0(5) = qfavg
      droplet_diag0(6) = prsavg
      droplet_diag0(7) = rhoavg
      droplet_diag0(8) = radmin
      droplet_diag0(9) = Tpmin
      droplet_diag0(10) = radmax
      droplet_diag0(11) = Tpmax

      ! The root rank will collect the diagnostic from different MPI ranks
      if ( myid == 0 ) then
         call mpi_reduce(MPI_IN_PLACE,droplet_diag0(1),7,MPI_REAL, &
                         MPI_SUM,0,MPI_COMM_WORLD,ierr)
      else
         call mpi_reduce(droplet_diag0(1),droplet_diag0(1),7,MPI_REAL, &
                         MPI_SUM,0,MPI_COMM_WORLD,ierr)
      end if
      if ( myid == 0 ) then
         call mpi_reduce(MPI_IN_PLACE,droplet_diag0(8),2,MPI_REAL, &
                         MPI_MIN,0,MPI_COMM_WORLD,ierr)
      else
         call mpi_reduce(droplet_diag0(8),droplet_diag0(8),2,MPI_REAL, &
                         MPI_MIN,0,MPI_COMM_WORLD,ierr)
      end if
      if ( myid == 0 ) then
         call mpi_reduce(MPI_IN_PLACE,droplet_diag0(10),2,MPI_REAL, &
                         MPI_MAX,0,MPI_COMM_WORLD,ierr)
      else
         call mpi_reduce(droplet_diag0(10),droplet_diag0(10),2,MPI_REAL, &
                         MPI_MAX,0,MPI_COMM_WORLD,ierr)
      end if
      if ( myid == 0 ) then 
         tnumpart = droplet_diag0(1)
         radavg = droplet_diag0(2)
         Tpavg = droplet_diag0(3)
         Tfavg = droplet_diag0(4)
         qfavg = droplet_diag0(5)
         prsavg = droplet_diag0(6)
         rhoavg = droplet_diag0(7)
         radmin = droplet_diag0(8)
         Tpmin = droplet_diag0(9)
         radmax = droplet_diag0(10)
         Tpmax = droplet_diag0(11)
#endif
      if (tnumpart .gt. 0) then
      radavg = radavg/tnumpart
      radsqr = radsqr/tnumpart
      Tpavg = Tpavg/tnumpart
      Tpsqr = Tpsqr/tnumpart
      Tfavg = Tfavg/tnumpart
      qfavg = qfavg/tnumpart
      prsavg = prsavg/tnumpart
      rhoavg = rhoavg/tnumpart
      else
      radavg = 0
      radsqr = 0
      Tpavg = 0
      Tpsqr = 0
      Tfavg = 0
      qfavg = 0
      prsavg = 0
      rhoavg = 0
      end if
       
#ifdef MPI
      end if
#endif

      !!!!!!! 1st order  !!!!!!!!
      !$acc parallel loop gang vector default(present)
      do iz=kb,ke
         dumzh(iz) = zh(1,1,iz)
      enddo
      !$acc end parallel

      !$acc parallel loop gang vector default(present)
      do iz=kb,ke+1
         dumzf(iz) = zf(1,1,iz)
      enddo
      !$acc end parallel

      !$acc parallel loop gang vector default(present)
      do k = 1, nk
         partnum(k) = 0.0
         numconc(k) = 0.0
      end do
      !$acc end parallel

      !$acc parallel loop gang vector default(present) reduction(partnum)
      do np=1,nparcelsLocalActive
         kflag = 1
         k = pdata_locind(np,3)   ! JS: do not mess up the "pdata_locind"
         call find_vertical_location_index (k,pdata(np,prz),kb,ke+1,dumzf(:),kflag,.TRUE.)
         partnum(k) = partnum(k) + 1.0
      end do
      !$acc end parallel

      if (stretch_x .eq. 0) then
         xl = dx*nx
      else
         xl = tot_x_len
      endif
      if (stretch_y .eq. 0) then
         yl = dy*nx
      else
         yl = tot_y_len
      endif

      !$acc update host (dumzf,dumzh,numconc,partnum)

#ifdef MPI
      ! The root rank will collect the diagnostic from different MPI ranks
      if ( myid == 0 ) then
         call mpi_reduce(MPI_IN_PLACE,partnum,nk,MPI_REAL, &
                         MPI_SUM,0,MPI_COMM_WORLD,ierr)
      else
         call mpi_reduce(partnum,partnum,nk,MPI_REAL, &
                         MPI_SUM,0,MPI_COMM_WORLD,ierr)
      end if
      if ( myid == 0 ) then 
#endif
      do iz=1,nk
         dzf = dumzf(iz+1)-dumzf(iz)
         numconc(iz) = partnum(iz)/dzf/xl/yl
         write(6,100) 'dumzh(',iz,'):',dumzh(iz)
         write(6,100) 'numconc(',iz,'):',numconc(iz)
      end do
#ifdef MPI
      end if
#endif
      !WRITE IT OUT:

      !Dump to screen for debugging purposes
#ifdef MPI
      if ( myid == 0 ) then
#endif
      write(6,200) 'rtime', rtime
      write(6,200) 'tnumpart',real(tnumpart)
      write(6,200) 'radavg',radavg
      write(6,200) 'radmin',radmin
      write(6,200) 'radmax',radmax
      write(6,200) 'Tpavg',Tpavg
      write(6,200) 'Tpmin',Tpmin
      write(6,200) 'Tpmax',Tpmax
      write(6,200) 'Tfavg',Tfavg
      write(6,200) 'qfavg',qfavg
      write(6,200) 'prsavg',prsavg
      write(6,200) 'rhoavg',rhoavg
#ifdef MPI
      end if
#endif

#ifdef NETCDF
      ! JS: this subroutine is not updated with the new MPI
      !     interface for the droplets yet
#ifdef MPI
      if ( myid == 0 ) then
#endif
      call writedropdiag_nc(nrec, &
           rtime,tnumpart,radavg,radsqr,radmin,radmax,Tpavg,Tpsqr,Tpmin,Tpmax,Tfavg,qfavg,prsavg,rhoavg, &
           dumzh,numconc)
#ifdef MPI
      end if
#endif
#endif

100     format(2x,'DROPLET_DIAG1:: ',A,I0,A,1x,e13.6,1x)
200     format(2x,'DROPLET_DIAG:: ',A10,':',1x,e13.6)

      !$acc end data

      end subroutine droplet_diag

      subroutine find_horizontal_location_index (loc_ind, loc, lb, ub, dsize, end_ind, ind)
      !$acc routine seq

      use constants, only : undefined_index

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

      use constants, only : undefined_index 

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
      ! - If input z location is outside the z range, return directly

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

      subroutine rad_solver2(guess,mflag,prsval,rhoval,tval,qval,Tp,m_s,radius,np)
      !$acc routine seq
      use constants
      use cm1libs , only : eslf
      implicit none

      real, intent(out) :: guess
      integer, intent(out) :: mflag
      real, intent(in) :: prsval,rhoval,tval,qval,Tp,m_s,radius
      integer, intent(in) :: np
      real :: a, c, esa, Q, R, M, theta, S, T, rhval

      mflag = 0
      esa = eslf(real(prsval),real(tval))  !Saturation vapor pressure at droplet temperature

      rhval = (ru*tval*rhoval*qval)/(mw*esa)

      !Calculate the equilibrium radius based on "rhval"
      !Solve the cubic for the equilibrium radius

      if (rhval .lt. 0.999) then
         a = -(2*mw*surften)/(ru*rhow*tval)/LOG(rhval)
         c = (ion*os*m_s*(mw/ms))/((4.0/3.0)*pi*rhow)/LOG(rhval) 
     
         !Use Cardano's method for cubic solution
         Q = (a**2.0)/9.0
         R = (2.0*a**3.0+27.0*c)/54.0
         M = R**2.0-Q**3.0


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

         end if

      else 

        guess = radius
        mflag = 1
      end if

      end subroutine rad_solver2

      subroutine gauss_newton_2d(diffnorm,dt_nondim,taup_scale,rt_start,rt_zeros,flag,prsval,rhoval,tval,qval,Tp,m_s,radius,np)
        !$acc routine seq

        implicit none

        double precision, intent(in) :: diffnorm,dt_nondim,taup_scale,rt_start(2)
        double precision, intent(in) :: prsval,rhoval,tval,qval,Tp,m_s,radius
        double precision, intent(out) :: rt_zeros(2)
        integer, intent(out) :: flag
        integer, intent(in) :: np

        double precision :: error,fv1(2),fv2(2),v1(2),rel,det
        double precision :: relax, coeff
        double precision, dimension(1:2, 1:2) :: J,fancy,inv,finalJ
        integer :: iterations,neg,counts,iteration_max

        double precision :: correct(2)

        iterations = 0
        flag = 0
        error = 1.e-8

        !Initialize the solution vector
        v1(1) = rt_start(1)
        v1(2) = rt_start(2)
         
        !Initialize the correction to zero
        correct(1) = 0.0
        correct(2) = 0.0

        !Default max iterations before Gauss-Newton deemed failure (and moves onto a new scheme)
        !Rare that it gets above ~5
        iteration_max = 50

        do while (iterations<iteration_max)

                iterations = iterations + 1

                !Compute the RHS of the droplet temp and radius equations
                call ie_vrt_nd(diffnorm,v1(1),v1(2),fv1,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius,np)
                !Approximate the Jacobian
                call jacob_approx_2d(diffnorm,v1(1),v1(2),dt_nondim,J,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius,np)

                !Matrix to be inverted via G-N method
                fancy(1,1) = J(1,1)*J(1,1)+J(2,1)*J(2,1)
                fancy(1,2) = J(1,1)*J(1,2)+J(2,1)*J(2,2)
                fancy(2,1) = J(1,2)*J(1,1)+J(2,2)*J(2,1)
                fancy(2,2) = J(1,2)*J(1,2)+J(2,2)*J(2,2)

                !Determinant -- exit if it is too near zero, indicates failed convergence
                det = fancy(1,1)*fancy(2,2) - fancy(1,2)*fancy(2,1)
                if (abs(det) .lt. 1.0e-10) then
                   flag = 1
                   EXIT
                end if
                
                call inverse_finder_2d(fancy,det,inv)

                finalJ(1,1) = inv(1,1)*J(1,1)+inv(1,2)*J(1,2)
                finalJ(1,2) = inv(1,1)*J(2,1)+inv(1,2)*J(2,2)
                finalJ(2,1) = inv(2,1)*J(1,1)+inv(2,2)*J(1,2)
                finalJ(2,2) = inv(2,1)*J(2,1)+inv(2,2)*J(2,2)

                !How much to correct the solution vector
                correct(1) = finalJ(1,1)*fv1(1)+finalJ(1,2)*fv1(2)
                correct(2) = finalJ(2,1)*fv1(1)+finalJ(2,2)*fv1(2)

                !Update the solution vector
                rt_zeros(1) = v1(1) - correct(1)
                rt_zeros(2) = v1(2) - correct(2)

                relax = 1.0
                counts = 0
                !If either the temperature or radius goes negative, take a series of smaller adjustment steps
                do while ((rt_zeros(1)<0) .OR. (rt_zeros(2)<0) .OR. (rt_zeros(1) .ne. rt_zeros(1)))

                   counts = counts + 1
                   coeff = 0.5
                   relax = relax * coeff
                   rt_zeros(1) = v1(1)-(finalJ(1,1)*fv1(1)+finalJ(1,2)*fv1(2))*relax
                   rt_zeros(2) = v1(2)-(finalJ(2,1)*fv1(1)+finalJ(2,2)*fv1(2))*relax

                   if (counts>10) EXIT
                end do

                !Failure to converge if these drop too low -- send to LV solver
                if (rt_zeros(1)<0.01 .OR. rt_zeros(2)<0.01) then
                   flag = 1
                   EXIT
                end if

                !Converged when the correction is small
                if (sqrt(correct(1)*correct(1)+correct(2)*correct(2))<error) then
                   EXIT
                end if

                v1(1) = rt_zeros(1)
                v1(2) = rt_zeros(2)

        end do
        !print *,'gauss_newton_2d: iterations: ',iterations
      !These indicate that this failed -- set flag = 1 to send to LV solver
      if (iterations == iteration_max) flag = 1
      if ((rt_zeros(1) .ne. rt_zeros(1)) .OR. rt_zeros(1)<0 .OR. (rt_zeros(2) .ne. rt_zeros(2)) .OR. rt_zeros(2)<0) flag = 1

      end subroutine gauss_newton_2d

      subroutine LV_solver(diffnorm,dt_nondim,taup_scale,rt_start,rt_zeros,flag,prsval,rhoval,tval,qval,Tp,m_s,radius,np)
        !$acc routine seq
        implicit none

        double precision, intent(in) :: diffnorm,dt_nondim,taup_scale,rt_start(2)
        double precision, intent(in) :: prsval,rhoval,tval,qval,Tp,m_s,radius
        double precision, intent(out) :: rt_zeros(2)
        integer, intent(out) :: flag
        integer, intent(in) :: np

        double precision :: error,fv1(2),fv2(2),v1(2),rel,det
        double precision :: diff,lambda,lup,ldown
        double precision :: C(2),newC(2),gradC(2),correct(2)
        double precision, dimension(1:2, 1:2) :: J,I,g,invg
        integer :: iterations,neg,iterations_max


        error = 1.0e-8

        
        !Preliminary
        I(1,1)=1.0
        I(2,1)=0.0
        I(1,2)=0.0
        I(2,2)=1.0
        iterations = 0
        iterations_max = 1000
        flag = 0
        v1(1) = rt_start(1)
        v1(2) = rt_start(2)
        fv2(1) = 1.0
        fv2(2) = 1.0

        !These are parameters to the solver, could be adjusted but these work
        lambda = 0.001
        lup = 2.0
        ldown = 2.0

        do while ((sqrt(fv2(1)*fv2(1)+fv2(2)*fv2(2)) > error) .AND. (iterations<iterations_max))

        iterations = iterations + 1

        !Approximate the Jacobian of the system
        call jacob_approx_2d(diffnorm,v1(1),v1(2),dt_nondim,J,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius,np)
        !Compute the RHS of the temperature and radius equations
        call ie_vrt_nd(diffnorm,v1(1),v1(2),fv1,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius,np)

        !Calculate the update matrix which must be inverted
        g(1,1) = J(1,1)*J(1,1)+J(2,1)*J(2,1)+lambda*I(1,1)
        g(1,2) = J(1,1)*J(1,2)+J(2,1)*J(2,2)+lambda*I(1,2)
        g(2,1) = J(1,2)*J(1,1)+J(2,2)*J(2,1)+lambda*I(2,1)
        g(2,2) = J(1,2)*J(1,2)+J(2,2)*J(2,2)+lambda*I(2,2)

        gradC(1) = J(1,1)*fv1(1)+J(2,1)*fv1(2)
        gradC(2) = J(1,2)*fv1(1)+J(2,2)*fv1(2)

        C(1) = 0.5*fv1(1)*fv1(1)
        C(2) = 0.5*fv1(2)*fv1(2)

        !Exit if the determinant is too close to zero -- failed convergence
        det = g(1,1)*g(2,2) - g(1,2)*g(2,1)
        if (abs(det) .lt. 1.0e-10) then
             flag = 1
             EXIT
        end if
        call inverse_finder_2d(g,det,invg)

        !Solve for correction vector
        correct(1) = invg(1,1)*gradC(1)+invg(1,2)*gradC(2)
        correct(2) = invg(2,1)*gradC(1)+invg(2,2)*gradC(2)
        
        !Successful convergence if correction is small
        if (sqrt(correct(1)*correct(1)+correct(2)*correct(2)) < error) then
           EXIT
        end if

        rt_zeros(1) = v1(1) - correct(1)
        rt_zeros(2) = v1(2) - correct(2)

        !Failed if temperature or radius goes negative
        if (rt_zeros(1)<0.0 .OR. rt_zeros(2)<0.0) then
            flag = 1
            EXIT
        end if

        !Use the RHS to update the lambda parameter
        call ie_vrt_nd(diffnorm,rt_zeros(1),rt_zeros(2),fv2,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius,np)
        newC(1) = 0.5*fv2(1)*fv2(1)
        newC(2) = 0.5*fv2(2)*fv2(2)

        if (sqrt(newC(1)*newC(1)+newC(2)*newC(2))<sqrt(C(1)*C(1)+C(2)*C(2))) then
           v1(1) = rt_zeros(1)
           v1(2) = rt_zeros(2)
           lambda = lambda/ldown
        else
           lambda = lambda*lup
        end if


        end do

        !Failed if iterations hits maximum
        if (iterations==iterations_max) then
           flag = 1
        end if

        !Failed if it returns negative radius or temperature
        if (rt_zeros(1) < 0 .OR. rt_zeros(2) < 0) then
           flag = 1
        end if

      end subroutine LV_solver

      subroutine inverse_finder_2d(C,det,invC)
        !$acc routine seq
        implicit none
        double precision, intent(in) :: det
        double precision, dimension(1:2, 1:2), intent(in) :: C
        double precision, dimension(1:2, 1:2), intent(out) :: invC

        invC = reshape((/C(2, 2), -C(2,1), -C(1, 2), C(1, 1)/),(/2,2/))
        invC = (1./det)*invC

      end subroutine inverse_finder_2d

      subroutine jacob_approx_2d(diffnorm,rnext,tnext,dt_nondim,J,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius,np)
        !$acc routine seq
        implicit none
        integer :: n

        double precision, intent(in) :: diffnorm,rnext,tnext,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius
        double precision, intent(out), dimension(1:2, 1:2) :: J
        double precision :: diff,v_output(3),rt_output(2),xper(2),fxper(2),ynext(2),xper2(2),fxper2(2)
        integer, intent(in) :: np

        !Take a very small difference to approximate the derivatives
        diff = 1E-12

        ynext(1) = rnext
        ynext(2) = tnext

        !Evaluate RHS 
        call ie_vrt_nd(diffnorm,rnext,tnext,rt_output,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius,np)

        xper(1) = ynext(1)
        xper(2) = ynext(2)
        xper2(1) = ynext(1)
        xper2(2) = ynext(2)

        !Now evaluate RHS a bit ahead and behind the base point
        do n=1, 2
           xper(n) = xper(n) + diff
           xper2(n) = xper2(n) - diff

           call ie_vrt_nd(diffnorm,xper(1),xper(2),fxper,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius,np)
           call ie_vrt_nd(diffnorm,xper2(1),xper2(2),fxper2,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius,np)

           !Use these differences to compute the Jacobian
           J(1,n) = (fxper(1)-rt_output(1))/diff
           J(2,n) = (fxper(2)-rt_output(2))/diff
           xper(n) = ynext(n)
           xper2(n) = ynext(n)
        end do

      end subroutine jacob_approx_2d

      subroutine ie_vrt_nd(diffnorm,tempr,tempt,rt_output,dt_nondim,taup_scale,prsval,rhoval,tval,qval,Tp,m_s,radius,np)
      !$acc routine seq
      use constants
      use input, only : viscosity,pr_num,sc_num
      use cm1libs , only : eslf
      implicit none

      double precision, intent(in) :: diffnorm,tempr,tempt,taup_scale,dt_nondim,prsval,rhoval,tval,qval,Tp,m_s,radius
      double precision, intent(out) :: rT_output(2)
      integer, intent(in) :: np

      double precision :: esa,dnext,rhop,Rep,taup,rprime,Tprime,qstr,Shp,Nup,VolP,lhv
      double precision :: Tnext,rnext

        ! quantities come in already non-dimensionalized, so must be converted back;
        rnext = tempr*radius
        Tnext = tempt*Tp
        dnext = rnext*2.0

        esa = eslf(real(prsval),real(tval))
        VolP = (4.0/3.0)*pi*rnext**3
        rhop = (m_s + VolP*rhow)/VolP

        Rep = dnext*diffnorm/viscosity
        taup = (rhop*dnext**2)/(18.0*rhoval*viscosity)
        Nup = 2.0 + 0.6*sqrt(Rep)*pr_num**(1.0/3.0)
        Shp = 2.0 + 0.6*sqrt(Rep)*sc_num**(1.0/3.0)

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

      subroutine droplet_injection(pdata,xf,yf,dt,s10_xym,randomNumbers)
      !Inject new particles according to some rule
      !Here use the sea spray generation function (SSGF) of Andreas (1998)
      use input, only : nparcelsLocal,npvals,myid,ni,nj,ib,ie,jb,je, &
                        prx,pry,prz,pru,prv,prw,prvpx,prvpy,prvpz,prrp,prtp,prms,pract, &
                        prmult,prt,prrho,prprs,prqv,nparcelsLocalActive,numprocs,nparcelsMax, &
                        time_phys_H2D,time_phys_D2H,time_droplet,timestats,mytime,injectHMax
      use constants
      use MersenneTwister_mod
#ifdef MPI
      use mpi
#endif

      real, intent(inout), dimension(nparcelsLocal,npvals) :: pdata
      type(randomNumberSequence),intent(inout) :: randomNumbers
      real, intent(in), dimension(ib:ie+1) :: xf
      real, intent(in), dimension(jb:je+1) :: yf
      real, intent(in) :: dt,s10_xym
    
      integer :: i,my_reintro,np,tot_after_reintro
      integer :: sum_buf(2)
      real :: dFssum(101)  !Discretized cumulative spray generation function (to be calculated)
      real :: binsdata(100)  !Discretized bins for cumulative SSGF

      real :: totdrops  !Total number being produced during this call to droplet_injection
      real :: rad_init  !Initial radius, being calculated one at a time according to SSGF
      real :: u10

      real :: xrange,yrange,zrange
      real :: salinity
      
      real :: c1,c2,c3,u14,cdn10,a,rand
      real :: a1,a2,r80,r0,dh
      real :: dFsdr80(100),dFmsdr0(100),dr80_dr0
      integer :: iter,nbin,num_create,j,ierr

      !implementing the Andreas 1998 sea spray generation function
      !Set the parameters of the two lognormals:
      real :: rmin,rmax,rmin10,rmax10

      rmin10 = log10(2e-06)
      rmax10 = log10(500e-06)
      dh = 0.0242

  !   ===== update x-axis for each bin =====
      binsdata(1) = rmin10
      do i = 1,99
        binsdata(i+1)= dh+binsdata(i)
      end do

      binsdata = 1e6*(10**binsdata)
  
      ! Now figure out how many particles are produced in each of
      ! those bins. This is a function of u10
      u10 = min(s10_xym,50.0)

      c1 = 10*smithssgf(u10,10.0,0.4)
      c2 = c1*(37.5**1.8)
      c3 = c2*(100**5.2)
      dFssum(1) = 0
      do i = 1,100
        r0 = binsdata(i)
        ! calculate radius at 80%RH
        r80 = 0.518*r0**0.976;
 
     ! now apply these to Eqs. 3.5 in Andreas 1998
        if (r80 .lt. 10) then
          dFsdr80(i) =  smithssgf(u10,r80,0.4)
        elseif ((r80 .ge. 10) .and. (r80 .lt. 37.5)) then
          dFsdr80(i) = c1/r80
        elseif ((r80 .ge. 37.5) .and. (r80 .lt. 100)) then
          dFSdr80(i) = c2*r80**-2.8
        elseif ((r80 .ge. 100) .and. (r80 .lt. 250)) then
          dFsdr80(i) = c3*r80**-8
        endif
 
       ! apply eq. 3.8 from Andreas 1998
       dFmsdr0(i) = 3.5*dFsdr80(i)*0.506*r0**-0.024
 
       if (i==1) then
         dFssum(i+1) = dFmsdr0(i)
       elseif (i .gt. 1) then
         dFssum(i+1) = dFssum(i) + dFmsdr0(i)
       endif
  
      end do

      totdrops = dFssum(101)    !totdrops at this point is # per unit area per unit time

      !Define these here
      xrange = xf(ni+1)-xf(1)
      yrange = yf(nj+1)-yf(1)
      zrange = injectHMax

      my_reintro = floor(xrange*yrange*dt*totdrops/drop_mult)      !Number of droplets to be produced by this MPI task

      !JMD my_reintro = 10


#ifdef MPI
      call mpi_allreduce(my_reintro+nparcelsLocalActive,tot_after_reintro,1,mpi_int,mpi_sum,mpi_comm_world,ierr)
#else
      !Calculate the total number which will occur after injection
      tot_after_reintro = my_reintro+nparcelsLocalActive
#endif



   if (myid==0) write(*,'(a15,i,6e15.6)')  'DROP INJECTION:',my_reintro,drop_mult,u10,totdrops,xrange,yrange,dt

      if (tot_after_reintro .lt. nparcelsMax) then   !Only add particles if the total number injected does not make the global exceed nparcelsMax

      do np=nparcelsLocalActive+1,nparcelsLocalActive+my_reintro

         if (random_droplets) then
         rand = getRandomReal(randomNumbers)
         a = totdrops*rand

         !Set the droplet radius based on the sea spray generation function (SSGF)
         !Loop over all of the discretized bins of the SSGF. Based on the random number, choose which bin to draw the radius from
         !Once a bind is identified by the conditional statement, the radius is set to a random number between the edges of the bin
         do i = 1,100
            if ((a .gt. dFssum(i)) .and. (a .le. dFssum(i+1))) then
               rand = getRandomReal(randomNumbers)
               rad_init = binsdata(i) + rand*(binsdata(i+1)-binsdata(i))
            endif
         enddo

         rad_init = rad_init*1.0e-6

        !Now populate the pdata array, starting at the list
       
        !!!!!!!! if (nparcelsActive+myreintro .lt. nparcelsActive_max)

        !Random location in the loweset 8 meters
        rand = getRandomReal(randomNumbers)
        pdata(np,prx) = xf(1)+rand*xrange
        rand = getRandomReal(randomNumbers)
        pdata(np,pry) = yf(1)+rand*yrange
        rand = getRandomReal(randomNumbers)
        pdata(np,prz) = rand*zrange

        else

        rad_init = 10.0e-6
        pdata(np,prx) = xf(1) + xrange/2.0
        pdata(np,pry) = yf(1) + yrange/2.0
        pdata(np,prz) = injectHMax

        end if

        pdata(np,prvpx) = 0.0
        pdata(np,prvpy) = 0.0
        pdata(np,prvpz) = 3.0  !RANDOM UPWARD VELOCITY?

        pdata(np,prrp) = rad_init


        salinity = 0.034
        pdata(np,prms) = salinity*rhow*4.0/3.0*pi*pdata(np,prrp)**3
 
        pdata(np,prtp) = 302.0
        pdata(np,prmult) = 1.0e9
        pdata(np,pract) = 1.0

        pdata(np,pru) = 0.0
        pdata(np,prv) = 0.0
        pdata(np,prw) = 0.0
        pdata(np,prt) = 0.0
        pdata(np,prqv) = 0.0
        pdata(np,prprs) = 0.0
        pdata(np,prrho) = 0.0

      end do
      if(timestats.ge.1) time_droplet=time_droplet+mytime()

      !Then once the new droplets have been added, update nparcelsLocalActive
      nparcelsLocalActive = nparcelsLocalActive + my_reintro

      else

        if (myid == 0) write(*,*) 'WARNING CANNOT ADD MORE PARTICLES, WILL EXCEED NPARCELSMAX'  

      end if

    end subroutine droplet_injection

    function smithssgf(u10,r80,vk)
    implicit none
    real :: u10,r80,cdn10,vk,u14
    real :: a1,a2,smithssgf


    ! added SEA93 function
    if (u10 .ge. 4 .and. u10 .lt. 11) then
       cdn10 = 1.2e-3
    elseif (u10 .ge. 11) then
       cdn10 = 1e-3*(0.49 + 0.065*u10)
    end if

    ! calculate 14 m wind speed
    u14 = u10*(1 + (sqrt(cdn10)/vk)*alog(14.0/10.0))
  
    a1 = 10**(0.0676*u14 + 2.43)
    a2 = 10**(0.959*sqrt(u14) - 1.476)
  

    ! calcaulate dFs/dr80 (Eq. A1 in Andreas 1998)
    smithssgf = a1*exp(-3.1*(alog(r80/2.1))**2) + a2*exp(-3.3*(alog(r80/9.2))**2)

    end function smithssgf


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  END MODULE droplet_module
