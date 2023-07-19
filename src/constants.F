  MODULE constants

  implicit none

  public

    real :: g,rd,rv,cp,cpinv,cv,cpv,cvv,rcp,                                 &
            cpdcv,rovcp,rddcp,rddcv,rddrv,cvdrd,cpdrd,eps,reps,repsm1,       &
            cpt,cvt,pnum,xlv,lathv,xls,lvdcp,condc,cpl,cpi,lv1,lv2,ls1,ls2,  &
            rhow,c_e1,c_e2,c_s,rcs,earth_radius,govtwo
    double precision :: rhow_dp,cp_dp,cpl_dp,mw,ms,surften,ion,os,ru,lv1_dp,lv2_dp

    !$acc declare create (ru,mw,surften,rhow,rhow_dp,ion,os,ms,lv1,lv1_dp,lv2,lv2_dp,cpl,cpl_dp,cp,cp_dp,eps)

      !----------------

      real, parameter ::  pi     = 3.1415926535897932384626433
      real, parameter ::  th0r   = 300.0
      real, parameter ::  to     = 273.15
      real, parameter ::  p00    = 1.0e5
      real, parameter ::  rp00   = 1.0/p00
      real, parameter ::  piddeg = pi/180.0
      real, parameter ::  degdpi = 180.0/pi
      real, parameter ::  clwsat = 1.e-6
      real, parameter ::  karman = 0.40
      real, parameter ::  rkarman = 1.0/karman
      real, parameter ::  omega  = 2.0*pi/86400.0

      !----------------
      ! Implicit vertical diffusion.  For vialpha:
      !      0.0 = explicit forward-in-time (unstable if K dt / (dz^2) > 0.5)
      !      0.5 = implicit centered-in-time (Crank-Nicholson)
      !      1.0 = implicit backward-in-time (implicit Euler)
      real, parameter ::  vialpha = 1.0

      ! DO NOT CHANGE VIBETA:
      real, parameter ::  vibeta = 1.0 - vialpha

      !----------------

      ! open bc phase velocity:
      real, parameter ::  cstar = 30.0

      ! open bc max velocity:
      real, parameter ::  csmax = 350.0

      !----------------

      ! parameters for LES subgrid turbulence:
      real, parameter ::  c_m  = 0.10
      real, parameter ::  c_l  = 0.8165
      real, parameter ::  ri_c = 0.25


      !----------------
      ! stuff for weno / advection:
      real, parameter :: epsilon = 1.0e-18
      !----------------


      integer, parameter :: nrkmax = 3

      real, parameter :: grads_undef  =  -999999.9

      integer, parameter :: undefined_index = -100

      real, parameter :: pdata_buffer = 0.10      ! additional storage space for pdata from
                                                  ! each MPI rank to account for the droplet
                                                  ! communication between MPI ranks;
                                                  ! this is a percent of
                                                  ! "nparcelsMax" value
      integer :: padding                          ! The actual number of words used for 
                                                  ! padding the "pdata" array

      real, parameter :: neg_huge = -1.0e30       ! define a large negative number

      integer, parameter :: num_nn = 8,  &        ! Number of nearest neighbor
                            inorth = 1,  &        ! north neighbor index 
                            isouth = 2,  &        ! south neighbor index 
                             iwest = 3,  &        ! west neighbor index 
                             ieast = 4,  &        ! east neighbor index 
                               inw = 5,  &        ! northwest neighbor index 
                               ine = 6,  &        ! northeast neighbor index 
                               isw = 7,  &        ! southwest neighbor index 
                               ise = 8            ! southeast neighbor index 

  CONTAINS

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine set_constants( testcase )
      implicit none

      integer, intent(in) :: testcase


      !--------------------
      !  For test cases:

      IF( testcase.eq.4 )THEN

        !  nonprecipitating stratocumulus (Stevens et al, 2005, MWR)
        g      = 9.81
        rd     = 287.0
        cp     = 1015.0
        cp_dp  = 1015.0d0
        cpv    = 1870.0
        xlv    = 2.47e6
        xls    = 2834000.0
        earth_radius  =  6370000.0

      ELSEIF( testcase.eq.5 )THEN

        !  drizzling stratocumulus (Ackerman et al, 2009, MWR)
        g      = 9.81
        rd     = 287.0
        cp     = 1004.0
        cp_dp  = 1004.0d0
        cpv    = 1870.0
        xlv    = 2.50e6
        xls    = 2834000.0
        earth_radius  =  6370000.0

      ELSE

        !  RCEMIP
!!!        g      = 9.79764
!!!        rd     = 287.04
!!!        cp     = 1004.64
!!!        cpv    = 1846.0
!!!        xlv    = 2501000.0
!!!        xls    = 2834700.0
!!!        earth_radius  =  6371000.0

        !  cm1 defaults:
        g      = 9.81
        rd     = 287.04
        cp     = 1005.7
        cp_dp  = 1005.7d0
        cpv    = 1870.0
        xlv    = 2501000.0
        xls    = 2834000.0
        earth_radius  =  6370000.0

      ENDIF

      !--------------------

      govtwo = 0.5*g
      rv     = 461.5
      cpinv  = 1.0/cp
      cv     = cp-rd
      cvv    = cpv-rv
      rcp    = 1.0/cp
      cpdcv  = cp/cv
      rovcp  = rd/cp
      rddcp  = rd/cp
      rddcv  = rd/cv
      rddrv  = rd/rv
      cvdrd  = cv/rd
      cpdrd  = cp/rd
      eps    = rd/rv
      reps   = rv/rd
      repsm1 = rv/rd-1.0
      cpt    = (cpv/cp)-1.0
      cvt    = (cvv/cv)-1.0
      pnum   = (cp*rv)-(cpv*rd)
      lathv  = xlv
      lvdcp  = xlv/cp
      condc  = xlv*xlv/(rv*cp)
      cpl    = 4190.0
      cpl_dp = 4190.0d0
      cpi    = 2106.0
      lv1    = xlv+(cpl-cpv)*to
      lv2    = cpl-cpv
    ! kludge !
      lv1_dp = 3134708.0d0
      lv2_dp = 2320.0d0
    ! kludge !
      ls1    = xls+(cpi-cpv)*to
      ls2    = cpi-cpv
      rhow   = 1.0e3
      rhow_dp = 1.0d3
      mw     = 0.018015d0  !kg/mol, molecular weight water
      ms     = 0.05844d0   !kg/mol, molecular weight of salt (or droplet solute)
      surften= 7.28d-2   !Droplet surface tension
      ion    = 2.0d0       !# of ions available in solute
      os     = 1.093d0     !Droplet osmotic coefficient
      ru     = 8.3144d0    !J/mol-K Universal gas constant



      IF( testcase.eq.4 .or. testcase.eq.5 )THEN
        lv1 = xlv
        lv2 = 0.0
      ENDIF


      !DATA FOR LAGRANGIAN DROPLETS


      ! DO NOT CHANGE THESE:
      c_e1 = c_m * c_l * c_l * ( 1.0 / ri_c - 1.0 )
      c_e2 = max( 0.0 , c_m * pi * pi - c_e1 )
      c_s = ( c_m * c_m * c_m / ( c_e1 + c_e2 ) )**0.25   ! Smagorinsky constant
      rcs = 1.0/c_s

      !$acc update device (ru,mw,surften,rhow,rhow_dp,ion,os,ms,lv1,lv1_dp,lv2,lv2_dp,cpl,cpl_dp,cp,cp_dp,eps)

      end subroutine set_constants

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  END MODULE constants
