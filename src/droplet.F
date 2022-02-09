#define TROUBLE 0
#define PITER 1048576
#ifdef _B4B
#undef _B4B01F
#undef _B4B02F
#undef _B4B03F
#endif
  MODULE droplet_module

  implicit none

  private
  public :: droplet_driver

  ! If we already know that a parcel stays on the same process 
  !    in the previous time step and it is still here, we start 
  !    the location search from the previous index and limit 
  !    the search range using the parameter below;
  ! This assumes that a parcel does not move too far and does
  !    not move between processes within this subroutine?
  integer, parameter :: location_offset = 3

  CONTAINS






!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  END MODULE droplet_module
