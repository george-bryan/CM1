MODULE bcast_module

  implicit none
  public :: bcast_int,bcast_real

  CONTAINS

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_int(val,len)

#ifdef MPI
      use mpi
#endif
      integer :: ierr
      integer,intent(in) :: len
      integer,intent(inout) :: val(*)
#ifdef MPI
      call MPI_BCAST(val,len,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
#endif
      !$acc update device(val)

      end subroutine bcast_int

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_real(val,len)

#ifdef MPI
      use mpi
#endif
      integer :: ierr
      integer,intent(in) :: len
      real,intent(inout) :: val(*)
#ifdef MPI
      call MPI_BCAST(val,len,MPI_REAL,0,MPI_COMM_WORLD,ierr)
#endif
      !$acc update device(val)

      end subroutine bcast_real

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


end MODULE bcast_module


