MODULE bcast_module

  implicit none
  public :: bcast_int,bcast_real

interface bcast_int 
   module procedure bcast_int_scalar 
   module procedure bcast_int_array
end interface bcast_int

interface bcast_real
   module procedure bcast_real_scalar
   module procedure bcast_real_array
end interface bcast_real

  CONTAINS

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_int_scalar(val)

#ifdef MPI
      use mpi
#endif
      integer :: ierr
      integer,intent(inout) :: val
#ifdef MPI
      call MPI_BCAST(val,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
#endif
      !$acc update device(val)

      end subroutine bcast_int_scalar

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_int_array(val,len)

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

      end subroutine bcast_int_array

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_real_scalar(val)

#ifdef MPI
      use mpi
#endif
      integer :: ierr
      real,intent(inout) :: val
#ifdef MPI
      call MPI_BCAST(val,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)
#endif
      !$acc update device(val)

      end subroutine bcast_real_scalar

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_real_array(val,len)

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

      end subroutine bcast_real_array

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

end MODULE bcast_module
