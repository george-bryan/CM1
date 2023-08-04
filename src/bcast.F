MODULE bcast_module

  implicit none
  public :: bcast_int,bcast_real,bcast_double,bcast_log

interface bcast_log
   module procedure bcast_log_scalar 
end interface bcast_log

interface bcast_int 
   module procedure bcast_int_scalar 
   module procedure bcast_int8_scalar 
   module procedure bcast_int_vector
end interface bcast_int

interface bcast_real
   module procedure bcast_real_scalar
   module procedure bcast_real_vector
end interface bcast_real

interface bcast_double
   module procedure bcast_double_scalar
   module procedure bcast_double_vector
end interface bcast_double

  CONTAINS

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_log_scalar(val)

#ifdef MPI
      use mpi
#endif
      integer :: ierr
      logical,intent(inout) :: val
#ifdef MPI
      call MPI_BCAST(val,1,MPI_LOGICAL,0,MPI_COMM_WORLD,ierr)
#endif
      !$acc update device(val) if_present

      end subroutine bcast_log_scalar

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
      !$acc update device(val) if_present

      end subroutine bcast_int_scalar

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_int8_scalar(val)

      use constants, only : i8
#ifdef MPI
      use mpi
#endif
      integer :: ierr
      integer(i8),intent(inout) :: val
#ifdef MPI
      call MPI_BCAST(val,1,MPI_INTEGER8,0,MPI_COMM_WORLD,ierr)
#endif
      !$acc update device(val) if_present

      end subroutine bcast_int8_scalar

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_int_vector(val,str_ind,end_ind)

#ifdef MPI
      use mpi
#endif
      integer :: ierr
      integer,intent(in) :: str_ind,end_ind
      integer,intent(inout) :: val(str_ind:end_ind)
      integer :: length
   
      length = end_ind - str_ind + 1
#ifdef MPI
      call MPI_BCAST(val,length,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
#endif
      !$acc update device(val(str_ind:end_ind)) if_present

      end subroutine bcast_int_vector

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
      !$acc update device(val) if_present

      end subroutine bcast_real_scalar

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_real_vector(val,str_ind,end_ind)

#ifdef MPI
      use mpi
#endif
      integer :: ierr
      integer,intent(in) :: str_ind,end_ind
      real,intent(inout) :: val(str_ind:end_ind)
      integer :: length

      length = end_ind - str_ind + 1

#ifdef MPI
      call MPI_BCAST(val,length,MPI_REAL,0,MPI_COMM_WORLD,ierr)
#endif
      !$acc update device(val(str_ind:end_ind)) if_present

      end subroutine bcast_real_vector

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_double_scalar(val)

#ifdef MPI
      use mpi
#endif
      integer :: ierr
      double precision,intent(inout) :: val
#ifdef MPI
      call MPI_BCAST(val,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
#endif
      !$acc update device(val) if_present

      end subroutine bcast_double_scalar

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_double_vector(val,str_ind,end_ind)

#ifdef MPI
      use mpi
#endif
      integer :: ierr
      integer,intent(in) :: str_ind,end_ind
      double precision,intent(inout) :: val(str_ind:end_ind)
      integer :: length

      length = end_ind - str_ind + 1

#ifdef MPI
      call MPI_BCAST(val,length,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
#endif
      !$acc update device(val(str_ind:end_ind)) if_present

      end subroutine bcast_double_vector

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


end MODULE bcast_module
