MODULE bcast_module

  implicit none
  public :: bcast_int,bcast_real,bcast_log

interface bcast_log
   module procedure bcast_log_scalar 
end interface bcast_log

interface bcast_int 
   module procedure bcast_int_scalar 
   module procedure bcast_int_vector
end interface bcast_int

interface bcast_real
   module procedure bcast_real_scalar
   module procedure bcast_real_vector
end interface bcast_real

  CONTAINS

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_log_scalar(val,device)

#ifdef MPI
      use mpi
#endif
      integer :: ierr
      logical,intent(inout) :: val
      logical, optional :: device
#ifdef MPI
      call MPI_BCAST(val,1,MPI_LOGICAL,0,MPI_COMM_WORLD,ierr)
#endif
      if(present(device) .and. device) then 
        !$acc update device(val)
      endif

      end subroutine bcast_log_scalar

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_int_scalar(val,device)

#ifdef MPI
      use mpi
#endif
      integer :: ierr
      integer,intent(inout) :: val
      logical, optional :: device
#ifdef MPI
      call MPI_BCAST(val,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
#endif
      if(present(device) .and. device) then 
        !$acc update device(val)
      endif

      end subroutine bcast_int_scalar

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
      !$acc update device(val(str_ind:end_ind))

      end subroutine bcast_int_vector

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine bcast_real_scalar(val,device)

#ifdef MPI
      use mpi
#endif
      integer :: ierr
      real,intent(inout) :: val
      logical, intent(in), optional :: device
#ifdef MPI
      call MPI_BCAST(val,1,MPI_REAL,0,MPI_COMM_WORLD,ierr)
#endif
      if(present(device) .and. device) then 
        !$acc update device(val)
      endif

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
      !$acc update device(val(str_ind:end_ind))

      end subroutine bcast_real_vector

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

end MODULE bcast_module
