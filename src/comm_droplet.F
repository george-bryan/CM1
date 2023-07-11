!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Jian Sun - 09/28/2022:                                         !
!    This module is generated following John Dennis's suggestion !
!    to separate the MPI communication of droplets from other    !
!    MPI calls in the existing comm.F code                       !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module comm_droplet_module

implicit none

private

public :: comm_droplet_number,comm_droplet_value
public :: setupIndexPointers, setupDepartDroplet
public :: makeContiguous

contains

#ifdef MPI

  subroutine comm_droplet_number(numHoles,Depart,Arrive)

    use input, only : myid,nparcelsLocal,npvals,mynw,mysw,myne, &
                      myse,myeast,mywest,mynorth,mysouth,ierr, &
                      nparcelsLocalActive
    use constants, only: undefined_index,num_nn,inorth,isouth, &
                         iwest,ieast,inw,ine,isw,ise
    use mpi

    implicit none

    integer, intent(in) :: numHoles                                      ! Current number of holes available 

    integer, intent(out) :: Depart(num_nn)                               ! Number of droplets that will enter each 
                                                                         ! nearest neighbor at different directions
    integer, intent(out) :: Arrive(num_nn)                               ! Number of droplets that will enter the 
                                                                         ! current MPI region from each nearest 
                                                                         ! neighbor at different directions
    ! Local variables

    integer :: n_recv,n_send
    integer :: reqs(16)
    integer :: tag_n,tag_s,tag_w,tag_e,tag_nw,tag_ne,tag_sw,tag_se,indx
    integer, dimension(mpi_status_size,16) :: status1
    integer :: i,n

    ! initiate some MPI index and tag values

    tag_n  = 1001
    tag_s  = 1002
    tag_w  = 1003
    tag_e  = 1004
    tag_nw = 1005
    tag_sw = 1006
    tag_ne = 1007
    tag_se = 1008

    ! We are just exchanging some integers, so just use the correct
    ! version that is already present on the host
    ! initiate the MPI non-blocking receive interface to 
    ! know how many droplets from the nearest neighbors

    n_recv = 1
    call mpi_irecv(Arrive(inorth),1,MPI_INT,mynorth,tag_n,MPI_COMM_WORLD, &
                   reqs(n_recv),ierr)

    n_recv = n_recv + 1
    call mpi_irecv(Arrive(isouth),1,MPI_INT,mysouth,tag_s,MPI_COMM_WORLD, &
                   reqs(n_recv),ierr)

    n_recv = n_recv + 1
    call mpi_irecv(Arrive(iwest),1,MPI_INT,mywest,tag_w,MPI_COMM_WORLD, &
                   reqs(n_recv),ierr)

    n_recv = n_recv + 1
    call mpi_irecv(Arrive(ieast),1,MPI_INT,myeast,tag_e,MPI_COMM_WORLD, &
                   reqs(n_recv),ierr)

    n_recv = n_recv + 1
    call mpi_irecv(Arrive(inw),1,MPI_INT,mynw,tag_nw,MPI_COMM_WORLD, &
                   reqs(n_recv),ierr)

    n_recv = n_recv + 1
    call mpi_irecv(Arrive(ine),1,MPI_INT,myne,tag_ne,MPI_COMM_WORLD, &
                   reqs(n_recv),ierr)

    n_recv = n_recv + 1
    call mpi_irecv(Arrive(isw),1,MPI_INT,mysw,tag_sw,MPI_COMM_WORLD, &
                   reqs(n_recv),ierr)

    n_recv = n_recv + 1
    call mpi_irecv(Arrive(ise),1,MPI_INT,myse,tag_se,MPI_COMM_WORLD, &
                   reqs(n_recv),ierr)

    ! initiate the MPI non-blocking send interface to 
    ! send how many droplets entering the nearest neighbor 

    n_send = 9
    call mpi_isend(Depart(inorth),1,MPI_INT,mynorth,tag_s,MPI_COMM_WORLD, &
                   reqs(n_send),ierr)

    n_send = n_send + 1
    call mpi_isend(Depart(isouth),1,MPI_INT,mysouth,tag_n,MPI_COMM_WORLD, &
                   reqs(n_send),ierr)

    n_send = n_send + 1
    call mpi_isend(Depart(iwest),1,MPI_INT,mywest,tag_e,MPI_COMM_WORLD, &
                   reqs(n_send),ierr)

    n_send = n_send + 1
    call mpi_isend(Depart(ieast),1,MPI_INT,myeast,tag_w,MPI_COMM_WORLD, &
                   reqs(n_send),ierr)

    n_send = n_send + 1
    call mpi_isend(Depart(inw),1,MPI_INT,mynw,tag_se,MPI_COMM_WORLD, &
                   reqs(n_send),ierr)

    n_send = n_send + 1
    call mpi_isend(Depart(ine),1,MPI_INT,myne,tag_sw,MPI_COMM_WORLD, &
                   reqs(n_send),ierr)

    n_send = n_send + 1
    call mpi_isend(Depart(isw),1,MPI_INT,mysw,tag_ne,MPI_COMM_WORLD, &
                   reqs(n_send),ierr)

    n_send = n_send + 1
    call mpi_isend(Depart(ise),1,MPI_INT,myse,tag_nw,MPI_COMM_WORLD, &
                   reqs(n_send),ierr)

    ! make sure that all the non-blocking MPI operations are complete

    call mpi_waitall(16,reqs,status1,ierr)

    ! sanity check: if too many droplets enter the current MPI region
    !               and exceed the number of "holes", stop the program
    !               with an error message

    if ( sum(Arrive) .gt. numHoles ) then
        write(*,*) "Too many new droplets will enter the MPI rank: ", myid
        write(*,*) "nparcelsLocalActive,nparcelsLocal",nparcelsLocalActive,nparcelsLocal
        write(*,*) "Arriving, holes to fill: ", sum(Arrive), numHoles
        write(*,*) "Stop the program ..."
        call stopcm1
    end if

#if 0 
    ! sanity check: if the total number of leaving droplets matches
    !               the total number of entering droploets across 
    !               all the MPI ranks

    n_send = sum(Depart)
    if ( myid == 0 ) then
       call mpi_reduce(MPI_IN_PLACE,n_send,1,MPI_INT, &
                       MPI_SUM,0,MPI_COMM_WORLD,ierr)
    else
       call mpi_reduce(n_send,n_send,1,MPI_INT, &
                       MPI_SUM,0,MPI_COMM_WORLD,ierr)
    end if

    n_recv = sum(Arrive)
    if ( myid == 0 ) then
       call mpi_reduce(MPI_IN_PLACE,n_recv,1,MPI_INT, &
                       MPI_SUM,0,MPI_COMM_WORLD,ierr)
    else
       call mpi_reduce(n_recv,n_recv,1,MPI_INT, &
                       MPI_SUM,0,MPI_COMM_WORLD,ierr)
    end if

    if ( myid == 0 ) then
       if ( n_send .ne. n_recv ) then
          write(*,*) "Unmatched total leaving droplets vs. total entering droplets..."
          write(*,*) "Total leaving droplets: ", n_send
          write(*,*) "Total entering droplets: ", n_recv
          write(*,*) "Stop the program ..."
          call stopcm1
       end if
    end if

#endif

  end subroutine comm_droplet_number

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  subroutine comm_droplet_value(holes_ind,Depart_ind,Depart,Arrive, &
                ptrDepart, ptrArrive, pdata)

    use input, only : myid,nparcelsLocal,npvals,mynw,mysw,myne, &
                      myse,myeast,mywest,mynorth,mysouth,ierr, &
                      pract,nparcelsLocalActive, &
                      timestats,mytime, &
      time_dropC4a,time_dropC4b,time_dropC4c, &
      time_dropC4d,time_dropC4e,time_dropC4f
    use constants, only: undefined_index,neg_huge,num_nn,inorth, &
                         isouth,iwest,ieast,inw,ine,isw,ise
    use mpi

    implicit none

    integer, intent(in), dimension(:) :: holes_ind                       ! location index in "pdata" that can 
                                                                         ! add a new droplet;
                                                                         ! not a fixed-size array but we do 
                                                                         ! not need to know its size
    integer, intent(in), dimension(:) :: Depart_ind                      ! location index in "pdata" of droplets
                                                                         ! that  are departing
    integer, intent(in) :: Depart(num_nn)                                ! Number of droplets that will enter each 
                                                                         ! nearest neighbor at different directions
    integer, intent(in) :: Arrive(num_nn)                                ! Number of droplets that will enter the 
                                                                         ! current MPI region from each nearest 
                                                                         ! neighbor at different directions
    integer, intent(in) :: ptrArrive(num_nn)                             ! location index into holes_ind 
    integer, intent(in) :: ptrDepart(num_nn)                             ! location index into Depart_ind 
    real, intent(inout), dimension(nparcelsLocal,npvals) :: pdata        ! droplet information
    ! integer, intent(in), dimension(nparcelsLocal) :: pdata_neighbor      ! array to store the new MPI region 
    !                                                                      ! info for all the droplets

    ! Local variables

    real, dimension(:,:), allocatable :: droplet_n1, droplet_s1, droplet_w1, &     ! information of droplets that will leave the 
                                         droplet_e1, droplet_nw1, droplet_ne1, &   ! current MPI region and enter the nearest neighbor;
                                         droplet_sw1, droplet_se1, &               ! 1 means sending array;
                                         droplet_n2, droplet_s2, droplet_w2, &     ! 2 means receiving array
                                         droplet_e2, droplet_nw2, droplet_ne2, &
                                         droplet_sw2, droplet_se2

    integer :: n_idx,s_idx,w_idx,e_idx,nw_idx,ne_idx,sw_idx,se_idx
    integer :: reqs(16),tag(num_nn,2)
    integer :: index_n,index_s,index_w,index_e,index_nw,index_ne, &
               index_sw,index_se,n_recv,n_send
    integer, dimension(mpi_status_size,16) :: status1
    integer :: i,j,k,n,indx
    integer :: numDepart,numArrive,num_holes,last_active
    integer :: num_tmp

    ! initialize some MPI related variables

    index_n  = undefined_index
    index_s  = undefined_index
    index_w  = undefined_index
    index_e  = undefined_index
    index_nw = undefined_index
    index_sw = undefined_index
    index_ne = undefined_index
    index_se = undefined_index

    do i = 1, num_nn
       tag(i,1) = undefined_index
       tag(i,2) = undefined_index
    end do

    numDepart = sum(Depart)
    numArrive = sum(Arrive)

     ! allocate spaces to store the droplets that will leave the current MPI region

     if ( Depart(inorth) .ne. 0 ) allocate(droplet_n1(Depart(inorth),npvals))
     if ( Depart(isouth) .ne. 0 ) allocate(droplet_s1(Depart(isouth),npvals))
     if ( Depart(iwest) .ne. 0 ) allocate(droplet_w1(Depart(iwest),npvals))
     if ( Depart(ieast) .ne. 0 ) allocate(droplet_e1(Depart(ieast),npvals))
     if ( Depart(inw) .ne. 0 ) allocate(droplet_nw1(Depart(inw),npvals))
     if ( Depart(ine) .ne. 0 ) allocate(droplet_ne1(Depart(ine),npvals))
     if ( Depart(isw) .ne. 0 ) allocate(droplet_sw1(Depart(isw),npvals))
     if ( Depart(ise) .ne. 0 ) allocate(droplet_se1(Depart(ise),npvals))

     ! allocate temporary arrays if there are new droplets from the nearest neighbor

     if ( Arrive(inorth) .ne. 0 ) allocate(droplet_n2(Arrive(inorth),npvals))
     if ( Arrive(isouth) .ne. 0 ) allocate(droplet_s2(Arrive(isouth),npvals))
     if ( Arrive(iwest) .ne. 0 ) allocate(droplet_w2(Arrive(iwest),npvals))
     if ( Arrive(ieast) .ne. 0 ) allocate(droplet_e2(Arrive(ieast),npvals))
     if ( Arrive(inw) .ne. 0 ) allocate(droplet_nw2(Arrive(inw),npvals))
     if ( Arrive(ine) .ne. 0 ) allocate(droplet_ne2(Arrive(ine),npvals))
     if ( Arrive(isw) .ne. 0 ) allocate(droplet_sw2(Arrive(isw),npvals))
     if ( Arrive(ise) .ne. 0 ) allocate(droplet_se2(Arrive(ise),npvals))

     !$acc data create(droplet_n1,droplet_s1,droplet_w1,droplet_e1,     &
     !$acc             droplet_nw1,droplet_ne1,droplet_sw1,droplet_se1, &
     !$acc             droplet_n2,droplet_s2,droplet_w2,droplet_e2,     &
     !$acc             droplet_nw2,droplet_ne2,droplet_sw2,droplet_se2)

     if(timestats.ge.1) time_dropC4a=time_dropC4a+mytime()

     ! extract the information of droplets that will leave the current MPI region 
     ! and reset the corresponding "pdata" record to a large negative value
     ! pack up droplet send buffers and mark departed droplets in pdata
     if(Depart(inorth) .gt. 0) &
        call packDroplets(inorth, Depart, ptrDepart, Depart_ind, pdata, droplet_n1)

     if(Depart(isouth) .gt. 0) &
        call packDroplets(isouth, Depart, ptrDepart, Depart_ind, pdata, droplet_s1)

     if(Depart(iwest) .gt. 0) &
        call packDroplets(iwest, Depart, ptrDepart, Depart_ind, pdata, droplet_w1)

     if(Depart(ieast) .gt. 0) &
        call packDroplets(ieast, Depart, ptrDepart, Depart_ind, pdata, droplet_e1)

     if(Depart(inw) .gt. 0) &
        call packDroplets(inw, Depart, ptrDepart, Depart_ind, pdata, droplet_nw1)

     if(Depart(ine) .gt. 0) &
        call packDroplets(ine, Depart, ptrDepart, Depart_ind, pdata, droplet_ne1)

     if(Depart(isw) .gt. 0) &
        call packDroplets(isw, Depart,  ptrDepart, Depart_ind, pdata, droplet_sw1)

     if(Depart(ise) .gt. 0) &
        call packDroplets(ise, Depart, ptrDepart, Depart_ind, pdata, droplet_se1)

     if(timestats.ge.1) time_dropC4b=time_dropC4b+mytime()


     ! initiate the MPI non-blocking receive interface to 
     ! obtain new droplet information from the nearest neighbors
     if(timestats.ge.1) time_dropC4c=time_dropC4c+mytime()

     n_recv   = 0
     if ( Arrive(inorth) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(inorth,2)  = 1000 + Arrive(inorth)
       !$acc host_data use_device(droplet_n2)
       call mpi_irecv(droplet_n2,Arrive(inorth)*npvals,MPI_REAL, &
                      mynorth,tag(inorth,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       !$acc end host_data
       index_n = n_recv
     end if
     if ( Arrive(isouth) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(isouth,2)  = 1000 + Arrive(isouth)
       !$acc host_data use_device(droplet_s2)
       call mpi_irecv(droplet_s2,Arrive(isouth)*npvals,MPI_REAL, &
                      mysouth,tag(isouth,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       !$acc end host_data
       index_s = n_recv
     end if
     if ( Arrive(iwest) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(iwest,2)  = 1000 + Arrive(iwest)
       !$acc host_data use_device(droplet_w2)
       call mpi_irecv(droplet_w2,Arrive(iwest)*npvals,MPI_REAL, &
                      mywest,tag(iwest,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       !$acc end host_data
       index_w = n_recv
     end if
     if ( Arrive(ieast) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(ieast,2)  = 1000 + Arrive(ieast)
       !$acc host_data use_device(droplet_e2)
       call mpi_irecv(droplet_e2,Arrive(ieast)*npvals,MPI_REAL, &
                      myeast,tag(ieast,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       !$acc end host_data
       index_e = n_recv
     end if
     if ( Arrive(inw) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(inw,2) = 1000 + Arrive(inw)
       !$acc host_data use_device(droplet_nw2)
       call mpi_irecv(droplet_nw2,Arrive(inw)*npvals,MPI_REAL, &
                      mynw,tag(inw,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       !$acc end host_data
       index_nw = n_recv
     end if
     if ( Arrive(ine) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(ine,2) = 1000 + Arrive(ine)
       !$acc host_data use_device(droplet_ne2)
       call mpi_irecv(droplet_ne2,Arrive(ine)*npvals,MPI_REAL, &
                      myne,tag(ine,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       !$acc end host_data
       index_ne = n_recv
     end if
     if ( Arrive(isw) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(isw,2) = 1000 + Arrive(isw)
       !$acc host_data use_device(droplet_sw2)
       call mpi_irecv(droplet_sw2,Arrive(isw)*npvals,MPI_REAL, &
                      mysw,tag(isw,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       !$acc end host_data
       index_sw = n_recv
     end if
     if ( Arrive(ise) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(ise,2) = 1000 + Arrive(ise)
       !$acc host_data use_device(droplet_se2)
       call mpi_irecv(droplet_se2,Arrive(ise)*npvals,MPI_REAL, &
                      myse,tag(ise,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       !$acc end host_data
       index_se = n_recv
     end if

     ! initiate the MPI non-blocking send interface to send the information of
     ! droplets that leave the current MPI region to the nearest neighbors 

     n_send = 8
     if ( Depart(inorth) .ne. 0 ) then
       n_send = n_send + 1
       tag(isouth,1) = 1000 + Depart(inorth)
       !$acc host_data use_device(droplet_n1)
       call mpi_isend(droplet_n1,Depart(inorth)*npvals,MPI_REAL, &
                      mynorth,tag(isouth,1),MPI_COMM_WORLD,reqs(n_send),ierr)
       !$acc end host_data
     end if
     if ( Depart(isouth) .ne. 0 ) then
       n_send = n_send + 1
       tag(inorth,1) = 1000 + Depart(isouth)
       !$acc host_data use_device(droplet_s1)
       call mpi_isend(droplet_s1,Depart(isouth)*npvals,MPI_REAL, &
                      mysouth,tag(inorth,1),MPI_COMM_WORLD,reqs(n_send),ierr)
       !$acc end host_data
     end if
     if ( Depart(iwest) .ne. 0 ) then
       n_send = n_send + 1
       tag(ieast,1) = 1000 + Depart(iwest)
       !$acc host_data use_device(droplet_w1)
       call mpi_isend(droplet_w1,Depart(iwest)*npvals,MPI_REAL, &
                      mywest,tag(ieast,1),MPI_COMM_WORLD,reqs(n_send),ierr)
       !$acc end host_data
     end if
     if ( Depart(ieast) .ne. 0 ) then
       n_send = n_send + 1
       tag(iwest,1) = 1000 + Depart(ieast)
       !$acc host_data use_device(droplet_e1)
       call mpi_isend(droplet_e1,Depart(ieast)*npvals,MPI_REAL, &
                      myeast,tag(iwest,1),MPI_COMM_WORLD,reqs(n_send),ierr)
       !$acc end host_data
     end if
     if ( Depart(inw) .ne. 0 ) then
       n_send = n_send + 1
       tag(ise,1) = 1000 + Depart(inw)
       !$acc host_data use_device(droplet_nw1)
       call mpi_isend(droplet_nw1,Depart(inw)*npvals,MPI_REAL, &
                      mynw,tag(ise,1),MPI_COMM_WORLD,reqs(n_send),ierr)
       !$acc end host_data
     end if
     if ( Depart(ine) .ne. 0 ) then
       n_send = n_send + 1
       tag(isw,1) = 1000 + Depart(ine)
       !$acc host_data use_device(droplet_ne1)
       call mpi_isend(droplet_ne1,Depart(ine)*npvals,MPI_REAL, &
                      myne,tag(isw,1),MPI_COMM_WORLD,reqs(n_send),ierr)
       !$acc end host_data
     end if
     if ( Depart(isw) .ne. 0 ) then
       n_send = n_send + 1
       tag(ine,1) = 1000 + Depart(isw)
       !$acc host_data use_device(droplet_sw1)
       call mpi_isend(droplet_sw1,Depart(isw)*npvals,MPI_REAL, &
                      mysw,tag(ine,1),MPI_COMM_WORLD,reqs(n_send),ierr)
       !$acc end host_data
     end if
     if ( Depart(ise) .ne. 0 ) then
       n_send = n_send + 1
       tag(inw,1) = 1000 + Depart(ise)
       !$acc host_data use_device(droplet_se1)
       call mpi_isend(droplet_se1,Depart(ise)*npvals,MPI_REAL, &
                      myse,tag(inw,1),MPI_COMM_WORLD,reqs(n_send),ierr)
       !$acc end host_data
     end if

     ! update the "holes" in "pdata" with the new droplets that
     ! enter the current MPI region from the nearest neighbors
     n = 1
     k = 1
     do while( n .le. n_recv )
       call mpi_waitany(n_recv,reqs(1:n_recv),indx,MPI_STATUS_IGNORE,ierr)
       if ( indx .eq. index_n ) then
          call update_new_droplet ( inorth, Arrive, ptrArrive, holes_ind, pdata, droplet_n2)
       else if ( indx .eq. index_s ) then
          call update_new_droplet ( isouth, Arrive, ptrArrive, holes_ind, pdata, droplet_s2)
       else if ( indx .eq. index_w ) then
          call update_new_droplet ( iwest, Arrive, ptrArrive, holes_ind, pdata, droplet_w2)
       else if ( indx .eq. index_e ) then
          call update_new_droplet ( ieast, Arrive, ptrArrive, holes_ind, pdata, droplet_e2)
       else if ( indx .eq. index_nw ) then
          call update_new_droplet ( inw, Arrive, ptrArrive, holes_ind, pdata, droplet_nw2)
       else if ( indx .eq. index_ne ) then
          call update_new_droplet ( ine, Arrive, ptrArrive, holes_ind, pdata, droplet_ne2)
       else if ( indx .eq. index_sw ) then
          call update_new_droplet ( isw, Arrive, ptrArrive, holes_ind, pdata, droplet_sw2)
       else if ( indx .eq. index_se ) then
          call update_new_droplet ( ise, Arrive, ptrArrive, holes_ind, pdata, droplet_se2)
       else
          write(*,*) indx," is not a nearest neighbor for myid = ",myid
          write(*,*) "Stop the program..."
          call stopcm1
       end if
       n = n + 1
     end do
     if(timestats.ge.1) time_dropC4d=time_dropC4d+mytime()

    ! make sure that all the non-blocking MPI send operations are complete

    n_send = n_send - 8
    if ( n_send .ge. 1 ) then
       call mpi_waitall(n_send,reqs(9:9+n_send-1),status1(1:mpi_status_size,9:9+n_send-1),ierr)
    end if

     if(timestats.ge.1) time_dropC4e=time_dropC4e+mytime()

     ! free up the memory for temporary variables
     !$acc end data

     if (allocated(droplet_n1))  deallocate(droplet_n1)
     if (allocated(droplet_n2))  deallocate(droplet_n2)
     if (allocated(droplet_s1))  deallocate(droplet_s1)
     if (allocated(droplet_s2))  deallocate(droplet_s2)
     if (allocated(droplet_w1))  deallocate(droplet_w1)
     if (allocated(droplet_w2))  deallocate(droplet_w2)
     if (allocated(droplet_e1))  deallocate(droplet_e1)
     if (allocated(droplet_e2))  deallocate(droplet_e2)
     if (allocated(droplet_ne1)) deallocate(droplet_ne1)
     if (allocated(droplet_ne2)) deallocate(droplet_ne2)
     if (allocated(droplet_nw1)) deallocate(droplet_nw1)
     if (allocated(droplet_nw2)) deallocate(droplet_nw2)
     if (allocated(droplet_se1)) deallocate(droplet_se1)
     if (allocated(droplet_se2)) deallocate(droplet_se2)
     if (allocated(droplet_sw1)) deallocate(droplet_sw1)
     if (allocated(droplet_sw2)) deallocate(droplet_sw2)

     if(timestats.ge.1) time_dropC4f=time_dropC4f+mytime()

  end subroutine comm_droplet_value

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine makeContiguous(Depart,Arrive,num_fallout,holes_ind,backfill_ind,pdata,pdata_locind)
      ! =================================================================
      ! The GPU-enabled makeCongiguous subroutine will fill in "holes" in
      ! the pdata array such that the all active droplets are
      ! contained in the first part of the "pdata" array. This
      ! is achieved by moving droplets from the end of the "pdata" 
      ! array to fill in any remaining holes.
      ! =================================================================

      use input, only : myid,nparcelsLocalActive,nparcelsLocal,npvals,pract
      use constants, only : num_nn,undefined_index,neg_huge

      integer, intent(in) :: Depart(num_nn)                              ! Number of droplets that will enter each
                                                                         ! nearest neighbor at different directions
      integer, intent(in) :: Arrive(num_nn)                              ! Number of droplets that will enter the
                                                                         ! current MPI region from each nearest
      integer, intent(in), dimension(:) :: holes_ind                     ! location index in "pdata" that can 
                                                                         ! add a new droplet;
                                                                         ! not a fixed-size array but we do 
                                                                         ! not need to know its size
                                                                         ! neighbor at different directions
      integer, intent(in), dimension(:) :: backfill_ind
      integer, intent(in) :: num_fallout
      real, intent(inout), dimension(nparcelsLocal,npvals) :: pdata      ! droplet information
      integer, intent(inout), dimension(nparcelsLocal,3) :: pdata_locind ! x/y/z location index of each droplet

      integer :: num_tmp,numArrive,numDepart,num_holes,indx,k,n
      integer :: iv
      logical, parameter :: Debug=.false.

      !simplified code that fills the holes
      numDepart = SUM(Depart)
      numArrive = SUM(Arrive)
      num_tmp = numDepart+num_fallout
      if ( numArrive .lt. num_tmp ) then
        num_holes = num_tmp - numArrive             ! number of remaining "holes" we need to fill
                                                    ! to make the active dropelts in
                                                    ! the "pdata" contiguous

        !$acc parallel loop gang vector default(present)
        do n=1,num_holes
           indx = holes_ind(numArrive+n)            ! index of hole to be filled
           k    = backfill_ind(n)                   ! index of droplet to move into hole
           if(indx .lt. k) then 
             if (Debug) then 
               write(*,*) ' backfilled droplet # ',indx,' with droplet # ',k
               call flush(6)
             endif
             !$acc loop seq
             do iv=1,npvals
               pdata(indx,iv) = pdata(k,iv)
               pdata(k,iv)    = neg_huge
             enddo
             pdata_locind(indx,1) = pdata_locind(k,1)
             pdata_locind(indx,2) = pdata_locind(k,2)
             pdata_locind(indx,3) = pdata_locind(k,3)
             pdata_locind(k,1) = undefined_index
             pdata_locind(k,2) = undefined_index
             pdata_locind(k,3) = undefined_index
           endif
        enddo
      end if


      end subroutine makeContiguous

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  subroutine packDroplets( dir_idx, Depart, ptrDepart, Depart_ind, &
                                      pdata, droplet_buffer )

  ! this subroutine is used to:
  !      1. save the information of a droplet in "pdata" that will 
  !         leave the current MPI region to a buffer
  !      2. reset the corresponding slot in "pdata" to a negative value
  !      3. record this "hole" index in "pdata"

  use input, only : npvals,nparcelsLocal
  use constants, only: undefined_index, neg_huge

  implicit none

  integer, intent(in)    :: dir_idx                              ! neighbor's directional index 
  integer, intent(in), dimension(:) :: Depart                    ! number of droplets departing 
  integer, intent(in), dimension(:) :: ptrDepart                 ! Pointer into the departure index "Depart_ind"
  integer, intent(in), dimension(:) :: Depart_ind                !  Departure indices
  real, intent(inout), dimension(nparcelsLocal,npvals) :: pdata  ! the "pdata" with droplet information
  real, intent(out), dimension(:,:) :: droplet_buffer            ! array to save the information of leaving droplet
  integer :: i,iv,numDepart,np,iptr

     iptr = ptrDepart(dir_idx)
     numDepart   = Depart(dir_idx)
     !$acc parallel loop gang vector collapse(2) default(present)
     do i=1,numDepart
       do iv=1,npvals
         np = Depart_ind(iptr+i-1)
         droplet_buffer(i,iv) = pdata(np,iv)
         pdata(np,iv) = neg_huge
       enddo
     enddo

  end subroutine packDroplets

    subroutine setupIndexPointers(Cnt,ptrCnt)

      use constants, only: num_nn,inorth,isouth, &
                         iwest,ieast,inw,ine,isw,ise
      implicit none

      integer, intent(in) :: Cnt(num_nn) 
      integer, intent(inout) :: ptrCnt(num_nn)

      ! local variables
      integer :: n_idx,s_idx,e_idx,w_idx,nw_idx,ne_idx,sw_idx,se_idx


      ptrCnt(inorth) = 1
      ptrCnt(isouth) = ptrCnt(inorth) + Cnt(inorth)
      ptrCnt(ieast)  = ptrCnt(isouth) + Cnt(isouth)
      ptrCnt(iwest)  = ptrCnt(ieast)  + Cnt(ieast)
      ptrCnt(inw)    = ptrCnt(iwest)  + Cnt(iwest)
      ptrCnt(ine)    = ptrCnt(inw)    + Cnt(inw)
      ptrCnt(isw)    = ptrCnt(ine)    + Cnt(ine)
      ptrCnt(ise)    = ptrCnt(isw)    + Cnt(isw)

    end subroutine setupIndexPointers
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    subroutine setupDepartDroplet(Depart_ind,ptrDepart,pdata_neighbor)

      use input, only : mynw,mysw,myne,myse,myeast,mywest,mynorth,mysouth, &
        nparcelsLocalActive
      use constants, only: num_nn,inorth,isouth, &
                         iwest,ieast,inw,ine,isw,ise,undefined_index

      integer, intent(inout) :: ptrDepart(num_nn)

      integer, intent(inout) :: Depart_ind(:)
      integer, intent(in) :: pdata_neighbor(:)

      ! local variables
      integer :: n_idx,s_idx,e_idx,w_idx,nw_idx,ne_idx,sw_idx,se_idx
      integer np


      n_idx  = ptrDepart(inorth)
      s_idx  = ptrDepart(isouth)
      e_idx  = ptrDepart(ieast)
      w_idx  = ptrDepart(iwest)
      nw_idx = ptrDepart(inw)
      ne_idx = ptrDepart(ine)
      sw_idx = ptrDepart(isw)
      se_idx = ptrDepart(ise)
      do np=1, nparcelsLocalActive
        if(pdata_neighbor(np) .eq. mynorth) then
          Depart_ind(n_idx) = np
          n_idx=n_idx+1
        else if(pdata_neighbor(np) .eq. mysouth) then
          Depart_ind(s_idx) = np
          s_idx=s_idx+1
        else if(pdata_neighbor(np) .eq. mywest) then
          Depart_ind(w_idx) = np
          w_idx=w_idx+1
        else if(pdata_neighbor(np) .eq. myeast) then
          Depart_ind(e_idx) = np
          e_idx=e_idx+1
        else if(pdata_neighbor(np) .eq. mynw) then
          Depart_ind(nw_idx) = np
          nw_idx=nw_idx+1
        else if(pdata_neighbor(np) .eq. myne) then
          Depart_ind(ne_idx) = np
          ne_idx=ne_idx+1
        else if(pdata_neighbor(np) .eq. mysw) then
          Depart_ind(sw_idx) = np
          sw_idx=sw_idx+1
        else if(pdata_neighbor(np) .eq. myse) then
          Depart_ind(se_idx) = np
          se_idx=se_idx+1
        endif
      enddo

     end subroutine setupDepartDroplet

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

     subroutine update_new_droplet ( dir_idx, Arrive, ptrArrive, holes_ind, pdata, droplet_buffer)

     ! this subroutine is used to update the new droplet information
     ! to the "pdata" array on a particular MPI rank after the MPI communication

     use input, only : npvals,nparcelsLocal

     implicit none

     integer, intent(in), dimension(:) :: holes_ind                  ! array to store the location index of "holes" in "pdata"
     integer, intent(in) :: dir_idx                               !  neighbor's directional index
     integer, intent(in), dimension(:) :: Arrive                    ! number of droplets departing
     integer, intent(in), dimension(:) :: ptrArrive                 ! Pointer into the departure index "Depart_ind"

     real, intent(in), dimension(:,:) :: droplet_buffer              ! array to save the information of new droplets
     real, intent(inout), dimension(nparcelsLocal,npvals) :: pdata   ! the "pdata" with droplet information

     integer :: numArrive,i,iptr,iv,ptr

     ptr = ptrArrive(dir_idx)
     numArrive = Arrive(dir_idx)
     !$acc parallel loop gang vector collapse(2) default(present)
     do i = 1, numArrive
       do iv=1,npvals
         iptr = holes_ind(ptr+i-1)
         pdata(iptr,iv) = droplet_buffer(i,iv)
       end do
     end do

  end subroutine update_new_droplet

#endif

end module comm_droplet_module
