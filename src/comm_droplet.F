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

contains

#ifdef MPI

  subroutine comm_droplet_number(holes_ind,Depart,Arrive,pdata, &
                                 pdata_locind,pdata_neighbor)

    use input, only : myid,nparcelsLocal,npvals,mynw,mysw,myne, &
                      myse,myeast,mywest,mynorth,mysouth,ierr, &
                      nparcelsActive
    use constants, only: undefined_index,num_nn,inorth,isouth, &
                         iwest,ieast,inw,ine,isw,ise
    use mpi

    implicit none

    integer, intent(inout), dimension(:) :: holes_ind                    ! location index in "pdata" that can 
                                                                         ! add a new droplet;
                                                                         ! not a fixed-size array but we do 
                                                                         ! not need to know its size
    integer, intent(out) :: Depart(num_nn)                               ! Number of droplets that will enter each 
                                                                         ! nearest neighbor at different directions
    integer, intent(out) :: Arrive(num_nn)                               ! Number of droplets that will enter the 
                                                                         ! current MPI region from each nearest 
                                                                         ! neighbor at different directions
    real, intent(in), dimension(nparcelsLocal,npvals) :: pdata           ! droplet information
    integer, intent(in), dimension(nparcelsLocal,3) :: pdata_locind      ! x/y/z location index of each droplet
    integer, intent(in), dimension(nparcelsLocal) :: pdata_neighbor      ! array to store the new MPI region 
                                                                         ! info for all the droplets
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

    do i = 1, num_nn
       Depart(i) = 0
    end do

    ! find out how many droplets will leave the 
    ! current MPI region and to which neighbor

    i = 1
    do n = 1, nparcelsLocal
       if ( (pdata_locind(n,1) .eq. undefined_index) .and. &
            (pdata_locind(n,2) .eq. undefined_index) ) then
          ! record the location index that can be used to store the new droplet
          ! information after the old drolpet leaves the current MPI region
          holes_ind(i) = n
          i = i + 1
          if ( n .le. nparcelsActive ) then
             if ( pdata_neighbor(n) .eq. mynorth ) then
                Depart(inorth) = Depart(inorth) + 1
             else if ( pdata_neighbor(n) .eq. mysouth ) then
                Depart(isouth) = Depart(isouth) + 1
             else if ( pdata_neighbor(n) .eq. mywest ) then
                Depart(iwest) = Depart(iwest) + 1
             else if ( pdata_neighbor(n) .eq. myeast ) then
                Depart(ieast) = Depart(ieast) + 1
             else if ( pdata_neighbor(n) .eq. mynw ) then
                Depart(inw) = Depart(inw) + 1
             else if ( pdata_neighbor(n) .eq. myne ) then
                Depart(ine) = Depart(ine) + 1
             else if ( pdata_neighbor(n) .eq. mysw ) then
                Depart(isw) = Depart(isw) + 1
             else if ( pdata_neighbor(n) .eq. myse ) then
                Depart(ise) = Depart(ise) + 1
             else
                cycle  ! this is an existing "hole" of "pdata"
                       ! no action is needed
             end if
          end if
       end if
    end do

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

    if ( sum(Arrive) .gt. size(holes_ind) ) then
        write(*,*) "Too many new droplets will enter the MPI rank: ", myid
        stop "Stop the program ..."
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
          stop "Stop the program ..."
       end if
    end if

#endif

  end subroutine comm_droplet_number

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  subroutine comm_droplet_value(holes_ind,Depart,Arrive,pdata, &
                                pdata_locind,pdata_neighbor)

    use input, only : myid,nparcelsLocal,npvals,mynw,mysw,myne, &
                      myse,myeast,mywest,mynorth,mysouth,ierr, &
                      pract,nparcelsActive
    use constants, only: undefined_index,neg_huge,num_nn,inorth, &
                         isouth,iwest,ieast,inw,ine,isw,ise
    use mpi

    implicit none

    integer, intent(in), dimension(:) :: holes_ind                       ! location index in "pdata" that can 
                                                                         ! add a new droplet;
                                                                         ! not a fixed-size array but we do 
                                                                         ! not need to know its size
    integer, intent(in) :: Depart(num_nn)                                ! Number of droplets that will enter each 
                                                                         ! nearest neighbor at different directions
    integer, intent(in) :: Arrive(num_nn)                                ! Number of droplets that will enter the 
                                                                         ! current MPI region from each nearest 
                                                                         ! neighbor at different directions
    real, intent(inout), dimension(nparcelsLocal,npvals) :: pdata        ! droplet information
    integer, intent(inout), dimension(nparcelsLocal,3) :: pdata_locind   ! x/y/z location index of each droplet
    integer, intent(in), dimension(nparcelsLocal) :: pdata_neighbor      ! array to store the new MPI region 
                                                                         ! info for all the droplets

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
    integer :: i,k,n,indx
    integer :: numDepart,numArrive,num_holes,last_active

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

    ! extract the information of droplets that will leave the current MPI region 
    ! and reset the corresponding "pdata" record to a large negative value

    n_idx  = 1
    s_idx  = 1
    w_idx  = 1
    e_idx  = 1
    nw_idx = 1
    ne_idx = 1
    sw_idx = 1
    se_idx = 1
    i = 1

    if ( numDepart .ne. 0 ) then
       do n = 1, nparcelsActive
          if ( pdata_neighbor(n) .eq. mynorth ) then
             call record_leaving_droplet ( n, n_idx, i, pdata, droplet_n1 )
          else if ( pdata_neighbor(n) .eq. mysouth ) then
             call record_leaving_droplet ( n, s_idx, i, pdata, droplet_s1 )
          else if ( pdata_neighbor(n) .eq. mywest ) then
             call record_leaving_droplet ( n, w_idx, i, pdata, droplet_w1 )
          else if ( pdata_neighbor(n) .eq. myeast ) then
             call record_leaving_droplet ( n, e_idx, i, pdata, droplet_e1 )
          else if ( pdata_neighbor(n) .eq. mynw ) then
             call record_leaving_droplet ( n, nw_idx, i, pdata, droplet_nw1 )
          else if ( pdata_neighbor(n) .eq. myne ) then
             call record_leaving_droplet ( n, ne_idx, i, pdata, droplet_ne1 )
          else if ( pdata_neighbor(n) .eq. mysw ) then
             call record_leaving_droplet ( n, sw_idx, i, pdata, droplet_sw1 )
          else if ( pdata_neighbor(n) .eq. myse ) then
             call record_leaving_droplet ( n, se_idx, i, pdata, droplet_se1 )
          else
             cycle   ! this droplet stays at the same MPI region or this is a "hole";
                     ! no action is needed
          end if
       end do
    end if

    ! allocate temporary arrays if there are new droplets from the nearest neighbor

    if ( Arrive(inorth) .ne. 0 ) allocate(droplet_n2(Arrive(inorth),npvals))
    if ( Arrive(isouth) .ne. 0 ) allocate(droplet_s2(Arrive(isouth),npvals))
    if ( Arrive(iwest) .ne. 0 ) allocate(droplet_w2(Arrive(iwest),npvals))
    if ( Arrive(ieast) .ne. 0 ) allocate(droplet_e2(Arrive(ieast),npvals))
    if ( Arrive(inw) .ne. 0 ) allocate(droplet_nw2(Arrive(inw),npvals))
    if ( Arrive(ine) .ne. 0 ) allocate(droplet_ne2(Arrive(ine),npvals))
    if ( Arrive(isw) .ne. 0 ) allocate(droplet_sw2(Arrive(isw),npvals))
    if ( Arrive(ise) .ne. 0 ) allocate(droplet_se2(Arrive(ise),npvals))

    ! initiate the MPI non-blocking receive interface to 
    ! obtain new droplet information from the nearest neighbors

    n_recv   = 0
    if ( Arrive(inorth) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(inorth,2)  = 1000 + Arrive(inorth)
       call mpi_irecv(droplet_n2,Arrive(inorth)*npvals,MPI_REAL, &
                      mynorth,tag(inorth,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       index_n = n_recv
    end if
    if ( Arrive(isouth) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(isouth,2)  = 1000 + Arrive(isouth)
       call mpi_irecv(droplet_s2,Arrive(isouth)*npvals,MPI_REAL, &
                      mysouth,tag(isouth,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       index_s = n_recv
    end if
    if ( Arrive(iwest) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(iwest,2)  = 1000 + Arrive(iwest)
       call mpi_irecv(droplet_w2,Arrive(iwest)*npvals,MPI_REAL, &
                      mywest,tag(iwest,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       index_w = n_recv
    end if
    if ( Arrive(ieast) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(ieast,2)  = 1000 + Arrive(ieast)
       call mpi_irecv(droplet_e2,Arrive(ieast)*npvals,MPI_REAL, &
                      myeast,tag(ieast,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       index_e = n_recv
    end if
    if ( Arrive(inw) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(inw,2) = 1000 + Arrive(inw)
       call mpi_irecv(droplet_nw2,Arrive(inw)*npvals,MPI_REAL, &
                      mynw,tag(inw,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       index_nw = n_recv
    end if
    if ( Arrive(ine) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(ine,2) = 1000 + Arrive(ine)
       call mpi_irecv(droplet_ne2,Arrive(ine)*npvals,MPI_REAL, &
                      myne,tag(ine,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       index_ne = n_recv
    end if
    if ( Arrive(isw) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(isw,2) = 1000 + Arrive(isw)
       call mpi_irecv(droplet_sw2,Arrive(isw)*npvals,MPI_REAL, &
                      mysw,tag(isw,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       index_sw = n_recv
    end if
    if ( Arrive(ise) .ne. 0 ) then
       n_recv = n_recv + 1
       tag(ise,2) = 1000 + Arrive(ise)
       call mpi_irecv(droplet_se2,Arrive(ise)*npvals,MPI_REAL, &
                      myse,tag(ise,2),MPI_COMM_WORLD,reqs(n_recv),ierr)
       index_se = n_recv
    end if

    ! initiate the MPI non-blocking send interface to send the information of
    ! droplets that leave the current MPI region to the nearest neighbors 

    n_send = 8
    if ( Depart(inorth) .ne. 0 ) then
       n_send = n_send + 1
       tag(isouth,1) = 1000 + Depart(inorth)
       call mpi_isend(droplet_n1,Depart(inorth)*npvals,MPI_REAL, &
                      mynorth,tag(isouth,1),MPI_COMM_WORLD,reqs(n_send),ierr)
    end if
    if ( Depart(isouth) .ne. 0 ) then
       n_send = n_send + 1
       tag(inorth,1) = 1000 + Depart(isouth)
       call mpi_isend(droplet_s1,Depart(isouth)*npvals,MPI_REAL, &
                      mysouth,tag(inorth,1),MPI_COMM_WORLD,reqs(n_send),ierr)
    end if
    if ( Depart(iwest) .ne. 0 ) then
       n_send = n_send + 1
       tag(ieast,1) = 1000 + Depart(iwest)
       call mpi_isend(droplet_w1,Depart(iwest)*npvals,MPI_REAL, &
                      mywest,tag(ieast,1),MPI_COMM_WORLD,reqs(n_send),ierr)
    end if
    if ( Depart(ieast) .ne. 0 ) then
       n_send = n_send + 1
       tag(iwest,1) = 1000 + Depart(ieast)
       call mpi_isend(droplet_e1,Depart(ieast)*npvals,MPI_REAL, &
                      myeast,tag(iwest,1),MPI_COMM_WORLD,reqs(n_send),ierr)
    end if
    if ( Depart(inw) .ne. 0 ) then
       n_send = n_send + 1
       tag(ise,1) = 1000 + Depart(inw)
       call mpi_isend(droplet_nw1,Depart(inw)*npvals,MPI_REAL, &
                      mynw,tag(ise,1),MPI_COMM_WORLD,reqs(n_send),ierr)
    end if
    if ( Depart(ine) .ne. 0 ) then
       n_send = n_send + 1
       tag(isw,1) = 1000 + Depart(ine)
       call mpi_isend(droplet_ne1,Depart(ine)*npvals,MPI_REAL, &
                      myne,tag(isw,1),MPI_COMM_WORLD,reqs(n_send),ierr)
    end if
    if ( Depart(isw) .ne. 0 ) then
       n_send = n_send + 1
       tag(ine,1) = 1000 + Depart(isw)
       call mpi_isend(droplet_sw1,Depart(isw)*npvals,MPI_REAL, &
                      mysw,tag(ine,1),MPI_COMM_WORLD,reqs(n_send),ierr)
    end if
    if ( Depart(ise) .ne. 0 ) then
       n_send = n_send + 1
       tag(inw,1) = 1000 + Depart(ise)
       call mpi_isend(droplet_se1,Depart(ise)*npvals,MPI_REAL, &
                      myse,tag(inw,1),MPI_COMM_WORLD,reqs(n_send),ierr)
    end if

    ! update the "holes" in "pdata" with the new droplets that
    ! enter the current MPI region from the nearest neighbors

    n = 1
    k = 1
    do while( n .le. n_recv )
       call mpi_waitany(n_recv,reqs(1:n_recv),indx,MPI_STATUS_IGNORE,ierr)
       if ( indx .eq. index_n ) then
          call update_new_droplet ( holes_ind, k, Arrive(inorth), &
                                    droplet_n2, pdata )
       else if ( indx .eq. index_s ) then
          call update_new_droplet ( holes_ind, k, Arrive(isouth), &
                                    droplet_s2, pdata )
       else if ( indx .eq. index_w ) then
          call update_new_droplet ( holes_ind, k, Arrive(iwest), &
                                    droplet_w2, pdata )
       else if ( indx .eq. index_e ) then
          call update_new_droplet ( holes_ind, k, Arrive(ieast), &
                                    droplet_e2, pdata )
       else if ( indx .eq. index_nw ) then
          call update_new_droplet ( holes_ind, k, Arrive(inw), &
                                    droplet_nw2, pdata )
       else if ( indx .eq. index_ne ) then
          call update_new_droplet ( holes_ind, k, Arrive(ine), &
                                    droplet_ne2, pdata )
       else if ( indx .eq. index_sw ) then
          call update_new_droplet ( holes_ind, k, Arrive(isw), &
                                    droplet_sw2, pdata )
       else if ( indx .eq. index_se ) then
          call update_new_droplet ( holes_ind, k, Arrive(ise), &
                                    droplet_se2, pdata )
       else
          write(*,*) indx," is not a nearest neighbor for myid = ",myid
          stop "Stop the program..."
       end if
       n = n + 1
    end do

    ! make sure that all the non-blocking MPI send operations are complete

    n_send = n_send - 8
    if ( n_send .ge. 1 ) then
       call mpi_waitall(n_send,reqs(9:9+n_send-1),status1(1:mpi_status_size,9:9+n_send-1),ierr)
    end if

    ! there will be remaining "holes" in the "pdata" only when more
    !     droplets leave rather than enter the current MPI region;
    !
    ! compress the new "pdata" to make sure that all the active
    !     droplets are contiguous (no "holes" between "pdata" slots) 

    if ( numArrive .lt. numDepart ) then
       num_holes = numDepart - numArrive        ! number of "holes" we need to fill
                                                ! to make the active dropelts in 
                                                ! the "pdata" contiguous
       last_active = nparcelsActive
       i = 0
       do n = 1, numDepart
          indx = holes_ind(n)
          if ( pdata(indx,pract) .lt. 0.0 ) then   ! this "hole" is not filled yet
             do k = last_active, indx, -1
                if ( pdata(k,pract) .gt. 0.0 ) then
                   pdata(indx,:) = pdata(k,:)
                   pdata_locind(indx,:) = pdata_locind(k,:)
                   pdata(k,:) = neg_huge
                   pdata_locind(k,:) = undefined_index
                   last_active = k - 1
                   i = i + 1
                   exit
                end if
             end do
             if ( k .lt. indx ) exit            ! the active dropelts in the 
                                                ! "pdata" are already contiguous
                                                ! and no action is needed
             if ( i .eq. num_holes ) exit       ! now the active dropelts in 
                                                ! the "pdata" are contiguous
                                                ! after filling the "holes"
          end if
       end do
    end if

    ! free up the memory for temporary variables

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

  end subroutine comm_droplet_value

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  subroutine record_leaving_droplet ( np, neighbor_idx, leave_idx, &
                                      pdata, droplet_buffer )

  ! this subroutine is used to:
  !      1. save the information of a droplet in "pdata" that will 
  !         leave the current MPI region to a buffer
  !      2. reset the corresponding slot in "pdata" to a negative value
  !      3. record this "hole" index in "pdata"

  use input, only : npvals,nparcelsLocal
  use constants, only: neg_huge

  implicit none

  integer, intent(in) :: np                                             ! droplet index in "pdata"
  integer, intent(inout) :: neighbor_idx                                ! idx-th droplet to enter a certain neighbor 
  integer, intent(inout) :: leave_idx                                   ! idx-th droplet leaves the current MPI region
  real, intent(inout), dimension(nparcelsLocal,npvals) :: pdata         ! the "pdata" with droplet information
  real, intent(out), dimension(:,:) :: droplet_buffer                   ! array to save the information of leaving droplet

  droplet_buffer(neighbor_idx,:) = pdata(np,:)
  neighbor_idx = neighbor_idx + 1
  pdata(np,:) = neg_huge
  leave_idx = leave_idx + 1

  end subroutine record_leaving_droplet

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  subroutine update_new_droplet ( holes_ind, hole_idx, new_count, &
                                  droplet_buffer, pdata )

  ! this subroutine is used to update the new droplet information
  ! to the "pdata" array on a particular MPI rank after the MPI communication

  use input, only : npvals,nparcelsLocal

  implicit none

  integer, intent(in), dimension(:) :: holes_ind                        ! array to store the location index of "holes" in "pdata"
  integer, intent(inout) :: hole_idx                                    ! hole_idx-th "hole" index in "pdata"
  integer, intent(in) :: new_count                                      ! number of new droplets entering the current
                                                                        ! MPI region from a particular neighbor
  real, intent(in), dimension(:,:) :: droplet_buffer                    ! array to save the information of new droplets
  real, intent(inout), dimension(nparcelsLocal,npvals) :: pdata         ! the "pdata" with droplet information

  integer :: i

  do i = 1, new_count
     pdata(holes_ind(hole_idx),:) = droplet_buffer(i,:)
     hole_idx = hole_idx + 1
  end do

  end subroutine update_new_droplet

#endif

end module comm_droplet_module
