# CM1
Cloud Model 1 (CM1) is a numerical model for idealized studies of the atmosphere

Note that this particular version of CM1 has been partially ported  to the GPU.  

program cm1
  
    MPI_INIT()
    MPI_COMM_RANK()
    MPMI_COMM_SIZE()
    wenocheck()
    allocate()
    param()
    allocate()
    base()
    init3d()
    setup_stat_vars()
    setup_parcel_vars()
    init_physics()
    init_surface()
    read_restart()
    getset()
    read_lsnudge()
    calccflquick()
    calcksquick()
    getnewdt()
    ...
    do while (mtime.lt.timax)
      ** !$acc copyto(GPU) **
      solve1()               ! GPU resident
      solve2()               ! GPU resident
    !$acc copyto(HOST)
    mp_driver()            !  CPU
    !$acc copyto(GPU)
      solve3()               ! GPU resident
    !$acc copyto(HOST)
    update_adapt_move()    !  CPU
    radiation_driver()     !  CPU
    !$acc copyto(GPU)
      sfc_and_turb()         ! GPU resident
    !$acc copyto(HOST)
    diff2def()             !  CPU
    getnewdt()             !  CPU
    statpack()             !  CPU
    writeout()             !  CPU
    parcel_interp()        !  CPU
    parcel_write()         !  CPU
    domaindiag()           !  CPU
    azimavg()              !  CPU
    writeout_hifrq()       !  CPU
    write_restart()        !  CPU
    mtime = mtime + dbldt
    enddo

