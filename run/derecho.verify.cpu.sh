#!/bin/bash -l
#PBS -N cm1
#PBS -l select=1:ncpus=2:mpiprocs=2:ompthreads=1:mem=235GB
#PBS -l walltime=01:30:00
#PBS -j oe
#PBS -q main
#PBS -A UNDM0006

module --force purge
module load ncarenv/23.06 intel/2023.0.0  ncarcompilers/1.0.0 cray-mpich/8.1.25
mpiexec -n 2 -ppn 2 ./cpu.intel.exe --namelist namelist_ASD.verify >& derecho.mpi.ifort.namelist.ASD.log

#module --force purge
#module load ncarenv/23.06 nvhpc/23.5 cray-mpich/8.1.25 ncarcompilers/1.0.0
#mpiexec -n 2 -ppn 2 ./cpu.nvhpc.exe --namelist namelist_ASD.verify >& derecho.mpi.nvhpc.namelist.ASD.log
