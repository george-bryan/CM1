#!/bin/bash
#PBS -N cm1
#PBS -l select=1:ncpus=4:mpiprocs=4:ngpus=4:mem=100gb
#PBS -l walltime=01:59:00
#PBS -j oe
#PBS -q main
#PBS -A UNDM0006

module purge
module load ncarenv nvhpc cuda cray-mpich ncarcompilers

export MPICH_GPU_SUPPORT_ENABLED=1

mpiexec -n 4 -ppn 4 get_local_rank ./cm1.exe --namelist namelist_ASD.restA >& gust.mpi.openacc.namelist.ASD.restA.log
mpiexec -n 4 -ppn 4 get_local_rank ./cm1.exe --namelist namelist_ASD.restB >& gust.mpi.openacc.namelist.ASD.restB.log
mpiexec -n 4 -ppn 4 get_local_rank ./cm1.exe --namelist namelist_ASD.restC >& gust.mpi.openacc.namelist.ASD.restC.log
