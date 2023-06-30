#!/bin/bash
#PBS -N cm1
#PBS -l select=1:ncpus=2:mpiprocs=2:ngpus=2:mem=100gb
#PBS -l walltime=00:40:00
#PBS -j oe
#PBS -q main
#PBS -A UNDM0006

module purge
module load ncarenv nvhpc cuda cray-mpich ncarcompilers

export MPICH_GPU_SUPPORT_ENABLED=1
mpiexec -n 2 -ppn 2 get_local_rank ./gpu.exe --namelist namelist_ASD.input >& gust.mpi.openacc.namelist.ASD.log
