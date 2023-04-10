#!/bin/bash
#PBS -N cm1
#PBS -l select=2:ncpus=4:mpiprocs=4:ngpus=4:mem=100gb
#PBS -l walltime=06:00:00
#PBS -j oe
#PBS -q main@gusched01
#PBS -A UNDM0006

module purge
module load nvhpc cuda cray-mpich

export MPICH_GPU_SUPPORT_ENABLED=1
export MPICH_GPU_MANAGED_MEMORY_SUPPORT_ENABLED=1
# mpiexec -n 8 -ppn 4 get_local_rank ./cm1.exe --namelist namelist.small.gpu >& gust.ngpu8.small.512x512.log
mpiexec -n 8 -ppn 4 get_local_rank ./cm1.exe --namelist namelist.figureE.gpu >& gust.ngpu8.figureE.512x512.log
