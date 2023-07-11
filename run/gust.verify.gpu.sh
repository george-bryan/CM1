#!/bin/bash
#PBS -N cm1
#PBS -l select=1:ncpus=2:mpiprocs=2:ngpus=2:mem=100gb
#PBS -l walltime=02:00:00
#PBS -j oe
#PBS -q main
#PBS -A UNDM0006

module purge
module load ncarenv nvhpc cuda cray-mpich ncarcompilers


export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/glade/u/apps/common/22.12/spack/opt/spack/nvhpc/23.3/Linux_x86_64/23.3/math_libs/12.0/targets/x86_64-linux/lib
export MPICH_GPU_SUPPORT_ENABLED=1
mpiexec -n 2 -ppn 2 get_local_rank ./gpu.exe --namelist namelist_ASD.verify >& gust.mpi.openacc.namelist.ASD.log
#mpiexec -n 2 -ppn 2 get_local_rank ./gpu.exe --namelist namelist_ASD.100M >& gust.mpi.openacc.namelist.ASD.log
