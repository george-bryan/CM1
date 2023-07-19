#!/bin/bash
#PBS -N cm1
#PBS -l select=1:ncpus=4:mpiprocs=4:ngpus=4:mem=200gb
#PBS -l walltime=02:00:00
#PBS -j oe
#PBS -q main
#PBS -A UNDM0006

module purge
module load ncarenv nvhpc cuda cray-mpich ncarcompilers cutensor


export MPICH_GPU_SUPPORT_ENABLED=1
mpiexec -n 4 -ppn 4 get_local_rank ./gpu.exe --namelist namelist_ASD.verify >& derecho.mpi.openacc.namelist.ASD.log
# mpiexec -n 4 -ppn 4 get_local_rank ./gpu.exe --namelist namelist_ASD.verify.v2 >& derecho.mpi.openacc.namelist.ASDv2.log
# mpiexec -n 4 -ppn 4 get_local_rank ./gpu.exe --namelist namelist_ASD.verify.v2.buggy >& derecho.mpi.openacc.namelist.ASDv2bug.log
