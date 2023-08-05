#!/bin/bash
#PBS -N cm1
#PBS -l select=1:ncpus=2:mpiprocs=2:ngpus=2:mem=235gb
#PBS -l walltime=00:20:00
#PBS -j oe
#PBS -q main
#PBS -A UNDM0006

module purge
module load ncarenv nvhpc cuda cray-mpich ncarcompilers cutensor

export MPICH_GPU_SUPPORT_ENABLED=1
mpiexec -n 2 -ppn 2 get_local_rank ./gpu.exe --namelist namelist_ASD.verify >& derecho.mpi.openacc.namelist.ASD.log
#mpiexec -n 16 -ppn 4 get_local_rank ./gpu.exe --namelist namelist_ASD.restA >& derecho.mpi.openacc.namelist.ASD.restA.log
#mpiexec -n 16 -ppn 4 get_local_rank ./gpu.exe --namelist namelist_ASD.restB >& derecho.mpi.openacc.namelist.ASD.restB.log
#mpiexec -n 16 -ppn 4 get_local_rank ./gpu.exe --namelist namelist_ASD.restC >& derecho.mpi.openacc.namelist.ASD.restC.log
