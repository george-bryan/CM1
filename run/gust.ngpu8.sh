#!/bin/bash
#PBS -N cm1
#PBS -l select=2:ncpus=4:mpiprocs=4:ngpus=4:mem=100gb
#PBS -l walltime=06:00:00
#PBS -j oe
#PBS -q main@gusched01
#PBS -A UNDM0006

#export PALS_NRANKS=8
#export PALS_PPN=4
#export PALS_DEPTH=1
#export PALS_CPU_BIND=depth
#export MPICH_GPU_SUPPORT_ENABLED=1
#mpiexec ./gpu.exe --namelist namelist.baseline.dx=2.5m.short-512 >& ngpus=8.dx=2.5m.short-512.log

#export PALS_DEPTH=1
#export PALS_CPU_BIND=depth

module purge
module load nvhpc cuda cray-mpich

export MPICH_GPU_SUPPORT_ENABLED=1
#export CUDA_VISIBLE_DEVICES=$PMI_LOCAL_RANK
export MPICH_GPU_MANAGED_MEMORY_SUPPORT_ENABLED=1
#export NVCOMPILER_ACC_NOTIFY=16
env 
mpiexec -n 8 -ppn 4 ./setdev.sh ./gpu.exe --namelist namelist.figureE.gpu >& gust.ngpu8.figureE.512x512.log
#mpiexec -n 8 -ppn 4 ./setdev.sh ./gpu.exe --namelist namelist.figureD.gpu >& gpu=8.figureD.256.log
