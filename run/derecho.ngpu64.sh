#!/bin/bash
#PBS -N cm1
#PBS -l select=16:ncpus=4:mpiprocs=4:ngpus=4:mem=200gb
#PBS -l walltime=01:00:00
#PBS -j oe
#PBS -q main
#PBS -A NTDD0004

module purge
module load ncarenv/23.06 nvhpc/23.5 cuda/11.7.1 ncarcompilers/1.0.0 cray-mpich/8.1.25 cutensor

export MPICH_GPU_SUPPORT_ENABLED=1
export MPICH_GPU_MANAGED_MEMORY_SUPPORT_ENABLED=1
# mpiexec -n 8 -ppn 4 get_local_rank ./cm1.exe --namelist namelist.small.gpu >& derecho.ngpu8.small.512x512.log
# mpiexec -n 8 -ppn 4 get_local_rank ./gpu.exe --namelist namelist.restC >& derecho.ngpu8.restC.log
# mpiexec -n 8 -ppn 4 get_local_rank ./nsys_wrap ./gpu.exe --namelist namelist.figureE.gpu >& derecho.ngpu8.figureEg.log

# mpiexec -n 64 -ppn 4 get_local_rank ./gpu.exe --namelist namelist.scale1k.gpu >& derecho.ngpu64.scale.1024.log
mpiexec -n 64 -ppn 4 get_local_rank ./gpu.exe --namelist namelist.scale1k.gpu >& derecho.ngpu64.scale.1024.log

#mpiexec -n 8 -ppn 4 get_local_rank ./gpu.exe --namelist namelist.figureE.gpu >& derecho.ngpu8.figureEc.log
#mpiexec -n 8 -ppn 4 get_local_rank ./gpu.exe --namelist namelist.figureE.gpu >& derecho.ngpu8.figureEd.log
#mpiexec -n 8 -ppn 4 get_local_rank ./gpu.exe --namelist namelist.figureE.gpu >& derecho.ngpu8.figureEe.log
#mpiexec -n 8 -ppn 4 get_local_rank ./gpu.exe --namelist namelist.figureE.gpu >& derecho.ngpu8.figureEf.log
