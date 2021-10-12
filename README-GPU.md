# Running CM1 on GPU

This version of CM1 includes an experimental port of CM1 to the GPU using the OpenACC langauge..  While large sections of CM1 have been ported to the GPU, there are still multiple sections that have not yet been ported.  Sections not ported will issue a fatal error message indicating the lack of a complete port. Additionally not all configurations of CM1 have been official verified for correct exection.  Use of the GPU port is therefore a use at your own risk configuration. 

We next describe directions for compiling and executing CM1 on GPUs within the Casper system.  CM1 is build from the 'src' directory.  Several targets have been added to the Makefile and can be located by looking for the following two lines 

```
single processor + GPU, Portland Group compiler
multiple processors + GPU,  distributed memory (MPI), Portland Group compiler
```

Also note that there are two sets of flags for each configuration PERF and DEBUG string that correspond to a performance enabled and debugging setting respective.  

## Single GPU job 

In order to run CM1 on a single GPU, uncomment the correspoding flags and configure your environment by performing the following module commands:

```
    module purge
    module load ncarenv/1.3 nvhpc/21.7 cuda/11.4 ncarcompilers/0.5.0
```

In the 'src' directory, You can then build the executable using make:

```
    make -j 3
```

Once the 'cm1.exe' command has been built, you will need to request a single GPU from the scheduler:

    execcasper -l select=1:ncpus=1:ngpus=1 -l gpu_type=v100 -l walltime=01:00:00

This will allocated an interactive job where CM1 can be executed simplying by executing the command in the 'run' directory.

```
./cm1.exe
```

## Multi GPU job 

In order to run CM1 on multiple GPUs, uncomment the correspoding flags and configure your environment by performing the following module commands:

```
    module purge
    module load ncarenv/1.3 nvhpc/21.7 cuda/11.4 ncarcompilers/0.5.0 openmpi
```

You can then build the executable using make in the 'src' directory:

```
    make -j 3
```

Once the 'cm1.exe' command has been built, you will need to request a multiple GPUs from the scheduler:

```
    execcasper -l select=1:ncpus=2:mpiprocs=2:ngpus=2 -l gpu_type=v100 -l walltime=1:00:00
```

It is also pssible submit a job into the gpudev queue using the following command:
   
```
qsub -I -q gpudev -A <PROJECT_ID> -l select=1:ncpus=2:mpiprocs=2:ngpus=2 -l gpu_type=v100 -l walltime=00:30:00
```

This will allocated an interactive job.  You will need to set several environment variables before you execute the job on multiple CPU.  

```
    setenv LD_LIBRARY_PATH ${NCAR_ROOT_CUDA}/lib64:${LD_LIBRARY_PATH}
    setenv UCX_TLS rc,sm,cuda_copy,cuda_ipc
    setenv OMPI_MCA_pml ucx
    setenv OMPI_MCA_btl self,vader,tcp,smcuda
    setenv UCX_MEMTYPE_CACHE n
    setenv UCX_RNDV_SCHEME put_zcopy
    setenv UCX_RNDV_THRESH 2
```

Once your environment is set simply execute the code using mpirun from the 'run' directory. 

```
    mpirun -np 2 ./cm1.exe
```

