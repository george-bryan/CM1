  MODULE writeout_nc_module

  implicit none

  private
  public :: netcdf_prelim

  CONTAINS

!-------------------------------------------------------------
!
!  This subroutine writes data in NetCDF files.
!
!  Code originally written by Daniel Kirshbaum
!  Code converted to netcdf4 (f90) by George Bryan, May 2013
!  Code last modified by George Bryan, 130910
!
!-------------------------------------------------------------


      subroutine netcdf_prelim(rtime,nwrite,fnum,ncid,time_index,qname,                      &
                               name_output,desc_output,unit_output,grid_output,cmpr_output,  &
                               xh,xf,yh,yf,xfref,yfref,sigma,sigmaf,zs,zh,zf,                &
                               d2d,ds,du,dv,                                                 &
                               dumz,dumx,dumy)
      use input
      use constants
      implicit none

      real, intent(in) :: rtime
      integer, intent(in) :: nwrite,fnum
      integer, intent(inout) :: ncid,time_index
      character(len=3), dimension(maxq), intent(in) :: qname
      character(len=60), intent(in), dimension(maxvars) :: desc_output
      character(len=40), intent(in), dimension(maxvars) :: name_output,unit_output
      character(len=1),  intent(in), dimension(maxvars) :: grid_output
      logical, intent(in), dimension(maxvars) :: cmpr_output
      real, dimension(ib:ie),   intent(in) :: xh
      real, dimension(ib:ie+1), intent(in) :: xf
      real, dimension(jb:je),   intent(in) :: yh
      real, dimension(jb:je+1), intent(in) :: yf
      real, intent(in), dimension(1-ngxy:nx+ngxy+1) :: xfref
      real, intent(in), dimension(1-ngxy:ny+ngxy+1) :: yfref
      real, dimension(kb:ke)  , intent(in) :: sigma
      real, dimension(kb:ke+1), intent(in) :: sigmaf
      real, dimension(ib:ie,jb:je), intent(in) :: zs
      real, dimension(ib:ie,jb:je,kb:ke),   intent(in) :: zh
      real, dimension(ib:ie,jb:je,kb:ke+1), intent(in) :: zf
      real, dimension(ni,nj) :: d2d
      real, dimension(ni,nj,nk) :: ds
      real, dimension(ni+1,nj,nk) :: du
      real, dimension(ni,nj+1,nk) :: dv
      real, intent(inout), dimension(nz+1) :: dumz
      real, intent(inout), dimension(d2i) :: dumx
      real, intent(inout), dimension(d2j) :: dumy


      if( myid.eq.0 ) print *,'  Leaving netcdf_prelim '

    end subroutine netcdf_prelim


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  END MODULE writeout_nc_module
