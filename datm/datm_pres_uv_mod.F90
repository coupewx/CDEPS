module datm_pres_uv_mod

  use ESMF             , only : ESMF_SUCCESS, ESMF_State, ESMF_StateItem_Flag
  use shr_kind_mod     , only : r8=>shr_kind_r8
  use shr_log_mod      , only : shr_log_error
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private

  public :: datm_pres_uv_advertise
  public :: datm_pres_uv_init_pointers
  public :: datm_pres_uv_advance

  ! export state data
  real(r8), pointer :: Sa_ephyto1UVA(:,:) => null()
  real(r8), pointer :: Sa_ephyto1UVB(:,:) => null()
  real(r8), pointer :: Sa_ephyto1UVC(:,:) => null()
  real(r8), pointer :: Sa_ephyto5UVA(:,:) => null()
  real(r8), pointer :: Sa_ephyto5UVB(:,:) => null()
  real(r8), pointer :: Sa_ephyto5UVC(:,:) => null()
  real(r8), pointer :: Sa_ephyto7UVA(:,:) => null()
  real(r8), pointer :: Sa_ephyto7UVB(:,:) => null()
  real(r8), pointer :: Sa_ephyto7UVC(:,:) => null()

  ! stream data
  real(r8), pointer :: strm_Sa_ephyto1UVA(:,:) => null() ! stream cmip7 uv data
  real(r8), pointer :: strm_Sa_ephyto1UVB(:,:) => null() ! stream cmip7 uv data
  real(r8), pointer :: strm_Sa_ephyto1UVC(:,:) => null() ! stream cmip7 uv data
  real(r8), pointer :: strm_Sa_ephyto5UVA(:,:) => null() ! stream cmip7 uv data
  real(r8), pointer :: strm_Sa_ephyto5UVB(:,:) => null() ! stream cmip7 uv data
  real(r8), pointer :: strm_Sa_ephyto5UVC(:,:) => null() ! stream cmip7 uv data
  real(r8), pointer :: strm_Sa_ephyto7UVA(:,:) => null() ! stream cmip7 uv data
  real(r8), pointer :: strm_Sa_ephyto7UVB(:,:) => null() ! stream cmip7 uv data
  real(r8), pointer :: strm_Sa_ephyto7UVC(:,:) => null() ! stream cmip7 uv data

  logical :: use_cmip7_uv

  character(len=*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_pres_uv_advertise(fldsExport)

    ! input/output variables
    type(fldlist_type) , pointer :: fldsexport
    !----------------------------------------------------------

    call dshr_fldList_add(fldsExport, 'Sa_ephyto1UVA', ungridded_lbound=1, ungridded_ubound=2) ! still need to add uvb, uvc, etc
    call dshr_fldList_add(fldsExport, 'Sa_ephyto1UVB', ungridded_lbound=1, ungridded_ubound=2) 
    call dshr_fldList_add(fldsExport, 'Sa_ephyto1UVC', ungridded_lbound=1, ungridded_ubound=2) 
    call dshr_fldList_add(fldsExport, 'Sa_ephyto5UVA', ungridded_lbound=1, ungridded_ubound=2)  
    call dshr_fldList_add(fldsExport, 'Sa_ephyto5UVB', ungridded_lbound=1, ungridded_ubound=2)  
    call dshr_fldList_add(fldsExport, 'Sa_ephyto5UVC', ungridded_lbound=1, ungridded_ubound=2)  
    call dshr_fldList_add(fldsExport, 'Sa_ephyto7UVA', ungridded_lbound=1, ungridded_ubound=2)  
    call dshr_fldList_add(fldsExport, 'Sa_ephyto7UVB', ungridded_lbound=1, ungridded_ubound=2)  
    call dshr_fldList_add(fldsExport, 'Sa_ephyto7UVC', ungridded_lbound=1, ungridded_ubound=2)  


  end subroutine datm_pres_uv_advertise

  !===============================================================================
  subroutine datm_pres_uv_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_uv_init_pointers): '
    !----------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Get pointer to export state
    call dshr_state_getfldptr(exportState, 'Sa_ephyto1UVA', fldptr2=Sa_ephyto1UVA, rc=rc) ! need to change
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Get pointer to stream data that will be used below - if the
    ! following stream fields are not in any sdat streams, then a null value is returned

    ! cmip7 uv forcing
    call shr_strdata_get_stream_pointer(sdat, 'Sa_ephyto1UVA', strm_Sa_ephyto1UVA, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
  

    ! determine module variable
    use_cmip7_uv = .true.

  end subroutine datm_pres_uv_init_pointers

  !===============================================================================
  subroutine datm_pres_uv_advance()

    if (use_cmip7_uv) then
       ! assume data is unitless
       Sa_ephyto1UVA(:,:) = strm_Sa_ephyto1UVA(:,:)
       Sa_ephyto1UVB(:,:) = strm_Sa_ephyto1UVB(:,:)
       Sa_ephyto1UVC(:,:) = strm_Sa_ephyto1UVC(:,:)
       Sa_ephyto5UVA(:,:) = strm_Sa_ephyto5UVA(:,:)
       Sa_ephyto5UVB(:,:) = strm_Sa_ephyto5UVB(:,:)
       Sa_ephyto5UVC(:,:) = strm_Sa_ephyto5UVC(:,:)
       Sa_ephyto7UVA(:,:) = strm_Sa_ephyto7UVA(:,:)
       Sa_ephyto7UVB(:,:) = strm_Sa_ephyto7UVB(:,:)
       Sa_ephyto7UVC(:,:) = strm_Sa_ephyto7UVC(:,:)

    !else
    !   ! convert ndep flux to units of kgN/m2/s (input is in gN/m2/s)
    !   Faxa_ndep(1,:) = strm_Faxa_ndep_nhx(:) / 1000._r8
    !   Faxa_ndep(2,:) = strm_Faxa_ndep_noy(:) / 1000._r8
    end if

  end subroutine datm_pres_uv_advance

end module datm_pres_uv_mod
