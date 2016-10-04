!< HASTY, HASh Table fortran container exploiting coarraY.
module hasty
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY, HASh Table fortran container exploiting coarraY.
!-----------------------------------------------------------------------------------------------------------------------------------
use hasty_container_adt
! use hasty_container_adt_polymorphic
! use hasty_container_integer32
use hasty_key_adt
! use hasty_key_string
use hasty_list
use hasty_list_node
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: container_adt
! public :: container_adt_polymorphic
! public :: container_integer32
public :: key_adt
! public :: key_string
public :: len
public :: list
public :: list_node
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule hasty
