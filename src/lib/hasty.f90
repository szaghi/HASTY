!< HASTY, HASh Table fortran container exploiting coarraY.
module hasty
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY, HASh Table fortran container exploiting coarraY.
!-----------------------------------------------------------------------------------------------------------------------------------
use hasty_content_adt
use hasty_key_base
use hasty_key_morton
use hasty_hash_table
use hasty_dictionary
use hasty_dictionary_node
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: content_adt
! public :: key_base, are_keys_equal, is_key_allowed, str_key
public :: key_base, is_key_allowed
public :: key_morton
public :: hash_table
public :: dictionary, key_iterator_interface
public :: dictionary_node
public :: len
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule hasty
