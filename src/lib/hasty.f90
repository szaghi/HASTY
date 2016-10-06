!< HASTY, HASh Table fortran container exploiting coarraY.
module hasty
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY, HASh Table fortran container exploiting coarraY.
!-----------------------------------------------------------------------------------------------------------------------------------
use hasty_content_adt
use hasty_key_adt
use hasty_hash_table
use hasty_list
use hasty_list_node
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: content_adt
public :: key_adt, are_keys_equal, is_key_allowed, str_key
public :: hash_table
public :: list, key_iterator_interface, len
public :: list_node
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule hasty
