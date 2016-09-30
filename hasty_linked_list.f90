!< HASTY class of generic (single) linked list.
module hasty_linked_list
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY class of generic (single) linked list.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: linked_list
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type:: linked_list
  !< Node of the generic (single) linked list and the list itself.
  private
  integer(I8P), allocatable  :: ID             !< ID (unique) of the current node.
  class(*),          pointer :: d    => null() !< Node data.
  type(linked_list), pointer :: next => null() !< Pointer to the next node of the list.
  contains
    procedure :: dat
    procedure :: destroy
    procedure :: node
    procedure :: put       
    procedure :: get  
    procedure :: del 
    procedure :: assign_
endtype linked_list
!-----------------------------------------------------------------------------------------------------------------------------------
contains
endmodule hasty_linked_list
