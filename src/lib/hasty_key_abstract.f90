!< HASTY abstract **key** class.
module hasty_key_abstract
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY abstract **key** class.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: key_abstract
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: key_abstract
  !< Abstract **key** class to identify a node of list.
  !<
  !< It can be extended to use any data as a key.
  contains
    ! private deferred methods
    procedure(is_equal_interface), private, deferred :: is_equal !< Implement `==` operator.
    ! public generics
    generic, public :: operator(==) => is_equal !< Overloading `==` operator.
endtype key_abstract

abstract interface
  !< Implement `==` operator.
  pure elemental logical function is_equal_interface(lhs, rhs)
  !< Implement `==` operator.
  import :: key_abstract
  class(key_abstract), intent(in) :: lhs !< Left hand side.
  class(key_abstract), intent(in) :: rhs !< Rigth hand side.
  endfunction is_equal_interface
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule hasty_key_abstract
