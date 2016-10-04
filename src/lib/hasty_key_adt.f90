!< HASTY abstract **key** class.
module hasty_key_adt
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY abstract **key** class.
!<
!< @note This module provides also an helper function for comparing two Unlimited Polymorphic Classes
!< for the keys comparison necessities.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: are_keys_equal
public :: key_adt
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: key_adt
  !< Abstract **key** class to identify a node of list.
  !<
  !< It can be extended to use any data as a key.
  contains
    ! private deferred methods
    procedure(is_equal_interface), pass(lhs), private, deferred :: is_equal !< Implement `==` operator.
    ! public generics
    generic, public :: operator(==) => is_equal !< Overloading `==` operator.
endtype key_adt

abstract interface
  !< Implement `==` operator.
  elemental logical function is_equal_interface(lhs, rhs)
  !< Implement `==` operator.
  import :: key_adt
  class(key_adt), intent(in) :: lhs !< Left hand side.
  class(key_adt), intent(in) :: rhs !< Rigth hand side.
  endfunction is_equal_interface
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  pure function are_keys_equal(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return true if the two keys are equal.
  !<
  !< @note Supported keys are:
  !<+ integer (any kinds);
  !<+ character (case sensitive);
  !<+ [[key_adt]].
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*), intent(in) :: lhs            !< Left hand side.
  class(*), intent(in) :: rhs            !< Right hand side.
  logical              :: are_keys_equal !< Test result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  are_keys_equal = .false.
  if (same_type_as(lhs, rhs)) then
    select type(lhs)
    class is(key_adt)
      select type(rhs)
      class is(key_adt)
        are_keys_equal = lhs==rhs
      endselect
    type is(integer(I1P))
      select type(rhs)
      type is(integer(I1P))
        are_keys_equal = lhs==rhs
      endselect
    type is(integer(I2P))
      select type(rhs)
      type is(integer(I2P))
        are_keys_equal = lhs==rhs
      endselect
    type is(integer(I4P))
      select type(rhs)
      type is(integer(I4P))
        are_keys_equal = lhs==rhs
      endselect
    type is(integer(I8P))
      select type(rhs)
      type is(integer(I8P))
        are_keys_equal = lhs==rhs
      endselect
    type is(character(len=*))
      select type(rhs)
      type is(character(len=*))
        are_keys_equal = lhs==rhs
      endselect
    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction are_keys_equal
endmodule hasty_key_adt
