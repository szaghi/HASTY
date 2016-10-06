!< HASTY abstract **key** class.
module hasty_key_adt
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY abstract **key** class.
!<
!< @note This module provides also helper procedures for:
!<
!<+ comparing two Unlimited Polymorphic Classes for the keys comparison necessities;
!<+ hasing a key;
!<+ typeguard for allowed types of keys;
!<+ key stringify;
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
! helper procedures
public :: are_keys_equal
public :: hash_key
public :: is_key_allowed
public :: str_key
! abstract **key** class
public :: key_adt
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: key_adt
  !< Abstract **key** class to identify a node of list.
  !<
  !< It can be extended to use any data as a key.
  contains
    ! public deferred methods
    procedure(stringify_interface), pass(self), deferred :: stringify !< Return a string representation of the key.
    ! private deferred methods
    procedure(is_equal_interface), pass(lhs), private, deferred :: is_equal !< Implement `==` operator.
    ! public generics
    generic, public :: operator(==) => is_equal !< Overloading `==` operator.
endtype key_adt

! private interfaces
abstract interface
  !< Return a string representation of the key.
  pure function stringify_interface(self) result(str_key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of the key.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: key_adt
  class(key_adt), intent(in)    :: self !< The key.
  character(len=:), allocatable :: str_key !< The key stringified.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction stringify_interface
endinterface

abstract interface
  !< Implement `==` operator.
  elemental logical function is_equal_interface(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Implement `==` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: key_adt
  class(key_adt), intent(in) :: lhs !< Left hand side.
  class(key_adt), intent(in) :: rhs !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal_interface
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public non TBP
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

  elemental function hash_key(key, buckets_number) result(bucket)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Hash the key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*),     intent(in) :: key            !< Key to hash.
  integer(I4P), intent(in) :: buckets_number !< Buckets number.
  integer(I4P)             :: bucket         !< Bucket index corresponding to the key.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  bucket = 0
  select type(key)
  type is(integer(I1P))
    bucket = mod(int(key, I4P), buckets_number)
  type is(integer(I2P))
    bucket = mod(int(key, I4P), buckets_number)
  type is(integer(I4P))
    bucket = mod(int(key, I4P), buckets_number)
  type is(integer(I8P))
    bucket = mod(int(key, I4P), buckets_number)
  type is(character(len=*))
    ! TODO implement
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction hash_key

  elemental function is_key_allowed(key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Typeguard for used key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*), intent(in) :: key            !< The key ID.
  logical              :: is_key_allowed !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_key_allowed = .false.
  select type(key)
  type is(integer(I1P))
    is_key_allowed = .true.
  type is(integer(I2P))
    is_key_allowed = .true.
  type is(integer(I4P))
    is_key_allowed = .true.
  type is(integer(I8P))
    is_key_allowed = .true.
  type is(character(len=*))
    if (len_trim(key)>=1) is_key_allowed = .true.
  class is (key_adt)
    is_key_allowed = .true.
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_key_allowed

  pure function str_key(key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the key as character string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*), intent(in)          :: key     !< The key.
  character(len=:), allocatable :: str_key !< The key stringified.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  str_key = ''
  select type(key)
  type is(integer(I1P))
    str_key = trim(str(key))
  type is(integer(I2P))
    str_key = trim(str(key))
  type is(integer(I4P))
    str_key = trim(str(key))
  type is(integer(I8P))
    str_key = trim(str(key))
  type is(character(len=*))
    str_key = key
  class is (key_adt)
    str_key = key%stringify()
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction str_key
endmodule hasty_key_adt
