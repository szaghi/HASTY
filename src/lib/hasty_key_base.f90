!< HASTY **key** class.
module hasty_key_base
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY **key** class.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: is_key_allowed
public :: key_base
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: key_base
  !< **Key** class to identify a node.
  !<
  !< It can be extended by user.
  integer(I8P),     allocatable :: id_       !< Unique key id.
  character(len=:), allocatable :: char_key_ !< Store character key.
  contains
    ! public methods
    procedure, pass(self) :: destroy        !< Destroy the key.
    procedure, pass(self) :: hash           !< Hash the key.
    procedure, pass(self) :: id             !< Return the id.
    procedure, nopass     :: is_key_allowed !< Typeguard for used key.
    procedure, pass(self) :: set            !< Set the key.
    procedure, pass(self) :: stringify      !< Return a string representation of the key.
    ! public generics
    generic, public :: operator(==) => is_equal !< Overloading `==` operator.
    ! private methods
    procedure, pass(lhs),  private :: is_equal    !< Implement `==` operator.
    procedure, nopass,     private :: hash_string !< Hash a string.
endtype key_base

interface key_base
  module procedure creator
endinterface key_base
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! private non TBP
  function creator(key, buckets_number) result(key_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return an  instance of [[key_base]]
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*),     intent(in)           :: key            !< The key value.
  integer(I4P), intent(in), optional :: buckets_number !< Buckets number.
  type(key_base)                     :: key_           !< Instance of [[key_base]].
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call key_%set(key=key, buckets_number=buckets_number)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator

  ! public methods
  elemental subroutine destroy(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(key_base), intent(inout) :: self !< The key.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%id_)) deallocate(self%id_)
  if (allocated(self%char_key_)) deallocate(self%char_key_)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy

  elemental function hash(self, buckets_number) result(bucket)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Hash the key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(key_base), intent(in) :: self           !< The key.
  integer(I4P),    intent(in) :: buckets_number !< Buckets number.
  integer(I4P)                :: bucket         !< Bucket index corresponding to the key.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  bucket = 0
  if (allocated(self%id_)) bucket = mod(int(self%id_, I4P), buckets_number)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction hash

  elemental function id(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the id.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(key_base), intent(in) :: self !< The key.
  integer(I8P)                :: id   !< Unique key id.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  id = 0
  if (allocated(self%id_)) id = self%id_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction id

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
  class is(key_base)
    is_key_allowed = .true.
  type is(integer(I1P))
    if (key>0) is_key_allowed = .true.
  type is(integer(I2P))
    if (key>0) is_key_allowed = .true.
  type is(integer(I4P))
    if (key>0) is_key_allowed = .true.
  type is(integer(I8P))
    if (key>0) is_key_allowed = .true.
  type is(character(len=*))
    is_key_allowed = .true.
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_key_allowed

  subroutine set(self, key, buckets_number)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(key_base), intent(inout)        :: self           !< The key.
  class(*),        intent(in)           :: key            !< The key value.
  integer(I4P),    intent(in), optional :: buckets_number !< Buckets number.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_key_allowed(key)) then
    call self%destroy
    allocate(self%id_)
    select type(key)
    class is (key_base)
      self%id_ = key%id_
    type is(integer(I1P))
      self%id_ = int(key, kind=I8P)
    type is(integer(I2P))
      self%id_ = int(key, kind=I8P)
    type is(integer(I4P))
      self%id_ = int(key, kind=I8P)
    type is(integer(I8P))
      self%id_ = int(key, kind=I8P)
    type is(character(len=*))
      if (present(buckets_number)) then
        self%id_ = self%hash_string(string=key, buckets_number=buckets_number)
      else
        self%id_ = self%hash_string(string=key, buckets_number=9973_I4P)
      endif
      self%char_key_ = key
    endselect
  else
    error stop 'error: key type not supported'
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  pure function stringify(self) result(str_key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of the key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(key_base), intent(in)   :: self    !< The key.
  character(len=:), allocatable :: str_key !< The key stringified.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  str_key = ''
  if (allocated(self%char_key_)) then
    str_key = self%char_key_
  else
    if (allocated(self%id_)) str_key = trim(str(self%id_))
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction stringify

  ! private methods
  elemental function hash_string(string, buckets_number) result(bucket)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Hash a string.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in) :: string         !< The string.
  integer(I4P), intent(in) :: buckets_number !< Buckets number.
  integer(I8P)             :: bucket         !< Bucket index corresponding to the string.
  integer(I4P)             :: c              !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  bucket = int(buckets_number, I8P)
  do c=1, len(string)
    bucket = (ishft(bucket, 5) + bucket) + ichar(string(c:c), kind=I8P)
  enddo
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction hash_string

  elemental logical function is_equal(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Implement `==` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(key_base), intent(in) :: lhs !< Left hand side.
  class(*),        intent(in) :: rhs !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_equal = .false.
  if (allocated(lhs%id_)) then
    select type(rhs)
    class is(key_base)
      if (allocated(rhs%id_)) is_equal = lhs%id_==rhs%id_
    type is(integer(I1P))
      is_equal = lhs%id_==rhs
    type is(integer(I2P))
      is_equal = lhs%id_==rhs
    type is(integer(I4P))
      is_equal = lhs%id_==rhs
    type is(integer(I8P))
      is_equal = lhs%id_==rhs
    type is(character(len=*))
      if (allocated(lhs%char_key_)) is_equal = lhs%char_key_==rhs
    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal
endmodule hasty_key_base
