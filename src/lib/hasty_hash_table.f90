!< HASTY class of hash table.
module hasty_hash_table
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY class of hash table.
!-----------------------------------------------------------------------------------------------------------------------------------
use hasty_list
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: hash_table
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
integer(I4P), parameter :: HT_BUCKETS_NUMBER_DEF = 9973_I4P !< Default number of buckets of hash table.

type :: hash_table
  !< **Hash table** class to storage any contents by means of abstract list buckets.
  type(list), allocatable :: bucket(:)              !< Hash table buckets.
  integer(I4P)            :: length=0_I4P           !< Length of the hash table, number of nodes actually stored.
  logical                 :: is_initialized=.false. !< Initialization status.
  contains
    ! public methods
    procedure, pass(self) :: destroy    !< Destroy the hash table.
    procedure, pass(self) :: hash       !< Hash the key.
    procedure, pass(self) :: initialize !< Initialize the table.
    ! private methods
    ! finalizer
    final :: finalize !< Finalize the list.
endtype hash_table
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public non TBP

  ! public methods
  subroutine destroy(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the hash table.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout) :: self !< The hash table.
  integer(I4P)                     :: b    !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%bucket)) then
    do b=1, size(self%bucket, dim=1)
      call self%bucket(b)%destroy
    enddo
    deallocate(self%bucket)
  endif
  self%length=0_I4P
  self%is_initialized=.false.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy

  elemental function hash(self, key) result(bucket)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Hash the key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in) :: self   !< The hash table.
  class(*),          intent(in) :: key    !< Key to hash.
  integer(I4P)                  :: bucket !< Bucket index corresponding to the key.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  bucket = 0
  if (self%is_initialized) then
    select type(key)
    type is(integer(I1P))
      bucket = mod(int(key, I4P), self%length)
    type is(integer(I2P))
      bucket = mod(int(key, I4P), self%length)
    type is(integer(I4P))
      bucket = mod(int(key, I4P), self%length)
    type is(integer(I8P))
      bucket = mod(int(key, I4P), self%length)
    endselect
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction hash

  subroutine initialize(self, buckets_number)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize the hash table.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout)        :: self            !< The hash table.
  integer(I4P),      intent(in), optional :: buckets_number  !< Number of buckets for initialize the hash table.
  integer(I4P)                            :: buckets_number_ !< Number of buckets for initialize the hash table, local variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  buckets_number_ = HT_BUCKETS_NUMBER_DEF ; if (present(buckets_number)) buckets_number_ = buckets_number
  call self%destroy
  allocate(self%bucket(1:buckets_number_))
  self%is_initialized=.true.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize

  ! private methods

  ! finalizer
  subroutine finalize(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Finalize the hash table.
  !<
  !< A wrapper for [[hash_table:destroy]]
  !---------------------------------------------------------------------------------------------------------------------------------
  type(hash_table), intent(inout) :: self !< The hash table.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize
endmodule hasty_hash_table
