!< HASTY class of hash table.
module hasty_hash_table
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY class of hash table.
!-----------------------------------------------------------------------------------------------------------------------------------
use hasty_key_adt
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
  integer(I4P)            :: buckets_number=0_I4P   !< Number of buckets used.
  integer(I4P)            :: length=0_I4P           !< Length of the hash table, number of nodes actually stored.
  logical                 :: is_initialized=.false. !< Initialization status.
  contains
    ! public methods
    procedure, pass(self) :: add_pointer !< Add a node pointer to the hash table.
    procedure, pass(self) :: add_clone   !< Add a node to the hash table cloning contents (non pointer add).
    procedure, pass(self) :: get         !< Return a pointer to a node's container in the hash table.
    procedure, pass(self) :: destroy     !< Destroy the hash table.
    procedure, pass(self) :: hash        !< Hash the key.
    procedure, pass(self) :: initialize  !< Initialize the table.
    ! private methods
    ! finalizer
    final :: finalize !< Finalize the list.
endtype hash_table
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public non TBP

  ! public methods
  subroutine add_pointer(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a node pointer to the hash table.
  !<
  !< @note If a node with the same key is already in the list, it is removed and the new one will replace it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout)       :: self    !< The hash table.
  class(*),          intent(in)          :: key     !< The key ID.
  class(*),          intent(in), pointer :: content !< The content.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_key_allowed(key)) error stop 'error: key type not supported'

  if (.not.self%is_initialized) call self%initialize ! initialize the table with default options

  call self%bucket(self%hash(key=key))%add_pointer(key=key, content=content)

  ! TODO update table length
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_pointer

  subroutine add_clone(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a node to the hash table cloning content (non pointer add).
  !<
  !< @note If a node with the same key is already in the list, it is removed and the new one will replace it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout) :: self    !< The hash table.
  class(*),          intent(in)    :: key     !< The key ID.
  class(*),          intent(in)    :: content !< The content.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_key_allowed(key)) error stop 'error: key type not supported'

  if (.not.self%is_initialized) call self%initialize ! initialize the table with default options

  call self%bucket(self%hash(key=key))%add_clone(key=key, content=content)

  ! TODO update table length
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_clone

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

  function get(self, key) result(content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a pointer to a node's content in the hash table.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in) :: self    !< The hash table.
  class(*),          intent(in) :: key     !< The key ID.
  class(*), pointer             :: content !< content pointer of the queried node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  content => null()
  if (self%is_initialized) then
    content => self%bucket(self%hash(key=key))%get(key=key)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get

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
  if (self%is_initialized) bucket = hash_key(key=key, buckets_number=self%buckets_number)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction hash

  subroutine initialize(self, buckets_number)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize the hash table.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout)        :: self           !< The hash table.
  integer(I4P),      intent(in), optional :: buckets_number !< Number of buckets for initialize the hash table.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%buckets_number = HT_BUCKETS_NUMBER_DEF ; if (present(buckets_number)) self%buckets_number = buckets_number
  call self%destroy
  allocate(self%bucket(1:self%buckets_number))
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
