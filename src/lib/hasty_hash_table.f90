!< HASTY class of hash table.
module hasty_hash_table
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY class of hash table.
!-----------------------------------------------------------------------------------------------------------------------------------
use hasty_key_adt
use hasty_dictionary
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: len
public :: hash_table
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
integer(I4P), parameter :: HT_BUCKETS_NUMBER_DEF = 9973_I4P !< Default number of buckets of hash table.

type :: hash_table
  !< **Hash table** class to storage any contents by means of generic dictionary buckets.
  private
  type(dictionary), allocatable :: bucket(:)              !< Hash table buckets.
  integer(I4P)                  :: buckets_number=0_I4P   !< Number of buckets used.
  integer(I4P)                  :: nodes_number=0_I4P     !< Number of nodes actually stored, namely the hash table length.
  logical                       :: is_initialized=.false. !< Initialization status.
  contains
    ! public methods
    procedure, pass(self) :: add_pointer !< Add a node pointer to the hash table.
    procedure, pass(self) :: add_clone   !< Add a node to the hash table cloning contents (non pointer add).
    procedure, pass(self) :: destroy     !< Destroy the hash table.
    procedure, pass(self) :: get_clone   !< Return a node's content in the hash table by cloning.
    procedure, pass(self) :: get_pointer !< Return a pointer to a node's content in the hash table.
    procedure, pass(self) :: hash        !< Hash the key.
    procedure, pass(self) :: has_key     !< Check if the key is present in the hash table.
    procedure, pass(self) :: initialize  !< Initialize the hash table.
    procedure, pass(self) :: print_keys  !< Print the hash table keys.
    procedure, pass(self) :: remove      !< Remove a node from the hash table, given the key.
    procedure, pass(self) :: traverse    !< Traverse hash table calling the iterator procedure.
    ! private methods
    ! finalizer
    final :: finalize !< Finalize the hash table.
endtype hash_table

interface len
  !< Overload `len` builtin for accepting a [[hash_table]].
  module procedure hash_table_len
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public non TBP
  elemental function hash_table_len(self) result(length)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the number of (actually stored) nodes of the hash table, namely the hash table length.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(hash_table), intent(in) :: self   !< The hash table.
  integer(I4P)                 :: length !< The hash table length.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  length = self%nodes_number
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction hash_table_len

  ! public methods
  subroutine add_pointer(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a node pointer to the hash table.
  !<
  !< @note If a node with the same key is already in the hash table, it is removed and the new one will replace it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout) :: self          !< The hash table.
  class(*),          intent(in)    :: key           !< The key ID.
  class(*), pointer, intent(in)    :: content       !< The content.
  integer(I4P)                     :: b             !< Bucket index, namely hashed key.
  integer(I4P)                     :: bucket_length !< Length (nodes number) of the bucket where the node is placed.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_key_allowed(key)) error stop 'error: key type not supported'
  if (.not.self%is_initialized) call self%initialize ! initialize the table with default options
  b = self%hash(key=key)
  bucket_length = len(self%bucket(b))
  call self%bucket(b)%add_pointer(key=key, content=content)
  self%nodes_number = self%nodes_number + (len(self%bucket(b)) - bucket_length)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_pointer

  subroutine add_clone(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a node to the hash table cloning content (non pointer add).
  !<
  !< @note If a node with the same key is already in the hash table, it is removed and the new one will replace it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout) :: self          !< The hash table.
  class(*),          intent(in)    :: key           !< The key ID.
  class(*),          intent(in)    :: content       !< The content.
  integer(I4P)                     :: b             !< Bucket index, namely hashed key.
  integer(I4P)                     :: bucket_length !< Length (nodes number) of the bucket where the node is placed.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_key_allowed(key)) error stop 'error: key type not supported'
  if (.not.self%is_initialized) call self%initialize ! initialize the table with default options
  b = self%hash(key=key)
  bucket_length = len(self%bucket(b))
  call self%bucket(b)%add_clone(key=key, content=content)
  self%nodes_number = self%nodes_number + (len(self%bucket(b)) - bucket_length)
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
  self%buckets_number = 0_I4P
  self%nodes_number = 0_I4P
  self%is_initialized = .false.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy

  subroutine get_clone(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a node's content in the hash table by cloning.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table),     intent(in)  :: self      !< The hash table.
  class(*),              intent(in)  :: key       !< The key ID.
  class(*), allocatable, intent(out) :: content   !< Content of the queried node.
  class(*), pointer                  :: content_p !< Content pointer of the queried node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  content_p => self%get_pointer(key=key)
  if (associated(content_p)) allocate(content, source=content_p)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_clone

  function get_pointer(self, key) result(content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a pointer to a node's content in the hash table.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in) :: self    !< The hash table.
  class(*),          intent(in) :: key     !< The key ID.
  class(*), pointer             :: content !< Content pointer of the queried node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  content => null()
  if (self%is_initialized) content => self%bucket(self%hash(key=key))%get(key=key)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_pointer

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

  function has_key(self, key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the key is present in the hash table.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in)  :: self    !< The hash table.
  class(*),          intent(in)  :: key     !< The key.
  logical                        :: has_key !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_key = .false.
  if (self%is_initialized) has_key = self%bucket(self%hash(key=key))%has_key(key=key)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_key

  subroutine initialize(self, buckets_number)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize the hash table.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout)        :: self           !< The hash table.
  integer(I4P),      intent(in), optional :: buckets_number !< Number of buckets for initialize the hash table.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy
  self%buckets_number = HT_BUCKETS_NUMBER_DEF ; if (present(buckets_number)) self%buckets_number = buckets_number
  allocate(self%bucket(1:self%buckets_number))
  self%is_initialized=.true.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize

  subroutine print_keys(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print the hash table keys.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in) :: self !< The hash table.
  integer(I4P)                  :: b    !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_initialized) then
    do b=1, self%buckets_number
      call self%bucket(b)%print_keys
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_keys

  subroutine remove(self, key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Remove a node from the hash table, given the key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout) :: self !< The hash table.
  class(*),          intent(in)    :: key  !< The key.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_initialized) call self%bucket(self%hash(key=key))%remove(key=key)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine remove

  subroutine traverse(self, iterator)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Traverse hash table calling the iterator procedure.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in)     :: self     !< The hash_table.
  procedure(key_iterator_interface) :: iterator !< The (key) iterator procedure to call for each node.
  integer(I4P)                      :: b        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_initialized) then
    do b=1, self%buckets_number
      call self%bucket(b)%traverse(iterator)
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine traverse

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
