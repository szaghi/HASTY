!< HASTY class of hash table.
module hasty_hash_table
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY class of hash table.
!-----------------------------------------------------------------------------------------------------------------------------------
use hasty_content_adt
use hasty_key_base
use hasty_dictionary, dictionary_get_clone=>get_clone
use penf, penf_is_initialized=>is_initialized
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: len
public :: hash_table
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
integer(I4P), parameter :: HT_BUCKETS_NUMBER_DEF = 557_I4P !< Default number of buckets of hash table.

type :: hash_table
  !< **Hash table** class to storage any contents by means of generic dictionary buckets.
  private
#ifdef CAF
  type(dictionary), allocatable :: bucket(:)[:]            !< Hash table buckets.
  integer(I8P),     allocatable :: ids_(:,:)[:]            !< Minimum and maximum id values actually stored into each bucket.
  integer(I4P),     allocatable :: nodes_number_[:]        !< Number of nodes actually stored, namely the hash table length.
#else
  type(dictionary), allocatable :: bucket(:)               !< Hash table buckets.
  integer(I8P),     allocatable :: ids_(:,:)               !< Minimum and maximum id values actually stored into each bucket.
  integer(I4P)                  :: nodes_number_=0_I4P     !< Number of nodes actually stored, namely the hash table length.
#endif
  integer(I4P)                  :: me=0                    !< Index of current CAF image.
  integer(I4P)                  :: images_number=0         !< Number of CAF images.
  integer(I4P)                  :: buckets_number=0_I4P    !< Number of buckets used.
  logical                       :: is_homogeneous_=.false. !< Homogeneity status-guardian.
  logical                       :: is_initialized_=.false. !< Initialization status.
  class(*), allocatable         :: typeguard_key           !< Key type guard (mold) for homogeneous keys check.
  class(*), allocatable         :: typeguard_content       !< Content type guard (mold) for homogeneous contents check.
  contains
    ! public methods
    procedure, pass(self) :: add_pointer    !< Add a node pointer to the hash table.
    procedure, pass(self) :: add_clone      !< Add a node to the hash table cloning contents (non pointer add).
    procedure, pass(self) :: destroy        !< Destroy the hash table.
    procedure, pass(self) :: get_clone      !< Return a node's content in the hash table by cloning.
    procedure, pass(self) :: get_pointer    !< Return a pointer to a node's content in the hash table.
    procedure, pass(self) :: has_key        !< Check if the key is present in the hash table.
    procedure, pass(self) :: ids            !< Return the list of ids actually stored.
    procedure, pass(self) :: initialize     !< Initialize the hash table.
    procedure, pass(self) :: is_homogeneous !< Return homogeneity status.
    procedure, pass(self) :: is_initialized !< Return initialization status.
    procedure, pass(self) :: print_keys     !< Print the hash table keys.
    procedure, pass(self) :: remove         !< Remove a node from the hash table, given the key.
    procedure, pass(self) :: traverse       !< Traverse hash table calling the iterator procedure.
    ! private methods
    procedure, pass(self), private :: allocate_members         !< Allocate dynamic memory members.
    procedure, pass(self), private :: check_type               !< Check type consistency.
    procedure, pass(self), private :: get_bucket_image_indexes !< Get the bucket and image indexes corresponding to the given key.
    procedure, pass(self), private :: hash                     !< Hash the key.
    procedure, pass(self), private :: set_buckets_number       !< Set buckets number.
    procedure, pass(self), private :: set_caf_dimensions       !< Set CAF dimensions by means of intrinsic inquiring functions.
    procedure, pass(self), private :: set_homogeneous          !< Set homogeneity flag.
    procedure, pass(self), private :: synchronize_images       !< Syncrhonize CAF images.
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
  elemental function hash_table_len(self, global) result(length)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the number of (actually stored) nodes of the hash table, namely the hash table length.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(hash_table), intent(in)           :: self   !< The hash table.
  logical,          intent(in), optional :: global !< Check the global values on all CAF images rather only on the local image.
  integer(I4P)                           :: length !< The hash table length.
#ifdef CAF
  integer(I4P)                           :: i      !< Counter.
#endif
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
#ifdef CAF
  length = 0
  if (self%is_initialized_) then
    length = self%nodes_number_
    if (present(global)) then
      if (global.and.self%images_number>1) then
        if (self%images_number>1) then
          do i=1, self%images_number
            if (self%me==i) cycle
            length = length + self%nodes_number_[i] ! blocking "get"
          enddo
        endif
      endif
    endif
  endif
#else
    length = self%nodes_number_
#endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction hash_table_len

  ! public methods
  subroutine add_pointer(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a node pointer to the hash table.
  !<
  !< @note If a node with the same key is already in the hash table, it is removed and the new one will replace it.
  !<
  !< @note If the table is **homogeneous** the key/content types are verified against the typeguard members: if those typeguards
  !< have not been initialized by an invocation of [[hash_table:initialize]] they are initialized by the types here passed.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout) :: self          !< The hash table.
  class(*),          intent(in)    :: key           !< The key.
  class(*), pointer, intent(in)    :: content       !< The content.
  integer(I4P)                     :: b             !< Bucket index.
  integer(I4P)                     :: i             !< Image index.
  integer(I4P)                     :: bucket_length !< Length (nodes number) of the bucket where the node is placed.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_key_allowed(key)) error stop 'error: key type not supported'
  if (.not.self%is_initialized_) call self%initialize ! initialize the table with default options
  call self%check_type(key=key, content=content)
  call self%get_bucket_image_indexes(key=key, bucket=b, image=i)
  if (b>0.and.i==self%me) then
    bucket_length = len(self%bucket(b))
    call self%bucket(b)%add_pointer(key=key, content=content)
    self%nodes_number_ = self%nodes_number_ + (len(self%bucket(b)) - bucket_length)
    self%ids_(1:2, b) = self%bucket(b)%ids()
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_pointer

  subroutine add_clone(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a node to the hash table cloning content (non pointer add).
  !<
  !< @note If a node with the same key is already in the hash table, it is removed and the new one will replace it.
  !<
  !< @note If the table is **homogeneous** the key/content types are verified against the typeguard members: if those typeguards
  !< have not been initialized by an invocation of [[hash_table:initialize]] they are initialized by the types here passed.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout) :: self          !< The hash table.
  class(*),          intent(in)    :: key           !< The key.
  class(*),          intent(in)    :: content       !< The content.
  integer(I4P)                     :: b             !< Bucket index.
  integer(I4P)                     :: i             !< Image index.
  integer(I4P)                     :: bucket_length !< Length (nodes number) of the bucket where the node is placed.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_key_allowed(key)) error stop 'error: key type not supported'
  if (.not.self%is_initialized_) call self%initialize ! initialize the table with default options
  call self%check_type(key=key, content=content)
  call self%get_bucket_image_indexes(key=key, bucket=b, image=i)
  if (b>0.and.i==self%me) then
    bucket_length = len(self%bucket(b))
    call self%bucket(b)%add_clone(key=key, content=content)
    self%nodes_number_ = self%nodes_number_ + (len(self%bucket(b)) - bucket_length)
    self%ids_(1:2, b) = self%bucket(b)%ids()
  endif
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
  if (allocated(self%ids_)) deallocate(self%ids_)
#ifdef CAF
  if (allocated(self%nodes_number_)) deallocate(self%nodes_number_)
#else
  self%nodes_number_ = 0_I4P
#endif
  self%me = 0
  self%images_number = 0
  self%buckets_number = 0_I4P
  self%is_homogeneous_ = .false.
  self%is_initialized_ = .false.
  if (allocated(self%typeguard_key)) then
    associate(key=>self%typeguard_key)
      select type(key)
      class is(key_base)
        call key%destroy
      endselect
    endassociate
    deallocate(self%typeguard_key)
  endif
  if (allocated(self%typeguard_content)) then
    associate(content=>self%typeguard_content)
      select type(content)
      class is(content_adt)
        call content%destroy
      endselect
    endassociate
    deallocate(self%typeguard_content)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy

  subroutine get_clone(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a node's content in the hash table by cloning.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table),     intent(in)  :: self    !< The hash table.
  class(*),              intent(in)  :: key     !< The key.
  class(*), allocatable, intent(out) :: content !< Content of the queried node.
  integer(I4P)                       :: b       !< Bucket index.
  integer(I4P)                       :: i       !< Image index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_initialized_) then
    call self%get_bucket_image_indexes(key=key, bucket=b, image=i)
    if (b>0) then
#ifdef CAF
      call dictionary_get_clone(self%bucket(b)[i], key=key, content=content)
      ! call self%bucket(b)[i]%get_clone(key=key, content=content)
#else
      call self%bucket(b)%get_clone(key=key, content=content)
#endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_clone

  function get_pointer(self, key) result(content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a pointer to a node's content in the hash table.
  !<
  !< @note The result is associated only if the queried node belongs to the current CAF image, otherwise pointer association is
  !< not allowed.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in) :: self    !< The hash table.
  class(*),          intent(in) :: key     !< The key.
  class(*), pointer             :: content !< Content pointer of the queried node.
  integer(I4P)                  :: b       !< Bucket index.
  integer(I4P)                  :: i       !< Image index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  content => null()
  if (self%is_initialized_) then
    call self%get_bucket_image_indexes(key=key, bucket=b, image=i)
    if (b>0.and.i==self%me) then
      content => self%bucket(b)%get_pointer(key=key)
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_pointer

  function has_key(self, key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the key is present in the hash table.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in)  :: self    !< The hash table.
  class(*),          intent(in)  :: key     !< The key.
  logical                        :: has_key !< Check result.
  integer(I4P)                   :: b       !< Bucket index.
  integer(I4P)                   :: i       !< Image index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_key = .false.
  if (self%is_initialized_) then
    call self%get_bucket_image_indexes(key=key, bucket=b, image=i)
    if (b>0.and.i==self%me) then
      has_key = self%bucket(b)%has_key(key=key)
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_key

  pure function ids(self, global)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the minimum and maximum unique key id values actually stored.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in)           :: self     !< The hash table.
  logical,           intent(in), optional :: global   !< Check the global values on all CAF images rather only on the local image.
  integer(I8P)                            :: ids(1:2) !< Minimum and maximum id values actually stored.
  integer(I4P)                            :: b        !< Bucket index, namely hashed key.
#ifdef CAF
  integer(I4P)                            :: i        !< Counter.
#else
#endif
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ids = [huge(1_I8P), -huge(1_I8P)]
  if (self%is_initialized_) then
    if (present(global)) then
      if (global.and.self%images_number>1) then
#ifdef CAF
        do i=1, self%images_number
          do b=1, self%buckets_number
            if (len(self%bucket(b)[i])>0) ids = [min(ids(1), self%ids_(1,b)[i]), max(ids(2), self%ids_(2,b)[i])]
          enddo
        enddo
#endif
      endif
    else
      do b=1, self%buckets_number
        if (len(self%bucket(b))>0) ids = [min(ids(1), self%ids_(1,b)), max(ids(2), self%ids_(2,b))]
      enddo
    endif
  endif
  if (ids(1)>ids(2)) ids = 0 ! nullify IDs in case of empty buckets
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction ids

  subroutine initialize(self, buckets_number, use_prime, homogeneous, typeguard_key, typeguard_content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize the hash table.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout)        :: self              !< The hash table.
  integer(I4P),      intent(in), optional :: buckets_number    !< Number of buckets for initialize the hash table.
  logical,           intent(in), optional :: use_prime         !< If true the buckets number is rendered prime.
  logical,           intent(in), optional :: homogeneous       !< If true the hash is supposed to accept only homogeneous nodes.
  class(*),          intent(in), optional :: typeguard_key     !< Key type guard (mold) for homogeneous keys check.
  class(*),          intent(in), optional :: typeguard_content !< content type guard (mold) for homogeneous contents check.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy
  call self%set_buckets_number(buckets_number=buckets_number, use_prime=use_prime)
  call self%allocate_members(typeguard_key=typeguard_key, typeguard_content=typeguard_content)
  call self%set_caf_dimensions
  call self%set_homogeneous(homogeneous=homogeneous)
  self%is_initialized_ = .true.
  call self%synchronize_images
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize

  elemental function is_homogeneous(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return homogeneity status.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in) :: self           !< The hash table.
  logical                       :: is_homogeneous !< Homogeneity status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_homogeneous = self%is_homogeneous_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_homogeneous

  elemental function is_initialized(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return initialization status.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in) :: self           !< The hash table.
  logical                       :: is_initialized !< Initialization status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_initialized = self%is_initialized_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_initialized

  subroutine print_keys(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print the hash table keys.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in) :: self !< The hash table.
  integer(I4P)                  :: b    !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_initialized_) then
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
  integer(I4P)                     :: b    !< Bucket index, namely hashed key.
  integer(I4P)                     :: i    !< Image index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_initialized_) then
    call self%get_bucket_image_indexes(key=key, bucket=b, image=i)
    if (b>0.and.i==self%me) then
      call self%bucket(b)%remove(key=key)
      self%nodes_number_ = self%nodes_number_ - 1
      self%ids_(1:2, b) = self%bucket(b)%ids()
    endif
  endif
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
  if (self%is_initialized_) then
    do b=1, self%buckets_number
      call self%bucket(b)%traverse(iterator)
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine traverse

  ! private methods
  subroutine allocate_members(self, typeguard_key, typeguard_content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Allocate dynamic memory members.
  !<
  !< @note No check is done on the allocations status that must be handled outside this procedure.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout)        :: self              !< The hash table.
  class(*),          intent(in), optional :: typeguard_key     !< Key type guard (mold) for homogeneous keys check.
  class(*),          intent(in), optional :: typeguard_content !< content type guard (mold) for homogeneous contents check.
  integer(I4P)                            :: b                 !< Bucket index, namely hashed key.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
#ifdef CAF
  allocate(self%bucket(1:self%buckets_number)[*])
  allocate(self%ids_(1:2,1:self%buckets_number)[*])
  allocate(self%nodes_number_[*])
#else
  allocate(self%bucket(1:self%buckets_number))
  allocate(self%ids_(1:2,1:self%buckets_number))
#endif
  do b=1, self%buckets_number
    self%ids_(1:2, b) = [huge(1_I8P), -huge(1_I8P)]
  enddo
  self%nodes_number_ = 0
  if (present(typeguard_key)) allocate(self%typeguard_key, mold=typeguard_key)
  if (present(typeguard_content)) allocate(self%typeguard_content, mold=typeguard_content)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine allocate_members

  subroutine check_type(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check type consistency.
  !<
  !< @note If the key/content type-guard memebers are not already allocated the passed key/content are used as mold variables.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout) :: self    !< The hash table.
  class(*),          intent(in)    :: key     !< The key.
  class(*),          intent(in)    :: content !< The content.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_homogeneous_) then
    if (.not.allocated(self%typeguard_key)) then
      allocate(self%typeguard_key, mold=key)
    elseif (.not.same_type_as(self%typeguard_key, key)) then
      error stop 'error: key type is different from type-guard one for the homogeneous table'
    endif
    if (.not.allocated(self%typeguard_content)) then
      allocate(self%typeguard_content, mold=content)
    elseif (.not.same_type_as(self%typeguard_content, content)) then
      error stop 'error: content type is different from type-guard one for the homogeneous table'
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine check_type

  pure subroutine get_bucket_image_indexes(self, key, bucket, image)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get the bucket and image indexes corresponding to the given key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in)  :: self            !< The hash table.
  class(*),          intent(in)  :: key             !< The key.
  integer(I4P),      intent(out) :: bucket          !< Bucket index.
  integer(I4P),      intent(out) :: image           !< Image index.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  bucket = self%hash(key=key)
  image = 1
  if (self%images_number>1) then
    image = mod(bucket, self%images_number) + 1
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_bucket_image_indexes

  elemental function hash(self, key) result(bucket)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Hash the key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(in) :: self   !< The hash table.
  class(*),          intent(in) :: key    !< Key to hash.
  integer(I4P)                  :: bucket !< Bucket index corresponding to the key.
  type(key_base)                :: key_   !< Key to hash casted to [[key_base]].
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  bucket = 0
  if (self%is_initialized_) then
    key_ = key_base(key=key, buckets_number=self%buckets_number)
    bucket = key_%hash(buckets_number=self%buckets_number)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction hash

  pure subroutine set_buckets_number(self, buckets_number, use_prime)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set buckets number.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout)        :: self           !< The hash table.
  integer(I4P),      intent(in), optional :: buckets_number !< Number of buckets for initialize the hash table.
  logical,           intent(in), optional :: use_prime      !< If true the buckets number is rendered prime.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%buckets_number = HT_BUCKETS_NUMBER_DEF
  if (present(buckets_number)) self%buckets_number = buckets_number
  if (present(use_prime)) then
    if (use_prime) then
      if (.not.is_prime(self%buckets_number)) self%buckets_number = find_next_prime(self%buckets_number)
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_buckets_number

  pure subroutine set_caf_dimensions(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set CAF dimensions by means of intrinsic inquiring functions.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout) :: self !< The hash table.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
#ifdef CAF
  self%me = this_image()
  self%images_number = num_images()
#else
  self%me = 1
  self%images_number = 1
#endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_caf_dimensions

  pure subroutine set_homogeneous(self, homogeneous)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set homogeneity flag.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout)        :: self              !< The hash table.
  logical,           intent(in), optional :: homogeneous       !< If true the hash is supposed to accept only homogeneous nodes.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%is_homogeneous_ = .false. ; if (present(homogeneous)) self%is_homogeneous_ = homogeneous
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_homogeneous

  subroutine synchronize_images(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Synchronize CAF images.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(hash_table), intent(inout) :: self !< The hash table.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
#ifdef CAF
  sync all
#endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine synchronize_images

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

  ! private non TBP
  pure function is_prime(n) result(is_prime_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return true if `n` is prime accordingly division test, false if `n` is composite.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent (in) :: n         !< Number inquired.
  logical                   :: is_prime_ !< Result of the test.
  integer(I4P)              :: i         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (n<2.or.mod(n, 2_I4P)==0_I4P.or.mod(n, 3_I4P)==0_I4P) then
    is_prime_ = .false.
    return
  elseif (n>1.and.n<=3) then
    is_prime_ = .true.
    return
  endif
  is_prime_ = .true.
  do i=3, int(sqrt(real(n, R8P)), I4P), 2
    if (mod(n, i)==0) then
      is_prime_ = .false.
      exit
    endif
  enddo
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_prime

  pure function find_next_prime(n) result(prime)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the next prime after `n` accordingly division test.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent (in) :: n     !< Number inquired.
  integer(I4P)              :: prime !< Next prime after `n`.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prime = n + 1_I4P
  do while(.not.is_prime(prime))
    prime = prime + 1_I4P
  enddo
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction find_next_prime
endmodule hasty_hash_table
