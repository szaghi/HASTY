!< HASTY dictionary class.
module hasty_dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY dictionary class.
!-----------------------------------------------------------------------------------------------------------------------------------
use hasty_key_base
use hasty_dictionary_node
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: key_iterator_interface
public :: len
public :: dictionary
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: dictionary
  !< **Dictionary** class to storage any contents by means of generic key/content pairs nodes.
  private
  type(dictionary_node), pointer :: head=>null()    !< The first node in the dictionary.
  type(dictionary_node), pointer :: tail=>null()    !< The last node in the dictionary.
  integer(I4P)                   :: nodes_number=0  !< Number of nodes in the dictionary.
  integer(I8P)                   :: ids_(1:2)=[0,0] !< Minimum and maximum unique key id values actually stored.
  contains
    ! public methods
    procedure, pass(self) :: add_pointer  !< Add a node pointer to the dictionary.
    procedure, pass(self) :: add_clone    !< Add a node to the dictionary cloning contents (non pointer add).
    procedure, pass(self) :: destroy      !< Destroy the dictionary.
    procedure, pass(self) :: get_clone    !< Return a node's content by cloning.
    procedure, pass(self) :: get_pointer  !< Return a pointer to a node's content.
    procedure, pass(self) :: ids          !< Return the list of ids actually stored.
    procedure, pass(self) :: has_key      !< Check if the key is present in the dictionary.
    procedure, pass(self) :: loop         !< Sentinel while-loop on nodes returning the key/content pair (for dictionary looping).
    procedure, pass(self) :: node         !< Return a pointer to a node in the dictionary.
    procedure, pass(self) :: print_keys   !< Print the dictionary keys.
    procedure, pass(self) :: remove       !< Remove a node from the dictionary, given the key.
    procedure, pass(self) :: traverse     !< Traverse dictionary from head to tail calling the iterator procedure.
    ! private methods
    procedure, pass(self), private :: add_id            !< Add a id to ids list.
    procedure, pass(self), private :: remove_by_pointer !< Remove node from dictionary, given pointer to it.
    procedure, pass(self), private :: remove_id         !< Remove a id from ids list.
    procedure, pass(self), private :: traverse_iterator !< Traverse dictionary from head to tail calling the iterator procedure.
    ! finalizer
    final :: finalize !< Finalize the dictionary.
endtype dictionary

! public interfaces
abstract interface
  !< Iterator procedure for traversing all nodes in a dictionary by keys.
  subroutine key_iterator_interface(key, content, done)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Iterator procedure for traversing all nodes in a dictionary by keys.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*),          intent(in)  :: key     !< The node key.
  class(*), pointer, intent(in)  :: content !< The generic content.
  logical,           intent(out) :: done    !< Flag to set to true to stop traversing.
  !---------------------------------------------------------------------------------------------------------------------------------
  end subroutine key_iterator_interface
endinterface

interface len
  !< Overload `len` builtin for accepting a [[dictionary]].
  module procedure dictionary_len
endinterface

! private interfaces
abstract interface
  !< Iterator procedure for traversing all nodes in a dictionary.
  subroutine iterator_interface(node, done)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Iterator procedure for traversing all nodes in a dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: dictionary_node
  type(dictionary_node), pointer, intent(in)  :: node !< Actual node pointer in the dictionary.
  logical,                        intent(out) :: done !< Flag to set to true to stop traversing.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine iterator_interface
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public non TBP
  elemental function dictionary_len(self) result(length)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the number of nodes of the dictionary, namely the dictionary length.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(dictionary), intent(in) :: self   !< The dictionary.
  integer(I4P)                 :: length !< The dictionary length.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  length = self%nodes_number
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction dictionary_len

  ! public methods

  subroutine add_pointer(self, key, content, buckets_number)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a node pointer to the dictionary.
  !<
  !< @note If a node with the same key is already in the dictionary, it is removed and the new one will replace it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(inout)        :: self           !< The dictionary.
  class(*),          intent(in)           :: key            !< The key.
  class(*), pointer, intent(in)           :: content        !< The content.
  integer(I4P),      intent(in), optional :: buckets_number !< Buckets number.
  type(dictionary_node), pointer          :: p              !< Pointer to scan the dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_key_allowed(key)) error stop 'error: key type not supported'

  ! if the node is already there, then remove it
  p => self%node(key=key)
  if (associated(p)) call self%remove_by_pointer(p)

  ! update next/previous pointers
  if (associated(self%tail)) then ! insert new node at the end
    allocate(self%tail%next)
    p => self%tail%next
    p%previous => self%tail
  else
    allocate(self%head) ! insert new node as first node
    p => self%head
  end if
  self%tail => p

  call p%set_pointer(key=key, content=content, buckets_number=buckets_number) ! fill the new node with provided contents

  call self%add_id(id=p%key%id())

  self%nodes_number = self%nodes_number + 1
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_pointer

  subroutine add_clone(self, key, content, buckets_number)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a node to the dictionary cloning content (non pointer add).
  !<
  !< @note If a node with the same key is already in the dictionary, it is removed and the new one will replace it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(inout)        :: self           !< The dictionary.
  class(*),          intent(in)           :: key            !< The key.
  class(*),          intent(in)           :: content        !< The content.
  integer(I4P),      intent(in), optional :: buckets_number !< Buckets number.
  type(dictionary_node), pointer          :: p              !< Pointer to scan the dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_key_allowed(key)) error stop 'error: key type not supported'

  ! if the node is already there, then remove it
  p => self%node(key=key)
  if (associated(p)) call self%remove_by_pointer(p)

  ! update next/previous pointers
  if (associated(self%tail)) then ! insert new node at the end
    allocate(self%tail%next)
    p => self%tail%next
    p%previous => self%tail
  else
    allocate(self%head) ! insert new node as first node
    p => self%head
  end if
  self%tail => p

  call p%set_clone(key=key, content=content, buckets_number=buckets_number) ! fill the new node with provided contents

  call self%add_id(id=p%key%id())

  self%nodes_number = self%nodes_number + 1
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_clone

  subroutine destroy(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(inout) :: self !< The dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(self%head)) call destroy_dictionary_node(node=self%head)
  self%head => null()
  self%tail => null()
  self%nodes_number = 0
  self%ids_ = 0
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy

  subroutine get_clone(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a node's content by cloning.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary),     intent(in)  :: self      !< The dictionary.
  class(*),              intent(in)  :: key       !< The key.
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
  !< Return a pointer to a node's content.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(in)  :: self    !< The dictionary.
  class(*),          intent(in)  :: key     !< The key.
  class(*), pointer              :: content !< Content pointer of the queried node.
  type(dictionary_node), pointer :: p       !< Pointer to scan the dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  content => null()
  p => self%node(key=key)
  if (associated(p)) content => p%get_pointer()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_pointer

  function has_key(self, key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the key is present in the dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(in)  :: self    !< The dictionary.
  class(*),          intent(in)  :: key     !< The key.
  logical                        :: has_key !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_key = .false.
  call self%traverse_iterator(iterator=key_iterator_search)
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine key_iterator_search(node, done)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Iterator procedure for searching a key.
    !-------------------------------------------------------------------------------------------------------------------------------
    type(dictionary_node), pointer, intent(in)  :: node !< Actual node pointer in the dictionary.
    logical,                        intent(out) :: done !< Flag to set to true to stop traversing.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    has_key = .false.
    if (node%has_key()) has_key = node%key==key
    done = has_key
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine key_iterator_search
  endfunction has_key

  pure function ids(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the minimum and maximum unique key id values actually stored.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(in)  :: self     !< The dictionary.
  integer(I8P)                   :: ids(1:2) !< Minimum and maximum id values actually stored.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ids = self%ids_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction ids

  function loop(self, key, content) result(again)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Sentinel while-loop on nodes returning the key/content pair (for dictionary looping).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary),     intent(in)            :: self      !< The dictionary.
  class(*), allocatable, intent(out), optional :: key       !< The key.
  class(*), pointer,     intent(out), optional :: content   !< The content.
  logical                                      :: again     !< Sentinel flag to contine the loop.
  type(dictionary_node), pointer, save         :: p=>null() !< Pointer to current node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  again = .false.
  if (present(content)) content => null()
  if (self%nodes_number>0) then
    if (.not.associated(p)) then
      p => self%head
      if (present(key).and.p%has_key()) allocate(key, source=p%key)
      if (present(content)) content => p%get_pointer()
      again = .true.
    elseif (associated(p%next)) then
      p => p%next
      if (present(key).and.p%has_key()) allocate(key, source=p%key)
      if (present(content)) content => p%get_pointer()
      again = .true.
    else
      p => null()
      again = .false.
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction loop

  function node(self, key) result(p)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a pointer to a node in the dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(in)  :: self !< The dictionary.
  class(*),          intent(in)  :: key  !< The key.
  type(dictionary_node), pointer :: p    !< Pointer to node queried.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  p => null()
  call self%traverse_iterator(iterator=key_iterator_search)
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine key_iterator_search(node, done)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Iterator procedure for searching a key.
    !-------------------------------------------------------------------------------------------------------------------------------
    type(dictionary_node), pointer, intent(in)  :: node !< Actual node pointer in the dictionary.
    logical,                        intent(out) :: done !< Flag to set to true to stop traversing.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    done = .false.
    if (node%has_key()) done = node%key==key
    if (done) then
      p => node
    endif
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine key_iterator_search
  endfunction node

  subroutine print_keys(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print the dictionary keys.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(in) :: self !< The dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%traverse_iterator(iterator=key_iterator_print)
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine key_iterator_print(node, done)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Iterator procedure for printing a key.
    !-------------------------------------------------------------------------------------------------------------------------------
    type(dictionary_node), pointer, intent(in)  :: node !< Actual node pointer in the dictionary.
    logical,                        intent(out) :: done !< Flag to set to true to stop traversing.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    if (node%has_key()) print '(A)', node%key%stringify()
    done = .false. ! never stop until the tail
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine key_iterator_print
  endsubroutine print_keys

  subroutine remove(self, key)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Remove a node from the dictionary, given the key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(inout) :: self !< The dictionary.
  class(*),          intent(in)    :: key  !< The key.
  type(dictionary_node), pointer   :: p    !< Pointer to scan the dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  p => self%node(key=key)
  if (associated(p)) call self%remove_by_pointer(p=p)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine remove

  subroutine traverse(self, iterator)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Traverse dictionary from head to tail calling the iterator procedure.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(in)     :: self     !< The dictionary.
  procedure(key_iterator_interface) :: iterator !< The (key) iterator procedure to call for each node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%traverse_iterator(key_iterator_wrapper)
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine key_iterator_wrapper(node, done)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Wrapper for calling the user-specified key_iterator procedure.
    !-------------------------------------------------------------------------------------------------------------------------------
    type(dictionary_node), pointer, intent(in)  :: node !< The dictionary node.
    logical,                        intent(out) :: done !< Flag to set to true to stop traversing.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    if (node%has_key()) call iterator(key=node%key, content=node%get_pointer(), done=done)
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine key_iterator_wrapper
  endsubroutine traverse

  ! private methods
  pure subroutine add_id(self, id)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a id to minimum and maximum unique key id values.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(inout) :: self !< The dictionary.
  integer(I8P),      intent(in)    :: id   !< Unique id to add.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%ids_(1) = min(self%ids_(1), id) ; if (self%ids_(1)==0) self%ids_(1) = id
  self%ids_(2) = max(self%ids_(2), id)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_id

  subroutine remove_by_pointer(self, p)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Remove node from dictionary, given pointer to it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary),              intent(inout) :: self         !< The dictionary.
  type(dictionary_node), pointer, intent(inout) :: p            !< Pointer to the node to remove.
  logical                                       :: has_next     !< Check if dictionary node has a next item.
  logical                                       :: has_previous !< Check if dictionary node has a previous item.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(p)) then
    call self%remove_id(id=p%key%id())
    call p%destroy ! destroy the node contents
    has_next     = associated(p%next)
    has_previous = associated(p%previous)
    if (has_next.and.has_previous) then ! neither first nor last in dictionary
      p%previous%next => p%next
      p%next%previous => p%previous
    elseif (has_next.and.(.not.has_previous)) then ! first one in dictionary
      self%head          => p%next
      self%head%previous => null()
    elseif (has_previous.and.(.not.has_next)) then ! last one in dictionary
      self%tail      => p%previous
      self%tail%next => null()
    elseif ((.not.has_previous).and.(.not.has_next)) then ! only one in the dictionary
      self%head => null()
      self%tail => null()
    endif
    deallocate(p)
    p => null()
    self%nodes_number = self%nodes_number - 1
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine remove_by_pointer

  subroutine remove_id(self, id)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Remove a id from ids list.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(inout) :: self !< The dictionary.
  integer(I8P),      intent(in)    :: id   !< Unique id to add.
  type(dictionary_node), pointer   :: p    !< Pointer to scan the dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%nodes_number==1) then
    self%ids_ = 0
  elseif (self%nodes_number>=2) then
    p => null()
    if (self%ids_(1)==id) then
      call self%traverse_iterator(iterator=id_iterator_search)
      if (associated(p)) then
        if (associated(p%next)) then
          if (p%next%has_key()) then
            self%ids_(1) = p%next%key%id()
          endif
        endif
      endif
    elseif (self%ids_(2)==id) then
      call self%traverse_iterator(iterator=id_iterator_search)
      if (associated(p)) then
        if (associated(p%previous)) then
          if (p%previous%has_key()) then
            self%ids_(2) = p%previous%key%id()
          endif
        endif
      endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine id_iterator_search(node, done)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Iterator procedure for searching a id.
    !-------------------------------------------------------------------------------------------------------------------------------
    type(dictionary_node), pointer, intent(in)  :: node !< Actual node pointer in the dictionary.
    logical,                        intent(out) :: done !< Flag to set to true to stop traversing.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    done = .false.
    if (node%has_key()) done = node%key%id()==id
    if (done) p => node
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine id_iterator_search
  endsubroutine remove_id

  subroutine traverse_iterator(self, iterator)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Traverse dictionary from head to tail calling the iterator procedure.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(in)  :: self     !< The dictionary.
  procedure(iterator_interface)  :: iterator !< The iterator procedure to call for each node.
  type(dictionary_node), pointer :: p        !< Pointer to scan the dictionary.
  logical                        :: done     !< Flag to set to true to stop traversing.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  done = .false.
  p => self%head
  do
    if (associated(p)) then
      call iterator(node=p, done=done)
      if (done) exit
      p => p%next
    else
      exit
    endif
  enddo
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine traverse_iterator

  ! finalizer
  subroutine finalize(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Finalize the dictionary.
  !<
  !< Wrapper for [[dictionary:destroy]]
  !---------------------------------------------------------------------------------------------------------------------------------
  type(dictionary),  intent(inout) :: self !< The dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize
endmodule hasty_dictionary
