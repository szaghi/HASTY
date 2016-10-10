!< HASTY dictionary class.
module hasty_dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY dictionary class.
!-----------------------------------------------------------------------------------------------------------------------------------
use hasty_key_adt
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
  type(dictionary_node), pointer :: head=>null()   !< The first node in the dictionary.
  type(dictionary_node), pointer :: tail=>null()   !< The last node in the dictionary.
  integer(I4P)                   :: nodes_number=0 !< Number of nodes in the dictionary.
  contains
    ! public methods
    procedure, pass(self) :: add_pointer  !< Add a node pointer to the dictionary.
    procedure, pass(self) :: add_clone    !< Add a node to the dictionary cloning contents (non pointer add).
    procedure, pass(self) :: destroy      !< Destroy the dictionary.
    procedure, pass(self) :: get_clone    !< Return a node's content by cloning.
    procedure, pass(self) :: get_pointer  !< Return a pointer to a node's content.
    procedure, pass(self) :: has_key      !< Check if the key is present in the dictionary.
    procedure, pass(self) :: loop         !< Sentinel while-loop on nodes returning the key/content pair (for dictionary looping).
    procedure, pass(self) :: loop_key     !< Sentinel while-loop on nodes returning the key (for key looping).
    procedure, pass(self) :: loop_content !< Sentinel while-loop on nodes returning the content (for content looping).
    procedure, pass(self) :: node         !< Return a pointer to a node in the dictionary.
    procedure, pass(self) :: print_keys   !< Print the dictionary keys.
    procedure, pass(self) :: remove       !< Remove a node from the dictionary, given the key.
    procedure, pass(self) :: traverse     !< Traverse dictionary from head to tail calling the iterator procedure.
    ! private methods
    procedure, pass(self), private :: remove_by_pointer !< Remove node from dictionary, given pointer to it.
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
  subroutine add_pointer(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a node pointer to the dictionary.
  !<
  !< @note If a node with the same key is already in the dictionary, it is removed and the new one will replace it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(inout) :: self    !< The dictionary.
  class(*),          intent(in)    :: key     !< The key.
  class(*), pointer, intent(in)    :: content !< The content.
  type(dictionary_node), pointer   :: p       !< Pointer to scan the dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_key_allowed(key)) error stop 'error: key type not supported'

  ! if the node is already there, then remove it
  p => self%node(key=key) ! associate p to the pointer allocated
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

  call p%set_pointer(key=key, content=content) ! fill the new node with provided contents

  self%nodes_number = self%nodes_number + 1
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_pointer

  subroutine add_clone(self, key, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a node to the dictionary cloning content (non pointer add).
  !<
  !< @note If a node with the same key is already in the dictionary, it is removed and the new one will replace it.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(inout) :: self    !< The dictionary.
  class(*),          intent(in)    :: key     !< The key.
  class(*),          intent(in)    :: content !< The content.
  type(dictionary_node), pointer   :: p       !< Pointer to scan the dictionary.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.is_key_allowed(key)) error stop 'error: key type not supported'

  ! if the node is already there, then remove it
  p => self%node(key=key) ! associate p to the pointer allocated
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

  call p%set_clone(key=key, content=content) ! fill the new node with provided contents

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
  if (associated(p)) content => p%content
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
    has_key = are_keys_equal(lhs=node%key, rhs=key)
    done = has_key
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine key_iterator_search
  endfunction has_key

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
      if (present(key).and.allocated(p%key)) allocate(key, source=p%key)
      if (present(content)) content => p%content
      again = .true.
    elseif (associated(p%next)) then
      p => p%next
      if (present(key).and.allocated(p%key)) allocate(key, source=p%key)
      if (present(content)) content => p%content
      again = .true.
    else
      p => null()
      again = .false.
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction loop

  function loop_key(self, key) result(again)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Sentinel while-loop on nodes returning the key (for key looping).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary),     intent(in)    :: self      !< The dictionary.
  class(*), allocatable, intent(out)   :: key       !< The key.
  logical                              :: again     !< Sentinel flag to contine the loop.
  type(dictionary_node), pointer, save :: p=>null() !< Pointer to current node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  again = .false.
  if (self%nodes_number>0) then
    if (.not.associated(p)) then
      p => self%head
      if (allocated(p%key)) allocate(key, source=p%key)
      again = .true.
    elseif (associated(p%next)) then
      p => p%next
      if (allocated(p%key)) allocate(key, source=p%key)
      again = .true.
    else
      p => null()
      again = .false.
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction loop_key

  function loop_content(self, content) result(again)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Sentinel while-loop on nodes returning the content pointer (for content looping).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary), intent(in)        :: self      !< The dictionary.
  class(*), pointer, intent(out)       :: content   !< The content.
  logical                              :: again     !< Sentinel flag to contine the loop.
  type(dictionary_node), pointer, save :: p=>null() !< Pointer to current node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  again = .false.
  content => null()
  if (self%nodes_number>0) then
    if (.not.associated(p)) then
      p => self%head
      content => p%content
      again = .true.
    elseif (associated(p%next)) then
      p => p%next
      content => p%content
      again = .true.
    else
      p => null()
      again = .false.
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction loop_content

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
    done = are_keys_equal(lhs=node%key, rhs=key)
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
    if (allocated(node%key)) print '(A)', str_key(node%key)
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
    call iterator(key=node%key, content=node%content, done=done)
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine key_iterator_wrapper
  endsubroutine traverse

  ! private methods
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
