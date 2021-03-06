!< HASTY dictionary node class.
module hasty_dictionary_node
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY dictionary node class.
!-----------------------------------------------------------------------------------------------------------------------------------
use hasty_content_adt
use hasty_key_base
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: destroy_dictionary_node
public :: dictionary_node
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: dictionary_node
  !< **Dictionary node** class to storage any contents by means of generic key/content pairs.
  !<
  !< @note The `next/previous` members of this class are public because they can be safely handled by the [[dictionary]] class.
  class(key_base),       allocatable, public  :: key              !< The key.
  class(*),              pointer,     private :: content_=>null() !< The generic content.
  type(dictionary_node), pointer,     public  :: next=>null()     !< The next node in the dictionary.
  type(dictionary_node), pointer,     public  :: previous=>null() !< The previous node in the dictionary.
  contains
    ! public methods
    procedure, pass(self) :: destroy     !< Destroy the node (key & content).
    procedure, pass(self) :: get_pointer !< Return a pointer to node's content.
    procedure, pass(self) :: has_key     !< Return .true. if the node has a key (or id) set-up.
    procedure, pass(self) :: is_filled   !< Return storage status.
    procedure, pass(self) :: set_pointer !< Set the node pointer-associating content.
    procedure, pass(self) :: set_clone   !< Set the node cloning content.
    ! private methods
    procedure, pass(self), private :: destroy_key     !< Destroy the node key.
    procedure, pass(self), private :: destroy_content !< Destroy the node content.
    ! finalizer
    final :: finalize !< Finalize the node.
endtype dictionary_node
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public non TBP
  recursive subroutine destroy_dictionary_node(node)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the node and its subsequent ones.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(dictionary_node), pointer, intent(inout) :: node !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(node)) then
    call node%destroy
    call destroy_dictionary_node(node=node%next)
    node%previous => null()
    deallocate(node)
    node => null()
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy_dictionary_node

  ! public methods
  subroutine destroy(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the node (key & content).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary_node), intent(inout) :: self !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy_key
  call self%destroy_content
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy

  function get_pointer(self) result(content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a pointer to node's content.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary_node), intent(in) :: self    !< The node.
  class(*), pointer                  :: content !< Content pointer of the node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(self%content_)) content => self%content_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_pointer

  elemental logical function has_key(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return .true. if the node has a key (or id) set-up.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary_node), intent(in) :: self !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_key = allocated(self%key)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_key

  elemental logical function is_filled(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return storage status.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary_node), intent(in) :: self !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_filled = .false.
  if (self%has_key()) then
    if (associated(self%content_)) then
      is_filled = .true.
      associate(content=>self%content_)
        select type(content)
        class is(content_adt)
          is_filled = content%is_filled()
        endselect
      endassociate
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_filled

  subroutine set_pointer(self, key, content, buckets_number)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the node pointer-associating content.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary_node), intent(inout)        :: self           !< The node.
  class(*),               intent(in)           :: key            !< The key.
  class(*), pointer,      intent(in)           :: content        !< The content.
  integer(I4P),           intent(in), optional :: buckets_number !< Buckets number.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy_key
  allocate(self%key, source=key_base(key=key, buckets_number=buckets_number))
  call self%destroy_content
  self%content_ => content
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_pointer

  subroutine set_clone(self, key, content, buckets_number)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the node cloning content.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary_node), intent(inout)        :: self           !< The node.
  class(*),               intent(in)           :: key            !< The key.
  class(*),               intent(in)           :: content        !< The content.
  integer(I4P),           intent(in), optional :: buckets_number !< Buckets number.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy_key
  allocate(self%key, source=key_base(key=key, buckets_number=buckets_number))
  call self%destroy_content
  allocate(self%content_, source=content)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_clone

  ! private methods
  subroutine destroy_key(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the node key.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary_node), intent(inout) :: self !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%key)) then
    associate(key=>self%key)
      select type(key)
      class is(key_base)
        call key%destroy
      endselect
    endassociate
    deallocate(self%key)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy_key

  subroutine destroy_content(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the node content.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(dictionary_node), intent(inout) :: self !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(self%content_)) then
    associate(content=>self%content_)
      select type(content)
      class is(content_adt)
        call content%destroy
      endselect
    endassociate
    deallocate(self%content_)
    self%content_ => null()
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy_content

  ! finalizer
  subroutine finalize(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Finalize the dictionary node.
  !<
  !< Wrapper for [[dictionary_node:destroy]]
  !---------------------------------------------------------------------------------------------------------------------------------
  type(dictionary_node), intent(inout) :: self !< The node.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%destroy
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize
endmodule hasty_dictionary_node
