!< HASTY abstract **content** class.
module hasty_content_adt
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY abstract **content** class.
!<
!< A very base class that is intended to be the parent of all contents.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: content_adt
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: content_adt
  !< Abstract **content** class to storage any contents.
  !<
  !< A very base class that is intended to be the parent of all contents.
  contains
    ! public/private deferred methods
    procedure(destroy_interface),   pass(self),         deferred :: destroy   !< Destroy the content.
    procedure(is_filled_interface), pass(self),         deferred :: is_filled !< Return storage status.
    procedure(set_interface),       pass(self),         deferred :: set       !< Set the content.
    procedure(typeguard_interface), pass(self),         deferred :: typeguard !< Check if the content type is allowed.
    procedure(is_equal_interface),  pass(lhs), private, deferred :: is_equal  !< Implement `==` operator.
    ! public generics
    generic, public :: operator(==) => is_equal !< Overloading `==` operator.
endtype content_adt

! public methods interfaces
abstract interface
  !< Destroy the content.
  elemental subroutine destroy_interface(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the content.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: content_adt
  class(content_adt), intent(inout) :: self !< The content.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy_interface
endinterface

abstract interface
  !< Return storage status.
  elemental logical function is_filled_interface(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return storage status.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: content_adt
  class(content_adt), intent(in) :: self !< The content.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_filled_interface
endinterface

abstract interface
  !< Check if the content type is allowed.
  elemental logical function typeguard_interface(self, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the content type is allowed.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: content_adt
  class(content_adt), intent(in) ::  self    !< The content.
  class(*),           intent(in) ::  content !< The content to be stored.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction typeguard_interface
endinterface

abstract interface
  !< Set the content of the storage to value passed.
  subroutine set_interface(self, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the content of the content.
  !<
  !< The type of the content variable provided must be the same as the content variable is designed to accept
  !< (as determined by the concrete implementation of the [[content:typeguard]] method in the extension)
  !< or be of the same type of content.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: content_adt
  class(content_adt), intent(out) :: self    !< The content.
  class(*),           intent(in) ::  content !< The content to be stored.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_interface
endinterface

! private methods interfaces
abstract interface
  !< Implement `==` operator.
  elemental logical function is_equal_interface(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Implement `==` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: content_adt
  class(content_adt), intent(in) :: lhs !< Left hand side.
  class(content_adt), intent(in) :: rhs !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal_interface
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule hasty_content_adt
