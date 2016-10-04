!< HASTY abstract **container** class.
module hasty_container_adt
!-----------------------------------------------------------------------------------------------------------------------------------
!< HASTY abstract **container** class.
!<
!< A very base class that is intended to be the parent of all containers.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: container_adt
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: container_adt
  !< Abstract **container** class to storage any contents.
  !<
  !< A very base class that is intended to be the parent of all containers.
  contains
    ! public/private deferred methods
    procedure(destroy_interface),   pass(self),         deferred :: destroy   !< Destroy the container content.
    procedure(is_filled_interface), pass(self),         deferred :: is_filled !< Return storage status.
    procedure(set_interface),       pass(self),         deferred :: set       !< Set the content of the container.
    procedure(typeguard_interface), pass(self),         deferred :: typeguard !< Check if the content is accepted by the container.
    procedure(is_equal_interface),  pass(lhs), private, deferred :: is_equal  !< Implement `==` operator.
    ! public generics
    generic, public :: operator(==) => is_equal !< Overloading `==` operator.
endtype container_adt

! public methods interfaces
abstract interface
  !< Destroy the container content.
  elemental subroutine destroy_interface(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Destroy the container content.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: container_adt
  class(container_adt), intent(inout) :: self !< The container.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine destroy_interface
endinterface

abstract interface
  !< Return storage status.
  elemental logical function is_filled_interface(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return storage status.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: container_adt
  class(container_adt), intent(in) :: self !< The container.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_filled_interface
endinterface

abstract interface
  !< Check if the content is accepted by the container.
  elemental logical function typeguard_interface(self, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the content is accepted by the container.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: container_adt
  class(container_adt), intent(in) ::  self    !< The container.
  class(*),             intent(in) ::  content !< The content of container.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction typeguard_interface
endinterface

abstract interface
  !< Set the content of the storage to value passed.
  subroutine set_interface(self, content)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the content of the container.
  !<
  !< The type of the content variable provided must be the same as the container variable is designed to accept
  !< (as determined by the concrete implementation of the [[container:typeguard]] method in the extension)
  !< or be of the same type of container.
  !---------------------------------------------------------------------------------------------------------------------------------
  import :: container_adt
  class(container_adt), intent(out) :: self    !< The container.
  class(*),             intent(in)  :: content !< The content of the container.
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
  import :: container_adt
  class(container_adt), intent(in) :: lhs !< Left hand side.
  class(container_adt), intent(in) :: rhs !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal_interface
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule hasty_container_adt
