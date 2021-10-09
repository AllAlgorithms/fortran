! Solves linear eqns: Ax = b for x.
program gauss_elimination
    use, intrinsic :: iso_fortran_env, only : dp=>real64, error_unit, input_unit
    implicit none
    integer :: i, j, k, n, pos, istat ! n - Number of unknowns
    real(dp), allocatable :: ag(:,:), x(:) ! Augumented matrix and solution vector
    real(dp) :: factor
    character(len=1024) :: msg ! string for holding error messages

    write(*, '(a)', advance='no') "Number of unknowns: "
    read (input_unit, *, iostat=istat) n
    if (istat /= 0) stop 1

    allocate(ag(n, n+1))
    allocate(x(n))

    call get_aug_matrix(ag)

    ! Forward elimination
    do k = 1, n-1
        ! Partial scaled pivoting
        pos = maxloc( abs(ag(k:n, k)/maxval(ag(k:n, :), dim=2)), dim=1 )
        j = k + pos - 1
        if (.not. j == k) call swap(ag(j,:), ag(k,:))
        ! Elimination
        do i = k+1, n
            factor = ag(i,k) / ag(k,k)
            ag(i, k:) = ag(i, k:) - factor*ag(k, k:)
        end do
    end do

    ! Back substitution
    x(n) = ag(n, n+1) / ag(n,n)
    do i = n-1, 1, -1
        x(i) = ( ag(i,n+1) - dot_product(ag(i, i+1:n), x(i+1:n)) ) / ag(i,i)
    end do

    print *, 'The solution vector x is: '
    print *, x

    deallocate(ag)
    deallocate(x)

    contains

    ! Subroutine to get the values of the Augumented Matrix
    subroutine get_aug_matrix(a)
        ! Note that read statements below make use of istat and msg from parent program
        real(dp), intent(inout) :: a(:,:)
        integer :: n
        n = size(a,1)
        print *, 'Enter the Coefficient Matrix A (Row-wise) '
        do i = 1, n
            write(*, '("A(",i0,", :) ")', advance='no') i
            read (input_unit, *, iostat=istat, iomsg=msg) a(i,1:n)
            if (istat /= 0) stop trim(msg)
        end do
        print *, 'Enter the RHS constants vector (b): '
        do i = 1, n
            write(*, '("b(",i0,") ")', advance='no') i
            read (input_unit, *, iostat=istat, iomsg=msg) a(i,n+1)
            if (istat /= 0) stop trim(msg)
        end do
    end subroutine get_aug_matrix

    ! Subroutine to swap rows of the matrix
    elemental subroutine swap(a, b)
        real(dp), intent(inout) :: a, b
        real(dp) :: temp
        temp = a
        a = b
        b = temp
    end subroutine swap

end program gauss_elimination
