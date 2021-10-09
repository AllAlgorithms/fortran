! To run the program using the gnu compiler, run the following
! gfortran -o factorial_recursive factorial_recursive.f90
! ./factorial_recursive
program factorial_recursive
    implicit none
    integer :: n, f

    print *, 'Enter an integer n to compute its factorial: '
    read *, n
    f = factorial(n)
    print '((i0),"! = ",(i0))', n, f

    contains
    recursive function factorial(n) result(f)
        integer, intent(in) :: n
        integer :: f
        if (n <= 0) then
            f = 1
        else
            f = n * factorial(n - 1)
        end if
    end function factorial
end program factorial_recursive
