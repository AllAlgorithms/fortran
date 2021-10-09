! To run the program using the gnu compiler, run the following
! gfortran -o binomial_coefficient binomial_coefficient.f90
! ./binomial_coefficient
program binomialcoeff
    implicit none
    integer :: n, k, b, i ! Pass by value; not reference

    write(*, '(a)', advance='no') "Enter n, k : "
    read (*,*) n, k

    b = 1
    do i = 1, k
        b = b * (n-i+1) / i
    end do
    write(*, "('The binomial coefficient of (',(i0),',',(i0),') is: ', (i0))") n, k, b

end program binomialcoeff
