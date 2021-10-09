! Solution based from https://stackoverflow.com/questions/22844838/pascal-triangle-in-fortran
! To run the program using the gnu compiler, run the following
! gfortran -o pascals_triangle pascals_triangle.f90
! ./pascals_triangle
program pascals_triangle
    implicit none
    integer :: i, j, k, n, p

    write (*, "(A)", advance='no') 'Please enter number of lines n: '
    read (*, *) n

    do i=0,n-1
        p = 1
        do j=1,n-1-i
            write(*, '(3X)', advance='no') ! write the leading spaces
        end do
        do k = 0, i
            write(*, '(i6)', advance='no') p ! max length of integer value is 6
            p = p*(i-k)/(k+1) ! Binomial coefficient B(i, k)
        end do
        write(*, *) ! write a newline character
    end do
end program pascals_triangle
