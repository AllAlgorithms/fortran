program testinsertionsort
        integer lst(10)
        lst = (/ 10, 9, 8, 4, 5, 6, 7, 3, 2, 1 /)
        call insertionsort(lst, 1, 10)
        call show(lst)
end program testinsertionsort

subroutine insertionsort(lst, a, b)
        integer a
        integer b
        integer lst(10)
        integer key
        integer j
        integer i
        do i = a+1, b
            key = lst(i)
            j = i - 1
            do while(j .ge. 0 .and. lst(j) .gt. key)
                lst(j+1) = lst(j)
                j = j - 1
            end do
            lst(j+1) = key
        end do
end

subroutine show(lst)
        integer lst(10)
        integer x
        do x = 1, 10
                print 100, lst(x)
        end do

100 format (i0)
end
