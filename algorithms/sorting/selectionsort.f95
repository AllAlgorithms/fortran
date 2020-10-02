program testselectionsort
        integer lst(10)
        lst = (/ 10, 9, 8, 4, 5, 6, 7, 3, 2, 1 /)
        call selectionsort(lst, 1, 10)
        call show(lst)
end program testselectionsort

subroutine selectionsort(lst, a, b)
        integer a
        integer b
        integer lst(10)
        integer i
        integer j
        integer min_id
        integer tmp
        do i = a, b-1
            min_id = i
            do j = i+1, b
                if (lst(min_id) .gt. lst(j)) then
                    min_id = j
                end if
            end do
            tmp = lst(i)
            lst(i) = lst(min_id)
            lst(min_id) = tmp
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
