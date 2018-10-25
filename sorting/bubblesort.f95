program testbubblesort
        integer lst(10)
        lst = (/ 10, 9, 8, 4, 5, 6, 7, 3, 2, 1 /)
        call bubblesort(lst, 1, 10)
        call show(lst)
end program testbubblesort

subroutine bubblesort(lst, a, b)
        integer a
        integer b
        integer lst(10)
        integer x
        integer y
        integer tmp
        do x = a, b-1
            do y = a, b-x
                if (lst(y) .gt. lst(y+1)) then
                    tmp = lst(y)
                    lst(y) = lst(y+1)
                    lst(y+1) = tmp
                end if
            end do
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
