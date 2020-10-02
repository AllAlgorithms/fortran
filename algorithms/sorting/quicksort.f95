program testquicksort
	integer lst(10)
	lst = (/ 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 /)
	call quicksort(lst, 1, 10)
	call show(lst)
end program testquicksort

recursive subroutine quicksort(lst, start, end_)
	integer lst(10)
	integer start, end_
	integer pivot, i, tmp
	if (start .lt. end_) then
		pivot = start
		do i = start+1, end_
			if (lst(i) .le. lst(start)) then
				pivot = pivot + 1
				tmp = lst(pivot)
				lst(pivot) = lst(i)
				lst(i) = tmp
			end if
		end do
		tmp = lst(pivot)
		lst(pivot) = lst(start)
		lst(start) = tmp

		call quicksort(lst, start, pivot)
		call quicksort(lst, pivot+1, end_)
	end if
end

subroutine show(lst)
	integer lst(10)
	integer x
	do x = 1, 10
		print 100, lst(x)
	end do

100 format (i0)
end
