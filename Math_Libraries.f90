module Math_Libraries
        implicit none
        Interface Least_Square
                module procedure :: Least_Square_Slope
                module procedure :: Least_Square_Complete
        end Interface Least_Square
contains

subroutine Least_Square_Complete(x,y,k,b)
        implicit none

        real,intent(in) :: x(:),y(:)
        real,intent(inout) :: b,k
        real :: average_x,average_y,xx,xy,sum_x,sum_y
        integer :: s
        integer :: i

        if (size(x(:)) /= size(y(:))) then
                write(*,*) "x and y are not the same size!!!"
                return
        end if

        s=size(x(:))
        xx=0.0
        xy=0.0
        sum_x=0.0
        sum_y=0.0
        
        do i=1,s
                sum_x=sum_x+x(i)
                sum_y=sum_y+y(i)
                xy=xy+x(i)*y(i)
                xx=xx+x(i)**2
        end do

        average_x=sum_x/s
        average_y=sum_y/s

        k=(xy-s*average_x*average_y)/(xx-s*average_x**2)
        b=average_y-k*average_x

        return
end subroutine Least_Square_Complete

subroutine Least_Square_Slope(x,y,k)
        implicit none
        real,intent(in) :: x(:),y(:)
        real,intent(inout) :: k
        real :: average_x,average_y,xx,xy,sum_x,sum_y
        integer :: s
        integer :: i

        if (size(x(:)) /= size(y(:))) then
                write(*,*) "x and y are not the same size!!!"
                return
        end if

        s=size(x(:))
        xx=0.0
        xy=0.0
        sum_x=0.0
        sum_y=0.0

        do i=1,s
                sum_x=sum_x+x(i)
                sum_y=sum_y+y(i)
                xy=xy+x(i)*y(i)
                xx=xx+x(i)**2
        end do

        average_x=sum_x/s
        average_y=sum_y/s

        k=(xy-s*average_x*average_y)/(xx-s*average_x**2)
        
        return
end subroutine Least_Square_Slope

subroutine Bubble_Sort(r,Sort_type,indecies)
        implicit none
        
        real :: r(:)
        integer,optional :: indecies(:)
        character(len=*) :: Sort_type
        integer :: i,j
        integer :: s,temp_index
        real :: temp
        if (present(indecies)) then
                if (size(r) /= size(indecies)) then
                        write(*,*) "Something wrong, r is not in the same size with indecies"
                        return
                end if
        end if

        s=size(r(:))
        if (trim(adjustl(Sort_type)) == 'asc') then
                do i=1,s-1
                        do j=1,s-i
                                if (r(j) > r(j+1)) then
                                        temp=r(j)
                                        r(j)=r(j+1)
                                        r(j+1)=temp

                                        if (present(indecies)) then
                                                temp_index=indecies(j)
                                                indecies(j)=indecies(j+1)
                                                indecies(j+1)=temp_index
                                        end if
                                end if
                        end do
                end do

        else if (trim(adjustl(Sort_type)) == 'desc') then
                do i=1,s-1
                        do j=1,s-i
                                if (r(j) < r(j+1)) then
                                        temp=r(j)
                                        r(j)=r(j+1)
                                        r(j+1)=temp

                                        if (present(indecies)) then
                                                temp_index=indecies(j)
                                                indecies(j)=indecies(j+1)
                                                indecies(j+1)=temp_index
                                        end if
                                end if
                        end do
                 end do

        else
                write(*,*) "Input wrong"
        end if
end subroutine Bubble_Sort


recursive subroutine Fast_Sort(A,indecies,sort_type)
        real :: A(:)
        integer :: indecies(:)
        character(len=*),optional :: sort_type
        integer :: head,tail,i,s,temp_index
        real :: temp
        real :: pivot
        if (.not. present(sort_type)) then
                sort_type='asc'
        end if
        head=0
        tail=size(A)+1
        pivot=A(1)
loop1:  do while (.true.)
                tail=tail-1
                head=head+1
tail_loop:      do while (.true.)
                        if (A(tail) <= pivot) then
                                exit tail_loop
                        end if
                        tail=tail-1
                end do tail_loop

head_loop:      do while (.true.) 
                        if (A(head) > pivot) then
                                exit head_loop
                        end if
                        head=head+1
                        if (head > size(A)) then
                                exit head_loop
                        end if
                end do head_loop

                if (head < tail) then
                        temp=A(head)
                        A(head)=A(tail)
                        A(tail)=temp

                        temp_index=indecies(head)
                        indecies(head)=indecies(tail)
                        indecies(tail)=temp_index

                else if (head == tail) then
                        temp=A(head)
                        A(head)=A(1)
                        A(1)=temp

                        temp_index=indecies(head)
                        indecies(head)=indecies(1)
                        indecies(1)=temp_index

                        exit loop1
                else if (head > tail) then
                        temp=A(tail)
                        A(tail)=A(1)
                        A(1)=temp

                        temp_index=indecies(tail)
                        indecies(tail)=indecies(1)
                        indecies(1)=temp_index

                        exit loop1
                end if
        end do loop1
                if (size(A(:tail-1)) > 1) then
                        call Fast_Sort(A(:tail-1),indecies(:tail-1))
                end if

                if (size(A(tail+1:)) > 1) then
                        call Fast_Sort(A(tail+1:),indecies(tail+1:))
                else 
                        return
                end if

        if (trim(adjustl(sort_type)) == 'desc') then
                call reverse_array(A)
                call reverse_array_int(indecies)
        end if
        

contains
        
subroutine reverse_array(array)
        implicit none
        real :: array(:)
        integer :: s,i,temp

        s=size(array)

        do i=1,s/2
                temp=array(i)
                array(i)=array(s+1-i)
                array(s+1-i)=temp
        end do
end subroutine reverse_array

subroutine reverse_array_int(array)
        implicit none
        integer :: array(:)
        integer :: s,i,temp

        s=size(array)

        do i=1,s/2
                temp=array(i)
                array(i)=array(s+1-i)
                array(s+1-i)=temp
        end do
end subroutine reverse_array_int

end subroutine Fast_Sort

subroutine minimum_image(displacement,lattice_constants)
        real :: displacement
        real :: lattice_constants
        displacement = displacement - nint(displacement / lattice_constant) * lattice_constant
end subroutine minimum_image


function cosin(x,y)
        real,dimension(3) :: x,y
        real :: cosin
        cosin = (dot_product(x,y))/(sqrt(x(1)**2+x(2)**2+x(3)**2)*sqrt(y(1)**2+y(2)**2+y(3)**2))
end function cosin

end module Math_Libraries
