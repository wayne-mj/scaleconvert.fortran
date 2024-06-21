module scaleconvertmodule
    use fractionmodule
    use scaleconverttypes
    implicit none

    contains

    ! Function to convert a string to a scale type
    function imperialscale(input, scale) result(output)
        type(imperialtype), intent(in) :: input
        real, intent(in) :: scale
        type(scaletype) :: output
        type(fractiontype) :: f1
        type(fractiontype), dimension(3) :: fractions
        real :: r
        integer :: temp

        ! If there is a feet value
        if (input%feet .gt. 0) then
            f1%unit = 0            
            f1%numerator = ((input%feet * 12) + input%inches%unit) * input%inches%denominator + input%inches%numerator
            f1%denominator = input%inches%denominator
        else if (input%inches%unit .gt. 0) then
            f1%unit = 0
            f1%numerator = input%inches%unit * input%inches%denominator + input%inches%numerator
            f1%denominator = input%inches%denominator
        else
            f1%unit = 0
            f1%numerator = input%inches%numerator
            f1%denominator = input%inches%denominator
        end if

        ! Decimal inches
        r = ((real(f1%numerator) / real(f1%denominator)) / scale)
        output%imperialdecimal = r

        ! Decimal mm
        r = r * inch2mm
        output%metricdecimal = r

        ! Scale
        output%scale = round(scale, 3)

        ! Closest imperial fractions
        fractions = closestimperialfraction(output%imperialdecimal)
        output%imperialbelow%inches = fractions(1)
        output%imperialvalue%inches = fractions(2)
        output%imperialabove%inches = fractions(3)
        
    end function imperialscale

    ! Find the closest imperial fractions
    function closestimperialfraction(imperialdecimal) result(fractions)
        real, intent(in) :: imperialdecimal
        type(fractiontype), dimension(3) :: fractions
        type(fractiontype) :: middlef, abovef, belowf
        integer :: inch, above, below, i

        inch = nint(imperialdecimal * 10.**precision) * (baseDenominator / (10.**precision))

        ! Middle
        middlef = lcd(inch, int(baseDenominator))

        ! Below
        do i=inchprecision, 1, -1
            if (real(i)/inchprecision .le. imperialdecimal) then
                below = i
                exit
            end if
        end do

        belowf = lcd(below, inchprecision)

        ! Above
        do i=1, inchprecision
            if (real(i)/inchprecision .ge. imperialdecimal) then
                above = i
                exit
            end if
        end do

        abovef = lcd(above, inchprecision)

        fractions(1) = belowf
        fractions(2) = middlef
        fractions(3) = abovef
    end function closestimperialfraction

    ! ! Function to convert a string to an imperialtype
    function str2imp(input) result(imperialstr)
        character(len=*), intent(in) :: input
        type(imperialtype) :: imperialstr
        type(fractiontype) :: frac
        character(len=32) :: temp
        integer :: i, length, feetpos, inchpos, slashpos, io_status, spacecount, count
        integer, allocatable :: spacepos(:)
        logical :: fdelim, idelim, sdelim
        
        ! Set default values
        fdelim = .false.
        idelim = .false.
        sdelim = .false.
        length = len_trim(input)
        feetpos = 0
        inchpos = 0
        slashpos = 0
        spacecount = 0
        count =1

        ! Initialize the imperialtype
        imperialstr%feet = 0 !-1
        imperialstr%inches%unit = 0 !-1
        imperialstr%inches%numerator = 1 !-1
        imperialstr%inches%denominator = 1 !-1
        imperialstr%status = "OK"
        
        ! Initialize the fractiontype
        frac%unit = 0 !-1
        frac%numerator = 1 !-1
        frac%denominator = 1 !-1
        frac%status = "OK"

        ! Determine what, if any, delimiters are present
        ! and where applicable, save their locations or count the number of spaces
        do i = 1, length
            if (input(i:i) == feetdelim) then
                feetpos = i
                fdelim = .true.
            elseif (input(i:i) == inchdelim) then
                inchpos = i
                idelim = .true.
            elseif (input(i:i) == slashdelim) then
                slashpos = i
                sdelim = .true.
            elseif (input(i:i) == " ") then
                spacecount = spacecount + 1
            end if
        end do
        
        ! If only spaces are present then save their locations
        if (spacecount .gt. 0) then
            allocate(spacepos(spacecount))
            do i = 1, length
                if (input(i:i) == " ") then
                    spacepos(count) = i
                    count = count + 1
                end if
            end do
        end if

        ! If no delimiters are present, only spaces.
        if ((fdelim .eqv. .false.) .and. (idelim .eqv. .false.) ) then
            ! If more than one space exists then there is a feet value and a fractions
            if ((length .gt. 0) .and. (spacecount .gt. 1)) then
                read(input(1:spacepos(1)-1), *, iostat=io_status) imperialstr%feet
                frac = returnfraction(trim(adjustl(input(spacepos(1)+1:))))
                imperialstr%inches = frac
            ! Only feet exists if there is no space
            else if ((length .gt. 0) .and. (spacecount .eq. 0) .and. (sdelim .eqv. .false.)) then
                read(input, *, iostat=io_status) imperialstr%feet
                imperialstr%inches = frac
            ! Only the inches exists if there is only one space
            else if ((length .gt. 0) .and. (spacecount .eq. 1)) then
                imperialstr%feet = 0
                temp = trim(adjustl(input(1:length)))
                frac = returnfraction(temp)
                imperialstr%inches = frac
            ! Only the fraction exists
            else if ((length .gt. 0) .and. (spacecount .eq. 0) .and. (sdelim .eqv. .true.)) then
                imperialstr%feet = 0
                temp = trim(adjustl(input(1:length)))
                frac = returnfraction(temp)
                imperialstr%inches = frac
            end if
        ! If delimiters are present
        else if ((fdelim .eqv. .true.) .and. (idelim .eqv. .true.) ) then
            if (length .gt. 0 .and. feetpos .gt. 0) then
                read(input(1:feetpos-1), *, iostat=io_status) imperialstr%feet
                temp = trim(adjustl(input(feetpos+1:inchpos-1)))
                frac = returnfraction(temp)
                imperialstr%inches = frac
            end if
        ! If only the feet delimiter is present
        else if ((fdelim .eqv. .true.) .and. (idelim .eqv. .false.) ) then
            if (length .gt. 0 .and. feetpos .gt. 0) then
                read(input(1:feetpos-1), *, iostat=io_status) imperialstr%feet
                imperialstr%inches = frac
            end if
        ! If only the inches delimiter is present
        else if ((fdelim .eqv. .false.) .and. (idelim .eqv. .true.)) then
            if (length .gt. 0 .and. inchpos .gt. 0) then
                imperialstr%feet = 0
                temp = trim(adjustl(input(1:inchpos-1)))
                frac = returnfraction(temp)
                imperialstr%inches = frac
            end if
        end if
    end function str2imp

    ! Perform rounding
    function round(value, precision)
        real :: value, round
        integer, intent(in) :: precision

        round = nint(value * 10.0**precision) / 10.0**precision
    end function round
end module scaleconvertmodule