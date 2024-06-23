module scaleconvertmodule
    use fractionmodule
    use scaleconverttypes
    use trigmod
    implicit none

    contains

    ! Function to create a plot of points
    ! for a given radius for a curve
    function generateplotpoints(radius, arc) result(output)
        real, intent(in) :: radius
        integer, intent(in) :: arc
        type(plotXY), dimension(arc) :: output
        integer :: i

        do i = 1, arc
            output(i)%x = adj_hyp_ang_deg(radius, real(i))
            output(i)%y = opp_hyp_ang_deg(radius, real(i))
        end do 
    end function generateplotpoints

    ! Function to calculate the height of a helix
    ! This is adapted from a spreadsheet that I used to create
    ! my helix for my original layout.  The only fault was space
    ! and heat, but the calculations were correct even if workmanship
    ! was not.  I used the math the create a radial arm helix using 8mm 
    ! slats to support 6mm ply, increasing height by 11mm every 45 degrees
    !      0.00     11.00      2.77    505.00     74.00     14.00
    !     45.00     22.00      2.77    505.00     74.00     14.00
    !     90.00     33.00      2.77    505.00     74.00     14.00
    !    135.00     44.00      2.77    505.00     74.00     14.00
    !    180.00     55.00      2.77    505.00     74.00     14.00
    !    225.00     66.00      2.77    505.00     74.00     14.00
    !    270.00     77.00      2.77    505.00     74.00     14.00
    !    315.00     88.00      2.77    505.00     74.00     14.00
    !    360.00     99.00      2.77    505.00     74.00     14.00
    function helix(radius, support, increment) result(output)
        real, intent(in) :: radius, support, increment
        type(helixtype), allocatable, dimension(:) :: output
        integer ::  count
        real, allocatable, dimension(:) :: height
        real :: gradient, i, degrees

        degrees = 45.
        count = 1

        ! Allocate the array
        allocate(output(9))
        allocate(height(9))

        ! Through a single revolution at 45 degree intervals
        do i = 0., 360., degrees
            if (count .gt. 1) then
                height(count) = height(count -1) + (increment)
            else
                height(count) = height(count) + (increment)
            end if
            output(count)%height = height(count)
            output(count)%degree = i
            output(count)%radius = radius
            count = count + 1
        end do

        count = 1
        ! Calculate the gradient more for consistency
        do i = degrees, 360.+degrees, degrees
            gradient = (height(count) / ((2. * pi * radius)/(360./i))) * 100.
            output(count)%gradient = gradient
            count = count + 1
        end do

        ! Calculate the clearence and supports
        do i = 1, 9
            output(i)%clearence = (output(size(output))%height - output(1)%height) - support
            output(i)%supports = support
        end do
    end function helix

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
            ! If the denominator is greater than 1 
            if (input%inches%denominator .gt. 1) then
                f1%numerator = ((input%feet * 12) + input%inches%unit) * input%inches%denominator + input%inches%numerator
                f1%denominator = input%inches%denominator
            ! Otherwise it is one.
            else
                f1%numerator = (input%feet * 12) + input%inches%unit
                f1%denominator = 1
            end if
        else if (input%inches%unit .gt. 0) then
            f1%unit = 0
            if (input%inches%denominator .gt. 1) then
                f1%numerator = input%inches%unit * input%inches%denominator + input%inches%numerator
                f1%denominator = input%inches%denominator
            else
                f1%numerator = input%inches%unit
                f1%denominator = 1
            end if
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

    ! Given a value in feet and a scale, generate a table of values
    function feettable(feet, scale) result(output)
        real, intent(in) :: feet
        real, intent(in) :: scale
        type(scaletype), dimension(13) :: output
        integer :: i, count

        count = 1

        output(count) = calculation(feet * 12.0, scale)
        output(count)%original%feet = feet
        output(count)%original%inches%unit = 0
        count = count + 1
        do i = 1, 11
            output(count) = calculation((feet * 12.0) + i, scale)
            output(count)%original%feet = feet
            output(count)%original%inches%unit = i
            count = count + 1
        end do
        output(count) = calculation((feet + 1) * 12.0, scale)
        output(count)%original%feet = feet + 1
        output(count)%original%inches%unit = 0
    end function feettable

    ! Given a value in inches and a scale, generate a table of values
    function inchtable(inches, division, scale) result(output)
        real, intent(in) :: inches, division, scale
        type(scaletype), dimension(int(division)+1) :: output
        integer :: i, count

        count = 1
        output(count) = calculation(inches, scale)
        output(count)%original%feet = 0
        output(count)%original%inches%unit = inches
        output(count)%original%inches%numerator = 0
        output(count)%original%inches%denominator = 1
        count = count + 1
        do i = 1, int(division-1)
            output(count) = calculation(inches + (i / division), scale)
            output(count)%original%feet = 0
            output(count)%original%inches%unit = inches
            output(count)%original%inches%numerator = i
            output(count)%original%inches%denominator = division
            count = count + 1
        end do
        output(count) = calculation(inches + 1.0, scale)
        output(count)%original%feet = 0
        output(count)%original%inches%unit = inches +1.0
        output(count)%original%inches%numerator = 0
        output(count)%original%inches%denominator = 1
    end function inchtable

    ! Perform the calculation and return the scaletype
    function calculation(inches, scale) result(output)
        real, intent(in) :: inches, scale
        type(scaletype) :: output
        type(fractiontype), dimension(3) :: fractions
        real :: r

        r = (inches) / scale
        output%imperialdecimal = r
        output%metricdecimal = r * inch2mm
        output%scale = scale
        fractions = closestimperialfraction(output%imperialdecimal)
        output%imperialbelow%inches = fractions(1)
        output%imperialvalue%inches = fractions(2)
        output%imperialabove%inches = fractions(3)
    end function calculation

    ! Function to convert a string to an imperialtype
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