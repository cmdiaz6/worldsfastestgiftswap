module errorcheckmod
    contains
    integer function errorcheck(check, string) result(ierr)
        implicit none
        logical,intent(IN) :: check
        character(len=*),intent(IN)  :: string
    
        ierr=0
        if (.not.check) then
            print *,string//' Restarting calculation'
            ierr=-1
            !print *,string//' Run failed'
            !stop
        end if
    end function

end module

module io_mod
    implicit none
    contains
    subroutine readList(filename, groupSize, partnerSize, name_list, partner_list, ierr)

        character(len=*),intent(IN) :: filename

        integer,intent(OUT) :: groupSize, partnerSize
        character(len=*),allocatable,intent(OUT) :: name_list(:)
        integer,allocatable,intent(OUT) :: partner_list(:)
        integer,intent(OUT) :: ierr

        integer :: iunit, stat, icount
        logical :: readPartners

        ! check if file exists
        inquire(file=filename,exist=readPartners)
        if (.not.readPartners) then
            ierr=-1
            return
        end if

        iunit=79902
        open(unit=iunit, file=filename)
        read(iunit,*,iostat=ierr) groupSize, partnerSize
        if (ierr/=0) return

        allocate(partner_list(groupSize))
        allocate(   name_list(groupSize))

        readPartners=.true.
        ierr=0
        icount=1
        partner_list(:)=0
        readfile: do
            ! check if done reading partners
            if (icount >= 2*partnerSize) readPartners=.false.
            if (readPartners) then
                read(iunit, *, iostat=stat) name_list(icount), name_list(icount+1)
            else
                read(iunit,*, iostat=stat) name_list(icount)
            end if
            if (stat==0) then
                if (readPartners) then
                    partner_list(icount)=icount+1
                    partner_list(icount+1)=icount
                    icount=icount+2
                else
                    icount=icount+1
                end if
            else
                print *,'Error reading',filename
                ierr=-1
            end if
            if (icount > groupSize) exit readfile
        end do readfile
        close(iunit)

    end subroutine readList
    subroutine get_arguments(groupSize, partnerSize, filename, readFromFile, ierr)
        integer,intent(INOUT) :: groupSize, partnerSize
        character(len=*),intent(INOUT) :: filename
        logical,intent(OUT) :: readFromFile
        integer,intent(OUT) :: ierr

        integer :: iarg
        character(len=32) :: opt, arg

        ierr = 0
        readFromFile = .false.
        do iarg = 1, command_argument_count(), 2
            call get_command_argument(iarg, opt)
            call get_command_argument(iarg+1, arg, status=ierr)
            select case(opt)
                case('-n')
                    read(arg,*,iostat=ierr) groupSize
                case('-c')
                    read(arg,*,iostat=ierr) partnerSize
                case('-f','--filename')
                    filename = arg
                    readFromFile = .true.
                case default
                    ierr=-1
            end select
            if (ierr /=0) then
                call print_options
                exit
            end if
        end do
    end subroutine
    subroutine print_options
        print *,'Command-line options:'
        print *,'  -f, --filename     file with participant names. See README for format'
        print *,'  -n                 total number of participants'
        print *,'  -c                 total number of couples.' 
        print *,'                     Must be <= (1/2)*number of participants'
    end subroutine
end module

program giftswap
    use errorcheckmod
    use io_mod
    implicit none
    integer, parameter :: max_checks = 1000
    !integer, parameter :: gift_group_size = 100000000
    integer :: gift_group_size = 8
    integer :: number_of_couples = 4
    real    :: rand
    integer,allocatable :: give_list(:), partner_list(:)
    integer,allocatable :: recv_list(:), free_list(:)
    character(len=100),allocatable :: name_list(:)
    integer :: last_spot, random_spot
    integer :: gift_giver, random_person, icheck, ierr
    logical :: partner_assigned, running, readFromFile
    integer :: iunit
    character(len=100) :: fmt1
    character(len=11)  :: filename='partnerlist'

    ! input options
    ! -f filename
    ! if not reading names from file:
    ! -n gift_group_size
    ! -p number_of_couples
    call get_arguments(gift_group_size, number_of_couples, filename, readFromFile, ierr)
    if (ierr/=0) stop
    
    ! read participants from file
    ! place partners on same line
    if (readFromFile) then
        call readList(filename, gift_group_size, number_of_couples, name_list, partner_list, ierr)
        if (ierr /= 0 ) readFromFile = .false.
    end if

    ! manual entry
    if (.not.readFromFile) then
        allocate(partner_list(gift_group_size))
        !partner_list(1:8)=[2,1, 4,3, 6,5, 8,7]
        do gift_giver = 1, 2*number_of_couples, 2
            partner_list(gift_giver)=gift_giver+1
            partner_list(gift_giver+1)=gift_giver
        end do
    end if    

    allocate(give_list(gift_group_size))
    allocate(recv_list(gift_group_size))
    allocate(free_list(gift_group_size))

    running=.true.
    restart: do while (running) ! itry=1,max_checks
        do gift_giver=1,gift_group_size
            free_list(gift_giver)=gift_giver
        end do
        
        give_list(:)=0
        recv_list(:)=0
        last_spot=gift_group_size
        
        ! pick a recipient for each member
        do gift_giver=1,gift_group_size
            ! check if run failed on last iteration
            if (gift_giver==gift_group_size) then
                ierr = errorcheck(free_list(1)/=gift_giver, 'Last person got themselves!')
                if (ierr/=0) cycle restart
                ierr = errorcheck(free_list(1)/=recv_list(gift_giver), 'Two people got eachother!')
                if (ierr/=0) cycle restart
                ierr = errorcheck(free_list(1)/=partner_list(gift_giver), 'Last member got their partner!')
                if (ierr/=0) cycle restart
            end if
        
            ! assign random person
            partner_assigned=.false.
            do icheck=1,max_checks
                ! random number generator
                call random_number(rand)
                random_spot = int(rand*(gift_group_size-gift_giver+1)) + 1
                random_person = free_list(random_spot)
        
                ! no picking yourself
                if (random_person == gift_giver) cycle ! try again.
                ! check to make sure 2 people didn't get eachother
                if (give_list(random_person) == gift_giver) cycle ! try again
                ! check to make sure no one got their partner
                if (partner_list(gift_giver) == random_person) cycle ! try agaiin
        
                ! if everything is ok, assign recipient
                give_list(gift_giver) = random_person
                recv_list(random_person) = gift_giver
        
                ! update list of who is free. move last person to free'd spot
                free_list(random_spot) = free_list(last_spot)
                ! update new last spot
                free_list(last_spot)   = 0
                last_spot = gift_group_size-gift_giver
                partner_assigned=.true.
        
                exit !exits random number loop
            end do
            ierr = errorcheck(partner_assigned, 'Max checks reached!')
            if (ierr/=0) cycle restart
        end do
        running=.false. ! finished. stop running
    end do restart

!    stop
    print *,'Printing gift give list to output.txt'
    iunit=79902
    open(unit=iunit,file='output.txt',form='formatted')
    fmt1="(A,I7,A,I7)"
    if (readFromFile) fmt1="(A,A,A,A)"
    do gift_giver=1,gift_group_size
        if (readFromFile) then
            write(iunit,fmt1) trim(name_list(gift_giver)),' gives to ',trim(name_list(give_list(gift_giver)))
            if (gift_group_size <= 20) &
                write(6,fmt1) trim(name_list(gift_giver)),' gives to ',trim(name_list(give_list(gift_giver)))
        else
            write(iunit,fmt1) 'member ',gift_giver,' gives to member ',give_list(gift_giver)
            if (gift_group_size <= 20) &
                write(*,fmt1) 'member ',gift_giver,' gives to member ',give_list(gift_giver)
        end if
    end do
    close(iunit)
end program

