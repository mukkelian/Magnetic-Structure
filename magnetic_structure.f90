!    Code magnetic_structure can be used to plot the magentic structure of
!    of a given magnetic material and works only for the calculations done
!    by ELK (https://elk.sourceforge.io/)
!    Copyright (C) 2024, Mukesh Kumar Sharma

!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU Affero General Public License as published
!    by the Free Software Foundation, either version 3 of the License, or
!    any later version.

!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU Affero General Public License for more details.

!    You should have received a copy of the GNU Affero General Public License
!    along with this program.  If not, see <https://www.gnu.org/licenses/>.

program magnetic_structure
	implicit none
	character(len=256) :: line, search_string, dummy_string, &
    		dummy_string1, dummy_string2, dummy_string3, &
                dummy_string4, dummy_string5, dummy_string6
    	character(len=256), allocatable :: species_name(:)

	integer :: unit_number, line_number, last_occurrence, iostat, &
		tot_atom, i, j, K, tot_words, word_present
	real :: vector(3, 3), position_momentum(200, 6)
	real, allocatable :: ith_species(:)

	position_momentum = 0
	search_string = "Moments :"
	unit_number = 10
	line_number = 0
	last_occurrence = -1
	print*, ''
        print*, "INPUT: 'GEOMETRY.OUT', 'INFO.OUT'"
	print*, ''
	print*, "Reading 'INFO.OUT' file"
	
	open(unit=unit_number, file="INFO.OUT", status="old", action="read")
	print*, "Finding moment informations in 'INFO.OUT'"
	
	do
		read(unit_number, '(A)', iostat=iostat) line
		if (iostat /= 0) exit
		line_number = line_number + 1
		if (index(line, search_string) /= 0) then
			last_occurrence = line_number
		end if
	end do

	close(unit_number)
	if (last_occurrence /= -1) then
		print*, 'Done!'
		print*, ''
		print *, "Final moment information found at line:", last_occurrence
	else
		print *, "Moment information not found"
		print*, 'STOPPING now'
		stop
	end if
        print*, ''
	print*, "Reading 'GEOMETRY.OUT' file"
!	reading GEOMETRY.OUT started	########################
	open(unit=unit_number+1, file="GEOMETRY.OUT", status="old", action="read")
	do line_number = 1, 14
		read(unit_number+1, '(A)') line
	end do
    
	do line_number = 1, 3
		read(unit_number+1, *) vector(line_number, 1:3)
	end do
	do line_number = 1, 2
		read(unit_number+1, '(A)') line
	end do
	read(unit_number+1, '(A)') line
	read(line, *) tot_atom, dummy_string
	allocate(ith_species(tot_atom), species_name(tot_atom))
	j = 0
	do line_number = 1, tot_atom
  	  	read(unit_number+1, '(A)') line
  	  	species_name(line_number) = line(2:3)
    		read(unit_number+1, '(A)') line
    		read(line, *) ith_species(line_number), dummy_string
 
		do i = 1, int(ith_species(line_number))
			j = j + 1
			read(unit_number+1, '(A)') line
			read(line, *) position_momentum(j, 1:6)
			position_momentum(j, 1:3) = position_momentum(j, 1)*vector(1, 1:3) +&
                                                    position_momentum(j, 2)*vector(2, 1:3) +&
                                                    position_momentum(j, 3)*vector(3, 1:3)
		end do
	end do
	close(unit_number+1)
	print*, 'Done!'
	print*, ''
!	reading GEOMETRY.OUT completed	########################
    
!	Searching moment data in INFO.OUT
	open(unit=unit_number, file="INFO.OUT", status="old", action="read")
	do line_number = 1, last_occurrence + 2
		read(unit_number, '(A)') line
	end do

	j = 0
	do line_number = 1, tot_atom
		read(unit_number, '(A)') line
		do i = 1, int(ith_species(line_number))
			j = j + 1
			read(unit_number, '(A)') line
			dummy_string = line

			tot_words = 0; word_present = 0
			do k = 1, len(dummy_string)
				if (dummy_string(k:k) .ne. ' ') then
					if (word_present == 0) then
						tot_words = tot_words + 1
						word_present = 1
					end if
				else
					word_present = 0
				end if
			end do

			if(tot_words.eq.4) then
			read(line, *) dummy_string1, dummy_string2, dummy_string3, &
			dummy_string4
			read(dummy_string4, *) position_momentum(j, 6)
			position_momentum(j, 4:5) = 0
			else
				read(line, *) dummy_string1, dummy_string2, dummy_string3, &
				dummy_string4, dummy_string5, dummy_string6
         			dummy_string = trim(dummy_string4)//' '//&
         					trim(dummy_string5)//' '//trim(dummy_string6)
				read(dummy_string, *) position_momentum(j, 4:6)
			end if

		end do
	end do

	close(unit_number)
!	Searching completed

	open(unit=unit_number, file="GEOMETRY.xsf", status="unknown", action="write")
	write(unit_number, *) 'CRYSTAL'
	write(unit_number, *) 'PRIMVEC'
	do i = 1, 3
		write(unit_number, *) vector(i, :)
	end do
	write(unit_number, *) 'CONVVEC'
	do i = 1, 3
		write(unit_number, *) vector(i, :)
	end do
	write(unit_number, *) 'PRIMCOORD'
	write(unit_number, *) sum(ith_species), '1'
	k = 0
	do i = 1, tot_atom
		do j = 1, int(ith_species(i))
			k = k + 1
			write(unit_number, *)  trim(adjustl(species_name(i))),' ', &
			position_momentum(k, :)
		end do
	end do
	close(unit_number)
        print*, "OUTPUT: 'GEOMETRY.xsf'"
        print*,''

end program magnetic_structure
