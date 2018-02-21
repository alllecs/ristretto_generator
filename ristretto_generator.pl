#!/usr/bin/perl -w
use strict;
use Math::BigInt;

# v1.10 01.02.2018
# by Alexander Smirnov
# alllecs@cs.niisi.ras.ru, alllecs@ya.ru
# by:   Trubitsyn Dmitriy trubicin@cs.niisi.ras.ru (get_settings part);
# run:	./ristretto_generator.pl > test.s
#
#	1.4  hangups fixed;
#	1.5  add random repeat test;
#	1.6  add disable false sharing
#	1.7  2 bases in test
#	1.8  add disable false sharing
#	1.9  generate start address

#srand;

my $start_0 = 0;
my $start_1 = 0;
my @start_array = ();
my @pointer = ();
my @total_array = ();
my @tmp_array = ();
my $dataspaces_for_core0_min = 0;
my $dataspaces_for_core0_max = 0;
my $dataspaces_for_core1_min = 0;
my $dataspaces_for_core1_max = 0;
my $dataspaces_size_min = 0;
my $dataspaces_size_max = 0;
my $one_test_lenght_min = 0;
my $one_test_lenght_max = 0;
my $randomize_instruction_seq = 0;
my $zone_reexec = 0;
my @zone_rexec = ();
my $test_zones = 0;
my $size_sectors = 22;
my $t_regs = 8;
my $s_regs = 8;
my $dataspaces_aligment1 = 0;
my $dataspaces_aligment2 = 0;
my $dataspaces_aligment3 = 0;
my $disable_false_sharing = 0;
#$start_array[$start_i][0] = ( int (rand(16)) * 0x20000 ) | 0x08000000;	#random addr 0x88000000 - 88200000

my @perc_rand = (0, 0, 0);
my @array_a_size = ();
my @array_a_ratio = ();
my @array_addr = ();

sub get_settings {
	my $i_addr = 0;
	my $i_a_size = 0;
	my $i_a_ratio = 0;
	my @array_settings = ();
	my @array1 = ();


	open my $file_fish, '<', 'settings.txt' or die "Can not open file set.txt\n"; #open

	while(<$file_fish>) {
#	    if(/\s*NUM_CORES\s*=\s*\d+/)	# grep the pattern
#		{@array=split /\s*=\s*/;	# set separator "=", put string parts to @array
#		 @array1=split / |;/, $array[1];	# cut " " and ";" of value
#		 $num_cores=$array1[0];}	# final value

		if(/\s*DATASPACES_FOR_CORE0_MIN\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$dataspaces_for_core0_min=$array1[0];
		} elsif(/\s*DATASPACES_FOR_CORE0_MAX\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$dataspaces_for_core0_max=$array1[0];
		} elsif(/\s*DATASPACES_FOR_CORE1_MIN\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$dataspaces_for_core1_min=$array1[0];
		} elsif(/\s*DATASPACES_FOR_CORE1_MAX\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$dataspaces_for_core1_max=$array1[0];
		} elsif(/\s*DATASPACES_ALIGNMENT1\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$dataspaces_aligment1=$array1[0];
		} elsif(/\s*DATASPACES_ALIGNMENT2\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$dataspaces_aligment2=$array1[0];
		} elsif(/\s*DATASPACES_ALIGNMENT3\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$dataspaces_aligment3=$array1[0];
		} elsif(/\s*DATASPACES_SIZE_MIN\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$dataspaces_size_min=$array1[0];
		} elsif(/\s*DATASPACES_SIZE_MAX\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$dataspaces_size_max=$array1[0];
		} elsif(/\s*RANDOMIZE_INSTRUCTION_SEQ\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$randomize_instruction_seq=$array1[0];
		} elsif(/\s*ZONE_REEXEC\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$zone_reexec=$array1[0];
		} elsif(/\s*DISABLE_FALSE_SHARING\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$disable_false_sharing=$array1[0];
		} elsif(/\s*ONE_TEST_LENGHT_MIN\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$one_test_lenght_min=$array1[0];
		} elsif(/\s*ONE_TEST_LENGHT_MAX\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$one_test_lenght_max=$array1[0];
		} elsif(/\s*TEST_ZONES\s*=\s*\d+/) {
			@array_settings=split /\s*=\s*/;
			@array1=split / |;/, $array_settings[1];
			$test_zones=$array1[0];
#		} elsif(/\s*DATA\s*\d+/) {			# grep "DATAn", n=1,2,3...
#			@array_settings=split /\s*,\s*/;	# separate string to array, separator ","
#			$ar_name=$array[0];		# first element is array name
#			print "array name is $ar_name\n";	#debug string
#			@$ar_name=($array[1],$array[2],$array[3]);	#forming new array
#			print join (", ", @$ar_name);	# debug print, print array
#		} else {
#			print "Option not found!\n";
		}
		if(/DATA[\d,\s]*\s([0-9abcdefx]+).*\s([0-9abcdefx]+).*R\/W/) {
			$array_addr[$i_addr][0] = $1;
			$array_addr[$i_addr][1] = $2;
			$i_addr++;
		} elsif(/DATASPACES_ALIGNMENT.*SIZE\s?=\s?([0-9abcdefx]+)/) {
			$array_a_size[$i_a_size] = $1;
			$i_a_size++;
		} elsif(/DATASPACES_ALIGNMENT.*SIZE_RATIO\s?=\s?(\d+)/) {
			$array_a_ratio[$i_a_ratio] = $1;
			$i_a_ratio++;
		}
	}

	close $file_fish;
#	print "\nNUM_CORES=$num_cores\n";
}

get_settings();
my @perc_r = ( (100 - $randomize_instruction_seq) / 2, (100 - $randomize_instruction_seq) / 2, $randomize_instruction_seq);
my $instructions_number = int(rand($one_test_lenght_max - $one_test_lenght_min) + $one_test_lenght_min); #the number of instructions in subtests
$start_0 = int(rand($dataspaces_for_core0_max - $dataspaces_for_core0_min) + $dataspaces_for_core0_min); #the number of sections for core 0
$start_1 = int(rand($dataspaces_for_core1_max - $dataspaces_for_core1_min) + $dataspaces_for_core1_min); #the number of sections for core 1

#Convert int to hex
sub get_hex {
	my ($value) = @_;
	return ( sprintf("0x%X", $value) );
}

#Generate start addr
sub get_start_addr {
	my $rand_index_addr = int(rand(@array_addr));
	my $max_ratio = 0;
	my $alignment_size = 0;
	my $max_size = Math::BigInt->new($array_addr[$rand_index_addr][1]) - Math::BigInt->new($array_addr[$rand_index_addr][0]);
	for(my $i = 0; $i < $#array_a_ratio; $i++) {
		$max_ratio += $array_a_ratio[$i];
	}
	my $rand_index_size = int(rand($max_ratio + 1));
	if($rand_index_size > 0 && $rand_index_size <= $array_a_ratio[0]) {
		$alignment_size = eval $array_a_size[0];
	} elsif ($rand_index_size > $array_a_ratio[0] && $rand_index_size <= $array_a_ratio[1]) {
		$alignment_size = eval $array_a_size[1];
	} elsif ($rand_index_size > $array_a_ratio[1] && $rand_index_size <= $array_a_ratio[2]) {
		$alignment_size = eval $array_a_size[2];
	} elsif ($rand_index_size > $array_a_ratio[2] && $rand_index_size <= $array_a_ratio[3]) {
		$alignment_size = eval $array_a_size[3];
	} else {
		$alignment_size = eval $array_a_size[int(rand(@array_a_size))];
	}

	my $point = $max_size / $alignment_size;
	my $start_addr = Math::BigInt->new($array_addr[$rand_index_addr][0]) + (int(rand($point)) * $alignment_size);
#	$start_addr = get_hex($start_addr);

	return $start_addr;
}


# Filling array: start address and section sizes
sub start {
	for(my $start_i = 0; $start_i < $start_0 + $start_1; $start_i++) {
#		$start_array[$start_i][0] = int (int ( rand($start_0 + $start_1) ) * eval($dataspaces_aligment2) ) | 0x08000000;	#random addr 0x88000000 - 88200000
		($start_array[$start_i][0]) = get_start_addr();
		$start_array[$start_i][1] = int (rand($dataspaces_size_max - $dataspaces_size_min) + $dataspaces_size_min);	#cache line 3 - 5 (* 32 Byte)

		# Check crossing sections
		for(my $i = 0; $i < @start_array - 1; $i++) {
			if ( ($start_array[$start_i][0] >= $start_array[$i][0]
			   && $start_array[$start_i][0] <= $start_array[$i][0] + $start_array[$i][1])
			   || ($start_array[$start_i][0] + $start_array[$start_i][1] >= $start_array[$i][0]
			   && $start_array[$start_i][0] + $start_array[$start_i][1] <= $start_array[$i][0] + $start_array[$i][1]) ) {
				$start_i--;
				last;
			}
		}
	}
	# Open file for random data
	open(FILE_DATA, "> random_data.h");

	print "\ninit_core0:\n";
	print "\tla s0, random_data_core0\n";
	#start write in file
	my $data_in_file = select(FILE_DATA);
	print "random_data_core0:\n";
	#stor write in file
	select($data_in_file);

	for(my $start_i = 0; $start_i < $start_0; $start_i++) {
		# Initialization data each sections for core 0
		data_initialize($start_array[$start_i][0], $start_array[$start_i][1]);
		$data_in_file = select(FILE_DATA);
		# Generate data in file
		generate_data($start_array[$start_i][1], 0);
		select($data_in_file);
	}
	$data_in_file = select(FILE_DATA);
	print ".word 0,0,0,0,0,0,0,0\n\n";
	select($data_in_file);
	print "\tjr ra\n\tnop\n";

	print "\ninit_core1:\n";
	print "\tla s0, random_data_core1\n";
	$data_in_file = select(FILE_DATA);
	print "random_data_core1:\n";
	select($data_in_file);

	for(my $start_i = $start_0; $start_i < $start_0 + $start_1; $start_i++) {
		# Initialization data each sections for core 1
		data_initialize($start_array[$start_i][0], $start_array[$start_i][1]);
		$data_in_file = select(FILE_DATA);
		# Generate data in file
		generate_data($start_array[$start_i][1], 1);
		select($data_in_file);
	}
	$data_in_file = select(FILE_DATA);
	print ".word 0,0,0,0,0,0,0,0\n\n";
	select($data_in_file);
	close(FILE_DATA);

	print "\tjr ra\n\tnop\n";

	owner_map();
}

# Initialization data
sub data_initialize {
	my ($start_addr, $total_size) = @_;
	my $start_addr_hex = get_hex($start_addr);

	print "\n\tdli s1, $start_addr_hex\n";
	print "\tli s2, $total_size\n";
	print "1:\tlw t0,(s0)\n";
	print "\tsw t0,(s1)\n";
	print "\tdaddiu s0, s0, 4\n";
	print "\tdaddiu s2, s2, -1\n";
	print "\tbne s2, r0, 1b\n";
	print "\tdaddiu s1, s1, 4\n";
	print "\tsw r0,(s1)\n";
	print "\tsw r0,4(s1)\n";
	print "\tsw r0,8(s1)\n";
	print "\tsw r0,12(s1)\n";
}

# Generate data in file
sub generate_data {
	my ($total_size, $core) = @_;
	my $rand_data = 0;

	if($core) {	#core 1
		for(my $i = 0; $i <=  $total_size; $i++) {
			$rand_data = sprintf(".word 0x%08X", int(rand(2 ** 32) | 0x88888888));
			print "$rand_data\n";
		}
	} else {	#core 0
		for(my $i = 0; $i <=  $total_size; $i++) {
			$rand_data = sprintf(".word 0x%08X", int(rand(2 ** 32) & 0x77777777));
			print "$rand_data\n";
		}
	}
}

# The shared sections on the places
sub owner_map {
	my $j = 0;
	for(my $i = 0; $i < @start_array; $i++) {
		my $addr = $start_array[$i][0];
		my $size = int(rand($size_sectors) + 1); #size 1-32
		my $rest_size = $start_array[$i][1] - $size;

		for ($j = 0; $addr + $size < $start_array[$i][0] + $start_array[$i][1]; $j++) {
			$pointer[$i][$j][0] = $addr;
			$pointer[$i][$j][1] = $size;
			$addr = $addr + $size;
			if ($start_array[$i][1] - $size > $size_sectors) {
				$size = int(rand($size_sectors) + 1); #fix 33 -> 39 for command cache
			} else {
				$size = int(rand($rest_size - 1) + 1);
			}
			$rest_size -= $size;
		}
	}
	generate_instructions();
}

# Generate instructions
sub generate_instructions {
	my $instr = 0;

	for (my $i = 0; $i < @pointer; $i++) {
		my $k0 = 0;
		my $k1 = 0;
		my $ch = 0;

		for (my $j = 0; $j < @{$pointer[$i]}; $j++) {
			$ch = (($ch - 1) ** 2) ** 0.5; #inversion select core
			my $new_addr = $pointer[$i][$j][0];
			while ($new_addr < ($pointer[$i][$j][0] + $pointer[$i][$j][1])) {
				# Get offset
				my $offset = $new_addr - $pointer[$i][0][0];

				# Get size double, word, half, byte
				if ($new_addr % 8 == 0 && $new_addr + 8 < $pointer[$i][$j][0] + $pointer[$i][$j][1]) {
					$new_addr += 8;
					$instr = "d";
				} elsif ($new_addr % 4 == 0 && $new_addr + 4 < $pointer[$i][$j][0] + $pointer[$i][$j][1]) {
					$new_addr += 4;
					$instr = "w";
				} elsif ($new_addr % 2 == 0 && $new_addr + 2 < $pointer[$i][$j][0] + $pointer[$i][$j][1]) {
					$new_addr += 2;
					$instr = "h";
				} else {
					$new_addr += 1;
					$instr = "b";
				}

				# Array with instructions and offset for cores
				if ($ch == 0) {
					$total_array[0][$i][$k0][0] = $instr;
					$total_array[0][$i][$k0][1] = $offset;
					$k0++;
				} elsif ($ch == 1) {
					$total_array[1][$i][$k1][0] = $instr;
					$total_array[1][$i][$k1][1] = $offset;
					$k1++;
				}
			}
		}
	}
}

sub cic {
	for(my $i = 0; $i < @pointer; $i++) {
		for(my $j = 0; $j < @{$pointer[$i]}; $j++) {
			print "$pointer[$i][$j][1] ";
		}
		print "\n";
	}
}

sub check_equality {
	my ($n1, $n2) = @_;

	if ($n1 == $n2) {
		if($n1 > 1) {
			$n1--;
		} else {
			$n1++;
		}
	}

	return $n1, $n2;
}
sub get_test {
	my ($test_n, $rand_core, $num_iter_1, $num_iter_2, $core) = @_;

	my $all_core = 2; # number all core
	my @tmp_array = @total_array;
	my $rand_reg = 0;
	my $rand_reg_1 = int(rand($t_regs)); # t0 - t7
	my $rand_reg_2 = int(rand($t_regs)); # t0 - t7
	my $num_iter = 0;
	my $rand_status = int(rand(3)); # increment, decrement, random
	my $addr_hex_1 = get_hex( $start_array[$num_iter_1][0] );
	my $addr_hex_2 = get_hex( $start_array[$num_iter_2][0] );

	# 's' register base 1 != 's' register base 2
	($rand_reg_1, $rand_reg_2) = check_equality($rand_reg_1, $rand_reg_2);

	print "\ntest_00${test_n}_core$core:\n"; #FIX NUM CORE!!!
	print "\tdli s$rand_reg_1, $addr_hex_1\n";
	print "\tdli s$rand_reg_2, $addr_hex_2\n";

	# Замес или переворот массива.
	for(my $i = 0, my $inst = 0, my $inst_1 = 0, my $inst_2 = 0; $i < $instructions_number; $i++) {
		my $num_reg = int(rand($s_regs));

		# Random instructions load or store
		my $instr = int(rand(2));

		if ($instr) {
			$instr = "l";
		} else {
			$instr = "s";
		}

		if ( int(rand(2)) ) {
			$num_iter = $num_iter_1;
			$rand_reg = $rand_reg_1;
			$inst = $inst_1;
			$inst_1++;
			if ($inst >= $#{$total_array[$rand_core][$num_iter]}) {
				$inst_1 = 0;
			}
		} else {
			$num_iter = $num_iter_2;
			$rand_reg = $rand_reg_2;
			$inst = $inst_2;
			$inst_2++;
			if ($inst >= $#{$total_array[$rand_core][$num_iter]}) {
				$inst_2 = 0;
			}
		}

		if ($rand_status == 2 && $perc_rand[$rand_status] * 100 / $test_zones < $perc_r[$rand_status] * $all_core) {
			#Random mix array
			$inst = int(rand(@{$total_array[$rand_core][$num_iter]}));
		} elsif ( $rand_status == 1 && $perc_rand[$rand_status] * 100 / $test_zones < $perc_r[$rand_status] * $all_core) {
			#Reverse array
			@{$tmp_array[$rand_core][$num_iter]} = reverse @{$total_array[$rand_core][$num_iter]};
		} elsif ( $rand_status == 0 && $perc_rand[$rand_status] * 100 / $test_zones >= $perc_r[$rand_status] * $all_core) {
			$rand_status = 2;
			$i--;
			$inst--; #FIX ME!!!
		} else {
			$rand_status = 0;
		}
		my $offset_hex = get_hex($tmp_array[$rand_core][$num_iter][$inst][1]);
		print "\t$instr$tmp_array[$rand_core][$num_iter][$inst][0] t$num_reg, $offset_hex(s$rand_reg)\n ";
	}
	$perc_rand[$rand_status]++;

	print "\tjr ra\n\tnop\n";

}

sub get_main {
	my ($core) = @_;

	print "main_core$core:\n";

	# Enable 64 bit addr
	print "\tmfc0 t0, C0_STATUS\n";
	print "\tori t0, t0, 0x80\n";
	print "\tmtc0 t0, C0_STATUS\n";

	# Initialization core
	print "\tjal init_core$core\n";
	print "\tnop\n";
	#Synchronization cores
	print "\tSYNC_CORES\n";

	#Initialization 't' registers
	for(my $j = 0; $j < 8; $j++) {	#t0-t7
		printf("\tdli t$j, 0x%016X\n", (int(rand(4294967296)) << 32) | int(rand(4294967296)) );
	}

	#Synchronization cores
	print "\tSYNC_CORES\n";

	# Test link
	my $z = 0;
	for (my $n = 1; $n <= $test_zones; $n++) {
		print "\tjal test_00${n}_core$core\n";
		print "\tnop\n";
		print "\tSYNC_CORES\n";

		# Repeat test
#		if (int(rand(2)) && $z < $zone_reexec * 100 / $test_zones) {
		if (int(rand(100)) < $zone_reexec) {
			$zone_rexec[$n]++;
			print "\tjal test_00${n}_core$core //reexec test $n\n";
			print "\tnop\n";
			print "\tSYNC_CORES\n";
			$z++;
		}
	}

	print "\twait 0x7ffff\n\n";
	print "\t.rept 10; nop; .endr\n\n";
}

sub run_gen {
	print "#include \"regdef_k64.h\"\n";
	print "#include \"kernel_k64.h\"\n\n";
	print "\t.macro SYNC_CORES\n";
	print "\t.set noreorder\n";
	print "\tmfc0 s5,\$15,1\n";
	print "\tandi s5,s5,1\n";
	print "\tdaddiu s5,s5,1        // (s5=1 for CORE0) | (s5=2 for CORE1)\n";
	print "\tlui s6,0xbb00\n";
	print "\tsw s5,0x154(s6)    // write-only bit\n";
	print "\tsync\n";
	print "\tssnop            // writing 1 will stop CORE0, writing 2 will stop CORE1\n";
	print "\tssnop            // writing 1 after 2, or 2 after 1 will release both cores\n";
	print "\t.endm\n";

	print ".set noreorder\n";
	print ".text\n";
	print ".globl\t_start\n";
	print "\t.org 0x0000\n";
	print "\tmfc0 t0, \$15, 1\n";
	print "\tandi t0, t0, 0xf\n";
	print "\tbeq t0, r0, main_core_0_\n\tnop\n";
	print "\tj main_core1\n\tnop\n";
	print "main_core_0_:\tj main_core0\n\tnop\n";
	print "\n";

	# Start generate address and size
	start();

	# Generate tests
	for (my $n = 1; $n <= $test_zones; $n++) {
		my $rand_core = int(rand(2));
		my $num_iter_0_1 = int(rand(@{$total_array[$rand_core]})); #for baze 1
		my $num_iter_0_2 = int(rand(@{$total_array[$rand_core]})); #for baze 2
		get_test($n, $rand_core, $num_iter_0_1, $num_iter_0_2, 0);

		$rand_core = (($rand_core - 1) ** 2) ** 0.5; #inversion select core
		my $num_iter_1_1 = int(rand(@{$total_array[$rand_core]})); #for baze 1
		my $num_iter_1_2 = int(rand(@{$total_array[$rand_core]})); #for baze 2
		# Disable false sharin
		if($disable_false_sharing) {
			($num_iter_1_1, $num_iter_0_1) = check_equality($num_iter_1_1, $num_iter_0_1);
			($num_iter_1_1, $num_iter_0_2) = check_equality($num_iter_1_1, $num_iter_0_2);
			($num_iter_1_2, $num_iter_0_1) = check_equality($num_iter_1_2, $num_iter_0_1);
			($num_iter_1_2, $num_iter_0_2) = check_equality($num_iter_1_2, $num_iter_0_2);
		}
		get_test($n, $rand_core, $num_iter_1_1, $num_iter_1_2, 1);
	}

	get_main(0);
	get_main(1);

	print "\n#include \"random_data.h\"\n";
#	print "$perc_rand[0] $perc_rand[1] $perc_rand[2] $test_zones \n";
}

run_gen();
