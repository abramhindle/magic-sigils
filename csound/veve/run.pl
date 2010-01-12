#!/usr/bin/perl
use strict;
use Time::HiRes qw(time);

my $path  = "$ENV{HOME}/project/magic-sigils/csound/veve";

use constant PI => 3.14159;
#my @drum_pitches = ( 7.01 7.02 7.03 7.04 7.05 6.09  );
my @drum_pitches = qw( 7.05 7.05 7.00 7.00 7.05 7.05 7.00 7.00 6.09 6.09 6.09);
my $global_pitch = 0;
my $tempo = 60;
my $ts = $tempo / 60;
$| = 1;
my $start_time = time();
my $nth = 5;
my $ticks = 0;
my $readstate = 0;
my @out = ();
open(FILE,"$path/head.sco");
my @head = <FILE>;
close(FILE);
open(FILE,">","output.sco");
print FILE @head;
print FILE $/;
print "f0 3600$/";
print FILE "f0 0$/";
my $drum_alive = 0;
my %bodies = ();

my $width = 328;
my $height = 248;
my $centerx = $width / 2;
my $centery = $height / 2;
my $ltime = time();
my $loop_time = 2.50;
while(my $line = <>) {
	my $time = time();
	chomp($line);
	if ($time - $ltime > $loop_time) {
            warn $line;
		my @path = split(/\s+/, $line);
		{
			my @path = @path;
			my $lx = shift @path;
			my $ly = shift @path;
			my $plen = @path / 2;
			my $t = 0;
			while (@path) {
				my $x = shift @path;		
				my $y = shift @path;
				my $xv = $x - $lx;		
				my $yv = $y - $ly;		
                                my $dur = $loop_time * ($xv * $xv + $yv * $yv) / ($centerx * $centerx + $centery * $centery) + 0.1;
				my $theta = atan2($xv,$yv);
				my $start = 0.01 + $t * $loop_time * 0.9 / $plen;
				my $duration = 1.5 * $dur ; #0.1 + 0.4 * abs(cos($x * $y));
				my $loudness = 1000 * abs($y - $centery) / $centery;
				my $pitch = (6.000 +  $theta / PI + 3.0 * log(1.0 + ($xv*$xv+$yv*$yv))/13.0);
                                ccomment(join(" ",$x,$y,$xv,$yv,$dur,$theta,$dur,$loudness));
				csprint("1902",$start,$duration,
					$loudness, $pitch,
					0.9, 0.136, 0.45, 0.40);
                                if ($dur < 0.11) {
                                    play_drum($start, $x, $y, $xv, $yv);
                                }
	
				$lx = $x;
				$ly = $y;
					$t++;
			}
		}
		$ltime = $time;
	}


	##warn "$ticks $readstate";
	#chomp $line;
	#if ($line =~ /^Tick/) {
	#	if ($readstate) {
	#		process(@out);
	#		@out = ();
	#	}
	#	$ticks++;
	#	# read every $nth
	#	$readstate = (($ticks % $nth) == 0);
	#} elsif ($readstate) {
	#	push @out,[ split(/\s+/,$line) ];
	#}
}
sub min { ($_[0] > $_[1])?$_[1]:$_[0] }
sub process {
	my @elms = @_;
	foreach my $elm (@elms) {
		my ($id,$mass,$radius,$x,$y,$xv,$yv) = @$elm;
		#csprint("1902",0.10,0.1,1000,$mass,$radius,$x,$y,$xv,$yv);
		$bodies{$id}->{instrument} ||= choose(qw(1902 1902 1902 1903 1903 1903  1904));
		my $instrument = $bodies{$id}->{instrument};
		$bodies{$id}->{alive} = $ticks;
		$bodies{$id}->{elm} = $elm;
		my $theta = atan2($xv,$yv);
		if ($instrument == 1902) {
			csprint("1902",0.01,0.1 + 0.4 * abs(cos($mass * $radius)),
				(1000*log(1.0+$mass * $radius)/25.0),
				(6.000 +  $theta / PI + 3.0 * log(1.0 + ($xv*$xv+$yv*$yv)/$radius)/24.0),
				0.9, 0.136, 0.45, 0.40);
		} elsif ($instrument == 1904) {
			play_drum(@$elm);
		} elsif ($instrument == 1903) {
			#         START  DUR    AMP      PITCH   PRESS  FILTER     EMBOUCHURE  REED TABLE
			# i 1903    0    16     6000      8.00     1.5  1000         .2            1
			my $mag = sqrt(($xv * $xv) + ($yv * $yv));
			my $dur = 0.4 + 0.2 * ($xv / $mag);
			my $amp = 200 + 3000*abs(cos($mass * $radius * $yv * $yv));
			my $pitch = 7.0 + 1.5 * $theta / PI;
			my $filter = 800 + min(20 * log($mass),300);
			my $pressure = 1.0 + 0.1 * log($mass)/30.0 + 0.9  * $theta / PI;
			csprint("1903", 0.01, $dur,
					$amp,
					$pitch,
					$pressure,
					$filter,
					0.2,
					1);
		}
	}
	foreach my $id (keys %bodies) {
		#did a star die?
		if ($bodies{$id}->{alive} < $ticks) {
			play_drum(@{$bodies{$id}->{elm}});
			delete $bodies{$id};
		}
	}
}
sub choose { return @_[rand(@_)]; }
sub play_drum {
	my ($time,$x,$y,$xv,$yv) = @_;
	my $nowtime = time();
	if ($nowtime - $drum_alive > 128) {
		# ; DRUM 1
		# ;       START  DUR  AMP    PITCH PREFILTER  TUBELENGTH  FEEDBACK1  FEEDBACK2
		# i 1905  0     128  160    5.11   100           4          4.4        4.3
		csprint("1905",0,128,160,5.11,100,4,4.4,4.3);
		$drum_alive = $nowtime;
	}
	my $pitch = $global_pitch++;
	my $amp = (1000*log($x * $y + 1.0 + $xv*$xv)/12.0),
	my $pitch = $drum_pitches[$pitch % scalar(@drum_pitches)];
	my $duration = 0.3 + 0.2 * cos($x * $y); 
	my $prefilter = 100 + 20*rand();
	csprint("1904",0.01+$time,$duration,$amp,$pitch,$prefilter);
}
sub ccomment {
    warn @_;
    print FILE ";",@_,$/;
}
sub csprint {
	my ($instr,$time,$dur,@o) = @_;
	my ($rt,$out) = map {
		my $time = $_;
		my $str = join(" ",("i$instr",(map { sprintf('%0.3f',$_) } ($time,$dur,@o)))).$/;
		$str;
	} ($time, $time + $ts * ((time() - $start_time)));
	warn $rt;
	print $rt;
	print FILE $out;
}

