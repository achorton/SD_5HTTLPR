#*****************************************************************************
#* 5-HTTLPR GxE analysis regession results files assembler
#* version 1
#*****************************************************************************
#*
#* created by Amy Horton and Rob Culverhouse
#* 08/01/13
#*
#*****************************************************************************
#!/usr/bin/env perl -w

use strict;
use Data::Dumper;
use List::Util qw(sum);
use File::Glob ':globally';

my $wd=$ARGV[0];
my @SITES;
my $arrayref;
my $hashref;

my $Ftest_thresh=0.05;

if (scalar @ARGV > 1) {
    shift @ARGV;
    @SITES=@ARGV;
}
unless ($wd){
    $wd="/home/amyh/SD_5HTTLPR/SD_5HTTLPR_beta7/";
#    $wd="/Users/achorton/SD_5HTTLPR/SD_5HTTLPR_beta7/";
}
unless (scalar @SITES) {
    @SITES = glob("${wd}Results\*");
#    @SITES=( "TEST_EA" );
}

my %total_regr;
for (my @old_files=glob("${wd}meta_raw_files/*.csv")) {
    unlink $_;
}

for my $SITE (@SITES) {
    my $scriptfile;
    my @scriptfiles=glob("${SITE}/SD_5HTTLPR_script_v8*.R");
    if (scalar @scriptfiles==1) { $scriptfile=$scriptfiles[0]; }
    my @regression_dirs=( "${SITE}/Basic/",
			  "${SITE}/Stress/Moderated/" );
    my $moderated_file="${SITE}/analyses_showing_moderating_effect.txt";
    my $unmoderated_file="${SITE}/analyses_negating_moderating_effect.txt";

    my $longitudinal_data;
    my $reporting_dep;
    my $dep_q_system;
    my $dep_q_interview;
    my $dep_q_query_span;
    my $dep_dx_system;
    my $dep_dx_interview;
    my $dep_dx_query_span;
    my $reporting_childhood_mal;
    my $reporting_life_stress;
    my $life_stress_time_period;
    my $child_mal_q_system;
    my $life_stress_q_system;
    my $interview_stress_q;
    my $child_mal_exp_system;
    my $life_stress_exp_system;
    my $interview_stress_exp;
    my $haplotypes_available;

    open(SCRIPT,"<",$scriptfile) or die "Cannot open script file $scriptfile to get dataset descriptor variables:$!\n";
    while(my $line=<SCRIPT>) {
	chomp $line;
	my @line;
	if ($line) {
	    @line=split(/\"/,$line);
	}


	if ($line[0]=~m/^\s*dep_dx_system\s\=\s/) {
	    $total_regr{$SITE}{"depression_dx_system"} = $line[1];
	    $dep_dx_system=$line[1];
	}   
	elsif ($line[0]=~m/^\s*dep_q_system\s\=\s/) {
	    $total_regr{$SITE}{"depression_q_system"} = $line[1];
	    $dep_q_system=$line[1];
	}   
	elsif ($line[0]=~m/^\s*child_mal_exp_system\s\=\s/) {
	    $total_regr{$SITE}{"child_mal_exp_system"} = $line[1];
	    $child_mal_exp_system=$line[1];
	} 
	elsif ($line[0]=~m/^\s*child_mal_q_system\s\=\s/) {
	    $total_regr{$SITE}{"child_mal_q_system"} = $line[1];
	    $child_mal_q_system=$line[1];
	} 
	elsif ($line[0]=~m/^\s*life_stress_exp_system\s\=\s/) {
	    $total_regr{$SITE}{"life_stress_exp_system"} = $line[1];
	    $life_stress_exp_system=$line[1];
	} 
	elsif ($line[0]=~m/^\s*life_stress_q_system\s\=\s/) {
	    $total_regr{$SITE}{"life_stress_q_system"} = $line[1];
	    $life_stress_q_system=$line[1];
	} 
	elsif ($line[0]=~m/^\s*longitudinal_data\s\=\s/) {
	    $total_regr{$SITE}{"longitudinal_data"} = $line[1];
	    $longitudinal_data=$line[1];
	}
	elsif ($line[0]=~m/^\s*reporting_childhood_mal\s\=\s/) {
	    $total_regr{$SITE}{"reporting_childhood_mal"} = $line[1];
	    $reporting_childhood_mal=$line[1];
	} 
	elsif ($line[0]=~m/^\s*reporting_life_stress\s\=\s/) {
	    $total_regr{$SITE}{"reporting_life_stress"} = $line[1];
	    $reporting_life_stress=$line[1];
	} 
	elsif ($line[0]=~m/^\s*reporting_dep\s\=\s/) {
	    $total_regr{$SITE}{"reporting_dep"} = $line[1];
	    $reporting_dep=$line[1];
	} 
	elsif ($line[0]=~m/^\s*dep_dx_interview\s\=\s/) {
	    $total_regr{$SITE}{"dep_dx_interview"} = $line[1];
	    $dep_dx_interview=$line[1];
	} 
	elsif ($line[0]=~m/^\s*haplotypes_available\s\=\s/) {
	    $total_regr{$SITE}{"haplotypes_available"} = $line[1];
	    $haplotypes_available=$line[1];
	} 
	elsif ($line[0]=~m/^\s*dep_dx_query_span\s\=\s/) {
	    $total_regr{$SITE}{"dep_dx_query_span"} = $line[1];
	    $dep_dx_query_span=$line[1];
	} 
	elsif ($line[0]=~m/^\s*dep_q_interview\s\=\s/) {
	    $total_regr{$SITE}{"dep_q_interview"} = $line[1];
	    $dep_q_interview=$line[1];
	} 
	elsif ($line[0]=~m/^\s*dep_q_query_span\s\=\s/) {
	    $total_regr{$SITE}{"dep_q_query_span"} = $line[1];
	    $dep_q_query_span=$line[1];
	} 
	elsif ($line[0]=~m/^\s*interview_stress_exp\s\=\s/) {
	    $total_regr{$SITE}{"interview_stress_exp"} = $line[1];
	    $interview_stress_exp=$line[1];
	} 
	elsif ($line[0]=~m/^\s*interview_stress_q\s\=\s/) {
	    $total_regr{$SITE}{"interview_stress_q"} = $line[1];
	    $interview_stress_q=$line[1];
	} 
	elsif ($line[0]=~m/^\s*life_stress_time_period\s\=\s/) {
	    $total_regr{$SITE}{"life_stress_time_period"} = $line[1];
	    $life_stress_time_period=$line[1];
	} 
    }
    close(SCRIPT);

    open(MODERATED,">",$moderated_file) or die "Cannot open script file $moderated_file to write moderated effects:$!\n";
    open(UNMODERATED,">",$unmoderated_file) or die "Cannot open script file $unmoderated_file to write dataset non-moderated effects:$!\n";

    for my $regdir (@regression_dirs) {
	my @filelist=glob("${regdir}*");
	for my $regression_file (@filelist){

	    my $dep_dx_label;
	    my $dep_q_label;

	    if($dep_dx_system eq "DSM4") {$dep_dx_label= 'DSM-IV MDD Diagnosis';}
	    elsif($dep_dx_system eq "ICD10") {$dep_dx_label= 'ICD10 MDD Diagnosis';}
	    else {$dep_dx_label='Depression Diagnosis';}

	    if($dep_q_system eq "DSM4 SYMPTOM COUNT") {
		$dep_q_label='DSM-IV MDD Symptom Count';}
	    elsif($dep_q_system eq "ICD10 SYMPTOM COUNT") {
		$dep_q_label='ICD10 MDD Symptom Count';}
	    else {
		$dep_q_label='Depression Symptom Count';}

	    open(REGR,"<",$regression_file) or die "Cannot open analysis file $regression_file:$!\n";
	    my @vars;
	    my %vars;
	    my $sample_sex;
	    my $outcome;
	    my $stratified;
	    my $age;
	    if ($regression_file=~m/_YAdult_/) {
		$age="YA";
	    }
	    elsif ($regression_file=~m/_AllAges_/) { $age="All"; }

	  READER: while(my $line=<REGR>) {
	      chomp $line;
	      my @line;
	      if (($line eq " ") ||
		  ($line=~m/^\s*$/) ||
		  ($line eq "")) { next; }
	      if ($line=~m/^STOP/) { 
		  $outcome="";
		  @vars=();
		  next READER; 
	      }
	      if ($line=~m/^\=\=\=\=\=\s/) {
		  if ($line=~m/\smales\s/) {
		      $sample_sex="males-only";
		  }
		  elsif ($line=~m/females/) {
		      $sample_sex="females-only";
		  }
		  elsif ($line=~m/combined/) {
		      $sample_sex="combined-sexes";
		  }
		  else { $sample_sex=""; }
	      }
	      elsif ($line=~m/^START/) {
		  my @terms=split(/\s+/, $line);
		  my $snp;
		  $outcome=$terms[1];
		  if ($line=~m/stratified/) { 
		      $stratified=1; 
		      $snp=$terms[$#terms];
		  } 
		  else { $stratified=0; }
		      
		  for (my $i=3;$i<=($#terms - 3*$stratified);$i+=2){
		      $terms[$i]=~s/\:/_x_/g;
		      $terms[$i]=~s/[\(\)]//g;
		      push @vars,$terms[$i];
		  }
		  for (my $i=1 + $#terms - 3*$stratified;$i<=17;$i+=2){
		      push @vars,"null";
		  }
		  $line=<REGR>;
		  chomp $line;
		  if (($line eq " ") ||
		      ($line=~m/^\s*$/) ||
		      ($line eq "")) {
		      $line=<REGR>;
		      chomp $line;
		  }
		  if ($line=~m/^STOP/) { 
		      $outcome="";
		      @vars=();
		      next READER; 
		  }

		  if ($line=~m/^Res/) {
		      $line=<REGR>; 
		      chomp $line;
		      my @Ftest_smaller=split(/\t/,$line);
		      $total_regr{$SITE}{$age}{$outcome}{$vars[0]}{$vars[1]}{$vars[2]}{$vars[3]}{$vars[4]}{$vars[5]}{$vars[6]}{$vars[7]}{$stratified}{$sample_sex}{"Ftest_smaller"} = { "Residual DF"=> $Ftest_smaller[0],
																							"Residual" =>	$Ftest_smaller[1],
																							"Deviance Df" =>	$Ftest_smaller[2] };
			  
		      $line=<REGR>; 
		      chomp $line;
		      my @Ftest_larger=split(/\t/,$line);
		      $total_regr{$SITE}{$age}{$outcome}{$vars[0]}{$vars[1]}{$vars[2]}{$vars[3]}{$vars[4]}{$vars[5]}{$vars[6]}{$vars[7]}{$stratified}{$sample_sex}{"Ftest_larger"}={ "Residual DF"=> $Ftest_larger[0],
																						     "Residual" =>	$Ftest_larger[1],
																						     "Deviance Df" =>	$Ftest_larger[2],
																						     "Deviance" => $Ftest_larger[3],
																						     "P_Chisq" =>	$Ftest_larger[4] };		    
		      if ($Ftest_larger[4]<=$Ftest_thresh) {
			  print MODERATED "Ftest\: $Ftest_larger[5] ",$outcome," = ","@vars"," = sample = $sample_sex\n";
		      }
		      else {
			  print UNMODERATED "Ftest\: $Ftest_larger[5] ",$outcome," = ","@vars"," = sample = $sample_sex\n";
		      }
		      <REGR>;$line=<REGR>;
		      chomp $line;
		      if($line=~m/^START/){
			  <REGR>;$line=<REGR>;
			  chomp $line;
		      }

		      my @subvar;
		      if (my ($match)=($line=~m/^N\=(\d+)/)) {
			  for my $var (@vars) {
			      my $subvar;
			      if ($var=~m/_x_/) {
				  $subvar ="null";
			      }
			      else { $subvar=$var; }
			      push @subvar, $subvar;
			  }

			  $total_regr{$SITE}{$age}{$outcome}{$subvar[0]}{$subvar[1]}{$subvar[2]}{$subvar[3]}{$subvar[4]}{$subvar[5]}{$subvar[6]}{$subvar[7]}{$stratified}{$sample_sex}{"N"}=$match;
			  <REGR>;
		      }
		      for (my $i=0;$i<=$#vars;$i){  ##########NOTE B1
			  $line=<REGR>;
			  chomp $line;
			  last if (($line eq " ") ||
				   ($line=~m/^\s*$/) ||
				   ($line eq ""));

			  my $hashref=\%total_regr;
			  $i=match_line_to_var($i,$SITE,$age,$outcome,$stratified,$sample_sex,\@vars,\@subvar,$hashref,$line);
		      }

		      <REGR>;
		      $line=<REGR>;		      
		      chomp $line;
		      if (($line eq " ") ||
			  ($line=~m/^\s*$/) ||
			  ($line eq "")) { $line=<REGR>; chomp $line; }  ######### added 10/28/14
		      if ($line=~m/^STOP/) {
			  $outcome="";
			  @vars=();
			  next READER;
		      }

		      if (my ($match)=($line=~m/^N\=(\d+)/)) {
			  $total_regr{$SITE}{$age}{$outcome}{$vars[0]}{$vars[1]}{$vars[2]}{$vars[3]}{$vars[4]}{$vars[5]}{$vars[6]}{$vars[7]}{$stratified}{$sample_sex}{"N"}=$match;
			  <REGR>;
		      }
#		      for (my $i=0;$i<=scalar @vars + 3;$i++){
		      for (my $i=0;$i<=scalar @vars + 3;$i){
			  my $line=<REGR>;
			  chomp $line;
			  if (($line eq " ") ||
			      ($line=~m/^\s*$/) ||
			      ($line eq "")) {
			      next;
			  }
			  elsif ($line=~m/^STOP/) {
			      $outcome="";
			      @vars=();
			      next READER;
			  }

			  my $hashref=\%total_regr;
			  $i=(match_line_to_var($i,$SITE,$age,$outcome,$stratified,$sample_sex,\@vars,\@vars,$hashref,$line) );

		      }
		  }

		  elsif ((my ($match0,$match1,$match2)=$line=~m/^N0\=(\d+)\=N1\=(\d+)\=N2\=(\d+)/) && $stratified) {
		      $total_regr{$SITE}{$age}{$outcome}{$vars[0]}{$vars[1]}{$vars[2]}{$vars[3]}{$vars[4]}{$vars[5]}{$vars[6]}{$vars[7]}{$stratified}{$sample_sex}{"N0"}=$match0;
		      $total_regr{$SITE}{$age}{$outcome}{$vars[0]}{$vars[1]}{$vars[2]}{$vars[3]}{$vars[4]}{$vars[5]}{$vars[6]}{$vars[7]}{$stratified}{$sample_sex}{"N1"}=$match1;
		      $total_regr{$SITE}{$age}{$outcome}{$vars[0]}{$vars[1]}{$vars[2]}{$vars[3]}{$vars[4]}{$vars[5]}{$vars[6]}{$vars[7]}{$stratified}{$sample_sex}{"N2"}=$match2;

		      <REGR>;
		      my $i=0;
		      while (my $line=<REGR>){
			  chomp $line;
			  if (($line eq " ") ||
			      ($line=~m/^\s*$/) ||
			      ($line eq "")) {
			      next READER;
			  }
			  if ($line=~m/^STOP/) { 
			      $outcome="";
			      @vars=();
			      next READER; 
			  }

			  my $hashref=\%total_regr;
			  $i=Strat_match_line_to_var($i,$SITE,$age,$outcome,$stratified,$sample_sex,$snp,\@vars,$hashref,$line);
			  if ($vars[$i] eq "null") { $i=0; }

		      }
		  }
		  elsif (my ($match)=($line=~m/^N\=(\d+)/)) {
		      $total_regr{$SITE}{$age}{$outcome}{$vars[0]}{$vars[1]}{$vars[2]}{$vars[3]}{$vars[4]}{$vars[5]}{$vars[6]}{$vars[7]}{$stratified}{$sample_sex}{"N"}=$match;
		      <REGR>;
		      my $i=0;
		      while (my $line=<REGR>) {
			  chomp $line;
			  if (($line eq " ") ||
			      ($line=~m/^\s*$/) ||
			      ($line eq "")) {
			      next;
			  }
			  if ($line=~m/^STOP/) { 
			      $outcome="";
			      @vars=();
			      next READER; 
			  }

			  my $hashref=\%total_regr;
			  $i=Basic_match_line_to_var($i,$SITE,$age,$outcome,$stratified,$sample_sex,\@vars,$hashref,$line);

		      }
		  }
	      }
	  }
	}
    }
    close MODERATED;
    close UNMODERATED;
}


my %first_site=set_firstsite_flags(\%total_regr);  

for my $SITE (keys %total_regr) {#{$SITE}
    for my $age (keys %{ $total_regr{$SITE} }) {#{$age}
	unless (($age !~ m/system$/) &&
		($age !~ m/data$/) &&
		($age !~ m/^reporting_/) &&
		($age !~ m/_interview$/) &&
		($age !~ m/^haplotypes_available$/) &&
		($age !~ m/_query_span$/) &&
		($age !~ m/^interview_/) &&
		($age !~ m/_period$/)) {
	    next;
	}

	for my $outcome (keys %{ $total_regr{$SITE}{$age} }) {#{$outcome}
	    print "Outputting to file $outcome\n";
	    if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		if ($total_regr{$SITE}{"depression_dx_system"} eq "DSM4") {
		    my $cond = "DepDx_DSM4_";
		    traverse_hash($cond,$SITE,$age,$outcome,\%{ $total_regr{$SITE}{$age}{$outcome} },\%first_site);
		}
		if ($total_regr{$SITE}{"depression_dx_system"} eq "ICD10") {
		    my $cond = "DepDx_ICD10_";
		    traverse_hash($cond,$SITE,$age,$outcome,\%{ $total_regr{$SITE}{$age}{$outcome} },\%first_site);
		}
		if ($total_regr{$SITE}{"depression_dx_system"}=~m/^OTHER/) {
		    my $cond = "DepDx_Other_";
		    traverse_hash($cond,$SITE,$age,$outcome,\%{ $total_regr{$SITE}{$age}{$outcome} },\%first_site);
		}
		if (($total_regr{$SITE}{"depression_dx_system"} eq "DSM4") | ($total_regr{$SITE}{"depression_dx_system"} eq "ICD10")) {
		    my $cond = "DepDx_DSM4ICD10_";
		    traverse_hash($cond,$SITE,$age,$outcome,\%{ $total_regr{$SITE}{$age}{$outcome} },\%first_site);
		}
		if ($total_regr{$SITE}{"depression_dx_system"} ne "NOT_AVAILABLE") {
		    my $cond = "DepDx_ANY_";
		    traverse_hash($cond,$SITE,$age,$outcome,\%{ $total_regr{$SITE}{$age}{$outcome} },\%first_site);
		}
	    }
	    elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		if ($total_regr{$SITE}{"depression_q_system"} eq "DSM4_SYMPTOM_COUNT") {
		    my $cond = "DepQ_DSM4_";
		    traverse_hash($cond,$SITE,$age,$outcome,\%{ $total_regr{$SITE}{$age}{$outcome} },\%first_site);
		}
		if ($total_regr{$SITE}{"depression_q_system"} eq "ICD10_SYMPTOM_COUNT") {
		    my $cond = "DepQ_ICD10_";
		    traverse_hash($cond,$SITE,$age,$outcome,\%{ $total_regr{$SITE}{$age}{$outcome} },\%first_site);
		}
		if ($total_regr{$SITE}{"depression_q_system"}=~m/^OTHER/) {
		    my $cond = "DepQ_Other_";
		    traverse_hash($cond,$SITE,$age,$outcome,\%{ $total_regr{$SITE}{$age}{$outcome} },\%first_site);
		}
		if (($total_regr{$SITE}{"depression_q_system"} eq "DSM4_SYMPTOM_COUNT") | ($total_regr{$SITE}{"depression_q_system"} eq "ICD10_SYMPTOM_COUNT")) {
		    my $cond = "DepQ_DSM4ICD10_";
		    traverse_hash($cond,$SITE,$age,$outcome,\%{ $total_regr{$SITE}{$age}{$outcome} },\%first_site);
		}
		if ($total_regr{$SITE}{"depression_q_system"} ne "NOT_AVAILABLE") {
		    my $cond = "DepQ_ANY_";
		    traverse_hash($cond,$SITE,$age,$outcome,\%{ $total_regr{$SITE}{$age}{$outcome} },\%first_site);
		}
	    }
	}
    }
}

exit;




sub print_stratified {
    my ($hash_ref,$cond,$SITE,$age,$outcome,$vars0,$vars1,$vars2,$vars3,$vars4,$vars5,$vars6,$vars7,$stratified,$sample_sex,$parameter,$byvar_value)=@_;

    my $outfile="${wd}meta_raw_files/${vars0}__${vars1}__${vars2}__${vars3}__${vars4}__${vars5}__${vars6}__${vars7}.csv";
    $outfile=~s/\:/_x_/;
    open(OUT,">>",$outfile) || die "Cannot open outputfile $outfile:$!\n";
    my $model;
    if ((($vars0 eq 'add_5http' || $vars1 eq 'add_5http' || $vars2 eq 'add_5http' || $vars3 eq 'add_5http' || $vars4 eq 'add_5http') ||
	 ($vars0 eq 'add_rs25531' || $vars1 eq 'add_rs25531' || $vars2 eq 'add_rs25531' || $vars3 eq 'add_rs25531' || $vars4 eq 'add_rs25531'))) {
	$model = "add";
    }
    elsif ((($vars0 eq 'Ldum1_5http' || $vars1 eq 'Ldum1_5http' || $vars2 eq 'Ldum1_5http' || $vars3 eq 'Ldum1_5http' || $vars4 eq 'Ldum1_5http') ||
	 ($vars0 eq 'L_Adum1_rs25531' || $vars1 eq 'L_Adum1_rs25531' || $vars2 eq 'L_Adum1_rs25531' || $vars3 eq 'L_Adum1_rs25531' || $vars4 eq 'L_Adum1_rs25531') ||
	 ($vars0 eq 'Ldum2_5http' || $vars1 eq 'Ldum2_5http' || $vars2 eq 'Ldum2_5http' || $vars3 eq 'Ldum2_5http' || $vars4 eq 'Ldum2_5http' || 
	  $vars5 eq 'Ldum2_5http') ||
	 ($vars0 eq 'L_Adum2_rs25531' || $vars1 eq 'L_Adum2_rs25531' || $vars2 eq 'L_Adum2_rs25531' || $vars3 eq 'L_Adum2_rs25531' || $vars4 eq 'L_Adum2_rs25531' || 
	  $vars5 eq 'L_Adum2_5http'))) {
	$model = "dum";
    }
    elsif (($vars0 eq 'Ldom_5http' || $vars1 eq 'Ldom_5http' || $vars2 eq 'Ldom_5http' || $vars3 eq 'Ldom_5http' || $vars4 eq 'Ldom_5http') ||
	   ($vars0 eq 'L_Adom_rs25531' || $vars1 eq 'L_Adom_rs25531' || $vars2 eq 'L_Adom_rs25531' || $vars3 eq 'L_Adom_rs25531' || $vars4 eq 'L_Adom_rs25531')) {
	$model = "dom";
    }
    elsif (($vars0 eq 'Lrec_5http' || $vars1 eq 'Lrec_5http' || $vars2 eq 'Lrec_5http' || $vars3 eq 'Lrec_5http' || $vars4 eq 'Lrec_5http') ||
	   ($vars0 eq 'L_Arec_rs25531' || $vars1 eq 'L_Arec_rs25531' || $vars2 eq 'L_Arec_rs25531' || $vars3 eq 'L_Arec_rs25531' || $vars4 eq 'L_Arec_rs25531')) {
	$model = "rec";
    }
    else { $model = "no_snp"; }

    my $beta=${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}{$stratified}{$sample_sex}{$parameter}{$byvar_value}{"estimate"};
    my $se=${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}{$stratified}{$sample_sex}{$parameter}{$byvar_value}{"se"};

    my $N;
    if($byvar_value == 0) {
	$N=${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}{$stratified}{$sample_sex}{"N0"};
    }
    if($byvar_value == 1) {
	$N=${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}{$stratified}{$sample_sex}{"N1"};
    }
    if($byvar_value == 2) {
	$N=${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}{$stratified}{$sample_sex}{"N2"};
    }
    $SITE=~s/${wd}//;
    my @bits=split("_",$SITE);
    my $study=$bits[1];
    for (my $i=2;$i<(scalar @bits) -1; $i++) {
	$study=$study."_".$bits[$i];
    }
    my $pop=$bits[(scalar @bits) -1];

    print OUT "${study}_${pop}\t${age}\tn_study\t${byvar_value}\t${parameter}\t${outcome}\t${model}\t${beta}\t${se}\t${N}\t${sample_sex}\t${cond}\n";
#    print OUT "${SITE}\t${age}\tn_study\t${byvar_value}\t${parameter}\t${outcome}\t${model}\t${beta}\t${se}\t${N}\t${sample_sex}\t${cond}\n";
    close OUT;
}

sub print_other {
    my ($hash_ref,$cond,$SITE,$age,$outcome,$vars0,$vars1,$vars2,$vars3,$vars4,$vars5,$vars6,$vars7,$stratified,$sample_sex,$parameter)=@_; 

    my $outfile="${wd}meta_raw_files/${vars0}__${vars1}__${vars2}__${vars3}__${vars4}__${vars5}__${vars6}__${vars7}.csv";
    $outfile=~s/\:/_x_/;

    open(OUT,">>",$outfile) || die "Cannot open outputfile $outfile:$!\n";
    my $model;
    if ((($vars0 eq 'add_5http' || $vars1 eq 'add_5http' || $vars2 eq 'add_5http' || $vars3 eq 'add_5http' || $vars4 eq 'add_5http') ||
	 ($vars0 eq 'add_rs25531' || $vars1 eq 'add_rs25531' || $vars2 eq 'add_rs25531' || $vars3 eq 'add_rs25531' || $vars4 eq 'add_rs25531'))) {
	$model = "add";
    }
    elsif ((($vars0 eq 'Ldum1_5http' || $vars1 eq 'Ldum1_5http' || $vars2 eq 'Ldum1_5http' || $vars3 eq 'Ldum1_5http' || $vars4 eq 'Ldum1_5http') ||
	 ($vars0 eq 'L_Adum1_rs25531' || $vars1 eq 'L_Adum1_rs25531' || $vars2 eq 'L_Adum1_rs25531' || $vars3 eq 'L_Adum1_rs25531' || $vars4 eq 'L_Adum1_rs25531') ||
	 ($vars0 eq 'Ldum2_5http' || $vars1 eq 'Ldum2_5http' || $vars2 eq 'Ldum2_5http' || $vars3 eq 'Ldum2_5http' || $vars4 eq 'Ldum2_5http' || 
	  $vars5 eq 'Ldum2_5http') ||
	 ($vars0 eq 'L_Adum2_rs25531' || $vars1 eq 'L_Adum2_rs25531' || $vars2 eq 'L_Adum2_rs25531' || $vars3 eq 'L_Adum2_rs25531' || $vars4 eq 'L_Adum2_rs25531' || 
	  $vars5 eq 'L_Adum2_5http'))) {
	$model = "dum";
    }
    elsif (($vars0 eq 'Ldom_5http' || $vars1 eq 'Ldom_5http' || $vars2 eq 'Ldom_5http' || $vars3 eq 'Ldom_5http' || $vars4 eq 'Ldom_5http') ||
	   ($vars0 eq 'L_Adom_rs25531' || $vars1 eq 'L_Adom_rs25531' || $vars2 eq 'L_Adom_rs25531' || $vars3 eq 'L_Adom_rs25531' || $vars4 eq 'L_Adom_rs25531')) {
	$model = "dom";
    }
    elsif (($vars0 eq 'Lrec_5http' || $vars1 eq 'Lrec_5http' || $vars2 eq 'Lrec_5http' || $vars3 eq 'Lrec_5http' || $vars4 eq 'Lrec_5http') ||
	   ($vars0 eq 'L_Arec_rs25531' || $vars1 eq 'L_Arec_rs25531' || $vars2 eq 'L_Arec_rs25531' || $vars3 eq 'L_Arec_rs25531' || $vars4 eq 'L_Arec_rs25531')) {
	$model = "rec";
    }
    else { $model = "no_snp"; }
    my $beta=${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}{$stratified}{$sample_sex}{$parameter}{"estimate"};
    my $se=${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}{$stratified}{$sample_sex}{$parameter}{"se"};
    my $N=${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}{$stratified}{$sample_sex}{"N"};
    $SITE=~s/${wd}//;
    my @bits=split("_",$SITE);
    my $study=$bits[1];
    for (my $i=2;$i<(scalar @bits) -1; $i++) {
	$study=$study."_".$bits[$i];
    }
    my $pop=$bits[(scalar @bits) -1];
#    print $model,"\n";
    print OUT "${study}_${pop}\t${age}\tn_study\tfull\t${parameter}\t${outcome}\t${model}\t${beta}\t${se}\t${N}\t${sample_sex}\t${cond}\n";
#    print OUT "${SITE}\t${age}\tn_study\tfull\t${parameter}\t${outcome}\t${model}\t${beta}\t${se}\t${N}\t${sample_sex}\t${cond}\n";
    close OUT;
}

sub set_firstsite_flags {
    my $hash_ref = shift;
    my %first_site;

    for my $SITE (keys %{ $hash_ref }) {#{$SITE}
	for my $age (keys %{ ${ $hash_ref }{$SITE} }) {#{$outcome}
	    unless (($age !~ m/system$/) &&
		    ($age !~ m/data$/) &&
		    ($age !~ m/^reporting_/) &&
		    ($age !~ m/_interview$/) &&
		    ($age !~ m/^haplotypes_available$/) &&
		    ($age !~ m/_query_span$/) &&
		    ($age !~ m/^interview_/) &&
		    ($age !~ m/_period$/)) {
		next;
	    }
	    for my $outcome (keys %{ ${ $hash_ref }{$SITE}{$age} }) {#{$outcome}
		for my $vars0 (keys %{ ${ $hash_ref }{$SITE}{$age}{$outcome} }) {#{$vars[0]}
		    for my $vars1 (keys %{ ${ $hash_ref }{$SITE}{$age}{$outcome}{$vars0} }) {#{$vars[1]}
			for my $vars2 (keys %{ ${ $hash_ref }{$SITE}{$age}{$outcome}{$vars0}{$vars1} })  {#{$vars[2]}
			    for my $vars3 (keys %{ ${ $hash_ref }{$SITE}{$age}{$outcome}{$vars0}{$vars1}{$vars2} }) {#{$vars[3]}
				for my $vars4 (keys %{ ${ $hash_ref }{$SITE}{$age}{$outcome}{$vars0}{$vars1}{$vars2}{$vars3} }) {#{$vars[4]}
				    for my $vars5 (keys %{ ${ $hash_ref }{$SITE}{$age}{$outcome}{$vars0}{$vars1}{$vars2}{$vars3}{$vars4} }) {#{$vars[5]}
					for my $vars6 (keys %{ ${ $hash_ref }{$SITE}{$age}{$outcome}{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5} }) {#{$vars[6]}
					    for my $vars7 (keys %{ ${ $hash_ref }{$SITE}{$age}{$outcome}{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6} }) {#{$vars[7]}
						for my $stratified (keys %{ ${ $hash_ref }{$SITE}{$age}{$outcome}{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7} }) {#{$stratified}
						    $first_site{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}=1;
						}
					    }
					}
				    }
				}
			    }
			}
		    }
		}
	    }
	}
    }
    return %first_site;
}

sub traverse_hash {
    my $cond = shift;
    my $SITE = shift;
    my $age = shift;
    my $outcome = shift;
    my $hash_ref = shift;
    my $hash_ref2 = shift;

    for my $vars0 (keys %{ $hash_ref }) {#{$vars[0]}
	for my $vars1 (keys %{ ${ $hash_ref }{$vars0} }) {#{$vars[1]}
	    for my $vars2 (keys %{ ${ $hash_ref }{$vars0}{$vars1} })  {#{$vars[2]}
		for my $vars3 (keys %{ ${ $hash_ref }{$vars0}{$vars1}{$vars2} }) {#{$vars[3]}
		    for my $vars4 (keys %{ ${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3} }) {#{$vars[4]}
			for my $vars5 (keys %{ ${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4} }) {#{$vars[5]}
			    for my $vars6 (keys %{ ${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5} }) {#{$vars[6]}
				for my $vars7 (keys %{ ${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6} }) {#{$vars[7]}
				    for my $stratified (keys %{ ${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7} }) {#{$stratified}
					if (${ $hash_ref2 }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}) {
					    my $outfile="${wd}meta_raw_files/${vars0}__${vars1}__${vars2}__${vars3}__${vars4}__${vars5}__${vars6}__${vars7}.csv";
					    $outfile=~s/\:/_x_/;

					    unlink $outfile;
					    ${ $hash_ref2 }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}=0;
					    open(OUT,">",$outfile) || die "Cannot open outputfile $outfile:$!\n";
					    print OUT "namestudy\tage_group\tn_study\tstrat\tsnp\tphen\tmodel\tbeta\tse\tcombined\tsample_sex\tcond\n";
					    close OUT;
					}
					for my $sample_sex (keys %{ ${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}{$stratified} }) {#{$sample_sex}
					    if ($sample_sex ne "first_site"){
						for my $parameter (keys %{ ${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}{$stratified}{$sample_sex} }) {#{$parameter}
						    if ($parameter!~m/^Ftest_\w+er$/ && $parameter !~ m/N(\d+)/ && $parameter ne "N"){
							if ($stratified){
							    for my $byvar_value (keys %{ ${ $hash_ref }{$vars0}{$vars1}{$vars2}{$vars3}{$vars4}{$vars5}{$vars6}{$vars7}{$stratified}{$sample_sex}{$parameter} }) {  #{$byvar_value}
								print_stratified($hash_ref,$cond,$SITE,$age,$outcome,$vars0,$vars1,$vars2,$vars3,$vars4,$vars5,$vars6,$vars7,$stratified,$sample_sex,$parameter,$byvar_value); 
							    }
							}
							else { #{"feature"} 
							    print_other($hash_ref,$cond,$SITE,$age,$outcome,$vars0,$vars1,$vars2,$vars3,$vars4,$vars5,$vars6,$vars7,$stratified,$sample_sex,$parameter); 
							}
						    }# not "Ftest..."
						}#{$parameter}
					    }# not "first_site"
					}#{$sample_sex}
				    }#{$stratified}
				}#{$vars7}	
			    }#{$vars6}	
			}#{$vars5}	
		    }#{$vars4}	
		}#{$vars3}	
	    }#{$vars2}	
	}#{$vars1}	
    }#{$vars0}	
}

sub get_indices {
    my $file=shift;
    my $arrayref=shift;

    open(FH,"<",$file) || die "Cannot open get_indices input file $file:$!\n";
    my $header=<FH>;
    chomp $header;
    my @columns=split("\t",$header);

    my @index;

    for (my $i=0;$i<=$#{ $arrayref };$i++){
	for (my $j=0;$j<=$#columns;$j++){	
	    if ($columns[$j] eq ${ $arrayref }[$i]) { $index[$i]=$j; }
	}
    }
    close FH;

    return @index;
}

sub get_element {
    my $elements_ref=shift;
    my $indices_ref=shift;
    my $columns_ref=shift;
    my $target=shift;

    my $element;

    for (my $i=0;$i<=$#{ $indices_ref };$i++){
	if (${ $elements_ref }[$i] eq $target) {
	    $element=${ $columns_ref }[${ $indices_ref }[$i]]
	}
    }
    return $element;
}




sub match_line_to_var {
    my $index=shift;
    my $SITE=shift;
    my $age=shift;
    my $outcome=shift;
    my $stratified=shift;
    my $sample_sex=shift;
    my $arrayref=shift;
    my $arrayref2=shift;
    my $hashref=shift;
    my $line=shift;

    my @array=@{ $arrayref };
    my @array2=@{ $arrayref2 };

   for (my $j=0;$j<=$#array;$j++) {
	if ((($j+1)<=$#array) &&
	    ($array[$j]=~m/dum2_\d*\w*\d*_x_/) &&
	    ($array[$j+1]=~m/dum1_\d*\w*\d*_x_/)) {
	    $array[$j]=~s/dum2/dum1/;
	}
	elsif (($array[$j]=~m/dum1_\d*\w*\d*_x_/) &&
	       ($array[$j-1]=~m/dum1_\d*\w*\d*_x_/)) {
	    $array[$j]=~s/dum1/dum2/;
	}
    }


    my @line=split(/\t/,$line);
    my $parameter=$line[1];
    $parameter=~s/\.XX\./_x_/g;
    $parameter=~s/\:/_x_/g;
    $parameter=~s/[\(\)]//g;
    my $Estimate=$line[2];	
    my $StdErr=$line[3];
    my $z_value=$line[4];	
    my $P=$line[5];	
    my $dev_df=$line[6];	
    my $deviance=$line[7];	
    my $vcov_label=$line[8];
    my @vcov;

    if (($parameter eq $array[$index]) ||
	($parameter eq "Intercept")) {
	for (my $j=0;$j<=$#array;$j++){
	    $vcov[$j]=$line[9 + $j];
	}
	$hashref->{$SITE}->{$age}->{$outcome}->{$array2[0]}->{$array2[1]}->{$array2[2]}->{$array2[3]}->{$array2[4]}->{$array2[5]}->{$array2[6]}->{$array2[7]}->{$stratified}->{$sample_sex}->{$parameter}={ "estimate" => $Estimate,
																						"se" => $StdErr,
																						"z" => $z_value,
																						"P" => $P,
																						"dev_df" => $dev_df,
																						"deviance" => $deviance,
																						"vcov_label" => $vcov_label,
																						"vcov" => \@vcov	  };

	unless ($parameter eq "Intercept") {
	    $index++;
	}
	return $index;
    } else {
	$index++;
	$index=match_line_to_var($index,$SITE,$age,$outcome,$stratified,$sample_sex,$arrayref,$arrayref2,$hashref,$line);
    }
}

sub Basic_match_line_to_var {
    my $index=shift;
    my $SITE=shift;
    my $age=shift;
    my $outcome=shift;
    my $stratified=shift;
    my $sample_sex=shift;
    my $arrayref=shift;
    my $hashref=shift;
    my $line=shift;

    my @array=@{ $arrayref };

    my @line=split(/\t/,$line);
    my $parameter=$line[1];
    $parameter=~s/\.XX\./_x_/g;
    $parameter=~s/\:/_x_/g;
    $parameter=~s/[\(\)]//g;
    my $Estimate=$line[2];	
    my $StdErr=$line[3];
    my $z_value=$line[4];	
    my $P=$line[5];	
    my $dev_df=$line[6];	
    my $deviance=$line[7];	
    my $vcov_label=$line[8];
    my @vcov;

    if (($parameter eq $array[$index]) ||
	($parameter eq "Intercept")) {
	for (my $j=0;$j<=$#array;$j++){
	    $vcov[$j]=$line[9 + $j];
	}
	$hashref->{$SITE}->{$age}->{$outcome}->{$array[0]}->{$array[1]}->{$array[2]}->{$array[3]}->{$array[4]}->{$array[5]}->{$array[6]}->{$array[7]}->{$stratified}->{$sample_sex}->{$parameter}={ "estimate" => $Estimate,
																						"se" => $StdErr,
																						"z" => $z_value,
																						"P" => $P,
																						"dev_df" => $dev_df,
																						"deviance" => $deviance,
																						"vcov_label" => $vcov_label,
																						"vcov" => \@vcov	  };

	unless ($parameter eq "Intercept") {
	    $index++;
	}
	return $index;
    } else {
	$index++;
	$index=Basic_match_line_to_var($index,$SITE,$age,$outcome,$stratified,$sample_sex,$arrayref,$hashref,$line);
    }
}




sub Strat_match_line_to_var {
    my $index=shift;
    my $SITE=shift;
    my $age=shift;
    my $outcome=shift;
    my $stratified=shift;
    my $sample_sex=shift;
    my $snp=shift;
    my $arrayref=shift;
    my $hashref=shift;
    my $line=shift;

    my @array=@{ $arrayref };

    my @line=split(/\t/,$line);
    my $byvar_value=$line[1];
    my $parameter=$line[2];
    $parameter=~s/\.XX\./_x_/g;
    $parameter=~s/\:/_x_/;
    $parameter=~s/[\(\)]//g;
    my $Estimate=$line[3];	
    my $StdErr=$line[4];
    my $z_value=$line[5];	
    my $P=$line[6];	
    my $dev_df=$line[7];	
    my $deviance=$line[8];	
    my $vcov_label=$line[9];
    my @vcov;

    if (($parameter eq $array[$index]) ||
	($parameter eq "Intercept")) {
	for (my $j=0;$j<=$#array;$j++){
	    $vcov[$j]=$line[10 + $j];
	}
	$hashref->{$SITE}->{$age}->{$outcome}->{$array[0]}->{$array[1]}->{$array[2]}->{$array[3]}->{$array[4]}->{$array[5]}->{$array[6]}->{$array[7]}->{$stratified}->{$sample_sex}->{$parameter}->{$byvar_value}={ "estimate" => $Estimate,
																								    "se" => $StdErr,
																								    "z" => $z_value,
																								    "P" => $P,
																								    "dev_df" => $dev_df,
																								    "deviance" => $deviance,
																								    "vcov_label" => $vcov_label,
																								    "vcov" => \@vcov,
																								    "byvar" => $snp 	  };

	unless ($parameter eq "Intercept") {
	    $index++;
	}
	return $index;
    } else {
	$index++;
	$index=Strat_match_line_to_var($index,$SITE,$age,$outcome,$stratified,$sample_sex,$snp,$arrayref,$hashref,$line);
    }
}
