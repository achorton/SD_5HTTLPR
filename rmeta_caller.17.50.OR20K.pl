#*****************************************************************************
#* 5-HTTLPR GxE analysis regession results files assembler
#* version 1
#*****************************************************************************
#*
#* created by Amy Horton and Rob Culverhouse
#* 08/16/13
#*
#*****************************************************************************
#!/usr/bin/env perl -w

use strict;
use Data::Dumper;
use List::Util qw(sum min max);
use File::Path qw(make_path);
use File::Copy;
use File::Basename;
use Storable qw(dclone);
use List::MoreUtils qw(first_index); 

my $wd=$ARGV[0];
#my $groupname="_all_studies";
my $groupname=$wd;
$groupname=~s/\/$//;
$groupname=basename($groupname);
#$groupname="_".$groupname;
#print "Groupname is: ",$groupname,"\n";
my @SITES;
my $arrayref;
my $hashref;
my $N_filter=50; ##50/100
#my $beta_filter=20; ##10/20
#my $beta_filter=9.9034875; ##10/20
my $beta_filter=20000;
my $beta_filter2;
my $min_beta_filter2;

my $left_clip_for_plot;
my $right_clip_for_plot;
my $left_end_of_x_axis;
my $right_end_of_x_axis;
my $tick_interval;


if (scalar @ARGV > 1) {
    shift @ARGV;
    @SITES=@ARGV;
}
unless ($wd){
    $wd="/home/amyh/SD_5HTTLPR/Returned/";
#    $wd="/Users/achorton/SD_5HTTLPR/SD_5HTTLPR_beta7/";
}
my $fail_file="${wd}meta_raw_files/failed_rmeta_script_calls.txt";
if (-d "${wd}meta_raw_files"){
    if(-f $fail_file) {
	unlink $fail_file;
    }
} else { make_path("${wd}meta_raw_files", { mode => 755 } );}

if((-d "${wd}meta_raw_files/N_summary") && (-f "${wd}meta_raw_files/N_summary/N_summary.csv")) {
    unlink("${wd}meta_raw_files/N_summary/N_summary.csv");
} else {
    unless(-d "${wd}meta_raw_files/N_summary") {
	make_path("${wd}meta_raw_files/N_summary", { mode => 755 } );
    }
}

#system("perl","/home/amyh/SD_5HTTLPR/Regression_parser.v5.pl",$wd);

my %tier_1;
my %tier_1b;
my %tier_2;
my %tier_3;
my %tier_4;
my %tier_5;
my %tier_6;

my %list;

for my $file (my @filenames = glob("${wd}meta_raw_files/*__*__*__*__*__*__*__*.csv")) {
    my @vars=split("__",$file);
    for (my $i=$#vars;$i>=0;$i--) {
	if ($vars[$i] eq "null") {
	    pop @vars;
	}
    }

    my @elements= ( "namestudy","age_group","n_study","strat","snp","phen","model","beta","se","combined","sample_sex","cond" );
    my $indices_ref=get_indices($file,\@elements);
    my @indices=@{ $indices_ref };

    open(FH,"<",$file) || die "Cannot open rmeta input file $file:$!\n";
    <FH>;
    while (my $line=<FH>) {
	chomp $line;
	my @columns=split("\t",$line);
	
	my $namestudy=get_element(\@elements,\@indices,\@columns,"namestudy");
	my $age=get_element(\@elements,\@indices,\@columns,"age_group");
	my $n_study=get_element(\@elements,\@indices,\@columns,"n_study");
	my $stratum=get_element(\@elements,\@indices,\@columns,"strat");
	my $parameter=get_element(\@elements,\@indices,\@columns,"snp");
	$parameter=~s/[\(\)]//g;
	my $outcome=get_element(\@elements,\@indices,\@columns,"phen");
	my $model=get_element(\@elements,\@indices,\@columns,"model");
	my $beta=get_element(\@elements,\@indices,\@columns,"beta");
	my $se=get_element(\@elements,\@indices,\@columns,"se");
	my $combined=get_element(\@elements,\@indices,\@columns,"combined");
	my $sample_sex=get_element(\@elements,\@indices,\@columns,"sample_sex");
	my $cond=get_element(\@elements,\@indices,\@columns,"cond");

	my @bar = map { $_ eq $cond ? 1:0 } @{ $list{$file}{"array_ref"} };
	unless (sum(@bar)){
	    push @{ $list{$file}{"array_ref"} },$cond;
	}

	@bar = map { $_ eq $age ? 1:0 } @{ $list{$file}{$cond}{"array_ref"} };
	unless (sum(@bar)){
	    push @{ $list{$file}{$cond}{"array_ref"} },$age;
	}

	@bar = map { $_ eq $outcome ? 1:0 } @{ $list{$file}{$cond}{$age}{"array_ref"} };
	unless (sum(@bar)){
	    push @{ $list{$file}{$cond}{$age}{"array_ref"} },$outcome;
	}

	@bar = map { ($_ eq $sample_sex) ? 1:0 } @{ $list{$file}{$cond}{$age}{$outcome}{"array_ref"} };
	unless (sum(@bar)){
	    push @{ $list{$file}{$cond}{$age}{$outcome}{"array_ref"} },$sample_sex;
	}

	@bar = map { ($_ eq $parameter) ? 1:0 } @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{"array_ref"} };

	unless (sum(@bar)){
	    push @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{"array_ref"} },$parameter;
	}

	@bar = map { ($_ eq $stratum) ? 1:0 } @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{"array_ref"} };
	unless (sum(@bar)){
	    push @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{"array_ref"} },$stratum;
	}

	@bar = map { ($_ eq $namestudy) ? 1:0 } @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} };
	unless (sum(@bar)){
	    push @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} },$namestudy;
	    push @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"n_study_ref"} },$n_study;
	    push @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"model_ref"} },$model;
	    push @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} },$beta;
	    push @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"se_ref"} },$se;
	    push @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"N_ref"} },$combined;
	}
    }
    close FH;

    for my $cond (@{ $list{$file}{"array_ref"} }) {
	for my $age (@{ $list{$file}{$cond}{"array_ref"} }) {  

	    for my $outcome (@{ $list{$file}{$cond}{$age}{"array_ref"} }) {

		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $beta_filter2=log($beta_filter);
		    $min_beta_filter2=(-1) * ${beta_filter2};
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $beta_filter2=$beta_filter;
		    $min_beta_filter2=(-1) * ${beta_filter};
		}

		for my $sample_sex (@{ $list{$file}{$cond}{$age}{$outcome}{"array_ref"} }) {
		    for my $parameter (@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{"array_ref"} }) {   
			unless ($parameter eq "Intercept") {
			    for my $stratum (@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{"array_ref"} }) {
				(my $tier_1ref,my $tier_1bref,my $tier_2ref,my $tier_3ref,my $tier_4ref,my $tier_5ref,my $tier_6ref)=tierer($file,$cond,$age,$outcome,$sample_sex,$parameter,$stratum,\%tier_1,\%tier_1b,\%tier_2,\%tier_3,\%tier_4,\%tier_5,\%tier_6);
				%tier_1=%{ $tier_1ref };
				%tier_1b=%{ $tier_1bref };
				%tier_2=%{ $tier_2ref };
				%tier_3=%{ $tier_3ref };
				%tier_4=%{ $tier_4ref };
				%tier_5=%{ $tier_5ref };
				%tier_6=%{ $tier_6ref };
			    }
			}
		    }
		}
	    }
	}
    }
}

my %list2 = %{ dclone(\%list) };

#my @tmp1=keys $tier_1{"/home/amyh/SD_5HTTLPR/Returned/AE_studies/meta_raw_files/female__age__add_5http__child_mal_exp__add_5http_x_child_mal_exp__null__null__null.csv"}{"DepDx_ANY_"}{"All"}{"DD_life"};


print scalar keys %tier_1,"\n";
print scalar keys %tier_1b,"\n";
print scalar keys %tier_2,"\n";
print scalar keys %tier_3,"\n";
print scalar keys %tier_4,"\n";
print scalar keys %tier_5,"\n";
print scalar keys %tier_6,"\n";

open(FAIL_FILE,">",$fail_file) || die "Cannot open failed rmeta_script call file:$!\n";

#if(0) {
for my $file (keys %tier_1) {
    for my $cond (keys $tier_1{$file} ) {
	for my $age (keys $tier_1{$file}{$cond}) {  
	    for my $outcome (keys $tier_1{$file}{$cond}{$age}) {
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $beta_filter2=log($beta_filter);
		    $min_beta_filter2=(-1) * ${beta_filter2};
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $beta_filter2=$beta_filter;
		    $min_beta_filter2=(-1) * ${beta_filter};
		}

		for my $sample_sex (keys $tier_1{$file}{$cond}{$age}{$outcome}) {
		    my @studynamelist;
		    my %studynamelist;
		    my %removelistmaster;
		    my %full_list2;

		    for my $parameter (keys $tier_1{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_1{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for my $study2 ( @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} } ) {
				    $studynamelist{$stratum}{$study2}{$parameter}="present";
				}
			    }
			}
		    }

		    for my $stratum2 (keys %studynamelist) {
			my $max_len=0;
			my $max_len2=0;
			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} > $max_len) {
				$max_len=scalar keys $studynamelist{$stratum2}{$study2};
			    }
			}
			for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			    if($parameter2 ne "array_ref" &&
			       scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} } > $max_len2) {
				$max_len2=scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} } ;
			    }
			}

			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} < $max_len) {
				$removelistmaster{$stratum2}{$study2}="TO_BE_REMOVED";
			    } else { $removelistmaster{$stratum2}{$study2}="KEEP";
			    }
			}

			for my $parameter (keys $tier_1{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			    unless($parameter eq "array_ref" || $parameter eq "Intercept") {
				if(scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} } == $max_len2) {
				    my @tmp=@{ dclone(\@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} }) };
				    $full_list2{$stratum2}=\@tmp; 
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_1{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_1{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if($removelistmaster{$stratum}{${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i]} eq "TO_BE_REMOVED") {
					$list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
				    }
				}
			    }
			}
		    }

		    for my $stratum2 (keys %removelistmaster) {
			my @full_list=sort keys $removelistmaster{$stratum2};
			my @full_list2=@{ $full_list2{$stratum2} };
			for (my $k=$#full_list;$k>=0;$k--) {
			    my $study2=$full_list[$k];
			    my $dist_end=$#full_list-$k;
			    for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
				unless($parameter2 eq "array_ref" || $parameter2 eq "Intercept") {
				    my @idx_list;
				    my $arraylen=scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} });
				    my @study3_array;
				    if (exists $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}) {
					@study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"} };
				    } else {
					@study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
				    }

				    for my $name (@study3_array){
					push @idx_list, first_index { /${name}/ } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
				    }


				    my @study3_array2;
				    my @study3_array3;

				    my @study3_N;
				    my @study3_se;
				    my @study3_n_study;
				    my @study3_model;
				    my @study3_beta;

				    my @study4_array2;
				    my @study4_array3;

				    my @study4_N;
				    my @study4_se;
				    my @study4_n_study;
				    my @study4_model;
				    my @study4_beta;

				    for my $idx (@idx_list) {
					push @study3_array2,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study3_array3,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study3_N,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					push @study3_se,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					push @study3_n_study,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					push @study3_model,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					push @study3_beta,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];


					push @study4_array2,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study4_array3,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study4_N,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					push @study4_se,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					push @study4_n_study,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					push @study4_model,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					push @study4_beta,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];
				    }

				    if((first_index { /${study2}/ } @study3_array) == -1) {
					if ($dist_end >= scalar @study3_array) {
					    unshift @study3_array,"TO_BE_REMOVED";
					    unshift @study3_array3,$study2;
					    unshift @study3_N,"0";
					    unshift @study3_se,"NA";
					    unshift @study3_n_study,"0";
					    unshift @study3_model,"NA";
					    unshift @study3_beta,"NA";

					    unshift @study4_array2,"TO_BE_REMOVED";
					    unshift @study4_array3,$study2;
					    unshift @study4_N,"0";
					    unshift @study4_se,"NA";
					    unshift @study4_n_study,"0";
					    unshift @study4_model,"NA";
					    unshift @study4_beta,"NA";
					} else {
					    my @tmp_array;
					    my @tmp_array3;
					    my @tmp_N;
					    my @tmp_se;
					    my @tmp_n_study;
					    my @tmp_model;
					    my @tmp_beta;

					    my @tmp4_array2;
					    my @tmp4_array3;
					    my @tmp4_N;
					    my @tmp4_se;
					    my @tmp4_n_study;
					    my @tmp4_model;
					    my @tmp4_beta;
					    for (my $j=0; $j<($arraylen - $dist_end); $j++) {
						push @tmp_array,$study3_array[$j];
						push @tmp_array3,$study3_array3[$j];
						push @tmp_N,$study3_N[$j];
						push @tmp_se,$study3_se[$j];
						push @tmp_n_study,$study3_n_study[$j];
						push @tmp_model,$study3_model[$j];
						push @tmp_beta,$study3_beta[$j];

						push @tmp4_array2,$study4_array2[$j];
						push @tmp4_array3,$study4_array3[$j];
						push @tmp4_N,$study4_N[$j];
						push @tmp4_se,$study4_se[$j];
						push @tmp4_n_study,$study4_n_study[$j];
						push @tmp4_model,$study4_model[$j];
						push @tmp4_beta,$study4_beta[$j];
					    }
					    push @tmp_array,"TO_BE_REMOVED";
					    push @tmp_array3,$study2;
					    push @tmp_N,"0";
					    push @tmp_se,"NA";
					    push @tmp_n_study,"0";
					    push @tmp_model,"NA";
					    push @tmp_beta,"NA";

					    push @tmp4_array2,"TO_BE_REMOVED";
					    push @tmp4_array3,$study2;
					    push @tmp4_N,"0";
					    push @tmp4_se,"NA";
					    push @tmp4_n_study,"0";
					    push @tmp4_model,"NA";
					    push @tmp4_beta,"NA";
					    for (my $j=($arraylen - $dist_end); $j < $arraylen; $j++) {
						push @tmp_array,$study3_array[$j];
						push @tmp_array3,$study3_array3[$j];
						push @tmp_N,$study3_N[$j];
						push @tmp_se,$study3_se[$j];
						push @tmp_n_study,$study3_n_study[$j];
						push @tmp_model,$study3_model[$j];
						push @tmp_beta,$study3_beta[$j];

						push @tmp4_array2,$study4_array2[$j];
						push @tmp4_array3,$study4_array3[$j];
						push @tmp4_N,$study4_N[$j];
						push @tmp4_se,$study4_se[$j];
						push @tmp4_n_study,$study4_n_study[$j];
						push @tmp4_model,$study4_model[$j];
						push @tmp4_beta,$study4_beta[$j];
					    }
					    @study3_array=@tmp_array;
					    @study3_array3=@tmp_array3;
					    @study3_N=@tmp_N;
					    @study3_se=@tmp_se;
					    @study3_n_study=@tmp_n_study;
					    @study3_model=@tmp_model;
					    @study3_beta=@tmp_beta;

					    @study4_array2=@tmp4_array2;
					    @study4_array3=@tmp4_array3;
					    @study4_N=@tmp4_N;
					    @study4_se=@tmp4_se;
					    @study4_n_study=@tmp4_n_study;
					    @study4_model=@tmp4_model;
					    @study4_beta=@tmp4_beta;
					}
				    }
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study3_array;
				    $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study3_array3;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study3_N;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study3_se;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study3_n_study;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study3_model;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study3_beta;
				    

				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study4_array2;
				    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study4_array3;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study4_N;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study4_se;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study4_n_study;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study4_model;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study4_beta;
				    
				}
			    }
			}
		    }
		
		    for my $parameter (keys $tier_1{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_1{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if ((${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"N_ref"} }[$i] <= $N_filter) || 
					(${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) > $beta_filter2 || 
					(${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) < $min_beta_filter2) {
					for my $parameter2 (keys $tier_1{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
					    $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
					}
				    }
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_1{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_1{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if (${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i] eq "TO_BE_REMOVED") {
					for my $parameter2 (keys $tier_1{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
					    for (my $j=0;$j<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$j++) {
						if(${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref3"} }[$j] eq ${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref3"} }[$i]) {
						    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$j]="TO_BE_REMOVED";
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
}
print "break\n";
for my $file (keys %tier_1b) {
    for my $cond (keys $tier_1b{$file} ) {
	for my $age (keys $tier_1b{$file}{$cond}) {  
	    for my $outcome (keys $tier_1b{$file}{$cond}{$age}) {
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $beta_filter2=log($beta_filter);
		    $min_beta_filter2=(-1) * ${beta_filter2};
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $beta_filter2=$beta_filter;
		    $min_beta_filter2=(-1) * ${beta_filter};
		}
		for my $sample_sex (keys $tier_1b{$file}{$cond}{$age}{$outcome}) {
		    my @studynamelist;
		    my %studynamelist;
		    my %removelistmaster;
		    my %full_list2;

		    for my $parameter (keys $tier_1b{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_1b{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for my $study2 ( @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} } ) {
				    $studynamelist{$stratum}{$study2}{$parameter}="present";
				}
			    }
			}
		    }

		    for my $stratum2 (keys %studynamelist) {
			my $max_len=0;
			my $max_len2=0;
			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} > $max_len) {
				$max_len=scalar keys $studynamelist{$stratum2}{$study2};
			    }
			}
			for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			    if($parameter2 ne "array_ref" &&
			       scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} } > $max_len2) {
				$max_len2=scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} } ;
			    }
			}

			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} < $max_len) {
				$removelistmaster{$stratum2}{$study2}="TO_BE_REMOVED";
			    } else { $removelistmaster{$stratum2}{$study2}="KEEP";
			    }
			}

			for my $parameter (keys $tier_1b{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			    unless($parameter eq "array_ref" || $parameter eq "Intercept") {
				if(scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} } == $max_len2) {
				    my @tmp=@{ dclone(\@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} }) };
				    $full_list2{$stratum2}=\@tmp; 
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_1b{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_1b{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if($removelistmaster{$stratum}{${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i]} eq "TO_BE_REMOVED") {
					$list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
				    }
				}
			    }
			}
		    }

		    for my $stratum2 (keys %removelistmaster) {
			my @full_list=sort keys $removelistmaster{$stratum2};
			my @full_list2=@{ $full_list2{$stratum2} };
			for (my $k=$#full_list;$k>=0;$k--) {
			    my $study2=$full_list[$k];
			    my $dist_end=$#full_list-$k;
			    for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
				unless($parameter2 eq "array_ref" || $parameter2 eq "Intercept") {
				    my @idx_list;
				    my $arraylen=scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} });
				    my @study3_array;
				    if (exists $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}) {
					@study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"} };
				    } else {
					@study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
				    }

				    for my $name (@study3_array){
					push @idx_list, first_index { /${name}/ } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
				    }


				    my @study3_array2;
				    my @study3_array3;

				    my @study3_N;
				    my @study3_se;
				    my @study3_n_study;
				    my @study3_model;
				    my @study3_beta;

				    my @study4_array2;
				    my @study4_array3;

				    my @study4_N;
				    my @study4_se;
				    my @study4_n_study;
				    my @study4_model;
				    my @study4_beta;

				    for my $idx (@idx_list) {
					push @study3_array2,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study3_array3,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study3_N,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					push @study3_se,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					push @study3_n_study,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					push @study3_model,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					push @study3_beta,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];


					push @study4_array2,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study4_array3,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study4_N,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					push @study4_se,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					push @study4_n_study,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					push @study4_model,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					push @study4_beta,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];
				    }

				    if((first_index { /${study2}/ } @study3_array) == -1) {
					if ($dist_end >= scalar @study3_array) {
					    unshift @study3_array,"TO_BE_REMOVED";
					    unshift @study3_array3,$study2;
					    unshift @study3_N,"0";
					    unshift @study3_se,"NA";
					    unshift @study3_n_study,"0";
					    unshift @study3_model,"NA";
					    unshift @study3_beta,"NA";

					    unshift @study4_array2,"TO_BE_REMOVED";
					    unshift @study4_array3,$study2;
					    unshift @study4_N,"0";
					    unshift @study4_se,"NA";
					    unshift @study4_n_study,"0";
					    unshift @study4_model,"NA";
					    unshift @study4_beta,"NA";
					} else {
					    my @tmp_array;
					    my @tmp_array3;
					    my @tmp_N;
					    my @tmp_se;
					    my @tmp_n_study;
					    my @tmp_model;
					    my @tmp_beta;

					    my @tmp4_array2;
					    my @tmp4_array3;
					    my @tmp4_N;
					    my @tmp4_se;
					    my @tmp4_n_study;
					    my @tmp4_model;
					    my @tmp4_beta;
					    for (my $j=0; $j<($arraylen - $dist_end); $j++) {
						push @tmp_array,$study3_array[$j];
						push @tmp_array3,$study3_array3[$j];
						push @tmp_N,$study3_N[$j];
						push @tmp_se,$study3_se[$j];
						push @tmp_n_study,$study3_n_study[$j];
						push @tmp_model,$study3_model[$j];
						push @tmp_beta,$study3_beta[$j];

						push @tmp4_array2,$study4_array2[$j];
						push @tmp4_array3,$study4_array3[$j];
						push @tmp4_N,$study4_N[$j];
						push @tmp4_se,$study4_se[$j];
						push @tmp4_n_study,$study4_n_study[$j];
						push @tmp4_model,$study4_model[$j];
						push @tmp4_beta,$study4_beta[$j];
					    }
					    push @tmp_array,"TO_BE_REMOVED";
					    push @tmp_array3,$study2;
					    push @tmp_N,"0";
					    push @tmp_se,"NA";
					    push @tmp_n_study,"0";
					    push @tmp_model,"NA";
					    push @tmp_beta,"NA";

					    push @tmp4_array2,"TO_BE_REMOVED";
					    push @tmp4_array3,$study2;
					    push @tmp4_N,"0";
					    push @tmp4_se,"NA";
					    push @tmp4_n_study,"0";
					    push @tmp4_model,"NA";
					    push @tmp4_beta,"NA";
					    for (my $j=($arraylen - $dist_end); $j < $arraylen; $j++) {
						push @tmp_array,$study3_array[$j];
						push @tmp_array3,$study3_array3[$j];
						push @tmp_N,$study3_N[$j];
						push @tmp_se,$study3_se[$j];
						push @tmp_n_study,$study3_n_study[$j];
						push @tmp_model,$study3_model[$j];
						push @tmp_beta,$study3_beta[$j];

						push @tmp4_array2,$study4_array2[$j];
						push @tmp4_array3,$study4_array3[$j];
						push @tmp4_N,$study4_N[$j];
						push @tmp4_se,$study4_se[$j];
						push @tmp4_n_study,$study4_n_study[$j];
						push @tmp4_model,$study4_model[$j];
						push @tmp4_beta,$study4_beta[$j];
					    }
					    @study3_array=@tmp_array;
					    @study3_array3=@tmp_array3;
					    @study3_N=@tmp_N;
					    @study3_se=@tmp_se;
					    @study3_n_study=@tmp_n_study;
					    @study3_model=@tmp_model;
					    @study3_beta=@tmp_beta;

					    @study4_array2=@tmp4_array2;
					    @study4_array3=@tmp4_array3;
					    @study4_N=@tmp4_N;
					    @study4_se=@tmp4_se;
					    @study4_n_study=@tmp4_n_study;
					    @study4_model=@tmp4_model;
					    @study4_beta=@tmp4_beta;
					}
				    }
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study3_array;
				    $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study3_array3;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study3_N;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study3_se;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study3_n_study;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study3_model;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study3_beta;
				    

				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study4_array2;
				    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study4_array3;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study4_N;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study4_se;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study4_n_study;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study4_model;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study4_beta;
				    
				}
			    }
			}
		    }
		
		    for my $parameter (keys $tier_1b{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_1b{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if ((${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"N_ref"} }[$i] <= $N_filter) || 
					(${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) > $beta_filter2 || 
					(${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) < $min_beta_filter2) {
					for my $parameter2 (keys $tier_1b{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
					    $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
					}
				    }
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_1b{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_1b{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if (${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i] eq "TO_BE_REMOVED") {
					for my $parameter2 (keys $tier_1b{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
					    for (my $j=0;$j<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$j++) {
						if(${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref3"} }[$j] eq ${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref3"} }[$i]) {
						    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$j]="TO_BE_REMOVED";
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
}

for my $file (keys %tier_2) {
    for my $cond (keys $tier_2{$file} ) {
	for my $age (keys $tier_2{$file}{$cond}) {  
	    for my $outcome (keys $tier_2{$file}{$cond}{$age}) {
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $beta_filter2=log($beta_filter);
		    $min_beta_filter2=(-1) * ${beta_filter2};
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $beta_filter2=$beta_filter;
		    $min_beta_filter2=(-1) * ${beta_filter};
		}

		for my $sample_sex (keys $tier_2{$file}{$cond}{$age}{$outcome}) {
		    my @studynamelist;
		    my %studynamelist;
		    my %removelistmaster;
		    my %full_list2;

		    for my $parameter (keys $tier_2{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for my $study2 ( @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} } ) {
				    $studynamelist{$stratum}{$study2}{$parameter}="present";
				}
			    }
			}
		    }

		    for my $stratum2 (keys %studynamelist) {
			my $max_len=0;
			my $max_len2=0;
			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} > $max_len) {
				$max_len=scalar keys $studynamelist{$stratum2}{$study2};
			    }
			}
			for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			    if($parameter2 ne "array_ref" &&
			       scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} } > $max_len2) {
				$max_len2=scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} } ;
			    }
			}

			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} < $max_len) {
				$removelistmaster{$stratum2}{$study2}="TO_BE_REMOVED";
			    } else { $removelistmaster{$stratum2}{$study2}="KEEP";
			    }
			}

			for my $parameter (keys $tier_2{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			    unless($parameter eq "array_ref" || $parameter eq "Intercept") {
				if(scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} } == $max_len2) {
				    my @tmp=@{ dclone(\@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} }) };
				    $full_list2{$stratum2}=\@tmp; 
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_2{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if($removelistmaster{$stratum}{${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i]} eq "TO_BE_REMOVED") {
					$list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
				    }
				}
			    }
			}
		    }

		    for my $stratum2 (keys %removelistmaster) {
			my @full_list=sort keys $removelistmaster{$stratum2};
			my @full_list2=@{ $full_list2{$stratum2} };
			for (my $k=$#full_list;$k>=0;$k--) {
			    my $study2=$full_list[$k];
			    my $dist_end=$#full_list-$k;
			    for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
				unless($parameter2 eq "array_ref" || $parameter2 eq "Intercept") {
				    my @idx_list;
				    my $arraylen=scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} });
				    my @study3_array;
				    if (exists $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}) {
					@study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"} };
				    } else {
					@study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
				    }

				    for my $name (@study3_array){
					push @idx_list, first_index { /${name}/ } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
				    }


				    my @study3_array2;
				    my @study3_array3;

				    my @study3_N;
				    my @study3_se;
				    my @study3_n_study;
				    my @study3_model;
				    my @study3_beta;

				    my @study4_array2;
				    my @study4_array3;

				    my @study4_N;
				    my @study4_se;
				    my @study4_n_study;
				    my @study4_model;
				    my @study4_beta;

				    for my $idx (@idx_list) {
					push @study3_array2,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study3_array3,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study3_N,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					push @study3_se,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					push @study3_n_study,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					push @study3_model,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					push @study3_beta,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];


					push @study4_array2,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study4_array3,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study4_N,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					push @study4_se,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					push @study4_n_study,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					push @study4_model,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					push @study4_beta,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];
				    }

				    if((first_index { /${study2}/ } @study3_array) == -1) {
					if ($dist_end >= scalar @study3_array) {
					    unshift @study3_array,"TO_BE_REMOVED";
					    unshift @study3_array3,$study2;
					    unshift @study3_N,"0";
					    unshift @study3_se,"NA";
					    unshift @study3_n_study,"0";
					    unshift @study3_model,"NA";
					    unshift @study3_beta,"NA";

					    unshift @study4_array2,"TO_BE_REMOVED";
					    unshift @study4_array3,$study2;
					    unshift @study4_N,"0";
					    unshift @study4_se,"NA";
					    unshift @study4_n_study,"0";
					    unshift @study4_model,"NA";
					    unshift @study4_beta,"NA";
					} else {
					    my @tmp_array;
					    my @tmp_array3;
					    my @tmp_N;
					    my @tmp_se;
					    my @tmp_n_study;
					    my @tmp_model;
					    my @tmp_beta;

					    my @tmp4_array2;
					    my @tmp4_array3;
					    my @tmp4_N;
					    my @tmp4_se;
					    my @tmp4_n_study;
					    my @tmp4_model;
					    my @tmp4_beta;
					    for (my $j=0; $j<($arraylen - $dist_end); $j++) {
						push @tmp_array,$study3_array[$j];
						push @tmp_array3,$study3_array3[$j];
						push @tmp_N,$study3_N[$j];
						push @tmp_se,$study3_se[$j];
						push @tmp_n_study,$study3_n_study[$j];
						push @tmp_model,$study3_model[$j];
						push @tmp_beta,$study3_beta[$j];

						push @tmp4_array2,$study4_array2[$j];
						push @tmp4_array3,$study4_array3[$j];
						push @tmp4_N,$study4_N[$j];
						push @tmp4_se,$study4_se[$j];
						push @tmp4_n_study,$study4_n_study[$j];
						push @tmp4_model,$study4_model[$j];
						push @tmp4_beta,$study4_beta[$j];
					    }
					    push @tmp_array,"TO_BE_REMOVED";
					    push @tmp_array3,$study2;
					    push @tmp_N,"0";
					    push @tmp_se,"NA";
					    push @tmp_n_study,"0";
					    push @tmp_model,"NA";
					    push @tmp_beta,"NA";

					    push @tmp4_array2,"TO_BE_REMOVED";
					    push @tmp4_array3,$study2;
					    push @tmp4_N,"0";
					    push @tmp4_se,"NA";
					    push @tmp4_n_study,"0";
					    push @tmp4_model,"NA";
					    push @tmp4_beta,"NA";
					    for (my $j=($arraylen - $dist_end); $j < $arraylen; $j++) {
						push @tmp_array,$study3_array[$j];
						push @tmp_array3,$study3_array3[$j];
						push @tmp_N,$study3_N[$j];
						push @tmp_se,$study3_se[$j];
						push @tmp_n_study,$study3_n_study[$j];
						push @tmp_model,$study3_model[$j];
						push @tmp_beta,$study3_beta[$j];

						push @tmp4_array2,$study4_array2[$j];
						push @tmp4_array3,$study4_array3[$j];
						push @tmp4_N,$study4_N[$j];
						push @tmp4_se,$study4_se[$j];
						push @tmp4_n_study,$study4_n_study[$j];
						push @tmp4_model,$study4_model[$j];
						push @tmp4_beta,$study4_beta[$j];
					    }
					    @study3_array=@tmp_array;
					    @study3_array3=@tmp_array3;
					    @study3_N=@tmp_N;
					    @study3_se=@tmp_se;
					    @study3_n_study=@tmp_n_study;
					    @study3_model=@tmp_model;
					    @study3_beta=@tmp_beta;

					    @study4_array2=@tmp4_array2;
					    @study4_array3=@tmp4_array3;
					    @study4_N=@tmp4_N;
					    @study4_se=@tmp4_se;
					    @study4_n_study=@tmp4_n_study;
					    @study4_model=@tmp4_model;
					    @study4_beta=@tmp4_beta;
					}
				    }
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study3_array;
				    $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study3_array3;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study3_N;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study3_se;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study3_n_study;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study3_model;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study3_beta;
				    

				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study4_array2;
				    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study4_array3;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study4_N;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study4_se;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study4_n_study;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study4_model;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study4_beta;
				    
				}
			    }
			}
		    }
		
		    for my $parameter (keys $tier_2{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if ((${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"N_ref"} }[$i] <= $N_filter) || 
					(${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) > $beta_filter2 || 
					(${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) < $min_beta_filter2) {
					for my $parameter2 (keys $tier_2{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
					    $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
					}
				    }
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_2{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if (${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i] eq "TO_BE_REMOVED") {
					for my $parameter2 (keys $tier_2{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
					    for (my $j=0;$j<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$j++) {
						if(${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref3"} }[$j] eq ${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref3"} }[$i]) {
						    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$j]="TO_BE_REMOVED";
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
}

for my $file (keys %tier_3) {
    for my $cond (keys $tier_3{$file} ) {
	for my $age (keys $tier_3{$file}{$cond}) {  
	    for my $outcome (keys $tier_3{$file}{$cond}{$age}) {
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $beta_filter2=log($beta_filter);
		    $min_beta_filter2=(-1) * ${beta_filter2};
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $beta_filter2=$beta_filter;
		    $min_beta_filter2=(-1) * ${beta_filter};
		}
		for my $sample_sex (keys $tier_3{$file}{$cond}{$age}{$outcome}) {
		    my @studynamelist;
		    my %studynamelist;
		    my %removelistmaster;
		    my %full_list2;

		    for my $parameter (keys $tier_3{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_3{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for my $study2 ( @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} } ) {
				    $studynamelist{$stratum}{$study2}{$parameter}="present";
				}
			    }
			}
		    }

		    for my $stratum2 (keys %studynamelist) {
			my $max_len=0;
			my $max_len2=0;
			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} > $max_len) {
				$max_len=scalar keys $studynamelist{$stratum2}{$study2};
			    }
			}
			for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			    if($parameter2 ne "array_ref" &&
			       scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} } > $max_len2) {
				$max_len2=scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} } ;
			    }
			}

			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} < $max_len) {
				$removelistmaster{$stratum2}{$study2}="TO_BE_REMOVED";
			    } else { $removelistmaster{$stratum2}{$study2}="KEEP";
			    }
			}

			for my $parameter (keys $tier_3{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			    unless($parameter eq "array_ref" || $parameter eq "Intercept") {
				if(scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} } == $max_len2) {
				    my @tmp=@{ dclone(\@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} }) };
				    $full_list2{$stratum2}=\@tmp; 
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_3{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_3{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if($removelistmaster{$stratum}{${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i]} eq "TO_BE_REMOVED") {
					$list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
				    }
				}
			    }
			}
		    }

		    for my $stratum2 (keys %removelistmaster) {
			my @full_list=sort keys $removelistmaster{$stratum2};
			my @full_list2=@{ $full_list2{$stratum2} };
			for (my $k=$#full_list;$k>=0;$k--) {
			    my $study2=$full_list[$k];
			    my $dist_end=$#full_list-$k;
			    for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
				unless($parameter2 eq "array_ref" || $parameter2 eq "Intercept") {
				    my @idx_list;
				    my $arraylen=scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} });
				    my @study3_array;
				    if (exists $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}) {
					@study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"} };
				    } else {
					@study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
				    }

				    for my $name (@study3_array){
					push @idx_list, first_index { /${name}/ } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
				    }


				    my @study3_array2;
				    my @study3_array3;

				    my @study3_N;
				    my @study3_se;
				    my @study3_n_study;
				    my @study3_model;
				    my @study3_beta;

				    my @study4_array2;
				    my @study4_array3;

				    my @study4_N;
				    my @study4_se;
				    my @study4_n_study;
				    my @study4_model;
				    my @study4_beta;

				    for my $idx (@idx_list) {
					push @study3_array2,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study3_array3,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study3_N,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					push @study3_se,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					push @study3_n_study,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					push @study3_model,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					push @study3_beta,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];


					push @study4_array2,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study4_array3,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study4_N,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					push @study4_se,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					push @study4_n_study,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					push @study4_model,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					push @study4_beta,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];
				    }

				    if((first_index { /${study2}/ } @study3_array) == -1) {
					if ($dist_end >= scalar @study3_array) {
					    unshift @study3_array,"TO_BE_REMOVED";
					    unshift @study3_array3,$study2;
					    unshift @study3_N,"0";
					    unshift @study3_se,"NA";
					    unshift @study3_n_study,"0";
					    unshift @study3_model,"NA";
					    unshift @study3_beta,"NA";

					    unshift @study4_array2,"TO_BE_REMOVED";
					    unshift @study4_array3,$study2;
					    unshift @study4_N,"0";
					    unshift @study4_se,"NA";
					    unshift @study4_n_study,"0";
					    unshift @study4_model,"NA";
					    unshift @study4_beta,"NA";
					} else {
					    my @tmp_array;
					    my @tmp_array3;
					    my @tmp_N;
					    my @tmp_se;
					    my @tmp_n_study;
					    my @tmp_model;
					    my @tmp_beta;

					    my @tmp4_array2;
					    my @tmp4_array3;
					    my @tmp4_N;
					    my @tmp4_se;
					    my @tmp4_n_study;
					    my @tmp4_model;
					    my @tmp4_beta;
					    for (my $j=0; $j<($arraylen - $dist_end); $j++) {
						push @tmp_array,$study3_array[$j];
						push @tmp_array3,$study3_array3[$j];
						push @tmp_N,$study3_N[$j];
						push @tmp_se,$study3_se[$j];
						push @tmp_n_study,$study3_n_study[$j];
						push @tmp_model,$study3_model[$j];
						push @tmp_beta,$study3_beta[$j];

						push @tmp4_array2,$study4_array2[$j];
						push @tmp4_array3,$study4_array3[$j];
						push @tmp4_N,$study4_N[$j];
						push @tmp4_se,$study4_se[$j];
						push @tmp4_n_study,$study4_n_study[$j];
						push @tmp4_model,$study4_model[$j];
						push @tmp4_beta,$study4_beta[$j];
					    }
					    push @tmp_array,"TO_BE_REMOVED";
					    push @tmp_array3,$study2;
					    push @tmp_N,"0";
					    push @tmp_se,"NA";
					    push @tmp_n_study,"0";
					    push @tmp_model,"NA";
					    push @tmp_beta,"NA";

					    push @tmp4_array2,"TO_BE_REMOVED";
					    push @tmp4_array3,$study2;
					    push @tmp4_N,"0";
					    push @tmp4_se,"NA";
					    push @tmp4_n_study,"0";
					    push @tmp4_model,"NA";
					    push @tmp4_beta,"NA";
					    for (my $j=($arraylen - $dist_end); $j < $arraylen; $j++) {
						push @tmp_array,$study3_array[$j];
						push @tmp_array3,$study3_array3[$j];
						push @tmp_N,$study3_N[$j];
						push @tmp_se,$study3_se[$j];
						push @tmp_n_study,$study3_n_study[$j];
						push @tmp_model,$study3_model[$j];
						push @tmp_beta,$study3_beta[$j];

						push @tmp4_array2,$study4_array2[$j];
						push @tmp4_array3,$study4_array3[$j];
						push @tmp4_N,$study4_N[$j];
						push @tmp4_se,$study4_se[$j];
						push @tmp4_n_study,$study4_n_study[$j];
						push @tmp4_model,$study4_model[$j];
						push @tmp4_beta,$study4_beta[$j];
					    }
					    @study3_array=@tmp_array;
					    @study3_array3=@tmp_array3;
					    @study3_N=@tmp_N;
					    @study3_se=@tmp_se;
					    @study3_n_study=@tmp_n_study;
					    @study3_model=@tmp_model;
					    @study3_beta=@tmp_beta;

					    @study4_array2=@tmp4_array2;
					    @study4_array3=@tmp4_array3;
					    @study4_N=@tmp4_N;
					    @study4_se=@tmp4_se;
					    @study4_n_study=@tmp4_n_study;
					    @study4_model=@tmp4_model;
					    @study4_beta=@tmp4_beta;
					}
				    }
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study3_array;
				    $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study3_array3;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study3_N;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study3_se;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study3_n_study;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study3_model;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study3_beta;
				    

				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study4_array2;
				    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study4_array3;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study4_N;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study4_se;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study4_n_study;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study4_model;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study4_beta;
				    
				}
			    }
			}
		    }
		
		    for my $parameter (keys $tier_3{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_3{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if ((${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"N_ref"} }[$i] <= $N_filter) || 
					(${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) > $beta_filter2 || 
					(${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) < $min_beta_filter2) {
					for my $parameter2 (keys $tier_3{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
					    $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
					}
				    }
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_3{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_3{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if (${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i] eq "TO_BE_REMOVED") {
					for my $parameter2 (keys $tier_3{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
					    for (my $j=0;$j<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$j++) {
						if(${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref3"} }[$j] eq ${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref3"} }[$i]) {
						    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$j]="TO_BE_REMOVED";
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
}


for my $file (keys %tier_4) {
    for my $cond (keys $tier_4{$file} ) {
	for my $age (keys $tier_4{$file}{$cond}) {  
	    for my $outcome (keys $tier_4{$file}{$cond}{$age}) {
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $beta_filter2=log($beta_filter);
		    $min_beta_filter2=(-1) * ${beta_filter2};
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $beta_filter2=$beta_filter;
		    $min_beta_filter2=(-1) * ${beta_filter};
		}
		for my $sample_sex (keys $tier_4{$file}{$cond}{$age}{$outcome}) {
		    my @studynamelist;
		    my %studynamelist;
		    my %removelistmaster;
		    my %full_list2;

		    for my $parameter (keys $tier_4{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_4{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for my $study2 ( @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} } ) {
				    $studynamelist{$stratum}{$study2}{$parameter}="present";
				}
			    }
			}
		    }

		    for my $stratum2 (keys %studynamelist) {
			my $max_len=0;
			my $max_len2=0;
			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} > $max_len) {
				$max_len=scalar keys $studynamelist{$stratum2}{$study2};
			    }
			}
			for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			    if($parameter2 ne "array_ref" &&
			       scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} } > $max_len2) {
				$max_len2=scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} } ;
			    }
			}

			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} < $max_len) {
				$removelistmaster{$stratum2}{$study2}="TO_BE_REMOVED";
			    } else { $removelistmaster{$stratum2}{$study2}="KEEP";
			    }
			}

			for my $parameter (keys $tier_4{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			    unless($parameter eq "array_ref" || $parameter eq "Intercept") {
				if(scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} } == $max_len2) {
				    my @tmp=@{ dclone(\@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} }) };
				    $full_list2{$stratum2}=\@tmp; 
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_4{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_4{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if($removelistmaster{$stratum}{${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i]} eq "TO_BE_REMOVED") {
					$list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
				    }
				}
			    }
			}
		    }

		    for my $stratum2 (keys %removelistmaster) {
			my @full_list=sort keys $removelistmaster{$stratum2};
			my @full_list2=@{ $full_list2{$stratum2} };
			for (my $k=$#full_list;$k>=0;$k--) {
			    my $study2=$full_list[$k];
			    my $dist_end=$#full_list-$k;
			    for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
				unless($parameter2 eq "array_ref" || $parameter2 eq "Intercept") {
				    my @idx_list;
				    my $arraylen=scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} });
				    my @study3_array;
				    if (exists $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}) {
					@study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"} };
				    } else {
					@study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
				    }

				    for my $name (@study3_array){
					push @idx_list, first_index { /${name}/ } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
				    }


				    my @study3_array2;
				    my @study3_array3;

				    my @study3_N;
				    my @study3_se;
				    my @study3_n_study;
				    my @study3_model;
				    my @study3_beta;

				    my @study4_array2;
				    my @study4_array3;

				    my @study4_N;
				    my @study4_se;
				    my @study4_n_study;
				    my @study4_model;
				    my @study4_beta;

				    for my $idx (@idx_list) {
					push @study3_array2,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study3_array3,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study3_N,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					push @study3_se,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					push @study3_n_study,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					push @study3_model,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					push @study3_beta,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];


					push @study4_array2,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study4_array3,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study4_N,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					push @study4_se,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					push @study4_n_study,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					push @study4_model,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					push @study4_beta,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];
				    }

				    if((first_index { /${study2}/ } @study3_array) == -1) {
					if ($dist_end >= scalar @study3_array) {
					    unshift @study3_array,"TO_BE_REMOVED";
					    unshift @study3_array3,$study2;
					    unshift @study3_N,"0";
					    unshift @study3_se,"NA";
					    unshift @study3_n_study,"0";
					    unshift @study3_model,"NA";
					    unshift @study3_beta,"NA";

					    unshift @study4_array2,"TO_BE_REMOVED";
					    unshift @study4_array3,$study2;
					    unshift @study4_N,"0";
					    unshift @study4_se,"NA";
					    unshift @study4_n_study,"0";
					    unshift @study4_model,"NA";
					    unshift @study4_beta,"NA";
					} else {
					    my @tmp_array;
					    my @tmp_array3;
					    my @tmp_N;
					    my @tmp_se;
					    my @tmp_n_study;
					    my @tmp_model;
					    my @tmp_beta;

					    my @tmp4_array2;
					    my @tmp4_array3;
					    my @tmp4_N;
					    my @tmp4_se;
					    my @tmp4_n_study;
					    my @tmp4_model;
					    my @tmp4_beta;
					    for (my $j=0; $j<($arraylen - $dist_end); $j++) {
						push @tmp_array,$study3_array[$j];
						push @tmp_array3,$study3_array3[$j];
						push @tmp_N,$study3_N[$j];
						push @tmp_se,$study3_se[$j];
						push @tmp_n_study,$study3_n_study[$j];
						push @tmp_model,$study3_model[$j];
						push @tmp_beta,$study3_beta[$j];

						push @tmp4_array2,$study4_array2[$j];
						push @tmp4_array3,$study4_array3[$j];
						push @tmp4_N,$study4_N[$j];
						push @tmp4_se,$study4_se[$j];
						push @tmp4_n_study,$study4_n_study[$j];
						push @tmp4_model,$study4_model[$j];
						push @tmp4_beta,$study4_beta[$j];
					    }
					    push @tmp_array,"TO_BE_REMOVED";
					    push @tmp_array3,$study2;
					    push @tmp_N,"0";
					    push @tmp_se,"NA";
					    push @tmp_n_study,"0";
					    push @tmp_model,"NA";
					    push @tmp_beta,"NA";

					    push @tmp4_array2,"TO_BE_REMOVED";
					    push @tmp4_array3,$study2;
					    push @tmp4_N,"0";
					    push @tmp4_se,"NA";
					    push @tmp4_n_study,"0";
					    push @tmp4_model,"NA";
					    push @tmp4_beta,"NA";
					    for (my $j=($arraylen - $dist_end); $j < $arraylen; $j++) {
						push @tmp_array,$study3_array[$j];
						push @tmp_array3,$study3_array3[$j];
						push @tmp_N,$study3_N[$j];
						push @tmp_se,$study3_se[$j];
						push @tmp_n_study,$study3_n_study[$j];
						push @tmp_model,$study3_model[$j];
						push @tmp_beta,$study3_beta[$j];

						push @tmp4_array2,$study4_array2[$j];
						push @tmp4_array3,$study4_array3[$j];
						push @tmp4_N,$study4_N[$j];
						push @tmp4_se,$study4_se[$j];
						push @tmp4_n_study,$study4_n_study[$j];
						push @tmp4_model,$study4_model[$j];
						push @tmp4_beta,$study4_beta[$j];
					    }
					    @study3_array=@tmp_array;
					    @study3_array3=@tmp_array3;
					    @study3_N=@tmp_N;
					    @study3_se=@tmp_se;
					    @study3_n_study=@tmp_n_study;
					    @study3_model=@tmp_model;
					    @study3_beta=@tmp_beta;

					    @study4_array2=@tmp4_array2;
					    @study4_array3=@tmp4_array3;
					    @study4_N=@tmp4_N;
					    @study4_se=@tmp4_se;
					    @study4_n_study=@tmp4_n_study;
					    @study4_model=@tmp4_model;
					    @study4_beta=@tmp4_beta;
					}
				    }
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study3_array;
				    $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study3_array3;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study3_N;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study3_se;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study3_n_study;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study3_model;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study3_beta;
				    

				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study4_array2;
				    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study4_array3;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study4_N;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study4_se;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study4_n_study;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study4_model;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study4_beta;
				    
				}
			    }
			}
		    }
		
		    for my $parameter (keys $tier_4{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_4{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if ((${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"N_ref"} }[$i] <= $N_filter) || 
					(${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) > $beta_filter2 || 
					(${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) < $min_beta_filter2) {
					for my $parameter2 (keys $tier_4{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
					    $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
					}
				    }
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_4{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_4{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if (${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i] eq "TO_BE_REMOVED") {
					for my $parameter2 (keys $tier_4{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
					    for (my $j=0;$j<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$j++) {
						if(${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref3"} }[$j] eq ${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref3"} }[$i]) {
						    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$j]="TO_BE_REMOVED";
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
}

for my $file (keys %tier_5) {
    for my $cond (keys $tier_5{$file} ) {
	for my $age (keys $tier_5{$file}{$cond}) {  
	    for my $outcome (keys $tier_5{$file}{$cond}{$age}) {
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $beta_filter2=log($beta_filter);
		    $min_beta_filter2=(-1) * ${beta_filter2};
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $beta_filter2=$beta_filter;
		    $min_beta_filter2=(-1) * ${beta_filter};
		}
		for my $sample_sex (keys $tier_5{$file}{$cond}{$age}{$outcome}) {
		    my @studynamelist;
		    my %studynamelist;
		    my %removelistmaster;
		    my %full_list2;

		    for my $parameter (keys $tier_5{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_5{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for my $study2 ( @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} } ) {
				    $studynamelist{$stratum}{$study2}{$parameter}="present";
				}
			    }
			}
		    }

		    for my $stratum2 (keys %studynamelist) {
			my $max_len=0;
			my $max_len2=0;
			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} > $max_len) {
				$max_len=scalar keys $studynamelist{$stratum2}{$study2};
			    }
			}
			for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			    if($parameter2 ne "array_ref" &&
			       scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} } > $max_len2) {
				$max_len2=scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} } ;
			    }
			}

			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} < $max_len) {
				$removelistmaster{$stratum2}{$study2}="TO_BE_REMOVED";
			    } else { $removelistmaster{$stratum2}{$study2}="KEEP";
			    }
			}

			for my $parameter (keys $tier_5{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			    unless($parameter eq "array_ref" || $parameter eq "Intercept") {
				if(scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} } == $max_len2) {
				    my @tmp=@{ dclone(\@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} }) };
				    $full_list2{$stratum2}=\@tmp; 
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_5{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_5{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if($removelistmaster{$stratum}{${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i]} eq "TO_BE_REMOVED") {
					$list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
				    }
				}
			    }
			}
		    }

		    for my $stratum2 (keys %removelistmaster) {
			my @full_list=sort keys $removelistmaster{$stratum2};
			my @full_list2=@{ $full_list2{$stratum2} };
			for (my $k=$#full_list;$k>=0;$k--) {
			    my $study2=$full_list[$k];
			    my $dist_end=$#full_list-$k;
			    for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
				unless($parameter2 eq "array_ref" || $parameter2 eq "Intercept") {
				    my @idx_list;
				    my $arraylen=scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} });
				    my @study3_array;
				    if (exists $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}) {
					@study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"} };
				    } else {
					@study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
				    }

				    for my $name (@study3_array){
					push @idx_list, first_index { /${name}/ } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
				    }


				    my @study3_array2;
				    my @study3_array3;

				    my @study3_N;
				    my @study3_se;
				    my @study3_n_study;
				    my @study3_model;
				    my @study3_beta;

				    my @study4_array2;
				    my @study4_array3;

				    my @study4_N;
				    my @study4_se;
				    my @study4_n_study;
				    my @study4_model;
				    my @study4_beta;

				    for my $idx (@idx_list) {
					push @study3_array2,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study3_array3,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study3_N,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					push @study3_se,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					push @study3_n_study,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					push @study3_model,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					push @study3_beta,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];


					push @study4_array2,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study4_array3,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					push @study4_N,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					push @study4_se,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					push @study4_n_study,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					push @study4_model,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					push @study4_beta,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];
				    }

				    if((first_index { /${study2}/ } @study3_array) == -1) {
					if ($dist_end >= scalar @study3_array) {
					    unshift @study3_array,"TO_BE_REMOVED";
					    unshift @study3_array3,$study2;
					    unshift @study3_N,"0";
					    unshift @study3_se,"NA";
					    unshift @study3_n_study,"0";
					    unshift @study3_model,"NA";
					    unshift @study3_beta,"NA";

					    unshift @study4_array2,"TO_BE_REMOVED";
					    unshift @study4_array3,$study2;
					    unshift @study4_N,"0";
					    unshift @study4_se,"NA";
					    unshift @study4_n_study,"0";
					    unshift @study4_model,"NA";
					    unshift @study4_beta,"NA";
					} else {
					    my @tmp_array;
					    my @tmp_array3;
					    my @tmp_N;
					    my @tmp_se;
					    my @tmp_n_study;
					    my @tmp_model;
					    my @tmp_beta;

					    my @tmp4_array2;
					    my @tmp4_array3;
					    my @tmp4_N;
					    my @tmp4_se;
					    my @tmp4_n_study;
					    my @tmp4_model;
					    my @tmp4_beta;
					    for (my $j=0; $j<($arraylen - $dist_end); $j++) {
						push @tmp_array,$study3_array[$j];
						push @tmp_array3,$study3_array3[$j];
						push @tmp_N,$study3_N[$j];
						push @tmp_se,$study3_se[$j];
						push @tmp_n_study,$study3_n_study[$j];
						push @tmp_model,$study3_model[$j];
						push @tmp_beta,$study3_beta[$j];

						push @tmp4_array2,$study4_array2[$j];
						push @tmp4_array3,$study4_array3[$j];
						push @tmp4_N,$study4_N[$j];
						push @tmp4_se,$study4_se[$j];
						push @tmp4_n_study,$study4_n_study[$j];
						push @tmp4_model,$study4_model[$j];
						push @tmp4_beta,$study4_beta[$j];
					    }
					    push @tmp_array,"TO_BE_REMOVED";
					    push @tmp_array3,$study2;
					    push @tmp_N,"0";
					    push @tmp_se,"NA";
					    push @tmp_n_study,"0";
					    push @tmp_model,"NA";
					    push @tmp_beta,"NA";

					    push @tmp4_array2,"TO_BE_REMOVED";
					    push @tmp4_array3,$study2;
					    push @tmp4_N,"0";
					    push @tmp4_se,"NA";
					    push @tmp4_n_study,"0";
					    push @tmp4_model,"NA";
					    push @tmp4_beta,"NA";
					    for (my $j=($arraylen - $dist_end); $j < $arraylen; $j++) {
						push @tmp_array,$study3_array[$j];
						push @tmp_array3,$study3_array3[$j];
						push @tmp_N,$study3_N[$j];
						push @tmp_se,$study3_se[$j];
						push @tmp_n_study,$study3_n_study[$j];
						push @tmp_model,$study3_model[$j];
						push @tmp_beta,$study3_beta[$j];

						push @tmp4_array2,$study4_array2[$j];
						push @tmp4_array3,$study4_array3[$j];
						push @tmp4_N,$study4_N[$j];
						push @tmp4_se,$study4_se[$j];
						push @tmp4_n_study,$study4_n_study[$j];
						push @tmp4_model,$study4_model[$j];
						push @tmp4_beta,$study4_beta[$j];
					    }
					    @study3_array=@tmp_array;
					    @study3_array3=@tmp_array3;
					    @study3_N=@tmp_N;
					    @study3_se=@tmp_se;
					    @study3_n_study=@tmp_n_study;
					    @study3_model=@tmp_model;
					    @study3_beta=@tmp_beta;

					    @study4_array2=@tmp4_array2;
					    @study4_array3=@tmp4_array3;
					    @study4_N=@tmp4_N;
					    @study4_se=@tmp4_se;
					    @study4_n_study=@tmp4_n_study;
					    @study4_model=@tmp4_model;
					    @study4_beta=@tmp4_beta;
					}
				    }
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study3_array;
				    $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study3_array3;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study3_N;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study3_se;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study3_n_study;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study3_model;
				    @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study3_beta;
				    

				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study4_array2;
				    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study4_array3;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study4_N;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study4_se;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study4_n_study;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study4_model;
				    @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study4_beta;
				    
				}
			    }
			}
		    }
		
		    for my $parameter (keys $tier_5{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_5{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if ((${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"N_ref"} }[$i] <= $N_filter) || 
					(${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) > $beta_filter2 || 
					(${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) < $min_beta_filter2) {
					for my $parameter2 (keys $tier_5{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
					    $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
					}
				    }
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_5{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_5{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
				    if (${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i] eq "TO_BE_REMOVED") {
					for my $parameter2 (keys $tier_5{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
					    for (my $j=0;$j<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$j++) {
						if(${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref3"} }[$j] eq ${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref3"} }[$i]) {
						    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$j]="TO_BE_REMOVED";
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
}
#}########
for my $file (keys %tier_6) {
    for my $cond (keys $tier_6{$file} ) {
	for my $age (keys $tier_6{$file}{$cond}) {  
	    for my $outcome (keys $tier_6{$file}{$cond}{$age}) {
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $beta_filter2=log($beta_filter);
		    $min_beta_filter2=(-1) * ${beta_filter2};
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $beta_filter2=$beta_filter;
		    $min_beta_filter2=(-1) * ${beta_filter};
		}

		for my $sample_sex (keys $tier_6{$file}{$cond}{$age}{$outcome}) {
		    my @studynamelist;
		    my %studynamelist;
		    my %removelistmaster;
		    my %full_list2;

		    for my $parameter (keys $tier_6{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_6{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				for my $study2 ( @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} } ) {
				    $studynamelist{$stratum}{$study2}{$parameter}="present";
				}
			    }
			}
		    }

		    for my $stratum2 (keys %studynamelist) {
			my $max_len=0;
			my $max_len2=0;
			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} > $max_len) {
				$max_len=scalar keys $studynamelist{$stratum2}{$study2};
			    }
			}




			for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {

			    if ($parameter2 eq "array_ref" && 
				$stratum2 eq "full" && 
				$file eq "/home/amyh/SD_5HTTLPR/Returned/EU_studies_gt49_le20/meta_raw_files/birth_decade__L_Arec_rs25531__stress_combined_exp_messy_5yr_curr__L_Arec_rs25531_x_stress_combined_exp_messy_5yr_curr__null__null__null__null.csv" && 
				$cond eq "DepQ_ANY_" && 
				$age eq "All" && 
				$outcome eq "QDz_5yr" && 
				$sample_sex eq "males-only") {
				print "break\n";
			    }
			    if ($parameter2 eq "birth_decade" && 
				$stratum2 == 0 && 
				$file eq "/home/amyh/SD_5HTTLPR/Returned/EU_studies_gt49_le20/meta_raw_files/female__birth_decade__life_stress_quant_5yr_life_z__null__null__null__null__null.csv" && 
				$cond eq "DepDx_ANY_" && 
				$age eq "YA" && 
				$outcome eq "DD_life_5yr" && 
				$sample_sex eq "females-only") {
				print "break\n";
			    }
			    if ($parameter2 eq "birth_decade" &&
				$stratum2 == 1 &&
				$file eq "/home/amyh/SD_5HTTLPR/Returned/EU_studies_gt49_le20/meta_raw_files/female__birth_decade__life_stress_exp_5yr_curr__null__null__null__null__null.csv" &&
				$cond eq "DepDx_ANY_" &&
				$age eq "YA" &&
				$outcome eq "DD_curr_5yr" &&
				$sample_sex eq "combined-sexes") {
				print "break\n";
			    }

#parameter2 array_ref
#stratum2 full
			    if ($file eq "/home/amyh/SD_5HTTLPR/Returned/EU_studies_gt49_le20/meta_raw_files/birth_decade__L_Arec_rs25531__stress_combined_exp_messy_5yr_curr__L_Arec_rs25531_x_stress_combined_exp_messy_5yr_curr__null__null__null__null.csv" &&
				$cond eq "DepQ_ANY_" &&
				$age eq "All" &&
				$outcome eq "QDz_5yr" &&
				$sample_sex eq "males-only") {
				print "break\n";
			    }

			    print "parameter2 ",$parameter2,"\n";
			    print "stratum2 ",$stratum2,"\n";
#			    print "study2",$study2,"\n";
			    print "file ",$file,"\n";
			    print "cond ",$cond,"\n";
			    print "age ",$age,"\n";
			    print "outcome ",$outcome,"\n";
			    print "sample_sex ",$sample_sex,"\n";
#			    print "scalar ",scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} },"\n";
			    print "max_len2 ",$max_len2,"\n";
			    print "max_len ",$max_len,"\n";
			    if($parameter2 ne "array_ref" && $parameter2 ne "Intercept" &&
			       $max_len > 1 &&
			       exists $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2} &&
			       exists $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} ) {
				my $tmp24 = scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} } < 1 ? 0 : scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
				if($tmp24 > $max_len2) {
				    $max_len2=$tmp24 ;
				}
			    }
			}

			for my $study2 (keys $studynamelist{$stratum2}) {
			    if( scalar keys $studynamelist{$stratum2}{$study2} < $max_len) {
				$removelistmaster{$stratum2}{$study2}="TO_BE_REMOVED";
			    } else { $removelistmaster{$stratum2}{$study2}="KEEP";
			    }
			}

			for my $parameter (keys $tier_6{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			    unless($parameter eq "array_ref" || $parameter eq "Intercept") {
				if($max_len > 1 && 
				   exists $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2} &&
				   exists $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} &&
				   scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} } == $max_len2) {
				    my @tmp=@{ dclone(\@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum2}{"array_ref"} }) };
				    $full_list2{$stratum2}=\@tmp; 
				} elsif ($max_len <= 1) {
				    my @tmp = ();
				    $full_list2{$stratum2}=\@tmp; 
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_6{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_6{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				if (exists $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum} && 
				    exists $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"}) {
				    for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
					if($removelistmaster{$stratum}{${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i]} eq "TO_BE_REMOVED") {
					    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
					} 
				    } 
				} else {
				    my @arr=( "TO_BE_REMOVED" );
				    $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} =\@arr;
				}
			    }
			}
		    }

		    if ($file eq '/home/amyh/SD_5HTTLPR/Returned/EU_studies_gt49_le20/meta_raw_files/female__birth_decade__stress_combined_exp_clean_5yr_life__null__null__null__null__null.csv' &&
			$cond eq 'DepDx_ANY_' &&
			$age eq 'All' &&
			$outcome eq 'DD_life_5yr' &&
			$sample_sex eq 'males-only') {
			print "break\n";
		    }

		    if ($file eq "/home/amyh/SD_5HTTLPR/Returned/EU_studies_gt49_le20/meta_raw_files/birth_decade__L_Arec_rs25531__stress_combined_exp_messy_5yr_curr__L_Arec_rs25531_x_stress_combined_exp_messy_5yr_curr__null__null__null__null.csv" &&
			$cond eq "DepQ_ANY_" &&
			$age eq "All" &&
			$outcome eq "QDz_5yr" &&
			$sample_sex eq "males-only") {
			print "break\n";
		    }

		    for my $stratum2 (keys %removelistmaster) {
			my @full_list=sort keys $removelistmaster{$stratum2};
			my @full_list2=@{ $full_list2{$stratum2} };
			for (my $k=$#full_list;$k>=0;$k--) {
			    my $study2=$full_list[$k];
			    my $dist_end=$#full_list-$k;
			    for my $parameter2 (keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
#				my %tmp23 = map { $_ => 1 }  keys $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2};
				unless($parameter2 eq "array_ref" || $parameter2 eq "Intercept") {
				    if (exists $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2} &&
					exists $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"}) {
					my @idx_list;
					my $arraylen=scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }) < 1 ? 0 : scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} });
					my @study3_array;
					if (exists $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}) {
					    @study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"} };
					} else {
					    @study3_array =sort { $a cmp $b } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
					}
					
					for my $name (@study3_array){
					    push @idx_list, first_index { /${name}/ } @{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} };
					}


					my @study3_array2;
					my @study3_array3;

					my @study3_N;
					my @study3_se;
					my @study3_n_study;
					my @study3_model;
					my @study3_beta;

					my @study4_array2;
					my @study4_array3;

					my @study4_N;
					my @study4_se;
					my @study4_n_study;
					my @study4_model;
					my @study4_beta;

					for my $idx (@idx_list) {
					    push @study3_array2,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					    push @study3_array3,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					    push @study3_N,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					    push @study3_se,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					    push @study3_n_study,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					    push @study3_model,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					    push @study3_beta,${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];


					    push @study4_array2,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					    push @study4_array3,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }[$idx];

					    push @study4_N,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }[$idx];
					    push @study4_se,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }[$idx];
					    push @study4_n_study,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }[$idx];
					    push @study4_model,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }[$idx];
					    push @study4_beta,${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }[$idx];
					}

					if((first_index { /${study2}/ } @study3_array) == -1) {
					    if ($dist_end >= scalar @study3_array) {
						unshift @study3_array,"TO_BE_REMOVED";
						unshift @study3_array3,$study2;
						unshift @study3_N,"0";
						unshift @study3_se,"NA";
						unshift @study3_n_study,"0";
						unshift @study3_model,"NA";
						unshift @study3_beta,"NA";

						unshift @study4_array2,"TO_BE_REMOVED";
						unshift @study4_array3,$study2;
						unshift @study4_N,"0";
						unshift @study4_se,"NA";
						unshift @study4_n_study,"0";
						unshift @study4_model,"NA";
						unshift @study4_beta,"NA";
					    } else {
						my @tmp_array;
						my @tmp_array3;
						my @tmp_N;
						my @tmp_se;
						my @tmp_n_study;
						my @tmp_model;
						my @tmp_beta;

						my @tmp4_array2;
						my @tmp4_array3;
						my @tmp4_N;
						my @tmp4_se;
						my @tmp4_n_study;
						my @tmp4_model;
						my @tmp4_beta;
						for (my $j=0; $j<($arraylen - $dist_end); $j++) {
						    push @tmp_array,$study3_array[$j];
						    push @tmp_array3,$study3_array3[$j];
						    push @tmp_N,$study3_N[$j];
						    push @tmp_se,$study3_se[$j];
						    push @tmp_n_study,$study3_n_study[$j];
						    push @tmp_model,$study3_model[$j];
						    push @tmp_beta,$study3_beta[$j];

						    push @tmp4_array2,$study4_array2[$j];
						    push @tmp4_array3,$study4_array3[$j];
						    push @tmp4_N,$study4_N[$j];
						    push @tmp4_se,$study4_se[$j];
						    push @tmp4_n_study,$study4_n_study[$j];
						    push @tmp4_model,$study4_model[$j];
						    push @tmp4_beta,$study4_beta[$j];
						}
						push @tmp_array,"TO_BE_REMOVED";
						push @tmp_array3,$study2;
						push @tmp_N,"0";
						push @tmp_se,"NA";
						push @tmp_n_study,"0";
						push @tmp_model,"NA";
						push @tmp_beta,"NA";

						push @tmp4_array2,"TO_BE_REMOVED";
						push @tmp4_array3,$study2;
						push @tmp4_N,"0";
						push @tmp4_se,"NA";
						push @tmp4_n_study,"0";
						push @tmp4_model,"NA";
						push @tmp4_beta,"NA";
						for (my $j=($arraylen - $dist_end); $j < $arraylen; $j++) {
						    push @tmp_array,$study3_array[$j];
						    push @tmp_array3,$study3_array3[$j];
						    push @tmp_N,$study3_N[$j];
						    push @tmp_se,$study3_se[$j];
						    push @tmp_n_study,$study3_n_study[$j];
						    push @tmp_model,$study3_model[$j];
						    push @tmp_beta,$study3_beta[$j];

						    push @tmp4_array2,$study4_array2[$j];
						    push @tmp4_array3,$study4_array3[$j];
						    push @tmp4_N,$study4_N[$j];
						    push @tmp4_se,$study4_se[$j];
						    push @tmp4_n_study,$study4_n_study[$j];
						    push @tmp4_model,$study4_model[$j];
						    push @tmp4_beta,$study4_beta[$j];
						}
						@study3_array=@tmp_array;
						@study3_array3=@tmp_array3;
						@study3_N=@tmp_N;
						@study3_se=@tmp_se;
						@study3_n_study=@tmp_n_study;
						@study3_model=@tmp_model;
						@study3_beta=@tmp_beta;

						@study4_array2=@tmp4_array2;
						@study4_array3=@tmp4_array3;
						@study4_N=@tmp4_N;
						@study4_se=@tmp4_se;
						@study4_n_study=@tmp4_n_study;
						@study4_model=@tmp4_model;
						@study4_beta=@tmp4_beta;
					    }
					}
					@{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study3_array;
					$list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study3_array3;
					@{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study3_N;
					@{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study3_se;
					@{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study3_n_study;
					@{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study3_model;
					@{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study3_beta;
					

					@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref"} }=@study4_array2;
					$list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"array_ref3"}=\@study4_array3;
					@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"N_ref"} }=@study4_N;
					@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"se_ref"} }=@study4_se;
					@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"n_study_ref"} }=@study4_n_study;
					@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"model_ref"} }=@study4_model;
					@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum2}{"beta_ref"} }=@study4_beta;

				    }				    
				}
			    }
			}
		    }
		
		    for my $parameter (keys $tier_6{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_6{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				if (exists $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"}) {
				    for (my $i=0;$i<scalar(@{ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
					if ((${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"N_ref"} }[$i] <= $N_filter) || 
					    (${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) > $beta_filter2 || 
					    (${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"beta_ref"} }[$i]) < $min_beta_filter2) {
					    for my $parameter2 (keys $tier_6{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
						$list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$i]="TO_BE_REMOVED";
					    }
					}
				    }
				}
			    }
			}
		    }

		    for my $parameter (keys $tier_6{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless($parameter eq "array_ref" || $parameter eq "Intercept") {
			    for my $stratum (keys $tier_6{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				if ( exists $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"}) {
				    for (my $i=0;$i<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$i++) {
					if (${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$i] eq "TO_BE_REMOVED") {
					    for my $parameter2 (keys $tier_6{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
						for (my $j=0;$j<scalar(@{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} });$j++) {
						    if(${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref3"} }[$j] eq ${ $list2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref3"} }[$i]) {
							$list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter2}{$stratum}{"array_ref"}[$j]="TO_BE_REMOVED";
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
    }
}






for my $file (keys %tier_1) {
    for my $cond (keys $tier_1{$file} ) {
	for my $age (keys $tier_1{$file}{$cond}) {  
	    for my $outcome (keys $tier_1{$file}{$cond}{$age}) {
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $left_clip_for_plot=0;
		    $right_clip_for_plot=3;
		    $left_end_of_x_axis=0;
		    $right_end_of_x_axis=3;
		    $tick_interval=0.5;
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $left_clip_for_plot=-1.5;
		    $right_clip_for_plot=1.5;
		    $left_end_of_x_axis=-1.5;
		    $right_end_of_x_axis=1.5;
		    $tick_interval=0.25;
		}

		for my $sample_sex (keys $tier_1{$file}{$cond}{$age}{$outcome}) {
		    for my $parameter (keys $tier_1{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless ($parameter eq "Intercept") {
			    for my $stratum (keys $tier_1{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				my $studylist="";
				for my $namestudy (sort { ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$a] <=> ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$b] } @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }) {
				    if ($namestudy!~m/TO_BE_REMOVED/) {
    $studylist=$studylist." ".$namestudy;
}
}
				print "break\n";
if ((scalar (split " ",$studylist) < 2) || (my $rc=system("cat /home/amyh/SD_5HTTLPR/rmeta_script.3.r | R --slave --args ---file $file ---effects random ---outcome $outcome ---term $parameter ---study $studylist ---label $result_label ---logscale $lgscl ---groupname $groupname ---stratum $stratum ---age $age ---sex $sample_sex ---condition $cond ---clip $left_clip_for_plot $right_clip_for_plot ---axis $left_end_of_x_axis $right_end_of_x_axis $tick_interval") )) {
#if ((scalar @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} } < 2) || (my $rc=system("cat /home/amyh/SD_5HTTLPR/rmeta_script.3.r | R --slave --args ---file $file ---effects random ---outcome $outcome ---term $parameter ---study $studylist ---label $result_label ---logscale $lgscl ---groupname $groupname ---stratum $stratum ---age $age ---sex $sample_sex ---condition $cond ---clip $left_clip_for_plot $right_clip_for_plot ---axis $left_end_of_x_axis $right_end_of_x_axis $tick_interval") )) {
#    print FAIL_FILE "$file $cond $outcome $sample_sex $parameter $stratum $studylist $result_label $lgscl $groupname $age\n";
    print FAIL_FILE "$file $cond $outcome $sample_sex $parameter $stratum $studylist $result_label $lgscl $groupname $age ---clip $left_clip_for_plot $right_clip_for_plot ---axis $left_end_of_x_axis $right_end_of_x_axis $tick_interval \n";
}
			    }
			}
		    }
		}
	    }
	}
    }
}
print FAIL_FILE "END of TIER1","\n";

for my $file (keys %tier_1b) {
    for my $cond (keys $tier_1b{$file} ) {
	for my $age (keys $tier_1b{$file}{$cond}) {  
	    for my $outcome (keys $tier_1b{$file}{$cond}{$age}) {
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $left_clip_for_plot=0;
		    $right_clip_for_plot=3;
		    $left_end_of_x_axis=0;
		    $right_end_of_x_axis=3;
		    $tick_interval=0.5;
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $left_clip_for_plot=-1.5;
		    $right_clip_for_plot=1.5;
		    $left_end_of_x_axis=-1.5;
		    $right_end_of_x_axis=1.5;
		    $tick_interval=0.25;
		}

		for my $sample_sex (keys $tier_1b{$file}{$cond}{$age}{$outcome}) {
		    for my $parameter (keys $tier_1b{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless ($parameter eq "Intercept") {
			    for my $stratum (keys $tier_1b{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				my $studylist="";
				for my $namestudy (sort { ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$a] <=> ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$b] } @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }) {
				    if ($namestudy!~m/TO_BE_REMOVED/) {
    $studylist=$studylist." ".$namestudy;
}
}
if ((scalar (split " ",$studylist) < 2) || (my $rc=system("cat /home/amyh/SD_5HTTLPR/rmeta_script.3.r | R --slave --args ---file $file ---effects random ---outcome $outcome ---term $parameter ---study $studylist ---label $result_label ---logscale $lgscl ---groupname $groupname ---stratum $stratum ---age $age ---sex $sample_sex ---condition $cond ---clip $left_clip_for_plot $right_clip_for_plot ---axis $left_end_of_x_axis $right_end_of_x_axis $tick_interval") )) {
    print FAIL_FILE "$file $cond $outcome $sample_sex $parameter $stratum $studylist $result_label $lgscl $groupname $age\n";
}
			    }
			}
		    }
		}
	    }
	}
    }
}

for my $file (keys %tier_2) {
    for my $cond (keys $tier_2{$file} ) {
	for my $age (keys $tier_2{$file}{$cond}) {  
	    for my $outcome (keys $tier_2{$file}{$cond}{$age}) {
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $left_clip_for_plot=0;
		    $right_clip_for_plot=3;
		    $left_end_of_x_axis=0;
		    $right_end_of_x_axis=3;
		    $tick_interval=0.5;
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $left_clip_for_plot=-1.5;
		    $right_clip_for_plot=1.5;
		    $left_end_of_x_axis=-1.5;
		    $right_end_of_x_axis=1.5;
		    $tick_interval=0.25;
		}

		for my $sample_sex (keys $tier_2{$file}{$cond}{$age}{$outcome}) {
		    for my $parameter (keys $tier_2{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless ($parameter eq "Intercept") {
			    for my $stratum (keys $tier_2{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				my $studylist="";
				for my $namestudy (sort { ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$a] <=> ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$b] } @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }) {
				    if ($namestudy!~m/TO_BE_REMOVED/) {
    $studylist=$studylist." ".$namestudy;
}
}
if ((scalar (split " ",$studylist) < 2) || (my $rc=system("cat /home/amyh/SD_5HTTLPR/rmeta_script.3.r | R --slave --args ---file $file ---effects random ---outcome $outcome ---term $parameter ---study $studylist ---label $result_label ---logscale $lgscl ---groupname $groupname ---stratum $stratum ---age $age ---sex $sample_sex ---condition $cond ---clip $left_clip_for_plot $right_clip_for_plot ---axis $left_end_of_x_axis $right_end_of_x_axis $tick_interval") )) {
    print FAIL_FILE "$file $cond $outcome $sample_sex $parameter $stratum $studylist $result_label $lgscl $groupname $age\n";
}
			    }
			}
		    }
		}
	    }
	}
    }
}

for my $file (keys %tier_3) {
    for my $cond (keys $tier_3{$file} ) {
	for my $age (keys $tier_3{$file}{$cond}) {  
	    for my $outcome (keys $tier_3{$file}{$cond}{$age}) {
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $left_clip_for_plot=0;
		    $right_clip_for_plot=3;
		    $left_end_of_x_axis=0;
		    $right_end_of_x_axis=3;
		    $tick_interval=0.5;
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $left_clip_for_plot=-1.5;
		    $right_clip_for_plot=1.5;
		    $left_end_of_x_axis=-1.5;
		    $right_end_of_x_axis=1.5;
		    $tick_interval=0.25;
		}

		for my $sample_sex (keys $tier_3{$file}{$cond}{$age}{$outcome}) {
		    for my $parameter (keys $tier_3{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless ($parameter eq "Intercept") {
			    for my $stratum (keys $tier_3{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				my $studylist="";
				for my $namestudy (sort { ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$a] <=> ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$b] } @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }) {
				    if ($namestudy!~m/TO_BE_REMOVED/) {
    $studylist=$studylist." ".$namestudy;
}
}
if ((scalar (split " ",$studylist) < 2) || (my $rc=system("cat /home/amyh/SD_5HTTLPR/rmeta_script.3.r | R --slave --args ---file $file ---effects random ---outcome $outcome ---term $parameter ---study $studylist ---label $result_label ---logscale $lgscl ---groupname $groupname ---stratum $stratum ---age $age ---sex $sample_sex ---condition $cond ---clip $left_clip_for_plot $right_clip_for_plot ---axis $left_end_of_x_axis $right_end_of_x_axis $tick_interval") )) {
    print FAIL_FILE "$file $cond $outcome $sample_sex $parameter $stratum $studylist $result_label $lgscl $groupname $age\n";
}
			    }
			}
		    }
		}
	    }
	}
    }
}

for my $file (keys %tier_4) {
    for my $cond (keys $tier_4{$file} ) {
	for my $age (keys $tier_4{$file}{$cond}) {  
	    for my $outcome (keys $tier_4{$file}{$cond}{$age}) {
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $left_clip_for_plot=0;
		    $right_clip_for_plot=3;
		    $left_end_of_x_axis=0;
		    $right_end_of_x_axis=3;
		    $tick_interval=0.5;
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $left_clip_for_plot=-1.5;
		    $right_clip_for_plot=1.5;
		    $left_end_of_x_axis=-1.5;
		    $right_end_of_x_axis=1.5;
		    $tick_interval=0.25;
		}

		for my $sample_sex (keys $tier_4{$file}{$cond}{$age}{$outcome}) {
		    for my $parameter (keys $tier_4{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless ($parameter eq "Intercept") {
			    for my $stratum (keys $tier_4{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				my $studylist="";
				for my $namestudy (sort { ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$a] <=> ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$b] } @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }) {
				    if ($namestudy!~m/TO_BE_REMOVED/) {
    $studylist=$studylist." ".$namestudy;
}
}
if ((scalar (split " ",$studylist) < 2) || (my $rc=system("cat /home/amyh/SD_5HTTLPR/rmeta_script.3.r | R --slave --args ---file $file ---effects random ---outcome $outcome ---term $parameter ---study $studylist ---label $result_label ---logscale $lgscl ---groupname $groupname ---stratum $stratum ---age $age ---sex $sample_sex ---condition $cond ---clip $left_clip_for_plot $right_clip_for_plot ---axis $left_end_of_x_axis $right_end_of_x_axis $tick_interval") )) {
    print FAIL_FILE "$file $cond $outcome $sample_sex $parameter $stratum $studylist $result_label $lgscl $groupname $age\n";
}
			    }
			}
		    }
		}
	    }
	}
    }
}

for my $file (keys %tier_5) {
    for my $cond (keys $tier_5{$file} ) {
	for my $age (keys $tier_5{$file}{$cond}) {  
	    for my $outcome (keys $tier_5{$file}{$cond}{$age}) {
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $left_clip_for_plot=0;
		    $right_clip_for_plot=3;
		    $left_end_of_x_axis=0;
		    $right_end_of_x_axis=3;
		    $tick_interval=0.5;
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $left_clip_for_plot=-1.5;
		    $right_clip_for_plot=1.5;
		    $left_end_of_x_axis=-1.5;
		    $right_end_of_x_axis=1.5;
		    $tick_interval=0.25;
		}

		for my $sample_sex (keys $tier_5{$file}{$cond}{$age}{$outcome}) {
		    for my $parameter (keys $tier_5{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless ($parameter eq "Intercept") {
			    for my $stratum (keys $tier_5{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				my $studylist="";
				for my $namestudy (sort { ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$a] <=> ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$b] } @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }) {
				    if ($namestudy!~m/TO_BE_REMOVED/) {
    $studylist=$studylist." ".$namestudy;
}
}
if ((scalar (split " ",$studylist) < 2) || (my $rc=system("cat /home/amyh/SD_5HTTLPR/rmeta_script.3.r | R --slave --args ---file $file ---effects random ---outcome $outcome ---term $parameter ---study $studylist ---label $result_label ---logscale $lgscl ---groupname $groupname ---stratum $stratum ---age $age ---sex $sample_sex ---condition $cond ---clip $left_clip_for_plot $right_clip_for_plot ---axis $left_end_of_x_axis $right_end_of_x_axis $tick_interval") )) {
    print FAIL_FILE "$file $cond $outcome $sample_sex $parameter $stratum $studylist $result_label $lgscl $groupname $age\n";
}
			    }
			}
		    }
		}
	    }
	}
    }
}

for my $file (keys %tier_6) {
#    unless ($file eq '/home/amyh/SD_5HTTLPR/Returned/EU_studies_gt49_le20_debug3/meta_raw_files/female__age__add_5http__child_mal_exp__add_5http_x_child_mal_exp__null__null__null.csv') { next;} ###############
    for my $cond (keys $tier_6{$file} ) {
#	unless ($cond eq 'DepDx_DSM4ICD10_') { next;} ###############
	for my $age (keys $tier_6{$file}{$cond}) {  
#	    unless ($age eq 'All') { next;} ###############
	    for my $outcome (keys $tier_6{$file}{$cond}{$age}) {
#		unless ($outcome eq 'DD_life') { next;} ###############
		my $lgscl;
		my $result_label;

		if (($outcome=~m/^dep_dx/) || ($outcome=~m/^DD_/)) {
		    $lgscl="TRUE";
		    $result_label="OR";
		    $left_clip_for_plot=0;
		    $right_clip_for_plot=3;
		    $left_end_of_x_axis=0;
		    $right_end_of_x_axis=3;
		    $tick_interval=0.5;
		}
		elsif (($outcome=~m/^dep_q/) || ($outcome=~m/^QD/)) {
		    $lgscl="FALSE";
		    $result_label="Beta";
		    $left_clip_for_plot=-1.5;
		    $right_clip_for_plot=1.5;
		    $left_end_of_x_axis=-1.5;
		    $right_end_of_x_axis=1.5;
		    $tick_interval=0.25;
		}

		for my $sample_sex (keys $tier_6{$file}{$cond}{$age}{$outcome}) {
#		    unless ($sample_sex eq 'females-only') { next;} ###############
		    for my $parameter (keys $tier_6{$file}{$cond}{$age}{$outcome}{$sample_sex}) {
			unless ($parameter eq "Intercept") {
			    for my $stratum (keys $tier_6{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}) {
				my $studylist="";
				for my $namestudy (sort { ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$a] <=> ${ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }[$b] } @{ $list{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}{"array_ref"} }) { ############### sorting no longer necessary
				    if ($namestudy!~m/TO_BE_REMOVED/) {
					$studylist=$studylist." ".$namestudy;
				    }
				}
				if ((scalar (split " ",$studylist) < 2) || (my $rc=system("cat /home/amyh/SD_5HTTLPR/rmeta_script.3.r | R --slave --args ---file $file ---effects random ---outcome $outcome ---term $parameter ---study $studylist ---label $result_label ---logscale $lgscl ---groupname $groupname ---stratum $stratum ---age $age ---sex $sample_sex ---condition $cond ---clip $left_clip_for_plot $right_clip_for_plot ---axis $left_end_of_x_axis $right_end_of_x_axis $tick_interval") )) {
    print FAIL_FILE "$file $cond $outcome $sample_sex $parameter $stratum $studylist $result_label $lgscl $groupname $age\n";
}
			    }
			}
		    }
		}
	    }
	}
    }
}

close FAIL_FILE;

exit;



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

    return \@index;
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


##   coding #   variant $file 
#1     add_5http in $file
#2     Ldom_5http in $file or Lrec_5http in $file or add_rs25531 in $file
#3     Ldum in $file or L_Adom_5http in $file or L_Arec_5http in $file
#4     L_Adum in $file
#
##   interaction $file
#1     _x_ in $file(name)
#3     _x_ NOT in $file(name)
#
##   stress $file
#1     _exp in $file
#2     _quant in $file and _z NOT in $file and _quant2 NOT in $file and stress_combined NOT in $file
#3     _quant in $file and (_z in $file or quant2 in $file or stress_combined in $file) 
#
##  covars $file
#1    female + age + NO birth_decade in $file
#5    female + NO age + NO birth_decade OR
#     female + age + birth_decade
#6    NO female OR
#     female + birth_decade + NO age
#
##   cond $cond
#1     DSM,DSMICD,ANY
#6     Dep[QD] and _Other_
#
##   timing ($outcome)
#1    before
#1.1  no_t (not before or 5yr)
#6    5yr
#
##  outcome $outcome
#1    DD_curr or DD_life
#2    QD_curr or QD_life
#3    QDz_curr or QDz_life
#4    not _curr and not _life
#
##   sample_sex $sample_sex
#1    both
#6    female/male
#
##   stratum $stratum
#1    full
#6    0/1/2


sub tierer {
    my $file=shift;
    my $cond=shift;
    my $age=shift;
    my $outcome=shift;
    my $sample_sex=shift;
    my $parameter=shift;
    my $stratum=shift;
#    my $namestudy=shift;
#    my $inhashref=shift;
    my $inhashref1=shift;
    my $inhashref1b=shift;
    my $inhashref2=shift;
    my $inhashref3=shift;
    my $inhashref4=shift;
    my $inhashref5=shift;
    my $inhashref6=shift;

#    my %tier_1;
#    my %tier_1b;
#    my %tier_2;
#    my %tier_3;
#    my %tier_4;
#    my %tier_5;
#    my %tier_6;
##    my %inhash=%{ $inhashref };

    if((($file=~m/add_5http/) &&
($file=~m/_x_/) &&
($file=~m/_exp/) &&
($file=~m/female/) &&
($file=~m/age/) &&
($file!~m/birth_decade/)) &&

(($cond eq "DepDx_ANY_") ||
($cond eq "DepDx_DSM4_") || 
($cond eq "DepDx_DSM4ICD10_") || 
($cond eq "DepQ_ANY_") || 
($cond eq "DepQ_DSM4_") || 
($cond eq "DepQ_DSM4ICD10_"))  &&

(($outcome=~m/DD_/) &&
(($outcome=~m/_curr/) ||
($outcome=~m/_life/)) &&   
(($outcome=~m/_before/) ||
($file=~m/child/))) && 

($sample_sex=~m/combined/) &&

($stratum eq "full")) {
    ${ $inhashref1 }{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}=1;
}
elsif((($file=~m/add_5http/) &&
($file=~m/_x_/) &&
($file=~m/_exp/) &&
($file=~m/female/) &&
($file=~m/age/) &&
($file!~m/birth_decade/)) &&

(($cond eq "DepDx_ANY_") ||
($cond eq "DepDx_DSM4_") || 
($cond eq "DepDx_DSM4ICD10_") || 
($cond eq "DepQ_ANY_") || 
($cond eq "DepQ_DSM4_") || 
($cond eq "DepQ_DSM4ICD10_"))  &&

(($outcome=~m/DD_/) &&
(($outcome=~m/_curr/) ||
($outcome=~m/_life/)) &&   
($outcome!~m/_before/) &&
($outcome!~m/_5yr/)) &&

($sample_sex=~m/combined/) &&

($stratum eq "full")) {
    ${ $inhashref1b }{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}=1;
}
elsif(((($file=~m/add_5http/) &&
($file=~m/_x_/) &&
($file=~m/_exp/) &&
($file=~m/female/) &&
($file=~m/age/) &&
($file!~m/birth_decade/)) ||

(($file=~m/_x_/) &&
($file=~m/female/) &&
($file=~m/age/) &&
($file!~m/birth_decade/) &&
((($file=~m/add_5http/) &&
($file=~m/_quant/) &&
($file!~m/_z/) &&
($file!~m/_quant2/) &&
($file!~m/stress_combined/)) ||
((($file=~m/Ldom_5http/) ||
($file=~m/Lrec_5http/) ||
($file=~m/add_rs25531/)) &&
((($file=~m/_quant/) &&
($file!~m/_z/) &&
($file!~m/_quant2/) &&
($file!~m/stress_combined/)) ||
($file=~m/_exp/)))))) &&

(($cond eq "DepDx_ANY_") ||
($cond eq "DepDx_DSM4_") || 
($cond eq "DepDx_DSM4ICD10_") || 
($cond eq "DepQ_ANY_") || 
($cond eq "DepQ_DSM4_") || 
($cond eq "DepQ_DSM4ICD10_"))  &&

((($outcome=~m/DD_/) || 
($outcome=~m/QD_/)) &&
(($outcome=~m/_curr/) ||
($outcome=~m/_life/)) &&   
($outcome!~m/_5yr/)) &&

($sample_sex=~m/combined/) &&

($stratum eq "full")) {
    ${ $inhashref2 }{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}=1;
}
elsif((($file=~m/female/) &&
($file=~m/age/) &&
($file!~m/birth_decade/) &&
(($file=~m/add_5http/) ||
($file=~m/Ldom_5http/) ||
($file=~m/Lrec_5http/) ||
($file=~m/add_rs25531/) ||
($file=~m/Ldum/) ||
($file=~m/L_Arec_5http/) ||
($file=~m/L_Adom_5http/))) &&

(($cond eq "DepDx_ANY_") ||
($cond eq "DepDx_DSM4_") || 
($cond eq "DepDx_DSM4ICD10_") || 
($cond eq "DepQ_ANY_") || 
($cond eq "DepQ_DSM4_") || 
($cond eq "DepQ_DSM4ICD10_"))  &&

((($outcome=~m/DD_/) || 
($outcome=~m/QD_/) || 
($outcome=~m/QDz_/)) &&
(($outcome=~m/_curr/) ||
($outcome=~m/_life/)) &&   
($outcome!~m/_5yr/)) &&

($sample_sex=~m/combined/) &&

($stratum eq "full")) {
    ${ $inhashref3 }{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}=1;
}
elsif((($file=~m/female/) &&
($file=~m/age/) &&
($file!~m/birth_decade/)) &&

(($file=~m/5http/) ||
($file=~m/rs25531/)) &&

(($file=~m/_exp/) ||
($file=~m/_quant/)) &&

(($cond eq "DepDx_ANY_") ||
($cond eq "DepDx_DSM4_") || 
($cond eq "DepDx_DSM4ICD10_") || 
($cond eq "DepQ_ANY_") || 
($cond eq "DepQ_DSM4_") || 
($cond eq "DepQ_DSM4ICD10_"))  &&

($outcome!~m/dep_/) &&

($sample_sex=~m/combined/) &&

($stratum eq "full")) {
    ${ $inhashref4 }{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}=1;
}
elsif((($file=~m/female/) &&
((($file=~m/age/) &&
($file=~m/birth_decade/)) ||
(($file!~m/age/) &&
($file!~m/birth_decade/)))) &&

(($file=~m/5http/) ||
($file=~m/rs25531/)) &&

(($file=~m/_exp/) ||
($file=~m/_quant/)) &&

(($cond eq "DepDx_ANY_") ||
($cond eq "DepDx_DSM4_") || 
($cond eq "DepDx_DSM4ICD10_") || 
($cond eq "DepQ_ANY_") || 
($cond eq "DepQ_DSM4_") || 
($cond eq "DepQ_DSM4ICD10_"))  &&

($outcome!~m/dep_/) &&

($sample_sex=~m/combined/) &&

($stratum eq "full")) {
    ${ $inhashref5 }{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}=1;
}
else {
    ${ $inhashref6 }{$file}{$cond}{$age}{$outcome}{$sample_sex}{$parameter}{$stratum}=1;
}

return \%{ $inhashref1 },\%{ $inhashref1b },\%{ $inhashref2 },\%{ $inhashref3 },\%{ $inhashref4 },\%{ $inhashref5 },\%{ $inhashref6 };
}

