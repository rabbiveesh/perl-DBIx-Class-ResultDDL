use Test2::V0;
no warnings 'once';

my $ret= eval q{
	package test::Table3;
	use DBIx::Class::ResultDDL -V0;
	table 'table3';
	col c0 => integer, auto_inc;
	col c1 => char(50), null, default("foo");
	col c2 => varchar(10);
	col c3 => date;
	col c4 => datetime('floating');
	col c5 => datetime('UTC');
	primary_key 'c0';
	1;
};
my $err= $@;
ok( $ret, 'eval column defs' ) or diag $err;

subtest auto_inc => sub {
	my @auto_inc0= eval q{
		package test::Autoinc0;
		use DBIx::Class::ResultDDL -V0;
		auto_inc
	} or diag $@;
	is \@auto_inc0, [ is_auto_increment => 1 ],
		'V0 auto_inc is just boolean flag';
	
	my @auto_inc1= eval q{
		package test::Autoinc1;
		use DBIx::Class::ResultDDL -V1;
		auto_inc
	} or diag $@;
	is \@auto_inc1, [ is_auto_increment => 1, 'extra.auto_increment_type' => 'monotonic' ],
		'V1 auto_inc also sets sqlite monotonic flag';
};

done_testing;
