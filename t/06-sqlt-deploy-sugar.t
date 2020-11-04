use Test2::V0;
no warnings 'once';

package Mock_sqlt_table {
	sub new { bless {}, shift }
	sub calls { $_[0]{calls} ||= [] }
	sub add_index { push @{shift->calls}, [ add_index => @_ ] }
	sub add_constraint { push @{shift->calls}, [ add_constraint => @_ ] }
};

my $ret= eval q{
	package test::Table1;
	use DBIx::Class::ResultDDL -V1;
	table 'table1';
	col c0 => integer, auto_inc;
	col c1 => integer;
	primary_key 'c0';
	add_index(name => 'x', fields => ['c1']);
	add_constraint(name => 'y');
	1;
};
my $err= $@;
ok( $ret, 'eval Table1' ) or diag $err;
ok( test::Table1->can('sqlt_deploy_hook'), 'deploy hook installed' );
my $mock_sqlt_table= Mock_sqlt_table->new;
test::Table1->sqlt_deploy_hook($mock_sqlt_table);
is( $mock_sqlt_table->calls,
	array {
		item 0 => array {
			item 0 => 'add_index';
			item 1 => 'name';
			item 2 => 'x';
			item 3 => 'fields';
			item 4 => array { item 0 => 'c1'; };
		};
		item 1 => array {
			item 0 => 'add_constraint';
			item 1 => 'name';
			item 2 => 'y';
		};
	},
	'correct call'
);

done_testing;
