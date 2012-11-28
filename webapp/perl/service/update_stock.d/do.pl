#!/usr/bin/perl
use 5.10.0;
use utf8;
use strict;
use warnings;
use open qw( :utf8 :std );
use Time::HiRes qw( gettimeofday tv_interval sleep );
use Data::Dumper;
use lib qw(
    /home/isucon/isucon2/webapp/perl/extlib/lib/perl5
    /home/isucon/isucon2/webapp/perl/extlib/lib/perl5/x86_64-linux-thread-multi
);
use DBI;
use Cache::Memcached;

my $MAX_INTERVAL = 0.5;

my $dbh = DBI->connect(
    "dbi:mysql:database=isucon2;host=192.168.1.124",
    "isucon2app",
    "isunageruna",
    { RaiseError => 1 },
)
    or die $DBI::errstr;

my $memd = Cache::Memcached->new(
    servers => [ "192.168.1.122:11211" ],
);

my $sth = $dbh->prepare( <<END_SQL );
SELECT id FROM variation
END_SQL
$sth->execute;
my @variation_ids = map { $_->[0] } @{ $sth->fetchall_arrayref };

$sth = $dbh->prepare( <<END_SQL );
UPDATE stock SET order_id = ? WHERE order_id IS NULL AND seat_id = ?
END_SQL

my %prev;

while ( 1 ) {
    my $start = [ gettimeofday ];

    foreach my $variation_id ( @variation_ids ) {
        my $seat_id_ref = $memd->get( "seat_id:$variation_id" )
            or next; # Does not enough for POST /admin
        my $nth = $memd->get( "nth:$variation_id" )
            or next;

        foreach my $xth ( ( $prev{ $variation_id } || 1 ) .. $nth ) {
            my $seat_id = $seat_id_ref->[ $xth - 1 ];
            my $order_id = $memd->get( "order:$seat_id" )
                or next;
            $sth->execute( $order_id, $seat_id );
        }

        $prev{ $variation_id } = $nth;
    }

    my $sleep_time = $MAX_INTERVAL - tv_interval( $start );

    if ( $sleep_time > 0 ) {
        sleep $sleep_time;
        warn "$sleep_time [s] slept.";
    }
}
