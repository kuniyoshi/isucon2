#!/usr/bin/perl
use 5.10.0;
use utf8;
use strict;
use warnings;
use open qw( :utf8 :std );
use Data::Dumper;
use Digest::MD5 qw( md5_hex );
use lib qw( extlib/lib/perl5 extlib/lib/perl5/x86_64-linux-thread-multi );
use DBI;

$Data::Dumper::Terse    = 1;
$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Indent   = 1;

my $dbh = DBI->connect(
    "dbi:mysql:database=isucon2;host=192.168.1.124",
    "isucon2app",
    "isunageruna",
    { RaiseError => 1 },
)
    or die $DBI::errstr;

$dbh->do( <<END_SQL );
DROP TABLE IF EXISTS random_seat
END_SQL

$dbh->do( <<END_SQL );
CREATE TABLE random_seat (
    id           int unsigned NOT NULL AUTO_INCREMENT,
    variation_id int unsigned NOT NULL DEFAULT 0,
    order_nth    int unsigned NOT NULL DEFAULT 0,
    stock_id     int unsigned NOT NULL DEFAULT 0,
    PRIMARY KEY (id),
    KEY variation_stock (variation_id, stock_id)
)
END_SQL

my @variation_ids = do {
    my $sth = $dbh->prepare( <<END_SQL );
SELECT id FROM variation
END_SQL
    $sth->execute;
    map { $_->[0] } @{ $sth->fetchall_arrayref };
};

foreach my $variation_id ( @variation_ids ) {
    my $sth = $dbh->prepare( <<END_SQL );
SELECT id
FROM stock
WHERE variation_id = ?
ORDER BY md5(seat_id)
END_SQL
    $sth->execute( $variation_id );
    my @ids = map { $_->[0] } @{ $sth->fetchall_arrayref };

    $sth = $dbh->prepare( <<END_SQL );
INSERT INTO random_seat
SET variation_id = ?, order_nth = ?, stock_id = ?
END_SQL

    for ( my $i = 0; $i < @ids; $i++ ) {
        my $id = $ids[ $i ];
        $sth->execute( $variation_id, $i + 1, $id );
    }
}

exit;

