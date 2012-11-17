#!/usr/bin/perl
use 5.10.0;
use utf8;
use strict;
use warnings;
use open qw( :utf8 :std );
use Data::Dumper;
use Time::HiRes qw( sleep gettimeofday tv_interval );
use lib "/home/isucon/isucon2/webapp/perl/extlib/lib/perl5";
use Furl;

my $MAX_INTERVAL = 0.90;
my @URLS         = (
    "http://192.168.1.121/recent_sold",
    "http://192.168.1.121/",
    "http://192.168.1.121/artist/1",
    "http://192.168.1.121/artist/2",
    "http://192.168.1.121/ticket/1",
    "http://192.168.1.121/ticket/2",
    "http://192.168.1.121/ticket/3",
    "http://192.168.1.121/ticket/4",
    "http://192.168.1.121/ticket/5",
);

my $ua = Furl->new;

while ( 1 ) {
    my $start = [ gettimeofday ];

    foreach my $url ( @URLS ) {
        my $res = $ua->request(
            method => "GRACE",
            url    => $url,
        );

        warn "GRACE $url";
    }

    my $interval = $MAX_INTERVAL - tv_interval( $start );

    if ( $interval > 0 ) {
        sleep $interval;
        warn "slept: ${interval}s";
    }
}
