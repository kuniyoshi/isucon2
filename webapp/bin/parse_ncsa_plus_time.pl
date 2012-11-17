#!/usr/bin/perl
use 5.10.0;
use utf8;
use strict;
use warnings;
use open qw( :utf8 :std );
use Data::Dumper;
use autodie qw( open close );

open my $FH, "<", "log.d/access.log";

my %count;
my %sum;
my %average;

while ( <$FH> ) {
    chomp( my $line = $_ );
    my %log = parse_line( $line );
    my $path = normalize_path( $log{path} );
    my $key = join q{ }, $log{method}, $path;
    $count{ $key }++;
    $sum{ $key } += $log{spent};
}

foreach my $key ( sort keys %count ) {
    $average{ $key } = $sum{ $key } / $count{ $key };

    say $key;
    say "\tcount:\t$count{$key}";
    printf "\tsum:\t%.2f\n", $sum{$key};
    printf "\tavg:\t%.2f\n", $average{$key};
}

close $FH;

exit;

sub normalize_path {
    my $path = shift;
    if ( $path =~ m{\A /admin/}msx ) {
        return "/admin";
    }
    elsif ( $path =~ m{\A /artist/\d+ \z}msx ) {
        return "/artist";
    }
    elsif ( $path =~ m{\A /css/ }msx ) {
        return "/css";
    }
    elsif ( $path =~ m{\A /images/ }msx ) {
        return "/images";
    }
    elsif ( $path =~ m{\A /js/ }msx ) {
        return "/js";
    }
    elsif ( $path =~ m{\A /ticket/\d+ \z}msx ) {
        return "/ticket";
    }
    return $path;
}

sub parse_line {
    my $line = shift;
    my %log;
    @log{ qw( ip garbage garbage date time_zone method path version status other ) }
        = split m{ }, $line, 10;
    delete $log{garbage};
    $log{date} = join q{ }, delete @log{ qw( date time_zone ) };
    $log{query} = join q{ }, @log{ qw( method path version ) };
    $log{method} =~ s{\A "}{}msx;
    ( $log{spent} ) = $log{other} =~ m{"([.\d]+)" \z}msx;
    return %log;
}

__END__
127.0.0.1 - - [13/Nov/2012:06:27:57 +0900] "POST /comment/3030 HTTP/1.1" 302 5 "-" "http_load 12mar2006" "192.168.1.101" "0.016"
