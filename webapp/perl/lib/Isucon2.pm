package Isucon2;
use 5.10.0;
use utf8;
use strict;
use warnings;
use autodie qw( open close );
use Data::Dumper;
use Time::HiRes qw( tv_interval gettimeofday );
use Time::StopWatchWithMessage;
use JSON qw( decode_json );
use DBIx::Sunny;
use Furl;
use Kossy;
use Cache::Memcached;

our $VERSION = "0.01";

sub config {
    my $self = shift;

    return $self->{_config} ||= do {
        my $env = $ENV{ISUCON_ENV} || "local";
        open my $FH, "<", $self->root_dir . "/../config/common.${env}.json";
        my $json = do { local $/; <$FH> };
        close $FH;
        decode_json( $json );
    };
}

sub ua {
    my $self = shift;

    return $self->{_ua} ||= Furl->new;
}

sub memd {
    my $self = shift;

    return $self->{_memd} ||= Cache::Memcached->new(
        servers => [ "192.168.1.122:11211" ],
    );
}

sub dbh {
    my( $self ) = @_;

    return $self->{_dbh} ||= do {
        my $dbconf = $self->config->{database};

        DBIx::Sunny->connect(
            "dbi:mysql:database=${$dbconf}{dbname};host=${$dbconf}{host};port=${$dbconf}{port}",
            $dbconf->{username},
            $dbconf->{password},
            {
                RaiseError           => 1,
                PrintError           => 0,
                ShowErrorStatement   => 1,
                AutoInactiveDestroy  => 1,
                mysql_enable_utf8    => 1,
                mysql_auto_reconnect => 1,
            },
        );
    };
}

sub dumper { Data::Dumper->new( [ splice @_, 1 ] )->Terse( 1 )->Sortkeys( 1 )->Indent( 1 )->Dump }

sub watch { Time::StopWatchWithMessage->new }

sub select_all {
    my $self = shift;
    my $rows_ref = $self->dbh->select_all( @_ );
    return @{ $rows_ref };
}

sub variation {
    my $self = shift;
    my $id   = shift;

    state $VARIATION = do {
        my %VARIATION;
        foreach my $cols_ref ( $self->select_all( "SELECT id, name, ticket_id FROM variation" ) ) {
            $VARIATION{ $cols_ref->{id} } = {
                name      => $cols_ref->{name},
                ticket_id => $cols_ref->{ticket_id},
            };
        }
        \%VARIATION;
    };

    return $VARIATION->{ $id };
}

sub ticket {
    my $self = shift;
    my $id   = shift;

    state $TICKET = do {
        my %TICKET;
        foreach my $cols_ref ( $self->select_all( "SELECT id, name, artist_id FROM ticket" ) ) {
            $TICKET{ $cols_ref->{id} } = {
                name      => $cols_ref->{name},
                artist_id => $cols_ref->{artist_id},
            };
        }
        \%TICKET;
    };

    return $TICKET->{ $id };
}

sub artist {
    my $self = shift;
    my $id   = shift;

    state $ARTIST = do {
        my %ARTIST;
        foreach my $cols_ref ( $self->select_all( "SELECT id, name FROM artist" ) ) {
            $ARTIST{ $cols_ref->{id} } = {
                name => $cols_ref->{name},
            };
        }
        \%ARTIST;
    };

    return $ARTIST->{ $id };
}

filter "recent_sold" => sub {
    my( $app ) = @_;

    return sub {
        my( $self, $c ) = @_;
        my $dbh = $self->dbh;
        my @orders;

        GET_ORDERS: {
            my @order_ids = map { $_->{id} } $self->select_all( <<END_SQL );
SELECT id FROM order_request ORDER BY id DESC LIMIT 10
END_SQL

            last GET_ORDERS
                unless @order_ids;

            my @stocks = $self->select_all(
                sprintf(
                    <<END_SQL,
SELECT variation_id, seat_id, order_id FROM stock WHERE order_id IN (%s)
END_SQL
                    join q{,}, ( "?" ) x @order_ids,
                ),
                @order_ids,
            );

            foreach my $stock_ref ( sort { $b->{order_id} <=> $a->{order_id} } @stocks ) {
                my $variation_id = $stock_ref->{variation_id};
                my $ticket_id    = $self->variation( $variation_id )->{ticket_id};
                my $artist_id    = $self->ticket( $ticket_id )->{artist_id};
                my %order        = (
                    seat_id => $stock_ref->{seat_id},
                    v_name  => $self->variation( $variation_id )->{name},
                    t_name  => $self->ticket( $ticket_id )->{name},
                    a_name  => $self->artist( $artist_id )->{name},
                );

                push @orders, \%order;
            }
        }

        $c->stash->{recent_sold} = \@orders;

        return $app->( $self, $c );
    };
};

get "/recent_sold" => [ qw( recent_sold ) ] => sub {
    my( $self, $c ) = @_;

    return $c->render( "recent_sold.tx" );
};

get "/" => sub {
    my( $self, $c ) = @_;
    my $rows = $self->dbh->select_all( "SELECT * FROM artist ORDER BY id" );

    return $c->render( "index.tx", { artists => $rows } );
};

get "/artist/:artistid" => sub {
    my( $self, $c ) = @_;

    my $artist = $self->dbh->select_row(
        "SELECT id, name FROM artist WHERE id = ? LIMIT 1",
        $c->args->{artistid},
    );
    my $tickets = $self->dbh->select_all(
        "SELECT id, name FROM ticket WHERE artist_id = ? ORDER BY id",
        $artist->{id},
    );

    for my $ticket ( @$tickets ) {
        my @variation_ids = $self->select_all(
            "SELECT id FROM variation WHERE ticket_id = ?",
            $ticket->{id},
        );
        my $count = $self->dbh->select_one(
            sprintf(
                <<END_SQL,
SELECT COUNT(*) FROM stock
WHERE variation_id IN (%s) AND order_id IS NULL
END_SQL
                join q{,}, ( "?" ) x @variation_ids,
            ),
            @variation_ids,
        );
        $ticket->{count} = $count;
    }

    return $c->render(
        "artist.tx",
        {
            artist  => $artist,
            tickets => $tickets,
        },
    );
};

get "/ticket/:ticketid" => sub {
    my( $self, $c ) = @_;
    my $ticket_id   = $c->args->{ticketid};
    my %ticket      = %{ $self->ticket( $ticket_id ) };
    $ticket{id}          = $ticket_id;
    $ticket{artist_name} = $self->artist( $ticket{artist_id} )->{name};

    my $variations = $self->dbh->select_all(
        "SELECT id, name FROM variation WHERE ticket_id = ? ORDER BY id",
        $ticket{id},
    );

    for my $variation ( @{ $variations } ) {
        $variation->{stock} = $self->dbh->selectall_hashref(
            "SELECT seat_id, order_id FROM stock WHERE variation_id = ?",
            "seat_id",
            { },
            $variation->{id},
        );
        $variation->{vacancy} = $self->dbh->select_one(
            "SELECT COUNT(*) FROM stock WHERE variation_id = ? AND order_id IS NULL",
            $variation->{id},
        );
    }

    return $c->render(
        "ticket.tx",
        {
            ticket     => \%ticket,
            variations => $variations,
        },
    );
};

sub init_cache_data {
    my $self = shift;
    my $memd = $self->memd;
    my $dbh  = $self->dbh;

    my @variation_ids = map { $_->{id} } @{ $dbh->select_all( "SELECT id FROM variation" ) };

    foreach my $id ( @variation_ids ) {
        $memd->set( "nth:$id", 0 );

        my @seat_ids = map { $_->{seat_id} } @{ $dbh->select_all( <<END_SQL, $id ) };
SELECT seat_id FROM stock WHERE variation_id = ? ORDER BY md5(seat_id)
END_SQL

        $memd->set( "seat_id:$id", \@seat_ids );
    }
}

sub get_seat_id {
    my $self         = shift;
    my $order_id     = shift;
    my $variation_id = shift;
    my $memd         = $self->memd;

    $memd->incr( "nth:$variation_id" );
    my $nth = $memd->get( "nth:$variation_id" );

    my $seat_id_ref = $memd->get( "seat_id:$variation_id" );
    my $seat_id     = $seat_id_ref->[ $nth - 1 ];
    $memd->set( "order:$seat_id", $order_id );

    return $seat_id;
}

post "/buy" => sub {
    my( $self, $c ) = @_;

$self->watch->start( "buy" );

    my $variation_id = $c->req->param( "variation_id" );
    my $member_id    = $c->req->param( "member_id" );
    my $dbh          = $self->dbh;

    my $txn = $dbh->txn_scope;
    $dbh->query(
        "INSERT INTO order_request (member_id) VALUES (?)",
        $member_id,
    );
    my $order_id = $dbh->last_insert_id;

    my $seat_id  = $self->get_seat_id( $order_id, $variation_id );

$self->watch->stop->start( "update" );

$self->watch->stop->start( "transaction" );

    if ( $seat_id ) {
        $txn->commit;
$self->watch->stop->start( "render" );
        my $res = $c->render(
            "complete.tx",
            { seat_id => $seat_id, member_id => $member_id },
        );
#$self->watch->stop->warn;
        return $res;
    }
    else {
        $txn->rollback;
        return $c->render( "soldout.tx" );
    }
};

# admin

get "/admin" => sub {
    my( $self, $c ) = @_;

    return $c->render( "admin.tx" );
};

get "/admin/order.csv" => sub {
    my( $self, $c ) = @_;
    my $body        = q{};

    my $orders = $self->dbh->select_all( <<END_SQL );
SELECT order_request.*, stock.seat_id, stock.variation_id, stock.updated_at
FROM order_request JOIN stock ON order_request.id = stock.order_id
ORDER BY order_request.id ASC
END_SQL

    for my $order ( @$orders ) {
        $body .= join q{,}, @{ $order }{ qw( id member_id seat_id variation_id updated_at ) };
        $body .= "\n";
    }

    $c->res->content_type( "text/csv" );
    $c->res->body( $body );

    return $c->res;
};

post "/admin" => sub {
    my( $self, $c ) = @_;

    open my $fh, "<", $self->root_dir . "/../config/database/initial_data.sql";

    while ( <$fh> ) {
        chomp( my $sql = $_ );

        next
            unless $sql;

        $self->dbh->query( $sql );
    }

    close $fh;

    $self->init_cache_data;

    return $c->redirect( "/admin" );
};

1;
