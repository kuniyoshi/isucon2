package Isucon2;
use utf8;
use strict;
use warnings;
use autodie qw( open close );
use Kossy;
use DBIx::Sunny;
use JSON qw( decode_json );

sub config {
    my $self = shift;

    return $self->{_config} ||= do {
        my $env = $ENV{ISUCON_ENV} || 'local';
        open my $FH, '<', $self->root_dir . "/../config/common.${env}.json";
        my $json = do { local $/; <$FH> };
        close $FH;
        decode_json( $json );
    };
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

filter 'recent_sold' => sub {
    my( $app ) = @_;

    return sub {
        my( $self, $c ) = @_;
        $c->stash->{recent_sold} = $self->dbh->select_all( <<END_SQL );
SELECT stock.seat_id, variation.name AS v_name, ticket.name AS t_name, artist.name AS a_name
FROM stock
JOIN variation ON stock.variation_id = variation.id
JOIN ticket ON variation.ticket_id = ticket.id
JOIN artist ON ticket.artist_id = artist.id
WHERE order_id IS NOT NULL
ORDER BY order_id DESC LIMIT 10
END_SQL
        $app->( $self, $c );
    };
};

get '/' => [ qw( recent_sold ) ] => sub {
    my( $self, $c ) = @_;
    my $rows = $self->dbh->select_all( 'SELECT * FROM artist ORDER BY id' );

    return $c->render( 'index.tx', { artists => $rows } );
};

get '/artist/:artistid' => [ qw( recent_sold ) ] => sub {
    my( $self, $c ) = @_;

    my $artist = $self->dbh->select_row(
        'SELECT id, name FROM artist WHERE id = ? LIMIT 1',
        $c->args->{artistid},
    );
    my $tickets = $self->dbh->select_all(
        'SELECT id, name FROM ticket WHERE artist_id = ? ORDER BY id',
        $artist->{id},
    );

    for my $ticket ( @$tickets ) {
        my $count = $self->dbh->select_one(
            <<END_SQL,
SELECT COUNT(*) FROM variation
INNER JOIN stock ON stock.variation_id = variation.id
WHERE variation.ticket_id = ? AND stock.order_id IS NULL
END_SQL
            $ticket->{id},
        );
        $ticket->{count} = $count;
    }

    return $c->render(
        'artist.tx',
        {
            artist  => $artist,
            tickets => $tickets,
        },
    );
};

get '/ticket/:ticketid' => [ qw( recent_sold ) ] => sub {
    my( $self, $c ) = @_;

    my $ticket = $self->dbh->select_row(
        <<END_SQL,
SELECT t.*, a.name AS artist_name
FROM ticket t INNER JOIN artist a ON t.artist_id = a.id
WHERE t.id = ? LIMIT 1
END_SQL
        $c->args->{ticketid},
    );
    my $variations = $self->dbh->select_all(
        'SELECT id, name FROM variation WHERE ticket_id = ? ORDER BY id',
        $ticket->{id},
    );

    for my $variation ( @{ $variations } ) {
        $variation->{stock} = $self->dbh->selectall_hashref(
            'SELECT seat_id, order_id FROM stock WHERE variation_id = ?',
            'seat_id',
            { },
            $variation->{id},
        );
        $variation->{vacancy} = $self->dbh->select_one(
            'SELECT COUNT(*) FROM stock WHERE variation_id = ? AND order_id IS NULL',
            $variation->{id},
        );
    }

    return $c->render(
        'ticket.tx',
        {
            ticket     => $ticket,
            variations => $variations,
        }
    );
};

post '/buy' => sub {
    my( $self, $c ) = @_;
    my $variation_id = $c->req->param( 'variation_id' );
    my $member_id    = $c->req->param( 'member_id' );

    my $txn = $self->dbh->txn_scope();
    $self->dbh->query(
        'INSERT INTO order_request (member_id) VALUES (?)',
        $member_id,
    );
    my $order_id = $self->dbh->last_insert_id;
    my $rows     = $self->dbh->query(
        <<END_SQL,
UPDATE stock SET order_id = ?
WHERE variation_id = ? AND order_id IS NULL
ORDER BY RAND() LIMIT 1
END_SQL
        $order_id,
        $variation_id,
    );

    if ( $rows > 0 ) {
        my $seat_id = $self->dbh->select_one(
            'SELECT seat_id FROM stock WHERE order_id = ? LIMIT 1',
            $order_id,
        );
        $txn->commit;
        return $c->render(
            'complete.tx',
            { seat_id => $seat_id, member_id => $member_id },
        );
    }
    else {
        $txn->rollback;
        return $c->render( 'soldout.tx' );
    }
};

# admin

get '/admin' => sub {
    my( $self, $c ) = @_;

    return $c->render( 'admin.tx' );
};

get '/admin/order.csv' => sub {
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

    $c->res->content_type( 'text/csv' );
    $c->res->body( $body );

    return $c->res;
};

post '/admin' => sub {
    my( $self, $c ) = @_;

    open my $fh, '<', $self->root_dir . '/../config/database/initial_data.sql';

    while ( <$fh> ) {
        chomp( my $sql = $_ );

        next
            unless $sql;

        $self->dbh->query( $sql );
    }

    close $fh;

    return $c->redirect( '/admin' );
};

1;
