backend default {
    .host = "127.0.0.1";
    .port = "3000";
}

acl purge {
    "localhost";
    "192.168.1.0"/24;
}

sub vcl_recv {
    if (req.request == "PURGE" || req.request == "GRACE") {
        if (!client.ip ~ purge) {
            error 405 "Not allowed.";
        }

        if (req.request == "GRACE") {
            set req.grace = 2s;
        }

        return (lookup);
    }
}

sub vcl_pass {
    if (req.request == "GRACE") {
        set bereq.request = "GET";
    }
}

sub vcl_hit {
    if (req.request == "PURGE" || req.request == "GRACE") {
        set obj.ttl = 0s;

        if (req.request == "GRACE") {
            set req.request = "GET";
            set obj.grace = 2s;
            return (restart);
        }

        error 200 "Purged.";
    }
}

sub vcl_miss {
    if (req.request == "PURGE") {
        error 404 "Not in cache.";
    } elsif (req.request == "GRACE") {
        set bereq.request = "GET";
        return (fetch);
    }
}

sub vcl_fetch {
    set beresp.do_esi = true;

    if (req.request == "GRACE") {
        set beresp.grace = 2s;
    }
}
