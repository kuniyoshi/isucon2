#!/bin/sh
sudo mv /usr/local/nginx/logs/access.log{,.$(date +%H:%M:%S)}
sudo mv /usr/local/nginx/logs/error.log{,.$(date +%H:%M:%S)}
sudo /etc/init.d/nginx restart
