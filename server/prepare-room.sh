#!/usr/bin/env bash
for i in {1..4}
do
    echo "Call #$i"
    curl -Nv \
     -H "Accept: text/event-stream" \
     -H "Cache-Control: no-cache" \
     -H "Connection: keep-alive" \
     http://localhost:4040/connect &
    sleep 1
    # curl -vH "Content-Type: application/xml" -d "<ready player-id=\"$i\"/>" http://localhost:4040
done
