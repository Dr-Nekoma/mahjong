#!/usr/bin/env bash
curl -v -H "Content-Type: application/xml" -d '<play player-id="1" action="&lt;discard suit=&quot;wind&quot; spec=&quot;east&quot; /&gt;" />' http://localhost:4040
