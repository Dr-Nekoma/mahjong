#!/usr/bin/env bash
curl -v -H "Content-Type: application/xml" -d '<play player-id="1"><discard suit="bamboo" spec="9" /></play>' http://localhost:4040
