#!/usr/bin/env bash
# curl -v -H "Content-Type: application/xml" -d '<play player-id="1"><discard suit="bamboo" spec="2" /></play>' http://localhost:4040
curl -v -H "Content-Type: application/xml" -d '<play player-id="2"><chii><tile suit="bamboo" spec="1"/><tile suit="bamboo" spec="3"/></chii></play>' http://localhost:4040
