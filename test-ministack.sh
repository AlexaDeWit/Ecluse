#!/bin/bash
docker run -d -p 4566:4566 ministackorg/ministack@sha256:5164592def36af01b8ac76364028e27c5ecd8f1494c8a53d5fcd811cc7dfb594
sleep 5
curl -v -X PUT http://localhost:4566/test.osv.bucket
curl -v -X PUT http://localhost:4566/test.osv.bucket/npm-v0.1.0-osv.db -d "some body"
docker ps -q | xargs docker stop
