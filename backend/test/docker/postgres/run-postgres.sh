#!/bin/sh
docker run -p 127.0.0.1:5432:5432 -it --rm -e POSTGRES_DB=app -e POSTGRES_USER=app -e POSTGRES_PASSWORD=app pg-reflex-app
