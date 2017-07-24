# Einfluss

A small application that transforms JSON payloads posted from 
[The Things Network]'s [HTTP integration] into the [InfluxDB] line protocol, and
attempts to insert them into a provided InfluxDB database.

## Install, Build, & Run

Clone this repository 

    git clone git@github.com:jkachmar/einfluss.git

Ensure that [yarn], [purescript], [pulp], and [bower] are installed

    npm install --global yarn purescript pulp bower
    
Install dependencies

    yarn install

Build the project

    yarn build
    
Ensure that the environment variables in `.env.example` are set in your local
environment.

Ensure the following environment variables are set:

- `DB_HOST`, `DB_NAME`, `DB_PORT`, `DB_USER`, `DB_PASS` with the appropriate 
[InfluxDB] connection info
- `ENV` to either `Development` or `Production` (default to `Development`, which
assumes a local [InfluxDB] instance, default `root` username and password, and
an existing database called `einfluss`
- `PORT` to the port to serve this API on (defaulting to `8080`)

Start the server

    yarn start
    
At this point, the server is running and can either be tested locally by posting
to `localhost/api/ttn-influx`, or deployed by giving it a publicly accessible
address and connecting the `/api/ttn-influx` endpoint to [The Things Network]'s
[HTTP Integration] service.

## ACHTUNG!

Currently the service decodes [The Things Network] payloads into a `StrMap` of 
`String`s (object keys) and `Number`s (object values), so sending any payload
that doesn't follow this format will fail during decoding.

[The Things Network]: https://www.thethingsnetwork.org 
[HTTP Integration]: https://www.thethingsnetwork.org/docs/applications/http/
[InfluxDB]: https://www.influxdata.com/time-series-platform/influxdb/
[yarn]: https://yarnpkg.com
[purescript]: http://www.purescript.org
[pulp]: https://github.com/purescript-contrib/pulp
[bower]: https://bower.io
