# ShopShare


## Server

To create the database:

    $ createdb -T template0 shopshare_dev

To recreate the database from the dump file:

    $ psql shopshare_dev < db_dump.sql

To compile and run the server:

    $ stack setup
    $ stack build
    $ stack exec server


## Elm Client

To compile the Elm Client:

    $ cd client && make

You can also use:

    $ make watch

    $ make debug

    $ make clean


## Swift Client
