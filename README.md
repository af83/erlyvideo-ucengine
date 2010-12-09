# UCengine plugin for Erlyvideo

## Introduction

erlyvideo_ucengine is a plugin to bind Erlyvideo events and UCengine events, it allows UCengine clients
to connect to Erlyvideo through UCengine events.

## Docs

### UCengine

[docs.ucengine.org](http://docs.ucengine.org)

## Configuration

Add the following lines to our erlyvideo.conf:

        {ucengine, [{host, "localhost"},
            {port, 5280},
            {uid, "erlyvideo"},
            {token, "da93ae03c1280f82709f857ffa22f0a30c26fa9c"}]}.

And don't forget to add 'ucengine' in our module list:

        {modules, [ucengine]}.

## Dependencies

        - ibrowse (https://github.com/dizzyd/ibrowse)

## Install it from scratch

         # Fetch Erlyvideo sources
         $ git clone https://github.com/erlyvideo/erlyvideo.git
         $ mkdir erlyvideo/plugins/
         $ cd erlyvideo/plugins/

         # Fetch erlyvideo_ucengine sources
         $ git clone git://github.com/AF83/erlyvideo-ucengine.git

         # Fetch ibrowse and build it
         $ cd ../deps/
         $ git clone https://github.com/dizzyd/ibrowse.git
         $ cd ibrowse
         $ make

         # Build Erlyvideo
         $ cd ../../
         $ make

         # Update configuration, then run Erlyvideo
         $ make run

## License

Copyright 2010, af83

The default license for all files is GPLv3.

