# UCengine plugin for Erlyvideo

## Introduction

[erlyvideo_ucengine](https://github.com/AF83/erlyvideo-ucengine) is a plugin to bind Erlyvideo events and UCengine events, it allows UCengine clients to connect to Erlyvideo through UCengine events.

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

And replace *trusted_login* by *ucengine_login* in *rtmp_handlers*, like that:

        {rtmp_handlers, [{auth_users_limit, 200}, ucengine_login, apps_push, apps_streaming, apps_recording]},

## Install it from scratch

         # Fetch Erlyvideo sources
         $ git clone https://github.com/erlyvideo/erlyvideo.git
         $ mkdir erlyvideo/plugins
         $ cd erlyvideo/plugins/

         # Fetch erlyvideo_ucengine sources
         $ git clone git://github.com/AF83/erlyvideo-ucengine.git erlyucengine

         # Build Erlyvideo
         $ cd ../../
         $ make

         # Update configuration, then run Erlyvideo
         $ make run

## License

Copyright 2010, af83

The default license for all files is GPLv3.
