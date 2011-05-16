# U.C.Engine plugin for Erlyvideo

## Introduction

[erlyvideo_ucengine](https://github.com/AF83/erlyvideo-ucengine) is a plugin to bind Erlyvideo events and U.C.Engine events, it allows U.C.Engine clients to connect to Erlyvideo through U.C.Engine events.

## Docs

### U.C.Engine

[docs.ucengine.org/erlyvideo.html](http://docs.ucengine.org/erlyvideo.html)

## Install it from scratch

    # Fetch af83's fork of Erlyvideo
    $ git clone https://github.com/AF83/erlyvideo.git --branch plugins
    $ mkdir erlyvideo/plugins
    $ cd erlyvideo/plugins/

    # Fetch erlyvideo_ucengine sources
    $ git clone git://github.com/AF83/erlyvideo-ucengine.git erlyucengine

    # Build Erlyvideo
    $ cd ../../
    $ make

    # Update configuration, then run Erlyvideo
    $ make run

## Configuration

Add the following lines to our *priv/erlyvideo.conf*:

```erlang
{ucengine, [{host, "localhost"},
    {port, 5280},
    {uid, "erlyvideo"},
    {token, "da93ae03c1280f82709f857ffa22f0a30c26fa9c"}]}.
```

And don't forget to add 'ucengine' in our module list:

```erlang
{modules, [ucengine]}.
```

And replace *trusted_login* by *ucengine_login* in *rtmp_handlers*, like that:

```erlang
{rtmp_handlers, [{auth_users_limit, 200}, ucengine_login, apps_push, apps_streaming, apps_recording]},
```

## License

Copyright 2010-2011, af83

The default license for all files is GPLv3.
