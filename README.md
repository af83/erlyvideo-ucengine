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

## License

Copyright 2010, af83

The default license for all files is GPLv3.

