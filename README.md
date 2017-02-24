# Heed - RSS Reader

Composed of 3 main parts:

    - heed-backend
    - heed-lib
    - heed-vty

## Build

Build with`stack` (tested), `cabal` (not tested), `cabal new-build` (tested with `cabal HEAD`).

Run **one** of:

    - stack build
    - cabal new-build heed-lib heed-backend heed-vty # Cabal 1.24
    - cabal new-build # cabal HEAD, requires ghc-8.0.2

Building will probably take a long time since the dependency list is quite big (~180 deps).

## Installing

Installation is still a very manual process, in the future installation/schema updates will be automated as much as possibile.

`heed-backend` expects `postgresql` running and listening  on the default port and
a conf file in `/etc/heed/backend.ini` (an example file is present in the `confs/` subfolder).
The `postgresql` user mentioned in the `ini` file should be able to connect without a password.

Once the user and database exists import `table.sql` into `postgres`.

Then you should add a user to the database (a command line options will be written sooner or later) by importing
`user.sql` (changing the password hash with your own password hash generated by calling `Heed.Crypto.generateHash yourpass`)

`heed-vty` expects a conf file: `$XDG_CONFIG_HOME/heed/heed.ini` (usually `$HOME/.config/heed/heed.ini`), an example file is available in `confs/`.

Then you can run `heed-vty` (with the correct `port/host/ssl` information in `heed.ini`).

Adding feeds is then a matter of pressing `n` and supplying the needed information.

# Components

## heed-backend

Downloads and saves feed information/items in `PostgreSQL`

## heed-vty

Displays information via a glances-like interface

### Vty keybindings

    - e: Opens 'Edit feed' screen
    - n: Opens 'Add feed' screen
    - j: Go to next item and set selected item as read
    - k: Go to previous item and set selected item as read
    - J: Go to next feed
    - K: Go to previous feed
    - o: Opens feed in chromium
    - a: Set all items of selected feed as read and go to next feed
    - r: Force refresh of selected feed
    - q/esc: quit

## heed-lib

Common library so we can, for example, keep serialization/unserialization code in sync between back and frontend.

# Some motivation and history

I wanted an rss reader that was completley usable without a mouse and what I was using before (`tt-rss`) crashed a couple of times
a day for no apparent reason.

The project started out with a `GCHJS` frontend but since javascript is very limited (I found no "sane" way of opening a link in the background
that would work across browsers) I switched to [brick](https://github.com/jtdaugherty/brick/), which has been very stable and easy to work with.

## Haskell notes

I created some mtl-style classes hoping to be able to test my functions. I hope to write these tests sooner or later.

Every feed will start an update thread that will download and add the latest items to the database and at the same time broadcast
to every connected user the number of new items.

All things SQL works thanks to [opaleye](https://github.com/tomjaguarpaw/haskell-opaleye).
