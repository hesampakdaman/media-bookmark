-- media-bookmark.lua: let Emacs find and bookmark mpv.
--
-- With this script in mpv's scripts directory:
--
-- - Every mpv without an IPC socket gets one, named after its process
--   ID, in $XDG_RUNTIME_DIR/mpv-sockets (or $TMPDIR/mpv-sockets, or
--   /tmp/mpv-sockets).  M-x media-bookmark-attach in Emacs looks there.
--
-- - Ctrl+b asks the Emacs following this mpv to bookmark what it
--   plays.  To use another key, add to input.conf:
--
--     KEY script-binding media_bookmark/bookmark
--
-- (mpv names this script media_bookmark.)
--
-- Install it with M-x media-bookmark-mpv-install-script.
--
-- SPDX-License-Identifier: GPL-3.0-or-later

local msg = require 'mp.msg'
local utils = require 'mp.utils'

local function show(text)
  msg.verbose(text)
  mp.osd_message(text, 3)
end

-- Bookmarking.  Emacs answers with "bookmarked"; without an answer,
-- no Emacs follows this mpv.

local waiting = nil

mp.add_key_binding('ctrl+b', 'bookmark', function ()
  mp.commandv('script-message', 'media-bookmark', 'bookmark')
  if waiting then waiting:kill() end
  waiting = mp.add_timeout(1, function ()
    waiting = nil
    show('No Emacs follows this mpv (M-x media-bookmark-attach)')
  end)
end)

mp.register_script_message('bookmarked', function (text)
  if waiting then
    waiting:kill()
    waiting = nil
  end
  show(text)
end)

-- IPC socket.

if mp.get_property('input-ipc-server', '') == '' then
  local base = os.getenv('XDG_RUNTIME_DIR') or os.getenv('TMPDIR') or '/tmp'
  local directory = base .. '/mpv-sockets'
  local socket = directory .. '/' .. utils.getpid()

  mp.command_native({name = 'subprocess', playback_only = false,
                     args = {'mkdir', '-p', directory}})
  mp.set_property('input-ipc-server', socket)

  mp.register_event('shutdown', function ()
    os.remove(socket)
  end)
end
