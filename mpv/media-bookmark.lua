-- media-bookmark.lua: give each mpv an IPC socket that Emacs can find.
--
-- With this script in mpv's scripts directory, every mpv without an
-- IPC socket gets one, named after its process ID, in
-- $XDG_RUNTIME_DIR/mpv-sockets (or $TMPDIR/mpv-sockets, or
-- /tmp/mpv-sockets).  M-x media-bookmark-attach in Emacs looks there.
-- Install it with M-x media-bookmark-mpv-install-script.
--
-- SPDX-License-Identifier: GPL-3.0-or-later

local utils = require 'mp.utils'

if mp.get_property('input-ipc-server', '') ~= '' then
  return
end

local base = os.getenv('XDG_RUNTIME_DIR') or os.getenv('TMPDIR') or '/tmp'
local directory = base .. '/mpv-sockets'
local socket = directory .. '/' .. utils.getpid()

mp.command_native({name = 'subprocess', playback_only = false,
                   args = {'mkdir', '-p', directory}})
mp.set_property('input-ipc-server', socket)

mp.register_event('shutdown', function ()
  os.remove(socket)
end)
