# joplin-mode
Emacs client for accessing Joplin note

## Requirements

- [markdown-mode](https://github.com/jrblevin/markdown-mode)

## Installation

Add this line on your init file:

    (add-to-list 'load-path 
                 (expand-file-name "*JOPLIN-MODE-PACKAGE-DIRECTORY*"))

    (require 'joplin-mode)
    
    (with-eval-after-load "markdown-mode"
      (add-to-list 'markdown-mode-hook 'joplin-note-mode))

Or, you couse use following `use-package` macro:

    (use-package joplin-mode
      :load-path (expand-file-name "*JOPLIN-MODE-PACKAGE-DIRECTORY*")
      :requires markdown-mode
      :config
      (add-to-list 'markdown-mode-hook 'joplin-note-mode))


In Joplin application, goto [Options]->[Web Clipper], and make sure you
enabled the clipper service.

### Linux or Mac
It should work.

### Windows
    
### Windows /w Emacs on WSL

This section assumes that you have WSL2 (Widnows subsystem for Linux) and did not change the network configuration of it.   Since, your Linux box is placed on a private network range, and JoplinApp only listen to 127.0.0.0 network, you cannot connect Joplin from your Linux box.  However, if you deploy proxy server on Windows, and let `joplin-mode` to connect to the proxy first, then you can use it.

First, check the IP address of the Windows from WSL.  Open the file `/etc/resolv.conf` and you can get the Windows IP address from the `nameserver` field.  (See [Accessing network applications with WSL](https://learn.microsoft.com/en-us/windows/wsl/networking) for more details.

    $ cat /etc/resolv.conf | grep nameserver
    172.22.144.1

Next, you'll need to run proxy server on Windows.  The easiet one is to install [mitmproxy](https://mitmproxy.org/).  By default it will listen on `8080` port on your Windows.  Launch the proxy server.

Then, using the Windows IP address and the proxy port, construct the proxy endpoint in your Emacs init file (`$HOME/.emacs` or `$HOME/.emacs.d/init.el`):

    (setq joplin-url-proxy '(("http" . "172.22.144.1:8080")))

For `use-package`, you could add above line in the `:config` part:

    (use-package joplin-mode
      ...
      :config
      (setq joplin-url-proxy '(("http" . "172.22.144.1:8080")))
      ...)


Make sure that you enabled Web Clipper service in Joplin.  To test it, from your WSL terminal, do this:

    $ curl -s --proxy http://172.22.144.1:8080 http://127.0.0.1:81184/
    JoplinClipperServer

If you get the string "JoplinClipperServer", congratulation!  Now you can access Joplin from Emacs on WSL.
    
## Usage

If you use `joplin-mode` first time, it will ask your permission to
get API token.  Make sure you allow it in Joplin application, then in
Emacs, press `Return` (or `Enter`) key.


### `joplin-mode`: Joplin Notebook buffer 

`M-x joplin` will list up all of your notebooks.  Unfortunately, there is nothing much you can do at the moment.


- `C-n` or `n`: move to the next folder
- `C-p` or `p`: move to the previous folder 
- `s`: search note (or `M-x joplin-search`)

### `joplin-search-mode`: Joplin Note Search buffer

`M-x joplin-search` will accept query string, and list the notes that match.

- `s` or `/`: new search
- `g`: revert (call Joplin again with the same search query)
- `C-n` or `n`: move to the next note
- `C-p` or `p`: move to the previous note
- `t i`: toggle id field on the buffer
- `t c`: toggle created time of the note in the buffer
- `t u`: toggle updated time of the note in the buffer
- `^`, `C-c C-j` or `C-c j j`: jump to folder buffer
- `RET`: visit the note
- `o`: visit the note in other window

For query format, see [Joplin Search Syntax](https://discourse.joplinapp.org/t/search-syntax-documentation/9110).

You can mark(select) multiple notes, then do certain operations in
bulk.  For example, moving one or more notes to another
folder(notebook).

- `m`: mark current note
- `u`: unmark current note
- `U`: unmark all notes
- `t`: toggle marks
- `% m`: mark notes that matches with regular expression

- `d`: flag current note for deletion
- `% d`: flag notes that matches with regular expression

- `M`: move marked notes to specific notebook
- `x`: delete all flagged notes

You can sort the list of notes in the search buffer:

- `S t`: sort by note title 
- `S u`: sort by updated time of the note
- `S c`: sort by created time of the note


### `joplin-note-mode`: Joplin Note buffer

There are two types of note buffer in `joplin-mode`:

- If you visit Joplin note from the search buffer, `joplin-mode` will create a buffer for the note.  This note does not have any association with the local file system, and any usual saving action will update the note in Joplin app.

- You can enable the minor mode `joplin-note-mode` on any emacs buffer.  The installation section already provided the init code so that any `markdown-mode` buffer will automatically enable `joplin-note-mode`. 
  - If you press `C-c j s`, `joplin-note-mode` will save the note to whatever associated file in your local file system, then upload the note to the Joplin, and de-associated the buffer with whatever file in your local file system.  Any subsequent saving action will update the note in Jolin app.
  - You'll need to provide the note title and notebook name.
 

