# joplin-mode
an Emacs client for accessing [Joplin](https://joplinapp.org/) notes.

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


In JoplinApp, goto [Options]->[Web Clipper], and make sure you enabled the clipper service.

### Linux or Mac
It should work.

### Windows
Not tested.

### Windows /w Emacs on WSL

This section assumes that you have WSL2 (Widnows subsystem for Linux) and did not change the network configuration of it.   Since, your Linux box is placed on a private network range, and JoplinApp only listen to 127.0.0.0 network, you cannot connect Joplin from your Linux box.  However, if you deploy proxy server on Windows, and let *joplin-mode* to connect to the proxy first, then you can use it.

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


Make sure that you enabled Web Clipper service in JoplinApp.  To test it, from your WSL terminal, do this:

    $ curl -s --proxy http://172.22.144.1:8080 http://127.0.0.1:81184/
    JoplinClipperServer

If you get the string "JoplinClipperServer", congratulation!  Now you can access Joplin from Emacs on WSL.
    
## Usage

If you use *joplin-mode* first time, it will ask your permission to get API token.  Make sure you allow it in Joplin application, then in Emacs, press `Return` (or `Enter`) key.


### `joplin-mode`: Joplin buffer (list of notebooks)

`M-x joplin` will list up all of your notebooks.

- `C-n` or `n`: move to the next folder
- `C-p` or `p`: move to the previous folder 
- `RET`: visit notebook buffer
- `s`: search note (or `M-x joplin-search`)

### `joplin-search-mode`: Joplin Note Search buffer

`M-x joplin-search` will accept query string, and list the notes that match.

- `s` or `/`: new search
- `g`: revert (call JoplinApp again with the same search query)
- `C-n` or `n`: move to the next note
- `C-p` or `p`: move to the previous note
- `t i`: toggle id field on the buffer
- `t c`: toggle created time of the note in the buffer
- `t u`: toggle updated time of the note in the buffer
- `^`, `C-c C-j` or `C-c j j`: jump to Joplin buffer
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

### Joplin Notebook buffer
When you visit notebook buffer from joplin buffer (by `RET`), you'll see the list of notes similar to Joplin Note Search buffer. 

### `joplin-note-mode`: Joplin Note buffer

There are two types of note buffer in `joplin-mode`:

- If you visit Joplin note from the search buffer, *joplin-mode* will create a buffer for the note.  Any usual saving action such as `C-x C-s` will update the note in JoplinApp.

- You can enable the minor mode `joplin-note-mode` on any emacs buffer.  The installation section already provided the init code so that any `markdown-mode` buffer will automatically enable `joplin-note-mode`. 
  - If you press `C-c j s`, `joplin-note-mode` will save the note to whatever associated file in your local file system first.
  - You'll need to provide the note title and notebook name.
  - If the markdown has a link to a file or an image link, and the file is accessible from the local file system, *joplin-mode* will upload the file to JoplinApp, and convert the link target to a Joplin resource. 
  - Then it will upload the note text to the JoplinApp, and de-associated the buffer with whatever file in your local file system.
  - Any subsequent modification in the buffer will be updated in JoplinApp not the original file.
 
Common key bindings:
  - `C-c j s`: save the buffer to JoplinApp
  - `C-c j l`: upload the file as a Joplin resource in the link at the point, convert the link target.  You don't need to use this as `C-c j s` will upload all local links.
  - `C-c j j`: jump to the parent buffer
  - `C-c j r`: list all resources belongs to this note
  - `C-c j t`: show tags of the current note
  - `C-u C-c j t`: edit tags of the current note
  
If you visit a note from Joplin, not from your local disk, *joplin-mode* will create buffer with `markdown-mode` and `view-mode` enabled, which means the buffer is read-only by default.    You can scroll the buffer with your usuall keybindings in addition to `<RET>`, which also scroll forward.   Pressing `e` will quit the *view-mode* and makes the buffer editable.  Pressing `q` will quit the *view-mode* and delete the buffer.   See also (the view-mode manual)[https://www.gnu.org/software/emacs/manual/html_node/emacs/View-Mode.html].

## Known problems / Help wanted

Here are current backlogs that I need to do.  Any help will be greatly appreciated.

  - The order of search results are not the same as JoplinApp.  I have not figured out how to sort the response from JoplinApp.  If somebody is able to demonstrate the search query request using `curl(1)`, and how to sort the output to match with JoplinApp...
  - Some help on how to register *joplin-mode* to [Melpa](https://melpa.org/).
  - keyword completion on search; autocompleting Joplin search operators such as "any:", "title:", "body:", etc.  I do not want to rely on external packages such as [helm](https://github.com/emacs-helm/helm), but want to implement with vanilla Emacs features.
  - In the current implementation, all Joplin note buffer are bound to a single temporary file.   This is necessary to override Emacs's default saving action to upload to JoplinApp.  Ideally, Joplin note buffer should not be bound to any local temporary files. 
  - Clean up the source code, and possibly optimize it.
  
  
