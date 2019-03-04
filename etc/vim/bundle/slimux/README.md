# Slimux

This is a SLIME inspired tmux integration plugin for Vim. It makes it easy to interact
with different tmux panes directly from Vim. It has two styles for interacting
with panes. REPL and Shell styles.

REPL commands are designed to work with various Read Eval Print Loops such as
`python`, `irb` (Ruby), `node` (Javascript), `coffee` (CoffeeScript) etc.
This is loosely modelled after [SLIME for Emacs][SLIME]. Shell commands are designed
to work with normal shells such as `bash`. These are useful for running tests for
example.

Main difference between these is pane configuration visibility. Each buffer has
own configuration for REPL, but for Shell there is only one global
configuration. The configuration is prompted from user when the commands are
used for the first time. The configuration happens interactively. You will see
list of available tmux panes and you can choose one by hitting enter on top of
one.

Also REPL commands can have custom pre, post and escape hooks. These allows
interaction with some more complex REPLs such as the CoffeeScript REPL.

This plugin borrows ideas and some code from [vim-slime][].


Blog post

http://esa-matti.suuronen.org/blog/2012/04/19/slimux-tmux-plugin-for-vim/

Ascii.io screencast

https://asciinema.org/a/409


## Installation

Use [pathogen][] and put files to
`$HOME/.vim/bundle/slimux/`

Slimux requires fairly recent tmux version. Be sure you have 1.5.x or later.

## REPL Commands

### SlimuxREPLSendLine

Send current line to the configured pane.

### SlimuxREPLSendSelection

Send last visually selected text to the configured pane.

### SlimuxREPLSendBuffer

Send current buffer to the configured pane.

### SlimuxREPLConfigure

Prompt pane configuration for the current buffer.


## Shell Commands

### SlimuxShellPrompt

Prompt for a shell command and send it to the configured tmux pane.

### SlimuxShellLast

Rerun last shell command(prompts for a shell command if none was run before).

### SlimuxShellRun

Specify a shell command to run directly, without the prompt:

    :SlimuxShellRun rspec spec/foo_spec.rb

Suitable for mapping and other automation.

Note that you need to escape strings intended for the shell.
E.g. to list files with actual asterisks in their name:

    :SlimuxShellRun ls *\**

### SlimuxShellConfigure [pane-id]

Select the tmux pane to which shell commands will be sent. If executed without a parameter it prompts global pane configuration for the shell commands. If called with parameter it selects the pane without user interaction, which will aid multi-pane workflows. Parameter is the tmux pane identifier in \d:\d:\d form and completed automatically with cline completion.


## Sending Keys

### SlimuxSendKeysPrompt

Prompt for a key sequence using the 'tmux send-keys' syntax and send it to
a configured tmux pane.
E.g. Lets say you want to stop the webserver currently running in a
shell(ctrl+c) and restart it(assuming the command to start is 'make run-server'):

    :SlimuxSendKeysPrompt
    KEYS>C-C 'make run-server' Enter
    
or run previous command with

    KEYS>Up Enter

In short, some strings such as 'C-C' or 'Enter' have special meanings,
while others are sent as a sequence of character keys(in the above example, 'make run-server')

### SlimuxSendKeysLast

Resends the last key sequence sent(prompts for a sequence if none was sent before).

### SlimuxSendKeysConfigure

Prompt global pane configuration to send the key sequence.


## Keyboard Shortcuts

Slimux does not force any shortcuts on your Vim, but here's something you can
put to your `.vimrc`

    map <Leader>s :SlimuxREPLSendLine<CR>
    vmap <Leader>s :SlimuxREPLSendSelection<CR>
    map <Leader>b :SlimuxREPLSendBuffer<CR>
    map <Leader>a :SlimuxShellLast<CR>
    map <Leader>k :SlimuxSendKeysLast<CR>

Or if you like something more Emacs Slime style try something like this:

    map <C-c><C-c> :SlimuxREPLSendLine<CR>
    vmap <C-c><C-c> :SlimuxREPLSendSelection<CR>

You may also add shortcuts to other commands too.

For Scheme/Racket Slimux has few extra bindings. Enable them with

    let g:slimux_scheme_keybindings=1
    let g:slimux_racket_keybindings=1

For more information refer to the [scheme plugin header](https://github.com/epeli/slimux/blob/master/ftplugin/scheme.vim).


## Adding support for new languages

Usually new there is no need to do anything. For example Ruby and Node.js REPLs
works just fine out of box, but for some languages you have to do some preprocessing
before the code can be sent. There are three hooks you can specify for
each language.

Custom escaping function

    function SlimuxEscape_<filetype>(text)
        return a:text
    endfunction

Pre send hook

    function SlimuxPre_<filetype>(target_pane)
    endfunction

Post send hook

    function SlimuxPost_<filetype>(target_pane)
    endfunction

Just add these to ftplugin directory contained within this plugin (and sent a pull request on Github!).
You can use [Python][] and [CoffeeScript][] hooks as examples.

## Options

- `g:slimux_tmux_path = /path/to/your/tmux` sets the path to the
  preferred tmux binary, if not specified, defaults to getting tmux
  command from path using `system('command -v tmux')`.

- `g:slimux_select_from_current_window = 1` select panes only from current
  window. Defaults to `0` to select panes from all tmux panes.

- `g:slimux_pane_format` customize the formatting of the panes, see the FORMATS section in `man tmux`.  
  The string "`#{pane_id}: `" is always prepended to the format so Slimux can identify the selected pane.


## Other Vim Slime plugins

Before I created this plugin I tried several others, but non of them satisfied me. They where too
complicated or just didn't support the languages I needed. So if Slimux isn't your cup of tea,
maybe one of these is:

  * <https://github.com/jpalardy/vim-slime>
  * <https://github.com/benmills/vimux>
  * <https://github.com/kikijump/tslime.vim>
  * <https://github.com/jgdavey/vim-turbux>
  * <http://www.vim.org/scripts/script.php?script_id=2531>
  * <https://github.com/ervandew/screen>
  * <https://github.com/mhinz/vim-tmuxify>



[tmux]: http://tmux.sourceforge.net/
[pathogen]: https://github.com/tpope/vim-pathogen
[vim-slime]: https://github.com/jpalardy/vim-slime
[SLIME]: http://common-lisp.net/project/slime/

[Python]: https://github.com/epeli/slimux/blob/master/ftplugin/python.vim
[CoffeeScript]: https://github.com/epeli/slimux/blob/master/ftplugin/coffee.vim
