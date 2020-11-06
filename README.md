# Task Manager Server

This application provides a web service for task management. It is a goal of
this project to provide various backends, for example, DBMS, [Linked Data](
https://www.w3.org/standards/semanticweb/data) and [TaskWarrior](
https://taskwarrior.org/).

## Building

If you have [Spago](https://github.com/purescript/spago) installed, simply do

    $ spago build

### Nix

If you have Nix installed, you can start a Nix shell with

    $ nix-shell

and then run

    $ spago build

## Running

Once you've built the application, you can run it with

    $ spago run
