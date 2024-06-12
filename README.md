# Jwno #

A tiling window manager for Windows 10/11, built with [Janet](https://janet-lang.org/).

## Features ##
 
* Highly customizable. It's configured with the language it's written in. See `example\example-config.janet` for a working example.
* REPL support. You can poke its internals and experiment with new configs while it's still running.
* Powerful key binding system that supports multi-level and transient key maps.
* Makes use of the more stable UIAutomation API to manipulate windows.
* Less intrusive, and does **not** keep enforcing window geometries.
* Multi-monitor support.
* Native Virtual Desktop support.

## Dependencies ##

* Visual Studio 2022 (the Community version will do)
* Janet >= 1.34.0 (compiled with the MSVC toolchain)
* [Jpm](https://janet-lang.org/docs/jpm.html)
* [Jw32](#)

## Compiling ##

1. Set `JANET_SOURCE_PATH` environment variable to point to the Janet source tree;
2. Start an `x64 Native Tools Command Prompt for VS 2022`;
3. Run `jpm -l build` in **Jw32** source directory;
4. Run `jpm --tree=path\to\jwno\jpm_tree install` in **Jw32** source directory, to install it as a dependency for **Jwno**;
5. Run `jpm -l deps` in **Jwno** source directory;
6. In **Jwno** source directory, run `jpm -l run embed-manifest`;
7. Check out the built artifact (`jwno.exe`) in `build\`.

## Installing ##

There's no installation needed. `Jwno.exe` is a self-contained executable that can be launched from anywhere in the file system.

## Configuration ##

Jwno's config file is a plain Janet source file. It should be named `jwno-config.janet` and placed alongside `jwno.exe`.

Or, you can use the `-c` flag to specify another config file. Just launch `jwno.exe` from the command line: `jwno.exe -c path\to\config\file.janet`

See `example\example-config.janet` for an example.

### Specifying Key Bindings ###

Jwno uses a simple syntax to specify the keys you want to bind. Some examples:

* `win+a`: Hold the Win key, then press the letter A.
* `win+shift+a`: Hold the Win key and the Shift key, then press the letter A.
* `lctrl+a ralt+b`: Hold the left Ctrl key, press the letter A, **release them**, then hold the right Alt key, press the letter B.
* `lshift+rshift+a`: Hold the left and right Shift keys at the same time, then press the letter A.
* `a b c`: Press and release the letters A, B, and C, in the specified order.

All key names are case-insensitive. Modifier keys has left and right variants (lwin, rshift, etc.). And you may use `-` in place of `+` if that suits your taste.

**Note** that Jwno only supports limited key codes at the moment. Check out the source code (`key.janet`) for all supported keys.

## Debugging ##

You can pass the `--log-level debug` and `--log-file <file-name>` flags to enable logging. **Note** that debug logs will output all keyboard events by default.

To inspect Jwno's internal states, start it with the option `--repl 127.0.0.1:9999`, then use `jwno.exe -C --repl 127.0.0.1:9999` to connect.

## Terminology ##

Jwno uses certain data structures to store and manipulate window states:

* Window: A managed  window, which can be moved around, resized, or fitted into a frame. Jwno knows nothing about unmanaged windows.
* Frame: A rectangular screen area that can contain either managed windows or other frames. Can be resized, but must conform to the sizes of its sibling and parent frames.
* Top Frame: The top level frame that represents the working area of a monitor. In Jwno, every monitor is represented by a top frame that can not be resized or closed.
* Frame Tree: A tree structure that consists of the frame hierarchy and the managed windows.
* Layout: A frame tree that stores the layout of frames and windows for a single Virtual Desktop. Each child of a layout is a top frame.
* Root: A container that stores layouts for Virtual Desktops. Each child of the root is a layout.
* Tree Node: A generic name for a node in the frame tree. May be a layout, a frame, or a window.
* Current Child: Every non-window tree node has an active child called the current child. Jwno uses this info to track the active monitor and windows etc.

To put it visually (Arrows means "parent -> children" relationship):

```
Root --->+-- Layout 1 (Desktop 1) --->+-- Top Frame 1 (Monitor 1) --->+-- Frame 1 --->+-- Window 1
         |                            |                               |               |
         +-- Layout 2 (Desktop 2)     +-- Top Frame 2 (Monitor 2)     +-- Frame 2     +-- Window 2
         |                            |                               |               |
         +-- Layout 3 (Desktop 3)     +-- Top Frame 3 (Monitor 3)     ...             +-- Window 3
         |                            |                                               |
         ...                          ...                                             ...

```

Frames cannot overlap each other, but windows in the same frame always overlap each other.

A frame can only contain other frames, or windows, not both.

You can use frame/window enumeration commands to navigate around frames/windows.
