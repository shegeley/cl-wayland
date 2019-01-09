
# cl-wayland

`cl-wayland` is a Common Lisp wrapper for libwayland. It aims to allow Wayland compositors and clients to be written in Common Lisp.

## Status

This version of `cl-wayland` is being developed primarily for
[mahogany](https://github.com/sdilts/mahogany), and covers most of the
functions and structs that are needed for usage in common lisp.

## Requiremnts

`cl-wayland` (obiously) requires libwayland and cffi. It is likely that libwayland already exists on your Linux installation if it is recent.

## Installation

```
CL-USER> (ql:quickload :cl-wayland)
```
## wayland-scanner

`cl-wayland` contains an implementation of the
wayland-scanner tool for common lisp, and features asdf
integration. It is known to generate the server bindings properly, but
it is untested whether the client bindings are generated 100%
correctly. It introduces two new asdf directives, `:c-wl-scanner` and
`:wl-scanner`.
+ `:c-wl-scanner` is used to generate c header files of
  the appropriate type. This is useful when you are grovelling files
  that depend on these header files. When using this directive, the
  file extension of the file to be generated needs to be included.
+ ':wl-scanner` directly generates lisp source files, and exports all of the generated
  source files based on the protocol type and the protocol name.

Both of these directives have several options that control what type
of interface they generate, and where the protocol xml files are
located.
+ `:protocol-name` is the name of the protocol.
+ `:protocol-type` determines if a client or server interface is
  generated. Valid values are `:client` and `:server`.
+ ':protocol-source' tells the generator where to find the xml
  files. Four options are possible: `:wl-server`, for when
  generating the wayland-server protocol, `:wl-client` for when
  generating the wayland-client protocol, `:wl-protos` for generating
  a protocol distributed with wayland-protocols, and finally, you can
  specify an absolute path of type pathname.

### Example asdf file:
Here is a minimal example of an asdf system definition file using wayland-scanner:

``` lisp
(asdf:defsystem #:wayland-protocols
  :depends-on (#:cl-wayland)
  :defsystem-depends-on (#:wayland-scanner)
  :components
  (
   ;; Generate a file called "xdg-shell-protocol" containing the server
   ;; protocol specified in the xdg-shell protocol.
   ;; Since :protocol-source is unspecified, :wl-protos is assumed.
   ;; The package name will be "xdg-shell-protocol"
   (:wl-scanner "xdg-shell-protocol" :protocol-type :server
                :protocol-name "xdg-shell")
   (:wl-scanner "wayland-server-protocol" :protocol-type :server
                :protocol-name "wayland"
                :protocol-source :wl-server)))
```