# POSIX Named Pipes for Haskell

[![CI](https://github.com/pjones/named-pipes/workflows/CI/badge.svg)](https://github.githubassets.com/images/modules/site/features/actions-icon-actions.svg)
[![GitHub tag (latest by date)](https://img.shields.io/github/v/tag/pjones/named-pipes?label=release)](https://github.com/pjones/named-pipes/releases)
[![Hackage](https://img.shields.io/hackage/v/named-pipes)](https://hackage.haskell.org/package/named-pipes)

Haskell has an advanced I/O manager in its runtime system.  However,
it relies on non-blocking/asynchronous I/O to work properly.
Attempting to work with POSIX [FIFO][] files (named pipes) using
Haskell's `Handle` abstraction often leads to surprising results due to the
unusual semantics of named pipes.

For example: a typical use case for named pipes is to connect a
process to the read-end of the pipe and block until a writer has
attached and started sending data downstream.  Since Haskell defaults
to non-blocking I/O, reading from a pipe with no writers will
immediately result in an end-of-file (EOF).

Similarly, opening the write-end of a pipe that has no readers will
throw an exception.  To avoid this, the file needs to be opened in
blocking mode, but doing so prevents the current thread from being
interrupted while it waits for a reader to arrive.  Infuriating!

## Enter the `named-pipes` Package

This package provides a mid-level interface for safely working with
named pipes.  It follows expected semantics for named pipes, blocking
the current thread without blocking the entire runtime.

[fifo]: https://www.man7.org/linux/man-pages/man7/fifo.7.html
