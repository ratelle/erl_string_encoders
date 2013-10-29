Erlang string encoders
===================

This is an erlang nif of Google's stringencoders library. https://code.google.com/p/stringencoders/

It should compile without problem if you installed string encoder in a standard location.

The implementation could be better, especially when it reallocates binaries because of the trailing null character. I will probably have to modify stringencoders for that.

The procedures can be called with any iolist but they will always return a binary.
