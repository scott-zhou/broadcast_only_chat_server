# Broadcast only chat server

Implemented in Erlang, for test the Erlang TCP performance.

The server part will start TCP listen on a specified port, and accept all remote TCP connects as a client.
Any message received from client will be broadcast to all other clients.

The client part is a simple implementation for test.

Created by [Scott Zhou](http://www.scottzhou.me)
