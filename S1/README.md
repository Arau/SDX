# Chatty #

## Version 1 ##
In this implementation, we have one server and multiple clients. The clients
send messages to the server and distributes them to the rest of clients.

You can explore our code [here](https://github.com/magarcia/SDX/tree/c56d7ba150c049833a655ac51a64ac95f80f9ff8/S1).

## Version 2 ##
For this version, we developed a more robust implementation because the first
version has a critical point, the server. It consist in having multiple
instances of server that they communicate with each other. The clients can be
connected with any of these server and they provide the received messages to
other servers and thus spreading messages to all clients.

You can explore our code [here](https://github.com/magarcia/SDX/tree/23b27e787095687654342e19bae970ec4624f404/S1).


