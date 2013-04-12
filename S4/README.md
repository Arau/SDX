# Groupy #
>The aim is to have several application layer
processes with a coordinated state i.e. they should all perform the same
sequence of state changes. A node that wishes to perform a state change
must first multicast the change to the group so that all nodes can execute it.
Since the multicast layer provides total order, all nodes will be synchronized.

>There will implement a group membership service that provides atomic mul-
ticast in view synchrony. The architecture of this service consists of a set
of nodes where one is the elected leader. All nodes that wish to multicast a
message will send the message to the leader and the leader will do a basic
multicast to all members of the group. If the leader dies a new leader is
elected.
>A new node that wishes to enter the group will contact any node in the
group and request to join the group. The leader will determine when the
node is to be included and will deliver a new group view to the group.
The application layer processes will use this group membership service
to synchronize their states. As commented, this service is implemented by
means of a set of group processes. Each application layer process will have
its own group process that it communicates with. The application layer
process will send the messages to be multicasted to the group process and
will receive all multicasted messages from it. The group process will tell the
application level process to deliver a message only when the rest of processes
have also delivered it. The application layer process must also be prepared
to decide if a new node should be allowed to enter the group and also decide
the initial state of this node.

