# Muty #
>Implement a distributed mutual-exclusion lock. 
The lock will use a multicast strategy and work in an asynchronous network where we do not have access to a synchronized clock. 
You will do the implementation in three versions: the deadlock prone, the unfair, and the Lamport clocked.

>The scenario is that a set of workers need to synchronize and, they will randomly decide to take a lock and when taken, hold it for a short period before releasing it.
The lock is distributed, and each worker will operate with a given instance of the lock. 
Each worker will collect statistics on how long it took them to acquire the lock so that it can present some interesting figures at the end of each test.

