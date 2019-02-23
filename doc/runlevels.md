= Runlevels

It is sometimes hard to know the relationship between different servers.
Runlevels organizes some servers into different layers.
Where each runlevel start_link:s the runlevel below.

Technically, the runlevel is a supervisor.
For runlevel R, the runlevel has two main purposes:

1. Supervise the runlevel below (R-1)
2. Supervise all servers / workers on current runlevel (R)

Thus, for the main application, you only have to start the last runlevel.

There are two special runlevels: 0 and N.
They are there only to help the developer and our mental model.
Runlevel 0 acts as the base case, the empty runlevel.
Runlevle N acts as the last runlevel, that never supervises workers, only the last real runlevel.
It is therefore always safe to use runlevelN:start_link(), to bring up the whole stack.

=== Testability

In addition to group the servers, runlevels are useful for testing.
When you are testing an individual module you can selectivly start the runlevel below.
Thus, if a server should run in runlevel R, always runlevel(R-1):start_link(), in the test setup.
That way you have a well defined support / foundation that you can use.
Those services, beloning to runlevel R-1, are up and running correctly.
