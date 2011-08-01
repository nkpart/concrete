= Concrete

This is a port of the acid-state and safe-copy packages to Scala. 

== Usage

=== SafeCopy

Check out the [example](https://github.com/nkpart/concrete/blob/master/src/main/scala/concrete/Example.scala), which shows a simple serialisation task and a versioned model.

=== AcidState

TODO

== Alternatives

I wanted an embedded facility for doing persistent data, and the options I looked at tended to depend on mutable state, or were a relational model which might not be necessary for all problems.

If you did want a relational data model, HSQLDB might be a solid choice. Otherwise, one of the "object databases" might fit, such as db4o.

== Compatibility

 * Scala 2.8.1

== Developing

    git clone git://github.com/nkpart/concrete.git
    sbt
    > run

At the moment, there are no tests. If it compiles, and the example runs - then you're okay! 

== Status

I'm actively tinkering on this project, but haven't used it in production yet.
